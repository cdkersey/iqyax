#include <iostream>
#include <sstream>
#include <iomanip>

#include <chdl/chdl.h>
#include <chdl/egress.h>
#include <chdl/cassign.h>

#include <chdl/counter.h>

#include "interfaces.h"

#ifdef RANDOM_STALL
#include <chdl/lfsr.h>
#endif

#ifdef BTB
#include <chdl/bloom.h>
#endif

#ifdef MUL_DIV
#include "muldiv.h"
#endif

#ifdef SST_MEM
#define SIM_
#include "chdl-sst.h"
#undef SIM_
#endif

using namespace std;
using namespace chdl;
using namespace s_core;

void Fetch(fetch_decode_t &out, exec_fetch_t &in,
           const char *hex_file, unsigned initial_pc);
void Decode(decode_reg_t &out, fetch_decode_t &in);
void Reg(reg_exec_t &out, decode_reg_t &in, mem_reg_t &in_wb);
void Exec(exec_mem_t &out, exec_fetch_t &out_pc, reg_exec_t &in, mem_exec_t &f);
void Mem(mem_reg_t &out, mem_exec_t &fwd, exec_mem_t &in,
         const char *hex_file, bool &stop_sim);

void simple_core(const char *hex_file, unsigned initial_pc, bool &stop_sim) {
  fetch_decode_t fetch_decode;
  decode_reg_t decode_reg;
  reg_exec_t reg_exec;
  exec_mem_t exec_mem;
  exec_fetch_t exec_fetch;
  mem_reg_t mem_reg;
  mem_exec_t mem_exec;

                                                         // Pipeline:
  Fetch(fetch_decode, exec_fetch, hex_file, initial_pc); //   STAGE 1
  Decode(decode_reg, fetch_decode);                      //   STAGE 2
  Reg(reg_exec, decode_reg, mem_reg);                    //     STAGE 5
  Exec(exec_mem, exec_fetch, reg_exec, mem_exec);        //   STAGE 3
  Mem(mem_reg, mem_exec, exec_mem, hex_file, stop_sim);  //   STAGE 4

  TAP(fetch_decode);
  TAP(decode_reg);
  TAP(reg_exec);
  TAP(exec_mem);
  TAP(exec_fetch);
  TAP(mem_exec);
  TAP(mem_reg);
}

int main(int argc, char** argv) {
  bool stop_sim(false);

  unsigned initial_pc(0x400000);
  if (argc >= 3) {
    istringstream iss(argv[2]);
    iss >> hex >> initial_pc;
  }

  simple_core((argc >= 2 ? argv[1] : "score.hex"), initial_pc, stop_sim);

  if (cycdet()) { cerr << "Error: Cycle detected.\n"; return 1; }

  optimize();

  ofstream cpr("score.cp");
  critpath_report(cpr);

  #ifdef SST_MEM
  chdl_sst_sim_run(stop_sim, TMAX);
  #else
  ofstream vcd("score.vcd");
  run(vcd, stop_sim, TMAX);
  #endif

  return 0;
}

#ifdef BTB
template <unsigned M, unsigned N> bvec<M> Fold(bvec<N> in) {
  HIERARCHY_ENTER();
  vec<M, bvec<N/M> > x;
  for (unsigned i = 0; i < N; i += M)
    for (unsigned j = 0; j < M; ++j)
      x[j][i/M] = in[i + j];

  bvec<M> r;
  for (unsigned i = 0; i < M; ++i)
    r[i] = XorN(x[i]);

  HIERARCHY_EXIT();

  return r;
}
#endif

void Fetch(fetch_decode_t &out_buf, exec_fetch_t &in,
           const char *hex_file, unsigned initial_pc)
{
  HIERARCHY_ENTER();
  fetch_decode_t out;

  word_t pc;

  #ifdef BTB
  node clear_bf, branch, stall(Lit(0)),
       isBranchFalsePositive(!_(in, "branch") && _(in, "bp_branch"));
  branch = BloomFilter<BF_SZ, BF_HASHES>(
    pc, _(in,"branch_pc"), _(in,"branch"), clear_bf
  );
  TAP(branch);

  #ifdef STALL_SIGNAL
  stall = _(out_buf, "stall");
  #endif

  // Clear bloom filter every 7 false positives
  bvec<3> isBranchFalsePositiveCount;
  isBranchFalsePositiveCount =
    Wreg(isBranchFalsePositive || isBranchFalsePositiveCount == Lit<3>(7),
           isBranchFalsePositiveCount + Lit<3>(1));
  clear_bf = AndN(isBranchFalsePositiveCount);

  Counter("isbranch_false_negatives",
            _(in, "branch") && !_(in, "bp_branch"));
  Counter("isbranch_false_positives", isBranchFalsePositive);
  Counter("isbranch_true_positives",
            _(in, "branch") && _(in, "bp_branch"));
  _(out, "bp_branch") = Latch(Reg(stall), branch);

  typedef ag<STP("valid"), node,
          ag<STP("next_pc"), bvec<N>,
          ag<STP("state"), bvec<2> > > > btb_t;

  node btb_wr(_(in, "branch"));
  bvec<BTB_SZ> btb_rd_idx(Hash<BTB_SZ>(pc, 0)),
               btb_wr_idx(Hash<BTB_SZ>(_(in, "branch_pc"), 0));
  btb_t btb_in,
        btb_out_pl(Syncmem(btb_rd_idx, Flatten(btb_in), btb_wr_idx, btb_wr)),
        btb_out(Latch(Reg(stall), Flatten(btb_out_pl)));

  _(btb_in, "valid") = Lit(1);
  _(btb_in, "next_pc") = _(in, "val");
  _(btb_in, "state") = _(in, "bp_state");

  // If we predicted a taken branch last cycle, give it a rest this cycle. The
  // execute stage depends on this in order to properly handle branch delay
  // slots.
  node bp_inhibit(Wreg(
    #ifdef STALL_SIGNAL
      !stall,
    #else
      Lit(1),
    #endif
      _(out_buf, "bp_valid") && _(out_buf, "bp_predict_taken")
  ));
  TAP(bp_inhibit);

  TAP(btb_in); TAP(btb_out); TAP(pc); TAP(clear_bf);
  #endif

  word_t next_pc;
  pc = Reg(next_pc, initial_pc);
  Cassign(next_pc).
    IF(_(in, "ldpc"), _(in, "val")).
    #ifdef STALL_SIGNAL
    IF(_(out_buf, "stall"), pc).
    #endif
    #ifdef BTB
    IF(_(out_buf, "bp_valid") && _(out_buf, "bp_predict_taken"),
      _(out_buf, "bp_pc")).
    #endif
    ELSE(pc + LitW(4));

  _(out, "inst") =
    LLRom<IROM_SZ, N>(Zext<IROM_SZ>(pc[range<CLOG2(N/8),N-1>()]), hex_file);
  _(out, "next_pc") = next_pc;
  _(out, "pc") = pc;

  _(out, "valid") = Lit(1);

  #ifdef STALL_SIGNAL
  out_buf = Wreg(!_(out_buf, "stall"), Flatten(out));
  _(out_buf, "valid") = Reg(_(out, "valid")) && !_(out_buf, "stall");
  #else
  out_buf = Reg(Flatten(out));
  #endif

  #ifdef BTB
  _(out_buf, "bp_valid") = _(out_buf, "bp_branch") && _(btb_out, "valid") &&
                           !Wreg(!stall, _(in, "ldpc")) && !bp_inhibit;
  _(out_buf, "bp_state") = _(btb_out, "state");
  _(out_buf, "bp_predict_taken") = _(btb_out, "state")[1];
  _(out_buf, "bp_pc") = _(btb_out, "next_pc");
  #endif

  HIERARCHY_EXIT();
}

void Decode(decode_reg_t &out, fetch_decode_t &in) {
  HIERARCHY_ENTER();
  inst_t inst(_(in, "inst"));
  opcode_t opcode(inst[range<N-6, N-1>()]);
  func_t func;
  Cassign(func).
    IF(opcode == Lit<6>(0x01) ||
       opcode == Lit<6>(0x06) ||
       opcode == Lit<6>(0x07), Zext<6>(inst[range<16, 20>()])).
    ELSE(inst[range<0, 5>()]);

  _(out, "rsrc0_idx") = inst[range<N-6-5, N-6-1>()];
  _(out, "rsrc1_idx") = inst[range<N-6-10, N-6-6>()];

  node dest_31,   // Destination register is r31 (jump/branch and link)
       dest_t,    // Destination register is "t" field of instruction
       imm_shift, // Instruction uses shift amount field as immediate
       sext_imm,  // Immediate must be sign extended
       j_inst;    // Instruction is j/jal  

  Cassign(_(out, "imm")).
    IF(j_inst,    Zext<N>(inst[range<0, 25>()])).
    IF(imm_shift, Zext<N>(inst[range<6, 10>()])).
    IF(sext_imm, Sext<N>(inst[range<0, 15>()])).
    ELSE(        Zext<N>(inst[range<0, 15>()]));

  j_inst = opcode == Lit<6>(0x02) || opcode == Lit<6>(0x03);

  sext_imm =
    opcode == Lit<6>(0x01) || // branch
    opcode == Lit<6>(0x04) || // beq
    opcode == Lit<6>(0x05) || // bne
    opcode == Lit<6>(0x08) || // addi
    opcode == Lit<6>(0x09) || // addiu
    opcode == Lit<6>(0x20) || // lb
    opcode == Lit<6>(0x23) || // lw
    opcode == Lit<6>(0x28) || // sb
    opcode == Lit<6>(0x2b);  //sw


  imm_shift =
    opcode == Lit<6>(0x00) && (
      func == Lit<6>(0x00) ||
      func == Lit<6>(0x02) ||
      func == Lit<6>(0x03));

  _(out, "imm_valid") = _(in, "valid") && (
      imm_shift ||
      opcode == Lit<6>(0x08) || // addi
      opcode == Lit<6>(0x09) || // addiu
      opcode == Lit<6>(0x0a) || // slti
      opcode == Lit<6>(0x0b) || // sltiu
      opcode == Lit<6>(0x0c) || // andi
      opcode == Lit<6>(0x0d) || // ori
      opcode == Lit<6>(0x0e) || // xori
      opcode == Lit<6>(0x20) || // lb
      opcode == Lit<6>(0x23) || // lw
      opcode == Lit<6>(0x28) || // sb
      opcode == Lit<6>(0x2b));  // sw

  Cassign(_(out, "rdest_idx")).
    IF(dest_t, inst[range<N-6-10, N-6-6>()]).
    IF(dest_31, Lit<5>(31)).
    ELSE(inst[range<N-6-15, N-6-11>()]);

  _(out, "rsrc0_valid") =
    _(in, "valid") && (
      (opcode == Lit<6>(0x00) && // ALU (add, sub, and, or, xor, ...)
        func != Lit<6>(0x00) &&  // sll    (These shifts are by an immediate
        func != Lit<6>(0x02) &&  // srl     amount and have no register src 0)
        func != Lit<6>(0x03)) || // sra
      opcode == Lit<6>(0x01) || // branch/branch-and-link
      opcode == Lit<6>(0x04) || // beq
      opcode == Lit<6>(0x05) || // bne
      opcode == Lit<6>(0x06) || // blez
      opcode == Lit<6>(0x07) || // bgtz
      opcode == Lit<6>(0x08) || // addi
      opcode == Lit<6>(0x09) || // addiu
      opcode == Lit<6>(0x0a) || // slti
      opcode == Lit<6>(0x0b) || // sltiu
      opcode == Lit<6>(0x0c) || // andi
      opcode == Lit<6>(0x0d) || // ori
      opcode == Lit<6>(0x0e) || // xori
      opcode == Lit<6>(0x20) || // lb
      opcode == Lit<6>(0x23) || // lw
      opcode == Lit<6>(0x28) || // sb
      opcode == Lit<6>(0x2b) ); // sw

  _(out, "rsrc1_valid") =
    _(in, "valid") && (
      opcode == Lit<6>(0x00) || // ALU
      opcode == Lit<6>(0x04) || // beq
      opcode == Lit<6>(0x05) || // bne
      opcode == Lit<6>(0x28) || // sb
      opcode == Lit<6>(0x2b) );   // sw

  _(out, "jal") =
    opcode == Lit<6>(0x03) ||    // jal
    opcode == Lit<6>(0x01) && (
      func == Lit<6>(0x10) ||    // bltzal
      func == Lit<6>(0x11));     // begzal
  dest_31 = _(out, "jal");

  dest_t =
    opcode == Lit<6>(0x08) || // addi
    opcode == Lit<6>(0x09) || // addiu
    opcode == Lit<6>(0x0a) || // slti
    opcode == Lit<6>(0x0b) || // sltiu
    opcode == Lit<6>(0x0c) || // andi
    opcode == Lit<6>(0x0d) || // ori
    opcode == Lit<6>(0x0e) || // xor
    opcode == Lit<6>(0x0f) || // lui
    opcode == Lit<6>(0x20) || // lb
    opcode == Lit<6>(0x23);   // lw

  _(out, "rdest_valid") =
    _(in, "valid") && (
      opcode == Lit<6>(0x00) && // ALU
        func != Lit<6>(0x08) && // jr
        func != Lit<6>(0x18) && // mult
        func != Lit<6>(0x19) && // multu
        func != Lit<6>(0x1a) && // div
        func != Lit<6>(0x1b)    // divu
      || dest_t || dest_31); // All of the instructions listed above

  _(out, "mem_rd") =
    _(in, "valid") && (
      opcode == Lit<6>(0x20) || // lb
      opcode == Lit<6>(0x23) ); // lw

   _(out, "mem_wr") =
    _(in, "valid") && (
      opcode == Lit<6>(0x28) || // sb
      opcode == Lit<6>(0x2b) ); // sw

  _(out, "mem_byte") =
    _(in, "valid") && (
      opcode == Lit<6>(0x28) || // sb
      opcode == Lit<6>(0x20) ); // lb

  _(out, "op") = opcode;
  _(out, "func") = func;
  _(out, "pc") = _(in, "pc");
  _(out, "next_pc") = _(in, "next_pc");
  _(out, "valid") = _(in, "valid");

  #ifdef BTB
  _(out, "bp_valid") = _(in, "bp_valid");
  _(out, "bp_state") = _(in, "bp_state");
  _(out, "bp_branch") = _(in, "bp_branch");
  _(out, "bp_predict_taken") = _(in, "bp_predict_taken");
  _(out, "bp_pc") = _(in, "bp_pc");
  #endif

  #ifdef STALL_SIGNAL
  _(in, "stall") = _(out, "stall");
  #endif

  HIERARCHY_EXIT();
}

void Reg(reg_exec_t &out_buf, decode_reg_t &in, mem_reg_t &in_wb) {
  HIERARCHY_ENTER();
  reg_exec_t out;

  // These are expensive to implement as Regs instead of as a multi-port SRAM,
  // but this enables constant inspection of all register values.
  bvec<32> wr(Decoder(_(in_wb, "rdest_idx"), _(in_wb, "rdest_valid")));
  vec<32, word_t> regs;
  for (unsigned i = 0; i < 32; ++i)
    regs[i] = Wreg(wr[i], _(in_wb, "result"));
  TAP(regs);

  Cassign(_(out, "val0")).
    IF(_(in, "rsrc0_idx") == _(in_wb, "rdest_idx") && _(in_wb, "rdest_valid"),
      _(in_wb, "result")).
    ELSE(Mux(_(in, "rsrc0_idx"), regs));

  Cassign(_(out, "val1")).
    IF(_(in, "rsrc1_idx") == _(in_wb, "rdest_idx") && _(in_wb, "rdest_valid"),
      _(in_wb, "result")).
    ELSE(Mux(_(in, "rsrc1_idx"), regs));

  _(out, "pc") = _(in, "pc");
  _(out, "next_pc") = _(in, "next_pc");
  _(out, "rdest_idx") = _(in, "rdest_idx");
  _(out, "rdest_valid") = _(in, "rdest_valid");
  _(out, "rsrc0_idx") = _(in, "rsrc0_idx");
  _(out, "rsrc0_valid") = _(in, "rsrc0_valid");
  _(out, "rsrc1_idx") = _(in, "rsrc1_idx");
  _(out, "rsrc1_valid") = _(in, "rsrc1_valid");
  _(out, "mem_rd") = _(in, "mem_rd");
  _(out, "mem_wr") = _(in, "mem_wr");
  _(out, "op") = _(in, "op");
  _(out, "func") = _(in, "func");
  _(out, "valid") = _(in, "valid");
  _(out, "jal") = _(in, "jal");
  _(out, "mem_byte") = _(in, "mem_byte");
  _(out, "imm") = _(in, "imm");
  _(out, "imm_valid") = _(in, "imm_valid");

  #ifdef STALL_SIGNAL
  _(in, "stall") = _(out_buf, "stall");
  out_buf = Wreg(!_(out_buf, "stall"), Flatten(out));
  _(out_buf, "valid") = Wreg(!_(in,"stall"), _(out,"valid")) && !_(in,"stall");
  #else
  out_buf = Reg(Flatten(out));
  #endif

  #ifdef BTB
  _(out, "bp_valid") = _(in, "bp_valid");
  _(out, "bp_state") = _(in, "bp_state");
  _(out, "bp_branch") = _(in, "bp_branch");
  _(out, "bp_predict_taken") = _(in, "bp_predict_taken");
  _(out, "bp_pc") = _(in, "bp_pc");
  #endif

  HIERARCHY_EXIT();
}

void Exec(exec_mem_t &out_buf, exec_fetch_t &out_pc, reg_exec_t &in,
          mem_exec_t &mem_fwd)
{
  HIERARCHY_ENTER();

  node in_valid;
  exec_mem_t out;

  #ifdef RANDOM_STALL
  // Test stall signal by generating random stalls.
  node random_stall = Lfsr<1, 15, 1, 0x1234>()[0] || _(out_buf, "stall");
  TAP(random_stall);
  #endif

  opcode_t op(_(in, "op"));
  func_t func(_(in, "func"));

  #ifdef STALL_SIGNAL  
  node gen_stall, stall(_(in, "stall"));
  #endif

  word_t actual_next_pc, pc(_(in, "pc")), next_pc(_(in, "next_pc")),
         val0, val0_in, val1, val1_in, imm(_(in, "imm")),
         bdest(pc + LitW(N/8) + imm * LitW(N/8)),
         jdest(Cat(pc[range<N-4, N-1>()], Zext<N-4>(imm * LitW(N/8))));

  node wr_val_0, wr_val_1,
       rdest_valid_1(_(out_buf, "rdest_valid")),
       rdest_valid_2(_(mem_fwd, "rdest_valid")),
       mem_rd_1(_(out_buf, "mem_rd")),
       mem_rd_2(_(mem_fwd, "mem_rd"));
  rname_t rdest_idx_1(_(out_buf, "rdest_idx")),
          rdest_idx_2(_(mem_fwd, "rdest_idx"));
  word_t result_1(_(out_buf, "result")),
         result_2(_(mem_fwd, "result"));

  node fwd_1_to_0(_(in, "rsrc0_valid") && rdest_valid_1 && !mem_rd_1 &&
                    rdest_idx_1 == _(in, "rsrc0_idx")),
       fwd_2_to_0(_(in, "rsrc0_valid") && rdest_valid_2 &&
                    rdest_idx_2 == _(in, "rsrc0_idx")),
       fwd_1_to_1(_(in, "rsrc1_valid") && rdest_valid_1 && !mem_rd_1 &&
                    rdest_idx_1 == _(in, "rsrc1_idx")),
       fwd_2_to_1(_(in, "rsrc1_valid") && rdest_valid_2 &&
                    rdest_idx_2 == _(in, "rsrc1_idx"));

  TAP(in_valid);

  Cassign(val0_in).
    IF(fwd_1_to_0, _(out_buf, "result")).
    IF(fwd_2_to_0, _(mem_fwd, "result")).
    ELSE(_(in, "val0"));

  Cassign(val1_in).
    IF(fwd_1_to_1, _(out_buf, "result")).
    IF(fwd_2_to_1, _(mem_fwd, "result")).
    ELSE(_(in, "val1"));

  #ifdef STALL_SIGNAL
  wr_val_0 = ((stall||Reg(stall)) && (fwd_1_to_0 || fwd_2_to_0)) || !Reg(stall);
  wr_val_1 = ((stall||Reg(stall)) && (fwd_1_to_1 || fwd_2_to_1)) || !Reg(stall);
  #else
  wr_val_0 = Lit(1);
  wr_val_1 = Lit(1);
  #endif

  val0 = Latch(!wr_val_0, val0_in);
  val1 = Latch(!wr_val_1, val1_in);

  TAP(fwd_1_to_0); TAP(fwd_2_to_0); TAP(fwd_1_to_1); TAP(fwd_2_to_1);
  TAP(val0); TAP(val1); TAP(val0_in); TAP(val1_in);
  TAP(wr_val_0); TAP(wr_val_1);

  Cassign(actual_next_pc).
    IF(op == Lit<6>(0x02) || op == Lit<6>(0x03), jdest). // j, jal
    IF(op == Lit<6>(0x00) && func == Lit<6>(0x08), val0). // jr
    IF(op == Lit<6>(0x04) && val0 == val1, bdest). // beq
    IF(op == Lit<6>(0x05) && val0 != val1, bdest). // bne
    IF(op == Lit<6>(0x01)).
      IF(func == Lit<6>(0x01) && !val0[N-1], bdest). // bgez
      IF(func == Lit<6>(0x10) && val0[N-1], bdest). // bltzal
      IF(func == Lit<6>(0x11) && !val0[N-1], bdest). // bgezal
      ELSE(pc + LitW(4)).
    END().IF(op == Lit<6>(0x06)).
      IF(func == Lit<6>(0x00) && (!OrN(val0) || val0[N-1]), bdest). // blez
      ELSE(pc + LitW(4)).
    END().IF(op == Lit<6>(0x07)).
      IF(func == Lit<6>(0x00) && OrN(val0) && !val0[N-1], bdest). // bgtz
      ELSE(pc + LitW(4)).
    END().
    ELSE(pc + LitW(4));

  // When a branch is mispredicted, the next two inputs are to be considered
  // invalid.
  bvec<2> next_bubble_ctr, bubble_ctr(Reg(next_bubble_ctr));
  node bubble;
  #ifdef DELAYED_BRANCH
  bubble = (bubble_ctr == Lit<2>(1));
  #else
  bubble = OrN(bubble_ctr);
  #endif
  in_valid = _(in, "valid") && !bubble;

  node branch_mispredict(in_valid && next_pc != actual_next_pc),
       branch_taken(in_valid && (_(in, "pc")+LitW(4)) != actual_next_pc);
  TAP(branch_mispredict);
  TAP(branch_taken);

  #ifdef BTB
    // Identify this instruction as a branch for the branch predictor
    _(out_pc, "branch") =
      #ifdef STALL_SIGNAL
      !stall &&
      #endif
      !bubble &&
      ((op == Lit<6>(0x02) || op == Lit<6>(0x03)) ||    // j, jal
       (op == Lit<6>(0x00) && func == Lit<6>(0x08)) ||  // jr
       (op == Lit<6>(0x04) || op == Lit<6>(0x05)) ||    // beq, bne
       (op == Lit<6>(0x01) && func == Lit<6>(0x01)) ||  // bgez
       (op == Lit<6>(0x01) && func == Lit<6>(0x10)) ||  // bltzal
       (op == Lit<6>(0x01) && func == Lit<6>(0x11)) ||  // bgezal
       (op == Lit<6>(0x06) && func == Lit<6>(0x00)) ||  // blez
       (op == Lit<6>(0x07) && func == Lit<6>(0x00)));   // bgtz

  _(out_pc, "bp_branch") = 
    !bubble &&
    #ifdef STALL_SIGNAL
    !stall &&
    #endif
    _(in, "bp_branch");
  _(out_pc, "branch_pc") = _(in, "pc");

  // The branch predictor state machine "saturating counter"
  Cassign(_(out_pc, "bp_state")).
    IF(_(in, "bp_valid")).
      IF(_(in, "bp_state") == Lit<2>(0)).
        IF(branch_taken, Lit<2>(1)). 
        ELSE(Lit<2>(0)).
      END().
      IF(_(in, "bp_state") == Lit<2>(1)).
        IF(branch_taken, Lit<2>(2)). 
        ELSE(Lit<2>(0)).
      END().
      IF(_(in, "bp_state") == Lit<2>(2)).
        IF(branch_taken, Lit<2>(3)). 
        ELSE(Lit<2>(1)).
      END().
      IF(_(in, "bp_state") == Lit<2>(3)).
        IF(branch_taken, Lit<2>(3)). 
        ELSE(Lit<2>(2)).
      END().
    END().
    ELSE(Lit<2>(1));

  // TODO: rename the original branch_mispredict
  node bp_failure_to_predict(!_(in, "bp_valid") && branch_taken),
       bp_false_positive(_(in, "bp_valid") && _(in, "bp_predict_taken")
         && !_(out_pc, "branch")
         #ifdef STALL_SIGNAL
         && !stall
         #endif
       ),
       bp_mispredict_t(_(in, "bp_valid") && _(in, "bp_predict_taken")
         && !branch_taken && _(out_pc, "branch") && in_valid
         #ifdef STALL_SIGNAL
         && !stall
         #endif
       ),
       bp_mispredict_nt(_(in, "bp_valid") && !_(in,"bp_predict_taken")
         && branch_taken && _(out_pc, "branch") && in_valid
         #ifdef STALL_SIGNAL
         && !stall
         #endif
       ),
       bp_wrong_target(_(in, "bp_valid") && actual_next_pc != _(in, "bp_pc") &&
                         !bp_mispredict_t && !bp_mispredict_nt && in_valid &&
		       _(in, "bp_branch") && _(out_pc, "branch") &&
                       _(in, "bp_predict_taken")
         #ifdef STALL_SIGNAL
         && !stall
         #endif
       );

  Counter("failure_to_predict", bp_failure_to_predict);
  Counter("mispredict_t", bp_mispredict_t);
  Counter("mispredict_nt", bp_mispredict_nt);
  Counter("bp_wrong_target", bp_wrong_target);
  Counter("branches", _(out_pc, "branch")); 

  TAP(bp_false_positive);
  #endif

  Counter("cycles", Lit(1));
  Counter("instructions", in_valid && !bubble
  #ifdef STALL_SIGNAL
    && !_(in, "stall")
  #endif
  );

  Cassign(next_bubble_ctr).
  #ifdef STALL_SIGNAL
    IF(_(in, "stall"), bubble_ctr).
  #endif
  #ifdef BTB
    IF(bubble_ctr == Lit<2>(0) &&
        (bp_mispredict_t || bp_mispredict_nt || bp_wrong_target ||
           bp_failure_to_predict || bp_false_positive),
          Lit<2>(2)).
  #else
    IF(branch_mispredict, Lit<2>(2)).
  #endif
    IF(bubble_ctr == Lit<2>(0), Lit<2>(0)).
    ELSE(bubble_ctr - Lit<2>(1));    

  TAP(bubble); TAP(bubble_ctr);

  // Compute address for memory accesses
  _(out, "addr") = val0 + _(in, "imm");
  _(out, "mem_rd") = _(in, "mem_rd") && in_valid;
  _(out, "mem_wr") = _(in, "mem_wr") && in_valid;
  _(out, "mem_byte") = _(in, "mem_byte");

  _(out, "rdest_idx") = _(in, "rdest_idx");
  _(out, "rdest_valid") = _(in, "rdest_valid") && in_valid;

  
  #ifdef BTB
  // If we corrected the PC last instruction, this false positive only affects
  // the bubble.
  node prev_ldpc(Wreg(
    #ifdef STALL_SIGNAL
    !stall,
    #else
    Lit(1),
    #endif
    /*bp_failure_to_predict || bp_mispredict_nt*/_(out_pc, "ldpc")
  ));

  _(out_pc, "ldpc") = bp_mispredict_t || bp_mispredict_nt ||
                      bp_failure_to_predict || bp_wrong_target ||
                      (bp_false_positive && !prev_ldpc);
  Cassign(_(out_pc, "val")).
    IF(bp_mispredict_t || bp_false_positive, _(in, "pc") + LitW(2*N/8)).
    ELSE(actual_next_pc);
  #else
  _(out_pc, "ldpc") = branch_mispredict;
  _(out_pc, "val") = actual_next_pc;
  #endif

  #ifdef MUL_DIV
  word_t hi, lo;
  #endif

  Cassign(_(out, "result")).
    IF(_(in, "jal"), pc + LitW(2*N/8)). // jump/branch and link
    IF(_(in, "mem_wr"), val1). // Store
    IF(op == Lit<6>(0)).
      IF(func == Lit<6>(0x00), val1 << Zext<CLOG2(N)>(imm)). // sll
      IF(func == Lit<6>(0x02), val1 >> Zext<CLOG2(N)>(imm)). // srl
      IF(func == Lit<6>(0x03),                               // sra
        Shifter(val1, Zext<CLOG2(N)>(imm), Lit(1), Lit(0), Lit(1))).
      IF(func == Lit<6>(0x04), val1 << Zext<CLOG2(N)>(val0)). // sllv
      IF(func == Lit<6>(0x06), val1 >> Zext<CLOG2(N)>(val0)). // srlv
      IF(func == Lit<6>(0x07),                                // srav
        Shifter(val1, Zext<CLOG2(N)>(val0), Lit(1), Lit(0), Lit(1))).
      #ifdef MUL_DIV
      IF(func == Lit<6>(0x10), hi).
      IF(func == Lit<6>(0x12), lo).
      #endif
      IF(func == Lit<6>(0x20), val0 + val1). // add
      IF(func == Lit<6>(0x21), val0 + val1). // addu
      IF(func == Lit<6>(0x22), val0 - val1). // sub
      IF(func == Lit<6>(0x23), val0 - val1). // subu
      IF(func == Lit<6>(0x24), val0 & val1). // and
      IF(func == Lit<6>(0x25), val0 | val1). // or
      IF(func == Lit<6>(0x26), val0 ^ val1). // xor
      IF(func == Lit<6>(0x2a), Cat(Lit<N-1>(0), (val0 - val1)[N-1])). // slt
      IF(func == Lit<6>(0x2b),                                 // sltu
        Cat(Lit<N-1>(0), Zext<N+1>(val0) < Zext<N+1>(val1))).
    END().
    IF(op == Lit<6>(0x08), val0 + imm). // addi
    IF(op == Lit<6>(0x09), val0 + imm). // addiu
    IF(op == Lit<6>(0x0a), Cat(Lit<N-1>(0), (val0 - imm)[N-1])). // slti
    IF(op == Lit<6>(0x0b),                                // sltiu
      Cat(Lit<N-1>(0), Zext<N+1>(val0) < Zext<N+1>(imm))).
    IF(op == Lit<6>(0x0c), val0 & imm). // andi
    IF(op == Lit<6>(0x0d), val0 | imm). // ori
    IF(op == Lit<6>(0x0e), val0 ^ imm). // xori
    IF(op == Lit<6>(0x0f), Cat(Zext<16>(imm), Lit<N-16>(0))). // lui
    ELSE(LitW(0));

  _(out, "pc") = pc; // For debugging purposes.

  #ifdef STALL_SIGNAL
  out_buf = Wreg(!stall, Flatten(out));
  _(out_buf, "mem_rd") = Wreg(!stall, _(out, "mem_rd")) && !gen_stall;
  _(out_buf, "mem_wr") = Wreg(!stall, _(out, "mem_wr")) && !gen_stall;
  _(out_buf, "rdest_valid") = Wreg(!stall, _(out, "rdest_valid")) && !gen_stall;
  #else
  out_buf = Reg(Flatten(out));
  #endif

  #ifdef MUL_DIV
  node mfhi(op == Lit<6>(0x00) && func == Lit<6>(0x10)),
       mflo(op == Lit<6>(0x00) && func == Lit<6>(0x12)),
       mult(op == Lit<6>(0x00) && func == Lit<6>(0x18) && in_valid),
       multu(op == Lit<6>(0x00) && func == Lit<6>(0x19) && in_valid),
       div(op == Lit<6>(0x00) && func == Lit<6>(0x1a) && in_valid),
       divu(op == Lit<6>(0x00) && func == Lit<6>(0x1b) && in_valid),
       mul_busy, start_mul(mult || multu), div_ready, div_busy, div_waiting,
       start_div(div || divu), next_div_sel, div_sel(Reg(next_div_sel));
  bvec<2*N> mul_out, mul_a(Cat(bvec<N>(mult && val0[N-1]), val0)),
            mul_b(Cat(bvec<N>(mult && val0[N-1]), val1));
  word_t div_out, div_rem;

  TAP(hi); TAP(lo); TAP(mult); TAP(multu); TAP(mfhi); TAP(mflo);
  TAP(div); TAP(divu); TAP(div_out); TAP(div_rem); TAP(div_busy);
  TAP(div_sel); TAP(next_div_sel);

  div_busy = !div_ready && !div_waiting;

  Cassign(next_div_sel).
    IF(start_mul, Lit(0)).
    IF(start_div, Lit(1)).
    ELSE(div_sel);

  mul_out = SerialMul(mul_busy, mul_a, mul_b, start_mul);
  SerialDiv(div_out, div_rem, div_ready, div_waiting,
            val0, val1, start_div, Lit(0));

  hi = Mux(div_sel, mul_out[range<N, 2*N-1>()], div_rem);
  lo = Mux(div_sel, mul_out[range<0, N-1>()], div_out);

  node muldiv_stall = (mfhi || mflo) && mul_busy || div_busy;

  TAP(mul_out); TAP(mul_a); TAP(mul_b); TAP(start_mul);
  #endif

  #ifdef SCOREBOARD
  node set_scoreboard(_(mem_fwd, "rdest_valid") && _(mem_fwd, "mem_rd")),
       clear_scoreboard(_(out, "mem_rd") && in_valid),
       check_scoreboard_0(_(in, "rsrc0_valid")),
       check_scoreboard_1(_(in, "rsrc1_valid")),
      check_scoreboard_dest(_(in, "rdest_valid"));
  rname_t set_scoreboard_idx(_(mem_fwd, "rdest_idx")),
          clear_scoreboard_idx(_(in, "rdest_idx")),
          check_scoreboard_0_idx(_(in, "rsrc0_idx")),
          check_scoreboard_1_idx(_(in, "rsrc1_idx")),
          check_scoreboard_dest_idx(_(in, "rdest_idx"));

  bvec<32> next_scoreboard, cur_scoreboard, scoreboard(cur_scoreboard);
  for (unsigned i = 0; i < 32; ++i) {
    node clear(clear_scoreboard && clear_scoreboard_idx == Lit<5>(i)),
         set(set_scoreboard && set_scoreboard_idx == Lit<5>(i));
    Cassign(next_scoreboard[i]).
      IF(clear, Lit(0)).
      IF(set, Lit(1)).
      ELSE(cur_scoreboard[i]);
  }
  cur_scoreboard = Reg(next_scoreboard, 0xffffffffu);

  node stall_scoreboard_0(check_scoreboard_0 &&
                            !Mux(check_scoreboard_0_idx, scoreboard)),
       stall_scoreboard_1(check_scoreboard_1 &&
                            !Mux(check_scoreboard_1_idx, scoreboard)),
       stall_scoreboard_dest(check_scoreboard_dest && 
                            !Mux(check_scoreboard_dest_idx, scoreboard));

  TAP(cur_scoreboard);
  TAP(next_scoreboard);
  TAP(scoreboard);
  TAP(set_scoreboard);
  TAP(set_scoreboard_idx);
  TAP(clear_scoreboard);
  TAP(clear_scoreboard_idx);
  TAP(check_scoreboard_0);
  TAP(check_scoreboard_0_idx);
  TAP(stall_scoreboard_0);
  TAP(check_scoreboard_1);
  TAP(check_scoreboard_1_idx);
  TAP(stall_scoreboard_1);
  #endif

  #ifdef STALL_SIGNAL
  gen_stall = 
    #ifdef MUL_DIV
    muldiv_stall ||
    #endif
    #ifdef RANDOM_STALL
    random_stall ||
    #endif
    #ifdef SCOREBOARD
    stall_scoreboard_0 || stall_scoreboard_1 || stall_scoreboard_dest ||
    #endif
    Lit(0);

  stall = _(out_buf, "stall") || gen_stall;
  #endif

  #ifdef SHOW_PC
  static unsigned long pcVal;

  EgressInt(pcVal, _(in, "pc"));

  EgressFunc([](bool x){
    if (x) cout << sim_time() << " PC> " << hex << pcVal << dec << endl;
  }, in_valid && !bubble
    #ifdef STALL_SIGNAL
    && !stall
    #endif
  );
  #endif

  HIERARCHY_EXIT();
}

// Scratchpad memory configuration
vec<N/8, bvec<8> > InternalMem(word_t a_in, vec<N/8, bvec<8> > d, bvec<N/8> wr,
                               const char *hex_file)
{
  const unsigned B(N/8), BB(CLOG2(N/8));

  word_t a(Reg(a_in));
  vec<B, bvec<8> > q;
  bvec<RAM_SZ> sram_addr(a_in[range<BB, BB + RAM_SZ - 1>()]);
  bvec<IROM_SZ> rom_addr(a_in[range<BB, BB + IROM_SZ - 1>()]);

  word_t irom_qw(Reg(LLRom<IROM_SZ, N>(rom_addr, hex_file)));
  vec<B, bvec<8> > irom_q;
  for (unsigned i = 0; i < B; ++i)
    for (unsigned j = 0; j < 8; ++j)
      irom_q[i][j] = irom_qw[i*8 + j];

  for (unsigned i = 0; i < N/8; ++i) {
    Cassign(q[i]).
      #ifdef MAP_ROM_COPY
      IF(a >= LitW(0x400000) && a < LitW(0x500000), irom_q[i]).
      #endif
      ELSE(Syncmem(sram_addr, d[i], wr[i]));
  }

  TAP(a);
  TAP(q);
  TAP(irom_q);

  return q;
}

#ifdef SST_MEM
void SimpleMemSSTRam(node &stall, simpleMemResp_t &resp, simpleMemReq_t &req) {
  simpleMemReq_t memSysReq;

  word_t addr(_(_(req, "contents"), "addr"));

  _(_(memSysReq, "contents"), "wr") = _(_(req, "contents"), "wr");
  _(_(memSysReq, "contents"), "addr") = _(_(req, "contents"), "addr");
  _(_(memSysReq, "contents"), "size") = _(_(req, "contents"), "size");
  _(_(memSysReq, "contents"), "data") = _(_(req, "contents"), "data");
  _(_(memSysReq, "contents"), "id") = _(_(req, "contents"), "id");
  _(memSysReq, "valid") = _(req, "valid") &&
    (addr < LitW(0x400000) || addr >= LitW(0x500000));
    
  SimpleMemReqPort("0", memSysReq);
  SimpleMemRespPort("0", resp);
}

void SimpleMemRom(node &stall, simpleMemResp_t &resp, simpleMemReq_t &req,
                  const char* hex_file)
{
  word_t addr(_(_(req, "contents"), "addr"));
  bvec<IROM_SZ> rom_addr(addr[range<CLOG2(N/8),CLOG2(N/8)+IROM_SZ-1>()]);

  node valid = _(req, "valid") &&
    (addr >= LitW(0x400000) && addr < LitW(0x500000));

  node fill, empty, next_full, full(Reg(next_full));
  Cassign(next_full).
    IF(!full && fill && !empty, Lit(1)).
    IF(full && empty && !fill, Lit(0)).
    ELSE(full);

  fill = valid;
  empty = full && _(resp, "ready");

  stall = full && !_(resp, "ready");
  _(resp, "valid") = full;
  _(_(resp, "contents"), "data") =
    Wreg(fill, LLRom<IROM_SZ, N>(rom_addr, hex_file) >> 
      Zext<CLOG2(N)>(Cat(addr[range<0,CLOG2(N/8)-1>()], Lit<3>(0)))
  );
  _(_(resp, "contents"), "id") = Wreg(fill, _(_(req, "contents"), "id"));
}

void SimpleMem(node &stall, simpleMemResp_t &resp, simpleMemReq_t &req,
               const char *hex_file)
{
  bvec<2> stallVec;
  vec<2, simpleMemResp_t> respVec;

  _(req, "ready") = Lit(1);

  #ifdef SST_MEM
  SimpleMemSSTRam(stallVec[0], respVec[0], req);
  #endif

  #ifdef MAP_ROM_COPY
  SimpleMemRom(stallVec[1], respVec[1], req, hex_file);
  #endif

  Arbiter(resp, ArbPriority<2>, respVec);
  stall = OrN(stallVec);
}
#endif

void Mem(mem_reg_t &out, mem_exec_t &fwd, exec_mem_t &in,
         const char *hex_file, bool &stop_sim) {
  HIERARCHY_ENTER();

  const unsigned B(N/8), BB(CLOG2(B));

  #ifdef STALL_SIGNAL
  _(in, "stall") = Lit(0);
  #endif
  vec<B, bvec<8> > memq, memd;
  bvec<BB> bytesel(_(in, "addr")[range<0, BB - 1>()]);

  #ifdef INTERNAL_MEM
  bvec<B> wr(Decoder(bytesel, _(in, "mem_wr")) |
            bvec<B>(_(in, "mem_wr") && !_(in, "mem_byte")));

  // Break input into bytes
  for (unsigned i = 0; i < B; ++i)
    for (unsigned j = 0; j < 8; ++j)
      Cassign(memd[i][j]).
        IF(_(in, "mem_byte"), _(in, "result")[j]).
        ELSE(_(in, "result")[i*8 + j]);

  // Instantiate the SRAMs
  memq = InternalMem(_(in, "addr"), memd, wr, hex_file);

  // Combine output bytes into single word
  word_t memq_word;
  for (unsigned i = 0; i < B; ++i)
    for (unsigned j = 0; j < 8; ++j)
      memq_word[i*8 + j] = memq[i][j];
  #endif

  #ifdef MSHR
  typedef ag<STP("valid"), node,
          ag<STP("rdest"), rname_t,
          ag<STP("byte"), node,
          ag<STP("bytesel"), bvec<BB> > > > > mshr_entry_t;

  mshr_entry_t mshr_in, mshr_out;
  _(mshr_in, "valid") = _(in, "mem_rd")
  #ifdef STALL_SIGNAL
    && !_(in, "stall")
  #endif
  ;
  _(mshr_in, "rdest") = _(in, "rdest_idx");
  _(mshr_in, "byte") = _(in, "mem_byte");
  _(mshr_in, "bytesel") = bytesel;

  bvec<MSHR_SZ> next_mshr_occupied, mshr_occupied(Reg(next_mshr_occupied));
  bvec<CLOG2(MSHR_SZ)> mshr_tag(Log2(~mshr_occupied)), mem_resp_tag;
  node mem_resp_valid;
  for (unsigned i = 0; i < MSHR_SZ; ++i) {
    Cassign(next_mshr_occupied[i]).
      IF(_(mshr_in, "valid") && mshr_tag == Lit<CLOG2(MSHR_SZ)>(i), Lit(1)).
      IF(mem_resp_tag == Lit<CLOG2(MSHR_SZ)>(i) && mem_resp_valid, Lit(0)).
      ELSE(mshr_occupied[i]);
  }

  node mshr_full(AndN(mshr_occupied));

  mshr_out = Syncmem(mem_resp_tag,
                     Flatten(mshr_in), mshr_tag,
                     _(mshr_in, "valid") && !mshr_full);

  TAP(mshr_in); TAP(mshr_out); TAP(mshr_tag); TAP(mshr_occupied);
  TAP(mshr_full); TAP(mem_resp_valid);

  #ifdef SST_MEM
  simpleMemReq_t sst_req;
  simpleMemResp_t sst_resp;

  _(sst_req, "valid") = (_(in, "mem_rd") || _(in, "mem_wr")) && !_(in, "stall");
  node sst_not_ready(
    (_(in, "mem_rd") || _(in, "mem_wr")) && !_(sst_req, "ready")
  );
  _(_(sst_req, "contents"), "wr") = _(in, "mem_wr");
  _(_(sst_req, "contents"), "addr") = Zext<ADDR_SZ>(_(in, "addr"));
  _(_(sst_req, "contents"), "size") =
    Zext<CLOG2(DATA_SZ/8 + 1)>(Mux(_(in, "mem_byte"), LitW(N/8), LitW(1)));
  _(_(sst_req, "contents"), "data") = Zext<DATA_SZ>(_(in, "result"));
  _(_(sst_req, "contents"), "uncached") = Lit(0);
  _(_(sst_req, "contents"), "llsc") = Lit(0);
  _(_(sst_req, "contents"), "locked") = Lit(0);
  _(_(sst_req, "contents"), "id") = Zext<ID_SZ>(mshr_tag); 

  _(sst_resp, "ready") = Lit(1);
  mem_resp_valid = _(sst_resp, "valid") && !_(_(sst_resp, "contents"), "wr");
  mem_resp_tag = Zext<CLOG2(MSHR_SZ)>(_(_(sst_resp, "contents"), "id"));

  node mem_stall;
  SimpleMem(mem_stall, sst_resp, sst_req, hex_file);

  TAP(sst_req);
  TAP(sst_resp);

  #endif

  #endif

  // Copies of the input and output destinations and values for forwarding
  _(fwd, "rdest_idx") = _(out, "rdest_idx");
  _(fwd, "rdest_valid") = _(out, "rdest_valid");
  _(fwd, "result") = _(out, "result");
  _(fwd, "mem_rd") =
  #ifdef INTERNAL_MEM
    Reg(_(in, "mem_rd"));
  #endif
  #ifdef SST_MEM
    Reg(mem_resp_valid);
  #endif

  // The destination register signals
  _(out, "rdest_idx") =
  #ifdef INTERNAL_MEM
    Reg(_(in, "rdest_idx"));
  #endif
  #ifdef SST_MEM
    Mux(Reg(mem_resp_valid), Reg(_(in, "rdest_idx")), _(mshr_out, "rdest"));
  #endif
  _(out, "rdest_valid") = 
  #ifdef INTERNAL_MEM
    Reg(_(in, "rdest_valid"));
  #endif
  #ifdef SST_MEM
    Wreg(!_(in, "stall"), _(in, "rdest_valid") && !_(in, "mem_rd")) ||
      Reg(mem_resp_valid);
  #endif

  // The final result: memory (word), memory(byte), or ALU
  Cassign(_(out, "result")).
    #ifdef INTERNAL_MEM
    IF(Reg(_(in, "mem_rd"))).
      IF(Reg(_(in, "mem_byte")), Zext<N>(Mux(Reg(bytesel), memq))).
      ELSE(memq_word).
    END().
    #endif
    #ifdef SST_MEM
    IF(Reg(mem_resp_valid)).
      IF(_(mshr_out, "byte"),
        Zext<N>(Zext<8>(Reg(_(_(sst_resp, "contents"), "data"))))).
        ELSE(Zext<N>(Reg(_(_(sst_resp, "contents"), "data")))).
    END().
    #endif
    #ifdef STALL_SIGNAL
    ELSE(Wreg(!_(in, "stall"), _(in, "result")));
    #else
    ELSE(Reg(_(in, "result")));
    #endif

  if (SOFT_IO) {
    static unsigned consoleOutVal;
    EgressInt(consoleOutVal, _(in, "result"));
    node wrConsole(_(in, "mem_wr") && _(in, "addr") == LitW(1ul<<(N-1))),
      wrChar(_(in, "mem_wr") && _(in, "addr") == LitW((1ul<<N-1)+N/4)),
         stopSimNode(_(in, "mem_wr") && _(in, "addr") == LitW((1ul<<N-1)+N/8));

    EgressFunc([](bool x){
      if (x) cout << "OUTPUT> " << consoleOutVal << endl;
    }, wrConsole);

    EgressFunc([](bool x){
        static string linebuf;
        if (x) {
          if (consoleOutVal == '\n') {
            cout << "OUTPUT> " << linebuf << endl;
            linebuf = "";
          } else {
            linebuf = linebuf + char(consoleOutVal);
          }
	}
    }, wrChar);

    Egress(stop_sim, stopSimNode);
  }

  if (DEBUG_MEM) {
    static unsigned dVal, qVal, prevAddrVal, addrVal, pcVal, prevPcVal,
                    resultVal;
    EgressInt(prevAddrVal, Reg(_(in, "addr")));
    EgressInt(addrVal, _(in, "addr"));
    EgressInt(dVal, _(in, "result"));
    EgressInt(qVal, _(out, "result"));
    EgressInt(pcVal, _(in, "pc"));
    EgressInt(prevPcVal, Reg(_(in, "pc")));
    EgressInt(resultVal, _(out, "result"));

    EgressFunc([](bool x){
      if (sim_time() % 10 == 0) cout << "# " << sim_time() << endl;
      if (x) cout << "MEM WR> " << hex << pcVal << ", " << addrVal << '('
                  << dec << addrVal << "), " << dVal << endl;
    }, _(in, "mem_wr"));

    EgressFunc([](bool x){
      if (x) cout << "MEM RD> " << hex << prevPcVal << '(' << prevAddrVal
                  << "), " << dec << prevAddrVal << ", " << qVal << endl;
    }, Reg(_(in, "mem_rd")));

    #ifdef MSHR
    // Debugging stuff for MSHRs.
    static bool mem_resp_valid_val, mshr_in_valid_val;
    static unsigned mshr_occupied_val;
    EgressInt(mshr_occupied_val, mshr_occupied);
    Egress(mem_resp_valid_val, mem_resp_valid);
    Egress(mshr_in_valid_val, _(mshr_in, "valid"));
    EgressFunc([](bool x){
	if (mshr_in_valid_val)
          cout << "MSHR Write. Occupancy vector "
               << hex << mshr_occupied_val << endl;
        if (mem_resp_valid_val)
          cout << "MSHR resp." << endl;
    }, _(mshr_in, "valid") || mem_resp_valid);
    #endif

    #ifdef SST_MEM
    static bool sst_resp_wr_val;
    static unsigned sst_resp_data_val, sst_resp_tag_val;
    Egress(sst_resp_wr_val, _(_(sst_resp, "contents"), "wr"));
    EgressInt(sst_resp_data_val, _(_(sst_resp, "contents"), "data"));
    EgressInt(sst_resp_tag_val, _(_(sst_resp, "contents"), "id"));
    // Debugging stuff for SST memory
    EgressFunc([](bool x) {
      if (x) cout << "MSHR SST memory response, " << sst_resp_wr_val << ", "
                  << sst_resp_tag_val << ", " << sst_resp_data_val << endl;
    }, _(sst_resp, "valid"));
    #endif

    EgressFunc([](bool x) {
      if (x) cout << "RESULT> " << resultVal << endl;
    }, _(out, "rdest_valid"));
  }

  #ifdef STALL_SIGNAL
  _(in, "stall") = 
    #ifdef MSHR
    mshr_full ||
    #endif
    #ifdef SST_MEM
    sst_not_ready || _(in, "rdest_valid") && mem_resp_valid || mem_stall ||
    #endif
    Lit(0);
  #endif

  HIERARCHY_EXIT();
}
