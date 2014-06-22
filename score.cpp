#include <iostream>
#include <sstream>
#include <iomanip>

#include <chdl/chdl.h>
#include <chdl/egress.h>
#include <chdl/cassign.h>

#include "interfaces.h"

#ifdef MUL_DIV
#include "muldiv.h"
#endif

using namespace std;
using namespace chdl;
using namespace s_core;

void fetch(fetch_decode_t &out, exec_fetch_t &in,
           const char *hex_file, unsigned initial_pc);
void decode(decode_reg_t &out, fetch_decode_t &in);
void reg(reg_exec_t &out, decode_reg_t &in, mem_reg_t &in_wb);
void exec(exec_mem_t &out, exec_fetch_t &out_pc, reg_exec_t &in, mem_exec_t &f);
void mem(mem_reg_t &out, mem_exec_t &fwd, exec_mem_t &in, bool &stop_sim);

void simple_core(const char *hex_file, unsigned initial_pc, bool &stop_sim) {
  fetch_decode_t fetch_decode;
  decode_reg_t decode_reg;
  reg_exec_t reg_exec;
  exec_mem_t exec_mem;
  exec_fetch_t exec_fetch;
  mem_reg_t mem_reg;
  mem_exec_t mem_exec;

                                                         // Pipeline:
  fetch(fetch_decode, exec_fetch, hex_file, initial_pc); //   STAGE 1
  decode(decode_reg, fetch_decode);                      //   STAGE 2
  reg(reg_exec, decode_reg, mem_reg);                    //     STAGE 5
  exec(exec_mem, exec_fetch, reg_exec, mem_exec);        //   STAGE 3
  mem(mem_reg, mem_exec, exec_mem, stop_sim);            //   STAGE 4

  TAP(fetch_decode);
  TAP(decode_reg);
  TAP(reg_exec);
  TAP(exec_mem);
  TAP(exec_fetch);
  TAP(mem_exec);
  TAP(mem_reg);
}

int main(int argc, char** argv) {
  bool stop_sim;

  unsigned initial_pc(0x400000);
  if (argc >= 3) {
    istringstream iss(argv[2]);
    iss >> hex >> initial_pc;
  }

  simple_core((argc >= 2 ? argv[1] : "score.hex"), initial_pc, stop_sim);

  optimize();

  ofstream cpr("score.cp");
  critpath_report(cpr);

  ofstream vcd("score.vcd");
  run(vcd, stop_sim, TMAX);

  return 0;
}

void fetch(fetch_decode_t &out_buf, exec_fetch_t &in,
           const char *hex_file, unsigned initial_pc)
{
  HIERARCHY_ENTER();
  fetch_decode_t out;

  word_t next_pc, pc(Reg(next_pc, initial_pc));
  Cassign(next_pc).
    IF(_(in, "ldpc"), _(in, "val")).
#ifdef STALL_SIGNAL
    IF(_(out_buf, "stall"), pc).
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
  HIERARCHY_EXIT();
}

void decode(decode_reg_t &out, fetch_decode_t &in) {
  HIERARCHY_ENTER();
  inst_t inst(_(in, "inst"));
  opcode_t opcode(inst[range<N-6, N-1>()]);
  func_t func;
  Cassign(func).
    IF(opcode == Lit<6>(0x01), Zext<6>(inst[range<16, 20>()])).
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
      opcode == Lit<6>(0x00) || // ALU
      dest_t || dest_31); // All of the instructions listed above

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

  #ifdef STALL_SIGNAL
  _(in, "stall") = _(out, "stall");
  #endif

  HIERARCHY_EXIT();
}

void reg(reg_exec_t &out_buf, decode_reg_t &in, mem_reg_t &in_wb) {
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

  #ifdef STALL_SIGNAL
  _(in, "stall") = _(out_buf, "stall");
  out_buf = Wreg(!_(out_buf, "stall"), Flatten(out));
  _(out_buf, "valid") = Wreg(!_(in, "stall"), _(out, "valid")) && !_(in, "stall");
  #else
  out_buf = Reg(Flatten(out));
  #endif

  HIERARCHY_EXIT();
}

void exec(exec_mem_t &out_buf, exec_fetch_t &out_pc, reg_exec_t &in,
          mem_exec_t &mem_fwd)
{
  HIERARCHY_ENTER();

  node in_valid;
  exec_mem_t out;

  #ifdef RANDOM_STALL
  // Test stall signal by generating random stalls.
  unsigned stall_seed(0x1234);
  bvec<15> stall_lfsr;
  for (unsigned i = 1; i < 15; ++i) stall_lfsr[i] = Reg(stall_lfsr[i-1], (stall_seed>>i)&1);
  stall_lfsr[0] = Reg(Xor(stall_lfsr[14], stall_lfsr[0]));
  node random_stall = stall_lfsr[14] || _(out_buf, "stall");
  #endif

  opcode_t op(_(in, "op"));
  func_t func(_(in, "func"));

  #ifdef STALL_SIGNAL  
  node stall(_(in, "stall"));
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
    END().IF(op == Lit<6>(0x07)).
      IF(func == Lit<6>(0x00) && OrN(val0) && !val0[N-1], bdest). // bgtz
    END().    
    ELSE(pc + LitW(4));

  // When a branch is mispredicted, the next two inputs are to be considered
  // invalid.
  bvec<2> next_bubble_ctr, bubble_ctr(Reg(next_bubble_ctr));
  node bubble(OrN(bubble_ctr));
  in_valid = _(in, "valid") && !bubble;

  node branch_mispredict(in_valid && next_pc != actual_next_pc);
  _(out_pc, "ldpc") = branch_mispredict;
  _(out_pc, "val") = actual_next_pc;

  Cassign(next_bubble_ctr).
  #ifdef STALL_SIGNAL
    IF(_(in, "stall"), bubble_ctr).
  #endif
    IF(branch_mispredict, Lit<2>(2)).
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

  #ifdef MUL_DIV
  word_t hi, lo;
  #endif

  Cassign(_(out, "result")).
    IF(_(in, "jal"), pc + LitW(N/8)). // jump/branch and link
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
  _(out_buf, "mem_rd") = Wreg(!stall, _(out, "mem_rd")) && !stall;
  _(out_buf, "mem_wr") = Wreg(!stall, _(out, "mem_wr")) && !stall;
  _(out_buf, "rdest_valid") = Wreg(!stall, _(out, "rdest_valid")) && !stall;
  #else
  out_buf = Reg(Flatten(out));
  #endif

  #ifdef MUL_DIV
  node mfhi(op == Lit<6>(0x00) && func == Lit<6>(0x10)),
       mflo(op == Lit<6>(0x00) && func == Lit<6>(0x12)),
       mult(op == Lit<6>(0x00) && func == Lit<6>(0x18) && in_valid),
       multu(op == Lit<6>(0x00) && func == Lit<6>(0x19) && in_valid),
       mul_busy, start_mul(mult || multu);
  bvec<2*N> mul_out, mul_a(Cat(bvec<N>(mult && val0[N-1]), val0)),
            mul_b(Cat(bvec<N>(mult && val0[N-1]), val1));

  TAP(hi); TAP(lo); TAP(mult); TAP(multu); TAP(mfhi); TAP(mflo);

  mul_out = SerialMul(mul_busy, mul_a, mul_b, start_mul);

  // TODO: Divider
  hi = mul_out[range<N, 2*N-1>()];
  lo = mul_out[range<0, N-1>()];

  node muldiv_stall = (mfhi || mflo) && mul_busy;

  TAP(mul_out); TAP(mul_a); TAP(mul_b); TAP(start_mul);
  #endif

  #ifdef STALL_SIGNAL
  stall = 
    #ifdef MUL_DIV
    muldiv_stall
    #endif
    #ifdef RANDOM_STALL
    || random_stall
    #endif
    || Lit(0)
  ;
  #endif

  HIERARCHY_EXIT();
}

void mem(mem_reg_t &out, mem_exec_t &fwd, exec_mem_t &in, bool &stop_sim) {
  HIERARCHY_ENTER();

  const unsigned B(N/8), BB(CLOG2(B));

  #ifdef STALL_SIGNAL
  _(in, "stall") = Lit(0);
  #endif

  vec<B, bvec<8> > memq, memd;
  bvec<CLOG2(N/8)> bytesel(_(in, "addr")[range<0, BB - 1>()]);
  bvec<B> wr(Decoder(bytesel, _(in, "mem_wr")) |
            bvec<B>(_(in, "mem_wr") && !_(in, "mem_byte")));

  // Break input into bytes
  for (unsigned i = 0; i < B; ++i)
    for (unsigned j = 0; j < 8; ++j)
      Cassign(memd[i][j]).
        IF(_(in, "mem_byte"), _(in, "result")[j]).
        ELSE(_(in, "result")[i*8 + j]);

  // Instantiate the SRAMs
  bvec<RAM_SZ> sram_addr(_(in, "addr")[range<BB, BB + RAM_SZ - 1>()]);
  for (unsigned i = 0; i < B; ++i)
    memq[i] = Syncmem(sram_addr, memd[i], wr[i]);

  // Combine output bytes into single word
  word_t memq_word;
  for (unsigned i = 0; i < B; ++i)
    for (unsigned j = 0; j < 8; ++j)
      memq_word[i*8 + j] = memq[i][j];

  // The destination register signals
  _(out, "rdest_idx") = Reg(_(in, "rdest_idx"));
  _(out, "rdest_valid") = Reg(_(in, "rdest_valid"));

  // Copies of the input and output destinations and values for 
  _(fwd, "rdest_idx") = _(out, "rdest_idx");
  _(fwd, "rdest_valid") = _(out, "rdest_valid");
  _(fwd, "result") = _(out, "result");

  // The final result: memory (word), memory(byte), or ALU
  Cassign(_(out, "result")).
    IF(Reg(_(in, "mem_rd"))).
      IF(Reg(_(in, "mem_byte")), Zext<N>(Mux(Reg(bytesel), memq))).
      ELSE(memq_word).
    END().
    ELSE(Reg(_(in, "result")));

  if (SOFT_IO) {
    static unsigned consoleOutVal;
    EgressInt(consoleOutVal, _(in, "result"));
    node wrConsole(wr[0] && _(in, "addr") == Lit<N>(1ul<<(N-1))),
         stopSimNode(wr[0] && _(in, "addr") == Lit<N>((1ul<<N-1) + N/8));

    EgressFunc([](bool x){
      if (x) cout << "OUTPUT> " << consoleOutVal << endl;
    }, wrConsole);

    Egress(stop_sim, stopSimNode);
  }

  if (DEBUG_MEM) {
    static unsigned dVal, qVal, prevAddrVal, addrVal, pcVal, prevPcVal,
                    resultVal;
    EgressInt(prevAddrVal, Reg(_(in, "addr")));
    EgressInt(addrVal, _(in, "addr"));
    EgressInt(dVal, _(in, "result"));
    EgressInt(qVal, memq_word);
    EgressInt(pcVal, _(in, "pc"));
    EgressInt(prevPcVal, Reg(_(in, "pc")));
    EgressInt(resultVal, _(out, "result"));

    EgressFunc([](bool x){
      if (sim_time() % 10 == 0) cout << "# " << sim_time() << endl;
      if (x) cout << "MEM WR> " << hex << pcVal << ", " << addrVal << '('
                  << dec << addrVal << "), " << dVal << endl;
    }, OrN(wr));

    EgressFunc([](bool x){
      if (x) cout << "MEM RD> " << hex << prevPcVal << '(' << prevAddrVal
                  << "), " << dec << prevAddrVal << ", " << qVal << endl;
    }, Reg(_(in, "mem_rd")));

    EgressFunc([](bool x){
      if (x) cout << "PC> " << hex << pcVal << dec << endl;
    }, Lit(1));
 
    EgressFunc([](bool x) {
      if (x) cout << "RESULT> " << resultVal << endl;
    }, _(out, "rdest_valid"));
  }
  HIERARCHY_EXIT();
}
