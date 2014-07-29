#ifndef CHDL_SST_H
#define CHDL_SST_H

#include <chdl/chdl.h>

#include <chdl/ag.h>
#include <chdl/net.h>

const unsigned ADDR_SZ(32), DATA_SZ(32), ID_SZ(5);

typedef chdl::flit<
  chdl::ag<STP("wr"), chdl::node,
  chdl::ag<STP("addr"), chdl::bvec<ADDR_SZ>,
  chdl::ag<STP("size"), chdl::bvec<chdl::CLOG2(DATA_SZ/8 + 1)>,
  chdl::ag<STP("data"), chdl::bvec<DATA_SZ>,
  chdl::ag<STP("uncached"), chdl::node,
  chdl::ag<STP("llsc"), chdl::node,
  chdl::ag<STP("locked"), chdl::node,
  chdl::ag<STP("id"), chdl::bvec<ID_SZ>
> > > > > > > > > simpleMemReq_t;

typedef chdl::flit<chdl::ag<STP("data"), chdl::bvec<DATA_SZ>,
                   chdl::ag<STP("id"), chdl::bvec<ID_SZ>,
                   chdl::ag<STP("wr"), chdl::node,
                   chdl::ag<STP("llsc"), chdl::node,
                   chdl::ag<STP("llsc_suc"), chdl::node
> > > > > > simpleMemResp_t;

void SimpleMemReqPort(std::string ident, simpleMemReq_t &req);
void SimpleMemRespPort(std::string ident, simpleMemResp_t &resp);

#ifdef SIM_
void chdl_sst_sim_run(bool &stop_sim,
                      const char *hex_file,
                      chdl::cycle_t cycles);
void chdl_sst_sim_run(bool &stop_sim, chdl::cycle_t cycles);
void chdl_sst_sim_run(chdl::cycle_t cycles);
#endif

#endif
