#ifndef CHDL_SST_H
#define CHDL_SST_H

#include <chdl/chdl.h>

#include <chdl/ag.h>
#include <chdl/net.h>
#include <chdl/memreq.h>

const unsigned ADDR_SZ(32), DATA_SZ(32), ID_SZ(5);

typedef chdl::mem_req<8, DATA_SZ/8, ADDR_SZ, ID_SZ> simpleMemReq_t;
typedef chdl::mem_resp<8, DATA_SZ/8, ID_SZ> simpleMemResp_t;

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
