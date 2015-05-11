#include <chdl/chdl.h>
#include <chdl/loader.h>

#include "chdl-mem.h"

using namespace std;
using namespace chdl;

void SimpleMemReqPort(string ident, simpleMemReq_t &req) {
  out_mem_req<8, DATA_SZ/8, ADDR_SZ - CLOG2(DATA_SZ/8), ID_SZ> req_out(req);
  Expose(ident + string("_req"), req_out);
}

void SimpleMemRespPort(string ident, simpleMemResp_t &resp) {
  in_mem_resp<8, DATA_SZ/8, ID_SZ> resp_in(resp);
  Expose(ident + string("_resp"), resp_in);
}
