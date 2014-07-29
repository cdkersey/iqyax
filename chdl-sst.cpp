#include "chdl-sst.h"

using namespace std;
using namespace chdl;

void SimpleMemReqPort(string ident, simpleMemReq_t &req) {
  _(req, "ready") = Input("simplemem_ready_req_" + ident);
  tap("simplemem_valid_req_" + ident, _(req, "valid"), true);
  tap("simplemem_addr_req_" + ident, _(_(req, "contents"), "addr"), true);
  tap("simplemem_size_req_" + ident, _(_(req, "contents"), "size"), true);
  tap("simplemem_data_req_" + ident, _(_(req, "contents"), "data"), true);
  tap("simplemem_uncached_req_"+ident, _(_(req, "contents"), "uncached"), true);
  tap("simplemem_llsc_req_"+ident, _(_(req,"contents"),"llsc"), true);
  tap("simplemem_locked_req_" + ident, _(_(req, "contents"), "locked"), true);
  tap("simplemem_id_req_" + ident, _(_(req, "contents"), "id"), true);
  tap("simplemem_wr_req_" + ident, _(_(req, "contents"), "wr"), true);
}

void SimpleMemRespPort(string ident, simpleMemResp_t &resp) {
  tap("simplemem_ready_resp_" + ident, _(resp, "ready"), true);
  _(resp, "valid") = Input("simplemem_valid_resp_" + ident);
  _(_(resp, "contents"), "data") = Input<DATA_SZ>("simplemem_data_resp_"+ident);
  _(_(resp, "contents"), "id") = Input<ID_SZ>("simplemem_id_resp_" + ident);
  _(_(resp, "contents"), "wr") = Input("simplemem_wr_resp_" + ident);
  _(_(resp, "contents"), "llsc") = Input("simplemem_llsc_resp_"+ident);
  _(_(resp, "contents"), "llsc_suc") = Input("simplemem_llscsuc_resp_"+ident);
}
