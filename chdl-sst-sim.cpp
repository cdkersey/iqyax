#include <map>
#include <string>
#include <cstdlib>

#include "chdl-sst.h"

#include <chdl/ingress.h>
#include <chdl/egress.h>

using namespace std;
using namespace chdl;

struct req_t {
  bool valid, ready, wr;
  unsigned long addr, data;
  unsigned size, id;
};

struct resp_t {
  bool valid, ready, wr;
  unsigned long data;
  unsigned id;
};

struct memUnit_t {
  req_t req;
  resp_t resp;

  map<unsigned long, unsigned long> contents;
};

struct respEvent_t {
  respEvent_t(memUnit_t *dest, unsigned long data, unsigned id, bool wr)
  : data(data), id(id), wr(wr), dest(dest)
  {}

  unsigned long data;
  unsigned id;
  bool wr;

  memUnit_t *dest;
};

map<string, memUnit_t*> mu;
multimap<cycle_t, respEvent_t*> eq;

void sched_resp(cycle_t t, memUnit_t *dest, unsigned long data,
                unsigned id, bool wr)
{
  eq.insert(make_pair(t, new respEvent_t(dest, data, id, wr)));

  cout << eq.size() << " events in queue.\n";
}

void tick_eq(cycle_t t) {
  cout << "tick_eq(" << t << ')' << endl;

  while (eq.find(t) != eq.end()) {
    cout <<   "Handling event" << endl;

    respEvent_t &r(*eq.find(t)->second);
    memUnit_t &m(*r.dest);

    if (m.resp.ready) {
      m.resp.valid = true;

      m.resp.data = r.data;
      m.resp.id = r.id;
      m.resp.wr = r.wr;
    } else {
      // This coresponds with how this is handled in SST now. Once a queue or
      // similar is added in SST, we can deflect these to later cycles by not
      // processing them and changning eq.find(t) to eq.begin() above
      cerr << "Not ready on attempt to write response.";
      abort();
    }

    if (!m.resp.wr) cout << "MEM VAL " << m.resp.data << endl;

    delete eq.find(t)->second;
    eq.erase(eq.find(t));
  }

  cout << eq.size() << " events in queue.\n";
  if (!eq.empty()) cout << "Next event at " << eq.begin()->first << endl;
}

void processReq(cycle_t now, memUnit_t *m) {
  if (m->req.valid) {
    cout << "MEM " << (m->req.wr?"WRITE":"READ") << ' ' << m->req.addr;
    if (m->req.wr) cout << ", " << (m->req.data);
    cout << endl;

    cycle_t respTime(now + (rand()&0x07));
    unsigned long rd_data;
    if (m->req.wr) {
      m->contents[m->req.addr] = m->req.data;
      cout << "  MEM WRITE " << m->req.addr << ", " << m->req.data << endl;
    } else {
      rd_data = m->contents[m->req.addr];
      cout << "  MEM READ " << m->req.addr << ", " << rd_data << endl;
    }
    
    sched_resp(respTime, m, rd_data, m->req.id, m->req.wr);
  }
}

void SimpleMemReqPort(string ident, simpleMemReq_t &req) {
  cout << "SimpleMemReqPort \"" << ident << '"' << endl;

  memUnit_t *p;
  if (mu.find(ident) == mu.end()) p = mu[ident] = new memUnit_t();
  else p = mu[ident];

  _(req, "ready") = Lit(1); //Ingress(p->req.ready);
  Egress(p->req.valid, _(req, "valid"));
  EgressInt(p->req.addr, _(_(req, "contents"), "addr"));
  EgressInt(p->req.data, _(_(req, "contents"), "data"));
  EgressInt(p->req.size, _(_(req, "contents"), "size"));
  EgressInt(p->req.id, _(_(req, "contents"), "id"));
  Egress(p->req.wr, _(_(req, "contents"), "wr"));
}

void SimpleMemRespPort(string ident, simpleMemResp_t &resp) {
  cout << "SimpleMemRespPort \"" << ident << '"' << endl;

  memUnit_t *p;
  if (mu.find(ident) == mu.end()) p = mu[ident] = new memUnit_t();
  else p = mu[ident];

  Egress(p->resp.ready, _(resp, "ready"));
  _(resp, "valid") = IngressAutoclear(p->resp.valid);
  _(_(resp, "contents"), "data") = IngressInt<DATA_SZ>(p->resp.data);
  _(_(resp, "contents"), "id") = IngressInt<ID_SZ>(p->resp.id);
  _(_(resp, "contents"), "wr") = Ingress(p->resp.wr);
}

extern unsigned aval, pcval, irval, seqval, mdrval;
extern bool rvval, rwval;

void chdl_sst_sim_run(bool &stop, cycle_t c) {
  for (unsigned i = 0; i < c && !stop; ++i) {
    advance();

    for (auto &x : mu)
      processReq(i, x.second);

    tick_eq(i);
  }
}

void chdl_sst_sim_run(cycle_t c) { bool x(false); chdl_sst_sim_run(x, c); }
