#ifndef IQYAX_MULDIV_H
#define IQYAX_MULDIV_H

#include <chdl/chdl.h>
#include <chdl/cassign.h>

template <unsigned N>
   chdl::bvec<N> SerialMul(chdl::node &busy, chdl::bvec<N> a, chdl::bvec<N> b,
                           chdl::node &start);

template <unsigned N>
  void SerialDiv(
    chdl::bvec<N> &q, chdl::bvec<N> &r, chdl::node &ready, chdl::node &waiting,
    chdl::bvec<N> n, chdl::bvec<N> d, chdl::node start, chdl::node stall
  );


template <unsigned N>
  chdl::bvec<N> SerialMul(chdl::node &busy,
                          chdl::bvec<N> a, chdl::bvec<N> b, chdl::node &start)
{
  using namespace chdl;

  bvec<N> shrreg;
  bvec<CLOG2(N+1)> next_state, state(Reg(next_state));
  Cassign(next_state).
    IF(state == Lit<CLOG2(N+1)>(0)).
    IF(start, Lit<CLOG2(N+1)>(2)).
    ELSE(Lit<CLOG2(N+1)>(0)).
    END().
    IF(state == Lit<CLOG2(N+1)>(N), Lit<CLOG2(N+1)>(0)).
    IF(OrN(shrreg), state + Lit<CLOG2(N+1)>(2)).
    ELSE(Lit<CLOG2(N+1)>(0));

  busy = OrN(state);

  bvec<N> next_shlreg, shlreg(Reg(next_shlreg));
  Cassign(next_shlreg).
    IF(start, b).
    ELSE(shlreg << Lit<CLOG2(N)>(2));

  bvec<N> next_shrreg;
  shrreg = Reg(next_shrreg);
  Cassign(next_shrreg).
    IF(start, a).
    ELSE(shrreg >> Lit<CLOG2(N)>(2));

  bvec<N> shlreg2(Cat(shlreg[range<0,N-2>()], Lit(0))),
          shlreg3(shlreg2 + shlreg);

  bvec<N> next_resultreg, resultreg(Reg(next_resultreg));
  Cassign(next_resultreg).
    IF(start, Lit<N>(0)).
    IF(busy && shrreg[0] && !shrreg[1], resultreg + shlreg).
    IF(busy && !shrreg[0] && shrreg[1], resultreg + shlreg2).
    IF(busy && shrreg[0] && shrreg[1], resultreg + shlreg3).
    ELSE(resultreg);

  static bool tapped(false);
  if (!tapped) {
    tap("mul_resultreg", resultreg);
    tap("mul_state", state);
    tap("mul_shrreg", shrreg);
    tap("mul_shlreg", shlreg);
    tap("mul_shlreg2", shlreg2);
    tap("mul_shlreg3", shlreg3);
    tapped = true;
  }

  return resultreg;
}

template <unsigned N, bool D>
  chdl::bvec<N> Shiftreg(
    chdl::bvec<N> in, chdl::node load, chdl::node shift, chdl::node shin
  )
{
  using namespace chdl;
  using namespace std;

  HIERARCHY_ENTER();  

  bvec<N+1> val;
  val[D?N:0] = shin;

  if (D) {
    for (int i = N-1; i >= 0; --i)
      val[i] = Reg(Mux(load, Mux(shift, val[i], val[i+1]), in[i]));
    HIERARCHY_EXIT();
    return val[range<0, N-1>()];
  } else {
    for (unsigned i = 1; i < N; ++i)
      val[i] = Reg(Mux(load, Mux(shift, val[i], val[i-1]), in[i-1]));
    HIERARCHY_EXIT();
    return val[range<1, N>()];
  }
}

template <unsigned N>
  chdl::bvec<N> Lshiftreg(
    chdl::bvec<N> in, chdl::node load, chdl::node shift,
    chdl::node shin = chdl::Lit(0)
  )
{ return Shiftreg<N, false>(in, load, shift, shin); }

template <unsigned N>
  chdl::bvec<N> Rshiftreg(
    chdl::bvec<N> in, chdl::node load, chdl::node shift,
    chdl::node shin = chdl::Lit(0)
  )
{ return Shiftreg<N, true>(in, load, shift, shin); }

template <unsigned N>
  void SerialDiv(
    chdl::bvec<N> &q, chdl::bvec<N> &r, chdl::node &ready, chdl::node &waiting,
    chdl::bvec<N> n, chdl::bvec<N> d, chdl::node start, chdl::node stall
  )
{
  using namespace chdl;

  // The controller
  bvec<CLOG2(N+3)> next_state, state(Reg(next_state));
  Cassign(next_state).
    IF(state == Lit<CLOG2(N+3)>(0)).
      IF(start, Lit<CLOG2(N+3)>(1)).
      ELSE(Lit<CLOG2(N+3)>(0)).
    END().IF(state == Lit<CLOG2(N+3)>(N+2)).
      IF(stall, Lit<CLOG2(N+3)>(N+2)).
      ELSE(Lit<CLOG2(N+3)>(0)).
    END().ELSE(state + Lit<CLOG2(N+3)>(1));

  tap("div_state", state);

  ready = (state == Lit<CLOG2(N+3)>(N+2));
  waiting = (state == Lit<CLOG2(N+3)>(0));

  tap("div_waiting", waiting);

  node shift;
  Cassign(shift).
    IF(state == Lit<CLOG2(N+3)>(0), Lit(0)).
    IF(state == Lit<CLOG2(N+3)>(N + 2), Lit(0)).
    ELSE(Lit(1));

  // The data path
  bvec<2*N> s(Rshiftreg(Cat(d, Lit<N>(0)), start, shift));
  node qbit(Cat(Lit<N>(0), r) >= s);
  r = Wreg(shift || start, Mux(start, Mux(qbit, r, r - s[range<0, N-1>()]), n));
  q = Lshiftreg(Lit<N>(0), start, shift, qbit);

  tap("div_s", s);
  tap("div_r", r);
  tap("div_q", q);
}

#endif
