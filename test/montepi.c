#include "lib.h"

#define KTRIALS 10
#define TRIALS (1000*KTRIALS)
#define MAX_CORES 1024

barrier_t b0, b1, b2;

int count[MAX_CORES];

void init_rng(unsigned *p) {
  for (unsigned i = 0; i < get_core_id(); i++) rand_r(p);
}

unsigned rand_mc(unsigned *p) {
  unsigned val;
  for (unsigned i = 0; i <= get_num_cores(); i++) val = rand_r(p);
  return val;
}

int main() {
  /* Initialize the RNG */
  unsigned rng, i, trials = TRIALS / get_num_cores(), x, y, active;
  rng = 0;
  init_rng(&rng);

  barrier(&b0);

  /* Do the simulation */
  for (i = 0; i < trials; ++i) {
    x = rand_mc(&rng) / ((1u<<31)/10000);
    y = rand_mc(&rng) / ((1u<<31)/10000);
    if (x * x + y * y <= 100000000) count[get_core_id()]++;
  }

  printf("core %u: %u\n", get_core_id(), count[get_core_id()]);

  barrier(&b1); if (get_core_id() == 0) barrier_init(&b0);
  barrier(&b2); if (get_core_id() == 0) barrier_init(&b1);
  /* Do the final reduction */

  for (active = get_num_cores()/2; active > 0; active /= 2) {
    if (get_core_id() < active) {
      unsigned idx0 = get_core_id(), idx1 = get_core_id() + active;
      count[idx0] = count[idx0] + count[idx1];
      printf("%u active cores, sum = %u\n", active, count[idx0]);
    }

    barrier(&b0); if (get_core_id() == 0) barrier_init(&b2);
    barrier(&b1); if (get_core_id() == 0) barrier_init(&b0);
    barrier(&b2); if (get_core_id() == 0) barrier_init(&b1);
  }

  if (get_core_id() == 0) {
    unsigned pi_whole = count[0]*4/TRIALS,
             pi_frac = count[0]*4%TRIALS/(TRIALS/100);
    printf("pi is approximately %u.%02u\n", pi_whole, pi_frac);
  }

  return 0;
}
