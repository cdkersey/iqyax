#include "lib.h"

#define N 16

barrier_t b0, b1, b2;

int a[N];

int main() {
  barrier(&b0);
  /* Initialize the array */
  unsigned start = N/get_num_cores() * get_core_id(),
           end   = (N/get_num_cores() * (get_core_id() + 1)),
           i, sum, active;

  printf("Core %u initializing %u - %u.\n", get_core_id(), start, end);

  for (i = start; i < end; i++) a[i] = i;
  
  barrier(&b1); if (get_core_id() == 0) barrier_init(&b0);
  /* Do the fully parallel part of the summation. */
  
  for (i = start, sum = 0; i < end; i++) sum += a[i];
  a[start] = sum;

  printf("Core %u: %u\n", get_core_id(), sum);

  barrier(&b2); if (get_core_id() == 0) barrier_init(&b1);
  /* Do the final reduction */
  for (active = get_num_cores()/2; active >= 1; active /= 2) {
    // Three barriers 
    barrier(&b0); if (get_core_id() == 0) barrier_init(&b2);
    barrier(&b1); if (get_core_id() == 0) barrier_init(&b0);
    barrier(&b2); if (get_core_id() == 0) barrier_init(&b1);

    if (get_core_id() < active) {
      unsigned idx0 = start,
               idx1 = (end - start)*(get_core_id() + active);
      a[idx0] = a[idx0] + a[idx1];
      printf("%u active cores, sum = %u\n", active, a[idx0]);
    }
  }

  return 0;
}
