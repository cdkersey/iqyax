#include "lib.h"

#define N 3

volatile unsigned i, b1, b2, b3, l;

int main() {
  unsigned j;

  printf("Greetings from core %u of %u\n", get_core_id(), get_num_cores());

  for (j = 0; j < N; ++j)
    atomic_increment(&i);

  barrier(&b1);

  printf("%u\n", i);

  barrier(&b2);

  for (j = 0; j < N; ++j) {
    spin_lock(&l);
    --i;
    spin_unlock(&l);
  }

  barrier(&b3);
  
  printf("%u\n", i);

  return 0;
}

