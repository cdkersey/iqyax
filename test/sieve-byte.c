#include "lib.h"

#define N 100

void outn(int x);

int main() {
  char a[N];
  int i, j;

  a[0] = a[1] = 0;

  for (i = 2; i < N; i++) a[i] = i;

  for (i = 2; i < N; i++) {
    if (a[i]) {
      printf("%d\n", i);
      for (j = i * i; j < N; j += i)
        a[j] = 0;
    }
  }

  return 0;
}
