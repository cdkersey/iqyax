#include "lib.h"

struct strizzy {
  enum { HURR, DURR, MURR, BURR } a;
  union {
    struct { int a; char b; int c; } hurr;
    struct { int a; int b; int c; } durr;
    struct { char a; int b; int c; } murr;
    int burr[3];
  } b;
  char x[12];
  int y[2];
  unsigned long z[4];  
};

const struct strizzy stoomp = {
  HURR, {.hurr = {1234, 234, 345}}, "fun", {1, 2}, {2, 3, 4, 5}
};

const struct strizzy moomp = {
  DURR, {.durr = {1234, 2345, 4567}}, "notfun", {-1, -2}, {3, 4, 5, 6}
};

const struct strizzy bloomp = {
  MURR, {.murr = {123, 2345, 4567}}, "lessfun", {100, 200}, {100, 200, 300, 400}
};

const struct strizzy poomp = {
  BURR, {.burr = {-1, -2, -3}}, "leastfun", {-100, -200},
  {0x100, 0x200, 0x300, 0x400}
};

void dump(unsigned char *c, unsigned n) {
  unsigned i;
  printf("Dumping %u-byte object:\n", n);
  for (i = 0; i < n; ++i) {
    printf(" %02x", c[i]);
    if (i%16 == 15) putc('\n', stdout);
  }
  putc('\n', stdout);
}

int main() {
  dump((unsigned char *)&stoomp, sizeof(stoomp));
  dump((unsigned char *)&moomp, sizeof(moomp));
  dump((unsigned char *)&bloomp, sizeof(bloomp));
  dump((unsigned char *)&poomp, sizeof(poomp));
  return 0;
}
