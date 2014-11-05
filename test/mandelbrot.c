/******************************************************************************\
| ASCII Graphics Mandelbrot Set                    |  2013  |  Chad D. Kersey  |
+--------------------------------------------------+--------+------------------+
| This is a simple ASCII mandelbrot set program, a prototype of a planned HARP |
| assembly language equivalent.                                                |
\*----------------------------------------------------------------------------*/
#include "lib.h"

static const unsigned ITER = 100, COLS = 80, ROWS = 30, FBITS = 28;

#define MUL(x, y) (((x)>>(FBITS/2))*((y)>>(FBITS/2)))

void cSquare(int32_t *xo, int32_t *yo, int32_t x, int32_t y) {
  *xo = MUL(x, x) - MUL(y, y);
  *yo = 2*MUL(x, y);
}

void doPix(int row, int col, int32_t l, int32_t r, int32_t t, int32_t b)
{
  int32_t x0 = l + ((r-l)>>(FBITS/2))*((1<<(FBITS/2))/COLS)*col,
          y0 = t + ((b-t)>>(FBITS/2))*((1<<(FBITS/2))/ROWS)*row,
          x = x0, y = y0;

  unsigned i;
  for (i = 0; i < ITER; i++) {
    cSquare(&x, &y, x, y);
    x += x0;
    y += y0;
    int32_t r2 = MUL(x, x) + MUL(y, y);
    if (r2 > (4<<FBITS)) {
      putc(' ', stdout);
      return;
    }
  }
  putc('*', stdout);
}

void renderFrame(int32_t l, int32_t r, int32_t t, int32_t b) {
  unsigned i, j;
  for (i = 0; i < ROWS; i++) {
    for (j = 0; j < COLS; j++)
      doPix(i, j, l, r, t, b);
    putc('\n', stdout);
  }
}

int main() {
  // Zoom in on -0.1011 + 0.9563i
  unsigned z;
  int32_t hr = 4*(1<<FBITS), vr = 3*(1<<FBITS),
          hc = 311*(1<<(FBITS-10)), vc = 33*(1<<(FBITS-10));
  for (z = 0; z < 80; ++z) {
    hr = (hr>>(FBITS/2))*((9<<(FBITS/2))/10);
    vr = (vr>>(FBITS/2))*((9<<(FBITS/2))/10);
    int32_t l = (hc - hr/2),
      r = (hc + hr/2),
      t = (vc - vr/2),
      b = (vc + vr/2);
    renderFrame(l, r, t, b);
  }
 
  return 0;
}
