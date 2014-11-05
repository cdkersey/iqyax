#include "lib.h"

FILE *stdout, *stderr;
unsigned rseed;
void *brk;

void init_lib(void) {
  /* Standard input and output. */
  stdout = stderr = (FILE*)0x80000008;

  /* Universal random seed */
  rseed = 0x5eed;

  /* Break for heap */
  brk = (void*)0x10000;
}

FILE *fopen(const char *path, const char *mode) { return (FILE*)0x80000008; }
int fclose(FILE *fp) { return 0; }
void putc(int c, FILE *f) { *(volatile char *)f = c; }
void fputc(int c, FILE *f) { putc(c, f); }

void puts(const char *s) { while (*s) fputc(*s++, stdout); fputc('\n', stdout); }
void fputs(const char *s, FILE *f) { while (*s) fputc(*s++, f); }

void putn(FILE *f, int zeropad, int len, int base, int firstdig, unsigned n) {
  int dig = n % base, c = (dig >= 10) ? dig - 10 + 'a' : dig + '0';

  if (!firstdig && n == 0 && len == 0) return;

  if (zeropad || n / base)
    putn(f, zeropad, len-1, base, 0, n / base);

  putc(c, f);
}

void putsn(FILE *f, int zeropad, int len, int base, int firstdig, int n) {
  if (n < 0) { putc('-', f); n = -n; }
  putn(f, zeropad, len, base, firstdig, n);
}

int vfprintf(FILE* f, const char *fmt, va_list args) {
  int count = 0, rd_fmt = 0, zeropad = 0, numlen = 0, lng = 0;

  while (*fmt) {
    if (*fmt == '%') { rd_fmt = 1; fmt++; }
    if (rd_fmt) {
      if (*fmt == '%') {
        putc('%', f);
        rd_fmt = zeropad = numlen = lng = 0;
      } else if (*fmt == '0') {
        if (rd_fmt == 1) zeropad = 1;
        else numlen *= 10;
      } else if (*fmt >= '1' && *fmt <= '9') {
        rd_fmt = 2;
        numlen = numlen*10 + *fmt - '0';
      } else if (*fmt == 'l' || *fmt == 'L') {
        lng = 1;
      } else if (!lng && *fmt == 'd') {
        putsn(f, zeropad, numlen, 10, 1, va_arg(args, int));
        count++;
        rd_fmt = zeropad = numlen = lng = 0;
      } else if (!lng && *fmt == 'u') {
        putn(f, zeropad, numlen, 10, 1, va_arg(args, unsigned));
        count++;
        rd_fmt = zeropad = numlen = lng = 0;
      } else if (!lng && *fmt == 'x') {
        putn(f, zeropad, numlen, 16, 1, va_arg(args, unsigned));
        count++;
        rd_fmt = zeropad = numlen = lng = 0;
      } else if (!lng && *fmt == 'o') {
        putn(f, zeropad, numlen, 16, 1, va_arg(args, unsigned));
        count++;
        rd_fmt = zeropad = numlen = lng = 0;
      } else if (lng && *fmt == 'd') {
        putsn(f, zeropad, numlen, 10, 1, va_arg(args, long));
        count++;
        rd_fmt = zeropad = numlen = lng = 0;
      } else if (lng && *fmt == 'u') {
        putn(f, zeropad, numlen, 10, 1, va_arg(args, unsigned long));
        count++;
        rd_fmt = zeropad = numlen = lng = 0;
      } else if (lng && *fmt == 'x') {
        putn(f, zeropad, numlen, 16, 1, va_arg(args, unsigned long));
        count++;
        rd_fmt = zeropad = numlen = lng = 0;
      } else if (lng && *fmt == 'o') {
        putn(f, zeropad, numlen, 8, 1, va_arg(args, unsigned long));
        count++;
        rd_fmt = zeropad = numlen = lng = 0;
      } else if (*fmt == 's') {
        fputs(va_arg(args, const char*), f);
        count++;
        rd_fmt = zeropad = numlen = lng = 0;
      } else if (*fmt == 'c') {
        fputc(va_arg(args, int), f);
        count++;
        rd_fmt = zeropad = numlen = lng = 0;
      }
    } else {
      putc(*fmt, f);
    }

    fmt++;
  }

  return count;
}

int fprintf(FILE *f, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  return vfprintf(f, fmt, args);
}

int printf(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  return vfprintf(stdout, fmt, args);
}

void srand(unsigned seed) { rseed = seed; }

int rand_r(unsigned int *seedp) {
  *seedp = (*seedp * 1103515245 + 12345)&0x7fffffff;
  return *seedp;
}

int rand(void) { return rand_r(&rseed); }

void *malloc(size_t sz) {
  void *ptr = brk;
  brk += sz;

  return ptr;
}

void free(void *ptr) {
  /* Just leak memory all over the place for now. */
}

#pragma GCC optimize ("1")
void *memset(void *s_vp, int c, unsigned long n) {
  unsigned i;
  char *s = s_vp;
  for (i = 0; i < n; ++i) s[i] = c;
  return s_vp;
}

void *memcpy(void *d_vp, void *s_vp, unsigned long n) {
  unsigned i;
  char *d = d_vp, *s = s_vp;
  for (i = 0; i < n; ++i) d[i] = s[i];
  return d_vp;
}

void *memmove(void *d_vp, void *s_vp, unsigned long n) {
  unsigned i;
  char *d = d_vp, *s = s_vp;
  for (i = 0; i < n; ++i) d[n-i-1] = s[n - i - 1];
  return d_vp;
}

core_id_t get_core_id(void) {
  return *(unsigned *)0x88000004;
}

core_id_t get_num_cores(void) {
  return *(unsigned *)0x88000008;
}

unsigned _ll(volatile unsigned *p) {
  unsigned val;
  __asm__ __volatile__ ("ll %0, 0(%1)\n":"=r"(val):"r"(p));
  return val;
}

unsigned _sc(volatile unsigned *addr, unsigned val) {
  __asm__ __volatile__ ("sc %0, 0(%1)\n":"+r"(val):"r"(addr));
  return val;
}

unsigned _tns(volatile unsigned *p) {
  unsigned val, success, delay;
  delay = get_core_id() + 1;
  do {
    val = _ll(p);
    unsigned i;
    for (i = 0; i < delay; ++i) __asm__ __volatile__("nop");
    success = _sc(p, 1);
  } while (!success);

  return val;
}

void atomic_increment(volatile unsigned *p) {
  unsigned val, success, delay;
  delay = get_core_id() + 1;
  do {
    val = _ll(p);
    unsigned i; 
    for (i = 0; i < delay; ++i) __asm__ __volatile__("nop");
    success = _sc(p, val+1);
  } while(!success);
}

void barrier_init(barrier_t *p) {
  *p = 0;
}

void barrier(barrier_t *p) {
  atomic_increment(p);

  while (*p != get_num_cores()) {
    unsigned i;
    for (i = 0; i < 10*(get_core_id() + 1); ++i)
       __asm__ __volatile__("nop");
  }
}

void spinlock_init(spinlock_t *p) {
  *p = 0;
}

void spin_lock(spinlock_t *p) {
  unsigned success;
  do {
    while (*p);
    success = !_tns(p);
  } while (!success);
}

void spin_unlock(spinlock_t *p) {
  *p = 0;
}

#pragma GCC reset_options
