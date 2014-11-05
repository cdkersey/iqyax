/* SimpleCore Stub Library
 */

#ifndef SCORE_LIB_H
#define SCORE_LIB_H

#ifdef HOST
static void init_lib(void) {}
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned core_id_t;
core_id_t get_core_id(void) { return 0; }
core_id_t get_num_cores(void) { return 1; }

void atomic_increment(unsigned *p) { (*p)++; }

typedef volatile unsigned spinlock_t;

void spinlock_init(spinlock_t *x) {}
void spin_lock(spinlock_t *x) {}
void spin_unlock(spinlock_t *x) {}

typedef volatile unsigned barrier_t;
void barrier_init(barrier_t *p) {}
void barrier(barrier_t *p) {}

#else
#include <stdarg.h>

typedef char FILE;
extern FILE *stdout, *stderr;

void init_lib(void);

FILE *fopen(const char *path, const char *mode);
int fclose(FILE *fp);

void putc(int c, FILE *f);
void fputc(int c, FILE *f);
void puts(const char *s);
void fputs(const char *s, FILE *f);

int fprintf(FILE *f, const char *fmt, ...);
int printf(const char *fmt, ...);

void srand(unsigned seed);
int rand(void);
int rand_r(unsigned int *seedp);

typedef unsigned long size_t;
void *malloc(size_t sz);
void free(void *ptr);

void *memset(void *s_vp, int c, unsigned long n);
void *memcpy(void *d_vp, void *s_vp, unsigned long n);
void *memmove(void *d_vp, void *s_vp, unsigned long n);

typedef unsigned core_id_t;

core_id_t get_core_id(void);
core_id_t get_num_cores(void);

void atomic_increment(volatile unsigned *p);

typedef volatile unsigned spinlock_t;

void spinlock_init(spinlock_t *s);
void spin_lock(spinlock_t *s);
void spin_unlock(spinlock_t *s);

typedef volatile unsigned barrier_t;

void barrier_init(barrier_t *b);
void barrier(barrier_t *b);

typedef unsigned char uint8_t;
typedef signed char int8_t;
typedef unsigned int uint32_t;
typedef int int32_t;
#endif

#endif
