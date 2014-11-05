#include "lib.h"

#define N 10
#define RADIX 8
#define L2RADIX 3
#define DIGITS 4

typedef struct trie {
  struct trie *next[RADIX];
} trie_t;

void init_trie(trie_t *t) { for (int i = 0; i < RADIX; ++i) t->next[i] = 0; }

void init(int *begin, int *end);

trie_t *build_trie(int *begin, int *end);
void output(trie_t *t);

int main() {
  int a[N];

  init(a, a + N);                   /* Initialize the array */
  for (int *p = a; p < a+N; ++p) printf("%d\n", *p);
  puts("Generated input. Building trie.");
  trie_t *t = build_trie(a, a + N); /* Build trie */
  puts("Built trie. Traversing.");
  output(t);                        /* Dump results */

  return 0;
}

void init(int *begin, int *end) {
  int *p;
  for (p = begin; p != end; ++p) *p = rand() & ((1<<(L2RADIX * DIGITS))-1);
}

int get_dig(int val, int l2radix, int idx) {
  return (val >> (l2radix*idx)) & ((1<<l2radix)-1);
}

trie_t *trie_insert(trie_t *t, int level, int val) {
  int d = get_dig(val, L2RADIX, level);

  trie_t **p = &(t->next[d]);

  if (!*p) {
    *p = malloc(sizeof(trie_t));
    init_trie(*p);
  }

  if (level != 0) trie_insert(*p, level-1, val);

  return t;
}

trie_t *build_trie(int *begin, int *end) {
  trie_t *t = malloc(sizeof(trie_t));
  init_trie(t);

  for (int *p = begin; p != end; ++p)
    trie_insert(t, DIGITS-1, *p);

  return t;
}

void output_internal(trie_t *t, int level, int val) {
  if (level == 0) { printf("%d\n", val); return; }

  for (int i = 0; i < RADIX; ++i) {
    if (t->next[i])
      output_internal(t->next[i], level-1, val | (i<<(L2RADIX * (level-1))));
  }
}

void output(trie_t *t) {
  output_internal(t, DIGITS, 0);
}
