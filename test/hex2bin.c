#include <stdio.h>

void read_word(FILE *in, FILE *out, unsigned word_size);

int main(int argc, char **argv) {
  size_t base_address = 0, size = 0, entry_point = 0x400000;
  unsigned word_size = 4;
  const char *out_filename;
  FILE *in, *out;

  /* Print usage messgae if number of arguments is incorrect. */
  if (argc != 3 && argc != 4 && argc != 5 && argc != 6) {
    fprintf(stderr, "Usage:\n  %s <hex file> "
                    "[base address [ total size [word size] ] ] "
                    "<output file>\n", argv[0]);
    return 1;
  }

  /* Open input file */
  in = fopen(argv[1], "r");
  if (!in) {
    fprintf(stderr, "Could not open \"%s\" for reading.\n", argv[1]);
    return 1;
  }

  /* Assign output filename */
  if (argc == 3) out_filename = argv[2];
  if (argc == 4) out_filename = argv[3];
  if (argc == 5) out_filename = argv[4];
  else           out_filename = argv[5];

  /* Open output file */
  out = fopen(out_filename, "wb");
  if (!out) {
    fprintf(stderr, "Could not open \"%s\" for writing.\n", argv[1]);
    return 1;
  }

  /* Read base address. */
  if (argc == 4 || argc == 5 || argc == 6) {
    if (!sscanf(argv[2], "%li", &base_address)) {
      fprintf(stderr, "Could not convert base address \"%s\".\n", argv[2]);
      return 1;
    } 
  }

  /* Read total size. */
  if (argc == 5 || argc == 6) {
    if (!sscanf(argv[3], "%li", &size)) {
      fprintf(stderr, "Could not convert size \"%s\".\n", argv[2]);
      return 1;
    } 
  }

  #if 0
  /* Read word size */
  if (argc == 6) {
    if (!sscanf(argv[4], "%i", &word_size)) {
      fprintf(stderr, "Could not convert word size \"%s\".\n", argv[3]);
      return 1;
    }
  }
  #else
  /* Read entry point */
  if (argc == 6) {
    if (!sscanf(argv[4], "%li", &entry_point)) {
      fprintf(stderr, "Could not convert entry point \"%s\".\n", argv[3]);
      return 1;
    }
  }
  #endif

  /* Skip to base address in output file. */
  fseek(out, base_address - 2 * word_size, SEEK_SET);

  /* Output jump-to-entry-point instruction. */
  {
    unsigned jmp = 0x08000000 | ((entry_point>>2)&0x3ffffff);
    unsigned char jb0 = jmp, jb1 = jmp >> 8, jb2 = jmp >> 16, jb3 = jmp >> 24;
    fwrite(&jb0, 1, 1, out);
    fwrite(&jb1, 1, 1, out);
    fwrite(&jb2, 1, 1, out);
    fwrite(&jb3, 1, 1, out);
  }
  
  /* Output nop. */
  {
    unsigned i;
    char b = 0;
    for (i = 0; i < 4; ++i) fwrite(&b, 1, 1, out);
  }
  

  /* Read input */
  while (!feof(in)) read_word(in, out, word_size);

  /* Skip to size. */
  fseek(out, size - 1, SEEK_SET);
  char b = 0;
  fwrite(&b, 1, 1, out);

  return 0;
}

void read_word(FILE *in, FILE *out, unsigned w) {
  unsigned i;
  unsigned long x;
  fscanf(in, "%lx", &x);
  for (i = 0; i < w; ++i) {
    char b = (x >> (8*i))&0xff;
    fwrite(&b, 1, 1, out);
  }
}
