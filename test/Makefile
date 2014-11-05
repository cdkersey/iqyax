MIPS_CC ?= mips-elf-gcc
MIPS_CXX ?= mips-elf-g++
MIPS_OBJDUMP = mips-elf-objdump
MIPS_CFLAGS = -EL -nostdlib -O2 -std=c99 -fno-builtin -mno-branch-likely -mips1\
              -Wa,-mips2
MIPS_LDFLAGS =

CFLAGS = -std=c99 -g

BIN = triesort.bin sieve-byte.bin mandelbrot.bin struct.bin partest.bin \
      vecsum.bin montepi.bin
HOST = triesort-host sieve-byte-host mandelbrot-host struct-host partest-host \
       vecsum-host montepi-host
DUMP = triesort.dump.s sieve-byte.dump.s mandelbrot.dump.s struct.dump.s \
       partest.dump.s vecsum.dump.s montepi.dump.s

all : $(BIN) $(HOST) $(DUMP)

%-host : %.c
	$(CC) $(CFLAGS) -DHOST -m32 -o $@ $<

%.bin : %.hex %.dump.s hex2bin
	./hex2bin $*.hex 0x400000 0x100000000 \
        0x`grep start $*.dump.s | sed 's/ .*//'` $*.bin

%.dump.s : %.elf
	$(MIPS_OBJDUMP) --disassemble-zeroes -M reg-names=numeric -d \
        -j .text -j .rodata $< > $@

%.hex : %.elf
	$(MIPS_OBJDUMP) --disassemble-zeroes -D -j .text -j .rodata $< | \
          grep "^ " | sed 's/[^\t]*\t//' | sed 's/\t.*//' > $@

%.elf : %.o boot.o lib.o
	$(MIPS_CC) $(MIPS_CFLAGS) -o $*.elf boot.o lib.o $*.o $(MIPS_LDFLAGS)

boot.o : boot.s
	$(MIPS_CC) $(MIPS_CFLAGS) -c boot.s

%.o : %.c lib.h
	$(MIPS_CC) $(MIPS_CFLAGS) -c $<

hex2bin: hex2bin.c
	$(CC) -o hex2bin hex2bin.c

%.elf : %.cpp
	$(MIPS_CXX) $(MIPS_CFLAGS) -o $@ $< $(MIPS_LDFLAGS)

clean:
	rm -f *.o *.elf *.hex hex2bin $(BIN) $(DUMP) $(HOST) *~
