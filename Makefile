CXXFLAGS = -std=c++11 -O2 -DOLD_CHDL # -DTRANS # -g
LDLIBS = -lchdl

iqyax : iqyax.o chdl-mem.o #chdl-mem-sim.o
	$(CXX) -o iqyax $(CXXFLAGS) $(LDFLAGS) $^ $(LDLIBS)

chdl-mem-sim.o: chdl-mem-sim.cpp chdl-mem.h
chdl-mem.o: chdl-mem.cpp chdl-mem.h

iqyax.o : iqyax.cpp interfaces.h muldiv.h chdl-mem.h

clean:
	rm -f iqyax *.o *~ iqyax.vcd iqyax.cpr iqyax.v iqyax.nand
