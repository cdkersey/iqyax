CXXFLAGS = -std=c++11 -O2 -DOLD_SST # -DTRANS # -g
LDLIBS = -lchdl

iqyax : iqyax.o chdl-sst.o #chdl-sst-sim.o
	$(CXX) -o iqyax $(CXXFLAGS) $(LDFLAGS) $^ $(LDLIBS)

chdl-sst-sim.o: chdl-sst-sim.cpp chdl-sst.h
chdl-sst.o: chdl-sst.cpp chdl-sst.h

iqyax.o : iqyax.cpp interfaces.h muldiv.h chdl-sst.h

clean:
	rm -f iqyax *.o *~ iqyax.vcd iqyax.cpr iqyax.v iqyax.nand
