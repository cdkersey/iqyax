CXXFLAGS = -std=c++11 -O2 -DOLD_SST # -DTRANS # -g
LDLIBS = -lchdl

#score.vcd : score score.hex
#	./score

score : score.o chdl-sst-sim.o #chdl-sst.o
	$(CXX) -o score $(CXXFLAGS) $(LDFLAGS) $^ $(LDLIBS)

chdl-sst-sim.o: chdl-sst-sim.cpp chdl-sst.h
chdl-sst.o: chdl-sst.cpp chdl-sst.h

score.o : score.cpp interfaces.h muldiv.h chdl-sst.h

clean:
	rm -f score *.o *~ score.vcd score.cp
