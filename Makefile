CXXFLAGS = -std=c++11
LDLIBS = -lchdl

#score.vcd : score score.hex
#	./score

score : score.o chdl-sst-sim.o
	$(CXX) -o score $(CXXFLAGS) $(LDFLAGS) score.o chdl-sst-sim.o $(LDLIBS)

chdl-sst-sim.o: chdl-sst-sim.cpp chdl-sst.h

score.o : score.cpp interfaces.h muldiv.h chdl-sst.h

clean:
	rm -f score *.o *~ score.vcd score.cp
