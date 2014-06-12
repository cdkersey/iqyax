CXXFLAGS = -std=c++11
LDLIBS = -lchdl

score.vcd : score score.hex
	./score

score : score.o
	$(CXX) -o score $(CXXFLAGS) $(LDFLAGS) score.o $(LDLIBS)

score.o : score.cpp interfaces.h

clean:
	rm -f score score.o *~ score.vcd
