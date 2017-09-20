%.s: %.cpp
	g++ -std=c++11 -S -o $@ $<

foo: foo.s bar.s
	g++ $^ -o $@

clean:
	rm -f *.s *.o *.ii foo
