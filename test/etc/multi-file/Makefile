%.s: %.cpp
	clang++ -std=c++11 -S -o $@ $<

foo: foo.s bar.s
	clang++ $^ -o $@

clean:
	rm -f *.s *.o *.ii foo
