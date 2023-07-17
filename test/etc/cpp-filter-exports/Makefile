CFLAGS = -std=c++20 -fprebuilt-module-path=.
CC = clang++

%_part.o: %_part.pcm
	$(CC) $(CFLAGS) $< -c -o $@

%.pcm: %.cppm
	$(CC) $(CFLAGS) $< --precompile -o $@

main: m.pcm m-interface_part.o m-impl_part.o main.o
	$(CC) $(CFLAGS) m.pcm -c -o m.o
	$(CC) $(CFLAGS) *.o -o main

main.o: main.cc
	$(CC) $(CFLAGS) main.cc -c -o main.o

m.pcm: m-interface_part.pcm m-impl_part.pcm

clean:
	rm -f *.pcm *.o main

allclean: clean
	rm -f *~
