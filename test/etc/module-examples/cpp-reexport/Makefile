CFLAGS = -std=c++20 -fprebuilt-module-path=.
CC = clang++

%.pcm: %.cppm
	$(CC) $(CFLAGS) $< --precompile -o $@

main: main.o
	$(CC) $(CFLAGS) a.pcm -c -o a.o
	$(CC) $(CFLAGS) b.pcm -c -o b.o
	$(CC) $(CFLAGS) *.o -o main

main.o: main.cc a.pcm b.pcm
	$(CC) $(CFLAGS) main.cc -c -o main.o

clean:
	rm -f *.pcm *.o main

allclean: clean
	rm -f *~
