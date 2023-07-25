# Makefile include to compile modules.
CFLAGS = -std=c++20 -fprebuilt-module-path=.
CC = clang++
CPPM ?= cppm
MAIN ?= main.cc

%.pcm: %.$(CPPM)
	$(CC) $(CFLAGS) -x c++-module $< --precompile -o $@

main: main.o
	$(foreach pcm,$(PCMS), $(CC) $(CFLAGS) $(pcm) -c -o $(basename $(pcm)).o;)
	$(CC) $(CFLAGS) *.o -o main

main.o: $(MAIN) $(PCMS) $(IMPL_FILES)
	$(CC) $(CFLAGS) $(MAIN) -c -o main.o
	$(foreach file,$(IMPL_FILES), $(CC) $(CFLAGS) $(file) -c -o $(basename $(file)).o;)


clean:
	rm -f *.pcm *.o main *~
