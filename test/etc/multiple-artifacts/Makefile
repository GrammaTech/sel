CC = clang

build: nested-dir/helper shared.so run_shared

nested-dir/helper: nested-dir/helper.c
	$(CC) nested-dir/helper.c -o nested-dir/helper

shared.so: shared.o
	$(CC) -shared shared.o -o shared.so
	ln -sf shared.so shared.so.link

shared.o: shared.c shared.h
	$(CC) shared.c -c

run_shared: run_shared.c
	$(CC) run_shared.c -ldl -o run_shared

binaries:
	@echo "run_shared shared.so.link nested-dir/helper"

clean:
	rm *.o
	rm *.so
	rm *.link
	rm run_shared
	rm nested-dir/helper

.PHONY=clean,binaries
