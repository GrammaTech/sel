# Setup for the tests
all: gcd/gcd.s gcd/gcd

gcd/gcd: gcd/gcd.c
	$(CC) $< -o $@

gcd/gcd.s: gcd/gcd.c
	$(CC) $< -S -o $@
