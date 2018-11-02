.PHONY: doc api

# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif

PACKAGE_NAME = software-evolution-library
PACKAGE_NICKNAME = sel
DOC_PACKAGES =	software-evolution-library		\
		software-evolution-library/utility	\
		software-evolution-library/view		\
		software-evolution-library/serapi-io	\
		software-evolution-library/ast-diff	\
		software-evolution-library/clang-diff	\
		software-evolution-library/lisp-diff	\
		software-evolution-library/rest

LISP_DEPS =				\
	$(wildcard *.lisp) 		\
	$(wildcard src/*.lisp)		\
	$(wildcard software/*.lisp)	\
	$(wildcard utility/*.lisp)	\
	$(wildcard view/*.lisp)		\
	$(wildcard ast-diff/*.lisp)	\
	$(wildcard clang-diff/*.lisp)	\
	$(wildcard lisp-diff/*.lisp)

TEST_ARTIFACTS = \
	test/etc/gcd/gcd \
	test/etc/gcd/gcd.s

BINS = clang-instrument \
	clang-diff

BIN_TEST_DIR = test/bin
BIN_TESTS =			\
	example-001-mutate	\
	example-002-evaluation

LONG_BIN_TESTS =		\
	example-003-neutral	\
	example-004-evolve

include cl.mk

test/etc/gcd/gcd: test/etc/gcd/gcd.c
	$(CC) $< -o $@

test/etc/gcd/gcd.s: test/etc/gcd/gcd.c
	gcc $< -S -masm=intel -o $@

