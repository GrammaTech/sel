.PHONY: doc api

# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif

PACKAGE_NAME = software-evolution
PACKAGE_NICKNAME = se
PACKAGE_NAME_FIRST = software-evolution-utility
LISP_DEPS =				\
	$(wildcard *.lisp) 		\
	$(wildcard src/*.lisp)		\
	$(wildcard software/*.lisp)	\
	$(wildcard utility/*.lisp)

TEST_ARTIFACTS = \
	test/etc/gcd/gcd \
	test/etc/gcd/gcd.s

BINS = clang-instrument

include cl.mk

test/etc/gcd/gcd: test/etc/gcd/gcd.c
	$(CC) $< -o $@

test/etc/gcd/gcd.s: test/etc/gcd/gcd.c
	$(CC) $< -S -o $@


## Documentation
doc: api
	make -C doc

api:
	$(LISP_HOME) $(LISP) $(LISP_FLAGS) --load $(USER_QUICK_LISP)/setup.lisp \
		--eval '(ql:quickload :cl-gendoc)' \
		--eval '(gendoc:gendoc (:output-filename "doc/api.html") (:mdf #P"./README.md") (:apiref :software-evolution :software-evolution-utility))' \
		--eval "#+sbcl (exit) #+ccl (quit)"
