.PHONY: doc api

# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif

PACKAGE_NAME = software-evolution-library
PACKAGE_NICKNAME = sel
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
		--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
		--eval '(ql:quickload :cl-gendoc)' \
		--eval '(ql:quickload :cl-ppcre)' \
		--eval '(ql:quickload :software-evolution-library)' \
		--eval '(ql:quickload :software-evolution-library/utility)' \
		--eval '(ql:quickload :software-evolution-library/view)' \
		--eval '(ql:quickload :software-evolution-library/mongo)' \
		--load .gendoc.lisp \
		--eval "#+sbcl (exit) #+ccl (quit)"

gh-pages: doc
	rsync -aruv doc/ . --exclude .gitignore
