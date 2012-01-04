all: soft-ev

LISP_FILES = 			\
	package.lisp		\
	utility.lisp		\
	software.lisp		\
	software/asm.lisp	\
	software/elf.lisp	\
	software/lisp.lisp	\
	evolution.lisp		\
	executable.lisp

compile: $(LISP_FILES:.lisp=.fasl)

%.fasl: %.lisp
	sbcl \
		--noinform \
		--non-interactive \
		--eval '(require :software-evolution)' \
		--eval "(compile-file \"$*.lisp\")" \
		--eval '(sb-ext:quit)'

soft-ev: $(LISP_FILES)
	buildapp \
		--output soft-ev \
		--eval '(load "~/.quicklisp/setup.lisp")' \
		--asdf-path ~/.asdf/ \
		--load-system software-evolution \
		--entry software-evolution:main

test/gcd/lisp-runner: test/gcd/lisp-runner.lisp
	buildapp \
		--output test/gcd/lisp-runner \
		--load test/gcd/lisp-runner.lisp \
		--entry main

check: $(LISP_FILES) test/gcd/lisp-runner
	sbcl \
		--noinform \
		--eval '(require :software-evolution)' \
		--eval '(load "test/tests.lisp")' \
		--eval '(in-package :software-evolution)' \
		--eval '(format t "~&~S~%" (software-evolution-test))' \
		--eval '(sb-ext:quit)'

clean:
	rm -f *.fasl software/*.fasl evolution/*.fasl test/*.fasl test/gcd/*.fasl soft-ev test/gcd/lisp-runner
