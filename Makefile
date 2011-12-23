all: soft-ev

LISP_FILES = 		\
	package.lisp	\
	util.lisp	\
	genomes.lisp	\
	soft.lisp	\
	softs/soft-asm.lisp	\
	softs/soft-elf.lisp	\
	ev.lisp		\
	main.lisp

compile: $(LISP_FILES:.lisp=.fasl)

%.fasl: %.lisp
	sbcl \
		--noinform \
		--non-interactive \
		--eval '(require :soft-ev)' \
		--eval "(compile-file \"$*.lisp\")" \
		--eval '(sb-ext:quit)'

soft-ev: $(LISP_FILES)
	buildapp \
		--output soft-ev \
		--eval '(load "~/.quicklisp/setup.lisp")' \
		--asdf-path ~/.asdf/ \
		--load-system soft-ev \
		--entry soft-ev:main

check: $(LISP_FILES)
	sbcl \
		--noinform \
		--eval '(require :soft-ev)' \
		--eval '(load "tests.lisp")' \
		--eval '(in-package :soft-ev)' \
		--eval '(format t "~&~S" (soft-ev-test))' \
		--eval '(sb-ext:quit)'

clean:
	rm -f *.fasl softs/*.fasl soft-ev
