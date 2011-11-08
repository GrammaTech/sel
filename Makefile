all: soft-ev

%.fasl: %.lisp
	sbcl \
		--noinform \
		--non-interactive \
		--eval '(require :soft-ev)'\
		--eval "(compile-file \"$*.lisp\")" \
		--eval '(sb-ext:quit)'

soft-ev: ev.lisp soft.lisp soft-asm.lisp soft-elf.lisp
	buildapp \
		--output soft-ev \
		--eval '(load "~/.quicklisp/setup.lisp")' \
		--asdf-path ~/.asdf/ \
		--load-system soft-ev \
		--load main.lisp \
		--entry main

clean:
	rm -f *.fasl soft-ev
