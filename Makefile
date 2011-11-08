all: soft-ev

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
