all: soft-ev

soft-ev: ev.lisp soft.lisp soft-asm.lisp soft-elf.lisp
	buildapp \
		--output soft-ev \
		--system soft-ev \
		--load main.lisp \
		--entry main
