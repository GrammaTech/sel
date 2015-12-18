.PHONY: check doc test

# Use buildapp as the lisp compiler.
LC:=buildapp

# You can set this as an environment variable to point to an alternate
# quicklisp install location.  If you do, ensure that it ends in a "/"
# character, and that you use the $HOME variable instead of ~.
QUICK_LISP?=$(HOME)/quicklisp/
ifeq "$(wildcard $(QUICK_LISP)/setup.lisp)" ""
$(warning $(QUICK_LISP) does not appear to be a valid quicklisp install)
$(error Please point QUICK_LISP to your quicklisp installation)
endif

LISP_LIBS+= software-evolution-test
LC_LIBS:=$(addprefix --load-system , $(LISP_LIBS))

LISP_DEPS =				\
	$(wildcard *.lisp) 		\
	$(wildcard software/*.lisp)	\
	$(wildcard test/*.lisp)

# Flags to buildapp
QUIT=(lambda (error hook-value)
QUIT+=(declare (ignorable hook-value))
QUIT+=(format *error-output* \"ERROR: ~a~%\" error)
QUIT+=\#+sbcl (sb-ext:exit :code 2) \#+ccl (quit 2))
LCFLAGS=--manifest-file $(QUICK_LISP)/local-projects/system-index.txt \
	--asdf-tree $(QUICK_LISP)/dists/quicklisp/software \
	--eval "(setf *debugger-hook* $(QUIT))"

ifneq ($(LISP_STACK),)
LCFLAGS+= --dynamic-space-size $(LISP_STACK)
endif

se-test: $(LISP_DEPS)
	$(LC) $(LCFLAGS) $(LC_LIBS) --output $@ \
		--entry "software-evolution-test:batch-test"


## Documentation
doc:
	make -C doc


## Testing
test:
	@make -sC test

check: se-test test
	@./$<
