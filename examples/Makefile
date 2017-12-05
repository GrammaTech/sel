# Use buildapp as the lisp compiler
LC:=buildapp

# You can set this as an environment variable to point to an alternate
# quicklisp install location.  If you do, ensure that it ends in a "/"
# character, and that you use the $HOME variable instead of ~.
QUICK_LISP?=$(HOME)/quicklisp/
ifeq "$(wildcard $(QUICK_LISP)/setup.lisp)" ""
$(warning $(QUICK_LISP) does not appear to be a valid quicklisp install)
$(error Please point QUICK_LISP to your quicklisp installation)
endif

# Lisp Libraries to compile into executables
#
#  Set the LISP_LIBS env variable to load another lisp system into the
#  compiled optimization executables.
#
LISP_LIBS+=software-evolution-library software-evolution-library-command-line
LC_LIBS:=$(addprefix --load-system , $(LISP_LIBS))

# Flags to buildapp
QUIT=(lambda (error hook-value)
QUIT+=(declare (ignorable hook-value))
QUIT+=(format *error-output* \"ERROR: ~a~%\" error)
QUIT+=\#+sbcl (sb-ext:exit :code 2) \#+ccl (quit 2))
LCFLAGS=--manifest-file $(QUICK_LISP)/local-projects/system-index.txt \
	--asdf-tree $(QUICK_LISP)/dists/quicklisp/software \
	--eval "(setf *debugger-hook* $(QUIT))" \
	$(LC_LIBS)

ifneq ($(LISP_STACK),)
LCFLAGS+= --dynamic-space-size $(LISP_STACK)
endif

# Compiled lisp executables
LISP_EXES=neutral repair

all: $(LISP_EXES)
.PHONY:  clean

%: %.lisp %.asd
	$(LC) $(LCFLAGS) --load-system $* --output $@ --entry "$*:main"

clean:
	rm -f $(LISP_EXES) **/*.fasl **/*.lx32fsl
