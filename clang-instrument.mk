.PHONY:
.PRECIOUS:

# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif

## Initial Compiler and Flags
CC ?= clang
CFLAGS ?= -O0

all: bin/clang-instrument


## Executables

# Use buildapp as the lisp compiler.
LC ?= buildapp

# You can set this as an environment variable to point to an alternate
# quicklisp install location.  If you do, ensure that it ends in a "/"
# character, and that you use the $HOME variable instead of ~.
QUICK_LISP?=$(HOME)/quicklisp/
ifeq "$(wildcard $(QUICK_LISP)/setup.lisp)" ""
$(warning $(QUICK_LISP) does not appear to be a valid quicklisp install)
$(error Please point QUICK_LISP to your quicklisp installation)
endif

MANIFEST_FILE=$(QUICK_LISP)/local-projects/system-index.txt
LISP_LIBS+= clang-instrument
LC_LIBS:=$(addprefix --load-system , $(LISP_LIBS))
LOADED_LIBS_TMP:=$(addprefix $(QUICK_LISP)/local-projects/, $(LISP_LIBS))
LOADED_LIBS:=$(LOADED_LIBS_TMP:=.loaded)

# Flags to buildapp
QUIT=(lambda (error hook-value)
QUIT+=(declare (ignorable hook-value))
QUIT+=(format *error-output* \"ERROR: ~a~%\" error)
QUIT+=\#+sbcl (sb-ext:exit :code 2) \#+ccl (quit 2))
LCFLAGS=--manifest-file $(MANIFEST_FILE) \
	--asdf-tree $(QUICK_LISP)/dists/quicklisp/software \
	--eval "(setf *debugger-hook* $(QUIT))"

ifneq ($(LISP_STACK),)
LCFLAGS+= --dynamic-space-size $(LISP_STACK)
endif

# Default lisp to build manifest file.
SBCL ?= sbcl
ifeq ("$(LISP)","")
ifeq ("$(SBCL_HOME)","")
SBCL_HOME=$(dir $(shell which $(SBCL)))../lib/sbcl
endif
LISP = SBCL_HOME=$(SBCL_HOME) $(SBCL)
endif
LISP ?= sbcl
ifneq (,$(findstring sbcl, $(LISP)))
LISP_FLAGS = --no-userinit --no-sysinit
else
LISP_FLAGS = --quiet --no-init
endif

$(MANIFEST_FILE): $(wildcard $(QUICK_LISP)/local-projects/*/*.asd)
	$(LISP) $(LISP_FLAGS) --load $(QUICK_LISP)/setup.lisp --eval '(ql:register-local-projects)' \
          --eval "#+sbcl (exit) #+ccl (quit)"

%.loaded:
	$(LISP) $(LISP_FLAGS) --load $(QUICK_LISP)/setup.lisp --eval '(ql:quickload :$(notdir $*))' \
          --eval "#+sbcl (exit) #+ccl (quit)"
	touch $@

bin/clang-instrument: src/clang-instrument.lisp $(LOADED_LIBS) $(MANIFEST_FILE)
	CC=$(CC) $(LC) $(LCFLAGS) $(LC_LIBS) --output $@ --entry "ci:main"


# Documentation
clang-instrument.1.gz: README.md
	pandoc -s -t man $^ -o $<.tmp
	gzip -9 < $<.tmp > $@

index.html: README.md
	pandoc -s -t html $< -o $@


# Administrative stuff
clean:
	@rm -f bin/clang-instrument
	@rm -f src/*.fasl
	@rm -f *{.md.tmp,.html,.1.gz}
