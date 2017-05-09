SHELL=bash

.PHONY: testbot-check check doc clean

# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif

# Use buildapp as the lisp compiler.
LC ?= buildapp

# You can set this as an environment variable to point to an alternate
# quicklisp install location.  If you do, ensure that it ends in a "/"
# character, and that you use the $HOME variable instead of ~.
INSTDIR ?= /usr/synth
QUICK_LISP ?= $(INSTDIR)/quicklisp/

ifeq "$(wildcard $(QUICK_LISP)/setup.lisp)" ""
$(warning $(QUICK_LISP) does not appear to be a valid quicklisp install)
$(error Please point QUICK_LISP to your quicklisp installation)
endif

LISP_LIBS+= software-evolution
LC_LIBS:=$(addprefix --load-system , $(LISP_LIBS))
LOADED_LIBS:=$(addprefix quicklisp/local-projects/, $(LISP_LIBS:=.loaded))

LISP_DEPS =				\
	$(wildcard *.lisp) 		\
	$(wildcard software/*.lisp)	\
	$(wildcard src/*.lisp)		\
	$(wildcard utility/*.lisp)

# Flags to buildapp
LCFLAGS=--manifest-file system-index.txt \
	--asdf-tree quicklisp/dists/quicklisp/software

ifneq ($(LISP_STACK),)
LCFLAGS+= --dynamic-space-size $(LISP_STACK)
endif

# Default lisp to build manifest file.
LISP ?= ccl
ifneq (,$(findstring sbcl, $(LISP)))
ifeq ("$(SBCL_HOME)","")
LISP_HOME = SBCL_HOME=$(dir $(shell which $(LISP)))../lib/sbcl
endif
endif
ifneq (,$(findstring sbcl, $(LISP)))
LISP_FLAGS = --no-userinit --no-sysinit
else
LISP_FLAGS = --quiet --no-init
endif

all: bin/clang-instrument

system-index.txt: qlfile
	$(LISP_HOME) $(LISP) $(LISP_FLAGS) --load $(QUICK_LISP)/setup.lisp \
		--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
		--eval '(ql:quickload :qlot)' \
		--eval '(qlot:install :software-evolution)' \
		--eval '(qlot:quickload :software-evolution)' \
		--eval '(qlot:with-local-quicklisp (:software-evolution) (ql:register-local-projects))' \
		--eval '#+sbcl (exit) #+ccl (quit)'

quicklisp/local-projects/%.loaded: | system-index.txt
	$(LISP_HOME) $(LISP) $(LISP_FLAGS) --load quicklisp/setup.lisp \
		--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
		--eval '(ql:quickload :$(notdir $*))' \
		--eval "#+sbcl (exit) #+ccl (quit)"
	touch $@

bin/clang-instrument: $(LISP_DEPS) $(LOADED_LIBS) system-index.txt
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LC) $(LCFLAGS) $(LC_LIBS) --output $@ --entry "se:clang-instrument"


## Documentation
doc:
	make -C doc


# Test executable
TEST_LISP_DEPS=$(wildcard test/src/*.lisp)
TEST_LISP_LIBS+= software-evolution-test
TEST_LC_LIBS:=$(addprefix --load-system , $(TEST_LISP_LIBS))
TEST_LOADED_LIBS:=$(addprefix quicklisp/local-projects/, $(TEST_LISP_LIBS:=.loaded))

bin/se-test: $(TEST_LISP_DEPS) $(LISP_DEPS) $(TEST_LOADED_LIBS) system-index.txt
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LC) $(LCFLAGS) $(TEST_LC_LIBS) --output $@ --entry "se-test:batch-test"

bin/se-testbot-test: $(TEST_LISP_DEPS) $(LISP_DEPS) $(TEST_LOADED_LIBS) system-index.txt
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LC) $(LCFLAGS) $(TEST_LC_LIBS) --output $@ --entry "se-test:testbot-test"


## Testing
test/etc/gcd/gcd: test/etc/gcd/gcd.c
	$(CC) $< -o $@

test/etc/gcd/gcd.s: test/etc/gcd/gcd.c
	$(CC) $< -S -o $@

TEST_ARTIFACTS = \
	test/etc/gcd/gcd \
	test/etc/gcd/gcd.s

check: bin/se-test $(TEST_ARTIFACTS)
	@$<

testbot-check: bin/se-testbot-test $(TEST_ARTIFACTS)
	@$<


## Interactive testing
SWANK_PORT ?= 4005
swank: quicklisp/setup.lisp
	$(LISP_HOME) $(LISP) $(LISP_FLAGS)			\
	--load $<						\
	--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	--eval '(ql:quickload :swank)'				\
	--eval '(ql:quickload :software-evolution)'		\
	--eval '(in-package :software-evolution)'		\
	--eval '(swank:create-server :port $(SWANK_PORT) :style :spawn :dont-close t)'

swank-test: quicklisp/setup.lisp $(TEST_ARTIFACTS)
	$(LISP_HOME) $(LISP) $(LISP_FLAGS)			\
	--load $<						\
	--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	--eval '(ql:quickload :swank)'				\
	--eval '(ql:quickload :software-evolution)'		\
	--eval '(ql:quickload :software-evolution-test)'	\
	--eval '(in-package :software-evolution-test)'		\
	--eval '(swank:create-server :port $(SWANK_PORT) :style :spawn :dont-close t)'

clean:
	rm -f bin/se-test bin/se-testbot-test bin/clang-instrument
	rm -f $(TEST_ARTIFACTS)

more-clean: clean
	find . -type f -name "*.fasl" -exec rm {} \+
	find . -type f -name "*.lx32fsl" -exec rm {} \+

real-clean: more-clean
	find . -type f -name "*.loaded" -exec rm {} \+
	rm -f qlfile.lock system-index.txt
	rm -rf quicklisp
