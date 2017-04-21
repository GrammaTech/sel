BASEDIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SHELL=bash

.PHONY: clang-instrument se-test se-testbot-test check doc clean

# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif

CLANG_INSTRUMENT_BIN = $(BASEDIR)/bin/clang-instrument
SE_TEST_BIN = $(BASEDIR)/bin/se-test
SE_TESTBOT_TEST_BIN = $(BASEDIR)/bin/se-testbot-test

# Use buildapp as the lisp compiler.
LC ?= buildapp

# You can set this as an environment variable to point to an alternate
# quicklisp install location.  If you do, ensure that it ends in a "/"
# character, and that you use the $HOME variable instead of ~.
QUICK_LISP ?= /usr/synth/.quicklisp/
ifeq "$(wildcard $(QUICK_LISP)/setup.lisp)" ""
$(warning $(QUICK_LISP) does not appear to be a valid quicklisp install)
$(error Please point QUICK_LISP to your quicklisp installation)
endif

QLOT_FILE=$(BASEDIR)/qlfile
QLOT_LOCK_FILE=$(BASEDIR)/qlfile.lock

MANIFEST_FILE=$(BASEDIR)/system-index.txt
LISP_LIBS+= software-evolution
LC_LIBS:=$(addprefix --load-system , $(LISP_LIBS))
LOADED_LIBS:=$(LISP_LIBS:=.loaded)

LISP_DEPS =				\
	$(wildcard *.lisp) 		\
	$(wildcard software/*.lisp)	\
	$(wildcard src/*.lisp)		\
	$(wildcard utility/*.lisp)

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

all: $(SE_TEST_BIN) $(CLANG_INSTRUMENT_BIN)

$(QLOT_LOCK_FILE): $(QLOT_FILE)
	$(LISP_HOME) $(LISP) $(LISP_FLAGS) --load $(QUICK_LISP)/setup.lisp \
		--eval '(pushnew (truename "$(BASEDIR)") ql:*local-project-directories*)' \
		--eval '(ql:quickload :qlot)' \
		--eval '(qlot:install :software-evolution)' \
		--eval '(qlot:update)' \
		--eval '#+sbcl (exit) #+ccl (quit)'

$(MANIFEST_FILE): $(QLOT_LOCK_FILE) $(wildcard $(QUICK_LISP)/local-projects/*/*.asd)
	$(LISP_HOME) $(LISP) $(LISP_FLAGS) --load $(QUICK_LISP)/setup.lisp \
		--eval '(pushnew (truename "$(BASEDIR)") ql:*local-project-directories*)' \
		--eval '(ql:register-local-projects)' \
		--eval '#+sbcl (exit) #+ccl (quit)'

%.loaded: | $(MANIFEST_FILE)
	$(LISP_HOME) $(LISP) $(LISP_FLAGS) --load $(QUICK_LISP)/setup.lisp \
		--eval '(pushnew (truename "$(BASEDIR)") ql:*local-project-directories*)' \
		--eval '(ql:quickload :$(notdir $*))' \
		--eval "#+sbcl (exit) #+ccl (quit)"
	touch $@

$(CLANG_INSTRUMENT_BIN): $(LISP_DEPS) $(LOADED_LIBS) $(MANIFEST_FILE)
	CC=$(CC) $(LISP_HOME) $(LC) $(LCFLAGS) $(LC_LIBS) --output $@ --entry "se:clang-instrument"

clang-instrument: $(CLANG_INSTRUMENT_BIN)


# Test executable
TEST_LISP_DEPS=$(wildcard test/src/*.lisp)
TEST_LISP_LIBS+= software-evolution-test
TEST_LC_LIBS:=$(addprefix --load-system , $(TEST_LISP_LIBS))
TEST_LOADED_LIBS:=$(TEST_LISP_LIBS:=.loaded)

$(SE_TEST_BIN): $(TEST_LISP_DEPS) $(LISP_DEPS) $(TEST_LOADED_LIBS) $(MANIFEST_FILE)
	CC=$(CC) $(LISP_HOME) $(LC) $(LCFLAGS) $(TEST_LC_LIBS) --output $@ --entry "se-test:batch-test"

se-test: $(SE_TEST_BIN)

$(SE_TESTBOT_TEST_BIN): $(TEST_LISP_DEPS) $(LISP_DEPS) $(TEST_LOADED_LIBS) $(MANIFEST_FILE)
	CC=$(CC) $(LISP_HOME) $(LC) $(LCFLAGS) $(TEST_LC_LIBS) --output $@ --entry "se-test:testbot-test"

se-testbot-test: $(SE_TESTBOT_TEST_BIN)


## Documentation
doc:
	make -C doc


## Testing
test/etc/gcd/gcd: test/etc/gcd/gcd.c
	$(CC) $< -o $@

test/etc/gcd/gcd.s: test/etc/gcd/gcd.c
	$(CC) $< -S -o $@

TEST_ARTIFACTS = \
	test/etc/gcd/gcd \
	test/etc/gcd/gcd.s

check: $(SE_TEST_BIN) $(TEST_ARTIFACTS)
	@$<

testbot-check: $(SE_TESTBOT_TEST_BIN) $(TEST_ARTIFACTS)
	@$<

clean:
	@find . -type f -name "*.fasl" -exec rm {} \+
	@rm -f $(SE_TEST_BIN) $(SE_TESTBOT_TEST_BIN) $(CLANG_INSTRUMENT_BIN) $(TEST_ARTIFACTS)
	@rm -f $(TEST_ARTIFACTS)

real-clean: clean
	@find . -type f -name "*.loaded" -exec rm {} \+
	@rm -f $(BASEDIR)/qlfile.lock
	@rm -f $(MANIFEST_FILE)
	@rm -rf $(BASEDIR)/quicklisp
