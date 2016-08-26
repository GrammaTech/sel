SHELL=bash
.PHONY: check doc test tests.md auto-check

all: bin/se-test bin/clang-instrument

# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif

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
LISP_LIBS+= software-evolution
LC_LIBS:=$(addprefix --load-system , $(LISP_LIBS))
LOADED_LIBS_TMP:=$(addprefix $(QUICK_LISP)/local-projects/, $(LISP_LIBS))
LOADED_LIBS:=$(LOADED_LIBS_TMP:=.loaded)

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

bin/clang-instrument: $(LISP_DEPS) $(LOADED_LIBS) $(MANIFEST_FILE)
	CC=$(CC) $(LC) $(LCFLAGS) $(LC_LIBS) --output $@ --entry "se:clang-instrument"


# Test executable
TEST_LISP_DEPS=$(wildcard test/src/*.lisp)
TEST_LISP_LIBS+= software-evolution-test uuid cxml
TEST_LC_LIBS:=$(addprefix --load-system , $(TEST_LISP_LIBS))
TEST_LOADED_LIBS_TMP:=$(addprefix $(QUICK_LISP)/local-projects/, $(TEST_LISP_LIBS))
TEST_LOADED_LIBS:=$(LOADED_LIBS_TMP:=.loaded)

bin/se-test: $(TEST_LISP_DEPS) $(LISP_DEPS) $(TEST_LOADED_LIBS) $(MANIFEST_FILE)
	CC=$(CC) $(LC) $(LCFLAGS) $(TEST_LC_LIBS) --output $@ --entry "se-test:batch-test"

bin/se-testbot-test: $(TEST_LISP_DEPS) $(LISP_DEPS) $(TEST_LOADED_LIBS) $(MANIFEST_FILE)
	CC=$(CC) $(LC) $(LCFLAGS) $(TEST_LC_LIBS) --output $@ --entry "se-test:testbot-test"


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

check: bin/se-test $(TEST_ARTIFACTS)
	@./$<

testbot-check: bin/se-testbot-test $(TEST_ARTIFACTS)
	@./$<

# Makefile target to support automated testing.
tests.md: bin/se-test test
	echo "### $$(date +%Y-%m-%d-%H-%M-%S)" >> tests.md
	echo "REPO" >> tests.md
	echo ":   $(REPO)" >> tests.md
	echo "" >> tests.md
	echo "BRANCH" >> tests.md
	echo ":   $(BRANCH)" >> tests.md
	echo "" >> tests.md
	echo "HEAD" >> tests.md
	echo ":   $(HEAD)" >> tests.md
	echo "" >> tests.md
	make -s check 2>&1|sed 's/^/    /' >> tests.md
	echo "" >> tests.md

tests.html: tests.md
	markdown $< > $@

auto-check: tests.html

clean:
	@find . -type f -name "*.fasl" -exec rm {} \+
	@rm -f bin/{se-test,clang-instrument} $(TEST_ARTIFACTS)
