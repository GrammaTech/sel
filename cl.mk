# To use this file first define the following variables in your
# makefile and then include cl.mk.
#
# PACKAGE_NAME ------- The full name of the CL package
# PACKAGE_NICKNAME --- The nickname of the CL package
#                      (default: package name)
# PACKAGE_NAME_FIRST - The first package name to require
#                      (default: package name)
# BINS --------------- Names of binaries to build with buildapp
# TEST_ARTIFACTS ----- Name of dependencies for testing
# LISP_DEPS ---------- Packages require to build CL package
# TEST_LISP_DEPS ----- Packages require to build CL test package

.PHONY: test-artifacts check-testbot check clean more-clean real-clean

.SECONDARY:

# Set default values of PACKAGE_NICKNAME/PACKAGE_NAME_FIRST
PACKAGE_NICKNAME ?= $(PACKAGE_NAME)
PICKAGE_NAME_FIRST ?= $(PACKAGE_NAME)

# Use buildapp as the lisp compiler.
LC ?= buildapp

# You can set this as an environment variable to point to an alternate
# quicklisp install location.  If you do, ensure that it ends in a "/"
# character, and that you use the $HOME variable instead of ~.
INSTDIR ?= /usr/synth
QUICK_LISP ?= $(INSTDIR)/quicklisp/

# This variable allows users to do everything using their own
# quicklisp directory instead of using qlot.
ifeq ($(USER_QUICK_LISP),)
USER_QUICK_LISP=quicklisp
MANIFEST=system-index.txt
else
MANIFEST=$(USER_QUICK_LISP)/local-projects/system-index.txt
endif

ifeq "$(wildcard $(QUICK_LISP)/setup.lisp)" ""
$(warning $(QUICK_LISP) does not appear to be a valid quicklisp install)
$(error Please point QUICK_LISP to your quicklisp installation)
endif

LISP_LIBS += $(PACKAGE_NAME)
LC_LIBS:=$(addprefix --load-system , $(LISP_LIBS))
LOADED_LIBS:=$(addprefix $(USER_QUICK_LISP)/local-projects/, $(LISP_LIBS:=.loaded))

LISP_DEPS ?=				\
	$(wildcard *.lisp) 		\
	$(wildcard src/*.lisp)

# Flags to buildapp
LCFLAGS=--manifest-file $(MANIFEST) \
	--asdf-tree $(USER_QUICK_LISP)/dists/quicklisp/software

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

all: $(addprefix bin/, $(BINS))

# In this target we require :$(PACKAGE_NAME_FIRST) instead of
# :$(PACKAGE_NAME) because the later may depend on the former causing
# an error if :$(PACKAGE_NAME_FIRST) is not found.
PACKAGE_NAME_FIRST ?= $(PACKAGE_NAME)
ifeq ($(USER_QUICK_LISP),quicklisp)
$(MANIFEST): qlfile
	$(LISP_HOME) $(LISP) $(LISP_FLAGS) --load $(QUICK_LISP)/setup.lisp \
		--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
		--eval '(ql:quickload :qlot)' \
		--eval '(qlot:install :$(PACKAGE_NAME_FIRST))' \
		--eval '(qlot:quickload :$(PACKAGE_NAME_FIRST))' \
		--eval '(qlot:with-local-quicklisp (:$(PACKAGE_NAME_FIRST)) (ql:register-local-projects))' \
		--eval '#+sbcl (exit) #+ccl (quit)'
else
$(MANIFEST):
	$(LISP_HOME) $(LISP) $(LISP_FLAGS) --load $(USER_QUICK_LISP)/setup.lisp \
		--eval '(ql:register-local-projects)' \
		--eval '#+sbcl (exit) #+ccl (quit)'
endif

$(USER_QUICK_LISP)/local-projects/%.loaded: | $(MANIFEST)
	$(LISP_HOME) $(LISP) $(LISP_FLAGS) --load $(USER_QUICK_LISP)/setup.lisp \
		--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
		--eval '(ql:quickload :$(notdir $*))' \
		--eval "#+sbcl (exit) #+ccl (quit)"
	touch $@

bin/%: $(LISP_DEPS) $(LOADED_LIBS) $(MANIFEST)
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LC) $(LCFLAGS) $(LC_LIBS) --output $@ --entry "$(PACKAGE_NICKNAME):$*"


# Test executable
BINS += $(PACKAGE_NICKNAME)-test
BINS += $(PACKAGE_NICKNAME)-testbot

TEST_LISP_DEPS ?= $(wildcard test/src/*.lisp)
TEST_LISP_LIBS += $(PACKAGE_NAME)-test
TEST_LC_LIBS:=$(addprefix --load-system , $(TEST_LISP_LIBS))
TEST_LOADED_LIBS:=$(addprefix $(USER_QUICK_LISP)/local-projects/, $(TEST_LISP_LIBS:=.loaded))

bin/$(PACKAGE_NICKNAME)-test: $(TEST_LISP_DEPS) $(LISP_DEPS) $(TEST_LOADED_LIBS) $(MANIFEST)
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LC) $(LCFLAGS) $(TEST_LC_LIBS) --output $@ --entry "$(PACKAGE_NICKNAME)-test:run-batch"

bin/$(PACKAGE_NICKNAME)-testbot: $(TEST_LISP_DEPS) $(LISP_DEPS) $(TEST_LOADED_LIBS) $(MANIFEST)
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LC) $(LCFLAGS) $(TEST_LC_LIBS) --output $@ --entry "$(PACKAGE_NICKNAME)-test:run-testbot"


## Testing
TEST_ARTIFACTS ?=

test-artifacts: $(TEST_ARTIFACTS)

check: bin/$(PACKAGE_NICKNAME)-test test-artifacts
	@$<

check-testbot: bin/$(PACKAGE_NICKNAME)-testbot test-artifacts
	@$<


## Interactive testing
SWANK_PORT ?= 4005
swank: $(USER_QUICK_LISP)/setup.lisp
	$(LISP_HOME) $(LISP) $(LISP_FLAGS)			\
	--load $<						\
	--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	--eval '(ql:quickload :swank)'				\
	--eval '(ql:quickload :$(PACKAGE_NAME))'		\
	--eval '(in-package :$(PACKAGE_NAME))'			\
	--eval '(swank:create-server :port $(SWANK_PORT) :style :spawn :dont-close t)'

swank-test: $(USER_QUICK_LISP)/setup.lisp test-artifacts
	$(LISP_HOME) $(LISP) $(LISP_FLAGS)			\
	--load $<						\
	--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	--eval '(ql:quickload :swank)'				\
	--eval '(ql:quickload :$(PACKAGE_NAME))'		\
	--eval '(ql:quickload :$(PACKAGE_NAME)-test)'		\
	--eval '(in-package :$(PACKAGE_NAME)-test)'		\
	--eval '(swank:create-server :port $(SWANK_PORT) :style :spawn :dont-close t)'

clean:
	rm -f bin/$(PACKAGE_NICKNAME)-test bin/$(PACKAGE_NICKNAME)-testbot $(addprefix bin/, $(BINS))
	rm -f $(TEST_ARTIFACTS)

more-clean: clean
	find . -type f -name "*.fasl" -exec rm {} \+
	find . -type f -name "*.lx32fsl" -exec rm {} \+
	find . -type f -name "*.lx64fsl" -exec rm {} \+

real-clean: more-clean
	find . -type f -name "*.loaded" -exec rm {} \+
	rm -f qlfile.lock $(MANIFEST)
	rm -rf quicklisp system-index.txt
