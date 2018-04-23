# To use this file first define the following variables in your
# makefile and then include cl.mk.
#
# PACKAGE_NAME ------- The full name of the CL package
# PACKAGE_NICKNAME --- The nickname of the CL package
#                      (default: PACKAGE_NAME)
# DOC_PACKAGES ------- Names of packages to document
#                      (default: PACKAGE_NAME)
# BINS --------------- Names of binaries to build
# TEST_ARTIFACTS ----- Name of dependencies for testing
# LISP_DEPS ---------- Packages require to build CL package
# TEST_LISP_DEPS ----- Packages require to build CL test package
# BIN_TEST_DIR ------- Directory holding command-line tests
# BIN_TESTS ---------- List of command line tests
# LONG_BIN_TESTS ----- List of longer running command line tests
#                      Used by the `real-check' target.

.PHONY: test-artifacts check-testbot check real-check clean more-clean real-clean Dockerfile

.SECONDARY:

# Set default values of PACKAGE_NICKNAME
PACKAGE_NICKNAME ?= $(PACKAGE_NAME)
DOC_PACKAGES ?= $(PACKAGE_NAME)

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

LISP_DEPS ?=				\
	$(wildcard *.lisp) 		\
	$(wildcard src/*.lisp)

# Default lisp to build manifest file.
LISP ?= ccl
ifneq (,$(findstring sbcl, $(LISP)))
ifeq ("$(SBCL_HOME)","")
LISP_HOME = SBCL_HOME=$(dir $(shell which $(LISP)))../lib/sbcl
endif
endif
REPL_STARTUP ?= ()

ifneq ($(LISP_STACK),)
ifneq (,$(findstring sbcl, $(LISP)))
LISP_FLAGS = --dynamic-space-size $(LISP_STACK) --no-userinit --no-sysinit
else
ifneq (,$(findstring ecl, $(LISP)))
# TODO: Figure out how to set --heap-size appropriately.
LISP_FLAGS = --norc
else
LISP_FLAGS = --stack-size $(LISP_STACK) --quiet --no-init
endif
endif
else
ifneq (,$(findstring sbcl, $(LISP)))
LISP_FLAGS = --no-userinit --no-sysinit
else
ifneq (,$(findstring ecl, $(LISP)))
LISP_FLAGS = --norc
else
LISP_FLAGS = --quiet --no-init
endif
endif
endif

ifneq ($(GT),)
LISP_FLAGS += --eval "(push :GT *features*)"
endif

all: $(addprefix bin/, $(BINS))

ifneq ($(GT),)
qlfile: .ci/qlfile.grammatech
	cp $< $@
else
qlfile: .ci/qlfile.external
	cp $< $@
endif

ifeq ($(USER_QUICK_LISP),quicklisp)
# If we're using qlot to grab all dependencies and *not* using the
# user's quicklisp, then we've USER_QUICK_LISP equal to
# "$(pwd)/quicklisp" and we'll use qlot:with-local-quicklisp to
# install everything into that new quicklisp location.  We then use
# $(USER_QUICK_LISP) for all quicklisp operations moving forward.
$(MANIFEST): qlfile
	$(LISP_HOME) $(LISP) $(LISP_FLAGS) --load $(QUICK_LISP)/setup.lisp \
		--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
		--eval '(ql:quickload :qlot)' \
		--eval '(qlot:install :$(PACKAGE_NAME))' \
		--eval '(qlot:quickload :$(PACKAGE_NAME))' \
		--eval '(qlot:with-local-quicklisp ("$(USER_QUICK_LISP)") (ql:register-local-projects))' \
		--eval '#+sbcl (exit) #+ccl (quit)'
else
$(MANIFEST): qlfile
	for dependency in $$(awk '{print $$3}' qlfile);do \
	base=$(USER_QUICK_LISP)/local-projects/$$(basename $$dependency .git); \
	[ -d $$base ] || git clone $$dependency $$base; \
	done
	$(LISP_HOME) $(LISP) $(LISP_FLAGS) --load $(USER_QUICK_LISP)/setup.lisp \
		--eval '(ql:register-local-projects)' \
		--eval '#+sbcl (exit) #+ccl (quit)'
endif

bin/%: $(LISP_DEPS) $(MANIFEST)
	@rm -f $@
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LISP) $(LISP_FLAGS) \
	--load $(USER_QUICK_LISP)/setup.lisp \
	--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	--eval '(ql:quickload :$(PACKAGE_NAME))' \
	--eval '(setf $(PACKAGE_NAME)::*lisp-interaction* nil)' \
	--eval '(asdf:make :$(PACKAGE_NAME)/$* :type :program :monolithic t)' \
	--eval '(quit)'


# Test executable
BINS += $(PACKAGE_NICKNAME)-test
BINS += $(PACKAGE_NICKNAME)-testbot

TEST_LISP_DEPS ?= $(wildcard test/src/*.lisp)
TEST_LISP_LIBS += $(PACKAGE_NAME)/test

bin:
	mkdir -p $@

bin/$(PACKAGE_NICKNAME)-test: $(TEST_LISP_DEPS) $(LISP_DEPS) $(MANIFEST) | bin
	@rm -f $@
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LISP) $(LISP_FLAGS) \
	--load $(USER_QUICK_LISP)/setup.lisp \
	--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	--eval '(ql:quickload :$(PACKAGE_NAME)/run-test)' \
	--eval '(setf $(PACKAGE_NAME)::*lisp-interaction* nil)' \
	--eval '(asdf:make :$(PACKAGE_NAME)/run-test :type :program :monolithic t)' \
	--eval '(quit)'

bin/$(PACKAGE_NICKNAME)-testbot: $(TEST_LISP_DEPS) $(LISP_DEPS) $(MANIFEST) | bin
	@rm -f $@
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LISP) $(LISP_FLAGS) \
	--load $(USER_QUICK_LISP)/setup.lisp \
	--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	--eval '(ql:quickload :$(PACKAGE_NAME)/run-testbot-test)' \
	--eval '(setf $(PACKAGE_NAME)::*lisp-interaction* nil)' \
	--eval '(asdf:make :$(PACKAGE_NAME)/run-testbot-test :type :program :monolithic t)' \
	--eval '(quit)'


## Testing
TEST_ARTIFACTS ?=

test-artifacts: $(TEST_ARTIFACTS)

unit-check: bin/$(PACKAGE_NICKNAME)-test test-artifacts | bin
	@$<

check: unit-check bin-check

real-check: check long-bin-check

unit-check-testbot: bin/$(PACKAGE_NICKNAME)-testbot test-artifacts | bin
	@$<

check-testbot: unit-check-testbot bin-check


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

repl: $(USER_QUICK_LISP)/setup.lisp
	$(LISP_HOME) $(LISP) $(LISP_FLAGS)			\
	--load $<						\
	--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	--eval '(ql:quickload :$(PACKAGE_NAME))'		\
	--eval '(in-package :$(PACKAGE_NAME))'			\
	--eval '$(REPL_STARTUP)'

repl-test: $(USER_QUICK_LISP)/setup.lisp test-artifacts
	$(LISP_HOME) $(LISP) $(LISP_FLAGS)			\
	--load $<						\
	--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	--eval '(ql:quickload :repl)'				\
	--eval '(ql:quickload :$(PACKAGE_NAME))'		\
	--eval '(ql:quickload :$(PACKAGE_NAME)-test)'		\
	--eval '(in-package :$(PACKAGE_NAME)-test)'		\
	--eval '$(REPL_STARTUP)'


## Command-line testing.
BIN_TEST_DIR ?= test/bin

PASS=\e[1;1m\e[1;32mPASS\e[1;0m
FAIL=\e[1;1m\e[1;31mFAIL\e[1;0m
check/%: $(BIN_TEST_DIR)/% $(addprefix bin/, $(BINS))
	@if ./$< >/dev/null 2>/dev/null;then \
	printf "$(PASS)\t\e[1;1m%s\e[1;0m\n" $*; exit 0; \
	else \
	printf "$(FAIL)\t\e[1;1m%s\e[1;0m\n" $*; exit 1; \
	fi

desc/%: check/%
	@$(BIN_TEST_DIR)/$* -d

bin-check: $(addprefix check/, $(BIN_TESTS))
bin-check-desc: $(addprefix desc/, $(BIN_TESTS))

long-bin-check: $(addprefix check/, $(LONG_BIN_TESTS))
long-bin-check-desc: $(addprefix desc/, $(LONG_BIN_TESTS))


## Docker file creation convenience target.
ifndef OS
ifneq ("$(shell which lsb_release 2>/dev/null)","")
# Use lsb_release to get the name of the Linux distribution.
OS=$(shell lsb_release -a|grep 'Distributor ID:'|sed 's/Distributor ID:[[:space:]]\+//'|tr 'A-Z' 'a-z' 2>/dev/null)
else
# Assume that if lsb_release is not installed we're on Arch Linux.
OS=arch
endif
endif

# This creates a Dockerfile which may be used to build a local image
# using the current directory instead of checking the repo out from
# git.  After running this command run the following to build the
# image.
#
#     Docker build -t my-local-image -f Dockerfile .
#
# You can then run the image as your would any other, e.g. as follows.
#
#     dr my-local-image
#
# or
#
#     docker run --net=host -e LOCAL_USER=root -it my-local-image
#
Dockerfile: Dockerfile.$(OS)
	cp $< $@


## Cleaning
clean:
	rm -f bin/$(PACKAGE_NICKNAME)-test bin/$(PACKAGE_NICKNAME)-testbot $(addprefix bin/, $(BINS))
	rm -f $(TEST_ARTIFACTS)

more-clean: clean
	find . -type f -name "*.fasl" -exec rm {} \+
	find . -type f -name "*.lx32fsl" -exec rm {} \+
	find . -type f -name "*.lx64fsl" -exec rm {} \+
	make -C doc clean

real-clean: more-clean
	rm -f qlfile qlfile.lock Dockerfile
	rm -rf quicklisp system-index.txt


## Documentation
doc: api
	make -C doc

api: doc/include/sb-texinfo.texinfo

oparen=(
cparen=)
LOADS=$(addprefix $(cparen)$(oparen)ql:quickload :, $(DOC_PACKAGES))

doc/include/sb-texinfo.texinfo: $(LISP_DEPS) $(wildcard software/*.lisp)
	SBCL_HOME=$(dir $(shell which sbcl))../lib/sbcl sbcl --load $(USER_QUICK_LISP)/setup.lisp \
	--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	--eval '(progn (list $(LOADS) $(cparen))' \
	--script .ci/.generate-api-docs includes $(DOC_PACKAGES)

gh-pages: doc
	rsync -aruv doc/$(PACKAGE_NAME)/ . --exclude .gitignore
