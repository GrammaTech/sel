# To use this file first define the following variables in your
# makefile and then include cl.mk.
#
# PACKAGE_NAME ------- The full name of the CL package
# PACKAGE_NICKNAME --- The nickname of the CL package
#                      (default: package name)
# PACKAGE_NAME_FIRST - The first package name to require
#                      (default: package name)
# BINS --------------- Names of binaries to build
# TEST_ARTIFACTS ----- Name of dependencies for testing
# LISP_DEPS ---------- Packages require to build CL package
# TEST_LISP_DEPS ----- Packages require to build CL test package

.PHONY: test-artifacts check-testbot check clean more-clean real-clean Dockerfile

.SECONDARY:

# Set default values of PACKAGE_NICKNAME/PACKAGE_NAME_FIRST
PACKAGE_NICKNAME ?= $(PACKAGE_NAME)
PICKAGE_NAME_FIRST ?= $(PACKAGE_NAME)

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

ifneq ($(LISP_STACK),)
ifneq (,$(findstring sbcl, $(LISP)))
LISP_FLAGS = --dynamic-space-size $(LISP_STACK) --no-userinit --no-sysinit
else
LISP_FLAGS = --stack-size $(LISP_STACK) --quiet --no-init
endif
else
ifneq (,$(findstring sbcl, $(LISP)))
LISP_FLAGS = --no-userinit --no-sysinit
else
LISP_FLAGS = --quiet --no-init
endif
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
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LISP) $(LISP_FLAGS) \
	--load $(USER_QUICK_LISP)/setup.lisp \
	--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	--eval '(ql:quickload :$(PACKAGE_NAME))' \
	--eval '(setf $(PACKAGE_NAME)::*lisp-interaction* nil)' \
	--eval '(asdf:make-build :$(PACKAGE_NAME)/$* :type :program :monolithic t)' \
	--eval '(quit)'


# Test executable
BINS += $(PACKAGE_NICKNAME)-test
BINS += $(PACKAGE_NICKNAME)-testbot

TEST_LISP_DEPS ?= $(wildcard test/src/*.lisp)
TEST_LISP_LIBS += $(PACKAGE_NAME)-test
TEST_LOADED_LIBS:=$(addprefix $(USER_QUICK_LISP)/local-projects/, $(TEST_LISP_LIBS:=.loaded))

bin:
	mkdir -p $@

bin/$(PACKAGE_NICKNAME)-test: $(TEST_LISP_DEPS) $(LISP_DEPS) $(TEST_LOADED_LIBS) $(MANIFEST) | bin
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LISP) $(LISP_FLAGS) \
	--load $(USER_QUICK_LISP)/setup.lisp \
	--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	--eval '(ql:quickload :$(PACKAGE_NAME)-test)' \
	--eval '(setf $(PACKAGE_NAME)::*lisp-interaction* nil)' \
	--eval '(asdf:make-build :$(PACKAGE_NAME)-test/test :type :program :monolithic t)' \
	--eval '(quit)'

bin/$(PACKAGE_NICKNAME)-testbot: $(TEST_LISP_DEPS) $(LISP_DEPS) $(TEST_LOADED_LIBS) $(MANIFEST) | bin
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LISP) $(LISP_FLAGS) \
	--load $(USER_QUICK_LISP)/setup.lisp \
	--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	--eval '(ql:quickload :$(PACKAGE_NAME)-test)' \
	--eval '(setf $(PACKAGE_NAME)::*lisp-interaction* nil)' \
	--eval '(asdf:make-build :$(PACKAGE_NAME)-test/testbot-test :type :program :monolithic t)' \
	--eval '(quit)'


## Testing
TEST_ARTIFACTS ?=

test-artifacts: $(TEST_ARTIFACTS)

check: bin/$(PACKAGE_NICKNAME)-test test-artifacts | bin
	@$<

check-testbot: bin/$(PACKAGE_NICKNAME)-testbot test-artifacts | bin
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
# using the current directory instead of checking BI out from git.
# After running this command run the following to build the image.
#
#     Docker build -t bug-injector-local -f Dockerfile .
#
# You can then run the image as your would any other, e.g. as follows.
#
#     dr bug-injector-local
#
# or
#
#     docker run --net=host -e LOCAL_USER=root -it bug-injector-local
#
Dockerfile: Dockerfile.$(OS)
	cat $<|sed -n '0,/^RUN mkdir -p /p;/^WORKDIR/,$$p;' \
	|sed '/RUN mkdir -p /d' \
	|sed "s|^CMD|ADD . .\n\nRUN $$(grep '^ \+make' $<)\n\n&|" > $@


## Cleaning
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
	rm -rf quicklisp
