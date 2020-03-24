LISP ?= sbcl
QUICK_LISP ?= ../../quicklisp/

ifneq (,$(findstring sbcl, $(LISP)))
ifeq ("$(SBCL_HOME)","")
LISP_HOME = SBCL_HOME=$(dir $(shell which $(LISP)))../lib/sbcl
endif
endif


ifneq ($(LISP_HEAP),)
ifneq (,$(findstring sbcl, $(LISP)))
LISP_FLAGS = --dynamic-space-size $(LISP_HEAP) --no-userinit --no-sysinit
else
LISP_FLAGS = --heap-reserve $(LISP_HEAP)M --quiet --no-init
endif
else
ifneq (,$(findstring sbcl, $(LISP)))
LISP_FLAGS = --no-userinit --no-sysinit
else
LISP_FLAGS = --quiet --no-init
endif
endif

LISP_DEPS ?= $(wildcard *.lisp)

BINS=repair-clang example

all: $(addprefix bin/, $(BINS))

$(MANIFEST):
	$(LISP_HOME) $(LISP) $(LISP_FLAGS) --load $(QUICK_LISP)/setup.lisp \
		--eval '(ql:register-local-projects)' \
		--eval '#+sbcl (exit) #+ccl (quit)'

bin/%: $(LISP_DEPS) $(MANIFEST)
	@rm -f $@
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LISP) $(LISP_FLAGS) \
	--load $(QUICK_LISP)/setup.lisp \
	--eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	--eval '(pushnew (truename "../../") ql:*local-project-directories*)' \
	--eval '(ql:quickload :software-evolution-library)' \
	--eval '(ql:quickload :software-evolution-library/utility)' \
	--eval '(ql:quickload :software-evolution-library/view)' \
	--eval '(setf software-evolution-library::*lisp-interaction* nil)' \
	--eval '(asdf:make :$* :type :program :monolithic t)' \
	--eval '(quit)'
