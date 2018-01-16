.PHONY: web

web:
	rm -rf web
	mkdir web
	make -C doc html pdf
	cp doc/*.html doc/*.pdf web/
	cp web/sb-texinfo.html web/index.html

pages: web
	git checkout gh-pages
	cp web/* .
	git commit -a -c master
	rm -rf web
	git checkout -f master

# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif

PACKAGE_NAME = software-evolution-library
PACKAGE_NICKNAME = sel
DOC_PACKAGES =	software-evolution-library		\
		software-evolution-library/utility	\
		software-evolution-library/view

LISP_DEPS =				\
	$(wildcard *.lisp) 		\
	$(wildcard src/*.lisp)		\
	$(wildcard software/*.lisp)	\
	$(wildcard utility/*.lisp)

TEST_ARTIFACTS = \
	test/etc/gcd/gcd \
	test/etc/gcd/gcd.s

BINS = clang-instrument

include cl.mk

test/etc/gcd/gcd: test/etc/gcd/gcd.c
	$(CC) $< -o $@

test/etc/gcd/gcd.s: test/etc/gcd/gcd.c
	$(CC) $< -S -o $@


## Documentation
doc: api
	make -C doc

api: doc/include/sb-texinfo.texinfo

doc/include/sb-texinfo.texinfo: $(LISP_DEPS) $(wildcard software/*.lisp)
	$(LISP_HOME) sbcl --load $(USER_QUICK_LISP)/setup.lisp \
	--script .generate-api-docs

gh-pages: doc
	rsync -aruv doc/ . --exclude .gitignore


