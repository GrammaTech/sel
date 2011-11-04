.PHONY: clean

all: html pdf info

clean:
	rm -rf include
	rm -f  *.pdf *.html *.info
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr
	rm -f document-package-sample.texinfo
	rm -f include-stamp

include-stamp: ../*.lisp ../*.asd
	sbcl \
	--eval '(let ((asdf:*central-registry* (cons #p"../" asdf:*central-registry*))) (require :sb-texinfo))' \
	--eval '(sb-texinfo:generate-includes "include/" (list :sb-texinfo) :base-package :sb-texinfo)' \
	--eval '(quit)'
	touch include-stamp

document-package-sample.texinfo: ../*.lisp ../*.asd
	sbcl \
	--eval '(let ((asdf:*central-registry* (cons #p"../" asdf:*central-registry*))) (require :sb-texinfo))' \
	--eval '(sb-texinfo:document-package :sb-texinfo :output-file "document-package-sample.texinfo")' \
	--eval '(quit)'

%.html: %.texinfo style.css include-stamp
	makeinfo --html --no-split --css-include=style.css $<

%.pdf: %.texinfo include-stamp
	texi2dvi -p $<

%.info: %.texinfo include-stamp
	makeinfo --no-split $<

html: sb-texinfo.html document-package-sample.html

pdf: sb-texinfo.pdf document-package-sample.pdf

info: sb-texinfo.info document-package-sample.info
