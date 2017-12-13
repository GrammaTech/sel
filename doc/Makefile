PROJECT=software-evolution-library

all: index.html CONTRIBUTING.html FAQ.html info html clang-instrument.1.gz

index.html: landing.md
CONTRIBUTING.html: ../CONTRIBUTING.md
FAQ.html: ../FAQ.md
CONTRIBUTING.html FAQ.html index.html:
	pandoc -s --css sel.css $< -o $@

info: $(PROJECT).texi
	makeinfo $<

html: $(PROJECT).texi
	makeinfo --html $<

clean:
	rm -f *.info;
	rm -rf $(PROJECT)/
	rm -rf software-evolution-library/
	find . -type f -name "*.html" -exec rm {} \+

%.md.tmp: %.md
	pandoc -s -t man $< -o $@

%.1.gz: %.md.tmp
	gzip -9 < $< > $@
