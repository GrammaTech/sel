PROJECT=software-evolution-library

all: info html clang-instrument.1.gz

info: $(PROJECT).texi
	makeinfo $<

html: $(PROJECT).texi
	makeinfo --html $<

clean:
	rm -f *.info;
	rm -rf $(PROJECT)/
	rm -rf software-evolution-library/
	find . -type f -name "*.html" -exec rm {} \+
	rm -f include/*

%.md.tmp: %.md
	pandoc -s -t man $< -o $@

%.1.gz: %.md.tmp
	gzip -9 < $< > $@
