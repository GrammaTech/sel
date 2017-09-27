PROJECT=software-evolution

all: index.html info html clang-instrument.1.gz

index.html: landing.md
	pandoc $< > $@

info: $(PROJECT).texi
	makeinfo $<

html: $(PROJECT).texi
	makeinfo --html $<

clean:
	rm -f *.info;
	rm -rf $(PROJECT)/

%.md.tmp: %.md
	pandoc -s -t man $< -o $@

%.1.gz: %.md.tmp
	gzip -9 < $< > $@
