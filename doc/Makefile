PROJECT=software-evolution-library
DESTDIR?=/usr
DATAROOTDIR?=/share
INFODIR?=$(DATAROOTDIR)/info

all: info html clang-instrument.1.gz

info: $(PROJECT).info

$(PROJECT).info: $(PROJECT).texi
	makeinfo $<

install: $(PROJECT).info
	install -m 644 $< "$(DESTDIR)$(INFODIR)"
	install-info --info-dir="$(DESTDIR)$(INFODIR)" "$(DESTDIR)$(INFODIR)/$<"

html: $(PROJECT).texi GrammaTech-CLA-SEL.pdf
	makeinfo --html $<
	cp GrammaTech-CLA-SEL.pdf software-evolution-library/

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
