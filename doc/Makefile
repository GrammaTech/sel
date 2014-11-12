PROJECT=software-evolution

all: info html

info: $(PROJECT).texi
	makeinfo $<

html: $(PROJECT).texi
	makeinfo --html $<

clean:
	rm -f *.info;
	rm -rf $(PROJECT)/
