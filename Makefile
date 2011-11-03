.PHONY: web

web:
	rm -f web/*
	make -C doc html
	cp doc/*.html web/
	mv web/sb-texinfo.html web/index.html
