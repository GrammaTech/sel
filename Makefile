.PHONY: web

web:
	rm -rf web
	mkdir web
	make -C doc html
	cp doc/*.html web/
	mv web/sb-texinfo.html web/index.html

gh-pages: web
	git checkout gh-pages
	cp web/* .
	git commit -a -c master
	rm -rf web
	git checkout -f master

