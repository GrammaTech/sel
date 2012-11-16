EMACS=emacs
BATCH_EMACS=$(EMACS) --batch -Q

software-evolution.txt: software-evolution.org
	$(BATCH_EMACS) $< -f org-export-as-utf8

README: software-evolution.txt
	cp $< $@
	sed -i '/^Author:/d' $@
	sed -i '/^Date:/d' $@

clean:
	rm -f software-evolution.txt
