.SUFFIXES: .tmpl .haml .html .sass .css
.PHONY: all

all: 4372460 index.html parts per_font gist

parts: css/style.css header.haml.inc footer.haml.inc

per_font: mini-pukupuku.html css/mini-pukupuku.css cgrtr.html css/cgrtr.css

gist: 4372460

.tmpl.haml:
	./tmpl2haml.pl $< >$(basename $<).haml

.haml.html:
	haml -q $< $(basename $<).html

.sass.css:
	sass $< $(basename $<).css

4372460:
	git clone git://gist.github.com/4372460.git
