.SUFFIXES: .tmpl .haml .html .sass .css

all: index.html parts per_font gist

parts: css/style.css header.haml.inc footer.haml.inc

per_font: mini-pukupuku.html css/mini-pukupuku.css cgrtr.html css/cgrtr.css

gist: 4372460

.tmpl.haml:
	./tmpl2haml.pl $< >$$(echo $< | sed 's/\.tmpl$$/.haml/')

.haml.html:
	haml -q $< $$(echo $< | sed 's/\.haml$$/.html/')

.sass.css:
	sass $< $$(echo $< | sed 's/\.sass$$/.css/')

4372460:
	git clone git://gist.github.com/4372460.git
