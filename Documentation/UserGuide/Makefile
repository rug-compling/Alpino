all: AlpinoUserGuide.html AlpinoUserGuide.pdf

AlpinoUserGuide.html: AlpinoUserGuide.txt
	asciidoc AlpinoUserGuide.txt

AlpinoUserGuide.pdf: AlpinoUserGuide.txt
	a2x -fpdf -dbook AlpinoUserGuide.txt

install:
	cp AlpinoUserGuide.html AlpinoUserGuide.pdf ../../

webinstall:
	cp AlpinoUserGuide.html AlpinoUserGuide.pdf ~/public_html/alp/Alpino/
