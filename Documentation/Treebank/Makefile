all: overview.html overview.pdf

overview.html: overview.txt
	asciidoc --attribute icons --attribute\
    iconsdir=http://www.methods.co.nz/asciidoc/images/icons overview.txt

overview.pdf: overview.txt
	a2x -fpdf -dbook overview.txt

install:
	cp overview.html ../../TreebankTools.html
	cp overview.pdf ../../TreebankTools.pdf

webinstall:
	cp overview.html ~/public_html/alp/Alpino/TreebankTools.html
	cp overview.pdf ~/public_html/alp/Alpino/TreebankTools.pdf
