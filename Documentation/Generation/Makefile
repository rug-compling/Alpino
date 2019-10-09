# Note: Apache FOP is required to convert XSL-FO output to PDF. Using
# FOP is sometimes a bit hit-or-miss, but FOP 0.94 seems to work fine
# for this input.

all: adt.html cg.html design.html adt.pdf cg.pdf design.pdf

%.html: %.txt
	asciidoc --attribute icons --attribute\
	    iconsdir=http://www.methods.co.nz/asciidoc/images/icons $<

%.xml: %.txt
	asciidoc -b docbook $<

%.fo: %.xml fo.xsl
	xsltproc -o $@ fo.xsl $<

%.pdf: %.fo
	fop11 $< $@

clean:

realclean:
	rm -f adt.html cg.html design.html adt.xml adt.fo

