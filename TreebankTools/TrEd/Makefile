
## if you change something for personal use only on your local machine,
## then this directory should be
## $(HOME)/.tred.d/extensions (and some of the copied files are redundant)
## The following is used to be put on the website, so that other users
## can update their Alpino extension
DST = $(HOME)/public_html/alp/Alpino/tred/

all: \
	dirs \
	$(DST)/HEADER.html \
	$(DST)/extensions.lst \
	$(DST)/alpino.zip \
	$(DST)/alpino_full.zip \
	$(DST)/alpino/package.xml \
	$(DST)/alpino/icons/alpino.png \
	$(DST)/alpino_full/package.xml \
	$(DST)/alpino_full/icons/alpino.png \

dirs:
	mkdir -p zip
	mkdir -p $(DST)/alpino/icons
	mkdir -p $(DST)/alpino_full/icons

$(DST)/HEADER.html: HEADER.html
	cp $^ $@

$(DST)/extensions.lst: extensions.lst
	cp $^ $@

$(DST)/alpino.zip: zip/alpino.zip
	cp $^ $@

$(DST)/alpino_full.zip: zip/alpino_full.zip
	cp $^ $@

$(DST)/alpino/package.xml: package_alpino.xml
	cp $^ $@

$(DST)/alpino/icons/alpino.png: alpino.png
	cp $^ $@

$(DST)/alpino_full/package.xml: package_alpino_full.xml
	cp $^ $@

$(DST)/alpino_full/icons/alpino.png: alpino.png
	cp $^ $@

zip/alpino.zip: alpino.mak alpino.png alpino2pml.xsl alpino_schema.xml contrib.mac package_alpino.xml pml2alpino.xsl pmlbackend_conf.inc
	rm -fr build $@
	mkdir -p build/contrib/alpino
	mkdir build/icons
	mkdir build/resources
	cp alpino.mak contrib.mac build/contrib/alpino
	cp alpino.png build/icons
	cp alpino2pml.xsl alpino_schema.xml pml2alpino.xsl pmlbackend_conf.inc build/resources
	cp package_alpino.xml build/package.xml
	cd build && zip -r ../$@ *
	rm -fr build

zip/alpino_full.zip: alpino.mak alpino.png alpino2pml.xsl alpino_schema_full.xml contrib.mac package_alpino_full.xml pml2alpino.xsl pmlbackend_conf.inc
	rm -fr build $@
	mkdir -p build/contrib/alpino
	mkdir build/icons
	mkdir build/resources
	cp alpino.mak contrib.mac build/contrib/alpino
	cp alpino.png build/icons
	cp alpino2pml.xsl pml2alpino.xsl pmlbackend_conf.inc build/resources
	cp alpino_schema_full.xml build/resources/alpino_schema.xml
	cp package_alpino_full.xml build/package.xml
	cd build && zip -r ../$@ *
	rm -fr build
