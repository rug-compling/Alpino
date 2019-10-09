PWD:=${shell pwd}

PLATFORM=${shell echo "prolog_flag(host_type,X), write(X), nl, fail."\
     | ../../hdrug -notk 2>/dev/null}
SPLIB=${shell echo "library_directory(X), write(X), nl, fail."\
     | ../../hdrug -notk 2>/dev/null}

TARGET=../$(PLATFORM)/hdrug

all: hdrug.exe data

hdrug.sh:
	$(MAKE) -f Makefile.bin

data:
	-mkdir -p Applications
	-mkdir -p Tcl
	-mkdir -p Tex
	-mkdir -p Manual
	cd ../../Tcl;\
           tar cf - --exclude=CVS . | ( cd $(PWD)/Tcl ; tar xvf - )
	cd ../../Tex ;\
           tar cf - --exclude=CVS . | ( cd $(PWD)/Tex ; tar xvf - )
	cd ../../Applications ;\
           tar cf - --exclude=CVS . | ( cd $(PWD)/Applications; tar xvf - )
	cd ../../Manual ;\
           tar cf - --exclude=CVS . | ( cd $(PWD)/Manual; tar xvf - )

hdrug.exe: ../../Prolog/hdrug.sav 
	spld --static --main=restore\
 "URL:x-sicstus-resource:/hdrug/hdrug.sav" \
 --resources=../../Prolog/hdrug.sav=/hdrug/hdrug.sav\
 --output=hdrug.exe 

COMMON=../../version ../../COPYING ../../Copyright ../../CHANGES\
       ../../README ../../hdrug.man ../../*.html ../../*.png ../../*.ico

install:
	rm -rf $(TARGET)
	mkdir -p $(TARGET)
	mv hdrug.exe* Tex Tcl Applications Manual $(TARGET)
	cp Makefile.bin $(TARGET)/Makefile
	cp README.INSTALL $(COMMON) $(TARGET)

clean:
	/bin/rm -f *.c *.o *.obj *.pdb *.lib *.exp 

realclean: clean
	/bin/rm -rf hdrug.exe hdrug.sh version Applications Tcl Tex version

VERSION=${shell cat ../../version}

exe:
	cp hdrug.nsi ../$(PLATFORM)
	( cd ../$(PLATFORM) ; C:/Program\ Files/NSIS/makensis /V4 hdrug.nsi )

zip:
	( cd ../$(PLATFORM) ; zip -r $(VERSION)-$(PLATFORM) hdrug )

tar:
	( cd ../$(PLATFORM) ; tar czf $(VERSION)-$(PLATFORM).tar.gz hdrug )
