include Makefile.include

## only those which include a Makefile
APPL_DIRS= Applications/Ale Applications/Alvey Applications/CBCG\
 Applications/Cfg Applications/Chat Applications/Dcg\
 Applications/Extraposition Applications/GenBook Applications/LexRules\
 Applications/Sdcg Applications/Shpsg Applications/Tag

all: create install

create: hdrug state apps

state:
	( cd Prolog ; $(MAKE) HDRUGDIR=$(HDRUGDIR) )

.PHONY: hdrug

ifeq "$(PROLOG)" "sicstus"

# Applications can also be built / installed / cleaned from their
# own directory
apps: 
	-for dir in $(APPL_DIRS); do \
           if [ -d $$dir ];\
           then ( cd $$dir ;\
                  if [ -f Makefile ];\
                  then $(MAKE) HDRUGDIR=$(HDRUGDIR);\
                  fi\
                );\
           fi;\
         done

## always re-builds, can be dependent on env. variables
hdrug: 
	echo "#!/bin/sh" > hdrug
	echo "export SP_CTYPE=utf8" >> hdrug
	echo "export SP_CSETLEN=212">> hdrug
	echo "$(SICSTUS) -r $(CURDIR)/Prolog/hdrug.sav -a\
              -flag hdrug_library $(CURDIR)/Tcl\
              -flag tex_library $(CURDIR)/Tex\
              blt=$(BLT) -flag blt_library $(BLTLIB)\
                \"\$$@\""\
                         >>hdrug
	echo "HDRUGFLAGS=-flag hdrug_library $(CURDIR)/Tcl\
              -flag tex_library $(CURDIR)/Tex\
              blt=$(BLT) -flag blt_library $(BLTLIB)"\
                         >Makefile.hdrug
	chmod +x hdrug

else

apps:

hdrug:

endif


# Installing
install: 

ifeq "$(PROLOG)" "sicstus"

	mkdir -p $(LIBDIR) $(BINDIR) $(MANDIR)/man1
	echo '#!/bin/sh' > $(BINDIR)/$(TARGET)
	echo "export SP_CTYPE=utf8" >> $(BINDIR)/$(TARGET)
	echo "export SP_CSETLEN=212">> $(BINDIR)/$(TARGET)
	echo '$(SICSTUS) -r $(PATH_TO_LIBDIR)/hdrug.sav -a\
              -flag hdrug_library $(PATH_TO_LIBDIR)\
              -flag tex_library $(PATH_TO_LIBDIR)\
              blt=$(BLT) -flag blt_library $(BLTLIB)' \
               "\"\$$@\""\
                         >>$(BINDIR)/$(TARGET)
	echo 'HDRUGFLAGS=-flag hdrug_library $(PATH_TO_LIBDIR)\
              -flag tex_library $(PATH_TO_LIBDIR)\
              blt=$(BLT) -flag blt_library $(BLTLIB)'\
                         >$(LIBDIR)/Makefile.hdrug
	chmod +x $(BINDIR)/$(TARGET)
	if [ ! -f $(LIBDIR)/hdrug.sav ];\
             then cp Prolog/hdrug.sav $(LIBDIR);\
	     else if cmp -s Prolog/hdrug.sav $(LIBDIR)/hdrug.sav;\
                  then true ;\
                  else cp -f Prolog/hdrug.sav $(LIBDIR)/hdrug.sav;\
	          fi;\
             fi
	cp Prolog/*.pl $(LIBDIR)
	cd Tcl ; tar cf - --exclude=.svn \
                     Clig Hdrug.ad bitmaps hdrug.tcl tkterm.tcl README\
                   | ( cd $(LIBDIR) ; tar xf - )
	cp Tex/avm.sty $(LIBDIR)
	cp hdrug.man $(MANDIR)/man1/$(TARGET).1
endif

clean :
	-for dir in $(APPL_DIRS); do \
           if [ -d $$dir ];\
           then ( cd $$dir ;\
                  if [ -f Makefile ];\
                  then $(MAKE) clean HDRUGDIR=$(HDRUGDIR);\
                  fi\
                );\
           fi;\
         done

realclean :
	/bin/rm -f Prolog/hdrug.sav hdrug Prolog/hdrug
	-for dir in $(APPL_DIRS); do \
           if [ -d $$dir ];\
           then ( cd $$dir ;\
                  if [ -f Makefile ];\
                  then $(MAKE) realclean HDRUGDIR=$(HDRUGDIR);\
                  fi\
                );\
           fi;\
         done
