#           -*-Mode: makefile;-*-

ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

.PHONY: all clean realclean install

ifeq "$(PROLOG)" "sicstus"
GUIDES=guides$(MODULEEXT) guides.s.o
else
GUIDES=guides$(MODULEEXT) 
endif

ifeq "$(DISK)" ""
DISK=$$ALPINO_HOME/src
endif

all: $(GUIDES)
	$(MAKE) Alpino gadata.pl state

install: 
	install -D Alpino ../bin/
	install -D Alpino $(BINDIR)
	install -D Alpino.man $(MANDIR)/man1/Alpino.1

## always re-builds, can be dependent on env. variables:
.PHONY: AlpinoNS Alpino

AlpinoNS:
	echo "#!/bin/sh"                           >AlpinoNS
	echo "export SP_CTYPE=utf8"               >>AlpinoNS
	echo "export SP_CSETLEN=212"              >>AlpinoNS
	echo 'if [ -z "$$ALPINO_HOME" ]; then'    >>AlpinoNS
	echo '    echo "$$0: Error: Please set your ALPINO_HOME environment variable" 1>&2'>>AlpinoNS
	echo '    exit 1'                        >>AlpinoNS
	echo 'fi'                                >>AlpinoNS
	echo 'echo using Alpino from directory $$ALPINO_HOME ...'>>AlpinoNS
	echo 'sicstus -l $$ALPINO_HOME/src/nosave -a  "$$@" ' >> AlpinoNS
	chmod a+x AlpinoNS

## same as debug=undefined without producing output
## undefined, so that yes/no batch will set debug to 0/1 resp.
debugcmd='set_flag(debug,undefined),set_flag(demo,undefined)'

#gitversion=$(shell git log --oneline | wc -l)
# that variant did not show full history for a recently checked out version

gitversion=$(shell git log -1 --format="%at" | xargs -I{} date -u -d @{} +%Y%m%d.%H%M%S)

ifeq "$(PROLOG)" "sicstus"

PLATFORM=${shell echo "prolog_flag(host_type,X), write(X), nl, fail."\
     | sicstus 2>/dev/null}

ifeq "$(shell if [ -r ../Hdrug/Makefile.hdrug ]; then echo yes; fi)" "yes"
include ../Hdrug/Makefile.hdrug
endif

state:
	-echo "Alpino-$(PLATFORM)-git$(gitversion)-$(PROLOG)" > ../version
	-echo "use_module('../Hdrug/Prolog/hdrug_main'),\
               use_module(start),\
   version('Alpino (development), `cat ../version`-$(shell date "+%Y-%m-%dT%H:%M%z")'),\
               $(EXTRA_SICSTUS_GOAL)\
               save_program('$(DISK)/Alpino.sav.new',hdrug_main)." |\
        sicstus  -f
	mv $(DISK)/Alpino.sav.new $(DISK)/Alpino.sav

Alpino: 
ifneq "$(wildcard Alpino)" ""
	mv Alpino Alpino.bak
endif
	echo "#!/bin/sh"                            >Alpino
	echo "export SP_CTYPE=utf8"                >>Alpino
	echo "export SP_CSETLEN=212"               >>Alpino
	echo "export ALPINO_HOME=$(ALPINO_HOME)"   >>Alpino
	echo 'echo using Alpino from directory $$ALPINO_HOME ... >&2'>>Alpino
	echo sicstus  -r '$(DISK)/Alpino.sav' -a\
   debug=0 $(HDRUGFLAGS) $(ALPINOFLAGS)\
  $(DEFAULT_ALPINO_OPTIONS)\
  '$$DEFAULT_ALPINO_OPTIONS'\
  -cmd \"$(debugcmd)\" \
 '"$$@"' >> Alpino
	chmod a+x Alpino

else

PLATFORM=${shell echo "prolog_flag(version,X), write(X), nl, fail."\
     | $(PROLOG) 2>/dev/null}

state:
	-echo "Alpino-$(PLATFORM)-git$(gitversion)-$(PROLOG)" > ../version
	$(PROLOG) -o $(DISK)/Alpino.sav --goal=hdrug_main\
           -c ../Hdrug/Prolog/swi.pl start.pl

Alpino:
ifneq "$(wildcard Alpino)" ""
	mv Alpino Alpino.bak
endif
	echo "#!/bin/sh"                          >Alpino
	echo "ulimit -s unlimited"                >>Alpino
	echo 'if [ -z "$$ALPINO_HOME" ]; then'    >>Alpino
	echo '    echo "$0: Error: Please set your ALPINO_HOME environment variable" 1>&2'>>Alpino
	echo '    exit 1'                        >>Alpino
	echo 'fi'                                >>Alpino
	echo 'echo using Alpino from directory $$ALPINO_HOME ... >&2'>>Alpino
	echo $(PROLOG) -x $(DISK)/Alpino.sav --\
   debug=0 $(HDRUGFLAGS) $(ALPINOFLAGS)\
  $(DEFAULT_ALPINO_OPTIONS)\
  '$$DEFAULT_ALPINO_OPTIONS'\
  -cmd \"$(debugcmd)\" \
 '"$$@"' >> Alpino
	chmod a+x Alpino

endif

ALPINOFLAGS='-flag application_name Alpino'

clean: 
	rm -f guides_data.o

realclean: clean
	/bin/rm -f Alpino $(DISK)/Alpino.sav guides${MODULEEXT}\
                   guides.s.o guides_data.c gadata.pl guides_swi_glue.c Alpino.bak

guides$(MODULEEXT): guides.pl guides.c guides_data.o
	$(SPLFR) guides.pl guides.c guides_data.o \
        --cflag="-O2 -Wall"

guides.s.o: guides.pl guides.c guides_data.o
	splfr --static guides.pl guides.c guides_data.o \
        --cflag="-O2 -Wall -fPIE" --static -LD


guides_data.c: guides_data.c.$(PROLOG)
	cp guides_data.c.$(PROLOG) guides_data.c

guides_data.o: guides_data.c
	gcc -c $< -o $@

gadata.pl: gadata.txt
	cat gadata.txt  | $(HDRUG) -notk -l gadata2pl > gadata.pl
