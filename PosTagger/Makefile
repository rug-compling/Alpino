ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

PWD:=${shell pwd}

ifeq ($(PLATFORM),darwin)
	SHAREDLIB=alpino_pos$(MODULEEXT)
	STATICLIB=alpino_pos.s.o
	TAGGER=tagger
else
	ifeq ($(PLATFORM),windows)
		CFLAGS=/O2 /FD /EHsc /MD /W3 /nologo /c /Wp64 /Zi /TP /errorReport:prompt /I../fadd
		SHAREDLIB=alpino_pos$(MODULEEXT)
		STATICLIB=alpino_pos.lib
		TAGGER=
	else
		SHAREDLIB=alpino_pos$(MODULEEXT)
		STATICLIB=alpino_pos.s.o
		TAGGER=tagger
	endif
endif

ifeq "$(PROLOG)" "sicstus"
all: $(SHAREDLIB) $(TAGGER) $(STATICLIB)
else
all: $(SHAREDLIB) $(TAGGER) 
endif

alpino_pos.so: tagger.o guesser.o alpino_pos.pl prolog_pos.cc
	$(SPLFR) alpino_pos.pl tagger.o guesser.o prolog_pos.cc\
                --cflag="-fPIC -I../fadd $(CCDEBUG) -D_GNU_SOURCE"\
                -LD -Wall -lstdc++ -L../fadd -lfadd\
                 -Wl,-rpath=$(PWD)/../fadd

alpino_pos.bundle alpino_pos.dylib: tagger.o guesser.o alpino_pos.pl prolog_pos.cc
	$(SPLFR) alpino_pos.pl tagger.o guesser.o prolog_pos.cc\
                --cflag="-fPIC -I../fadd $(CCDEBUG) -D_GNU_SOURCE"\
                -LD-Wall -lstdc++ -L../fadd -lfadd\
                -Wl,-rpath=$(PWD)/../fadd
	install_name_tool -change libfadd.dylib @rpath/libfadd.dylib $@

alpino_pos.dll: tagger.obj guesser.obj alpino_pos.pl prolog_pos.cc
	splfr alpino_pos.pl tagger.obj guesser.obj prolog_pos.cc \
		../fadd/fadd.lib --cflag="/EHsc /I../fadd"
	mt -manifest alpino_pos.dll.manifest -outputresource:alpino_pos.dll\;2

alpino_pos.s.o: tagger.o guesser.o alpino_pos.pl prolog_pos.cc
	splfr alpino_pos.pl tagger.o guesser.o prolog_pos.cc\
                --cflag="-I../fadd $(CCDEBUG) -D_GNU_SOURCE -fPIE"\
                --static\
                -LD -Wall -lstdc++ -L../fadd -lfadd\
                 -Wl,-rpath=$(PWD)/../fadd

alpino_pos.lib: tagger.obj guesser.obj alpino_pos.pl prolog_pos.cc
	splfr alpino_pos.pl tagger.obj guesser.obj prolog_pos.cc \
		../fadd/fadd.obj --static --cflag="/EHsc /I../fadd"

#CCDEBUG=
CCDEBUG=-DDEBUG

tagger: main.o tagger.o guesser.o
	g++ -O3 -Wall main.o tagger.o guesser.o -o tagger -I../fadd \
      -LD -Wall -lstdc++ -L../fadd -lfadd -Xlinker -rpath $(PWD)/../fadd

tagger.exe: main.obj tagger.obj guesser.obj
	link /OUT:tagger.exe main.obj tagger.obj guesser.obj ../fadd/fadd.lib

tagger.o: tagger.h tagger.cc
	g++ -Wall -fPIC -O3 -c tagger.cc -o tagger.o -D_GNU_SOURCE -I../fadd

tagger.obj: tagger.h tagger.cc
	cl $(CFLAGS) tagger.cc

main.o: main.cc tagger.h
	g++ -Wall -fPIC -O3 -c main.cc -o main.o -D_GNU_SOURCE -I../fadd

main.obj: main.cc tagger.h
	cl $(CFLAGS) main.cc

guesser.o: tagger.h guesser.cc
	g++ -Wall -fPIC -O3 -c guesser.cc -o guesser.o -D_GNU_SOURCE -I../fadd

guesser.obj: tagger.h guesser.cc
	cl $(CFLAGS) guesser.cc

clean:
	rm -f *.o *.obj *.lib *.manifest *.pdb *.idb

install:
	cp tagger ../bin
	cp tagger $(BINDIR)/

realclean: clean
	/bin/rm -f $(SHAREDLIB) tagger *.dll alpino_pos_swi_glue.c

TAGGER=./tagger

include  Construct/Makefile.eval

pbs:
	( echo "#!/bin/sh" ;\
          echo "#PBS -l walltime=0:30:00" ;\
          echo "#PBS -l nodes=1" ;\
          echo "cd \$$PBS_O_WORKDIR" ;\
          echo "$(command)";\
        ) | qsub -V -N alpino.tagger - 


