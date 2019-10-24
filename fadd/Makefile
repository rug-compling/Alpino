# Makefile for building a finite-state library for Alpino
# Copyright (c) Jan Daciuk <j.daciuk@let.rug.nl>, 2001.
#

ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

PWD:=${shell pwd}

SHAREDLIB=libfadd$(DYLIBEXT)
PRO_FADDLIB=pro_fadd$(MODULEEXT)

PRO_FADDSTATIC=pro_fadd.s.o

TEST_FADD=test_fadd

ifeq ($(PLATFORM),darwin)
	LINKFLAGS=-fPIC -dynamiclib
else
	ifeq ($(PLATFORM),windows)
		SHAREDLIB=
		PRO_FADDSTATIC=pro_fadd.lib
		TEST_FADD=
	else
		LINKFLAGS=-fPIC -shared
	endif
endif

# Compile options (for fsa_build add FLEXIBLE and NUMBERS)
# LOOSING_RPM	- to work around a bug in rpm libstdc++ library
# MORPH_INFIX	- makes it possible to use -I and -P options in fsa_morph
#		  for recognition of coded prefixes and infixes
# NEXTBIT	- changes the format of the automaton, so that when there are
#		  chains of nodes, one following another, one bit is set
#		  in the goto field to indicate that fact, and only one byte
#		  from the goto field is used; it usually gives smaller
#		  automata
# POOR_MORPH	- enables -A option in fsa_morph for morphological analysis
#		  giving only categories, and no base forms.
# STOPBIT	- changes the format of the automaton, so that there are
#		  no counters, but for each arc there is a bit that says
#		  whether it is the last one in the node; this gives smaller
#		  automata
# TAILS		- changes the format of the automaton, allowing for more
#		  arc sharing, so more compression at the cost of construction
#		  time
# WEIGHTED	- introduces weights on arcs for guessing automata.
# OLD_COMPILER	- older versions of g++ create files for ifstream if not
#		  given ios::nocreate option. The new ones probably don't,
#		  and they also treat ios::nocreat as error. Happy compiling!
#
#
# Some versions of g++ (or stdlibc++) are broken - if so, don't use -O2!
# !!! If you change these, please do make clean first before each make
CPPFLAGS=-O2 -Wall \
  -DFLEXIBLE \
  -DNUMBERS \
  -DSTOPBIT \
  -DMORPH_INFIX \
  -DPOOR_MORPH \
  -DLOOSING_RPM \
  -DMULTICOLUMN \
  -I../util 
  # -DDEBUG
  # -DOLD_COMPILER
  # -DINTERPOLATED 
  #-DDEBUG -DDMALLOC


# Normally empty
#LDFLAGS=-L/usr/local/lib -ldmallocxx
LDFLAGS=-lboost_thread -lboost_system
ifeq ($(PLATFORM),darwin)
	LDFLAGS=-lboost_thread-mt -lboost_system-mt
endif


########################################################################

SCRIPTS=print_tuple.pl maketuple.pl

ifeq "$(PROLOG)" "sicstus"
all: $(SHAREDLIB) $(SCRIPTS) $(PRO_FADDLIB) $(TEST_FADD) $(PRO_FADDSTATIC)
else
all: $(SHAREDLIB) $(SCRIPTS) $(PRO_FADDLIB) $(TEST_FADD)
endif

maketuple.pl: maketuple.pl.in
	sed -e 's|@@S_FSA_PATH@@|$(S_FSA)|g' <maketuple.pl.in \
                                                 >maketuple.pl
	chmod +x maketuple.pl
print_tuple.pl: print_tuple.pl.in
	sed -e 's|@@S_FSA_PATH@@|$(S_FSA)|g' <print_tuple.pl.in \
                                                 >print_tuple.pl
	chmod +x print_tuple.pl

pro_fadd.so: $(SHAREDLIB) fadd.h fsa.h pro_fadd.cc pro_fadd.pl
	$(SPLFR) pro_fadd.pl pro_fadd.cc --cflag="${CPPFLAGS}"\
             -LD  -Wl,-rpath,$(PWD) -L. -lfadd $(LDFLAGS) -lstdc++

pro_fadd.bundle pro_fadd.dylib: $(SHAREDLIB) fadd.h fsa.h pro_fadd.cc pro_fadd.pl
	$(SPLFR) pro_fadd.pl pro_fadd.cc --cflag="${CPPFLAGS}"\
             -LD  -Wl,-rpath,$(PWD) -L. -lfadd $(LDFLAGS) -lstdc++
	install_name_tool -change libfadd.dylib @rpath/libfadd.dylib $@

pro_fadd.dll: fadd.lib fadd.h fsa.h pro_fadd.cc pro_fadd.pl
	splfr pro_fadd.pl pro_fadd.cc fadd.lib
	mt -manifest pro_fadd.dll.manifest -outputresource:pro_fadd.dll\;2

pro_fadd.s.o: $(SHAREDLIB) fadd.h fsa.h pro_fadd.cc pro_fadd.pl
	splfr pro_fadd.pl pro_fadd.cc --cflag="${CPPFLAGS}"\
              --static\
             -LD -Wl,-rpath,$(PWD) -L. -lfadd $(LDFLAGS) -lstdc++

pro_fadd.lib: fadd.obj fadd.h fsa.h pro_fadd.cc pro_fadd.pl
	splfr pro_fadd.pl pro_fadd.cc fadd.obj $(LDFLAGS) --static

$(SHAREDLIB): fadd.cc fadd.h
	gcc $(CPPFLAGS) fadd.cc -lstdc++ $(LDFLAGS) $(LINKFLAGS) -o $@

fadd.obj: fadd.cc fadd.h fsa.h
	cl /O2 /D "WIN32" /D "NDEBUG" /D "_LIB" /D "FLEXIBLE" /D "NUMBERS" \
		/D "STOPBIT" /D "MORPH_INFIX" /D "POOR_MORPH" /D "LOOSING_RPM" \
		/D "MULTICOLUMN" /D "_UNICODE" /D "UNICODE" /FD /EHsc /MD /W3 /nologo \
		/c /Wp64 /Zi /TP /errorReport:prompt fadd.cc
fadd.lib: fadd.obj
	lib /OUT:"fadd.lib" /NOLOGO /LTCG fadd.obj

test_fadd: $(SHAREDLIB) test_fadd.o
	g++ test_fadd.o -Wl,-rpath,$(PWD) -L. -lfadd -o test_fadd

acctuple: acctuple.o
	g++ $(CPPFLAGS) acctuple.o -o acctuple -pg

acctuple.o: acctuple.cc
	g++ $(CPPFLAGS) -c acctuple.cc -pg

test_fadd.o: fadd.h test_fadd.c
	$(CC) $(CPPFLAGS) -c test_fadd.c

test_guess: test_guess.o $(SHAREDLIB)
	g++ $(CPPFLAGS) $< -Wl,-rpath,$(PWD) -L. -lfadd -o $@

test_guess.o: test_guess.c
	$(CC) $(CPPFLAGS) -c $<

clean:
	rm -f *.o *.obj *.lib *.manifest *.pdb *.idb

realclean: clean
	rm -f $(SHAREDLIB) $(PRO_FADDLIB) test_fadd $(SCRIPTS) *.dll\
              test_fadd.exe pro_fadd_swi_glue.c


## to test sicstus interface:
## sicstus -l fadd
## ?- pro_init_tuple(['test1.tab.tpl'],Number), 
##    pro_word_tuple_grams([jan],Number, Result).
## 
##
## cd ~vannoord/z/Alpino/Lexicon
## sicstus -l tst
## ?- fadd:init_morph('tst_dict.fsa',0,0,0,0,Dict),
##    fadd:pro_morph_word(kom,Dict,List).

install:
