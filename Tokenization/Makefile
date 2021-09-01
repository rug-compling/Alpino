ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

SHAREDLIB=tokenize$(MODULEEXT)
STATICLIB=tokenize.s.o
TOK=tok tok_no_breaks alp_escape partok lines entities
CFLAGS=-Wall

ifeq ($(PLATFORM),windows)
	CFLAGS=/O2 /FD /TC /MD /W3 /nologo /c /Wp64 /Zi /errorReport:prompt
	STATICLIB=tokenize.lib
	ENTITIES=
	TOK=
endif

ifeq "$(PROLOG)" "sicstus"
all: $(SHAREDLIB) $(TOK) $(STATICLIB)
else
all: $(SHAREDLIB) $(TOK)
endif

tok: libtok.c tok_main.c
	gcc $(CFLAGS) tok_main.c libtok.c -o $@

tok_no_breaks: libtok_no_breaks.c tok_main.c
	gcc $(CFLAGS) tok_main.c libtok_no_breaks.c -o $@

libtok_no_breaks.o: libtok_no_breaks.c
	gcc $(CFLAGS) -fPIC -c -o $@ $<

#libtok_breaks$(DYLIBEXT): libtok.o
#	gcc $(CFLAGS) -fPIC -shared -o $@ $<

#libtok.obj: libtok.c
#	cl $(CFLAGS) /bigobj libtok.c

tokenize.so: pl_libtok.c libtok_no_breaks.o tokenize.pl
	$(SPLFR) tokenize.pl pl_libtok.c libtok_no_breaks.o\
             -LD  -Wl,-rpath,$(PWD) $(LDFLAGS)

#tokenize.bundle tokenize.dylib: pl_libtok.c libtok.o tokenize.pl
#	$(SPLFR) tokenize.pl pl_libtok.c libtok.o\
#             -LD $(LDFLAGS)

#tokenize.dll: pl_libtok.c libtok.obj tokenize.pl
#	splfr tokenize.pl pl_libtok.c libtok.obj
#	mt -manifest tokenize.dll.manifest -outputresource:tokenize.dll\;2
# make tokenize.s.o dependent on tokenize.so so that those targets are
# not built in parallel (create same temporary files...)
tokenize.s.o: tokenize.so pl_libtok.c libtok_no_breaks.o tokenize.pl
	splfr -cflag -fPIE tokenize.pl pl_libtok.c libtok_no_breaks.o\
             --static\
             -LD  -Wl,-rpath,$(PWD) $(LDFLAGS)

#tokenize.lib: pl_libtok.c libtok.obj tokenize.pl
#	splfr tokenize.pl pl_libtok.c libtok.obj --static

partok: partok.go
	go build partok.go

clean:
	/bin/rm -f *.o *.obj *.lib *.manifest *.pdb *.idb

realclean: clean
	/bin/rm -f $(SHAREDLIB) $(TOK) $(STATICLIB) tokenize_swi_glue.c

install:

