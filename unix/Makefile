ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

PRO_unix=unix$(MODULEEXT)

PRO_unix_STATIC=unix.s.o

ifeq "$(PROLOG)" "sicstus"
all: $(PRO_unix) $(PRO_unix_STATIC) 
else
all: $(PRO_unix)
endif

unix.so: unix.c unix.pl
	$(SPLFR) unix.pl unix.c \
		-lz -LD -Wl,-rpath,$(PWD) $(LDFLAGS)

unix.bundle unix.dylib: unix.c unix.pl
	$(SPLFR) unix.pl unix.c \
		-lz -LD -Wl,-rpath,$(PWD) $(LDFLAGS)

unix.s.o: unix.c unix.pl
	splfr unix.pl unix.c \
		-cflag -fPIE --static -LD

clean:

realclean: clean
	rm -f $(PRO_unix) *.s.o unix_swi_glue.c

install:
