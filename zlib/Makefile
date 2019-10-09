ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

PRO_ZLIB=zlib$(MODULEEXT)

PRO_ZLIB_STATIC=zlib.s.o

ifeq "$(PROLOG)" "sicstus"
all: $(PRO_ZLIB) $(PRO_ZLIB_STATIC) 
else
all: $(PRO_ZLIB)
endif

zlib.so: zlib.c zlib.pl
	$(SPLFR) zlib.pl zlib.c \
		-lz -LD -Wl,-rpath,$(PWD) $(LDFLAGS)

zlib.bundle zlib.dylib: zlib.c zlib.pl
	$(SPLFR) zlib.pl zlib.c \
		-lz -LD -Wl,-rpath,$(PWD) $(LDFLAGS)

zlib.s.o: zlib.c zlib.pl
	splfr zlib.pl zlib.c \
		--static -LD

test: zlib.so
	echo "test." | sicstus -l test

clean:

realclean: clean
	rm -f $(PRO_ZLIB) $(PRO_ZLIB_STATIC)

install:
