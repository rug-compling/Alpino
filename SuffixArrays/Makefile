ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

# Todo: port getopt before enabling Windows support.
ifeq ($(PLATFORM),windows)
	SA_BIN=
else
	SA_BIN=suffix_array
endif

FADD=${shell pwd}/../fadd

all: $(SA_BIN) 

clean: 
	rm -f ssort.o

realclean:clean
	rm -f suffix_array

suffix_array: main.c ssort.o 
	$(CC) $(CFLAGS) -Wall -I$(FADD) -o suffix_array ssort.c main.c\
         -L$(FADD) -lfadd -Wl,-rpath=$(FADD)

suffix_array.exe: main.c ssort.c
	cl /O2 /FD /TC /MD /W3 /nologo /c /Wp64 /Zi /errorReport:prompt \
		/Fesuffix_array.exe main.c ssort.c

## under cygwin the -Wl,-rpath, switch appears to be ingored. Therefore,
## install fadd.dll in a directory that is in your PATH

web: 
	( cd .. ; tar czf $(HOME)/public_html/SuffixArrays.tgz\
                           ./SuffixArrays )

install:

