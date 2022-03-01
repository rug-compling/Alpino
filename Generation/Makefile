ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

ifeq "$(PROLOG)" "sicstus"
FILTER=filter$(MODULEEXT) filter.s.o
else
FILTER=filter$(MODULEEXT) 
endif

default: $(FILTER)

filter$(MODULEEXT): filter.pl filter.c local.o bigram.o
	$(SPLFR) filter.pl filter.c local.o bigram.o \
        --cflag="-O2 -Wall"

filter.s.o: filter.pl filter.c local.o bigram.o
	splfr --static filter.pl filter.c local.o bigram.o \
        --cflag="-O2 -Wall -fPIE" --static -LD

local.c: local.c.$(PROLOG)
	cp local.c.$(PROLOG) local.c

bigram.c: bigram.c.$(PROLOG)
	cp bigram.c.$(PROLOG) bigram.c

%.o: %.c
	gcc -c $< -o $@

