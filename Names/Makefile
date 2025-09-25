ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

ifeq "$(PROLOG)" "sicstus"
POS=features.po 
else
POS=
endif

all: features.pl $(POS) name_accents.pl

.PHONY:clean realclean install

%.po : %.pl
	echo "compile('$*'), save_files('$*','$*')." | ../Hdrug/hdrug -notk

features.pl: features.t Create/features2pl.pl
	$(HDRUG) -notk -l Create/features2pl.pl -cmd features2pl <features.t >features.pl

name_accents.pl: name_accents.pl.$(PROLOG)
	cp name_accents.pl.$(PROLOG) name_accents.pl

realclean:
	rm -f features.pl $(POS)

clean:

install:

