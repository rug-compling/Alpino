ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

PWD:=${shell pwd}

ifndef BINDIR	
	BINDIR=../bin
endif

ifeq ($(PLATFORM),windows)
	ALPINO_BIN=Alpino.exe
else
	ALPINO_BIN=Alpino.bin
endif

default: $(ALPINO_BIN)

Alpino.bin: ../src/Alpino.sav
	spld -v --keep --main=restore\
        --config=spconfig\
        --static\
        --moveable\
        --respath=../src:../fadd:../TreebankTools/IndexedCorpus/prolog:../Tokenization:../PosTagger:../Generation:../Generation/fluency:../unix\
        -o Alpino.bin \
        --resources=../src/Alpino.sav=/alpino.sav \
        -LD -Wall  -L../TreebankTools/IndexedCorpus   \
        -L../fadd \
        -Wl,-rpath=$(ALPINO_HOME)/TreebankTools/IndexedCorpus:$(ALPINO_HOME)/fadd

## I had to add a couple of libraries to spconfig
## cp the spconfig file from the distribution 
## cp .../opt/sicstus/bin/spconfig-3.12.11 spconfig
## change the variable SP_EXE_LIBS so that it reads:
## SPLD_EXE_LIBS=-lcorpus -lboost_system -lboost_filesystem -lfadd -ldl -lz -lstdc++ -lm -lpthread
## DO THIS AGAIN IF SICSTUS IS INSTALLED IN DIFFERENT LOCATION

## this has worked years ago
Alpino.exe: ../src/Alpino.sav
	cp ../fadd/pro_fadd.lib .
	cp ../PosTagger/alpino_pos.lib .
	cp ../Tokenization/tokenize.lib .
	spld -v --static --main=restore\
        -o Alpino.exe \
        --resources=../src/Alpino.sav=/alpino.sav \
	--respath=.
	mt -manifest Alpino.exe.manifest -outputresource:Alpino.exe\;1
	rm -f pro_fadd.lib alpino_pos.lib tokenize.lib

clean:
	rm -f *.ilk *.pdb *.lib *.exp *.manifest

realclean: clean

ifeq ($(PLATFORM),darwin)
install:
	sed 's/LD_LIBRARY_PATH/DYLD_LIBRARY_PATH/g' Alpino > \
		$(BINDIR)/Alpino
	chmod +x $(BINDIR)/Alpino
else
ifeq ($(PLATFORM),windows)
install:
	cp Alpino.bat $(BINDIR)/Alpino.bat
else
install:
	cp Alpino $(BINDIR)/Alpino
endif
endif
