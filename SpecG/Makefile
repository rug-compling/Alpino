ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

ifeq "$(shell if [ -r ../Makefile.hpc ]; then echo yes; fi)" "yes"
include ../Makefile.hpc
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.hpc ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.hpc
endif
endif

ALPINO=$(ALPINO_HOME)/src/Alpino -notk cmdint=off\
          debug=1 demo=off\
          display_main_parts=off\
          max_sentence_length=20\
          user_max=90000\
          batch_command=paraphrase\
          filter_local_trees=off\
          paraphrase_nderiv=on\
          $(EXTRA)\

HPCTARGET=nderiv

%.log.gz: ../Suites/Machine/%.sents.gz
	zcat $< | \
        $(ALPINO)  -flag suite undefined  2> $(DISK)/$*-spec.log | gzip > $(DISK)/$*.lc.gz
	gzip $(DISK)/$*-spec.log
	mv $(DISK)/$*-spec.log.gz $*.log.gz
	mv $(DISK)/$*.lc.gz .

include $(ALPINO_HOME)/Makefile.errormining

TTS=tt_part00001.log.gz tt_part00002.log.gz tt_part00003.log.gz tt_part00004.log.gz tt_part00005.log.gz tt_part00006.log.gz tt_part00007.log.gz tt_part00008.log.gz tt_part00009.log.gz tt_part00010.log.gz tt_part00011.log.gz tt_part00012.log.gz tt_part00013.log.gz tt_part00014.log.gz tt_part00015.log.gz tt_part00016.log.gz tt_part00017.log.gz tt_part00018.log.gz tu_part00001.log.gz tu_part00002.log.gz tu_part00003.log.gz tu_part00004.log.gz tu_part00005.log.gz tu_part00006.log.gz tu_part00007.log.gz tu_part00008.log.gz tu_part00009.log.gz

tts:
	$(MAKE) -j $(J) $(TTS) 

PPI=ppi_part00001.log.gz ppi_part00002.log.gz ppi_part00003.log.gz ppi_part00004.log.gz ppi_part00005.log.gz ppi_part00006.log.gz ppi_part00007.log.gz ppi_part00008.log.gz ppi_part00009.log.gz ppi_part00010.log.gz ppi_part00011.log.gz ppi_part00012.log.gz ppi_part00013.log.gz ppi_part00014.log.gz ppi_part00015.log.gz ppi_part00016.log.gz ppi_part00017.log.gz ppi_part00018.log.gz ppi_part00019.log.gz ppi_part00020.log.gz ppi_part00021.log.gz ppi_part00022.log.gz ppi_part00023.log.gz ppi_part00024.log.gz ppi_part00025.log.gz ppi_part00026.log.gz ppi_part00027.log.gz ppi_part00028.log.gz ppi_part00029.log.gz ppi_part00030.log.gz ppi_part00031.log.gz ppi_part00032.log.gz ppi_part00033.log.gz ppi_part00034.log.gz ppi_part00035.log.gz ppi_part00036.log.gz ppi_part00037.log.gz ppi_part00038.log.gz ppi_part00039.log.gz ppi_part00040.log.gz ppi_part00041.log.gz ppi_part00042.log.gz ppi_part00043.log.gz ppi_part00044.log.gz ppi_part00045.log.gz ppi_part00046.log.gz ppi_part00047.log.gz ppi_part00048.log.gz ppi_part00049.log.gz ppi_part00050.log.gz ppi_part00051.log.gz ppi_part00052.log.gz ppi_part00053.log.gz ppi_part00054.log.gz ppi_part00055.log.gz ppi_part00056.log.gz ppi_part00057.log.gz ppi_part00058.log.gz ppi_part00059.log.gz ppi_part00060.log.gz ppi_part00061.log.gz ppi_part00062.log.gz ppi_part00063.log.gz ppi_part00064.log.gz ppi_part00065.log.gz ppi_part00066.log.gz ppi_part00067.log.gz ppi_part00068.log.gz ppi_part00069.log.gz ppi_part00070.log.gz ppi_part00071.log.gz ppi_part00072.log.gz ppi_part00073.log.gz ppi_part00074.log.gz

ad2000sents=$(notdir $(wildcard $(ALPINO_HOME)/Suites/Machine/ad2000*sents.gz))
ad2000=$(ad2000sents:%.sents.gz=%.log.gz)

vk2004sents=$(notdir $(wildcard $(ALPINO_HOME)/Suites/Machine/vk2004*sents.gz))
vk2004=$(vk2004sents:%.sents.gz=%.log.gz)

wssents=$(notdir $(wildcard $(ALPINO_HOME)/Suites/Machine/WS*sents.gz))
ws=$(wssents:%.sents.gz=%.log.gz)


J=20
ad:
	$(MAKE) -j $(J) $(ad2000) 

ppis:
	$(MAKE) -j $(J) $(PPI) 

vk:
	$(MAKE) -j $(J) $(vk2004) 

ws:
	$(MAKE) -j $(J) $(ws)

PAT=

ALL=$(wildcard *.lc.gz)
BIGS=$(ALL:%.lc.gz=%.big)

bigs:$(BIGS)

sorted.pl: $(ALL)
	zcat $(wildcard $(PAT)*.lc.gz) | grep ^local | sort -u > sorted.pl

local.pl: sorted.pl
	Alpino -notk -l map cmdint=off batch_command=create_locals| sort -u > local.pl

install: local.c bigram.c
	cp local.c ../Generation/local.c.$(PROLOG)
	cp bigram.c ../Generation/bigram.c.$(PROLOG)

%.big: %.lc.gz
	zcat $< | grep ^deriv | \
        Alpino -notk -l map cmdint=off batch_command=create_bigrams| sort -u > $*.big

bigram_indexes: $(BIGS)
	cat $(BIGS) | sort -u | Alpino -notk -l index_bigrams batch_command=index > bigram_indexes

local_indexes: local.pl
	cat local.pl | Alpino -notk -l index_bigrams batch_command=index > local_indexes

bigram.c: bigram_indexes ../Specialization/make_guide_vector
	cat bigram_indexes | ../Specialization/make_guide_vector |\
            sed -e 's/guideVector/bigramGuideVector/' -e 's/guideSize/bigramGuideSize/' > bigram.c

local.c: local_indexes ../Specialization/make_guide_vector
	cat local_indexes | ../Specialization/make_guide_vector |\
            sed -e 's/guideVector/localGuideVector/'  -e 's/guideSize/localGuideSize/' > local.c
