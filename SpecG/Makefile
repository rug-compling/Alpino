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

SIMPLIFY=$(ALPINO_HOME)/src/Alpino -notk cmdint=off\
          debug=1 demo=off\
          display_main_parts=off\
          max_sentence_length=20\
          user_max=90000\
          batch_command=paraphrase\
          filter_local_trees=off\
          paraphrase_nderiv=on\
          simplify_split=on\
          simplify_words=on\
          simplify_passive=on\
          simplify_modifier=on\
          generate_robustness=off\
          $(EXTRA)\

HPCTARGET=nderiv

%.log.gz: ../Suites/Machine/%.sents.gz
	zcat $< | \
        $(ALPINO)  -flag suite undefined  2> $(DISK)/$*-spec.log | gzip > $(DISK)/$*.lc.gz
	gzip $(DISK)/$*-spec.log
	mv $(DISK)/$*-spec.log.gz $*.log.gz
	mv $(DISK)/$*.lc.gz .

%.sim.gz: ../TestSim/%.sents.gz
	zcat $< | \
        $(SIMPLIFY)  -flag suite undefined  2> $(DISK)/$*-spec.sim | gzip > $(DISK)/$*.lc.gz
	gzip $(DISK)/$*-spec.sim
	mv $(DISK)/$*-spec.sim.gz $*.log.gz
	mv $(DISK)/$*.lc.gz .

include $(ALPINO_HOME)/Makefile.errormining

TTS=tt_part00001.log.gz tt_part00002.log.gz tt_part00003.log.gz tt_part00004.log.gz tt_part00005.log.gz tt_part00006.log.gz tt_part00007.log.gz tt_part00008.log.gz tt_part00009.log.gz tt_part00010.log.gz tt_part00011.log.gz tt_part00012.log.gz tt_part00013.log.gz tt_part00014.log.gz tt_part00015.log.gz tt_part00016.log.gz tt_part00017.log.gz tt_part00018.log.gz tu_part00001.log.gz tu_part00002.log.gz tu_part00003.log.gz tu_part00004.log.gz tu_part00005.log.gz tu_part00006.log.gz tu_part00007.log.gz tu_part00008.log.gz tu_part00009.log.gz

tts:
	$(MAKE) -j $(J) $(TTS) 

PPI=ppi_part00001.sim.gz ppi_part00002.sim.gz ppi_part00003.sim.gz ppi_part00004.sim.gz ppi_part00005.sim.gz ppi_part00006.sim.gz ppi_part00007.sim.gz ppi_part00008.sim.gz ppi_part00009.sim.gz ppi_part00010.sim.gz ppi_part00011.sim.gz ppi_part00012.sim.gz ppi_part00013.sim.gz ppi_part00014.sim.gz ppi_part00015.sim.gz ppi_part00016.sim.gz ppi_part00017.sim.gz ppi_part00018.sim.gz ppi_part00019.sim.gz ppi_part00020.sim.gz ppi_part00021.sim.gz ppi_part00022.sim.gz ppi_part00023.sim.gz ppi_part00024.sim.gz ppi_part00025.sim.gz ppi_part00026.sim.gz ppi_part00027.sim.gz ppi_part00028.sim.gz ppi_part00029.sim.gz ppi_part00030.sim.gz ppi_part00031.sim.gz ppi_part00032.sim.gz ppi_part00033.sim.gz ppi_part00034.sim.gz ppi_part00035.sim.gz ppi_part00036.sim.gz ppi_part00037.sim.gz ppi_part00038.sim.gz ppi_part00039.sim.gz ppi_part00040.sim.gz ppi_part00041.sim.gz ppi_part00042.sim.gz ppi_part00043.sim.gz ppi_part00044.sim.gz ppi_part00045.sim.gz ppi_part00046.sim.gz ppi_part00047.sim.gz ppi_part00048.sim.gz ppi_part00049.sim.gz ppi_part00050.sim.gz ppi_part00051.sim.gz ppi_part00052.sim.gz ppi_part00053.sim.gz ppi_part00054.sim.gz ppi_part00055.sim.gz ppi_part00056.sim.gz ppi_part00057.sim.gz ppi_part00058.sim.gz ppi_part00059.sim.gz ppi_part00060.sim.gz ppi_part00061.sim.gz ppi_part00062.sim.gz ppi_part00063.sim.gz ppi_part00064.sim.gz ppi_part00065.sim.gz ppi_part00066.sim.gz ppi_part00067.sim.gz ppi_part00068.sim.gz ppi_part00069.sim.gz ppi_part00070.sim.gz ppi_part00071.sim.gz ppi_part00072.sim.gz ppi_part00073.sim.gz ppi_part00074.sim.gz

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

sorted.pl: $(ALL) missing
	zcat $(wildcard $(PAT)*.lc.gz) | grep ^local | sort -u > sorted0.pl
	cat missing sorted0.pl | sort -u > sorted.pl

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
