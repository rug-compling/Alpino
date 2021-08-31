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
          -fast\
          keep_notree=on\
          robustness_allows_skips=off\
          debug=1 demo=off\
          display_main_parts=off\
          max_sentence_length=50\
          user_max=900000\
          use_guides=off\
          end_hook=left_corners\
          $(EXTRA)\

HPCTARGET=left_corners

left_corners:
	zcat ../Suites/Machine/$(suite).sents.gz | \
        $(ALPINO) -flag suite undefined -parse 2> $(DISK)/$(suite)-spec.log | gzip > $(DISK)/$(suite).lc.gz 
	gzip $(DISK)/$(suite)-spec.log
	mv $(DISK)/$(suite)-spec.log.gz $(suite).log.gz
	mv $(DISK)/$(suite).lc.gz .
	$(MAKE) $(suite).ps

%.ps: %.lc.gz
	zcat $< \
        | grep '^LEFTCORNER#' \
        | sed -e 's/^LEFTCORNER#//'\
        | sort -u \
        | ./generate_prefixes \
        | sort -u > $@

%.log.gz: ../Suites/Machine/%.sents.gz
	zcat $< | \
        $(ALPINO)  -flag suite undefined -parse 2> $(DISK)/$*-spec.log | gzip > $(DISK)/$*.lc.gz
	gzip $(DISK)/$*-spec.log
	mv $(DISK)/$*-spec.log.gz $*.log.gz
	mv $(DISK)/$*.lc.gz .

include $(ALPINO_HOME)/Makefile.errormining

PAT=
DATA=$(wildcard $(PAT)*.log.gz)

echo:
	echo $(DATA:%.log.gz=%.ps)

noparses:
	sort  -u *.ps > guides.t
	cat guides.t |\
   ../src/Alpino cmdint=off debug=1 -notk -l to_predict_guides \
             -cmd "to_sguides(tpl_prefix)" | sort -u > guides.txt
	awk -F\| '{ print $$1}' guides.txt | ./make_guide_vector > guides_data.c
	cp guides_data.c ../src/guides_data.c.$(PROLOG)

guides.t: $(DATA:%.log.gz=%.ps) make_guide_vector
	sort  -u $(DATA:%.log.gz=%.ps) > guides.t

guides.txt: guides.t ../src/guides.pl
	cat guides.t |\
   ../src/Alpino cmdint=off debug=1 -notk -l to_predict_guides \
             -cmd "to_sguides(tpl_prefix)" | sort -u > guides.txt

make_guide_vector: make_guide_vector.cpp
	g++ -Wall -o $@ -O2 $<

guides_data.c: guides.txt make_guide_vector
	awk -F\| '{ print $$1}' guides.txt | ./make_guide_vector > $@

install: guides_data.c
	cp guides_data.c ../src/guides_data.c.$(PROLOG)

nodata: make_guide_vector
	echo "Expect a few odd syntax errors with euro signs - just ignore~n"
	cat guides.t |\
         ../src/Alpino cmdint=off debug=1 -notk -l to_predict_guides \
             -cmd "to_sguides(tpl_prefix)" |\
         sort -u |\
         awk -F\| '{ print $$1}' |\
         ./make_guide_vector > ../src/guides_data.c.$(PROLOG)

TTS=tt_part00001.log.gz tt_part00002.log.gz tt_part00003.log.gz tt_part00004.log.gz tt_part00005.log.gz tt_part00006.log.gz tt_part00007.log.gz tt_part00008.log.gz tt_part00009.log.gz tt_part00010.log.gz tt_part00011.log.gz tt_part00012.log.gz tt_part00013.log.gz tt_part00014.log.gz tt_part00015.log.gz tt_part00016.log.gz tt_part00017.log.gz tt_part00018.log.gz tu_part00001.log.gz tu_part00002.log.gz tu_part00003.log.gz tu_part00004.log.gz tu_part00005.log.gz tu_part00006.log.gz tu_part00007.log.gz tu_part00008.log.gz tu_part00009.log.gz

tts:
	$(MAKE) -j $(J) $(TTS) EXTRA=pos_tagger_m=5

rules:
	cat *.ps| grep finish |awk -F\| '{ printf "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n",$$2,$$3,$$4,$$5,$$6,$$7,$$8,$$9,$$10,$$11,$$12,$$13,$$14,$$15,$$16,$$17,$$18,$$19 }' | grep -v 'lex(' | grep -v finish | grep . | sort | uniq -c | sort -nr | awk '{ printf "frequent_rule(%s,%s).\n",$$2,$$1 }' > rules

