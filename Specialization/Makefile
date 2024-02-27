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
          user_max=90000\
          use_guides=off\
          end_hook=left_corners\
          $(extra)

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

noparses: make_guide_vector
	echo "some syntax errors for euro sign expected...(just ignore)"
	cat guides.t |\
   ../src/Alpino cmdint=off debug=1 -notk -l to_predict_guides \
             -cmd "to_sguides(tpl_prefix)" | sort -u > guides.txt
	awk -F\| '{ print $$1}' guides.txt | ./make_guide_vector > guides_data.c
	cp guides_data.c ../src/guides_data.c.$(PROLOG)

guides.t: $(DATA:%.log.gz=%.ps) make_guide_vector
	sort -u $(DATA:%.log.gz=%.ps) > guides.t

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
	cp guides_data.c ../src/guides_data.c.$(PROLOG)

rules:
	cat *.ps| grep finish |awk -F\| '{ printf "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n",$$2,$$3,$$4,$$5,$$6,$$7,$$8,$$9,$$10,$$11,$$12,$$13,$$14,$$15,$$16,$$17,$$18,$$19 }' | grep -v 'lex(' | grep -v finish | grep . | sort | uniq -c | sort -nr | awk '{ printf "frequent_rule(%s,%s).\n",$$2,$$1 }' > rules

## usage e.g.: make -j 20 PATTERN=pei mores 
MORE=$(basename $(basename $(notdir $(wildcard ../Suites/Machine/$(PATTERN)*.sents.gz))))
mores:
	$(MAKE) $(MORE:%=%.log.gz)

alpino: cdb cgn_exs eans extra g_suite h_suite i_suite j_suite lassy-doc\
        leuven_yellow_pages lot_test_suite1 novelsample_corrected ntv-suite qa Titaantjes wpspel

lassysmall:
	$(MAKE) mores PATTERN=lassysmall\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/LassySmall end_hook=best_score\(left_corners\) max_sentence_length=25"

cdb:
	$(MAKE) mores PATTERN=cdb\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/cdb end_hook=best_score\(left_corners\) max_sentence_length=25"

cgn_exs:
	$(MAKE) mores PATTERN=cgn_exs\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/cgn_exs end_hook=best_score\(left_corners\) max_sentence_length=25"

eans:
	$(MAKE) mores PATTERN=eans\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/eans end_hook=best_score\(left_corners\) max_sentence_length=25"

extra:
	$(MAKE) mores PATTERN=extra_\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/extra end_hook=best_score\(left_corners\) max_sentence_length=25"

g_suite:
	$(MAKE) mores PATTERN=g_suite\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/g_suite end_hook=best_score\(left_corners\) max_sentence_length=25"

h_suite:
	$(MAKE) mores PATTERN=h_suite\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/h_suite end_hook=best_score\(left_corners\) max_sentence_length=25"

j_suite:
	$(MAKE) mores PATTERN=j_suite\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/j_suite end_hook=best_score\(left_corners\) max_sentence_length=25"

i_suite:
	$(MAKE) mores PATTERN=i_suite\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/i_suite end_hook=best_score\(left_corners\) max_sentence_length=25"

lassy-doc:
	$(MAKE) mores PATTERN=lassy-doc\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/lassy-doc end_hook=best_score\(left_corners\) max_sentence_length=25"

leuven_yellow_pages:
	$(MAKE) mores PATTERN=leuven_yellow_pages\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/leuven_yellow_pages end_hook=best_score\(left_corners\) max_sentence_length=25"

lot_test_suite1:
	$(MAKE) mores PATTERN=lot_test_suite1\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/lot_test_suite1 end_hook=best_score\(left_corners\) max_sentence_length=25"

novelsample_corrected:
	$(MAKE) mores PATTERN=novelsample_corrected\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/novelsample_corrected end_hook=best_score\(left_corners\) max_sentence_length=25"

ntv-suite:
	$(MAKE) mores PATTERN=ntv-suite\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/ntv-suite end_hook=best_score\(left_corners\) max_sentence_length=25"

qa:
	$(MAKE) mores PATTERN=qa\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/qa end_hook=best_score\(left_corners\) max_sentence_length=25"

Titaantjes:
	$(MAKE) mores PATTERN=Titaantjes\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/Titaantjes end_hook=best_score\(left_corners\) max_sentence_length=25"

wpspel:
	$(MAKE) mores PATTERN=wpspel\
        extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/wpspel end_hook=best_score\(left_corners\) max_sentence_length=25"


