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

#HPCTARGET=left_corners
HPCTARGET=annotated_job

left_corners:
	zcat ../Suites/Machine/$(suite).sents.gz | \
        $(ALPINO) -flag suite undefined -parse 2> $(DISK)/$(suite)-spec.log | gzip > $(DISK)/$(suite).lc.gz 
	gzip $(DISK)/$(suite)-spec.log
	mv $(DISK)/$(suite)-spec.log.gz $(suite).log.gz
	mv $(DISK)/$(suite).lc.gz .
	$(MAKE) $(suite).ps

### for LassySmall:
### make jobs PATTERN=lassysmall
annotated_job:
	zcat $(ALPINO_HOME)/$(suite).sents.gz | \
        $(ALPINO) $(ANNOTATED_OPTIONS) -flag comments $(suite) -flag treebank $(ALPINO_HOME)/Treebank/LassySmall\
                  -flag suite undefined -parse 2> $(suite)-spec.log | gzip > $(suite).lc.gz

%.ps: %.lc.gz
	zcat $< \
        | grep '^LEFTCORNER#' \
        | sed -e 's/^LEFTCORNER#[^|]*[|]//'\
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

psfiles=$(DATA:%.log.gz=%.ps) 
used_rules.pl: $(psfiles)
	cat $(psfiles)| grep finish |awk -F\| '{ printf "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n",$$2,$$3,$$4,$$5,$$6,$$7,$$8,$$9,$$10,$$11,$$12,$$13,$$14,$$15,$$16,$$17,$$18,$$19 }' | grep -v 'lex(' | grep -v finish | grep . | sort | uniq -c | sort -nr | awk '{ printf "frequent_rule(%s,%s).\n",$$2,$$1 }' > used_rules.pl

all_rules.pl: ../Grammar/grammar_lc.pl
	Alpino batch_command="list_rules" > all_rules.pl

unused: used_rules.pl all_rules.pl
	(  echo "[used_rules].";\
           echo "[all_rules].";\
           echo 'rule_id(Id), \+ ( frequent_rule(Id,_); frequent_rule(gap(Id),_) ), format("~w~n",[Id]), fail ; true.'\
        )  | sicstus

disappeared: used_rules.pl all_rules.pl
	(  echo "[used_rules].";\
           echo "[all_rules].";\
           echo 'frequent_rule(Id,_), \+ ( rule_id(Id);  Id = gap(Id2), rule_id(Id2)), format("~w~n",[Id]), fail ; true.'\
        )  | sicstus

## usage e.g.: make -j 20 PATTERN=pei mores 
MORE=$(basename $(basename $(notdir $(wildcard ../Suites/Machine/$(PATTERN)*.sents.gz))))
mores:
	$(MAKE) $(MORE:%=%.log.gz)


ANNOTATED_OPTIONS=-slow stop_if_optimal=on end_hook=best_score\(left_corners\) max_sentence_length=25 after_timeout_options=testN user_max=720000

alpino: cdb cgn_exs eans extra g_suite h_suite i_suite j_suite lassy-doc\
        leuven_yellow_pages lot_test_suite1 novelsample_corrected ntv-suite qa Titaantjes wpspel

lassysmall:
	$(MAKE) mores PATTERN=lassysmall\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/LassySmall"

cdb:
	$(MAKE) mores PATTERN=cdb\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/cdb"

cgn_exs:
	$(MAKE) mores PATTERN=cgn_exs\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/cgn_exs"

eans:
	$(MAKE) mores PATTERN=eans\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/eans"

extra:
	$(MAKE) mores PATTERN=extra_\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/extra"

g_suite:
	$(MAKE) mores PATTERN=g_suite\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/g_suite"

h_suite:
	$(MAKE) mores PATTERN=h_suite\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/h_suite"

j_suite:
	$(MAKE) mores PATTERN=j_suite\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/j_suite"

i_suite:
	$(MAKE) mores PATTERN=i_suite\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/i_suite"

lassy-doc:
	$(MAKE) mores PATTERN=lassy-doc\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/lassy-doc"

leuven_yellow_pages:
	$(MAKE) mores PATTERN=leuven_yellow_pages\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/leuven_yellow_pages"

lot_test_suite1:
	$(MAKE) mores PATTERN=lot_test_suite1\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/lot_test_suite1"

novelsample_corrected:
	$(MAKE) mores PATTERN=novelsample_corrected\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/novelsample_corrected"

ntv-suite:
	$(MAKE) mores PATTERN=ntv-suite\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/ntv-suite"

qa:
	$(MAKE) mores PATTERN=qa\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/qa"

Titaantjes:
	$(MAKE) mores PATTERN=Titaantjes\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/Titaantjes"

wpspel:
	$(MAKE) mores PATTERN=wpspel\
        extra="$(ANNOTATED_OPTIONS) -flag treebank $(ALPINO_HOME)/Treebank/wpspel"


#### random subset of SoNaR:
#### find -name '*.sents.gz' | xargs zcat | $A/Suites/random_lines -p 0.01 | ( cd /dev/shm ; $A/Suites/split_in_parts -p 5000 -o ll )

