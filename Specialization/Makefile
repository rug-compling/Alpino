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


# only add additional left corners:
#EXTRA=end_hook=new_left_corners

# specializes a specialized version further!:
##EXTRA=-veryfast max_sentence_length=35 end_hook=new_left_corners

HPCTARGET=left_corners

left_corners:
	zcat ../Suites/Machine/$(suite).sents.gz | \
        $(ALPINO) $(extra) -flag suite undefined -parse 2> $(DISK)/$(suite)-spec.log
	gzip $(DISK)/$(suite)-spec.log
	mv $(DISK)/$(suite)-spec.log.gz $(suite).log.gz
	$(MAKE) $(suite).ps

nleft_corners:
	echo "parser_comparisons." | \
        $(ALPINO) -flag suite ../Suites/$(suite) $(extra) 2> $(DISK)/$(suite)-spec.log
	gzip $(DISK)/$(suite)-spec.log
	mv $(DISK)/$(suite)-spec.log.gz $(suite).log.gz

#%.left_corners:%.log.gz
#	zcat $< |\
#	grep '^LEFTCORNER#' \
#       | sed -e 's/^LEFTCORNER#//'\
#        | gzip > $@

%.ps: %.log.gz ../src/guides.pl
	zcat $< \
        | grep '^LEFTCORNER#' \
        | sed -e 's/^LEFTCORNER#//'\
        | sort -u \
        | ./generate_prefixes \
        | sort -u > $@

%.log.gz:../Suites/Machine/%.sents.gz
	zcat $< | \
        $(ALPINO) $(extra) -flag suite undefined -parse 2> $(DISK)/$*-spec.log
	gzip $(DISK)/$*-spec.log
	mv $(DISK)/$*-spec.log.gz $*.log.gz

include $(ALPINO_HOME)/Makefile.errormining

PAT=
DATA=$(wildcard $(PAT)*.log.gz)

echo:
	echo $(DATA:%.log.gz=%.ps)

guides.t: $(DATA:%.log.gz=%.ps)
	sort  -u $(DATA:%.log.gz=%.ps) > guides.t

guides.txt: guides.t
	cat guides.t |\
   ../src/Alpino cmdint=off debug=1 -notk -l to_predict_guides \
             -cmd "to_sguides(tpl_prefix)" | sort -u > guides.txt

make_guide_vector: make_guide_vector.cpp
	g++ -Wall -o $@ -O2 $<

guides_data.c: guides.txt make_guide_vector
	awk -F\| '{ print $$1}' guides.txt | ./make_guide_vector > $@

install: guides_data.c
	cp guides_data.c ../src/guides_data.c.$(PROLOG)

tts:
	$(MAKE) left_corners suite=tt_part00001 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00002 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00003 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00004 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00005 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00006 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00007 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00008 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00009 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00010 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00011 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00012 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00013 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00014 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00015 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00016 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00017 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00018 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00019 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00020 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00021 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00022 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00023 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00024 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tt_part00025 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tu_part00001 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tu_part00002 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tu_part00003 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tu_part00004 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tu_part00005 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tu_part00006 EXTRA=pos_tagger_m=5 &
	$(MAKE) left_corners suite=tu_part00007 EXTRA=pos_tagger_m=5 &

rules:
	cat *.ps | grep finish |awk -F\| '{ printf "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n",$$2,$$3,$$4,$$5,$$6,$$7,$$8,$$9,$$10,$$11,$$12,$$13,$$14,$$15,$$16,$$17,$$18,$$19 }' | grep -v 'lex(' | grep -v finish | grep . | sort | uniq -c | sort -nr | awk '{ printf "frequent_rule(%s,%s).\n",$$2,$$1 }' > rules

