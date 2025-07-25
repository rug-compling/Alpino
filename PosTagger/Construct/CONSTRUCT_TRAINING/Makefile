include $(ALPINO_HOME)/Makefile.hpc

ALPINO=$(ALPINO_HOME)/src/Alpino -notk cmdint=off\
          -fast\
          pos_tagger=off\
          robustness_allows_skips=off\
          debug=1 demo=off\
          display_main_parts=off\
          end_hook=frames\
          $($(TRAIN_TEST)_options)

TRAIN_TEST=train
#train_options=user_max=60000 use_guides=on max_sentence_length=30
train_options=user_max=120000 use_guides=off max_sentence_length=28 after_timeout_options=off use_guides=on
test_options=user_max=1000000 use_guides=on max_sentence_length=30 after_timeout_options=off

ANNOTATED_OPTIONS=-slow stop_if_optimal=on end_hook=best_score\(frames\) max_sentence_length=25 after_timeout_options=testN user_max=720000 

HPCTARGET=frames
TARGET=.

frames:
	date > $(DISK)/$(suite)-fr.log
	zcat $(SUITES)/$(suite).sents.gz | \
        $(ALPINO) $(extra) -flag suite undefined -parse\
              2>> $(DISK)/$(suite)-fr.log
	gzip $(DISK)/$(suite)-fr.log
	mv $(DISK)/$(suite)-fr.log.gz $(TARGET)/$(suite).log.gz

lexical_analysis:
	zcat $(SUITES)/$(suite).sents.gz | \
        $(ALPINO_HOME)/src/Alpino\
           suite=undefined\
           penalties=undefined\
           pos_tagger=off\
           grammar=undefined\
          display_lexical_analysis=tagger_test\
          $(extra) batch_command=lex_all\
                         2>$(suite).la.log > $(suite).la


test:
	$(MAKE) -s lexical_analysis suite=parool2001_test &
	$(MAKE) ps
	zcat $(PS:%=%.log.gz) | gzip > parool2001_test.log.gz

suite=parool2001_test
install_test:
	cp $(suite).la $(suite).log.gz ..

include $(ALPINO_HOME)/Makefile.errormining

PS=p_part00001 p_part00002 p_part00003 p_part00004 p_part00005 p_part00006 p_part00007 p_part00008 p_part00009 p_part00010 p_part00011 p_part00012 p_part00013 p_part00014 p_part00015 p_part00016 p_part00017 p_part00018 p_part00019 p_part00020

ps:
	$(MAKE) -j $(J) $(PS:%=%.test.log.gz) 

$(TARGET)/%.log.gz: ../../../Suites/Machine/%.sents.gz
	$(MAKE) frames suite=$*

%.test.log.gz: ../../../Suites/Machine/%.sents.gz
	$(MAKE) frames suite=$* TRAIN_TEST=test TARGET=.

## usage e.g.: make -j 20 PATTERN=pei mores 
MORE=$(basename $(basename $(notdir $(wildcard ../../../Suites/Machine/$(PATTERN)*.sents.gz))))
mores:
	$(MAKE) $(MORE:%=../TRAINING/%.log.gz) TARGET=../TRAINING

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

