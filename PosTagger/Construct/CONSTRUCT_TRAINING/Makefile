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
train_options=user_max=120000 use_guides=off max_sentence_length=25
test_options=user_max=1000000 use_guides=on max_sentence_length=30

HPCTARGET=frames

frames:
	date > $(DISK)/$(suite)-fr.log
	zcat $(SUITES)/$(suite).sents.gz | \
        $(ALPINO) $(extra) -flag suite undefined -parse\
              2>> $(DISK)/$(suite)-fr.log
	gzip $(DISK)/$(suite)-fr.log
	mv $(DISK)/$(suite)-fr.log.gz $(suite).log.gz

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

suite=parool2001_test

test:
	$(MAKE) -s lexical_analysis  &
	$(MAKE) -s frames TRAIN_TEST=test &

install_test:
	cp $(suite).la $(suite).log.gz ..

include $(ALPINO_HOME)/Makefile.errormining

tts:
	$(MAKE) frames suite=tt_part00001 &
	$(MAKE) frames suite=tt_part00002 &
	$(MAKE) frames suite=tt_part00003 &
	$(MAKE) frames suite=tt_part00004 &
	$(MAKE) frames suite=tt_part00005 &
	$(MAKE) frames suite=tt_part00006 &
	$(MAKE) frames suite=tt_part00007 &
	$(MAKE) frames suite=tt_part00008 &
	$(MAKE) frames suite=tt_part00009 &
	$(MAKE) frames suite=tt_part00010 &
	$(MAKE) frames suite=tt_part00011 &
	$(MAKE) frames suite=tt_part00012 &
	$(MAKE) frames suite=tt_part00013 &
	$(MAKE) frames suite=tt_part00014 &
	$(MAKE) frames suite=tt_part00015 &
	$(MAKE) frames suite=tt_part00016 &
	$(MAKE) frames suite=tt_part00017 &
	$(MAKE) frames suite=tt_part00018 &
	$(MAKE) frames suite=tt_part00019 &
	$(MAKE) frames suite=tt_part00020 &
	$(MAKE) frames suite=tt_part00021 &
	$(MAKE) frames suite=tt_part00022 &
	$(MAKE) frames suite=tt_part00023 &
	$(MAKE) frames suite=tt_part00024 &
	$(MAKE) frames suite=tt_part00025 &

extra:
	$(MAKE) frames suite=extra \
                 extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/extra end_hook=best_score\(frames\)"
