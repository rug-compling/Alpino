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
train_options=user_max=120000 use_guides=off max_sentence_length=25 after_timeout_options=off
test_options=user_max=1000000 use_guides=on max_sentence_length=30 after_timeout_options=off

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


test:
	$(MAKE) -s lexical_analysis suite=parool2001_test &
	$(MAKE) ps
	zcat $(PS:%=%.log.gz) > parool2001_test.log.gz
#	$(MAKE) -s frames TRAIN_TEST=test &

install_test:
	cp $(suite).la $(suite).log.gz ..

include $(ALPINO_HOME)/Makefile.errormining


TTS=tt_part00001 tt_part00002 tt_part00003 tt_part00004 tt_part00005 tt_part00006 tt_part00007 tt_part00008 tt_part00009 tt_part00010 tt_part00011 tt_part00012 tt_part00013 tt_part00014 tt_part00015 tt_part00016 tt_part00017 tt_part00018 tt_part00019 tt_part00020

TUS=tu_part00001 tu_part00002 tu_part00003 tu_part00004 tu_part00005 tu_part00006 tu_part00007 tu_part00008 tu_part00009 tu_part00010 

PS=p_part00001 p_part00002 p_part00003 p_part00004 p_part00005 p_part00006 p_part00007 p_part00008 p_part00009 p_part00010 p_part00011 p_part00012 p_part00013 p_part00014 p_part00015 p_part00016 p_part00017 p_part00018 p_part00019 p_part00020

ps:
	$(MAKE) -j $(J) $(PS:%=%.test.log.gz)

tts:
	$(MAKE) -j $(J) $(TTS:%=%.log.gz) 

tus:
	$(MAKE) -j $(J) $(TUS:%=%.tu.log.gz) 

%.log.gz: ../../../Suites/Machine/%.sents.gz
	$(MAKE) frames suite=$*

%.test.log.gz: ../../../Suites/Machine/%.sents.gz
	$(MAKE) frames suite=$* TRAIN_TEST=test &

%.tu.log.gz: ../../../Suites/Machine/%.sents.gz
	$(MAKE) frames suite=$* extra="-slow stop_if_optimal=on -flag treebank $(ALPINO_HOME)/Treebank/extra end_hook=best_score\(frames\)"

