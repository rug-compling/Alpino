## defaults per suite for test1 target
cdb_options=-flag syntax_features cdb/pc_weights
extra_options=-flag syntax_features cdb/px_weights

annotate:
	mkdir -p $(suite)
	$(ALPINO_HOME)/src/Alpino -notk cmdint=off\
          -veryfast\
          user_max=600000 debug=1\
          xml_format_frame=on\
          xml_format_failed_parse=on\
          display_main_parts=off\
          display_quality=off\
          -flag suite ../SENTS/$(suite)\
          -flag treebank $(suite)\
          demo=off\
          end_hook=xml $(extra)\
          $($(suite)_options)\
          batch_command="parser_comparisons" 2> $(suite).log


Parser/%.conllu.cln: Parser/%.conllu Gold/%.conllu
	python3 goodkeys.py Gold/$*.conllu Parser/$*.conllu > Parser/$*.conllu.cln


%.compare: Gold/%.conllu Parser/%.conllu.cln
	../UD2.0/conll17_ud_eval.py -v Gold/$*.conllu Parser/$*.conllu.cln > $*.compare


ALUD=~/bin/alud
ALUD_OPTIONS=-t -d -e

conllu: gold_conllu parser_conllu

SUITES=cdb cgn_exs eans extra g_suite h_suite i_suite leuven_yellow_pages lot_test_suite1 novelsample_corrected ntv-suite qa wpspel

gold_conllu: $(SUITES:%=Gold/%.conllu)

parser_conllu: $(SUITES:%=Parser/%.conllu)

compare: $(SUITES:%=%.compare)

Gold/%.conllu: ../Treebank/Enhanced/%.dact $(ALUD)
	ls -v ../Treebank/Enhanced/$*/*.xml | $(ALUD) $(ALUD_OPTIONS) > Gold/$*.conllu 2> Gold/$*.conllu.err

Parser/%.conllu: %/*.xml $(ALUD)
	ls -v $*/*.xml |  $(ALUD) $(ALUD_OPTIONS) -x > Parser/$*.conllu 2>Parser/$*.conllu.err

parse: $(SUITES:%=%.log)

%.log:
	$(MAKE) annotate suite=$*

