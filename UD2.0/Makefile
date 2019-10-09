UD_EVAL=./conll17_ud_eval.py -v

#XQ2=/net/aistaff/gosse/Corpus/UniversalDependencies/UD_Dutch2.0/universal_dependencies_2.0.xq
XQ2=universal_dependencies_2.0.xq
### copied from /net/aistaff/gosse/Corpus/UniversalDependencies/UD_Dutch2.0/Makefile
### difference: this takes a single xml file with multiple parses, in order to
### ensure that the ordering of parses remains identical to the input
%.conllu : %.xml
	saxon $(XQ2) MODE=conll xml=$< \
      | sed 's/<code sentence-id="\([^"]*.xml\)" class="conllu">/# sent_id = \1/'  \
      | sed 's/<sentence.*>\(.*\)<\/sentence>/# text = \1/' \
      | grep -v 'pre>' |grep -v 'code>' \
      | sed -e 's/^ *!//' -e 's/^ *//' -e 's/ $$//' \
            -e 's/\&amp;/\&/g' -e 's/\&lt;/</g' \
      | sed -e '/text = / s/ \([:;,\.!?]\)/\1/g' \
            -e '/text = / s/( \([^)]*\) )/(\1)/g' \
            -e '/text = / s/" \([^"]*\) "/"\1"/g' \
            -e "/text = / s/' \([^']*\) '/'\1'/g" \
            -e '/text = / s/« \([^»]*\) »/«\1»/g' > $@

DEVTEST=test
#DEVTEST=dev

alp-$(DEVTEST).xml:
	make -s -f $(ALPINO_HOME)/Treebank/Makefile.Machine dump\
             SUITES=$(ALPINO_HOME)/Suites\
             suite=ud2-$(DEVTEST) > $@ 2> $@.log

alp-model-$(DEVTEST).xml:
	make -s -f $(ALPINO_HOME)/Treebank/Makefile.Machine dump\
             SUITES=$(ALPINO_HOME)/Suites\
             suite=ud2-$(DEVTEST)\
             extra=syntax_features=ud2_weights > $@ 2> $@.log

tokenized-$(DEVTEST).xml:
	make -s -f $(ALPINO_HOME)/Treebank/Makefile.Machine dump\
             SUITES=$(ALPINO_HOME)/Suites\
             suite=ud2-$(DEVTEST)-tokenized > $@ 2> $@.log

tokenized-model-$(DEVTEST).xml:
	make -s -f $(ALPINO_HOME)/Treebank/Makefile.Machine dump\
             SUITES=$(ALPINO_HOME)/Suites\
             suite=ud2-$(DEVTEST)-tokenized\
             extra=syntax_features=ud2_weights > $@ 2> $@.log

udpipe-$(DEVTEST).xml:
	make -s -f $(ALPINO_HOME)/Treebank/Makefile.Machine dump\
             SUITES=$(ALPINO_HOME)/Suites\
             suite=ud2-$(DEVTEST)-udpipe > $@ 2> $@.log

udpipe-model-$(DEVTEST).xml:
	make -s -f $(ALPINO_HOME)/Treebank/Makefile.Machine dump\
             SUITES=$(ALPINO_HOME)/Suites\
             suite=ud2-$(DEVTEST)-udpipe\
             extra=syntax_features=ud2_weights > $@ 2> $@.log

NEW=/net/corpora/LassyDevelop/Treebank
new.xml: test.conllu $(NEW)/w*/*.xml
	echo '<?xml version="1.0" encoding="UTF-8"?>' > $@
	echo "<alpino_ds_list>" >> $@
	( for file in `grep sent_id test.conllu | sed -e 's/# sent_id = //'`;\
          do cat $(NEW)/*/$$file;\
          done ) | grep -v 'encoding="UTF-8"' >> $@
	echo "</alpino_ds_list>" >> $@


%.conll:%.conllu
	/net/shared/UniversalDependenciesTools/conllu_to_conllx.pl < $< | tr ' ' '_space_'  > $@

