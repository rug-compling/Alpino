ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

sents=$(wildcard *.sents)
named_sents=$(wildcard *.named_sents)

suites=$(sents:%.sents=%) $(named_sents:%.named_sents=%)

all: $(suites:%=%.pl) tt tu

%.po : %.pl
	echo "compile('$*'), save_files('$*','$*')." | $(HDRUG) -notk

install:

clean:

realclean:
	/bin/rm -f $(suites:%=%.pl) tt tu tt.sents tu.sents

init=1
keyprefix=""

%.pl : %.sents
	echo ":- module(alpino_suite,[ sentence/2 ])." > $*.pl
	echo >> $*.pl
	cat $*.sents |\
        sed -e "s/  *$$//" \
            -e "s/\\\\/\\\\\\\\/g" \
            -e "s/'/\\\\'/g" \
            -e "s/ /','/g" |\
        awk 'BEGIN   { N=$(init) } \
             /^--/ { N++ }       \
             !/^(%|--)/ { printf("my_sentence($(keyprefix)%d,[%c%s%c]).\n",N,39,$$0,39); N++;}' | $(HDRUG) -notk -l $(ALPINO_HOME)/Suites/echo >>$*.pl
	(echo; echo "expand_sentences."; echo) >>$*.pl

qa_application_type_text=":- hdrug_util:set_flag(application_type,qa)."


## assume there already is a key (after |-symbol):
## this is robust against multiple occurrences of |
%.pl : %.named_sents
	echo ":- module(alpino_suite,[ sentence/2 ])." > $*.pl
	echo >> $*.pl
	echo $($*_application_type_text) >> $*.pl
	echo >> $*.pl
	cat $< |\
	sed -e "s/| /|/" \
            -e "s/  *$$//" \
        | $(HDRUG) -notk -l $(ALPINO_HOME)/src/utils -l $(ALPINO_HOME)/Suites/create_pl_suite \
                 -cmd "to_suite(key)" >>$*.pl
	(echo; echo "expand_sentences."; echo) >>$*.pl

train_test.pl: 
	$(HDRUG) -notk -l create_train_test.pl >train_test.pl

parool_test:
	zcat TwNC-01/parool2001/parool2001_part01.sents.gz |\
             lines -f 1 -t 2000 | gzip > parool2001_test.sents.gz

include $(ALPINO_HOME)/SuffixArrays/Examples/Makefile

THIS=$(ALPINO_HOME)/SuffixArrays

SUFFIX_ARRAY_OPTIONS=-n 30 -f 5 -N -v

COLLOCATION_OPTION= -t 0.01
%.colc : %.ngram
	colc $(COLLOCATION_OPTION) $< | sort -t\| -k1,1nr -k2,2nr > $@

%.fsa: %.sents.gz
	zcat $*.sents.gz |\
        tr -s '\012\011 ' '\012' |\
        grep .|\
        LANG=POSIX LC_ALL=POSIX sort -u |\
        fsa_build -N -o $@

%.ngram: %.fsa
	zcat $*.sents.gz |\
	tr -s '\012\011 ' '\012' |\
        $(ALPINO_HOME)/SuffixArrays/suffix_array -e -d $*.fsa -w -G\| -n 20 -f 1 -u -N > $@

questions.sents: questions.pl
	Alpino -flag suite questions batch_command=list_questions \
           > questions.sents

#%.sents: %.pl
#	../src/Alpino -notk -flag suite $* batch_command=list_sentences \
#             > $*.sents


%.pl:%.tags
	Alpino -notk -u cmdint=off -l read_tags < $< > $*.pl

PPH_$(i).pl: PPH.pl test_$(i).pl
	cat PPH.pl test_$(i).pl > PPH_$(i).pl

PPHs:
	for i in 2 3 4 5 6 7 8 9; do make PPH_$$i.pl i=$$i; done

OneLiner.sents.gz: OneLiner_NL_text.txt
	$(ALPINO_HOME)/Tokenization/add_key < OneLiner_NL_text.txt \
      | $(ALPINO_HOME)/Tokenization/tokenize.sh \
      | $(ALPINO_HOME)/Tokenization/number_sents \
      | gzip > OneLiner.sents.gz

## op machine faunus.let.rug.nl
## directory /mnt/D/erikt/twitter/
## (  for file in 20110106:*.out.gz; do zcat $file ; done ) | ./getTwitterText  > /storage2/hpc/Alpino/Suites/twitter.txt

%.sents: %.twt
	cat $*.twt \
          | sed -e 's|&gt;|>|g' \
          | sed -e 's|&lt;|<|g' \
          | sed -e 's|&bull;|•|g' \
          | $(ALPINO_HOME)/Tokenization/tokenize.sh \
          | awk '{N++; printf "%s|%s\n",N,$$0 }' \
         >  $*.sents

sentsfiles=g_suite.sents h_suite.sents i_suite.sents m_suite.sents leuven_yellow_pages.sents\
           h_suite.sents journaal.sents lot_test_suite1.sents ovis.sents
namedsentsfiles=cgn_exs.named_sents lassy.named_sents qa.named_sents

ifeq "$(shell if [ -d Machine/ ]; then echo yes; fi)" "yes"
include ../Makefile.include
tt: $(sentsfiles) $(namedsentsfiles) 
	cat $(sentsfiles) | grep -v -e '^[%]' -e '^--' > tt.sents
	awk -F\| '{ print $$2 }' $(namedsentsfiles) >> tt.sents
	cat tt.sents  | ./split_in_parts -p 500 -o tt
	for file in tt_part*.sents.gz ; do if cmp -s $$file Machine/$$file; then true ; else cp -f $$file Machine/$$file; fi; done
	touch tt

tu: extra.sents
	cat extra.sents | grep -v -e '^[%]' -e '^--' > tu.sents
	cat tu.sents  | ./split_in_parts -p 200 -o tu
	for file in tu_part*.sents.gz; do if cmp -s $$file Machine/$$file; then true ; else cp -f $$file Machine/$$file; fi; done
	gzip -c extra.sents > Machine/extra.sents.gz
	touch tu
else

tt:

tu:

endif

u: nl_lassysmall-ud-dev.pl nl_lassysmall-ud-test.pl nl_lassysmall-ud-train.pl
	gzip -c nl_lassysmall-ud-dev.named_sents > Machine/nl_lassysmall-ud-dev.sents.gz
	gzip -c nl_lassysmall-ud-test.named_sents > Machine/nl_lassysmall-ud-test.sents.gz
	gzip -c nl_lassysmall-ud-train.named_sents > Machine/nl_lassysmall-ud-train.sents.gz

UNIVERSAL=/net/aistaff/gosse/Corpus/UniversalDependencies/UD_Dutch_LassySmall4/
nl_lassysmall-ud-dev.named_sents: $(UNIVERSAL)/nl_lassysmall-ud-dev.conllu
	conllu2sent.py < $<  | sed -e 's/[.]xml//' > $@
nl_lassysmall-ud-test.named_sents: $(UNIVERSAL)/nl_lassysmall-ud-test.conllu
	conllu2sent.py < $<  | sed -e 's/[.]xml//' > $@
nl_lassysmall-ud-train.named_sents: $(UNIVERSAL)/nl_lassysmall-ud-train.conllu
	conllu2sent.py < $<  | sed -e 's/[.]xml//' > $@

