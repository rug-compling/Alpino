ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

sentsfiles=g_suite.sents h_suite.sents m_suite.sents\
           leuven_yellow_pages.sents \
           lot_test_suite1.sents ovis.sents\
           i_suite.sents extra.sents
namedsentsfiles=cgn_exs.named_sents lassy-doc.named_sents qa.named_sents j_suite.named_sents\
                mwu_alt.named_sents novelsample_corrected.named_sents\
                Titaantjes.named_sents

suites=$(sentsfiles:%.sents=%) $(namedsentsfiles:%.named_sents=%)

all: $(suites:%=%.pl) installsents


%.po : %.pl
	echo "overrule_module('$*'), save_files('$*','$*')." | Alpino -notk cmdint=off 

install:

clean:

realclean:
	/bin/rm -f $(suites:%=%.pl) 

init=1
keyprefix=""

%.named_sents : %.sents
	cat $*.sents |\
        sed -e "s/  *$$//" |\
        awk 'BEGIN   { N=$(init) } \
             /^--/ { N++ }       \
             !/^(%|--)/ { printf("$(keyprefix)%s|%s\n",N,$$0); N++;}' >$*.named_sents

## assume there already is a key (after |-symbol):
## this is robust against multiple occurrences of |
%.pl : %.named_sents
	echo ":- module(alpino_suite,[ sentence/2 ])." > $*.pl
	echo >> $*.pl
	echo ":- hdrug_util:set_flag(print_suite,'$*')." >> $*.pl
	echo >> $*.pl
	echo $($*_application_type_text) >> $*.pl
	echo >> $*.pl
	cat $< |\
        tr -d '' |\
	sed -e "s/  *$$//" \
        | awk -F\| '{ if (NF>1 && $$2 ~ /./) print $$0 }' \
        | $(HDRUG) -notk -l $(ALPINO_HOME)/src/utils -l $(ALPINO_HOME)/Suites/create_pl_suite \
                 -cmd "to_suite(key)" >>$*.pl
	(echo; echo "expand_sentences."; echo) >>$*.pl

SHELL := /bin/bash
j_suite.named_sents:../Grammar/rules.gram
	grep '^%%EX%%' ../Grammar/rules.gram | sed -e 's/^%%EX%%  *//' > j_suite.named_sents0
	paste -d '|' <(tr ' :' '_@' <j_suite.named_sents0 |\
        sed -e "s/'/__Q1__/g" -e 's/"/__Q2__/g'   \
            -e "s/[(]/__Q3__/g" -e 's/[)]/__Q4__/g' \
            -e "s/;/__Q5__/g" \
            -e "s/«/__Q6__/g" -e 's/»/__Q7__/g' \
            -e "s/é/__Q7__/g" ) j_suite.named_sents0 \
          > j_suite.named_sents

installsents: $(suites:%=%.named_sents) $(suites:%=Parts/%_part00001.sents.gz)
	@for file in $(notdir $(wildcard Parts/*.sents.gz));\
        do if cmp -s Parts/$$file Machine/$$file;\
           then true;\
           else echo $$file; cp -f Parts/$$file Machine/ ;\
           fi;\
        done

Parts/%_part00001.sents.gz: %.named_sents
	cat $< |  (  cd Parts; $(ALPINO_HOME)/Suites/split_in_parts -o $* -p 20 )
