ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

sents=$(wildcard *.sents)

all: $(sents:%.sents=%.pl) 

install:

clean:

qa_application_type_text=":- hdrug_util:set_flag(application_type,qa)."

## assume there already is a key (after |-symbol):
## this is robust against multiple occurrences of |
%.pl : %.sents
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

