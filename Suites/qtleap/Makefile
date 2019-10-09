ifeq "$(shell if [ -r ../../Makefile.include ]; then echo yes; fi)" "yes"
include ../../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

txts=${wildcard *_nl.txt}
sents=${txts:%.txt=%.sents}
pls=${txts:%.txt=%.pl}
gzs=${txts:%.txt=%.sents.gz}

all:$(pls) $(gzs)

%.sents : %.txt
	awk '{ N=N+1; print "#",N; print "\n\n", $$0, "\n\n" }' < $*.txt \
        | ../../Tokenization/tokenize.sh \
        | awk '/^# [0-9]/ {M=0; N=$$2} /^[^#]/ { M=M+1; printf "$*-%d-%d|%s\n",N,M,$$0}' \
        > $*.sents

%.pl : %.sents
	echo ":- module(alpino_suite,[ sentence/2 ])." > $*.pl
	echo >> $*.pl
	cat $< |\
	sed -e "s/| /|/" \
            -e "s/  *$$//" \
        | $(HDRUG) -notk -l $(ALPINO_HOME)/src/utils -l $(ALPINO_HOME)/Suites/create_pl_suite \
                 -cmd "to_suite(key)" >>$*.pl

%.sents.gz: %.sents
	gzip $*.sents

install:
	cp $(pls) ..
	cp $(gzs) ../Machine/

