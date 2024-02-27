ifeq "$(shell if [ -r ../../Makefile.include ]; then echo yes; fi)" "yes"
include ../../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

#########################################################################

TARGETDIR           = ../MODELS/

TESTSUITE           = parool2001_test
TESTINGDATA         = $(TESTSUITE).log.gz
TR_TESTINGDATA      = $(TESTINGDATA:%.log.gz=%.fr)
STR_TESTINGDATA     = $(TESTINGDATA:%.log.gz=%.sfr)
TESTINGLADATA       = $(TESTINGDATA:%.log.gz=%.la)
MAKETUPLE           = $(ALPINO_HOME)/fadd/maketuple.pl

EXTRACONTEXT        = 1
MAXSCORE            = 1350

sort = LANG=POSIX LC_ALL=POSIX sort -u

## this is also in CONSTRUCT_TRAINING/Makefile, do it there!
# parool2001_test.la: $(ALPINO_HOME)/PosTagger/tr_tag.pl $(ALPINO_HOME)/PosTagger/Construct/tr.pl
# 	zcat ../../Suites/parool2001_test.sents.gz |\
#         $(ALPINO_HOME)/src/Alpino\
#            suite=undefined\
#            penalties=undefined\
#            pos_tagger=off\
#            grammar=undefined\
#           display_lexical_analysis=tagger_test\
#           $(extra) batch_command=lex_all\
#                          2>parool2001_test.la.log > parool2001_test.la

###########################################################################

default: all

all: 
	make testing
	( cd TRAINING ; make bin; make -j sfrs; make -j freqframes ; make all )
	$(MAKE) tag2.tpl tag3.tpl tag4.tpl prefix2.tpl prefix3.tpl context3.tpl\
                wordTag.tpl tagWord.tpl # wordTagLex.fsa lexicon_files
	$(MAKE) logs

logs: $(TESTSUITE).frames.testing $(TESTSUITE).la.testing
	@wc -l words tags usedContext tag2 tag3 tag4 | grep -v total >>log
	@make evals | grep '^#S' >>log

install:
	for t in context3.tpl  prefix2.tpl  prefix3.tpl  tag2.tpl\
                 tag3.tpl  tag4.tpl  tagWord.tpl  wordTag.tpl\
                 context.fsa  tags.fsa  words.fsa usedContext;\
           do if cmp -s $$t $(TARGETDIR)/$$t; then true ; else cp -f $$t $(TARGETDIR)/$$t; fi; done
	if cmp -s TRAINING/frames.pl ../../Grammar/frames.pl; then true ; else cp -f TRAINING/frames.pl ../../Grammar/frames.pl; fi

include Makefile.patterns

###########################################################################

testing: $(TESTSUITE).frames.testing $(TESTSUITE).la.testing

##########################################################################

$(TESTSUITE).frames.testing: $(STR_TESTINGDATA)
	zcat $(STR_TESTINGDATA) |\
         $(ALPINO_HOME)/PosTagger/Construct/rewriteNgramData\
             > $(TESTSUITE).frames.testing

$(TESTSUITE).la.testing: $(TESTINGDATA) $(TESTINGLADATA)
	zcat $(TESTINGDATA) | cat | $(ALPINO_HOME)/PosTagger/Construct/filterAnalysis -l $(TESTINGLADATA) -n $(TESTSUITE).la.testing0
	cat $(TESTSUITE).la.testing0 | \
          $(ALPINO_HOME)/PosTagger/Construct/rewriteNgramData\
             > $(TESTSUITE).la.testing

##########################################################################

tag2.tpl: tags.fsa context.fsa tag2
	$(MAKETUPLE) total=3 words=2 sep='\|' out=tag2.tpl tag2 +context +tags

tag3.tpl: tags.fsa context.fsa tag3
	$(MAKETUPLE) total=4 words=3 sep='\|' out=tag3.tpl tag3 +tags +context +tags

tag4.tpl: tags.fsa context.fsa tag4
	$(MAKETUPLE) total=5 words=4 sep='\|' out=tag4.tpl tag4 +tags +tags +context +tags

prefix2.tpl: tags.fsa context.fsa prefix2
	$(MAKETUPLE) total=4 words=2 sep='\|' out=prefix2.tpl prefix2 +tags +context

prefix3.tpl: tags.fsa context.fsa prefix3
	$(MAKETUPLE) total=5 words=3 sep='\|' out=prefix3.tpl prefix3 +tags +tags +context

wordTag.tpl: tags.fsa words.fsa wordTag
	$(MAKETUPLE) total=3 words=2 sep='\|' out=wordTag.tpl wordTag +words +tags

tagWord.tpl: tags.fsa words.fsa tagWord
	$(MAKETUPLE) total=3 words=2 sep='\|' out=tagWord.tpl tagWord +tags +words

context3.tpl: tags.fsa context.fsa context3
	$(MAKETUPLE) total=4 words=3 sep='\|' out=context3.tpl context3 +context +tags +context

#############################################################################

## sort again, perhaps we changed something manually...
tags.fsa: tags
	$(sort) tags | grep . | $(S_FSA_BUILD) -N -o tags.fsa

words.fsa: words
	$(sort) words | grep . | $(S_FSA_BUILD) -N -o words.fsa

context.fsa: context
	cat context | grep . | $(S_FSA_BUILD) -N -o context.fsa

wordTagLex.fsa: wordTagLex
	$(sort) wordTagLex |\
        $(S_FSA_MORPH_SCRIPT) |\
        $(sort) | grep . |\
        $(S_FSA_BUILD) > wordTagLex.fsa

## use both wordTag and tagWord, because different thresholds are used
words: wordTag tagWord
	awk -F\| '{ print $$1 }' wordTag | $(sort) > words0
	awk -F\| '{ print $$2 }' tagWord | $(sort) >> words0
	$(sort) words0 > words
	rm -f words0

tags: wordTag tag2 tag3 tag4 context3\
             prefix3 prefix2
	awk -F\| '{ print $$2 }' wordTag                      | $(sort) >  tags0
	awk -F\| '{ print $$2 }' tag2                         | $(sort) >> tags0
	awk -F\| '{ print $$2 }' context3                     | $(sort) >> tags0
	awk -F\| '{ print $$1 }' prefix2                      | $(sort) >> tags0
	awk -F\| '{ printf "%s\n%s\n",$$1,$$3 }' tag3         | $(sort) >> tags0
	awk -F\| '{ printf "%s\n%s\n",$$1,$$2 }' prefix3      | $(sort) >> tags0
	awk -F\| '{ printf "%s\n%s\n%s\n",$$1,$$2,$$4 }' tag4 | $(sort) >> tags0
	$(sort) tags0 >tags
	rm -f tags0

context: context3
	awk -F\| '{ printf "%s\n%s\n",$$1,$$3 }' context3 | $(sort) > context

############################################################################

createlex: createlex.cc
	g++ createlex.cc -o createlex -Wall

lexicon_files: wordTagLex createlex
	$(ALPINO_HOME)/PosTagger/Construct/createlex wordTagLex sufLexAll sufLexCaps sufLexNonCaps
	touch lexicon_files

sufLexAll_prep_atg: lexicon_files
	prep_atg.pl <sufLexAll >sufLexAll_prep_atg
sufLexCaps_prep_atg: lexicon_files
	prep_atg.pl <sufLexCaps >sufLexCaps_prep_atg
sufLexNonCaps_prep_atg: lexicon_files
	prep_atg.pl <sufLexNonCaps >sufLexNonCaps_prep_atg

sufLexAll.fsa: sufLexAll_prep_atg
	$(sort) sufLexAll_prep_atg |  $(S_FSA_BUILD) -A '+' -X >sufLexAll.fsa

sufLexCaps.fsa: sufLexCaps_prep_atg
	$(sort) sufLexCaps_prep_atg |  $(S_FSA_BUILD) -A '+' -X >sufLexCaps.fsa

sufLexNonCaps.fsa: sufLexNonCaps_prep_atg
	$(sort) sufLexNonCaps_prep_atg  | $(S_FSA_BUILD) -A '+' -X >sufLexNonCaps.fsa

suffixLexiconAutomata: sufLexAll.fsa sufLexCaps.fsa sufLexNonCaps.fsa wordTagLex.fsa

installSuffixLexicon: sufLexAll.fsa sufLexCaps.fsa sufLexNonCaps.fsa wordTagLex.fsa
	cp sufLexAll.fsa sufLexCaps.fsa sufLexNonCaps.fsa heuristics wordTagLex.fsa $(TARGETDIR)

cleanLexicon:
	rm *_prep_atg -f

realcleanLexicon: cleanLexicon
	rm sufLexAll* sufLexCaps* sufLexNonCaps* -f

###########################################################################

TAGGER=$(ALPINO_HOME)/PosTagger/tagger

include $(ALPINO_HOME)/PosTagger/Construct/Makefile.eval

.PRECIOUS : %.fr %.tmi %.tmo %.sfr

# surv_big.pl: fr
# 	cat fr |\
#         awk -v IF="\t" -v OFS="\t" '{ if ($$2 > 20) { print $$3,$$4,$$5,$$6 }}' |\
#         Alpino cmdint=off -notk batch_command=go -l toprolog.pl > surv_big.pl

# frs = $(wildcard TRAINING/*.fr)
# fr: $(frs)
# 	zcat $(frs) |\
#         ./create_surv_big.py > fr

