ifeq "$(shell if [ -r ../../Makefile.include ]; then echo yes; fi)" "yes"
include ../../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

PTH=1
TH=30
###sorted=$(wildcard *.sorted)

sorted=$(LLSORTED) $(LLESORTED) $(LLRSORTED)

LL=/net/corpora/LassyLarge
LLE=/net/corpora/LassyLargeExtra
LLR=/net/shared/vannoord/LassyLargeRestricted

## all except NLWIKI20110804 and EUROPARL, for which there are extended
## versions in $(LLE)
LLSORTED=$(LL)/EINDHOVEN.sorted \
 $(LL)/EMEA.sorted \
 $(LL)/SENSEVAL.sorted \
 $(LL)/TROONREDE.sorted \
 $(LL)/WR-P-E-A.sorted \
 $(LL)/WR-P-E-C.sorted \
 $(LL)/WR-P-E-E.sorted \
 $(LL)/WR-P-E-F.sorted \
 $(LL)/WR-P-E-G.sorted \
 $(LL)/WR-P-E-H.sorted \
 $(LL)/WR-P-E-I.sorted \
 $(LL)/WR-P-E-J.sorted \
 $(LL)/WR-P-E-K.sorted \
 $(LL)/WR-P-E-L.sorted \
 $(LL)/WR-P-P-B.sorted \
 $(LL)/WR-P-P-C.sorted \
 $(LL)/WR-P-P-D.sorted \
 $(LL)/WR-P-P-E.sorted \
 $(LL)/WR-P-P-F.sorted \
 $(LL)/WR-P-P-G.sorted \
 $(LL)/WR-P-P-H.sorted \
 $(LL)/WR-P-P-I.sorted \
 $(LL)/WR-P-P-J.sorted \
 $(LL)/WR-P-P-K.sorted \
 $(LL)/WR-U-E-A.sorted \
 $(LL)/WR-U-E-D.sorted \
 $(LL)/WR-U-E-E.sorted \
 $(LL)/WS-U-E-A.sorted \
 $(LL)/WS-U-T-B.sorted


LLESORTED=$(LLE)/Books.sorted \
 $(LLE)/CHILDES.sorted \
 $(LLE)/CLEF.sorted \
 $(LLE)/DGT.sorted \
 $(LLE)/DutchWebCorpus.sorted \
 $(LLE)/europarl7.sorted \
 $(LLE)/GELOOFDERKAMERADEN.sorted \
 $(LLE)/GlobalVoices.sorted \
 $(LLE)/JRC-Acquis.sorted \
 $(LLE)/News-Commentary11.sorted \
 $(LLE)/ParaCrawl.sorted \
 $(LLE)/Tatoeba.sorted \
 $(LLE)/TED2013.sorted \
 $(LLE)/wiki2017.sorted \
 $(LLE)/OpenSubtitles2018.sorted \

LLRSORTED=$(LLR)/CGN.sorted \
 $(LLR)/VK1997.sorted \
 $(LLR)/TWNC.sorted \
 $(LLR)/MEDIARGUS.sorted \
 $(LLR)/NLCOW.sorted

## remember you might want to use 
## export TMPDIR=/dev/shm
all:
	$(MAKE) all-sorted marginals temp1.fsa temp2.fsa dep.pl corpus_frequency_features corpus_frequency_features.fsa inspect 

all-sorted: $(sorted)
	LANG=POSIX LC_ALL=POSIX sort -m $(sorted) \
        | $(ALPINO_HOME)/SuffixArrays/merge_ngrams \
        | awk -F\| '{ if (NF==3 && $$3 > $(PTH)) {printf "%s|%s|%s\n", $$1,$$2,$$3}}' \
        | gzip > all-sorted

%.marginals: %.sorted
	cat $*.sorted | Alpino -notk -l dep_stats.pl batch_command=marginals \
        | LANG=POSIX LC_ALL=POSIX sort \
        | $(ALPINO_HOME)/SuffixArrays/merge_ngrams \
        > $*.marginals


temp1.fsa: marginals
	zcat marginals \
        | awk -F\| '{ if ($$3>$(TH)) {printf "%s\t%s\t%d\n", $$2,$$2,$$3}}' \
        | $(S_FSA_MORPH_SCRIPT)\
        | LANG=POSIX LC_ALL=POSIX sort -u\
        | $(S_FSA_BUILD) -o temp1.fsa

temp2.fsa: all-sorted
	zcat all-sorted \
        | awk -F\| '{ if ($$3>$(TH)) {printf "%s\t%s\t%d\n", $$2,$$2,$$3}}' \
        | $(S_FSA_MORPH_SCRIPT)\
        | LANG=POSIX LC_ALL=POSIX sort -u\
        | $(S_FSA_BUILD) -o temp2.fsa

marginals: $(sorted:%.sorted=%.marginals)
	LANG=POSIX LC_ALL=POSIX sort -m $(sorted:%.sorted=%.marginals) \
        | $(ALPINO_HOME)/SuffixArrays/merge_ngrams \
        | awk -F\| '{ if (NF==3 && $$3 > $(PTH)) {printf "%s|%s|%s\n", $$1,$$2,$$3}}' \
        | gzip\
        > marginals

dep.pl: temp2.fsa
	fsa_prefix -d temp2.fsa -a\
        | sed -e 's/^ //' \
        | awk -F+ '{ if (NF==3 && $$3 > $(TH)) { printf "%s.\n",$$1}}' | gzip > dep.pl

corpus_frequency_features: dep.pl dep_stats.pl temp1.fsa temp2.fsa
	zcat dep.pl | Alpino -notk  \
                      -l dep_stats batch_command=go | gzip > corpus_frequency_features

corpus_frequency_features.fsa: corpus_frequency_features
	zcat corpus_frequency_features \
        | $(S_FSA_MORPH_SCRIPT)\
        | LANG=POSIX LC_ALL=POSIX sort -u\
        | $(S_FSA_BUILD) -o corpus_frequency_features.fsa

clean:
	rm -f all-sorted corpus_frequency_features.fsa corpus_frequency_features temp1.fsa temp2.fsa dep.pl inspect

install:
	cp corpus_frequency_features.fsa $(ALPINO_HOME)/Grammar/corpus_frequency_features.fsa

inspect: corpus_frequency_features
	zcat corpus_frequency_features | tr ' ' '_'  | awk '{ print $$3,$$1 }' | sort -nr > inspect
