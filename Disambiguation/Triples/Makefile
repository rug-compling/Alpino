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

S_FSA_PREFIX=/net/aps/haytabo/src/s_fsa-0.51/fsa_prefix 

ls:
	ls -lhS $(sorted)

## all except NLWIKI20110804 and EUROPARL, for which there are extended
## versions in $(LLE)
LLSORTED=$(LL)/EINDHOVEN/COMPACT.sorted \
 $(LL)/EMEA/COMPACT.sorted \
 $(LL)/SENSEVAL/COMPACT.sorted \
 $(LL)/TROONREDE/COMPACT.sorted \
 $(LL)/WR-P-E-A/COMPACT.sorted \
 $(LL)/WR-P-E-C/COMPACT.sorted \
 $(LL)/WR-P-E-E/COMPACT.sorted \
 $(LL)/WR-P-E-F/COMPACT.sorted \
 $(LL)/WR-P-E-G/COMPACT.sorted \
 $(LL)/WR-P-E-H/COMPACT.sorted \
 $(LL)/WR-P-E-I/COMPACT.sorted \
 $(LL)/WR-P-E-J/COMPACT.sorted \
 $(LL)/WR-P-E-K/COMPACT.sorted \
 $(LL)/WR-P-E-L/COMPACT.sorted \
 $(LL)/WR-P-P-B/COMPACT.sorted \
 $(LL)/WR-P-P-C/COMPACT.sorted \
 $(LL)/WR-P-P-D/COMPACT.sorted \
 $(LL)/WR-P-P-E/COMPACT.sorted \
 $(LL)/WR-P-P-F/COMPACT.sorted \
 $(LL)/WR-P-P-G/COMPACT.sorted \
 $(LL)/WR-P-P-H/COMPACT.sorted \
 $(LL)/WR-P-P-I/COMPACT.sorted \
 $(LL)/WR-P-P-J/COMPACT.sorted \
 $(LL)/WR-P-P-K/COMPACT.sorted \
 $(LL)/WR-U-E-A/COMPACT.sorted \
 $(LL)/WR-U-E-D/COMPACT.sorted \
 $(LL)/WR-U-E-E/COMPACT.sorted \
 $(LL)/WS-U-E-A/COMPACT.sorted \
 $(LL)/WS-U-T-B/COMPACT.sorted


LLESORTED=$(LLE)/Books/COMPACT.sorted \
 $(LLE)/CHILDES/COMPACT.sorted \
 $(LLE)/CLEF/COMPACT.sorted \
 $(LLE)/DGT/COMPACT.sorted \
 $(LLE)/DutchWebCorpus/COMPACT.sorted \
 $(LLE)/europarl7/COMPACT.sorted \
 $(LLE)/GELOOFDERKAMERADEN/COMPACT.sorted \
 $(LLE)/GlobalVoices/COMPACT.sorted \
 $(LLE)/JRC-Acquis/COMPACT.sorted \
 $(LLE)/News-Commentary11/COMPACT.sorted \
 $(LLE)/ParaCrawl/COMPACT.sorted \
 $(LLE)/Tatoeba/COMPACT.sorted \
 $(LLE)/TED2013/COMPACT.sorted \
 $(LLE)/wiki2017/COMPACT.sorted \
 $(LLE)/OpenSubtitles2018/COMPACT.sorted \

LLRSORTED=$(LLR)/CGN/COMPACT.sorted \
 $(LLR)/VK1997/COMPACT.sorted \
 $(LLR)/TWNC/COMPACT.sorted \
 $(LLR)/MEDIARGUS/COMPACT.sorted \
 $(LLR)/NLCOW/COMPACT.sorted

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
	$(S_FSA_PREFIX) -d temp2.fsa -a\
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
