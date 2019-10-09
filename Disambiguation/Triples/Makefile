ifeq "$(shell if [ -r ../../Makefile.include ]; then echo yes; fi)" "yes"
include ../../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

PTH=1
TH=50
sorted=$(wildcard *.sorted)

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
