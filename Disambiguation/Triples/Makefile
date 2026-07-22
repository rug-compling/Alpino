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

S_FSA_PREFIX=/net/aps/opt/s_fsa-0.51/bin/fsa_prefix 

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
 $(LLE)/wiki2024/COMPACT.sorted \
 $(LLE)/OpenSubtitles2018/COMPACT.sorted \

LLRSORTED=$(LLR)/CGN/COMPACT.sorted \
 $(LLR)/VK1997/COMPACT.sorted \
 $(LLR)/TWNC/COMPACT.sorted \
 $(LLR)/MEDIARGUS/COMPACT.sorted \
 $(LLR)/NLCOW/COMPACT.sorted

FILTER=awk 'length($$0) < 110'

## remember you might want to use 
## export TMPDIR=/dev/shm
all:
	$(MAKE) all-sorted marginals temp1.fsa temp2.fsa dep.pl corpus_frequency_features corpus_frequency_features.fsa inspect 

all-sorted: $(sorted)
	LANG=POSIX LC_ALL=POSIX sort -m $(sorted) \
        | $(ALPINO_HOME)/SuffixArrays/merge_ngrams \
        | awk -F\| '{ if (NF==3 && $$3 > $(PTH)) {printf "%s|%s|%s\n", $$1,$$2,$$3}}' \
        | $(FILTER) | gzip > all-sorted

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
        | $(FILTER) | gzip > marginals

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

%/COMPACT.sorted: %
	( cd $* ; make -f $(ALPINO_HOME)/Makefile.triples COMPACT.sorted )

%.sorted: %
	( cd $*/.. ; make -f $(ALPINO_HOME)/Makefile.triples $*.sorted )

ll: $(LLSORTED)
lle: $(LLESORTED)

med = $(LLR)/MEDIARGUS/COMPACT/DM1998.sorted $(LLR)/MEDIARGUS/COMPACT/DM1999.sorted $(LLR)/MEDIARGUS/COMPACT/DM2000.sorted $(LLR)/MEDIARGUS/COMPACT/DM2001.sorted $(LLR)/MEDIARGUS/COMPACT/DM2002.sorted $(LLR)/MEDIARGUS/COMPACT/DM2003.sorted $(LLR)/MEDIARGUS/COMPACT/DM2004.sorted $(LLR)/MEDIARGUS/COMPACT/DM2005.sorted $(LLR)/MEDIARGUS/COMPACT/DM2006.sorted $(LLR)/MEDIARGUS/COMPACT/DM2007.sorted $(LLR)/MEDIARGUS/COMPACT/DS1998.sorted $(LLR)/MEDIARGUS/COMPACT/DS1999.sorted $(LLR)/MEDIARGUS/COMPACT/DS2000.sorted $(LLR)/MEDIARGUS/COMPACT/DS2001.sorted $(LLR)/MEDIARGUS/COMPACT/DS2002.sorted $(LLR)/MEDIARGUS/COMPACT/DS2003.sorted $(LLR)/MEDIARGUS/COMPACT/DS2004.sorted $(LLR)/MEDIARGUS/COMPACT/DS2005.sorted $(LLR)/MEDIARGUS/COMPACT/DS2006.sorted $(LLR)/MEDIARGUS/COMPACT/DS2007.sorted $(LLR)/MEDIARGUS/COMPACT/LN1999.sorted $(LLR)/MEDIARGUS/COMPACT/LN2000.sorted $(LLR)/MEDIARGUS/COMPACT/LN2001.sorted $(LLR)/MEDIARGUS/COMPACT/LN2002.sorted $(LLR)/MEDIARGUS/COMPACT/LN2003.sorted $(LLR)/MEDIARGUS/COMPACT/LN2004.sorted $(LLR)/MEDIARGUS/COMPACT/LN2005.sorted $(LLR)/MEDIARGUS/COMPACT/LN2006.sorted $(LLR)/MEDIARGUS/COMPACT/LN2007.sorted $(LLR)/MEDIARGUS/COMPACT/NB1998.sorted $(LLR)/MEDIARGUS/COMPACT/NB1999.sorted $(LLR)/MEDIARGUS/COMPACT/NB2000.sorted $(LLR)/MEDIARGUS/COMPACT/NB2001.sorted $(LLR)/MEDIARGUS/COMPACT/NB2002.sorted $(LLR)/MEDIARGUS/COMPACT/NB2003.sorted $(LLR)/MEDIARGUS/COMPACT/NB2004.sorted $(LLR)/MEDIARGUS/COMPACT/NB2005.sorted $(LLR)/MEDIARGUS/COMPACT/NB2006.sorted $(LLR)/MEDIARGUS/COMPACT/NB2007.sorted

twnc = $(LLR)/TWNC/COMPACT/A.sorted $(LLR)/TWNC/COMPACT/M.sorted $(LLR)/TWNC/COMPACT/N.sorted $(LLR)/TWNC/COMPACT/P.sorted $(LLR)/TWNC/COMPACT/T.sorted $(LLR)/TWNC/COMPACT/V.sorted

nlcow = $(LLR)/NLCOW/COMPACT/000.sorted $(LLR)/NLCOW/COMPACT/001.sorted $(LLR)/NLCOW/COMPACT/002.sorted $(LLR)/NLCOW/COMPACT/003.sorted $(LLR)/NLCOW/COMPACT/004.sorted $(LLR)/NLCOW/COMPACT/010.sorted $(LLR)/NLCOW/COMPACT/011.sorted $(LLR)/NLCOW/COMPACT/012.sorted $(LLR)/NLCOW/COMPACT/013.sorted $(LLR)/NLCOW/COMPACT/014.sorted $(LLR)/NLCOW/COMPACT/020.sorted $(LLR)/NLCOW/COMPACT/021.sorted $(LLR)/NLCOW/COMPACT/022.sorted $(LLR)/NLCOW/COMPACT/023.sorted $(LLR)/NLCOW/COMPACT/024.sorted $(LLR)/NLCOW/COMPACT/030.sorted $(LLR)/NLCOW/COMPACT/031.sorted $(LLR)/NLCOW/COMPACT/032.sorted $(LLR)/NLCOW/COMPACT/033.sorted $(LLR)/NLCOW/COMPACT/034.sorted $(LLR)/NLCOW/COMPACT/040.sorted $(LLR)/NLCOW/COMPACT/041.sorted $(LLR)/NLCOW/COMPACT/042.sorted $(LLR)/NLCOW/COMPACT/043.sorted $(LLR)/NLCOW/COMPACT/044.sorted $(LLR)/NLCOW/COMPACT/050.sorted $(LLR)/NLCOW/COMPACT/051.sorted $(LLR)/NLCOW/COMPACT/052.sorted $(LLR)/NLCOW/COMPACT/053.sorted $(LLR)/NLCOW/COMPACT/054.sorted $(LLR)/NLCOW/COMPACT/060.sorted $(LLR)/NLCOW/COMPACT/061.sorted $(LLR)/NLCOW/COMPACT/062.sorted $(LLR)/NLCOW/COMPACT/063.sorted $(LLR)/NLCOW/COMPACT/064.sorted $(LLR)/NLCOW/COMPACT/070.sorted $(LLR)/NLCOW/COMPACT/071.sorted $(LLR)/NLCOW/COMPACT/072.sorted $(LLR)/NLCOW/COMPACT/073.sorted $(LLR)/NLCOW/COMPACT/074.sorted $(LLR)/NLCOW/COMPACT/080.sorted $(LLR)/NLCOW/COMPACT/081.sorted $(LLR)/NLCOW/COMPACT/082.sorted $(LLR)/NLCOW/COMPACT/083.sorted $(LLR)/NLCOW/COMPACT/084.sorted $(LLR)/NLCOW/COMPACT/090.sorted $(LLR)/NLCOW/COMPACT/091.sorted $(LLR)/NLCOW/COMPACT/092.sorted $(LLR)/NLCOW/COMPACT/093.sorted $(LLR)/NLCOW/COMPACT/094.sorted $(LLR)/NLCOW/COMPACT/100.sorted $(LLR)/NLCOW/COMPACT/101.sorted $(LLR)/NLCOW/COMPACT/102.sorted $(LLR)/NLCOW/COMPACT/103.sorted $(LLR)/NLCOW/COMPACT/104.sorted $(LLR)/NLCOW/COMPACT/110.sorted $(LLR)/NLCOW/COMPACT/111.sorted $(LLR)/NLCOW/COMPACT/112.sorted $(LLR)/NLCOW/COMPACT/113.sorted $(LLR)/NLCOW/COMPACT/114.sorted $(LLR)/NLCOW/COMPACT/120.sorted $(LLR)/NLCOW/COMPACT/121.sorted $(LLR)/NLCOW/COMPACT/122.sorted $(LLR)/NLCOW/COMPACT/123.sorted $(LLR)/NLCOW/COMPACT/124.sorted $(LLR)/NLCOW/COMPACT/130.sorted $(LLR)/NLCOW/COMPACT/131.sorted $(LLR)/NLCOW/COMPACT/132.sorted $(LLR)/NLCOW/COMPACT/133.sorted $(LLR)/NLCOW/COMPACT/134.sorted $(LLR)/NLCOW/COMPACT/140.sorted $(LLR)/NLCOW/COMPACT/141.sorted $(LLR)/NLCOW/COMPACT/142.sorted $(LLR)/NLCOW/COMPACT/143.sorted $(LLR)/NLCOW/COMPACT/144.sorted $(LLR)/NLCOW/COMPACT/150.sorted $(LLR)/NLCOW/COMPACT/151.sorted $(LLR)/NLCOW/COMPACT/152.sorted $(LLR)/NLCOW/COMPACT/153.sorted $(LLR)/NLCOW/COMPACT/154.sorted $(LLR)/NLCOW/COMPACT/160.sorted $(LLR)/NLCOW/COMPACT/161.sorted $(LLR)/NLCOW/COMPACT/162.sorted $(LLR)/NLCOW/COMPACT/163.sorted $(LLR)/NLCOW/COMPACT/164.sorted $(LLR)/NLCOW/COMPACT/170.sorted $(LLR)/NLCOW/COMPACT/171.sorted $(LLR)/NLCOW/COMPACT/172.sorted $(LLR)/NLCOW/COMPACT/173.sorted $(LLR)/NLCOW/COMPACT/174.sorted $(LLR)/NLCOW/COMPACT/180.sorted $(LLR)/NLCOW/COMPACT/181.sorted $(LLR)/NLCOW/COMPACT/182.sorted $(LLR)/NLCOW/COMPACT/183.sorted $(LLR)/NLCOW/COMPACT/184.sorted $(LLR)/NLCOW/COMPACT/190.sorted $(LLR)/NLCOW/COMPACT/191.sorted $(LLR)/NLCOW/COMPACT/192.sorted $(LLR)/NLCOW/COMPACT/193.sorted $(LLR)/NLCOW/COMPACT/194.sorted $(LLR)/NLCOW/COMPACT/200.sorted $(LLR)/NLCOW/COMPACT/201.sorted $(LLR)/NLCOW/COMPACT/202.sorted $(LLR)/NLCOW/COMPACT/203.sorted $(LLR)/NLCOW/COMPACT/204.sorted $(LLR)/NLCOW/COMPACT/210.sorted $(LLR)/NLCOW/COMPACT/211.sorted $(LLR)/NLCOW/COMPACT/212.sorted $(LLR)/NLCOW/COMPACT/213.sorted $(LLR)/NLCOW/COMPACT/214.sorted $(LLR)/NLCOW/COMPACT/220.sorted $(LLR)/NLCOW/COMPACT/221.sorted $(LLR)/NLCOW/COMPACT/222.sorted $(LLR)/NLCOW/COMPACT/223.sorted $(LLR)/NLCOW/COMPACT/224.sorted $(LLR)/NLCOW/COMPACT/230.sorted $(LLR)/NLCOW/COMPACT/231.sorted $(LLR)/NLCOW/COMPACT/232.sorted $(LLR)/NLCOW/COMPACT/233.sorted $(LLR)/NLCOW/COMPACT/234.sorted $(LLR)/NLCOW/COMPACT/240.sorted $(LLR)/NLCOW/COMPACT/241.sorted $(LLR)/NLCOW/COMPACT/242.sorted $(LLR)/NLCOW/COMPACT/243.sorted $(LLR)/NLCOW/COMPACT/244.sorted $(LLR)/NLCOW/COMPACT/250.sorted $(LLR)/NLCOW/COMPACT/251.sorted $(LLR)/NLCOW/COMPACT/252.sorted $(LLR)/NLCOW/COMPACT/253.sorted $(LLR)/NLCOW/COMPACT/254.sorted $(LLR)/NLCOW/COMPACT/260.sorted $(LLR)/NLCOW/COMPACT/261.sorted $(LLR)/NLCOW/COMPACT/262.sorted $(LLR)/NLCOW/COMPACT/263.sorted $(LLR)/NLCOW/COMPACT/264.sorted $(LLR)/NLCOW/COMPACT/270.sorted $(LLR)/NLCOW/COMPACT/271.sorted $(LLR)/NLCOW/COMPACT/272.sorted $(LLR)/NLCOW/COMPACT/273.sorted $(LLR)/NLCOW/COMPACT/274.sorted $(LLR)/NLCOW/COMPACT/280.sorted $(LLR)/NLCOW/COMPACT/281.sorted $(LLR)/NLCOW/COMPACT/282.sorted $(LLR)/NLCOW/COMPACT/283.sorted $(LLR)/NLCOW/COMPACT/284.sorted $(LLR)/NLCOW/COMPACT/290.sorted $(LLR)/NLCOW/COMPACT/291.sorted $(LLR)/NLCOW/COMPACT/292.sorted $(LLR)/NLCOW/COMPACT/293.sorted $(LLR)/NLCOW/COMPACT/294.sorted $(LLR)/NLCOW/COMPACT/300.sorted $(LLR)/NLCOW/COMPACT/301.sorted $(LLR)/NLCOW/COMPACT/302.sorted $(LLR)/NLCOW/COMPACT/303.sorted $(LLR)/NLCOW/COMPACT/304.sorted $(LLR)/NLCOW/COMPACT/310.sorted $(LLR)/NLCOW/COMPACT/311.sorted

llr: $(med) $(LLR)/CGN/COMPACT.sorted $(twnc) $(LLR)/VK1997/COMPACT.sorted $(nlcow)

ls:
	@ls -lLhS $(LLSORTED) $(LLESORTED) $(LLRSORTED)

lsm:
	ls -lLhS $(sorted:%.sorted=%.marginals)

