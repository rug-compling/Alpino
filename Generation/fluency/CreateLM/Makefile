ifneq ("$(wildcard ../../../Makefile.include)", "")
include ../../../Makefile.include
else
ifneq ("$(wildcard $(ALPINO_HOME)/Makefile.include)", "")
include $(ALPINO_HOME)/Makefile.include
endif
endif

ifeq ($(PLATFORM),windows)
	TRAIN_NGRAM_MODEL=
else
	PRO_LM_LIB=pro_lm$(MODULEEXT)
	PRO_LM_STATIC=pro_lm.s.o
endif

FADD=$(ALPINO_HOME)/fadd

MAKETUPLE=$(FADD)/maketuple.pl

DIR=$(ALPINO_HOME)/PosTagger/Construct/TRAINING
CURRENT=$(ALPINO_HOME)/Generation/fluency/CreateLM

ALL=$(wildcard $(DIR)/*.sfr )
#PARTS=VK1997 AD1999 TROUW2000 VK2001 NRC2002 PAROOL2003 AD2004 CLEF WRPPH WRPPG WSUEA
#PARTS=$(notdir $(basename $(ALL)))
PARTS=AD1994 NRC1995 VK1997 TROUW2000 PAROOL2001 NRC2002 VK2003 AD2004 AD2005 WIKI0 WYT11 WSUEA
#PARTS=AD1994

DATA=$(PARTS:%=$(DIR)/%)
SFR=$(DATA:%=%.sfr)

DISK=./disk

default:

$(DISK)/frames: $(SFR)
	zcat $(SFR) | python $(CURRENT)/sfr2frames.py > $(DISK)/frames

$(DISK)/corpus: $(SFR)
	zcat $(SFR) | python $(CURRENT)/sfr2words.py > $(DISK)/corpus

new: 
	rm -f $(DISK)/frames $(DISK)/corpus
	$(MAKE) model

#### tuples cannot be parallel because fadd uses fixed names for some of its temp files :-(

model:
	$(MAKE) w
	$(MAKE) t

t: $(DISK)/frames
	$(MAKE) $(DISK)/tags.fsa 
	$(MAKE) $(DISK)/tag-unigrams.tpl
	$(MAKE) $(DISK)/tag-bigrams.tpl
	$(MAKE) $(DISK)/tag-trigrams.tpl

w: $(DISK)/corpus
	$(MAKE) $(DISK)/words.fsa
	$(MAKE) $(DISK)/unigrams.tpl
	$(MAKE) $(DISK)/bigrams.tpl
	$(MAKE) $(DISK)/trigrams.tpl

train_ngram_model: train_ngram_model.cpp
	g++ $(CXXFLAGS) -std=c++11 -o $@ $<

$(DISK)/unigrams: train_ngram_model $(DISK)/corpus
	$(CURRENT)/train_ngram_model $(DISK)/corpus 3 $(DISK)/unigrams $(DISK)/bigrams $(DISK)/trigrams

$(DISK)/tag-unigrams: train_ngram_model $(DISK)/frames
	$(CURRENT)/train_ngram_model $(DISK)/frames 0 $(DISK)/tag-unigrams $(DISK)/tag-bigrams $(DISK)/tag-trigrams

$(DISK)/words: $(DISK)/unigrams
	cut -f1 $(DISK)/unigrams | LANG=POSIX LC_ALL=POSIX sort -u > $(DISK)/words

$(DISK)/words.fsa: $(DISK)/words
	$(S_FSA_BUILD) -N -o $(DISK)/words.fsa < $(DISK)/words

$(DISK)/unigrams.tpl: $(DISK)/unigrams $(DISK)/words.fsa
	$(MAKETUPLE) total=2 words=1 sep='	' out=$(DISK)/unigrams.tpl $(DISK)/unigrams +$(DISK)/words

$(DISK)/bigrams.tpl: $(DISK)/bigrams $(DISK)/words.fsa
	$(MAKETUPLE) total=3 words=2 sep='	' out=$(DISK)/bigrams.tpl $(DISK)/bigrams +$(DISK)/words +$(DISK)/words

$(DISK)/trigrams.tpl: $(DISK)/trigrams $(DISK)/words.fsa
	$(MAKETUPLE) total=4 words=3 sep='	' out=$(DISK)/trigrams.tpl $(DISK)/trigrams +$(DISK)/words +$(DISK)/words +$(DISK)/words

$(DISK)/tags: $(DISK)/tag-unigrams
	cut -f1 $(DISK)/tag-unigrams | LANG=POSIX LC_ALL=POSIX sort -u > $(DISK)/tags

$(DISK)/tags.fsa: $(DISK)/tags
	$(S_FSA_BUILD) -N -o $(DISK)/tags.fsa < $(DISK)/tags

$(DISK)/tag-unigrams.tpl: $(DISK)/tag-unigrams $(DISK)/tags.fsa
	$(MAKETUPLE) total=2 words=1 sep='	' out=$(DISK)/tag-unigrams.tpl $(DISK)/tag-unigrams +$(DISK)/tags

$(DISK)/tag-bigrams.tpl: $(DISK)/tag-bigrams $(DISK)/tags.fsa
	$(MAKETUPLE) total=3 words=2 sep='	' out=$(DISK)/tag-bigrams.tpl $(DISK)/tag-bigrams +$(DISK)/tags +$(DISK)/tags

$(DISK)/tag-trigrams.tpl: $(DISK)/tag-trigrams $(DISK)/tags.fsa
	$(MAKETUPLE) total=4 words=3 sep='	' out=$(DISK)/tag-trigrams.tpl $(DISK)/tag-trigrams +$(DISK)/tags +$(DISK)/tags +$(DISK)/tags

targets = $(DISK)/bigrams.tpl  $(DISK)/tag-bigrams.tpl  $(DISK)/tags.fsa  $(DISK)/tag-trigrams.tpl \
          $(DISK)/tag-unigrams.tpl  $(DISK)/trigrams.tpl  $(DISK)/unigrams.tpl  $(DISK)/words.fsa $(DISK)/stem_word_freq.pl

temps = $(DISK)/words $(DISK)/tags $(DISK)/unigrams $(DISK)/bigrams $(DISK)/trigrams\
        $(DISK)/tag-unigrams $(DISK)/tag-bigrams $(DISK)/tag-trigrams $(DISK)/*.wd[012]

clean: 
	rm -f $(targets) $(temps)

realclean: 
	rm -f train_ngram_model $(targets) $(temps)

install:
	mv $(targets) ../

evaluate:
	Alpino -notk -s cdb batch_command=alpino_ngram_lm:evaluate_fluency -flag ngram_model_dir $(DISK)
	Alpino -notk -s cdbw batch_command=alpino_ngram_lm:evaluate_fluency -flag ngram_model_dir $(DISK)

# previous stable release: (trigrams.tpl 487Mb!)
# total score 1125378.0 after 7136 sentences
# total score 1388170.0 after 7136 sentences 262792

# threshold 5, all data+PPH    (trigrams.tpl 36Mb!)
# total score  990210.0 after 7136 sentences
# total score 1310265.0 after 7136 sentences 320055

# threshold 5, all data-PPH    (trigrams.tpl 27Mb!)
# total score 1000030.0 after 7136 sentences
# total score 1318430.0 after 7136 sentences

# threshold 3, all data-PPH    (trigrams.tpl 53Mb!)
# total score 980077.0 after 7136 sentences
# total score 1310038.0 after 7136 sentences



# threshold 0, all data+PPH    (trigrams.tpl 714Mb)
# total score 930546.0 after 7136 sentences
# total score 1284568.0 after 7136 sentences 354022

# threshold 2, all data+PPH    (trigrams.tpl 137Mb) 339320
# total score 956580.0 after 7136 sentences
# total score 1295900.0 after 7136 sentences

# threshold 0, only AD1994
# total score  998205.0 after 7136 sentences
# total score 1317860.0 after 7136 sentences

# threshold 5, only AD1994
# total score 1097470.0 after 7136 sentences
# total score 1358285.0 after 7136 sentences

# threshold 10, only AD1994
# total score 1141197.0 after 7136 sentences
# total score 1377176.0 after 7136 sentences

LOGS=$(PARTS:%=$(DIR)/%.log.gz )

$(DISK)/stem_word:
	zcat $(LOGS) \
        | grep '^FRAME#' \
        | awk -F\| '{ print $$8 "|" $$1 }' \
        | sed -e 's/FRAME#//'  \
        | sort \
        | uniq -c \
        | sed -e 's/^\(.......\) /\1|/' \
        > $(DISK)/stem_word

THR=5
$(DISK)/stem_word_freq: $(DISK)/stem_word
	cat $(DISK)/stem_word \
        | awk -F\| '{ if ($$1 > $(THR)) { print $$0 }}'\
        | awk -F\| '{ if (P==$$2 && ok==0) { print L; print $$0; ok=1} else {if (P==$$2 && ok==1)  print $$0 ; else { ok=0} } ; P=$$2; L=$$0}'\
        | awk -F\| '{ printf "%s\t%s\t%d\n",$$2,$$3,$$1 }' \
        > $(DISK)/stem_word_freq

$(DISK)/stem_word_freq.pl: toprolog.pl
	echo ":- module(alpino_lemma_word_freq,[ lemma_word_freq/3 ])." > $(DISK)/stem_word_freq.pl
	echo >> $(DISK)/stem_word_freq.pl
	Alpino debug=0 cmdint=off -u  -notk -l toprolog < disk/stem_word_freq >> $(DISK)/stem_word_freq.pl


