include $(ALPINO_HOME)/Makefile.errormining
FRAMES=$(wildcard *.frames)
LOGS=$(wildcard *.log.gz)
cat=zcat

all.unknowns: $(FRAMES)
	$(MAKE) -s assigned_unknowns > all.unknowns

all.unknowns_with_heur: $(FRAMES)
	$(MAKE) -s assigned_unknowns_with_heur > all.unknowns_with_heur

%.q: %.log.gz
	zcat $*.log.gz  | grep '^Q#' | gzip > $*.q

clef.q: CLEF.log.gz
	zcat CLEF.log.gz | grep '^Q#' | grep -F -f relevant_clef_keys\
      | gzip > clef.q

freqframes=$(FRAMES:%.frames=%.freqframes)

%.freqframes: %.frames
	zcat $< | awk -F\| '{ printf "%s|%s\n",$$1,$$2 }' \
      | LANG=POSIX LC_ALL=POSIX sort -t\| -k1,1 -k2,2 \
      | $(ALPINO_HOME)/Disambiguation/Triples/uniq-c \
      > $@

frames.pl: $(freqframes)
	echo ':- module(frames, [ z_f2/3 ]).' > frames.pl
	echo >> frames.pl
	LANG=POSIX LC_ALL=POSIX sort -m -t\| -k2,2 $(freqframes) \
      | ./cnt-frames\
      | Alpino debug=0 cmdint=off -u -notk -l frames2pl  >> frames.pl


counts:
	for file in $(FRAMES:%.frames=%);\
        do echo $$file \
             `zcat $$file.frames|awk -F\| '{ print $$3 }' | uniq | wc -l`;\
           echo $$file \
             `zcat $$file.log.gz|$(ALPINO_HOME)/bin/q -o | wc -l`;\
        done

m_ngram: $(FRAMES:%.frames=%.m_ngram) 
	cat $(FRAMES:%.frames=%.m_ngram) |\
          LANG=POSIX LC_ALL=POSIX sort -t\| -k1,1n -k2,2 |\
          $(SADIR)/merge_ngrams > m_ngram

a_ngram: $(FRAMES:%.frames=%.a_ngram) 
	cat $(FRAMES:%.frames=%.a_ngram) |\
          LANG=POSIX LC_ALL=POSIX sort -t\| -k1,1n -k2,2 |\
          $(SADIR)/merge_ngrams  > a_ngram

ok: m_ngram a_ngram
	$(SADIR)/parsability_table.with-binom-test m_ngram a_ngram |\
          sort  -k 1,1n -k2,2rn > ok

mistakes: $(FRAMES:%.frames=%.mistakes)
