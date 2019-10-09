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

freqframes=$(LOGS:%.log.gz=%.freqframes)

%.freqframes: %.log.gz %.ftm
	zcat $< | ./to_freq $*.ftm \
      | LANG=POSIX LC_ALL=POSIX sort -t\| -k1,1 -k2,2 \
      | $(ALPINO_HOME)/Disambiguation/Triples/uniq-c \
      > $@

frames.pl: $(freqframes)
	echo ':- module(alpino_frames, [ z_f2/3 ]).' > frames.pl
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

include ../Makefile.patterns

OUTDIR=..
OPTPRE=-c 1
OPTPOST=-s -f 2 -g 1350

SRC=$(wildcard *.log.gz)

OBJS=$(SRC:%.log.gz=%.sfr.cTF)

sfrs: $(SRC:%.log.gz=%.sfr.cTF)

%.sfr.cTF : %.sfr
	gunzip -c $< | ./getngdpre -b $< $(OPTPRE)

% : %.c
	gcc -s -Wall -O3 -o $@ $<

% : %.cc
	g++ -std=c++0x -s -Wall -O3 -o $@ $<

% : %.go
	go build $<

objs: bin $(OBJS)

OUTS=$(OUTDIR)/context3 $(OUTDIR)/tag4 $(OUTDIR)/tag3 $(OUTDIR)/tag2\
     $(OUTDIR)/prefix3 $(OUTDIR)/prefix2 $(OUTDIR)/wordTag $(OUTDIR)/tagWord\
     $(OUTDIR)/usedContext $(OUTDIR)/wordTagLex

all: bin $(OUTS)

$(OUTS): $(OBJS) 
	./getngdpost -d $(OUTDIR) $(OPTPOST) $(OBJS)

bin: getNGD getNGD2 getngd getngd2 getngdpre getngdpost

realclean:
	rm getNGD getNGD2 getngd getngd2 getngdpre getngdpost

.PRECIOUS : %.fr %.tmi %.tmo %.sfr
