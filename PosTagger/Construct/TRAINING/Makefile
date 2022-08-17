include $(ALPINO_HOME)/Makefile.errormining
LOGS=$(wildcard *.log.gz)
cat=zcat

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

all: bin $(OUTS) frames.pl

$(OUTS): $(OBJS) 
	./getngdpost -d $(OUTDIR) $(OPTPOST) $(OBJS)

bin: getNGD getNGD2 getngd getngd2 getngdpre getngdpost

realclean:
	rm getNGD getNGD2 getngd getngd2 getngdpre getngdpost

.PRECIOUS : %.fr %.tmi %.tmo %.sfr

