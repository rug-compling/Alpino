include $(ALPINO_HOME)/Makefile.errormining
include ../Makefile.patterns

LOGS=$(wildcard *.log.gz)
cat=zcat

freqframes=$(LOGS:%.log.gz=%.freqframes)

frames.pl: $(freqframes) ./cnt-frames
	echo ':- module(alpino_frames, [ z_f2/3 ]).' > frames.pl
	echo >> frames.pl
	LANG=POSIX LC_ALL=POSIX sort -m -t\| -k2,2 $(freqframes) \
      | ./cnt-frames\
      | Alpino debug=0 cmdint=off -u -notk -l frames2pl  >> frames.pl

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

outs: $(OUTS)

$(OUTS): $(OBJS) 
	./getngdpost -d $(OUTDIR) $(OPTPOST) $(OBJS)

bin: getNGD getNGD2 getngd getngd2 getngdpre getngdpost

BUILT=*.sfr *.sfr.* *.tmi *.tmo *.ftmi *.ftmo *.freqframes *.fr

realclean:
	rm -f getNGD getNGD2 getngd getngd2 getngdpre getngdpost $(BUILT)

.PRECIOUS : %.fr %.tmi %.tmo %.sfr

