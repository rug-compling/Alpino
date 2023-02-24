include $(ALPINO_HOME)/Makefile.errormining
cat=cat
LOGS=*.log

SHELL := /bin/bash

ALPINO=Alpino batch_command=paraphrase user_max=300000 simplify_split=on simplify_words=on\
       simplify_passive=on simplify_modifier=on display_quality=off display_main_parts=off\
       generate_robustness=off\
       -l $(ALPINO_HOME)/simplify\
       copy_input_if_paraphrase_failed=msg\(noparaphrase\)\
       copy_input_if_no_transformation=off\
       copy_input_if_no_parse=msg\(noparse\) $(EXTRA)

suites=$(wildcard *.sents) $(wildcard *.sents.gz)

## nohup $(MAKE) -s -j jobs
jobs: $(suites:%.sents=%.job) $(suites:%.sents.gz=%.job)

%.job: %.sents
	$(ALPINO) < $< 2> $*.log > $*.para

%.job: %.sents.gz
	zcat $< |\
        sed -e 's/^[^|]*[|]//' |\
        $(ALPINO) 2> $*.log > $*.para

diffs: $(suites:%.sents=%.diff) $(suites:%.sents.gz=%.diff)

errors: $(suites:%.sents=%.error) $(suites:%.sents.gz=%.error)

## differences with expected
%.diff: %.para
	@echo $*
	@paste -d\| $< <(grep -v '^%' $*.expected )  |\
	awk -F\| '{ K=K+1; if ($$1!="") { N=N+1; if($$1!=$$2)\
            { M=M+1; printf("%d< %s\n%d> %s\n\n",K,$$1,K,$$2)}}}\
            END { printf("%d differences out of %d lines\n",M,N)}'

%.error: %.log
	@echo $*
	@grep 'warning: cannot generate adt' $*.log || true

## differences with input
%.trans: %.para
	@echo $*
	@paste -d\|  <(grep -v '^%' $*.sents ) $<  |\
	awk -F\| '{ if ($$1!="") { N=N+1; if($$1!=$$2)\
            { M=M+1; printf("%d< %s\n%d> %s\n\n",N,$$1,N,$$2)}}}\
            END { printf("%d differences out of %d lines\n",M,N)}'


updates: $(suites:%.sents=%.update) $(suites:%.sents.gz=%.update)
%.update: %.para
	cp $*.para $*.expected

cleaner: $(suites:%.sents=%.clean) $(suites:%.sents.gz=%.clean)
%.clean: %.expected
	cp $*.expected $*.para
	rm -f $*.log

views: $(suites:%.sents=%.view) $(suites:%.sents.gz=%.view)
%.view: %.para %.sents
#	@pr --width=200 -m -t $*.para $*.sents | cat -n | head -n `cat $*.para | wc -l`
	paste -d '\n' $*.sents $*.para

%.view: %.para %.sents.gz
#	@pr --width=200 -m -t $*.para <(zcat $*.sents.gz | sed -e 's/^[^|]*[|]//') | cat -n | head -n `cat $*.para| wc -l`
	paste -d '\n' <(zcat $*.sents | sed -e 's/^[^|]*[|]//') $*.para


times: *.log
	@cat *.log | grep '^paraphrase cputime' | tr -d '[()]' | awk '{ P=P+$$4; N=N+$$6; L=L+1} END {printf "%12d %12d %7d %7.0f %7.0f\n",P,N,L,P/L,N/L}'

nops: $(suites:%.sents=%.nop) $(suites:%.sents.gz=%.nop)

%.nop: %.para %.sents
	@paste $*.para $*.sents | grep noparaphrase | cut -f2

%.nop: %.para %.sents.gz
	@paste $*.para <(zcat $*.sents.gz | sed -e 's/^[^|]*[|]//') | grep noparaphrase | cut -f2


nopas: $(suites:%.sents=%.nopa) $(suites:%.sents.gz=%.nopa)

%.nopa: %.para %.sents
	@paste $*.para $*.sents | grep noparse | cut -f2

%.nopa: %.para %.sents.gz
	@paste $*.para <(zcat $*.sents.gz | sed -e 's/^[^|]*[|]//') | grep noparse | cut -f2


%.v: %.para %.sents.gz
	@paste -d @ $*.para <(zcat $*.sents.gz  | sed -e 's/^[^|]*[|]//') | grep -v -e noparse -e noparaphrase | cut -d@ -f2 |wc | awk '{ print "normal",$$2, $$1, $$2/$$1}'
	@paste -d @ $*.para <(zcat $*.sents.gz  | sed -e 's/^[^|]*[|]//') | grep -v -e noparse -e noparaphrase | cut -d@ -f1 | $(ALPINO_HOME)/Tokenization/tokenize.sh | wc | awk '{ print "simplified",$$2, $$1, $$2/$$1}'

%.v: %.para %.sents
	@paste -d @ $*.para <(cat $*.sents  | sed -e 's/^[^|]*[|]//') | grep -v -e noparse -e noparaphrase | cut -d@ -f2 |wc | awk '{ print "normal",$$2, $$1, $$2/$$1}'
	@paste -d @ $*.para <(cat $*.sents  | sed -e 's/^[^|]*[|]//') | grep -v -e noparse -e noparaphrase | cut -d@ -f1 | $(ALPINO_HOME)/Tokenization/tokenize.sh | wc | awk '{ print "simplified", $$2, $$1, $$2/$$1}'

v:
	@make -s $(suites:%.sents=%.v) $(suites:%.sents.gz=%.v) | \
       awk '/^normal/ { NW=NW+$$2; NS=NS+$$3 } /^simplified/ { SW=SW+$$2; SS=SS+$$3 } END {print "normal:",NW,NS,NW/NS,"simplified:",SW,SS,SW/SS }'



#### ppi:
####
#### zcat /net/corpora/LassyLarge/WR-P-P-I/SUITES/WR-P-P-I_part000* | words -f 15 -t 20 |../Suites/split_in_parts -o ppi -p 1000

ppis=$(wildcard ppi_part*.sents.gz)

all=$(ppis:%.sents.gz=%.job)
all: $(all)

small=ppi_part00007.job ppi_part00043.job ppi_part00028.job

small:  $(small) 

big: $(filter-out $(small), $(all))

report:
	@echo `cat *.sents <(zcat *.sents.gz) | wc -l` inputs
	@echo `cat *.para | wc -l` results
	@echo `cat *.para | grep -c noparaphrase` noparaphrase
	@echo `cat *.para | grep -c noparse` noparse
	@make -s v
	@make -s times
	@echo `cat *.log |grep -c 'timed out'` timed out


