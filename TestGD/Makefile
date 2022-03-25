include $(ALPINO_HOME)/Makefile.errormining
cat=cat
LOGS=*.log

SHELL := /bin/bash

ALPINO=Alpino batch_command=paraphrase user_max=300000 simplify_split=off simplify_words=off\
       simplify_passive=off simplify_modifier=off display_quality=off display_main_parts=off\
       generate_robustness=off\
       copy_input_if_paraphrase_failed=msg\(noparaphrase\)\
       copy_input_if_no_transformation=off\
       copy_input_if_no_parse=msg\(noparse\) $(EXTRA)

suites=$(wildcard *.sents) 

## nohup $(MAKE) -s -j jobs
jobs: $(suites:%.sents=%.job)

%.job: %.sents
	$(ALPINO) < $< 2> $*.log > $*.para

diffs: $(suites:%.sents=%.diff)

errors: $(suites:%.sents=%.error)

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


updates: $(suites:%.sents=%.update)
%.update: %.para
	cp $*.para $*.expected

cleaner: $(suites:%.sents=%.clean) 
%.clean: %.expected
	cp $*.expected $*.para
	rm -f $*.log

views: $(suites:%.sents=%.view) 
%.view: %.para %.sents
	@pr --width=200 -m -t $*.para $*.sents | cat -n | head -n `cat $*.para | wc -l`

times: *.log
	@cat *.log | grep '^paraphrase cputime' | tr -d '[()]' | awk '{ P=P+$$4; N=N+$$6; L=L+1} END {printf "%12d %12d %7d %7.0f %7.0f\n",P,N,L,P/L,N/L}'

nops: $(suites:%.sents=%.nop)
%.nop: %.para %.sents
	@paste $*.para $*.sents | grep noparaphrase | cut -f2

%.v: %.para %.sents
	@paste -d @ $*.para $*.sents | grep -v -e noparse -e noparaphrase | cut -d@ -f2 |wc | awk '{ print "normal",$$2, $$1, $$2/$$1}'
	@paste -d @ $*.para $*.sents | grep -v -e noparse -e noparaphrase | cut -d@ -f1 | $(ALPINO_HOME)/Tokenization/tokenize.sh | wc | awk '{ print "paraphrased", $$2, $$1, $$2/$$1}'

v:
	@make -s $(suites:%.sents=%.v) | \
       awk '/^normal/ { NW=NW+$$2; NS=NS+$$3 } /^paraphrased/ { SW=SW+$$2; SS=SS+$$3 } END {print "normal:",NW,NS,NW/NS,"paraphrased:",SW,SS,SW/SS }'



report:
	@echo `cat *.sents | wc -l` inputs
	@echo `cat *.para | wc -l` results
	@echo `cat *.para | grep -c noparaphrase` noparaphrase
	@echo `cat *.para | grep -c noparse` noparse
	@make -s v
	@make -s times
	@echo `cat *.log |grep -c 'timed out'` timed out


