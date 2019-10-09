SHELL := /bin/bash

ALPINO=Alpino demo=off batch_command=paraphrase user_max=300000 -flag paraphrase_file src/passive.pl

suites=journaal.sents para.sents clefshort.sents wikishort.sents vk2005.sents selection.sents

## nohup $(MAKE) -s -j jobs
jobs: $(suites:%.sents=%.job)

%.job: %.sents
	$(ALPINO) copy_input_if_no_transformation=on copy_input_if_paraphrase_failed=on < $< 2> $*.log > $*.para

clefshort.job: clefshort.sents
	$(ALPINO) copy_input_if_no_transformation=off copy_input_if_paraphrase_failed=on < $< 2> clefshort.log > clefshort.para

selection.job: selection.sents
	$(ALPINO) copy_input_if_no_transformation=ignore copy_input_if_paraphrase_failed=off < $< 2> selection.log > selection.para

diffs: $(suites:%.sents=%.diff)

errors:
	@diff <($(MAKE) -s errors0) all.errors

errors0: $(suites:%.sents=%.error)

## differences with expected
%.diff: %.para
	@echo $*
	@paste -d\| $< <(grep -v '^%' $*.expected )  |\
	awk -F\| '{ if ($$1!="") { N=N+1; if($$1!=$$2)\
            { M=M+1; printf("%d< %s\n%d> %s\n\n",N,$$1,N,$$2)}}}\
            END { printf("%d differences out of %d lines\n",M,N)}'

%.error: %.log
	@echo $*
	@grep 'warning: cannot generate adt' $*.log || true

## differences with input
%.trans: %.para
	@echo $*
	@paste -d\| $< <(grep -v '^%' $*.sents )  |\
	awk -F\| '{ if ($$1!="") { N=N+1; if($$1!=$$2)\
            { M=M+1; printf("%d< %s\n%d> %s\n\n",N,$$1,N,$$2)}}}\
            END { printf("%d differences out of %d lines\n",M,N)}'


ll:
	(  cd /net/corpora/LassyLarge ; find -name '*.index' | xargs dtlist | dtxslt --stdin -s ~/z/Alpino/TreebankTools/stylesheets/dt2sent_with_file2.xsl  -q '//node[@sc="passive" and (@root="ben" or @root="word") and not(//node[number(@begin) > 25])] ' ) > LassyLarge25.sents


updates: $(suites:%.sents=%.update)
%.update: %.para
	cp $*.para $*.expected
