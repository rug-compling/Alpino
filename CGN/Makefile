DIR=/net/corpora/CGN_2.0.3/data/annot/text/plk

files=$(shell ( cd $(DIR) ; find -name '*.plk.gz') )

all: $(files)
	find -name '*.named_sents' | xargs cat | gzip > ../Suites/cgn.sents.gz

%.plk.gz:
	zcat $(DIR)/$@ | recode latin1 | python3 convert_plk_alpino.py $(notdir $*) > $*.named_sents

realclean:
	find -name '*.named_sents' | xargs rm
