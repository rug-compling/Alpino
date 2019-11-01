DIR=/net/corpora/CGN_2.0.3/data/annot/text/plk

files=$(shell ( cd $(DIR) ; find -name '*.plk.gz') )


e:
	echo $(files)

all: $(files)


%.plk.gz:
	zcat $(DIR)/$@ | recode latin1 | python3 convert_plk_alpino.py > $(notdir $*).named_sents	
