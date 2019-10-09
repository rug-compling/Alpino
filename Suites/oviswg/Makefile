files=$(shell ls -v *.nd)

%.m:%.nd
	fsa -domain $*.nd | fsa -minimize > $*.m

oviswg.named_sents: $(files:%.nd=%.m)
	sicstus -l sample -a $(files:%.nd=%.m)\
        | sed -e "s/s_/'s /g"\
              -e "s/een1/één/g" > oviswg.named_sents


### best N

N=4

oviswg.scores: oviswg.named_sents
	awk -F \| '{ print $$2 }' oviswg.named_sents\
        | ../../Generation/fluency/apply\
             ../../Generation/fluency/words.fsa\
             ../../Generation/fluency/unigrams.tpl\
             ../../Generation/fluency/bigrams.tpl\
             ../../Generation/fluency/trigrams.tpl\
        | awk -F\| '{ print $$1 }' > oviswg.scores

oviswg.keys: oviswg.named_sents
	awk -F \| '{ print $$1 }' oviswg.named_sents \
        | tr '#' ' ' > oviswg.keys

oviswg.bestkeys: oviswg.keys oviswg.scores
	paste oviswg.keys oviswg.scores\
        | sort -k1,1n -k3,3n\
        | awk 'BEGIN {PREV=500} {if (PREV != $$1) { N=0; PREV=$$1 } else N++; if (N<=$$N) printf "%s#%s\n",$$1,$$2}'\
        > oviswg.bestkeys

oviswgbest.named_sents: oviswg.bestkeys
	grep -w -F -f oviswg.bestkeys  oviswg.named_sents >oviswgbest.named_sents