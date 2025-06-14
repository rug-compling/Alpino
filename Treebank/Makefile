ifeq "$(shell ls ../Makefile.include)" "../Makefile.include"
include ../Makefile.include
else
include $(ALPINO_HOME)/Makefile.include
endif

XSL=$(ALPINO_HOME)/TreebankTools/stylesheets

LASSYSMALL=/net/corpora/LassyDevelop

all:
	$(MAKE) -j dtchecks novel
	$(MAKE) -j prepare_enhanced_all
	$(MAKE) -j enhanced_all poslemma
	$(MAKE) -j 30 sentspl freq trees_doc conllu pair installsents alpino.dact

novel:
	( cd fast; make -j alpino )

DIRECTORIES=cdb cgn_exs eans extra g_suite h_suite i_suite lassy-doc leuven_yellow_pages lot_test_suite1 \
             novelsample_corrected ntv-suite qa Titaantjes wpspel j_suite

%.doc:
	(  cd Enhanced/$*; $(MAKE) index.html )

trees_doc: $(DIRECTORIES:%=%.doc)

%.realclean:
	(  cd Enhanced/$*; $(MAKE) realclean )

clean:

install: 

realclean: clean $(DIRECTORIES:%=%.realclean)

transform:
	find $(DIRECTORIES) -name '*.xml' | xargs Alpino -u -xml_converse

alpino.dact: $(DIRECTORIES:%=Enhanced/%/*.xml)
	( cd Enhanced ; rm -f alpino.dact ; alto $(DIRECTORIES) -o alpino.dact )
	mv Enhanced/alpino.dact alpino.dact

freq: $(DIRECTORIES:%=MWU/%.mwu) FREQ/mwu-tagged.freq $(TMPDATA)
	mkdir -p FREQ LP TRIPLES
	$(MAKE) FREQ/POS.freq FREQ/LEMMA.freq FREQ/RLEMMA.freq FREQ/WORD.freq \
                FREQ/WORD-POS.freq FREQ/WORD-LEMMA-POS.freq FREQ/WORD-AMB.freq FREQ/TAG-AMB.freq \
                FREQ/mwu-tagged.freq FREQ/deps.freq FREQ/heads.freq FREQ/triples.freq FREQ/POS-WORD-LEMMA.freq

## removes the .mwu file, because if it is empty it would otherwise
## be rebuilt over and over again!
MWU/%.mwu: Enhanced/%/*.xml $(XSL)/mwu_tags.xsl
	rm -f MWU/$*.mwu
	ls -v Enhanced/$*/*.xml | dtxslt --stdin -q '//node[@cat="mwu"]' -s $(XSL)/mwu_tags.xsl > MWU/$*.mwu

pair: $(DIRECTORIES:%=PAIR/%.pair) $(DIRECTORIES:%=PAIR/%.triple) 
	$(MAKE) FREQ/inconsistent-pairs FREQ/inconsistent-triples

## the odd sed commands for XX[0-9]XX are there to fool awk into treating
## numbers as strings, otherwise 3.00 = 3.0 = 3 etc
FREQ/inconsistent-words: POSLEMMA/*.poslemma $(LASSYSMALL)/POSLEMMA/*.poslemma
	cat POSLEMMA/*.poslemma $(LASSYSMALL)/POSLEMMA/*.poslemma \
        | grep -v '<sent id=' \
        | grep -v 'SPEC(deeleigen)'\
        | grep -v 'SPEC(vreemd)'\
        | grep -v 'SPEC(symb)'\
	| sed -e 's/ADJ(prenom,basis,zonder)/ADJ(basis)/g'\
	| sed -e 's/ADJ(vrij,basis,zonder)/ADJ(basis)/g'\
	| sed -e 's/ADJ(prenom,comp,zonder)/ADJ(comp)/g'\
	| sed -e 's/ADJ(vrij,comp,zonder)/ADJ(comp)/g'\
	| sed -e 's/ADJ(prenom,sup,zonder)/ADJ(sup)/g'\
	| sed -e 's/ADJ(vrij,sup,zonder)/ADJ(sup)/g'\
        | sed -e 's/ADJ(prenom,basis,met-e,stan)/ADJ(met-e)/g'\
        | sed -e 's/ADJ(nom,basis,met-e,zonder-n,stan)/ADJ(met-e)/g'\
	| sed -e 's/WW(inf,vrij,zonder)/WW(inf)/g'\
	| sed -e 's/WW(inf,nom,zonder,zonder-n)/WW(inf)/g'\
	| sed -e 's/WW(pv,tgw,mv)/WW(inf)/g'\
        | sed -e 's/WW(vd,prenom,zonder)/WW(vd)/g'\
        | sed -e 's/WW(vd,vrij,zonder)/WW(vd)/g'\
        | sed -e 's/WW(od,prenom,zonder)/WW(od)/g'\
        | sed -e 's/WW(od,vrij,zonder)/WW(od)/g'\
	| sed -e 's/TW(hoofd,prenom,stan)/TW/g'\
	| sed -e 's/TW(hoofd,vrij)/TW/g'\
	| sed -e 's/TW(hoofd,nom,zonder-n,basis)/TW/g'\
	| sed -e 's/TW(rang,nom,zonder-n)/TW(rang)/g'\
	| sed -e 's/TW(rang,prenom,stan)/TW(rang)/g'\
        | sort \
        | uniq -c \
        | sed -e 's/[0-9]/XX&XX/g'\
        | awk '{ if ($$2 == P) printf("%s\n%s\n\n",N,$$0); P=$$2; N=$$0}' \
        | sed -e 's/XX\([0-9]\)XX/\1/g'\
        > FREQ/inconsistent-words
	wc -l FREQ/inconsistent-words

FREQ/inconsistent-pairs: PAIR/*.pair $(LASSYSMALL)/PAIR/*.pair
	cat PAIR/*.pair $(LASSYSMALL)/PAIR/*.pair \
        | sort \
        | uniq -c \
        | awk '{ if ($$2 == P && $$3 == Q) printf("%s\n%s\n\n",N,$$0); P=$$2; Q=$$3; N=$$0}' \
        > FREQ/inconsistent-pairs
	wc -l FREQ/inconsistent-pairs

FREQ/inconsistent-triples: PAIR/*.triple $(LASSYSMALL)/PAIR/*.triple
	cat PAIR/*.triple $(LASSYSMALL)/PAIR/*.triple \
        | sed -e 's/TW(hoofd,prenom,stan)/TW/g'\
	| sed -e 's/TW(hoofd,vrij)/TW/g'\
        | sort \
        | uniq -c \
        | awk '{ if ($$2 == P && $$3 == Q && $$4 == R) printf("%s\n%s\n\n",N,$$0); P=$$2; Q=$$3; R=$$4; N=$$0}' \
        > FREQ/inconsistent-triples
	wc -l FREQ/inconsistent-triples

PAIR/%.pair: Enhanced/%/*.xml $(XSL)/dt2pair.xsl
	rm -f PAIR/$*.pair
	ls -v Enhanced/$*/*.xml | dtxslt --stdin -s $(XSL)/dt2pair.xsl > PAIR/$*.pair


PAIR/%.triple: Enhanced/%/*.xml $(XSL)/dt2triple.xsl
	rm -f PAIR/$*.triple
	ls -v Enhanced/$*/*.xml | dtxslt --stdin -s $(XSL)/dt2triple.xsl > PAIR/$*.triple


FREQ/mwu-tagged.freq: $(DIRECTORIES:%=MWU/%.mwu) $(LASSYSMALL)/MWU/*.mwu
	cat $(DIRECTORIES:%=MWU/%.mwu) $(LASSYSMALL)/MWU/*.mwu\
        | sort\
        | uniq -c\
        | sort -nr > FREQ/mwu-tagged.freq
	cat FREQ/mwu-tagged.freq | python3 $(ALPINO_HOME)/TreebankTools/find-inconsistent m > FREQ/mwu-inconsistent

poslemma: $(DIRECTORIES:%=POSLEMMA/%.poslemma)
	$(MAKE) FREQ/inconsistent-words

gitversion=$(shell git log --oneline | wc -l)

tars: $(DIRECTORIES:%=%.tar)

tars-install: $(DIRECTORIES:%=%.tar-install) alpino.dact
	cp alpino.dact ~/public_html/treebanks

%.tar:Enhanced/%
	( cd Enhanced ; tar czf $*-$(gitversion).tar.gz $* )

%.tar-install:Enhanced/%
	mv Enhanced/$*-$(gitversion).tar.gz ~/public_html/treebanks/
	( cd ~/public_html/treebanks; rm -f $*.tar.gz; ln -s $*-$(gitversion).tar.gz $*.tar.gz )

dtchecks:
	touch .dtchecks2
	dtchecks $(shell find -L $(DIRECTORIES) -name '*.xml' -newer .dtchecks | awk -F/ '{ printf "%s\n",$$1 }' | sort -u)
	mv .dtchecks2 .dtchecks

all_dtchecks:
	touch .dtchecks2
	dtchecks $(shell find -L $(DIRECTORIES) -name '*.xml' | awk -F/ '{ printf "%s\n",$$1 }' | sort -u)
	mv .dtchecks2 .dtchecks

sqldir=/net/aps/haytabo/opt/alpinotools
prepare_enhanced_all: $(DIRECTORIES:%=sql/%.sqlite)

sql/%.sqlite: best/%/*.xml fast/%/*.xml
	rm -f sql/$*.sqlite
	$(sqldir)/auto2sql sql/$*.sqlite best/$* fast/$*

enhanced_all: $(DIRECTORIES:%=%.enhanced)

%.enhanced: 
	$(sqldir)/alpinomod -u -p $*  sql/$*.sqlite $* Enhanced/$*

sents: $(DIRECTORIES:%=../SENTS/%.sents)
../SENTS/%.sents: Enhanced/%/*.xml $(XSL)/dt2sent_with_sentid.xsl
	( cd Enhanced/$* ;\
             ls -v ./*.xml |\
             dtxslt --stdin -s\
             $(XSL)/dt2sent_with_sentid.xsl |\
             sed -e 's/\[/\\\[/g' \
                 -e 's/\]/\\\]/g') > $@
sentspl: sents
	(  cd ../SENTS ; make )

conllu: $(DIRECTORIES:%=../CONLLU/%.conllu)

../CONLLU/%.conllu: Enhanced/%/*.xml
	ls -v ../Treebank/Enhanced/$*/*.xml | alud > ../CONLLU/$*.conllu 2> ../CONLLU/$*.conllu.err


#	( cd fast; make eans.log BASE=/net/aistaff/vannoord/z/Alpino )

## all steps for one suite only
update:
	$(MAKE) novel
	$(MAKE) sql/$(suite).sqlite corpus=$(suite)
	$(MAKE) $(suite).enhanced
	$(MAKE) ../SENTS/$(suite).sents sentspl installsents
	$(MAKE) POSLEMMA/$(suite).poslemma
	$(MAKE) MWU/$(suite).mwu FREQ/mwu-tagged.freq
	(  cd Enhanced/$(suite); $(MAKE) -s --no-print-directory -j 30 index.html )
	$(MAKE) ../CONLLU/$(suite).conllu

TMPDATA:=$(DIRECTORIES:%=LP/%.lp) $(LASSYSMALL)/LP/*.lp

LP/%.lp: Enhanced/%/*.xml $(XSL)/dt2lemma-postags.xsl
	ls -v Enhanced/$*/*.xml | dtxslt --stdin -s $(XSL)/dt2lemma-postags.xsl > LP/$*.lp

FREQ/triples.freq: $(DIRECTORIES:%=TRIPLES/%.triples)
	sort -m $(DIRECTORIES:%=TRIPLES/%.triples) $(LASSYSMALL/TRIPLES/*.triples) \
           | uniq -c \
           | sort -nr \
           > FREQ/triples.freq

FREQ/deps.freq: $(DIRECTORIES:%=TRIPLES/%.triples)
	awk -F\| '{ printf "%s|%s|%s\n",$$3,$$4,$$5}' $(DIRECTORIES:%=TRIPLES/%.triples) $(LASSYSMALL/TRIPLES/*.triples)\
	   | sort \
           | uniq -c \
           | sort -nr \
           > FREQ/deps.freq

FREQ/heads.freq: $(DIRECTORIES:%=TRIPLES/%.triples)
	awk -F\| '{ printf "%s|%s|%s\n",$$1,$$2,$$3}' $(DIRECTORIES:%=TRIPLES/%.triples) $(LASSYSMALL/TRIPLES/*.triples)\
	   | sort \
           | uniq -c \
           | sort -nr \
           > FREQ/heads.freq


TRIPLES/%.triples: %/*.xml
	dtlist --recursive $(dir $<) \
             | xargs Alpino -treebank_lassy_triples \
             | awk -F\| '{ printf "%s|%s|%s|%s|%s\n",$$1,$$2,$$3,$$4,$$5}' \
             | sort \
             >  $@

FREQ/WORD.freq: $(TMPDATA)
	cut -f1  $(TMPDATA) | sort | uniq -c | sort -nr > FREQ/WORD.freq 

FREQ/LEMMA.freq: $(TMPDATA)
	cut -f2  $(TMPDATA) | sort | uniq -c | sort -nr > FREQ/LEMMA.freq 

FREQ/RLEMMA.freq: $(TMPDATA)
	grep -v -e 'SPEC(vreemd)' -e 'SPEC(deeleigen)' -e 'SPEC(afgebr)' -e 'SPEC(symb)' $(TMPDATA) | cut -f2 | sort | uniq > FREQ/RLEMMA.freq 

FREQ/WORD-POS.freq: $(TMPDATA)
	cut -f1,3  $(TMPDATA) | sort | uniq -c | sort -nr > FREQ/WORD-POS.freq 

FREQ/WORD-LEMMA-POS.freq: $(TMPDATA)
	cut -f1,2,3 $(TMPDATA) | sort | uniq -c | sort -nr > FREQ/WORD-LEMMA-POS.freq 

FREQ/WORD-AMB.freq:FREQ/WORD-LEMMA-POS.freq
	sort -k2,2 -k1,1nr FREQ/WORD-LEMMA-POS.freq\
        | awk '{ if ($$2 == P && $$3 == Q) printf("%s\n%s\n\n",N,$$0); P=$$2; Q=$$3; N=$$0}' > FREQ/WORD-AMB.freq

FREQ/TAG-AMB.freq: FREQ/WORD-LEMMA-POS.freq
	sort -k2,2 -k4,4 -k1,1nr FREQ/WORD-LEMMA-POS.freq\
        | awk '{ if ($$2 == P) printf("%s\t%s\n",Q,$$4); P=$$2; Q=$$4 }' \
	| sort |uniq -c | sort -nr > FREQ/TAG-AMB.freq

FREQ/POS.freq: $(TMPDATA)
	cat $(TMPDATA) | cut -f3 \
        | sort | uniq -c | sort -nr > FREQ/POS.freq 

POSLEMMA/%.poslemma: Enhanced/%/*.xml
	ls -v Enhanced/$*/*.xml | dtxslt --stdin -s $(XSL)/plat.xsl > POSLEMMA/$*.poslemma

mwu: $(DIRECTORIES:%=MWU/%.mwu)

lp: $(DIRECTORIES:%=LP/%.lp)

## removes the .mwu file, because if it is empty it would otherwise
## be rebuilt over and over again!
MWU/%.mwu: Enhanced/%/*.xml $(XSL)/mwu_tags.xsl
	rm -f MWU/$*.mwu
	ls -v Enhanced/$*/*.xml | dtxslt --stdin -q '//node[@cat="mwu"]' -s $(XSL)/mwu_tags.xsl > MWU/$*.mwu

FREQ/POS-WORD-LEMMA.freq: FREQ/POS.freq FREQ/WORD-LEMMA-POS.freq
	for tag in `awk '{ print $$2 }' FREQ/POS.freq|sort`;\
        do echo "--------- $$tag ---------------" ;\
           grep -F $$tag FREQ/WORD-LEMMA-POS.freq;\
        done > FREQ/POS-WORD-LEMMA.freq

suites: $(DIRECTORIES:%=Suites/%.sents)

Suites/%.sents: Enhanced/%/*.xml
	( cd Enhanced/$* ;\
             ls -v ./*.xml |\
             dtxslt --stdin -s\
             $(XSL)/dt2sent_with_key.xsl |\
             sed -e 's/\[/\\\[/g' \
                 -e 's/\]/\\\]/g') > $@

installsents: $(DIRECTORIES:%=Suites/Machine/%_part00001.sents.gz)
	@for file in $(wildcard Suites/Machine/*.sents.gz);\
        do if cmp -s $$file $(ALPINO_HOME)/$$file;\
           then true;\
           else echo cp -f $$file $(ALPINO_HOME)/Suites/Machine/ ;\
                cp -f $$file $(ALPINO_HOME)/Suites/Machine/ ;\
           fi;\
        done

Suites/Machine/%_part00001.sents.gz: Suites/%.sents
	cat $< | (cd Suites/Machine ; $(ALPINO_HOME)/Suites/split_in_parts -o $* -p 200 )
