ifeq "$(shell ls ../Makefile.include)" "../Makefile.include"
include ../Makefile.include
else
include $(ALPINO_HOME)/Makefile.include
endif

XSL=$(ALPINO_HOME)/TreebankTools/stylesheets

LASSYSMALL=/net/corpora/LassyDevelop

all: dtchecks novel prepare_enhanced_all enhanced_all poslemma mwu trees_doc sents sentspl conllu pair dact

## slow, because it retries all timed-out sentences
bestrun:
	( cd best; make -j i_suite.log extra.log eans.log )

novel:
	( cd fast; make -j i_suite.log extra.log eans.log )

DIRECTORIES=cgn_exs eans extra g_suite h_suite i_suite leuven_yellow_pages lot_test_suite1 \
             ntv-suite qa wpspel novelsample_corrected cdb Titaantjes

trees_doc: 
	for dir in $(DIRECTORIES); do\
                   ( cd Enhanced/$$dir ;\
                     echo Enhanced/$$dir;\
                     $(MAKE)  -j 30 index.html );\
                   done

trees_realclean:
	for dir in $(DIRECTORIES); do\
                   ( cd Enhanced/$$dir ;\
                     echo Enhanced/$$dir;\
                     $(MAKE) realclean );\
                   done

clean:

install: 

realclean: clean trees_realclean

HDRUGDIR=$(ALPINO_HOME)/Hdrug/Tcl

PWD:=${shell pwd}

transform:
	find $(DIRECTORIES) -name '*.xml' | xargs Alpino -u -xml_converse

dact: $(DIRECTORIES:%=Enhanced/%.dact) 

Enhanced/%.dact: Enhanced/%/*.xml
	alpinocorpus-create -d Enhanced/$*.dact.new Enhanced/$*
	chmod 644 Enhanced/$*.dact.new
	mv Enhanced/$*.dact.new Enhanced/$*.dact

mwu: $(DIRECTORIES:%=MWU/%.mwu) FREQ/mwu-tagged.freq

## removes the .mwu file, because if it is empty it would otherwise
## be rebuilt over and over again!
MWU/%.mwu: Enhanced/%/*.xml $(XSL)/mwu_tags.xsl
	rm -f MWU/$*.mwu
	ls -v Enhanced/$*/*.xml | dtxslt --stdin -q '//node[@cat="mwu"]' -s $(XSL)/mwu_tags.xsl > MWU/$*.mwu

pair: $(DIRECTORIES:%=PAIR/%.pair) $(DIRECTORIES:%=PAIR/%.triple)
	$(MAKE) inconsistent-pairs inconsistent-triples

## the odd sed commands for XX[0-9]XX are there to fool awk into treating
## numbers as strings, otherwise 3.00 = 3.0 = 3 etc
inconsistent-words: # POSLEMMA/*.poslemma $(LASSYSMALL)/POSLEMMA/*.poslemma
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
        > inconsistent-words
	wc -l inconsistent-words

inconsistent-pairs: PAIR/*.pair $(LASSYSMALL)/PAIR/*.pair
	cat PAIR/*.pair $(LASSYSMALL)/PAIR/*.pair \
        | sort \
        | uniq -c \
        | awk '{ if ($$2 == P && $$3 == Q) printf("%s\n%s\n\n",N,$$0); P=$$2; Q=$$3; N=$$0}' \
        > inconsistent-pairs
	wc -l inconsistent-pairs

inconsistent-triples: PAIR/*.triple $(LASSYSMALL)/PAIR/*.triple
	cat PAIR/*.triple $(LASSYSMALL)/PAIR/*.triple \
        | sort \
        | uniq -c \
        | awk '{ if ($$2 == P && $$3 == Q && $$4 == R) printf("%s\n%s\n\n",N,$$0); P=$$2; Q=$$3; R=$$4; N=$$0}' \
        > inconsistent-triples
	wc -l inconsistent-triples

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
	rm inconsistent-words
	$(MAKE) inconsistent-words

POSLEMMA/%.poslemma: Enhanced/%/*.xml
	dtxslt -s $(XSL)/plat.xsl Enhanced/$* > POSLEMMA/$*.poslemma


gitversion=$(shell git log --oneline | wc -l)

tars:
	for dir in $(DIRECTORIES); do make tar dir=$$dir; done

tars-install:
	for dir in $(DIRECTORIES); do make tar-install dir=$$dir; done

tar:
	( cd Enhanced ; tar czf $(dir)-$(gitversion).tar.gz $(dir) )

tar-install: 
	mv Enhanced/$(dir)-$(gitversion).tar.gz ~/public_html/treebanks/
	( cd ~/public_html/treebanks; rm -f $(dir).tar.gz; ln -s $(dir)-$(gitversion).tar.gz $(dir).tar.gz )

dtchecks:
	touch .dtchecks2
	dtchecks $(shell find $(DIRECTORIES) -name '*.xml' -newer .dtchecks | awk -F/ '{ printf "%s\n",$$1 }' | sort -u)
	mv .dtchecks2 .dtchecks

all_dtchecks:
	touch .dtchecks2
	dtchecks $(shell find $(DIRECTORIES) -name '*.xml' | awk -F/ '{ printf "%s\n",$$1 }' | sort -u)
	mv .dtchecks2 .dtchecks

sqldir=/net/aps/haytabo/opt/alpinotools
prepare_enhanced_all: 
	for corpus in $(DIRECTORIES); do make sql/$$corpus.sqlite corpus=$$corpus; done

sql/$(corpus).sqlite: best/$(corpus)/*.xml fast/$(corpus)/*.xml
	rm -f sql/$(corpus).sqlite
	$(sqldir)/auto2sql sql/$(corpus).sqlite best/$(corpus) fast/$(corpus)

enhanced_all:
	for corpus in $(DIRECTORIES); do make enhanced corpus=$$corpus; done 

enhanced: 
	$(sqldir)/alpinomod -p $(corpus)  sql/$(corpus).sqlite $(corpus) Inter/$(corpus)
	ls Inter/$(corpus)/*.xml | sed -e 's/Inter/Enhanced/' | xargs make corpus=$(corpus)

ALUD=~/bin/alud
ALUD_OPTIONS=-t -d

Enhanced/$(corpus)/%.xml: Inter/$(corpus)/%.xml $(ALUD)
	$(ALUD) -a $< > $@


sents: $(DIRECTORIES:%=../SENTS/%.sents)
../SENTS/%.sents: Enhanced/%/*.xml $(XSL)/dt2sent_with_sentid.xsl
	( cd Enhanced/$* ;\
             ls -v *.xml |\
             xargs dtxslt -s\
             $(XSL)/dt2sent_with_sentid.xsl |\
             sed -e 's/\[/\\\[/g' \
                 -e 's/\]/\\\]/g') > $@
sentspl:
	(  cd ../SENTS ; make )

conllu: $(DIRECTORIES:%=../CONLLU/%.conllu)

../CONLLU/%.conllu: Enhanced/%/*.xml $(ALUD)
	ls -v ../Treebank/Enhanced/$*/*.xml | $(ALUD) $(ALUD_OPTIONS) > ../CONLLU/$*.conllu 2> ../CONLLU/$*.conllu.err


#	( cd fast; make eans.log BASE=/net/aistaff/vannoord/z/Alpino )

## all steps for one suite only
update:
	$(MAKE) sql/$(suite).sqlite corpus=$(suite)
	$(MAKE) enhanced corpus=$(suite)
	$(MAKE) POSLEMMA/$(suite).poslemma
	$(MAKE) MWU/$(suite).mwu FREQ/mwu-tagged.freq
	(  cd Enhanced/$(suite); $(MAKE) -s --no-print-directory -j 30 index.html )
	$(MAKE) ../SENTS/$(suite).sents sentspl
	$(MAKE) ../CONLLU/$(suite).conllu
	$(MAKE) Enhanced/$(suite).dact

