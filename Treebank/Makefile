ifeq "$(shell ls ../Makefile.include)" "../Makefile.include"
include ../Makefile.include
else
include $(ALPINO_HOME)/Makefile.include
endif

default:

all: trees_doc prepare_enhanced_all enhanced_all dact poslemma mwu

DIRECTORIES=$(TREEBANKS)

trees_doc:
	for dir in $(DIRECTORIES); do\
                   ( cd $$dir ;\
                     echo $$dir;\
                     $(MAKE) --no-print-directory -j 30 doc );\
                   done

trees_realclean:
	for dir in $(DIRECTORIES); do\
                   ( cd $$dir ;\
                     echo $$dir;\
                     $(MAKE) realclean );\
                   done

clean:

install: 

realclean: clean trees_realclean

HDRUGDIR=$(ALPINO_HOME)/Hdrug/Tcl

PWD:=${shell pwd}

transform:
	find $(DIRECTORIES) -name '*.xml' | xargs Alpino -u -xml_converse

d:
	@echo $(DIRECTORIES:%=Enhanced/%.dact)

dact: $(DIRECTORIES:%=Enhanced/%.dact) 

%.dact: %/*.xml
	alpinocorpus-create -d $*.dact.new $*
	chmod 644 $*.dact.new
	mv $*.dact.new $*.dact

mwu: $(DIRECTORIES:%=MWU/%.mwu)

## removes the .mwu file, because if it is empty it would otherwise
## be rebuilt over and over again!
MWU/%.mwu: %.dact mwu_tags.xsl
	rm -f MWU/$*.mwu
	alpinocorpus-xslt -q '//node[@cat="mwu"]' mwu_tags.xsl $*.dact > MWU/$*.mwu

poslemma: $(DIRECTORIES:%=POSLEMMA/%.poslemma)

POSLEMMA/%.poslemma: %.dact
	dtxslt -s plat.xsl $* > POSLEMMA/$*.poslemma


gitversion=$(shell git log --oneline | wc -l)

# enhanced:
# 	/net/corpora/LassyDevelopMod/lassymod -t -u Enhanced/cdb
# 	/net/corpora/novelsample_correctedMod/novelmod -u Enhanced/novelsample_corrected

tars:
	for dir in $(enhanceddirs); do make tar dir=$$dir; done

tars-install:
	for dir in $(enhanceddirs); do make tar-install dir=$$dir; done

tar:
	( cd Enhanced ; tar czf $(dir)-$(gitversion).tar.gz $(dir) )

tar-install: 
	mv Enhanced/$(dir)-$(gitversion).tar.gz ~/public_html/treebanks/
	( cd ~/public_html/treebanks; rm -f $(dir).tar.gz; ln -s $(dir)-$(gitversion).tar.gz $(dir).tar.gz )

dtchecks:
	dtchecks $(DIRECTORIES)


index_manual:
	for dir in $(DIRECTORIES); do ( cd $$dir ; make index.html ) ; done

clean_manual:
	for dir in $(DIRECTORIES); do ( cd $$dir ; rm -f *svg *pdf index.html ) ; done

enhanceddirs=cgn_exs eans extra g_suite h_suite i_suite leuven_yellow_pages lot_test_suite1 \
             ntv-suite qa wpspel novelsample_corrected cdb

sqldir=/net/aps/haytabo/opt/alpinotools
prepare_enhanced_all:
	for corpus in $(enhanceddirs); do make sql/$$corpus.sqlite corpus=$$corpus; done

sql/$(corpus).sqlite: best/$(corpus)/*.xml fast/$(corpus)/*.xml
	rm -f sql/$(corpus).sqlite
	$(sqldir)/auto2sql sql/$(corpus).sqlite best/$(corpus) fast/$(corpus)

enhanced_all:
	for corpus in $(enhanceddirs); do make enhanced corpus=$$corpus; done

enhanced: 
	$(sqldir)/alpinomod -p $(corpus) -u sql/$(corpus).sqlite $(corpus) Enhanced/$(corpus)


