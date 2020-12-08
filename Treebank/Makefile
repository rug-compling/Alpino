ifeq "$(shell ls ../Makefile.include)" "../Makefile.include"
include ../Makefile.include
else
include $(ALPINO_HOME)/Makefile.include
endif

default:

all: trees_doc enhanced dact poslemma

ignoredoc=lassy $(wildcard RSTCode/????) gen_g_suite medfail

DIRECTORIES=$(filter-out $(ignoredoc), $(wildcard $(TREEBANKS)))

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

DACTS=$(filter-out cdb novelsample_corrected, $(DIRECTORIES))

dact: $(DACTS:%=%.dact) Enhanced/cdb.dact Enhanced/novelsample_corrected.dact

%.dact: %/*.xml
	alpinocorpus-create -d $*.dact.new $*
	chmod 644 $*.dact.new
	mv $*.dact.new $*.dact

alpino.dact: $(DIRECTORIES:%=%.dact)
	alpinocorpus-create -d alpino.dact.new $(DIRECTORIES)
	chmod 644 alpino.dact.new
	mv alpino.dact.new alpino.dact

poslemma: $(DIRECTORIES:%=POSLEMMA/%.poslemma)

POSLEMMA/%.poslemma: %.dact
	dtxslt -s plat.xsl $* > POSLEMMA/$*.poslemma


gitversion=$(shell git log --oneline | wc -l)

enhanced:
	/net/corpora/LassyDevelopMod/lassymod -t -u Enhanced/cdb
	/net/corpora/novelsample_correctedMod/novelmod -u Enhanced/novelsample_corrected

tar:
	( cd Enhanced ; tar czf cdb-$(gitversion).tar.gz cdb )

tar-install: tar
	mv Enhanced/cdb-$(gitversion).tar.gz ~/public_html/treebanks/
	( cd ~/public_html/treebanks; rm cdb.tar.gz; ln -s cdb-$(gitversion).tar.gz cdb.tar.gz )

dtchecks:
	dtchecks $(DIRECTORIES)


index_manual:
	for dir in $(DIRECTORIES); do ( cd $$dir ; make index.html ) ; done

clean_manual:
	for dir in $(DIRECTORIES); do ( cd $$dir ; rm -f *svg *pdf index.html ) ; done
