ifeq "$(shell ls ../Makefile.include)" "../Makefile.include"
include ../Makefile.include
else
include $(ALPINO_HOME)/Makefile.include
endif

default:

all: trees_doc dact

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

m:
	echo $(DIRS_FOR_INDEX)

make_index:
	for dir in $(DIRS_FOR_INDEX); do $(ALPINO_HOME)/Treebank/create_index $$dir $$dir/index.html; done

TARGET=.
DIRS_FOR_INDEX=$(shell find $(TARGET) -type d |grep -v '.svn')

##MANUAL=cdb cgn_exs conll2006-test extra g_suite h_suite i_suite leuven_yellow_pages lot_test_suite1 ovis qa

dact: $(DIRECTORIES:%=%.dact)

%.dact: %/*.xml
	alpinocorpus-create -d $*.dact.new $*
	chmod 644 $*.dact.new
	mv $*.dact.new $*.dact

alpino.dact: $(DIRECTORIES:%=%.dact)
	alpinocorpus-create -d alpino.dact.new $(DIRECTORIES)
	chmod 644 alpino.dact.new
	mv alpino.dact.new alpino.dact

gitversion=$(shell git log --oneline | wc -l)

enhanced:
	/net/corpora/LassyDevelopMod/lassymod -t -u Enhanced/cdb
	$(MAKE) Enhanced/cdb.dact
	( cd Enhanced ; tar czf cdb-$(gitversion).tar.gz cdb )

dtchecks:
	dtchecks $(DIRECTORIES)


index_manual:
	for dir in $(DIRECTORIES); do ( cd $$dir ; make index.html ) ; done

clean_manual:
	for dir in $(DIRECTORIES); do ( cd $$dir ; rm -f *svg *pdf index.html ) ; done
