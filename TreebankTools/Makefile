all: stylesheets/sanity-checks.xsl
	$(MAKE) -C IndexedCorpus all
	$(MAKE) -C miniact all
	$(MAKE) -C enhance all

DIRS=IndexedCorpus miniact enhance

ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

install: 
	cp bin/act bin/dtchecks bin/dtget bin/dtgrep bin/dtlist \
		bin/dtsearch bin/dtview bin/dtxslt ../bin/
	cp bin/act bin/dtchecks bin/dtget bin/dtgrep bin/dtlist \
		bin/dtsearch bin/dtview bin/dtxslt $(BINDIR)
	for d in $(DIRS); do ( cd $$d ; $(MAKE) install ); done

clean:
	-for d in $(DIRS); do ( cd $$d ; $(MAKE) clean ); done

realclean:
	rm -f stylesheets/sanity-checks.xsl
	for d in $(DIRS); do ( cd $$d ; $(MAKE) realclean ); done

# create a wrapper so that Alpino/DTView know how to find TrEd,
# this (among others) to prevent problems with graphvizs tred 

ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.defs ]; then echo yes; fi)" "yes"
MAKEFILEDEFSDEP=$(ALPINO_HOME)/Makefile.defs
else
MAKEFILEDEFSDEP=
endif

stylesheets/sanity-checks.xsl: create-sanity-check-stylesheet.py
	if echo "" | python 2>/dev/null; then\
		python create-sanity-check-stylesheet.py > \
		stylesheets/sanity-checks.xsl;\
	fi

PORT=44444
start_treebank-server:
	nice -n 20 ./treebank-server -p$(PORT)\
        --corpus-root=/storage/vannoord/z/Alpino/Treebank\
          2>treebank_server.$(PORT).log 1>&2 &

