include Makefile.include

## zlib and Derivbank removed
## Derivbank gave segmentation violation when loaded in SICStus

DIRS= TreebankTools fadd unix SuffixArrays PosTagger\
      Names Tokenization Generation/fluency Suites Lexicon\
      Grammar Treebank src

.PHONY: all install clean realclean xref checks

export ALPINO_HOME

default:
	echo '"make quick" for most users'
	echo '"make rebuild" if you have all relevant tools'

quick:
	( cd Hdrug ; $(MAKE) hdrug state )
	( cd src ; $(MAKE) guides$(MODULEEXT) )
	for dir in $(DIRS); do ( if [ -d $$dir ]; \
                                 then cd $$dir ; $(MAKE);\
                                 fi ); done

rebuild:
	( cd Lexicon/Build; $(MAKE) ; $(MAKE) install )
	( cd Names/Build; $(MAKE) ; $(MAKE) install )
	$(MAKE) quick

treebank:
	( cd Treebank ; $(MAKE) all )

install:
	-for dir in $(DIRS); do ( if [ -d $$dir ]; \
                                  then cd $$dir ; $(MAKE) install;\
                                  fi ); done
	install -D Annotate $(BINDIR)

clean:
	-for dir in Hdrug $(DIRS); do ( if [ -d $$dir ]; \
                                  then cd $$dir ; $(MAKE) clean;\
                                  fi ); done

realclean:
	-for dir in Hdrug $(DIRS); do ( if [ -d $$dir ]; \
                                  then cd $$dir ; $(MAKE) realclean;\
                                  fi ); done

xref:
	@spxref -i xref.pl -w - Hdrug/Prolog/hdrug_main Hdrug/Prolog/hooks\
                         src/start.pl src/hooks.pl\
                        Generation/fluency/pro_lm\
                        Grammar/penalties.pl Generation/fluency/maxent.pl Lexicon/lex.pl\
        | grep -v 'can not follow'
	@echo "(26 warnings expected; 1 unused ugraphs)"


include Makefile.export

