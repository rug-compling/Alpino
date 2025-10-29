include Makefile.include

## zlib and Derivbank removed
## Derivbank gave segmentation violation when loaded in SICStus

DIRS= TreebankTools fadd unix SuffixArrays PosTagger\
      Names Tokenization Generation Generation/fluency Suites Lexicon\
      Grammar src

.PHONY: all install clean realclean xref checks

export ALPINO_HOME

quick:
	$(MAKE) -C Hdrug hdrug state
	$(MAKE) -C src guides$(MODULEEXT)
	$(MAKE) -C TreebankTools
	$(MAKE) -C fadd
	$(MAKE) -C unix
	$(MAKE) -C SuffixArrays
	$(MAKE) -C PosTagger
	$(MAKE) -C Names
	$(MAKE) -C Tokenization
	$(MAKE) -C Generation
	$(MAKE) -C Generation/fluency
	$(MAKE) -C Suites
	$(MAKE) -C Lexicon
	$(MAKE) -C Grammar
	$(MAKE) -C src

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
	rm -f version

xref:
	@spxref -i xref.pl -w - Hdrug/Prolog/hdrug_main Hdrug/Prolog/hooks\
                         src/start.pl src/hooks.pl\
                        Generation/fluency/pro_lm\
                        Grammar/penalties.pl Generation/fluency/maxent.pl Lexicon/lex.pl\
        | grep -v 'can not follow'
	@echo "(25 warnings expected; 1 unused ugraphs)"


include Makefile.export

