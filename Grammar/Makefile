#           -*-Mode: makefile;-*-

EXTRA=cdb/pc_weights.t cdb/pd_weights.t cdb/pp_weights.t cdb/px_weights.t cdb/pw_weights.t cdb/pi_weights.t cdb/pu_weights.t

ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

ifeq "$(PROLOG)" "sicstus"
POS=penalty_weights.po grammar_lc.po frames.po penalties.po $(EXTRA:%.t=%.po)
else
POS=penalty_weights.pl grammar_lc.pl $(EXTRA:%.t=%.pl)
endif

all: $(POS)

## make sure this works even if Alpino has not been built a first time yet
ALPINO=$(HDRUG) grammar=n penalties=n lex=n lexicon=n suite=n\
          -l ../src/start cmdint=off -notk debug=1

ALPINOGEN=$(HDRUG) penalties=n lex=n lexicon=n suite=n\
          -l ../src/start cmdint=off -notk debug=1

grammar_lc.pl: grammar types.pl rules.gram data.pl\
          ../src/lc.pl ../src/wappend.pl
	echo "dump_grammar." |  $(ALPINO) grammar=grammar

checks:
	Alpino suite=undefined lexicon=undefined -d -l checks  \
           batch_command=grammar_checks

clean:

realclean: clean
	/bin/rm -f penalty_weights.pl grammar_lc.pl *.po

install:

frames.po : frames.pl
	echo "prolog_flag(single_var_warnings,_,off),\
              compile(frames),\
              save_files(frames,frames)." | $(HDRUG) -notk

%.po : %.pl
	echo "compile('$*'), save_files('$*','$*')." | $(HDRUG) -notk

%.pl:%.t ../Disambiguation/index.pl ../Disambiguation/estimate2pl
	../Disambiguation/estimate2pl < $< |\
          $(HDRUG) -l ../Disambiguation/index > $*.pl


