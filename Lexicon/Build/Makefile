#           -*-Mode: makefile;-*-

ifeq "$(shell if [ -r ../../Makefile.include ]; then echo yes; fi)" "yes"
include ../../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

.PHONY: all install clean realclean

TARGETS=lex.fsa lex_prefix.fsa lex_inv.fsa lex_accent.pl lex_with_dt.pl 

all: $(TARGETS)

install:
	for t in $(TARGETS); do if cmp -s $$t ../$$t; then true ; else cp -f $$t ../; fi; done

clean:
	/bin/rm -f lex_accent0.pl lex_with_dt0.pl lex.t lex_prefix.t lex_inv.t 

realclean: clean
	/bin/rm -f $(TARGETS) lex_with_dt.pl lex_accent.pl

#  extra=expand_subcat=off
extra=

lex_inv.t lex.t lex_accent0.pl lex_prefix.t lex_with_dt0.pl: decl.pl nouns.pl\
                              verbs.pl adjectives.pl add_compounds.pl\
                              numbers.pl misc.pl adverbs.pl extra.pl
	echo "list_entries." | $(HDRUG) -notk -l decl $(extra)

%.fsa: %.t
	LANG=POSIX LC_ALL=POSIX sort -u $*.t |\
        sed -e 's/+/@@@@@/g' |\
        $(S_FSA_MORPH_SCRIPT) |\
	LANG=POSIX LC_ALL=POSIX sort -u |\
	$(S_FSA_BUILD) -o $*.fsa

lex_accent.pl: lex_accent0.pl
	sort -u lex_accent0.pl > lex_accent.pl

lex_with_dt.pl: lex_with_dt0.pl
	sort -u lex_with_dt0.pl > lex_with_dt.pl

### check if participles are adjectives too:
### grep verb lex.t |grep 'verb([^,]*,psp' | cut -f1 | grep -v ' ' | sort -u > psp
### grep 'adjective(' lex.t |cut -f1 |sort -u > adj
### for line in `cat psp`; do if `grep -q $line adj`; then echo ; else echo "$line"; fi; done |grep . > TODO


