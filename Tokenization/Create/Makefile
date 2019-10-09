ifeq "$(shell if [ -r ../../Makefile.include ]; then echo yes; fi)" "yes"
include ../../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif


ifeq ($(fsa),)
	fsa=fsa -v
endif

all: entities.c libtok.c alp_escape.c libtok_no_breaks.c

tokenize_before_breaks.m: tokenize_before_breaks.pl words_titel.m\
                          words_afkortingen.m abbrs.pl\
                          words_afkorting_with_number.m\
                          words_poule.m
	$(fsa) -aux tokenize_before_breaks -r tokenize_before_breaks \
           > tokenize_before_breaks.m

tokenize_after_breaks.m: words_ambiguous_afkorting_after.m\
                         words_afkorting_after.m abbrs.pl tokenize_after_breaks.pl
	$(fsa) -aux tokenize_after_breaks -r tokenize_after_breaks \
           > tokenize_after_breaks.m

words_afkorting_after.m words_titel.m words_afkortingen.m words_afkorting_with_number.m\
    words_poule.m words_ambiguous_afkorting_after.m: afkortingen.pl
	$(fsa) -aux afkortingen.pl -r $(basename $@) > $@

long.c: long.pl abbrs.pl
	$(fsa) fl_ignore_complex_symbols=on write=c  -aux long -r long_breaks > long.c

#entities.cc: entities.pl
#	$(fsa) fl_ignore_complex_symbols=on write=cpp -aux entities.pl -r entities >entities.cc

entities.c: entities.pl
	$(fsa) fl_ignore_complex_symbols=on write=c -aux entities.pl -r entities >entities.c

libtok_no_breaks.c: tok.pl tokenize_before_breaks.m tokenize_after_breaks.m abbrs.pl
	$(fsa) fl_ignore_complex_symbols=on write=c c_with_main=off -aux tok.pl -r tok_no_breaks > libtok_no_breaks.c

libtok.c: tok.pl tokenize_before_breaks.m tokenize_after_breaks.m abbrs.pl
	$(fsa) fl_ignore_complex_symbols=on write=c c_with_main=off -aux tok.pl -r tok > libtok.c

clean:
	/bin/rm -f *.m *.c

realclean:clean

install:
	if cmp -s libtok.c ../libtok.c; then true; else cp -f libtok.c ../ ; fi
	if cmp -s entities.c ../entities.c; then true; else cp -f entities.c ../ ; fi
	if cmp -s alp_escape.c ../alp_escape.c; then true; else cp -f alp_escape.c ../ ; fi
	if cmp -s libtok_no_breaks.c ../libtok_no_breaks.c; then true; else cp -f libtok_no_breaks.c ../ ; fi

alp_escape.c: alp_escape.pl
	$(fsa) write=c -aux alp_escape.pl -r alp_escape > alp_escape.c

tok: libtok.c ../tok_main.c
	gcc $(CFLAGS) ../tok_main.c libtok.c -o $@

