#           -*-Mode: makefile;-*-

ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

ifeq "$(PROLOG)" "sicstus"
POS=lex_accent.po lex_with_dt.po lex_more.po lex.po
else
POS=
endif

TARGETS=$(POS)

.PHONY: all install clean realclean

all: $(TARGETS)

install:

clean:

realclean: clean
	/bin/rm -f $(TARGETS)

%.po: %.pl
	echo "compile(alpino_lex:$*), save_files($*,$*)." |\
              $(HDRUG) -notk

### to create pairs of adjectives and related participles:
### Alpino, load adjectives:adjectives
### tell(pairs), 
### adjectives:m(Stem,_,_), 
### lex:lexicon(verb(_,psp,Frame),Label,[Stem],[],_), 
### functor(Frame,Fun,_), 
### \+ atom_concat(part_,_,Fun),
### format("~q.~n",[pair(Stem,Label)]), 
### fail; told.

DATA=$(ALPINO_HOME)/PosTagger/Construct/TRAINING/*.log.gz

extra:
	zcat $(DATA) | grep FRAME |\
          sed -e 's/FRAME#//' |\
          grep -e 'normal(num_na)' \
               -e 'normal(number_expression)' \
               -e 'normal(number_komma_number)' \
               -e 'normal(number_tiende)' \
               -e 'normal(tussen_number)' \
               -e 'normal(amount)' \
               -e 'normal(temporal_expression)' \
               -e 'normal(date_expression)' \
               -e 'normal(date_year)' \
               -e 'normal(tussen_nu_en_year)' \
               -e 'normal(opening_hours)' \
               -e 'normal(num_meter_num)' \
               -e 'normal(pre_np_adverb)' \
               -e 'normal(date_sequence)' \
               -e 'normal(score)' \
               -e 'normal(chess)' \
               -e 'normal(bridge)' \
               -e 'normal(number_rang)' \
               -e 'normal(numberjarig)' \
               -e 'normal(numberpersoons)' \
               -e 'normal(numberklas)' \
               -e 'normal(numbereeuws)' \
               -e 'normal(numbereeuwse)' \
               -e 'normal(numberklasser)' \
               -e 'normal(numberklassers)' \
               -e 'normal(numbertal)' \
               -e 'normal(numbertallen)' \
               -e 'normal(numberkaart)' \
               -e 'normal(numberkaarten)' \
               -e 'normal(numberdeurs)' \
               -e 'normal(numberwieler)' \
               -e 'normal(numberwielers)' \
               -e 'normal(numberplusser)' \
               -e 'normal(numberplussers)' \
               -e 'normal(numbercilinder)' \
               -e 'normal(numberluik)' \
               -e 'normal(zwemslag)' \
               -e 'normal(topnumber)' \
               -e 'normal(procents)' \
               -e 'normal(een_N_of_NUM)' \
               -e 'normal(een_stuk_of_NUM)' \
               -e 'normal(num_meter_loper)' \
               -e 'normal(hash_tag)' \
               -e 'normal(at_tag)' \
               -e 'normal(retweet)' \
               -e 'normal(enumeration)' \
               -e 'normal(ten_xste)' \
               -e 'normal(verNvoudig)' \
               -e 'normal(enhalve)' \
               -e 'normal(eneenhalve)' \
               -e 'normal(x_voor_x)' \
               -e 'normal(toonsoort)' \
               -e 'normal(Nx)' \
               -e 'normal(spaced_letters)' \
               -e 'normal(url)' \
               -e 'normal(strange_punct)' |\
        grep with_dt |\
        awk -F\| 'BEGIN { OFS="|" } { print $$1,$$2,$$8 }' |\
        sort | uniq -c | sort -nr > extra.in
