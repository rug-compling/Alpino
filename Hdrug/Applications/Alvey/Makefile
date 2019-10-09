## include ../../Makefile.defs for .pl.po definition
include ../../Makefile.include

SH=../../hdrug -l start -notk -flag grammar anlt2.conv\
      load_grammar_mode=interpret

POS=anlt2_lc.po anlt2_hc_lc.po hc.po lc.po hc_compile.po lc_compile.po start.po

all_pos: $(POS:.po=.pl)
	( for file in $(?:%.pl=%) ;\
          do echo "save_files('$$file','$$file.po').  ";\
          done ) | ../../hdrug -l start -notk
	touch all_pos

clean:

realclean:
	/bin/rm -f $(POS) all_pos

anlt2_lc.pl:  anlt2.conv
	echo "dump_grammar('anlt2_lc.pl')."|\
   $(SH) parser=lc -parser lc on -parser hc off

anlt2_hc.pl: anlt2.conv anlt2.head
	echo "dump_grammar('anlt2_hc.pl')."|\
   $(SH) parser=hc -parser lc off -parser hc on

anlt2_hc_lc.pl: anlt2.conv anlt2.head
	echo "dump_grammar('anlt2_hc_lc.pl')."|\
   $(SH) parser=hc -parser lc off -parser hc on head=left

RUN=echo "hdrug_go." | ../../hdrug -l start -notk -flag gramar\
    load_grammar_mode=compile -parser hc off\
    -parser -parser lc off object_saving=off

anlt2_test: 
	date >>anlt2.results
	$(RUN) -parser lc on parser=lc           >>anlt2.results
	$(RUN) -parser hc on parser=hc           >>anlt2.results
	$(RUN) -parser hc on parser=hc head=left >>anlt2.results
	date >>anlt2.results

state:
	echo "save_program(state)." | hdrug -l start -notk

