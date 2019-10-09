include ../../Makefile.include

POS=lexicon.po templates.po wlists.po lex_string.po approx.po bug_dd.po\
    hc.po hc_d.po\
    hc_dtrs.po lc.po rcp3.po rcp3_d.po rcp3_dtrs.po right_chart.po\
    shift_reduce.po approx_parser.po suite.po compilation.po types.po\
    inflection.po rules.po decl.po start.po

all_pos: $(POS:.po=.pl)
	( for file in $(?:%.pl=%) ;\
          do echo "save_files('$$file','$$file.po').  ";\
          done ) | ../../hdrug -l start -notk
	touch all_pos

clean:

realclean:
	/bin/rm -f $(POS) all_pos
