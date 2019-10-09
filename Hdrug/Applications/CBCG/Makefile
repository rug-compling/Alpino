include ../../Makefile.include

POS=cf_grammar.po cug.po declarations.po interface.po lexicon.po parser.po\
    shift_reduce.po start.po suite.po types.po ubg_utils.po

all_pos: $(POS:.po=.pl)
	( for file in $(?:%.pl=%) ;\
          do echo "save_files('$$file','$$file.po').  ";\
          done ) | ../../hdrug -l start -notk
	touch all_pos

clean:

realclean:
	/bin/rm -f $(POS) all_pos

