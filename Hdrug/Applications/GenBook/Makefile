include ../../Makefile.include

POS=mj.po compilation.po transitive.po defaults.po start.po suite.po\
    hc_ch.po hc_mp.po hc_mmp.po hc.po lc_ch.po left_ch.po lex_string.po\
    head_ch.po right_ch.po bug.po bug_wf.po database.po restrict.po d.po\
    packing.po wf.po

all_pos: $(POS:.po=.pl)
	( for file in $(?:%.pl=%) ;\
          do echo "save_files('$$file','$$file.po').  ";\
          done ) | ../../hdrug -l start -notk
	touch all_pos

clean:

realclean:
	/bin/rm -f $(POS) all_pos

