include ../../Makefile.include

POS= bt.po bt_d.po bt_tree.po mm.po mm_d.po mm_tree.po\
     pack.po pack_d.po pack_tree.po parse.po memo.po\
     compile.po gram.po pretty.po start.po suite.po

all_pos: $(POS:.po=.pl)
	( for file in $(?:%.pl=%) ;\
          do echo "save_files('$$file','$$file.po').  ";\
          done ) | ../../hdrug -l start -notk
	touch all_pos

clean:

realclean:
	/bin/rm -f $(POS) all_pos

