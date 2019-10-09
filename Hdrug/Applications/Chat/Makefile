include ../../Makefile.include

POS=newg.po borders.po contain.po cities.po aggreg.po clotab.po countries.po load.po ndtabl.po newdict.po newtop.po ptree.po qplan.po readin.po rivers.po scopes.po slots.po start.po suite.po talkr.po templa.po world0.po  xgrun.po 

all_pos: $(POS:.po=.pl)
	( for file in $(?:%.pl=%) ;\
          do echo "save_files('$$file','$$file.po').  ";\
          done ) | ../../hdrug -l start -notk
	touch all_pos

clean:

realclean:
	/bin/rm -f $(POS) all_pos

