include ../../Makefile.include

POS= ale.po change.po generator.po parser.po show.po start.po pp_chart.po

all_pos: $(POS:.po=.pl)
	( for file in $(?:%.pl=%) ;\
          do echo "save_files('$$file','$$file.po').  ";\
          done ) | ../../hdrug -l start -notk\
                      -grammar undefined -suite undefined -decl undefined
	touch all_pos

clean:

realclean:
	/bin/rm -f $(POS) all_pos



