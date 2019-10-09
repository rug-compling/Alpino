ifeq "$(shell if [ -r ../../Makefile.include ]; then echo yes; fi)" "yes"
include ../../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

%.fsa: %.t
	sort -u $*.t |\
        $(S_FSA_MORPH_SCRIPT) |\
	LANG=POSIX LC_ALL=POSIX sort -u |\
	$(S_FSA_BUILD) -o $*.fsa

