ifeq "$(shell if [ -r ../../Makefile.include ]; then echo yes; fi)" "yes"
include ../../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

.PHONY: all

all: 
	$(MAKE) sorted f=org
	$(MAKE) sorted f=geo
	$(MAKE) sorted f=per
	$(MAKE) sorted f=misc
	$(MAKE) seeds.fsa
	$(MAKE) name_accents.pl

f=geo
sorted:
	if ! LANG=POSIX LC_ALL=POSIX sort -uc $(f); then cp $(f) $(f).bak; LANG=POSIX LC_ALL=POSIX sort -u $(f).bak | grep . >$(f); rm $(f).bak ; fi

seeds: org per geo misc
	cat org  | awk '{ printf "%s\t%s\t%s\n",$$0,$$0,"ORG"  }'  > seeds
	cat per  | awk '{ printf "%s\t%s\t%s\n",$$0,$$0,"PER"  }' >> seeds
	cat geo  | awk '{ printf "%s\t%s\t%s\n",$$0,$$0,"LOC"  }' >> seeds
	cat misc | awk '{ printf "%s\t%s\t%s\n",$$0,$$0,"MISC" }' >> seeds
	cat org per geo misc | awk '{ if (NF>1) printf "%s\t%s\t#%s\n",\
                $$1,$$1,NF-1 }' | sort -u >> seeds

seeds.fsa: seeds
	cat seeds |\
        sed -e 's/+/@@@@@/g' |\
        $(S_FSA_MORPH_SCRIPT) |\
        LANG=POSIX LC_ALL=POSIX sort -u |\
        $(S_FSA_BUILD) -o seeds.fsa

clean:
	/bin/rm -f seeds

realclean: clean
	/bin/rm -f seeds.fsa features.po features.pl

install:
	if cmp -s seeds.fsa ../seeds.fsa; then true ; else cp seeds.fsa ../ ; fi
	if cmp -s name_accents.pl ../name_accents.pl.$(PROLOG);\
        then true ;\
        else cp name_accents.pl ../name_accents.pl.$(PROLOG);\
        fi

name_accents.pl: seeds
	cut -f1 seeds | Alpino -notk -l accents | sort -u > name_accents.pl
