files:=${basename ${shell ls -v | grep '[0-9]*.xml$$' }}
include $(ALPINO_HOME)/Treebank/Makefile.sub

LASSY=/net/corpora/LassyDevelop/Treebank/WR-P-P-L-0000000003

update:
	for f in $(wildcard WR*.xml); do $(MAKE) -s updatefile f=$$f; done

updatefile:
	if cmp -s $(LASSY)/$(f) $(f); then true; else echo "update $(f)"; cp -f $(LASSY)/$(f) .  ; fi

