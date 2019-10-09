TOK=$(ALPINO_HOME)/Tokenization
dir=algemeen_dagblad
DB=/net/corpora/CLEF/$(dir)

FINDSGML=find $(DB) -name '[a-z][a-z]199[45][0-9][0-9][0-9][0-9]' -print | sort
FINDXML=find . -name '*.xml' -print | sort

default: sents

xml:
	for file in `$(FINDSGML)`; do $(MAKE) sgml2xml file=$$file; done

sgml2xml:
	cat head     > $(dir)/$(notdir $(file)).xml
	cat $(file) | recode latin1 >> $(dir)/$(notdir $(file)).xml
	cat tail    >> $(dir)/$(notdir $(file)).xml


FILTER= $(TOK)/tokenize.sh

SENTSQUERY= xsltproc sents.xsl 

keys_with_ru:
	$(FINDXML) | xargs xsltproc ru.xsl > keys_with_ru

xmls=$(wildcard */*.xml)

sents: $(xmls:%.xml=%.sents)

%.sents : %.xml
	$(SENTSQUERY) $*.xml | $(FILTER) | ./number_sents > $*.sents


