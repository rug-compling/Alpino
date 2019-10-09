DB=database

TNEW=tokenized
TOLD=tokenized.bak
TOK=$(ALPINO_HOME)/Tokenization

FIND=find $(DB) -follow -name '$(PATTERN)*.xml' -print 
CFIND=( cd $(TNEW); find -name '$(PATTERN)*.sents.gz' -print )

FILTER= $(TOK)/tokenize.sh

QUERY= xargs xsltproc all.xsl 

## bash is required for target comp
SHELL=/bin/bash

all: sents

remove_new:
	find $(TNEW)/database -name '*.sents.gz' -exec rm {} \;

sents:
	@for file in `$(FIND)`; do $(MAKE)  $$file.sents.gz file=$$file; done

comps:
	@for file in `$(CFIND)`; do $(MAKE) --silent comp file=$$file; done

update:
	rm -rf $(TOLD)/database
	mv $(TNEW)/database $(TOLD)/database

$(file).sents.gz: tokenized/$(basename $(file)).sents.gz

tokenized/$(basename $(file)).sents.gz:	correct_escaped_entities all.xsl\
               $(file) treat_specifics number_sents $(FILTER)\
               $(TOK)/tok $(TOK)/tokenize.sh $(TOK)/recognize_enumerations\
               $(TOK)/entities $(TOK)/tokenize_more
	if   `head -n1 $(file)| grep -q utf-8`;\
        then sed -e 's/CO\&amp;inf2;/CO2/g' $(file) \
           | ./correct_escaped_entities\
             >$(file).temp;\
        else cat $(file)\
           | recode latin1 \
           | sed -e 's/encoding="ISO-8859-1/encoding="UTF-8/' \
           | ./correct_escaped_entities\
             > $(file).temp;\
        fi
	mkdir -p tokenized/$(dir $(file))
	echo $(file).temp | $(QUERY) | $(FILTER) \
         | ./treat_specifics $(file) \
         | grep -v 'De uitgever heeft geen publicatierecht' \
         | ./number_sents  > tokenized/$(basename $(file)).sents
	gzip tokenized/$(basename $(file)).sents
	rm $(file).temp

#%% make sent file=database/.../..../...xml
sent:
	$(MAKE) $(file).sents.gz

#COMPFILTER=cat
COMPFILTER=tr -d '”,"`' | tr -d "'" | tr -s ' ' | sed -e 's|\\\]|)|g' -e 's|\\\[|(|g'

comp:
	@echo $(basename $(file))
	-diff <( zcat $(TNEW)/$(file) | $(COMPFILTER) ) <( zcat  $(TOLD)/$(file) | $(COMPFILTER) )

correct_escaped_entities.c: correct_escaped_entities.pl
	fsa -vv write=c -aux correct_escaped_entities.pl -r entities >correct_escaped_entities.c

newspapers: 19.sents 20.sents dd.sents els.sents gra.sents hpt.sents nh.sents trouw1998.sents\
            vnl.sents www.sents ad.sents nrc.sents parool.sents trouw.sents vk.sents volkskrant.sents

19.sents:
	$(MAKE) sents  PATTERN=19
20.sents:
	$(MAKE) sents  PATTERN=20
dd.sents:
	$(MAKE) sents  PATTERN=dd  
els.sents:
	$(MAKE) sents  PATTERN=els  
gra.sents:
	$(MAKE) sents  PATTERN=gra  
hpt.sents:
	$(MAKE) sents  PATTERN=hpt  
nh.sents:
	$(MAKE) sents  PATTERN=nh  
trouw1998.sents:
	$(MAKE) sents  PATTERN=trouw1998 
vnl.sents:
	$(MAKE) sents  PATTERN=vnl 
www.sents:
	$(MAKE) sents  PATTERN=www  
ad.sents:
	$(MAKE) sents  PATTERN=ad  
nrc.sents:
	$(MAKE) sents  PATTERN=nrc  
parool.sents:
	$(MAKE) sents  PATTERN=parool  
trouw.sents:
	$(MAKE) sents  PATTERN=trouw  
vk.sents:
	$(MAKE) sents  PATTERN=vk 
volkskrant.sents:
	$(MAKE) sents  PATTERN=volkskrant 

compare:
	$(MAKE) comps  PATTERN=19  
	$(MAKE) comps  PATTERN=20  
	$(MAKE) comps  PATTERN=dd  
	$(MAKE) comps  PATTERN=els  
	$(MAKE) comps  PATTERN=gra  
	$(MAKE) comps  PATTERN=hpt  
	$(MAKE) comps  PATTERN=nh  
	$(MAKE) comps  PATTERN=trouw1998  
	$(MAKE) comps  PATTERN=vnl  
	$(MAKE) comps  PATTERN=www  
	$(MAKE) comps  PATTERN=ad  
	$(MAKE) comps  PATTERN=nrc  
	$(MAKE) comps  PATTERN=parool  
	$(MAKE) comps  PATTERN=trouw  
	$(MAKE) comps  PATTERN=vk  
	$(MAKE) comps  PATTERN=volkskrant  











