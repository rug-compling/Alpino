TOK=$(ALPINO_HOME)/Tokenization

eans.named_sents: xlist01.named_sents xlist02.named_sents xlist03.named_sents xlist04.named_sents xlist05.named_sents xlist06.named_sents xlist07.named_sents  xlist08.named_sents xlist09.named_sents xlist10.named_sents xlist11.named_sents xlist12.named_sents xlist13.named_sents  xlist14.named_sents xlist15.named_sents xlist16.named_sents xlist17.named_sents xlist18.named_sents xlist19.named_sents  xlist20.named_sents xlist21.named_sents xlist22.named_sents xlist23.named_sents xlist24.named_sents xlist26.named_sents  xlist27.named_sents xlist28.named_sents xlist29.named_sents xlist30.named_sents
	cat xlist??.named_sents |\
        grep -v '|$$' |\
        sed -e 's/[(].*[)] *$$//' |\
        sed -e 's/[(] = .*[)] //' \
        >eans.named_sents


# xml2examples, tokenize, remove spaces in initial quotes, remove final comments ( )
# remove examples with A : (dialogues with meta-linguistic stuff)
# remove empty examples (ending in '|')
# remove examples with '/' (alternatives), and '|' (position markings?) and internal '( )' comments...
# replace / in ids by _
%.named_sents : %.xml xml2examples.pl
	sicstus -l xml2examples -a $*.xml |\
          sed -e 's/$$/#/g' | $(TOK)/tok | tr '#\012' '\012 ' |\
        sed -e s/"^'"// \
            -e s/"^ '"// \
            -e s/" ' | "/"|"/ \
            -e s/"\/"/"\_"/g  > $*.named_sents 

## for the ANS stuff, assume there already is a key (after |-symbol):
%.pl : %.named_sents
	echo ":- module(suite,[ sentence/2 ])." > $*.pl
	echo >> $*.pl
	cat $*.named_sents |\
        sed -e "s/  *$$//" \
            -e "s/'/\\\\'/g" \
            -e "s/\\\\\\\\'/\\\\'/g" \
            -e "s/ /','/g" |\
        awk -F\| '/^[^-%]/ { printf("sentence(%c%s%c,[%c%s%c]).\n",39,$$1,39,39,$$2,39); N++;}' | sicstus -l ../echo >>$*.pl

