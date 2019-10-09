MAKETUPLE=../maketuple.pl
VERSION=

%.po : %.pl
	echo "compile('$*'), save_files('$*','$*')." | hdrug -notk

%.btpl : %.bigram
	awk -F\| '{ print $$1 }' $<  > words
	awk -F\| '{ print $$2 }' $< >> words
	sort -u words | fsa_build -N -o $*_b.fsa
	$(MAKETUPLE) $(VERSION) total=3 words=2 sep='\|' out=$*.btpl $< +$*_b +$*_b
	wc -c $*_b.fsa $*.btpl

%.ttpl : %.trigram
	awk -F\| '{ print $$1 }' $<  > words
	awk -F\| '{ print $$2 }' $< >> words
	awk -F\| '{ print $$3 }' $< >> words
	sort -u words | fsa_build -N -o $*_t.fsa
	$(MAKETUPLE) $(VERSION) total=4 words=3 sep='\|' out=$*.ttpl $< +$*_t +$*_t +$*_t
	wc -c $*_t.fsa $*.ttpl

%.fpttpl : %.fptrigram
	awk -F\| '{ print $$1 }' $<  > words
	awk -F\| '{ print $$2 }' $< >> words
	awk -F\| '{ print $$3 }' $< >> words
	sort -u words | fsa_build -N -o $*_t.fsa
	$(MAKETUPLE) ver=4 total=4 words=3 sep='\|' out=$*.fpttpl $< +$*_t +$*_t +$*_t
	wc -c $*_t.fsa $*.fpttpl

%.ftpl : %.fourgram
	awk -F\| '{ print $$1 }' $<  > words
	awk -F\| '{ print $$2 }' $< >> words
	awk -F\| '{ print $$3 }' $< >> words
	awk -F\| '{ print $$4 }' $< >> words
	sort -u words | fsa_build -N -o $*_f.fsa
	$(MAKETUPLE) $(VERSION) total=5 words=4 sep='\|' out=$*.ftpl $< +$*_f +$*_f +$*_f +$*_f
	wc -c $*_f.fsa $*.ftpl

%.bconc : %.bigram
	awk -F\| '{ printf "%s#%s|%d\n",$$1,$$2,$$3 }' $<  > $*.bc
	awk -F\| '{ print $$1 }' $*.bc | sort -u | fsa_build -N -o $*_cb.fsa
	$(MAKETUPLE) $(VERSION) total=2 words=1 sep='\|' out=$*.bconc $*.bc +$*_cb
	wc -c $*_cb.fsa $*.bconc

%.tconc : %.trigram
	awk -F\| '{ printf "%s#%s#%s|%d\n",$$1,$$2,$$3,$$4 }' $<  > $*.tc
	awk -F\| '{ print $$1 }' $*.tc | sort -u | fsa_build -N -o $*_ct.fsa
	$(MAKETUPLE) $(VERSION) total=2 words=1 sep='\|' out=$*.tconc $*.tc +$*_ct
	wc -c $*_ct.fsa $*.tconc

%.fptconc : %.fptrigram
	awk -F\| '{ printf "%s#%s#%s|%d\n",$$1,$$2,$$3,$$4 }' $<  > $*.tc
	awk -F\| '{ print $$1 }' $*.tc | sort -u | fsa_build -N -o $*_ct.fsa
	$(MAKETUPLE) ver=3 total=2 words=1 sep='\|' out=$*.fptconc $*.tc +$*_ct
	wc -c $*_ct.fsa $*.fptconc

%.fconc : %.fourgram
	awk -F\| '{ printf "%s#%s#%s#%s|%d\n",$$1,$$2,$$3,$$4,$$5 }' $< > $*.fc
	awk -F\| '{ print $$1 }' $*.fc | sort -u | fsa_build -N -o $*_cf.fsa
	$(MAKETUPLE) $(VERSION) total=2 words=1 sep='\|' out=$*.fconc $*.fc +$*_cf
	wc -c $*_cf.fsa $*.fconc

llfeats.tpl: data.weights
	$(MAKETUPLE) ver=3 total=2 words=1 sep='\|' out=llfeats.tpl \
            data.weights llfeats
	wc -c llfeats.tpl llfeats.fsa

clean:
	rm -f *.[btf]tpl *.wd? *.fsa *.[btf]c rest words tmp\
              *.pl *.po *.[btf]conc *.fpttpl *.fptconc

realclean: clean
	rm -f *.bigram *.trigram *.fourgram load_bigram load_trigram load_fourgram *.fptrigram load_bigram_p load_trigram_fp
