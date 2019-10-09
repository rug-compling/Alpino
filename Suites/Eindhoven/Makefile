%.po : %.pl
	echo "compile('$*'), save_files('$*','$*')." | hdrug -notk

clean:
	/bin/rm -f cdb.pl rno.pl pwe.pl obl.pl gbl.pl

realclean: 
	$(MAKE) clean
	/bin/rm -f *.po

init=1
keyprefix=$*

%.pl : %.sents
	echo ":- module(suite,[ sentence/2 ])." > $*.pl
	echo >> $*.pl
	cat $*.sents |\
        sed -e "s/  *$$//" \
            -e "s/'/\\\\'/g" \
            -e "s/\\\\\\\\'/\\\\'/g" \
            -e "s/ /','/g" |\
        awk 'BEGIN   { N=$(init) } \
             /^--$$/ { N++ }       \
             !/^(%|--)/ { printf("sentence($(keyprefix)%d,[%c%s%c]).\n",N,39,$$0,39); N++;}' | sicstus -l ../echo >>$*.pl

