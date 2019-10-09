THIS=..

%.fsa: %.txt
	tr -s '\012\011 ' '\012' <$< |\
        LANG=POSIX LC_ALL=POSIX sort -u |\
        fsa_build -N -o $@

%.fsa: %.txt.gz
	zcat $< |\
        tr -s '\012\011 ' '\012' |\
        LANG=POSIX LC_ALL=POSIX sort -u |\
        fsa_build -N -o $@

%.fsa: %.sents.gz
	zcat $< |\
        tr -s '\012\011 ' '\012' |\
        LANG=POSIX LC_ALL=POSIX sort -u |\
        fsa_build -N -o $@

f=1
N=30
SUFFIX_ARRAY_OPTIONS=
%.ngram: %.sents.gz %.fsa
	zcat $< |\
	tr -s '\012\011 ' '\012' |\
        LANG=POSIX LC_ALL=POSIX $(THIS)/suffix_array -e -d $*.fsa -N -w -G\| -n $(N) -f $(f) -u $(SUFFIX_ARRAY_OPTIONS)> $@

%.ngram: %.txt.gz %.fsa
	zcat $< |\
	tr -s '\012\011 ' '\012' |\
        LANG=POSIX LC_ALL=POSIX $(THIS)/suffix_array -e -d $*.fsa -N -w -G\| -n $(N) -f $(f) -u $(SUFFIX_ARRAY_OPTIONS)> $@

%.ngram: %.txt %.fsa
	tr -s '\012\011 ' '\012' <$< |\
        LANG=POSIX LC_ALL=POSIX $(THIS)/suffix_array -e -d $*.fsa -N -w -G\| -n $(N) -f $(f) -u $(SUFFIX_ARRAY_OPTIONS)> $@

table: mistakes.ngram all.ngram
	$(THIS)/parsability_table mistakes.ngram all.ngram |\
           sort  -k 1,1n -k2,2rn > table

.PRECIOUS : %.fsa %.ngram

