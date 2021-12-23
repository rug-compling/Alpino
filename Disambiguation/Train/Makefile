MODEL_NUMBER_ANALYSES=200
MODEL_MAX_SENTENCE_LENGTH=400
TESTDEBUG=
ESTIMATE=tinyest --l1 0.0001

## for which we build a model:
MPARTS=px pp pd pc pZ pw pi pu pl pe
## parts which are required for other models:
TPARTS=px pp pd po pq pc pw pi pu pl pe
TESTPARTS=px pp pd pc pw pi pu pl pe

MIN_FEATURE_FREQ=$(shell cat min_feature_freq)

default: 

##### to make new data:
##### ( cd Data ; make realclean; make -j 25 ; make install )

##### after that:
##### make tenfold

##### and to check:
##### make test

install: 
	cp pu.weights ../../Grammar/cdb/pu_weights.t
	cp pd.weights ../../Grammar/cdb/pd_weights.t
	cp pp.weights ../../Grammar/cdb/pp_weights.t
	cp px.weights ../../Grammar/cdb/px_weights.t
	cp pc.weights ../../Grammar/cdb/pc_weights.t
	cp pw.weights ../../Grammar/cdb/pw_weights.t
	cp pi.weights ../../Grammar/cdb/pi_weights.t
	cp pl.weights ../../Grammar/cdb/pl_weights.t
	cp pe.weights ../../Grammar/cdb/pe_weights.t
	cp pZ.weights ../../Grammar/penalty_weights.t

clean:
	rm -f *.tcounts *.weights *.iweights *.features *.zest 

realclean:
	rm -f *.tcounts *.weights *.iweights *.features *.zest *.gz *.counts *.test nohup.out

### TESTING and LOGGING

test: 
	$(MAKE) -j $(TESTPARTS:%=%.test)
	echo "START RUN" > tenfold.log
	-@awk '/first-p\/m/ {XFP=XFP+$$2;XFN=XFN+$$3}\
              /best-p\/m/  {XBP=XBP+$$2;XBN=XBN+$$3}\
              /test-p\/m/  {XTP=XTP+$$2;XTN=XTN+$$3}\
              /first-av/ {XFS=XFS+$$2;XFC=XFC+$$3}\
              /best-av/  {XBS=XBS+$$2;XBC=XBC+$$3}\
              /test-av/  {XTS=XTS+$$2;XTC=XTC+$$3;XXX++}\
              END       {FIRST=100*(1-XFP/XFN);\
                         BEST=100*(1-XBP/XBN);\
                         TEST=100*(1-XTP/XTN);\
                         printf "total result after %s tests\n",XXX;\
                         printf "first %.2f\n", FIRST;\
                         printf "best  %.2f\n", BEST;\
                         printf "test  %.2f\n", TEST;\
                         printf "rate  %.2f\n", \
                           100*(TEST-FIRST)/(BEST-FIRST);\
                         printf "p/s-first %.2f\n",XFS/XFC;\
                         printf "p/s-best  %.2f\n",XBS/XBC;\
                         printf "p/s-test  %.2f\n",XTS/XTC;\
                         printf "p/s-rate  %.2f\n",\
                           100*(XTS/XTC-XFS/XFC)/(XBS/XBC-XFS/XFC);\
                        } ' $(TESTPARTS:%=%.test) >>tenfold.log
	-wc -l *.weights >>tenfold.log
	-echo out of memory `cat Data/p*.log | grep -c 'out of memory'` >>tenfold.log
	-echo timed out `cat Data/p*.log | grep -c 'timed out'`         >>tenfold.log
	-echo sentences `cat Data/p*.log | grep -c 'Done parser'`       >>tenfold.log
	-cat Data/p*.log | grep '^Found [0-9][0-9]* solution[(]s[)]$$' |\
              awk '{ N=N+$$2 } END {print N,"parses in total"} '   >>tenfold.log
	( mv parses parses0 ; $(MAKE) -s parses )
	grep -w score *.test >>tenfold.log
	echo "END RUN" >> tenfold.log
	cat tenfold.log >>log

%.test: %.gz %.weights
	zcat $*.gz |\
        grep '^MAXENT#'|\
        sed -e 's/MAXENT#//' |\
        $(ALPINO_HOME)/Disambiguation/kleiweg/eval -w $*.weights $(TESTDEBUG) > $@

tenfold.analyse:

parses:
	-cat Data/p*.log | grep '^T#' > parses

prec:
	@awk '/^first-precision/ {FP=FP+$$2; FN=FN+1};\
             /^first-recall/    {FR=FR+$$2};\
             /^first-fscore/    {FF=FF+$$2};\
             /^best-precision/ {BP=BP+$$2; BN=BN+1};\
             /^best-recall/    {BR=BR+$$2};\
             /^best-fscore/    {BF=BF+$$2};\
             /^test-precision/ {TP=TP+$$2; TN=TN+1};\
             /^test-recall/    {TR=TR+$$2};\
             /^test-fscore/    {TF=TF+$$2};\
             END { printf "first-precision-total %s\n",FP/FN;\
                   printf "first-recall-total    %s\n",FR/FN;\
                   printf "first-fscore-total    %s\n",FF/FN;\
                   printf "best-precision-total  %s\n",BP/BN;\
                   printf "best-recall-total     %s\n",BR/BN;\
                   printf "best-fscore-total     %s\n",BF/BN;\
                   printf "test-precision-total  %s\n",TP/TN;\
                   printf "test-recall-total     %s\n",TR/TN;\
                   printf "test-fscore-total     %s\n",TF/TN; }' $(log)

### PREPARATION OF DATA FILES, AND ACTUAL TRAINING

%.counts: %.sgz ignore_features
	zcat $*.sgz\
        | $(ALPINO_HOME)/Disambiguation/count_features\
        | grep -v -f ignore_features > $*.counts

%.zest : %.features
	zcat p[$($*_PARTS)].sgz |\
             $(ALPINO_HOME)/Disambiguation/prepare_estimate -l $(MODEL_MAX_SENTENCE_LENGTH)\
                                 -f $*.features | $(ALPINO_HOME)/Disambiguation/score0-1.py | gzip > $@

%.tcounts: *.counts
	cat p[$($*_PARTS)].counts | $(ALPINO_HOME)/Disambiguation/count_features -a \
                > $@

%.iweights: %.zest 
	$(ESTIMATE)  $<  >  $@

%.weights: %.iweights %.features
	paste -d\| $*.features $< | awk -F\| '{ if ($$3>0||$$3<0) printf("%s|%s\n",$$2,$$3) }' >$@

%.sgz : %.gz
	zcat $*.gz | sed -e 's/MAXENT#//'  | $(ALPINO_HOME)/Disambiguation/extract_subset -n $(MODEL_NUMBER_ANALYSES) | gzip > $*.sgz

%.features: %.tcounts min_feature_freq
	awk -F\| 'BEGIN { N=0 }\
                  { if ( $$4 > $(MIN_FEATURE_FREQ) )\
                              {  print N "|" $$1; N++} }'\
           $*.tcounts >$*.features

tenfold:
	$(MAKE) -j $(TPARTS:%=%.sgz) 
	$(MAKE) -j $(TPARTS:%=%.counts) 
	$(MAKE) -j $(MPARTS:%=%.tcounts) 
	$(MAKE) -j $(MPARTS:%=%.features) 
	$(MAKE) -j $(MPARTS:%=%.zest) 
	$(MAKE) $(MPARTS:%=%.weights) 

pw_PARTS=cdipuxle
pp_PARTS=cdiuwxle
px_PARTS=cdipuwle
pd_PARTS=cipuwxle
pZ_PARTS=cdiopquwxle
pc_PARTS=dipuwxle
pi_PARTS=cdpuwxle
pu_PARTS=cdipwzle
pl_PARTS=cdipwzue
pe_PARTS=cdipwzul
