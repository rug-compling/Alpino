default:
	echo "( cd Data ; make -j ; make install )"
	echo "make -j all"

MODEL_NUMBER_ANALYSES=100
ESTIMATE=tinyest
#VARIANCES=--l1 0.0001 --grafting 1
VARIANCES=$(shell cat variances)
MIN_FEATURE_FREQ=$(shell cat min_feature_freq)
filter_features_flags=

gzs= px.gz pc.gz pp.gz pd.gz pw.gz pZ.gz pi.gz pu.gz pl.gz pe.gz pq.gz po.gz
sgzs= $(gzs:%.gz=%.sgz)
tcounts= $(gzs:%.gz=%.tcounts)
zests=$(gzs:%.gz=%.zest)
counts=$(gzs:%.gz=%.counts)
weights=$(gzs:%.gz=%.weights)
features=$(gzs:%.gz=%.features)
tests=$(filter-out pZ.test, $(gzs:%.gz=%.test) )

pZ.gz:
	echo -n | gzip > pZ.gz

sgzs: $(sgzs)

counts: $(counts)

tcounts: $(tcounts)

zests: $(zests)

features: $(features)

weights: $(weights)

tests: $(tests)
	date >> log
	grep "Average GTM score" *.test >> log


## this actually builds 6 models, and you can check
## the results of 5-fold cross validation
all: weights tests

%.sgz: %.gz
	zcat $*.gz | $(ALPINO_HOME)/Disambiguation/extract_subset -f -n $(MODEL_NUMBER_ANALYSES) | gzip > $*.sgz

%.tcounts: $(filter-out $*.counts, $(counts))
	cat $(filter-out $*.counts, $(counts)) | $(ALPINO_HOME)/Disambiguation/count_features -a > $@

%.counts: %.sgz ignore_features
	zcat $*.sgz\
        | $(ALPINO_HOME)/Disambiguation/count_features -f \
        | grep -v -f ignore_features \
        | python3 filter_features.py $(filter_features_flags) > $*.counts

%.features: %.tcounts min_feature_freq
	awk -F\| 'BEGIN { N=0 }\
                { if ( $$4 > $(MIN_FEATURE_FREQ) )\
                {  print N "|" $$1; N++} }' \
        $*.tcounts >$*.features

%.zest: $(filter-out $*.sgz, $(sgzs)) %.features 
	zcat $(filter-out $*.sgz, $(sgzs)) | \
        $(ALPINO_HOME)/Disambiguation/prepare_estimate -y -f $*.features | \
        $(ALPINO_HOME)/Disambiguation/score0-1.py | gzip > $@

%.iweights: %.zest variances
	zcat $< | $(ESTIMATE) $(VARIANCES) > $@

%.weights: %.iweights %.features
	paste -d\| $*.features $< | awk -F\| '{ if ($$3 && ($$3>0||$$3<0)) printf("%s|%s\n",$$2,$$3) }' >$@

%.test: %.weights
	@echo "--------------------------------------"           > $*.test
	@echo "number of features: `wc -l $*.weights`"           > $*.test
	@echo "min_feature_freq: $(MIN_FEATURE_FREQ)"            > $*.test
	@echo "variances:        $(VARIANCES)"                   > $*.test
	@zcat $*.gz | python3 evaluate.py -f $*.weights           > $*.test

N=1
overview:
	grep "Average GTM score" *.test

install:
	cp pZ.weights ../maxent_feature_weights.t
	cp pc.weights ../maxent_feature_weights_c.t
	cp px.weights ../maxent_feature_weights_x.t
	cp pp.weights ../maxent_feature_weights_p.t
	cp pd.weights ../maxent_feature_weights_d.t
	cp pw.weights ../maxent_feature_weights_w.t
	cp pi.weights ../maxent_feature_weights_i.t
	cp pu.weights ../maxent_feature_weights_u.t
	cp pe.weights ../maxent_feature_weights_e.t
	cp pl.weights ../maxent_feature_weights_l.t
	cp pq.weights ../maxent_feature_weights_q.t
