OPTIONS=
DEBUG=
#DEBUG=-d
TRAIN=ned.train
TEST=ned.testa

all: $(TRAIN).features

new:
	rm -f *.features *.model *.c45

install: $(TRAIN).features
	cp $(TRAIN).features ../features.t

# $(TRAIN): conll/$(TRAIN) conll2sents select_not_dict morelex
# 	./conll2sents conll/$(TRAIN) | ./select_not_dict >$(TRAIN)

# $(TEST): conll/$(TEST) conll2sents 
# 	./conll2sents conll/$(TEST)  >$(TEST)

#SENTS2C45=../sents2c45
SENTS2C45= Alpino -notk -u batch_command=alpino_classify_named_entity:sents2c45

%.c45: % 
	$(SENTS2C45) < $< >$@

conll: $(TRAIN).model

%.model: %.c45
	./classify --model=$@\
                   --train=$< --prior=315000 $(OPTIONS)

%.features:%.model
	./classify --model=$< -y > $@

%.correct : %
	awk -F\| '{ print $$1 }' $* > $*.correct

%.input : %
	awk -F\| '{ print $$2 }' $* > $*.input

%.with_dict: %.maxent %.input ../Build/geo ../Build/per ../Build/misc ../Build/org
	if [ -f $*.with_dict ]; then cp $*.with_dict $*.with_dict.orig; fi
	cat $*.input | ./combine $*.maxent > $*.with_dict

compare: $(TEST).input
	paste -d\| $(TEST).input $(TEST).with_dict $(TEST).with_dict.orig $(TEST) | awk -F\| '{ if ($$2 != $$3) print $$0}'

%.maxent: $(TRAIN).model %.c45
	./classify --model=$(TRAIN).model --test=$*.c45\
                   -b -w > $*.maxent

test: $(TEST).correct $(TEST).with_dict $(TEST).input
	echo $(TEST)
	paste $(TEST).correct $(TEST).with_dict $(TEST).input\
      | ./eval $(DEBUG)

SHELL := /bin/bash
simple: $(TEST).correct $(TRAIN).model $(TEST).input $(TEST).c45
	paste $(TEST).correct <( ./classify --model=$(TRAIN).model --test=$(TEST).c45 -b -w ) $(TEST).input\
      | ./simple-eval $(DEBUG)

errors:
	$(MAKE) test DEBUG=-d | grep wrong | sort | uniq -c | sort -nr

testme: $(TRAIN).model $(TEST).c45
	./classify --model=$(TRAIN).model --test $(TEST).c45

clean:
	rm -f *.assigned *.maxent *.with_dict *.model *.c45 *.features\
              *.correct *.input

realclean: clean

%.iob : %.with_dict %.input
	@paste -d '\|' $*.input $*.with_dict | ./to_iob > $*.iob

# do it twice, first ensure all files are there,
# and then again with results to log
tests:
	@$(MAKE) -s test
	@$(MAKE) -s test TEST=ned.testb
	@echo >> log
	@echo >> log
	@echo >> log
	@date >> log
	@$(MAKE) -s test >> log
	@$(MAKE) -s test TEST=ned.testb >> log

sonar1.train:
	python3 sonar2train.py < SoNaR_corpus_formatted/SoNaR1_training.txt > sonar1.train

sonar1.test:
	python3 sonar2train.py < SoNaR_corpus_formatted/SoNaR1_test.txt > sonar1.test
