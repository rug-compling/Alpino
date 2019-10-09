include ../../Makefile.include

default: po

data:
	fsa pred_module=fsa_frozen -aux gen -r s |\
	fsa nr_sol_max=$(n) -sample | ./make_suite $(mode) $(start) >>$(file) 

suite.pl: gen.pl
	$(MAKE) data n=200 start=1 mode=train file=suite.pl
	$(MAKE) data n=50 start=201 mode=test file=suite.pl

treebank.pl: suite.pl
	../../hdrug -notk -l make_treebank  >treebank.pl

POS=treebank.po parser.po pretty.po probs.po suite.po

po:
	make $(POS)

clean:

realclean: clean

