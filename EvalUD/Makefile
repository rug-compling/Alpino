CONLLU=$(ALPINO_HOME)/CONLLU
SUITES=cdb cgn_exs eans extra g_suite h_suite i_suite leuven_yellow_pages lot_test_suite1 novelsample_corrected ntv-suite qa wpspel
SUITESDIR=../SENTS

## defaults per suite for test1 target
cdb_test1_options=-flag syntax_features cdb/pc_weights
extra_test1_options=-flag syntax_features cdb/px_weights
eans_test1_options=-flag parse_unannotated_only on
i_suite_test1_options=-flag parse_unannotated_only on

include $(ALPINO_HOME)/Makefile.EvalUD

