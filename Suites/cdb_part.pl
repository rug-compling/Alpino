:- module(alpino_suite, [ sentence/2 ]).

:- use_module(train_test).

:- hdrug_util:initialize_flag(cdb_part,0).

sentence(A,B) :-
    hdrug_util:hdrug_flag(cdb_part,Part),
    tt_suite:sentence(Part,A,B).

