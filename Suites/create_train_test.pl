%% Divide the annotated part of the cdb corpus into parts for cross-validation

:- use_module(library(random)).
:- use_module(cdb).

:- hdrug_util:initialize_flag(folds,10).

split([]).
split([sentence(A,B)|Rest]) :-
    hdrug_util:hdrug_flag(folds,Folds),
    random(0,Folds,R),
    format("~q.~n",[sentence(R,A,B)]),
    split(Rest).

%% (should we maybe re-seed the random number generator?)

:- findall(sentence(A,B),sentence(A,B),List),
    format(":- module(tt_suite, [ sentence/3 ]).~n~n",[]),
    split(List),
    halt.
