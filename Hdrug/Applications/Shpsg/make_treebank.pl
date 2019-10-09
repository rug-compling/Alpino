%% treebank(Tree,Freq)

:- compile(start).

:- use_module(random, library(random), all).

:- hdrug_util:initialize_flag(key,0).

:- dynamic weight/2.

%:- multifile result_hook/4.

result_hook(parse,_,o(parse(T,_),_,Fs),_) :-
    score_treebank(Fs,Scores),
    lists:sum_list(Scores,S),
    S1 is exp(S) * 10000,
    hdrug_util:hdrug_flag(key,Key),
    format("treebank(~d,~d,~q).~n",[Key,S1,T]).

score_treebank([],[]).
score_treebank([F|Fs],[S|Ss]) :-
    lookup(F,S),
    score_treebank(Fs,Ss).

lookup(F,S) :-
    weight(F,S), !.

lookup(F,S) :-
      random(-1.0,1.0,S),
      assert(weight(F,S)).

:- ( sentence(Key,_,Sentence),
       hdrug_util:set_flag(key,Key),
       parse_compare(Sentence),
       fail
   ;
       halt
   ).

