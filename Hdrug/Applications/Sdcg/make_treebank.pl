%% treebank(Tree,Freq)

:- compile(start).

:- hdrug_util:initialize_flag(freq,0).

result_hook(parse,_,o(parse(T,_),_,_),_) :-
    hdrug_util:hdrug_flag(freq,Freq),
    format("treebank(~d,~q).~n",[Freq,T]).

:- ( tsentence(Key,_,Freq),
       hdrug_util:set_flag(freq,Freq),
       parser_comparison(Key),
       fail
   ; halt
   ).
