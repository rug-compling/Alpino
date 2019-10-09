:- expects_dialect(sicstus).

:- (   current_prolog_flag(language, sicstus)
   ->  compile('$ALPINO_HOME)/Hdrug/Prolog/conditional_compilation')
   ;   true
   ).


swi     :- current_prolog_flag(dialect, swi).
sicstus :- current_prolog_flag(language, sicstus).

:- use_module('../pro_fadd').

test(Word):-
    pro_fadd:init_morph('lex.fsa',0,0,0,0,Dict),
    format(user_error,"initialized lex.fsa: ~w~n",[Dict]),
    test(Word,Dict).

test(Word,Dict) :-
    pro_fadd:morph_word(Word,Dict,_,Result),
    format(user_error,"~w --> ~w~n",[Word,Result]).
