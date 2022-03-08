index :-
    repeat,
    read(Term),
    (   Term == end_of_file
    ->  !
    ;	(   catch(index_term(Term),P,print_message(error,P))
	->  true
	;   format(user_error,"error: ~w~n",[Term]),
	    fail
	),
	fail
    ).

index_term(Term):-
    terms:term_hash(Term,Index),
    (  var(Index)
    -> format(user_error,"variable in derivation tree: ~w~n",[Term])
    ;  format("~w~n",[Index])
    ).

