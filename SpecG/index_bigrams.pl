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
    format("~w~n",[Index]).

