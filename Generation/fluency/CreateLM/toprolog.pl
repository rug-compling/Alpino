chars2pl(Chars) :-
    split_string(Chars,"	",Fields),
    evaluate(Fields).

evaluate([L,W,I]) :-
    atom_codes(Lemma,L),
    atom_codes(Word,W),
    number_codes(Int,I),
    format("~q.~n",[lemma_word_freq(Lemma,Word,Int)]).

go :-
    repeat,
    read_line(Chars),
    (   Chars == end_of_file
    ->  true
    ;	(   chars2pl(Chars)
	->  true
	;   format(user_error,"error: ~s~n",[Chars]),
	    fail
	),
	fail
    ).

:- go.
