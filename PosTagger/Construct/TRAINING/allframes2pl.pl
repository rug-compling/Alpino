frames2pl :-
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

chars2pl(Chars) :-
    split_string(Chars,"	",Fields),
    (  evaluate(Fields),
	fail
    ;   true
    ).

evaluate([WordStr,TagStr,RootStr]) :-
    atom_codes(Word,WordStr),
    atom_codes(Root,RootStr),
    lists:append(TagStr," .",TagStr1),
    charsio:read_from_chars(TagStr1,Tag),
    hdrug_util:prettyvars(Tag),
    format("frame(~q,~q,~q).~n",[Word,Tag,Root]).


:- frames2pl.
