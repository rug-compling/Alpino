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

chars2pl(end_of_file,_,ERR) :-
    !,
    format(user_error,
	   "~n~nerror: no output!~n",[]),
    hdrug_cmdint:copy_stream(ERR),
    raise_exception(alpino_error(frames2pl_error)).

chars2pl(Chars) :-
    split_string(Chars,"|",Fields),
    (  evaluate(Fields),
	fail
    ;   true
    ).

evaluate([WordStr,TagStr,LogStr]) :-
    atom_codes(Word,WordStr),
    lists:append(TagStr," .",TagStr1),
    charsio:read_from_chars(TagStr1,Tag),
    hdrug_util:prettyvars(Tag),
    number_codes(Log,LogStr),
    format("z_f2(~q,~q,~5f).~n",[Word,Tag,Log]).

:- frames2pl.
