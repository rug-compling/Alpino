chars2pl(end_of_file,_,ERR) :-
    !,
    format(user_error,
	   "~n~nerror: no output!~n",[]),
    hdrug_cmdint:copy_stream(ERR),
    raise_exception(alpino_error(ssf_error)).

chars2pl(Chars) :-
    split_string(Chars,"|",Fields),
    (  evaluate(Fields),
	fail
    ;   true
    ).

ssf(with_dt(Tag0,_),Tag) :-
    !,
    Tag0=Tag.
ssf(Tag,Tag).

evaluate([TagStr]) :-
    lists:append(TagStr," .",TagStr1),
    charsio:read_from_chars(TagStr1,Tag),
    (   %%alpino_penalties:z_f2_simplify_frame(Tag,SimpleTag),
	alpino_dt:somewhat_simplify_frame(Tag,SimpleTag),
	hdrug_util:prettyvars(SimpleTag),
	format("~q~n",[SimpleTag])
    ->  true
    ;   format(user_error,"cannot map tag ~w~n",[Tag]),
	fail
    ).

:-
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

