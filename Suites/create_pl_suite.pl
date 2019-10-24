to_suite(Key) :-
    init_count(Count),
    read_line(Chars),
    (   Chars == end_of_file
    ->  !
    ;	(   ignore_prefix(Chars)
	->  true
	;   chars2line(Key,Chars,Count)
	->  true
	;   format(user_error,"error: ~s~n",[Chars]),
	    fail
	),
	fail
    ).

chars2line(Key,Chars,Count) :-
    key(Key,Chars,Count,Chars1,Id),
    alpino_util:codes_to_words(Chars1,Words),
    format("my_sentence(~q,~q).~n",[Id,Words]).

ignore_prefix([37|_]).
ignore_prefix([45,45|_]).

key(key,Chars,_,Chars1,Id) :-
    lists:append(IdChars,[124|Chars1],Chars),
    atom_codes(Id,IdChars).
key(nokey,Chars,Count,Chars,Count).

init_count(X) :-
    init_count(1,X).

init_count(X,X).
init_count(X,Y) :-
    Z is X+1,
    init_count(Z,Y).

    