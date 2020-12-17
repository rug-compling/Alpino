go:-
    repeat,
    read_line(Chars),
    (   Chars == end_of_file
    ->  !
    ;   (   catch(chars2pl(Chars),P,print_message(error,P))
        ->  true
        ;   format(user_error,"error: ~s~n",[Chars]),
            fail
        ),
        fail
    ).

%% ONLY FOR FIRST FIELD!
chars2pl(Chars) :-
    alpino_util:split_string(Chars," ",[F1|_]),
    atom_codes(T1,F1),
    write_if_accented(T1).


write_if_accented(Word0) :-
    (   atom(Word0),
        atom_codes(Word0,Chars0),
        contains_accent(Chars0),
        alpino_latin1:deaccent_chars(Chars0,Chars),
        atom_codes(Word,Chars),
        format("~q.~n",[accent(Word,Word0)]),
        fail
    ;   true
    ).

contains_accent([]) :-
    false.
contains_accent([In0|_]) :-
    alpino_latin1:isaccented(In0), !.
contains_accent([_|Ins]) :-
    contains_accent(Ins).

:- go.
