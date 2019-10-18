:- expects_dialect(sicstus).

:- use_module('utils').
:- use_module(library(terms), [ term_hash/2 ]).

fields_to_term([Word|Tail],human(W,Gender)):-
    atom_codes(W,Word),
    gender(Tail,Gender).

gender([],'').
gender([GenderString],Gender) :-
    atom_codes(Gender,GenderString).

%% lines starting with # are comments
chars_to_term([35|_]) :- !.
chars_to_term(Chars) :-
    split_string(Chars,"	",Fields),
    fields_to_term(Fields,Term),
    format("~q.~n",[Term]).

print_header :-
    format(":- module(gadata, [ human/2 ]).~n~n",[]).

gadata2pl :-
    print_header,
    repeat,
    read_line(Chars),
    (   Chars == end_of_file
    ->  true
    ;	(   chars_to_term(Chars)
	->  true
	;   format(user_error,"error: ~s~n",[Chars]),
	    fail
	),
	fail
    ).

:- gadata2pl, halt.

