:- expects_dialect(sicstus).

:- use_module('../../src/utils').
:- use_module(library(terms), [ term_hash/2 ]).

fields_to_term(["CATEGORY",
		CatCodes,
		ScoreCodes], category(Cat,Score)) :-
    atom_codes(Cat,CatCodes),
    number_codes(Score,ScoreCodes).

fields_to_term(["FEATURE",
		NCodes,
		ValueCodes,
		CatCodes,
		ScoreCodes], feature(Hash,N,Value,Cat,Score)) :-
    number_codes(N,NCodes), integer(N),
    atom_codes(Value,ValueCodes),
    atom_codes(Cat,CatCodes),
    number_codes(Score,ScoreCodes),
    term_hash(f(N,Value,Cat),Hash).

chars_to_term(Chars) :-
    split_string(Chars,"|",Fields),
    fields_to_term(Fields,Term),
    format("~q.~n",[Term]).

print_header :-
    format(":- module(alpino_named_entity_features, [ ]).~n~n",[]),
    format(":- use_module(library(terms), [ term_hash/2 ]).~n~n",[]),
    format("feature(A,B,C,W) :-~n",[]),
    format("    term_hash(f(A,B,C),Ix), feature(Ix,A,B,C,W).~n~n",[]).

features2pl :-
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

% :- features2pl, halt.

