:- use_module(library(terms), [ term_hash/2 ]).
:- use_module(library(charsio)).

print_header :-
    format(":- module(alpino_fluency_weights,[ feature_weight/2 ]).~n~n",[]),
    format("feature_weight(F,W) :-~n",[]),
    format("    terms:term_hash(F,Ix), feature_weight(Ix,F,W),!.~n~n",[]).

index(feature_weight(F,W)) :-
    W =\= 0,
    term_hash(F,Ix),
    format("feature_weight(~q,~q,~q).~n",[Ix,F,W]).

:-
    print_header,
    repeat,
    read_line(Chars),
    (   Chars == end_of_file
    ->  true
    ;	catch(charsio:read_from_chars(Chars,X),
	      E,
	      (	  print_message(error, E),
		  fail
	      )),
         index(X),
	 fail
    ),
    halt.


