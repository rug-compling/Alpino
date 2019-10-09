:- module(parser,[]).

parse(o(Obj,Str,_)) :-
	parse0(Obj,Str,[]).

parse0(Obj) -->
	[W],
	{ user:lexicon(W,Obj) }.
parse0(Obj) -->
	{ user:rule(Obj,Ds) },
	parse_ds(Ds).

parse_ds([]) --> [].
parse_ds([H|T]) -->
	parse0(H),
	parse_ds(T).

