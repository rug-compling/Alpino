:- module(generator,[]).

generate(o(Obj,Str,_)) :-
	generate0(Obj,Str,[]).

generate0(Obj,[Word|P],P) :-
	user:lexicon(Word,Obj).
generate0(Obj,P0,P) :-
	user:rule(Obj,Ds),
	generate_ds(Ds,P0,P).

generate_ds([],P,P).
generate_ds([H|T],P0,P) :-
	generate0(H,P0,P1),
	generate_ds(T,P1,P).

