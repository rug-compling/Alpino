:- module(hc_xoldt,[]).

:- use_module([ '$HOME/Prolog/xoldt/xoldt' ]).

parse(o(Cat,String,_)) :-
	length(String,L),
	parse(Cat,0,L,0,L).

:- table hc_xoldt:parse/5.

:- notable hc_xoldt:hc/8, hc_xoldt:parse_l/5, hc_xoldt:parse_r/5, 
           hc_xoldt:parse_l2/5, hc_xoldt:parse_r2/5.

parse(Cat,P0,P,E0,E) :-
	predict(Cat,P0,P,E0,E,Small,QL,QR),
	hc(Small,QL,QR,Cat,P0,P,E0,E).

predict(Cat,P0,P,E0,E,Small,QL,QR) :-
	user:lex(QL,QR,Small,_Name),
	check_hlink(Small,QL,QR,Cat,P0,P),
	smaller(E0,QL),
	smaller(QR,E).

predict(Cat,P0,P,_E0,_E,Gap,Q,Q) :-
	user:h_gap(Gap,_Name),
	check_hlink(Gap,Q,Q,Cat,P0,P).


check_hlink(X,Q0,Q, Y,P0,P) :-
	\+ \+	user:h_link(x(X,Q0,Q), x(Y,P0,P)),
	user:hfc(X,Y,P0,P,Q0,Q),
	user:check_hfc(X,Y,P0,P,Q0,Q).

hc(X,Y,Z,X,Y,Z,_,_).
hc(Small,Q0,Q,Goal,P0,P,E0,E) :-
	user:h_rule(Small,Mid,Lefties,Righties,_Name),
	parse_l(Lefties,QL,Q0,E0,E),
	parse_r(Righties,Q,QR,E0,E),
	check_hlink(Mid,QL,QR,Goal,P0,P),
	hc(Mid,QL,QR,Goal,P0,P,E0,E).


parse_l(Ds,P0,P,E0,E):-
	(  nonvar(P)
	-> parse_l2(Ds,P0,P,E0,P)
        ;  parse_l2(Ds,P0,P,E0,E)
        ).

parse_l2([],L,L,_,_).
parse_l2([H|T],L0,L,E0,E):-
	parse(H,L1,L,E0,E),
	parse_l(T,L0,L1,E0,E).

parse_r(Ds,P0,P,E0,E):-
	(  nonvar(P0)
	-> parse_r2(Ds,P0,P,P0,E)
        ;  parse_r2(Ds,P0,P,E0,E)
        ).

parse_r2([],L,L,_,_).
parse_r2([H|T],L0,L,E0,E):-
	parse(H,L0,L1,E0,E),
	parse_r(T,L1,L,E0,E).


smaller(A,_):-
	var(A),!.
smaller(_,B):-
	var(B),!.
smaller(A,B) :-
	A =< B.






