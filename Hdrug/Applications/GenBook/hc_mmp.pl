:- module(hc_mmp,[]).

:- use_module(database).

clean :-
	retractall(done_goal(_,_,_,_,_)),
	retractall(done_hc(_,_,_,_,_,_,_,_)),
	clean_up_database(hc_mmp:item(_,_,_)),
	clean_up_database(hc_mmp:tt(_,_,_,_)).

count(G,H,I,T):-
	hdrug_util:count_edges(hc_mmp:done_goal(_,_,_,_,_),G),
	hdrug_util:count_edges(hc_mmp:done_hc(_,_,_,_,_,_,_,_),H),
	hdrug_util:count_edges(hc_mmp:item(_,_,_),I),
	hdrug_util:count_edges(hc_mmp:tt(_,_,_,_),T).

count :-
	count(G,H,I,T),
	write(count(G,H,I,T)),nl.

count(To):-
	count(A,B,C,D),
	To is A + B + C + D.

parse(o(Cat,String,_)) :-
	length(String,L),
	goal(Cat,0,L,0,L),
	write('recover parse trees'), nl,
	packing:apply_semantics(Cat,0,L,hc_mmp).

parse(Cat,P0,P,E0,E):-
	goal(Cat,P0,P,E0,E),
	item(P0,P,Cat),
	hc:smaller(E0,P0),
	hc:smaller(P,E).

goal(Cat,P0,P,E0,E) :-
	copy_term(done_goal(P0,P,Cat),done_goal(P0c,Pc,Catc)),
	numbervars(done_goal(P0c,Pc,Catc),0,_),
	done_goal(P0c,Pc,G0,G,Catc),
	larger_interval(G0,G,E0,E),
	!.

goal(Cat,P0,P,E0,E) :-
	( assertz(done_goal(P0,P,E0,E,Cat)),
	  predict(Cat,P0,P,E0,E,Small,QL,QR),
	  hc(Small,QL,QR,Cat,P0,P,E0,E),
	  fail
        ; true 
        ).

predict(Cat,P0,P,E0,E,Small,QL,QR) :-
	user:hfc(Small,Cat,P0,P,QL,QR),
	user:ign_h_link(x(Small,QL,QR),x(Cat,P0,P)),
	user:ign_lex(QL,QR,Small,_Name),
	user:check_hfc(Small,Cat,P0,P,QL,QR),
	hc:smaller(E0,QL),
	hc:smaller(QR,E).

predict(Cat,P0,P,_E0,_E,Small,Q,Q) :-
	user:hfc(Small,Cat,P0,P,Q,Q),
	user:ign_h_link(x(Small,Q,Q),x(Cat,P0,P)),
	user:ign_h_gap(Small,Name),
	user:check_hfc(Small,Cat,P0,P,Q,Q),
        assertz_most_general(hc_mmp:tt(Q,Q,Name,[]),_).

hc(Small,P0,P,Goal,Q0,Q,E0,E) :-
	copy_term(done_hc(P0,P,Q0,Q,Small,Goal),
	          done_hc(P0c,Pc,Q0c,Qc,Smallc,Goalc)),
	numbervars(done_hc(P0c,Pc,Q0c,Qc,Smallc,Goalc),0,_),
	done_hc(P0c,Pc,Q0c,Qc,G0,G,Smallc,Goalc),
	larger_interval(G0,G,E0,E),
	!,
	fail.

hc(X,Y,Z,X,Y,Z,_,_) :-
	assertz_most_general(hc_mmp:item(Y,Z,X),_),
	fail.

hc(Small,Q0,Q,Goal,P0,P,E0,E) :-
	assertz(done_hc(Q0,Q,P0,P,E0,E,Small,Goal)),
	user:ign_h_rule(Small,Mid,Lefties,Righties,Name),
	user:check_hfc(Mid,Goal,P0,P,QL,QR),
	parse_l(Lefties,QL,Q0,E0,E,[t(Q0,Q,Small)|Rt],TT),
	parse_r(Righties,Q,QR,E0,E,Rt),
	\+ \+	user:ign_h_link(x(Mid,QL,QR), x(Goal,P0,P)),
	assertz_most_general(hc_mmp:tt(QL,QR,Name,TT),_),
	hc(Mid,QL,QR,Goal,P0,P,E0,E).

% parse_l(+Ds,?P0,?P,+E0,+E)
% parse a reversed list of daughters from position P0 to P
% where E0 is smaller than P0 and E larger than P (E0-E are
% the extremes within which the interval P0-P should be 
% found). 

% Usually P is instantiated (because we parse from a given head
% to the left), in that case we should use P as
% the extreme right position instead. (P is not instantiated in case
% the head was a gap, P-P).
parse_l(Ds,P0,P,E0,E,His0,His):-
	(  nonvar(P)
	-> parse_l2(Ds,P0,P,E0,P,His0,His)
        ;  parse_l2(Ds,P0,P,E0,E,His0,His)
        ).

parse_l2([],L,L,_,_,His,His).
parse_l2([H|T],L0,L,E0,E,His0,His):-
	parse(H,L1,L,E0,E),
	parse_l(T,L0,L1,E0,E,[t(L1,L,H)|His0],His).

parse_r(Ds,P0,P,E0,E,T):-
	(  nonvar(P0)
	-> parse_r2(Ds,P0,P,P0,E,T)
        ;  parse_r2(Ds,P0,P,E0,E,T)
        ).

parse_r2([],L,L,_,_,[]).
parse_r2([H|T],L0,L,E0,E,[t(L0,L1,H)|Tt]):-
	parse(H,L0,L1,E0,E),
	parse_r(T,L1,L,E0,E,Tt).

pack_rule(Name,Mother,Ds) :-
	user:rule(Mother,Ds,Name).


larger_interval(G0,G,E0,E) :-
	hc:smaller(G0,E0),
	hc:smaller(E,G).

