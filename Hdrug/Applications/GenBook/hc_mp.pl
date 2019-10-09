:- module(hc_mp,[]).


:- use_module(database).

count :-
	c_memo(A,B,C),
	write(A),
	nl,
	write(B),
	nl,
	write(C),
	nl.

count(B) :-
	hdrug_util:count_edges(hc_mp:'MEMO_ITEM'(_,_,_,_,_,_,_,_),B).

c_memo('MEMO'=A,'MEMO_ITEM'=B,tt=C) :-
	hdrug_util:count_edges(hc_mp:'MEMO'(_,_,_,_,_),A),
	hdrug_util:count_edges(hc_mp:'MEMO_ITEM'(_,_,_,_,_,_,_,_),B),
	hdrug_util:count_edges(hc_mp:tt(_,_,_,_),C).

list :-
	listing('MEMO'),
	listing('MEMO_ITEM'),
	listing(tt).

clean :-
	retractall('MEMO'(_,_,_,_,_)),
	retractall('MEMO_ITEM'(_,_,_,_,_,_,_,_)),
	clean_up_database(hc_mp:tt(_,_,_,_)).

parse(o(Cat,String,_)) :-
	length(String,L),
	( parse(Cat,0,L,0,L),
	  fail
        ; true),
	write('recover parse trees'), nl,
	packing:apply_semantics(Cat,0,L,hc_mp).

parse(Cat,P0,P,E0,E) :-
	finished_memo(E0,E,P0,P,Cat),
	!,
	'MEMO_ITEM'(E0,E,P0,P,Cat,_,_,_).

parse(Cat,P0,P,E0,E) :-
	predict(Cat,P0,P,E0,E,Small,QL,QR,Loc),
	hc(Small,QL,QR,Cat,P0,P,E0,E,Loc),
	add_item(E0,E,P0,P,Cat),
	fail.

parse(Cat,P0,P,E0,E):-
	assertz('MEMO'(E0,E,P0,P,Cat)),
	'MEMO_ITEM'(E0,E,P0,P,Cat,_,_,_).

predict(Cat,P0,P,E0,E,Small,QL,QR,[]) :-
	user:hfc(Small,Cat,P0,P,QL,QR),
	user:ign_h_link(x(Small,QL,QR),x(Cat,P0,P)),
	user:ign_lex(QL,QR,Small,_Name),
	user:check_hfc(Small,Cat,P0,P,QL,QR),
	hc:smaller(E0,QL),
	hc:smaller(QR,E).

predict(Cat,P0,P,_E0,_E,Small,Q,Q,[tt(Q,Q,Name,[])]) :-
	user:hfc(Small,Cat,P0,P,Q,Q),
	user:ign_h_link(x(Small,Q,Q),x(Cat,P0,P)),
	user:ign_h_gap(Small,Name),
	user:check_hfc(Small,Cat,P0,P,Q,Q).

assert_tts([]).
assert_tts([H|T]):-
	assertz_most_general(hc_mp:H,_),
	assert_tts(T).

hc(X,Y,Z,X,Y,Z,_,_,List) :-
	assert_tts(List).
hc(Small,Q0,Q,Goal,P0,P,E0,E,Tail) :-
	user:ign_h_rule(Small,Mid,Lefties,Righties,Name),
	user:check_hfc(Mid,Goal,P0,P,QL,QR),
	parse_l(Lefties,QL,Q0,E0,E,[t(Q0,Q,Small)|Rt],TT),
	parse_r(Righties,Q,QR,E0,E,Rt),
	\+ \+	user:ign_h_link(x(Mid,QL,QR), x(Goal,P0,P)),
	hc(Mid,QL,QR,Goal,P0,P,E0,E,[tt(QL,QR,Name,TT)|Tail]).

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

parse_l2([],L,L,_,_,H,H).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% memo items administration %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_item(E0,E,P0,P,Cat):-
	copy_term(f(P0,P,Cat),f(PP0,PP,Ccat)),
	numbervars(f(PP0,PP,Ccat),0,_),
	add_item(E0,E,P0,P,Cat,PP0,PP,Ccat).

add_item(E0,E,_,_,_,P0,P,Cat) :-
	'MEMO_ITEM'(E0,E,_,_,_,P0,P,Cat),!,fail.

add_item(E0,E,P0,P,Cat,PP0,PP,Ccat):-
	retractall('MEMO_ITEM'(E0,E,_,_,_,P0,P,Cat)),
	assertz('MEMO_ITEM'(E0,E,P0,P,Cat,PP0,PP,Ccat)).

finished_memo(E0,E,P0,P,Cat) :-
	copy_term(f(P0,P,Cat),f(PP0,PP,Ccat)),
	numbervars(f(PP0,PP,Ccat),0,_),  % a more general goal has already been tried
	'MEMO'(E0,E,PP0,PP,Ccat).

pack_rule(Name,Mother,Ds) :-
	user:rule(Mother,Ds,Name).








