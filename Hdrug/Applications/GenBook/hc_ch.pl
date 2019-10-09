:- module(hc_ch,[]).

clean :-
	retractall(rhc(_,_,_,_,_,_)),
	retractall(lhc(_,_,_,_,_,_)),
	database:clean_up_database(hc_ch:lgoal(_,_,_)),
	database:clean_up_database(hc_ch:rgoal(_,_,_)),
	database:clean_up_database(hc_ch:cyk(_,_,_)),	
	database:clean_up_database(hc_ch:tt(_,_,_,_)).

count :-
	hdrug_util:count_edges(hc_ch:rhc(_,_,_,_,_,_),LHc),
	hdrug_util:count_edges(hc_ch:lhc(_,_,_,_,_,_),RHc),
	hdrug_util:count_edges(hc_ch:lgoal(_,_,_),Goall),
	hdrug_util:count_edges(hc_ch:rgoal(_,_,_),Goalr),
	hdrug_util:count_edges(hc_ch:cyk(_,_,_),Cyk),	
	hdrug_util:count_edges(hc_ch:tt(_,_,_,_),TT),
	format("hc: ~w~n goal~w~n cyk: ~w~n tt: ~w~n",
               [LHc+RHc,Goall+Goalr,Cyk,TT]).

count(I):-
	hdrug_util:count_edges(hc_ch:lhc(_,_,_,_,_,_),LHc),
	hdrug_util:count_edges(hc_ch:rhc(_,_,_,_,_,_),RHc),
	hdrug_util:count_edges(hc_ch:lgoal(_,_,_),Goall),
	hdrug_util:count_edges(hc_ch:rgoal(_,_,_),Goalr),
	hdrug_util:count_edges(hc_ch:cyk(_,_,_),Cyk),	
	I is LHc + RHc + Goall + Goalr + Cyk.

parse(o(Cat,String,_)) :-
	length(String,L),
	( store(lgoal(Cat,0,L))
	; %% write('recover parse trees'), nl,
	  packing:apply_semantics(Cat,0,L,hc_ch)
        ).

predict(Cat,P0,P,E0,E,Small,QL,QR):-
	user:hfc(Small,Cat,P0,P,QL,QR),
	user:ign_h_link(x(Small,QL,QR),x(Cat,P0,P)),
	user:ign_lex(QL,QR,Small,_Name),
	user:check_hfc(Small,Cat,P0,P,QL,QR),
	hc:smaller(E0,QL),
	hc:smaller(QR,E).

predict(Cat,P0,P,E0,E,Small,Q,Q) :-
	user:hfc(Small,Cat,P0,P,Q,Q),
	user:ign_h_link(x(Small,Q,Q),x(Cat,P0,P)),
	user:ign_h_gap(Small,Name),
	user:check_hfc(Small,Cat,P0,P,Q,Q),
%	larger(E0,Q,E),
	hdrug_util:between(E0,E,Q),
	assertz_most_general(tt,Q,Q,Name,[]).

store(lgoal(Cat,P0,E)):-
	assertz_most_general(lgoal,Cat,P0,E),
	closure(lgoal(Cat,P0,E)).

store(rgoal(Cat,P0,E)):-
	assertz_most_general(rgoal,Cat,P0,E),
	closure(rgoal(Cat,P0,E)).

store(lhc(Cat,P0,P,SCat,SP0,E)):-
	assertz_most_general(lhc,Cat,P0,P,SCat,SP0,E),
	closure(lhc(Cat,P0,P,SCat,SP0,E)).

store(rhc(Cat,P0,P,SCat,SP0,E)):-
	assertz_most_general(rhc,Cat,P0,P,SCat,SP0,E),
	closure(rhc(Cat,P0,P,SCat,SP0,E)).

store(cyk(Cat,P0,P)):-
	assertz_most_general(cyk,Cat,P0,P).

closure(lgoal(Cat,P0,E)):-
	predict(Cat,P0,_,P0,E,Small,QL,QR),
	store(lhc(Small,QL,QR,Cat,P0,E)),
	fail.

closure(rgoal(Cat,P,E0)):-
	predict(Cat,_,P,E0,P,Small,QL,QR),
	store(rhc(Small,QL,QR,Cat,P,E0)),
	fail.

closure(lhc(Small,Q0,Q,Goal,P0,E)):-
	head_corner(Small,Q0,Q,Goal,P0,_,P0,E,Mid,QL,QR),
	store(lhc(Mid,QL,QR,Goal,P0,E)),
	fail.

closure(rhc(Small,Q0,Q,Goal,P,E0)):-
	head_corner(Small,Q0,Q,Goal,_,P,E0,P,Mid,QL,QR),
	store(rhc(Mid,QL,QR,Goal,P,E0)),
	fail.

closure(lhc(Small,Q0,Q,Small,Q0,E)):-
	hc:smaller(Q,E),
	store(cyk(Small,Q0,Q)).

closure(rhc(Small,Q0,Q,Small,Q,E0)):-
	hc:smaller(E0,Q0),
	store(cyk(Small,Q0,Q)).

head_corner(Small,Q0,Q,Goal,P0,P,E0,E,Mid,QL,QR) :-
	user:ign_h_rule(Small,Mid,Lefties,Righties,Name),
	user:check_hfc(Mid,Goal,P0,P,QL,QR),
	parse_l(Lefties,QL,Q0,E0,E,[t(Q0,Q,Small)|Rt],TT),
	parse_r(Righties,Q,QR,E0,E,Rt),
	\+ \+	user:ign_h_link(x(Mid,QL,QR), x(Goal,P0,P)),
	assertz_most_general(tt,QL,QR,Name,TT).

% parse_l(+Ds,?P0,?P,+E0,+E)
% parse a reversed list of daughters from position P0 to P
% where E0 is smaller than P0 and E larger than P (E0-E are
% the extremes within which the interval P0-P should be 
% found). 

% Usually P is instantiated (because we parse from a given head
% to the left), in that case we should use P as
% the extreme right position instead. (P is not instantiated in case
% the head was a gap, P-P).

parse_l(Ds,L0,L,E0,E,His0,His):-
	(  var(L)
        -> parse_l2(Ds,L0,L,E0,E,His0,His)
        ;  parse_l2(Ds,L0,L,E0,L,His0,His)
        ).

parse_l2([],L,L,_,_,H,H).
parse_l2([H|T],L0,L,E0,E,His0,His):-
	( store(rgoal(H,L,E0))
	; cyk(L1,L,H),
          hc:smaller(E0,L1),
	  parse_l(T,L0,L1,E0,E,[t(L1,L,H)|His0],His)
        ).

parse_r(Ds,L0,L,E0,E,His):-
	(  var(L0)
        -> parse_r2(Ds,L0,L,E0,E,His)
        ;  parse_r2(Ds,L0,L,L0,E,His)
        ).

parse_r2([],L,L,_,_,[]).
parse_r2([H|T],L0,L,E0,E,[t(L0,L1,H)|Tt]):-
	( store(lgoal(H,L0,E))
        ; cyk(L0,L1,H),
          hc:smaller(L1,E),
	  parse_r(T,L1,L,E0,E,Tt)
        ).

pack_rule(Name,Mother,Ds) :-
	user:rule(Mother,Ds,Name).

assertz_most_general(tt,P0,P,Name,Ds):-
	database:assertz_most_general(hc_ch:tt(P0,P,Name,Ds),_,delete).

assertz_most_general(lgoal,Cat,P0,P):-
	database:assertz_most_general(hc_ch:lgoal(P0,P,Cat),no,nodelete).

assertz_most_general(rgoal,Cat,P0,P):-
	database:assertz_most_general(hc_ch:rgoal(P0,P,Cat),no,nodelete).

assertz_most_general(cyk,Cat,P0,P):-
	database:assertz_most_general(hc_ch:cyk(P0,P,Cat),no,delete).

assertz_most_general(lhc,Cat,P0,P,Top,Q0,E) :-
	copy_term(t(Cat,Top,P0,P,Q0),t(CCat,CTop,CP0,CP,Q0C)),
	numbervars(t(CCat,CTop,CP0,CP,Q0C),0,_),
	lhc(CP0,CP,CCat,CTop,Q0C,E1),
	check_larger(E1,E),
	!,
	fail.
assertz_most_general(lhc,Cat,P0,P,Top,Q0,E):-
	assertz(lhc(P0,P,Cat,Top,Q0,E)).

assertz_most_general(rhc,Cat,P0,P,Top,Q0,E) :-
	copy_term(t(Cat,Top,P0,P,Q0),t(CCat,CTop,CP0,CP,CQ0)),
	numbervars(t(CCat,CTop,CP0,CP,CQ0),0,_),
	rhc(CP0,CP,CCat,CTop,CQ0,E1),
	check_smaller(E1,E),
	!,
	fail.
assertz_most_general(rhc,Cat,P0,P,Top,Q0,E):-
	assertz(rhc(P0,P,Cat,Top,Q0,E)).


check_larger(E1,_):-
	var(E1),!.
check_larger(_,E):-
	var(E),
	!,
	fail.
check_larger(E1,E):-
	E1 >= E.

check_smaller(E1,_):-
	var(E1),!.
check_smaller(_,E):-
	var(E),
	!,
	fail.
check_smaller(E1,E):-
	E1 =< E.

