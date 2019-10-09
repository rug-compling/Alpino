:- module(lc_ch,[]).

:- use_module(database).

:- use_module(lists, library(lists), [ append/3 ]).

count(No) :-
	hdrug_util:count_edges(lc_ch:inactive(_,_,_),Inactive),
	hdrug_util:count_edges(lc_ch:  active(_,_,_,_,_),Active),
	No is Active + Inactive.

count :-
	hdrug_util:report_count_edges(lc_ch:inactive(_,_,_)),
	hdrug_util:report_count_edges(lc_ch:  active(_,_,_,_,_)),
	hdrug_util:report_count_edges(lc_ch:      tt(_,_,_,_)),
	hdrug_util:hdrug_flag(ticks,Ticks),
	write(Ticks),   write('          ticks'),nl.

clean :-
	clean_up_database(lc_ch:inactive(_,_,_)),
	clean_up_database(lc_ch:active(_,_,_,_,_)),
	clean_up_database(lc_ch:tt(_,_,_,_)).

list :-
	listing(inactive),
	listing(active),
	listing(tt).

parse(o(Node,String,_)) :-
	hdrug_util:hdrug_flag(ticks,_,0),
	store([Node],'$$$',0,0,tree('$$$',[]),Edge),
	process([Edge]),
	left_right(String,0,Max),
	write('recover parse trees'), nl,
	packing:apply_semantics(Node,0,Max,lc_ch).

left_right([],Max,Max).   % finished

left_right([_|T],I1,Max) :-
	lexical_lookup(I1,Agenda),   % scan next one
	process(Agenda),         % process resulting edges
	I2 is I1 + 1,            
	left_right(T,I2,Max).   % move to the right in sentence

lexical_lookup(I0,Agenda) :-
	findall(Edge,lookup(I0,Edge),Agenda).

lookup(I0,Edge):-
	user:ign_lex(I0,I,M,Name),
	store([],M,I0,I,tree(Name,[]),Edge).

% process(+ListOfEdges)
% process a list of edges
process([]).
process([Edge|OldAgenda]):-
	process_one(Edge,SubAgenda),         %% process each one in turn
	append(SubAgenda,OldAgenda,Agenda),  %% depth first!
	process(Agenda).                     %% process the rest

% process_one(+Edge,-Agenda)
% depending on the form of Edge, builds all new edges from Edge 
% by prediction or completion
%
% for inactive edge: completion
% for active edge: prediction, but also completion with
% respect to gaps!
%
% for lexical edges, do nothing

process_one(inactive(Cat,P0,P1),Agenda):-
	findall(Edge,predictor(Cat,P0,P1,Edge),Agenda1),
	findall(Edge,completer(Cat,P0,P1,Edge),Agenda2),
	append(Agenda1,Agenda2,Agenda).

process_one(active(M,Ds,P0,P,His),Agenda):-
	findall(Edge,gap_completer(M,Ds,P0,P,His,Edge),Agenda).

gap_completer(M,[H|T],P0,P,tree(Name,His),Edge):-
	user:ign_gap(H,Gap),
	assertz_most_general(lc_ch:tt(P,P,Gap,[])),
	append(His,[t(P,P,H)],His2),
	store(T,M,P0,P,tree(Name,His2),Edge).

% completer(+Cat,+P1,+P,-Edge)
% selects active edge whose leftmost symbol matches Cat
% ('move-dot')
completer(Cat,P1,P,Edge):-
	active(P1,P0,M,[Cat|Rest],tree(R,His)),
	append(His,[t(P1,P,Cat)],His2),
	store(Rest,M,P0,P,tree(R,His2),Edge).   % store in database

% if the completer is called with an empty edge, then it may be
% the case that not all edges are already around, for which completion
% is possible...

% predictor(Cat,P0,P1,Edge) also eats leftmost daughter
predictor(Cat,P0,P,Edge):-
	user:ign_l_rule(Cat,X0,Body,Name),
	active(P0,_,_,[Next|_],_),
	user:ign_l_link(X0,Next),
	store(Body,X0,P0,P,tree(Name,[t(P0,P,Cat)]),Edge).

% store(+Edge,-Edge)
% stores an edge, and depending on form gives different representation
% of the same edge back. If the edge already exists, then failure.
store(_,_,_,_,_,_):-
	hdrug_util:hdrug_flag(ticks,V),
	V2 is V+1,
	hdrug_util:hdrug_flag(ticks,_,V2),
	fail.
store([],M,P0,P,tree(Rule,His),inactive(M,P0,P)):-
	assertz_most_general(lc_ch:tt(P0,P,Rule,His),_),
	assertz_most_general(lc_ch:inactive(P0,P,M),no).

store([H|T],M,P0,P,His,active(M,[H|T],P0,P,His)):-
	assertz_most_general(lc_ch:active(P,P0,M,[H|T],His),no).

pack_rule(Name,Mother,[]):-
	user:ign_gap(Mother,Name),
	user:    gap(Mother,Name).

pack_rule(Name,Mother,[H|T]) :-
	user:l_rule(H,Mother,T,Name).


