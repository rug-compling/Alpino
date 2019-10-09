:- module(rcp3_dtrs,[]).

% right_chart.pl + packing.

% this is an inactive chart parser with packing.
% The idea is that the items used
% forget about delayed constraints and 
% feature constraints. This makes
% administration of those items cheap, but 
% recovering of parse trees more expensive.
% Items that are asserted are supposedly ground.
% Therefore first phase of the parser is
% context-free technology. During `recovery' features
% and other constraints are applied.

%% The first phase of the parser is SO MUCH FASTER than
%% the second phase that it is not worthwile to consider
%% speed-up here. What would be important is to consider
%% strategies that imply that less tt items are asserted.

%% rule name of mother, category of daughter


clean :-
	retractall(inactive(_,_,_)),
	retractall(tt(_,_,_,_,_,_)),
	retractall(tt(_,_,_)).

parse(o(Node0,String,_)) :-
	user:unify_except(Node0,Node,tree),
	user:deriv_tree(Node0,Tree),
	length(String,Max),
	( bu_lr(0,Max), fail
        ; write('recover parse trees'), nl,
	  ttyflush,
	  apply_semantics(Node,0,Max,Tree)).

bu_lr(I,I) :- !.

bu_lr(I0,I):-
	( user:cfg_approx_lex(I0,Ix,M,Name),
          store(M,I0,Ix,tree(Name)),
	  fail
        ; I1 is I0 + 1,
	  bu_lr(I1,I)
        ).

% completer(+Cat,+P1,+P,-Edge)
% selects active edge whose rightmost symbol matches Cat
infer(P1,P,Cat):-
	apply_rule(Cat,P1,P,M,P0,P,Tree),
	store(M,P0,P,Tree).   % store in database

% rightmost is not the head
apply_rule(Cat,P3,P,Mother,P0,P,tree(Rule,t(P1,P2,Head),Lds,Rds)) :-
	user:cfg_approx_rule(Head,Mother,Ls,[Cat|RestRs],Rule),
	select_lefties(RestRs,P2,P3,[t(P3,P,Cat)],Rds),
	inactive(P2,P1,Head),
	select_lefties_nr(Ls,P0,P1,Lds).

% rightmost is the head
apply_rule(Head,P1,P,Mother,P0,P,tree(Rule,t(P1,P,Head),Lds,[])) :-
	user:cfg_approx_rule(Head,Mother,Ls,[],Rule),
	select_lefties_nr(Ls,P0,P1,Lds).

select_lefties([],P,P,His,His).
select_lefties([H|T],P0,P,HisIn,HisOut):-
	inactive(P,P1,H),
	select_lefties(T,P0,P1,[t(P1,P,H)|HisIn],HisOut).

select_lefties_nr([],P,P,[]).
select_lefties_nr([H|T],P0,P,[t(P1,P,H)|HisOut]):-
	inactive(P,P1,H),
	select_lefties_nr(T,P0,P1,HisOut).

% store(+M,+P0,+P,+Tree)
store(M,P0,P,tree(Rule,Head,Lds,Rds)):-
	assert_tt(tt(P0,P,Rule,Head,Lds,Rds),_),
	assert_tt(inactive(P,P0,M),no),
	infer(P0,P,M).

store(M,P0,P,tree(Rule)) :-
	assert_tt(tt(P0,P,Rule),_),
	assert_tt(inactive(P,P0,M),no),
	infer(P0,P,M).

assert_tt(Pred,Bool):-
	Pred,!,Bool=yes.
assert_tt(Pred,no) :-
	assertz(Pred).

list :-
	listing(inactive/4),
	listing(tt/3),
	listing(tt/6).

count(B,D,F) :-
	hdrug_util:count_edges(rcp3_dtrs:inactive(_,_,_),B),
	hdrug_util:count_edges(rcp3_dtrs:tt(_,_,_,_,_,_),D),
	hdrug_util:count_edges(rcp3_dtrs:tt(_,_,_),F).

count(D) :-
	count(_,D,_F).

count :-
	count(B,D,F),
	write(B),write(' inactive edges'),nl,
	write(D),write(' tt/6 edges'),nl,
	write(F),write(' tt/3 edges'),nl.


% apply_semantics(Cat,P0,P,Tree)

% builds results on the basis of packed representation,
% using the rules WITH the semantics (and all other constraints).
% It is assumed that in Module the predicate tt represents
% the packed forest, such that tt(P0,P,Name,Head,Ls,Rs), where
% P0, P start/end positions, Name is rulename, and Ls and Rs is
% a list of t(P0,P,Cat) triples, or tt(P0,P,Name) for daughterless.

% Note that unpacking is head-driven!

apply_semantics(Mother,P0,P,tree(Mother,_,[])) :-
	tt(P0,P,Name),
	user:lex(P0,P,Mother,Name).
apply_semantics(Mother,P0,P,tree(Mother,_,Trees)):-
	tt(P0,P,Name,t(Q0,Q,Head0),Ls,Rs),
	user:explode_cat(Head0,Head),
	user:h_rule(Head,Mother,Lds,Rds,Name),
	apply_semantics(Head,Q0,Q,Htree),
	apply_ds(Rs,Rds,Rtrees),
	apply_ds(Ls,Lds,[Htree|Rtrees],Trees).

apply_ds([],[],[]).
apply_ds([t(P0,P,Cat0)|Tail],[Cat|DT],[T0|Tt]):-
	user:explode_cat(Cat0,Cat),
	apply_semantics(Cat,P0,P,T0),
	apply_ds(Tail,DT,Tt).

apply_ds([],[],L,L).
apply_ds([t(P0,P,Cat0)|Tail],[Cat|DT],L0,L):-
	user:explode_cat(Cat0,Cat),
	apply_semantics(Cat,P0,P,L1),
	apply_ds(Tail,DT,[L1|L0],L).

