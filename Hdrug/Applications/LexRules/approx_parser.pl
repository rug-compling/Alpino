:- module(approx_parser,[]).

% This version does not know anything about the categories used by
% the approximation cfg. Therefore you don't have to worry about 
% spurious ambiguities, that might otherwise occur (if your cfg makes
% distinctions the constraint grammar does not make). In general this
% results in a smaller parse forest, where each item contains less information.
% Tests indicate that this approach is slightly faster.

% To do: allow regular expressions in rules.
% Needed: way to indicate head

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



clean :-
	retractall(inactive(_,_,_)),
	retractall(tt(_,_,_,_,_,_)),
	retractall(tt(_,_,_)).

parse(o(Node,String,_)) :-
	length(String,Max),
	( bu_lr(0,Max), fail
        ; write('recover parse trees'), nl,
	  ttyflush,
	  apply_semantics(Node,0,Max)).

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
apply_rule(Cat,P3,P,Mother,P0,P,tree(Rule,t(P1,P2),Lds,Rds)) :-
	user:cfg_approx_rule(Head,Mother,Ls,[Cat|RestRs],Rule),
	select_lefties(RestRs,P2,P3,[t(P3,P)],Rds),
	inactive(P2,P1,Head),
	select_lefties_nr(Ls,P0,P1,Lds).

% rightmost is the head
apply_rule(Head,P1,P,Mother,P0,P,tree(Rule,t(P1,P),Lds,[])) :-
	user:cfg_approx_rule(Head,Mother,Ls,[],Rule),
	select_lefties_nr(Ls,P0,P1,Lds).

select_lefties([],P,P,His,His).
select_lefties([H|T],P0,P,HisIn,HisOut):-
	inactive(P,P1,H),
	select_lefties(T,P0,P1,[t(P1,P)|HisIn],HisOut).

select_lefties_nr([],P,P,[]).
select_lefties_nr([H|T],P0,P,[t(P1,P)|HisOut]):-
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
	Pred,!,Bool=yes.        % assumes that items are ground
                                % which is valid for cfg
assert_tt(Pred,no) :-
	assertz(Pred).

list :-
	listing(inactive/4),
	listing(tt/3),
	listing(tt/6).

count(B,D,F) :-
	hdrug_util:count_edges(approx_parser:inactive(_,_,_),B),
	hdrug_util:count_edges(approx_parser:tt(_,_,_,_,_,_),D),
	hdrug_util:count_edges(approx_parser:tt(_,_,_),F).

count(D) :-
	count(_,D,_F).

count :-
	count(B,D,F),
	write(B),write(' inactive edges'),nl,
	write(D),write(' tt/6 edges'),nl,
	write(F),write(' tt/3 edges'),nl.


% apply_semantics(Cat,P0,P,Module)

% builds results on the basis of packed representation,
% using the rules WITH the semantics (and all other constraints).
% It is assumed that in Module the predicate tt represents
% the packed forest, such that tt(P0,P,Name,Head,Ls,Rs), where
% P0, P start/end positions, Name is rulename, and Ls and Rs is
% a list of t(P0,P) pairs, or tt(P0,P,Name) for daughterless.

% Note that unpacking is head-driven!

apply_semantics(Mother,P0,P) :-
	tt(P0,P,Name),
	user:lex(P0,P,Mother,Name).
apply_semantics(Mother,P0,P):-
	tt(P0,P,Name,t(Q0,Q),Ls,Rs),
	user:h_rule(Head,Mother,Lds,Rds,Name),
	apply_semantics(Head,Q0,Q),
	apply_ds(Rs,Rds),
	apply_ds(Ls,Lds).

apply_ds([],[]).
apply_ds([t(P0,P)|Tail],[Cat|DT]):-
	apply_semantics(Cat,P0,P),
	apply_ds(Tail,DT).




