% TRANSITIVE.PL

% zie FSA utilities voor _veel_ betere transitive_ground implementatie

% transitive(+RelSymbol,+NewRelSymbol)
% computes transitive closure of binary relation. 
% terminates iff number of pairs is finite.

% transitive(+RelSymbol,+NewRelSymbol,+RestrictionRelSymbol)
% computes transitive closure of binary relation. 
% always terminates
 

:- module(transitive,[ transitive/2, transitive/3 ]).

:- use_module(lists, library(lists),[ member/2, append/3 ]).

:- use_module(database).

% if we have
% p(1,2)
% p(2,3)
% p(3,4)
% then we obtain, upon  ?- transitive(p,trans_p)
% the clauses:
%
%
% trans_p(1,2).
% trans_p(1,3).
%  ..
% trans_p(3,4).

% :- meta_predicate transitive(:,:).

add_module(Pred0,M:Pred) :-
    (	Pred0 = M:Pred
    ->	true
    ;	Pred0 = Pred,
	M=user
    ).

transitive(A1,A2):-
    add_module(A1,B1),
    add_module(A2,B2),
    transitive2(B1,B2,user:'=').

transitive(A1,A2,R1):-
    add_module(A1,B1),
    add_module(A2,B2),
    add_module(R1,R2),
    transitive2(B1,B2,R2).

transitive2(M0:Rel,M:NewRel,Res):-
%	abolish(M:NewRel,2),
    NewRelC =.. [NewRel,_,_],
    clean_up_database(M:NewRelC),
    foundation(M0:Rel,M:NewRel,Res,P0),
    transitive_n(P0,M:NewRel,Res).

foundation(RelSym,NewRelSym,Res,P0):-
    findall(Pair,t0(RelSym,NewRelSym,Res,Pair),P0).

t0(M0:RelSym,M:NewRelSym,Mr:Res,Pair):-
    Rel =.. [RelSym,A1,A2],
    Pair1 =.. [NewRelSym,A1,A2],
    CalRes =.. [Res,Pair1,Pair],
    M0:Rel,
    Mr:CalRes,
    assertz_most_general(M:Pair,no). %=> database.pl, fails if already exists

transitive_n([],_,_Res).
transitive_n([H|T],Rel,Res):-
    new_pairs(Rel,H,Res,P),
    append(T,P,T2),
    transitive_n(T2,Rel,Res).

new_pairs(NewRel,Pair1,Res,Pn):-
    findall(Pair,new_pair(NewRel,Pair1,Res,Pair),Pn).

new_pair(M:Rel,Sel1,Mr:Res,Pair):-
    Pair1 =.. [Rel,Arg1,Arg3],
    Sel1 =.. [Rel,Arg1,Arg2],
    Sel2 =.. [Rel,Arg2,Arg3],
    ResCall =.. [Res,Pair1,Pair],
    M:Sel2,
    Mr:ResCall,
    assertz_most_general(M:Pair,no). %=> database.pl, fails if already exists

