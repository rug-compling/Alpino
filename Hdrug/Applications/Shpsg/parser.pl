%% Stochastic HPSG parser

:- module(parser,[]).

%%% edge(Id,Start,End,LHS,Found,ToFind).

:- dynamic edge/6.

%% Bottom up chart parser

parse(o(parse(Obj,P),Str,Fs)) :-
    parse0(Obj,Str),
    hdrug_util:hdrug_flag(model,Model),
    user:fs_prob(Model,Obj,Fs,P).

parse0(Cat,String) :-
    V0 is 1,
    retractall(edge(_,_,_,_,_,_)),
    start_chart(V0,Vn,String),
    edge(_,V0,Vn,Cat,_,[]).

%%%
%%% Gazdar and Mellish style bottom up chart parser
%%%

%% Initialize chart
       
start_chart(V,V,[]).
start_chart(V0,Vn,[W|Ws]) :-
    V1 is V0 + 1,
    foreach(user:clexicon(W,Cat),
	    add_edge(V0,V1,Cat,[lex(W)],[])),
    start_chart(V1, Vn, Ws).
       
%% Attempt to add an existing edge
       
add_edge(Vfrom,Vto,Cat,Found,ToFind) :-
    edge(_, Vfrom, Vto, Cat, Found, ToFind),
    !.
       
%% Add a passive edge.
%%  1. add I to the chart
%%  2. for each grammar rule
%%     - where the leftmost symbol in the right-hand side = Cat
%%       (i.e. the rule is of the form LHS --> Cat Cs)
%%     ==> add an active edge that needs I to the chart
%%  3. for each active edge A
%%     - that immediately precedes I
%%       (i.e. end vertex of A = start vertex of I)
%%     - and that can combine with I
%%       (i.e. A needs an edge of I's category)
%%     ==> add the combination of A with I to the chart
       
add_edge(V1, V2, Cat, Found, []) :-
    hdrug_util:gen_sym(ID,edge),
    asserta(edge(ID, V1, V2, Cat, Found, [])),
    foreach(user:rule(LHS,[Cat|Cs]),
	    add_edge(V1, V1, LHS, [], [Cat | Cs])),
    foreach(edge(_, V0, V1, LeftCat, LeftFound, [Cat | LeftToFind]),
	    add_edge(V0, V2, LeftCat, [ID | LeftFound], LeftToFind)).
		    
%% Add an active edge
%%  1. add A to the chart
%%  2. for each inactive edge I
%%     - that immediately follows A
%%       (i.e. end vertex of A = start vertex of I)
%%     - and that can combine with A
%%       (i.e. A needs an edge of I's category)
%%     ==> add the combination of A with I to the chart

add_edge(V0, V1, Cat, Found, [M1|MRest]) :-
    hdrug_util:gen_sym(ID,edge),
    asserta(edge(ID, V0, V1, Cat, Found, [M1|MRest])),
    foreach(edge(InactiveEdgeID, V1, V2, M1, _, []),
	    add_edge(V0, V2, Cat, [InactiveEdgeID | Found], MRest)).
       
       
foreach(X, Y) :-
    call(X),
    once(Y),
    fail.

foreach(_, _) :-
    true.

once(Goal) :-
    call(Goal),
    !.

%% Grammar stuff

:- multifile user:user_clause/2.

%user:user_clause(H,B) :-
%    user:rule(r(_,H,_),B).

%user:user_clause(H,[B]) :-
%    user:lexicon(B,r(_,H,_)).

%user:user_clause(feature(M,H),[B]) :-
%    user:feature(M,H,B).

%% Chart stuff

show_chart(active) :-
    findall(P,edge(_,P,_,_,_,_),P0),
    findall(P,edge(_,_,P,_,_,_),P1),
    lists:append(P0,P1,Ps),
    findall(edge(V0,V1,Name,Id),edge(Id,Name,V0,V1,_,_,_),Edges),
    format("~q~n",[Edges]),
    hdrug_chart:pp_chart(Ps,Edges,[]).

show_chart(passive) :-
    findall(P,edge(_,P,_,_,_,_),P0),
    findall(P,edge(_,_,P,_,_,_),P1),
    lists:append(P0,P1,Ps),
    findall(edge(V0,V1,Name,Id),edge(Id,Name,V0,V1,_,_,[]),Edges),
    hdrug_chart:pp_chart(Ps,Edges,[]).

user:pp_chart_item(Ident) :-
    write(Ident),nl,
    edge(Ident,_,_,Cat,_,_),
    hdrug_clig:clig_fs(Cat).
