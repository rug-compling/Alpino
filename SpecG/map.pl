
create_locals :-
    use_module(sorted),
    local(M,Ds0),
    alpino_cg:adapt(Ds0,Ds),
    format("~q.~n",[local(M,Ds)]),
    fail.

create_bigrams :-
    repeat,
    read(Tree),
    (   Tree == end_of_file
    ->  !
    ;   create_bigrams(Tree),
	fail
    ).

create_bigrams(deriv(_,Tree)) :-
    create_bigrams_(Tree).

create_bigrams_(tree(_,_,Ds)) :-
    create_bigrams_ds(Ds).
create_bigrams_(tree(M,_,Ds0)) :-
    local_trees_ds(Ds0,Ds),
    format("~q.~n",[bigram(M,Ds)]).

create_bigrams_ds(Ds) :-
    lists:member(tree(M,_,[D|E]),Ds),
    create_bigrams_(tree(M,_,[D|E])).

local_trees_ds([],[]).
local_trees_ds([H0|T0],[H|T]):-
    local_trees(H0,H),
    local_trees_ds(T0,T).

local_trees(tree(Node,_,Ds),Result):-
    local_trees_i(Ds,Node,Result).

local_trees_i([],gap(Gap),gap(Gap)).

local_trees_i([],lex(Tag),lex(Class)):-
    alpino_tr_tag:tr_tag(Tag,Class).
local_trees_i([D0|S0],Rule,tree(Rule,Ds)):-
    local_trees_ds1([D0|S0],Ds).


local_trees_ds1([],[]).
local_trees_ds1([H0|T0],[H|T]):-
    local_trees1(H0,H),
    local_trees_ds1(T0,T).

local_trees1(tree(Node,_,Ds),Result):-
    local_trees_i1(Ds,Node,Result).

local_trees_i1([],gap(Gap),gap(Gap)).

local_trees_i1([],lex(Tag),lex(Class)):-
    alpino_tr_tag:tr_tag(Tag,Class).
local_trees_i1([_|_],Rule,Rule).

