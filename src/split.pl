:- module(alpino_user_transformation, [ apply_adt_transformations/2 ]).

%% --------------------------------------------------------------------------------------------- %%

apply_adt_transformations(Tree0,Tree) :-
    dont_paraphrase(Tree0), !,
    Tree0 = Tree.

apply_adt_transformations(Tree0,Tree) :-
    apply_split_transformations(Tree0,Tree).

apply_split_transformations(tree(Cat0,Ds0),tree(Cat,Ds)) :-
    split_transformation(Cat0,Ds0,Cat1,Ds1),
    !,
    apply_split_transformations(tree(Cat1,Ds1),tree(Cat,Ds)).
apply_split_transformations(tree(Cat,Ds0),tree(Cat,Ds)) :-
    apply_split_transformations_list(Ds0,Ds).

apply_split_transformations_list([],[]).
apply_split_transformations_list([H0|T0],[H|T]) :-
    apply_split_transformations(H0,H),
    apply_split_transformations_list(T0,T).

split_transformation(r(top,p(top)),Ds0,r(split,p(split)),Ds) :-
    select_replace(Ds0,D,X1,X2,Ds),
    split_transformation(D,X1,X2).


%% replace element by two elements
select_replace([H|T],H,X1,X2,[X1,X2|T]).
select_replace([H|T],D,X1,X2,[H|NT]) :-
    select_replace(T,D,X1,X2,NT).

split_transformation(tree(r('--',p(conj)),Ds0),X1,X2) :-
    D1 = tree(r(crd,adt_lex(_,en,_,_,_)),[]),
    D2 = tree(r(cnj,P1),L1),
    D3 = tree(r(cnj,P2),L2),
    lists:select(D1,Ds0,Ds1),
    lists:select(D2,Ds1,Ds2),
    lists:select(D3,Ds2,[]),
    X1 = tree(r(top,p(top)),[tree(r('--',P1),L1)]),
    X2 = tree(r(top,p(top)),[tree(r('--',P2),L2)]).

dont_paraphrase(Tree) :-
    tree_member(Sub,Tree),
    dont_paraphrase_sub(Sub).

dont_paraphrase_sub(tree(r(_,adt_lex(_,_,_,_,Atts)),[])) :-
    lists:member(stype=topic_drop,Atts).
dont_paraphrase_sub(tree(r(_,i(_,adt_lex(_,_,_,_,Atts))),[])) :-
    lists:member(stype=topic_drop,Atts).

tree_member(Sub,Sub).
tree_member(Sub,tree(_,Ds)) :-
    lists:member(D,Ds),
    tree_member(Sub,D).

