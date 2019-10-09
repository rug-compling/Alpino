:- module(alpino_user_transformation, [ apply_adt_transformations/2 ]).

%% --------------------------------------------------------------------------------------------- %%

apply_adt_transformations(Tree0,Tree) :-
    dont_paraphrase(Tree0), !,
    Tree0 = Tree.

apply_adt_transformations(Tree0,Tree) :-
    apply_words_transformations(Tree0,Tree).

apply_words_transformations(tree(Cat0,Ds0),tree(Cat,Ds)) :-
    words_transformation(Cat0,Ds0,Cat1,Ds1),
    !,
    apply_words_transformations(tree(Cat1,Ds1),tree(Cat,Ds)).
apply_words_transformations(tree(Cat,Ds0),tree(Cat,Ds)) :-
    apply_words_transformations_list(Ds0,Ds).

apply_words_transformations_list([],[]).
apply_words_transformations_list([H0|T0],[H|T]) :-
    apply_words_transformations(H0,H),
    apply_words_transformations_list(T0,T).

words_transformation(r(Rel,VAR),A,r(Rel2,i(X,Cat2)),C) :-
    nonvar(VAR),
    VAR = i(X,Cat),
    words_transformation(r(Rel,Cat),A,r(Rel2,Cat2),C).

words_transformation(r(Rel,p(Cat0)),Ds0,r(Rel,p(Cat)),[Hd|Ds]) :-
    Hd0 = tree(r(HD,adt_lex(_,Old,Old,D,E)),[]),
    Hd  = tree(r(HD,adt_lex(_,New,New,D,E)),[]),
    head_rel(HD),
    lists:select(Hd0,Ds0,Ds),
    simplify(Old,New,Cat0,Cat).

words_transformation(r(Rel,adt_lex(A,Old,Old,D,E)),[],
		     r(Rel,adt_lex(A,New,New,D,E)),[]) :-
    simplify(Old,New).

head_rel(hd).
head_rel(cmp).

%% must replace something...
replace(El0,El,[El0|Tail],[El|Tail]).
replace(El0,El,[X|Tail0],[X|Tail]):-
    replace(El0,El,Tail0,Tail).

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

simplify(de,{[de,het]}).
simplify(het,{[de,het]}).
simplify(dit,{[deze,dit]}).
simplify(deze,{[deze,dit]}).
simplify(die,{[die,dat]}).
simplify(dat,{[die,dat]}).
simplify(ons,{[ons,onze]}).
simplify(onze,{[ons,onze]}).

simplify(eenvoudig,simpel).
simplify(incidenteel,eenmalig).
simplify(momenteel,nu).
simplify(staatshoofd,koning).

simplify(teneinde,om,cp,oti).
