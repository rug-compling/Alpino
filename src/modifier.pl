:- module(alpino_user_transformation, [ apply_adt_transformations/2 ]).

%% --------------------------------------------------------------------------------------------- %%

apply_adt_transformations(Tree0,Tree) :-
    dont_paraphrase(Tree0), !,
    Tree0 = Tree.

apply_adt_transformations(Tree0,Tree) :-
    apply_modifier_transformations(Tree0,Tree).

apply_modifier_transformations(tree(Cat0,Ds0),tree(Cat,Ds)) :-
    modifier_transformation(Cat0,Ds0,Cat1,Ds1),
    !,
    apply_modifier_transformations(tree(Cat1,Ds1),tree(Cat,Ds)).
apply_modifier_transformations(tree(Cat,Ds0),tree(Cat,Ds)) :-
    apply_modifier_transformations_list(Ds0,Ds).

apply_modifier_transformations_list([],[]).
apply_modifier_transformations_list([H0|T0],[H|T]) :-
    apply_modifier_transformations(H0,H),
    apply_modifier_transformations_list(T0,T).

modifier_transformation(r(Rel,VAR),A,
			r(Rel2,i(X,Cat2)),B) :-
    nonvar(VAR),
    VAR = i(X,Cat),
    modifier_transformation(r(Rel,Cat),A,
			    r(Rel2,Cat2),B).

modifier_transformation(r(Rel,_),Ds0,r(Rel,AppCat),AppDs) :-
    Hd = tree(r(hd,_),_),
    lists:select(Hd,Ds0,Ds1),
    App = tree(r(app,AppCat),AppDs),
    lists:select(App,Ds1,Ds2),
    (  AppCat = adt_lex(_,_,_,name,_)
    ;  AppCat = p(mwu(_,_))
    ;  AppCat = p(conj)
    ),
    (   Ds2 = [tree(r(det,adt_lex(_,_,de,_,_)),[])]
    ;   Ds2 = []
    ).

modifier_transformation(r(Rel,VAR),Ds0,r(Rel,VAR),Ds) :-
    Mod = tree(r(ModRel,ModInfo),_),
    lists:select(Mod,Ds0,Ds),
    modifier_rel(ModRel,ModInfo),
    \+ important_modifier(Mod),
    \+ container_head(Ds,Mod).

container_head(Ds,Mod):-
    np(Mod),
    Hd = tree(r(hd,adt_lex(_,Zak,_,noun,_)),[]),
    lists:member(Hd,Ds),
    alpino_lex:inv_lex(Zak,Zakje),
    (  alpino_lex:lexicon(noun(_,_,_,measure),Zak,[Zakje],[],_)
    ;  alpino_lex:lexicon(meas_mod_noun(_,_,_,measure),Zak,[Zakje],[],_)
    ).

np(tree(r(_,p(np)),_)).
np(tree(r(_,adt_lex(np,_,_,_,_)),_)).

important_mod_stem(niet).
important_mod_stem(nooit).
important_mod_stem(anders).

important_mod(r(_,adt_lex(_,Stem,_,_,_))):-
	important_mod_stem(Stem).

important_modifier(tree(Cat,_)):-
    important_mod(Cat).
important_modifier(tree(_Cat,Ds)) :-
    lists:member(Mod,Ds),
    important_modifier1(Mod).

important_modifier1(tree(Cat,_)):-
    important_mod(Cat).

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

modifier_rel(predm,_).
modifier_rel(mod,_).
modifier_rel(app,Cat) :-
    Cat \= adt_lex(_,_,_,name,_),
    Cat \= p(mwu(_,_)),
    Cat \= p(conj).
