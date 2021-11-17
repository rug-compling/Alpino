:- module(alpino_user_transformation, [ apply_adt_transformations/2 ]).

:- use_module([simplify_words,
	       simplify_passive,
	       simplify_modifier,
	       simplify_split
	      ]).

apply_adt_transformations(Tree0,Tree) :-
    hdrug_util:hdrug_flag(simplify_split,Split),
    apply_split_transformations(Split,Tree0,Tree1),
    hdrug_util:hdrug_flag(simplify_passive,Pass),
    apply_passive_transformations(Pass,Tree1,Tree2),
    hdrug_util:hdrug_flag(simplify_modifier,Mod),
    apply_modifier_transformations(Mod,Tree2,Tree3),
    hdrug_util:hdrug_flag(simplify_words,Words),
    apply_words_transformations(Words,Tree3,Tree).

apply_modifier_transformations(Mod,Tree0,Tree1) :-
    (   Mod == on
    ->  apply_modifier_transformations(Tree0,Tree1)
    ;   Tree0 = Tree1
    ).

apply_words_transformations(Words,Tree1,Tree2) :-
    (   Words == on
    ->  apply_words_transformations(Tree1,Tree2)
    ;   Tree1 = Tree2
    ).

apply_passive_transformations(Pass,Tree2,Tree3) :-
    (   Pass == on
    ->  apply_passive_transformations(Tree2,Tree3)
    ;   Tree2 = Tree3
    ).

apply_split_transformations(Split,Tree3,Tree) :-
    (   Split == on
    ->  apply_split_transformations(Tree3,Tree)
    ;   Tree3 = Tree
    ).



    

