:- module(alpino_user_transformation, [ user_transformation/8 ]).

:- user:set_flag(alpino_ds_version,'1.3').


%% --------------------------------------------------------------------------------------------- %%


user_transformation(r(Rel,i(X,Cat)),A,B,
                    r(Rel2,i(X,Cat2)),C,D,E,F) :-
    user_transformation(r(Rel,Cat),A,B,
                        r(Rel2,Cat2),C,D,E,F).



user_transformation(r(REL,l(read_from_treebank(Az,L0,Tag),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,L1,Tag),Cat,W/[P0,P])),B,[],_,_) :-
    root_lemma(W,L0,L1).

user_transformation(r(REL,l(read_from_treebank(Az,L0,Tag),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,L1,Tag),Cat,W/[P0,P])),B,[],_,_) :-
    tag_lemma(Tag,L0,L1).

user_transformation(r(REL,l(read_from_treebank(Az,L0,Tag),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,L1,Tag),Cat,W/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Surf),
    surf_lemma(Surf,Tag,L0,L1).

user_transformation(r(REL,l(read_from_treebank(Az,L0,Tag),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,L1,Tag),Cat,W/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Surf),
    surf_lemma(Surf,L0,L1).

user_transformation(r(REL,l(read_from_treebank(Az,L0,Tag0),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,L1,Tag),Cat,W/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Surf),
    surf_lemma(Surf,Tag0,Tag,L0,L1).

user_transformation(r(REL,l(read_from_treebank(Az,L0,Tag0),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,L1,Tag),Cat,W/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Surf),
    surf_lemma(Surf,REL,Tag0,Tag,L0,L1).

user_transformation(r(REL,l(read_from_treebank(Az,L0,Tag),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,L1,Tag),Cat,W/[P0,P])),B,[],_,_) :-
    lemma(L0,L1).

root_lemma(_,_,_) :- fail.

surf_lemma(_,_,_,_) :- fail.

surf_lemma(_,_,_) :- fail.

surf_lemma('XVde','15','XV').

surf_lemma(Word,'N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)',L,L) :-
    eigen(Word).
surf_lemma(Word,'N(soort,mv,basis)','N(eigen,mv,basis)',L,L) :-
    eigen(Word).


surf_lemma(Word,'N(eigen,ev,basis,genus,stan)','N(eigen,ev,basis,zijd,stan)',L,L) :-
    de_naam(Word).
surf_lemma(Word,'N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)',L,L) :-
    de_naam(Word).

surf_lemma(Word,'N(eigen,ev,basis,genus,stan)','N(eigen,ev,basis,onz,stan)',L,L) :-
    het_naam(Word).
surf_lemma(Word,'N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)',L,L) :-
    het_naam(Word).

surf_lemma(Word,'N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)',L,L) :-
    genus_naam(Word).
surf_lemma(Word,'N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)',L,L) :-
    genus_naam(Word).

surf_lemma(Word,'N(eigen,ev,basis,genus,stan)','N(eigen,mv,basis)',L,L) :-
    pl_naam(Word).
surf_lemma(Word,'N(eigen,ev,basis,zijd,stan)','N(eigen,mv,basis)',L,L) :-
    pl_naam(Word).
surf_lemma(Word,'N(eigen,ev,basis,onz,stan)','N(eigen,mv,basis)',L,L) :-
    pl_naam(Word).

eigen(_):-
    fail.


pl_naam(_) :-
    fail.

het_naam(_):-
    fail.

de_naam(_) :-
    fail.

genus_naam(_) :-
    fail.


surf_lemma(_,_,_,_,_,_) :- fail.

tag_lemma(_,_,_) :- fail.

lemma(_,_) :-
    fail.

lemma('ZYPREXA','Zyprexa').
lemma('VELOTAB','Velotab').
lemma('RIZIV','Riziv').

genus_naam('Forsteo').
genus_naam('Liprolog').
genus_naam('Evista').