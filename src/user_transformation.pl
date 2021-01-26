:- module(alpino_user_transformation, [ user_transformation/8 ]).

:- user:set_flag(alpino_ds_version,'1.3').


%% --------------------------------------------------------------------------------------------- %%


user_transformation(r(Rel,i(X,Cat)),A,B,
                    r(Rel2,i(X,Cat2)),C,D,E,F) :-
    user_transformation(r(Rel,Cat),A,B,
                        r(Rel2,Cat2),C,D,E,F).


% user_transformation(r(Rel,Cat),A,Ds0,
% 		    r(Rel,Cat),A,[Hd,Obj1|Hdf],_,_) :-
%     Obj1 = tree(r(obj1,i(Ix)),Obj1A,Obj1Ds),
%     Hd0  = tree(r(hd,l(read_from_treebank(Az,L,'VZ(init)'),Cat,W)),HdA,[]),
%     Hd   = tree(r(hd,l(read_from_treebank(Az,L,'VZ(fin)'),Cat,W)),HdA,[]),
%     lists:select(Obj1,Ds0,Ds1),
%     lists:select(Hd0,Ds1,Hdf).
		   

% user_transformation(r(Rel,Cat),A,[Obj1,Hd0|Hdf],
% 		    r(Rel,Cat),A,[Obj1,Hd|Hdf],_,_) :-
%     Obj1 = tree(r(obj1,CAT),Obj1A,Obj1Ds),
%     Hd0  = tree(r(hd,l(read_from_treebank(Az,L,'VZ(init)'),Cat,W)),HdA,[]),
%     Hd   = tree(r(hd,l(read_from_treebank(Az,L,'VZ(fin)'),Cat,W)),HdA,[]).
		   

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


surf_lemma(Word,Rel,Pos0,Pos,Lem0,Lem) :-
    \+ Rel = mwp,
    surf(Word,Pos,Lem),
    \+ Pos0/Lem0 = Pos/Lem.


tag_lemma(_,_,_) :- fail.

lemma(_,_) :-
    fail.

surf(_,_,_) :-
    fail.


user:query:-
    findall(L,lemma(L,_),Ls),
    (   Ls = [_|_]
    ->  query(Ls,Chars,[]),
	format("~n~nconverse '//node[@lemma=(~s)]'~n",[Chars])
    ;   true
    ),
    findall(S,surf(S,_,_),Ss),
    (   Ss = [_|_]
    ->  query(Ss,Chars2,[]),
	format("~nconverse '//node[@word=(~s)]'~n~n",[Chars2])
    ;   true
    ).

query([]) --> [].
query([H|T]) -->
    q(T,H),
    query(T).

q([],H) -->
    charsio:format_to_chars("\"~w\"",H).
q([_|_],H) -->
    charsio:format_to_chars("\"~w\",",H).
    

user_transformation(r(REL,p(mwu)),B,Ds0,
		    r(REL,p(mwu)),B,Ds ,String,_) :-
    surfs(Ds0,Surfs,String),
    correct_tags(Surfs,Lemmas,Tags),
    assign_tags(Lemmas,Tags,Ds0,Ds),
    \+ Ds0 = Ds.

surfs([],[],_).
surfs([tree(r(mwp,l(_,_,_/[P0,P])),_,_)|Trees],[Surf|Surfs],String) :-
    alpino_treebank:get_root(P0,P,String,Surf),
    surfs(Trees,Surfs,String).
surfs([tree(r(mwp,i(_,l(_,_,_/[P0,P]))),_,_)|Trees],[Surf|Surfs],String) :-
    alpino_treebank:get_root(P0,P,String,Surf),
    surfs(Trees,Surfs,String).

assign_tags([],[],[],[]).
assign_tags([Lem|Lems],[Tag|Tags],[Tree0|Trees0],[Tree|Trees]) :-
    assign_tag(Lem,Tag,Tree0,Tree),
    assign_tags(Lems,Tags,Trees0,Trees).

assign_tag(Lem, Tag,
	   tree(r(mwp,l(read_from_treebank(Az,_,_),Cat,W)),B,[]),
	   tree(r(mwp,l(read_from_treebank(Az,Lem,Tag),Cat,W)),B,[])
	  ).
assign_tag(Lem, Tag,
	   tree(r(mwp,i(Index,l(read_from_treebank(Az,_,_),Cat,W))),B,[]),
	   tree(r(mwp,i(Index,l(read_from_treebank(Az,Lem,Tag),Cat,W))),B,[])
	  ).

let(',').
let('.').
let('(').
let(')').
let('/').
let('"').   % "
let('\'').
let('!').
let('?').
let(':').
let(';').

deeleigen1(L,Tag) :-
    let(L),
    !,
    Tag = 'LET()'.
deeleigen1(_,'SPEC(deeleigen)').

vreemd1(L,Tag) :-
    let(L),
    !,
    Tag = 'LET()'.
vreemd1(_,'SPEC(vreemd)').

deeleigen([],[]).
deeleigen([LET|T],[Tag|L]) :-
    deeleigen1(LET,Tag),
    deeleigen(T,L).

vreemd([],[]).
vreemd([LET|T],[Tag|L]) :-
    vreemd1(LET,Tag),
    vreemd(T,L).

correct_tags(L,L,Deeleigen) :-
    flat(L),
    deeleigen(L,Deeleigen).

correct_tags(L,L,Deeleigen) :-
    vreemd(L),
    vreemd(L,Deeleigen).

correct_tags([op,den,duur],[op,de,duur],['VZ(init)','LID(bep,dat,evmo)','N(soort,ev,basis,zijd,stan)']).

flat(_) :-
    fail.

vreemd(_) :-
    fail.

