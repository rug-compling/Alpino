:- module(alpino_user_transformation, [ user_transformation/8 ]).

:- user:set_flag(alpino_ds_version,'1.3').


%% --------------------------------------------------------------------------------------------- %%


user_transformation(r(Rel,i(X,Cat)),A,B,
                    r(Rel2,i(X,Cat2)),C,D,E,F) :-
    user_transformation(r(Rel,Cat),A,B,
                        r(Rel2,Cat2),C,D,E,F).

%user_transformation(X,_,_,_,_,_,_,_):-
%    format(user_error,"~w~n",[X]),fail.

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
    ( \+ REL = mwp
    ; \+ Tag = 'SPEC(deeleigen)',
      \+ Tag = 'SPEC(vreemd)'
    ),
    lemma(L0,L1), nonvar(L1),
    format(user_error,"~w -> ~w~n",[L0,L1]).

user_transformation(r(REL,l(read_from_treebank(Az,L0,Tag),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,L1,Tag),Cat,W/[P0,P])),B,[],_,_) :-
    mlemma(L0,L1),
    format(user_error,"~w -> ~w~n",[L0,L1]).

user_transformation(r(REL,p(mwu)),B,Ds0,
		    r(REL,p(mwu)),B,Ds ,String,_) :-
    surfs(Ds0,Surfs,String), 
    mwu_postag(Surfs,Tags,Lemmas),
    assign_tags(Lemmas,Tags,Ds0,Ds),
    \+ Ds0 = Ds.

/*
%user_transformation(REL,A,B,_,_,_,_,_) :-
%   format(user_error,"~w~n",[REL]), fail.

user_transformation(r(Rel,p(mwu)),B,[D1,D2],
		    r(Rel,p(advp)),B,[E1,E2],_,_) :-
    D1 = tree(r(mwp,l(read_from_treebank(_,alleen,D13),D14,D15)),D16,[]),
    E1 = tree(r(hd,l(read_from_treebank(adv,alleen,D13),D14,D15)),D16,[]),
    D2 = tree(r(mwp,l(read_from_treebank(_,al,D23),D24,D25)),D26,[]),
    E2 = tree(r(mod,l(read_from_treebank(adv,al,D23),D24,D25)),D26,[]).


user_transformation(r(app,p(mwu)),B,[N1,Dash1,N2],
		    r(app,p(conj)),B,[C1,C2],_,_) :-
    Dash1 = tree(r(mwp,l(read_from_treebank(_,'-','LET()'),_,_)),_,[]),
    N1   = tree(r(mwp,l(read_from_treebank(num,A2,'TW(hoofd,vrij)'),A4,A5/A6)),_,[]),
    N2   = tree(r(mwp,l(read_from_treebank(num,B2,'TW(hoofd,vrij)'),B4,B5/B6)),_,[]),
    C1   = tree(r(cnj,l(read_from_treebank(num,A2,'TW(hoofd,vrij)'),A4,A5/A6)),_,[]),
    C2   = tree(r(cnj,l(read_from_treebank(num,B2,'TW(hoofd,vrij)'),B4,B5/B6)),_,[]).
*/

/*
user_transformation(r(obj1,p(ap)),B,Ds,
		    r(obj1,p(np)),B,Ds,_,_) :-
    Hd = tree(r(hd,l(read_from_treebank(_,veel,_),_,W)),_,[]),
    lists:member(Hd,Ds).

user_transformation(r(obj1,p(ap)),B,Ds,
		    r(obj1,p(np)),B,Ds,_,_) :-
    Hd = tree(r(hd,l(read_from_treebank(_,weinig,_),_,W)),_,[]),
    lists:member(Hd,Ds).

surf_lemma(Word,'N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)',L,L) :-
    eigen(Word).
surf_lemma(Word,'N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)',L,L) :-
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

surf_lemma(Word,'N(eigen,ev,basis,genus,stan)','N(eigen,mv,basis)',_,L) :-
    pl_naam(Word,L).
surf_lemma(Word,'N(eigen,ev,basis,zijd,stan)','N(eigen,mv,basis)',_,L) :-
    pl_naam(Word,L).
surf_lemma(Word,'N(eigen,ev,basis,onz,stan)','N(eigen,mv,basis)',_,L) :-
    pl_naam(Word,L).


surf_lemma(Word,'N(eigen,ev,basis,genus,stan)','N(eigen,mv,basis)',L,L) :-
    pl_naam(Word).
surf_lemma(Word,'N(eigen,ev,basis,zijd,stan)','N(eigen,mv,basis)',L,L) :-
    pl_naam(Word).
surf_lemma(Word,'N(eigen,ev,basis,onz,stan)','N(eigen,mv,basis)',L,L) :-
    pl_naam(Word).


surf_lemma(Word,'N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)',L,L) :-
    eigen(Word).
surf_lemma(Word,'N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)',L,L) :-
    eigen(Word).


surf_lemma(Word,Rel,Pos0,Pos,Lem0,Lem) :-
    \+ Rel = mwp,
    surf(Word,Pos,Lem),
    \+ Pos0/Lem0 = Pos/Lem.

surf_lemma(Word,Rel,Pos0,Pos,Lem,Lem) :-
    \+ Rel = mwp,
    surf(Word,Pos),
    \+ Pos0 = Pos.
*/

user:query:-
    findall(L,lemma(L,_),Ls),
    (   Ls = [_|_]
    ->  query(Ls,Chars,[]),
	format("~n~nconverse '//node[@lemma=(~s)]'~n",[Chars])
    ;   true
    ),
    findall(M,eigen(M),Ms),
    (   Ms = [_|_]
    ->  query(Ms,Chars3,[]),
	format("~n~nconverse '//node[@word=(~s)]'~n",[Chars3])
    ;   true
    ),
    findall(S,( surf(S,_,_)
	      ; surf(S,_)
	      ; surf_lemma(S,_,_,_,_)
	      ), Ss),
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

mwu_postag(L,Deeleigen,L) :-
    flat(L),
    deeleigen(L,Deeleigen).

mwu_postag(L,Deeleigen,L) :-
    vreemd(L),
    vreemd(L,Deeleigen).

mwu_postag(L,Deeleigen,S) :-
    correct_tags(L,Deeleigen,S).

flat(_) :-
    fail.

vreemd(_) :-
    fail.

correct_tags(_,_,_) :-
    fail.

root_lemma(_,_,_) :- fail.

surf_lemma(_,_,_,_,_,_) :- fail.

surf_lemma(_,_,_,_,_) :- fail.

surf_lemma(_,_,_,_) :- fail.

surf_lemma(_,_,_) :- fail.

eigen(_):-
    fail.

pl_naam(_) :-
    fail.

pl_naam(_,_) :-
    fail.

het_naam(_):-
    fail.

de_naam(_) :-
    fail.

genus_naam(_) :-
    fail.

tag_lemma(_,_,_) :- fail.

lemma(_,_) :-
    fail.

mlemma(_,_) :-
    fail.

surf(_,_,_) :-
    fail.

surf(_,_) :-
    fail.


surf(Word,'N(eigen,ev,basis,zijd,stan)',Word) :-
    person(Word).

surf(Word,'N(eigen,ev,basis,onz,stan)',Word) :-
    org(Word).

surf(Bont,'N(soort,ev,basis,onz,stan)',Bont) :-
    het(Bont).
surf(Bont,'N(soort,ev,basis,zijd,stan)',Bont) :-
    de(Bont).
surf(Kort,'ADJ(nom,basis,zonder,zonder-n)',Kort) :-
    adj(Kort).

person(_):-fail.
org(_):-fail.
de(_) :- fail.
het(_):-fail.
adj(_):-fail.

%% TODO doelgroep- doel_groep
%% TODO genus in names
%% '//node[@genus="genus" and @rel="hd" and ../node[@rel="det" and @lemma=("de","het","dit","dat","die","deze")]]'
%% TODO soort genus, also compounds with soort
%%
%% *gevend
%% *lui/*lieden
%%
%%% Liege
%% gebaat zij
%%
%% niet- en niet_
%% non- en non_

surf_lemma(W,'ADJ(prenom,basis,met-e,stan)','WW(od,prenom,met-e)',_,L):-
    ende(W,_,L).

surf_lemma(W,'ADJ(prenom,basis,zonder)','WW(od,prenom,zonder)',_,L):-
    ende(_,W,L).
surf_lemma(W,'ADJ(vrij,basis,zonder)','WW(od,vrij,zonder)',_,L):-
    ende(_,W,L).

surf_lemma(W,'ADJ(prenom,basis,met-e,stan)','WW(vd,prenom,met-e)',_,L):-
    ge_de(W,_,L).
surf_lemma(W,'ADJ(nom,basis,met-e,zonder-n,stan)','WW(vd,nom,met-e,zonder-n)',_,L):-
    ge_de(W,_,L).

surf_lemma(W,'ADJ(prenom,basis,zonder)','WW(vd,prenom,zonder)',_,L):-
    ge_de(_,W,L).
surf_lemma(W,'ADJ(vrij,basis,zonder)','WW(vd,vrij,zonder)',_,L):-
    ge_de(_,W,L).

surf_lemma(W,_,'WW(od,nom,met-e,mv-n)',_,L) :-
    enden(W,L).

surf_lemma(W,_,'WW(vd,nom,met-e,mv-n)',_,L) :-
    gen(W,L).

surf_lemma(X,_,'SPEC(symb)',_,X) :-
    symb(X).

ende(_,_,_) :-
    fail.

ge_de(_,_,_) :-
    fail.

enden(_,_) :-
    fail.

gen(_,_) :-
    fail.

symb(_) :-
    fail.

%% tweebond
