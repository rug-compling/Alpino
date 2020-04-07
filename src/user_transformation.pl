:- module(alpino_user_transformation, [ user_transformation/8 ]).

:- user:set_flag(alpino_ds_version,'1.3').


%% --------------------------------------------------------------------------------------------- %%

user_transformation(r(Rel,i(X,Cat)),A,B,
                    r(Rel2,i(X,Cat2)),C,D,E,F) :-
    user_transformation(r(Rel,Cat),A,B,
                        r(Rel2,Cat2),C,D,E,F).

/*
user_transformation(r(REL,l(read_from_treebank(Az,Lem,'WW(od,vrij,zonder'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,Lem,'WW(od,vrij,zonder)'),Cat,W/[P0,P])),B,[],_,_).
*/

/*
user_transformation(r(REL,p(pp)),A,[Als0|Volgt],
		    r(REL,p(ppres)),A,[Als|Volgt],_,_) :-
    Als0   = tree(r(hd,l(read_from_treebank(Az,betreffende,'VZ(init)'),       pp,   Ws)),I,[]),
    Als    = tree(r(hd,l(read_from_treebank(Az,betreffen,'WW(od,vrij,zonder)'),ppres,Ws)),I,[]).
*/



/*
user_transformation(r(REL,p(mwu)),A,[Als0,Volgt0],
		    r(REL,p(mwu)),A,[Als,Volgt],String,_) :-
    Als0   = tree(r(mwp,l(read_from_treebank(Az,_,         _),               Cat,_        /[P0a,P0b])),I,[]),
    Als    = tree(r(mwp,l(read_from_treebank(Az,'Vlaamse','SPEC(deeleigen)'),Cat,'Vlaamse'/[P0a,P0b])),I,[]),
    alpino_treebank:get_root(P0a,P0b,String,'Vlaamse'),
    Volgt0 = tree(r(mwp,l(read_from_treebank(Bz,_,         _),                Cat2,_         /[Pa,Pb])),J,[]),
    Volgt  = tree(r(mwp,l(read_from_treebank(Bz,'Regering','SPEC(deeleigen)'),Cat2,'Regering'/[Pa,Pb])),J,[]),
    alpino_treebank:get_root(Pa,Pb,String,'Regering').
*/

/*
user_transformation(r(predc,p(predc)),A,Ds,
		    r(predc,p(pp)),A,Ds,_,_).
*/
/*
user_transformation(r(REL,p(mwu)),A,[Als0,Volgt0],
		    r(REL,p(cp)),A,[Als,Volgt],_,_) :-
    Als0   = tree(r(mwp,l(read_from_treebank(Az,als,'VG(onder)'),Cat,W)),I,[]),
    Als    = tree(r(cmp,l(read_from_treebank(Az,als,'VG(onder)'),Cat,W)),I,[]),
    Volgt0 = tree(r(mwp,l(read_from_treebank(Bz,volgen,'WW(pv,tgw,met-t)'),Cat2,W2)),J,[]),
    Volgt  = tree(r(body,l(read_from_treebank(Bz,volgen,'WW(pv,tgw,met-t)'),Cat2,W2)),J,[]).
*/
/*
user_transformation(r(REL,l(read_from_treebank(Az,eerder,'ADJ(vrij,basis,zonder)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,eerder,'ADJ(vrij,comp,zonder)'),Cat,W/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,verder,'ADJ(vrij,basis,zonder)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,verder,'ADJ(vrij,comp,zonder)'),Cat,W/[P0,P])),B,[],_,_).
*/

/*
user_transformation(r(top,p(top)),A,Ds0,
		    r(top,p(top)),A,Ds,_,_) :-
    Main = tree(r('--',p(du)),_,[TAG0,NUCL0]),
    TAG0 = tree(r(tag,l(read_from_treebank(PUNTAG,PUN,SPEC),SPECCAT,W/[P0,P])),C,[]),
    TAG  = tree(r('--',l(read_from_treebank(PUNTAG,PUN,SPEC),SPECCAT,W/[P0,P])),C,[]),
    NUCL0= tree(r(nucl,NUCLCAT),D,NUCLDS),
    NUCL = tree(r('--',NUCLCAT),D,NUCLDS),
    lists:select(Main,Ds0,Ds1),
    lists:member(PUN,['*','·']),
    Ds = [TAG,NUCL|Ds1].
*/

/*
user_transformation(r(Rel,p(rel)),A,Ds0,
		    r(Rel,p(rel)),A,Ds,_,_) :-
    Ds0 = [RHD0,BODY0],
    Ds  = [RHD, BODY],
    RHD0 = tree(r(rhd,i(X,Cat)),RHDI,RHDDS),
    RHD  = tree(r(rhd,Cat),RHDI,RHDDS),
    BODY0= tree(r(body,p(np)),NPI,[MOD,NPD1,NPD2|NPDS]),
    BODY = tree(r(body,p(np)),NPI,[NPD1,NPD2|NPDS]),
    MOD  = tree(r(mod,i(X)),_,[]).

user_transformation(r(Rel,p(rel)),A,Ds0,
		    r(Rel,p(rel)),A,Ds,_,_) :-
    Ds0 = [RHD0,BODY0],
    Ds  = [RHD, BODY],
    RHD0 = tree(r(rhd,i(X,Cat)),RHDI,RHDDS),
    RHD  = tree(r(rhd,Cat),RHDI,RHDDS),
    BODY0= tree(r(body,p(np)),_,[MOD,NPD]),
    NPD  = tree(r(hd,LEX),_,LEXDS),
    BODY = tree(r(body,LEX),_,LEXDS),  % for mwu LEXDS is non empty
    MOD  = tree(r(mod,i(X)),_,[]).
*/
/*
user_transformation(r(REL,l(read_from_treebank(Az,'Frans','ADJ(nom,basis,met-e,mv-n)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'Fransen', 'N(eigen,mv,basis)'),Cat,W/[P0,P])),B,[],_,_).
user_transformation(r(REL,l(read_from_treebank(Az,'Franse','ADJ(nom,basis,met-e,mv-n)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'Fransen', 'N(eigen,mv,basis)'),Cat,W/[P0,P])),B,[],_,_).
user_transformation(r(REL,l(read_from_treebank(Az,'Fransman','ADJ(nom,basis,met-e,mv-n)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'Fransen', 'N(eigen,mv,basis)'),Cat,W/[P0,P])),B,[],_,_).
user_transformation(r(REL,l(read_from_treebank(Az,'Frans','N(eigen,mv,basis)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'Fransen', 'N(eigen,mv,basis)'),Cat,W/[P0,P])),B,[],_,_).
user_transformation(r(REL,l(read_from_treebank(Az,'Franse','N(eigen,mv,basis)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'Fransen', 'N(eigen,mv,basis)'),Cat,W/[P0,P])),B,[],_,_).
user_transformation(r(REL,l(read_from_treebank(Az,'Fransman','N(eigen,mv,basis)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'Fransen', 'N(eigen,mv,basis)'),Cat,W/[P0,P])),B,[],_,_).


user_transformation(r(REL,l(read_from_treebank(Az,engelsen,'N(soort,mv,basis)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'Engelsen', 'N(eigen,mv,basis)'),Cat,W/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,'Engelsman','N(eigen,mv,basis)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'Engelsen', 'N(eigen,mv,basis)'),Cat,W/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,'Engels','ADJ(nom,basis,met-e,mv-n)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'Engelsen', 'N(eigen,mv,basis)'),Cat,W/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,'Engels','ADJ(nom,basis,zonder,mv-n)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'Engelsen', 'N(eigen,mv,basis)'),Cat,W/[P0,P])),B,[],_,_).


*/

/*
user_transformation(r(REL,l(read_from_treebank(Az,levend,'ADJ(prenom,basis,met-e,stan)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,leven, 'WW(od,prenom,zonder)'),Cat,W/[P0,P])),B,[],_,_).
user_transformation(r(REL,l(read_from_treebank(Az,levend,'ADJ(vrij,basis,zonder)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,leven, 'WW(od,vrij,zonder)'),Cat,W/[P0,P])),B,[],_,_).
*/

/*
user_transformation(r(REL,l(read_from_treebank(Az,commissie,'N(soort,ev,basis,zijd,stan)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'Commissie','N(eigen,ev,basis,zijd,stan)'),Cat,W/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,'Commissie').
*/

/*
user_transformation(r(REL,p(inf)),B,Ds0,
		    r(REL,p(inf)),B,[MOD|Ds],_String,_) :-
    MOD0 = tree(r(hd,l(read_from_treebank(Az,Bz,'WW(inf,nom,zonder,zonder-n)'),Cat,R)),AA,BB),
    MOD  = tree(r(hd,l(read_from_treebank(Az,Bz,'WW(inf,vrij,zonder)'),Cat,R)),AA,BB),
    lists:select(MOD0,Ds0,Ds).

user_transformation(r(REL,p(np)),B,Ds0,
		    r(REL,p(np)),B,[MOD|Ds],_String,_) :-
    MOD0 = tree(r(hd,l(read_from_treebank(Az,Bz,'WW(inf,vrij,zonder)'),Cat,R)),AA,BB),
    MOD  = tree(r(hd,l(read_from_treebank(Az,Bz,'WW(inf,nom,zonder,zonder-n)'),Cat,R)),AA,BB),
    lists:select(MOD0,Ds0,Ds).
*/

/*
user_transformation(r(REL,l(read_from_treebank(Az,Bz,'WW(inf,vrij,zonder)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,Bz,'WW(inf,nom,zonder,zonder-n)'),Cat,W/[P0,P])),B,[],_,_) :-
    lists:member(REL,[cnj,predc]).
*/
/*
user_transformation(r(mod,p(ti)),B, [TE, tree(r(body,l(read_from_treebank(Az,Bz,'WW(inf,vrij,zonder)'),Cat,W/[P0,P])),ZA,ZB)],
		    r(mod,p(ti)),B, [TE, tree(r(body,l(read_from_treebank(Az,Bz,'WW(inf,prenom,zonder)'),Cat,W/[P0,P])),ZA,ZB)],_,_).
*/
/*
user_transformation(r(REL,l(read_from_treebank(Az,enig,'ADJ(prenom,basis,met-e,stan)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,enig,'VNW(onbep,det,stan,prenom,met-e,rest)'),Cat,W/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,enig,'ADJ(nom,basis,met-e,zonder-n,stan)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,enig,'VNW(onbep,det,stan,nom,met-e,zonder-n)'),Cat,W/[P0,P])),B,[],_,_).
*/

/*
user_transformation(r(REL,l(read_from_treebank(Az,als,'VZ(init)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,als,'VG(onder)'),Cat,W/[P0,P])),B,[],_,_).
*/

/*
user_transformation(r(REL,l(read_from_treebank(Az,als,'VG(onder)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,als,'VZ(init)'),Cat,W/[P0,P])),B,[],_,_).
*/


/*
user_transformation(r(REL,l(read_from_treebank(Az,driekwart,'ADJ(prenom,basis,zonder)'),Cat,W/[P0,P])),B,[]
		    r(REL,l(read_from_treebank(Az,driekwart,'TW(hoofd,prenom,stan)'),Cat,W/[P0,P])),B,[],_,_).
user_transformation(r(REL,l(read_from_treebank(Az,driekwart,'ADJ(nom,basis,zonder,zonder-n)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,driekwart,'TW(hoofd,nom,zonder-n,basis)'),Cat,W/[P0,P])),B,[],_,_).
user_transformation(r(REL,l(read_from_treebank(Az,driekwart,'ADJ(vrij,basis,zonder)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,driekwart,'TW(hoofd,vrij)'),Cat,W/[P0,P])),B,[],_,_).
*/


/*
user_transformation(r(REL,l(read_from_treebank(Az,bepaald,'ADJ(prenom,basis,met-e,stan)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,bepalen,'WW(vd,prenom,met-e)'),Cat,W/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,bepaald,'ADJ(prenom,basis,zonder)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,bepalen,'WW(vd,prenom,zonder)'),Cat,W/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,bepaald,'ADJ(vrij,basis,zonder)'),Cat,W/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,bepalen,'WW(vd,vrij,zonder)'),Cat,W/[P0,P])),B,[],_,_).
*/


/*
user_transformation(r(cmp,l(read_from_treebank(Az,Bz,'BW()'),Cat,eerder/[P0,P])),B,[],
		    r(cmp,l(read_from_treebank(Az,Bz,'ADJ(vrij,basis,zonder)'),Cat,eerder/[P0,P])),B,[],_,_).

user_transformation(r(cmp,l(read_from_treebank(Az,Bz,'ADJ(vrij,comp,zonder)'),Cat,eerder/[P0,P])),B,[],
		    r(cmp,l(read_from_treebank(Az,Bz,'ADJ(vrij,basis,zonder)'),Cat,eerder/[P0,P])),B,[],_,_).

user_transformation(r(cmp,l(read_from_treebank(Az,Bz,'ADJ(prenom,comp,met-e,stan)'),Cat,eerder/[P0,P])),B,[],
		    r(cmp,l(read_from_treebank(Az,Bz,'ADJ(prenom,basis,met-e,stan)'),Cat,eerder/[P0,P])),B,[],_,_).

*/


%user_transformation(r(REL,p(CAT)),B,Ds,
%		    r(REL,p(mwu)),B,Ds,_,_) :-
%    atom(CAT),
%    atom_concat('mwu([',_,CAT).

%% NB: structure of first argument of l(...) of lexical nodes:
%% read_from_treebank/4: for machine generated treebanks
%% read_from_treebank/3: for manually corrected treebanks

%user_transformation(r(REL,l(read_from_treebank(Az,Bz,Pos,TAG1),Cat,Root/[P0,P])),B,[],
%		    r(REL,l(read_from_treebank(Az,Bz,Pos,TAG2),Cat,Root/[P0,P])),B,[],_,_) :-
%    correct_postag(TAG1,TAG2).

/*
user_transformation(r(REL,l(read_from_treebank(Az,Bz1,TAG1),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,Bz2,TAG2),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,J),
    correct_postag_lemma(J,Bz1,Bz2,TAG1,TAG2).


correct_postag_lemma(_,nieuw,nieuws,Tag,Tag) :-
    atom_concat('N(',_,Tag).
*/
/*
user_transformation(r(REL,l(read_from_treebank(Az,Bz,Pos1),Cat,Stem/Of)),B,[],
		    r(REL,l(read_from_treebank(Az,Bz,Pos2),Cat,Stem/Of)),B,[],_,_) :-
    map_stem(Stem,Pos1,Pos2).

map_stem('Rekenkamer','N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)').
*/

/*
user_transformation(r(REL,p(mwu)),B,[Ver0,Weg0],
		    r(REL,p(advp)),B,[Ver,Weg] ,_,_) :-
    Ver0 = tree(r(mwp,l(read_from_treebank(Az,Bz,_ ),Cat,ver/W)),Bt,[]),
    Ver  = tree(r(mod,l(read_from_treebank(Az,Bz,'ADJ(vrij,basis,zonder)'),Cat,ver/W)),Bt,[]),
    Weg0 = tree(r(mwp,l(read_from_treebank(XAz,XBz, _),XCat,weg/XW)),XBt,[]),
    Weg  = tree(r(hd,l(read_from_treebank(XAz,XBz,'BW()'),XCat,weg/XW)),XBt,[]).

*/
/*
user_transformation(r(REL,p(mwu)),B,[Code0,CodeRest],
		    r(REL,p(mwu)),B,[Code,CodeRest] ,_,_) :-
    Code0 = tree(r(mwp,l(read_from_treebank(Az,Bz,_),Cat,Num/W)),Bt,[]),
    Code  = tree(r(mwp,l(read_from_treebank(Az,Bz,'TW(hoofd,vrij)'),Cat,Num/W)),Bt,[]),
    atom_codes(Num,[C1,C2,P,C3,C4]),
    p(P),
    n(C1), n(C2), n(C3), n(C4),
    CodeRest   = tree(r(mwp,l(_,_,uur/_)),_,[]).

p(46).
p(48).

n(C) :-
    C >= 48, C =< 57.

c(C) :-
    C >= 65, C =< 90.

*/

/*
user_transformation(r(REL,p(mwu)),B,[Code0,CodeRest],
		    r(REL,p(mwu)),B,[Code,CodeRest] ,_,_) :-
    Code0 = tree(r(mwp,l(read_from_treebank(Az,Bz,_),Cat,Num/W)),Bt,[]),
    Code  = tree(r(mwp,l(read_from_treebank(Az,Bz,'TW(hoofd,vrij)'),Cat,Num/W)),Bt,[]),
    atom_codes(Num,[C1,C2,C3,C4]),
    n(C1), n(C2), n(C3), n(C4),
    CodeRest   = tree(r(mwp,l(_,_,Uurlemma/_)),_,[]),
    atom_codes(Uurlemma,[C5,C6]),
    c(C5), c(C6).

n(C) :-
    C >= 48, C =< 57.

c(C) :-
    C >= 65, C =< 90.
*/

/*
user_transformation(r(REL,p(mwu)),B,[Tien0,Uur|Ds],
		    r(REL,p(mwu)),B,[Tien,Uur|Ds] ,_,_) :-
    Tien0 = tree(r(mwp,l(read_from_treebank(Az,Bz,'TW(hoofd,prenom,stan)'),Cat,Num/W)),Bt,[]),
    Tien  = tree(r(mwp,l(read_from_treebank(Az,Bz,'TW(hoofd,vrij)'),Cat,Num/W)),Bt,[]),
    Uur   = tree(r(mwp,l(_,_,Uurlemma/_)),_,[]),
    lists:member(Uurlemma,[januari,februari,maart,april,mei,juni,juli,augustus,september,oktober,november,december,uur]),
    format(user_error,"~w ~w ",[Num,Uurlemma]).
*/

/*
user_transformation(r(mwp,l(read_from_treebank(Az,Bz,'SPEC(symb)'),Cat,I/W)),B,[],
		    r(mwp,l(read_from_treebank(Az,Bz,'SPEC(deeleigen)'),Cat,I/W)),B,[],_,_) :-
    (   lists:member(I,['I','II','III','IV','V','VI','VII','VIII','IX','X'])
    ->   true
    ;   format(user_error,"~w~n",[I]),
	fail
    ).
*/
/*
user_transformation(r(REL,l(read_from_treebank(Az,Bz,'N(soort,ev,basis,zijd,stan)'),Cat,keer/W)),B,[],
		    r(REL,l(read_from_treebank(Az,Bz,'N(soort,ev,basis,genus,stan)'),Cat,keer/W)),B,[],_,_).
user_transformation(r(REL,l(read_from_treebank(Az,Bz,'N(soort,ev,basis,onz,stan)'),Cat,keer/W)),B,[],
		    r(REL,l(read_from_treebank(Az,Bz,'N(soort,ev,basis,genus,stan)'),Cat,keer/W)),B,[],_,_).
*/

/*
user_transformation(r(REL,p(mwu)),B,Ds0,
		    r(REL,p(mwu)),B,Ds ,_,_) :-
    roots(Ds0,Roots),
    correct_tags(Roots,Tags),
    assign_tags(Tags,Ds0,Ds).

roots([],[]).
roots([tree(r(mwp,l(_,_,Root/_)),_,_)|Trees],[Root|Roots]) :-
    roots(Trees,Roots).

assign_tags([],[],[]).
assign_tags([Tag|Tags],[Tree0|Trees0],[Tree|Trees]) :-
    assign_tag(Tag,Tree0,Tree),
    assign_tags(Tags,Trees0,Trees).

assign_tag(Tag,
	   tree(r(mwp,l(read_from_treebank(Az,Bz,_),Cat,W)),B,[]),
	   tree(r(mwp,l(read_from_treebank(Az,Bz,Tag),Cat,W)),B,[])
	  ).	   
      

correct_tags(Roots,Tags) :-
    correct_tags(Roots),
    hdrug_util:concat_all(Roots,Atom,' '),
    alpino_cgn_postags:mwu_postag(Atom,Tags).

correct_tags([goed,avond]).
*/


/*
user_transformation(r(app,l(read_from_treebank(Az,Bz,'TW(hoofd,vrij)'),Cat1,In/InRest)),_,[],
 		    r(mod,l(read_from_treebank(Az,Bz,'TW(hoofd,vrij)'),Cat1,In/InRest)),_,[],String,_) :-
    
     InRest = [P0,P],
     alpino_treebank:get_root(P0,P,String,WordP),
     format(user_error,"app => mod for ~w~n",[WordP]).
*/

/*
user_transformation(r(svp,l(read_from_treebank(Az,Bz,'BW()'),   Cat1,In/InRest)),_,[],
 		    r(svp,l(read_from_treebank(Az,Bz,'VZ(fin)'),Cat1,In/InRest)),_,[],String,_) :-
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,voort).
user_transformation(r(svp,l(read_from_treebank(Az,Bz,'BW()'),   Cat1,In/InRest)),_,[],
 		    r(svp,l(read_from_treebank(Az,Bz,'VZ(fin)'),Cat1,In/InRest)),_,[],String,_) :-
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,mede).
*/

/*
user_transformation(r(REL,l(read_from_treebank(Az,Bz,'ADJ(vrij,basis,zonder)'),Cat1,In/InRest)),_,[],
 		    r(REL,l(read_from_treebank(Az,Bz,'BW()'                  ),Cat1,In/InRest)),_,[],String,_) :-
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,grotendeels).

user_transformation(r(REL,l(read_from_treebank(Az,Bz,'WW(inf,nom,zonder,zonder-n)'),Cat1,In/InRest)),_,[],
 		    r(REL,l(read_from_treebank(Az,Bz,'N(soort,ev,basis,onz,stan)' ),Cat1,In/InRest)),_,[],String,_) :-
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,bestaan).

user_transformation(r(REL,l(read_from_treebank(Az,Bz,'WW(inf,nom,zonder,zonder-n)'),Cat1,In/InRest)),_,[],
 		    r(REL,l(read_from_treebank(Az,Bz,'N(soort,ev,basis,onz,stan)' ),Cat1,In/InRest)),_,[],String,_) :-
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,optreden).
*/

/*
user_transformation(r(REL,l(read_from_treebank(Az,Bz,POSTAG0),Cat1,In/InRest)),_,[],
 		    r(REL,l(read_from_treebank(Az,Bz,'SPEC(symb)'),Cat1,In/InRest)),_,[],String,_) :-
    POSTAG0 \== 'SPEC(symb)',
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,Word),
    sym(Word).

sym('cm.').
sym(cm).
sym(km).
sym(mg).
sym('mm.').
sym('m.').
sym('km²').
sym(km2).
sym('ha.').
sym('ha').
sym(sec).
sym(m).
sym(mm).
sym('CO2').


user_transformation(r(REL,l(read_from_treebank(Az,Bz,'SPEC(afk)'),Cat1,In/InRest)),_,[],
 		    r(REL,l(read_from_treebank(Az,Bz,POSTAG),Cat1,In/InRest)),_,[],String,_) :-
     InRest = [P0,P],
     alpino_treebank:get_root(P0,P,String,Word),
     afk(Word,POSTAG).

afk('CA','SPEC(symb)').
afk(tel,'N(soort,ev,basis,zijd,stan)').
afk('TBS','N(soort,ev,basis,zijd,stan)').
afk('V&D','N(eigen,ev,basis,zijd,stan)').
afk('AVRO','N(eigen,ev,basis,zijd,stan)').
afk('ANVR','N(eigen,ev,basis,zijd,stan)').
afk('S-FOR','N(eigen,ev,basis,zijd,stan)').
afk('DCA','N(eigen,ev,basis,zijd,stan)').
afk('KLJ','N(eigen,ev,basis,zijd,stan)').
afk('AFMP','N(eigen,ev,basis,zijd,stan)').
afk('ADSL','N(eigen,ev,basis,zijd,stan)').
afk('SP-PS','N(eigen,ev,basis,zijd,stan)').
afk('BSP-PSB','N(eigen,ev,basis,zijd,stan)').
afk('KNIL','N(eigen,ev,basis,zijd,stan)').
afk('U.S.A.','N(eigen,ev,basis,zijd,stan)').
afk('DSI','N(eigen,ev,basis,zijd,stan)').
afk('ENCI','N(eigen,ev,basis,zijd,stan)').
afk('A.S.O.','N(eigen,ev,basis,zijd,stan)').
afk('UCI','N(eigen,ev,basis,zijd,stan)').
afk('P-R.','N(eigen,ev,basis,zijd,stan)').
afk('P-R','N(eigen,ev,basis,zijd,stan)').
afk('GAM','N(eigen,ev,basis,zijd,stan)').
afk('SFOR','N(eigen,ev,basis,zijd,stan)').
afk('VMBO','N(eigen,ev,basis,onz,stan)').
afk('CMO','N(eigen,ev,basis,onz,stan)').
afk('VB','N(eigen,ev,basis,onz,stan)').
afk('CDh','N(eigen,ev,basis,onz,stan)').
afk('O.M.','N(eigen,ev,basis,onz,stan)').
afk('ROC','N(eigen,ev,basis,onz,stan)').
afk('GAK','N(eigen,ev,basis,onz,stan)').
afk('KNIL','N(eigen,ev,basis,onz,stan)').
afk('U-W-V','N(eigen,ev,basis,onz,stan)').
afk('Nasa','N(eigen,ev,basis,zijd,stan)').
afk('DSM','N(eigen,ev,basis,zijd,stan)').
afk('VNO','N(eigen,ev,basis,zijd,stan)').
afk('OSF','N(eigen,ev,basis,zijd,stan)').
afk('OUNH','N(eigen,ev,basis,zijd,stan)').
afk('NAM','N(eigen,ev,basis,zijd,stan)').
afk('IND','N(eigen,ev,basis,zijd,stan)').
afk('AVRO','N(eigen,ev,basis,zijd,stan)').
afk('Avro','N(eigen,ev,basis,zijd,stan)').
afk('MAMAC','N(eigen,ev,basis,onz,stan)').
afk('MUHKA','N(eigen,ev,basis,onz,stan)').
afk('NFI','N(eigen,ev,basis,onz,stan)').
afk('ECN','N(eigen,ev,basis,onz,stan)').
afk('Dui','N(eigen,ev,basis,onz,stan)').
afk('Bel','N(eigen,ev,basis,onz,stan)').
afk('Fra','N(eigen,ev,basis,onz,stan)').
afk('Est','N(eigen,ev,basis,onz,stan)').
afk('P.R.T','N(eigen,ev,basis,onz,stan)').
afk('cda','N(eigen,ev,basis,onz,stan)').
afk('KMSKA','N(eigen,ev,basis,onz,stan)').
afk('MoMu','N(eigen,ev,basis,onz,stan)').
afk('EHBO','N(eigen,ev,basis,zijd,stan)').
afk('PWO','N(eigen,ev,basis,genus,stan)').
afk('T.H.G.','N(eigen,ev,basis,genus,stan)').
afk('NMA','N(eigen,ev,basis,zijd,stan)').
afk('NMA.','N(eigen,ev,basis,zijd,stan)').
afk('veb','N(eigen,ev,basis,zijd,stan)').
afk('ICA','N(eigen,ev,basis,zijd,stan)').
afk('Affa','N(eigen,ev,basis,zijd,stan)').
afk('FNP','N(eigen,ev,basis,zijd,stan)').
afk('BHR','N(eigen,ev,basis,zijd,stan)').
afk('IRNA','N(eigen,ev,basis,genus,stan)').
afk('C&C','N(eigen,ev,basis,genus,stan)').
afk('DC','N(eigen,ev,basis,genus,stan)').
afk('LD','N(eigen,ev,basis,zijd,stan)').
afk('LO','N(eigen,ev,basis,zijd,stan)').
afk('NHA','N(eigen,ev,basis,zijd,stan)').
afk('OZH','N(eigen,ev,basis,zijd,stan)').
afk('Sp.a','N(eigen,ev,basis,zijd,stan)').
afk('sp.a','N(eigen,ev,basis,zijd,stan)').
afk('sp.a.','N(eigen,ev,basis,zijd,stan)').
afk('iD21','N(eigen,ev,basis,zijd,stan)').
afk('UMTS','N(eigen,ev,basis,zijd,stan)').
afk('MIVB','N(eigen,ev,basis,zijd,stan)').
afk('TGV','N(eigen,ev,basis,zijd,stan)').
afk('ICE','N(eigen,ev,basis,zijd,stan)').
afk('WHO','N(eigen,ev,basis,zijd,stan)').
afk('NVM','N(eigen,ev,basis,zijd,stan)').
afk('UMTS','N(eigen,ev,basis,zijd,stan)').
afk('K3','N(eigen,ev,basis,zijd,stan)').
afk('Nasa','N(eigen,ev,basis,zijd,stan)').
afk('VGT','N(eigen,ev,basis,zijd,stan)').
afk('LSFB','N(eigen,ev,basis,zijd,stan)').
afk('BUB','N(eigen,ev,basis,zijd,stan)').
afk('J.F.K.','N(eigen,ev,basis,zijd,stan)').
afk('TBS','N(eigen,ev,basis,zijd,stan)').
afk('BB','N(eigen,ev,basis,zijd,stan)').
afk('UDC','N(eigen,ev,basis,zijd,stan)').
afk('ICA','N(eigen,ev,basis,zijd,stan)').
afk('MP','N(eigen,ev,basis,zijd,stan)').
afk('IMC-FMC','N(eigen,ev,basis,zijd,stan)').
afk('KSJ-KSA-VKSJ','N(eigen,ev,basis,zijd,stan)').
afk('resp.','ADJ(vrij,basis,zonder)').
afk('j.l.','ADJ(vrij,basis,zonder)').
afk('H.I.V.','N(soort,ev,basis,genus,stan)').
afk('Inc','N(soort,ev,basis,genus,stan)').
afk('CR','N(soort,ev,basis,genus,stan)').
afk('SARS','N(soort,ev,basis,zijd,stan)').
afk('AIDS','N(soort,ev,basis,zijd,stan)').
afk(ald,'BW()').
afk(kath,'N(soort,mv,basis)').
afk(lib,'N(soort,mv,basis)').
afk('Nr.','N(soort,ev,basis,onz,stan)').
afk('b.v.','BW()').
afk(pg,'N(soort,ev,basis,zijd,stan)').
afk(blz,'N(soort,mv,basis)').
afk('Ad','VZ(init)').


afk('sp.a','N(eigen,ev,basis,zijd,stan)').
afk('VR','N(eigen,ev,basis,zijd,stan)').
afk('ISS','N(eigen,ev,basis,onz,stan)').
afk('WW','N(soort,ev,basis,zijd,stan)').
afk('zgn.','ADJ(prenom,basis,met-e,stan)').
afk('ESA','N(eigen,ev,basis,zijd,stan)').
afk('GGC','N(eigen,ev,basis,zijd,stan)').
afk('PKU','N(eigen,ev,basis,zijd,stan)').
afk('RAI','N(eigen,ev,basis,zijd,stan)').
afk('EVA','N(eigen,ev,basis,zijd,stan)').
afk('FDF','N(eigen,ev,basis,zijd,stan)').
afk('PJU-PDB','N(eigen,ev,basis,zijd,stan)').
afk('FN','N(eigen,ev,basis,onz,stan)').
afk('AP','N(eigen,ev,basis,genus,stan)').
afk('PR','SPEC(symb)').
afk('LW','SPEC(symb)').
afk('DLS','SPEC(symb)').
afk('kg.','SPEC(symb)').
afk('C.','SPEC(symb)').
afk('EG','SPEC(symb)').
afk('MG','SPEC(symb)').
afk('R&B','N(soort,ev,basis,zijd,stan)').
afk('NPS','N(soort,ev,basis,zijd,stan)').
afk('LPG','N(soort,ev,basis,zijd,stan)').
afk(fr,'N(soort,ev,basis,onz,stan)').
afk('NIS','N(soort,ev,basis,genus,stan)').
afk('GB','N(soort,ev,basis,genus,stan)').
*/
/*
user_transformation(r(REL,l(read_from_treebank(Az,Bz,Old),Cat1,In/InRest)),_,[],
 		    r(REL,l(read_from_treebank(Az,Bz,New),Cat1,In/InRest)),_,[],String,_) :-
    
     InRest = [P0,P],
     alpino_treebank:get_root(P0,P,String,WordP),
     map_postag(WordP,Old,New),
     Old \== New.

map_postag('VWS',_,'N(eigen,ev,basis,genus,stan)').
map_postag('VROM',_,'N(eigen,ev,basis,genus,stan)').

map_postag('VN',_,'N(eigen,mv,basis)').

map_postag(west,_,'ADJ(vrij,basis,zonder)').
map_postag(oost,_,'ADJ(vrij,basis,zonder)').
map_postag(zuid,_,'ADJ(vrij,basis,zonder)').
map_postag(noord,_,'ADJ(vrij,basis,zonder)').
map_postag('West',_,'ADJ(vrij,basis,zonder)').
map_postag('Oost',_,'ADJ(vrij,basis,zonder)').
map_postag('Noord',_,'ADJ(vrij,basis,zonder)').
map_postag('Zuid',_,'ADJ(vrij,basis,zonder)').

map_postag('Ahold','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('IBM','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('IBM','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Heineken','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Heineken','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Philips','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Philips','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Unilever','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Unilever','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Vodafone','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Vodafone','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Interbrew','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Interbrew','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Sabena','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Sabena','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Shell','N(eigen,ev,basis,genus,stan)','N(eigen,ev,basis,zijd,stan)').
map_postag('EUAIN','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('Ecolo','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('Nature','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Nature','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Mannesmann','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Mannesmann','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Vinnof','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Vinnof','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Rituals','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Rituals','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('NUON','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('NUON','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Nokia','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Nokia','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Mobistar','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Mobistar','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('IRIS','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('IRIS','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Interpay','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Interpay','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').

map_postag('Science','N(eigen,ev,basis,genus,stan)','N(eigen,ev,basis,zijd,stan)').
map_postag('Science','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)').
map_postag('Prive','N(eigen,ev,basis,genus,stan)','N(eigen,ev,basis,zijd,stan)').


map_postag('Oekraïne','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)').
map_postag(medicijngebruik,'N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)').
map_postag(douanebeleid,'N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)').
map_postag(handelsbeleid,'N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)').
map_postag('Kuifje','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)').
map_postag('Google','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Google','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Fortis','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Fortis','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Yukos','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Yukos','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Laurus','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Laurus','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Smead','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Smead','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Belga','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Belga','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Al-Qaida','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Al-Qaida','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Whitehall','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Whitehall','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Warner','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Warner','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').

map_postag('AVBB','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)').
map_postag('ABVV','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').

map_postag('Reisadvies','N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)').
map_postag(subsidie,'N(soort,ev,basis,genus,stan)','N(soort,ev,basis,zijd,stan)').
map_postag(leeshandicap,'N(soort,ev,basis,onz,stan)','N(soort,ev,basis,zijd,stan)').
map_postag(epilepsieaanval,'N(soort,ev,basis,onz,stan)','N(soort,ev,basis,zijd,stan)').
map_postag('Sinaï','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)').
map_postag('Giscard','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)').
map_postag('UGent','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)').

map_postag('DDT','N(soort,ev,basis,onz,stan)','N(soort,ev,basis,zijd,stan)').
map_postag('DDT','N(soort,ev,basis,genus,stan)','N(soort,ev,basis,zijd,stan)').
map_postag('Lee','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)').
map_postag(telefoonnummer,'N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)').
map_postag(braille,'N(soort,ev,basis,genus,stan)','N(soort,ev,basis,onz,stan)').
map_postag(astma,'N(soort,ev,basis,onz,stan)','N(soort,ev,basis,zijd,stan)').
map_postag(astma,'N(soort,ev,basis,genus,stan)','N(soort,ev,basis,zijd,stan)').

map_postag('IB-Groep','N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)').
map_postag('HR-ketel','N(eigen,ev,basis,onz,stan)','N(soort,ev,basis,zijd,stan)').
map_postag('EU-kader','N(eigen,ev,basis,onz,stan)','N(soort,ev,basis,onz,stan)').
map_postag('GLB','N(eigen,ev,basis,onz,stan)','N(soort,ev,basis,onz,stan)').
map_postag('BTW','N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)').
map_postag('AOW','N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)').

map_postag('Hamas','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)').
map_postag('Hamas','N(eigen,ev,basis,genus,stan)','N(eigen,ev,basis,zijd,stan)').

map_postag('EcoConsult','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('EcoConsult','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Dassault','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Dassault','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Agusta','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Agusta','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('DAT','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('DAT','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Word','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Word','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Word','N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Word','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').

map_postag('Dexia','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Dexia','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Vivant','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Vivant','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Euronext','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Euronext','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Enron','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('Enron','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('entecavir','N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,genus,stan)').
map_postag('entecavir','N(soort,ev,basis,onz,stan)','N(soort,ev,basis,genus,stan)').

map_postag('ECTS','N(soort,ev,basis,onz,stan)','N(soort,mv,basis)').
map_postag('ECTS','N(eigen,ev,basis,onz,stan)','N(soort,mv,basis)').

map_postag('CBR','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('CBR','N(soort,ev,basis,genus,stan)','N(eigen,ev,basis,onz,stan)').

map_postag('DG','N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)').
map_postag('DG','N(soort,ev,basis,onz,stan)','N(soort,ev,basis,zijd,stan)').

map_postag('coma','N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)').


map_postag(',','SPEC(symb)','LET()').
%map_postag(',','LET()','SPEC(symb)').

map_postag('ISS','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('ISS','N(eigen,ev,basis,genus,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('Juventus','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('Juventus','N(eigen,ev,basis,genus,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('4FM','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('4FM','N(eigen,ev,basis,genus,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('Tennessee','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('Texas','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('Virginia','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('Florida','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('Alabama','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('FDF','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('Georgia','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('Maasdijk','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').
map_postag('Lincoln','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)').

map_postag('Vroenhof','N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)').

map_postag('ICT','N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)').

map_postag('SN','N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)').
map_postag('SN','N(eigen,ev,basis,genus,stan)','N(soort,ev,basis,zijd,stan)').

map_postag('Nobelprijs','N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)').

map_postag('FOD','N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)').
map_postag('EZ','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('EZ','N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('EZ','N(soort,ev,basis,genus,stan)','N(eigen,ev,basis,genus,stan)').
map_postag('FOD','SPEC(deeleigen)','N(soort,ev,basis,zijd,stan)').
map_postag('Justitie','SPEC(deeleigen)','N(soort,ev,basis,zijd,stan)').
*/

/*
user_transformation(r(predc,p(ap)),B,Ds0,
		    r(predc,p(ap)),B,[Eens|Ds],String,_) :-
    Eens0 =  tree(r(hd,l(read_from_treebank(X,Y,'BW()'),Z,I/[P0,P])),J,[]),
    Eens  =  tree(r(hd,l(read_from_treebank(X,Y,'ADJ(vrij,basis,zonder)'),Z,I/[P0,P])),J,[]),
    lists:select(Eens0,Ds0,Ds),
    alpino_treebank:get_root(P0,P,String,eens).
*/

/*
user_transformation(r(REL,l(read_from_treebank(X,LEMMA0,'SPEC(deeleigen)'),Z,I/[P0,P])),B,DS,
		    r(REL,l(read_from_treebank(X,LEMMA,POS),Z,I/[P0,P])),B,DS,
		    String,_) :-
    \+ REL = mwp,
    alpino_treebank:get_root(P0,P,String,WordP),
    (   WordP == 'WK'
    ->  POS = 'N(eigen,ev,basis,onz,stan)',
	LEMMA0=LEMMA
    ;   (   alpino_lex:lexicon(PT,LEMMA,[WordP],[],_),
	    map_tag(PT,POS0),
	    (   POS0 == 'N(eigen,ev,basis,genus,stan)'
	    ->  try_dehet(P0,String,POS)
	    ;   POS0 = POS
	    )
	;   \+ alpino_lex:lexicon(PT,_,[WordP],[],_),
	    POS0 = 'N(eigen,ev,basis,genus,stan)',
	    try_dehet(P0,String,POS),
	    LEMMA0=LEMMA
	)
    ).

try_dehet(P,String,POS) :-
    P0 is P-1,
    alpino_treebank:get_root(P0,P,String,DeHet),
    (   DeHet == de
    ->  POS = 'N(eigen,ev,basis,zijd,stan)'
    ;   DeHet == het
    ->  POS = 'N(eigen,ev,basis,onz,stan)'
    ;   POS = 'N(eigen,ev,basis,genus,stan)'
    ).


map_tag(tag,                      'SPEC(symb)').

map_tag(noun(het,_,sg),           'N(eigen,ev,basis,onz,stan)').
map_tag(noun(de,_,sg),            'N(eigen,ev,basis,zijd,stan)').
map_tag(noun(_,_,pl),             'N(eigen,mv,basis)').
map_tag(name_determiner(pron,_),  'N(eigen,ev,basis,gen)').
map_tag(proper_name(sg),          'N(eigen,ev,basis,genus,stan)').
map_tag(proper_name(sg,'LOC'),    'N(eigen,ev,basis,onz,stan)').
map_tag(proper_name(sg,'PER'),    'N(eigen,ev,basis,zijd,stan)').
map_tag(proper_name(sg,'ORG'),    'N(eigen,ev,basis,onz,stan)').

map_tag(PT,_) :-
    format(user_error,"~w ~n",[PT]), fail.

*/
/*
user_transformation(r(REL,p(ap)),   B,Ds,
		    r(REL,p(ppart)),B,Ds,String,_) :-
    lists:member(tree(r(hd,l(read_from_treebank(_,_,Posz),_,_/[P0,P])),_,[]),Ds),
    atom_concat('WW(vd',_,Posz),
    alpino_treebank:get_root(P0,P,String,WordP),
    \+ WordP == geboren,
    format(user_error,"~w ~w~n",[WordP,Posz]).

user_transformation(r(REL,p(ppart)),   B,Ds,
		    r(REL,p(ap)),B,Ds,String,_) :-
    lists:member(tree(r(hd,l(read_from_treebank(_,_,Posz),_,_/[P0,P])),_,[]),Ds),
    atom_concat('ADJ(',_,Posz),
    alpino_treebank:get_root(P0,P,String,WordP),
    \+ WordP == geboren,
    format(user_error,"~w ~w~n",[WordP,Posz]).

user_transformation(r(REL,p(ap)),   B,Ds,
		    r(REL,p(ppres)),B,Ds,String,_) :-
    lists:member(tree(r(hd,l(read_from_treebank(_,_,Posz),_,_/[P0,P])),_,[]),Ds),
    atom_concat('WW(od',_,Posz),
    alpino_treebank:get_root(P0,P,String,WordP),
    format(user_error,"~w ~w~n",[WordP,Posz]).

*/

/*
user_transformation(r(det,l(read_from_treebank(Az,Bz,'SPEC(deeleigen)'),Cat1,In/InRest)),_,[],
 		    r(det,l(read_from_treebank(Az,Bz,'N(eigen,ev,basis,gen)'),Cat1,In/InRest)),_,[],String,_) :-
     InRest = [P0,P],
     alpino_treebank:get_root(P0,P,String,WordP),
     format(user_error,"deeleigen => name for ~w~n",[WordP]).

user_transformation(r(REL,l(read_from_treebank(Az,Bz,'SPEC(deeleigen)'),Cat1,In/InRest)),_,[],
 		    r(REL,l(read_from_treebank(Az,Bz,'N(eigen,ev,basis,genus,stan)'),Cat1,In/InRest)),_,[],String,_) :-
     \+ REL==mwp,
     InRest = [P0,P],
     alpino_treebank:get_root(P0,P,String,WordP),
     format(user_error,"deeleigen => name for ~w~n",[WordP]).

*/
/*
user_transformation(r(hd,l(read_from_treebank(Az,Bz,'SPEC(deeleigen)'),Cat1,In/InRest)),_,[],
 		    r(hd,l(read_from_treebank(Az,Bz,'VZ(init)'),       Cat1,In/InRest)),_,[],String,_) :-
     InRest = [P0,P],
     alpino_treebank:get_root(P0,P,String,van).

user_transformation(r(REL,CAT),B,Ds0,
		    r(REL,CAT),B,Ds,_String,_) :-
    TalVan = tree(r(det,p(mwu)),_,[Tal,Van]),
    Tal    = tree(r(mwp,l(read_from_treebank(T1,tal,T3),T4,T5)),T6,[]),
    Van    = tree(r(mwp,l(read_from_treebank(V1,van,V3),V4,V5)),V6,[]),
    lists:select(TalVan,Ds0,Ds1),
    Ds = [Tal2,VanPP],
    VanPP  = tree(r(mod,p(pp)),_,[Van2,Obj]),
    Tal2   = tree(r(hd,l(read_from_treebank(T1,tal,T3),T4,T5)),T6,[]),
    Van2   = tree(r(hd,l(read_from_treebank(V1,van,V3),V4,V5)),V6,[]),
    (   Ds1 = [_,_|_],
	Obj    = tree(r(obj1,p(np)),_,Ds1)
    ;   Ds1 = [tree(r(hd,LEX),LEX2,[])],
	Obj    = tree(r(obj1,LEX),LEX2,[])
    ).
*/

/*
user_transformation(r(REL,p(mwu)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,_P1),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,_P2),Cat2,Totaal/TotaalRest)),_,[])
				    ],
		    r(REL,p(np)),B,[tree(r(hd,l(read_from_treebank(Az,Bz,'N(soort,ev,basis,zijd,stan)'),Cat1,In/InRest)),_,[]),
				     tree(r(app,l(read_from_treebank(Cz,Dz,'TW(hoofd,vrij)'),Cat2,Totaal/TotaalRest)),_,[])
				    ],String,_) :-
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,'Postbus').

user_transformation(r(REL,p(mwu)),B,[tree(r(hd,l(read_from_treebank(Az,Bz,P1),Cat1,In/InRest)),_,[]),
				     tree(r(app,l(read_from_treebank(Cz,Dz,P2),Cat2,Totaal/TotaalRest)),_,[])
				    ],
		    r(REL,p(np)),B,[tree(r(hd,l(read_from_treebank(Az,Bz,'N(soort,ev,basis,zijd,stan)'),Cat1,In/InRest)),_,[]),
				     tree(r(app,l(read_from_treebank(Cz,Dz,'TW(hoofd,vrij)'),Cat2,Totaal/TotaalRest)),_,[])
				    ],String,_) :-
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,'Postbus').
*/

/*

user_transformation(r(REL,p(mwu)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,P1),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,P2),Cat2,Totaal/TotaalRest)),_,[])
				    ],
		    r(REL,p(mwu)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,'SPEC(deeleigen)'),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,'SPEC(deeleigen)'),Cat2,Totaal/TotaalRest)),_,[])
				    ],String,_) :-
    \+ P1/P2 = 'SPEC(deeleigen)'/'SPEC(deeleigen)',
    lists:member('SPEC(deeleigen)',[P1,P2]),
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,WordP),
    TotaalRest = [Q0,Q],
    alpino_treebank:get_root(Q0,Q,String,WordQ),
    format(user_error,"~w ~w~n",[WordP,WordQ]).

user_transformation(r(REL,p(mwu)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,P1),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,P2),Cat2,Totaal/TotaalRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Ez,Fz,P3),Cat2,Dus/DusRest)),_,[])
				    ],
		    r(REL,p(mwu)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,'SPEC(deeleigen)'),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,'SPEC(deeleigen)'),Cat2,Totaal/TotaalRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Ez,Fz,'SPEC(deeleigen)'),Cat2,Dus/DusRest)),_,[])
				    ],String,_) :-
    \+ P1/P2/P3 = 'SPEC(deeleigen)'/'SPEC(deeleigen)'/'SPEC(deeleigen)',
    lists:member('SPEC(deeleigen)',[P1,P2,P3]),
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,WordP),
    TotaalRest = [Q0,Q],
    alpino_treebank:get_root(Q0,Q,String,WordQ),
    DusRest = [R0,R],
    alpino_treebank:get_root(R0,R,String,WordR),
    format(user_error,"~w ~w ~w~n",[WordP,WordQ,WordR]).

user_transformation(r(REL,p(mwu)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,P1),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,P2),Cat2,Totaal/TotaalRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Ez,Fz,P3),Cat3,Dus/DusRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Gz,Hz,P4),Cat4,Meer/MeerRest)),_,[])
				    ],
		    r(REL,p(mwu)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,'SPEC(deeleigen)'),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,'SPEC(deeleigen)'),Cat2,Totaal/TotaalRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Ez,Fz,'SPEC(deeleigen)'),Cat3,Dus/DusRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Gz,Hz,'SPEC(deeleigen)'),Cat4,Meer/MeerRest)),_,[])
				    ],String,_) :-
    \+ P1/P2/P3/P4 = 'SPEC(deeleigen)'/'SPEC(deeleigen)'/'SPEC(deeleigen)'/'SPEC(deeleigen)',
    lists:member('SPEC(deeleigen)',[P1,P2,P3,P4]),
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,WordP),
    TotaalRest = [Q0,Q],
    alpino_treebank:get_root(Q0,Q,String,WordQ),
    DusRest = [R0,R],
    alpino_treebank:get_root(R0,R,String,WordR),
    MeerRest = [S0,S],
    alpino_treebank:get_root(S0,S,String,WordS),
    format(user_error,"~w ~w ~w ~w~n",[WordP,WordQ,WordR,WordS]).
*/
/*
user_transformation(r(REL,p(CAT)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,Q1,P1),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,Q2,P2),Cat2,Totaal/TotaalRest)),_,[])
				    ],
		    r(REL,p(CAT)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,Q1,'SPEC(deeleigen)'),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,Q2,'SPEC(deeleigen)'),Cat2,Totaal/TotaalRest)),_,[])
				    ],String,_) :-
    \+ P1/P2 = 'SPEC(deeleigen)'/'SPEC(deeleigen)',
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,WordP),
    TotaalRest = [Q0,Q],
    alpino_treebank:get_root(Q0,Q,String,WordQ),
    trname(WordP,WordQ),
    format(user_error,"~w ~w~n",[WordP,WordQ]).

user_transformation(r(REL,p(CAT)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,Q1,P1),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,Q2,P2),Cat2,Totaal/TotaalRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Ez,Fz,Q3,P3),Cat2,Dus/DusRest)),_,[])
				    ],
		    r(REL,p(CAT)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,Q1,'SPEC(deeleigen)'),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,Q2,'SPEC(deeleigen)'),Cat2,Totaal/TotaalRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Ez,Fz,Q3,'SPEC(deeleigen)'),Cat2,Dus/DusRest)),_,[])
				    ],String,_) :-
    \+ P1/P2/P3 = 'SPEC(deeleigen)'/'SPEC(deeleigen)'/'SPEC(deeleigen)',
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,WordP),
    TotaalRest = [Q0,Q],
    alpino_treebank:get_root(Q0,Q,String,WordQ),
    DusRest = [R0,R],
    alpino_treebank:get_root(R0,R,String,WordR),
    trname(WordP,WordQ,WordR),
    format(user_error,"~w ~w ~w~n",[WordP,WordQ,WordR]).

user_transformation(r(REL,p(CAT)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,Q1,P1),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,Q2,P2),Cat2,Totaal/TotaalRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Ez,Fz,Q3,P3),Cat3,Dus/DusRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Gz,Hz,Q4,P4),Cat4,Meer/MeerRest)),_,[])
				    ],
		    r(REL,p(CAT)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,Q1,'SPEC(deeleigen)'),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,Q2,'SPEC(deeleigen)'),Cat2,Totaal/TotaalRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Ez,Fz,Q3,'SPEC(deeleigen)'),Cat3,Dus/DusRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Gz,Hz,Q4,'SPEC(deeleigen)'),Cat4,Meer/MeerRest)),_,[])
				    ],String,_) :-
    \+ P1/P2/P3/P4 = 'SPEC(deeleigen)'/'SPEC(deeleigen)'/'SPEC(deeleigen)'/'SPEC(deeleigen)',
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,WordP),
    TotaalRest = [Q0,Q],
    alpino_treebank:get_root(Q0,Q,String,WordQ),
    DusRest = [R0,R],
    alpino_treebank:get_root(R0,R,String,WordR),
    MeerRest = [S0,S],
    alpino_treebank:get_root(S0,S,String,WordS),
    trname(WordP,WordQ,WordR,WordS),
    format(user_error,"~w ~w ~w ~w~n",[WordP,WordQ,WordR,WordS]).

user_transformation(r(REL,p(CAT)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,Q1,P1),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,Q2,P2),Cat2,Totaal/TotaalRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Ez,Fz,Q3,P3),Cat3,Dus/DusRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Gz,Hz,Q4,P4),Cat4,Meer/MeerRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Iz,Jz,Q5,P5),Cat5,Dan/DanRest)),_,[])
				    ],
		    r(REL,p(CAT)),B,[tree(r(mwp,l(read_from_treebank(Az,Bz,Q1,'SPEC(deeleigen)'),Cat1,In/InRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Cz,Dz,Q2,'SPEC(deeleigen)'),Cat2,Totaal/TotaalRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Ez,Fz,Q3,'SPEC(deeleigen)'),Cat3,Dus/DusRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Gz,Hz,Q4,'SPEC(deeleigen)'),Cat4,Meer/MeerRest)),_,[]),
				     tree(r(mwp,l(read_from_treebank(Iz,Jz,Q5,'SPEC(deeleigen)'),Cat5,Dan/DanRest)),_,[])
				    ],String,_) :-
    \+ P1/P2/P3/P4/P5 = 'SPEC(deeleigen)'/'SPEC(deeleigen)'/'SPEC(deeleigen)'/'SPEC(deeleigen)'/'SPEC(deeleigen)',
    InRest = [P0,P],
    alpino_treebank:get_root(P0,P,String,WordP),
    TotaalRest = [Q0,Q],
    alpino_treebank:get_root(Q0,Q,String,WordQ),
    DusRest = [R0,R],
    alpino_treebank:get_root(R0,R,String,WordR),
    MeerRest = [S0,S],
    alpino_treebank:get_root(S0,S,String,WordS),
    DanRest = [T0,T],
    alpino_treebank:get_root(T0,T,String,WordT),
    trname(WordP,WordQ,WordR,WordS,WordT),
    format(user_error,"~w ~w ~w ~w ~w~n",[WordP,WordQ,WordR,WordS,WordT]).

trname(A,B) :-
    hdrug_util:concat_all([A,B],Stem,' '),
    alpino_cgn_postags:name2(Stem).


trname(A,B,C) :-
    hdrug_util:concat_all([A,B,C],Stem,' '),
    alpino_cgn_postags:name3(Stem).

trname(A,B,C,D):-
    hdrug_util:concat_all([A,B,C,D],Stem,' '),
    alpino_cgn_postags:name4(Stem).

*/
% 
% 


%% --------------------------------------------------------------------------------------------- %%


/*
user_transformation(r(REL,CAT),   B,Ds0,
		    r(REL,CAT),B,[Geboren|Ds],String,_) :-
    Geboren0 =  tree(r(REL0,l(read_from_treebank(X,nl,'BW()'),Z,I/[P0,P])),J,[]),
    Geboren  =  tree(r(REL0,l(read_from_treebank(X,'Nederland','N(eigen,ev,basis,onz,stan)'),Z,I/[P0,P])),J,[]),
    format(user_error,"~w~n",[Ds0]),
    lists:select(Geboren0,Ds0,Ds),
    alpino_treebank:get_root(P0,P,String,nl).
*/

/*
user_transformation(r(REL,p(mwu)),B,Ds0,
		    r(REL,p(mwu)),B,Ds,String,_) :-
    words_lemmas(Ds0,Ds,Words,Lemmas0,Lemmas,Pos0,Pos,String),
    mwu_words(Words,Lemmas0,Lemmas,Pos0,Pos),
    \+ Ds0 = Ds,
    format(user_error,"~w~n",[Words]).

words_lemmas([],[],[],[],[],[],[],_).
words_lemmas([Dh|Th],[D|T],[W|Ws],[L0|L0s],[L|Ls],[P0|P0s],[P|Ps],String) :-
    word_lemma(Dh,D,W,L0,L,P0,P,String),
    words_lemmas(Th,T,Ws,L0s,Ls,P0s,Ps,String).

word_lemma(tree(r(mwp,l(read_from_treebank(Pos, L0,PT0),CAT,R/[P0,P])),I,[]),
	   tree(r(mwp,l(read_from_treebank(Pos, L, PT ),CAT,R/[P0,P])),I,[]),
	   W,L0,L,PT0,PT,String):-
    alpino_treebank:get_root(P0,P,String,W).


mwu_words(Words,_,Words,['SPEC(deeleigen)','SPEC(deeleigen)'],['SPEC(deeleigen)','SPEC(deeleigen)']).
mwu_words(Words,_,Words,['SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)'],['SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)']).
mwu_words(Words,_,Words,['SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)'],['SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)']).
mwu_words(Words,_,Words,['SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)'],['SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)']).

mwu_words(['De','Band','Krijgt','Kinderen'],_,['De','Band','Krijgt','Kinderen'],_,['SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)']).
mwu_words(['De','Morgan\'s'],_,['De','Morgan'],_,['SPEC(deeleigen)','N(eigen,ev,basis,gen)']).
mwu_words(['Buitenlandse','Zaken'],_,[buitenlands,zaak],_,['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)']).
*/

/*
mwu_words(['Eén',en,ander],_,  [één,en,ander],_, ['TW(hoofd,nom,zonder-n,basis)','VG(neven)','ADJ(nom,basis,zonder,zonder-n)']).
mwu_words([nog,geen],_,        [nog,geen],_,     ['BW()','VNW(onbep,det,stan,prenom,zonder,agr)']).
mwu_words(['Voor',het,eerst],_,[voor,het,één],_, ['VZ(init)','LID(bep,stan,evon)','TW(rang,nom,zonder-n)']).
mwu_words([ten,uitvoer],_,     [te,uitvoer],_,   ['VZ(versm)','N(soort,ev,basis,zijd,stan)']).
mwu_words([min,of,meer],_,     [min,of,veel],_,  ['BW()','VG(neven)','VNW(onbep,grad,stan,vrij,zonder,comp)']).
mwu_words([in,staat],_,        [in,staat],_,     ['VZ(init)','N(soort,ev,basis,zijd,stan)']).
mwu_words([beeldend,kunstenaar],_,[beelden,kunstenaar],['WW(od,prenom,zonder)','N(soort,ev,basis,zijd,stan)']).
mwu_words([voor,rekening,van],_,[voor,rekening,van],['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)']).
mwu_words([met,medewerking,van],_,[met,medewerking,van],['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)']).
mwu_words([op,last,van],_,[op,last,van],['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)']).
mwu_words([op,basis,van],_,[op,basis,van],['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)']).
mwu_words([met,behulp,van],_,[met,behulp,van],['VZ(init)','N(soort,ev,basis,onz,stan)','VZ(init)']).
mwu_words([op,het,gebied,van],_,[op,het,gebied,van],['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','VZ(init)']).
mwu_words([met,name],_,         [met,naam],         ['VZ(init)','N(soort,ev,basis,dat)']).
mwu_words([geen,van,allen],_,   [geen,van,allen],   ['VNW(onbep,det,stan,prenom,zonder,agr)','VZ(init)','NW(onbep,det,stan,nom,met-e,mv-n)']).
mwu_words([onder,meer],_,       [onder,veel],       ['VZ(init)','VNW(onbep,grad,stan,vrij,zonder,comp)']).


mwu_words(Words,_,Words,_,P) :-
    correct(Words,P).
mwu_words(Words,_,Lemmas,_,P) :-
    correct(Words,Lemmas,P).

correct([in,de,hand],['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)']).
correct(['13',juni,'2004'],['TW(hoofd,vrij)','N(eigen,ev,basis,zijd,stan)','TW(hoofd,vrij)']).
correct(['Onafhankelijke','Senaatsfractie'],['SPEC(deeleigen)','SPEC(deeleigen)']).
correct(['Algemene','Vergadering'],['SPEC(deeleigen)','SPEC(deeleigen)']).
correct(['Limburgs','Universitair','Centrum'],['SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)']).
correct(['9',november],['TW(hoofd,vrij)','N(eigen,ev,basis,zijd,stan)']).
correct(['op','het','gebied','van'],['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','VZ(init)']).
correct(['kabinet-Van','Agt','II'],['N(soort,ev,basis,onz,stan)','SPEC(deeleigen)','SPEC(deeleigen)']).
correct(['23','februari'],['TW(hoofd,vrij)','N(eigen,ev,basis,zijd,stan)']).
correct(['tot','taak'],['VZ(init)','N(soort,ev,basis,zijd,stan)']).
correct(['1','oktober'],['TW(hoofd,vrij)','N(eigen,ev,basis,zijd,stan)']).
correct(['met','de','grond'],['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)']).
correct(['onder','de','voet'],['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)']).
correct(['17','juni'],['TW(hoofd,vrij)','N(eigen,ev,basis,zijd,stan)']).
correct(['tot','doel'],['VZ(init)','N(soort,ev,basis,onz,stan)']).
correct(['voor','rekening','van'],['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)']).
correct(['als','gevolg','van'],['VZ(init)','N(soort,ev,basis,onz,stan)','VZ(init)']).
correct(['Franse','Gemeenschapscommissie'],['SPEC(deeleigen)','SPEC(deeleigen)']).
correct(['Raad','van','Vlaanderen'],['SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)']).
correct(['te','weten'],['VZ(init)','WW(inf,vrij,zonder)']).
correct(['met','uitzondering','van'],['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)']).
correct(['Gemeenschappelijke','Gemeenschapscommissie'],['SPEC(deeleigen)','SPEC(deeleigen)']).
correct(['21','januari','2003'],['TW(hoofd,vrij)','N(eigen,ev,basis,zijd,stan)','TW(hoofd,vrij)']).
correct(['in','het','kader','van'],['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','VZ(init)']).
correct(['in','de','hand'],['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)']).
correct(['4','oktober'],['TW(hoofd,vrij)','N(eigen,ev,basis,zijd,stan)']).
correct(['14','oktober','1966'],['TW(hoofd,vrij)','N(eigen,ev,basis,zijd,stan)','TW(hoofd,vrij)']).
correct(['met','betrekking','tot'],['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)']).
correct(['Les','bourgeois'],['SPEC(vreemd)','SPEC(vreemd)']).
correct(['op','het','getouw'],['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)']).
correct(['Kleurrijk','Vlaanderen'],['SPEC(deeleigen)','SPEC(deeleigen)']).
correct(['17','januari','1995'],['TW(hoofd,vrij)','N(eigen,ev,basis,zijd,stan)','TW(hoofd,vrij)']).


correct(['In','opdracht','van'],[in,opdracht,van],['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)']).
correct(['Regeling','Eisen','Geschiktheid','2000'],['regeling','eis','geschiktheid','2000'],['N(soort,ev,basis,zijd,stan)','N(soort,mv,basis)','N(soort,ev,basis,zijd,stan)','TW(hoofd,vrij)']).
correct(['De','oestereetster'],['de','oestereetster'],['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)']).
correct(['ten','hoogste'],[te,hoog],['VZ(versm)','ADJ(nom,sup,met-e,zonder-n,stan)']).
correct(['De','Zevende','Dag'],['de','zeven','dag'],['LID(bep,stan,rest)','TW(rang,prenom,stan)','N(soort,ev,basis,zijd,stan)']).
correct(['ter','voorbereiding','op'],['te','voorbereiding','op'],['VZ(versm)','N(soort,ev,basis,zijd,stan)','VZ(init)']).
correct(['Ministerie','van','Buitenlandse','Zaken'],['ministerie','van','buitenlands','zaak'],['N(soort,ev,basis,onz,stan)','VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)']).
correct(['het','beste'],[het,goed],['LID(bep,stan,evon)','ADJ(nom,sup,met-e,zonder-n,stan)']).
correct(['meer','dan'],['veel','dan'],['VNW(onbep,grad,stan,vrij,zonder,comp)','VG(onder)']).
correct(['aan','de','wieg'],[],[]).

*/

/*
mwu_words(['New','Yorkse'],_,['New','York'],_,['SPEC(deeleigen)','ADJ(prenom,basis,met-e,stan)']).


mwu_words(['Economische','Zaken'],_,[economisch,zaak],_,['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)']).
mwu_words(['Economische','zaken'],_,[economisch,zaak],_,['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)']).
mwu_words(['Buitenlandse','Zaken'],_,[buitenlands,zaak],_,['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)']).
mwu_words(['Binnenlandse','Zaken'],_,[binnenlands,zaak],_,['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)']).
mwu_words(['Binnenlandse','Zaken',en,'Koninkrijksrelaties'],_,[binnenlands,zaak,en,koninkrijkrelatie],_,
	  ['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','VG(neven)','N(soort,mv,basis)']).
mwu_words(['Buitenlandse','zaken'],_,[buitenlands,zaak],_,['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)']).
mwu_words(['Binnenlandse','zaken'],_,[binnenlands,zaak],_,['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)']).
mwu_words(['Binnenlandse','zaken',en,'Koninkrijksrelaties'],_,[binnenlands,zaak,en,koninkrijkrelatie],_,
	  ['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','VG(neven)','N(soort,mv,basis)']).

mwu_words(['Meer',dan],_,[veel,dan],POS,POS).
mwu_words([het,eerst],_,[het,één],POS,POS).
mwu_words(['In',memoriam],_,[in,memoriam],_,['SPEC(vreemd)','SPEC(vreemd)']).
mwu_words([best,practices],_,[best,practices],_,['SPEC(vreemd)','SPEC(vreemd)']).
mwu_words(['Het','Lam','Gods'],_,[het,lam,'God'],_,['LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,gen)']).

mm(['Lage','Landen']).
mm(['Fortis','Bank']).
mm(['Goede','Vrijdag']).
mm(['Gouden','Beer']).
mm(['Europese','Conventie']).
mm(['Europees','Vakverbond']).
mm(['Europese','Gemeenschap']).
mm(['Europese','Grondwet']).
mm(['Euro','2000']).
mm(['Dolle','Dinsdag']).
mm(['Vlaamse','Gemeenschap']).
mm(['Franse','Gemeenschap']).
mm(['Duitstalige','Gemeenschap']).
mm(['Vlaamse','Gewest']).
mm(['Vlaams','Gewest']).
mm(['Waalse','Gewest']).
mm(['Waals','Gewest']).
mm(['Middellandse','Zee']).
mm(['Kaspische','Zee']).
mm(['Koninklijk','Paleis']).
mm(['Kommunistische','Partij']).
mm(['Centraal','Station']).
mm(['Centraal','Planbureau']).
mm(['Atlantische','Oceaan']).

mm(['Het','Laatste','Nieuws']).
mm(['Internationaal','Monetair','Fonds']).
mm(['Filips',de,'Goede']).
mm(['Europese','Economische','Gemeenschap']).
mm(['Leger',des,'Heils']).
mm(['Brussels','Hoofdstedelijk','Gewest']).

mm(['Vlaamse','Gemeenschap',van,'België']).
*/

/*
mwu_words([voor,het,eerst],[voor,het,'één']).
mwu_words(['Voor',het,eerst],[voor,het,'één']).
mwu_words([meer,dan],[veel,dan]).
mwu_words([het,best],[het,goed]).
mwu_words([onder,meer],[onder,veel]).
mwu_words([zonder,meer],[zonder,veel]).
mwu_words([zij,het],[zijn,het]).
mwu_words([niet,minder,dan],[niet,weinig,dan]).
mwu_words(['Basistakenpakket','Jeugdgezondheidszorg'],['basistakenpakket','jeugdgezondheidszorg']).
mwu_words([ter,sprake],[te,spraak]).
mwu_words([met,name],[met,naam]).
mwu_words(['Koninklijke','Musea',voor,'Schone','Kunsten'],['koninklijk','museum',voor,'schoon','kunst']).
mwu_words([een,en,ander],[één,en,ander]).
mwu_words(['Douane','2002'],[douane,'2002']).
mwu_words(['De','Tweeling'],[de,tweeling]).
mwu_words(['Wet','Verbetering','Poortwachter'],[wet,verbetering,poortwachter]).
mwu_words(['Wet',op,de,'Medische','Keuringen'],[wet,op,de,medisch,keuring]).
mwu_words(['Werkgroep','Zeventiende','Eeuw'],[werkgroep,zeventien,eeuw]).
mwu_words(['Verzoek',inkomen,ouders,opvragen,inkomen,ouders,buiten,beschouwing,laten],[verzoek,inkomen,ouder,opvragen,inkomen,ouder,buiten,beschouwing,laten]).
mwu_words(['Verzoek',inkomen,ouders,opvragen,'/',inkomen,ouders,buiten,beschouwing,laten],[verzoek,inkomen,ouder,opvragen,'/',inkomen,ouder,buiten,beschouwing,laten]).
mwu_words(['Verklaring','Sociale','Hygiëne'],[verklaring,sociaal,hygiëne]).
mwu_words(['Verkeer',en,'Waterstaat'],[verkeer,en,waterstaat]).
*/

mwu_words(Words,Words) :-
    mwu_words(Words).

/*
mwu_words([nieuws,in,het,kort]).
mwu_words(['Vlaams','Blok']).
mwu_words([her,en,der]).
mwu_words(['Provinciale','Staten']).
mwu_words(['Katwijk',aan,'Zee']).
mwu_words(['Gazet',van,'Antwerpen']).
mwu_words(['Franse','Revolutie']).
mwu_words(['zo\'n','beetje']).
mwu_words(['Wereld','Natuur','Fonds']).
mwu_words(['Vlaams','Verbond']).
mwu_words(['Vlaamse','Gemeenschap','van','België']).
mwu_words(['Vlaamse','Gemeenschap']).
mwu_words(['Verenigde','Staten','van','Amerika']).
mwu_words(['Verenigde','Oostindische','Compagnie']).
mwu_words(['Verenigde','Arabische','Emiraten']).
mwu_words(['Universiteit','van','Amsterdam']).
mwu_words(['Staten','Generaal']).
mwu_words(['Standaard','Uitgeverij']).
mwu_words(['Sociaal','Progressief','Alternatief']).
mwu_words(['smoking','gun']).
mwu_words(['Radio','Oranje']).
mwu_words(['op','til']).
mwu_words(['Openbare','Bibliotheek','Amsterdam']).
mwu_words(['Olympische','Winterspelen']).
mwu_words(['NV','De','Vlijt']).
*/

% mwu_is_words(['Verenigde','Staten']).
% mwu_is_words(['Verenigde','Naties']).
% mwu_is_words(['Europese','Unie']).
% mwu_is_words(['Tweede','Wereldoorlog']).
% mwu_is_words(['Eerste','Wereldoorlog']).
% mwu_is_words(['Nationaal','Epilepsie','Fonds']).
% mwu_is_words(['Europese','Raad']).
% mwu_is_words(['Verenigd','Koninkrijk']).
% mwu_is_words(['Groen','!']).
% mwu_is_words(['Raad','van','State']).
% mwu_is_words(['Europese','Commissie']).
% mwu_is_words(['Europese','Unie']).
% mwu_is_words(['Koude','Oorlog']).
% mwu_is_words(['Olympische','Spelen']).
% mwu_is_words(['Grote','Markt']).
% mwu_is_words(['Europees','Parlement']).
% mwu_is_words(['De','Post']).
% mwu_is_words(['Vlaams','Gewest']).
% mwu_is_words(['Nederlandse','Antillen']).
% mwu_is_words(['Witte','Huis']).
% mwu_is_words(['Nieuw-Vlaamse','Alliantie']).
% mwu_is_words(['Vrije','Universiteit','Brussel']).
% mwu_is_words(['Tweede','Kamer']).
% mwu_is_words(['Rode','Kruis']).
% mwu_is_words(['Provinciale','Staten']).
% mwu_is_words(['Nieuws','in','het','Kort']).
% mwu_is_words(['Katwijk','aan','Zee']).
% mwu_is_words(['IJzeren','Rijn']).
% mwu_is_words(['Gazet','van','Antwerpen']).
% mwu_is_words(['Europese','Centrale','Bank']).
% mwu_is_words(['Westelijke','Jordaanoever']).
% mwu_is_words(['Wereld','Natuur','Fonds']).
% mwu_is_words(['Vrije','Universiteit']).
% mwu_is_words(['Vlaamse','Gemeenschap']).
% mwu_is_words(['Stille','Oceaan']).
% mwu_is_words(['Staten','Generaal']).
% mwu_is_words(['Ronde','van','Italië']).
% mwu_is_words(['Ronde','van','Frankrijk']).
% mwu_is_words(['Rad','van','Fortuin']).
% mwu_is_words(['Radio','Oranje']).
% mwu_is_words(['Raad','van','Europa']).
% mwu_is_words(['Perzische','Golf']).
% mwu_is_words(['Pater','Damiaan']).


/*
user_transformation(r(REL,p(mwu)),B,Ds0,
		    r(REL,p(mwu)),B,Ds,_,_) :-
    lists:member(tree(r(mwp,l(read_from_treebank(_,_,TAG),_,_)),_,_),Ds0),
    TAG \== 'SPEC(deeleigen)',
    mwu_words(Ds0,Words),
    !,
    change_postag(Ds0,Ds),
    format(user_error,"~w~n",[Words]).

mwu_words([],[]).
mwu_words([tree(r(mwp,l(read_from_treebank(name,_,_),_,W/_)),_,_)|Trees],[W|Words]) :-
    mwu_words(Trees,Words).

change_postag([],[]).
change_postag([H0|T0],[H|T]) :-
    change_post(H0,H),
    change_postag(T0,T).

change_post(tree(r(mwp,l(read_from_treebank(Az,_,_                   ),Cat,Word/[P0,P])),Ix,[]),
	    tree(r(mwp,l(read_from_treebank(Az,Word,'SPEC(deeleigen)'),Cat,Word/[P0,P])),Ix,[])).


*/


/*
user_transformation(r(REL,p(mwu)),B,[tree(r(mwp,l(Pos1,Cat1,In)),_,[]),
				     tree(r(mwp,l(Pos2,Cat2,Totaal)),_,[])
				    ],
		    r(REL,p(pp)),B,[tree(r(hd,l(Pos1,Cat1,In)),_,[]),
				    tree(r(obj1,l(Pos2,Cat2,Totaal)),_,[])
				    ],_,_) :-
    In = in/_,
    Totaal = totaal/_.
*/

/*
user_transformation(r(REL,p(VRIJCAT)),B,Ds0,
		    r(REL,p(VRIJCAT)),B,[tree(MOD,XX,YY)|Ds],String,_) :-
    MOD0 = r(VRIJREL,l(read_from_treebank(Az,Bz0,POS0),Cat,Root0/[P0,P])),
    MOD  = r(VRIJREL,l(read_from_treebank(Az,Bz,POS),Cat,Root/[P0,P])),
    lists:select(tree(MOD0,XX,YY),Ds0,Ds),
    alpino_treebank:get_root(P0,P,String,Word),
    vrijpos(REL,VRIJCAT,VRIJREL,Word,Bz0,Bz,Root0,Root,POS0,POS),
    format(user_error,"~w~n",[Word]).
user_transformation(r(REL,p(VRIJCAT)),B,Ds0,
		    r(REL,p(VRIJCAT)),B,[tree(MOD,XX,YY)|Ds],String,_) :-
    MOD0 = r(VRIJREL,i(Ix,l(read_from_treebank(Az,Bz0,POS0),Cat,Root0/[P0,P]))),
    MOD  = r(VRIJREL,i(Ix,l(read_from_treebank(Az,Bz,POS),Cat,Root/[P0,P]))),
    lists:select(tree(MOD0,XX,YY),Ds0,Ds),
    alpino_treebank:get_root(P0,P,String,Word),
    vrijpos(REL,VRIJCAT,VRIJREL,Word,Bz0,Bz,Root0,Root,POS0,POS),
    format(user_error,"~w~n",[Word]).

vrijpos(_,_,_,eentje,eentje,één,Root,Root,Tag,Tag) :-
    Tag \= 'SPEC(deeleigen)'.

*/
/*

user_transformation(r(REL,p(VRIJCAT)),B,Ds0,
		    r(REL,p(VRIJCAT)),B,[tree(MOD,XX,YY)|Ds],String,_) :-
    MOD0 = r(VRIJREL,l(read_from_treebank(Az,Bz0,POS0),Cat,Root0/[P0,P])),
    MOD  = r(VRIJREL,l(read_from_treebank(Az,Bz,POS),Cat,Root/[P0,P])),
    lists:select(tree(MOD0,XX,YY),Ds0,Ds),
    alpino_treebank:get_root(P0,P,String,Word),
    vrijpos(REL,VRIJCAT,VRIJREL,Word,Bz0,Bz,Root0,Root,POS0,POS),
    format(user_error,"~w~n",[Word]).
user_transformation(r(REL,p(VRIJCAT)),B,Ds0,
		    r(REL,p(VRIJCAT)),B,[tree(MOD,XX,YY)|Ds],String,_) :-
    MOD0 = r(VRIJREL,i(Ix,l(read_from_treebank(Az,Bz0,POS0),Cat,Root0/[P0,P]))),
    MOD  = r(VRIJREL,i(Ix,l(read_from_treebank(Az,Bz,POS),Cat,Root/[P0,P]))),
    lists:select(tree(MOD0,XX,YY),Ds0,Ds),
    alpino_treebank:get_root(P0,P,String,Word),
    vrijpos(REL,VRIJCAT,VRIJREL,Word,Bz0,Bz,Root0,Root,POS0,POS),
    format(user_error,"~w~n",[Word]).






user_transformation(r(REL,p(VRIJCAT)),B,Ds0,
		    r(REL,p(VRIJCAT)),B,[tree(MOD,XX,YY)|Ds],String,_) :-
    MOD0 = r(hd,l(read_from_treebank(Az,Bz,POS0),Cat,Root/[P0,P])),
    MOD  = r(hd,l(read_from_treebank(Az,Bz,POS),Cat,Root/[P0,P])),
    DET  = r(det,l(_,_,DetRoot/_)),
    lists:select(tree(MOD0,XX,YY),Ds0,Ds),
    lists:select(tree(DET,_,_),Ds,_),
    alpino_treebank:get_root(P0,P,String,Word),
    vrijpos(DetRoot,Word,POS0,POS),
    format(user_error,"~w~n",[Word]).

vrijpos(de,_,'N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)').
vrijpos(het,_,'N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)').


user_transformation(r(REL,p(VRIJCAT)),B,Ds0,
		    r(REL,p(VRIJCAT)),B,[tree(MOD,XX,YY)|Ds],String,_) :-
    MOD0 = r(VRIJREL,l(read_from_treebank(Az,Bz0,POS0),Cat,Root0/[P0,P])),
    MOD  = r(VRIJREL,l(read_from_treebank(Az,Bz,POS),Cat,Root/[P0,P])),
    lists:select(tree(MOD0,XX,YY),Ds0,Ds),
    alpino_treebank:get_root(P0,P,String,Word),
    vrijpos(REL,VRIJCAT,VRIJREL,Word,Bz0,Bz,Root0,Root,POS0,POS),
    format(user_error,"~w --> ~w~n",[Bz0,Word]).
user_transformation(r(REL,p(VRIJCAT)),B,Ds0,
		    r(REL,p(VRIJCAT)),B,[tree(MOD,XX,YY)|Ds],String,_) :-
    MOD0 = r(VRIJREL,i(Ix,l(read_from_treebank(Az,Bz0,POS0),Cat,Root0/[P0,P]))),
    MOD  = r(VRIJREL,i(Ix,l(read_from_treebank(Az,Bz,POS),Cat,Root/[P0,P]))),
    lists:select(tree(MOD0,XX,YY),Ds0,Ds),
    alpino_treebank:get_root(P0,P,String,Word),
    vrijpos(REL,VRIJCAT,VRIJREL,Word,Bz0,Bz,Root0,Root,POS0,POS),
    format(user_error,"~w --> ~w~n",[Bz0,Word]).

vrijpos(_,_,REL,'Romeinen',_,'Romein',_,'Roemein','SPEC(deeleigen)','N(soort,mv,basis)') :-
    REL \== mwp.
vrijpos(_,_,REL,'Russen',_,'Rus',_,'Rus','SPEC(deeleigen)','N(soort,mv,basis)') :-
    REL \== mwp.
vrijpos(_,_,REL,'Cryptogram',_,cryptogram,_,cryptogram,'SPEC(deeleigen)','N(soort,ev,basis,onz,stan)') :-
    REL \== mwp.

vrijpos(_,_,REL,'Pvda',Bz,Bz,Root,Root,'N(eigen,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)') :-
    REL \== mwp.

vrijpos(_,_,REL,Word,Bz,Bz,Root,Root,'SPEC(deeleigen)','N(eigen,ev,basis,zijd,stan)') :-
    REL \== mwp,
    zijd_naam(Word).
vrijpos(_,_,REL,Word,Bz,Bz,Root,Root,'SPEC(deeleigen)','N(eigen,ev,basis,onz,stan)') :-
    REL \== mwp,
    onz_naam(Word).

vrijpos(_,_,REL,Word,Bz,Bz,Root,Root,'SPEC(deeleigen)','N(eigen,ev,basis,onz,stan)') :-
    REL \== mwp,
    alpino_lex:lexicon(proper_name(_,'LOC'),_,[Word],[],names_dictionary).
vrijpos(_,_,REL,Word,Bz,Bz,Root,Root,'SPEC(deeleigen)','N(eigen,ev,basis,onz,stan)') :-
    REL \== mwp,
    alpino_lex:lexicon(proper_name(_,'ORG'),_,[Word],[],names_dictionary).
vrijpos(_,_,REL,Word,Bz,Bz,Root,Root,'SPEC(deeleigen)','N(eigen,ev,basis,zijd,stan)') :-
    REL \== mwp,
    alpino_lex:lexicon(proper_name(_,'PER'),_,[Word],[],names_dictionary).
vrijpos(_,_,REL,_Word,Bz,Bz,Root,Root,'SPEC(deeleigen)','N(eigen,ev,basis,genus,stan)') :-
    REL \== mwp.


zijd_naam('Mercurius').
zijd_naam('Venus').
zijd_naam('Mars').
zijd_naam('Saturnus').
zijd_naam('Titan').
zijd_naam('Pluto').
zijd_naam('Jupiter').

zijd_naam('Hamida').
zijd_naam('Tour').

onz_naam('Vaticaan').




vrijpos(_,np,det,deze,deze,'ADJ(prenom,basis,met-e,bijz)','VNW(aanw,det,gen,prenom,met-e,rest3)',_,_).

vrijpos(_,np,hd,het,het,'LID(bep,stan,evon)','VNW(pers,pron,stan,red,3,ev,onz)',_,_).
vrijpos(_,du,dp,het,het,'LID(bep,stan,evon)','VNW(pers,pron,stan,red,3,ev,onz)',_,_).
vrijpos(_,_,su,het,het,'LID(bep,stan,evon)','VNW(pers,pron,stan,red,3,ev,onz)',_,_).
vrijpos(_,np,hd,een,één,'LID(onbep,stan,agr)','TW(hoofd,nom,zonder-n,basis)',_,_).
vrijpos(_,np,hd,één,één,'LID(onbep,stan,agr)','TW(hoofd,nom,zonder-n,basis)',_,_).
vrijpos(_,_,obj1,een,één,'LID(onbep,stan,agr)','TW(hoofd,nom,zonder-n,basis)',_,_).
vrijpos(_,_,su,een,één,'LID(onbep,stan,agr)','TW(hoofd,nom,zonder-n,basis)',_,_).

vrijpos(_,np,det,'De',de,R,R,'SPEC(deeleigen)','LID(bep,stan,rest)',_,_).
vrijpos(_,np,det,X,X,R,R,'SPEC(symb)','TW(hoofd,prenom,stan)',_,_) :-
    atom(X),
    atom_codes(X,[L|_]),
    alpino_latin1:isdigit(L).

vrijpos(_,np,det,AtomS,Atom,AtomS,Atom,'SPEC(deeleigen)','N(eigen,ev,basis,gen)',_,_) :-
    atom_concat(Atom,'\'s',AtomS),!.
vrijpos(_,np,det,AtomS,Atom,AtomS,Atom,'SPEC(deeleigen)','N(eigen,ev,basis,gen)',_,_) :-
    atom_concat(Atom,s,AtomS).
*/  

/*
user_transformation(r(REL,l(read_from_treebank(Az,Bz1,TAG1),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,Bz2,TAG2),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,J),
    correct_postag_lemma(J,Bz1,Bz2,TAG1,TAG2).

correct_postag_lemma(_,minder,weinig,'VNW(onbep,grad,stan,vrij,zonder,comp)','VNW(onbep,grad,stan,vrij,zonder,comp)').
correct_postag_lemma(_,minder,weinig,'VNW(onbep,grad,stan,prenom,zonder,agr,comp)','VNW(onbep,grad,stan,prenom,zonder,agr,comp)').
correct_postag_lemma(_,meer,veel,'VNW(onbep,grad,stan,vrij,zonder,comp)','VNW(onbep,grad,stan,vrij,zonder,comp)').
correct_postag_lemma(_,meer,veel,'VNW(onbep,grad,stan,prenom,zonder,agr,comp)','VNW(onbep,grad,stan,prenom,zonder,agr,comp)').
*/

/*
user_transformation(r(REL,l(read_from_treebank(Az,Bz1,TAG),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,Bz2,TAG),Cat,Root/[P0,P])),B,[],_,_) :-
    correct_lemma(TAG,Bz1,Bz2).


correct_postag_lemma(vroeg,vroeg,vragen,'WW(pv,verl,ev)','WW(pv,verl,ev)').

correct_postag_lemma(uitgebreide,uitbreiden,uitbreiden,'ADJ(prenom,basis,met-e,stan)','WW(vd,prenom,met-e)').
correct_postag_lemma(uitgebreid,uitbreiden,uitbreiden,'ADJ(prenom,basis,zonder)','WW(vd,prenom,zonder)').
correct_postag_lemma(uitgebreid,uitbreiden,uitbreiden,'ADJ(vrij,basis,zonder)','WW(vd,vrij,zonder)').

correct_postag_lemma(verbonden,verbond,verbinden,'WW(vd,vrij,zonder)','WW(vd,vrij,zonder)').

correct_postag_lemma(gevangene,_,vangen,_,'WW(vd,nom,met-e,zonder-n)').
correct_postag_lemma(gevangenen,_,vangen,_,'WW(vd,nom,met-e,mv-n)').

correct_postag_lemma(geraakt,_,geraken,'WW(pv,tgw,met-t)','WW(pv,tgw,met-t)').
correct_postag_lemma(geraakte,_,geraken,'WW(pv,verl,ev)','WW(pv,verl,ev)').
correct_postag_lemma(geraakten,_,geraken,'WW(pv,verl,mv)','WW(pv,verl,mv)').
correct_postag_lemma(geraken,_,geraken,'WW(pv,tgw,mv)','WW(pv,tgw,mv)').
correct_postag_lemma(geraken,_,geraken,'WW(inf,vrij,zonder)','WW(inf,vrij,zonder)').

correct_postag_lemma(gelukt,gelukken,lukken,'WW(vd,vrij,zonder)','WW(vd,vrij,zonder)').


correct_postag_lemma(gedacht,gedenken,denken,'WW(vd,vrij,zonder)','WW(vd,vrij,zonder)').
correct_postag_lemma('Gedacht',gedenken,denken,'WW(vd,vrij,zonder)','WW(vd,vrij,zonder)').


correct_lemma('WW(pv,tgw,ev)',Bz1,Bz2) :-
    correct_lemma_pv1(Bz1,Bz2).

correct_lemma('WW(vd,vrij,zonder)',gevallen,vallen).

correct_lemma('WW(pv,tgw,mv)',vlucht,vluchten).
correct_lemma('WW(pv,tgw,mv)',stam,stammen).

correct_lemma('WW(vd,vrij,zonder)',gevoelen,voelen).

correct_lemma('WW(vd,vrij,zonder)',gezien,zien).

correct_lemma('WW(pv,tgw,met-t)',staat,staan).

correct_lemma('WW(inf,vrij,zonder)',uitvoer,uitvoeren).
correct_lemma('WW(inf,vrij,zonder)',reis,reizen).
correct_lemma('WW(inf,vrij,zonder)',moord,moorden).
correct_lemma('WW(inf,vrij,zonder)',scherm,schermen).
correct_lemma('WW(inf,vrij,zonder)',slaap,slapen).
correct_lemma('WW(inf,vrij,zonder)',feest,feesten).
correct_lemma('WW(inf,vrij,zonder)',fiets,fietsen).
correct_lemma('WW(inf,vrij,zonder)',spook,spoken).
correct_lemma('WW(inf,vrij,zonder)',brand,branden).
correct_lemma('WW(inf,vrij,zonder)',vis,vissen).
correct_lemma('WW(inf,vrij,zonder)',vlucht,vluchten).
correct_lemma('WW(inf,vrij,zonder)',ontwerp,ontwerpen).
correct_lemma('WW(inf,nom,zonder,zonder-n)',reis,reizen).
correct_lemma('WW(inf,nom,zonder,zonder-n)',smelt,smelten).
correct_lemma('WW(inf,nom,zonder,zonder-n)',bestuur,besturen).
correct_lemma('WW(inf,nom,zonder,zonder-n)',inkoop,inkopen).
correct_lemma('WW(inf,nom,zonder,zonder-n)',vis,vissen).
correct_lemma('WW(pv,tgw,ev)',schat,schattenn).
correct_lemma('WW(pv,verl,ev)',was,zijn).
correct_lemma('WW(pv,verl,ev)',ontwikkeld,ontwikkelen).
correct_lemma('WW(pv,verl,mv)',wilde,willen).
correct_lemma('WW(pv,conj,ev)',waar,zijn).
correct_lemma('WW(pv,conj,ev)',koste,kosten).
correct_lemma('WW(pv,conj,ev)',geschiede,geschieden).
correct_lemma('WW(vd,vrij,zonder)',verslag,verslaan).

correct_lemma_pv1(wil,willen).
correct_lemma_pv1(kijk,kijken).
correct_lemma_pv1(lijk,lijken).
correct_lemma_pv1(geef,geven).
correct_lemma_pv1(los,lossen).
correct_lemma_pv1(ga,gaan).
correct_lemma_pv1(lucht,luchten).
correct_lemma_pv1(vlucht,vluchten).
correct_lemma_pv1(stel,stellen).
correct_lemma_pv1(kaart,kaarten).
correct_lemma_pv1(roest,roesten).
correct_lemma_pv1(slaap,slapen).
correct_lemma_pv1(vertrek,vertrekken).

user_transformation(r(REL,l(read_from_treebank(Az,ijzer,'ADJ(prenom,basis,zonder'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,ijzeren,'ADJ(prenom,basis,zonder)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,J),
    lists:member(J,[ijzeren,'IJzeren','Ijzeren','IJZEREN']).

user_transformation(r(REL,p(VRIJCAT)),B,Ds0,
		    r(REL,p(VRIJCAT)),B,[tree(MOD,XX,YY)|Ds],String,_) :-
    MOD0 = r(VRIJREL,l(read_from_treebank(Az,Bz,POS0),Cat,Root/Pos)),
    MOD  = r(VRIJREL,l(read_from_treebank(Az,Bz,POS),Cat,Root/Pos)),
    lists:select(tree(MOD0,XX,YY),Ds0,Ds),
    vrijpos(REL,VRIJCAT,VRIJREL,POS0,POS,String,Pos).

vrijpos(_,pp,hd,_,'VZ(fin)',String,[P0,P]) :-
    alpino_treebank:get_root(P0,P,String,N),
    prep(N).

prep(bovenaan).
prep(bovenop).
prep(bovenin).
prep(vooraan).
prep(tegenaan).
prep(binnenuit).
prep(dichtbij).
prep(langszij).
prep(onderaan).
prep(achteraan).
prep(achterin).
prep(vlakbij).
prep(bovenop).
prep(voorbij).
prep(middendoor).
prep(vanachter).
*/

/*
correct_postag('WW(vd,postnom,zonder)',
	       'WW(vd,vrij,zonder)').
correct_postag('WW(od,postnom,zonder)',
	       'WW(od,vrij,zonder)').
correct_postag('WW(inf,postnom,zonder)',
	       'WW(inf,vrij,zonder)').
correct_postag('WW(pv,tgw,met-e)',
	       'WW(pv,tgw,met-t)').
correct_postag('WW(vd,prenom,met-e,zonder-n)',
	       'WW(vd,prenom,met-e)').
correct_postag('WW(p,tgw,mv)',
	       'WW(pv,tgw,mv)').
correct_postag('WW(od,nom,nom,met-e,mv-n)',
	       'WW(od,nom,met-e,mv-n)').
correct_postag('WW(inf,nom,zonder,zoner-n)',
	       'WW(inf,nom,zonder,zonder-n)').
correct_postag('WW(pv,tgw,zonder)',
	       'WW(pv,tgw,ev)').
correct_postag('WW(od,nom,met-e,getal-n)',
	       'WW(od,nom,met-e,zonder-n)').
correct_postag('WW(vd,prenom,basis)',
	       'WW(vd,prenom,zonder)').

correct_postag('ADJ(prenom,basis,met-e)',
	       'ADJ(prenom,basis,met-e,stan)').
correct_postag('ADJ(nom,basis,met-e,zonder-n)',
	       'ADJ(nom,basis,met-e,zonder-n,stan)').
correct_postag('ADJ(nom,basis,met-e,stan)',
	       'ADJ(nom,basis,met-e,zonder-n,stan)').
correct_postag('ADJ(prenom,basis,stan)',
	       'ADJ(prenom,basis,zonder)').
correct_postag('ADJ(sup,zonder)',
	       'ADJ(vrij,sup,zonder)').
correct_postag('ADJ(prenom,basis,met-e,zonder-n,stan)',
	       'ADJ(prenom,basis,met-e,stan)').
correct_postag('ADJ(vrij,basis,met-e)',
	       'ADJ(vrij,basis,zonder)').
correct_postag('ADJ(nom,basis,zonder,zonder-e)',
	       'ADJ(nom,basis,zonder,zonder-n)').
correct_postag('ADJ(nom,basis,met-e,mv-m)',
	       'ADJ(nom,basis,met-e,mv-n)').
correct_postag('ADJ(nom,basis,met-e,getal-n)',
	       'ADJ(nom,basis,met-e,zonder-n,stan)').

correct_postag('N(soort,mv,stan)',
	       'N(soort,mv,basis)').
correct_postag('N(soort,ev,basis,onz)',
	       'N(soort,ev,basis,onz,stan)').
correct_postag('N(soort,ev,basis,stan)',
	       'N(soort,ev,basis,onz,stan)').
correct_postag('N(soort,ev,basis,onz,basis)',
	       'N(soort,ev,basis,onz,stan)').
correct_postag('N(soort,basis,mv)',
	       'N(soort,mv,basis)').

correct_postag('VNW(onbep,grad,stan,nom,zonder,sup)',
	       'VNW(onbep,grad,stan,nom,zonder,zonder-n,sup)').
correct_postag('VNW(bez,det,stan,vol,1,mv,nom,met-e,getal-n)',
	       'VNW(bez,det,stan,vol,1,mv,nom,met-e,zonder-n)').
correct_postag('VNW(bez,det,stan,vol,3m,ev,nom,met-e,getal-n)',
	       'VNW(bez,det,stan,vol,3m,ev,nom,met-e,zonder-n)').
correct_postag('VNW(aanw,det,stan,nom,met-e,getal-n)',
	       'VNW(aanw,det,stan,nom,met-e,zonder-n)').
correct_postag('VNW(vb,det,stan,prenom,buiging,agr)',
	       'VNW(vb,det,stan,prenom,met-e,rest)').
correct_postag('VNW(onbep,det,stan,nom,met-e,zonder-n,basis)',
	       'VNW(onbep,det,stan,nom,met-e,zonder-n)').
correct_postag('VNW(betr,pron,stan,vol,3o,ev)',
	       'VNW(betr,det,stan,nom,zonder,zonder-n)').

correct_postag('TW(rang,prenom,met-e)',
	       'TW(rang,prenom,stan)').
correct_postag('TW(hoofd,prenom)',
	       'TW(hoofd,prenom,stan)').
correct_postag('TW(hoofd,prenom,stna)',
	       'TW(hoofd,prenom,stan)').
correct_postag('TW(hoofd,prenom,basis)',
	       'TW(hoofd,prenom,stan)').
correct_postag('TW(hoofd,postnom)',
	       'TW(hoofd,vrij)').

correct_postag('LID(bep,stan,agr)',
	       'LID(bep,stan,rest)').
*/

/*
%user_transformation(r(REL,p(pp)),B,Ds0,
%		    r(REL,p(pp)),B,[tree(MOD,XX,YY)|Ds],_,_) :-
%    MOD0 = r(hd,l(read_from_treebank(Az,Bz,'VZ(fin)'),Cat,Root/[P0,P])),
%    MOD  = r(hd,l(read_from_treebank(Az,Bz,'VZ(init)'),Cat,Root/[P0,P])),
%    lists:select(tree(MOD0,XX,YY),Ds0,Ds),
%    lists:member(tree(r(obj1,l(_,_,RRoot/[Q0,_Q])),_,_),Ds),
%    Q0 > P0,
%    format(user_error,"~w ~w~n",[Root,RRoot]).

user_transformation(r(REL,l(read_from_treebank(Az,'\'t','VNW(pers,pron,stan,red,3,ev,onz)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,het,'VNW(pers,pron,stan,red,3,ev,onz)'),Cat,Root/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,'-','SPEC(symb)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'-','LET()'),Cat,Root/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,'<','SPEC(symb)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'<','LET()'),Cat,Root/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,'>','SPEC(symb)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'>','LET()'),Cat,Root/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,'—','SPEC(symb)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'—','LET()'),Cat,Root/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,'_','SPEC(symb)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'_','LET()'),Cat,Root/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,'=','SPEC(symb)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'=','LET()'),Cat,Root/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,'/','SPEC(symb)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'/','LET()'),Cat,Root/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,',,','SPEC(symb)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,',,','LET()'),Cat,Root/[P0,P])),B,[],_,_).

user_transformation(r(REL,l(read_from_treebank(Az,'`','SPEC(symb)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,'`','LET()'),Cat,Root/[P0,P])),B,[],_,_).

user_transformation(r(hd,l(read_from_treebank(Az,Bz,'ADJ(postnom,comp,zonder)'),Cat,Root/[P0,P])),B,[],
		    r(hd,l(read_from_treebank(Az,Bz,'ADJ(vrij,comp,zonder)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    lists:member(Zijn,[later,'Later','LATER']).

user_transformation(r(REL,p(VRIJCAT)),B,Ds0,
		    r(REL,p(VRIJCAT)),B,[tree(MOD,XX,YY)|Ds],String,_) :-
    MOD0 = r(VRIJREL,l(read_from_treebank(Az,Bz,POS0),Cat,Root/Pos)),
    MOD  = r(VRIJREL,l(read_from_treebank(Az,Bz,POS),Cat,Root/Pos)),
    lists:select(tree(MOD0,XX,YY),Ds0,Ds),
    vrijpos(REL,VRIJCAT,VRIJREL,POS0,POS,String,Pos).

user_transformation(r(REL,p(VRIJCAT)),B,Ds0,
		    r(REL,p(VRIJCAT)),B,[tree(MOD,XX,YY)|Ds],String,_) :-
    MOD0 = r(VRIJREL,i(ZZ,l(read_from_treebank(Az,Bz,POS0),Cat,Root/Pos))),
    MOD  = r(VRIJREL,i(ZZ,l(read_from_treebank(Az,Bz,POS),Cat,Root/Pos))),
    lists:select(tree(MOD0,XX,YY),Ds0,Ds),
    vrijpos(REL,VRIJCAT,VRIJREL,POS0,POS,String,Pos).



user_transformation(r(REL,p(CAT)),B,Ds0,
		    r(REL,p(CAT)),B,[tree(r(su,l(read_from_treebank(Ax,Bx,Posy),Catx,Rootx/[P0,P])),_,[])|Ds],String,_) :-
    lists:member(tree(r(hd,l(read_from_treebank(_,_,Posz),_,_)),_,[]),Ds0),
    lists:select(tree(r(su,l(read_from_treebank(Ax,Bx,Posx),Catx,Rootx/[P0,P])),_,[]),Ds0,Ds),
    alpino_treebank:get_root(P0,P,String,ZIJ),
    lists:member(ZIJ,[zij,'Zij','ZIJ',
		      ze, 'Ze', 'ZE']),
    xxx(Posz,Posx,Posy).
user_transformation(r(REL,p(CAT)),B,Ds0,
		    r(REL,p(CAT)),B,[tree(r(su,i(ZZ,l(read_from_treebank(Ax,Bx,Posy),Catx,Rootx/[P0,P]))),_,[])|Ds],String,_) :-
    lists:member(tree(r(hd,l(read_from_treebank(_,_,Posz),_,_)),_,[]),Ds0),
    lists:select(tree(r(su,i(ZZ,l(read_from_treebank(Ax,Bx,Posx),Catx,Rootx/[P0,P]))),_,[]),Ds0,Ds),
    alpino_treebank:get_root(P0,P,String,ZIJ),
    lists:member(ZIJ,[zij,'Zij','ZIJ',
		      ze, 'Ze', 'ZE']),
    xxx(Posz,Posx,Posy).

%% zij
xxx('WW(pv,verl,ev)',  'VNW(pers,pron,nomin,vol,3p,mv)','VNW(pers,pron,nomin,vol,3v,ev,fem)') :- !.
xxx('WW(pv,tgw,ev)',   'VNW(pers,pron,nomin,vol,3p,mv)','VNW(pers,pron,nomin,vol,3v,ev,fem)') :- !.
xxx('WW(pv,tgw,met-t)','VNW(pers,pron,nomin,vol,3p,mv)','VNW(pers,pron,nomin,vol,3v,ev,fem)') :- !.

xxx('WW(pv,verl,mv)',  'VNW(pers,pron,nomin,vol,3v,ev,fem)','VNW(pers,pron,nomin,vol,3p,mv)') :- !.
xxx('WW(pv,tgw,mv)',  'VNW(pers,pron,nomin,vol,3v,ev,fem)','VNW(pers,pron,nomin,vol,3p,mv)') :- !.

%% ze
xxx('WW(pv,verl,mv)',  'VNW(pers,pron,stan,red,3,ev,fem)','VNW(pers,pron,stan,red,3,mv)') :- !.
xxx('WW(pv,tgw,mv)',   'VNW(pers,pron,stan,red,3,ev,fem)','VNW(pers,pron,stan,red,3,mv)') :- !.
xxx('WW(pv,verl,ev)',  'VNW(pers,pron,stan,red,3,mv)','VNW(pers,pron,stan,red,3,ev,fem)') :- !.
xxx('WW(pv,tgw,met-t)','VNW(pers,pron,stan,red,3,mv)','VNW(pers,pron,stan,red,3,ev,fem)') :- !.
xxx('WW(pv,tgw,ev)',   'VNW(pers,pron,stan,red,3,mv)','VNW(pers,pron,stan,red,3,ev,fem)') :- !.

xxx(A,B,B) :-
    format(user_error,"~w ~w~n",[A,B]),
    fail.

*/

%vrijpos(_,mwu,mwp,_,'ADJ(nom,basis,met-e,zonder-n,bijz)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,volle).

%vrijpos(su,np,hd,'N(soort,mv,basis)','WW(inf,vrij,zonder)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,R),
%    lists:member(R,[reizen,'Reizen','REIZEN']).

%vrijpos(_,np,hd,'VNW(aanw,adv-pron,stan,red,3,getal)','VNW(onbep,adv-pron,gen,red,3,getal)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    lists:member(VERD,[er,'Er','ER']).


%vrijpos(_,np,hd,'ADJ(prenom,basis,met-e,stan)','ADJ(nom,basis,met-e,zonder-n,stan)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).
%
%vrijpos(_,np,hd,'VNW(onbep,grad,stan,prenom,zonder,agr,comp)','VNW(onbep,grad,stan,vrij,zonder,comp)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).
%
%vrijpos(_,np,hd,'VNW(onbep,det,stan,prenom,met-e,evz)','VNW(onbep,det,stan,nom,met-e,zonder-n)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).
%
%vrijpos(_,np,hd,'VNW(onbep,det,stan,prenom,met-e,rest)','VNW(onbep,det,stan,nom,met-e,zonder-n)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).
%
%vrijpos(_,np,hd,'VNW(aanw,det,stan,prenom,met-e,rest)','VNW(aanw,det,stan,nom,met-e,zonder-n)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).
%
%vrijpos(_,np,hd,'VNW(onbep,grad,stan,prenom,met-e,agr,basis)','VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).



%vrijpos(_,np,hd,'VNW(onbep,grad,stan,prenom,zonder,agr,basis)','VNW(onbep,grad,stan,vrij,zonder,basis)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).
%vrijpos(_,np,hd,'VNW(aanw,det,stan,prenom,met-e,rest)','VNW(aanw,det,stan,nom,met-e,zonder-n)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).

%vrijpos(_,np,hd,_,'TW(hoofd,nom,mv-n,basis)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    lists:member(VERD,[miljarden,'Miljarden','MILJARDEN']).
%
%vrijpos(_,np,hd,'ADJ(prenom,basis,zonder)','ADJ(nom,basis,zonder,zonder-n)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).

%vrijpos(_,np,hd,'WW(vd,prenom,met-e)','WW(vd,nom,met-e,zonder-n)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).
%vrijpos(_,np,hd,'ADJ(prenom,basis,met-e,stan)','ADJ(nom,basis,met-e,zonder-n,stan)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]),
%    lists:member(VERD,[andere,'Andere','ANDERE']).
%vrijpos(_,np,hd,'ADJ(prenom,comp,met-e,stan)','ADJ(nom,comp,met-e,zonder-n,stan)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).
%vrijpos(_,np,hd,'ADJ(prenom,sup,met-e,stan)','ADJ(nom,sup,met-e,zonder-n,stan)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).

%vrijpos(_,np,hd,'TW(hoofd,prenom,stan)','TW(hoofd,nom,zonder-n,basis)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).
%
%vrijpos(_,np,hd,'VNW(onbep,det,stan,prenom,zonder,evon)','VNW(onbep,det,stan,vrij,zonder)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).

%vrijpos(_,np,hd,'VNW(aanw,det,stan,prenom,zonder,rest)','VNW(aanw,pron,stan,vol,3,getal)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).

% vrijpos(_,np,hd,'VNW(onbep,grad,stan,prenom,met-e,mv,basis)','VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)',_).

%vrijpos(_,np,hd,'TW(rang,prenom,stan)','TW(rang,nom,zonder-n)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).
%vrijpos(_,np,hd,'VNW(onbep,det,stan,prenom,met-e,rest)','VNW(onbep,det,stan,nom,met-e,zonder-n)',String,[P0,P]) :-
%    alpino_treebank:get_root(P0,P,String,VERD),
%    format(user_error,"~w~n",[VERD]).



/*

user_transformation(r(REL,l(read_from_treebank(Az,Bz,'VNW(onbep,det,stan,prenom,met-e,agr)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,Bz,'VNW(onbep,pron,stan,vol,3o,ev)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Je),
    lists:member(Je,[wat,'Wat','WAT']).




vrijpos(_,_,cmp,'VZ(fin)','VZ(init)',_).
vrijpos(_,_,cmp,'VG(neven)','VG(onder)',_).


vrijpos(_,_,su,'VNW(onbep,grad,stan,prenom,met-e,mv,basis)','VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)',_).
vrijpos(_,_,obj1,'VNW(onbep,grad,stan,prenom,met-e,mv,basis)','VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)',_).
vrijpos(_,_,obj2,'VNW(onbep,grad,stan,prenom,met-e,mv,basis)','VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)',_).
vrijpos(_,_,predm,'VNW(onbep,grad,stan,prenom,met-e,mv,basis)','VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)',_).
vrijpos(_,_,dp,'VNW(onbep,grad,stan,prenom,met-e,mv,basis)','VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)',_).

vrijpos(_,np,mod,'ADJ(vrij,basis,zonder)','ADJ(prenom,basis,zonder)',_).
vrijpos(_,np,mod,'ADJ(vrij,comp,zonder)','ADJ(prenom,comp,zonder)',_).
vrijpos(_,np,mod,'ADJ(vrij,sup,zonder)','ADJ(prenom,sup,zonder)',_).
vrijpos(_,np,mod,'WW(vd,vrij,zonder)','WW(vd,prenom,zonder)',_).
vrijpos(_,np,mod,'WW(od,vrij,zonder)','WW(od,prenom,zonder)',_).

vrijpos(_,np,det,'VNW(aanw,det,stan,nom,met-e,zonder-n)','VNW(aanw,det,stan,prenom,met-e,rest)',_).

vrijpos(_,np,det,'VNW(pr,pron,obl,vol,1,mv)','VNW(bez,det,stan,vol,1,mv,prenom,zonder,evon)',_).
vrijpos(_,np,det,'VNW(aanw,pron,stan,vol,3,getal)','VNW(aanw,det,stan,prenom,zonder,rest)',_).


vrijpos(_,np,det,'VNW(onbep,pron,stan,vol,3o,ev)','VNW(onbep,det,stan,prenom,met-e,agr)',_).
vrijpos(_,np,det,'VNW(vb,pron,stan,vol,3o,ev)','VNW(onbep,det,stan,prenom,met-e,agr)',_).

user_transformation(r(REL,l(read_from_treebank(Az,_,'ADJ(prenom,basis,met-e,stan)'),Cat,_/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,aanstaande,'ADJ(prenom,basis,zonder)'),Cat,aanstaande/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Aanstaande),
    lists:member(Aanstaande,[aanstaande,'Aanstaande','AANSTAANDE']).


vrijpos(np,hd,'TW(hoofd,prenom,stan)','TW(hoofd,nom,zonder-n,basis)',_).
vrijpos(np,hd,'TW(hoofd,vrij)','TW(hoofd,nom,zonder-n,basis)',_).
vrijpos(np,hd,'ADJ(prenom,basis,met-e,stan)','ADJ(nom,basis,met-e,zonder-n,stan)',_).
vrijpos(np,hd,'ADJ(prenom,comp,met-e,stan)','ADJ(nom,comp,met-e,zonder-n,stan)',_).
vrijpos(np,hd,'ADJ(prenom,sup,met-e,stan)','ADJ(nom,sup,met-e,zonder-n,stan)',_).
vrijpos(np,hd,'ADJ(prenom,basis,zonder)','ADJ(nom,basis,zonder,zonder-n)',_).
vrijpos(np,hd,'WW(inf,vrij,zonder)','WW(inf,nom,zonder,zonder-n)',_).

user_transformation(r(det,l(read_from_treebank(Az,Bz,'TW(hoofd,vrij)'),Cat,Root/[P0,P])),B,[],
		    r(det,l(read_from_treebank(Az,Bz,'TW(hoofd,prenom,stan)'),Cat,Root/[P0,P])),B,[],_String,_).

user_transformation(r(det,l(read_from_treebank(Az,Bz,'VNW(onbep,grad,stan,vrij,zonder,comp)'),Cat,Root/[P0,P])),B,[],
		    r(det,l(read_from_treebank(Az,Bz,'VNW(onbep,grad,stan,prenom,zonder,agr,comp)'),Cat,Root/[P0,P])),B,[],_String,_).

user_transformation(r(det,l(read_from_treebank(Az,Bz,'VNW(aanw,det,stan,nom,met-e,zonder-n)'),Cat,Root/[P0,P])),B,[],
		    r(det,l(read_from_treebank(Az,Bz,'VNW(aanw,det,stan,prenom,met-e,rest)'),Cat,Root/[P0,P])),B,[],_String,_).

user_transformation(r(det,l(read_from_treebank(Az,Bz,'VNW(onbep,grad,stan,vrij,zonder,basis)'),Cat,Root/[P0,P])),B,[],
		    r(det,l(read_from_treebank(Az,Bz,'VNW(onbep,grad,stan,prenom,zonder,agr,basis)'),Cat,Root/[P0,P])),B,[],_String,_).


user_transformation(r(M,l(read_from_treebank(Az,Bz,'TW(hoofd,prenom,stan)'),Cat,Root/[P0,P])),B,[],
		    r(M,l(read_from_treebank(Az,Bz,'VNW(onbep,grad,stan,prenom,zonder,agr,basis)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,teveel).

user_transformation(r(mod,l(read_from_treebank(Az,Bz,'TW(rang,nom,zonder-n)'),Cat,Root/[P0,P])),B,[],
		    r(mod,l(read_from_treebank(Az,Bz,'TW(rang,prenom,stan)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,eerste).





%vrijpos(np,mod,'WW(vd,vrij,zonder)','WW(vd,prenom,zonder)',_).
user_transformation(r(REL,p(VRIJCAT)),B,Ds0,
		    r(REL,p(VRIJCAT)),B,[tree(MOD,XX,YY)|Ds],String,_) :-
    MOD0 = r(VRIJREL,l(read_from_treebank(Az,Bz,POS0),Cat,Root/Pos)),
    MOD  = r(VRIJREL,l(read_from_treebank(Az,Bz,POS),Cat,Root/Pos)),
    vrijpos(VRIJCAT,VRIJREL,POS0,POS,String),
    lists:select(tree(MOD0,XX,YY),Ds0,Ds).

user_transformation(r(REL,p(VRIJCAT)),B,Ds0,
		    r(REL,p(VRIJCAT)),B,[tree(MOD,XX,YY)|Ds],String,_) :-
    MOD0 = r(VRIJREL,i(ZZ,l(read_from_treebank(Az,Bz,POS0),Cat,Root/Pos))),
    MOD  = r(VRIJREL,i(ZZ,l(read_from_treebank(Az,Bz,POS),Cat,Root/Pos))),
    vrijpos(VRIJCAT,VRIJREL,POS0,POS,String),
    lists:select(tree(MOD0,XX,YY),Ds0,Ds).


user_transformation(r(REL,p(VRIJCAT)),B,Ds0,
		    r(REL,p(VRIJCAT)),B,[tree(MOD,XX,YY)|Ds],String,_) :-
    MOD0 = r(VRIJREL,l(read_from_treebank(Az,Bz,POS0),Cat,Root/Pos)),
    MOD  = r(VRIJREL,l(read_from_treebank(Az,Bz,POS),Cat,Root/Pos)),
    vrijpos(VRIJCAT,VRIJREL,POS0,POS,String),
    lists:select(tree(MOD0,XX,YY),Ds0,Ds).

user_transformation(r(REL,p(VRIJCAT)),B,Ds0,
		    r(REL,p(VRIJCAT)),B,[tree(MOD,XX,YY)|Ds],String,_) :-
    MOD0 = r(VRIJREL,i(ZZ,l(read_from_treebank(Az,Bz,POS0),Cat,Root/Pos))),
    MOD  = r(VRIJREL,i(ZZ,l(read_from_treebank(Az,Bz,POS),Cat,Root/Pos))),
    vrijpos(VRIJCAT,VRIJREL,POS0,POS,String),
    lists:select(tree(MOD0,XX,YY),Ds0,Ds).


vrijpos(_,obj1,'VNW(pers,pron,nomin,red,2v,ev)','VNW(pr,pron,obl,red,2v,getal)',_).
vrijpos(_,obj2,'VNW(pers,pron,nomin,red,2v,ev)','VNW(pr,pron,obl,red,2v,getal)',_).
vrijpos(_,se,'VNW(pers,pron,nomin,red,2v,ev)','VNW(pr,pron,obl,red,2v,getal)',_).
vrijpos(_,mwp,'VNW(pers,pron,nomin,red,2v,ev)','VNW(pr,pron,obl,red,2v,getal)',_).

vrijpos(_,det,'VNW(pr,pron,obl,red,2v,getal)','VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)',_).


user_transformation(r(REL,l(read_from_treebank(Az,',,','TW(hoofd,prenom,stan)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,',,','LET()'),Cat,Root/[P0,P])),B,[],_,_).

vrijpos(smain,mod,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).
vrijpos(ssub,mod,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).
vrijpos(sv1,mod,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).
vrijpos(ppart,mod,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).
vrijpos(ap,mod,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).
vrijpos(inf,mod,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).
vrijpos(ppres,mod,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).

vrijpos(smain,predc,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).
vrijpos(ssub,predc,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).
vrijpos(sv1,predc,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).
vrijpos(ppart,predc,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).
vrijpos(ap,predc,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).
vrijpos(inf,predc,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).
vrijpos(ppres,predc,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).

vrijpos(du,dp,'ADJ(prenom,basis,zonder)','ADJ(vrij,basis,zonder)',_).

vrijpos(smain,mod,'ADJ(prenom,comp,zonder)','ADJ(vrij,comp,zonder)',_).
vrijpos(ssub,mod,'ADJ(prenom,comp,zonder)','ADJ(vrij,comp,zonder)',_).
vrijpos(sv1,mod,'ADJ(prenom,comp,zonder)','ADJ(vrij,comp,zonder)',_).
vrijpos(ppart,mod,'ADJ(prenom,comp,zonder)','ADJ(vrij,comp,zonder)',_).
vrijpos(ap,mod,'ADJ(prenom,comp,zonder)','ADJ(vrij,comp,zonder)',_).
vrijpos(inf,mod,'ADJ(prenom,comp,zonder)','ADJ(vrij,comp,zonder)',_).
vrijpos(ppres,mod,'ADJ(prenom,comp,zonder)','ADJ(vrij,comp,zonder)',_).

vrijpos(smain,predc,'ADJ(prenom,comp,zonder)','ADJ(vrij,comp,zonder)',_).
vrijpos(ssub,predc,'ADJ(prenom,comp,zonder)','ADJ(vrij,comp,zonder)',_).
vrijpos(sv1,predc,'ADJ(prenom,comp,zonder)','ADJ(vrij,comp,zonder)',_).
vrijpos(ppart,predc,'ADJ(prenom,comp,zonder)','ADJ(vrij,comp,zonder)',_).
vrijpos(ap,predc,'ADJ(prenom,comp,zonder)','ADJ(vrij,comp,zonder)',_).
vrijpos(inf,predc,'ADJ(prenom,comp,zonder)','ADJ(vrij,comp,zonder)',_).
vrijpos(ppres,predc,'ADJ(prenom,comp,zonder)','ADJ(vrij,comp,zonder)',_).

vrijpos(smain,mod,'WW(od,prenom,zonder)','WW(od,vrij,zonder)',_).
vrijpos(ssub,mod,'WW(od,prenom,zonder)','WW(od,vrij,zonder)',_).
vrijpos(sv1,mod,'WW(od,prenom,zonder)','WW(od,vrij,zonder)',_).
vrijpos(ppart,mod,'WW(od,prenom,zonder)','WW(od,vrij,zonder)',_).
vrijpos(ap,mod,'WW(od,prenom,zonder)','WW(od,vrij,zonder)',_).
vrijpos(inf,mod,'WW(od,prenom,zonder)','WW(od,vrij,zonder)',_).
vrijpos(ppres,mod,'WW(od,prenom,zonder)','WW(od,vrij,zonder)',_).

vrijpos(smain,predc,'WW(od,prenom,zonder)','WW(od,vrij,zonder)',_).
vrijpos(ssub,predc,'WW(od,prenom,zonder)','WW(od,vrij,zonder)',_).
vrijpos(sv1,predc,'WW(od,prenom,zonder)','WW(od,vrij,zonder)',_).
vrijpos(ppart,predc,'WW(od,prenom,zonder)','WW(od,vrij,zonder)',_).
vrijpos(ap,predc,'WW(od,prenom,zonder)','WW(od,vrij,zonder)',_).
vrijpos(inf,predc,'WW(od,prenom,zonder)','WW(od,vrij,zonder)',_).
vrijpos(ppres,predc,'WW(od,prenom,zonder)','WW(od,vrij,zonder)',_).

vrijpos(smain,mod,'VNW(onbep,grad,stan,prenom,zonder,agr,comp)','VNW(onbep,grad,stan,vrij,zonder,comp)',_).
vrijpos(ssub,mod,'VNW(onbep,grad,stan,prenom,zonder,agr,comp)','VNW(onbep,grad,stan,vrij,zonder,comp)',_).
vrijpos(sv1,mod,'VNW(onbep,grad,stan,prenom,zonder,agr,comp)','VNW(onbep,grad,stan,vrij,zonder,comp)',_).
vrijpos(ppart,mod,'VNW(onbep,grad,stan,prenom,zonder,agr,comp)','VNW(onbep,grad,stan,vrij,zonder,comp)',_).
vrijpos(ap,mod,'VNW(onbep,grad,stan,prenom,zonder,agr,comp)','VNW(onbep,grad,stan,vrij,zonder,comp)',_).
vrijpos(inf,mod,'VNW(onbep,grad,stan,prenom,zonder,agr,comp)','VNW(onbep,grad,stan,vrij,zonder,comp)',_).
vrijpos(ppres,mod,'VNW(onbep,grad,stan,prenom,zonder,agr,comp)','VNW(onbep,grad,stan,vrij,zonder,comp)',_).

vrijpos(smain,mod,'VNW(onbep,grad,stan,prenom,zonder,agr,basis)','VNW(onbep,grad,stan,vrij,zonder,basis)',_).
vrijpos(ssub,mod,'VNW(onbep,grad,stan,prenom,zonder,agr,basis)','VNW(onbep,grad,stan,vrij,zonder,basis)',_).
vrijpos(sv1,mod,'VNW(onbep,grad,stan,prenom,zonder,agr,basis)','VNW(onbep,grad,stan,vrij,zonder,basis)',_).
vrijpos(ppart,mod,'VNW(onbep,grad,stan,prenom,zonder,agr,basis)','VNW(onbep,grad,stan,vrij,zonder,basis)',_).
vrijpos(ap,mod,'VNW(onbep,grad,stan,prenom,zonder,agr,basis)','VNW(onbep,grad,stan,vrij,zonder,basis)',_).
vrijpos(inf,mod,'VNW(onbep,grad,stan,prenom,zonder,agr,basis)','VNW(onbep,grad,stan,vrij,zonder,basis)',_).
vrijpos(ppres,mod,'VNW(onbep,grad,stan,prenom,zonder,agr,basis)','VNW(onbep,grad,stan,vrij,zonder,basis)',_).

vrijpos(_,su,'VNW(aanw,det,stan,prenom,zonder,evon)','VNW(aanw,pron,stan,vol,3o,ev)',_).
vrijpos(_,obj1,'VNW(aanw,det,stan,prenom,zonder,evon)','VNW(aanw,pron,stan,vol,3o,ev)',_).
vrijpos(_,obj2,'VNW(aanw,det,stan,prenom,zonder,evon)','VNW(aanw,pron,stan,vol,3o,ev)',_).

vrijpos(_,obj1,'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)','VNW(pers,pron,obl,vol,3,getal,fem)',_).

vrijpos(_,su,'VNW(aanw,det,stan,prenom,zonder,rest)','VNW(aanw,pron,stan,vol,3,getal)',_).
vrijpos(_,obj1,'VNW(aanw,det,stan,prenom,zonder,rest)','VNW(aanw,pron,stan,vol,3,getal)',_).
vrijpos(_,obj2,'VNW(aanw,det,stan,prenom,zonder,rest)','VNW(aanw,pron,stan,vol,3,getal)',_).

vrijpos(_,su,'VNW(aanw,det,stan,prenom,met-e,rest)','VNW(aanw,det,stan,nom,met-e,zonder-n)',_).
vrijpos(_,obj1,'VNW(aanw,det,stan,prenom,met-e,rest)','VNW(aanw,det,stan,nom,met-e,zonder-n)',_).
vrijpos(_,obj2,'VNW(aanw,det,stan,prenom,met-e,rest)','VNW(aanw,det,stan,nom,met-e,zonder-n)',_).

vrijpos(_,predm,'VNW(onbep,det,stan,prenom,zonder,evon)','VNW(onbep,det,stan,vrij,zonder)',_).

vrijpos(_,obj1,'TW(hoofd,prenom,stan)','TW(hoofd,vrij)',_).
vrijpos(du,sat,'TW(hoofd,prenom,stan)','TW(hoofd,vrij)',_).
vrijpos(_,su,'TW(hoofd,prenom,stan)','TW(hoofd,vrij)',_).
vrijpos(_,predc,'TW(hoofd,prenom,stan)','TW(hoofd,vrij)',_).

vrijpos(_,obj1,'VNW(bez,det,stan,vol,3,mv,prenom,zonder,agr)','VNW(pers,pron,obl,vol,3p,mv)',_).
vrijpos(_,obj2,'VNW(bez,det,stan,vol,3,mv,prenom,zonder,agr)','VNW(pers,pron,obl,vol,3p,mv)',_).

vrijpos(_,obj1,'VNW(vb,det,stan,prenom,met-e,rest)','VNW(vb,det,stan,nom,met-e,zonder-n)',_).

vrijpos(cp,body,'TW(rang,prenom,stan)','TW(rang,nom,zonder-n)',_).

vrijpos(_,obj1,'VNW(onbep,grad,stan,prenom,zonder,agr,basis)','VNW(onbep,grad,stan,vrij,zonder,basis)',_).

*/

/*
user_transformation(r(det,l(read_from_treebank(Az,Bz,'TW(hoofd,vrij)'),Cat,Root/[P0,P])),B,[],
		    r(det,l(read_from_treebank(Az,Bz,'TW(hoofd,prenom,stan)'),Cat,Root/[P0,P])),B,[],String,_).
user_transformation(r(det,l(read_from_treebank(Az,een,'TW(hoofd,prenom,stan)'),Cat,Root/[P0,P])),B,[],
		    r(det,l(read_from_treebank(Az,een,'LID(onbep,stan,agr)'),Cat,Root/[P0,P])),B,[],String,_).
user_transformation(r(det,l(read_from_treebank(Az,één,'TW(hoofd,prenom,stan)'),Cat,Root/[P0,P])),B,[],
		    r(det,l(read_from_treebank(Az,een,'LID(onbep,stan,agr)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Z),
    \+ member(Z,[één,'Eén']).


user_transformation(r(su,l(read_from_treebank(Az,het,'LID(bep,stan,evon)'),Cat,Root/[P0,P])),B,[],
		    r(su,l(read_from_treebank(Az,het,'VNW(pers,pron,stan,red,3,ev,onz)'),Cat,Root/[P0,P])),B,[],String,_).
user_transformation(r(predc,l(read_from_treebank(Az,het,'LID(bep,stan,evon)'),Cat,Root/[P0,P])),B,[],
		    r(predc,l(read_from_treebank(Az,het,'VNW(pers,pron,stan,red,3,ev,onz)'),Cat,Root/[P0,P])),B,[],String,_).
user_transformation(r(sup,l(read_from_treebank(Az,het,'LID(bep,stan,evon)'),Cat,Root/[P0,P])),B,[],
		    r(sup,l(read_from_treebank(Az,het,'VNW(pers,pron,stan,red,3,ev,onz)'),Cat,Root/[P0,P])),B,[],String,_).
user_transformation(r(obj1,l(read_from_treebank(Az,het,'LID(bep,stan,evon)'),Cat,Root/[P0,P])),B,[],
		    r(obj1,l(read_from_treebank(Az,het,'VNW(pers,pron,stan,red,3,ev,onz)'),Cat,Root/[P0,P])),B,[],String,_).
user_transformation(r(pobj1,l(read_from_treebank(Az,het,'LID(bep,stan,evon)'),Cat,Root/[P0,P])),B,[],
		    r(pobj1,l(read_from_treebank(Az,het,'VNW(pers,pron,stan,red,3,ev,onz)'),Cat,Root/[P0,P])),B,[],String,_).





user_transformation(r(det,l(read_from_treebank(Az,Bz,'VNW(aanw,pron,stan,vol,3o,ev)'),Cat,Root/[P0,P])),B,[],
		    r(det,l(read_from_treebank(Az,Bz,'VNW(aanw,det,stan,prenom,zonder,evon)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[dat,'Dat','DAT',
		 dit,'Dit','DIT']).

user_transformation(r(det,l(read_from_treebank(Az,Bz,'VG(onder)'),Cat,Root/[P0,P])),B,[],
		    r(det,l(read_from_treebank(Az,Bz,'VNW(aanw,det,stan,prenom,zonder,evon)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[dat,'Dat','DAT']).

user_transformation(r(REL,l(read_from_treebank(Az,voldoend,PT),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,voldoende,PT),Cat,Root/[P0,P])),B,[],String,_).

user_transformation(r(REL,l(read_from_treebank(Az,onvoldoend,PT),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,onvoldoende,PT),Cat,Root/[P0,P])),B,[],String,_).

user_transformation(r(REL,l(read_from_treebank(Az,voldoen,'WW(od,prenom,met-e)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,voldoende,'ADJ(prenom,basis,zonder)'),Cat,Root/[P0,P])),B,[],String,_).

user_transformation(r(REL,l(read_from_treebank(Az,voldoen,'WW(od,vrij,zonder)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,voldoende,'ADJ(vrij,basis,zonder)'),Cat,Root/[P0,P])),B,[],String,_).

user_transformation(r(det,l(read_from_treebank(Az,Bz,'WW(inf,vrij,zonder)'),Cat,Root/[P0,P])),B,[],
		    r(det,l(read_from_treebank(Az,Bz,'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[zijn,'Zijn','ZIJN']).

user_transformation(r(REL,l(read_from_treebank(Az,Bz,'VNW(betr,pron,stan,vol,persoon,getal)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,Bz,'VNW(aanw,pron,stan,vol,3,getal)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[die,'Die','DIE']),
    member(REL,[su,obj1,obj2,hd]).


user_transformation(r(REL,l(read_from_treebank(Az,Bz,'VNW(betr,pron,stan,vol,3,ev)'),Cat,Root/[P0,P])),B,[],
		    r(REL,l(read_from_treebank(Az,Bz,'VNW(aanw,pron,stan,vol,3o,ev)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[dat,'Dat','DAT']),
    member(REL,[su,obj1,obj2,hd]).


user_transformation(r(cmp,l(read_from_treebank(Az,Bz,'VNW(betr,pron,stan,vol,3,ev)'),Cat,Root/[P0,P])),B,[],
		    r(cmp,l(read_from_treebank(Az,Bz,'VG(onder)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[dat,'Dat','DAT']).

user_transformation(r(rhd,l(read_from_treebank(Az,Bz,'VG(onder)'),Cat,Root/[P0,P])),B,[],
		    r(rhd,l(read_from_treebank(Az,Bz,'VNW(betr,pron,stan,vol,3,ev)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[dat,'Dat','DAT']).

user_transformation(r(cmp,l(read_from_treebank(Az,Bz,'VNW(aanw,pron,stan,vol,3o,ev)'),Cat,Root/[P0,P])),B,[],
		    r(cmp,l(read_from_treebank(Az,Bz,'VG(onder)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[dat,'Dat','DAT']).

user_transformation(r(cmp,l(read_from_treebank(Az,Bz,'VNW(aanw,det,stan,prenom,zonder,evon)'),Cat,Root/[P0,P])),B,[],
		    r(cmp,l(read_from_treebank(Az,Bz,'VG(onder)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[dat,'Dat','DAT']).

user_transformation(r(rhd,l(read_from_treebank(Az,Bz,'VNW(aanw,pron,stan,vol,3o,ev)'),Cat,Root/[P0,P])),B,[],
		    r(rhd,l(read_from_treebank(Az,Bz,'VNW(betr,pron,stan,vol,3,ev)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[dat,'Dat','DAT']).

user_transformation(r(rhd,l(read_from_treebank(Az,Bz,'VNW(aanw,det,stan,prenom,zonder,evon)'),Cat,Root/[P0,P])),B,[],
		    r(rhd,l(read_from_treebank(Az,Bz,'VNW(betr,pron,stan,vol,3,ev)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[dat,'Dat','DAT']).

user_transformation(r(det,l(read_from_treebank(Az,Bz,'VNW(betr,pron,stan,vol,persoon,getal)'),Cat,Root/[P0,P])),B,[],
		    r(det,l(read_from_treebank(Az,Bz,'VNW(aanw,det,stan,prenom,zonder,rest)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[die,'Die','DIE']).

user_transformation(r(rhd,l(read_from_treebank(Az,Bz,'VNW(aanw,det,stan,prenom,zonder,rest)'),Cat,Root/[P0,P])),B,[],
		    r(rhd,l(read_from_treebank(Az,Bz,'VNW(betr,pron,stan,vol,persoon,getal)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[die,'Die','DIE']).

user_transformation(r(rhd,l(read_from_treebank(Az,Bz,'VNW(aanw,pron,stan,vol,3,getal)'),Cat,Root/[P0,P])),B,[],
		    r(rhd,l(read_from_treebank(Az,Bz,'VNW(betr,pron,stan,vol,persoon,getal)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[die,'Die','DIE']).

user_transformation(r(rhd,l(read_from_treebank(Az,Bz,'VNW(onbep,pron,stan,vol,3o,ev)'),Cat,Root/[P0,P])),B,[],
		    r(rhd,l(read_from_treebank(Az,Bz,'VNW(vb,pron,stan,vol,3o,ev)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[wat,'Wat','WAT']).

user_transformation(r(rhd,l(read_from_treebank(Az,Bz,'VNW(excl,pron,stan,vol,3,getal)'),Cat,Root/[P0,P])),B,[],
		    r(rhd,l(read_from_treebank(Az,Bz,'VNW(vb,pron,stan,vol,3o,ev)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[wat,'Wat','WAT']).

user_transformation(r(whd,l(read_from_treebank(Az,Bz,'VNW(onbep,pron,stan,vol,3o,ev)'),Cat,Root/[P0,P])),B,[],
		    r(whd,l(read_from_treebank(Az,Bz,'VNW(vb,pron,stan,vol,3o,ev)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[wat,'Wat','WAT']).

user_transformation(r(whd,l(read_from_treebank(Az,Bz,'VNW(excl,pron,stan,vol,3,getal)'),Cat,Root/[P0,P])),B,[],
		    r(whd,l(read_from_treebank(Az,Bz,'VNW(vb,pron,stan,vol,3o,ev)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[wat,'Wat','WAT']).

user_transformation(r(det,l(read_from_treebank(Az,Bz,VNW1),Cat,Root/[P0,P])),B,[],
		    r(det,l(read_from_treebank(Az,Bz,VNW2),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Je),
    ppn(Je,VNW1,VNW2).

ppn(je,'VNW(pers,pron,nomin,red,2v,ev)','VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)').
ppn(het,'VNW(pers,pron,stan,red,3,ev,onz)','LID(bep,stan,evon)').
ppn('Je','VNW(pers,pron,nomin,red,2v,ev)','VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)').
ppn('Het','VNW(pers,pron,stan,red,3,ev,onz)','LID(bep,stan,evon)').
		      
ppn(de,'VNW(pers,pron,dial)','LID(bep,stan,rest)').
ppn('De','VNW(pers,pron,dial)','LID(bep,stan,rest)').

ppn(haar,'VNW(pers,pron,obl,vol,3,getal,fem)','VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)').
ppn('Haar','VNW(pers,pron,obl,vol,3,getal,fem)','VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)').

*/


/*
user_transformation(r(REL,CAT),B,Ds0,
		    r(REL,CAT),B,[VC|Ds],String,_) :-
    member(tree(r(hd,l(_,_,Root/_)),_,[]),Ds0),
    member(Root,[heb,ben]),
    VC0 = tree(r(vc,p(ppart)),_,_),
    select(VC0,Ds0,Ds),
    repair_cat(VC0,VC,String).

repair_cat(tree(r(vc,p(ppart)),A,Ds),
	   tree(r(vc,p(inf)),A,Ds),String) :-
    member(tree(r(hd,l(_,_,Root/[P0,P])),_,[]),Ds),
    alpino_treebank:get_root(P0,P,String,Word),
    checkl(Word,inf),
    \+ checkl(Word,psp).   
*/

%user_transformation(r(REL,l(POS,CAT,ROOT0/PS)),A,B,
%		    r(REL,l(POS,CAT,ROOT /PS)),A,B,_,_) :-
%    sub_atom(ROOT0,_,_,_,' '),
%    no_spaces(ROOT0,ROOT).


/*

user_transformation(r(REL,p(CAT)),B,Ds,
		    r(REL,p(mwu)),B,Ds,_,_) :-
    atom_concat('mwu([',_,CAT).

user_transformation(r(REL,p(CAT)),B,Ds0,
		    r(REL,p(CAT)),B,[HD|Ds],String,_) :-
    HD0 = tree(r(hd,_),_,[]),
    select(HD0,Ds0,Ds),
    repair_hd(CAT,HD0,HD,String).

user_transformation(r(det,l(read_from_treebank(Az,Bz,'WW(pv,tgw,mv)'),Cat,Root/[P0,P])),B,[],
		    r(det,l(read_from_treebank(Az,Bz,'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)'),Cat,Root/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Zijn),
    member(Zijn,[zijn,'Zijn','ZIJN']).

user_transformation(r(hd,l(read_from_treebank(Az,Bz,'N(soort,ev,basis,onz,stan)'),Cat,bal/[P0,P])),B,[],
		    r(hd,l(read_from_treebank(Az,Bz,'N(soort,ev,basis,zijd,stan)'),Cat,bal/[P0,P])),B,[],String,_) :-
    alpino_treebank:get_root(P0,P,String,Bal),
    member(Bal,[bal,'Bal','BAL']).

repair_hd(Cat,tree(r(hd,l(read_from_treebank(Az,Bz,POS0),
			  CAT,ROOT/[P0,P])),B,[]),
	      tree(r(hd,l(read_from_treebank(Az,Bz,POS ),
			  CAT,ROOT/[P0,P])),B,[]),String) :-
    alpino_treebank:get_root(P0,P,String,Word),
    repair_hd_postag(Cat,POS0,POS,Word).

repair_hd_postag(inf,'WW(pv,tgw,mv)','WW(inf,vrij,zonder)',Word) :-
    checkl(Word,inf).
repair_hd_postag(inf,'WW(vd,vrij,zonder)','WW(inf,vrij,zonder)',Word) :-
    checkl(Word,inf).
repair_hd_postag(inf,'N(soort,mv,basis)','WW(inf,vrij,zonder)',Word) :-
    checkl(Word,inf).
repair_hd_postag(inf,'N(soort,ev,basis,onz,stan)','WW(inf,vrij,zonder)',Word) :-
    checkl(Word,inf).
repair_hd_postag(inf,'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)','WW(inf,vrij,zonder)',zijn).

repair_hd_postag(ppart,'WW(pv,tgw,met-t)','WW(vd,vrij,zonder)',Word) :-
    checkl(Word,psp).
repair_hd_postag(ppart,'WW(pv,tgw,ev)','WW(vd,vrij,zonder)',Word) :-
    checkl(Word,psp).
repair_hd_postag(ppart,'WW(inf,vrij,zonder)','WW(vd,vrij,zonder)',Word) :-
    checkl(Word,psp).

repair_hd_postag(ssub,'WW(inf,vrij,zonder)','WW(pv,tgw,mv)',Word) :-
    checkl(Word,mv).
repair_hd_postag(ssub,'WW(vd,vrij,zonder)','WW(pv,tgw,met-t)',Word) :-
    checkl(Word,t).
repair_hd_postag(ssub,'WW(vd,vrij,zonder)','WW(pv,tgw,ev)',Word) :-
    checkl(Word,ev).
repair_hd_postag(ssub,'WW(vd,vrij,zonder)','WW(pv,tgw,mv)',Word) :-
    checkl(Word,mv).
repair_hd_postag(ssub,'WW(vd,vrij,zonder)','WW(pv,verl,mv)',Word) :-
    checkl(Word,verlmv).
repair_hd_postag(ssub,'WW(vd,vrij,zonder)','WW(pv,verl,ev)',Word) :-
    checkl(Word,verlev).
repair_hd_postag(ssub,'WW(vd,prenom,met-e)','WW(pv,verl,ev)',Word) :-
    checkl(Word,verlev).
repair_hd_postag(ssub,'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)','WW(pv,tgw,mv)',zijn).
repair_hd_postag(ssub,'ADJ(vrij,basis,zonder)','WW(pv,verl,ev)',vroeg).

repair_hd_postag(smain,'WW(inf,vrij,zonder)','WW(pv,tgw,mv)',Word) :-
    checkl(Word,mv).
repair_hd_postag(smain,'WW(vd,vrij,zonder)','WW(pv,tgw,met-t)',Word) :-
    checkl(Word,t).
repair_hd_postag(smain,'WW(vd,vrij,zonder)','WW(pv,tgw,ev)',Word) :-
    checkl(Word,ev).
repair_hd_postag(smain,'WW(vd,vrij,zonder)','WW(pv,tgw,mv)',Word) :-
    checkl(Word,mv).
repair_hd_postag(smain,'WW(vd,vrij,zonder)','WW(pv,verl,mv)',Word) :-
    checkl(Word,verlmv).
repair_hd_postag(smain,'WW(vd,vrij,zonder)','WW(pv,verl,ev)',Word) :-
    checkl(Word,verlev).
repair_hd_postag(smain,'WW(vd,prenom,met-e)','WW(pv,verl,ev)',Word) :-
    checkl(Word,verlev).
repair_hd_postag(smain,'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)','WW(pv,tgw,mv)',zijn).
repair_hd_postag(smain,'ADJ(vrij,basis,zonder)','WW(pv,verl,ev)',vroeg).


repair_hd_postag(sv1,'WW(inf,vrij,zonder)','WW(pv,tgw,mv)',Word) :-
    checkl(Word,mv).
repair_hd_postag(sv1,'WW(vd,vrij,zonder)','WW(pv,tgw,met-t)',Word) :-
    checkl(Word,t).
repair_hd_postag(sv1,'WW(vd,vrij,zonder)','WW(pv,tgw,ev)',Word) :-
    checkl(Word,ev).
repair_hd_postag(sv1,'WW(vd,vrij,zonder)','WW(pv,tgw,mv)',Word) :-
    checkl(Word,mv).
repair_hd_postag(sv1,'WW(vd,vrij,zonder)','WW(pv,verl,mv)',Word) :-
    checkl(Word,verlmv).
repair_hd_postag(sv1,'WW(vd,vrij,zonder)','WW(pv,verl,ev)',Word) :-
    checkl(Word,verlev).
repair_hd_postag(sv1,'WW(vd,prenom,met-e)','WW(pv,verl,ev)',Word) :-
    checkl(Word,verlev).
repair_hd_postag(sv1,'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)','WW(pv,tgw,mv)',zijn).
repair_hd_postag(sv1,'ADJ(vrij,basis,zonder)','WW(pv,verl,ev)',vroeg).

repair_hd_postag(sv1,'N(soort,ev,basis,zijd,stan)','WW(pv,tgw,ev)',Word) :-
    checkl(Word,ev).
repair_hd_postag(sv1,'N(soort,ev,basis,onz,stan)','WW(pv,tgw,ev)',Word) :-
    checkl(Word,ev).

checkl(Word,V) :-
    alpino_lex:lexicon(verb(_,Inf,_),_,[Word],[],_,_),
    checklf(V,Inf).

checklf(mv,pl).
checklf(verlmv,past(pl)).
checklf(verlev,past(sg)).
checklf(t,sg3).
checklf(ev,sg1).
checklf(ev,sg).
checklf(inf,inf).
checklf(inf,inf(_)).
checklf(psp,psp).


*/

/*
%% lower conjunctions
user_transformation(r(REL,p(conj)),Index,Ds0,
		    r(REL,CAT),Index,[ tree(r(VC,p(conj)),_,ConjDs) | OrigDs],_,_) :-
    cnj_crd_ds(Ds0,Cnjs,[],Crds,[],CAT),
    coindexed_but_one(Cnjs,OrigDs,RealDs0),
    replace_rel_with_cnj(RealDs0,RealDs,VC),
    lists:append(RealDs,Crds,ConjDs).

cnj_crd_ds([],Cnjs,Cnjs,Crds,Crds,_).
cnj_crd_ds([H|T],Cnjs0,Cnjs,Crds0,Crds,Cat) :-
    cnj_crd_d(H,Cnjs0,Cnjs1,Crds0,Crds1,Cat),
    cnj_crd_ds(T,Cnjs1,Cnjs,Crds1,Crds,Cat).

cnj_crd_d(tree(r(Rel,Cat),Ix,Ds),Cnjs0,Cnjs,Crds0,Crds,Cat2) :-
    (   Rel == cnj
    ->  Cnjs0 = [tree(r(Rel,Cat),Ix,Ds)|Cnjs],
	Crds0 = Crds,
	Cat=Cat2
    ;   Rel == crd,
	Cnjs0 = Cnjs,
	Crds0 = [tree(r(Rel,Cat),Ix,Ds)|Crds]
    ).

%% OrigDs: the antencedents of the coindexed daughters
%% RealDs: the non-coindexed daughter of each conj
coindexed_but_one(Cnjs,Ds2,[Real|RealDs]) :-
    lists:select(tree(_,_,Ds),Cnjs,Cnjs1),  % select the "antecedent"
    lists:select(Real,Ds,Ds2),              % select a real d from the antecedent
    all_ds_coi(Ds2,Cnjs1,Cnjs2),      % remove all coindexed ones
    each_real(Cnjs2,Real,RealDs).     % remaining ones should be all reals


%% each D in Ds should have a coindexed alternative in each of Cnjs
all_ds_coi([],C,C).
all_ds_coi([H|T],C0,C) :-
    get_index_rel(H,IX,REL),
    all_d_coi(C0,IX,REL,C1),
    all_ds_coi(T,C1,C).

get_index_rel(tree(r(REL,i(IX,_)),_,_),IX,REL) :-
    nonvar(IX).
get_index_rel(tree(r(REL,i(IX)),_,[]),IX,REL) :-
    nonvar(IX).

all_d_coi([],_,_,[]).
all_d_coi([tree(A,B,Ds0)|T0],IX,REL,[tree(A,B,Ds)|T]) :-
    lists:select(tree(r(REL,i(IX)),_,[]),Ds0,Ds), 
    all_d_coi(T0,IX,REL,T).

%% every remaining D in DS should be unary tree with same relation as REAL
each_real([],_,[]).
each_real([tree(_,_,[tree(A,B,C)])|T],tree(A,D,E),[tree(A,B,C)|T2]) :-
    each_real(T,tree(A,D,E),T2).

replace_rel_with_cnj([],[],_).
replace_rel_with_cnj([tree(r(REL,CAT),Ix,Ds)|T0],[tree(r(cnj,CAT),Ix,Ds)|T],REL) :-
    replace_rel_with_cnj(T0,T,REL).




*/
