:- module(alpino_simplify_modifier, [ modifier_transformation/5 ]).

%% correction of alleen/adv => alleen/adj
modifier_transformation(r(Rel,p(du)),[D1,D2],r(Rel,p(du)),[DN,D2],_) :-
    D1 = tree(r(dp,adt_lex(advp,Lem,Alleen,adv,Atts)),[]),
    DN = tree(r(dp,adt_lex(ap,Lem,Alleen,adj,Atts)),[]),
    adv_adj(Alleen).

%% de motor van en drijfveer achter X =>
%% de motor van X en drijfveer erachter
%% after that, each of the individual PPs might be removed without
%% causing trouble
modifier_transformation(r(Rel,p(conj)),[Cnj1,Crd,Cnj2],r(Rel,p(conj)),[CnjA,Crd,CnjB],_) :-
    Crd = tree(r(crd,_),_),
    Cnj1 = tree(r(cnj,p(np)),Cnj1Ds),
    Cnj2 = tree(r(cnj,p(np)),Cnj2Ds0),
    MOD1 = tree(r(mod,p(pp)),[tree(r(hd,_),_),OBJ1]),
    lists:member(MOD1,Cnj1Ds),
    OBJ1 = tree(r(obj1,i(X,_)),_),
    MOD2 = tree(r(mod,p(pp)),[tree(r(hd,adt_lex(_,Prep,_,_,_)),[]),OBJ2]),
    ERPREP=tree(r(mod,adt_lex(pp,ErPrep,_,_,[])),[]),
    replace(MOD2,ERPREP,Cnj2Ds0,Cnj2Ds),
    er_adverb_prep(Prep,ErPrep),
    OBJ2 = tree(r(obj1,i(X)),[]),
    CnjA = tree(r(cnj,p(np)),Cnj1Ds),
    CnjB = tree(r(cnj,p(np)),Cnj2Ds).

modifier_transformation(r(Rel,p(du)),[D1,D2],r(Rel,p(du)),[DN,D2],_) :-
    D1 = tree(r(dp,adt_lex(advp,Lem,Alleen,adv,Atts)),[]),
    DN = tree(r(dp,adt_lex(ppart,Lem,Alleen,adj,Atts)),[]),
    adv_ppart(Alleen).

%% correction niet alleen
modifier_transformation(r(dp,p(advp)),[D1,D2],r(dp,p(ap)),[DN,D2],_) :-
    D1 = tree(r(hd,adt_lex(advp,Lem,Alleen,adv,Atts)),[]),
    DN = tree(r(hd,adt_lex(ap,Lem,Alleen,adj,Atts)),[]),
    adv_adj(Alleen).

%% use hd as context if available
modifier_transformation(Cat,Ds0,Cat,Ds,Ctxt) :-
    lists:member(tree(r(hd,adt_lex(_,Hd,_,_,_)),[]),Ds0),
    lists:select(Tree,Ds0,Ds),
    ignore_modifier(Tree,Ctxt,Hd).
%% otherwise []
modifier_transformation(Cat,Ds0,Cat,Ds,Ctxt) :-
    \+ lists:member(tree(r(hd,adt_lex(_,_,_,_,_)),[]),Ds0),
    lists:select(Tree,Ds0,Ds),
    ignore_modifier(Tree,Ctxt,[]).

%% in obcomp, body can be dp-dp. Remove second dp.
%% meer dan alleen maar codificatie
%% => meer dan codificatie
%% are we sure we select the intended DP? No. But that is bad
%% choice in grammar.
modifier_transformation(r(obcomp,p(cp)),Ds0,r(obcomp,p(cp)),Ds,_) :-
    CMP = tree(r(cmp,_),_),
    BODY = tree(r(body,p(du)),[_,DU1|_]),
    lists:select(CMP,Ds0,[BODY]),
    DU1 = tree(r(dp,NPCAT),NPBODY),
    Ds = [CMP,tree(r(body,NPCAT),NPBODY)].   

modifier_transformation(r(TOP,p(du)),Ds0,r(TOP,Cat),Ds,_) :-
    Tag = tree(r(tag,p(ppart)),TagDs),
    lists:select(Tag,Ds0,[Nucl]),
    Nucl = tree(r(nucl,Cat),Ds),
    Anders = tree(r(mod,adt_lex(ap,anders,_,_,_)),[]),
    lists:select(Anders,TagDs,[Gezegd]),
    Gezegd = tree(r(hd,adt_lex(ppart,zeg,_,_,_)),[]).

%% mijnheer de voorzitter => mijnheer
modifier_transformation(r(Rel,p(np)),Ds0,r(Rel,adt_lex(H1,Mijnheer,H3,H4,H5)),[],_) :-
    Hd = tree(r(hd,adt_lex(H1,Mijnheer,H3,H4,H5)),[]),
    lists:select(Hd,Ds0,Ds1),
    lemma_in(Mijnheer,[mijnheer,meneer,mevrouw]),
    App = tree(r(app,p(np)),DeVz),
    lists:select(App,Ds1,[]),
    De = tree(r(det,adt_lex(_,de,_,_,_)),[]),
    VZ = tree(r(hd,_),_),
    lists:select(De,DeVz,DeVz0),
    lists:select(VZ,DeVz0,[]).

%% me dunkt => 0
modifier_transformation(r(Rel,p(du)),Ds0,r(Rel,Cat),Ds,_) :-
    Nucl = tree(r(nucl,Cat),Ds),
    Tag =  tree(r(tag,p(sv1)),TagDs0),
    lists:select(Nucl,Ds0,Ds1),
    lists:select(Tag,Ds1,[]),
    me_dunkt_ds(TagDs0).

%% de voorzitter Carel Jansen -> Carel Jansen
%% voorzitter Jansen -> Jansen
%% het jaar 1990 -> 1990
modifier_transformation(r(Rel,_),Ds0,r(Rel,AppCat),AppDs,Ctxt) :-
    Hd = tree(r(hd,HdLex),_),
    lists:select(Hd,Ds0,Ds1),
    App = tree(r(app,AppCat0),AppDs),
    lists:select(App,Ds1,Ds2),
    (   is_name(AppCat0,AppDs),
        check_app_name(Rel,AppCat0,AppCat),
	\+ Ctxt = [r(obj1,_)/_,r(predc,p(pp))/_|_] % predc [van type X] =/= prec [van X]
    ;   HdLex = adt_lex(_,Begrip,_,_,_),
	lemma_in(Begrip,[begrip,woord]),
	AppCat0 = adt_lex(np,_,_,_,_),
	AppCat0 = AppCat
    ;   HdLex = adt_lex(_,B,_,noun,_),
     	lemma_in(B,[jaar]),
	AppCat0 = adt_lex(_,N,_,num,_),
	AppCat = adt_lex(_,N,_,noun,[neclass=year]),
	alpino_lex:date_year([N],[])
    ),
    (   Ds2 = [tree(r(det,adt_lex(_,De,_,_,_)),[])],
	lemma_in(De,[de,het])
    ;   Ds2 = []
    ),
    \+ conj_and_index(AppCat0,Rel).

%% de maanden mei en juni --> mei en juni
%% but in that case, it really should be app?
modifier_transformation(r(Rel,_),Ds0,r(Rel,AppCat),AppDs,_) :-
    Hd = tree(r(hd,HdLex),_),
    lists:select(Hd,Ds0,Ds1),
    App = tree(r(mod,AppCat),AppDs),
    lists:select(App,Ds1,Ds2),
    np(App),
    HdLex = adt_lex(_,B,_,noun,_),
    lemma_in(B,[maand]),
    (   Ds2 = [tree(r(det,adt_lex(_,De,_,_,_)),[])],
	lemma_in(De,[de,het])
    ;   Ds2 = []
    ).

modifier_transformation(r(Rel,p(np)),Ds0,r(Rel,p(np)),[Hd|Ds2],_) :-
    Hd = tree(r(hd,HdLex),HdDs),
    lists:select(Hd,Ds0,Ds1),
    is_name(HdLex,HdDs),
    App = tree(r(app,_),_),
    lists:select(App,Ds1,Ds2).

modifier_transformation(r(Rel,p(np)),Ds0,r(Rel,p(np)),Ds,_) :-
    App = tree(r(app,_),AppDs),
    lists:select(App,Ds0,Ds),
    Det = tree(r(det,_),_),
    (   lists:member(Det,AppDs)
    ;   Cnj = tree(r(cnj,_),CnjDs),
	lists:member(Cnj,AppDs),
	lists:member(Det,CnjDs)
    ;   Hd = tree(r(hd,adt_lex(_,AppHdLex,_,_,_)),[]),
	lists:member(Hd,AppDs),
	lemma_in(AppHdLex,[die,één])
    ;   lists:member(tree(r(mod,p(mwu('met name',_))),_),AppDs)
    ;   lists:member(tree(r(hd,adt_lex(_,Wij,_,_,_)),[]),Ds0),
	lemma_in(Wij,[wij])
    ).

%% de realisering van de verbouwing --> de verbouwing
modifier_transformation(r(Rel,p(np)),Ds0,r(Rel,NPCat),Ds,_) :-
    De = tree(r(det,adt_lex(_,de,_,_,_)),[]),
    Hd = tree(r(hd, adt_lex(_,V,_,_,_)),[]),
    lists:select(Hd,Ds0,Ds1),
    lemma_in(V,[kwestie,vraagstuk,verwezenlijking,realisering]),
    lists:select(De, Ds1, Ds2),
    Mod = tree(r(mod,p(pp)),PPDs0),
    lists:select(Mod,Ds2,[]),
    Van = tree(r(hd,adt_lex(_,van,_,_,_)),[]),
    lists:select(Van,PPDs0,PPDs1),
    NP = tree(r(obj1,NPCat),Ds),
    lists:select(NP,PPDs1,[]),
    np(NP).


%% tal van ontwikkelingen -> ontwikkelingen
modifier_transformation(r(Rel,p(np)),Ds0,r(Rel,NPCat),Ds,_) :-
    Hd = tree(r(hd, adt_lex(_,V,_,_,_)),[]),
    lists:select(Hd,Ds0,Ds1),
    lemma_in(V,[tal]), 
    Mod = tree(r(mod,p(pp)),PPDs0),
    lists:select(Mod,Ds1,[]), 
    Van = tree(r(hd,adt_lex(_,van,_,_,_)),[]),
    lists:select(Van,PPDs0,PPDs1), 
    NP = tree(r(obj1,NPCat),Ds),
    lists:select(NP,PPDs1,[]),
    np(NP).

%% de aardige kinderen van de buren -> de kinderen
modifier_transformation(r(Rel,VAR),Ds0,r(Rel,VAR),Ds,Up) :-
    \+ partitive(Ds0),
    \+ nominalization_with_van(Ds0),
    Mod = tree(r(ModRel,ModInfo),_),
    lists:select(Mod,Ds0,Ds),
    Hd = tree(r(hd,Head),HeadDs),
    lists:select(Hd,Ds,DsRest),
    modifier_rel(ModRel,ModInfo),
    \+ pre_wh(VAR,Ds0,Mod),
    \+ important_modifier(Mod,Head,HeadDs,Up,DsRest),
    \+ container_head(Ds,Mod).

%% met name om het gebruik te bevorderen -> om het gebruik te bevorderen
modifier_transformation(r(Rel,VAR),Ds0,r(Rel,VAR),Ds,Up) :-
    Mod = tree(r(ModRel,ModInfo),_),
    lists:select(Mod,Ds0,Ds),
    modifier_rel(ModRel,ModInfo),
    Hd = tree(r(hd,_),_),
    \+ lists:member(Hd,Ds),
    \+ important_modifier(Mod,none,none,Up,Ds).

%% veel te leuk -> te leuk
%% twee meter te ver -> te ver
modifier_transformation(r(Rel,p(advp)),Ds0,r(Rel,p(advp)),Ds,_) :-
    Mod = tree(r(me,_),_),
    lists:select(Mod,Ds0,Ds).

container_head(Ds,Mod):-
    np(Mod),
    Hd = tree(r(hd,adt_lex(_,Zak,_,noun,_)),[]),
    lists:member(Hd,Ds),
    alpino_lex:inv_lex(Zak,Zakje),
    (  alpino_lex:lexicon(noun(_,_,_,measure),Zak,[Zakje],[],_)
    ;  alpino_lex:lexicon(meas_mod_noun(_,_,_,measure),Zak,[Zakje],[],_)
    ).

%% het creeren van X, is meestal direct object of subject, dus niet weg
nominalization_with_van(Ds) :-
    Hd = tree(r(hd,adt_lex(np,_,_,verb,_)),[]),
    lists:select(Hd,Ds,Ds1),
    Ad = tree(r(mod,p(pp)),AdDs),
    AdHd = tree(r(hd,adt_lex(_,van,_,_,_)),[]),
    lists:select(Ad,Ds1,_),
    lists:select(AdHd,AdDs,_).

%% één van de aanwezigen =/= één
%% drie van de mannen =/= drie
partitive(Ds0) :-
    Hd = tree(r(hd,_),[]),
    lists:select(Hd,Ds0,Ds1),
    Ad = tree(r(mod,p(pp)),AdDs),
    lists:select(Ad,Ds1,_),
    lists:select(AdHd,AdDs,_),
    (   Hd = tree(r(hd,adt_lex(_,_,_,num,_)),[]),
        AdHd = tree(r(hd,adt_lex(_,van,_,_,_)),[])
    ;   Hd = tree(r(hd,adt_lex(_,één,_,_,_)),[])
    ;   Hd = tree(r(hd,adt_lex(_,deel,_,_,_)),[]),
        AdHd = tree(r(hd,adt_lex(_,van,_,_,_)),[])
    ;   Hd = tree(r(hd,adt_lex(_,elk,_,_,_)),[]),
        AdHd = tree(r(hd,adt_lex(_,van,_,_,_)),[])
    ;   Hd = tree(r(hd,adt_lex(_,veel,_,_,_)),[]),
        AdHd = tree(r(hd,adt_lex(_,van,_,_,_)),[])
    ).

pre_wh(p(np),Ds,Mod) :-
    Det = tree(r(det,adt_lex(_,Welk,_,_,_)),[]),
    lists:member(Det,Ds),
    lemma(Welk,W),
    lists:member(W,[welk,wat]),
    (   Mod = tree(r(mod,adt_lex(_,Onverschillig,_,_,_)),[]),
	lemma(Onverschillig,O),
	lists:member(O,[eender,gelijk,ongeacht,onverschillig,willekeurig])
    ;   Mod = tree(r(mod,p(mwu('om het even',_))),_)
    ).
    

np(tree(r(_Rel,Cat),_Ds)):-
    simple_np(Cat).
np(tree(r(_,p(conj)),Ds)):-
    Cnj = tree(r(cnj,_),_),
    lists:member(Cnj,Ds),
    np(Cnj).

simple_np(p(np)).
simple_np(adt_lex(np,_,_,_,_)).

important_mod_stem(aldus,_,geschied,_).
important_mod_stem(zo,_,geschied,_).

important_mod_stem(Lem,_) :-
    negatief_element(Lem).

important_mod_stem(afschuwelijk,_).
important_mod_stem(ander,adj).
important_mod_stem(anders,_).
important_mod_stem(behalve,_).
important_mod_stem(daar,_).
important_mod_stem(dan,_).  % for als/dan zinnen. If not als/dan, then already removed
important_mod_stem(er,_).
important_mod_stem(fout,_).
important_mod_stem(goed,_).
important_mod_stem(hier,_).
important_mod_stem(hoe,_).   % ik vraag me af hoe eerlijk ze zijn -> *ik vraag me af eerlijk ze zijn
important_mod_stem(hoeveel,_).   % ik vraag me af hoeveel gemakkelijker ...
important_mod_stem(hoelaat,_).   
important_mod_stem(hoelang,_).   
important_mod_stem(hoeveelste,_).
important_mod_stem(hoogstens,_).
important_mod_stem(incoherent,_).
important_mod_stem(moeilijk,_).   % je kunt moeilijk gaan lopen; ze kunnen daar moeilijk terecht
important_mod_stem(negatief,_).
important_mod_stem(positief,_).
important_mod_stem(slecht,_).
important_mod_stem(te,adv).   % als prep can be ignored "ten vroegste"
important_mod_stem(vaag,_).
important_mod_stem(verkeerd,_).

%% alleen, alleen maar, slechts?
negatief_element(amper).
negatief_element(evenmin).
negatief_element(geen).
negatief_element(geenszins).
negatief_element(nauwelijks).
negatief_element(niemand).
negatief_element(nergens).
negatief_element(niet).
negatief_element(niets).
negatief_element(nimmer).
negatief_element(noch).  % zij kon gisteren noch vandaag aanwezig zijn -> *zij kon aanwezig zijn
negatief_element(nooit).
negatief_element(weinig).


important_mod(r(_,Cat),A,B) :-
    important_mod(Cat,A,B).
important_mod(i(_,L),A,B) :-
    important_mod(L,A,B).
important_mod(adt_lex(_,_,_,num,Atts),_,_) :-
    lists:member(numtype=rang,Atts).
important_mod(adt_lex(_,Lem,_,adj,Atts),_,_) :-
    lists:member(aform=compar,Atts),
    \+ lemma_in(Lem,[eerder,laat,nader,ver]).
important_mod(adt_lex(_,_,_,adj,Atts),_,_) :-
    lists:member(aform=super,Atts).
important_mod(adt_lex(_,_,_,adj,Atts),_,_) :-
    lists:member(iets=true,Atts).
important_mod(adt_lex(_,Stem,_,Pos,_),_,_):-
    lemma(Stem,Stem2),
    important_mod_stem(Stem2,Pos).
important_mod(adt_lex(_,Stem,_,Pos,_),adt_lex(_,Stem2,_,Pos2,_),_):-
    lemma(Stem,StemA),
    lemma(Stem2,Stem2A),
    important_mod_stem(StemA,Pos,Stem2A,Pos2).

ignore_modifier(tree(r(Rel,i(_,Cat)),Ds),Ctxt,Hd) :-
    ignore_modifier(tree(r(Rel,Cat),Ds),Ctxt,Hd).

ignore_modifier(Tree,Ctxt,Hd) :-
    ignore_modifier_pattern(Pattern,Ctxt,Hd),
    match_pattern(Pattern,Tree).

ignore_modifier(tree(r(mod,adt_lex(_,W,_,Pos,Atts)),[]),[r(_,p(MCat))/_|_],Hd) :-
    ignore_modifier_stem(W,Pos,Atts,MCat,Hd).

ignore_modifier(tree(r(mod,adt_lex(_,W,_,Pos,Atts)),[]),[r(_,i(_,p(MCat)))/_|_],Hd) :-
    ignore_modifier_stem(W,Pos,Atts,MCat,Hd).

ignore_modifier(tree(r(mod,adt_lex(_,dan,_,_,_)),[]),Ctxt,_) :-
    \+ lists: append(_,[r(nucl,p(smain))/_,r(_,p(du))/[tree(r(sat,_),_)|_]|_],Ctxt).

ignore_modifier(tree(r(mod,p(ap)),[tree(r(hd,adt_lex(_,zo,_,_,_)),[]),tree(r(obcomp,i(_)),[])]),_,_).

ignore_modifier_pattern(mod=dt(conj,[crd=en,
				     cnj=meer,
				     cnj=meer]),_,_).

ignore_modifier_pattern(mod=dt(advp,
			       [hd=wel,
				mod=degelijk]),_,_).

ignore_modifier_pattern(mod=dt(pp,
			       [hd={[in,op]},
				obj1=dt(np,
					[hd=plaats,
					 det=de,
					 mod=l(_,num,[numtype=rang])])]),_,_).

ignore_modifier_pattern(mod=dt(pp,
			       [hd=in,
				obj1=dt(np,
					[hd=geval,
					 {[det,mod]}={[ander,bepalen,dat,de,deze,die,dit,elk,
						       ieder,kom_voor,sommig,veel]}
					])]),_,_).

ignore_modifier_pattern(mod=dt(pp,
			       [hd={[in,over]},
				obj1=dt(np,[hd=algemeen,
					    det=het])]),_,_).

ignore_modifier_pattern(mod=dt(np,
			       [hd=beetje,
				det=een]),_,_).

ignore_modifier_pattern(mod=dt(advp,
			       [hd=ongeveer,
				mod=zo]),_,_).

ignore_modifier_pattern(mod=dt(pp,
			      [hd=op,
			       obj1=dt(np,
				       [det=l(_,_,_),
					hd=moment])]),_,_).

ignore_modifier_pattern(mod=dt(pp,
			      [hd=met,
			       obj1=dt(np,
				       [mod=ander,
					hd=woord])]),_,_).

ignore_modifier_pattern(mod=dt(pp,
			      [hd=in,
			       obj1=dt(np,
				       [mod=_,
					hd=opzicht])]),_,_).

ignore_modifier_pattern(mod=dt(pp,
			      [hd=in,
			       obj1=dt(np,
				       [det=_,
					hd=opzicht])]),_,_).

ignore_modifier_pattern(mod=dt(pp,
			      [hd=in,
			       obj1=dt(np,
				       [det=l(_,_,_),
					hd=context])]),_,_).

ignore_modifier_pattern(mod=dt(pp,
			      [hd=naar,
			       obj1=dt(np,
				       [det=mijn,
					hd=idee])]),_,_).

ignore_modifier_pattern(mod=dt(pp,
			      [hd=naar,
			       obj1=dt(np,
				       [det=mijn,
					hd=smaak])]),_,_).

ignore_modifier_pattern(mod=dt(advp,
			       [hd=eens,
				mod=dt(mwu(_,_),
				       [mwp=te,
					mwp=veel])]),_,_).

ignore_modifier_pattern(mod=dt(ap,
			       [hd=hoe,
				mod=dt(mwu(_,_),
				       [mwp=dan,
					mwp=ook])]),_,_).

ignore_modifier_pattern(mod=dt(mwu(_,_),
			       [mwp=met,
				mwp=name]),_,_).

ignore_modifier_pattern(mod=dt(pp,
			       [hd=volgens,
				obj1=mij]),_,_).

ignore_modifier_pattern(mod=dt(pp,
			       [hd=in,
				obj1=principe]),_,_).

ignore_modifier_pattern(det=dt(detp,
			       [hd=wat,
				mod=heel]),_,_).

ignore_modifier_pattern(mod=dt(pp,
			       [hd=tot,
				hdf=toe,
				obj1=nu
			       ]),_,_).
ignore_modifier_pattern(mod=dt(pp,
			       [hd=tot,
				hdf=toe,
				obj1=nog
			       ]),_,_).


match_pattern(RelPat=Pat,tree(r(Rel,Cat),Ds)) :-
    match_pattern_rel(RelPat,Rel),
    match_pattern_node(Pat,Cat,Ds).

match_pattern_rel(Var,_) :-
    var(Var),
    !.
match_pattern_rel({List},Rel) :-
    !,
    lists:member(Rel,List).
match_pattern_rel(R,R).

match_pattern_node(Var,_,_) :-
    var(Var), !.
match_pattern_node({List},Cat,Ds) :-
    !,
    Cat = adt_lex(_,Lem,_,_,_),
    Ds = [],
    lists:member(Lem,List).
match_pattern_node(dt(Cat0,PatDs),Cat,Ds) :-
    !,
    Cat = p(Cat0),
    match_pattern_ds(PatDs,Ds).
match_pattern_node(l(LemPat,Pos,PatAtts),Cat,Ds) :-
    !,
    Cat = adt_lex(_,Lem,_,Pos,Atts),
    Ds=[],
    match_pattern_lem(LemPat,Lem),
    match_pattern_atts(PatAtts,Atts).
match_pattern_node(Atom,Cat,Ds) :-
    atom(Atom),
    !,
    Cat = adt_lex(_,Atom,_,_,_),
    Ds = [].

match_pattern_lem(Var,_) :-
    var(Var),
    !.
match_pattern_lem({List},Lem) :-
    !,
    lists:member(Lem,List).
match_pattern_lem(L,L).

match_pattern_ds(Var,_) :-
    var(Var), !.
match_pattern_ds([],[]).
match_pattern_ds([H|T],Ds0) :-
    lists:select(D,Ds0,Ds),
    match_pattern(H,D),
    match_pattern_ds(T,Ds).

match_pattern_atts(Var,_):-
    var(Var),
    !.
match_pattern_atts([],_).
match_pattern_atts([PatAtt|PatAtts],Atts) :-
    lists:member(PatAtt,Atts),
    match_pattern_atts(PatAtts,Atts).


ignore_modifier_stem(absoluut,_,_,_,_).
ignore_modifier_stem(al,_,_,_,_).
ignore_modifier_stem(alleen,_,_,_,_).
ignore_modifier_stem(allereerst,_,_,_,_).
ignore_modifier_stem(altijd,_,_,_,_).
ignore_modifier_stem(amper,_,_,_,_).
ignore_modifier_stem(bepaald,_,_,_,_).
ignore_modifier_stem(beslist,_,_,_,_).
ignore_modifier_stem(bijvoorbeeld,_,_,_,_).
ignore_modifier_stem(bijzonder,_,_,ap,_).
ignore_modifier_stem(bovendien,_,_,_,_).
ignore_modifier_stem(buitengewoon,_,_,ap,_).
ignore_modifier_stem(circa,_,_,_,_).
ignore_modifier_stem(daarom,_,_,_,_).
ignore_modifier_stem(daarbij,_,_,_,_).
ignore_modifier_stem(daarentegen,_,_,_,_).
ignore_modifier_stem(daarnaast,_,_,_,_).
ignore_modifier_stem(daarna,_,_,_,_).
ignore_modifier_stem(daarnet,_,_,_,_).
ignore_modifier_stem(derhalve,_,_,_,_).
ignore_modifier_stem(destijds,_,_,_,_).
ignore_modifier_stem(duidelijk,_,_,_,_).
ignore_modifier_stem(dus,_,_,_,_).
ignore_modifier_stem(echt,_,_,_,_).
ignore_modifier_stem(echter,_,_,_,_).
ignore_modifier_stem(eens,_,_,_,_).
ignore_modifier_stem(eerst,_,_,_,_).
ignore_modifier_stem(enkel,adj,_,_,_).
ignore_modifier_stem(erg,_,_,_,_).
ignore_modifier_stem(even,_,_,_,_).
ignore_modifier_stem(evenwel,_,_,_,_).
ignore_modifier_stem(detailleren,adj,_,np,_).
ignore_modifier_stem(globaal,_,_,_,_).
ignore_modifier_stem(grondig,_,_,_,_).
ignore_modifier_stem(heel,_,_,np,_).
ignore_modifier_stem(heel,_,_,ap,_).
ignore_modifier_stem(heel,_,_,advp,_).   % heel wat => *wat  todo: heel weinig, heel veel -> weinig, veel
ignore_modifier_stem(heel,_,_,detp,weinig).
ignore_modifier_stem(heel,_,_,detp,veel).
ignore_modifier_stem(helaas,_,_,_,_).
ignore_modifier_stem(helemaal,_,_,_,_).
ignore_modifier_stem(hierbij,_,_,_,_).
ignore_modifier_stem(immers,_,_,_,_).
ignore_modifier_stem(inderdaad,_,_,_,_).
ignore_modifier_stem(inmiddels,_,_,_,_).
ignore_modifier_stem(langzamerhand,_,_,_,_).
ignore_modifier_stem(maar,_,_,_,_).
ignore_modifier_stem(meestal,_,_,_,_).
ignore_modifier_stem(misschien,_,_,_,_).
ignore_modifier_stem(namelijk,_,_,_,_).
ignore_modifier_stem(natuurlijk,_,_,_,_).
ignore_modifier_stem(nauwgezet,_,_,_,_).
%ignore_modifier_stem(net,adv,_,_,_).   % ?? net zo goed -> zo goed
ignore_modifier_stem(nochtans,_,_,_,_).
ignore_modifier_stem(nog,_,_,_,_).
ignore_modifier_stem(nogal,_,_,_,_).
ignore_modifier_stem(nogmaals,_,_,_,_).
ignore_modifier_stem(nu,_,_,_,_).
ignore_modifier_stem(onderhand,_,_,_,_).
ignore_modifier_stem(ongetwijfeld,_,_,_,_).
ignore_modifier_stem(ongeveer,_,_,_,_).
ignore_modifier_stem(onlangs,_,_,_,_).
ignore_modifier_stem(opeens,_,_,_,_).
ignore_modifier_stem(opnieuw,_,_,_,_).
ignore_modifier_stem(overigens,_,_,_,_).
ignore_modifier_stem(ook,_,_,_,_).
ignore_modifier_stem(precies,_,_,_,_).
ignore_modifier_stem(reeds,_,_,_,_).
ignore_modifier_stem(ronduit,_,_,_,_).
ignore_modifier_stem(sowieso,_,_,_,_).
ignore_modifier_stem(specifiek,_,_,np,_).
ignore_modifier_stem(steeds,_,_,_,_).
ignore_modifier_stem(tenslotte,_,_,_,_).
ignore_modifier_stem(terdege,_,_,_,_).
ignore_modifier_stem(tevens,_,_,_,_).
ignore_modifier_stem(toch,_,_,_,_).
ignore_modifier_stem(toen,_,_,_,_).
ignore_modifier_stem(trouwens,_,_,_,_).
ignore_modifier_stem(uiteindelijk,_,_,_,_).
ignore_modifier_stem(uiteraard,_,_,_,_).
ignore_modifier_stem(vaak,_,_,_,_).
ignore_modifier_stem(veel,_,_,_,_).
%ignore_modifier_stem(veel,adv,_,_,_).  % meer, veel is adj  TODO:  veel meer -> meer
%% ignore_modifier_stem(ver,adj,Atts,_) :-  % verder willen met; verder brengen
%%   lists:member(aform=compar,Atts).
ignore_modifier_stem(verdommen,_,_,_,_).
ignore_modifier_stem(volkomen,_,_,_,_).
ignore_modifier_stem(volledig,_,_,_,_).
ignore_modifier_stem(volstrekt,_,_,_,_).
ignore_modifier_stem(vooral,_,_,_,_).
ignore_modifier_stem(duur_voort,_,_,_,_).
ignore_modifier_stem(vrijwel,_,_,_,_).
ignore_modifier_stem(weer,_,_,_,_).
ignore_modifier_stem(wel,_,_,_,_).
ignore_modifier_stem(weliswaar,_,_,_,_).
ignore_modifier_stem(wellicht,_,_,_,_).
ignore_modifier_stem(zeer,_,_,_,_).
ignore_modifier_stem(zelfs,_,_,_,_).
ignore_modifier_stem(zojuist,_,_,_,_).

important_modifier_pattern(mod=dt(mwu(_,_),[mwp=dan,mwp=ook])).

important_modifier(_,adt_lex(smain,_,_,_,_),[],_,[]). % final dependent of main verb in smain should stay
                                                     % "hierin staat : we vertrekken "
%% werkt niet voor:
%% In dit artikel staat aangegeven dat de Raad als enige instelling de uitvoerende bevoegdheden van de Commissie mag vaststellen .

important_modifier(_,p(mwu('\'s','\'s')),_,_,_).
%% ik lust geen brood met kaas =/= ik lust geen brood
important_modifier(tree(r(mod,p(Cat)),_),adt_lex(np,_,_,_,_),_,_,DsRest) :-
    lists:member(Cat,[rel,pp]),
    alpino_simplify_split:contains_negatief_element(DsRest).
important_modifier(tree(r(mod,adt_lex(_,zo,_,_,_)),[]),_,_,[r(tag,p(smain))/_|_],_).  % introduces dip. "root, zo begin hij zijn verhaal"
important_modifier(_,adt_lex(_,Die,_,_,_),[],[r(obj1,p(np))/_,r(_,p(pp))/_|_],[]) :-
    lemma_in(Die,[die,dat]).
important_modifier(Tree,_,_,_,_) :-
    important_modifier_pattern(Pattern),
    match_pattern(Pattern,Tree).
important_modifier(tree(Cat,_),Hd,HdDs,_,_):-
    important_mod(Cat,Hd,HdDs).
important_modifier(tree(_Cat,Ds),Hd,HdDs,_,_) :-
    lists:member(Mod,Ds),
    important_modifier1(Mod,Hd,HdDs).
%% PPs met "geen" function as negation
%% in geen honderd jaar
%% met geen mogelijkheid
important_modifier(tree(r(mod,p(pp)),Ds),_,_,_,_) :-
    lists:member(tree(r(obj1,_),ObjDs),Ds),
    lists:member(tree(r(det,adt_lex(_,Geen,_,_,_)),[]),ObjDs),
    lemma_in(Geen,[geen]).
important_modifier(tree(r(mod,p(rel)),_),_,_,[r('--',p(np))/_|_],_).
important_modifier(tree(r(mod,p(rel)),_),ADTLEX,[],_,_) :-
    alpino_simplify_split:forbid_rel_split(ADTLEX,[]).
important_modifier(tree(r(mod,p(conj)),Ds),C0,C1,C2,C3) :-
    lists:member(tree(r(cnj,p(rel)),XX),Ds),
    important_modifier(tree(r(mod,p(rel)),XX),C0,C1,C2,C3).
important_modifier(tree(r(mod,adt_lex(VanCat,Van,_,VanPos,_)),[]),adt_lex(GebruikCat,Gebruik,_,GebruikPos,_),[],_,_) :-
    (   check_pmi(Gebruik,GebruikPos,GebruikCat,Van,VanPos,VanCat)
    ->  true
    ;   hdrug_util:debug_message(1,"check_pmi: dropping modifier ~w ~w~n",[Gebruik,Van]),
	fail
    ).


important_modifier(tree(r(mod,p(_)),PPDS),adt_lex(GebruikCat,Gebruik,_,GebruikPos,_),[],_,_) :-
    PREPD = tree(r(hd,adt_lex(VanCat,Van,_,VanPos,_)),[]),
    lists:member(PREPD,PPDS),
    (   check_pmi(Gebruik,GebruikPos,GebruikCat,Van,VanPos,VanCat)
    ->  true
    ;   hdrug_util:debug_message(1,"check_pmi: dropping modifier ~w ~w~n",[Gebruik,Van]),
	fail
    ).

:- hdrug_util:initialize_flag(pmi_modifier_threshold,2000).

check_pmi(Gebruik0,GebruikPos0,GebruikCat,Van0,VanPos0,VanCat) :-
    hdrug_util:hdrug_flag(pmi_modifier_threshold,THRESHOLD),
    adapt_psp(GebruikCat,Gebruik0,Gebruik),
    adapt_psp(VanCat,Van0,Van),
    adapt_pos(GebruikPos0,GebruikPos),
    adapt_pos(VanPos0,VanPos),
    hdrug_util:debug_message(2,"checking modifier ~w ~w ~w ~w... ~n",[Gebruik,GebruikPos,Van,VanPos]),
    alpino_penalties:corpus_frequency_lookup(dep35(Van,VanPos,hd/mod,GebruikPos,Gebruik),Val),
    hdrug_util:debug_message(2,"checking modifier ~w ~w ~w ~w: ~w ~n",[Gebruik,GebruikPos,Van,VanPos,Val]),
    Val > THRESHOLD,
    !,
    hdrug_util:debug_message(1,"keeping modifier ~w ~w: ~w ~n",[Gebruik,Van,Val]).

adapt_pos(X,X).
adapt_pos(noun,noun(tmp)).
adapt_pos(noun,noun(meas_mod)).
adapt_pos(noun,noun(amount_meas_mod)).
adapt_pos(noun,noun(year)).
adapt_pos(noun,noun(mod)).
adapt_pos(noun,noun(post_n_n)).
adapt_pos(noun,noun(ge_v_noun)).
adapt_pos(name,name('ORG')).
adapt_pos(name,name('PER')).
adapt_pos(name,name('LOC')).
adapt_pos(name,name('MISC')).
adapt_pos(num,num(hoofd)).
adapt_pos(num,num(rang)).
adapt_pos(num,num(score)).
adapt_pos(det,det(nwh)).
adapt_pos(det,det(adj_number)).
adapt_pos(verb,verb(v_noun)).
adapt_pos(pron,pron(rel)).
adapt_pos(pron,pron(nwh)).
adapt_pos(adj,adj(ywh)).
adapt_pos(adj,adj(iets)).
adapt_pos(adv,adv(eenmaal)).
adapt_pos(adv,adv(sentence)).
adapt_pos(adv,adv(tmp)).
adapt_pos(adv,adv(ywh)).
adapt_pos(adv,adv(hoe)).
adapt_pos(adv,adv(pre_wh)).
adapt_pos(adv,adv(dir)).
adapt_pos(adv,adv(loc)).
adapt_pos(adv,adv(predm)).
adapt_pos(adv,adv(postadj)).
adapt_pos(adv,adv(postadv)).
adapt_pos(adv,adv(post_wh)).
adapt_pos(adv,adv(tmp_app)).
adapt_pos(adv,adv(postnp)).
adapt_pos(adv,adv(intensifier)).
adapt_pos(adv,adv(me_intensifier)).
adapt_pos(adv,adv(er_loc)).
adapt_pos(pp,pp(waar)).
adapt_pos(pp,pp(er)).


adapt_psp(ap,achterste,achterst).
adapt_psp(ppart,Zeggen,Gezegd) :- 
    alpino_genlex:dict_entry(Zeggen,adjective(Inf),Gezegd),
    ge_adj(Inf).
adapt_psp(_,X,X).

ge_adj(ge_no_e(_)).
ge_adj(ge_both(_)).

important_modifier1(tree(Cat,_),Hd,HdDs):-
    important_mod(Cat,Hd,HdDs).

modifier_rel(_,i(_)) :-
    !,
    fail. % e.g. in conjunction, let other modifier determine if you stay

modifier_rel(Rel,i(X,P)) :-
    nonvar(X),
    modifier_rel(Rel,P).

modifier_rel(predm,_).
modifier_rel(mod,_).

me_dunkt_ds(TagDs) :-
    Hd = tree(r(hd,adt_lex(_,Denk,_,_,_)),[]),
    Me = tree(r(su,adt_lex(_,Mij,_,_,_)),[]),
    lists:select(Hd,TagDs,[Me]),
    me_dunkt(Denk,Mij).

me_dunkt(dunk,me).
me_dunkt(dunk,mij).
me_dunkt(denk,ik).
me_dunkt(vermoed,ik).
me_dunkt(vrees,ik).
me_dunkt(geloof,ik).
me_dunkt(lijk,me).
me_dunkt(lijk,mij).

%% meneer X , ik begrijp u niet  ==>
%% X , ik begrijp u niet (only if X is a PER)
check_app_name(tag,adt_lex(A,B,C,D,Atts0),Result) :-
    lists:select(neclass=Val,Atts0,Atts),
    Val \= 'PER',
    !,
    Result=adt_lex(A,B,C,D,[neclass='PER'|Atts]).
check_app_name(_,L,L).

adv_adj(alleen).
adv_adj(onmiddellijk).

adv_ppart(gemiddeld).

is_name(adt_lex(_,_,_,name,_),_).
is_name(_,Ds) :-
    lists:member(tree(r(mwp,Cat),MwpDs),Ds),
    is_name(Cat,MwpDs).
is_name(_,Ds) :-
    lists:member(tree(r(cnj,Cat),MwpDs),Ds),
    is_name(Cat,MwpDs).

lemma_in({Lemma},List):-
    !,
    lists:member(L,Lemma),
    lists:member(L,List).
lemma_in(Lemma,List) :-
    lists:member(Lemma,List).

lemma({Lemma},Lem) :-
    !,
    lists:member(Lem,Lemma).
lemma(L,L).

conj_and_index(i(_,_),cnj).

%% must replace something...
replace(El0,El,[El0|Tail],[El|Tail]).
replace(El0,El,[X|Tail0],[X|Tail]):-
    replace(El0,El,Tail0,Tail).

er_adverb_prep(aan,eraan).
er_adverb_prep(achter,erachter).
er_adverb_prep(bij,erbij).
er_adverb_prep(binnen,erbinnen).
er_adverb_prep(boven,erboven).
er_adverb_prep(buiten,erbuiten).
er_adverb_prep(door,erdoor).
er_adverb_prep(in,erin).
er_adverb_prep(langs,erlangs).
er_adverb_prep(met,ermee).
er_adverb_prep(na,erna).
er_adverb_prep(naar,ernaar).
er_adverb_prep(naast,ernaast).
er_adverb_prep(om,erom).
er_adverb_prep(onder,eronder).
er_adverb_prep(op,erop).
er_adverb_prep(over,erover).
er_adverb_prep(tegen,ertegen).
er_adverb_prep(tot,ertoe).
er_adverb_prep(tussen,ertussen).
er_adverb_prep(uit,eruit).
er_adverb_prep(van,ervan).
er_adverb_prep(vanaf,ervanaf).
er_adverb_prep(vanuit,ervanuit).
er_adverb_prep(voor,ervoor).
