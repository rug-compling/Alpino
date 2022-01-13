:- module(alpino_simplify_modifier, [ apply_modifier_transformations/2 ]).

%% --------------------------------------------------------------------------------------------- %%

%%% now: just do a transformation anywhere, until no further transformations are possible
%%%      so we do not assume that a single tree-walk suffices, because a transformation downstairs
%%%      may make another transformation upstairs possible
%%% old situation: pragmatisch maar ook zeer juist => pragmatisch maar zeer juist
%%%                pragmatisch maar zeer juist => pragmatisch maar juist

apply_modifier_transformations(Tree0,Tree) :-
    apply_a_transformation(Tree0,Tree1),
    !,
    apply_modifier_transformations(Tree1,Tree).
apply_modifier_transformations(Tree,Tree).

apply_a_transformation(tree(Cat0,Ds0),tree(Cat,Ds)) :-
    modifier_transformation_and_flatten(Cat0,Ds0,Cat,Ds,[Cat0]).
apply_a_transformation(tree(Cat,Ds0),tree(Cat,Ds)) :-
    apply_a_transformation_list(Ds0,Ds,[Cat]).

apply_a_transformation(tree(Cat0,Ds0),tree(Cat,Ds),Up) :-
    modifier_transformation_and_flatten(Cat0,Ds0,Cat,Ds,[Cat0|Up]).
apply_a_transformation(tree(Cat,Ds0),tree(Cat,Ds),Up) :-
    apply_a_transformation_list(Ds0,Ds,[Cat|Up]).

apply_a_transformation_list([H0|T],[H|T],Up) :-
    apply_a_transformation(H0,H,Up).
apply_a_transformation_list([H|T0],[H|T],Up) :-
    apply_a_transformation_list(T0,T,Up).

modifier_transformation_and_flatten(r(Rel,Cat0),Ds0,r(Rel,Cat),Ds,Up) :-
    modifier_transformation(r(Rel,Cat0),Ds0,r(Rel,Cat1),Ds1,Up),
    flatten(Cat1,Ds1,Cat,Ds).

modifier_transformation(r(Rel,VAR),A,
			r(Rel2,i(X,Cat2)),B,Up) :-
    nonvar(VAR),
    VAR = i(X,Cat),
    modifier_transformation(r(Rel,Cat),A,
			    r(Rel2,Cat2),B,Up).

%% het begrip industrialisering --> industrialisering
%% de voorzitter Carel Jansen -> Carel Jansen
modifier_transformation(r(Rel,_),Ds0,r(Rel,AppCat),AppDs,_) :-
    Hd = tree(r(hd,HdLex),_),
    lists:select(Hd,Ds0,Ds1),
    App = tree(r(app,AppCat),AppDs),
    lists:select(App,Ds1,Ds2),
    (   AppCat = adt_lex(_,_,_,name,_)
    ;   AppCat = p(mwu(_,_))
    ;   HdLex = adt_lex(_,B,_,noun,_),
	lists:member(B,[begrip,jaar,maand,periode,term,uitdrukking,woord])
    ),
    (   Ds2 = [tree(r(det,adt_lex(_,De,_,_,_)),[])],
	lists:member(De,[de,het])
    ;   Ds2 = []
    ).

%% de maanden mei en juni --> mei en juni
%% but in that case, it really should be app?
modifier_transformation(r(Rel,_),Ds0,r(Rel,AppCat),AppDs,_) :-
    Hd = tree(r(hd,HdLex),_),
    lists:select(Hd,Ds0,Ds1),
    App = tree(r(mod,AppCat),AppDs),
    lists:select(App,Ds1,Ds2),
    np(App),
    HdLex = adt_lex(_,B,_,noun,_),
    lists:member(B,[maand]),
    (   Ds2 = [tree(r(det,adt_lex(_,De,_,_,_)),[])],
	lists:member(De,[de,het])
    ;   Ds2 = []
    ).

%% de realisering van de verbouwing --> de verbouwing
modifier_transformation(r(Rel,p(np)),Ds0,r(Rel,NPCat),Ds,_) :-
    De = tree(r(det,adt_lex(_,de,_,_,_)),[]),
    Hd = tree(r(hd, adt_lex(_,V,_,_,_)),[]),
    lists:select(Hd,Ds0,Ds1),
    lists:member(V,[kwestie,vraagstuk,verwezenlijking,realisering]),
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
    lists:member(V,[tal]), 
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
    lists:member(Hd,Ds),
    modifier_rel(ModRel,ModInfo),
    \+ important_modifier(Mod,Head,HeadDs,Up),
    \+ container_head(Ds,Mod).

%% met name om het gebruik te bevorderen -> om het gebruik te bevorderen
modifier_transformation(r(Rel,VAR),Ds0,r(Rel,VAR),Ds,Up) :-
    Mod = tree(r(ModRel,ModInfo),_),
    lists:select(Mod,Ds0,Ds),
    modifier_rel(ModRel,ModInfo),
    Hd = tree(r(hd,_),_),
    \+ lists:member(Hd,Ds),
    \+ important_modifier(Mod,none,none,Up).

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

%% het creeren van X, is meestal direct object, dus niet weg
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
    ).

np(tree(r(_Rel,Cat),_Ds)):-
    simple_np(Cat).
np(tree(r(_,p(conj)),Ds)):-
    Cnj = tree(r(cnj,_),_),
    lists:member(Cnj,Ds),
    np(Cnj).

simple_np(p(np)).
simple_np(adt_lex(np,_,_,_,_)).


important_mod_stem(afschuwelijk,_).
important_mod_stem(amper,_).
important_mod_stem(ander,adj).
important_mod_stem(anders,_).
important_mod_stem(daar,_).
important_mod_stem(er,_).
important_mod_stem(fout,_).
important_mod_stem(geenszins,_).
important_mod_stem(goed,_).
important_mod_stem(hier,_).
important_mod_stem(hoe,_).   % ik vraag me af hoe eerlijk ze zijn -> *ik vraag me af eerlijk ze zijn
important_mod_stem(nauwelijks,_).
important_mod_stem(negatief,_).
important_mod_stem(positief,_).
important_mod_stem(niet,_).
important_mod_stem(niets,_).
important_mod_stem(nimmer,_).
important_mod_stem(nooit,_).
important_mod_stem(slecht,_).
important_mod_stem(te,adv).   % als prep can be ignored "ten vroegste"
important_mod_stem(vaag,_).
important_mod_stem(verkeerd,_).
important_mod_stem(weinig,_).

important_mod(r(_,Cat),A,B) :-
    important_mod(Cat,A,B).
important_mod(i(_,L),A,B) :-
    important_mod(L,A,B).
important_mod(adt_lex(_,_,_,num,Atts),_,_) :-
    lists:member(numtype=rang,Atts).
important_mod(adt_lex(_,Lem,_,adj,Atts),_,_) :-
    lists:member(aform=compar,Atts),
    \+ lists:member(Lem,[eerder,laat,nader,ver]).
important_mod(adt_lex(_,_,_,adj,Atts),_,_) :-
    lists:member(aform=super,Atts).
important_mod(adt_lex(_,_,_,adj,Atts),_,_) :-
    lists:member(iets=true,Atts).
important_mod(adt_lex(_,Stem,_,Pos,_),_,_):-
    important_mod_stem(Stem,Pos).


%% todo
%% "in sommige/bepaalde gevallen"
ignore_modifier(tree(r(mod,p(pp)),
		     [tree(r(hd,adt_lex(pp,in,_,_,_)),[]),
		      tree(r(obj1,p(np)),[tree(r(hd,adt_lex(np,geval,_,_,_)),[]),
					  tree(r(DetMod,adt_lex(_,Sommig,_,_,_)),[])])])) :-
    lists:member(DetMod,[det,mod]),
    lists:member(Sommig,[ander,bepalen,dat,de,deze,die,dit,elk,ieder,kom_voor,sommig,veel]).

%% "in het algemeen"
ignore_modifier(tree(r(mod,p(pp)),
		     [tree(r(hd,adt_lex(pp,In,_,prep,_)),[]),
		      tree(r(obj1,p(np)),
			   [tree(r(hd,adt_lex(np,algemeen,_,noun,_)),[]),
			    tree(r(det,adt_lex(detp,het,_,det,_)),[])])])) :-
    lists:member(In,[in,over]).

%% "een beetje"
ignore_modifier(tree(r(mod,p(np)),
		     [tree(r(hd,adt_lex(np,beetje,_,noun,_)),[]),
		      tree(r(det,adt_lex(detp,een,_,det,_)),[])]
		    )).
%% "op X moment"
ignore_modifier(tree(r(mod,p(pp)),
		     [tree(r(hd,adt_lex(pp,op,_,prep,_)),[]),
		      tree(r(obj1,p(np)),
			   [tree(r(hd,adt_lex(np,moment,_,noun,_)),[]),
			    tree(r(det,adt_lex(detp,_,_,det,_)),[])])])).

%% eens te meer
ignore_modifier(tree(r(mod,p(advp)),
		     [tree(r(hd,adt_lex(advp,eens,_,_,_)),[]),
		      tree(r(mod,p(mwu('te veel','te veel'))),
			   [tree(r(mwp,adt_lex(advp,te,_,_,_)),[]),
			    tree(r(mwp,adt_lex(advp,veel,_,_,_)),[])])])).

%% volgens mij
ignore_modifier(tree(r(mod,p(pp)),
		     [tree(r(hd,adt_lex(pp,volgens,_,_,_)),[]),
		      tree(r(obj1,adt_lex(np,mij,_,_,_)),[])
		     ])).


ignore_modifier(tree(r(mod,adt_lex(_,W,_,Pos,Atts)),[])) :-
    ignore_modifier_stem(W,Pos,Atts).


ignore_modifier_stem(al,_,_).
ignore_modifier_stem(alleen,_,_).
ignore_modifier_stem(altijd,_,_).
ignore_modifier_stem(amper,_,_).
ignore_modifier_stem(bepaald,_,_).
ignore_modifier_stem(beslist,_,_).
ignore_modifier_stem(bijvoorbeeld,_,_).
ignore_modifier_stem(bovendien,_,_).
ignore_modifier_stem(circa,_,_).
ignore_modifier_stem(daarom,_,_).
ignore_modifier_stem(daarbij,_,_).
ignore_modifier_stem(daarnaast,_,_).
ignore_modifier_stem(daarna,_,_).
ignore_modifier_stem(daarnet,_,_).
ignore_modifier_stem(dan,_,_).
ignore_modifier_stem(derhalve,_,_).
ignore_modifier_stem(destijds,_,_).
ignore_modifier_stem(duidelijk,_,_).
ignore_modifier_stem(dus,_,_).
ignore_modifier_stem(echt,_,_).
ignore_modifier_stem(echter,_,_).
ignore_modifier_stem(eens,_,_).
ignore_modifier_stem(eerst,_,_).
ignore_modifier_stem(enkel,adj,_).
ignore_modifier_stem(erg,_,_).
ignore_modifier_stem(even,_,_).
ignore_modifier_stem(evenwel,_,_).
ignore_modifier_stem(heel,_,_).
ignore_modifier_stem(helaas,_,_).
ignore_modifier_stem(hierbij,_,_).
ignore_modifier_stem(immers,_,_).
ignore_modifier_stem(inderdaad,_,_).
ignore_modifier_stem(inmiddels,_,_).
ignore_modifier_stem(langzamerhand,_,_).
ignore_modifier_stem(maar,_,_).
ignore_modifier_stem(meestal,_,_).
ignore_modifier_stem(misschien,_,_).
ignore_modifier_stem(namelijk,_,_).
ignore_modifier_stem(natuurlijk,_,_).
ignore_modifier_stem(nauwgezet,_,_).
ignore_modifier_stem(net,adv,_).
ignore_modifier_stem(nochtans,_,_).
ignore_modifier_stem(nog,_,_).
ignore_modifier_stem(nogal,_,_).
ignore_modifier_stem(nogmaals,_,_).
ignore_modifier_stem(nu,_,_).
ignore_modifier_stem(onderhand,_,_).
ignore_modifier_stem(ongetwijfeld,_,_).
ignore_modifier_stem(ongeveer,_,_).
ignore_modifier_stem(onlangs,_,_).
ignore_modifier_stem(opeens,_,_).
ignore_modifier_stem(opnieuw,_,_).
ignore_modifier_stem(overigens,_,_).
ignore_modifier_stem(ook,_,_).
ignore_modifier_stem(reeds,_,_).
ignore_modifier_stem(sowieso,_,_).
ignore_modifier_stem(steeds,_,_).
ignore_modifier_stem(tenslotte,_,_).
ignore_modifier_stem(toch,_,_).
ignore_modifier_stem(toen,_,_).
ignore_modifier_stem(trouwens,_,_).
ignore_modifier_stem(uiteindelijk,_,_).
ignore_modifier_stem(uiteraard,_,_).
ignore_modifier_stem(vaak,_,_).
ignore_modifier_stem(veel,_,_).
%ignore_modifier_stem(veel,adv,_).  % meer, veel is adj  TODO:  veel meer -> meer
%% ignore_modifier_stem(ver,adj,Atts) :-  % verder willen met; verder brengen
%%   lists:member(aform=compar,Atts).
ignore_modifier_stem(volledig,_,_).
ignore_modifier_stem(volstrekt,_,_).
ignore_modifier_stem(vooral,_,_).
ignore_modifier_stem(vrijwel,_,_).
ignore_modifier_stem(weer,_,_).
ignore_modifier_stem(wel,_,_).
ignore_modifier_stem(wellicht,_,_).
ignore_modifier_stem(zeer,_,_).
ignore_modifier_stem(zelfs,_,_).
ignore_modifier_stem(zojuist,_,_).

important_modifier(Tree,_,_,_) :-
    ignore_modifier(Tree),
    !,
    fail.

important_modifier(tree(Cat,_),Hd,HdDs,_):-
    important_mod(Cat,Hd,HdDs).
important_modifier(tree(_Cat,Ds),Hd,HdDs,_) :-
    lists:member(Mod,Ds),
    important_modifier1(Mod,Hd,HdDs).
important_modifier(tree(r(mod,p(rel)),_),_,_,[r('--',p(np))|_]).
important_modifier(tree(r(mod,p(rel)),_),adt_lex(_,er,_,_,_),[],_).   % clefts, er zijn er die problemen hebben
important_modifier(tree(r(mod,p(rel)),_),adt_lex(_,het,_,_,_),[],_).  % clefts, het zijn schurken die dat doen
important_modifier(tree(r(mod,p(rel)),_),adt_lex(_,iets,_,_,_),[],_). % dat is iets waar we naar verlangen
important_modifier(tree(r(mod,adt_lex(VanCat,Van,_,VanPos,_)),[]),adt_lex(GebruikCat,Gebruik,_,GebruikPos,_),[],_) :-
    check_pmi(Gebruik,GebruikPos,GebruikCat,Van,VanPos,VanCat).

important_modifier(tree(r(mod,p(_)),PPDS),adt_lex(GebruikCat,Gebruik,_,GebruikPos,_),[],_) :-
    PREPD = tree(r(hd,adt_lex(VanCat,Van,_,VanPos,_)),[]),
    lists:member(PREPD,PPDS),
    check_pmi(Gebruik,GebruikPos,GebruikCat,Van,VanPos,VanCat).

check_pmi(Gebruik0,GebruikPos0,GebruikCat,Van0,VanPos0,VanCat) :-
    adapt_psp(GebruikCat,Gebruik0,Gebruik),
    adapt_psp(VanCat,Van0,Van),
    adapt_pos(GebruikPos0,GebruikPos),
    adapt_pos(VanPos0,VanPos),
    hdrug_util:debug_message(2,"checking modifier ~w ~w ~w ~w... ~n",[Gebruik,GebruikPos,Van,VanPos]),
    alpino_penalties:corpus_frequency_lookup(dep35(Van,VanPos,hd/mod,GebruikPos,Gebruik),Val),
    hdrug_util:debug_message(2,"checking modifier ~w ~w ~w ~w: ~w ~n",[Gebruik,GebruikPos,Van,VanPos,Val]),
    Val > 1500,
    hdrug_util:debug_message(1,"keeping modifier ~w ~w ~w ~w: ~w ~n",[Gebruik,GebruikPos,Van,VanPos,Val]).

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



adapt_psp(_,X,X).
adapt_psp(ppart,Zeggen,Gezegd) :-
    alpino_lexical_analysis:search_tag_stem(Zeggen,tag(_,_,_,_,Zeggen,Gezegd,_,adjective(Inf))),
    ge_adj(Inf).

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
modifier_rel(app,Cat) :-
    Cat \= adt_lex(_,_,_,name,_),
    Cat \= adt_lex(_,_,_,num,_),
    Cat \= p(mwu(_,_)).

flatten(i(Id,Cat0),Ds0,i(Id,Cat),Ds) :-
    !,
    flatten(Cat0,Ds0,Cat,Ds).
flatten(adt_lex(A,B,C,D,E),[],adt_lex(A,B,C,D,E),[]).
flatten(p(_),[tree(r(hd,HdCat),Ds)],HdCat,Ds).
flatten(p(VAR),Ds,p(VAR),Ds) :-
    Ds = [_,_|_].
