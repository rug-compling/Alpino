:- module(alpino_treex,[ apply_transformations/2 ]).

:- use_module(library(lists)).
:- use_module(hdrug(hdrug_util)).

%% for TREEX input
apply_transformations(Tree0,Tree) :-
    apply_transformations0(Tree0,Tree1),
    (   Tree0 == Tree1
    ->  Tree1 = Tree
    ;   apply_transformations(Tree1,Tree)
    ).

apply_transformations0(Tree0,Tree) :-
    transform_rule(Tree0,Tree1),
    debug_call(2,hdrug_show:show(tree(adt),user,[value(Tree0)])),
    debug_call(2,hdrug_show:show(tree(adt),user,[value(Tree1)])),
    !,
    apply_transformations0(Tree1,Tree).
apply_transformations0(tree(Node,Ds0),tree(Node,Ds)) :-
    transform_ds(Ds0,Ds).

transform_ds([],[]).
transform_ds([H0|T0],[H|T]):-
    apply_transformations(H0,H),
    transform_ds(T0,T).

transform_rule(tree(r(Rel,_),[Left|Ds0]),tree(r(Rel,adt_lex(_,Root,_,name,[])),[])):-
    Left = tree(r(_,adt_lex(_,LQ,_,_,_)),[]),
    alpino_lex:xl(LQ,punct(aanhaal_links),_,[],[]),
    append(Mid,[Right],Ds0), 
    Right = tree(r(_,adt_lex(_,RQ,_,_,_)),[]),
    alpino_lex:xl(RQ,punct(aanhaal_rechts),_,[],[]),
    leaves_or_single(Mid,Root),
    debug_message(1,"treex correction: ` ~w ' is a name~n",[Root]).

transform_rule(tree(r(Rel,_),Ds0),tree(r(Rel,adt_lex(_,Lemma,_,name,[])),[]) ) :-
    El = tree(r(_,adt_lex(_,'>',_,_,_)),[]),
    member(El,Ds0),
    \+ member(tree(_,[_|_]),Ds0),  % not recursive
    leaves(Ds0,Lemma),
    debug_message(1,"treex correction: ~w is a name~n",[Lemma]).

transform_rule(tree(r(mod,adt_lex(_,keer,_,noun,_)),[]),
	       tree(r(mod,p(np)),[Een,Keer])):-
    Een = tree(r(det,adt_lex(_,een,_,det,[])),[]),
    Keer= tree(r(hd,adt_lex(_,keer,_,noun,[rnum=sg])),[]),
    debug_message(1,"treex correction: mod keer => een keer~n",[]).

transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,p(du)),[D1,D2])) :-
    Mod = tree(r(mod,adt_lex(_,';',_,_,_)),[]),
    append(Prefix,[Mod|Suffix],Ds0), 
    (   Prefix = [_,_|_],
        D1 = tree(r(dp,Cat),Prefix)
    ;   Prefix = [tree(r(_,D1Cat),D1Ds)],
	D1 = tree(r(dp,D1Cat),D1Ds)
    ),
    (   Suffix = [_,_|_],
	D2 = tree(r(dp,p(_)),Suffix)
    ;   Suffix = [tree(r(_,D2Cat),D2Ds)],
	D2 = tree(r(dp,D2Cat),D2Ds)
    ), 
    debug_message(1,"treex correction: 'X ; Y' --> du[dp,dp] ~n",[]).

transform_rule(tree(Cat0,Ds0),tree(Cat,Ds)):-
    select(D,Ds0,Ds1),
    ignore_d(D,Msg),
    treat_unary(Ds1,Cat0,Cat,Ds),
    debug_message(1,"treex correction: ignore ~w~n",[Msg]). 

transform_rule(tree(Node,[Zou,Er,MZ]), tree(Node,[Su,Zou,Er,MZ2])) :-
    Er = tree(r(mod,adt_lex(_,er,_,_,_)),[]),
    Zou = tree(r(hd,adt_lex(_,zullen,_,_,_)),[]),
    MZ = tree(r(vc,p(inf)),[M,PZ]),
    M  = tree(r(hd,adt_lex(_,moeten,_,_,_)),[]),
    PZ = tree(r(vc,p(inf)),Ds0),
    \+ member(tree(r(su,_),_),Ds0),
    PREDC = tree(r(predc,SuCat),SuDs),
    select(PREDC,Ds0,Ds1),
    looks_like_n_or_np(PREDC),
    Z = tree(r(hd,adt_lex(_,zijn,_,_,_)),[]),
    member(Z,Ds0),
    Su = tree(r(su,i(I,SuCat)),SuDs),
    Su2 = tree(r(su,i(I)),[]),
    MZ2 = tree(r(vc,p(inf)),[M,Su2,PZ2]),
    PZ2 = tree(r(vc,p(inf)),[Su2|Ds1]),
    debug_message(1,"er zou moeten zijn X~n",[]). 
    

transform_rule(tree(r(Rel0,N),Ds0),tree(r(Rel,N),Ds)):-
    Hd0 = tree(r(hd,i(X,adt_lex(A,Meer0,_,Pron,E0))),[]),
    Hd1 = tree(r(hd,i(X,adt_lex(A,Meer,_,Adj,E))),[]),
    replace(Hd0,Hd1,Ds0,Ds),
    wrong_pos(Meer0,Meer,Pron,Adj,Rel0,Rel,E0,E),
    debug_message(1,"treex correction: '~w' as ~w has pos=~w, not ~w 1~n",[Meer,Rel,Adj,Pron]). 

transform_rule(tree(r(Rel0,N),Ds0),tree(r(Rel,N),Ds)):-
    Hd0 = tree(r(hd,adt_lex(A,Meer0,_,Pron,E0)),[]),
    Hd1 = tree(r(hd,adt_lex(A,Meer,_,Adj,E)),[]),
    replace(Hd0,Hd1,Ds0,Ds),
    wrong_pos(Meer0,Meer,Pron,Adj,Rel0,Rel,E0,E),
    debug_message(1,"treex correction: '~w' as ~w has pos=~w, not ~w 2~n",[Meer,Rel,Adj,Pron]). 
    
transform_rule(tree(r(Rel0,adt_lex(A,Meer0,_,Pron,E0)),[]),
	       tree(r(Rel,adt_lex(A,Meer,_,Adj,E)),[])) :-
    wrong_pos(Meer0,Meer,Pron,Adj,Rel0,Rel,E0,E),
    debug_message(1,"treex correction: '~w' as ~w has pos=~w, not ~w 3~n",[Meer,Rel,Adj,Pron]). 

transform_rule(tree(r(Rel0,i(X,adt_lex(A,Meer0,_,Pron,E0))),[]),
	       tree(r(Rel,i(X,adt_lex(A,Meer,_,Adj,E))),[])) :-
    wrong_pos(Meer0,Meer,Pron,Adj,Rel0,Rel,E0,E),
    debug_message(1,"treex correction: '~w' as ~w has pos=~w, not ~w 4~n",[Meer,Rel,Adj,Pron]). 

%% als x vc ==> sat:[als x] nucl:[vc]
transform_rule(tree(r(Rel,p(cp)),Ds), tree(r(Rel,p(du)),[Sat,Nucl])) :-
    Ds = [tree(r(cmp,Cmp),CmpDs),
	  tree(r(body,p(ssub)),BodyDs0)
	 ],
    Sat = tree(r(sat,p(cp)),
	       [tree(r(cmp,Cmp),CmpDs),
		tree(r(body,p(ssub)),BodyDs)
	       ]),
    Cmp = adt_lex(_,als,_,_,_),
    Nucl = tree(r(nucl,p(sv1)),ImpDs),
    append(BodyDs,[ImpD],BodyDs0),
    ImpD = tree(r(vc,_),ImpDs),
    Hd = tree(r(hd,adt_lex(_,_,_,verb,Atts)),[]),
    member(Hd,ImpDs),
    member(stype=imparative,Atts),
    debug_message(1,"treex correction: als x vc ==> sat:[als x] nucl:[vc]~n",[]).

%% where it says X => X

transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hetzegt = tree(_,[Waar,Body]),
    Waar = tree(r(_,i(I,adt_lex(_,waar,_,_,_))),[]),
    Body = tree(r(body,_),[tree(r(mod,i(I)),[]),
			   tree(r(su,adt_lex(_,het,_,_,_)),[]),
			   tree(r(hd,adt_lex(_,zeg,_,_,_)),[])
			  ]),
    select(Hetzegt,Ds0,Ds),
    debug_message(1,"treex correction: waar het zegt removed~n",[]).

transform_rule(tree(r(Rel,_),[Waar,Body]),  tree(r(Rel,Cat),Ds)) :-
    Waar = tree(r(_,i(I,adt_lex(_,waar,_,_,_))),[]),
    Body = tree(r(body,_),[tree(r(mod,i(I)),[]),
			   tree(r(su,adt_lex(_,het,_,_,_)),[]),
			   tree(r(hd,adt_lex(_,zeggen,_,_,_)),[]),
			   tree(r(Obj1,Cat),Ds)
			  ]),
    member(Obj1,[obj1,predc]),
    debug_message(1,"treex correction: waar het zegt X => X~n",[]).

%% het veld dat zegt X => het veld X
transform_rule(tree(r(mod,_),[Waar,Body]),  tree(r(app,Cat),Ds)) :-
    Waar = tree(r(rhd,i(I,adt_lex(_,Die,_,_,_))),[]),
    member(Die,[die,dat]),
    Body = tree(r(body,_),[tree(r(su,i(I)),[]),
			   tree(r(hd,adt_lex(_,zeggen,_,_,_)),[]),
			   tree(r(obj1,Cat),Ds)
			  ]),
    debug_message(1,"treex correction: waar het zegt X => X~n",[]).

%% verb mod
transform_rule(tree(Node,Ds0), tree(Node,Ds) ):-
    HD = tree(r(hd,adt_lex(_,Optie,_,_,_)),[]),
    member(HD,Ds0),
    app_noun(Optie),
    MOD1 = tree(r(mod,adt_lex(A,Meer0,Meer1,Verb,Atts)),[]),
    MOD2 = tree(r(app,adt_lex(A,Meer0,Meer1,Verb,Atts)),[]),
    replace(MOD1,MOD2,Ds0,Ds),
    Verb == verb,
    \+ member(Meer0,[volgen,volg]),
    debug_message(1,"treex correction: '~w' as mod with ~w is app~n",[Meer0,Optie]). 

transform_rule(tree(Node,Ds0), tree(Node,Ds) ):-
    HD = tree(r(hd,adt_lex(_,Optie,_,_,_)),[]),
    member(HD,Ds0),
    app_noun(Optie),
    MOD1 = tree(r(mod,Cat),ModDs),
    MOD2 = tree(r(app,Cat),ModDs),
    replace(MOD1,MOD2,Ds0,Ds),
    (   ModHd = tree(r(hd,adt_lex(_,Meer0,_,Verb,_)),[]),
	member(ModHd,ModDs)
    ;   Cat = adt_lex(_,Meer0,_,Verb,_)
    ),
    \+ member(Meer0,[volgen,volg]),
    (   Verb==noun
    ;   Verb == verb,
        NotSu = tree(r(su,_),_),
        \+ member(NotSu,ModDs)
    ),
    debug_message(1,"treex correction: '~w' as mod with ~w is app~n",[Meer0,Optie]). 

transform_rule(tree(r(mod,N),Ds0),tree(r(mod,N),Ds)):-
    Hd0 = tree(r(hd,i(X,adt_lex(A,Meer0,_,Verb,_))),[]),
    Hd1 = tree(r(hd,i(X,adt_lex(A,Meer,_,adj,[aform=base]))),[]),
    replace(Hd0,Hd1,Ds0,Ds),
    Verb == verb,
    NotSu = tree(r(su,_),_),
    \+ member(NotSu,Ds),
    NotObj1 = tree(r(obj1,_),_),
    (   \+ member(NotObj1,Ds),
	verb_mod(Meer0,Meer)
    ;   Meer0 = noem, Meer = genaamd
    ),
    debug_message(1,"treex correction: '~w' as mod is ~w~n",[Meer0,Meer]). 

transform_rule(tree(r(mod,N),Ds0),tree(r(mod,N),Ds)):-
    Hd0 = tree(r(hd,adt_lex(A,Meer0,_,Verb,_)),[]),
    Hd1 = tree(r(hd,adt_lex(A,Meer,_,adj,[aform=base])),[]),
    replace(Hd0,Hd1,Ds0,Ds),
    Verb == verb,
    NotSu = tree(r(su,_),_),
    \+ member(NotSu,Ds),
    NotObj1 = tree(r(obj1,_),_),
    (   \+ member(NotObj1,Ds),
	verb_mod(Meer0,Meer)
    ;   Meer0 = noem, Meer = genaamd
    ),
    debug_message(1,"treex correction: '~w' as mod is ~w~n",[Meer0,Meer]). 

transform_rule(tree(r(mod,adt_lex(A,Meer0,_,Verb,_)),[]),
	       tree(r(mod,adt_lex(A,Meer,_,adj,[aform=base])),[])) :-	      
    Verb == verb,
    verb_mod(Meer0,Meer),
    debug_message(1,"treex correction: '~w' as mod is ~w~n",[Meer0,Meer]). 

transform_rule(tree(r(mod,i(X,adt_lex(A,Meer0,_,Verb,_))),[]),
	       tree(r(mod,i(X,adt_lex(A,Meer,_,adj,[aform=base]))),[])) :-
    Verb == verb,
    verb_mod(Meer0,Meer),
    debug_message(1,"treex correction: '~w' as mod is ~w~n",[Meer0,Meer]). 

transform_rule(tree(WHQ,[tree(r(whd,i(INDEX,adt_lex(A,Waar,C,D,E))),[]),
			 tree(r(body,CAT),Ds0)]),
	       tree(WHQ,[tree(r(whd,i(INDEX,adt_lex(A,Waar,C,D,E))),[]),
			 tree(r(body,CAT),Ds)])) :-
    member(Waar,[waar,wanneer,hoe,waarom]),
    replace_td(tree(r(obj1,i(INDEX0)),[]),
	       tree(r(mod, i(INDEX)),[]),Ds0,Ds),
    INDEX0==INDEX,
    debug_message(1,"treex correction: antecedent of ~w is mod, not obj1~n",[Waar]).

transform_rule(tree(REL,[tree(r(rhd,i(INDEX,adt_lex(A,Waar,C,D,E))),[]),
			 tree(r(body,CAT),Ds0)]),
	       tree(REL,[tree(r(rhd,i(INDEX,adt_lex(A,Waar,C,D,E))),[]),
			 tree(r(body,CAT),Ds)])) :-
    (   member(Waar,[waar,wanneer,hoe,waarom])
    ;   alpino_lex:xl(Waar,waar_adverb(_),Waar,[],[])
    ),
    replace_td(tree(r(obj1,i(INDEX0)),[]),
	       tree(r(mod, i(INDEX)),[]),Ds0,Ds),
    INDEX0==INDEX,
    debug_message(1,"treex correction: antecedent of ~w is mod, not obj1~n",[Waar]).

transform_rule(tree(REL,[tree(r(rhd,i(INDEX,adt_lex(A,Waar,C,D,E))),[]),
			 tree(r(body,CAT),Ds0)]),
	       tree(REL,[tree(r(rhd,i(INDEX,adt_lex(A,Waar,C,D,E))),[]),
			 tree(r(body,CAT),Ds)])) :-
    (   member(Waar,[waar,wanneer,hoe,waarom])
    ;   alpino_lex:xl(Waar,waar_adverb(_),Waar,[],[])
    ),
    replace_td(tree(r(predc,i(INDEX0)),[]),
	       tree(r(mod,  i(INDEX)),[]),Ds0,Ds),
    INDEX0==INDEX,
    debug_message(1,"treex correction: antecedent of ~w is mod, not predc~n",[Waar]).

%% no longer required?
transform_rule(tree(r('--',p(whq)),[WHD0,BODY]),
	       tree(r('--',p(du)),[SAT,tree(r(nucl,p(whq)),[WHD,BODY])])) :-
    WHD0 = tree(r(whd,i(INDEX,_)),[MOD,HOE]),
    HOE  = tree(r(hd,adt_lex(A,hoe,C,D,E)),[]),
    WHD  = tree(r(whd,i(INDEX,adt_lex(A,hoe,C,D,E))),[]),
    MOD  = tree(r(mod,MODCAT),MODDS),
    SAT  = tree(r(sat,MODCAT),MODDS),
    debug_message(1,"treex correction: MOD-hoe -> sat/nucl~n",[]).

transform_rule(tree(r(Rel,adt_lex(A,B0,_,verb,C)),[]),
	       tree(r(Rel,adt_lex(A,B,_,verb,C)),[])) :-
    alpino_lex:un_is_verb_lemma(B0,B),
    \+ B0 = B,
    debug_message(1,"treex correction: lemma ~w -> root ~w~n",[B0,B]).

transform_rule(tree(r(Rel,adt_lex(A,B0,_,D,E)),[]),
	       tree(r(Rel,adt_lex(A,B,_,D,E)),[])) :-
    atom_concat('obj:',B,B0),
    debug_message(1,"treex correction: root ~w -> root ~w~n",[B0,B]).

transform_rule(tree(r(Rel,adt_lex(A,B,_,verb,C0)),[]),
	       tree(r(Rel,adt_lex(A,B,_,verb,[stype=ynquestion|C])),[])) :-
    select(stype=imparative,C0,C),
    member(B,[ben,wil,moet,mag,kan]),
    debug_message(1,"treex correction: imparative for ~w? -> ynquestion!~n",[B]).

transform_rule(tree(r(Rel,p(SSUB)),Ds0), tree(r(Rel,p(SSUB)),Ds)):-
    SSUB == ssub,
    Hd0 = tree(r(hd,adt_lex(A,B,C,verb,E0)),[]),
    Hd  = tree(r(hd,adt_lex(A,B,C,verb,E )),[]),
    replace(Hd0,Hd,Ds0,Ds),
    lists:select(stype=_,E0,E),
    debug_message(1,"treex correction: stype irrelevant for head of ssub~n",[]).

transform_rule(tree(r(Rel,p(_)),Ds0),tree(r(Rel,p(du)),Ds)):-
    Ds0=[tree(r(hd,adt_lex(A,Ja,C,_,E)),[]),
	 tree(r(Body,Cat),BodyDs)
	],
    member(Body,[body,mod]),
    member(Ja,[ja,jawel,nee,neen]),
    Ds= [tree(r(tag,adt_lex(A,Ja,C,tag,E)),[]),
	 tree(r(nucl,Cat),BodyDs)
	],
    debug_message(1,"treex correction: ja Body has tag/nucl~n",[]).

transform_rule(tree(r(Rel,Cat),[Cmp,Body]),tree(r(Rel,Cat2),Ds)):-
    Body = tree(r(BodyRel,p(du)),[Tag,tree(r(nucl,NuclCat),NuclDs)]),
    Cat2 = p(du),
    Tag = tree(r(tag,_),_),
    Cmp \= tree(r(tag,_),_),
    Ds = [Tag,tree(r(nucl,Cat),[Cmp,tree(r(BodyRel,NuclCat),NuclDs)])],
    debug_message(1,"treex correction: promote tag/nucl~n",[]).

transform_rule(tree(r(Rel,Cat),[Ja|Ds0]),tree(r(Rel,p(du)),Ds)):-
    Ja = tree(r(NoTag,adt_lex(A,JaL,C,_,E)),[]),
    Ds0 = [_|_],
    NoTag \== tag,
    member(JaL,[ja,jawel,nee,neen]),
    Ds= [tree(r(tag,adt_lex(A,JaL,C,tag,E)),[]),
	 tree(r(nucl,Cat),Ds0)
	],
    debug_message(1,"treex correction: ja Body has tag/nucl~n",[]).

transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,p(du)),Ds)):-
    Ja = tree(r(mod,adt_lex(A,JaL,C,_,E)),[]),
    VC0 = tree(r(vc,VcCat),VcDs0),
    VC  = tree(r(vc,VcCat),VcDs),
    replace(VC0,VC,Ds0,Ds1),
    select(Ja,VcDs0,VcDs),
    member(JaL,[ja,jawel,nee,neen]),
    Ds= [tree(r(tag,adt_lex(A,JaL,C,tag,E)),[]),
	 tree(r(nucl,Cat),Ds1)
	],
    debug_message(1,"treex correction: ja Body has tag/nucl~n",[]).

transform_rule(tree(r(mod,p(advp)),Ds0),
	       tree(r(mod,p(advp)),Ds)) :-
    Hd = tree(r(hd,adt_lex(_A,zo,_,adv,_E)),[]),
    member(Hd,Ds0),
    Mog = tree(r(mod,adt_lex(A,mogelijk,_,adj,E)),[]),
    Obc = tree(r(obcomp,adt_lex(A,mogelijk,_,adj,E)),[]),
    replace(Mog,Obc,Ds0,Ds),
    debug_message(1,"treex correction: zo .. mogelijk has obcomp~n",[]).

transform_rule(tree(REL,[CMP,OBCOMP]),tree(REL,[HD,OBCOMP])):-
    HD = tree(r(hd, A),B),
    CMP= tree(r(cmp,A),B),
    OBCOMP = tree(r(obcomp,_),_),
    debug_message(1,"treex correction: adjective with obcomp is hd~n",[]).

transform_rule(tree(r(obcomp,CAT),[HD,BODY]),tree(r(obcomp,CAT),[CMP,BODY])):-
    HD = tree(r(hd, A),B),
    CMP= tree(r(cmp,A),B),
    BODY = tree(r(body,_),_),
    debug_message(1,"treex correction: comparative in obcomp is cmp~n",[]).

transform_rule(tree(r(top,Cat),[tree(r(Rel,VC),Ds)]),
	       tree(r(top,Cat),[tree(r('--',VC),Ds)])) :-
    \+ Rel == '--',
    debug_message(1,"treex correction: relation under top must be --~n",[]).

%% todo adt393.xml
transform_rule(tree(r(Rel,Cat),Ds0),
	       tree(r(Rel,Cat),Ds)):-
    Cat = p(SMAIN),
    member(SMAIN,[smain,ssub]), 
    Hd = tree(r(hd,adt_lex(_,Zijn,_,_,Atts)),[]),
    Er = tree(r(mod,adt_lex(_,er,_,_,_)),[]),
    member(Hd,Ds0),
    member(Er,Ds0),
    member(Zijn,[ben,zijn]),
    (   member(stype=declarative,Atts)
    ;   \+ member(stype=_,Atts)
    ),
    Su = tree(r(su,_),_),
    \+ member(Su,Ds0),
    Predc = tree(r(predc,SuCat),PredcDs),
    Su2   = tree(r(   su,SuCat),PredcDs),
    replace(Predc,Su2,Ds0,Ds),
    debug_message(1,"lonely predc in smain -> su ~w~n",[r(Rel,Cat)]).

transform_rule(tree(r(Rel,Cat),Ds0),
	       tree(r(Rel,Cat),Ds)):-
    Cat = p(sv1),
    Hd = tree(r(hd,adt_lex(_,Zijn,_,_,Atts)),[]),
    member(Hd,Ds0),
    member(Zijn,[ben,zijn]),
    (   member(stype=ynquestion,Atts)
    ;   member(stype=declarative,Atts)
    ;   member(stype=imparative,Atts)  % will be corrected later
    ;   \+ member(stype=_,Atts)
    ),
    Su = tree(r(su,_),_),
    \+ member(Su,Ds0),
    Predc = tree(r(predc,SuCat),PredcDs),
    Su2   = tree(r(   su,SuCat),PredcDs),
    replace(Predc,Su2,Ds0,Ds),
    debug_message(1,"lonely predc in ynquestion/declarative -> su ~w~n",[r(Rel,Cat)]).

transform_rule(tree(r(Rel,p(Cat)),Ds0),
	       tree(r(Rel,p(Cat)),Ds)):-
    \+ Rel == body, 
    Hd0 = tree(r(hd,adt_lex(A,B,C,verb,Atts0)),[]),
    Hd = tree(r(hd,adt_lex(A,B,C,verb,Atts)),[]),
    replace(Hd0,Hd,Ds0,Ds),
    select(stype=declarative,Atts0,Atts),
    Su = tree(r(su,_),_),
    \+ member(Su,Ds),
    debug_message(1,"treex correction: declarative without subject? Ignore attribute~n",[]).

transform_rule(tree(r(Rel,adt_lex(A,B,C,Name,E)),[]),
	       tree(r(Rel,adt_lex(A,B,C,name,[rnum=sg|E])),[])):-
    nonvar(Name), Name = name,
    \+ member(rnum=_,E),
    \+ atom_concat(_,s,B),
    \+ lists:member(B,[euro,gulden,seconde,jaar,uur,meter,kilo_meter,kilometer,kilo,byte,kilo_byte,mega_byte,giga_byte,tera_byte,penta_byte,terra_byte]),
    debug_message(1,"treex correction: name is singular by default~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,_,_,PosHd,_)),_),
    member(Hd,Ds0),
    nonvar(PosHd),
    Mod  = tree(r(Rel,adt_lex(A,B,B,Pos0,E0)),[]),
    Mod2 = tree(r(Rel,adt_lex(A,B1,B1,Pos, E )),[]),
    replace(Mod,Mod2,Ds0,Ds),
    wrong_pos_hd(B,B1,Pos0,Pos,Rel,E0,E,PosHd),
    debug_message(1,"treex correction: ~w is ~w instead of ~w as ~w of ~w~n",[B,Pos0,Pos,Rel,PosHd]).

transform_rule(tree(r(VC,p(ti)),[Te,Mod]),
	       tree(r(VC,p(ti)),[Te,Body])) :-
    Te  = tree(r(cmp,adt_lex(_,te,_,_,_)),[]),
    Mod = tree(r(mod,_),Ds0),
    Body= tree(r(body,p(inf)),Ds),
    Hd0 = tree(r(hd,adt_lex(A,B,C,noun,_)),[]),
    Hd  = tree(r(hd,adt_lex(A,B,C,verb,[])),[]),
    replace(Hd0,Hd,Ds0,Ds),
    debug_message(1,"treex correction: te complementizer takes verb as head of inf~n",[]).

transform_rule(tree(r(mod,p(_)),[Er,Door]), tree(r(mod,adt_lex(_,Adv,_,pp,[])),[])) :-
    Er = tree(r(obj1,adt_lex(_,EL,_,adv,_)),[]),
    Door = tree(r(hd,adt_lex(_,DoorLemma,_,prep,_)),[]),
    (   DoorLemma=tot -> DL = toe
    ;   DoorLemma=met -> DL = mee
    ;   DoorLemma = DL
    ),
    atom_concat(EL,DL,Adv),
    met_waarmee(DoorLemma,Adv),
    debug_message(1,"treex correction: er_adverb is a pc~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,_)),[]),
    member(Hd,Ds0),
    Mod = tree(r(mod,adt_lex(A,Omhoog,_,Pos,E)),[]),
    LD  = tree(r(ld, adt_lex(A,Omhoog,_,Pos,E)),[]),
    replace(Mod,LD,Ds0,Ds),
    verb_ld(Verb,Omhoog),
    debug_message(1,"treex correction: '~w' + '~w' is LD, not MOD~n",[Verb,Omhoog]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,_)),[]),
    member(Hd,Ds0),
    Mod = tree(r(mod,ModCat),ModDs),
    LD  = tree(r(ld, ModCat),ModDs),
    replace(Mod,LD,Ds0,Ds),
    Hd = tree(r(hd,adt_lex(_,Omhoog,_,_,_)),[]),
    member(Hd,ModDs),
    verb_ld(Verb,Omhoog),
    debug_message(1,"treex correction: '~w' + '~w' is LD, not MOD~n",[Verb,Omhoog]).

%% je nodig X ==> je moet X
transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,Cat),Ds)):-
    Hd0 = tree(r(hd,adt_lex(_,nodig,_,adj,_)),[]),
    Hd  = tree(r(hd,adt_lex(_,moet,_,verb,[stype=declarative])),[]),
    replace(Hd0,Hd,Ds0,Ds1),
%    Su  = tree(r(su,adt_lex(_,je,_,_,_)),_),
    Su  = tree(r(su,_),_),
    member(Su,Ds0),
    \+ member(tree(r(obj1,_),_),Ds0),
    VC0 = tree(r(VCREL,VCCAT),VCDs),
    VC1 = tree(r(vc,VCCAT),VCDs),
    replace(VC0,VC,Ds1,Ds), 
    member(VCREL,[vc,body]),
    omte_inf(VC1,VC),
    debug_message(1,"treex correction: je nodig => je moeten 1~n",[]).

transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,Cat),Ds)):-
    Hd0 = tree(r(hd,adt_lex(_,nodig,_,verb,Atts)),[]),
    Hd  = tree(r(hd,adt_lex(_,moet,_,verb,Atts)),[]),
    replace(Hd0,Hd,Ds0,Ds1),
    VC0 = tree(r(VCREL,VCCAT),VCDs),
    VC1 = tree(r(vc,VCCAT),VCDs),
    replace(VC0,VC,Ds1,Ds),
    VcHd =  tree(r(hd,_),_),
    member(VcHd,VCDs),  % prefer VC with head 
    member(VCREL,[vc,body]),
    omte_inf(VC1,VC),
    debug_message(1,"treex correction: nodig => moeten 2a~n",[]).

transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,Cat),Ds)):-
    Hd0 = tree(r(hd,adt_lex(_,nodig,_,verb,Atts)),[]),
    Hd  = tree(r(hd,adt_lex(_,moet,_,verb,Atts)),[]),
    replace(Hd0,Hd,Ds0,Ds1),
    VC0 = tree(r(VCREL,VCCAT),VCDs),
    VC1 = tree(r(vc,VCCAT),VCDs),
    replace(VC0,VC,Ds1,Ds), 
    member(VCREL,[vc,body,mod]),
    omte_inf(VC1,VC),
    debug_message(1,"treex correction: nodig => moeten 2b~n",[]).

transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,Cat),Ds)):-
    Hd0 = tree(r(hd,adt_lex(_,nodig,_,adj,_)),[]),
    Hd  = tree(r(hd,adt_lex(_,moet,_,verb,[stype=declarative])),[]),
    replace(Hd0,Hd,Ds0,Ds),
    Su  = tree(r(su,adt_lex(_,je,_,_,_)),_),
    member(Su,Ds0),
    Obj1 = tree(r(obj1,_),_),
    \+ member(Obj1,Ds),
    debug_message(1,"treex correction: je nodig => je moeten 3~n",[]).

transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,Cat),Ds)):-
    Hd0 = tree(r(hd,adt_lex(_,nodig,_,verb,Atts)),[]),
    Hd  = tree(r(hd,adt_lex(_,moet,_,verb,Atts)),[]),
    replace(Hd0,Hd,Ds0,Ds),
    debug_message(1,"treex correction: nodig => moeten 4~n",[]).

transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,Cat),Ds)) :-
    Head = tree(r(hd,adt_lex(_,Modal,_,verb,_)),_),
    member(Head,Ds0),
    member(Modal,[willen,kunnen,mogen,moeten,laten,zullen,wil,kun,mag,moet,laat,zal]),
    \+ member(tree(r(obj1,_),_),Ds0),
    \+ member(tree(r(vc,_),_),Ds0),
    \+ member(tree(r(body,_),_),Ds0),
    replace(MOD,VC,Ds0,Ds),
    looks_like_inf(MOD,VC),
    debug_message(1,"treex correction: modal + inf => VC~n",[]).


%% als obj1 => je hebt X nodig
%% anders   => X is nodig
transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,Cat),[Hd|Ds])):-
    \+ Rel = mod,
    \+ Rel = predc,
    Hd0 = tree(r(hd,adt_lex(_,nodig,_,adj,Adj)),[]),
    Hd  = tree(r(hd,adt_lex(_,heb,_,verb,[stype=declarative])),[]),
    Predc = tree(r(predc,adt_lex(_,nodig,_,adj,Adj)),[]),
    NotPredc = tree(r(predc,_),_),
    Obj1 = tree(r(obj1,_),_),
    member(Obj1,Ds0),
    replace(Hd0,Predc,Ds0,Ds),
    \+ member(NotPredc,Ds0),
    debug_message(1,"treex correction: adj nodig => predc~n",[]).

transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,Cat),[Hd|Ds])):-
    \+ Rel = mod,
    \+ Rel = predc,
    Hd0 = tree(r(hd,adt_lex(_,nodig,_,adj,Adj)),[]),
    Hd  = tree(r(hd,adt_lex(_,ben,_,verb,[stype=declarative])),[]),
    Predc = tree(r(predc,adt_lex(_,nodig,_,adj,Adj)),[]),
    NotPredc = tree(r(predc,_),_),
    replace(Hd0,Predc,Ds0,Ds),
    \+ member(NotPredc,Ds0),
    debug_message(1,"treex correction: adj nodig => predc~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,willen,_,verb,_)),[]),
    member(Hd,Ds0),
    Mod = tree(r(mod,i(_)),[]),
    select(Mod,Ds0,Ds1),
    VC0= tree(r(vc, _),[tree(r(cmp,adt_lex(_,om,_,_,_)),[]),
	                tree(r(body,_),[tree(r(cmp,adt_lex(_,te,_,_,_)),[]),
					tree(r(body,_),DS)
				       ])
		       ]
	     ),
    VC=  tree(r(vc,p(cp)),[tree(r(cmp,adt_lex(_,dat,_,comp,[])),[]),
			   tree(r(body,p(ssub)),[Mod|DS])
			  ]),
    replace(VC0,VC,Ds1,Ds),		
    debug_message(1,"treex correction: 'wilt om te... --> wilt dat...' ~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,willen,_,verb,_)),[]),
    member(Hd,Ds0),
    VC0= tree(r(vc, _),[tree(r(cmp,adt_lex(_,om,_,_,_)),[]),
	                tree(r(body,_),[tree(r(cmp,adt_lex(_,te,_,_,_)),[]),
					tree(r(body,_),DS)
				       ])
		       ]
	     ),
    VC=  tree(r(vc,p(cp)),[tree(r(cmp,adt_lex(_,dat,_,comp,[])),[]),
			   tree(r(body,p(ssub)),DS)
			  ]),
    replace(VC0,VC,Ds0,Ds),		
    debug_message(1,"treex correction: 'wilt om te... --> wilt dat...' ~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    TI = tree(r(vc,p(_)),[Te|_]),
    Te = tree(r(cmp,adt_lex(_,OmTe,_,_,_)),[]),
    member(OmTe,[om,te]),
    OTI = tree(r(vc,p(oti)),[Om|OTIDS]),
    MOD = tree(r(mod,p(oti)),[Om|OTIDS]),
    replace(OTI,MOD,Ds0,Ds),
    Om = tree(r(cmp,adt_lex(_,om,_,_,_)),[]),
    member(TI,Ds),
    debug_message(1,"treex correction: 2x VC ti oti => ti mod ~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,doen,_,verb,_)),[]),
    member(Hd,Ds0),
    Obj = tree(r(obj1,_),_),
    member(Obj,Ds0),
    VC = tree(r(vc,p(oti)),VDs),
    MOD = tree(r(mod,p(oti)),VDs),
    replace(VC,MOD,Ds0,Ds),
    debug_message(1,"treex correction: 'doen' + obj1 takes MOD oti, not VC~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Lemma,_,verb,_)),[]),
    member(Hd,Ds0),
    VC = tree(r(vc,p(Oti)),VDs),
    MOD = tree(r(mod,p(Oti)),VDs),
    replace(VC,MOD,Ds0,Ds),
    Oti == oti,
    \+ (  alpino_lex:un_is_verb_lemma(Lemma,Root),
	  alpino_penalties:corpus_frequency_lookup(dep35(om,comp,hd/vc,verb,Root),_)
       ),
    debug_message(1,"treex correction: ~w takes MOD oti, not VC~n",[Lemma]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,hebben,_,verb,_)),[]),
    member(Hd,Ds0),
    Obj = tree(r(obj1,_),_),
    member(Obj,Ds0),
%    Predc = tree(r(predc,_),_),
%    member(Predc,Ds0),
    VC = tree(r(vc,VCAT),VDs),
    MOD = tree(r(mod,VCAT),VDs),
    replace(VC,MOD,Ds0,Ds),
    debug_message(1,"treex correction: 'hebben' + obj1 takes MOD, not VC~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,aantal,_,noun,_)),[]),
    member(Hd,Ds0),
    VC = tree(r(vc,p(oti)),VDs),
    MOD = tree(r(mod,p(oti)),VDs),
    replace(VC,MOD,Ds0,Ds),
    debug_message(1,"treex correction: 'aantal' takes MOD oti, not VC~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,_)),[]),
    member(Hd,Ds0),
    VC0   = tree(r(vc,p(oti)),[Om,TeInf]),
    Om    = tree(r(cmp,adt_lex(_,om,_,_,_)),[]),
    TeInf = tree(r(body,p(ti)),[Te,Inf]),
    VC1   = tree(r(vc,p(ti)),[Te,Inf]),
    Te    = tree(r(cmp,adt_lex(_,te,_,_,_)),[]),
    replace(VC0,VC1,Ds0,Ds),
    \+ member(tree(r(vc,p(oti)),[]),Ds),
    verb_vc_te_om(Verb),
    debug_message(1,"treex correction: '~w' typically takes te V, not om te V~n",[Verb]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,_)),[]),
    member(Hd,Ds0),
    verb_vc_mod(Verb,om),
    VC =   tree(r(vc, ModCat),ModDs),
    MOD =  tree(r(mod,ModCat),ModDs),
    replace(VC,MOD,Ds0,Ds),
    OM = tree(r(cmp,adt_lex(_,om,_,_,_)),_),
    member(OM,ModDs),
    debug_message(1,"treex correction: '~w' takes om-MOD, not om-VC (verb_vc_mod)~n",[Verb]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,_)),[]),
    member(Hd,Ds0),
    VC2=   tree(r(vc,p(ti)),_),
    member(VC2,Ds0),
    Te = tree(r(cmp,adt_lex(_,te,_,_,_)),[]),
    member(Te,VC2),
    VC =   tree(r(vc, ModCat),ModDs),
    MOD =  tree(r(mod,ModCat),ModDs),
    replace(VC,MOD,Ds0,Ds),
    OM = tree(r(cmp,adt_lex(_,om,_,_,_)),_),
    member(OM,ModDs),
    debug_message(1,"treex correction: '~w' takes om-MOD, not om-VC~n",[Verb]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Verb,_,noun,_)),[]),
    member(Hd,Ds0),
    noun_vc_mod(Verb,om),
    VC =   tree(r(vc, ModCat),ModDs),
    MOD =  tree(r(mod,ModCat),ModDs),
    replace(VC,MOD,Ds0,Ds),
    OM = tree(r(cmp,adt_lex(_,om,_,_,_)),_),
    member(OM,ModDs),
    debug_message(1,"treex correction: '~w' takes om-MOD, not om-VC~n",[Verb]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,Atts)),[]),
    member(Hd,Ds0),
    member(tense=_,Atts),
    Predc = tree(r(predc,_),_),
    member(Predc,Ds0),
    VC =    tree(r(vc,   ModCat),ModDs),
    OBJ1 =  tree(r(su, ModCat),ModDs),
    \+ member(OBJ1,Ds0),
    replace(VC,OBJ1,Ds0,Ds),
    debug_message(1,"treex correction: '~w' takes SU, not VC~n",[Verb]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,_)),[]),
    member(Hd,Ds0),
    verb_vc_obj1(Verb),
    VC =    tree(r(vc,   ModCat),ModDs),
    OBJ1 =  tree(r(obj1, ModCat),ModDs),
    \+ member(OBJ1,Ds0),
    replace(VC,OBJ1,Ds0,Ds),
    debug_message(1,"treex correction: '~w' takes OBJ1, not VC~n",[Verb]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,_)),[]),
    member(Hd,Ds0),
    verb_obj1_ld(Verb,Prep),
    OBJ1 =  tree(r(obj1, _),_),
    PC =    tree(r(ld,   p(pp)),PCDs),
    \+ member(PC,Ds0),
    PCDs = [tree(r(hd,adt_lex(_,Prep,Prep,prep,[])),[]),OBJ1],
    replace(OBJ1,PC,Ds0,Ds),
    \+ member(tree(r(obj1,_),_),Ds),  %% not if there is a second obj1, something else may be wrong
    debug_message(1,"treex correction: '~w OBJ1' ==> ~w ~w OBJ1~n",[Verb,Verb,Prep]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,_)),[]),
    member(Hd,Ds0),
    verb_obj1_pc(Verb,Prep),
    OBJ1 =  tree(r(obj1, _),_),
    PC =    tree(r(pc,   p(pp)),PCDs),
    \+ member(PC,Ds0),
    PCDs = [tree(r(hd,adt_lex(_,Prep,Prep,prep,[])),[]),OBJ1],
    replace(OBJ1,PC,Ds0,Ds),
    debug_message(1,"treex correction: '~w OBJ1' ==> ~w ~w OBJ1~n",[Verb,Verb,Prep]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,_)),[]),
    member(Hd,Ds0),
    VC =    tree(r(vc,   ModCat),ModDs),
    OBJ1 =  tree(r(obj1, ModCat),ModDs),
    \+ member(OBJ1,Ds0),
    replace(VC,OBJ1,Ds0,Ds),
    DET = tree(r(det,_),_),
    member(DET,ModDs),
    debug_message(1,"treex correction: '~w' takes OBJ1, not VC because it has a determiner~n",[Verb]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,_)),[]),
    member(Hd,Ds0),
    VC =    tree(r(vc,   ModCat),[]),
    OBJ1 =  tree(r(obj1, ModCat),[]),
    \+ member(OBJ1,Ds0),
    replace(VC,OBJ1,Ds0,Ds),
    ModCat = adt_lex(_,_,_,Noun,_),
    member(Noun,[noun,name]),
    debug_message(1,"treex correction: '~w' takes OBJ1, not VC because it is a noun/name~n",[Verb]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,_)),[]),
    member(Hd,Ds0),
    verb_predc_obj1(Verb),
    VC =    tree(r(predc,ModCat),ModDs),
    OBJ1 =  tree(r(obj1, ModCat),ModDs),
    \+ member(OBJ1,Ds0),
    replace(VC,OBJ1,Ds0,Ds),
    debug_message(1,"treex correction: '~w' takes OBJ1, not PREDC~n",[Verb]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,_)),[]),
    member(Hd,Ds0),
    verb_predc_mod(Verb),
    VC =    tree(r(predc,ModCat),ModDs),
    OBJ1 =  tree(r(mod,  ModCat),ModDs),
    replace(VC,OBJ1,Ds0,Ds),
    \+ looks_like_np(OBJ1),
    debug_message(1,"treex correction: '~w' takes no PREDC, trying MOD~n",[Verb]).

%% after previous rule...
%% ????
transform_rule(tree(r(obj1,CAT),Ds0),
	       tree(r(obj1,CAT),Ds)):-
    SU = tree(r(su,i(_)),[]),
    select(SU,Ds0,Ds).

transform_rule(tree(r(pc,adt_lex(A,Adj,_,adj,E)),[]),
	       tree(r(predc,adt_lex(A,Adj,_,adj,E)),[])):-
    debug_message(1,"treex correction: adj pc must be a predc~n",[]).


transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Cmp = tree(r(cmp,adt_lex(_,voor,voor,comp,_)),[]),
    select(Cmp,Ds0,Ds1),
    Zover = tree(r(mod,adt_lex(_,zover,zover,comp,_)),[]),
    Voorzover = tree(r(cmp,adt_lex(cp,voorzover,voorzover,comp,[])),[]),
    replace(Zover,Voorzover,Ds1,Ds),
    debug_message(1,"treex correction: 'voor zover' is mwu complementizer~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Ds0    = [In,Plaats,Van,NP],
    In     = tree(r(obj1,adt_lex(_,Inr,_,prep,_)),[]),
    Plaats = tree(r(obj1,adt_lex(_,Plaatsr,_,prep,_)),[]),
    Van    = tree(r(hd,adt_lex(_,Vanr,_,prep,_)),[]),
    Inplaatsvan= tree(r(hd,adt_lex(_,Mwu,_,prep,[])),[]),
    concat_all([Inr,Plaatsr,Vanr],Mwu,' '),
    Ds = [Inplaatsvan,NP],
    debug_message(1,"treex correction: mwu ~w~n",[Mwu]).

transform_rule(tree(r(mod,p(_)),Ds0),tree(r(mod,p(pp)),Ds)) :-
    Ds0    = [Verwijzen,NaarPP],
    Verwijzen = tree(r(hd,adt_lex(_,Verw,_,AdjVerb,_)),[]),
    member(Verw,[verwijs,verwezen,verwijzen]),
    member(AdjVerb,[adj,verb]),
    NaarPP = tree(r(mod,p(pp)),[Naar,Obj1]),
    Naar = tree(r(hd,adt_lex(_,naar,_,prep,_)),[]),
    Ds = [ tree(r(hd,adt_lex(_,'met betrekking tot',_,prep,[])),[]),
	   Obj1
	 ],
    debug_message(1,"treex correction: MOD: verwijzen naar => met betrekking tot~n",[]).

transform_rule(tree(r(Rel,p(Cat)),Ds0),tree(r(Rel,p(Cat)),Ds)) :-
    \+ ( nonvar(Cat), member(Cat,[inf,ppart])),
    Hd = tree(r(hd,adt_lex(_,_,_,verb,Atts)),[]),
    member(Hd,Ds0),
    \+ member(stype=imparative,Atts),
    Obj = tree(r(obj1,SuCat),ObjDs),
    Su = tree(r(su,SuCat),ObjDs),
    Su2 = tree(r(su,_),_),
    Obj2 = tree(r(obj1,_),_),
    \+ member(Su2,Ds0),
    replace(Obj,Su,Ds0,Ds),
    member(Obj2,Ds),
    debug_message(1,"treex correction: 2x obj1, guessing the first one is su~n",[]).

transform_rule(tree(r(Rel,p(Cat)),Ds0),tree(r(Rel,p(Cat)),Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Lemma,_,verb,_)),[]),
    member(Hd,Ds0),
    Obj11 = tree(r(obj1,  _       ),_      ),
    Obj12 = tree(r(obj1,  Obj12Cat),Obj12Ds),
    Obj2  = tree(r(obj2,  Obj12Cat),Obj12Ds),
    sublist([Obj11,Obj12],Ds0),
    NoObj2 = tree(r(obj2,_),_),
    \+ member(NoObj2,Ds0),
    replace(Obj12,Obj2,Ds0,Ds),
    alpino_lex:xl(Lemma,verb(_,_,Sc),_,[],[]), member(np_np,Sc),
    debug_message(1,"treex correction: 2x obj1, guessing the second one is obj2~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)):-
    append(Prefix,[Obj11,Obj12|Tail],Ds0),
    Obj11 = tree(r(obj1,Obj1Cat),Obj1Ds), Obj1Ds = [_|_],
    Obj12 = tree(r(obj1,Obj2Cat),Obj2Ds),
    append(Prefix,[Obj|Tail],Ds),
    append(Obj1Ds,[tree(r('{app,mod}',Obj2Cat),Obj2Ds)],ObjDs),
    Obj = tree(r(obj1,Obj1Cat),ObjDs),
    debug_message(1,"treex correction: 2x obj1, guessing the second one is app/mod of first~n",[]).
    
transform_rule(tree(r(Rel,p(Cat)),Ds0),tree(r(Rel,p(Cat)),Ds)) :-
    Hd = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(Hd,Ds0),
    Obj11 = tree(r(obj1,  _       ),_      ),
    Obj12 = tree(r(obj1,  Obj12Cat),Obj12Ds),
    Mod   = tree(r(mod,   Obj12Cat),Obj12Ds),
    reverse(Ds0,Ds1),
    replace(Obj12,Mod,Ds1,Ds2),
    reverse(Ds2,Ds),
    member(Obj11,Ds), \+ Obj11 == Obj12,
    debug_message(1,"treex correction: 2x obj1, guessing the second one is mod~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,_,_,verb,Atts)),[]),
    member(Hd,Ds0),
    \+ member(stype=imparative,Atts),

    Su1 = tree(r(su,  _     ),_    ),
    Su2 = tree(r(su,  Su2Cat),Su2Ds),
    Obj2= tree(r(obj1,Su2Cat),Su2Ds),
    sublist([Su1,Su2],Ds0),
    NoObj = tree(r(obj1,_),_),
    \+ member(NoObj,Ds0),
    replace(Su2,Obj2,Ds0,Ds),
    debug_message(1,"treex correction: 2x su, guessing the second one is obj1~n",[]).

transform_rule(tree(r(mod,p(cp)),Ds0),
	       tree(r(mod,p(cp)),Ds)) :-
    Cmp = tree(r(cmp,adt_lex(_,door,_,_,_)),[]),
    member(Cmp,Ds0),
    VC   = tree(r(vc  ,Node),VCDs),
    BODY = tree(r(body,Node),VCDs),
    replace(VC,BODY,Ds0,Ds),
    debug_message(1,"treex correction: cmp (door) takes body complement, not vc~n",[]).


transform_rule(tree(Node,Ds0),tree(Node,[Hd|Ds1])):-
    member(tree(r(Rel,_),_),Ds0),
    Rel = mwp,
    mwu_words_part(Ds0,Ds1,[W0,W1|Words]),
    hdrug_util:concat_all([W0,W1|Words],Stem,' '),
    Hd = tree(r(hd,adt_lex(_,Stem,_,name,[])),[]),
    debug_message(1,"treex correction: mwu daughters moved into separate node~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Ds0 = [_,_], %% only two daugters, hd and mod
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[probeer,proberen]),
    Obj = tree(r(mod,ObjCat),ObjDs0),
    VC  = tree(r(vc,  ObjCat),ObjDs),
    replace(Obj,VC,Ds0,Ds),
    V0 = tree(r(hd,adt_lex(_,Update,_,Verb,_)),[]),
     V = tree(r(hd,adt_lex(_,Update,_,verb,[])),[]),
    replace(V0,V,ObjDs0,ObjDs),
    NoDet = tree(r(det,_),_),
    \+ member(NoDet,ObjDs),
    probeer_mod(Verb,Update),
    debug_message(1,"treex correction: proberen + mod => vc~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[probeer,proberen]),
    Obj = tree(r(obj1,ObjCat),ObjDs),
    VC  = tree(r(vc,  ObjCat),ObjDs),
    replace(Obj,VC,Ds0,Ds),
    V = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(V,ObjDs),
    NoDet = tree(r(det,_),_),
    \+ member(NoDet,ObjDs),
    debug_message(1,"treex correction: proberen + obj1 headed by verb => vc~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[probeer,proberen]),
    NoVc = tree(r(vc,_),_),
    \+ member(NoVc,Ds0),
    NoOb = tree(r(obj1,_),_),
    \+ member(NoOb,Ds0),
    Mod = tree(r(mod, ObjCat),ObjDs),
    VC  = tree(r(vc,  ObjCat),ObjDs),
    replace(Mod,VC,Ds0,Ds),
    member(V,ObjDs),
    V = tree(r(hd,adt_lex(_,_,_,Verb,_)),[]),
    Verb == verb,
    debug_message(1,"treex correction: proberen + mod headed by verb => vc~n",[]).

% probeer vc [v] -> probeer vc [te [v]]
transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[wens,wensen,
		     probeer,proberen,begin,beginnen,hoef,hoeven,raad_aan,aan_raden,adviseer,adviseren]),
    VC0 = tree(r(vc,p(inf)),VCDs),
    replace(VC0,VC,Ds0,Ds),
    Verb = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(Verb,VCDs),
%    NoSu = tree(r(su,_),_),
%    \+ member(NoSu,VCDs),
    \+ member(tree(r(sup,_),_),VCDs),
    VC = tree(r(vc,p(ti)),
	      [tree(r(cmp,adt_lex(_,te,_,comp,[])),[]),
	       tree(r(body,p(inf)),VCDs)
	       ]),
    debug_message(1,"treex correction: ~w V -> ~w te V~n",[Proberen,Proberen]).

transform_rule(tree(CAT,DS0),tree(CAT,DS)):-
    VC0=tree(r(vc,p(Var)),VCDs),
    VC =tree(r(vc,p(cp)),
	     [tree(r(cmp,adt_lex(_,Dat,_,comp,[])),[]),
	      tree(r(body,p(ssub)),VCDs)]),
    replace(VC0,VC,DS0,DS),
    H = tree(r(hd,adt_lex(_,Hlemma,_,Hpos,_)),[]),
    member(H,DS0),
    var(Var),
    Su = tree(r(su,SuNode),_),
    member(Su,VCDs), \+ SuNode = i(_),
    Hd = tree(r(hd,adt_lex(_,_,_,verb,Atts)),[]),
    member(Hd,VCDs),
    Obj = tree(r(obj1,_),_),
    \+ member(Obj,DS0),
    \+ member(stype=imparative,Atts),
    \+ member(stype=ynquestion,Atts),
    \+ member(stype=whquestion,Atts),
    alpino_lex:un_is_verb_lemma(Hlemma,Hroot),

    (   preferred_combination(dep35(dat,comp,hd/vc,Hpos,Hroot),
			      dep35( of,comp,hd/vc,Hpos,Hroot))
    ->  Dat = of
    ;   alpino_penalties:corpus_frequency_lookup(dep35(dat,comp,hd/vc,Hpos,Hroot),_),
	Dat = dat
    ),
    debug_message(1,"treex correction: finite vc/VP --> cp(~w,VP)~n",[Dat]).

% add su control
transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[wens,wensen,besluit,besluiten,
		     probeer,proberen,begin,beginnen,hoef,hoeven]),
    Su0 = tree(r(su,SuNode),SuDs),  
    Su  = tree(r(su,NewSuNode),SuDs),
    replace(Su0,Su,Ds0,Ds1),
    (   \+ SuNode = i(_),
	\+ SuNode = i(_,_),
	NewSuNode = i(Index,SuNode)
    ;   SuNode = i(Index),
	NewSuNode = i(Index)
    ;   SuNode = i(Index,SuNodeRest),
	NewSuNode = i(Index,SuNodeRest)
    ),	
    (   VC0 = tree(r(vc,p(ti)),[tree(r(cmp,Cmp),[]),
				tree(r(body,p(inf)),VCDs0)
			       ])
    ;   VC0 = tree(r(vc,p(ti)),[tree(r(body,p(inf)),VCDs0),
				tree(r(cmp,Cmp),[])				
			       ])
    ),

    member(VC0,Ds1),
    Cmp = adt_lex(_,te,_,_,_),
    NotSu = tree(r(su,_),_), \+ member(NotSu,VCDs0),
    NewSu = tree(r(su,i(Index)),[]),
    VC = tree(r(vc,p(ti)),[tree(r(cmp,Cmp),[]),
			    tree(r(body,p(inf)),[NewSu|VCDs0])
			   ]),
    replace(VC0,VC,Ds1,Ds),
    debug_message(1,"treex correction: add su control~n",[]).

% add obj2 control: ti
transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[raad_aan,aan_raden,adviseer,adviseren]),
    Obj2 = tree(r(obj2,Obj2Cat),Obj2Ds),
    Obj2N= tree(r(obj2,i(Index,Obj2Cat)),Obj2Ds),
    replace(Obj2,Obj2N,Ds0,Ds1),
    \+ Obj2Cat = i(_), \+ Obj2Cat = i(_,_),
    VC0 = tree(r(vc,p(ti)),[tree(r(cmp,Cmp),[]),
			    tree(r(body,p(inf)),VCDs0)
			   ]),
    member(VC0,Ds1),
    Cmp = adt_lex(_,te,_,_,_),
    NotSu = tree(r(su,_),_), \+ member(NotSu,VCDs0),
    NewSu = tree(r(su,i(Index)),[]),
    VC = tree(r(vc,p(ti)),[tree(r(cmp,Cmp),[]),
			    tree(r(body,p(inf)),[NewSu|VCDs0])
			   ]),
    replace(VC0,VC,Ds1,Ds),
    debug_message(1,"treex correction: add obj2 control~n",[]).

% add obj2 control: ti, obj2 was in wrong place
transform_rule(tree(Node,Ds0),tree(Node,[NewObj2|Ds])) :-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[raad_aan,aan_raden,adviseer,adviseren]),
    \+ member(tree(r(obj2,_),_),Ds0),
    VC0 = tree(r(vc,p(ti)),[tree(r(cmp,Cmp),[]),
			    tree(r(body,p(inf)),VCDs0)
			   ]),
    VC =  tree(r(vc,p(ti)),[tree(r(cmp,Cmp),[]),
			    tree(r(body,p(inf)),VCDs)
			   ]),
    replace(VC0,VC,Ds0,Ds),
    Cmp = adt_lex(_,te,_,_,_),
    Su = tree(r(su,Obj2Cat),Obj2Ds), 
    NewSu = tree(r(su,i(Index)),[]),
    replace(Su,NewSu,VCDs0,VCDs),
    \+ Obj2Cat = i(_), \+ Obj2Cat = i(_,_),
    NewObj2= tree(r(obj2,i(Index,Obj2Cat)),Obj2Ds),
    debug_message(1,"treex correction: promote su of inf to obj2~n",[]).

% add obj2 control: oti
transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[raad_aan,aan_raden,adviseer,adviseren]),
    Obj2 = tree(r(obj2,Obj2Cat),Obj2Ds),
    Obj2N= tree(r(obj2,i(Index,Obj2Cat)),Obj2Ds),
    replace(Obj2,Obj2N,Ds0,Ds1),
    \+ Obj2Cat = i(_), \+ Obj2Cat = i(_,_),
    VC0 = tree(r(vc,p(oti)),[Om,tree(r(body,p(ti)),[tree(r(cmp,Cmp),[]),
						   tree(r(body,p(inf)),VCDs0)
						  ])
			   ]),
    member(VC0,Ds1),
    Cmp = adt_lex(_,te,_,_,_),
    NotSu = tree(r(su,_),_), \+ member(NotSu,VCDs0),
    NewSu = tree(r(su,i(Index)),[]),
    VC = tree(r(vc,p(oti)),[Om,tree(r(body,p(ti)),[tree(r(cmp,Cmp),[]),
						   tree(r(body,p(inf)),[NewSu|VCDs0])
						  ])
			   ]),
    replace(VC0,VC,Ds1,Ds),
    debug_message(1,"treex correction: add obj2 control~n",[]).

% add su control
transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[probeer,proberen]),
    Su0 = tree(r(su,SuNode),SuDs),  
    Su  = tree(r(su,NewSuNode),SuDs),
    replace(Su0,Su,Ds0,Ds1),
    (   \+ SuNode = i(_),
	\+ SuNode = i(_,_),
	NewSuNode = i(Index,SuNode)
    ;   SuNode = i(Index),
	NewSuNode = i(Index)
    ;   SuNode = i(Index,SuNodeRest),
	NewSuNode = i(Index,SuNodeRest)
    ),
    VC0 = tree(r(vc,p(oti)),[tree(r(cmp,Om),[]),
			     tree(r(body,p(ti)),[tree(r(cmp,Cmp),[]),
						 tree(r(body,p(inf)),VCDs0)
						])]),
    member(VC0,Ds1),
    Cmp = adt_lex(_,te,_,_,_),
    Om  = adt_lex(_,om,_,_,_),
    NotSu = tree(r(su,_),_), \+ member(NotSu,VCDs0),
    NewSu = tree(r(su,i(Index)),[]),
    VC = tree(r(vc,p(oti)),[tree(r(cmp,Om),[]),
			    tree(r(body,p(ti)),[tree(r(cmp,Cmp),[]),
						tree(r(body,p(inf)),[NewSu|VCDs0])
					       ])]),
    replace(VC0,VC,Ds1,Ds),
    debug_message(1,"treex correction: add su control~n",[]).

% add su control
transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[wil,willen,kan,kunnen,mag,mogen,moet,moeten,zal,zullen]),
    Su0 = tree(r(su,SuNode),SuDs),  
    Su  = tree(r(su,NewSuNode),SuDs),
    replace(Su0,Su,Ds0,Ds1),
    (   \+ SuNode = i(_),
	\+ SuNode = i(_,_),
	NewSuNode = i(Index,SuNode)
    ;   SuNode = i(Index),
	NewSuNode = i(Index)
    ;   SuNode = i(Index,SuNodeRest),
	NewSuNode = i(Index,SuNodeRest)
    ),	
    NewSu = tree(r(su,i(Index)),[]),
    (   replace(VC0,VC,Ds1,Ds),
	add_su_to_inf(VC0,VC,NewSu)
    ;   VC0 = tree(r(vc,p(conj)),[Cnj10,Crd,Cnj20]),
	VC  = tree(r(vc,p(conj)),[Cnj11,Crd,Cnj22]),
	replace(VC0,VC,Ds1,Ds), 
	add_su_to_inf_c(Cnj10,Cnj11,NewSu),
	add_su_to_inf_c(Cnj20,Cnj22,NewSu)
    ),
    debug_message(1,"treex correction: add su control for modals 1~n",[]).

% add su control
transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[heb,hebben]),
    Su0 = tree(r(su,SuNode),SuDs),  
    Su  = tree(r(su,NewSuNode),SuDs),
    \+ member(tree(r(obj1,_),_), Ds0),
    replace(Su0,Su,Ds0,Ds1),
    (   \+ SuNode = i(_),
	\+ SuNode = i(_,_),
	NewSuNode = i(Index,SuNode)
    ;   SuNode = i(Index),
	NewSuNode = i(Index)
    ;   SuNode = i(Index,SuNodeRest),
	NewSuNode = i(Index,SuNodeRest)
    ),	
    NewSu = tree(r(su,i(Index)),[]),
    (   replace(VC0,VC,Ds1,Ds),
	add_su_to_ppart(VC0,VC,NewSu)
    ;   VC0 = tree(r(vc,p(conj)),[Cnj10,Crd,Cnj20]),
	VC  = tree(r(vc,p(conj)),[Cnj11,Crd,Cnj22]),
	replace(VC0,VC,Ds1,Ds), 
	add_su_to_ppart_c(Cnj10,Cnj11,NewSu),
	add_su_to_ppart_c(Cnj20,Cnj22,NewSu)
    ),
    debug_message(1,"treex correction: add su control for hebben 1~n",[]).


% add su control
transform_rule(tree(Node,Ds0),tree(Node,[SuN|Ds1])) :-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[wil,willen,kan,kunnen,mag,mogen,moet,moeten,zal,zullen]),
    VC0 = tree(r(vc,p(inf)),VCDS0),
    VC  = tree(r(vc,p(inf)),VCDS),
    NotSu = tree(r(su,_),_),
    \+ member(NotSu,Ds0),
    Cmp = tree(r(cmp,_),_),
    \+ member(Cmp,VCDS0),
    replace(VC0,VC,Ds0,Ds1),
    (   Su0 = tree(r(su,i(INDEX,_SuCat)),_SuDs),
	SuN = Su0,
	SuCat = ok
    ;   Su0 = tree(r(su,SuCat),SuDs),
	SuN = tree(r(su,i(INDEX,SuCat)),SuDs)
    ),
    Su1 = tree(r(su,i(INDEX)),[]),
    replace(Su0,Su1,VCDS0,VCDS),
    \+ SuCat = i(_),
    \+ SuCat = i(_,_),
    debug_message(1,"treex correction: add su control for modals 2~n",[]).

% add su control
transform_rule(tree(Node,Ds0),tree(Node,[SuN|Ds1])) :-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    \+ member(tree(r(obj1,_),_), Ds0),
    member(Proberen,[heb,hebben]),
    VC0 = tree(r(vc,p(ppart)),VCDS0),
    VC  = tree(r(vc,p(ppart)),VCDS),
    NotSu = tree(r(su,_),_),
    \+ member(NotSu,Ds0),
    Cmp = tree(r(cmp,_),_),
    \+ member(Cmp,VCDS0),
    replace(VC0,VC,Ds0,Ds1),
    (   Su0 = tree(r(su,i(INDEX,_SuCat)),_SuDs),
	SuN = Su0,
	SuCat = ok
    ;   Su0 = tree(r(su,SuCat),SuDs),
	SuN = tree(r(su,i(INDEX,SuCat)),SuDs)
    ),
    Su1 = tree(r(su,i(INDEX)),[]),
    replace(Su0,Su1,VCDS0,VCDS),
    \+ SuCat = i(_),
    \+ SuCat = i(_,_),
    debug_message(1,"treex correction: add su control for hebben 2~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[wil,willen,kan,kunnen,mag,mogen,moet,moeten,zal,zullen]),
    VC0 = tree(r(vc,p(inf)),VCDS),
    VC  = tree(r(vc,p(inf)),[MOD|VCDS]),
    MOD = tree(r(mod,_ModCat),_ModDs),
    replace(VC0,VC,Ds0,Ds1),
    select(MOD,Ds1,Ds),
    debug_message(1,"treex correction: lower modifier for modals~n",[]).
    
transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[hoef,hoeven]),
    VC0 = tree(r(vc,p(ti)),[Te,tree(r(body,p(inf)),VCDS)]),
    VC  = tree(r(vc,p(ti)),[Te,tree(r(body,p(inf)),[MOD|VCDS])]),
    MOD = tree(r(mod,_ModCat),_ModDs),
    replace(VC0,VC,Ds0,Ds1),
    select(MOD,Ds1,Ds),
    debug_message(1,"treex correction: lower modifier for modals~n",[]).
    
transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[word,worden,ben,zijn]),
    VC0 = tree(r(vc,p(Ppart)),VCDS), 
    VC  = tree(r(vc,p(ppart)),[MOD|VCDS]),
    MOD = tree(r(mod,_ModCat),_ModDs),
    replace(VC0,VC,Ds0,Ds1),
    Ppart == ppart,
    select(MOD,Ds1,Ds),
    debug_message(1,"treex correction: lower modifier for auxiliaries~n",[]).

transform_rule(tree(r(vc,p(inf)),Ds0),tree(r(vc,p(conj)),Ds)):-
    Su = tree(r(su,_),_),
    select(Su,Ds0,Ds1),
    Cnj1 = tree(r(cnj,p(inf)),Cnj1Ds),
    Cnj2 = tree(r(cnj,p(inf)),[Su|Cnj1Ds]),
    replace(Cnj1,Cnj2,Ds1,Ds2),
    NotSu = tree(r(su,_),_),
    \+ member(NotSu,Cnj1Ds),
    more_cnj(Ds2,Su,Ds),
    debug_message(1,"treex correction: distribute su over conjuncts~n",[]).

transform_rule(tree(r(Rel,p(Cat)),[Su,Cmp,Body]),tree(r(Rel,p(Cat)),[Cmp,Body2])) :-
     Su   = tree(r(su,_),_),
     Cmp  = tree(r(cmp,_),_),
     Body = tree(r(body,BodyCat),[B|BodyDs]),
     Body2= tree(r(body,BodyCat),[Su,B|BodyDs]),
     debug_message(1,"treex correction: distribute su over cmp-body 1~n",[]).
    
transform_rule(tree(r(Rel,p(Cat)),[Su,Cmp,Body]),tree(r(Rel,p(Cat)),[Cmp,Body2])) :-
     Su   = tree(r(su,_),_),
     Cmp  = tree(r(cmp,_),_),
     Body = tree(r(body,adt_lex(A,B,C,verb,E)),[]),
       Hd = tree(r(hd,adt_lex(A,B,C,verb,E)),[]),
     Body2= tree(r(body,p(inf)),[Su,Hd]),
     debug_message(1,"treex correction: distribute su over cmp-body 2~n",[]).
    

% add passive
transform_rule(tree(Node,Ds0),tree(Node,[SuI|Ds])) :-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[word,worden]),
    Su  = tree(r(su,SuCat),SuDs),
    \+ member(Su,Ds0),
    VC0 = tree(r(vc,p(ppart)),VCDs0),
    VC  = tree(r(vc,p(ppart)),VCDs),
    replace(VC0,VC,Ds0,Ds),
    ObjI = tree(r(obj1,i(INDEX)),[]),
    SuI  = tree(r(su,i(INDEX,SuCat)),SuDs),
    replace(Su,ObjI,VCDs0,VCDs),
    debug_message(1,"treex correction: promote su for passives~n",[]).


transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[word,worden]),
    Su0 = tree(r(su,SuNode),SuDs),  
    Su  = tree(r(su,NewSuNode),SuDs),
    replace(Su0,Su,Ds0,Ds1),
    (   \+ SuNode = i(_),
	\+ SuNode = i(_,_),
	NewSuNode = i(Index,SuNode)
    ;   SuNode = i(Index),
	NewSuNode = i(Index)
    ;   SuNode = i(Index,SuNodeRest),
	NewSuNode = i(Index,SuNodeRest)
    ),	
    NewSu = tree(r(obj1,i(Index)),[]),
    (   VC0 = tree(r(vc,p(ppart)),VCDs0),
	member(VC0,Ds1),
	NotCmp = tree(r(cmp,_),_), \+ member(NotCmp,VCDs0),
	NotSu = tree(r(su,_),_), \+ member(NotSu,VCDs0),
	NotObj1 = tree(r(obj1,_),_), \+ member(NotObj1,VCDs0),
	NotPObj1 = tree(r(pobj1,_),_), \+ member(NotPObj1,VCDs0),
	VC = tree(r(vc,p(ppart)),[NewSu|VCDs0]),
	replace(VC0,VC,Ds1,Ds)
    ;   VC0 = tree(r(vc,adt_lex(A,B,C,D,E)),[]),
	VC  = tree(r(vc,p(ppart)),[NewSu,tree(r(hd,adt_lex(A,B,C,D,E)),[])]),
	replace(VC0,VC,Ds1,Ds)
    ),
    debug_message(1,"treex correction: add obj1 co-indexing for passives~n",[]).



% add su control
transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(Hd,Ds0),
    Su0 = tree(r(su,SuNode),SuDs),  
    Su  = tree(r(su,NewSuNode),SuDs),
    replace(Su0,Su,Ds0,Ds1),
    (   \+ SuNode = i(_),
	\+ SuNode = i(_,_),
	NewSuNode = i(Index,SuNode)
    ;   SuNode = i(Index),
	NewSuNode = i(Index)
    ;   SuNode = i(Index,SuNodeRest),
	NewSuNode = i(Index,SuNodeRest)
    ),	
    VC0 = tree(r(vc,p(ahi)),[tree(r(cmp,Cmp),[]),
			    tree(r(body,p(inf)),VCDs0)
			   ]),
    member(VC0,Ds1),
    (   Cmp = adt_lex(_,'aan het',_,_,_)
    ;   Cmp = adt_lex(_,'obj:aan het',_,_,_)
    ),
    NotSu = tree(r(su,_),_), \+ member(NotSu,VCDs0),
    NewSu = tree(r(su,i(Index)),[]),
    VC = tree(r(vc,p(ahi)),[tree(r(cmp,Cmp),[]),
			    tree(r(body,p(inf)),[NewSu|VCDs0])
			   ]),
    replace(VC0,VC,Ds1,Ds),
    debug_message(1,"treex correction: add su control~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,persist,_,verb,Atts)),[]),
    Su = tree(r(su,Cat),SuDs),
    select(Hd,Ds0,Ds1),
    select(Su,Ds1,Ds2),
    Ds = [tree(r(hd,adt_lex(_,blijf,_,verb,Atts)),[]),
	  tree(r(su,i(L,Cat)),SuDs),
	  tree(r(vc,p(inf)),[tree(r(hd,adt_lex(_,besta,_,verb,[])),[]),
			     tree(r(su,i(L)),[])|Ds2])
	 ],
    debug_message(1,"treex correction: persist => blijft bestaan~n",[]).	       

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,probeer,_,verb,_)),[]),
    Obj1 = tree(r(obj1,_),_),
    VC = tree(r(vc,_),_),
    member(Hd,Ds0),
    \+ member(Obj1,Ds0),
    \+ member(VC,Ds0),
    MOD = tree(r(mod,p(oti)),[OM,VP]),
    VC = tree(r(vc,p(oti)),[OM,VP]),
    OM = tree(r(cmp,adt_lex(_,OML,_,_,_)),_),
    replace(MOD,VC,Ds0,Ds),
    member(OML,[om,te,dat]),
    debug_message(1,"treex correction: probeer + om is VC~n",[]).	       

transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd0 = tree(r(hd, adt_lex(_,Cellphone,_,noun,Atts)),[]),
    Hd  = tree(r(hd, adt_lex(_,Phone,_,noun,Atts)),[]),
    phone(Mobiel,Cellphone,Phone),
    replace(Hd0,Hd,Ds0,Ds),
    Mod = tree(r(mod,adt_lex(_,Mobiel,_,_,_)),[]),
    member(Mod,Ds0),
    debug_message(1,"treex correction: ~w ~w => ~w ~w~n",[Mobiel,Cellphone,Mobiel,Phone]).

transform_rule(tree(Cat,Ds0), tree(Cat,Ds) ) :-
    Ds0 = [Unplug0,NP],
    Ds  = [Unplug1,Obj1],
    NP = tree(r(mod,ModCat),NPDS),
    Obj1 = tree(r(obj1,ModCat),NPDS),
    Unplug0 = tree(r(hd,adt_lex(_,B0,_,_,_)),[]),
    Unplug1 = tree(r(hd,adt_lex(_,B1,_,verb,[])),[]), 
    unplug(B0,B1),
    looks_like_np(NP),
    debug_message(1,"treex correction: ~w mod => ~w obj1~n",[B0,B1]).

transform_rule(tree(C,Ds0),tree(C,[InBeslag|Ds])):-
    Hd0 = tree(r(hd,adt_lex(_,occupy,_,verb,Atts)),[]),
    Hd1 = tree(r(hd,adt_lex(_,neem,_,verb,Atts)),[]),
    replace(Hd0,Hd1,Ds0,Ds),
    InBeslag = tree(r(svp,adt_lex(_,'in beslag',_,fixed,[])),[]),
    debug_message(1,"treex correction: occupy => neemt in beslag~n",[]).

transform_rule(tree(C,Ds0),tree(C,Ds)):-
    D0 = tree(r(Rel,adt_lex(A,B0,_,Pos0,E0)),[]),
    D1 = tree(r(Rel,adt_lex(A,B ,_,Pos, E)),[]),
    replace(D0,D1,Ds0,Ds),
    Hd = tree(r(ContextRel,adt_lex(_,Head,_,HeadPos,_)),[]),
    member(Hd,Ds0),
    special_lemma(Head,HeadPos,ContextRel,Rel,B0,B,Pos0,Pos,E0,E),
    debug_message(1,"treex correction: adapt lemma in context: ~w ~w ~w~n",[B0,B,Head]).

transform_rule(tree(C,Ds0),tree(C,Ds)):-
    D0 = tree(r(Rel,adt_lex(A,B0,_,Pos0,E0)),[]),
    D1 = tree(r(Rel,adt_lex(A,B ,_,Pos, E)),[]),
    replace(D0,D1,Ds0,Ds),
    ContextD = tree(r(ContextRel,_),ContextDs),
    ContextHd = tree(r(hd,adt_lex(_,Head,_,HeadPos,_)),[]),
    member(ContextD,Ds0),
    member(ContextHd,ContextDs),
    special_lemma(Head,HeadPos,ContextRel,Rel,B0,B,Pos0,Pos,E0,E),
    debug_message(1,"treex correction: adapt lemma in context: ~w ~w ~w~n",[B0,B,Head]).

transform_rule(tree(C,Ds0),tree(C,Ds)):-
    D0 = tree(r(Rel,adt_lex(A,B0,_,Pos0,E0)),[]),
    D1 = tree(r(Rel,adt_lex(A,B ,_,Pos, E)),[]),
    replace(D0,D1,Ds0,Ds),
    special_lemma(B0,B,Pos0,Pos,E0,E),
    debug_message(1,"treex correction: adapt lemma: ~w ~w~n",[B0,B]).

transform_rule(tree(C,Ds0),tree(C,Ds)):-
    D0 = tree(r(Rel,i(Ix,adt_lex(A,B0,_,Pos0,E0))),[]),
    D1 = tree(r(Rel,i(Ix,adt_lex(A,B ,_,Pos, E))),[]),
    replace(D0,D1,Ds0,Ds),
    special_lemma(B0,B,Pos0,Pos,E0,E),
    debug_message(1,"treex correction: adapt lemma: ~w ~w~n",[B0,B]).

transform_rule(tree(r(Rel,adt_lex(A,B0,_,D0,E0)),[]),tree(r(Rel,adt_lex(A,B ,_,D,E)),[])):-
    special_lemma(B0,B,D0,D,E0,E),
    debug_message(1,"treex correction: adapt lemma: ~w ~w~n",[B0,B]).

transform_rule(tree(r(REL,Node),Ds0),tree(r(REL,Node),Ds)):-
    Hd = tree(r(hd,adt_lex(_,Verb,_,_,_)),[]),
    member(Hd,Ds0),
    Pc = tree(r(pc,i(INDEX)),[]),
    Mod = tree(r(mod,i(INDEX)),[]),
    replace(Pc,Mod,Ds0,Ds),
    debug_message(1,"treex correction: pc -> mod ~w~n",[Verb]).

transform_rule(tree(r(REL,Node),Ds0),tree(r(REL,Node),Ds)):-
    Hd = tree(r(hd,adt_lex(_,Verb,_,Pos,_)),[]),
    member(Hd,Ds0),
    Pc = tree(r(pc,p(Cat)),PcDs),
    Mod = tree(r(mod,p(Cat)),PcDs),
    replace(Pc,Mod,Ds0,Ds),
    (   PrepD = tree(r(hd,adt_lex(_,Prep,_,prep,_)),[]),
	member(PrepD,PcDs),
	(   REL=obj1  % typically in a nominalization
	;   unlikely_pc_combination(Pos,Verb,Prep)
	;   Pc2 = tree(r(pc,_),Pc2Ds),
	    member(Pc2,Ds),
	    PrepD2 = tree(r(hd,adt_lex(_,Prep2,_,prep,_)),[]),
	    member(PrepD2,Pc2Ds),
	    ( Prep == Prep2
	    ;   alpino_lex:un_is_verb_lemma(Verb,VerbR),
		preferred_combination(dep35(Prep2,prep,hd/pc,verb,VerbR),
				      dep35(Prep, prep,hd/pc,verb,VerbR))
	    )
	;   Predc = tree(r(predc,_),_),
	    Se = tree(r(se,_),_),
	    member(Predc,Ds),
	    member(Se,Ds)
	;   VC = tree(r(vc,_),_),
	    member(VC,Ds)
	;   OBJ1 = tree(r(obj1,_),_),
	    member(OBJ1,Ds)
	)
    ;   Cnj = tree(r(cnj,_),_),
	member(Cnj,PcDs),
	Prep='(conjunction)'
    ),  
    debug_message(1,"treex correction: pc -> mod ~w ~w~n",[Verb,Prep]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Verbinden,_,_,_)),[]),
    member(Hd,Ds0),
    MOD1= tree(r(mod,ModCat),[Prep1,Obj1]),
    Prep1 = tree(r(hd,adt_lex(Px,Naar,_,prep,Pz)),[]),
    Prep2 = tree(r(hd,adt_lex(Px,Met,_,prep,Pz)),[]),
    MOD2= tree(r(ModRel,ModCat),[Prep2,Obj1]),
    verbinden_naar_met(Verbinden,Naar,Met),
    \+ member(MOD2,Ds0),
    MOD3= tree(_,MOD3DS),
    \+ ( member(MOD3,Ds0), member(MOD2,MOD3DS) ),  %e.g. conjunction
    ModRel=mod,
    replace(MOD1,MOD2,Ds0,Ds),
    debug_message(1,"treex correction: ~w ~w => ~w ~w~n",[Verbinden,Naar,Verbinden,Met]).

transform_rule(tree(r(Rel0,p(pp)),Ds0), tree(r(Rel,p(pp)),Ds)) :-
    Cmp =  tree(r(cmp,adt_lex(_,Bij,_,comp,_)),[]),
    Prep = tree(r(hd,adt_lex(_,Bij,_,prep,[])),[]),
    replace(Cmp,Prep,Ds0,Ds1),
    member(Bij,[bij,op,naast,met,na,voor]),
    Body=  tree(r(body,p(np)),BodyDs),
    Obj1 = tree(r(obj1,p(np)),BodyDs),
    replace(Body,Obj1,Ds1,Ds),
    (   Rel0 == vc ->  Rel = mod ; true ),
    debug_message(1,"treex correction: prep+nominalization is hd/obj1, not cmp/body~n",[]).

transform_rule(tree(r(Rel0,p(pp)),Ds0), tree(r(Rel,p(pp)),Ds)) :-
    \+ Rel = body, % om te
    Cmp =  tree(r(cmp,adt_lex(_,Bij,_,comp,_)),[]),
    Prep = tree(r(hd,adt_lex(_,Bij,_,prep,[])),[]),
    replace(Cmp,Prep,Ds0,Ds1),
    Body=  tree(r(body,BodyCat),BodyDs),
    Obj1 = tree(r(obj1,BodyCat),BodyDs),
    replace(Body,Obj1,Ds1,Ds),
    Det = tree(r(det,_),_),
    member(Det,BodyDs),
    (   Rel0 == vc ->  Rel = mod ; true ),
    debug_message(1,"treex correction: prep+nominalization is hd/obj1, not cmp/body~n",[]).

transform_rule(tree(r(Rel0,p(pp)),Ds0), tree(r(Rel,p(pp)),Ds)) :-
    Cmp =  tree(r(cmp,adt_lex(_,Bij,_,comp,_)),[]),
    Prep = tree(r(hd,adt_lex(_,Bij,_,prep,[])),[]),
    replace(Cmp,Prep,Ds0,Ds1), 
    member(Bij,[na]),
    Body=  tree(r(body,adt_lex(A,B,C,verb,E)),BodyDs),
    Obj1 = tree(r(obj1,adt_lex(A,B,C,verb,E)),BodyDs),
    replace(Body,Obj1,Ds1,Ds),
    (   Rel0 == vc ->  Rel = mod ; true ),
    debug_message(1,"treex correction: prep+nominalization is hd/obj1, not cmp/body~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Mod = tree(r(vc,p(oti)),DsM),
    Su  = tree(r(su,p(oti)),DsM),
    replace(Mod,Su,Ds0,Ds),
    Hd = tree(r(cmp,adt_lex(_,om,_,comp,_)),[]),
    member(Hd,DsM),
    Predc = tree(r(predc,_),_),
    member(Predc,Ds0),
    NotSu = tree(r(su,_),_),
    \+ member(NotSu,Ds0),
    debug_message(1,"treex correction: oti is su, not vc~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    V = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    P = tree(r(predc,_),_),
    member(V,Ds0),
    member(P,Ds0),
    Het = tree(r(su,adt_lex(_,het,_,_,_)),[]),
    member(Het,Ds0),
    VC = tree(r(vc,VCCat),VCDs),
    TE = tree(r(vc,p(cp)),[tree(r(cmp,adt_lex(cp,dat,dat,comp,[])),[]),
			   tree(r(body,VCCat),VCDs)]),
    replace(VC,TE,Ds0,Ds),
    V2= tree(r(hd,adt_lex(_,_,_,verb,Atts)),[]),
    member(V2,VCDs),
    member(stype=declarative,Atts),
    debug_message(1,"treex correction: add dat to SSUB~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    V = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    P = tree(r(predc,_),_),
    member(V,Ds0),
    member(P,Ds0),
    Het = tree(r(su,adt_lex(_,het,_,_,_)),[]),
    member(Het,Ds0),
    VC = tree(r(vc,VCCat),VCDs),
    TE = tree(r(vc,p(ti)),[tree(r(cmp,adt_lex(_,te,te,comp,[])),[]),
			   tree(r(body,VCCat),VCDs)]),
    replace(VC,TE,Ds0,Ds),
    V2= tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(V2,VCDs),
    debug_message(1,"treex correction: add te to INF~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Mod = tree(r(vc,p(Oti)),DsM0),
    Su  = tree(r(su,p(Oti)),DsM),
    replace(Mod,Su,Ds0,Ds1),
    (  Oti = oti,
       Hd = tree(r(cmp,adt_lex(_,om,_,comp,_)),[])
    ;  Oti = ti,
       Hd = tree(r(cmp,adt_lex(_,te,_,comp,_)),[])
    ),
    member(Hd,DsM0),
    Predc = tree(r(predc,_),_),
    member(Predc,Ds0),
    HetSu = tree(r(su,i(Index,adt_lex(A,het,C,D,E))),[]),
    Sup = tree(r(sup,adt_lex(A,het,C,D,E)),[]),
    replace(HetSu,Sup,Ds1,Ds),
    remove_index_ds(DsM0,Index,DsM),
    debug_message(1,"treex correction: oti/ti is su, not vc; het is sup~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Mod = tree(r(vc,p(cp)),DsM),
    Su  = tree(r(su,p(cp)),DsM),
    replace(Mod,Su,Ds0,Ds1),
    Hd = tree(r(cmp,adt_lex(_,dat,_,comp,_)),[]),
    member(Hd,DsM),
    Predc = tree(r(predc,_),_),
    member(Predc,Ds0),
    HetSu = tree(r(su, adt_lex(A,het,C,D,E)),[]),
    Sup   = tree(r(sup,adt_lex(A,het,C,D,E)),[]),
    replace(HetSu,Sup,Ds1,Ds),
    debug_message(1,"treex correction: dat-cp is su, not vc; het is sup~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,[SU|Ds])) :-
    HetSu  = tree(r(su,adt_lex(A,het,C,D,E)),[]),
    HetSup = tree(r(sup,adt_lex(A,het,C,D,E)),[]),
    replace(HetSu,HetSup,Ds0,Ds1),
    PredC0  = tree(r(predc,PredCat),PredDs0),
    PredC   = tree(r(predc,PredCat),PredDs),
    replace(PredC0,PredC,Ds1,Ds),
    VC = tree(r(vc,VCCat),[tree(r(cmp,CMP),CMPDS),Rest]),
    SU = tree(r(su,VCCat),[tree(r(cmp,CMP),CMPDS),Rest]),
    select(VC,PredDs0,PredDs),
    debug_message(1,"treex correction: vc of predc is su; het is sup~n",[]).
    
		    
transform_rule(tree(Node,Ds0), tree(Node,[SU|Ds])) :-
    HetSu  = tree(r(su,i(Index)),[]),
    HetSup = tree(r(sup,i(Index)),[]),  % en nu maar hopen dat het naar "het" verwijst
    replace(HetSu,HetSup,Ds0,Ds1),
    PredC0  = tree(r(predc,PredCat),PredDs0),
    PredC   = tree(r(predc,PredCat),PredDs),
    replace(PredC0,PredC,Ds1,Ds),
    VC = tree(r(vc,VCCat),[tree(r(cmp,CMP),CMPDS),Rest]),
    SU = tree(r(su,VCCat),[tree(r(cmp,CMP),CMPDS),Rest]),
    select(VC,PredDs0,PredDs),
    debug_message(1,"treex correction: vc of predc is su; indexed su is sup~n",[]).
    
		    
transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Mod = tree(r(vc,p(Oti)),DsM),
    Su  = tree(r(su,p(Oti)),DsM),
    replace(Mod,Su,Ds0,Ds1),
    (  Oti = oti,
       Hd = tree(r(cmp,adt_lex(_,om,_,comp,_)),[])
    ;  Oti = ti,
       Hd = tree(r(cmp,adt_lex(_,te,_,comp,_)),[])
    ),
    member(Hd,DsM),
    Predc = tree(r(predc,_),_),
    member(Predc,Ds0),
    HetSu = tree(r(su,adt_lex(A,het,C,D,E)),[]),
    Sup = tree(r(sup,adt_lex(A,het,C,D,E)),[]),
    replace(HetSu,Sup,Ds1,Ds),
    debug_message(1,"treex correction: oti/ti is su, not vc; het is sup~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,_)),[]),
    member(Hd,Ds0),
    Om = tree(r(vc,p(oti)),VCDs),
    Om2 = tree(r(mod,p(oti)),VCDs),
    replace(Om,Om2,Ds0,Ds),
    OmW = tree(r(cmp,adt_lex(_,om,_,comp,_)),[]),
    member(OmW,VCDs),
    \+ alpino_penalties:corpus_frequency_lookup(dep35(om,comp,hd/vc,verb,Verb),_),
    \+ (   alpino_lex:un_is_verb_lemma(Verb,Root),
	   alpino_penalties:corpus_frequency_lookup(dep35(om,comp,hd/vc,verb,Root),_)
       ),
    debug_message(1,"treex correction: oti is mod, not vc here~n",[]).
    
transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,Verb,_,verb,_)),[]),
    member(Hd,Ds0),
    alpino_lex:un_is_verb_lemma(Verb,VerbRoot),
    Pc =  tree(r(obj1,adt_lex(A,B,_,pron,E)),[]),
    Mod = tree(r(  se,adt_lex(A,B,_,pron,E)),[]),
    replace(Pc,Mod,Ds0,Ds),
    worse_combination(dep35(B,pron(wkpro,nwh),hd/obj1,verb,VerbRoot),
		      dep35(B,pron(refl),hd/se,  verb,VerbRoot)),
    debug_message(1,"treex correction: obj1 -> se~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,Worden,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Worden,[word,worden]),
    Obj1 = tree(r(obj1,Cat),PcDs),
    Mod = tree(r(predc,Cat),PcDs),
    replace(Obj1,Mod,Ds0,Ds),
    debug_message(1,"treex correction: 'worden' + obj1 -> predc~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,Worden,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Worden,[word,worden]),
    NotVC = tree(r(vc,_),_),
    \+ member(NotVC,Ds0),
    NotPred = tree(r(predc,_),_),
    \+ member(NotPred,Ds0),
    Mod = tree(r(mod,p(ppart)),PcDs),
    VC = tree(r(vc,p(ppart)),PcDs),
    replace(Mod,VC,Ds0,Ds),
    V = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(V,PcDs),
    debug_message(1,"treex correction: 'worden' + mod -> vc~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Cmp0 = tree(r(cmp,adt_lex(A,te,_,part,C)),[]),
    Cmp = tree(r(cmp,adt_lex(A,te,_,comp,C)),[]),
    replace(Cmp0,Cmp,Ds0,Ds),
    debug_message(1,"treex correction: 'te' has pos=comp~n",[]).

transform_rule(tree(r(Rel,p(cp)),Ds0),tree(r(Rel,p(du)),Ds)):-
    Cmp0 = tree(r(hd,adt_lex(A,B,_,_,E)),[]),
    Cmp = tree(r(dlink,adt_lex(A,B,_,comp,E)),[]),
    Body0 = tree(r(body,p(ssub)),BodyDs),
    Body = tree(r(nucl,p(smain)),BodyDs),
    replace(Cmp0,Cmp,Ds0,Ds1),
    replace(Body0,Body,Ds1,Ds),
    member(B,[want,maar,of,en,dus]),
    debug_message(1,"treex correction: complementizer has rel=dlink~n",[]).

transform_rule(tree(r(Rel,p(conj)),[Cmp0,Body0]),tree(r(Rel,p(du)),[Cmp,Body])):-
    Cmp0 = tree(r(crd,adt_lex(A,B,_,vg,E)),[]),
    Cmp = tree(r(dlink,adt_lex(A,B,_,comp,E)),[]),
    Body0 = tree(r(cnj,p(ssub)),BodyDs),
    Body = tree(r(nucl,p(smain)),BodyDs),
    member(B,[want,maar,of,en,dus]),
    debug_message(1,"treex correction: crd has rel=dlink~n",[]).

transform_rule(tree(r(Rel,p(cp)),Ds0),tree(r(Rel,p(cp)),Ds)):-
    Cmp0 = tree(r(hd,adt_lex(A,B,_,comp,E)),[]),
    Cmp = tree(r(cmp,adt_lex(A,B,_,comp,E)),[]),
    Body = tree(r(body,p(_)),_),
    member(Body,Ds0),
    replace(Cmp0,Cmp,Ds0,Ds),
    debug_message(1,"treex correction: complementizer has rel=cmp~n",[]).

transform_rule(tree(r('--',_),[Please,Body]), tree(r('--',BodyCat),[tree(r(mod,PleaseLex),[])|BodyDs])) :-
    Please = tree(r(hd,PleaseLex),[]),
    PleaseLex = adt_lex(_,alstublieft,_,_,_),
    Body = tree(r(body,BodyCat),BodyDs),
    debug_message(1,"treex correction: lower alstublieft~n",[]).

transform_rule(tree(r(Rel,p(CAT)),[Cmp,Body0]),tree(r(Rel,p(CAT)),[Cmp,Body])):-
    Cmp   = tree(r(cmp,adt_lex(_,_,_,comp,_)),[]),
    Body0 = tree(r(mod,BodyCat),BodyDs),
    Body  = tree(r(body,BodyCat),BodyDs),
    debug_message(1,"treex correction: complement of complementizer has rel=body~n",[]).

transform_rule(tree(r(Rel,adt_lex(A,B,_,Noun,[])),[]),
	       tree(r(Rel,adt_lex(A,B,_,adj,[personalized=true,aform=base])),[])) :-
    nonvar(Noun), Noun=noun,
    \+ Rel = predc, \+ Rel = hd, % ??
    alpino_genlex:dict_entry(B,nominalized_adjective,_),
    \+ alpino_genlex:dict_entry(B,noun(_,_,_),_),
    \+ alpino_genlex:dict_entry(B,tmp_noun(_,_,_),_),
    debug_message(1,"treex correction: ~w is personalized adjective~n",[B]).

transform_rule(tree(r(Rel,adt_lex(A,veel,_,pron,[rnum=pl])),[]),
	       tree(r(Rel,adt_lex(A,veel,_,adj,[personalized=true,aform=base])),[])) :-
    \+ Rel = mod,
    debug_message(1,"treex correction: 'veel' (plural) is personalized adjective~n",[]).

transform_rule(tree(r(REL,p(Var)),Ds),
	       tree(r(REL,p(smain)),Ds)):-
    var(Var),
    member(REL,['--',cnj]),
    Hd = tree(r(hd,adt_lex(_,_,_,verb,Atts)),[]),
    member(Hd,Ds),
    \+ member(stype=ynquestion,Atts),
    \+ member(stype=imparative,Atts),
    Su = tree(r(su,_),_),
    member(Su,Ds),
    debug_message(1,"treex correction: fill in cat=smain~n",[]).

transform_rule(tree(r('--',p(conj)),Ds),
	       tree(r('--',p(conj)),Ds)):-
    Cnj = tree(r(cnj,p(VAR)),CnjDs),
    member(Cnj,Ds),
    var(VAR),
    Hd = tree(r(hd,adt_lex(_,_,_,verb,Atts)),[]),
    member(Hd,CnjDs),
    member(stype=declarative,Atts),
    Su = tree(r(su,_),_),
    member(Su,CnjDs),
    VAR=smain,
    debug_message(1,"treex correction: fill in cat=smain in conjunction~n",[]).

transform_rule(tree(r('--',p(conj)),Ds0),
	       tree(r('--',p(conj)),Ds)):-
    Cnj = tree(r(cnj,p(Smain)),CnjDs),
    Cnj2 = tree(r(cnj,p(smain)),CnjDs2),
    Su = tree(r(su,SuNode),SuDs),
    lists:sublist([Cnj,Cnj2],Ds0),  % cnj<cnj2 otherwise new co-indexing does not work
    Hd = tree(r(hd,adt_lex(_,L,_,verb,Atts)),[]),
    member(Hd,CnjDs2),
    \+ member(stype=imparative,Atts),
    \+ ignore_lemma(L),
    Hd2 = tree(r(hd,adt_lex(_,L2,_,verb,_)),[]),
    member(Hd2,CnjDs),
    \+ ignore_lemma(L2),
    nonvar(Smain), Smain=smain,
    \+ member(Su,CnjDs2),
    member(Su,CnjDs),
    (   \+ SuNode = i(_),
	\+ SuNode = i(_,_),
	NewSuNode = i(Index,SuNode)
    ;   SuNode = i(Index),
	NewSuNode = i(Index)
    ;   SuNode = i(Index,SuNodeRest),
	NewSuNode = i(Index,SuNodeRest)
    ),	    
    Su2 = tree(r(su,NewSuNode),SuDs),
    replace(Su,Su2,CnjDs,NCnjDs),
    NCnj = tree(r(cnj,p(smain)),NCnjDs),
    replace(Cnj,NCnj,Ds0,Ds1),
    NCnj2 = tree(r(cnj,p(smain)),[tree(r(su,i(Index)),[])|CnjDs2]),
    replace(Cnj2,NCnj2,Ds1,Ds),
    debug_message(1,"treex correction: add su coindexing in conjunction of smain + sv1~n",[]).

transform_rule(tree(r('--',p(Var)),Ds),
	       tree(r('--',p(whq)),Ds)):-
    var(Var),
    Hd = tree(r(whd,_),_),
    member(Hd,Ds),
    Body = tree(r(body,p(sv1)),_),
    member(Body,Ds),
    debug_message(1,"treex correction: fill in cat=whq~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Predc = tree(r(predc,A),B),
    findall(_,member(Predc,Ds0),[_,_|_]),
    Su = tree(r(su,_),_),
    \+ member(Su,Ds0),
    Su2 = tree(r(su,A),B),
    replace(Predc,Su2,Ds0,Ds),
    debug_message(1,"treex correction: 2x predc, one is su~n",[]).

transform_rule(tree(Node,Ds),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,Word,_,_,_)),[]),
    member(Hd,Ds),
    member(Word,[word,worden]),
    VC = tree(r(vc,p(Var)),_),
    member(VC,Ds),
    var(Var), Var = ppart,
    debug_message(1,"treex correction: fill in cat=ppart~n",[]).

transform_rule(tree(Rel,[Cmp,Body0]), tree(Rel,[Cmp,Body1])) :-
    Cmp   = tree(r(cmp,_),_),
    Body0 = tree(r(body,p(Cat)),Ds),
    Body1 = tree(r(body,p(ssub)),Ds),
    var(Cat),
    Hd = tree(r(hd,adt_lex(_,_,_,verb,E)),[]),
    member(Hd,Ds),
    member(tense=_,E),
    debug_message(1,"treex correction: fill in cat=ssub~n",[]).

transform_rule(tree(r(Rel,p(Var)),Ds),
	       tree(r(Rel,p(oti)),Ds)) :-
    var(Var), % ensure we can only do this once
    Cmp = tree(r(cmp,adt_lex(_,om,_,comp,_)),[]),
    Body = tree(r(body,_),_),
    member(Cmp,Ds),
    member(Body,Ds),
    debug_message(1,"treex correction: fill in cat=oti~n",[]).

transform_rule(tree(r(Rel,p(Var)),Ds),
	       tree(r(Rel,p(ti)),Ds)) :-
    var(Var), % ensure we can only do this once
    Cmp = tree(r(cmp,adt_lex(_,te,_,comp,_)),[]),
    Body = tree(r(body,_),_),
    member(Cmp,Ds),
    member(Body,Ds),
    debug_message(1,"treex correction: fill in cat=ti~n",[]).

transform_rule(tree(r(Rel,p(Var)),Ds),
	       tree(r(Rel,p(cp)),Ds)) :-
    var(Var), % ensure we can only do this once
    Cmp = tree(r(cmp,adt_lex(_,Als,_,comp,_)),[]),
    Body = tree(r(body,_),_),
    member(Cmp,Ds),
    member(Body,Ds),
    member(Als,[als,dat,omdat,toen,totdat,of]),
    debug_message(1,"treex correction: fill in cat=cp~n",[]).

transform_rule(tree(r(Rel,p(Ti)),Ds),
	       tree(r(Rel,p(Ti)),Ds)) :-
    nonvar(Ti), Ti == cp,
    Body = tree(r(body,p(Var)),BodyDs),
    member(Body,Ds),
    var(Var),
    Head = tree(r(hd,adt_lex(_,_,_,verb,_)),_),
    member(Head, BodyDs), % so no conjunction
    Var = ssub,
    debug_message(1,"treex correction: fill in cat=ssub~n",[]).

transform_rule(tree(r(Rel,p(Ti)),Ds),
	       tree(r(Rel,p(Ti)),Ds)) :-
    nonvar(Ti), Ti == ti,
    Body = tree(r(body,p(Var)),BodyDs),
    member(Body,Ds),
    var(Var),
    Head = tree(r(hd,_),_),
    member(Head, BodyDs), % so no conjunction
    Var = inf,
    debug_message(1,"treex correction: fill in cat=inf~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Head = tree(r(hd,_),_),
    member(Head,Ds0),
    Cmp = tree(r(cmp,N),CmpDs),
    Mod = tree(r(mod,N),CmpDs),
    replace(Cmp,Mod,Ds0,Ds),
    debug_message(1,"treex correction: cmp -> mod~n",[]). 

transform_rule(tree(Node,Ds),tree(Node,Ds)):-
    Head = tree(r(hd,adt_lex(_,Modal,_,verb,_)),_),
    member(Head,Ds),
    VC = tree(r(vc,p(Cat)),VCDs),
    member(VC,Ds),
    var(Cat),
    VCHd = tree(r(hd,adt_lex(_,_,_,verb,_)),_),
    member(VCHd,VCDs),
    Cat = inf,
    member(Modal,[willen,kunnen,mogen,moeten,laten,zullen,
		  wil,kun,mag,moet,laat,zal]),
    debug_message(1,"treex correction: fill in cat=inf for modals~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Head = tree(r(hd,adt_lex(_,Modal,_,verb,_)),_),
    member(Head,Ds0),
    member(Modal,[willen,kunnen,mogen,moeten,laten,zullen,
		  wil,kun,mag,moet,laat,zal]),
    Obj1 = tree(r(obj1,_),_),
    Obj = tree(r(obj1,_),_),
    select(Obj1,Ds0,Ds1),
    VC0 = tree(r(vc,p(inf)),VCDs),
    VC  = tree(r(vc,p(inf)),[Obj1|VCDs]),
    replace(VC0,VC,Ds1,Ds),
    \+ member(Obj,VCDs),
    VCHd = tree(r(hd,adt_lex(_,_,_,verb,_)),_),
    member(VCHd,VCDs),
    debug_message(1,"treex correction: obj1 of modal -> obj1 of vc~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Head = tree(r(hd,adt_lex(_,_,_,verb,_)),_),
    member(Head,Ds0),
    VC0 = tree(r(body,p(Cat)),VCDs),
    VC  = tree(r(  vc,p(Cat)),VCDs),
    replace(VC0,VC,Ds0,Ds),
    debug_message(1,"treex correction: verbs do not take body complements, but VC~n",[]).

%% aan_boren -> tikken op
transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Head0 = tree(r(hd,adt_lex(_,aan_boren,_,verb,Atts)),[]),
    Head  = tree(r(hd,adt_lex(_,tikken,tikken,verb,Atts)),[]),
    replace(Head0,Head,Ds0,Ds1),
    Obj1 = tree(r(obj1,_),_),
     PP  = tree(r(mod,p(pp)),[ tree(r(hd,adt_lex(_,op,op,prep,[])),[]),
			       Obj1]),
    replace(Obj1,PP,Ds1,Ds),
    debug_message(1,"treex correction: aanboren => tikken op~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Head0 = tree(r(hd,adt_lex(_,aan_boren,_,verb,Atts)),[]),
    Head  = tree(r(hd,adt_lex(_,tikken,tikken,verb,Atts)),[]),
    replace(Head0,Head,Ds0,Ds),
    debug_message(1,"treex correction: aanboren => tikken~n",[]).


transform_rule(tree(r(Rel,p(Cat)),Ds),
	       tree(r(Rel,p(Cat)),Ds)):-
    var(Cat),
    Cat = conj,
    Cnj = tree(r(cnj,_),_),
    member(Cnj,Ds),
    debug_message(1,"treex correction: fill in cat=conj~n",[]).

transform_rule(tree(r(su,p(np)),Ds0), tree(r(su,i(Index,p(np))),Ds)) :-
    Head0 = tree(r(hd,i(Index,Lex)),[]),
    Head = tree(r(hd,Lex),[]),
    replace(Head0,Head,Ds0,Ds),
    debug_message(1,"treex correction: index from head to mother node~n",[]). 

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Head = tree(r(hd,_),_),
    Mwp = tree(r(mwp,_),_),
    findall(Mwp,member(Mwp,Ds0),[Mwp1]),
    \+ member(Head,Ds0),
    Mwp1 = tree(r(mwp,NodeM),[]),
    Head1 = tree(r(hd,NodeM),[]),
    replace(Mwp1,Head1,Ds0,Ds),
    debug_message(1,"treex correction: lonely mwp is the head~n",[]). 

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Det = tree(r(det,adt_lex(A,Deze,_,num,D)),[]),
    Mod = tree(r(mod,adt_lex(A,Deze,_,num,D)),[]),
    replace(Det,Mod,Ds0,Ds),
    alpino_lex:xl(Deze,number(rang),Deze,[],[]),
    debug_message(1,"treex correction: '~w' is a mod, not a det~n",[Deze]). 

transform_rule(tree(Node,[Mod|Ds]),tree(Node,[Det|Ds])):-
    Mod = tree(r(mod,adt_lex(A,Deze,_,num,D)),[]),
    Det = tree(r(det,adt_lex(A,Deze,_,num,D)),[]),
    alpino_lex:xl(Deze,number(hoofd(_)),Deze,[],[]),
    debug_message(1,"treex correction: '~w' is a det, not a mod~n",[Deze]). 

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,_,_,Noun,_)),[]),
    member(Hd,Ds0),
    member(Noun,[noun,name]),
    Mod = tree(r(mod,adt_lex(A,Deze,_,Num,D)),[]),
    App = tree(r(app,adt_lex(A,Deze,_,num,D)),[]),
    replace(Mod,App,Ds0,Ds),
    Num == num,
    A \== ap,
    debug_message(1,"treex correction: '~w' is an app, not a mod~n",[Deze]). 

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,_,_,Noun,_)),[]),
    member(Hd,Ds0),
    member(Noun,[noun,name]),
    Mod = tree(r(mod,ModCat),ModDs),
    App = tree(r(app,ModCat),ModDs),
    replace(Mod,App,Ds0,Ds),
    ModHd = tree(r(HD,adt_lex(_,Deze,_,Num,_)),[]),
    member(ModHd,ModDs),
    Num == num,
    member(HD,[hd,cnj]),
    debug_message(1,"treex correction: phrase headed by '~w' is an app, not a mod~n",[Deze]). 

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,_,_,noun,_)),[]),
    member(Hd,Ds0),
    Mod = tree(r(mod,adt_lex(A,zowel,_,_Pron,_)),[]),
    Det = tree(r(det,adt_lex(A,beide,_,num,[])),[]),
    replace(Mod,Det,Ds0,Ds),
    debug_message(1,"treex correction: '~w' => '~w' as a det, not a mod~n",[zowel,beide]). 

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    (  Hd = tree(r(hd,adt_lex(_,_,_,noun,_)),[])
    ;  Hd = tree(r(hd,adt_lex(_,_,_,name,_)),[])
    ;  Node = r(obj1,_)
    ;  Node = r(obj2,_)
    ;  Node = r(predc,_)
    ),
    member(Hd,Ds0),
    Mod = tree(r(mod,adt_lex(A,Deze,_,_Pron,D0)),[]),
    Det = tree(r(det,adt_lex(A,Deze,_,det,D)),[]),
    replace(Mod,Det,Ds0,Ds),
    (  select(aform=_,D0,D)
    ;  D0=D
    ),
    lists:member(Deze,[jouw,veel,deze,die,dat,dit,geen,alle,meerdere,elk,ieder,dezelfde,hetzelfde,datzelfde,sommige,hoeveel,beide]),
    debug_message(1,"treex correction: '~w' is a det, not a mod~n",[Deze]). 

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    (  Hd = tree(r(hd,adt_lex(_,_,_,noun,_)),[])
    ;  Hd = tree(r(hd,adt_lex(_,_,_,name,_)),[])
    ),
    member(Hd,Ds0),
    Mod = tree(r(mod,ModCat),ModDs0),
    Det = tree(r(det,ModCat),ModDs),
    replace(Mod,Det,Ds0,Ds),
    ModHd = tree(r(hd,adt_lex(A,Deze,_,_Pron,D0)),[]),
    DetHd = tree(r(hd,adt_lex(A,Deze,_,det,D)),[]),
    replace(ModHd,DetHd,ModDs0,ModDs),
    (  select(aform=_,D0,D)
    ;  D0=D
    ),
    lists:member(Deze,[deze,die,dat,dit,geen,alle,meerdere,elk,ieder,dezelfde,hetzelfde,datzelfde,sommige,hoeveel]),
    debug_message(1,"treex correction: clause headed by ~w' is a det, not a mod~n",[Deze]). 

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,_,_,num,_)),[]),
    member(Hd,Ds0),
    NogGeen1 = tree(r(mod,_),[tree(r(mod,adt_lex(_,nog,_,_,_)),[]),
			      tree(r(hd,adt_lex(_,geen,_,_,_)),[])
			     ]),
    NogGeen2 = tree(r(mod,adt_lex(_,'nog geen','nog geen',_,[])),[]),
    replace(NogGeen1,NogGeen2,Ds0,Ds),
    debug_message(1,"treex correction: 'nog geen' is a mwu~n",[]). 

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,_,_,noun,_)),[]),
    member(Hd,Ds0),
    Mod = tree(r(mod,Cat),[MD1|MDT]),
    Det = tree(r(det,Cat),[MD1|MDT]),
    replace(Mod,Det,Ds0,Ds),
    GeenHd = tree(r(hd,adt_lex(_,Geen,_,_,_)),[]),
    lists:member(GeenHd,[MD1|MDT]),
    lists:member(Geen,[geen,soort,dezelfde]),
    debug_message(1,"treex correction: phrase headed by '~w' is a det, not a mod~n",[Geen]). 

transform_rule(tree(r(mod,p(conj)),[Een0,Of,Meer0]),
	       tree(r(det,p(conj)),[Een,Of,Meer])) :-
    Een0  = tree(r(cnj,adt_lex(_,een,_,_,_)),[]),
    Een   = tree(r(cnj,adt_lex(detp,een,een,det,[])),[]),
    Of    = tree(r(crd,_),_),
    Meer0 = tree(r(cnj,adt_lex(_,meer,_,_,_)),[]),
    Meer  = tree(r(cnj,adt_lex(ap,meer,meer,adj,[aform=compar])),[]),
    debug_message(1,"treex correction: 'een of meer' is a det, not a mod~n",[]). 

transform_rule(tree(R,Ds0),tree(R,Ds)):-
    Det1 = tree(r(det,adt_lex(_,de,_,_,_)),[]),
    Det2 = tree(r(det,adt_lex(_,_,_,_,_)),[]),
    member(Det2,Ds0),
    select(Det1,Ds0,Ds), Det1 \== Det2,
    debug_message(1,"treex correction: double determiner 'de' removed~n",[]).

transform_rule(tree(R,Ds0),tree(R,Ds)):-
    Det1 = tree(r(det,adt_lex(_,het,_,_,_)),[]),
    Det2 = tree(r(det,adt_lex(_,_,_,_,_)),[]),
    member(Det2,Ds0),
    select(Det1,Ds0,Ds), Det1 \== Det2,
    debug_message(1,"treex correction: double determiner 'het' removed~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    T1 = tree(r(Rel,i(I,_)),[W,A,B]),
    T2 = tree(r(Rel,i(I,adt_lex(_,Stem,_,name,[]))),[]),
    replace(T1,T2,Ds0,Ds),
    W = tree(r(_,adt_lex(_,Wstem,_,_,_)),[]),
    A = tree(r(_,adt_lex(_,Astem,_,_,_)),[]),
    B = tree(r(_,adt_lex(_,Bstem,_,_,_)),[]),
    trigram_name(Wstem,Astem,Bstem,Stem),
    debug_message(1,"treex correction: trigram ~w is a name trigram i 1~n",[Stem]).
    
transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    T1 = tree(r(Rel,i(I,_)),[W,A,B,RR|REST]),
    T2 = tree(r(Rel,i(I,p(np))),[tree(r(hd,adt_lex(_,Stem,_,name,[])),[]),RR|REST]),
    replace(T1,T2,Ds0,Ds),
    W = tree(r(_,adt_lex(_,Wstem,_,_,_)),[]),
    A = tree(r(_,adt_lex(_,Astem,_,_,_)),[]),
    B = tree(r(_,adt_lex(_,Bstem,_,_,_)),[]),
    trigram_name(Wstem,Astem,Bstem,Stem),
    debug_message(1,"treex correction: trigram ~w is a name trigram i 2~n",[Stem]).
    
transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    T1 = tree(r(Rel,_),[W,A,B]),
    T2 = tree(r(Rel,adt_lex(_,Stem,_,name,[])),[]),
    replace(T1,T2,Ds0,Ds),
    W = tree(r(_,adt_lex(_,Wstem,_,_,_)),[]),
    A = tree(r(_,adt_lex(_,Astem,_,_,_)),[]),
    B = tree(r(_,adt_lex(_,Bstem,_,_,_)),[]),
    trigram_name(Wstem,Astem,Bstem,Stem),
    debug_message(1,"treex correction: trigram ~w is a name trigram 1~n",[Stem]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    T1 = tree(r(Rel,_),[DET,W,A,B|REST]),
    T2 = tree(r(Rel,p(np)),[DET,tree(r(hd,adt_lex(_,Stem,_,name,[])),[])|REST]),
    replace(T1,T2,Ds0,Ds),
    W = tree(r(_,adt_lex(_,Wstem,_,_,_)),[]),
    A = tree(r(_,adt_lex(_,Astem,_,_,_)),[]),
    B = tree(r(_,adt_lex(_,Bstem,_,_,_)),[]),
    trigram_name(Wstem,Astem,Bstem,Stem),
    debug_message(1,"treex correction: trigram ~w is a name trigram 2~n",[Stem]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    T1 = tree(r(Rel,i(I,_)),[W,A,B]),
    T2 = tree(r(Rel,i(I,adt_lex(_,Stem,_,noun,Atts))),[]),
    replace(T1,T2,Ds0,Ds),
    W = tree(r(_,adt_lex(_,Wstem,_,_,_)),[]),
    A = tree(r(_,adt_lex(_,Astem,_,_,_)),[]),
    B = tree(r(_,adt_lex(_,Bstem,_,_,Atts)),[]),
    trigram_noun(Wstem,Astem,Bstem,Stem),
    debug_message(1,"treex correction: trigram ~w is a noun trigram i 1~n",[Stem]).
    
transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    T1 = tree(r(Rel,i(I,_)),[W,A,B,RR|REST]),
    T2 = tree(r(Rel,i(I,p(np))),[tree(r(hd,adt_lex(_,Stem,_,noun,Atts)),[]),RR|REST]),
    replace(T1,T2,Ds0,Ds),
    W = tree(r(_,adt_lex(_,Wstem,_,_,_)),[]),
    A = tree(r(_,adt_lex(_,Astem,_,_,_)),[]),
    B = tree(r(_,adt_lex(_,Bstem,_,_,Atts)),[]),
    trigram_noun(Wstem,Astem,Bstem,Stem),
    debug_message(1,"treex correction: trigram ~w is a noun trigram i 2~n",[Stem]).
    
transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    T1 = tree(r(Rel,_),[W,A,B]),
    T2 = tree(r(Rel,adt_lex(_,Stem,_,noun,Atts)),[]),
    replace(T1,T2,Ds0,Ds),
    W = tree(r(_,adt_lex(_,Wstem,_,_,_)),[]),
    A = tree(r(_,adt_lex(_,Astem,_,_,_)),[]),
    B = tree(r(_,adt_lex(_,Bstem,_,_,Atts)),[]),
    trigram_noun(Wstem,Astem,Bstem,Stem),
    debug_message(1,"treex correction: trigram ~w is a noun trigram 1~n",[Stem]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    T1 = tree(r(Rel,_),[DET,W,A,B|REST]),
    T2 = tree(r(Rel,p(np)),[DET,tree(r(hd,adt_lex(_,Stem,_,noun,Atts)),[])|REST]),
    replace(T1,T2,Ds0,Ds),
    W = tree(r(_,adt_lex(_,Wstem,_,_,_)),[]),
    A = tree(r(_,adt_lex(_,Astem,_,_,_)),[]),
    B = tree(r(_,adt_lex(_,Bstem,_,_,Atts)),[]),
    trigram_noun(Wstem,Astem,Bstem,Stem),
    debug_message(1,"treex correction: trigram ~w is a noun trigram 2~n",[Stem]).
    

transform_rule(tree(R,Ds0),tree(R,Ds)) :-
    Hd =  tree(r(hd,adt_lex(_,Tabblad,_,noun,_)),[]),
    member(Hd,Ds0),
    app_noun(Tabblad),
    Body =tree(r(body,BodyCat),BodyDs),
    App  =tree(r(app,BodyCat),BodyDs),
    replace(Body,App,Ds0,Ds),
    \+ member(tree(r(su,adt_lex(_,je,_,_,_)),[]), BodyDs),
    debug_message(1,"treex correction: body looks like app of ~w~n",[Tabblad]).
    

transform_rule(tree(R,Ds0),tree(R,Ds)):-
    Body = tree(r(body,p(ssub)),BodyDs0),
    Hd = tree(r(hd,adt_lex(_,_,_,noun,_)),[]),
    member(Hd,Ds0),
    Die  = tree(r(su,adt_lex(_,DieDat,_,_,_)),[]),
    Die2 = tree(r(su,i(INDEX)),[]),
    Rel  = tree(r(rhd,i(INDEX,adt_lex(_,DieDat,_,pron,[]))),[]),
    Mod  = tree(r(mod,p(rel)),[Rel,tree(r(body,p(ssub)),BodyDs)]),
    replace(Body,Mod,Ds0,Ds),
    NoCnj = tree(r(cnj,_),_),
    \+ member(NoCnj,BodyDs0),
    replace(Die,Die2,BodyDs0,BodyDs),
    member(DieDat,[die,dat]),
    debug_message(1,"treex correction: looks like a relative clause 1~n",[]).

transform_rule(tree(R,Ds0),tree(R,Ds)):-
    Body = tree(r(body,p(ssub)),BodyDs0),
    Hd = tree(r(hd,adt_lex(_,_,_,noun,_)),[]),
    member(Hd,Ds0),
    Rel  = tree(r(rhd,adt_lex(_,die,_,pron,[])),[]),
    Mod  = tree(r(mod,p(rel)),[Rel,Body]),
    replace(Body,Mod,Ds0,Ds),
    NoCnj = tree(r(cnj,_),_),
    \+ member(NoCnj,BodyDs0),
    \+ ( member(D,BodyDs0), ignore_d(D,_) ),
    debug_message(1,"treex correction: looks like a relative clause 2~n",[]).

transform_rule(tree(R,Ds0),tree(R,Ds)):-
    Ds0  = [Rel, Body],
    Rel  = tree(r(rhd,adt_lex(A,B,C,pron,Atts)),[]),
    Body = tree(r(body,p(ssub)),BodyDs),
    Ds   = [Rel2, Body2],
    Rel2 = tree(r(rhd,i(INDEX,adt_lex(A,B,C,pron,Atts))),[]),
    (   Su    = tree(r(su,_),_),
        \+ member(Su,BodyDs),
        Body2 = tree(r(body,p(ssub)),[tree(r(su,i(INDEX)),[])|BodyDs])
    ;   Obj1  = tree(r(obj1,_),_),
	\+ member(Obj1,BodyDs),
	Body2 = tree(r(body,p(ssub)),[tree(r(obj1,i(INDEX)),[])|BodyDs])
    ),
    debug_message(1,"treex correction: add co-indexing in relative clause~n",[]).

transform_rule(tree(r(Rel,p(rel)),[Rhd,Mod]),
	       tree(r(Rel,p(rel)),[Rhd,Body])) :-
    Rhd = tree(r(rhd,_),_),
    Mod = tree(r(mod,Cat),BodyDs),
    Body = tree(r(body,Cat),BodyDs),
    debug_message(1,"treex correction: relative clause: mod => body ~n",[]).

transform_rule(tree(Cat,Ds0),tree(Cat,Ds)) :-
    SU = tree(r(su,_),SuDs),
    member(SU,Ds0),
    CMP = tree(r(cmp,_),_),
    member(CMP,SuDs),
    Het0 = tree(r(su, adt_lex(A,het,C,D,E)),[]),
    Het  = tree(r(sup,adt_lex(A,het,C,D,E)),[]),
    replace(Het0,Het,Ds0,Ds),
    debug_message(1,"treex correction: first su must be sup~n",[]). 

transform_rule(tree(Cat,Ds0),tree(Cat,Ds)) :-
    SU = tree(r(su,_),SuDs),
    member(SU,Ds0),
    CMP = tree(r(cmp,_),_),
    member(CMP,SuDs),
    Het0 = tree(r(su, i(INDEX)),[]),
    Het  = tree(r(sup,i(INDEX)),[]),
    replace(Het0,Het,Ds0,Ds),
    debug_message(1,"treex correction: first su must be sup~n",[]). 

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,_,_,noun,_)),[]),
    member(Hd,Ds0),
    Mod = tree(r(mod,adt_lex(A,Deze,_,pron,_D)),[]),
    Det = tree(r(mod,adt_lex(A,Deze,_,adj,[aform=compar])),[]),
    replace(Mod,Det,Ds0,Ds),
    lists:member(Deze,[ander]),
    debug_message(1,"treex correction: '~w' is an adj, not a pron~n",[Deze]). 

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,_,_,noun,_)),[]),
    member(Hd,Ds0),
    Mod = tree(r(mod,adt_lex(A,Deze,_,adj,D0)),[]),
    Det = tree(r(mod,adt_lex(A,Deze,_,adj,[aform=compar|D])),[]),
    replace(Mod,Det,Ds0,Ds),    
    lists:member(Deze,[ander]),
    \+ member(aform=compar,D0),
    (  select(aform=base,D0,D)
    ;  D0 = D
    ),
    debug_message(1,"treex correction: '~w' needs aform=compar~n",[Deze]). 

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,_,_,noun,_)),[]),
    member(Hd,Ds0),
    Mod = tree(r(mod,adt_lex(A,Deze,_,PronAdj,_D)),[]),
    Det = tree(r(det,adt_lex(A,Deze,_,adj,[aform=compar])),[]),
    replace(Mod,Det,Ds0,Ds),
    lists:member(Deze,[meer,minder]),
    lists:member(PronAdj,[pron,adj]),
    debug_message(1,"treex correction: '~w' is a dat/adj, not a mod/~w~n",[Deze,PronAdj]). 

transform_rule(tree(r(Rel,p(conj)),Ds0),tree(r(Rel,p(conj)),Ds)) :-
    Cnj = tree(r(cnj,_),_),
    member(Cnj,Ds0),
    Crd0 = tree(r(NoCrd,adt_lex(_,zowel,_,_,_)),[]),
    Crd  = tree(r(crd,adt_lex(_,zowel,_,vg,[])),[]),
    replace(Crd0,Crd,Ds0,Ds),
    \+ NoCrd == crd,
    debug_message(1,"treex correction: 'zowel' is a crd here~n",[]).

transform_rule(tree(r(Rel,p(conj)),Ds0),
	       tree(r(Rel,p(conj)),Ds)) :-
    Crd = tree(r(crd,adt_lex(_,zowel,_,vg,_)),[]),
    member(Crd,Ds0),
    Cnj = tree(r(cnj,adt_lex(A,Als,_,vg,E)),[]),
    Crd2 = tree(r(crd,adt_lex(A,Als,_,vg,E)),[]),
    replace(Cnj,Crd2,Ds0,Ds),
    member(Als,[als,en]),
    debug_message(1,"treex correction: zowel .. als: 2x crd~n",[]). 

transform_rule(tree(r(Rel,p(conj)),Ds0),
	       tree(r(Rel,p(conj)),[En|Ds])) :-
    Cnj0 =  tree(r(cnj,_),[En0,Body0]),
    En0 =   tree(r(hd,adt_lex(A,en,C,vg,E)),[]),
    Body0 = tree(r(body,Cat),SelDs),
    replace(Cnj0,Cnj,Ds0,Ds),
    En  =   tree(r(crd,adt_lex(A,en,C,vg,E)),[]),
    Cnj =   tree(r(cnj,Cat),SelDs),
    debug_message(1,"treex correction: 'en' is a crd~n",[]). 

transform_rule(tree(r(Rel,p(conj)),Ds0),
	       tree(r(Rel,p(conj)),Ds)) :-
    Crd = tree(r(crd,adt_lex(_,zowel,_,vg,_)),[]),
    member(Crd,Ds0),
    Crd1 = tree(r(crd,adt_lex(A,en,_,vg,E)),[]),
    Crd2 = tree(r(crd,adt_lex(A,als,_,vg,E)),[]),
    replace(Crd1,Crd2,Ds0,Ds),
    debug_message(1,"treex correction: zowel .. en => zowel .. als~n",[]). 

transform_rule(tree(r(Rel,adt_lex(A,B,_,D,E0)),[]),
	       tree(r(Rel,adt_lex(A,B,_,D,E)),[])) :-
    lists:select(rnum=sg,E0,E),
    lists:member(B,[euro,gulden,jaar,uur,meter,kilo_meter,kilometer,kilo,byte,kilo_byte,mega_byte,giga_byte,tera_byte,penta_byte,terra_byte]),
    debug_message(1,"treex correction: ignore number for ~w~n",[B]). 

transform_rule(tree(r(Rel,adt_lex(A,Router,C,noun,[rnum=sg])),[]),
	       tree(r(Rel,p(np)),[tree(r(det,adt_lex(_,De,_,det,[])),[]),
				  tree(r(hd,adt_lex(A,Router,C,noun,[rnum=sg])),[])
				  ])) :-
    Rel \= hd, Rel \= app,
    missing_de(Router,De),
    debug_message(1,"treex correction: added ~w before ~w~n",[De,Router]).

transform_rule(tree(r(Rel,i(IX,adt_lex(A,Router,C,noun,[rnum=sg]))),[]),
	       tree(r(Rel,i(IX,p(np))),[tree(r(det,adt_lex(_,De,_,det,[])),[]),
				       tree(r(hd,adt_lex(A,Router,C,noun,[rnum=sg])),[])
				      ])) :-
    Rel \= hd,
    missing_de(Router,De),
    debug_message(1,"treex correction: added ~w before ~w~n",[De,Router]).

transform_rule(tree(r(Rel,p(NP)),Ds), tree(r(Rel,p(NP)),[Het|Ds]) ):-
    NP == np,
    \+ member(tree(r(det,_),_),Ds),
    member(tree(r(hd,adt_lex(_,V,_,verb,_)),[]),Ds),
    Het = tree(r(det,adt_lex(_,het,_,det,[])),[]),
    debug_message(1,"add het to nominalization with ~w~n",[V]).
    

transform_rule(tree(r(mod,p(_)),[Det,Meer]),
	       tree(r(det,adt_lex(A,meer,_,adj,[aform=compar|E])),[])) :-
    Det = tree(r(det,adt_lex(_,een,_,det,[])),[]),
    Meer = tree(r(hd,adt_lex(A,meer,_,pron,E)),[]),
    debug_message(1,"treex correction: det(een,meer) -> meer ~n",[]).

transform_rule(tree(r(Rel,p(Var)),Ds),
	       tree(r(Rel,p(conj)),Ds)) :-
    var(Var),
    Cnj = tree(r(cnj,_),_),
    member(Cnj,Ds),
    debug_message(1,"treex correction: fill in cat=conj~n",[]).

transform_rule(tree(r(Rel,p(Var)),Ds),
	       tree(r(Rel,p(pp)),Ds)) :-
    var(Var),
    Hd = tree(r(hd,adt_lex(_,_,_,prep,_)),_),
    member(Hd,Ds),
    Obj1 = tree(r(obj1,_),_),
    member(Obj1,Ds),
    debug_message(1,"treex correction: fill in cat=pp~n",[]).

transform_rule(tree(r(Rel,p(Var)),Ds),
	       tree(r(Rel,p(np)),Ds)) :-
    var(Var),
    Hd = tree(r(hd,adt_lex(_,_,_,Noun,_)),_),
    member(Hd,Ds),
    member(Noun,[noun,name]),
    Det = tree(r(det,_),_),
    member(Det,Ds),
    debug_message(1,"treex correction: fill in cat=np~n",[]).

transform_rule(tree(r(Rel,i(I,p(Var))),Ds),
	       tree(r(Rel,i(I,p(np))),Ds)) :-
    var(Var),
    Hd = tree(r(hd,adt_lex(_,_,_,Noun,_)),_),
    member(Hd,Ds),
    member(Noun,[noun,name]),
    Det = tree(r(det,_),_),
    member(Det,Ds),
    debug_message(1,"treex correction: fill in cat=np~n",[]).

transform_rule(tree(Node,Ds),tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,_,_,noun,_)),_),
    member(Hd,Ds),
    Mod = tree(r(mod,p(Cat)),ModDs),
    member(Mod,Ds),
    var(Cat),
    Rel = tree(r(rhd,_),_),
    member(Rel,ModDs),
    Cat = rel,
    debug_message(1,"treex correction: fill in cat=rel~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Mod = tree(r(mod,p(cp)),DsM),
    VC  = tree(r(vc,p(cp)),DsM),
    replace(Mod,VC,Ds0,Ds),
    Hd = tree(r(cmp,adt_lex(_,Dat,_,comp,_)),[]),
    member(Hd,DsM),
    member(Dat,[dat,of]),
    Obj = tree(r(obj1,_),_),
    \+ member(Obj,Ds),
    Predc = tree(r(predc,_),_),
    \+ member(Predc,Ds),
    debug_message(1,"treex correction: ~w-cp is vc, not mod~n",[Dat]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Mod = tree(r(mod,p(CAT)),DsM0),
    VC  = tree(r(vc,p(whsub)),DsM),
    replace(Mod,VC,Ds0,Ds),
    CAT = whsub,
    Hd1 = tree(r(rhd,Rhd),RhdDs),
    Hd2 = tree(r(whd,Rhd),RhdDs),
    replace(Hd1,Hd2,DsM0,DsM),
    \+ Rhd = i(_,adt_lex(_,die,_,_,_)),
    \+ Rhd = i(_,adt_lex(_,dat,_,_,_)),
    Obj = tree(r(obj1,_),_),
    \+ member(Obj,Ds0),
    Vc = tree(r(vc,_),_),
    \+ member(Vc,Ds0),
    Predc = tree(r(predc,_),_),
    \+ member(Predc,Ds0),
    debug_message(1,"treex correction: relative is vc, not mod~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Mod = tree(r(mod,p(cp)),DsM),
    Su  = tree(r(su,p(cp)),DsM),
    replace(Mod,Su,Ds0,Ds),
    Hd = tree(r(cmp,adt_lex(_,Dat,_,comp,_)),[]),
    member(Hd,DsM),
    member(Dat,[dat,of]),
    Predc = tree(r(predc,_),_),
    member(Predc,Ds0),
    Su = tree(r(su,_),_),
    \+ member(Su,Ds0),
    debug_message(1,"treex correction: ~w-cp is su, not mod~n",[Dat]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Zelfde,_,_,_)),[]),
    member(Hd,Ds0),
    member(Zelfde,[dezelfde,hetzelfde,datzelfde,diezelfde,ditzelfde,'zo\'n']),
    MOD = tree(r(mod,_),[Als,Body]),
    Als = tree(r(hd,adt_lex(_,als,_,_,_)),[]),
    Body = tree(r(mod,BodyCat),BodyDs),
    OBCOMP=tree(r(obcomp,p(cp)),[Als2,Body2]),
    Als2 = tree(r(cmp,adt_lex(_,als,_,comparative,[])),[]),
    Body2 =tree(r(body,BodyCat),BodyDs),
    replace(MOD,OBCOMP,Ds0,Ds),
    debug_message(1,"treex correction: ~w als is OBCOMP~n",[Zelfde]).

transform_rule(tree(r(Rel,p(Var)),Ds0), tree(r(Rel,p(sv1)),Ds)) :-
    var(Var),
    Hd = tree(r(hd,adt_lex(A,zijn,_,verb,E)),[]),
    Hd2 = tree(r(hd,adt_lex(A,zijn,_,verb,[stype=ynquestion|E])),[]),
    replace(Hd,Hd2,Ds0,Ds1),
    \+ member(stype=_,E),
    member(tense=_,E),
    Su = tree(r(su,_),_),
    \+ member(Su,Ds0),
    Predc = tree(r(predc,Cat),PredcDs),
    NewSu = tree(r(su,Cat),PredcDs),
    replace(Predc,NewSu,Ds1,Ds),
    debug_message(1,"treex correction: fill in stype=ynquestion, and predc->su~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)):-
    Hd = tree(r(hd,_),_),
    App = tree(r('{app,mod}',p(Np)),AppDs),
    App2 = tree(r(app,p(Np)),AppDs),
    (  var(Np); Np == np ),
    lists:sublist([Hd,App],Ds0),
    replace(App,App2,Ds0,Ds),
    debug_message(1,"treex correction: assuming {app,mod} is an app~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)):-
    Hd = tree(r(hd,_),_),
    App = tree(r('{app,mod}',adt_lex(A,B,_,name,E)),[]),
    App2 = tree(r(app,        adt_lex(A,B,_,name,E)),[]),
    lists:sublist([Hd,App],Ds0),
    replace(App,App2,Ds0,Ds),
    debug_message(1,"treex correction: assuming {app,mod} is an app~n",[]).

transform_rule(tree(r('{app,mod}',Cat),Ds),
	       tree(r(mod,Cat),Ds)) :-       
    debug_message(1,"treex correction: assuming {app,mod} is a mod~n",[]).

transform_rule(tree(r(Rel,p(pp)),[Hd,Obj1]),tree(r(mod,p(cp)),[Cmp,Body])) :-
    member(Rel,[mod,pc]),
    Hd = tree(r(hd,adt_lex(_,Als,_,Comparative,_)),[]),
    member(Als,[als,zoals,indien,inclusief]),
    member(Comparative,[comparative,prep,comp]),
    Cmp= tree(r(cmp,adt_lex(_,Als,_,comp,[])),[]),
    Obj1 = tree(r(obj1,Cat),Ds),
    Body = tree(r(body,Cat),Ds),
    debug_message(1,"~w is not a preposition, but a complementizer~n",[Als]).

transform_rule(tree(r(Rel,_),[D|Ds0]),tree(r(Rel,adt_lex(_,Stem,_,name,[])),[])) :-
    \+ Rel == top,
    names_and_punctuation([D|Ds0],Stems,0,1),
    hdrug_util:concat_all(Stems,Stem,' '),
    debug_message(1,"treex correction: include punctuation in name: ~w~n",[Stem]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd   = tree(r(hd,adt_lex(_,B,_,Noun,Atts)),[]),
    member(Hd,Ds0),
    member(Noun,[noun,name]),
    \+ member(rnum=pl,Atts),
    Rel0 = tree(r(mod,p(rel)),[Rhd0,Body]),
    Rel  = tree(r(mod,p(rel)),[Rhd, Body]),
    Rhd0 = tree(r(rhd,i(INDEX,adt_lex(_,Die,_,_,Atts1))),[]),
    Rhd  = tree(r(rhd,i(INDEX,adt_lex(_,Dat,_,pron,Atts1))),[]),
    replace(Rel0,Rel,Ds0,Ds),
    dehet(B,DeHet), 
    adapt_dehet_rel(DeHet,Die,Dat),
    debug_message(1,"treex correction: noun ~w with relative ~w~n",[B,Dat]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd   = tree(r(hd,adt_lex(_,B,_,Noun,Atts)),[]),
    member(Hd,Ds0),
    member(Noun,[noun,name]),
    \+ member(rnum=pl,Atts),
    Det0 = tree(r(det,adt_lex(_,D,_,det,Atts1)),[]),
    Det1 = tree(r(det,adt_lex(_,D1,_,det,Atts1)),[]),
    replace(Det0,Det1,Ds0,Ds),
    dehet(B,DeHet),
    adapt_dehet(DeHet,D,D1),
    debug_message(1,"treex correction: ~w --> ~w in context of ~w~n",[D,D1,B]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd   = tree(r(hd,adt_lex(_,_,_,_,Atts)),[]),
    member(Hd,Ds0),
    member(rnum=sg,Atts),
    Det0 = tree(r(det,adt_lex(_,D0,_,det,Atts1)),[]),
    Det1 = tree(r(det,adt_lex(_,D1,_,det,Atts1)),[]),
    replace(Det0,Det1,Ds0,Ds),
    sg_det(D0,D1),
    debug_message(1,"treex correction: ~w --> ~w in context of singular noun~n",[D0,D1]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd1 = tree(r(hd,adt_lex(A,Draai,_,D,E)),[]),
    Hd2 = tree(r(hd,adt_lex(A,SchakelIn,_,D,E)),[]),
    Svp = tree(r(svp,adt_lex(_,on,_,_,_)),[]),
    select(Svp,Ds0,Ds1),
    replace(Hd1,Hd2,Ds1,Ds),
    draai_on(Draai,SchakelIn),
    debug_message(1,"~w on ==> ~w~n",[Draai,SchakelIn]).

transform_rule(tree(R,Ds0),tree(R,Ds)):-
    Cnj = tree(r(cnj,_),[En,Slash]),
    select(Cnj,Ds0,Ds1),
    En    = tree(r(_,adt_lex(_,en,_,_,_)),[]),
    Slash = tree(r(_,adt_lex(_,'/',_,_,_)),[]),
    replace(Crd,Crd2,Ds1,Ds),
    Crd   = tree(r(crd,adt_lex(_,of,_,_,_)),[]),
    Crd2  = tree(r(crd,adt_lex(_,'en/of',_,vg,[])),[]),
    debug_message(1,"treex correction: en / + of ==> en/of~n",[]).

transform_rule(tree(r(R,_),Ds0),tree(r(R,Lex),[])) :-
    Ds0 = [tree(r(_,adt_lex(_,A,_,_,_)),[]),
	   tree(r(_,adt_lex(_,'/',_,_,_)),[]),
	   tree(r(_,adt_lex(_,B,_,Pos,Atts)),[])],
    hdrug_util:concat_all([A,'/',B],AB,''),
    Lex = adt_lex(_,AB,_,Pos,Atts),
    debug_message(1,"treex correction: ~w / ~w ==> ~w~n",[A,B,AB]).

transform_rule(tree(r(R,_),Ds0),tree(r(R,Lex),[])) :-
    Ds0 = [tree(r(_,adt_lex(_,A,_,_,_)),[]),
	   tree(_,[tree(r(_,adt_lex(_,'/',_,_,_)),[]),
		   tree(r(_,adt_lex(_,B,_,Pos,Atts)),[])])],	       
    hdrug_util:concat_all([A,'/',B],AB,''),
    Lex = adt_lex(_,AB,_,Pos,Atts),
    debug_message(1,"treex correction: ~w / ~w ==> ~w~n",[A,B,AB]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd0   = tree(r(hd,adt_lex(_,Toegang,_,noun,_)),[]),
    Hd    = tree(r(hd,adt_lex(_,Open,_,verb,[])),[]),
    replace(Hd0,Hd,Ds0,Ds1),
    \+ member(tree(r(det,adt_lex(_,de,_,_,_)),[]), Ds0),
    noun_verb(Toegang,Open),
    (   Obj1  = tree(r(obj1,_),_),
	member(Obj1,Ds0),
	Ds1 = Ds
    ;   Su    = tree(r(su,_),_),
	member(Su,Ds0),
	Ds1 = Ds
    ;   Obj1  = tree(r(mod,NP),Obj1Ds),
	Obj2  = tree(r(obj1,NP),Obj1Ds),
	replace(Obj1,Obj2,Ds1,Ds),
	(  NP == p(np)
	;  NP = p(np),
	   member(tree(r(hd,adt_lex(_,_,_,noun,_)),[]), Obj1Ds)
	)
    ;   Obj1  = tree(r(mod,CONJ),ConjDs),
	Obj2  = tree(r(obj1,CONJ),ConjDs),
	replace(Obj1,Obj2,Ds1,Ds),
	CONJ == p(conj),
	member(tree(r(cnj,NP),_),ConjDs),
	NP == p(np)
    ),
    debug_message(1,"treex correction: ~w with obj1 -> ~w~n",[Toegang,Open]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd0   = tree(r(hd,adt_lex(_,Open,_,adj,_)),[]),
    Hd    = tree(r(hd,adt_lex(_,Open2,_,verb,[])),[]),
    Obj1  = tree(r(Obj1Cat,_),_),
    member(Obj1,Ds0),
    replace(Hd0,Hd,Ds0,Ds),
    adj_verb(Open,Open2),
    member(Obj1Cat,[obj1,det]),
    debug_message(1,"treex correction: adj ~w with obj1 -> ~w verb~n",[Open,Open2]).

transform_rule(tree(r('--',Node),Ds0),tree(r('--',Node),Ds)):-
    Hd0   = tree(r(hd,adt_lex(_,gebruik,_,noun,_)),[]),
    Hd    = tree(r(hd,adt_lex(_,gebruik,_,verb,[stype=imparative])),[]),
    Obj1  = tree(r(obj1,_),_),
    member(Obj1,Ds0),
    replace(Hd0,Hd,Ds0,Ds),
    debug_message(1,"treex correction: gebruik with obj1 -> verb~n",[]).

transform_rule(tree(r(mod,Node),Ds0),tree(r(mod,Node),Ds)):-
    Hd0   = tree(r(hd,adt_lex(_,gebruik,_,noun,_)),[]),
    Hd    = tree(r(hd,adt_lex(_,'met behulp van',_,prep,[])),[]),
    Obj1  = tree(r(obj1,_),_),
    member(Obj1,Ds0), 
    replace(Hd0,Hd,Ds0,Ds),
    debug_message(1,"treex correction: gebruik with obj1 -> met behulp van~n",[]).

transform_rule(tree(r(body,Node),Ds0),tree(r(body,Node),Ds)):-
    Hd0   = tree(r(hd,adt_lex(_,gebruik,_,noun,_)),[]),
    Hd    = tree(r(hd,adt_lex(_,gebruik,_,verb,[])),[]),
    Obj1  = tree(r(obj1,_),_),
    member(Obj1,Ds0), 
    replace(Hd0,Hd,Ds0,Ds),
    debug_message(1,"treex correction: gebruik with obj1 -> verb~n",[]).

transform_rule(tree(r(Rel,p(NP)),Ds0), tree(r(Rel,p(NP)),Ds)) :-
    NP == np,
    Mod1 =  tree(r(mod,Node),ModDs0),
    Mod2 =  tree(r(mod,Node),ModDs),
    replace(Mod1,Mod2,Ds0,Ds),
    Hd0   = tree(r(hd,adt_lex(_,gebruik,_,_,_)),[]),
    Hd    = tree(r(hd,adt_lex(_,'met behulp van',_,prep,[])),[]),
    Obj1  = tree(r(obj1,_),_),
    member(Obj1,ModDs0), 
    replace(Hd0,Hd,ModDs0,ModDs),
    debug_message(1,"treex correction: gebruik with obj1 -> met behulp van~n",[]).


transform_rule(tree(r('--',p(Var)),Ds0),tree(r('--',p(sv1)),Ds)):-
    var(Var),
    Hd0   = tree(r(hd,adt_lex(_,Open,_,verb,[])),[]),
    Hd    = tree(r(hd,adt_lex(_,Open,_,verb,[stype=imparative])),[]),
    NotSu = tree(r(su,_),_),
    \+ member(NotSu,Ds0),
    replace(Hd0,Hd,Ds0,Ds),
    debug_message(1,"treex correction: ~w seems to be imparative here~n",[Open]).

transform_rule(tree(r('--',p(conj)),TDs0), tree(r('--',p(conj)),TDs)):-
    Cnj = tree(r(cnj,p(Var)),Ds0),
    Cnj2= tree(r(cnj,p(sv1)),Ds),
    replace(Cnj,Cnj2,TDs0,TDs),
    var(Var),
    Hd0   = tree(r(hd,adt_lex(_,Open,_,verb,[])),[]),
    Hd    = tree(r(hd,adt_lex(_,Open,_,verb,[stype=imparative])),[]),
    NotSu = tree(r(su,_),_),
    \+ member(NotSu,Ds0),
    replace(Hd0,Hd,Ds0,Ds),
    debug_message(1,"treex correction: ~w seems to be imparative here~n",[Open]).

transform_rule(tree(r(Rel,i(I,_)),[W,A]),tree(r(Rel,i(I,adt_lex(_,Stem,_,name,[]))),[])) :-
    W = tree(r(_,adt_lex(_,Wstem,_,_,_)),[]),
    A = tree(r(_,adt_lex(_,Astem,_,_,_)),[]),
    bigram_name(Wstem,Astem,Stem),
    debug_message(1,"treex correction: ~w is a name bigram i 1~n",[Stem]).

transform_rule(tree(r(Rel,_),[W,A]),tree(r(Rel,adt_lex(_,Stem,_,name,[])),[])) :-
    W = tree(r(_,adt_lex(_,Wstem,_,_,_)),[]),
    A = tree(r(_,adt_lex(_,Astem,_,_,_)),[]),
    bigram_name(Wstem,Astem,Stem),
    debug_message(1,"treex correction: ~w is a name bigram 1~n",[Stem]).

transform_rule(tree(R,Ds0),tree(R,Ds)):-
    W =  tree(r(_,adt_lex(_,Wstem,_,_,_)),[]),
    A =  tree(r(Rel,adt_lex(_,Astem,_,_,_)),[]),
    append(Prefix,[W,A|Suffix],Ds0),
    bigram_name(Wstem,Astem,Stem),
    Bi = tree(r(Rel,adt_lex(_,Stem,_,name,[])),[]),
    append(Prefix,[Bi|Suffix],Ds),
    debug_message(1,"treex correction: ~w is a name~n",[Stem]).

transform_rule(tree(R,Ds0),tree(R,Ds)):-
    W =  tree(r(_,adt_lex(_,Wstem,_,Wpos,Watts)),[]),
    A =  tree(r(Rel,adt_lex(_,Astem,_,Apos,Atts)),[]),
    append(Prefix,[W,A|Suffix],Ds0),
    bigram_noun(Wstem,Wpos,Watts,Astem,Apos,Atts,Stem),
    Bi = tree(r(Rel,adt_lex(_,Stem,_,noun,Atts)),[]),
    append(Prefix,[Bi|Suffix],Ds),
    debug_message(1,"treex correction: ~w ~w is the noun ~w~n",[Wstem,Astem,Stem]).

transform_rule(tree(r(Rel,Cat),Ds0),
	       tree(r(Rel,Cat),Ds)) :-
    Body0 = tree(r(body,adt_lex(A,B,C,verb,E)),[]),
    Body  = tree(r(  hd,adt_lex(A,B,C,verb,E)),[]),
      Hd0 = tree(r(  hd,adt_lex(F,G,H,noun,J)),[]),
       Hd = tree(r(obj1,adt_lex(F,G,H,noun,J)),[]),
       replace(Body0,Body,Ds0,Ds1),
       replace(Hd0,Hd,Ds1,Ds),
       Det = tree(r(det,_),_),
       \+ member(Det,Ds0),
    debug_message(1,"treex correction: body hd ==> hd obj1~n",[]).

transform_rule(tree(r(Rel,_),[W,A]),tree(r(Rel,adt_lex(_,Stem,Stem,noun,Atts)),[])) :-
    W = tree(r(obj1,adt_lex(_,Wstem,_,noun,_)),[]),
    A = tree(r(hd,  adt_lex(_,Astem0,_,noun,Atts)),[]),
    compound_part(noun,Astem0,Astem,Atts),
    hdrug_util:concat_all([Wstem,Astem],Stem,'_'),
    debug_message(1,"treex correction: ~w is a compound 1~n",[Stem]).

transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,Cat),Ds)) :-
    W = tree(r(obj1,adt_lex(_,Wstem,_,noun,_)),[]),
    A = tree(r(hd,  adt_lex(_,Astem0,_,noun,Atts)),[]),
    select(W,Ds0,Ds1),
    replace(A,tree(r(hd,adt_lex(_,Stem,Stem,noun,Atts)),[]),Ds1,Ds),
    compound_part(noun,Astem0,Astem,Atts),
    hdrug_util:concat_all([Wstem,Astem],Stem,'_'),
    debug_message(1,"treex correction: ~w is a compound 2~n",[Stem]).

transform_rule(tree(r(Rel,p(_)),Ds0),tree(r(Rel,adt_lex(_,NewH,_,noun,Atts)),[])) :-
    Hd = tree(r(hd,adt_lex(_,H,_,Noun,Atts)),[]),
    append(Prefix,[Hd],Ds0),
    member(Noun,[noun,name]),
    mod_nouns(Prefix,Stems,[H2]),
    compound_part(Noun,H,H2,Atts),
    hdrug_util:concat_all(Stems,NewH,' '),
    debug_message(1,"treex correction: ~w is a compound 3~n",[NewH]).

transform_rule(tree(r(Rel,i(INDEX,p(_))),Ds0),tree(r(Rel,i(INDEX,adt_lex(_,NewH,_,noun,Atts))),[])) :-
    Hd = tree(r(hd,adt_lex(_,H,_,Noun,Atts)),[]),
    append(Prefix,[Hd],Ds0),
    member(Noun,[noun,name]),
    mod_nouns(Prefix,Stems,[H2]),
    compound_part(Noun,H,H2,Atts),
    hdrug_util:concat_all(Stems,NewH,' '),
    debug_message(1,"treex correction: ~w is a compound 3 i~n",[NewH]).

transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,Cat),Ds)) :-
    Hd = tree(r(hd,adt_lex(_,H,_,Noun,Atts)),[]),
    append(Prefix,[Hd|Rest],Ds0),
    member(Noun,[noun,name]),
    append(Pre,Suf,Prefix),
    mod_nouns(Suf,Stems,[H2]),
    compound_part(Noun,H,H2,Atts),
    hdrug_util:concat_all(Stems,NewH,' '),
    NewHd = tree(r(hd,adt_lex(_,NewH,_,noun,Atts)),[]),
    append(Pre,[NewHd|Rest],Ds),
    debug_message(1,"treex correction: ~w is a compound 4~n",[NewH]).

transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,Cat),Ds)) :-
    Hd = tree(r(hd,adt_lex(_,H,_,Noun,Atts)),[]),
    append(Prefix,[Hd|Rest],Ds0),
    member(Noun,[noun,name]),
    append(Pre,Suf,Rest),
    mod_nouns(Pre,Stems,[]),
    compound_part(Noun,H,H2,Atts),
    hdrug_util:concat_all([H2|Stems],NewH,' '),
    NewHd = tree(r(hd,adt_lex(_,NewH,_,noun,Atts)),[]),
    append(Prefix,[NewHd|Suf],Ds),
    debug_message(1,"treex correction: ~w is a compound 5~n",[NewH]).

transform_rule(tree(r(mod,_),[W,A]),tree(r(mod,adt_lex(_,'het meest',_,adj,[aform=super])),[])) :-
    W = tree(r(_,adt_lex(_,het,_,_,_)),[]),
    A = tree(r(_,adt_lex(_,meest,_,_,_)),[]),
    debug_message(1,"treex correction: het meest is a mwu~n",[]).

transform_rule(tree(r(mod,_),[W,A]),tree(r(mod,adt_lex(_,'het minst',_,adj,[aform=super])),[])) :-
    W = tree(r(_,adt_lex(_,het,_,_,_)),[]),
    A = tree(r(_,adt_lex(_,minst,_,_,_)),[]),
    debug_message(1,"treex correction: het minst is a mwu~n",[]).

transform_rule(tree(r(Rel,p(cp)),[Als,tree(r(body,p(ssub)),BodyDs)]),
	       tree(r(Rel,p(smain)),[tree(r(mod,p(cp)),
					  [Als,
					   tree(r(body,p(ssub)),AlsDs)])|VCDS])) :-
    Als = tree(r(cmp,adt_lex(_,als,_,_,_)),[]),
    Hd = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(Hd,BodyDs),
    VC = tree(r(vc,CAT),VCDS),
    select(VC,BodyDs,AlsDs),
    CAT \== p(inf),
    Su = tree(r(su,SuCat),_),
    member(Su,VCDS),
    \+ SuCat = i(_),
    debug_message(1,"als-clause should be dependent of main clause 1~n",[]).

transform_rule(tree(r(Rel,p(cp)),[Als,tree(r(body,p(ssub)),BodyDs)]),
	       tree(r(Rel,p(sv1)),[tree(r(mod,p(cp)),
					  [Als,
					   tree(r(body,p(ssub)),AlsDs)])|VCDS])) :-
    Als = tree(r(cmp,adt_lex(_,als,_,_,_)),[]),
    Hd = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(Hd,BodyDs),
    VC = tree(r(vc,_),VCDS),
    select(VC,BodyDs,AlsDs),
    Su = tree(r(su,_),_),
    \+ member(Su,VCDS),
    Probeer = tree(r(hd,adt_lex(_,_,_,verb,Atts)),[]),
    member(Probeer,VCDS),
    member(stype=imparative,Atts),
    debug_message(1,"als-clause should be dependent of main clause 2~n",[]).

transform_rule(tree(r(Rel,_),[D|Ds]),tree(r(Rel,adt_lex(_,Root,_,Pos,Atts)),[])):-
    illegal_hd_lex_ds([D|Ds],Root,Pos,Atts),
    debug_message(1,"treex correction: fall-back: ~w is a ~w~n",[Root,Pos]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    append(Pre,Post,Ds0),
    append(Illegal,Rest,Post),
    illegal_hd_lex_ds(Illegal,Root,Pos,Atts),
    New = tree(r(hd,adt_lex(_,Root,_,Pos,Atts)),[]),
    append(Pre,[New|Rest],Ds),
    debug_message(1,"treex correction: fall-back: ~w is a ~w~n",[Root,Pos]).

transform_rule(tree(r(REL,p(np)),Ds0),
	       tree(r(REL,p(np)),Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Root,_,noun,_)),[]),
    member(Hd,Ds0),
    alpino_lex:xl(Root,noun(_,_,_,measure),Root,[],[]),
    swap(Mod,Mod1,Ds0,Ds),
    (   Mod = tree(r(mod,p(NP)),NPDs), NP == np,
	Hd2 = tree(r(hd,adt_lex(_,_,_,Noun,_)),[]),
	member(Hd2,NPDs),
	nonvar(Noun), Noun=noun
    ;   Mod = tree(r(mod,adt_lex(_,_,_,Noun,_)),[]),
    	nonvar(Noun), Noun=noun
    ),
    \+ (   Mod1 = tree(r(mod,p(np)),NPDs2),
	   Hd22 = tree(r(hd,adt_lex(_,_,_,noun,_)),[]),
	   member(Hd22,NPDs2)
       ),
    \+ Mod1 = tree(r(mod,adt_lex(_,_,_,noun,_)),[]),
    debug_message(1,"treex correction: move np modifier rightward~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Contr,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Contr,[controleren,controleer,stel_vast,vast_stellen,na_gaan,ga_na,kijk,kijken]),
    NoVc = tree(r(vc,_),_),
    \+ member(NoVc,Ds0),
    NoObj = tree(r(obj1,_),_),
    \+ member(NoObj,Ds0),
    Mod = tree(r(mod,p(cp)),ModDs0),
    member(Mod,Ds0),
    Als = tree(r(cmp,adt_lex(F,als,_,comp,G)),[]),
    Of  = tree(r(cmp,adt_lex(F,of, _, comp,G)),[]),
    replace(Als,Of,ModDs0,ModDs),
    Mod2 = tree(r(vc,p(cp)),ModDs),
    replace(Mod,Mod2,Ds0,Ds),
    debug_message(1,"treex correction: controleren als -> controleren of~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Contr,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Contr,[controleren,controleer,stel_vast,vast_stellen,na_gaan,ga_na,kijk,kijken]),
    Mod = tree(r(vc,p(cp)),ModDs0),
    member(Mod,Ds0),
    Als = tree(r(cmp,adt_lex(F,als,_,comp,G)),[]),
    Of  = tree(r(cmp,adt_lex(F,of, _, comp,G)),[]),
    replace(Als,Of,ModDs0,ModDs),
    Mod2 = tree(r(vc,p(cp)),ModDs),
    replace(Mod,Mod2,Ds0,Ds),
    debug_message(1,"treex correction: controleren als -> controleren of~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Contr,_,verb,Atts)),[]),
    member(Hd,Ds0),
    member(stype=imparative,Atts),
    member(Contr,[controleren,controleer,stel_vast,vast_stellen,na_gaan,ga_na,ijk,kijken]),
    Mod = tree(r(vc,p(cp)),ModDs0),
    member(Mod,Ds0),
    Als = tree(r(cmp,adt_lex(F,dat,_,comp,G)),[]),
    Of  = tree(r(cmp,adt_lex(F,of, _, comp,G)),[]),
    replace(Als,Of,ModDs0,ModDs),
    Mod2 = tree(r(vc,p(cp)),ModDs),
    replace(Mod,Mod2,Ds0,Ds),
    debug_message(1,"treex correction: controleren als -> controleren of~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd1 = tree(r(hd,adt_lex(_,Contr,_,verb,Atts)),[]),
    Hd2 = tree(r(hd,adt_lex(_,kijk,_,verb,Atts)),[]),
    replace(Hd1,Hd2,Ds0,Ds1),
    member(Contr,[zie,zien]),
    NoVc = tree(r(vc,_),_),
    \+ member(NoVc,Ds1),
    NoObj1 = tree(r(obj1,_),_),
    \+ member(NoObj1,Ds1),
    Mod = tree(r(mod,p(cp)),ModDs0),
    member(Mod,Ds1),
    Als = tree(r(cmp,adt_lex(F,als,_,comp,G)),[]),
    Of  = tree(r(cmp,adt_lex(F,of, _, comp,G)),[]),
    replace(Als,Of,ModDs0,ModDs),
    Mod2 = tree(r(vc,p(cp)),ModDs),
    replace(Mod,Mod2,Ds1,Ds),
    debug_message(1,"treex correction: zien als => kijken of~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd1 = tree(r(hd,adt_lex(_,Contr,_,verb,Atts)),[]),
    Hd2 = tree(r(hd,adt_lex(_,kijk,_,verb,Atts)),[]),
    replace(Hd1,Hd2,Ds0,Ds1),
    member(Contr,[zie,zien,kijk,kijken]),
    NoVc = tree(r(vc,_),_),
    \+ member(NoVc,Ds1),
    NoObj1 = tree(r(obj1,_),_),
    \+ member(NoObj1,Ds1),
    Conj0 = tree(r(MOD,p(conj)),ConjDs0),
    Conj1 = tree(r(MOD,p(conj)),ConjDs1),
    Cnj0 =  tree(r(cnj,CnjCat),CnjDs0),
    Cnj1 =  tree(r(cnj,CnjCat),CnjDs1),
    Als = tree(r(cmp,adt_lex(F,als,_,comp,G)),[]),
    Of  = tree(r(cmp,adt_lex(F,of, _, comp,G)),[]),
    replace(Conj0,Conj1,Ds1,Ds),
    replace(Cnj0,Cnj1,ConjDs0,ConjDs1),
    replace(Als,Of,CnjDs0,CnjDs1),
    member(MOD,[mod,vc]),
    debug_message(1,"treex correction: zien als => kijken of~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Mod = tree(r(mod,p(conj)),DsC),
    VC  = tree(r(vc,p(conj)),DsC),
    replace(Mod,VC,Ds0,Ds),
    Cnj = tree(r(cnj,p(cp)),DsM),
    member(Cnj,DsC),
    Hd = tree(r(cmp,adt_lex(_,Dat,_,comp,_)),[]),
    member(Hd,DsM),
    member(Dat,[dat,of]),
    Obj = tree(r(obj1,_),_),
    \+ member(Obj,Ds),
    Predc = tree(r(predc,_),_),
    \+ member(Predc,Ds),
    debug_message(1,"treex correction: ~w-cp is vc, not mod~n",[Dat]).

transform_rule(tree(R,Ds0),tree(R,Ds)):-
    Su1 = tree(r(su,adt_lex(_,_,_,_,_)),[]),
    Su2 = tree(r(su,adt_lex(_,jullie,_,_,_)),[]),
    member(Su1,Ds0),
    select(Su2,Ds0,Ds), Su1 \== Su2,
    debug_message(1,"treex correction: double subject jullie removed~n",[]).

transform_rule(tree(r(Rel,p(smain)),Ds0),
	       tree(r(Rel,p(sv1)),Ds)):-
    Su = tree(r(su,adt_lex(_,jullie,_,pron,_)),[]),
    lists:select(Su,Ds0,Ds),
    debug_message(1,"treex correction: su/jullie is an imparative...~n",[]).

transform_rule(tree(r(Rel,p(smain)),Ds0),
	       tree(r(Rel,p(sv1)),Ds)):-
    Su = tree(r(su,i(Index,adt_lex(_,jullie,_,pron,_))),[]),
    lists:select(Su,Ds0,Ds1),
    remove_index_ds(Ds1,Index,Ds),
    debug_message(1,"treex correction: su/jullie is an imparative...~n",[]).

transform_rule(tree(r(Rel,Cat),Ds0),
	       tree(r(Rel,Cat),Ds)):-
    Su = tree(r(su,_),_),
    member(Su,Ds0),
    Hd0 = tree(r(hd,adt_lex(A,B,C,verb,E0)),[]),
    Hd  = tree(r(hd,adt_lex(A,B,C,verb,E )),[]),
    replace(Hd0,Hd,Ds0,Ds),
    lists:select(stype=imparative,E0,E),
    debug_message(1,"treex correction: ignore stype=imparative in context of subject~n",[]).    

transform_rule(tree(r('--',p(Var)),Ds),
	       tree(r('--',p(sv1)),Ds)):-
    var(Var),
    Hd = tree(r(hd,adt_lex(_,_,_,verb,Atts)),[]),
    member(Hd,Ds),
    (   member(stype=ynquestion,Atts)
    ;   member(stype=imparative,Atts)
    ),
    debug_message(1,"treex correction: fill in cat=sv1~n",[]).

transform_rule(tree(r(cnj,p(Var)),Ds),
	       tree(r(cnj,p(sv1)),Ds)):-
    var(Var),
    Hd = tree(r(hd,adt_lex(_,_,_,verb,Atts)),[]),
    member(Hd,Ds),
    (   member(stype=ynquestion,Atts)
    ;   member(stype=imparative,Atts)
    ),
    debug_message(1,"treex correction: fill in cat=sv1~n",[]).

transform_rule(tree(R,Ds0), tree(R,Ds)) :-
    Ds0 = [Cnj1,Crd1,tree(r(cnj,_),[Cnj2,Crd2,Cnj3])],
    Cnj1 = tree(r(cnj,_),_),
    Crd1 = tree(r(crd,_),_),
    Crd2 = tree(r(crd,_),_),
    Cnj2 = tree(r(cnj,_),_),
    Cnj3 = tree(r(cnj,_),_),
    Ds = [Cnj1,Crd1,Cnj2,Crd2,Cnj3],
    debug_message(1,"treex correction: flatten conjunction~n",[]).

transform_rule(tree(r('--',p(conj)),[Cnj1,Crd,Cnj2]),tree(r('--',p(conj)),[Cnj1,Crd,Cnj22])) :-
    Cnj1 = tree(r(cnj,p(sv1)),Cnj1Ds),
    Crd = tree(r(crd,_),_),
    Cnj2 = tree(r(cnj,p(sv1)),Cnj2Ds),
    Cnj22 = tree(r(cnj,p(sv1)),Cnj22Ds),
    Hd = tree(r(hd,adt_lex(_,_,_,verb,Atts)),[]),
    member(Hd,Cnj1Ds),
    member(stype=Type,Atts),
    Hd2 = tree(r(hd,adt_lex(X1,X2,X3,verb,Atts2)),[]),
    Hd3 = tree(r(hd,adt_lex(X1,X2,X3,verb,[stype=Type|Atts3])),[]),
    replace(Hd2,Hd3,Cnj2Ds,Cnj22Ds),
    (   lists:select(stype=Type2,Atts2,Atts3),
	\+ Type2 = Type
    ;   lists:select(tense=_,Atts2,Atts3)
    ),
    debug_message(1,"treex correction: sv1 conjunction, agree attributes~n",[]).

transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,Cat),[Nodig|Ds])):-
    Rel \== predc,
    Hd0 = tree(r(hd,adt_lex(_,nodig,_,Adj,Atts)),[]),
    Hd  = tree(r(hd,adt_lex(_,heb,_,verb,[])),[]),
    replace(Hd0,Hd,Ds0,Ds),
    member(Adj,[adj,verb]),
    OBJ1 = tree(r(obj1,_),_),
    member(OBJ1,Ds0),
    Nodig = tree(r(predc,adt_lex(_,nodig,_,adj,Atts)),[]),
    debug_message(1,"treex correction: nodig => nodig hebben~n",[]).

transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,Cat),Ds)):-
    Rel \== predc,
    Hd0 = tree(r(hd,adt_lex(_,nodig,_,Adj,_)),[]),
    Hd  = tree(r(hd,adt_lex(_,moet,_,verb,[])),[]),
    replace(Hd0,Hd,Ds0,Ds1),
    member(Adj,[adj,verb]),
    SU = tree(r(su,_),_),
    member(SU,Ds0),
    VC0 = tree(r(mod,ModCat),VCDS),
    VC1 = tree(r(vc,ModCat),VCDS),
    replace(VC0,VC1,Ds1,Ds),
    VCHD = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(VCHD,VCDS),
    debug_message(1,"treex correction: nodig => moeten~n",[]).

transform_rule(tree(r(Rel,Cat),Ds0),tree(r(Rel,Cat),Ds)):-
    Rel \== predc,
    Hd0 = tree(r(hd,adt_lex(_,nodig,_,Adj,_)),[]),
    Hd  = tree(r(hd,adt_lex(_,moet,_,verb,[])),[]),
    replace(Hd0,Hd,Ds0,Ds1),
    member(Adj,[adj,verb]),
    SU = tree(r(su,_),_),
    member(SU,Ds0),
    VC0 = tree(r(mod,ModCat),VCDS),
    VC1 = tree(r(vc,ModCat),VCDS),
    replace(VC0,VC1,Ds1,Ds),
    VCHD = tree(r(cnj,_),_),
    member(VCHD,VCDS),
    debug_message(1,"treex correction: nodig => moeten~n",[]).

transform_rule(tree(R,Ds0),tree(R,[Opnieuw|Ds])):-
    Hd0 = tree(r(hd,adt_lex(_,L1,_,_,Atts)),[]),
    Hd  = tree(r(hd,adt_lex(_,L2,_,verb,Atts)),[]),
    replace(Hd0,Hd,Ds0,Ds),
    re_lemma(L1,L2),
    Opnieuw = tree(r(mod,adt_lex(advp,opnieuw,opnieuw,adv,[])),[]),
    debug_message(1,"treex correction: ~w => opnieuw ~w~n",[L1,L2]).

transform_rule(tree(R,Ds0),tree(R,Ds)):-
    A = tree(r(hd,adt_lex(_,maken,_,verb,Atts)),[]),
    B = tree(_,[Zorg,Body]),
    append(Prefix,[A,B],Ds0),
    append(Prefix,[C,D],Ds),
    Zorg = tree(r(hd,adt_lex(_,zorgen,_,_,_)),[]),
    Body = tree(r(vc,_),VCDS),
    Verb = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(Verb,VCDS),
    C = tree(r(hd,adt_lex(_,controleer,_,verb,Atts)),[]),
    D = tree(r(vc,p(cp)),[Of,tree(r(body,_),VCDS)]),
    Of = tree(r(cmp,adt_lex(_,of,_,comp,[])),[]),
    debug_message(1,"treex correction: 'maak zorgen' ==> 'controleer of'~n",[]).

transform_rule(tree(R,[A,B]),tree(R,[C,D])):-
    A = tree(r(hd,adt_lex(_,maken,_,verb,Atts)),[]),
    B = tree(_,[Zorg,Body]),
    Zorg = tree(r(hd,adt_lex(_,zorgen,_,_,_)),[]),
    Body = tree(r(vc,VCCAT),VCDS),
    C = tree(r(hd,adt_lex(_,controleer,_,verb,Atts)),[]),
    D = tree(r(vc,VCCAT),VCDS),
    debug_message(1,"treex correction: 'maak zorgen' ==> 'controleer'~n",[]).

transform_rule(tree(R,Ds0),tree(R,Ds)):-
    Hd0 = tree(r(hd,adt_lex(_,Lopen,_,Pos,Atts)),[]),
    Hd1 = tree(r(hd,adt_lex(_,Voeren,_,verb,Atts)),[]),
    replace(Hd0,Hd1,Ds0,Ds),
    verb_obj1(Lopen,Pos,Voeren),
    Obj1 = tree(r(obj1,_),_),
    member(Obj1,Ds0),
    debug_message(1,"treex correction: '~w' + OBJ1 => ~w~n",[Lopen,Voeren]).

transform_rule(tree(r(Rel,p(SV1)),[Hd,Obj1]), tree(r(Rel,p(conj)),Ds)) :-
    Hd   = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    Obj1 = tree(r(O,_),[A,And,B]), member(O,[obj1,mod]),
    A    = tree(r(cnj,Acat),Ads),
    And  = tree(r(crd,_),_),
    B    = tree(r(cnj,_),Bds),
    Hd2   = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(Hd2,Bds),
    Ds   = [tree(r(cnj,p(SV1)),[Hd,tree(r(O,Acat),Ads)]),
	    And,B],
    debug_message(1,"treex correction: V [ OBJ1 and VP] ==> [V OBJ1] AND VP~n",[]).

transform_rule(tree(r(Rel,p(SV1)),[Hd,Obj1]), tree(r(Rel,p(conj)),Ds)) :-
    Hd   = tree(r(hd,adt_lex(_,_,_,noun,_)),[]),
    Obj1 = tree(r(obj1,_),[A,And,B]),
    A    = tree(r(cnj,Acat),Ads),
    And  = tree(r(crd,_),_),
    B    = tree(r(cnj,_),Bds),
    Hd2  = tree(r(hd,adt_lex(_,_,_,noun,_)),[]),
    member(Hd2,Bds),
    member(tree(r(obj1,_),_),Bds),
    Ds   = [tree(r(cnj,p(SV1)),[Hd,tree(r(obj1,Acat),Ads)]),
	    And,B],
    debug_message(1,"treex correction: N [ OBJ1 and [N OBJ]] ==> [N OBJ1] AND [N OBJ]~n",[]).

transform_rule(tree(r(Rel,p(SV1)),[MOD,Hd,Obj1]), tree(r(Rel,p(conj)),Ds)) :-
    Hd   = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    Obj1 = tree(r(vc,_),[A,And,B]), 
    A    = tree(r(cnj,Acat),Ads),
    And  = tree(r(crd,_),_),
    B    = tree(r(cnj,_),Bds),
    Hd2  = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    MOD  = tree(r(mod,_),_),
    member(Hd2,Bds),
    Ds   = [tree(r(cnj,p(SV1)),[MOD,Hd,tree(r(vc,Acat),Ads)]),
	    And,B],
    debug_message(1,"treex correction: MOD V [ VC and VP] ==> [MOD V VC] AND VP~n",[]).

transform_rule(tree(r(Rel,p(SV1)),[Hd,Obj1]), tree(r(Rel,p(conj)),Ds)) :-
    Hd   = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    Obj1 = tree(r(vc,_),[A,And,B]), 
    A    = tree(r(cnj,Acat),Ads),
    And  = tree(r(crd,_),_),
    B    = tree(r(cnj,_),Bds),
    Hd2  = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(Hd2,Bds),
    Ds   = [tree(r(cnj,p(SV1)),[Hd,tree(r(vc,Acat),Ads)]),
	    And,B],
    debug_message(1,"treex correction: V [ VC and VP] ==> [V VC] AND VP~n",[]).

transform_rule(tree(r(Rel,p(sv1)),Ds0),tree(r(Rel,p(sv1)),Ds)):-
    Hd   = tree(r(hd,adt_lex(_,Gaan,_,verb,Atts)),[]),
    member(Hd,Ds0),
    member(stype=imparative,Atts),
    member(Gaan,[ga,gaan]),
    Obj1 = tree(r(obj1,_),_),
    Mod  = tree(r(mod,p(pp)),[Naar,Obj1]),
    Naar = tree(r(hd,adt_lex(_,naar,_,prep,[])),[]),
    \+ member(Mod,Ds0),
    replace(Obj1,Mod,Ds0,Ds),
    debug_message(1,"treex correction: ga + OBJ1 ==> ga naar OBJ1~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd   = tree(r(hd,adt_lex(_,ga,_,verb,_)),[]),
    member(Hd,Ds0),
    Obj1 = tree(r(mod,Obj1Cat),Obj1Ds),
    Mod  = tree(r(mod,p(pp)),[Naar,tree(r(obj1,Obj1Cat),Obj1Ds)]),
    Naar = tree(r(hd,adt_lex(_,naar,_,prep,[])),[]),
    \+ member(Mod,Ds0),
    replace(Obj1,Mod,Ds0,Ds),
    (   Noun = tree(r(hd,adt_lex(_,_,_,Tag,_)),[]),
	member(Noun,Obj1Ds), member(Tag,[noun,name])
    ;   Obj1Cat = adt_lex(_,_,_,Tag,_), member(Tag,[noun,name])
    ),	
    debug_message(1,"treex correction: ga + MOD ==> ga naar OBJ1~n",[]).

transform_rule(tree(Node,[Te,tree(r(mod,ModCat),ModDs)]),
	       tree(Node,[Te,tree(r(body,ModCat),ModDs)])) :-
    Te = tree(r(cmp,adt_lex(_,te,_,comp,_)),[]),
    debug_message(1,"treex correction: te + mod => te + body~n",[]).

transform_rule(tree(Node,[Te,tree(r(body,ModCat),ModDs0)]),
	       tree(Node,[Te,tree(r(body,ModCat),ModDs)])) :-
    Te = tree(r(cmp,adt_lex(_,te,_,comp,_)),[]),
    Hd0 = tree(r(hd,adt_lex(_,Verbinding,_,Noun,_)),[]),
    Hd  = tree(r(hd,adt_lex(_,Verbind,_,verb,[])),[]),
    replace(Hd0,Hd,ModDs0,ModDs),
    \+ Noun == verb,
    noun_verb(Verbinding,Verbind),
    debug_message(1,"treex correction: ~w is a verb here: ~w~n",[Verbinding,Verbind]).

transform_rule(tree(r(Rel,Cat),Ds0),
	       tree(r(Rel,p(conj)),
		    [tree(r(cnj,Cat),Ds2),
		     Crd,
		     Cnj2])) :- 
    Obj1 = tree(r(obj1,p(conj)),[Cnj1,Crd,Cnj2]),
    Cnj1 = tree(r(cnj,Cnj1Cat),Cnj1Ds),
    Cnj2 = tree(r(cnj,_),Cnj2Ds),
    Crd  = tree(r(crd,_),_),
    append(Ds1,[Obj1],Ds0),
    append(Ds1,[tree(r(obj1,Cnj1Cat),Cnj1Ds)],Ds2),
    Verb = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(Verb,Cnj2Ds),
    debug_message(1,"treex correction: promote coordination headed by verb out of obj1~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,[PC|Ds])):-
    Hd0 = tree(r(hd,adt_lex(_,contact,_,noun,_)),[]),
    Obj0= tree(r(obj1,_),_),
    Hd  = tree(r(hd,adt_lex(_,neem_op,_,verb,[])),[]),
    Obj1= tree(r(obj1,adt_lex(_,contact,_,noun,[])),[]),
    PC  = tree(r(pc,p(pp)),[Met,Obj0]),
    Met = tree(r(hd,adt_lex(_,met,_,prep,[])),[]),
    replace(Hd0,Hd,Ds0,Ds1),
    replace(Obj0,Obj1,Ds1,Ds),
    debug_message(1,"treex correction: contact opnemen met~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd =   tree(r(hd,adt_lex(_,B,_,Noun,_)),[]),
    member(Hd,Ds0),
    (  member(Noun,[noun,name])
    ;  Node = r(_,p(NP)), NP == np
    ),
    Obj1 = tree(r(obj1,Obj1Cat),Obj1Ds),
    Mod  = tree(r(mod,p(pp)),[Van,tree(r(obj1,Obj1Cat),Obj1Ds)]),
    Van  = tree(r(hd,adt_lex(_,van,_,prep,[])),[]),
    select(Obj1,Ds0,Ds1),
    append(Ds1,[Mod],Ds), % place behind any potential relative clauses
    debug_message(1,"treex correction: ~w + OBJ1 ==> ~w van OBJ1~n",[B,B]).

%% fout: 64
transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Obj1 = tree(r(mod,Obj1Cat),Obj1Ds),
    Mod  = tree(r(mod,p(pp)),[Van,tree(r(obj1,Obj1Cat),Obj1Ds)]),
    Van  = tree(r(hd,adt_lex(_,van,_,prep,[])),[]),
    select(Obj1,Ds0,Ds1),
    (   (   Obj1Cat==p(np)
	;   member(tree(r(det,_),_),Obj1Ds)
	),
	\+ member(tree(r(hd,adt_lex(_,keer,_,_,_)),[]), Obj1Ds)  % other mod/tmp nouns too?
    ;	Obj1Cat=adt_lex(_,Lemma,_,Noun,_),
	Noun == noun,
	\+ member(Lemma,[er,daar,keer]),                         % other mod/tmp nouns too?
	\+ name(Obj1Cat)
    ),
    append(Ds1,[Mod],Ds), % place behind any potential relative clauses
    debug_message(1,"treex correction: mod NP ==> van NP~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Your = tree(r(predc,adt_lex(_,Jouw,_,noun,_)),[]),
    Mod  = tree(r(mod,p(pp)),[Van,tree(r(obj1,adt_lex(_,jou,_,pron,[])),[])]),
    Van  = tree(r(hd,adt_lex(_,van,_,prep,[])),[]),
    replace(Your,Mod,Ds0,Ds),
    member(Jouw,[your,jouw]),
    debug_message(1,"treex correction: ~w ==> van jou~n",[Jouw]).

transform_rule(tree(Node,Ds0), tree(Node,Ds) ):-
    Int = tree(r(mod,adt_lex(_,Te,_,_,_)),_),
    select(Int,Ds0,Ds1),
    member(Te,[te,heel,hoogst]),
    append(Ds1,[Int],Ds),
    \+ Ds0 = Ds,
    debug_message(1,"reorder int_adv modifiers~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd =   tree(r(hd,adt_lex(_,B,_,verb,_)),[]),
    member(Hd,Ds0),
    MOD =  tree(r(mod,ModCat),ModDs),
    Obj1=  tree(r(obj1,ModCat),ModDs),
    member(MOD,Ds0),
    (  Noun = tree(r(hd,adt_lex(_,NounRoot,_,noun,_)),[]),
       member(Noun,ModDs)
    ;  ModCat = adt_lex(_,NounRoot,_,noun,_)
    ),
    NoObj1 = tree(r(obj1,_),_),
    \+ member(NoObj1,Ds0),
    replace(MOD,Obj1,Ds0,Ds),
    alpino_lex:un_is_verb_lemma(B,B1),
    \+ alpino_penalties:corpus_frequency_lookup(dep35(NounRoot,noun,hd/mod,verb,B1),_),
    alpino_penalties:corpus_frequency_lookup(dep35(NounRoot,noun,hd/obj1,verb,B1),_),
    debug_message(1,"treex correction: ~w + np mod looks like OBJ1~n",[B]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd   = tree(r(hd,adt_lex(_,B,_,noun,_)),[]),
    member(Hd,Ds0),
    Mod  = tree(r(mod,adt_lex(_,M,_,_,_)),[]),
    Mod2 = tree(r(mod,adt_lex(_,N,_,Pos,Atts)),[]),
    replace(Mod,Mod2,Ds0,Ds),
    noun_mod(B,M,N,Pos,Atts),
    debug_message(1,"treex correction: ~w => ~w as modifier of ~w~n",[M,N,B]).

transform_rule(tree(N,X0), tree(N,X)) :-
    Het = tree(r(su,i(Index,adt_lex(_,het,_,_,_))),[]),
    member(Het,X0), 
    Het2= tree(r(obj1,i(Index)),[]),
    Het3= tree(r(pobj1,i(Index)),[]),
    replace(tree(r(vc,CAT),Ds0),tree(r(vc,CAT),Ds),X0,X),
    Hd = tree(r(hd,adt_lex(_,Zeg,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Zeg,[zeg,zeggen,raad_aan,aan_raden,adviseer,adviseren,vertel,vertellen]),
    VC = tree(r(vc,_),_),
    member(VC,Ds0),
    replace(Het2,Het3,Ds0,Ds),
    debug_message(1,"treex correction: ~w indexed Het OBJ1 VC => Het POBJ1 VC~n",[Zeg]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,Zeg,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Zeg,[zeg,zeggen,raad_aan,aan_raden,adviseer,adviseren,vertel,vertellen]),
    VC = tree(r(vc,_),_),
    member(VC,Ds0),
    OBJ1 = tree(r(obj1,adt_lex(A,het,C,D,E)),[]),
    OBJ2 = tree(r(pobj1,adt_lex(A,het,C,D,E)),[]),
    \+ member(OBJ2,Ds0),
    replace(OBJ1,OBJ2,Ds0,Ds),
    debug_message(1,"treex correction: ~w OBJ1 VC => POBJ1 VC~n",[Zeg]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,Zeg,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Zeg,[zeg,zeggen,raad_aan,aan_raden,adviseer,adviseren,vertel,vertellen]),
    VC = tree(r(vc,_),_),
    member(VC,Ds0),
    OBJ1 = tree(r(obj1,Obj1Cat),Obj1Ds),
    OBJ2 = tree(r(obj2,Obj1Cat),Obj1Ds),
    \+ member(OBJ2,Ds0),
    replace(OBJ1,OBJ2,Ds0,Ds),
    debug_message(1,"treex correction: ~w OBJ1 VC => OBJ2 VC~n",[Zeg]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    TI = tree(r(vc,p(TiCat)),_),
    member(TI,Ds0), 
    TiCat == oti,
    OTI = tree(r(vc,p(CP)),OTIDS0),
    MOD = tree(r(mod,p(CP)),OTIDS),
    replace(OTI,MOD,Ds0,Ds),
    CP == cp,
    OF  = tree(r(cmp,adt_lex(_,of,_,_,_)),[]),
    ALS = tree(r(cmp,adt_lex(_,als,_,_,[])),[]),
    replace(OF,ALS,OTIDS0,OTIDS),
    debug_message(1,"treex correction: 2x VC of om => als om~n",[]).

transform_rule(tree(r(Node,p(conj)),Ds0),tree(r(Node,p(conj)),Ds)) :-
    Cnj = tree(r(cnj,CnjCat),CnjDs),
    Mod = tree(r(mod,_ModCat),_ModDs),
    append(Prefix,[Mod,Cnj|Suffix],Ds0),
    append(Prefix,[Cnj2|Suffix],Ds),
    (   CnjDs = [_|_],
	Cnj2 = tree(r(cnj,CnjCat),[Mod|CnjDs])
    ;   CnjDs = [],
	Cnj2 = tree(r(cnj,p(_)),[Mod,tree(r(hd,CnjCat),[])])
    ),
    debug_message(1,"treex correction: conjunction with mod: put it in cnj~n",[]).

transform_rule(tree(r(Node,p(conj)),Ds0),tree(r(Node,p(conj)),Ds)) :-
    Cnj = tree(r(cnj,CnjCat),CnjDs),
    Mod = tree(r(mod,_ModCat),_ModDs),
    append(Prefix,[Cnj,Mod|Suffix],Ds0),
    append(Prefix,[Cnj2|Suffix],Ds),
    (   CnjDs = [_|_],
	append(CnjDs,[Mod],NewDs),
	Cnj2 = tree(r(cnj,CnjCat),NewDs)
    ;   CnjDs = [],
	Cnj2 = tree(r(cnj,p(_)),[tree(r(hd,CnjCat),[]),Mod])
    ),
    debug_message(1,"treex correction: conjunction with mod: put it in cnj~n",[]).

transform_rule(tree(Node,[Su,Cnj1,Crd,Cnj2|Cnjs]), tree(Node,[Cnj11,Crd,Cnj22|Cnjs])):-
    Crd  = tree(r(crd,_),_),
    Cnj1 = tree(r(cnj,Cat1),[D1|Ds1]), Cnj11 = tree(r(cnj,Cat1),[Su,D1|Ds1]),
    Cnj2 = tree(r(cnj,Cat2),[D2|Ds2]), Cnj22 = tree(r(cnj,Cat2),[Su,D2|Ds2]),
    (   Su   = tree(r(su,adt_lex(_,_,_,_,_)),_)
    ;   Su   = tree(r(su,i(_,adt_lex(_,_,_,_,_))),_)
    ),	
    NotSu = tree(r(su,_),_),
    \+ member(NotSu,Ds2),
    debug_message(1,"treex correction: looks like su should be lowered in cnj (1)~n",[]).

transform_rule(tree(Node,[Su,Cnj1,Cnj2,Crd,Cnj3]), tree(Node,[Cnj11,Cnj22,Crd,Cnj33])):-
    Crd  = tree(r(crd,_),_),
    Cnj1 = tree(r(cnj,Cat1),[D1|Ds1]), Cnj11 = tree(r(cnj,Cat1),[Su,D1|Ds1]),
    Cnj2 = tree(r(cnj,Cat2),[D2|Ds2]), Cnj22 = tree(r(cnj,Cat2),[Su,D2|Ds2]),
    Cnj3 = tree(r(cnj,Cat3),[D3|Ds3]), Cnj33 = tree(r(cnj,Cat3),[Su,D3|Ds3]),
    Su   = tree(r(su,adt_lex(_,_,_,_,_)),_),
    NotSu = tree(r(su,_),_),
    \+ member(NotSu,Ds2),
    \+ member(NotSu,Ds3),
    debug_message(1,"treex correction: looks like su should be lowered in cnj (2)~n",[]).

transform_rule(tree(Node,[Su,Cnj1,Crd,Cnj2]), tree(Node,[Cnj11,Crd,Cnj2])):-
    Crd  = tree(r(crd,_),_),
    Cnj1 = tree(r(cnj,Cat1),[D1|Ds1]), Cnj11 = tree(r(cnj,Cat1),[Su,D1|Ds1]),
    Cnj2 = tree(r(cnj,_),Ds2), 
    Su   = tree(r(su,adt_lex(_,_,_,_,_)),_),
    NotSu = tree(r(su,_),_),
    member(NotSu,Ds2),
    debug_message(1,"treex correction: looks like su should be lowered in cnj (3)~n",[]).

transform_rule(tree(Node,[Su,Crd,Cnj]), tree(Node,[SuCnj,Crd,Cnj])):-
    Crd = tree(r(crd,_),_),
    Cnj = tree(r(cnj,_),_),
    Su  = tree(r(SuRel,SuCat),SuDs),
    \+ member(SuRel,[cnj,crd]),
    SuCnj = tree(r(cnj,SuCat),SuDs),
    debug_message(1,"treex correction: looks like ~w should be cnj~n",[SuRel]).

transform_rule(tree(r(Rel,p(Conj)),[D,D2a|Ds]),tree(r(Rel,p(conj)),[D2b|Ds])):-
    Conj == conj,
    D  = tree(r(su,_),_),
    D2a= tree(r(cnj,N),DDs),
    D2b= tree(r(cnj,N),[D|DDs]),
    member(tree(r(hd,adt_lex(_,_,_,verb,_)),_),DDs),
    \+ member(tree(r(su,_),_),DDs),
    debug_message(1,"treex correction: su daughter of conj appears to be su of first conjunct~n",[]).

transform_rule(tree(r(Rel,p(Conj)),Ds0),tree(r(Rel,p(conj)),Ds)):-
    Conj == conj,
    D = tree(r(DRel,Node),DDs),
    D2= tree(r(cnj,Node),DDs),
    replace(D,D2,Ds0,Ds),
    \+ member(DRel,[crd,cnj]),
    debug_message(1,"treex correction: ~w daughter of conj -> cnj~n",[DRel]).

transform_rule(tree(r(predc,p(VAR)),Ds),tree(r(predc,p(CAT)),Ds)) :-
    var(VAR),
    Hd = tree(r(hd,adt_lex(_,A,_,Adj,_)),[]),
    member(Hd,Ds),
    Adj == adj,
    NoDet = tree(r(det,_),_),
    \+ member(NoDet,Ds),
    a_cat(A,CAT),
    debug_message(1,"treex correction: fill in cat=~w for predc headed by adj~n",[CAT]).

transform_rule(tree(r(predc,adt_lex(VAR,A,B,C,D)),[]),tree(r(predc,adt_lex(VAR,A,B,C,D)),[])) :-
    var(VAR),
    a_cat(A,VAR),
    debug_message(1,"treex correction: fill in cat=~w for predc headed by ~w~n",[VAR,A]).

transform_rule(tree(Node,Ds0), tree(Node,[Het,Hd,Predc])):-
    Ds0 = [_,_],
    Su = tree(r(su,Cat),CatDs),
    Hd = tree(r(hd,adt_lex(_,ben,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Su,Ds0),
    Predc = tree(r(predc,Cat),CatDs),
    (   Cat = adt_lex(_,het,_,_,_)
    ->  Het = tree(r(su,adt_lex(_,dat,_,_,[])),[])
    ;   Het = tree(r(su,adt_lex(_,het,_,_,[])),[])
    ),
    debug_message(1,"treex correction: 'X is.' --> 'Het is X.' ~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hulp = tree(r(obj1,adt_lex(_,hulp,_,_,_)),[]),
    Niet = tree(r(hd,adt_lex(_,niet,_,_,_)),[]),
    Help = tree(r(hd,adt_lex(_,help,_,verb,[])),[]),
    Niet2= tree(r(mod,adt_lex(_,niet,_,adv,[])),[]),
    replace(Hulp,Help,Ds0,Ds1),
    replace(Niet,Niet2,Ds1,Ds),
    debug_message(1,"treex correction: 'hulp niet' -> 'niet helpt' ~n",[]).

transform_rule(tree(Node,[V,Mod]),tree(Node,[V,VC])):-
    V = tree(r(hd,adt_lex(_,L,_,verb,_)),[]),
    Mod = tree(r(mod,Cat),ModDs),
    VC = tree(r(vc,Cat),ModDs),
    ModV = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(ModV,ModDs),
    (   ModSu= tree(r(su,_),_),
	member(ModSu,ModDs)
    ;   ModObj1= tree(r(obj1,_),_),
	member(ModObj1,ModDs)
    ),
    debug_message(1,"treex correction: mod of ~w looks like a VC~n",[L]).
    

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd =  tree(r(hd,adt_lex(_,Kan,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Kan,[kan,mag,moet,wil]),
    App = tree(r(app,AppCat),AppDs),
    VC  = tree(r(vc, AppCat),AppDs),
    replace(App,VC,Ds0,Ds),
    Verb = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(Verb,AppDs),
    debug_message(1,"treex correction: kunnen takes VC, not APP~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd =  tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(Hd,Ds0),
    App = tree(r(app,AppCat),AppDs),
    VC  = tree(r(obj1, AppCat),AppDs),
    replace(App,VC,Ds0,Ds),
    NotObj1 = tree(r(obj1,_),_),
    \+ member(NotObj1,Ds0),
    debug_message(1,"treex correction: kunnen takes OBJ1, not APP~n",[]).

transform_rule(tree(r(Rel,p(Cat)),Ds0),tree(r(Rel,NewCat),Ds)):-
    Hd  = tree(r(hd,adt_lex(_,H,_,Noun,A)),[]),
    Mod = tree(r(mod,adt_lex(_,Punct,_,_,_)),[]),
    append(Prefix,[Hd,Mod|Suffix],Ds0),
    member(Noun,[noun,name]),
    \+ Punct = ':',
    alpino_lex:xl(Punct,punct(_),_,[],[]),
    concat_all([H,Punct],New,' '),
    (   Prefix = [], Suffix = []
    ->  NewCat = adt_lex(_,New,_,Noun,A),
	Ds = []
    ;   p(Cat) = NewCat,
	append(Prefix,[tree(r(hd,adt_lex(_,New,_,Noun,A)),[])|Suffix],Ds)
    ),
    debug_message(1,"treex correction: ~w is a compound 5~n",[New]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,_,_,noun,_)),[]),
    member(Hd,Ds0),
    SlechtGeval = tree(r(mod,p(np)),[Slecht,Geval]),
    Slecht = tree(r(mod,adt_lex(_,slecht,_,adj,[aform=super])),[]),
    Geval  = tree(r(hd, adt_lex(_,geval, _,noun,_)),[]),
    replace(SlechtGeval,Slecht,Ds0,Ds),
    debug_message(1,"treex correction: slechts geval N -> slechtste N~n",[]).
    
transform_rule(tree(r(Rel,CAT),Ds0),tree(r(Rel,CAT),Ds)):-
    (   CAT = p(np) ; CAT = i(_,p(np)) ),
    append(Prefix,[Mod,Hd|Suffix],Ds0),
    append(Prefix,[Mob,Tel2,Hd|Suffix],Ds),
    Hd  = tree(r(hd,adt_lex(_,_,_,noun,_)),_),
    Mod = tree(r(mod,_),[Mob,Tel]),
    Mob = tree(r(mod,_),_),
    Tel = tree(r(hd,adt_lex(N1,N2,N3,noun,N4)),N5),
    Tel2 = tree(r(mod,adt_lex(N1,N2,N3,noun,N4)),N5),
    debug_message(1,"treex correction: [Mod N] N => [Mod N N]~n",[]).
	       
transform_rule(tree(r(Rel,p(np)),Ds0),tree(r(Rel,p(np)),Ds)):-
    append(Prefix,[Mod,Hd|Suffix],Ds0),
    append(Prefix,[Det,Tel2,Hd|Suffix],Ds),
    Hd  = tree(r(hd,adt_lex(_,_,_,noun,_)),_),
    Mod = tree(r(mod,_),[Det,Tel]),
    Det = tree(r(det,_),_),
    Tel = tree(r(hd,adt_lex(N1,N2,N3,noun,N4)),N5),
    Tel2 = tree(r(mod,adt_lex(N1,N2,N3,noun,N4)),N5),
    debug_message(1,"treex correction: [Det N] N => [Det N N]~n",[]).

transform_rule(tree(r(Rel,p(pp)),[Op,Moment]),tree(r(Rel,p(pp)),[Op,DitMoment])):-
    Op  = tree(r(hd,adt_lex(_,op,_,prep,_)),[]),
    Moment = tree(r(obj1,adt_lex(M1,moment,M3,noun,M5)),[]),
    DitMoment = tree(r(obj1,p(np)),[Dit,Moment2]),
    Dit = tree(r(det,adt_lex(_,dit,_,det,[])),[]),
    Moment2 = tree(r(hd,adt_lex(M1,moment,M3,noun,M5)),[]),
    debug_message(1,"treex correction: 'op moment' => 'op dit moment' ~n",[]).

transform_rule(tree(r(Rel,p(np)),[Adj,One]),tree(r(Rel,p(np)),[Een,Adje])):-
    Adj = tree(r(mod,adt_lex(_,B,_,adj,E)),[]),
    One = tree(r(hd,adt_lex(_,één,_,num,_)),[]),
    Een = tree(r(det,adt_lex(_,een,_,det,[])),[]),
    Adje= tree(r(hd,adt_lex(np,B,_,adj,[rnum=sg|E])),[]),
    debug_message(1,"treex correction: ADJ one ==> a ADJ ~n",[]).

transform_rule(tree(r(mod,p(pp)),[Op,Bovenaan]),tree(r(mod,adt_lex(_,Lemma,_,adv,Atts)),[])) :-
    Op = tree(r(hd,adt_lex(_,P,_,prep,_)),[]),
    Bovenaan = tree(r(obj1,adt_lex(_,Lemma,_,adv,Atts)),[]),
    op_bovenaan(P,Lemma),
    debug_message(1,"treex correction: ~w ~w ==> ~w ~n",[P,Lemma,Lemma]).

transform_rule(tree(r(mod,p(pp)),[Op,Bovenaan]),tree(r(mod,adt_lex(_,Lemma,_,adj,Atts)),[])) :-
    Op = tree(r(hd,adt_lex(_,P,_,prep,_)),[]),
    Bovenaan = tree(r(obj1,adt_lex(_,Lemma,_,adj,Atts)),[]),
    voor_gratis(P,Lemma),
    debug_message(1,"treex correction: ~w ~w ==> ~w ~n",[P,Lemma,Lemma]).

transform_rule(tree(r(REL,p(pp)),[Prep,Het]), tree(r(REL,p(pp)),[Er,Op])) :-
    Prep = tree(r(hd,  adt_lex(_,L,_,prep,_)),[]),
    Het  = tree(r(obj1,adt_lex(_,het,_,_,_)),[]),
    Er   = tree(r(obj1,adt_lex(_,er,_,_,[])),[]),
    Op   = tree(r(hd,  adt_lex(_,L2,_,prep,[])),[]),
    (   L == tot
    ->  L2 = toe
    ;   L == met
    ->  L2 = mee
    ;   L  = L2
    ),
    debug_message(1,"treex correction: ~w het ==> er ~w~n",[L,L2]).

transform_rule(tree(r(REL,p(pp)),[Prep,Het]), tree(r(REL,adt_lex(_,Lemma,_,pp,[])),[])) :-
    Prep = tree(r(hd,  adt_lex(_,L,_,prep,_)),[]),
    Het  = tree(r(obj1,adt_lex(_,Dit,_,_,_)),[]),
    member(Dit,[dit,dat]),
    (   L == tot
    ->  L2 = toe
    ;   L == met
    ->  L2 = mee
    ;   L  = L2
    ),
    atom_concat(daar,L2,Lemma),
    debug_message(1,"treex correction: ~w ~w ==> ~w~n",[L,Dit,Lemma]).

transform_rule(tree(r(Node,p(Cat0)),Ds0),tree(r(Node,Cat),Ds)):-
    append(PRE,[Punct|POST],Ds0),
    Punct = tree(r(mod,adt_lex(_,P,_,_,_)),[]),
    lists:member(P,['-']),
    append(PRE,POST,Ds1),
    (   Ds1 = [tree(r(_,Cat2),Ds2)]
    ->  Cat = Cat2, Ds = Ds2
    ;   Cat = p(Cat0), Ds = Ds1
    ),
    debug_message(1,"treex correction: skip ~w~n",[P]).

transform_rule(tree(r(Rel,p(_)),[Adv,N]),tree(r(Rel,p(np)),[Adv2,N2])):-
    Adv = tree(r(hd, adt_lex(A,B,C,adv,E)),[]),
    Adv2= tree(r(mod,adt_lex(A,B,C,adv,E)),[]),
    N   = tree(r(mod,adt_lex(F,G,H,Noun,I)),[]),
    N2  = tree(r(hd,adt_lex(F,G,H,Noun,I)),[]),
    member(Noun,[noun,name]),
    debug_message(1,"treex correction: adv-hd noun-mod -> adv-mod noun-hd~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd0 = tree(r(hd,  adt_lex(A,weer,_,verb,Atts)),[]),
    Hd1 = tree(r(hd,  adt_lex(A,schakel_in,_,verb,Atts)),[]),
    replace(Hd0,Hd1,Ds0,Ds1),
    And = tree(r(mod, adt_lex(_,and_off,_,_,_)),[]),
    select(And,Ds1,Ds),
    debug_message(1,"treex correction: weer and-off --> schakel_in~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,  adt_lex(_,schakel_in,_,verb,_)),[]),
    member(Hd,Ds0),
    And = tree(r(mod, adt_lex(_,and_off,_,_,_)),[]),
    select(And,Ds0,Ds),
    debug_message(1,"treex correction: weer and-off --> schakel_in~n",[]).

/*transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd0 = tree(r(hd,  adt_lex(A,toon,_,verb,Atts)),[]),
    Hd1 = tree(r(hd,  adt_lex(A,sta,_,verb,Atts)),[]),
    replace(Hd0,Hd1,Ds0,Ds),
    VC = tree(r(vc,_),_),
    OBJ1 = tree(r(obj1,_),_),
    \+ member(VC,Ds0),
    \+ member(OBJ1,Ds0),
    debug_message(1,"treex correction: intransitive tonen --> staan~n",[]).
*/

transform_rule(tree(Node,Ds0),tree(Node,[PREDC|Ds])) :-
    Hd0 = tree(r(hd, adt_lex(A,overheat,_,verb,Atts)),[]),
    Hd  = tree(r(hd, adt_lex(A,raak,_,verb,Atts)),[]),
    replace(Hd0,Hd,Ds0,Ds),
    NoOBJ1 = tree(r(obj1,_),_),
    NoPREDC = tree(r(predc,_),_),
    \+ member(NoOBJ1,Ds0),
    \+ member(NoPREDC,Ds0),
    PREDC = tree(r(predc,adt_lex(_,oververhit,_,adj,[])),[]),
    debug_message(1,"treex correction: overhead => oververhit raken~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,[Hd,Rechts|Ds])) :-
    Hd0 = tree(r(hd, adt_lex(A,'right-click',_,D,E)),[]),
    Hd  = tree(r(hd, adt_lex(A,klik,_,D,E)),[]),
    select(Hd0,Ds0,Ds),
    Met = tree(r(hd, adt_lex(_,met,met,prep,[])),[]),
    De  = tree(r(det,adt_lex(_,de,de,det,[])),[]),
    RM  = tree(r(hd, adt_lex(_,rechtermuisknop,_,noun,[])),[]),
    Rechts = tree(r(mod,p(pp)),[Met,tree(r(obj1,p(np)),[De,RM])]),
    debug_message(1,"treex correction: 'right-click' ==> 'klik rechts'~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd0 = tree(r(Rel, adt_lex(A,'right-click',_,D,E)),[]), 
    Hd  = tree(r(hd, adt_lex(A,klik,_,D,E)),[]),
    VP  = tree(r(Rel, _),[Hd,Rechts]),
    replace(Hd0,VP,Ds0,Ds),
    \+ Rel = hd,
    Met = tree(r(hd, adt_lex(_,met,met,prep,[])),[]),
    De  = tree(r(det,adt_lex(_,de,de,det,[])),[]),
    RM  = tree(r(hd, adt_lex(_,rechtermuisknop,_,noun,[])),[]),
    Rechts = tree(r(mod,p(pp)),[Met,tree(r(obj1,p(np)),[De,RM])]),
    debug_message(1,"treex correction: 'right-click' ==> 'klik rechts'~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd = tree(r(hd, adt_lex(_,Stopcontact,_,_,_)),[]),
    stopcontact(Electrisch,Stopcontact),
    member(Hd,Ds0),
    Mod = tree(r(mod,adt_lex(_,Electrisch,_,_,_)),[]),
    select(Mod,Ds0,Ds),
    debug_message(1,"treex correction: ~w ~w => ~w~n",[Electrisch,Stopcontact,Stopcontact]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd0 = tree(r(hd, adt_lex(_,Sleutel,_,_,Atts)),[]),
    Mod = tree(r(mod,adt_lex(_,Kapot,_,_,_)),[]),
    sleutel(Kapot,Sleutel,Toets),
    member(Mod,Ds0),
    Hd1 = tree(r(hd, adt_lex(_,Toets,_,_,Atts)),[]),
    replace(Hd0,Hd1,Ds0,Ds),
    debug_message(1,"treex correction: ~w ~w => ~w~n",[Kapot,Sleutel,Toets]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    OnAgain = tree(r(Mod,_),[On1,Again1]),
    On1 = tree(r(hd,On),[]),
    On = adt_lex(_,on,_,_,_),
    On2 = tree(r(svp,On),[]),
    Again1 = tree(r(obj1,Ag),AgDs),
    Again2 = tree(r(mod,Ag),AgDs),
    select(OnAgain,Ds0,Ds1),
    member(Mod,[pc,mod]),
    append(Ds1,[Again2,On2],Ds),
    debug_message(1,"treex correction: on opnieuw~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,probeer,_,_,_)),[]),
    member(Hd,Ds0),
    Om = tree(r(vc,p(oti)),[OmLex,TeInf]),
    TeInf = tree(r(body,p(ti)),TeDs),
    OmLex = tree(r(cmp,adt_lex(_,om,_,_,_)),[]),
    Te = tree(r(vc,p(ti)),TeDs),
    replace(Om,Te,Ds0,Ds),
    debug_message(1,"treex correction: proberen om te => proberen te ~n",[]).
    
transform_rule(tree(r(Rel,p(Np1)),NPDS0), tree(r(Rel,p(np)),NPDS)) :-
    MOD0 = tree(r(mod,p(Np2)),NP2DS), 
    MOD  = tree(r(mod,p(pp)),[tree(r(hd,adt_lex(_,van,van,prep,[])),[]),
			      tree(r(obj1,p(np)),NP2DS)
			     ]
	       ),
    replace(MOD0,MOD,NPDS0,NPDS),
    Np1 == np, Np2 == np,
    debug_message(1,"treex correction: np mod within np gets 'van' PP~n",[]).

transform_rule(tree(r(Rel,Node),Ds0), tree(r(Rel,p(du)),[SAT,NUCL])) :-
    IfNiet = tree(_,[If,Niet]),
    (  If   = tree(r(_,adt_lex(_,if,_,_,_)),[])
    ;  If   = tree(r(_,adt_lex(_,je,_,_,_)),[])
    ),
    Niet = tree(r(_,adt_lex(_,niet,_,_,_)),[]),
    select(IfNiet,Ds0,Ds),
    SAT =  tree(r(sat,adt_lex(_,'zo niet','zo niet',_,[])),[]),
    NUCL = tree(r(nucl,Node),Ds),
    debug_message(1,"treex correction: if niet => zo niet with NUCL/SAT~n",[]).

%% als het is, already sat
transform_rule(tree(r(sat,p(cp)),[Als,HetIs]), tree(r(sat,adt_lex(_,'zo ja',_,_,[])),[])) :-
    Als = tree(r(cmp,adt_lex(_,als,_,_,_)),[]),
    HetIs = tree(r(body,p(ssub)),[Het,Is]),
    Het = tree(r(su,adt_lex(_,het,_,_,_)),[]),
    Is = tree(r(hd,adt_lex(_,zijn,_,_,_)),[]),
    debug_message(1,"treex correction: als het is => zo ja~n",[]).

%% als het niet is, already sat
transform_rule(tree(r(sat,p(cp)),[Als,HetIs]), tree(r(sat,adt_lex(_,'zo niet',_,_,[])),[])) :-
    Als = tree(r(cmp,adt_lex(_,als,_,_,_)),[]),
    HetIs = tree(r(body,p(ssub)),[Het,Niet,Is]),
    Het = tree(r(su,adt_lex(_,het,_,_,_)),[]),
    Niet = tree(r(mod,adt_lex(_,niet,_,_,_)),[]),
    Is = tree(r(hd,adt_lex(_,zijn,_,_,_)),[]),
    debug_message(1,"treex correction: als het niet is => zo niet~n",[]).

%% als het is, mod
transform_rule(tree(r(Top,Cat),Ds0),tree(r(Top,p(du)),[Sat,Nucl])) :-
    AlsHetIs = tree(r(mod,p(cp)),[Als,HetIs]),
    select(AlsHetIs,Ds0,Ds),
    Als = tree(r(cmp,adt_lex(_,als,_,_,_)),[]),
    HetIs = tree(r(body,p(ssub)),[Het,Is]),
    Het = tree(r(su,adt_lex(_,het,_,_,_)),[]),
    Is = tree(r(hd,adt_lex(_,zijn,_,_,_)),[]),
    Sat = tree(r(sat,adt_lex(_,'zo ja',_,_,[])),[]),
    Nucl = tree(r(nucl,Cat),Ds),
    debug_message(1,"treex correction: als het is => zo ja~n",[]).    

%% als het niet is, mod
transform_rule(tree(r(Top,Cat),Ds0),tree(r(Top,p(du)),[Sat,Nucl])) :-
    AlsHetNietIs = tree(r(mod,p(cp)),[Als,HetNietIs]),
    select(AlsHetNietIs,Ds0,Ds),
    Als = tree(r(cmp,adt_lex(_,als,_,_,_)),[]),
    HetNietIs = tree(r(body,p(ssub)),[Het,Niet,Is]),
    Het = tree(r(su,adt_lex(_,het,_,_,_)),[]),
    Is = tree(r(hd,adt_lex(_,zijn,_,_,_)),[]),
    Niet = tree(r(mod,adt_lex(_,niet,_,_,_)),[]),
    Sat = tree(r(sat,adt_lex(_,'zo niet',_,_,[])),[]),
    Nucl = tree(r(nucl,Cat),Ds),
    debug_message(1,"treex correction: als het is => zo ja~n",[]).    

%% als je doet, mod
transform_rule(tree(r(Top,Cat),Ds0),tree(r(Top,p(du)),[Sat,Nucl])) :-
    AlsHetNietIs = tree(r(mod,p(cp)),[Als,HetNietIs]),
    select(AlsHetNietIs,Ds0,Ds),
    Als = tree(r(cmp,adt_lex(_,als,_,_,_)),[]),
    HetNietIs = tree(r(body,p(ssub)),[Het,Is]),
    Het = tree(r(su,adt_lex(_,je,_,_,_)),[]),
    Is = tree(r(hd,adt_lex(_,doen,_,_,_)),[]),
    Sat = tree(r(sat,adt_lex(_,'zo ja',_,_,[])),[]),
    Nucl = tree(r(nucl,Cat),Ds),
    debug_message(1,"treex correction: als het is => zo ja~n",[]).    

%% als je niet doet, mod
transform_rule(tree(r(Top,Cat),Ds0),tree(r(Top,p(du)),[Sat,Nucl])) :-
    AlsHetNietIs = tree(r(mod,p(cp)),[Als,HetNietIs]),
    select(AlsHetNietIs,Ds0,Ds),
    Als = tree(r(cmp,adt_lex(_,als,_,_,_)),[]),
    HetNietIs = tree(r(body,p(ssub)),[Het,Niet,Is]),
    Het = tree(r(su,adt_lex(_,je,_,_,_)),[]),
    Is = tree(r(hd,adt_lex(_,doen,_,_,_)),[]),
    Niet = tree(r(mod,adt_lex(_,niet,_,_,_)),[]),
    Sat = tree(r(sat,adt_lex(_,'zo niet',_,_,[])),[]),
    Nucl = tree(r(nucl,Cat),Ds),
    debug_message(1,"treex correction: als het is => zo ja~n",[]).    

%% als je hebt, mod
transform_rule(tree(r(Top,Cat),Ds0),tree(r(Top,p(du)),[Sat,Nucl])) :-
    AlsHetNietIs = tree(r(mod,p(cp)),[Als,HetNietIs]),
    select(AlsHetNietIs,Ds0,Ds),
    Als = tree(r(cmp,adt_lex(_,als,_,_,_)),[]),
    HetNietIs = tree(r(body,p(ssub)),[Het,Is]),
    Het = tree(r(su,adt_lex(_,je,_,_,_)),[]),
    Is = tree(r(hd,adt_lex(_,hebben,_,_,_)),[]),
    Sat = tree(r(sat,adt_lex(_,'zo ja',_,_,[])),[]),
    Nucl = tree(r(nucl,Cat),Ds),
    debug_message(1,"treex correction: als het is => zo ja~n",[]).    

%% als je niet hebt, mod
transform_rule(tree(r(Top,Cat),Ds0),tree(r(Top,p(du)),[Sat,Nucl])) :-
    AlsHetNietIs = tree(r(mod,p(cp)),[Als,HetNietIs]),
    select(AlsHetNietIs,Ds0,Ds),
    Als = tree(r(cmp,adt_lex(_,als,_,_,_)),[]),
    HetNietIs = tree(r(body,p(ssub)),[Het,Niet,Is]),
    Het = tree(r(su,adt_lex(_,je,_,_,_)),[]),
    Is = tree(r(hd,adt_lex(_,hebben,_,_,_)),[]),
    Niet = tree(r(mod,adt_lex(_,niet,_,_,_)),[]),
    Sat = tree(r(sat,adt_lex(_,'zo niet',_,_,[])),[]),
    Nucl = tree(r(nucl,Cat),Ds),
    debug_message(1,"treex correction: als het is => zo ja~n",[]).    

%% het is, mod
transform_rule(tree(r(Top,Cat),Ds0),tree(r(Top,p(du)),[Sat,Nucl])) :-
    HetIs = tree(r(vc,p(ssub)),[Het,Is]),
    Het = tree(r(su,adt_lex(_,het,_,_,_)),[]),
    Is = tree(r(hd,adt_lex(_,zijn,_,_,_)),[]),
    select(HetIs,Ds0,Ds),
    Sat = tree(r(sat,adt_lex(_,'zo ja',_,_,[])),[]),
    Nucl = tree(r(nucl,Cat),Ds),
    debug_message(1,"treex correction: het is => zo ja~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Luk = tree(r(hd,adt_lex(_,Lemma,_,_,_)),[]),
    member(Luk,Ds0),
    member(Lemma,[misluk,luk]),
    Su = tree(r(su,adt_lex(_,het,_,_,_)),[]),
    select(Su,Ds0,Ds1),
    Ob = tree(r(obj1,ObCat),ObDs),
    Su2= tree(r(su,ObCat),ObDs),
    replace(Ob,Su2,Ds1,Ds),
    debug_message(1,"treex correction: het lukt OBJ => OBJ lukt~n",[]).

transform_rule(tree(r(Rel,p(Inf)),Ds0),tree(r(Rel,p(Inf)),Ds)) :-
    Inf == inf,
    Su = tree(r(su,adt_lex(_,het,_,_,_)),[]),
    select(Su,Ds0,Ds),
    debug_message(1,"treex correction: drop 'het' as subject in infinitive~n",[]).

transform_rule(tree(r(Rel,p(Inf)),Ds0),tree(r(Rel,p(Inf)),Ds)) :-
    Inf == inf,
    Su = tree(r(su,Cat),SuDs),
    Obj1 = tree(r(obj1,Cat),SuDs),
    \+ member(Obj1,Ds0),
    \+ member(tree(r(sup,_),_),Ds0),
    replace(Su,Obj1,Ds0,Ds),
    \+ Cat = i(_),
    \+ Cat = i(_,_),
    debug_message(1,"treex correction: guessing subject of infinitive is object~n",[]).

transform_rule(tree(Node,[Als,Nodig]), tree(Node,[Indien,Nodig])):-
    Als   = tree(r(cmp, adt_lex(Als0,als,_,Als3,Als4)),[]),
    Nodig = tree(r(body,adt_lex(_,nodig,_,_,_)),[]),
    Indien= tree(r(cmp, adt_lex(Als0,indien,_,Als3,Als4)),[]),
    debug_message(1,"treex correction: als nodig => indien nodig~n",[]).

transform_rule(tree(Node,[In,Algemeen]), tree(Node,[In,HetAlgemeen])) :-
    In       = tree(r(hd,  adt_lex(_,in,_,_,_)),[]),
    Algemeen = tree(r(obj1,adt_lex(A1,algemeen,A3,A4,A5)),[]),
    Het      = tree(r(det, adt_lex(_,het,het,det,[])),[]),
    HetAlgemeen = tree(r(obj1,p(np)),[Het,Algemeen2]),
    Algemeen2= tree(r(hd,  adt_lex(A1,algemeen,A3,A4,A5)),[]),
    debug_message(1,"treex correction: in algemeen => in het algemeen~n",[]).

transform_rule(tree(r(Rel,_),[Als,VeelTijdZoalsNodig]), tree(r(Rel,p(ap)),[Vaak,ZoAlsNodig])) :-
    (   Als = tree(r(_,adt_lex(_,als,_,_,_)),[])
    ;   Als = tree(r(_,adt_lex(_,zo,_,_,_)),[])
    ),
    (   VeelTijdZoalsNodig = tree(_,[Veel,Tijd,ZoalsNodig]),
	Veel = tree(r(_,adt_lex(_,veel,_,_,_)),[]),
	Tijd = tree(r(_,adt_lex(_,tijd,_,_,_)),[])
    ;   VeelTijdZoalsNodig = tree(_,[Vaak,ZoalsNodig]),
	Vaak = tree(r(_,adt_lex(_,vaak,_,_,_)),[])
    ),
    ZoalsNodig = tree(_,[Zoals,Nodig]),
    Zoals = tree(r(_,adt_lex(_,zoals,_,_,_)),[]),
    Nodig = tree(r(_,adt_lex(_,nodig,_,_,_)),[]),
    Vaak = tree(r(hd,adt_lex(_,vaak,vaak,adj,[])),[]),
    ZoAlsNodig = tree(r(mod,p(advp)),[Zo,AlsNodig]),
    Zo = tree(r(hd,adt_lex(advp,zo,zo,adv,[])),[]),
    AlsNodig = tree(r(obcomp,p(cp)),[Als2,Nodig2]),
    Als2 = tree(r(cmp,adt_lex(_,als,_,comparative,[])),[]),
    Nodig2 = tree(r(body,adt_lex(_,noodzakelijk,_,adj,[])),[]),
    debug_message(1,"treex correction: als veel tijd zoals nodig => zo vaak als noodzakelijk~n",[]).
    
transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    append(Prefix,[Als,VeelTijd,ZoalsNodig|Rest],Ds0),
    append(Prefix,[tree(r(mod,p(ap)),[Vaak,ZoAlsNodig])|Rest],Ds),
    (   Als = tree(r(_,adt_lex(_,als,_,_,_)),[])
    ;   Als = tree(r(_,adt_lex(_,zo,_,_,_)),[])
    ),
    (   VeelTijd = tree(_,[Veel,Tijd]),
	Veel = tree(r(_,adt_lex(_,veel,_,_,_)),[]),
	Tijd = tree(r(_,adt_lex(_,tijd,_,_,_)),[])
    ;   VeelTijd = tree(r(_,adt_lex(_,vaak,_,_,_)),[])
    ),
    ZoalsNodig = tree(_,[Zoals,Nodig]),
    Zoals = tree(r(_,adt_lex(_,zoals,_,_,_)),[]),
    Nodig = tree(r(_,adt_lex(_,nodig,_,_,_)),[]),
    Vaak = tree(r(hd,adt_lex(_,vaak,vaak,adj,[])),[]),
    ZoAlsNodig = tree(r(mod,p(advp)),[Zo,AlsNodig]),
    Zo = tree(r(hd,adt_lex(advp,zo,zo,adv,[])),[]),
    AlsNodig = tree(r(obcomp,p(cp)),[Als2,Nodig2]),
    Als2 = tree(r(cmp,adt_lex(_,als,_,comparative,[])),[]),
    Nodig2 = tree(r(body,adt_lex(_,noodzakelijk,_,adj,[])),[]),
    debug_message(1,"treex correction: als veel tijd zoals nodig => zo vaak als noodzakelijk~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds) ) :-
    Hd = tree(r(hd,adt_lex(_,_,_,noun,_)),[]),
    member(Hd,Ds0),
    Mod1 = tree(r(mod,adt_lex(_,nodig,_,adj,Atts)),[]),
    Mod2 = tree(r(mod,adt_lex(_,benodigd,_,adj,Atts)),[]),
    replace(Mod1,Mod2,Ds0,Ds),
    debug_message(1,"treex correction: nodig => benodigd~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds) ) :-
    Hd = tree(r(hd,adt_lex(_,_,_,noun,_)),[]),
    member(Hd,Ds0),
    Su = tree(r(su,_),_),
    Mod1 = tree(r(mod,Cat),ModDs0),
    Mod2 = tree(r(mod,Cat),ModDs),
    replace(Mod1,Mod2,Ds0,Ds),
    \+ member(Su,ModDs0),
    ModHd1 = tree(r(hd,adt_lex(_,nodig,_,adj,Atts)),[]),
    ModHd2 = tree(r(hd,adt_lex(_,benodigd,_,adj,Atts)),[]),
    replace(ModHd1,ModHd2,ModDs0,ModDs),
    debug_message(1,"treex correction: nodig => benodigd~n",[]).

transform_rule(tree(r(Rel,p(sv1)),Ds0), tree(r(Rel,p(conj)),Ds) ):-
    Vc = tree(r(vc,p(conj)),[Cnj1,En,Cnj2]),
    append(Mid,[Vc],Ds0),
    Cnj1 = tree(r(cnj,p(pp)),Cnj1Ds),
      En = tree(r(crd,adt_lex(_,en,_,_,_)),[]),
    Cnj2 = tree(r(cnj,p(sv1)),_),
    append(Mid,[tree(r(mod,p(pp)),Cnj1Ds)],NewDs),
    Ds = [tree(r(cnj,p(sv1)),NewDs),
	  En,
	  Cnj2
	 ],
    debug_message(1,"treex correction: [ V .. [ MOD en SV1]] ==> [ V .. MOD ] en SV1~n",[]).

transform_rule(tree(Node,[Je,Kunt|Ds]), tree(Node,[Dat,Kunt|Ds])) :-
    Je = tree(r(su,adt_lex(_,je,_,_,_)),[]),
    Kunt = tree(r(hd,adt_lex(_,kan,_,_,_)),[]),
    Dat = tree(r(su,adt_lex(_,dat,_,det,[])),[]),
    \+ member(tree(r(obj1,_),_),Ds),
    \+ member(tree(r(vc,_),_),Ds),
    debug_message(1,"treex correction: je kunt => dat kan~n",[]).

transform_rule(tree(Node,[Het0,Wordt,Body]), tree(Node,[Het,Is,PredC]) ):-
    Het0 = tree(r(su,i(I,adt_lex(A,het,C,D,E))),[]),
    Het  = tree(r(su,adt_lex(A,het,C,D,E)),[]),
    Wordt = tree(r(hd,adt_lex(_,word,_,_,_)),[]),
    Is    = tree(r(hd,adt_lex(_,ben,_,verb,[])),[]),
    Obj1 = tree(r(obj1,i(I)),[]),
    Body = tree(r(vc,_),BodyDs),
    select(Obj1,BodyDs,BodyDs1),
    Nodig = tree(r(hd,adt_lex(_,nodig,_,_,_)),[]),
    member(Nodig,BodyDs1),
    PredC = tree(r(predc,p(ap)),BodyDs1),
    debug_message(1,"treex correction: het wordt nodig => het is nodig ~n",[]).    

transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,word,_,_,_)),[]),
    member(Hd,Ds0),
    \+ member(tree(r(vc,_),_),Ds0),
    \+ member(tree(r(predc,_),_),Ds0),
    \+ member(tree(r(obj1,_),_),Ds0),
    Adj1 = tree(r(mod,p(ap)),ADs),
    Adj2 = tree(r(predc,p(ap)),ADs),
    replace(Adj1,Adj2,Ds0,Ds),
    Adj = tree(r(hd,adt_lex(_,_,_,adj,_)),[]),
    member(Adj,ADs),
    debug_message(1,"treex correction: mod of worden is predc 1~n",[]).
    
transform_rule(tree(Node,Ds0),tree(Node,Ds)):-
    Hd = tree(r(hd,adt_lex(_,word,_,_,_)),[]),
    member(Hd,Ds0),
    \+ member(tree(r(vc,_),_),Ds0),
    \+ member(tree(r(predc,_),_),Ds0),
    \+ member(tree(r(obj1,_),_),Ds0),
    Adj1 = tree(r(mod,adt_lex(A,B,C,adj,E)),[]),
    Adj2 = tree(r(predc,adt_lex(A,B,C,adj,E)),[]),
    replace(Adj1,Adj2,Ds0,Ds),
    debug_message(1,"treex correction: mod of worden is predc 2~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    PP0 = tree(r(vc,p(Cat)),PPs),
    PP1 = tree(r(mod,p(Cat)),PPs),
    replace(PP0,PP1,Ds0,Ds),
    Cat == pp,
    debug_message(1,"treex correction: pp is not VC but MOD~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Invullen,_,_,_)),[]),
    member(Hd,Ds0),
    invullen_in(Invullen,Prep),
    PP = tree(r(mod,p(pp)),[In,Obj1]),
    In = tree(r(hd,adt_lex(_,Prep,_,_,_)),[]),
    Obj1 = tree(r(obj1,_),_),
    \+ member(Obj1,Ds0),
    replace(PP,Obj1,Ds0,Ds),
    debug_message(1,"treex correction: ~w ~w X => ~w X~n",[Invullen,In,Invullen]).

transform_rule(tree(Node,Ds0),tree(Node,[SU0|Ds1])) :-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[wil,willen,kan,kunnen,mag,mogen,moet,moeten,zal,zullen]),
    \+ member(tree(r(su,_),_),Ds0),
    VC0 = tree(r(vc,VcCat),VcDs0),
    VC  = tree(r(vc,VcCat),VcDs),
    replace(VC0,VC,Ds0,Ds1),
    SU0 = tree(r(su,i(I,_)),_),
    SU  = tree(r(su,i(I)),[]),
    replace(SU0,SU,VcDs0,VcDs),
    debug_message(1,"treex correction: add su to modal i~n",[]).

transform_rule(tree(Node,Ds0),tree(Node,[tree(r(su,i(I,SuCat)),SuDs)|Ds1])) :-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Proberen,[wil,willen,kan,kunnen,mag,mogen,moet,moeten,zal,zullen]),
    \+ member(tree(r(su,_),_),Ds0),
    VC0 = tree(r(vc,VcCat),VcDs0),
    VC  = tree(r(vc,VcCat),VcDs),
    replace(VC0,VC,Ds0,Ds1),
    SU0 = tree(r(su,SuCat),SuDs),
    SU  = tree(r(su,i(I)),[]),
    replace(SU0,SU,VcDs0,VcDs),
    \+ SuCat = i(_),
    \+ SuCat = i(_,_),
    debug_message(1,"treex correction: add su to modal~n",[]).

transform_rule(tree(r('--',p(Cat)),Ds0),tree(r('--',p(du)),[D1,D2])) :-
    D1 = tree(r(nucl,p(Cat)),Ds),
    D2 = tree(r(sat,ColonArg),[]),
    select_colon_arg(Ds0,Ds,ColonArg),
    debug_message(1,"treex correction: promote colon arg~n",[]).

transform_rule(tree(Cat0,Ds0),tree(Cat,Ds)):-
    append(Pref,[Colon|Next],Ds0),
    Colon = tree(r(_,adt_lex(_,:,_,_,_)),[]),
    (   Next = []
    ;   Next = [NextTree0|_],
	\+ (  apply_transformations(NextTree0,NextTree),        % apply compounding etc
	      NextTree = tree(r(_,adt_lex(_,_,_,_,_)),[])	% use select_colon_arg in that case
	   )
    ),
    append(Pref,Next,Ds1),
    treat_unary(Ds1,Cat0,Cat,Ds),
    debug_message(1,"treex correction: ignore :~n",[]). 

transform_rule(tree(r('--',p(Cat)),Ds0),tree(r('--',p(du)),[D1,D2])) :-
    D1 = tree(r(tag,NeeArg),[]),
    D2 = tree(r(nucl,p(Cat)),Ds),
    select_tag_arg(Ds0,Ds,NeeArg),
    debug_message(1,"treex correction: promote ja/nee as tag~n",[]).

transform_rule(tree(r(Rel,p(oti)),[Om,TeDocGa]), tree(r(Rel,adt_lex(_,'Document To Go',_,name,[])),[])) :-
    Om = tree(r(cmp,adt_lex(_,om,_,_,_)),[]),
    TeDocGa = tree(r(body,p(ti)),[Te,DocGa]),
    Te = tree(r(cmp,adt_lex(_,te,_,_,_)),[]),
    DocGa= tree(r(body,p(inf)),[Doc,Ga]),
    Doc = tree(r(_,adt_lex(_,document,_,_,_)),[]),
    Ga = tree(r(_,adt_lex(_,gaan,_,_,_)),[]),
    debug_message(1,"treex correction: Document To Go~n",[]).

transform_rule(tree(r(mod,p(oti)),[Om,TeInf]),  tree(r(mod,p(cp)),[Zodat,Ssub])) :-
    Om =    tree(r(cmp,adt_lex(_,om,_,_,_)),[]),
    TeInf = tree(r(body,p(ti)),[Te,Inf]),
    Te =    tree(r(cmp,adt_lex(_,te,_,_,_)),[]),
    Inf =   tree(r(body,p(inf)),Ds),
    Su = tree(r(su,Cat),_),
    member(Su,Ds),
    \+ Cat = i(_),
    Zodat = tree(r(cmp,adt_lex(_,zodat,_,comp,[])),[]),
    Ssub  = tree(r(body,p(ssub)),Ds),
    debug_message(1,"treex correction: om te [su inf] => zodat [su ssub]~n",[]).

transform_rule(tree(r(mod,p(oti)),[Om,TeNoun]),  tree(r(mod,p(pp)),[Tot,Noun])) :-
    Om =    tree(r(cmp,adt_lex(_,om,_,_,_)),[]),
    TeNoun =tree(r(body,p(ti)),[Te,Noun0]),
    Te =    tree(r(cmp,adt_lex(_,te,_,_,_)),[]),
    Noun0 = tree(r(body,adt_lex(A,B,C,N,E)),[]),
    member(N,[noun,name]),
    Tot =   tree(r(hd,adt_lex(pp,tot,tot,prep,[])),[]),
    Noun=   tree(r(obj1,adt_lex(A,B,C,N,E)),[]),
    debug_message(1,"treex correction: om te noun => tot noun~n",[]).

% (er is geen probleem) in hebben X => om X te hebben
transform_rule(tree(r(Rel,p(cp)),[In,Body]), tree(r(Rel,p(oti)),[Om,TeBody])) :-
    In   = tree(r(cmp,adt_lex(_,in,_,_,_)),[]),
    Body = tree(r(body,_),_),
    Om   = tree(r(cmp,adt_lex(_,om,_,comp,[])),[]),
    TeBody = tree(r(body,p(ti)),[Te,Body]),
    Te   = tree(r(cmp,adt_lex(_,te,_,comp,[])),[]),
    debug_message(1,"treex correction: (er is geen probleem) in hebben X => om X te hebben~n",[]).

transform_rule(tree(r(Rel,p(cp)),[Dat,Conj]),tree(r(Rel,p(conj)),[CP,CRD,CNJ2])) :-
    Dat =  tree(r(cmp,_),_),
    Conj = tree(r(body,p(conj)),[SSUB,CRD,CNJ2]), 
    SSUB = tree(r(cnj,p(Nonvar)),SSUBDS), Nonvar == ssub,
    CNJ2 = tree(r(cnj,p(Nonvar2)),_), nonvar(Nonvar2), member(Nonvar2,[smain,sv1,cp]),
    CP   = tree(r(cnj,p(cp)),[Dat,tree(r(body,p(ssub)),SSUBDS)]),
    debug_message(1,"treex correction: dat [ssub and sv1] ==> [dat ssub] and sv1~n",[]).

transform_rule(tree(r(Rel,p(pp)),[Van,Natuurlijk]), tree(r(Rel,adt_lex(_,Bijwoord,_,Pos,[])),[]) ):-
    Van = tree(r(hd,adt_lex(_,V,_,_,_)),[]),
    Natuurlijk = tree(r(obj1,adt_lex(_,N,_,_,_)),[]),
    van_natuurlijk(V,N,Bijwoord,Pos),
    debug_message(1,"treex correction: van natuurlijk => natuurlijk~n",[]).

transform_rule(tree(r(Rel,Cat),[Alle,De|Rest]), tree(r(Rel,Cat),[AlDe|Rest])) :-
    (  Cat = p(np) ; Cat = i(_,p(np)) ),
    Alle = tree(r(det,adt_lex(_,alle,_,det,_)),[]),
    De   = tree(r(det,DeLex),[]),
    AlDe = tree(r(det,p(detp)),[Al,De2]),
    Al   = tree(r(mod,adt_lex(_,al,_,det,[])),[]),
    De2  = tree(r(hd,DeLex),[]),
    debug_message(1,"treex correction: alle det .. => [al det] .. ~n",[]).

transform_rule(tree(r(vc,_),Ds0), tree(r(mod,p(pp)), Ds)):-
    Regard = tree(r(hd,adt_lex(_,regard,_,verb,_)),[]),
    select(Regard, Ds0, Ds1),
    Su = tree(r(su,i(_)),[]),
    (   select(Su,Ds1,Ds2)
    ;   Ds1 = Ds2
    ),
    Ds2 = [tree(r(obj1,_),_)],
    Ds = [tree(r(hd,adt_lex(_,over,_,prep,[])),[])|Ds2],
    debug_message(1,"treex correction: regard X => over X~n",[]).

transform_rule(tree(Cat,Ds0), tree(Cat,[PP|Ds]) ):-
    Q = tree(r(hd,adt_lex(_,Quarantine,_,verb,Atts)),[]),
    H = tree(r(hd,adt_lex(_,Plaats,_,verb,Atts)),[]),
    replace(Q,H,Ds0,Ds),
    PP = tree(r(mod,p(pp)),[In,N]),
    In = tree(r(hd,adt_lex(_,P,_,prep,[])),[]),
    N  = tree(r(obj1,adt_lex(_,Quarantaine,_,noun,[])),[]),
    verb_verb_pp(Quarantine,Plaats,P,Quarantaine),
    debug_message(1,"treex correction: quarantine 1~n",[]).

transform_rule(tree(Cat,Ds0), tree(Cat,Ds) ):-
    Q = tree(r(REL,adt_lex(_,Quarantine,_,verb,Atts)),[]),
    H = tree(r(REL,p(_)),[tree(r(hd,adt_lex(_,Plaats,_,verb,Atts)),[]),PP]),			  
    replace(Q,H,Ds0,Ds),
    PP = tree(r(mod,p(pp)),[In,N]),
    In = tree(r(hd,adt_lex(_,P,_,prep,[])),[]),
    N  = tree(r(obj1,adt_lex(_,Quarantaine,_,noun,[])),[]),
    verb_verb_pp(Quarantine,Plaats,P,Quarantaine),
    debug_message(1,"treex correction: quarantine 2~n",[]).

transform_rule(tree(Cat,Ds0), tree(Cat,Ds) ):-
    Hd = tree(r(hd,adt_lex(_,Heb,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Heb,[heb,hebben]),
    VC0 = tree(r(vc,VcCat),VCDS0),
    VC  = tree(r(vc,VcCat),VCDS1),
    replace(VC0,VC,Ds0,Ds),
    V = tree(r(hd,adt_lex(_,_,_,verb,_)),[]),
    member(V,VCDS0),
    Su = tree(r(su,SuCat),SuDs),
    Obj1 = tree(r(obj1,SuCat),SuDs),
    replace(Su,Obj1,VCDS0,VCDS1),
    \+ member(tree(r(obj1,_),_),VCDS0),
    \+ member(tree(r(vc,_),_),VCDS0),
    debug_message(1,"treex correction: su of VP is probably OBJ1~n",[]).

transform_rule(tree(Cat,Ds0), tree(Cat,Ds) ):-
    Hd = tree(r(hd,adt_lex(_,Heb,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Heb,[heb,hebben]),
    VC0 = tree(r(vc,p(ppart)),VCDS0),
    VC  = tree(r(vc,p(ppart)),VCDS1),
    replace(VC0,VC,Ds0,Ds),
    V0 = tree(r(hd,adt_lex(_,L,_,verb,[_|_])),[]),
    V  = tree(r(hd,adt_lex(_,L,_,verb,[])),[]),
    replace(V0,V,VCDS0,VCDS1),
    debug_message(1,"treex correction: hebben takes ppart, ignore attributes of embedded verb~n",[]).

transform_rule(tree(r(mod,p(rel)),[Wanneer0,Body0]), tree(r(mod,p(cp)),[Wanneer,Body])):-
    Wanneer0 = tree(r(rhd,i(I,adt_lex(_,wanneer,_,_,_))),[]),
    Wanneer  = tree(r(cmp,adt_lex(_,wanneer,_,comp,[])),[]),
    remove_tree(tree(r(_,i(I)),[]),Body0,Body),
    debug_message(1,"treex correction: wanneer cannot head a relative clause~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds) ):-
    member(tree(r(hd,adt_lex(_,_,_,verb,_)),[]),Ds0),
    Det = tree(r(det,p(Cat)),DetDs),
    Obj1= tree(r(obj1,p(Cat)),DetDs),
    replace(Det,Obj1,Ds0,Ds),
    \+ member(tree(r(obj1,_),_),Ds0),
    debug_message(1,"treex correction: det of verb is perhaps obj1~n",[]).

transform_rule(tree(Node,[C0,C1,CRD,C2]), tree(Node,[C11,CRD,C2]) ):-
    C0 = tree(r(cnj,adt_lex(A,B,C,det,E)),[]),
    C1 = tree(r(cnj,adt_lex(F,G,H,noun,I)),[]),
    CRD = tree(r(crd,_),_),
    C2 = tree(r(cnj,_),_),
    C11 = tree(r(cnj,p(np)),[tree(r(det,adt_lex(A,B,C,det,E)),[]),
			     tree(r(hd, adt_lex(F,G,H,noun,I)),[])
			    ]),
    debug_message(1,"treex correction: det n and n => np and n~n",[]).

transform_rule(tree(r(Rel,Cat),Ds0), tree(r(Rel,p(du)),[Sat,Nucl2]) ):-
    VC0 = tree(r(vc,p(du)), [Sat,Nucl]),
    Sat = tree(r(sat,_),_),
    Nucl= tree(r(nucl,NuclCat),NuclDs),
    VC  = tree(r(vc,NuclCat),NuclDs),
    replace(VC0,VC,Ds0,Ds),
    Nucl2 = tree(r(nucl,Cat),Ds),
    debug_message(1,"treex correction: promote sat-nucl out of vc",[]).

transform_rule(tree(r(--,p(Smain)),Ds0), tree(r(--,p(du)),[Dp1,Dp2])) :-
    Smain == smain,
    append(Pre,[MOD],Ds0),
    MOD = tree(r(mod,p(Var)),MODDS),
    var(Var),
    member(tree(r(hd,adt_lex(_,I,_,Verb,_)),[]),MODDS),
    Verb == verb,
    \+ ignore_lemma(I),
    \+ member(tree(r(su,_),_), MODDS),
    Dp1 = tree(r(dp,p(smain)),Pre),
    Dp2 = tree(r(dp,p(smain)),[tree(r(su,adt_lex(_,dit,_,det,[])),[])|MODDS]),
    debug_message(1,"treex correction: add su to smain mod => dp dp~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)):-
    append(Pre,[Cnj1,Cnj2|Post],Ds0),
    looks_like_mod(Cnj1),
    looks_like_np(Cnj2),
    Cnj1 = tree(r(cnj,Cnj1Node),Cnj1Ds),
    Cnj2 = tree(r(cnj,Cnj2Node),Cnj2Ds),
    New = tree(r(cnj,Cnj2Node),[tree(r(mod,Cnj1Node),Cnj1Ds)|Cnj2Ds]),
    append(Pre,[New|Post],Ds),
    debug_message(1,"treex correction: conjunct looks like mod~n",[]).

transform_rule(tree(r(Rel,i(Index,p(pp))),[MetTree,WaarmeeTree]),
	       tree(r(Rel,i(Index,adt_lex(pp,Waarmee,_,pp,[]))),[])) :-
    MetTree = tree(r(hd,adt_lex(_,Met,_,prep,_)),[]),
    WaarmeeTree = tree(r(obj1,adt_lex(_,Waarmee,_,pron,_)),[]),
    met_waarmee(Met,Waarmee),
    debug_message(1,"treex correction: ~w ~w => ~w~n",[Met,Waarmee,Waarmee]).

transform_rule(tree(r(Rel,p(pp)),[MetTree,WaarmeeTree]),
	       tree(r(Rel,adt_lex(_,Waarmee,_,pp,[])),[])) :-
    MetTree = tree(r(hd,adt_lex(_,Met,_,prep,_)),[]),
    WaarmeeTree = tree(r(obj1,adt_lex(_,Waarmee,_,pron,_)),[]),
    met_waarmee(Met,Waarmee),
    debug_message(1,"treex correction: ~w ~w => ~w~n",[Met,Waarmee,Waarmee]).

%% [ ... [ X and Y ]] => [ ... X ] and Y  where X and Y unlike categories
transform_rule(tree(r(Rel,Node),Ds0), tree(r(Rel,p(conj)),Ds) ):-
    Rel \== 'top',
    append(Pre,[Conj],Ds0),
    Conj = tree(r(RelC,p(conj)),[Cnj1,Crd,Cnj2]),
    Cnj1 = tree(r(cnj,C1),CDs1),
    Crd  = tree(r(crd,_),_),
    Cnj2 = tree(r(cnj,C2),CDs2),
    append(Pre,[tree(r(RelC,C1),CDs1)],NewDs),
    Ds = [ tree(r(cnj,Node),NewDs), Crd, Cnj2 ],
    unlike_cats(C1,CDs1,C2,CDs2),
    debug_message(1,"treex correction: promote conjuntion, unlike categories~n",[]).

transform_rule(tree(r(Rel,p(pp)),[Voor,DezeDaartoe]),
	       tree(r(Rel,adt_lex(_,daarvoor,_,pp,[])),[])) :-
    Voor        = tree(r(hd,adt_lex(_,voor,_,_,_)),[]),
    DezeDaartoe = tree(r(obj1,p(np)),[Deze,Daartoe]),
    Deze        = tree(r(_,adt_lex(_,deze,_,_,_)),[]),
    Daartoe     = tree(r(_,adt_lex(_,daartoe,_,_,_)),[]),
    debug_message(1,"treex correction: voor deze daartoe => daarvoor~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds) ):-
    Neem       = tree(r(hd,adt_lex(_,N,_,verb,Atts)),[]),
    Plaats     = tree(r(_,adt_lex(_,P,_,_,_)),[]),
    VindPlaats = tree(r(hd,adt_lex(_,VP,_,verb,Atts)),[]),
    replace(Neem,VindPlaats,Ds0,Ds1),
    takes_place(N,P,VP),
    select(Plaats,Ds1,Ds),
    debug_message(1,"treex correction: ~w ~w => ~w~n",[N,P,VP]).

transform_rule(tree(r('--',Cat),Ds0), tree(r('--',Cat),Ds)) :-
    Afh1 = tree(r(hd,adt_lex(_,AfhL,_,_,_)),[]),
    Afh2 = tree(r(hd,adt_lex(_,AfhV,_,verb,[])),[]),
    member(tree(r(su,_),_),Ds0),
    replace(Afh1,Afh2,Ds0,Ds),
    adj_verb_lemma(AfhL,AfhV),
    debug_message(1,"treex correction: ~w => ~w",[AfhL,AfhV]).

transform_rule(tree(r(Rel,p(Inf)),Ds0), tree(r(Rel,p(Inf)),Ds)) :-
    Inf == inf,
    Hd0 = tree(r(hd,adt_lex(Y,V,W,verb,[_|_])),[]),
    Hd  = tree(r(hd,adt_lex(Y,V,W,verb,[])),[]),
    replace(Hd0,Hd,Ds0,Ds),
    debug_message(1,"treex correction: ignore attributes of infinitive~n",[]).

transform_rule(tree(r(Rel,p(cp)),[Zoals,Body]), tree(r(Rel,p(smain)),Ds) ):-
    Zoals = tree(r(cmp,adt_lex(_,zoals,_,_,_)),[]),
    Body =  tree(r(body,p(ssub)),[Dergelijk|Ds]),
    Dergelijk = tree(r(_,adt_lex(_,dergelijk,_,_,_)),[]),
    debug_message(1,"treex correction: ignore 'zoals dergelijk (as such)'~n",[]).

%% om te with subj: 'to do that, ...'
transform_rule(tree(r('--',p(oti)),[Om,Body1]), tree(r('--',Cat),Ds)):-
    Om =    tree(r(cmp,adt_lex(_,om,_,_,_)),[]),
    Body1 = tree(r(body,p(ti)),[Te,Body2]),
    Te =    tree(r(cmp,adt_lex(_,te,_,_,_)),[]),
    Body2 = tree(r(body,Cat),Ds),
    member(tree(r(su,_),_), Ds),  % otherwise it can be a normal oti
    debug_message(1,"treex correction: ignore top oti~n",[]).

%% *no* subject control
transform_rule(tree(Node, Ds0), tree(Node, Ds) ) :-
    Hd = tree(r(hd,adt_lex(_,R,_,verb,_)),[]),
    member(Hd,Ds0),
    member(R,[aanraden,raad_aan,adviseren,adviseer,verzoek,verzoeken,vraag,vragen,waarschuwen,waarschuw]), % vp_no_control
    (   Su0 = tree(r(su,i(I,SuNode)),SuDs),
	Su  = tree(r(su,SuNode),SuDs),
	replace(Su0,Su,Ds0,Ds1)
    ;   Su  = tree(r(su,i(I)),[]),
	member(Su,Ds0),
	Ds0 = Ds1
    ),
    Vc0 = tree(r(vc,p(ti)),[Cmp,Body0]),
    Vc1 = tree(r(vc,p(ti)),[Cmp,Body1]),
    Cmp = tree(r(cmp,adt_lex(_,te,_,_,_)),[]),
    Body0 = tree(r(body,p(inf)),VDs0),
    Body1 = tree(r(body,p(inf)),VDs1),
    replace(Vc0,Vc1,Ds1,Ds),
    EmbSu = tree(r(su,i(I)),[]),
    select(EmbSu,VDs0,VDs1),
    debug_message(1,"treex correction: vp_no_control verbs have no subj control~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Modal,_,_,_)),[]),
    member(Hd,Ds0),
    member(Modal,[zal,wil,kun,moet]),
    O1 = tree(r(obj1,adt_lex(_,toegang,_,_,_)),[]),
    O2 = tree(r(vc,adt_lex(_,open,_,verb,[])),[]),
    replace(O1,O2,Ds0,Ds),
    debug_message(1,"treex correction: modal + toegang => open~n",[]).

transform_rule(tree(Node,Ds0), tree(Node,Ds)) :-
    Hd = tree(r(hd,adt_lex(_,Wens,_,verb,_)),[]),
    member(Hd,Ds0),
    member(Wens,[wens]),
    Obj1 = tree(r(obj1,_),_),
    select(Obj1,Ds0,Ds1), 
    VC1  = tree(r(vc,p(ti)),[Te,Inf]),
    replace(VC1,VC2,Ds1,Ds),
    VC2  = tree(r(vc,p(ti)),[Te,ObjInf]),
    Te   = tree(r(cmp,adt_lex(_,te,_,_,_)),[]),
    (    Inf  = tree(r(body,p(inf)),InfDs),
      ObjInf  = tree(r(body,p(inf)),[Obj1|InfDs])
    ;    Inf  = tree(r(body,adt_lex(A,B,C,verb,E)),[]),
      ObjInf  = tree(r(body,p(inf)),[Obj1,tree(r(hd,adt_lex(A,B,C,verb,E)),[])])
    ),
    debug_message(1,"treex correction: lower obj1 in vc for ~w~n",[Wens]).

transform_rule(tree(r(Rel,p(np)),[Alle,VanNp]), tree(r(Rel,p(np)),Ds) ) :-
    Alle  = tree(r(hd,adt_lex(_,alle,_,_,_)),[]),
    VanNp = tree(r(mod,p(pp)),[Van,NP]),
    Van   = tree(r(hd,adt_lex(_,van,_,_,_)),[]),
    NP    = tree(r(obj1,p(np)),Ds0),
    Det0  = tree(r(det,adt_lex(A,B,C,D,E)),[]),
    Det1  = tree(r(det,p(detp)),[Al,tree(r(hd,adt_lex(A,B,C,D,E)),[])]),
    Al    = tree(r(mod,adt_lex(_,al,_,adv,[])),[]),
    replace(Det0,Det1,Ds0,Ds),
    debug_message(1,"treex correction: alle van de => al de ~n",[]).

met_waarmee(Prep,Adv) :-
    alpino_lex:xl(Adv,Tag,_,[],[]),
    (  Tag = waar_adverb(Prep)
    ;  Tag = er_adverb(Prep)
    ).

unplug('Unplug',verwijder).

looks_like_mod(tree(r(_,adt_lex(_,_,_,adv,_)),[])).
looks_like_mod(tree(r(_,p(advp)),Ds)) :-
    member(tree(r(hd,adt_lex(_,_,_,adv,_)),[]),Ds).
looks_like_mod(tree(r(_,p(pp)),Ds)) :-
    member(tree(r(hd,adt_lex(_,_,_,prep,_)),[]),Ds).

looks_like_n_or_np(Tree) :-
    looks_like_np(Tree).
looks_like_n_or_np(tree(r(_,adt_lex(_,_,_,noun,_)),[])).

looks_like_np(tree(r(_,p(NP)),_)) :-
    NP == np.
looks_like_np(tree(_,Ds)) :-
    member(tree(r(det,_),_),Ds).
looks_like_np(tree(_,Ds)) :-
    member(tree(r(hd,adt_lex(_,_,_,noun,_)),_),Ds).


verb_verb_pp(quarantine,breng,in,quarantaine).

leaves_or_single(Trees,Lemma) :-
    leaves(Trees,Lemma).
leaves_or_single([tree(_,[Tree|Trees])],Lemma) :-
    leaves([Tree|Trees],Lemma).

leaves(Nodes,Lemma) :-
    leaves(Nodes,Lemmas,[]),  
    special_leaves(Lemmas,Lemmas2,['"']),   %" 
    concat_all(['"'|Lemmas2],Lemma,' ').    %"

special_leaves([],L,L).
special_leaves([A,B,C|T],[L0|L1],L) :-
    trigram_name(A,B,C,L0),
    !,
    special_leaves(T,L1,L).
special_leaves([A,B|T],[L0|L1],L) :-
    bigram_name(A,B,L0),
    !,
    special_leaves(T,L1,L).
special_leaves([A|T],[L0|L1],L) :-
    member(Pos,[noun,name]),
    special_lemma(A,L0,Pos),
    !,
    special_leaves(T,L1,L).
special_leaves([H|T],[H|L1],L) :-
    special_leaves(T,L1,L).

leaves([],L,L).
leaves([tree(Node,Ds)|T],L0,L) :-
    leaves_node(Ds,Node,L0,L1),
    leaves(T,L1,L).

leaves_node([],r(_,adt_lex(_,L,_,_,_)),[L|T],T) :-
    (    L='>'
    ;    \+ ignore_lemma(L)
    ).
leaves_node([],r(_,adt_lex(_,L,_,_,_)),T,T) :-
    ignore_lemma(L).
leaves_node([H|T],_,L0,L) :-
    leaves([H|T],L0,L).

stopcontact(electrisch,stopcontact).

dehet(account,het) :- !.
dehet(commando,het) :- !.
dehet(deel,het) :- !.
dehet(geval,het) :- !.
dehet(menu,het) :- !.
dehet(Menu,het) :-
    atom_concat(_,'Menu',Menu),
    !.
dehet(Menu,het) :-
    atom_concat(_,menu,Menu),
    !.
dehet(pc,de) :- !.
dehet(venster,het) :- !.
dehet(virus,het) :- !.
dehet(Atom,de) :-
    atom_concat(_,drive,Atom), !.
dehet(Atom,het) :-
    atom_concat(_,adres,Atom), !.
dehet(Atom,de) :-
    atom_concat(web_site,_,Atom), !.
dehet(Atom,de) :-
    atom_concat('plug-in',_,Atom), !.
dehet(DeHet,het) :-
       alpino_genlex:dict_entry(DeHet,noun(het,_,sg),_),
    \+ alpino_genlex:dict_entry(DeHet,noun( de,_,sg),_).
dehet(DeHet,de) :-
    \+ alpino_genlex:dict_entry(DeHet,noun(het,_,sg),_),
       alpino_genlex:dict_entry(DeHet,noun( de,_,sg),_).

adapt_dehet_rel(de,dat,die).
adapt_dehet_rel(het,die,dat).

adapt_dehet(het,A,B):-
    adapt_dehet(B,A).
adapt_dehet(de,A,B):-
    adapt_dehet(A,B).

adapt_dehet(het,de).
adapt_dehet(hetzelfde,dezelfde).
adapt_dehet(dit,deze).
adapt_dehet(dat,die).

remove_index_ds([],_,[]).
remove_index_ds([H0|T0],Index,T) :-
    indexed_node(H0,Index), !,
    remove_index_ds(T0,Index,T).
remove_index_ds([H0|T0],Index,[H|T]) :-
    remove_index(H0,Index,H),
    remove_index_ds(T0,Index,T).

indexed_node(tree(r(_,i(Index0)),[]),Index) :-
    Index0 == Index.

remove_index(tree(Node,Ds0), Index, tree(Node,Ds)):-
    remove_index_ds(Ds0,Index,Ds).


preferred_combination(Current,Alt) :-
    alpino_penalties:corpus_frequency_lookup(Alt,A),
    (   alpino_penalties:corpus_frequency_lookup(Current,C)
    -> 	A > C			% Altternative has higher score
    ;   true			% Current does not exist
    ).

worse_combination(Current,Alt) :-
    \+ alpino_penalties:corpus_frequency_lookup(Current,_),
    alpino_penalties:corpus_frequency_lookup(Alt,_).

unlikely_pc_combination(_,_,via).
unlikely_pc_combination(verb,heb,in).
unlikely_pc_combination(verb,halen,uit).
unlikely_pc_combination(verb,plaatsen,voor).
unlikely_pc_combination(verb,geven,aan).
unlikely_pc_combination(verb,zitten,op).
unlikely_pc_combination(verb,zien,op).
unlikely_pc_combination(verb,zijn,_).
unlikely_pc_combination(verb,maken,met).
unlikely_pc_combination(verb,krijgen,met).
unlikely_pc_combination(verb,maken,op).
unlikely_pc_combination(verb,maken,voor).
unlikely_pc_combination(verb,nemen,met).
unlikely_pc_combination(verb,openen,met).
unlikely_pc_combination(verb,hebben,_).
unlikely_pc_combination(verb,drukken,op).
unlikely_pc_combination(verb,in_stellen,naar).
unlikely_pc_combination(verb,vinden,in).
unlikely_pc_combination(verb,zetten,op).
unlikely_pc_combination(verb,proberen,met).
unlikely_pc_combination(HdPos,Hd,Prep) :-
    \+ alpino_penalties:corpus_frequency_lookup(dep35(Prep,prep,hd/pc,HdPos,Hd),_),
    \+ (  alpino_lex:un_is_verb_lemma(Hd,Root),
	  alpino_penalties:corpus_frequency_lookup(dep35(Prep,prep,hd/pc,HdPos,Root),_)
       ).

swap(A,B,[A,B|T],[B,A|T]).
swap(A,B,[H|T0],[H|T]):-
    swap(A,B,T0,T).

%% must replace something...
replace(El0,El,[El0|Tail],[El|Tail]).
replace(El0,El,[X|Tail0],[X|Tail]):-
    replace(El0,El,Tail0,Tail).

%% must replace anywhere in the tree
replace_t(El0,El,El0,El).
replace_t(El0,El,tree(Node,Ds0),tree(Node,Ds)) :-
    replace_td(El0,El,Ds0,Ds).

replace_td(El0,El,[D0|Ds],[D|Ds]) :-
    replace_t(El0,El,D0,D).
replace_td(El0,El,[D|Ds0],[D|Ds]) :-
    replace_td(El0,El,Ds0,Ds).

adv_adj(alleen).
adv_adj(direct).
adv_adj(laat).
adv_adj(rechts).
adv_adj(ver).
adv_adj(waarschijnlijk).

wrong_pos_hd(eerste,eerst,num,adv,mod,_,[],verb).
wrong_pos_hd(eerste,eerst,adj,adv,mod,_,[],verb).
wrong_pos_hd(Root,Root,Pos0,Pos,Rel,Atts0,Atts,Hd):-
    wrong_pos_hd(Root,Pos0,Pos,Rel,Atts0,Atts,Hd).

wrong_pos_hd(Adj,adv,adj,mod,E,E,verb) :-
    adv_adj(Adj).
wrong_pos_hd(Adj,adv,adj,predc,E,E,verb) :-
    adv_adj(Adj).

wrong_pos_hd(al,adv,det,mod,E,E,det).



wrong_pos(Lemma,Lemma,Tag0,Tag,Rel,Rel,Atts0,Atts):-
    wrong_pos(Lemma,Tag0,Tag,Rel,Atts0,Atts).
wrong_pos(Lemma0,Lemma,Tag0,Tag,Rel,Rel,Atts0,Atts):-
    wrong_pos(Lemma0,Lemma,Tag0,Tag,Rel,Atts0,Atts).

%wrong_pos(gebruik,'door middel van',noun,prep,obj1,mod,_,[]).
%wrong_pos(gebruiken,'door middel van',verb,prep,mod,_,[]).
%wrong_pos(gebruik,'door middel van',noun,prep,mod,_,[]).

wrong_pos(op_lichten,gemarkeerd,verb,adj,mod,_,[]).

wrong_pos('doesn´t', niet,_,adv,_,_,[]).
wrong_pos('don´t',   niet,_,adv,_,_,[]).

wrong_pos(Lemma,Lemma,Noun,name,_,_,[]):-
    \+ Noun == name,
    (   atom_concat('http:',_,Lemma)
    ;   atom_concat('https:',_,Lemma)
    ;   atom_concat(Prefix,_,Lemma),
	alpino_lex:qtleap_hide_it_prefix(Prefix)
    ).

wrong_pos(waaronder,inclusief,adv,comp,_,_,[]).

wrong_pos(pers,druk,verb,verb,_,Atts,Atts).
wrong_pos(klop,goed,verb,adj,predc,_,[]).

wrong_pos(tegelijkertijd,gelijktijdig,adv,adj,_,_,[]).
wrong_pos('double-click',dubbelklik,_,verb,_,_,[]).
wrong_pos(zoek,zoek,noun,verb,_,_,[]).
wrong_pos(anomaly,fout,adv,noun,_,_,[]).
wrong_pos(used,gebruikt,adj,adj,_,_,[]).


wrong_pos(zullen,zal,N,verb,_,_,[]) :-
    \+ N==verb.
wrong_pos(kunnen,kan,N,verb,_,_,[]) :-
    \+ N==verb.
wrong_pos(mogen,kan,N,verb,_,_,[]) :-
    \+ N==verb.
wrong_pos(moeten,moet,N,verb,_,_,[]) :-
    \+ N==verb.
wrong_pos(worden,word,N,verb,_,_,[]) :-
    \+ N==verb.

wrong_pos(ontgrendelen,ontgrendel,name,verb,_,_,[]).

wrong_pos(mogelijk,heb,verb,verb,_,_,[]).
wrong_pos(opnieuw,vernieuw,verb,verb,_,_,[]).
wrong_pos(naam,benoem,verb,verb,_,Atts,Atts).
wrong_pos(preexist,besta,verb,adj,_,_,[]).
wrong_pos(download,gedownload,adj,adj,_,Atts,Atts).

wrong_pos(router,verb,noun,_,_,[]).
wrong_pos(bereid,noun,adj,_,_,[aform=base]).

wrong_pos(zich,pron,pron,se,[_|_],[]).

wrong_pos(sommige,pron,pron,_,[_|_],[]).

wrong_pos(er,Var,adv,mod,E,E) :- var(Var).

%wrong_pos(_,det,det,_,E0,E):-
%    select(per=_,E0,E).

wrong_pos('Outlook',noun,name,_,_,[]).

wrong_pos(aantal,pron,noun,_,E,E).
wrong_pos(slechts,VAR,adv,_,_,[]) :- var(VAR).
wrong_pos(hoeveel,det,adj,det,E,E).
wrong_pos(dat,det,pron,rhd,E,E).
wrong_pos(die,det,pron,rhd,E,E).
wrong_pos(enig,pron,adj,mod,E,E).
wrong_pos(geen,pron,det,_,E,E).
wrong_pos(best,noun,adj,_,E,E).
%wrong_pos(het,pron,noun,sup,E,E).
%wrong_pos(het,pron,noun,hd,_,[]).
%wrong_pos(het,pron,det,Rel,E,E) :- \+ Rel == sup, \+ Rel== hd.
wrong_pos(het,pron,det,det,_,[]).
wrong_pos(het,Nonvar,_,Rel,_,[]) :-
    \+ Rel == det,
    nonvar(Nonvar).
wrong_pos(meer,adv,adj,mod,E,[aform=compar|E]).
wrong_pos(meer,pron,adj,mod,E,[aform=compar|E]).
wrong_pos(meest,pron,adj,mod,E,[aform=super|E]).
wrong_pos(meest,noun,adj,mod,E,[aform=super|E]).
wrong_pos(normaal,noun,adj,predc,E,E).
wrong_pos(natuurlijk,adv,adj,mod,E,E).
wrong_pos(twitter,adj,name,mod,_,[]).
wrong_pos(uur,Pos,Pos,_,[_|_],[]).
wrong_pos(wat,pron,adv,mod,_,[]).
wrong_pos(zowel,pron,adv,_,_,[]).
wrong_pos(beide,pron,adv,mod,_,[]).

wrong_pos(wel,adv,adj,_,E,E).
wrong_pos(hoe,adv,adj,mod,E,E).
wrong_pos(hoe,adv,adj,whd,E,E).
wrong_pos(waar,pron,adv,_,_,[]).

wrong_pos(enkel,pron,adj,mod,_,[]).

wrong_pos(hier,pron,adv,mod,_,[]).

wrong_pos(te,Var,adv,mod,_,[]) :- var(Var).

wrong_pos(dus,vg,comp,dlink,E,E).
wrong_pos(en,vg,comp,dlink,E,E).
wrong_pos(en,Vg0,vg,Rel,E,E) :- Rel \== dlink, Vg0 \== vg.
wrong_pos(of,vg,comp,dlink,E,E).
wrong_pos(want,vg,comp,dlink,E,E).
wrong_pos(maar,vg,comp,dlink,E,E).

wrong_pos(draadloos,adj,noun,su,E,E).
wrong_pos(draadloos,adj,noun,obj1,E,E).

wrong_pos(maar,adj,adv,mod,_,[]).

wrong_pos(herstart,name,verb,_,_,[]).
wrong_pos(herstart,noun,verb,_,_,[]).
wrong_pos(verbind,noun,verb,_,_,[]).
wrong_pos(verwijder,name,verb,_,_,[]).
wrong_pos(verwijder,noun,verb,_,_,[]).
wrong_pos(standaard,adj,noun,obj1,_,[]).

wrong_pos(computer,name,noun,_,A,A).
wrong_pos(internet,name,noun,_,A,A).

wrong_pos(dezelfde,adj,det,hd,_,[]).
wrong_pos(hetzelfde,adj,det,hd,_,[]).
wrong_pos(datzelfde,adj,det,hd,_,[]).

wrong_pos(nog,Var,adv,mod,E,E):-
    var(Var).
wrong_pos(nog,Var,adv,hd,E,E):-
    var(Var).

wrong_pos(recent,adv,adj,mod,_,[aform=base]).

wrong_pos(geaccidenteerd,verb,adj,mod,E,E).
%wrong_pos(Ge,verb,adj,mod,E,E):-
%    \+ member(Ge,[besta,bestaan]),
%    alpino_lex:xl(Ge,adjective(GeType),Ge,[],[]),
%    member(GeType,[ge_no_e(_),ge_both(_)]).

wrong_pos(Word,noun,adj,predc,E,E):-
    alpino_lex:xl(Word,adjective(_),Word,[],[]),
    \+ alpino_lex:xl(Word,noun(_,_,_),Word,[],[]).

wrong_pos(Word,verb,adj,predc,E,E):-
    alpino_lex:xl(Word,adjective(_),Word,[],[]).

%gebruik(gebruik).
%gebruik(gebruiken).

%% at least 1 punctuation
names_and_punctuation([],[],X,X).
names_and_punctuation([H0|T0],[H|T],X0,X):-
    names_and_punct(H0,H,X0,X1),
    names_and_punctuation(T0,T,X1,X).

names_and_punct(tree(r(_,adt_lex(_,H,_,name,_)),[]),H,X,X).
names_and_punct(tree(r(_,adt_lex(_,P,_,_,_)),[]),P,_,1) :-
    alpino_lex:xl(P,punct(_),P,[],[]).

bigram_name('32','bit','32-bit').  % hack
bigram_name('64','bit','64-bit').  % hack

bigram_name('32 bit',venster,'32-bit Windows').
bigram_name('32-bit',venster,'32-bit Windows').
bigram_name('64 bit',venster,'64-bit Windows').
bigram_name('64-bit',venster,'64-bit Windows').

bigram_name(aandeel,koppeling,'Share link').
bigram_name(af_drukken,dienst,'Afdruk service').
bigram_name('Control',paneel,'Configuratiescherm').
bigram_name('Diapresentatie',tonen,'Diavoorstelling').
bigram_name(dienst,verpakking,'service pack').
bigram_name('Fix',zelf,'Fix It').
bigram_name('Google',af_spelen,'Google Play').
bigram_name(hoog,density,'high density').
bigram_name(juist,hand,rechter).
bigram_name(juist,hoek,rechterhoek).
bigram_name(kantoor,'Licence','Office Licence').
bigram_name('Licensing','Terms',licentie_voorwaarde).
bigram_name(licentie,'Terms',licentie_voorwaarde).
bigram_name(linker,kant,linkerkant).
bigram_name(links,hand,linker).
bigram_name(macht,energie_voorziening,energie_voorziening).
bigram_name('Microsoft Office','2010','Microsoft Office 2010').
bigram_name(open,bron,'open source').
bigram_name(open,'Office','Open Office').
bigram_name('Opmaak','Factory','Format Factory').
bigram_name(foto,'Gallery','Photo Gallery').
bigram_name(rechter,kant,rechterkant).
bigram_name(rechts,'hand hoek',rechter_hoek).
bigram_name('Shut',omlaag,'Shutdown').
bigram_name(sport,televisie,'Sport TV').
bigram_name(sport,tv,'Sport TV').
bigram_name('Start Menu','Shutdown','Start Menu >Shutdown').
bigram_name(stroom,energie_voorziening,energie_voorziening).
bigram_name('System',herstellen,'Systeemherstel').
bigram_name('System','Hulpmiddelen','Systeem Instellingen').
bigram_name(toegang,draaien,toegangscodes).
bigram_name(venster,'8.1','Windows 8').
bigram_name(venster,'8','Windows 8').
bigram_name(vragen,werk_balk,'Ask Toolbar').
bigram_name(voice,bericht,'Voice Mail').
bigram_name('Windows','8.1','Windows 8.1').
bigram_name('Windows','8','Windows 8').
bigram_name('Windows',bij_werken,'Windows Update').
bigram_name(woord,'Starter','Word Starter').

bigram_name(Name,Name2,Lemma) :-
    alpino_unknowns:starts_with_capital(Name),
    atom_concat('" >',_,Name2),             %"
    atom_concat('" ',Rest,Name2),           %"
    concat_all(['"',Name,Rest],Lemma,' ').  %"


bigram_noun(W,_,_,A,_,_,Stem) :-
    bigram_noun(W,A,Stem).
bigram_noun(goed,_,Atts,signaal,_,_,geluid_signaal) :-
    member(aform=base,Atts).


bigram_noun(pak_aan,balk,adres_balk).
bigram_noun(advertising,banner,advertentie).
bigram_noun(computer,geval,computer_behuizing).
bigram_noun(gebruiksaanwijzing,handleiding,gebruiksaanwijzing).
bigram_noun(gesprek_venster,venster,gesprek_venster).
bigram_noun(meld_aan,detail,inlog_gegeven).
bigram_noun(meld_aan,inlog_gegeven,inlog_gegeven).
bigram_noun(netwerk_verbinding,verbinding,netwerk_verbinding).
bigram_noun(besturing_systeem,systeem,besturing_systeem).
bigram_noun(draadloos,detail,'draadloos netwerk_detail').
bigram_noun(draadloos,naam,'draadloos netwerknaam'). % en niet draadloze netwerk
bigram_noun(draadloos,netwerk,'draadloos netwerk'). % en niet draadloze netwerk
bigram_noun(draadloos,kaart,'draadloos netwerkkaart'). 
bigram_noun(draadloos,knop,'draadloos netwerkknop'). 
bigram_noun(draadloos,pictogram,'draadloos netwerkpictogram').
bigram_noun(electrisch,knop,'AAN/UIT knop').
bigram_noun(netwerk,kabel,netwerk_kabel).
bigram_noun(hoofd_menu,menu,hoofd_menu).
bigram_noun(programming,taal,programmeer_taal).
bigram_noun(prullenbak,bin,prullenbak).
bigram_noun(activation,toets,activatie_sleutel).
bigram_noun(activatie,toets,activatie_sleutel).
bigram_noun(breed,band,breedband).
bigram_noun(rechts,zijde,rechter_zijde).
bigram_noun(reset,pin,'Reset knop').
bigram_noun(resetten,pin,'Reset knop').
bigram_noun(scannen,knop,'Scan knop').
bigram_noun(terug_zetten,pin,'Reset knop').
bigram_noun(toegang,toets,toegang_code).
bigram_noun(pagina,'Pagina_indeling','pagina_indeling').
bigram_noun(web_pagina,pagina,web_pagina).
bigram_noun(installatie_voorschrift,instructie,installatie_instructie).


%trigram_name(A,'>',C,Lemma) :-
%    concat_all([A,'>',C],Lemma).
trigram_name(A,'/',C,Lemma) :-
    concat_all([A,'/',C],Lemma).
trigram_name('Ctrl',verschuiving,verwijderen,'ctrl shift delete').
trigram_name(verpakking,'2','Office 2007 Service','Office 2007 Service Pack 2').
trigram_name('Windows','8',onderneming,'Windows 8 Enterprise').
trigram_name('Windows','8','Pro','Windows 8 Pro').
trigram_name('USB',knipperen,station,'USB-Flash drive').
trigram_name('USB',knipper,station,'USB-Flash drive').

trigram_name('Ctrl','Shift',verwijderen,'Ctrl Shift Delete').

trigram_noun(aan_melden,toegang,detail,inlog_gegeven).
trigram_noun(bovenste,rechts,hoek,rechter_boven_hoek).

special_lemma(manier,    _,     obj1,  hd,   in,       op,            prep,prep,Atts,Atts).
%% in de anywhere wereld
special_lemma(wereld,     _,    hd,    mod,  anywhere, heel,          _,    adj,  _,   []  ).
special_lemma(pictogram,  _,    hd,    mod,  kantoor,  'Office',      _,    name, _,   []  ).
special_lemma(geluid,     _,    obj1,  hd,   emit,     maak,          verb, verb, Atts,Atts).
special_lemma(_,          _,    det,   hd,   linker,   linker_zijde,  adj,  noun, _,   []  ).
special_lemma(_,          _,    det,   hd,   rechter,  rechter_zijde, adj,  noun, _,   []  ).
special_lemma(computer,   _,    obj1,  hd,   maak_op,  formatteer,    verb, verb, Atts,Atts).
special_lemma(schijf,     _,    obj1,  hd,   maak_op,  formatteer,    verb, verb, Atts,Atts).
special_lemma(in,         _,    hd,    obj1, woord,    'Word',        _,    name, Atts,Atts).
special_lemma(van,        _,    mod,   hd,   onderaan, onderkant,     _,    noun, _,   []  ).
special_lemma(installeer, _,    hd,    obj1, kantoor,  'Office',      _,    name, _,   []  ).
special_lemma(L,          _,    mod,   hd,   brief,    letter,        _,    noun, Atts,Atts):-
    atom_length(L,1).
special_lemma(L,          _,    app,   hd,   brief,    letter,        _,    noun, Atts,Atts):-
    atom_length(L,1).
special_lemma(_,          _,    hd,   obj1,  wireless, 'draadloos netwerk',  adj,  noun, _,   []  ).
special_lemma(_,          verb, hd,   mod,   volg,      vervolgens,   adj, adv,  _,   []).
special_lemma(_,          verb, hd,   predc, kunnen,    mogelijk,     verb, adj, _,   []).

special_lemma(A,B,C,C,E,E):-
    special_lemma(A,B,C).
special_lemma(plaats,            'in plaats van',  adj,  pp, _, []    ).
special_lemma(mains,             stroom, noun,noun,_,[]).
special_lemma(herstarten,        herstart,name,verb,A,A).
special_lemma(gewoonlijk,        vaak, adj,adj,Atts,Atts) :-
    member(aform=compar,Atts).
special_lemma(tablet, tablets,noun,noun,[rnum=pl],[rnum=pl]).  % hack
special_lemma(itune,  iTunes, _,_,_,[]).  % ignore plural

special_lemma(A,B,_) :-
    atom(A),
    atom_concat(B,…,A).

special_lemma(aanvoer,           voer_in,           verb     ).
special_lemma(accu,              batterij,          _        ).
special_lemma(accumulate,        verzamel,          _        ).
special_lemma(accumulation,      ophoping,          _        ).
special_lemma(accurate,          juist,             _        ).
special_lemma(activation,        activatie,         _        ).
special_lemma('activation sleutel',activatiesleutel,_        ).
special_lemma(actualisering,     update,             _       ).
special_lemma('afbeelding manipulation',beeldbewerking, _    ).
special_lemma('afbeelding bewerking',beeldbewerking, _       ).
special_lemma(airplane,          vliegtuig,          _       ).
special_lemma(anybody,           iedereen,           _       ).
special_lemma('assistance centrum','service_centrum',       _       ).
special_lemma(authenticity,      autorisatie,        _       ).
special_lemma(automated,         automatisch,        _       ).
special_lemma(her_start,         herstart,           _       ).
special_lemma(alternatively,     anders,             _       ).
special_lemma(analyze,           analyseer,          _       ).
special_lemma(andof,             uit,                _       ).
special_lemma(animation,         animatie,           _       ).
special_lemma(antenna,           antenne,            _       ).
special_lemma('antivirus blockage',antivirusprogramma,_      ).
special_lemma(assistant,         assistent,          _       ).
special_lemma(bandwidth,         bandbreedte,        _       ).
special_lemma(bericht_kop,       'Koptekst',         _       ).
special_lemma('" bericht_kop "', '" Koptekst "',         _       ).
special_lemma(beschadigd,        beschadig,          verb    ).
special_lemma(besparen,          bewaar,             _       ).
special_lemma(bespaar,           bewaar,             _       ).
special_lemma('bestand menu',    'Bestand menu',     _       ).
special_lemma('bestand uitbreiding',bestandextensie, _     ).
special_lemma('Better prestatie','Betere Prestaties',_       ).
special_lemma(blink,             knipper,            _       ).
special_lemma(blocked,           geblokkeerd,        adj     ).
special_lemma(bloke,             blokkeer,           verb    ).
special_lemma(bodem,             onderkant,          _       ).
special_lemma(brightness,        helderheid,         _       ).
special_lemma('briefwisseling klant','email client', _       ).
special_lemma('bureau vergunning','Office License',  _       ).
special_lemma(burn,              brand,              verb    ).
special_lemma(categorize,        categoriseer,       _       ).
special_lemma('cel telefoon_nummer getal','mobiele nummer',_ ).
special_lemma(cellphone,         'mobiele telefoon', _       ).
special_lemma('cel-telefoon',    'mobiele telefoon', _       ).
special_lemma('cel telefoon',    'mobiele telefoon', _       ).
special_lemma(charger,           oplader,            _       ).
special_lemma(chauffeur,         driver,             _       ).
special_lemma(check_out,         controleer,         _       ).
special_lemma(circle,            cirkel,             _       ).
special_lemma(comfortable,       comfortabel,        _       ).
special_lemma(compass,           kompas,             _       ).
special_lemma(compatibility,     compatibiliteit,    _       ).
special_lemma(connector,         aansluiting,        _       ).
special_lemma(constantly,        constant,           _       ).
special_lemma(consume,           verbruik,           verb    ).
special_lemma('Control paneel',  'Configuratiescherm',_      ).
special_lemma('Control paneel>geluid',  'Configuratiescherm>Audio',_      ).
special_lemma(conversation,      bericht,            _       ).
special_lemma('cooling systeem', koel_systeem,       _       ).
special_lemma(correspond,        correspondeer,      _       ).
special_lemma(coverage,          dekking,            _       ).
special_lemma(credentials,       inlog_gegeven,      noun    ).
special_lemma('Ctrl Shift verwijderen','Ctrl Shift Delete',_ ).
special_lemma(bestuurder,        driver,             _       ).
special_lemma('controleer brightness','monitor helderheid',_ ).
special_lemma(damaged,           kapot,              _       ).
special_lemma(dash,              streep_DIM,         noun    ).
special_lemma(deactivate,        deactiveer,         _       ).
special_lemma(dedicated,         specifiek,          adj     ).
special_lemma(descend,           ga,                 _       ).
special_lemma(deselect,          deselecteer,        _       ).
special_lemma(designate,         duid_aan,           _       ).
special_lemma(diabetesverpleegkundige,'Professional',_       ).
special_lemma(dial,              bel,                _       ).
special_lemma('dienst_verpakking','service pack',    _       ).
special_lemma('dienst verpakking','service pack',    _       ).
special_lemma(differently,       verschillend,       _       ).
special_lemma(discharge,         ontlading,          _       ).
special_lemma(dirt,              vuil,               _       ).
special_lemma(display,           beeldscherm,        _       ).
special_lemma('Doel_map map',    doel_folder,        _       ).
special_lemma(dot,               punt_DIM,           _       ).
special_lemma(doubt,             twijfel,            _       ).
special_lemma(downloadable,      beschikbaar,        _       ).
special_lemma(draai,             schakel_in,         verb    ).
special_lemma(earn,              verdien,            _       ).
special_lemma(eens,              schrijf_in,         verb    ).  % subscribe
special_lemma(eigendom,          eigenschap,         _       ).
special_lemma(electricity,       electriciteit,      _       ).
special_lemma(erase,             wis,                verb    ).
special_lemma(erkend,            erken,              verb    ).
special_lemma(erkennen,          herken,             _       ).
special_lemma(except,            behalve,            prep    ).
special_lemma(executable,        uitvoerbaar,        _       ).
special_lemma('exhibition modus','weergave modus',   _       ).
special_lemma(existence,         bestaan,            _       ).
special_lemma(expire,            verloop,            _       ).
special_lemma(factory,           fabriek,            _       ).
special_lemma(fairly,            tamelijk,           _       ).
special_lemma(favorite,          favoriet,           _       ).
special_lemma(fee,               bedrag,             _       ).
special_lemma(find_out,          achterhaal,         verb    ).
special_lemma(finger,            vinger,             _       ).
special_lemma(flaw,              fout,               _       ).
special_lemma(flip,              draai_om,           _       ).
special_lemma(followers,         'Volgers',          _       ).
special_lemma('Footer',          'Voettekst',        _       ).
special_lemma('" Footer "',      '" Voettekst "',    _       ).
special_lemma(footnote,          voetnoot,           _       ).
special_lemma(fragmentation,     fragmentatie,       _       ).
special_lemma('in-built',        ingebouwd,          _       ).
special_lemma(in_gaan,           voer_in,            _       ).
special_lemma(inbegrip,          'Toevoegen',        _       ).
special_lemma(free_up,           maak_vrij,          _       ).
special_lemma(freeware,          'gratis software',  _       ).
special_lemma(functionality,     functionaliteit,    _       ).
special_lemma(functioning,       functioneer,        _       ).
special_lemma(gear,              tandwiel,           _       ).
special_lemma('Gear',            tandwiel,           _       ).
special_lemma('gebruiker gebruiker_account','gebruiker_account',_).
special_lemma(geloof_brief,      gegeven,            _       ).
special_lemma('gsm telefoon',    'mobiele telefoon', _       ).
special_lemma('Google contact Sync','Google Contact Sync',_  ).
special_lemma('Google af_spelen','Google Play',      _       ).
special_lemma(haven,             poort,              _       ).
special_lemma(heating,           verhitting,         _       ).
special_lemma(highlighted,       gemarkeerd,         _       ).
special_lemma(hold_down,         druk_in,            _       ).
special_lemma(hole,              gat_DIM,            _       ).
special_lemma('Hulpmiddelen>optie','Bewerken > Instellingen',_).
special_lemma(indifferent,       onbelangrijk,       _       ).
special_lemma(infect,            infecteer,          verb    ).
special_lemma(inside,            in,                 prep    ).
special_lemma(instead,           dan,                adv     ).
special_lemma(intended,          gewenst,            _       ).
special_lemma(interact,          ga_om,              _       ).
special_lemma('internet dienst provider','Internet Service Provider',_).
special_lemma(invite,            nodig_uit,          _       ).
special_lemma('in_voeren toets', 'Enter toets',      _       ).
special_lemma(invoice,           factuur,            _       ).
special_lemma('ISO afbeelding',  'ISO Image',        _       ).
special_lemma(jullie,            je,                 det     ).
special_lemma('kantoor Mobile',  'Office Mobile',    _       ).
special_lemma('klant gebied',    'klant_omgeving',   _       ).
special_lemma(leave_off,         zet_uit,            _       ).
special_lemma(lifespan,          levensduur,         _       ).
special_lemma(malfunction,       fout,               _       ).
special_lemma(manufacter,        fabrikant,          _       ).
special_lemma(maximize,          maximaliseer,       _       ).
special_lemma(meherinneren,      herinner,           _       ).
special_lemma(me_herinneren,     herinner,           _       ).
special_lemma(keer_toe,          schakel_in,         _       ).
special_lemma(illuminate,        licht_op,           _       ).
special_lemma(leverancier,       provider,           _       ).
special_lemma(links,             linker,             adj     ).
special_lemma(listen,            luister,            verb    ).
special_lemma(look_up,           zoek_op,            _       ).
special_lemma(loudspeaker,       luidspreker,        _       ).
special_lemma(mag,               kan,                verb    ).
special_lemma(magnet,            magneet,            _       ).
special_lemma('magnifying glas', vergrootglas,       _       ).
special_lemma(manipulation,      bewerking,          _       ).
special_lemma(mandatory,         verplicht,          _       ).
special_lemma(mention,           noem,               verb    ).
special_lemma(mobile,            mobiel,             adj     ).
special_lemma(modification,      aanpassing,         _       ).
special_lemma(mother_board,      moederbord,         _       ).
special_lemma(motherboard,       moederbord,         _       ).
special_lemma(muis_wijzer,       cursor,             _       ).
special_lemma(narration,         commentaar_stem,    _       ).
special_lemma(neither_nor,       noch,               _       ).
special_lemma('Network',         'Netwerk',          _       ).
% special_lemma(nodig,             benodigd,          _       ).
special_lemma(occupy,            bezet,              _       ).
special_lemma(on,                aan,                _       ).
special_lemma(onderneming_klimaat,'zakelijke omgeving',   _  ).
special_lemma(onderschrijf,      schrijf_in,         _       ).
special_lemma(oorzaak,           veroorzaak,         verb    ).
special_lemma('Operating Systeem','Operating System',_       ).
special_lemma(optic,             optisch,            _       ).
special_lemma(organize,          organiseer,         _       ).
special_lemma(outgoing,          uitgaand,           _       ).
special_lemma(outlet,            stopcontact,        _       ).
special_lemma(opwaardering,      upgrade,            _       ).
special_lemma('[password]wachtwoord', wachtwoord,    _       ).
special_lemma(periodically,      periodiek,          _       ).
special_lemma(persist,           blijf,              _       ).
special_lemma(phone,             telefoon,           _       ).
special_lemma(plaatse,           plaats,             _       ).
special_lemma(pop_up,            verschijn,          verb    ).
special_lemma(popular,           populair,           _       ).
special_lemma(portable,          draagbaar,          _       ).
special_lemma('Portuguese',      'Portugees',        _       ).
special_lemma('possess',         bezit,              _       ).
special_lemma('pre-installed',   voorgeïnstalleerd,  _       ).
special_lemma(precise,           precies,            _       ).
special_lemma(precede,           ga_vooraf,          _       ).
special_lemma(preferably,        'het beste',        adv     ).
special_lemma(preferred,         gewenst,            _       ).
special_lemma('programming taal',programmeer_taal,   _       ).
special_lemma('Programs',        'Programma\'s',     _       ).
special_lemma(proper,            juist,              _       ).
special_lemma(protected,         beveiligd,          _       ).
special_lemma(protective,        beschermend,        _       ).
special_lemma(put_up,            plaats,             verb    ).
special_lemma(reconnect,         verbind,            _       ).
special_lemma(recover,           achterhaal,         _       ).
special_lemma('rechterkant kant',rechterkant,        _       ).
%special_lemma(rechts,            rechter,            adj     ).
special_lemma(rectangle,         rechthoek,          _       ).
special_lemma(redirect,          schakel_door,       verb    ).
special_lemma(registration,      registratie,        _       ).
special_lemma('Rehearse Timing', 'Tijdsinstelling voor try-out',_).
special_lemma(rekening,          account,            _       ).
special_lemma(removable,         verwisselbaar,      _       ).
special_lemma('Removable',       verwisselbaar,      _       ).
special_lemma(renew,             vernieuw,           verb    ).
special_lemma(repeatedly,        herhaaldelijk,      _       ).
special_lemma(reply,             antwoord,           _       ).
special_lemma(respective,        respectievelijk,    _       ).
special_lemma(retrieval,         ophalen,            _       ).
special_lemma(richt_op,          configureer,        _       ).
special_lemma('right-hand',      rechter,            _       ).
special_lemma(satelliet_navigatie,navigatie,         _       ).
special_lemma(schade,            beschadig,          _       ).
special_lemma('Sharing',         'Delen',            _       ).
special_lemma(shake,             schud,              _       ).
special_lemma(shine,             schijn,             verb    ).
special_lemma(should,            moet,               verb    ).
special_lemma(show_up,           verschijn,          _       ).
special_lemma(shut_down,         schakel_uit,        verb    ).
special_lemma(simultaneously,    tegelijkertijd,     _       ).
special_lemma('sleutel combinatie',toets_combinatie, _       ).
special_lemma(sorce,             bron,               _       ).
special_lemma(sorteren_kolom,    toets,              _       ).  % key
special_lemma(specialized,       gespecialiseerd,    _       ).
special_lemma(spot,              plek,               noun    ).
special_lemma(spreker,           speaker,            _       ).
special_lemma(standardized,      gestandaardiseerd,  _       ).
special_lemma('starten menu',    'Start Menu',       _       ).
special_lemma(steek,             plaats,             verb    ).
special_lemma(stick,             zit_vast,           verb    ).
special_lemma(strictly,          echt,               _       ).
special_lemma(stuck,             kapot,              adj     ).
special_lemma(subsequently,      vervolgens,         _       ).
special_lemma(subscribe,         meld_aan,           verb    ).
special_lemma(suspect,           wantrouw,           verb    ).
special_lemma(synchronization,   synchronisatie,     _       ).
special_lemma(synchronise,       synchroniseer,      _       ).
special_lemma(synchronize,       synchroniseer,      _       ).
special_lemma('System',          'Systeem',          _       ).
special_lemma('taak balk',       taak_balk,          _       ).
special_lemma(taste,             smaak,              _       ).
special_lemma(tend,              kan,                verb    ).
special_lemma(tendency,          neiging,            _       ).
special_lemma(testing,           test,               _       ).
special_lemma(tick,              kies,               verb    ).
special_lemma('tijd capsule',    'Time Capsule',     _       ).
special_lemma(thunderstorm,      onweer,             _       ).
special_lemma(turn,              schakel_in,         _       ).
special_lemma(toezicht,          monitor,            _       ).
special_lemma(toezichthouder,    monitor,            _       ).
special_lemma('top-left',        linker_bovenkant,   _       ).
special_lemma(transmission,      transmissie,        _       ).
special_lemma(trustworthy,       betrouwbaar,        _       ).
special_lemma(turnon,            schakel_in,         _       ).
special_lemma(turn_on,           schakel_in,         _       ).
special_lemma(turnoff,           schakel_uit,        _       ).
special_lemma(turn_off,          schakel_uit,        _       ).
special_lemma(u_aan_melden,      meld_aan,           _       ).
special_lemma(uitloop,           stopcontact,        _       ).
special_lemma(underneath,        onder,              _       ).
special_lemma(uncheck,           vink_uit,           _       ).
special_lemma(uninstall,         deïnstalleer,       _       ).
special_lemma(uninstalling,      deïnstalleer,       _       ).
special_lemma(unmask,            vink_uit,           _       ).
special_lemma('Unplug',          verwijder,          _       ).
special_lemma(unplug,            verwijder,          _       ).
special_lemma(unlock,            ontgrendel,         _       ).
special_lemma(vary,              varieer,            _       ).
special_lemma('validation toets',validatie_sleutel,  _       ).
special_lemma(vast,              los_op,             verb    ).
special_lemma('veiligheid toets',beveilingssleutel,  _       ).
special_lemma(ventilate,         ventileer,          _       ).
special_lemma(verdeling,         distributie,        _       ).
special_lemma(verkooppunt,       stopcontact,        _       ).
special_lemma(verwerf,           schaf_aan,          _       ).
special_lemma(vis_tuig,          tandwiel,           _       ).
special_lemma(vistuig,           tandwiel,           _       ).
special_lemma(vrij,              gratis,             _       ).
special_lemma(vulnerability,     kwetsbaarheid,      _       ).
special_lemma(youe,              jouw,               _       ).
special_lemma(your,              jouw,               _       ).
special_lemma('you´ll',          je,                 _       ).
special_lemma(watch,             bekijk,             _       ).
special_lemma(whenever,          wanneer,            _       ).
special_lemma(wireless,          draadloos,          adj     ).
special_lemma(wireless,          'draadloos netwerk',noun    ).
special_lemma('+ sleutel',       '+ toets',          _       ).

special_lemma('Microsoft Office huis','Microsoft Office Home',_).
special_lemma('Microsoft Office diabetes_verpleegkundige','Microsoft Office Professional',_).
special_lemma('Windows Movie besluitvormer','Windows Movie Maker',_).
special_lemma('Movie besluitvormer','Movie Maker',_).
special_lemma('op/OFF',           'Aan/Uit', _).
special_lemma('PDF lezer',        'PDF reader',              _).
special_lemma('PDF schrijver',    'PDF writer',              _).
special_lemma('USB knipper drang','USB-Flash drive',_).
special_lemma('Windows medium speler','Windows Media Player',_).
special_lemma('zoek tekst_vak',   zoekvak, _).
special_lemma(macht,              stroom, _).
special_lemma('macht knop',       'AAN/UIT knop',            _).
special_lemma('stroom knop',      'AAN/UIT knop',            _).
special_lemma('Schepper',         'Writer',                  _).
special_lemma('energie kwijting', energieontlading,          _).
special_lemma('afspeel winkel',   'Play Store',              _).

%% qtleap corpus: pc is misspelled as PC
special_lemma(pc,'PC',_).
special_lemma('Pc','PC',_).

special_lemma('aanmeld inlog_gegeven',inlog_gegeven,_).
special_lemma('aanmeld toegang_gegeven',inlog_gegeven,_).
special_lemma('aanmeld detail',inlog_gegeven,_).
special_lemma('besturing_systeem systeem',besturing_systeem,_).
special_lemma('besturing_systeem systemen',besturing_systeem,_).
special_lemma('besturing_systeem System',besturing_systeem,_).
special_lemma('besturing_systeem Systeem',besturing_systeem,_).
special_lemma('gebruiker gebruikersinterface,',gebruiker_interface,_).
special_lemma('muis_knop knop','muis_knop',_).
special_lemma('webpagina pagina','web_pagina',_).
special_lemma('web_pagina pagina','web_pagina',_).
special_lemma('web web_browser','web_browser',_).
special_lemma('web web_browser cache','web_browser cache',_).
special_lemma('registratie_proces proces','registratie_proces',_).
special_lemma('muis_klik klik','muis_klik',_).
special_lemma('koel_systeem systeem','koel_systeem',_).
special_lemma('gebruiker_naam naam','gebruiker_naam',_).
special_lemma('contact_lijst lijst','contact_lijst',_).
special_lemma('bestand_systeem systeem','bestand_systeem',_).
special_lemma('bestand bestand_systeem','bestand_systeem',_).
special_lemma('veiligheid_reden reden','veiligheid_reden',_).
special_lemma('telefoon_nummer nummer','telefoon_nummer',_).
special_lemma('programmeer_taal taal','programmeer_taal',_).
special_lemma('netwerk_verbinding verbinding','netwerk_verbinding',_).
special_lemma('gebruiker_account account','gebruiker_account',_).
special_lemma('email_bericht bericht','email_bericht',_).
special_lemma('bestand_type type','bestand_type',_).
special_lemma('krediet credit card','credit card',_).
special_lemma('onderneming_klimaat omgeving','zakelijke omgeving',_).
special_lemma('netwerk_verbinding verbinding','netwerk verbinding',_).
special_lemma('e-mail_adres adres','e-mail_adres',_).
special_lemma('e-mail_adres_adres','e-mail_adres',_).
special_lemma('email email_programma','email programma',_).
special_lemma('muiswijzer cursor',cursor,_).
special_lemma('muis_wijzer cursor',cursor,_).
special_lemma('Cursor',cursor,_).
special_lemma('webserver server','webserver',_).
special_lemma('internet_verbinding verbinding','internet_verbinding',_).
special_lemma('internet internetprovider','internetprovider',_).
special_lemma('web_site site','web_site',_).
special_lemma(format_teren,formatteer,_).
special_lemma('MEO web_site web_site','MEO web_site',_).
special_lemma('commando_regel lijn','command prompt',_).
special_lemma('tv_kanaal_kanaal','tv_kanaal',_).
special_lemma('tv_kanaal_kanalen','tv_kanaal',_).
special_lemma('MEO ga dienst','MEO Go dienst',_).
special_lemma('MEO ga pakket','MEO Go pakket',_).
special_lemma('printer_stuur_programma stuur_programma','printer_stuur_programma',_).

special_lemma(dicht,             stop,               verb    ).
special_lemma(dichten,           stop,               verb    ).
special_lemma(magnetized,        magnetisch,         adj     ).
special_lemma(stekker,           sluit_aan,          verb    ).
special_lemma(toegang,           open,               verb    ).
special_lemma(verband,           link,               noun    ).
special_lemma(verband,           relateer,           verb    ).
special_lemma(verbinding,        verbind,            verb    ).
special_lemma(wear_out,          slijt,              verb    ).

special_lemma(Atom0, Atom, _) :-
    sub_atom(Atom0,_,_,_,' '),
    \+ alpino_lex:inv_lex(Atom0,_),
    atom_concat(First,Last,Atom0),
    atom_concat(' ',Rest,Last),
    concat_all([First,Rest],Atom,'_'),
    \+ \+ alpino_lex:inv_lex(Atom,_).


verb_ld(moeten,omhoog).
verb_ld(moet,omhoog).

verb_vc_mod(heb,om).
verb_vc_mod(drukken,om).
verb_vc_mod(druk,om).
verb_vc_mod(voeg_toe,om).
verb_vc_mod(Lemma,om) :-
    alpino_lex:un_is_verb_lemma(Lemma,Root),
    \+ alpino_penalties:corpus_frequency_lookup(dep35(om,comp,hd/vc,verb,Root),_),
    \+ alpino_penalties:corpus_frequency_lookup(dep35(te,comp,hd/vc,verb,Root),_).

verb_vc_te_om(Lemma) :-
    alpino_lex:un_is_verb_lemma(Lemma,Root),
    \+ Root = heb,
    \+ Root = ben,
    preferred_combination(dep35(om,comp,hd/vc,verb,Root),
			  dep35(te,comp,hd/vc,verb,Root)).

% verb_vc_om_te(Lemma) :-
%     alpino_lex:un_is_verb_lemma(Lemma,Root),
%     preferred_combination(dep35(te,comp,hd/vc,verb,Root),
% 			  dep35(om,comp,hd/vc,verb,Root)).

noun_vc_mod(Root,Comp) :-
    \+ alpino_penalties:corpus_frequency_lookup(dep35(Comp,comp,hd/vc,noun,Root),_).

verb_vc_obj1(klikken).
verb_vc_obj1(selecteren).
verb_vc_obj1(klik).
verb_vc_obj1(selecteer).

verb_obj1_ld(drukken,op).
verb_obj1_ld(druk,op).

verb_obj1_pc(klikken,op).
verb_obj1_pc(klik,op).

verb_predc_obj1(maak).
verb_predc_obj1(maken).
verb_predc_obj1(download).
verb_predc_obj1(downloaden).

verb_predc_mod(V) :-
    alpino_lex:un_is_verb_lemma(V,V1),
    \+ member(V1,[ben,word,maak,vind,blijf,heb,noem,ga,lijk,sta,blijk,raak,houd,zie,voel,heet,
		  lig,doe,stel,beschouw,zit,klink,neem,kan,zie_uit,werk]).

illegal_hd_lex_ds([tree(r(body,adt_lex(_,Root0,_,_,_)),[]),
		   tree(r(hd,adt_lex(_,Root1,_,Pos,Atts)),[])], Root,Pos,Atts):-
    compound_part(Root0,Root00),
    compound_part(Pos,Root1,Root11,Atts),
    hdrug_util:concat_all([Root00,Root11],Root,' ').

compound_part(Root0,Root) :-
    (  alpino_lex:un_is_verb_lemma(Root0,Root1)
    ;  Root0=Root1
    ),
    (  alpino_genlex:dict_entry(Root1,verb(_,sg1,ninv(_,_)),Root)
    ;  alpino_genlex:dict_entry(Root1,verb(_,sg, ninv(_,_)),Root)
    ;  alpino_genlex:dict_entry(Root1,verb(_,sg1,_        ),Root)
    ;  alpino_genlex:dict_entry(Root1,verb(_,sg, _        ),Root)
    ;  Root1=Root
    ).

%compound_part(noun,Root0,Root,Atts):-
%    member(rnum=pl,Atts),
%    alpino_genlex:dict_entry(Root0,noun(_,_,pl),Root),
%    !.
compound_part(_,Root0,Root,_) :-
    compound_part(Root0,Root).

mod_nouns([H|T],M0,M):-
    mod_noun(H,M0,M1),
    mod_nouns_(T,M1,M).

mod_nouns_([],M,M).
mod_nouns_([H|T],M0,M):-
    mod_noun(H,M0,M1),
    mod_nouns_(T,M1,M).

mod_noun(tree(r(REL,adt_lex(_,L,_,Noun,Atts)),[]),[L|M],M) :-
    mod_noun_rel(REL),
    \+ wrong_pos(L,_,Noun,_,REL,_,Atts,_),
    \+ noun_verb(L,_),
    mod_noun_root_pos(L,Noun).

/*
mod_noun(tree(_,Ds),M0,M):-
    mod_nouns_r(Ds,M0,M).

mod_nouns_r([H|T],M0,M):-
    mod_noun_r(H,M0,M1),
    mod_nouns_r_(T,M1,M).

mod_nouns_r_([],M,M).
mod_nouns_r_([H|T],M0,M):-
    mod_noun_r(H,M0,M1),
    mod_nouns_r_(T,M1,M).

mod_noun_r(Node,M0,M):-
    mod_noun(Node,M0,M).
mod_noun_r(tree(r(hd,adt_lex(_,L,_,noun,_)),[]),[L|M],M).
*/

mod_noun_rel(mod).
mod_noun_rel('{app,mod}').
mod_noun_rel(app).

mod_noun_root_pos(_,Noun) :-
    Noun == noun.
mod_noun_root_pos(L,_) :-					       
    alpino_lex:xl(L,punct(_),L,[],[]).
mod_noun_root_pos(L,_) :-					       
    \+ alpino_lex:xl(L,_,_,[],[]).

ignore_d(tree(r(_,p(pp)),[As,Well]),'as as_well') :-
    As = tree(r(_,adt_lex(_,as,_,_,_)),[]),
    Well=tree(r(_,adt_lex(_,as_well,_,_,_)),[]).

ignore_d(tree(r(_,adt_lex(_,Atom,_,_,_)),[]),Atom) :-
    ignore_lemma(Atom).

ignore_lemma('/').
ignore_lemma('>').
ignore_lemma('.').
ignore_lemma('[').
ignore_lemma(']').
ignore_lemma('<').
ignore_lemma('`').
ignore_lemma('``').
ignore_lemma('\'').
ignore_lemma('\'\'').
ignore_lemma('‘').
ignore_lemma('‘‘').
ignore_lemma('" "').

ignore_lemma(into).
ignore_lemma(s).
ignore_lemma(up).  % controle up systeem
ignore_lemma(whom).

more_cnj(Ds0,Su,Ds) :-
    Cnj1 = tree(r(cnj,p(inf)),Cnj1Ds),
    Cnj2 = tree(r(cnj,p(inf)),[Su|Cnj1Ds]),
    replace(Cnj1,Cnj2,Ds0,Ds1),
    NotSu = tree(r(su,_),_),
    \+ member(NotSu,Cnj1Ds),
    more_cnj(Ds1,Su,Ds).
more_cnj(Ds,_,Ds).

noun_mod(moment,tegelijkertijd,zelfde,adj,[aform=base]).

treat_unary([D1,D2|Ds],Cat,Cat,[D1,D2|Ds]).
treat_unary([tree(r(_,D1Cat),D1Ds)],r(Rel,_),r(Rel,D1Cat),D1Ds).

noun_verb(beurt,schakel_uit).
noun_verb(beschadig,beschadig).
noun_verb(schade,beschadig).
noun_verb(staat,sta_toe).
noun_verb(steun,ondersteun).
noun_verb(ondersteuning,ondersteun).
noun_verb(toegang,open).
noun_verb(verbinding,verbind).
noun_verb(verslag,rapporteer).
noun_verb(wijziging,wijzig).
noun_verb(opsporing,spoor_op).

verb_obj1(lopen,verb,voer_uit).
verb_obj1(dring_aan,verb,druk_in).
verb_obj1(type,noun,typ).

a_cat(A,ap) :-
    (  alpino_lex:xl(A,adjective(no_e(_)),_,[],[])
    ;  alpino_lex:xl(A,adjective(both(_)),_,[],[])
    ),
    \+  (  alpino_lex:xl(A,adjective(ge_no_e(_)),_,[],[])
	;  alpino_lex:xl(A,adjective(ge_both(_)),_,[],[])
	).
a_cat(A,ppart) :-
    (  alpino_lex:xl(A,adjective(ge_no_e(_)),_,[],[])
    ;  alpino_lex:xl(A,adjective(ge_both(_)),_,[],[])
    ),
    \+  (  alpino_lex:xl(A,adjective(no_e(_)),_,[],[])
	;  alpino_lex:xl(A,adjective(both(_)),_,[],[])
	).

verb_mod(moet,_) :- !,fail.
verb_mod(moeten,_) :- !,fail.
verb_mod(ben,_) :- !,fail.
verb_mod(zijn,_) :- !,fail.

verb_mod(Lemma0,Lemma) :-
    \+ member(Lemma0,[besta,bestaan,volg,volgen]),
    alpino_lex:un_is_verb_lemma(Lemma0,Lemma1),
    alpino_genlex:dict_entry(Lemma1,verb(_,psp,_),Lemma),
    alpino_genlex:dict_entry(Lemma,adjective(_),Lemma).
verb_mod(Lemma0,Lemma) :-
    alpino_lex:un_is_verb_lemma(Lemma0,Lemma).

adj_verb(open,open).
adj_verb(schoon,leeg).

op_bovenaan(op,bovenaan).

voor_gratis(voor,gratis).

missing_de(disc,de).
% missing_de(pictogram,het).
missing_de(programma,het).
missing_de(rechthoek,de).
missing_de(router,de).
missing_de(telefoon,de).
missing_de(werkbalk,de).

%% todo: conjunction
omte_inf(tree(r(vc,p(inf)),DS),
	 tree(r(vc,p(inf)),DS)) :-
    member(tree(r(hd,_),_),DS).
omte_inf(tree(r(vc,adt_lex(X1,X2,X3,verb,X4)),[]),
	 tree(r(vc,adt_lex(X1,X2,X3,verb,X4)),[])).
omte_inf(tree(r(vc,p(ti)),DS),Tree) :-
    DS = [tree(r(cmp,adt_lex(_,te,_,_,_)),[]),
	  tree(r(BodyRel,Body),BodyDs)],
    member(BodyRel,[body,mod]),
    omte_inf(tree(r(vc,Body),BodyDs),Tree).
omte_inf(tree(r(vc,p(oti)),DS),Tree) :-
    DS = [tree(r(cmp,adt_lex(_,om,_,_,_)),[]),
	  tree(r(BodyRel,Body),BodyDs)],
    member(BodyRel,[body,mod]),
    omte_inf(tree(r(vc,Body),BodyDs),Tree).

probeer_mod(verb,_).
probeer_mod(noun,stekker).
probeer_mod(noun,update).

verbinden_naar_met(afhankelijk,op,van).
verbinden_naar_met(hang_af,op,van).
verbinden_naar_met(brand,aan,op).    % op een CD branden
verbinden_naar_met(controleer,voor,op).
verbinden_naar_met(ga,in,naar).
verbinden_naar_met(kies,van,uit).
verbinden_naar_met(kijk,voor,naar).
verbinden_naar_met(kom_overeen,naar,met).
verbinden_naar_met(meld_aan,naar,bij).
verbinden_naar_met(scan,van,op).
verbinden_naar_met(sluit_aan,aan,op).
verbinden_naar_met(sluit_aan,in,op).
verbinden_naar_met(stuur,tot,naar).
verbinden_naar_met(verbind,aan, met).
verbinden_naar_met(verbind,naar,met).
verbinden_naar_met(verbind,op,  met).
verbinden_naar_met(verschil,volgens,'naar gelang').
verbinden_naar_met(vraag,voor,om).
verbinden_naar_met(zoek,voor,naar).

draai_on(draaien,schakel_in).  % of uit

phone(mobiel,cellphone,telefoon).

% unlikely_vc(gebruikt,adj).

app_noun(koppeling).
app_noun(letter).  % de letter E
app_noun(link).
app_noun(menu).
app_noun(optie).
app_noun(tab_blad).
app_noun(toets).
app_noun(vak_DIM).
app_noun(widget).

re_lemma(Lemma,NLemma):-
    atom_concat('re-',Lemma0,Lemma),!,
    re_lemma0(Lemma0,NLemma).
re_lemma(Lemma,NLemma):-
    atom_concat(re,Lemma0,Lemma),
    re_lemma0(Lemma0,NLemma).

re_lemma0(activate,       activeer).
re_lemma0(boot,           start_op).
re_lemma0(configure,      configureer).
re_lemma0(connect,        verbind).
re_lemma0(enter,          voer_in).
re_lemma0(instal,         installeer).
re_lemma0(install,        installeer).
re_lemma0(open,           open).
re_lemma0(plug,           sluit_aan).

sleutel(kapot,sleutel,toets).

invullen_in(typ_in,in).
invullen_in(vul_in,in).

name(adt_lex(_,_,_,name,_)).
name(adt_lex(_,C,_,_,_)):-
    alpino_unknowns:starts_with_capital(C).
name(adt_lex(_,C,_,_,_)):-
    alpino_lex:qtleap_hide_it_prefix(C).

%% names prefixed by :
select_colon_arg([D0|Ds],[D|Ds],ColonArg) :-
    select_colon_arg_tree(D0,D,ColonArg).
select_colon_arg([D|Ds0],[D|Ds],ColonArg) :-
    select_colon_arg(Ds0,Ds,ColonArg).
select_colon_arg([Colon,Arg|Ds],Ds,ColonArg):-
    Colon = tree(r(mod,adt_lex(_,':',_,_,_)),[]),
    Arg   = tree(r(_,ColonArg),[]),
    name(ColonArg).

select_colon_arg_tree(tree(Node1,Ds0),tree(Node,Ds),ColonArg) :-
    select_colon_arg(Ds0,Ds1,ColonArg),
    treat_unary(Ds1,Node1,Node,Ds).

%% ja/nee
select_tag_arg([D0|Ds],[D|Ds],Arg) :-
    select_tag_arg_tree(D0,D,Arg).
select_tag_arg([D|Ds0],[D|Ds],Arg) :-
    select_tag_arg(Ds0,Ds,Arg).
select_tag_arg([tree(r(Tag,Arg),[])|Ds],Ds,Arg) :-
    Tag \== tag,
    Arg = adt_lex(_,JN,_,_,_),
    member(JN,[ja,nee,neen,jawel]).

select_tag_arg_tree(tree(Node1,Ds0),tree(Node,Ds),Arg) :-
    select_tag_arg(Ds0,Ds1,Arg),
    treat_unary(Ds1,Node1,Node,Ds).

mwu_words_part([],[],[]).
mwu_words_part([tree(r(mwp,adt_lex(_,Hw,_,_,_)),[])|T0],T,[Hw|Ws]):-
    !,
    mwu_words_part(T0,T,Ws).
mwu_words_part(T,T,[]).


looks_like_inf(tree(r(mod,p(inf)),InfDs),
	       tree(r(vc,p(inf)),InfDs)):-
    member(tree(r(hd,adt_lex(_,_,_,verb,Atts)),[]),InfDs),
    \+ member(stype=_,Atts),
    \+ member(tense=_,Atts).
looks_like_inf(tree(r(mod,adt_lex(A,B,C,verb,Atts)),[]),
	       tree(r(vc,adt_lex(A,B,C,verb,Atts)),[]) ) :-
    \+ member(stype=_,Atts),
    \+ member(tense=_,Atts).
looks_like_inf(tree(r(mod,p(conj)),[tree(r(cnj,Cnj1Cat),Cnj1Ds),
				    tree(r(crd,CrdCat),CrdDs),
				    tree(r(cnj,Cnj2Cat),Cnj2Ds)]),
	       tree(r(vc,p(conj)),[tree(r(cnj,Cnj1Cat),Cnj1Ds),
				    tree(r(crd,CrdCat),CrdDs),
				    tree(r(cnj,Cnj2Cat),Cnj2Ds)])) :-
    looks_like_inf(tree(r(mod,Cnj1Cat),Cnj1Ds),_),
    looks_like_inf(tree(r(mod,Cnj2Cat),Cnj2Ds),_).

add_su_to_inf(VC0,VC,NewSu) :-
    VC0 = tree(r(vc,p(inf)),VCDs0),
    NotCmp = tree(r(cmp,_),_), \+ member(NotCmp,VCDs0),
    NotSu = tree(r(su,_),_), \+ member(NotSu,VCDs0),
    VC = tree(r(vc,p(inf)),[NewSu|VCDs0]).

add_su_to_inf(VC0,VC,NewSu) :-
    VC0 = tree(r(vc,VcHd),[]),
    VC  = tree(r(vc,p(inf)),[NewSu,tree(r(hd,VcHd),[])]).

add_su_to_inf_c(VC0,VC,NewSu) :-
    VC0 = tree(r(cnj,p(inf)),VCDs0),
    NotCmp = tree(r(cmp,_),_), \+ member(NotCmp,VCDs0),
    NotSu = tree(r(su,_),_), \+ member(NotSu,VCDs0),
    VC = tree(r(cnj,p(inf)),[NewSu|VCDs0]).

add_su_to_inf_c(VC0,VC,NewSu) :-
    VC0 = tree(r(cnj,VcHd),[]),
    VC  = tree(r(cnj,p(inf)),[NewSu,tree(r(hd,VcHd),[])]).

add_su_to_ppart(VC0,VC,NewSu) :-
    VC0 = tree(r(vc,p(ppart)),VCDs0),
    NotCmp = tree(r(cmp,_),_), \+ member(NotCmp,VCDs0),
    NotSu = tree(r(su,_),_), \+ member(NotSu,VCDs0),
    VC = tree(r(vc,p(ppart)),[NewSu|VCDs0]).

add_su_to_ppart(VC0,VC,NewSu) :-
    VC0 = tree(r(vc,VcHd),[]),
    VC  = tree(r(vc,p(ppart)),[NewSu,tree(r(hd,VcHd),[])]).

add_su_to_ppart_c(VC0,VC,NewSu) :-
    VC0 = tree(r(cnj,p(ppart)),VCDs0),
    NotCmp = tree(r(cmp,_),_), \+ member(NotCmp,VCDs0),
    NotSu = tree(r(su,_),_), \+ member(NotSu,VCDs0),
    VC = tree(r(cnj,p(ppart)),[NewSu|VCDs0]).

add_su_to_ppart_c(VC0,VC,NewSu) :-
    VC0 = tree(r(cnj,VcHd),[]),
    VC  = tree(r(cnj,p(ppart)),[NewSu,tree(r(hd,VcHd),[])]).

remove_tree(Del,tree(Node0,Ds0),tree(Node,Ds)) :-
    select(Del,Ds0,Ds1),
    treat_unary(Ds1,Node0,Node,Ds).
remove_tree(Del,tree(Node,Ds0), tree(Node,Ds)) :-
    remove_tree_ds(Ds0,Ds,Del).

remove_tree_ds([H|T],[NH|T],Del):-
    remove_tree(Del,H,NH).
remove_tree_ds([H|T0],[H|T],Del):-
    remove_tree_ds(T0,T,Del).

sg_det(alle,ieder).

unlike_cats(C1,D1,C2,D2) :-
    unlike_cats1(C1,D1,C2,D2).
unlike_cats(C1,D1,C2,D2) :-
    unlike_cats1(C2,D2,C1,D1).

unlike_cats1(adt_lex(_,_,_,adj,_),[],p(_),Ds):-
    member(tree(r(hd,adt_lex(_,_,_,verb,_)),[]),Ds).

takes_place(nemen,plaats,vind_plaats).
takes_place(neem,plaats,vind_plaats).

adj_verb_lemma(afhankelijk,hang_af).

van_natuurlijk(in,front,vooraan,adv).
van_natuurlijk(in,italics,cursief,adj).
van_natuurlijk(van,natuurlijk,natuurlijk,adj).
van_natuurlijk(voor,bijvoorbeeld,bijvoorbeeld,adv).
van_natuurlijk(voor,voorbeeld,bijvoorbeeld,adv).
