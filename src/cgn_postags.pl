%% TODO: should this look at the alt-form
%% -ie instead of hij
%% omdat [ @alt hij -ie ] dacht , dat Lien mij oogjes gaf .

%% TODO
%% breuken eenderde

:- module(alpino_cgn_postags, [ cgn_postag/9 ]).

cgn_postag(Frame0,Stem,Surf0,Q0,Q,Result,His,L0,L) :-
    frame_map(Frame0,Stem,Frame),
    (   surf_map(Surf0,Surf,Q0,Q)
    ->  true
    ;   Surf0 = Surf
    ),
    cgn_postag_c(Frame,Stem,Surf,Q0,Q,Result,His,L0,L),
    hdrug_util:debug_call(1,\+ \+ alpino_cgn_postags:report_mwu(Q0,Q,Surf,L0,L)).

report_mwu(Q0,Q,Surf,L0,[]) :-
    (   Q-Q0 =:= 1
    ->  true
    ;   report_mwu(Surf,L0)
    ->  true
    ;   format("warning: report_mwu failed~n",[])
    ).

report_mwu(Surf,L0):-
    format(user_error,"MWU#~w	",[Surf]),
    sort_mwu(L0,L),
    report_mwu(L).

%% necc for some with_dt, e.g. "welk een"
sort_mwu(L0,L) :-
    add_q0(L0,L1),
    keysort(L1,L2),
    add_q0(L,L2).

add_q0([],[]).
add_q0([cgn_postag(Q0,Q,Stem,Tag)|Tags0],
       [Q0-cgn_postag(Q0,Q,Stem,Tag)|Tags]) :-
    add_q0(Tags0,Tags).

report_mwu([]) :-
    format(user_error,"~n",[]).
report_mwu([cgn_postag(_,_,Stem0,Tag)|Tags]):-
    (   Stem0 = v_root(_,Stem)
    ->  true
    ;   Stem0 = Stem
    ),
    format(user_error,"~w:~w ",[Tag,Stem]),
    report_mwu(Tags).

cgn_postag_c(Frame,Stem,Surf,Q0,Q,Result,_His) -->
    {   Q > Q0 + 1   },
    mwu_postag(Frame,Stem,Surf,Q0,Q,Result),
    !.

cgn_postag_c(with_dt(_,Tree),_Stem,_Surf,Q0,_,_,_) -->
    !,
    with_dt_tags(Tree,Q0).

cgn_postag_c(Frame,Stem0,Surf,Q0,Q,_Cat,_) -->
    {  Q is Q0 + 1,
       exceptional_word_tag(Surf,Stem0,Stem,Frame,Tag) },
    !,
    tags(Q0,Q,Stem,Surf,Tag).

cgn_postag_c(Frame,Stem,Surf,Q0,Q,_Cat,_) -->
    {  Q is Q0 + 1,
       exceptional_word_tag(Surf,Frame,Tag) },
    !,
    tags(Q0,Q,Stem,Surf,Tag).

cgn_postag_c(Frame,Stem,Surf,Q0,Q,Result,His) -->
    history_tags(His,Q0,Q,Stem,Surf,Frame,Result),
    !.

cgn_postag_c(Proper,Stem,Surf,Q0,Q,Result,His) -->
    {  ignore_cap(Stem,Stem2,Proper,Tag) },
    !,
    cgn_postag_c(Tag,Stem2,Surf,Q0,Q,Result,His).

cgn_postag_c(Frame,Stem0,Surf,Q0,Q,_Cat,_) -->
    {  exceptional_stem_tag(Stem0,Surf,Frame,Tag,Lemma) },
    !,
    tags(Q0,Q,Lemma,Surf,Tag).

cgn_postag_c(Frame,Stem,Surf,Q0,Q,_Cat,_) -->
    {  exceptional_stem_tag(Stem,Frame,Tag,Lemma) },
    !,
    tags(Q0,Q,Lemma,Surf,Tag).

cgn_postag_c(Frame,Stem,Surf,Q0,Q,_Cat,_) -->
    {  exceptional_stem_tag(Stem,Frame,Tag) },
    !,
    tags(Q0,Q,Stem,Surf,Tag).

cgn_postag_c(particle(_),Stem0,Surf,Q0,Q,_,_) -->
    {  particle_tag(Stem0,Stem,Tag) },
    !,
    tags(Q0,Q,Stem,Surf,Tag).

cgn_postag_c(fixed_part([_]),Stem0,Surf,Q0,Q,_,_) -->
    {  particle_tag(Stem0,Stem,Tag) },
    !,
    tags(Q0,Q,Stem,Surf,Tag).

cgn_postag_c(Frame,Stem,Surf,Q0,Q,_Cat,_) -->
    {  stem_dependent_tag(Frame,Stem,Tag) },
    !,
    tags(Q0,Q,Stem,Surf,Tag).

cgn_postag_c(Frame,Stem0,Surf,Q0,Q,_Cat,_) -->
    {  stem_dependent_tag(Frame,Stem0,Stem,Tag) },
    !,
    tags(Q0,Q,Stem,Surf,Tag).

cgn_postag_c(Frame,Stem,Surf,Q0,Q,Cat,_) -->
    {  context_dependent_tag_lemma(Frame,Tag,Stem,Surf,Stem1,Q0,Q,Cat) },
    !,
    tags(Q0,Q,Stem1,Surf,Tag).

cgn_postag_c(number(hoofd(Agr)),Stem,Surf,Q0,Q,Cat,_) -->
    {  context_dependent_tag(number(hoofd(Agr)),Tag,Stem,Q0,Q,Cat) },
    !,
    number_tags(Q0,Q,Stem,Surf,Tag).

cgn_postag_c(Frame,Stem,Surf,Q0,Q,Cat,_) -->
    {  context_dependent_tag(Frame,Tag,Stem,Q0,Q,Cat) },
    !,
    tags(Q0,Q,Stem,Surf,Tag).

cgn_postag_c(amount_meas_mod_noun(_,_,_),Stem,_,Q0,Q,_,_) -->
    !,
    amount_tags(Q0,Q,Stem).

cgn_postag_c(Frame,Stem,Surf,Q0,Q,_Cat,_) -->
    {  Q is Q0 + 1,
       cgn_postag_c(Frame,Tag) },
    !,
    tags(Q0,Q,Stem,Surf,Tag).

cgn_postag_c(robust_skip,Stem,_,Q0,Q,_,_) -->
    !,
    guess_tags(Q0,Q,robust_skip,Stem).

cgn_postag_c(skip,Stem,_,Q0,Q,_,_) -->
    !,
    guess_tags(Q0,Q,skip,Stem).

cgn_postag_c(fixed_part(P),Stem,_,Q0,Q,_,_) -->
    {  atom(Stem),
       alpino_util:split_atom(Stem," ",Stems),
       length(Stems,Len),
       Q is Len + Q0
    },
    guess_tags(Q0,Q,fixed_part(P),Stems),
    !.

%% en/of written as 'en / of'
cgn_postag_c(Frame,Stem,Surf,Q0,Q,_,_) -->
    {  atom(Surf),
       alpino_util:split_atom(Surf," ",SurfEls),
       length(SurfEls,Len),
       (   hdrug_util:concat_all(SurfEls,Stem,'')
       ;   hdrug_util:concat_all(SurfEls,Stem,'-')
       ),
       Len is Q-Q0
    },
    guess_tag_list(SurfEls,Frame,Q0,Q),
    !.

cgn_postag_c(Frame,Stem,Surf,Q0,Q,_,_) -->
    {  format(user_error,"error: no cgn tag for ~w ~w ~w~n",[Surf,Stem,Frame]) },
    guess_tags(Q0,Q,Frame,Surf).

add_tags([],Q,Q,_Tag) --> [].
add_tags([Stem|Stems],Q0,Q,Tag) -->
    [cgn_postag(Q0,Q1,Stem,Tag)],
    {  Q1 is Q0 + 1 },
    add_tags(Stems,Q1,Q,Tag).

tags(Q0,Q,Stem0,Surf,Tag,L0,L) :-
    lemma_map(Stem0,Stem),
    alpino_util:split_atom(Surf," ",Surfs),
    Len is Q-Q0,
    (   Len =:= 1
    ->  L0 = [cgn_postag(Q0,Q,Stem,Tag)|L]
    ;	alpino_util:split_atom(Stem," ",Stems),
	length(Stems,Len2),
	(   Len =:= Len2
	->  new_tags(Stems,Surfs,Q0,Q,Tag,L0,L)
	;   alpino_util:split_atom(Stem,"_",Stems2),
	    length(Stems2,Len3),
	    (   Len =:= Len3
	    ->  new_tags(Stems2,Surfs,Q0,Q,Tag,L0,L)
	    ;   new_tags(Surfs,Surfs,Q0,Q,Tag,L0,L)
	    )
	)
    ).

new_tags([],_,Q,Q,_) --> [].
new_tags([Stem0|Stems],[Surf|Surfs],Q0,Q,Tag) -->
    {  Q1 is Q0 + 1,
       lemma_map(Stem0,Stem)
    },
    first_m_tag(Stems,Stem,Surf,Q0,Q1,Tag),
    new_tags(Stems,Surfs,Q1,Q,Tag).

first_m_tag([],Stem,_,Q0,Q,Tag) -->
    [ cgn_postag(Q0,Q,Stem,Tag) ].
first_m_tag([_|_],Stem,_,Q0,Q,Tag0) -->
    { m_tag(Tag0,Tag) },
    [ cgn_postag(Q0,Q,Stem,Tag) ].
first_m_tag([_|_],Stem,_Surf,Q0,Q,'SPEC(deeleigen)') -->
    !,
    [ cgn_postag(Q0,Q,Stem,'SPEC(deeleigen)') ].
first_m_tag([_|_],_,Surf,Q0,Q,_) -->
    guess_lex(Q0,Q,none,Surf,_),!.
first_m_tag([_|_],Stem,_Surf,Q0,Q,Tag) -->
    [ cgn_postag(Q0,Q,Stem,Tag) ].
    
m_tag('N(eigen,ev,basis,gen)','SPEC(deeleigen)').
m_tag('N(eigen,ev,dim,gen)',  'SPEC(deeleigen)').

history_tags(decap(X),Q0,Q,Stem,Surf,Frame,Result) -->
    history_tags(X,Q0,Q,Stem,Surf,Frame,Result).

history_tags(normal(X),Q0,Q,Stem,Surf,Frame,Result) -->
    history_tags(X,Q0,Q,Stem,Surf,Frame,Result).

history_tags(variant(ignore_internal_brackets,normal),Q0,Q,_,Surf,_,_) -->
    {  Q is Q0 + 1 },
    !,
    [cgn_postag(Q0,Q,Surf,'SPEC(enof)')].

history_tags(numbereeuwse,Q0,Q,_,Surf,adjective(e),_) -->
    {  Q is Q0 + 2,
       Q1 is Q0 + 1,
       atom(Surf),
       alpino_util:split_atom(Surf," ",[Rang,Eeuws]),
       alpino_lex:lexicon(number(rang),RangStem,[Rang],[],_),
       alpino_lex:lexicon(adjective(e),EeuwsStem,[Eeuws],[],_)
    },
    [cgn_postag(Q0,Q1,RangStem,'TW(rang,prenom,stan)')],
    [cgn_postag(Q1,Q, EeuwsStem,'ADJ(prenom,basis,met-e,stan)')].

history_tags('op zijn Belgisch'(normal),Q0,Q,_,Surf,pp,_) -->
    { Q is Q0 + 3, Q1 is Q0 + 1, Q2 is Q1 + 1,
      atom(Surf),
      alpino_util:split_atom(Surf," ",[Op,Zijn,Adj]),
      lists:member(Op,[op,'Op','OP']),
      (  Zijn = 'zijn', Tag = 'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)'
      ;  Zijn = 'z\'n', Tag = 'VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)'
      )      
    },
    [cgn_postag(Q0,Q1,op,'VZ(init)'),
     cgn_postag(Q1,Q2,zijn,Tag),
     cgn_postag(Q2,Q ,Adj,'ADJ(vrij,basis,zonder)')
    ].

history_tags('in mijn eentje',Q0,Q,_,Surf,pp,_) -->
    { Q is Q0 + 3, Q1 is Q0 + 1, Q2 is Q1 + 1,
      atom(Surf),
      alpino_util:split_atom(Surf," ",[Op,Zijn,eentje]),
      (  lists:member(Op,[op,'Op','OP']), OpLemma=op
      ;  lists:member(Op,[in,'In','IN']), OpLemma=in
      ),
      zijn_tag(Zijn,Tag,ZijnLemma)
    },
    [cgn_postag(Q0,Q1,OpLemma,'VZ(init)'),
     cgn_postag(Q1,Q2,ZijnLemma,Tag),
     cgn_postag(Q2,Q ,één,'TW(hoofd,nom,zonder-n,dim)')
    ].

history_tags(ten_xste,Q0,Q,Stem,_Surf,_Frame,_Result) -->
    { Q1 is Q0 + 1,
      atom(Stem),
      alpino_util:split_atom(Stem," ",[ten,Tweede]),
      alpino_lex:lexicon(number(rang),Twee,[Tweede],[],_)
    },
    [ cgn_postag(Q0,Q1,te,'VZ(versm)')],
    [ cgn_postag(Q1,Q,Twee,'TW(rang,nom,zonder-n)')].

%%% het Leonardo da Vinci-programma
history_tags(double_compound,Q0,Q,Stem,_Surf,Frame,_) -->
    {  atom(Stem),
       alpino_util:split_atom(Stem," ",Parts),
       length(Parts,Len),
       Len is Q - Q0,
       lists:append(TNY,[TimesColumnist],Parts),
       alpino_util:split_atom(TimesColumnist,"_",[Times,_]),
       lists:append(TNY,[Times],NameList),
       alpino_lex:lexicon(proper_name(_,_),_,NameList,[],_),
       cgn_postag_c(Frame,Tag) 
    },
    !,
    mwu_name_tags_(TNY,Q0,Q1),
    [ cgn_postag(Q1,Q,TimesColumnist,Tag) ].

%% The New York Times-columnist
history_tags(double_compound,Q0,Q,Stem,_,Frame,_) -->
    {  atom(Stem),
       alpino_util:split_atom(Stem," ",Parts),
       length(Parts,Len),
       Len is Q - Q0,
       lists:append(TNY,[TimesColumnist],Parts),
       alpino_util:split_atom(TimesColumnist,"_",[Times,_]),
       lists:append(TNY,[Times],NameList),
       hdrug_util:concat_all(NameList,Vreemd,' '),
       mwu_postag(Vreemd,Vreemd,Tags,Stems),
       lists:append(TagsUsed,[_],Tags),
       lists:append(StemsUsed,[_],Stems),
       cgn_postag_c(Frame,Tag) 
    },
    !,
    mwu_tags_stems(TagsUsed,StemsUsed,Q0,Q1),
    [ cgn_postag(Q1,Q,TimesColumnist,Tag) ].

history_tags(double_compound,Q0,Q,Stem,_Surf,Frame,Result) -->
    { 2 is Q-Q0,
      starts_with_capital(Stem),
      Q1 is Q0 + 1,
      atom(Stem),
      alpino_util:split_atom(Stem," ",[Stem1,Stem2])
    },
    [ cgn_postag(Q0,Q1,Stem1,'SPEC(deeleigen)')],
    cgn_postag_c(Frame,Stem2,Stem2,Q1,Q,Result,no).
    
history_tags(english_compound(normal),Q0,Q,Stem,Surf,Frame,Result) -->
    { 2 is Q-Q0,
      Q1 is Q0 + 1,
      atom(Stem),
      alpino_util:split_atom(Stem,"_",[Stem1,Stem2]),
      alpino_util:split_atom(Surf," ",[Surf1,Surf2])
    },
    guess_lex(Q0,Q1,none,Surf1,Stem1),
    cgn_postag_c(Frame,Stem2,Surf2,Q1,Q,Result,no).

%% is this one ever used?
history_tags(english_compound(normal),P0,P,Stem,_Surf,Frame,_Result) -->
    { atom(Stem),
      alpino_util:split_atom(Stem,"_",Stems),
      cgn_postag_c(Frame,Tag)
    },
    add_tags(Stems,P0,P,Tag).

history_tags(abbreviation(normal),Q0,Q,Stem,'\'t',determiner(het,nwh,nmod,pro,nparg,wkpro),_) -->
    { 1 is Q-Q0 }, 
    [ cgn_postag(Q0,Q,Stem,'LID(bep,stan,evon)') ].

history_tags(abbreviation(normal),Q0,Q,Stem,_,_,_) -->
    { 1 is Q-Q0,
      lemma_map(Stem,Stem1)}, 
    [ cgn_postag(Q0,Q,Stem1,'SPEC(afk)') ].

history_tags(slash(His1,His2),Q0,Q,Stem0,_Surf,Tag,Result) -->
    { ( Stem0 = v_root(_,Stem)
      ; Stem0 = Stem
      ),
      atom(Stem),
      alpino_util:split_atom(Stem," ",[Stem1,Stem2,Stem3]),
      Q1 is Q0 + 1,
      Q2 is Q1 + 1,
      Q  is Q2 + 1
    },
    cgn_postag_c(Tag,Stem1,Stem1,Q0,Q1,Result,His1),
    cgn_postag_c(punct(Stem2),Stem2,Stem2,Q1,Q2,Result,slash),
    cgn_postag_c(Tag,Stem3,Stem3,Q2,Q ,Result,His2).
    

history_tags(chess,Q0,Q,_,Surf,_,_) -->
    { 1 is Q-Q0 },
    [ cgn_postag(Q0,Q,Surf,'SPEC(symb)') ].

history_tags(part_verb_conjunct,Q0,Q,_,Surf,_,_) -->
    { 1 is Q-Q0 }, 
    [ cgn_postag(Q0,Q,Surf,'SPEC(afgebr)') ].

history_tags(url,Q0,Q,_,Surf,_,_) -->
    { 1 is Q-Q0 }, 
    [ cgn_postag(Q0,Q,Surf,'SPEC(symb)') ].

history_tags(quoted_name(_,_),Q0,Q,_,Surf,Frame,_) -->
    {  atom(Surf),
       atom_codes(Surf,Codes),
       alpino_util:codes_to_words(Codes,Words),
       length(Words,Len),
       Len is Q - Q0
    },
    guess_tag_list(Words,Frame,Q0,Q).

history_tags(enumeration,Q0,Q,Stem,_,_,_) -->
    { 1 is Q-Q0,
      is_a_number(Stem)
    }, 
    !,
    [ cgn_postag(Q0,Q,Stem,'TW(hoofd,vrij)') ].

history_tags(enumeration,Q0,Q,_,Surf,_,_) -->
    { 1 is Q-Q0 }, 
    !,
    [ cgn_postag(Q0,Q,Surf,'SPEC(symb)') ].

history_tags(enumeration,Q0,Q,Stem,_,Frame,_) -->
    {  atom(Stem),
       atom_codes(Stem,Codes),
       alpino_util:codes_to_words(Codes,Words),
       length(Words,Len),
       Len is Q - Q0
    },
    guess_tag_list(Words,Frame,Q0,Q).

history_tags(variant(variant21(_Lemma,L1,L2),_His),P0,P,_Stem,_,Frame,_Result) -->
    { cgn_postag_c(Frame,Tag) },
    add_tags([L1,L2],P0,P,Tag).

history_tags(variant(variant31(_Lemma,L1,L2,L3),_His),P0,P,_Stem,_,Frame,_Result) -->
    { cgn_postag_c(Frame,Tag) },
    add_tags([L1,L2,L3],P0,P,Tag).

history_tags(english_compound(normal),P0,P,Stem,_Surf,Frame,_Result) -->
    { atom(Stem),
      alpino_util:split_atom(Stem,"_",Stems),
      cgn_postag_c(Frame,Tag)
    },
    add_tags(Stems,P0,P,Tag).

history_tags(x_voor_x,P0,P,Stem,_,_,_) -->
    { atom(Stem),
      alpino_util:split_atom(Stem," ",[S,Voor,S]),
      P1 is P0 + 1,
      P2 is P1 + 1,
      P  is P2 + 1
    },
    guess_lex(P0,P1,_,S,_),
    guess_lex(P1,P2,_,Voor,_),
    guess_lex(P2,P,_,S,_).

%% garbage in, garbage out
history_tags(spaced_letters,P0,P,_,Surf,_,_) -->
    {  alpino_util:split_atom(Surf," ",ListOfLetters),
       length(ListOfLetters,Len),
       Len is P - P0
    },
    symb_tags(ListOfLetters,P0,P).

%% garbage in, garbage out
history_tags(spaced,P0,P,_,Surf,_,_) -->
    {  alpino_util:split_atom(Surf," ",ListOfLetters),
       length(ListOfLetters,Len),
       Len is P - P0
    },
    symb_tags(ListOfLetters,P0,P).

%% garbage in, garbage out
history_tags(add_space,P0,P,_,Surf,_,_) -->
    {  alpino_util:split_atom(Surf," ",ListOfLetters),
       length(ListOfLetters,Len),
       Len is P - P0
    },
    symb_tags(ListOfLetters,P0,P).

history_tags(chess,P0,P,_,Surf,_,_) -->
    {  alpino_util:split_atom(Surf," ",ListOfLetters),
       length(ListOfLetters,Len),
       Len is P - P0
    },
    symb_tags(ListOfLetters,P0,P).

history_tags(number_tiende,Q0,Q,Stem,_,Frame,_) -->
    {  atom(Stem),
       atom_codes(Stem,Codes),
       alpino_util:codes_to_words(Codes,Words),
       length(Words,Len),
       Len is Q - Q0,
       lists:append(Prefix,[Tiende],Words),
       Q1 is Q - 1,
       alpino_lex:lexicon(number(rang),RangStem,[Tiende],[],_)
    },
    !,
    guess_tag_list(Prefix,Frame,Q0,Q1),
    [cgn_postag(Q1,Q,RangStem,'TW(rang,nom,zonder-n)')].

history_tags(number_tiende,Q0,Q,Stem,_,Frame,_) -->
    {  atom(Stem),
       atom_codes(Stem,Codes),
       alpino_util:codes_to_words(Codes,Words),
       length(Words,Len),
       Len is Q - Q0
    },
    guess_tag_list(Words,Frame,Q0,Q).

guess_tag_list([],_,Q,Q) --> [].
guess_tag_list([H|T],Frame,Q0,Q) -->
    {  Q1 is Q0 + 1 },
    guess_tag(H,Frame,Q0,Q1),
    guess_tag_list(T,Frame,Q1,Q).

guess_tags(Q0,Q,Frame,[Stem]) -->
    {  Q is Q0 + 1 },
    !,
    guess_tag(Stem,Frame,Q0,Q).
guess_tags(Q0,Q,Frame,Stem) -->
    {  Q is Q0 + 1,
       atom(Stem)
    },
    !,
    guess_tag(Stem,Frame,Q0,Q).
guess_tags(Q0,Q,Frame,[Stem|Stems]) -->
    !,
    guess_stags([Stem|Stems],Q0,Q,Frame).
guess_tags(Q0,Q,robust_skip,Surf) -->
    {  alpino_lexical_analysis:tag(_,_,Q0,Q,Stem,Surf,His,Frame) },    
    cgn_postag(Frame,Stem,Surf,Q0,Q,no,His),
    !.
guess_tags(Q0,Q,skip,Surf) -->
    {  alpino_lexical_analysis:tag(_,_,Q0,Q,Stem,Surf,His,Frame) },    
    cgn_postag(Frame,Stem,Surf,Q0,Q,no,His),
    !.
guess_tags(Q0,Q,Frame,Stem) -->
    {  atom(Stem),
       alpino_util:split_atom(Stem," ",Stems)
    },
    guess_stags(Stems,Q0,Q,Frame).

guess_stags([],Q,Q,_) --> [].
guess_stags([_|Sts],Q0,Q,fixed_part([H|T])) -->
    {  Q1 is Q0 + 1 },
    guess_tag(H,fixed_part(H),Q0,Q1),
    guess_stags(Sts,Q1,Q,fixed_part(T)).
guess_stags([StemIn|Stems],Q0,Q,Frame) -->
    {  Q1 is Q0 + 1 },
    guess_lex(Q0,Q1,Frame,StemIn,_),
    !,
    guess_stags(Stems,Q1,Q,Frame).
guess_stags([StemIn|Stems],Q0,Q,Frame) -->
    {  Q1 is Q0 + 1 },
    [ cgn_postag(Q0,Q1,StemIn,'NA()') ],
    guess_stags(Stems,Q1,Q,Frame).

guess_tag(Stem,skip,Q0,Q) -->
    { punct(Stem),
      Q is Q0 + 1
    },
    !,
    [cgn_postag(Q0,Q,Stem,'LET()')].

guess_tag(Stem,skip,Q0,Q) -->
    { alpino_lexical_analysis:hesitation(Stem),
      Q is Q0 + 1
    },
    !,
    [cgn_postag(Q0,Q,Stem,'TSW()')].

guess_tag(Stem,skip,Q0,Q) -->
    { gcnd(Stem),
      Q is Q0 + 1
    },
    !,
    [cgn_postag(Q0,Q,Stem,'TSW()')].

guess_tag(Surf,_,Q0,Q) -->
    { afk(Surf,Stem,_),
      Q is Q0 + 1
    },
    !,
    [cgn_postag(Q0,Q,Stem,'SPEC(afk)')].

guess_tag(Stem0,_,Q0,Q) -->
    [cgn_postag(Q0,Q,Stem,Tag)],
    { Q is Q0 + 1,
      lassy(Stem0,Stem,Tag)
    },
    !.
guess_tag(Surf,Frame,Q0,Q) -->
    guess_lex(Q0,Q,Frame,Surf,_),
    !.
guess_tag(Stem,_,Q0,Q) -->
    { Q is Q0 + 1 },
    [cgn_postag(Q0,Q,Stem,'NA()')].

guess_lex(Q0,Q,Frame,Surf,Stem) -->
    {  var(Surf),
       !,
       alpino_lexical_analysis:thread_flag(current_input_sentence,Input),
       alpino_lexical_analysis:surface_form(Input,Q0,Q,Surf)
    },
    guess_lex(Q0,Q,Frame,Surf,Stem).

guess_lex(Q0,Q,_,Surf,Stem) -->
    {  lassy(Surf,Stem,Tag) },
    !,
    [  cgn_postag(Q0,Q,Stem,Tag) ].
guess_lex(Q0,Q,Frame0,Surf,Stem) -->
    {  alpino_lexical_analysis:tag(_,_,Q0,Q,Stem,Surf,His,Frame1),
       frame_map(Frame1,Stem,Frame),
       \+ Frame = Frame0
    },
    cgn_postag(Frame,Stem,Surf,Q0,Q,no,His),
    !.
guess_lex(Q0,Q,Frame0,Surf,Stem) -->
    {  frequent_copy_tag(Q0,Q,Stem,Surf,Frame1),
       frame_map(Frame1,Stem,Frame),
       \+ Frame = Frame0
    },
    cgn_postag(Frame,Stem,Surf,Q0,Q,no,copy_tag),
    !.

%% no longer necc?
%% used only for material that is in lexical_analysis:ignore()
%% \[ MISSING PARA \]
guess_lex(Q0,Q,Frame0,Surf,Stem) -->
    {  frequent_tag(Surf,Stem,Frame1),
       frame_map(Frame1,Stem,Frame),
       \+ Frame = Frame0
    },
    cgn_postag(Frame,Stem,Surf,Q0,Q,no,no),
    { format(user_error,"unexpected guess_lex (1): ~w ~w ~w~n",[Surf,Stem,Frame]) },
    !.
%% no longer necc?
%% only used for parts of mwu that are not in lexicon without decap
%% "Eerste Kamerfractie"
guess_lex(Q0,Q,_,Surf,Stem) -->
    {  alpino_unknowns:decap_first(Surf,Surf1),
       frequent_tag(Surf1,Stem,Frame)
    },
    cgn_postag(Frame,Stem,Surf1,Q0,Q,no,no),
    {  format(user_error,"unexpected guess_lex (2): ~w ~w ~w~n",[Surf,Stem,Frame]) },
    !.
guess_lex(Q0,Q,_,Surf,_Stem) -->
    {  atom(Surf),
       atom_concat(_,'-',Surf),
       !
    },
    [  cgn_postag(Q0,Q,Surf,'SPEC(afgebr)') ].
%% no longer necc?
%% only used for parts of mwu that are capitalized and unknown without capital too
%% "Fast forward" before it was in the list of vreemd()
guess_lex(Q0,Q,_,S,S) -->
    {  starts_with_capital(S),
       !
    },
    { format(user_error,"unexpected guess_lex (3): ~w ~w ~w~n",[Surf,Surf,'N(eigen,ev,basis,zijd,stan)']) },
    [  cgn_postag(Q0,Q,S,'N(eigen,ev,basis,zijd,stan)') ].
guess_lex(Q0,Q,_,S,S) -->
    [  cgn_postag(Q0,Q,S,'N(soort,ev,basis,zijd,stan)') ].

%% HEEL
%% heel de wereld -> vrij
%% heel Frankrijk -> prenom
context_dependent_tag_lemma(modal_adverb(adv_noun_prep),'ADJ(vrij,basis,zonder)',heel,_,heel,Q0,Q,Result) :-
    find_mother_node(Q0,Q,Result,Node),
    \+ alpino_data:indef(Node).
context_dependent_tag_lemma(modal_adverb(adv_noun_prep),'ADJ(prenom,basis,zonder)',heel,_,heel,_,_,_).

%% WIER
context_dependent_tag_lemma(determiner(pron,rwh),Postag,wier,_,wie,Q0,Q,Result) :-
    !,
    find_node(Q0,Q,Result,Node),
    (   wh_relagr_pl(Node)
    ->  Postag = 'VNW(betr,pron,gen,vol,3o,mv)'
    ;   Postag = 'VNW(betr,pron,gen,vol,3o,ev)'
    ).

context_dependent_tag_lemma(proper_name(_,SUB),Postag,Stem0,Surf,Stem,Q0,Q,Result) :-
    !,
    cgn_postag_proper(Postag,Stem0,Surf,Stem,Q0,Q,Result,SUB).

%% LIEVER
context_dependent_tag_lemma(adjective(er(adv)),'BW()',lief,_,liever,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    adv_path(Path).

%% ENIGE
context_dependent_tag_lemma(adjective(stof),'VNW(onbep,det,stan,nom,met-e,zonder-n)',enig,_,enig,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).
context_dependent_tag_lemma(adjective(stof),'VNW(onbep,det,stan,prenom,met-e,rest)',enig,_,enig,_,_,_).

%% MINDERE
context_dependent_tag_lemma(adjective(ere),'ADJ(nom,comp,met-e,zonder-n,stan)',weinig,_,weinig,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).
context_dependent_tag_lemma(adjective(ere),'VNW(onbep,grad,stan,prenom,met-e,agr,comp)',weinig,_,weinig,_Q0,_Q,_Result).

%% ?
context_dependent_tag_lemma(adjective(e),'VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)',weinig,_,weinig,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).
context_dependent_tag_lemma(adjective(e),'VNW(onbep,grad,stan,prenom,met-e,agr,basis)',weinig,_,weinig,_Q0,_Q,_Result).

%% MINSTE
context_dependent_tag_lemma(adjective(ste),'VNW(onbep,grad,stan,nom,met-e,zonder-n,sup)',weinig,_,weinig,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).
context_dependent_tag_lemma(adjective(ste),'VNW(onbep,grad,stan,prenom,met-e,agr,sup)',weinig,_,weinig,_Q0,_Q,_Result).

%% DUREND DURENDE
context_dependent_tag_lemma(np_me_adjective(no_e(_)),'WW(od,vrij,zonder)',duren,_,duren,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node),!.
context_dependent_tag_lemma(np_me_adjective(no_e(_)),'WW(od,prenom,zonder)',duren,_,duren,_Q0,_Q,_Result).
context_dependent_tag_lemma(np_me_adjective(e),'WW(od,prenom,met-e)',duren,_,duren,_Q0,_Q,_Result).

%% ENE
context_dependent_tag_lemma(adjective(e),'VNW(onbep,det,stan,nom,met-e,zonder-n)',een,_,een,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).
context_dependent_tag_lemma(adjective(e),'VNW(onbep,det,stan,prenom,met-e,evz)',een,_,een,_Q0,_Q,_Result).

%% MEESTE
context_dependent_tag_lemma(adjective(ste),Tag,veel,_,veel,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    (   nattr(Node)
    ->  Tag = 'VNW(onbep,grad,stan,vrij,zonder,sup)'
    ;   find_path(Q0,Q,Result,Path),
	(   noun_path(Path)
	->  Tag = 'VNW(onbep,grad,stan,nom,met-e,zonder-n,sup)'
	;   Tag = 'VNW(onbep,grad,stan,prenom,met-e,agr,sup)'
	)
    ).

%% adjective(meer)
context_dependent_tag_lemma(adjective(meer),PosTag,Stem0,_,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Stem0,_,PosTag)
    ;   adv_path(Path)
    ->  det_pron(Stem0,_,PosTag)
    ;   det_pron(Stem0,PosTag,_)
    ),
    (   meer_lemma(Stem0,Stem1)
    ->  Stem1 = Stem
    ;   Stem0 = Stem
    ).

%% adjective(ge_e) but adjective
context_dependent_tag_lemma(adjective(ge_e),Tag,Stem0,Surf,Stem,Q0,Q,Result) :-
    vd_is_adj(Stem0,Stem1),
    !,
    context_dependent_tag_lemma(adjective(e),Tag,Stem1,Surf,Stem,Q0,Q,Result).
context_dependent_tag_lemma(adjective(ge_no_e(Sub)),Tag,Stem0,Surf,Stem,Q0,Q,Result) :-
    vd_is_adj(Stem0,Stem1),
    !,
    context_dependent_tag_lemma(adjective(no_e(Sub)),Tag,Stem1,Surf,Stem,Q0,Q,Result).
context_dependent_tag_lemma(adjective(ge_both(Sub)),Tag,Stem0,Surf,Stem,Q0,Q,Result) :-
    vd_is_adj(Stem0,Stem1),
    !,
    context_dependent_tag_lemma(adjective(both(Sub)),Tag,Stem1,Surf,Stem,Q0,Q,Result).

%% -ENDE but adjective
context_dependent_tag_lemma(adjective(ende(_)),Tag,Aanstaand0,_,Aanstaand,Q0,Q,Result) :-
    od_is_adj(Aanstaand0,Aanstaand),
    find_path(Q0,Q,Result,Path),
    post_noun_path(Path),
    !,
    Tag = 'ADJ(postnom,basis,zonder)'.
context_dependent_tag_lemma(adjective(ende(_)),Tag,Aanstaand0,_,Aanstaand,Q0,Q,Result) :-
    od_is_adj(Aanstaand0,Aanstaand),
    find_node(Q0,Q,Result,Node),
    nattr(Node),
    !,
    Tag = 'ADJ(vrij,basis,zonder)'.
context_dependent_tag_lemma(adjective(ende(_)),Tag,Stem0,_,Stem,_Q0,_Q,_Result) :-
    od_is_adj(Stem0,Stem),
    !,
    Tag = 'ADJ(prenom,basis,met-e,stan)'.

%% -END but adjective
context_dependent_tag_lemma(adjective(end(_)),Tag,Verschil,_,Stem,Q0,Q,Result) :-
    od_is_adj(Verschil,Stem),
    find_path(Q0,Q,Result,Path),
    post_noun_path(Path),
    !,
    Tag = 'ADJ(postnom,basis,zonder)'.
context_dependent_tag_lemma(adjective(end(_)),Tag,Verschil,_,Stem,Q0,Q,Result) :-
    od_is_adj(Verschil,Stem),
    find_node(Q0,Q,Result,Node),
    nattr(Node),
    !,
    Tag = 'ADJ(vrij,basis,zonder)'.
context_dependent_tag_lemma(adjective(end(_)),Tag,Verschil,_,Stem,_Q0,_Q,_Result) :-
    od_is_adj(Verschil,Stem),
    !,
    Tag = 'ADJ(prenom,basis,zonder)'.

%% -END
%% for WW, there is no postnom
%% for end(_), there is no nom
context_dependent_tag_lemma(adjective(end(_)),Tag,Stem,_,Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node),
    !,
    Tag = 'WW(od,vrij,zonder)'.
context_dependent_tag_lemma(adjective(end(_)),Tag,Stem,_,Stem,_Q0,_Q,_Result) :-
    !,
    Tag = 'WW(od,prenom,zonder)'.

%% -ENDE
context_dependent_tag_lemma(adjective(ende(_)),Tag,Stem,_,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path),
    !,
    Tag = 'WW(od,nom,met-e,zonder-n)'.
context_dependent_tag_lemma(adjective(ende(_)),Tag,Stem,_,Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node),
    !,
    Tag = 'WW(od,vrij,zonder)'.
context_dependent_tag_lemma(adjective(ende(_)),Tag,Stem,_,Stem,_Q0,_Q,_Result) :-
    !,
    Tag = 'WW(od,prenom,met-e)'.

%% NO_E(ODET_ADV)
context_dependent_tag_lemma(adjective(no_e(odet_adv)),PosTag,Stem,_,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   adv_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   det_pron(Stem,PosTag,_)
    ).

context_dependent_tag_lemma(wh_adjective(odet_adv),PosTag,Stem,_,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   adv_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   det_pron(Stem,PosTag,_)
    ).

%% GE_NO_E
%% for WW, there is no postnom
context_dependent_tag_lemma(adjective(ge_no_e(_)),'WW(vd,vrij,zonder)',Stem,_,Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).
context_dependent_tag_lemma(adjective(ge_no_e(_)),'WW(vd,prenom,zonder)',Stem,_,Stem,_Q0,_Q,_Result).

%% GE_BOTH
context_dependent_tag_lemma(adjective(ge_both(_)),'WW(vd,vrij,zonder)',Stem,_,Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).
context_dependent_tag_lemma(adjective(ge_both(_)),'WW(vd,prenom,zonder)',Stem,_,Stem,_Q0,_Q,_Result).

%% GE_E
context_dependent_tag_lemma(adjective(ge_e),'WW(vd,nom,met-e,zonder-n)',Stem,_,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).
context_dependent_tag_lemma(adjective(ge_e),'WW(vd,prenom,met-e)',Stem,_,Stem,_Q0,_Q,_Result).

%% NO_E
context_dependent_tag_lemma(adjective(no_e(_)),'ADJ(postnom,basis,zonder)',Stem,_,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    post_noun_path(Path).
context_dependent_tag_lemma(adjective(no_e(_)),'ADJ(nom,basis,zonder,zonder-n)',Stem,_,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).
context_dependent_tag_lemma(adjective(no_e(_)),'ADJ(vrij,basis,zonder)',Stem,_,Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).
context_dependent_tag_lemma(adjective(no_e(_)),'ADJ(prenom,basis,zonder)',Stem,_,Stem,_Q0,_Q,_Result).

%% -ER (no postnom?)    
context_dependent_tag_lemma(adjective(er(_)),'ADJ(vrij,comp,zonder)',Stem,_,Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).
context_dependent_tag_lemma(adjective(er(_)),'ADJ(prenom,comp,zonder)',Stem,_,Stem,_Q0,_Q,_Result).

%% BOTH
context_dependent_tag_lemma(adjective(both(Sub)),'ADJ(postnom,basis,zonder)',Stem,_,Stem,Q0,Q,Result) :-
    Sub \= osentadv,
    find_path(Q0,Q,Result,Path),
    post_noun_path(Path).
context_dependent_tag_lemma(adjective(both(Sub)),'ADJ(vrij,basis,zonder)',Stem,_,Stem,Q0,Q,Result) :-
    Sub \= osentadv,
    find_node(Q0,Q,Result,Node),
    nattr(Node).
context_dependent_tag_lemma(adjective(both(Sub)),'ADJ(prenom,basis,zonder)',Stem,_,Stem,_Q0,_Q,_Result) :-
    Sub \= osentadv.

%% -ST
context_dependent_tag_lemma(adjective(st(_)),'ADJ(vrij,sup,zonder)',Stem,_,Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).

context_dependent_tag_lemma(adjective(aller_st(_)),'ADJ(vrij,sup,zonder)',Stem,_,Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).

%% -E but not really
context_dependent_tag_lemma(adjective(e),Tag,Stem0,Surf,Stem,Q0,Q,Result) :-
    \+ real_e(Stem0),
    context_dependent_tag_lemma(adjective(no_e(adv)),Tag,Stem0,Surf,Stem,Q0,Q,Result).

%% -E
%% for -e, there is no postnom
context_dependent_tag_lemma(adjective(e),'ADJ(nom,basis,met-e,zonder-n,stan)',Stem,_,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).
context_dependent_tag_lemma(adjective(e),'ADJ(prenom,basis,met-e,stan)',Stem,_,Stem,_Q0,_Q,_Result).

%% -ERE
context_dependent_tag_lemma(adjective(ere),'ADJ(nom,comp,met-e,zonder-n,stan)',Stem,_,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).
context_dependent_tag_lemma(adjective(ere),'ADJ(prenom,comp,met-e,stan)',Stem,_,Stem,_Q0,_Q,_Result).

%% -STE
context_dependent_tag_lemma(adjective(ste),'ADJ(nom,sup,met-e,zonder-n,stan)',Stem,_,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).
context_dependent_tag_lemma(adjective(ste),'ADJ(prenom,sup,met-e,stan)',Stem,_,Stem,_Q0,_Q,_Result).

%% STOF
context_dependent_tag_lemma(adjective(stof),'ADJ(nom,basis,met-e,zonder-n,stan)',Stem,_,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).
context_dependent_tag_lemma(adjective(stof),'ADJ(prenom,basis,zonder)',Stem,_,Stem,_Q0,_Q,_Result).


context_dependent_tag(preposition(_,_),'VZ(fin)',_Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    alpino_data:slashed_prep(Node).

context_dependent_tag(pronoun(nwh,je,sg,de,both,def,wkpro),PosTag,_,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    (   \+ alpino_data:nominative(Node)
    ->  PosTag = 'VNW(pr,pron,obl,red,2v,getal)'
    ;   PosTag = 'VNW(pers,pron,nomin,red,2v,ev)'
    ).

context_dependent_tag(pronoun(nwh,u,sg,de,both,def),PosTag,u,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    (   \+ alpino_data:nominative(Node)
    ->  PosTag = 'VNW(pr,pron,obl,vol,2,getal)'
    ;   PosTag = 'VNW(pers,pron,nomin,vol,2b,getal)'
    ).

context_dependent_tag(pronoun(nwh,u,sg,de,both,def),PosTag,uzelf,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    (   \+ alpino_data:nominative(Node)
    ->  PosTag = 'VNW(pr,pron,obl,nadr,2,getal)'
    ;   PosTag = 'VNW(pers,pron,nomin,nadr,2b,getal)'
    ).

context_dependent_tag(determiner(geen,nwh,mod,pro,yparg,nwkpro,geen),PosTag,geen,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(geen,_,PosTag)
    ;   det_pron(geen,PosTag,_)
    ).

context_dependent_tag(determiner(de,nwh,nmod,pro,yparg),PosTag,gene,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(gene,_,PosTag)
    ;   det_pron(gene,PosTag,_)
    ).

context_dependent_tag(determiner(welk,rwh,nmod,pro,yparg),PosTag,welk,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(welk,_,PosTag)
    ;   det_pron(welk,PosTag,_)
    ).

context_dependent_tag(determiner(welke,rwh,nmod,pro,yparg),PosTag,welk,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(welke,_,PosTag)
    ;   det_pron(welke,PosTag,_)
    ).

context_dependent_tag(determiner(de,nwh,mod,pro,yparg),PosTag,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   det_pron(Stem,PosTag,_)
    ).

context_dependent_tag(determiner(zulke,nwh,nmod,pro,yparg),PosTag,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   det_pron(Stem,PosTag,_)
    ).

context_dependent_tag(determiner(zulk),PosTag,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   det_pron(Stem,PosTag,_)
    ).

context_dependent_tag(determiner(pl_num,nwh,nmod,pro,yparg),PosTag,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   det_pron(Stem,PosTag,_)
    ).

context_dependent_tag(determiner(de,nwh,nmod,pro,_Nparg),PosTag,Deze,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Deze,_,PosTag)
    ;   det_pron(Deze,PosTag,_)
    ).

context_dependent_tag(determiner(het,nwh,nmod,pro,nparg,wkpro),PosTag,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   det_pron(Stem,PosTag,_)
    ).

context_dependent_tag(determiner(het,nwh,mod,pro,yparg),PosTag,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   det_pron(Stem,PosTag,_)
    ).

context_dependent_tag(determiner(wat,nwh,mod,pro,yparg),PosTag,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   det_pron(Stem,PosTag,_)
    ).

context_dependent_tag(determiner(wat,nwh,mod,pro,nparg),PosTag,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   det_pron(Stem,PosTag,_)
    ).

context_dependent_tag(determiner(_,nwh,nmod,pro,nparg),PosTag,Stem,Q0,Q,Result):-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   det_pron(Stem,PosTag,_)
    ).


context_dependent_tag(adj_number(enkele),'VNW(onbep,det,stan,nom,met-e,zonder-n)',_,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    number_nom_path(Path).

context_dependent_tag(adj_number(pl_num),'VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)',_,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    number_vrij_path(Path).

context_dependent_tag(number(rang),'ADJ(nom,sup,met-e,zonder-n,stan)',laat,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).

context_dependent_tag(number(rang),'ADJ(nom,sup,met-e,zonder-n,stan)',laat,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    pronoun_path(Path).

context_dependent_tag(number(rang),'ADJ(prenom,sup,met-e,stan)',laat,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    \+ noun_path(Path),
    \+ pronoun_path(Path).

context_dependent_tag(number(rang),'ADJ(nom,sup,met-e,zonder-n,stan)',voor_laat,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).

context_dependent_tag(number(rang),'ADJ(nom,sup,met-e,zonder-n,stan)',voor_laat,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    pronoun_path(Path).

context_dependent_tag(number(rang),'ADJ(prenom,sup,met-e,stan)',voor_laat,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    \+ noun_path(Path),
    \+ pronoun_path(Path).

context_dependent_tag(number(hoofd(_)),'TW(hoofd,nom,zonder-n,basis)',_Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    number_nom_path(Path).

context_dependent_tag(number(hoofd(_)),'TW(hoofd,vrij)',_Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    number_vrij_path(Path).

context_dependent_tag(number(hoofd(_)),'TW(hoofd,prenom,stan)',_Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    \+ number_nom_path(Path),
    \+ number_vrij_path(Path).

context_dependent_tag(pronoun(nwh,thi,sg,both,both,indef,strpro),'TW(hoofd,nom,zonder-n,basis)',_Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    number_nom_path(Path).

context_dependent_tag(number(rang),'TW(rang,nom,zonder-n)',_Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    number_vrij_path(Path).

%% todo: ze/zij in coordination e.g. hij of zij
context_dependent_tag(pronoun(nwh,thi,both,de,both,def,wkpro),'VNW(pers,pron,stan,red,3,ev,fem)',ze,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    \+ \+ alpino_data:nominative(Node),  % in obj and obl, ze as plural is more likely?
    \+ alpino_data:plural(Node).

context_dependent_tag(pronoun(nwh,thi,both,de,nom,def),       'VNW(pers,pron,nomin,vol,3v,ev,fem)',zij,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    \+ alpino_data:plural(Node).

context_dependent_tag(noun(DeHet,_,SgPl),Tag,Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    (   DeHet == both
    ->  find_dehet(Node,DeHetVal,DeHetValSg,Stem)
    ;   DeHetVal = DeHet,
	DeHetValSg = DeHet
    ),
    (   SgPl == both
    ->  find_sgpl(Node,SgPlVal)
    ;   SgPlVal = SgPl
    ),
    (   is_name(Stem)
    ->  name_postag(DeHetVal,DeHetValSg,SgPlVal,'MISC',Stem,Tag,Q0,Q)
    ;   noun_postag(DeHetVal,DeHetValSg,SgPlVal,Tag)
    ).

context_dependent_tag(noun(both,_,sg),'N(soort,ev,basis,onz,stan)',_,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    alpino_data:agr(Node,Agr),
    \+ alpino_data:de(Agr).

%%%context_dependent_tag(complementizer(als),'VG(onder)',_,Q0,Q,Result) :-
%%%    find_path(Q0,Q,Result,Path),
%%%    vg_als_path(Path).

%%%vg_als_path([sbar(vp)/1|_]).


context_dependent_tag(verb(_,inf,_),'WW(inf,prenom,zonder)',_,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    prenom_path(Path).

wh_relagr_pl(Node) :-
    alpino_data:wh_relagr(Node,Agr),
    alpino_data:pl(Pl),
    \+ \+ Agr = Pl.


prenom_path([te/2,vb_v/1,adj_te_v/1|_]).
prenom_path([te/2,vb_part_v/2,adj_te_v/1|_]).

is_name(W) :-
    atom(W),
    (    sub_atom(W,_,1,_,'_')
    ->   complex_name(W)
    ;    is_simple_name(W)
    ).

%% right hand head rule: you are a name if
%% rightmost morpheme is
complex_name(W) :-
    atom(W),
    alpino_util:split_atom(W,"_",Words0),
    lists:last(Words0,L),
    is_simple_name(L),
    \+ post_h(Words0).

post_h([A,_]) :-
    post_h_word(A,_).

post_h_word(bewind,het).
post_h_word(commissie,de).
post_h_word(kabinet,het).
post_h_word(regering,de).
post_h_word(verslag,het).
post_h_word(zaak,de).

is_simple_name(W) :-
    atom(W),
    atom_codes(W,[F,G|_T]),
    alpino_latin1:isupper(F),
    alpino_latin1:islower(G).

exceptional_stem_tag(Stem,Surf,_,'SPEC(vreemd)',Surf) :-
    vreemd_lemma(Stem).

exceptional_stem_tag(Stem,Surf,_,'SPEC(enof)',Surf) :-
    enof_lemma(Stem).

% this should not happen
exceptional_stem_tag(Var,_,_,_) :-
    var(Var),
    !,
    format(user_error,"Huh? error: Variable stem...~n",[]),
    fail.

exceptional_stem_tag(zogezegd,adjective(no_e(adv)),                 'BW()',zogezegd).

exceptional_stem_tag(meest,nominalized_adjective,                   'VNW(onbep,grad,stan,nom,met-e,mv-n,sup)',veel).
exceptional_stem_tag(meest,nominalized_super_adjective,             'VNW(onbep,grad,stan,nom,met-e,mv-n,sup)',veel).
exceptional_stem_tag(veel,adjective(st(_)),                         'VNW(onbep,grad,stan,vrij,zonder,sup)',   veel).    % meest
exceptional_stem_tag(veel,adjective(aller_st(_)),                         'VNW(onbep,grad,stan,vrij,zonder,sup)',   veel).    % allermeest
exceptional_stem_tag(weinig,adjective(st(_)),                       'VNW(onbep,grad,stan,vrij,zonder,sup)',   weinig).  % minst
exceptional_stem_tag(weinig,adjective(aller_st(_)),                       'VNW(onbep,grad,stan,vrij,zonder,sup)',   weinig).  % allerminst
exceptional_stem_tag('z\'n',determiner(pron),                       'VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)',zijn).

exceptional_stem_tag(één,pronoun(nwh,thi,sg,de,both,indef,strpro),  'TW(hoofd,nom,zonder-n,basis)',één).

exceptional_stem_tag(twee,    noun(de,count,pl),   'TW(hoofd,nom,mv-n,basis)', twee).
exceptional_stem_tag(drie,    noun(de,count,pl),   'TW(hoofd,nom,mv-n,basis)', drie).
exceptional_stem_tag(vier,    noun(de,count,pl),   'TW(hoofd,nom,mv-n,basis)', vier).
exceptional_stem_tag(vijf,    noun(de,count,pl),   'TW(hoofd,nom,mv-n,basis)', vijf).
exceptional_stem_tag(zes,     noun(de,count,pl),   'TW(hoofd,nom,mv-n,basis)', zes).
exceptional_stem_tag(zeven,   noun(de,count,pl),   'TW(hoofd,nom,mv-n,basis)', zeven).
exceptional_stem_tag(acht,    noun(de,count,pl),   'TW(hoofd,nom,mv-n,basis)', acht).
exceptional_stem_tag(negen,   noun(de,count,pl),   'TW(hoofd,nom,mv-n,basis)', negen).
exceptional_stem_tag(tien,    noun(de,count,pl),   'TW(hoofd,nom,mv-n,basis)', tien).
exceptional_stem_tag(twintig, noun(de,count,pl),   'TW(hoofd,nom,mv-n,basis)', twintig).
exceptional_stem_tag(dertig,  noun(de,count,pl),   'TW(hoofd,nom,mv-n,basis)', dertig).
exceptional_stem_tag(vijftig, noun(de,count,pl),   'TW(hoofd,nom,mv-n,basis)', vijftig).

exceptional_stem_tag(elf,       noun(de,count,pl), 'TW(hoofd,nom,mv-n,basis)', elf).
exceptional_stem_tag(twaalf,    noun(de,count,pl), 'TW(hoofd,nom,mv-n,basis)', twaalf).
exceptional_stem_tag(dertien,   noun(de,count,pl), 'TW(hoofd,nom,mv-n,basis)', dertien).
exceptional_stem_tag(veertien,  noun(de,count,pl), 'TW(hoofd,nom,mv-n,basis)', veertien).
exceptional_stem_tag(vijftien,  noun(de,count,pl), 'TW(hoofd,nom,mv-n,basis)', vijftien).
exceptional_stem_tag(zestien,   noun(de,count,pl), 'TW(hoofd,nom,mv-n,basis)', zestien).
exceptional_stem_tag(zeventien, noun(de,count,pl), 'TW(hoofd,nom,mv-n,basis)', zeventien).
exceptional_stem_tag(achttien,  noun(de,count,pl), 'TW(hoofd,nom,mv-n,basis)', achttien).
exceptional_stem_tag(negentien, noun(de,count,pl), 'TW(hoofd,nom,mv-n,basis)', negentien).

exceptional_stem_tag(twee_DIM,noun(het,count,pl),  'TW(hoofd,nom,mv-n,dim)',   twee).
exceptional_stem_tag(drie_DIM,noun(het,count,pl),  'TW(hoofd,nom,mv-n,dim)',   drie).
exceptional_stem_tag(vier_DIM,noun(het,count,pl),  'TW(hoofd,nom,mv-n,dim)',   vier).
exceptional_stem_tag(vijf_DIM,noun(het,count,pl),  'TW(hoofd,nom,mv-n,dim)',   vijf).
exceptional_stem_tag(zes_DIM, noun(het,count,pl),  'TW(hoofd,nom,mv-n,dim)',   zes).

exceptional_stem_tag('Koninkrijksrelaties',_,'N(soort,mv,basis)',koninkrijk_relatie).
exceptional_stem_tag('Media',_,'N(soort,mv,basis)',medium).
exceptional_stem_tag('Milieuzaken',_,'N(soort,mv,basis)',milieu_zaak).
exceptional_stem_tag('Nederlanden',_,'N(eigen,mv,basis)','Nederland').
exceptional_stem_tag('Pensioenen',_,'N(soort,mv,basis)',pensioen).
exceptional_stem_tag('Vreemdelingenzaken',_,'N(soort,mv,basis)',vreemdeling_zaak).

exceptional_stem_tag('Indiaan',noun(de,count,sg),'N(soort,ev,basis,zijd,stan)',indiaan).
exceptional_stem_tag('Indiaan',noun(de,count,pl),'N(soort,mv,basis)',indiaan).


exceptional_stem_tag(aan,adjective(_),                        'VZ(fin)',aan).
exceptional_stem_tag(apart,postn_adverb,                      'ADJ(postnom,basis,zonder)',apart).
exceptional_stem_tag(betreffen,preposition(betreffende,[]),   'WW(od,vrij,zonder)',betreffen).
exceptional_stem_tag(derden,noun(both,count,pl),              'TW(rang,nom,mv-n)',drie).
exceptional_stem_tag(dode,noun(de,count,pl),                  'ADJ(nom,basis,met-e,mv-n)',dood).
exceptional_stem_tag(dode,noun(de,count,sg),                  'ADJ(nom,basis,met-e,zonder-n,stan)',dood).
exceptional_stem_tag(detineren,noun(de,count,sg),             'WW(vd,nom,met-e,zonder-n)',detineren).
exceptional_stem_tag(detineren,noun(de,count,pl),             'WW(vd,nom,met-e,mv-n)',detineren).
exceptional_stem_tag(diens,determiner(pron),                  'VNW(aanw,pron,gen,vol,3m,ev)',die).
exceptional_stem_tag(gevangen,particle(_),                    'WW(vd,vrij,zonder)',vangen).
exceptional_stem_tag(wennen,np_adjective,                     'WW(vd,vrij,zonder)',wennen).
exceptional_stem_tag(gezien,complementizer,                   'WW(vd,vrij,zonder)',zien).
exceptional_stem_tag(gezien,preposition(_,_),                 'WW(vd,vrij,zonder)',zien).
exceptional_stem_tag(hoeveelste, wh_number(rang),             'TW(rang,prenom,stan)',hoeveel).
exceptional_stem_tag(in,adjective(_),                         'VZ(fin)',in).
exceptional_stem_tag(jammer,tag,                              'ADJ(vrij,basis,zonder)',jammer).
exceptional_stem_tag(jongstleden,postn_adverb,                'ADJ(postnom,basis,zonder)',jongstleden).
exceptional_stem_tag(jouwe,noun(both,count,both),             'VNW(bez,det,stan,vol,2v,ev,nom,met-e,zonder-n)',jou).
exceptional_stem_tag(jouwe,noun(both,count,pl),               'VNW(bez,det,stan,vol,2v,ev,nom,met-e,mv-n)',jou).
exceptional_stem_tag(junior,postn_adverb,                     'ADJ(postnom,basis,zonder)',junior).
exceptional_stem_tag(kortweg,_,                               'BW()',kortweg).
exceptional_stem_tag(man,postn_adverb,                        'N(soort,mv,basis)',man).
exceptional_stem_tag(mee,loc_adverb,                          'VZ(fin)',mee).
exceptional_stem_tag(mee,particle(mee),                       'VZ(fin)',mee).
exceptional_stem_tag(mee,preposition(met,[mee],extracted_np), 'VZ(fin)',met).
exceptional_stem_tag(ons,noun(both,count,sg),                 'VNW(bez,det,stan,vol,1,mv,nom,met-e,zonder-n)',ons).
exceptional_stem_tag(op,adjective(_),                         'VZ(fin)',op).
exceptional_stem_tag(over,adjective(_),                       'VZ(fin)',over).
exceptional_stem_tag(overledene,noun(de,count,sg),            'WW(vd,nom,met-e,zonder-n)',overlijden).
exceptional_stem_tag(overledenen,noun(de,count,pl),           'WW(vd,nom,met-e,mv-n)',overlijden).
exceptional_stem_tag(senior,postn_adverb,                     'ADJ(postnom,basis,zonder)',senior).
exceptional_stem_tag(sprake,_,                                'N(soort,ev,basis,dat)',spraak).
exceptional_stem_tag(stel,tag,                                'WW(pv,tgw,ev)',stellen).
exceptional_stem_tag(streven,noun(het,mass,sg),               'WW(inf,nom,zonder,zonder-n)',streven).
exceptional_stem_tag(tegen,adjective(_),                      'VZ(fin)',tegen).
exceptional_stem_tag(toe,preposition(tot,[],extracted_np),    'VZ(fin)',tot).
exceptional_stem_tag(toe,tag,                                 'VZ(fin)',toe).
exceptional_stem_tag(toe,particle(toe),                       'VZ(fin)',toe).
exceptional_stem_tag(uitgerekend,modal_adverb,                'WW(vd,vrij,zonder)',uit_rekenen).
exceptional_stem_tag(verder,particle(verder),                 'ADJ(vrij,comp,zonder)',ver).
exceptional_stem_tag(verdenken,noun(de,count,sg),             'WW(vd,nom,met-e,zonder-n)',verdenken).
exceptional_stem_tag(verdenken,noun(de,count,pl),             'WW(vd,nom,met-e,mv-n)',verdenken).
exceptional_stem_tag(Verdenken,noun(de,count,sg),             'WW(vd,nom,met-e,zonder-n)',Verdenken) :-
    atom_concat(_,'_verdenken',Verdenken).
exceptional_stem_tag(Verdenken,noun(de,count,pl),             'WW(vd,nom,met-e,mv-n)',Verdenken):- 
    atom_concat(_,'_verdenken',Verdenken).
exceptional_stem_tag(vrouw,postn_adverb,                      'N(soort,mv,basis)',vrouw).
exceptional_stem_tag(weg,adjective(_),                        'BW()',weg).
exceptional_stem_tag(weg,particle(weg),                       'BW()',weg).

exceptional_stem_tag(mijne, noun(both,count,both),            'VNW(bez,det,stan,vol,1,ev,nom,met-e,zonder-n)',    mijn).
exceptional_stem_tag(onze,  noun(both,count,sg),              'VNW(bez,det,stan,vol,1,mv,nom,met-e,zonder-n)',    ons).
exceptional_stem_tag(onze,  noun(both,count,pl),              'VNW(bez,det,stan,vol,1,mv,nom,met-e,mv-n)',        ons).
exceptional_stem_tag(uwe,   noun(both,count,sg),              'VNW(bez,det,stan,vol,2,getal,nom,met-e,zonder-n)', u).
exceptional_stem_tag(uwe,   noun(both,count,pl),              'VNW(bez,det,stan,vol,2,getal,nom,met-e,mv-n)',     u).
exceptional_stem_tag(jouwe, noun(both,count,both),            'VNW(bez,det,stan,vol,2v,ev,nom,met-e,zonder-n)',   jou).
exceptional_stem_tag(jouwe, noun(both,count,pl),              'VNW(bez,det,stan,vol,2v,ev,nom,met-e,mv-n)',       jou).
exceptional_stem_tag(zijne, noun(both,count,both),            'VNW(bez,det,stan,vol,3m,ev,nom,met-e,zonder-n)',   zijn).
exceptional_stem_tag(zijne, noun(both,count,pl),              'VNW(bez,det,stan,vol,3m,ev,nom,met-e,mv-n)',       zijn).
exceptional_stem_tag(hare,  noun(both,count,both),            'VNW(bez,det,stan,vol,3v,ev,nom,met-e,zonder-n)',   haar).
exceptional_stem_tag(hunne, noun(both,count,sg),              'VNW(bez,det,stan,vol,3p,mv,nom,met-e,zonder-n)',   hun).
exceptional_stem_tag(hunne, noun(both,count,pl),              'VNW(bez,det,stan,vol,3p,mv,nom,met-e,mv-n)',       hun).

exceptional_stem_tag(ge, _, 'VNW(pers,pron,nomin,red,2,getal)',ge).
exceptional_stem_tag(gij,_, 'VNW(pers,pron,nomin,vol,2,getal)',gij).

exceptional_stem_tag(mekaar,_,'VNW(recip,pron,obl,vol,persoon,mv)',mekaar).

exceptional_stem_tag(red,      tag,                                    'SPEC(afk)',redactie).
exceptional_stem_tag('red.',   tag,                                    'SPEC(afk)',redactie).
exceptional_stem_tag('Red.',   tag,                                    'SPEC(afk)',redactie).

exceptional_stem_tag(wiens,determiner(pron,rwh),                    'VNW(vb,pron,gen,vol,3m,ev)',wie).


exceptional_stem_tag(Var,_,_) :-
    var(Var),
    !,
    fail.

exceptional_stem_tag(een,pre_num_adv(_),'LID(onbep,stan,agr)').

%% for nominalized adjectives such as "invaliden" where lemma ends in -e, the tag is different!
exceptional_stem_tag(Lemma,nominalized_adjective,                'ADJ(nom,basis,zonder,mv-n)'):-
    \+ real_e(Lemma).

exceptional_stem_tag(v_root(_,Lem),Pos,Tag) :-
    exceptional_stem_tag(Lem,Pos,Tag).

exceptional_stem_tag(Stem,noun(both,both,both),'N(soort,ev,dim,onz,stan)') :-
    atom(Stem),
    atom_concat(_,je,Stem).
exceptional_stem_tag('AEX-index',proper_name(sg,_),                 'N(soort,ev,basis,zijd,stan)').
exceptional_stem_tag('AOW',proper_name(sg,_),                       'N(soort,ev,basis,zijd,stan)').
exceptional_stem_tag('BTW',proper_name(sg,_),                       'N(soort,ev,basis,zijd,stan)').
exceptional_stem_tag('ICT',proper_name(sg,_),                       'N(soort,ev,basis,zijd,stan)').
exceptional_stem_tag('Oranje',proper_name(both,'ORG'),              'N(eigen,ev,basis,onz,stan)').
exceptional_stem_tag('Oranje',proper_name(sg,'ORG'),                'N(eigen,ev,basis,onz,stan)').
exceptional_stem_tag('SRG',meas_mod_noun(_,_,_),                    'N(eigen,ev,basis,zijd,stan)').
exceptional_stem_tag('SRG',meas_mod_noun(_,_,_,_),                  'N(eigen,ev,basis,zijd,stan)').
exceptional_stem_tag('VS',proper_name(both,'LOC'),                  'N(eigen,mv,basis)').
exceptional_stem_tag('\'s',determiner(pron),                        'LID(bep,gen,evmo)').
exceptional_stem_tag('zo\'n',determiner(een),                       'VNW(aanw,det,stan,prenom,zonder,agr)').
exceptional_stem_tag(*,_,                                           'LET()').
exceptional_stem_tag(aan,complementizer(aan_het),                   'VZ(init)').
exceptional_stem_tag(achter,loc_adverb,                             'VZ(fin)').
exceptional_stem_tag(achter,pred_np_me_adjective(_),                'VZ(fin)').
exceptional_stem_tag(achteraan,loc_adverb,                          'VZ(fin)').
exceptional_stem_tag(achtereenvolgens,_,                            'BW()').
exceptional_stem_tag(af,adjective(pred(_)),                         'VZ(fin)').
exceptional_stem_tag(al,noun(both,both,pl),                         'VNW(onbep,det,stan,nom,met-e,mv-n)'). % in with_dt
exceptional_stem_tag(al,determiner(der),                            'VNW(onbep,det,gen,prenom,met-e,mv)').
exceptional_stem_tag(aldus,_,                                       'BW()').
exceptional_stem_tag(algemeen,noun(het,mass,sg),                    'ADJ(nom,basis,zonder,zonder-n)').
exceptional_stem_tag(alleen,modal_adverb,                           'BW()').
exceptional_stem_tag(alle,pronoun(nwh,thi,pl,de,both,indef),        'VNW(onbep,det,stan,nom,met-e,mv-n)').
exceptional_stem_tag(allebei,_,                                     'VNW(onbep,det,stan,vrij,zonder)').
exceptional_stem_tag(allemaal,_,                                    'BW()').
exceptional_stem_tag(allemachtig,tag,                               'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(aller,determiner(pron),                        'VNW(onbep,det,gen,prenom,met-e,mv)').
exceptional_stem_tag(allerhande,postn_adverb,                       'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(allerminst,adverb,                             'ADJ(vrij,sup,zonder)').
exceptional_stem_tag(alles,noun(het,mass,sg),                       'VNW(onbep,pron,stan,vol,3o,ev)').
exceptional_stem_tag(alsook,_,                                      'VG(neven)').
exceptional_stem_tag(alsjeblieft,_,                                 'BW()').
exceptional_stem_tag(alstublieft,_,                                 'BW()').
exceptional_stem_tag(althans,_,                                     'BW()').
exceptional_stem_tag(alvorens,_,                                    'VG(onder)').
exceptional_stem_tag(ander,post_adjective(er),                      'ADJ(postnom,basis,met-s)').
exceptional_stem_tag(ander,nominalized_compar_adjective_sg,         'ADJ(nom,basis,zonder,zonder-n)').
exceptional_stem_tag(ander,nominalized_compar_adjective,            'ADJ(nom,basis,met-e,mv-n)').
exceptional_stem_tag(andere,determiner(de,nwh,nmod,pro,yparg),      'ADJ(prenom,basis,met-e,stan)').
exceptional_stem_tag(anderhalf,number(hoofd(both)),                 'ADJ(prenom,basis,zonder)').
exceptional_stem_tag(andermans,determiner(pron),                    'VNW(onbep,pron,gen,vol,3p,ev)').
exceptional_stem_tag(annex,conj(annex),                             'VZ(init)').
exceptional_stem_tag(beetje,noun(het,count,sg),                     'N(soort,ev,dim,onz,stan)').
exceptional_stem_tag(beetje,noun(het,count,sg,_),                   'N(soort,ev,dim,onz,stan)').
exceptional_stem_tag(beetje,noun(het,count,pl),                     'N(soort,mv,dim)').
exceptional_stem_tag(beetje,noun(het,count,pl,_),                   'N(soort,mv,dim)').
exceptional_stem_tag(behalve,_,                                     'VG(onder)').
exceptional_stem_tag(belangstellende,noun(de,count,sg),             'WW(od,nom,met-e,zonder-n)').
exceptional_stem_tag(belangstellende,noun(de,count,pl),             'WW(od,nom,met-e,mv-n)').
exceptional_stem_tag(beneden,loc_adverb,                            'VZ(fin)').
exceptional_stem_tag(beschoren,np_adjective,                        'WW(vd,vrij,zonder)').
exceptional_stem_tag(best,noun(both,mass,sg),                       'ADJ(nom,sup,zonder,zonder-n)').
exceptional_stem_tag(beu,np_adjective,                              'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(beu,clause_np_adjective,                       'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(bij,conj(en),                                  'VZ(init)').
exceptional_stem_tag(bijster,_,                                     'BW()').
exceptional_stem_tag(binnen,loc_adverb,                             'VZ(fin)').
exceptional_stem_tag(blauw, noun(het,mass,sg),                      'ADJ(nom,basis,zonder,zonder-n)').
exceptional_stem_tag(boven,loc_adverb,                              'VZ(fin)').
exceptional_stem_tag(bovengronds,pp(_),                             'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(buiten,loc_adverb,                             'VZ(fin)').
exceptional_stem_tag(daarom, _,                                     'BW()').
exceptional_stem_tag(datgeen,_,                                     'VNW(aanw,det,stan,nom,met-e,zonder-n)').
exceptional_stem_tag(degeen,pronoun(nwh,thi,sg,de,both,def,strpro), 'VNW(aanw,det,stan,nom,met-e,zonder-n)').
exceptional_stem_tag(degeen,pronoun(nwh,thi,pl,de,both,def,strpro), 'VNW(aanw,det,stan,nom,met-e,mv-n)').
exceptional_stem_tag(denk, denk_ik,                                 'WW(pv,tgw,ev)').
exceptional_stem_tag(deskundig,noun(de,count,pl),                   'ADJ(nom,basis,met-e,mv-n)').
exceptional_stem_tag(deskundig,noun(de,count,sg),                   'ADJ(nom,basis,met-e,zonder-n,stan)').
exceptional_stem_tag(destijds,_,                                    'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(deze,determiner(der),                          'VNW(aanw,det,gen,prenom,met-e,rest3)').
exceptional_stem_tag(deze,determiner(de,nwh,nmod,pro,yparg),        'VNW(aanw,det,stan,prenom,met-e,rest)').
exceptional_stem_tag(deze,pronoun(nwh,thi,pl,de,both,def,strpro),   'VNW(aanw,det,stan,nom,met-e,mv-n)').
exceptional_stem_tag(die,determiner(pron),                          'VNW(aanw,pron,gen,vol,3m,ev)').
exceptional_stem_tag(doodleuk,adverb,                               'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(driemaal,noun(both,count,bare_meas),           'BW()').
exceptional_stem_tag(drug,noun(de,mass,sg),                         'N(soort,mv,basis)').
exceptional_stem_tag(duizend,noun(de,count,pl),                     'TW(hoofd,nom,mv-n,basis)').
exceptional_stem_tag(dus,_,                                         'BW()').
exceptional_stem_tag(echt,adverb,                                   'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(eenmaal,noun(_,_,_),                           'BW()').
exceptional_stem_tag(eender,pre_wh_adverb,                          'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(eens,_,                                        'BW()').
exceptional_stem_tag(eerder,tmp_app_noun,                           'ADJ(vrij,comp,zonder)').
exceptional_stem_tag(één,noun(both,count,pl),                       'TW(rang,nom,mv-n)').
exceptional_stem_tag(eindelijk,tag,                                 'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(eindje,noun(het,count,sg),                     'N(soort,ev,dim,onz,stan)').
exceptional_stem_tag(eindje,noun(het,count,pl),                     'N(soort,mv,dim)').
exceptional_stem_tag(elk,predm_adverb,                              'VNW(onbep,det,stan,vrij,zonder)').
exceptional_stem_tag(elkaar,pronoun(nwh,thi,pl,de,dat_acc,def),     'VNW(recip,pron,obl,vol,persoon,mv)').
exceptional_stem_tag(elkaar,determiner(pron),                       'VNW(recip,pron,gen,vol,persoon,mv)').
exceptional_stem_tag(elkander,determiner(pron),                     'VNW(recip,pron,gen,vol,persoon,mv)').
exceptional_stem_tag(enkel,nominalized_adjective,                   'VNW(onbep,det,stan,nom,met-e,mv-n)').
exceptional_stem_tag(ergens,_,                                      'VNW(onbep,adv-pron,obl,vol,3o,getal)').
exceptional_stem_tag(even,adjective(both(tmpadv)),                  'BW()').
exceptional_stem_tag(eventjes,_,                                    'BW()').
exceptional_stem_tag(evenveel,left_conj(_),                         'TW(hoofd,prenom,stan)').
exceptional_stem_tag(exclusief,preposition(_,_),                    'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(failliet,noun(_,_,_),                          'ADJ(nom,basis,zonder,zonder-n)').
exceptional_stem_tag(gaandeweg,_,                                   'BW()').
exceptional_stem_tag(gelijk,pre_wh_adverb,                          'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(gelijkelijk,adverb,                            'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(gene,pronoun(nwh,thi,pl,de,both,def,strpro),   'VNW(aanw,det,stan,nom,met-e,mv-n)').
exceptional_stem_tag(genoeg, _,                                     'BW()').
exceptional_stem_tag(ggg,_,                                         'SPEC(onverst)').
exceptional_stem_tag(geleden,_,                                     'BW()').
exceptional_stem_tag(gelukkig,tag,                                  'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(god,determiner(pron),                          'N(soort,ev,basis,gen)').
exceptional_stem_tag('God',determiner(pron),                        'N(eigen,ev,basis,gen)').
exceptional_stem_tag(god,pronoun(nwh,thi,sg,de,gen,def),            'N(soort,ev,basis,gen)').
exceptional_stem_tag('God',pronoun(nwh,thi,sg,de,gen,def),          'N(eigen,ev,basis,gen)').
exceptional_stem_tag(goed,tag,                                      'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(groen, noun(het,mass,sg),                      'ADJ(nom,basis,zonder,zonder-n)').
exceptional_stem_tag(grootmoeder,determiner(pron),                  'N(soort,ev,basis,gen)').
exceptional_stem_tag(grootvader,determiner(pron),                   'N(soort,ev,basis,gen)').
exceptional_stem_tag(haar,determiner(pron),                         'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)').
exceptional_stem_tag(haar,pronoun(nwh,thi,sg,de,dat_acc,def,wkpro), 'VNW(pers,pron,obl,vol,3,getal,fem)').
exceptional_stem_tag(haarzelf,_,                                    'VNW(pers,pron,obl,nadr,3v,getal,fem)').
exceptional_stem_tag(heel,intensifier,                              'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(hem,pronoun(nwh,thi,sg,de,dat_acc,def,wkpro),  'VNW(pers,pron,obl,vol,3,ev,masc)').
exceptional_stem_tag(hemzelf,_,                                     'VNW(pers,pron,obl,nadr,3m,ev,masc)').
exceptional_stem_tag(henzelf,_,                                     'VNW(pers,pron,obl,nadr,3p,mv)').
exceptional_stem_tag(hetzelfde,sentence_adverb,                     'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(hij,pronoun(nwh,thi,sg,de,nom,def),            'VNW(pers,pron,nomin,vol,3,ev,masc)').
exceptional_stem_tag(hij,pronoun(nwh,thi,sg,de,nom,def,wkpro),      'VNW(pers,pron,nomin,red,3,ev,masc)').
exceptional_stem_tag(hijzelf,_,                                     'VNW(pers,pron,nomin,nadr,3m,ev,masc)').
exceptional_stem_tag(honderd_duizend,noun(de,count,pl),             'TW(hoofd,nom,mv-n,basis)').
exceptional_stem_tag(honderd,noun(de,count,pl),                     'TW(hoofd,nom,mv-n,basis)').
exceptional_stem_tag(hoogst,intensifier,                            'BW()').
exceptional_stem_tag(hopelijk,vandaar_adverb,                       'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(hun,determiner(pron),                          'VNW(bez,det,stan,vol,3,mv,prenom,zonder,agr)').
exceptional_stem_tag(ieder,predm_adverb,                            'VNW(onbep,det,stan,vrij,zonder)').
exceptional_stem_tag(ieder,determiner(pron),                       'VNW(onbep,pron,gen,vol,3p,ev)').
exceptional_stem_tag(iemand,determiner(pron),                       'VNW(onbep,pron,gen,vol,3p,ev)').
exceptional_stem_tag(iemand,_,                                      'VNW(onbep,pron,stan,vol,3p,ev)').
exceptional_stem_tag(iets,_,                                        'VNW(onbep,pron,stan,vol,3o,ev)').
exceptional_stem_tag(ikzelf,_,                                      'VNW(pers,pron,nomin,nadr,1,ev)').
exceptional_stem_tag(immers,_,                                      'BW()').
exceptional_stem_tag(inderdaad,tag,                                 'BW()').
exceptional_stem_tag(ja,_,                                          'TSW()').
exceptional_stem_tag(je,determiner(pron),                           'VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)').
exceptional_stem_tag(jezelf,_,                                      'VNW(pr,pron,obl,nadr,2v,getal)').
exceptional_stem_tag(jong,noun(de,count,pl),                        'ADJ(nom,comp,met-e,mv-n)').
exceptional_stem_tag(jong,noun(de,count,sg),                        'ADJ(nom,comp,met-e,zonder-n,stan)').
exceptional_stem_tag(jou,determiner(pron),                          'VNW(bez,det,stan,vol,2v,ev,prenom,zonder,agr)').
exceptional_stem_tag(juist,modal_adverb,                            'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(jullie,determiner(pron),                       'VNW(bez,det,stan,nadr,2v,mv,prenom,zonder,agr)').
exceptional_stem_tag(keizer,determiner(pron),                       'N(soort,ev,basis,gen)').
exceptional_stem_tag(kerke,noun(_,_,_),                             'N(soort,ev,basis,dat)').
exceptional_stem_tag(kortom,tag,                                    'BW()').
exceptional_stem_tag(kwijt,_,                                       'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(land,determiner(pron),                         'N(soort,ev,basis,gen)').
exceptional_stem_tag(later,tmp_app_noun,                            'ADJ(vrij,comp,zonder)').
exceptional_stem_tag(liefst,modal_adverb(_),                        'BW()').
exceptional_stem_tag(liefste,tag,                                   'ADJ(nom,sup,met-e,zonder-n,stan)').
exceptional_stem_tag(man,determiner(pron),                          'N(soort,ev,basis,gen)').
exceptional_stem_tag(maximum,adjective(_),                          'N(soort,ev,basis,onz,stan)').
exceptional_stem_tag(veel,postadv_adverb,                           'VNW(onbep,grad,stan,vrij,zonder,comp)').
exceptional_stem_tag(medio,_,                                       'BW()').
exceptional_stem_tag(meerdere,determiner(pl_num,nwh,nmod,pro,yparg),'VNW(onbep,det,stan,prenom,met-e,mv)').
exceptional_stem_tag(meerdere,nominalized_adjective,                'ADJ(nom,basis,met-e,mv-n)').
exceptional_stem_tag(meisje,noun(het,count,sg),                     'N(soort,ev,dim,onz,stan)').
exceptional_stem_tag(meisje,noun(het,count,pl),                     'N(soort,mv,dim)').
exceptional_stem_tag(men,pronoun(nwh,thi,sg,de,nom,def),            'VNW(pers,pron,nomin,red,3p,ev,masc)').
exceptional_stem_tag(mezelf,_,                                      'VNW(pr,pron,obl,nadr,1,ev)').
exceptional_stem_tag(mijzelf,_,                                     'VNW(pr,pron,obl,nadr,1,ev)').
exceptional_stem_tag(mijn,determiner(pron),                         'VNW(bez,det,stan,vol,1,ev,prenom,zonder,agr)').
exceptional_stem_tag(miljard,noun(_,_,meas),                        'N(soort,ev,basis,onz,stan)').
exceptional_stem_tag(miljoen,noun(_,_,meas),                        'N(soort,ev,basis,onz,stan)').
exceptional_stem_tag(minimum,adjective(_),                          'N(soort,ev,basis,onz,stan)').
exceptional_stem_tag(minus,preposition(_,_),                        'BW()').
exceptional_stem_tag(moeder,determiner(pron),                       'N(soort,ev,basis,gen)').
exceptional_stem_tag(na,adjective(pred(_)),                         'VZ(fin)').
exceptional_stem_tag(naargelang,_,                                  'VG(onder)').
exceptional_stem_tag(namelijk,_,                                    'BW()').
exceptional_stem_tag(natuurlijk,vandaar_adverb,                     'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(natuurlijk,tag,                                'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(nee,_,                                         'TSW()').
exceptional_stem_tag(nergens,_,                                     'VNW(onbep,adv-pron,obl,vol,3o,getal)').
exceptional_stem_tag(net,adjective(no_e(adv)),                      'BW()').
exceptional_stem_tag(net,modal_adverb,                              'BW()').
exceptional_stem_tag(niemand,_,                                     'VNW(onbep,pron,stan,vol,3p,ev)').
exceptional_stem_tag(niemands,determiner(pron),                     'VNW(onbep,pron,gen,vol,3p,ev)').
exceptional_stem_tag(niets,_,                                       'VNW(onbep,pron,stan,vol,3o,ev)').
exceptional_stem_tag(niks,_,                                        'VNW(onbep,pron,stan,vol,3o,ev)').
exceptional_stem_tag(nou,tag,                                       'BW()').
exceptional_stem_tag(nu,tag,                                        'BW()').
exceptional_stem_tag(nummer,noun(both,count,sg),                    'N(soort,ev,basis,onz,stan)').
exceptional_stem_tag(komen,tag,                                     'WW(pv,tgw,ev)').
exceptional_stem_tag(kijken,tag,                                    'WW(pv,tgw,ev)').
exceptional_stem_tag(om,adjective(pred(_)),                         'VZ(fin)').
exceptional_stem_tag(ondanks,_,                                     'VZ(init)').
exceptional_stem_tag(onder,loc_adverb,                              'VZ(fin)').
exceptional_stem_tag(ondergronds,loc_adverb,                        'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(onderweg,adjective(_),                         'BW()').
exceptional_stem_tag(oneens,_,                                      'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(onmiddellijk,modal_adverb(_),                  'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(ons,determiner(pron),                          'VNW(bez,det,stan,vol,1,mv,prenom,zonder,evon)').
exceptional_stem_tag(onszelf,_,                                     'VNW(pr,pron,obl,nadr,1,mv)').
exceptional_stem_tag(onverschillig,preposition(_,_,of_sbar),        'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(op,adjective(pred(_)),                         'VZ(fin)').
exceptional_stem_tag(openbaar,noun(het,mass,sg),                    'ADJ(nom,basis,zonder,zonder-n)').
exceptional_stem_tag(overal,_,                                      'VNW(onbep,adv-pron,obl,vol,3o,getal)').
exceptional_stem_tag(overeenkomstig,preposition(_,_),               'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(overstag,_,                                    'BW()').
exceptional_stem_tag(respectievelijk,conj(_),                       'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(respectievelijk,left_conj(_),                  'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(reuze,_,                                       'BW()').
exceptional_stem_tag(richting,preposition(_,_),                     'N(soort,ev,basis,zijd,stan)').
exceptional_stem_tag(rood, noun(het,mass,sg),                       'ADJ(nom,basis,zonder,zonder-n)').
exceptional_stem_tag(rondom,pp(_),                                  'VZ(fin)').
exceptional_stem_tag(samen,_,                                       'BW()').
exceptional_stem_tag(sinds,complementizer,                          'VZ(init)').
exceptional_stem_tag(land,determiner(pron),                         'N(soort,ev,basis,gen)').
exceptional_stem_tag(schuldig,np_adjective,                         'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(sommig,nominalized_adjective,                  'VNW(onbep,det,stan,nom,met-e,mv-n)').
exceptional_stem_tag(streven,noun(sg,_,het),                        'WW(inf,nom,zonder,zonder-n)').
exceptional_stem_tag(tal,determiner(wat,nwh,mod,pro,yparg),         'N(soort,ev,basis,onz,stan)').
exceptional_stem_tag(tegemoet,_,                                    'BW()').
exceptional_stem_tag(teneinde,complementizer(inf),                  'VG(onder)').
exceptional_stem_tag(tenminste,tag,                                 'BW()').
exceptional_stem_tag(tezamen,_,                                     'BW()').
exceptional_stem_tag(thuis,_,                                       'BW()').
exceptional_stem_tag(tien_duizend,noun(de,count,pl),                'TW(hoofd,nom,mv-n,basis)').
exceptional_stem_tag(toedoen,_,                                     'WW(inf,nom,zonder,zonder-n)').
exceptional_stem_tag(tot,conj(tot),                                 'VG(neven)').
exceptional_stem_tag(tot,complementizer,                            'VZ(init)').
exceptional_stem_tag(trouwens,_,                                    'BW()').
exceptional_stem_tag(tweemaal,noun(both,count,bare_meas),           'BW()').
exceptional_stem_tag(u,determiner(pron),                            'VNW(bez,det,stan,vol,2,getal,prenom,zonder,agr)').
exceptional_stem_tag(uisluitend,modal_adverb,                       'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(uw,determiner(pron),                           'VNW(bez,det,stan,vol,2,getal,prenom,zonder,agr)').
exceptional_stem_tag(jijzelf,_,                                     'VNW(pers,pron,nomin,nadr,2v,ev)').
exceptional_stem_tag(vader,determiner(pron),                        'N(soort,ev,basis,gen)').
exceptional_stem_tag(veel,adjective(e),                             'VNW(onbep,grad,stan,prenom,met-e,agr,basis)').
exceptional_stem_tag(veel,nominalized_adjective,                    'VNW(onbep,grad,stan,nom,met-e,mv-n,basis)').
exceptional_stem_tag(veevoer,noun(both,mass,sg),                    'N(soort,ev,basis,onz,stan)').
exceptional_stem_tag(verderop,_,                                    'BW()').
exceptional_stem_tag(versus,_,                                      'VZ(init)').
exceptional_stem_tag(viermaal,noun(both,count,bare_meas),           'BW()').
exceptional_stem_tag(volwassen,noun(de,count,sg),                   'ADJ(nom,basis,met-e,zonder-n,stan)').
exceptional_stem_tag(volwassen,noun(de,count,pl),                   'ADJ(nom,basis,met-e,mv-n)').
exceptional_stem_tag(voor,complementizer,                           'VZ(init)').
exceptional_stem_tag(voor,adjective(pred(_)),                       'VZ(fin)').
exceptional_stem_tag(vooraleer,_,                                   'VG(onder)').
exceptional_stem_tag(voornamelijk,_,                                'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(waar,complementizer(_),                        'VNW(vb,adv-pron,obl,vol,3o,getal)').
exceptional_stem_tag(waard,np_adjective,                            'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(waard,clause_np_adjective,                     'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(waard,subject_sbar_pred_np_adjective,          'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(waard,subject_vp_pred_np_adjective,            'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(wat,adverb,                                    'VNW(onbep,pron,stan,vol,3o,ev)').
exceptional_stem_tag(watte,pronoun(ywh,thi,sg,het,both,indef,nparg),'VNW(vrag,pron,stan,nadr,3o,ev)').
exceptional_stem_tag(welletjes,_,                                   'BW()').
exceptional_stem_tag(wereld,determiner(pron),                       'N(soort,ev,basis,gen)').
exceptional_stem_tag(wit,determiner(pron),                          'N(soort,ev,basis,gen)').
exceptional_stem_tag(wit, noun(het,mass,sg),                        'ADJ(nom,basis,zonder,zonder-n)').
exceptional_stem_tag(xxx,_,                                         'SPEC(onverst)').
exceptional_stem_tag(zat,np_adjective,                              'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(zat,clause_np_adjective,                       'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(zat,postadj_adverb,                            'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(zeggen,tag,                                    'WW(pv,tgw,ev)').
exceptional_stem_tag(zichzelf,_,                                    'VNW(refl,pron,obl,nadr,3,getal)').
exceptional_stem_tag(zijn,determiner(pron),                         'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)').
exceptional_stem_tag(zijn,preposition(zijnde,[]),                   'WW(od,vrij,zonder)').
exceptional_stem_tag(zijzelf,_,                                     'VNW(pers,pron,nomin,nadr,3v,ev,fem)').
exceptional_stem_tag(zo,_,                                          'BW()').
exceptional_stem_tag('zo\'n',_,                                     'VNW(aanw,det,stan,prenom,zonder,agr)').
exceptional_stem_tag(zoveel,adjective(e),                           'TW(hoofd,prenom,stan)').
exceptional_stem_tag(zoveel,sbar_adjective(e),                      'TW(hoofd,prenom,stan)').
exceptional_stem_tag(zover,_,                                       'BW()').
exceptional_stem_tag(zwart,determiner(pron),                        'N(soort,ev,basis,gen)').

% hij/zij
exceptional_stem_tag(hij_zij, pronoun(nwh,thi,sg,de,nom,def),         'SPEC(enof)').
exceptional_stem_tag(zij_hij, pronoun(nwh,thi,sg,de,nom,def),         'SPEC(enof)').
% hem/haar
exceptional_stem_tag(hem_haar,pronoun(nwh,thi,sg,de,dat_acc,def),     'SPEC(enof)').
exceptional_stem_tag(haar_hem,pronoun(nwh,thi,sg,de,dat_acc,def),     'SPEC(enof)').
% zulks
exceptional_stem_tag(zulk,    pronoun(nwh,thi,sg,het,both,indef),     'VNW(aanw,pron,stan,vol,3o,ev)').
% u
exceptional_stem_tag(u,       reflexive(u,sg),                        'VNW(pr,pron,obl,vol,2,getal)').
% wien
exceptional_stem_tag(wie,     pronoun(ywh,thi,both,de,dat_acc,def),       'VNW(vb,pron,dat,vol,3p,getal)').
% ener
exceptional_stem_tag(een,     determiner(ener),                       'LID(onbep,gen,evf)').
% hunner
exceptional_stem_tag(hun,     pronoun(nwh,thi,pl,both,gen,def),       'VNW(pers,pron,gen,vol,3p,mv)').
% zijner zijns
exceptional_stem_tag(zijn,    pronoun(nwh,thi,sg,both,gen,def),       'VNW(pers,pron,gen,vol,3m,ev)').
% dezes
exceptional_stem_tag(deze,    pronoun(nwh,thi,both,both,gen,def),     'VNW(aanw,det,gen,nom,met-e,zonder-n)').
% kun -de
exceptional_stem_tag(je,      pronoun(nwh,inv,sg,de,both,def,wkpro),  'VNW(pers,pron,nomin,red,2v,ev)').
% welks
exceptional_stem_tag(welk,    determiner(pron,rwh),                   'VNW(betr,pron,gen,vol,3o,ev)').
% uwer
exceptional_stem_tag(u,       pronoun(nwh,u,both,both,gen,def),       'VNW(pers,pron,gen,vol,2,getal)').
% hoevelen
exceptional_stem_tag(hoeveel, pronoun(ywh,thi,pl,de,both,indef),      'TW(hoofd,nom,mv-n,basis)').
% jullie[pron] Vlaamse varianten
exceptional_stem_tag(jullie,  pronoun(nwh,je,pl,de,dat_acc,def),      'VNW(pers,pron,stan,nadr,2v,mv)').
exceptional_stem_tag(jullie,  pronoun(nwh,inv,both,de,nom,def),       'VNW(pers,pron,stan,nadr,2v,mv)').
exceptional_stem_tag(jullie,  pronoun(nwh,je,both,de,nom,def),        'VNW(pers,pron,stan,nadr,2v,mv)').
% jullie[refl]
exceptional_stem_tag(jullie,  reflexive(je,pl),                       'VNW(pers,pron,stan,nadr,2v,mv)').
% uzelf
exceptional_stem_tag(uzelf,   reflexive(u,sg),                        'VNW(pers,pron,nomin,nadr,2b,getal)').
% hoeveelste
% onzer
exceptional_stem_tag(ons,     pronoun(nwh,fir,pl,both,gen,def),       'VNW(pers,pron,gen,vol,1,mv)').

exceptional_stem_tag('Copa',  _,                                      'N(eigen,ev,basis,zijd,stan)').

exceptional_stem_tag(Compound,Frame,Tag) :-
    atom(Compound),
    alpino_util:split_atom(Compound,"_",[_|Parts]), % at least two parts
    lists:append(_,[Head],Parts),
    exceptional_stem_tag(Head,Frame,Tag).

exceptional_stem_tag(Num,tag,'SPEC(symb)') :-
    atom(Num),
    atom_codes(Num,[C|_]),
    hdrug_util:between(49,57,C).



%  exceptional_word_tag(meer,adjective(meer),'VNW(onbep,grad,stan,vrij,zonder,comp)').

exceptional_word_tag('-ie',pronoun(nwh,thi,sg,de,nom,def),      'VNW(pers,pron,nomin,red,3,ev,masc)').
exceptional_word_tag('\'ie',pronoun(nwh,thi,sg,de,nom,def),      'VNW(pers,pron,nomin,red,3,ev,masc)').

exceptional_word_tag(Stem,number(hoofd(_)),'SPEC(symb)') :-
    atom(Stem),
    alpino_util:split_atom(Stem,"/",[Left,Right]),
    atom_codes(Left,LeftCodes),
    atom_codes(Right,RightCodes),
    alpino_lex:number_codes_silent(_,LeftCodes),
    alpino_lex:number_codes_silent(_,RightCodes).

exceptional_word_tag(Stem,np(year),'SPEC(symb)') :-
    atom(Stem),
    alpino_util:split_atom(Stem,"/",[Left,Right]),
    atom_codes(Left,LeftCodes),
    atom_codes(Right,RightCodes),
    alpino_lex:number_codes_silent(_,LeftCodes),
    alpino_lex:number_codes_silent(_,RightCodes).

exceptional_word_tag(Word,_,'SPEC(afgebr)') :-
    atom(Word),
    atom_concat(_,'*a',Word).

exceptional_word_tag('{',_,                                          'LET()').
exceptional_word_tag('}',_,                                          'LET()').

exceptional_word_tag('\'m', pronoun(nwh,thi,sg,de,dat_acc,def,wkpro),'VNW(pers,pron,obl,red,3,ev,masc)').
exceptional_word_tag('z\'n',_,                                       'VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)').
exceptional_word_tag('Z\'n',_,                                       'VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)').
exceptional_word_tag('m\'n',determiner(pron),                        'VNW(bez,det,stan,red,1,ev,prenom,zonder,agr)').
exceptional_word_tag('M\'n',determiner(pron),                        'VNW(bez,det,stan,red,1,ev,prenom,zonder,agr)').
exceptional_word_tag('d\'r',determiner(pron),                        'VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)').
exceptional_word_tag('D\'r',determiner(pron),                        'VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)').
exceptional_word_tag(graden,noun(de,count,meas),                     'N(soort,mv,basis)').
exceptional_word_tag('-ie',pronoun(nwh,thi,sg,de,nom,def),           'VNW(pers,pron,nomin,red,3,ev,masc)').
%% alle/allen both stem al, but now we miss the capitalized versions
exceptional_word_tag(alle,predm_adverb,                             'VNW(onbep,det,stan,nom,met-e,zonder-n)').
exceptional_word_tag(allen,predm_adverb,                             'VNW(onbep,det,stan,nom,met-e,mv-n)').
exceptional_word_tag(beide,pronoun(nwh,thi,pl,de,both,indef),        'VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)').
exceptional_word_tag(beide,predm_adverb,                             'VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)').
exceptional_word_tag(beiden,predm_adverb,                            'VNW(onbep,grad,stan,nom,met-e,mv-n,basis)').
exceptional_word_tag('Beide',pronoun(nwh,thi,pl,de,both,indef),        'VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)').
exceptional_word_tag('Beide',predm_adverb,                             'VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)').
exceptional_word_tag('Beiden',predm_adverb,                            'VNW(onbep,grad,stan,nom,met-e,mv-n,basis)').
exceptional_word_tag(gevangene,_,                                      'WW(vd,nom,met-e,zonder-n)').
exceptional_word_tag(gevangenen,_,                                     'WW(vd,nom,met-e,mv-n)').
exceptional_word_tag(krijgsgevangene,_,                                'WW(vd,nom,met-e,zonder-n)').
exceptional_word_tag(krijgsgevangenen,_,                               'WW(vd,nom,met-e,mv-n)').
exceptional_word_tag('Gevangene',_,                                    'WW(vd,nom,met-e,zonder-n)').
exceptional_word_tag('Gevangenen',_,                                   'WW(vd,nom,met-e,mv-n)').
exceptional_word_tag('Krijgsgevangene',_,                              'WW(vd,nom,met-e,zonder-n)').
exceptional_word_tag('Krijgsgevangenen',_,                             'WW(vd,nom,met-e,mv-n)').
exceptional_word_tag('\'k',pronoun(nwh,fir,sg,de,nom,def),           'VNW(pers,pron,nomin,red,1,ev)').

exceptional_word_tag('\'r',pronoun(nwh,thi,sg,de,dat_acc,def,wkpro),  'VNW(pers,pron,obl,red,3v,getal,fem)').
exceptional_word_tag('\'r',determiner(pron),                          'VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)').
exceptional_word_tag('d\'r',pronoun(nwh,thi,sg,de,dat_acc,def,wkpro), 'VNW(pers,pron,obl,red,3v,getal,fem)').
exceptional_word_tag('d\'r',determiner(pron),                         'VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)').

exceptional_word_tag(mij,reflexive(fir,sg),                  'VNW(pr,pron,obl,vol,1,ev)').
exceptional_word_tag(mij,pronoun(nwh,fir,sg,de,dat_acc,def), 'VNW(pr,pron,obl,vol,1,ev)').
exceptional_word_tag('Mij',reflexive(fir,sg),                  'VNW(pr,pron,obl,vol,1,ev)').
exceptional_word_tag('Mij',pronoun(nwh,fir,sg,de,dat_acc,def), 'VNW(pr,pron,obl,vol,1,ev)').

%% kom de gij mee
exceptional_word_tag(de,    pronoun(nwh,inv,sg,both,both,def),       'VNW(pers,pron,dial)').

exceptional_word_tag(tweetjes,_,'TW(hoofd,nom,mv-n,dim)').
exceptional_word_tag(drietjes,_,'TW(hoofd,nom,mv-n,dim)').
exceptional_word_tag(viertjes,_,'TW(hoofd,nom,mv-n,dim)').
exceptional_word_tag(vijfjes,_,'TW(hoofd,nom,mv-n,dim)').
exceptional_word_tag(zesjes,_,'TW(hoofd,nom,mv-n,dim)').
exceptional_word_tag(zeventjes,_,'TW(hoofd,nom,mv-n,dim)').
exceptional_word_tag(negentjes,_,'TW(hoofd,nom,mv-n,dim)').

%%% exceptional_word_tag/5
exceptional_word_tag(Word,_,Word,Tag,'SPEC(symb)') :-
    symb(Word,Tag).

exceptional_word_tag(eentje,één,één, pronoun(nwh,thi,sg,de,both,indef,strpro),  'TW(hoofd,nom,zonder-n,dim)').
exceptional_word_tag(ééntje,één,één, pronoun(nwh,thi,sg,de,both,indef,strpro),  'TW(hoofd,nom,zonder-n,dim)').
exceptional_word_tag('Eentje',één,één, pronoun(nwh,thi,sg,de,both,indef,strpro),  'TW(hoofd,nom,zonder-n,dim)').
exceptional_word_tag('Eéntje',één,één, pronoun(nwh,thi,sg,de,both,indef,strpro),  'TW(hoofd,nom,zonder-n,dim)').

exceptional_word_tag(allen,  al,al,_,                                'VNW(onbep,det,stan,nom,met-e,mv-n)').
exceptional_word_tag('Allen',al,al,_,                                'VNW(onbep,det,stan,nom,met-e,mv-n)').
exceptional_word_tag('COMPLEET',_,compleet,robust_skip,'ADJ(vrij,basis,zonder)').
exceptional_word_tag('BETA',_,'BETA',robust_skip,'SPEC(vreemd)').
exceptional_word_tag('MISSING',_,'MISSING',robust_skip,'SPEC(vreemd)').
exceptional_word_tag('TABLE',_,'TABLE',robust_skip,'SPEC(vreemd)').
exceptional_word_tag('INDEX',_,index,robust_skip,'N(soort,ev,basis,zijd,stan)').
exceptional_word_tag('PARA',_,'PARA',robust_skip,'SPEC(vreemd)').
exceptional_word_tag('KWOOT',_,quote,_,'N(soort,ev,basis,zijd,stan)').
exceptional_word_tag('Kwoot',_,quote,_,'N(soort,ev,basis,zijd,stan)').
exceptional_word_tag(kwoot,_,quote,_,'N(soort,ev,basis,zijd,stan)').
exceptional_word_tag(kwoots,_,quote,_,'N(soort,mv,basis)').

exceptional_word_tag(nl,_,'Nederland',noun(_,_,_),'SPEC(afk)').
exceptional_word_tag(nl,_,namelijk,sentence_adverb,'SPEC(afk)').
exceptional_word_tag(Word,_,Stem,Tag,'SPEC(afk)') :-
    afk(Word,Stem,Tag).

afk('a.k.a','also known as',_).
afk('B.B.','Bescherming Bevolking',_).
afk('b.d.','buiten dienst',_).
afk('b.d.','buiten dienst',_).
afk('blz',bladzijde,_).
afk('blz.',bladzijde,_).
afk('Blz',bladzijde,_).
afk('Blz.',bladzijde,_).
afk(brt,'bruto register ton',_).
afk('Bulg.','Bulgaars',_).
afk('b.v.',bijvoorbeeld,_).
afk('BW','Burgerlijk Wetboek',_).
afk('ca',circa,_).
afk('ca.',circa,_).
afk('Ca.',circa,_).
afk('cf.',confer,_).
afk(cfr,confer,_).
afk('Chr.','Christus',_).
afk('ct.',cent,_).
afk('c.q.','casu quo',_).
afk('cq','casu quo',_).
afk('c.s.','cum suis',_).
afk('d.d.','de dato',_).
afk('d.i.','dit is',_).
afk('dierl.',dierlijk,_).
afk(dmv,'door middel van',_).
afk('d.w.z.','dat wil zeggen',_).
afk('dwz.','dat wil zeggen',_).
afk('e.a.','en andere',_).
afk('e.d.','en dergelijke',_).
afk('e.d.','en dergelijke',_).
afk('e.v.',eerstvolgende,_).
afk(evt,eventueel,_).
afk('i.h.b.','in het bijzonder',_).
afk('incl.',inclusief,_).
afk('i.p.v.','in plaats van',_).
afk('i.s.m.','in samenwerking met',_).
afk('i.v.m.','in verband met',_).
afk('j.l.',jongstleden,_).
afk('jr',junior,_).
afk('jr.',junior,_).
afk('max.',maximaal,_).
afk('mln',miljoen,_).
afk('mln.',miljoen,_).
afk('m.n.','met name',_).
afk('n.a.g.','niet afzonderlijk genoemd',_).
afk('n.a.v.','naar aanleiding van',_).
afk('N.B.','nota bene',_).
afk('n.Chr.','na Christus',_).
afk('Ned','Nederland',_).
afk('Nederl.','Nederlands',_).
afk('NL','Nederland',_).
afk('No.',numero,_).
afk('nom.',nominaal,_).
afk('nr.',nummer,_).
afk('Nr.',nummer,_).
afk(nvdr,'noot van de redactie',_).
afk('n.v.t.','niet van toepassing',_).
afk('N.v.t.','niet van toepassing',_).
afk('o.a.','onder ander',_).
afk('O.a.','onder ander',_).
afk('O.M.','Openbaar Ministerie',proper_name(sg,'ORG')).
afk('OM','Openbaar Ministerie',proper_name(sg,'ORG')).
afk('o.m.','onder meer',_).
afk('O.m.','onder meer',_).
afk('plant.',plantaardig,_).
afk('plm','plus minus',_).
afk('plm.','plus minus',_).
afk('resp.',respectievelijk,_).
afk('r.k.','rooms-katholiek',_).
afk('t.a.v.','ten aanzien van',_).
afk('t/m','tot en met',_).
afk('t.o.v.','ten opzichte van',_).
afk(tov,'ten opzichte van',_).
afk('t.w.','te weten',_).
afk('t.z.t.','te zijner tijd',_).
afk('v.Chr.','voor Christus',_).
afk('v.C.','voor Christus',_).
afk('v.j.','vorig jaar',_).
afk('vs.',versus,_).
afk('v.',voor,_).
afk('z.g.',zogenaamd,_).
afk('z.i.','zijn inzien',_).

stem_dependent_tag(cleft_het_noun,het,'VNW(pers,pron,stan,red,3,ev,onz)').
stem_dependent_tag(cleft_het_noun,dat,'VNW(aanw,pron,stan,vol,3o,ev)').
stem_dependent_tag(cleft_het_noun,dit,'VNW(aanw,pron,stan,vol,3o,ev)').
stem_dependent_tag(modal_adverb,enkel,'BW()').
stem_dependent_tag(adjective(no_e(_)),enkel,'BW()').
stem_dependent_tag(determiner(pron),Cap, 'N(eigen,ev,basis,gen)') :-
    starts_with_capital(Cap). 
stem_dependent_tag('--', Eh, 'TSW()') :-
    alpino_lexical_analysis:hesitation(Eh).
stem_dependent_tag('--', Name, 'SPEC(deeleigen)') :-
    starts_with_capital(Name).
    
stem_dependent_tag(modal_adverb,Word,'ADJ(vrij,basis,zonder)') :-
    alpino_lex:lexicon(adjective(_),_,[Word],[],_).
stem_dependent_tag(modal_adverb(_),Word,'ADJ(vrij,basis,zonder)') :-
    \+ Word = heel,  % -> context
    alpino_lex:lexicon(adjective(_),_,[Word],[],_).

stem_dependent_tag(pronoun(nwh,thi,sg,de,dat_acc,def),hem,  'VNW(pers,pron,obl,vol,3,ev,masc)').
stem_dependent_tag(pronoun(nwh,thi,sg,de,dat_acc,def),haar, 'VNW(pers,pron,obl,vol,3,getal,fem)').

stem_dependent_tag(left_conj(_),Stem,Tag) :-
    !,
    (   Stem == zowel
    ->  Tag = 'BW()'
    ;   Stem == niet
    ->  Tag = 'BW()'
    ;   Stem == 'niet alleen'
    ->  Tag = 'BW()'
    ;   Tag = 'VG(neven)'
    ).

stem_dependent_tag(complementizer(np),Stem,Tag) :-
    !,
    (   Stem == zo
    ->  Tag = 'BW()'
    ;   Stem == niet
    ->  Tag = 'BW()'
    ;   Stem == zoniet
    ->  Tag = 'BW()'
    ;   Stem == namelijk
    ->  Tag = 'BW()'
    ;   Stem = inclusief
    ->  Tag = 'ADJ(vrij,basis,zonder)'
    ;   Tag = 'VG(onder)'
    ).

stem_dependent_tag(noun(both,_,sg),Stem,'N(eigen,ev,basis,genus,stan)') :-
    eigen_noun(Stem).
stem_dependent_tag(noun(de,_,sg),Stem,'N(eigen,ev,basis,zijd,stan)') :-
    eigen_noun(Stem).
stem_dependent_tag(noun(het,_,sg),Stem,'N(eigen,ev,basis,onz,stan)') :-
    eigen_noun(Stem).
stem_dependent_tag(noun(_,_,pl),Stem,'N(eigen,mv,basis)') :-
    eigen_noun(Stem).

stem_dependent_tag(noun(de,count,sg),Stem,'N(eigen,ev,basis,zijd,stan)') :-
    (   alpino_lex:date_month(Stem)
    ;   lists:member(Stem,[zondag,maandag,dinsdag,woensdag,donderdag,vrijdag,zaterdag])
    ).

stem_dependent_tag(tmp_np,middernacht,'N(soort,ev,basis,zijd,stan)').

stem_dependent_tag(sentence_adverb,Stem0,Stem,'ADJ(vrij,dim,zonder)'):-
    tjes(Stem0,Stem).
stem_dependent_tag(adjective(both(tmpadv)),Stem0,Stem,'ADJ(vrij,dim,zonder)'):-
    tjes(Stem0,Stem).
stem_dependent_tag(adjective(pred(_)),Stem0,Stem,'ADJ(vrij,dim,zonder)'):-
    tjes(Stem0,Stem).
stem_dependent_tag(noun(het,count,sg),Stem0,Stem,'N(soort,ev,dim,onz,stan)') :-
    atom(Stem0),
    atom_concat(Stem,'_DIM',Stem0).
stem_dependent_tag(noun(_,_,pl),Stem0,Stem,'N(soort,mv,dim)') :-
    atom(Stem0),
    atom_concat(Stem,'_DIM',Stem0).


tjes(bleekjes,bleek).
tjes(droogjes,droog).
tjes(dunnetjes,dun).
tjes(fijntjes,fijn).
tjes(flauwtjes,flauw).
tjes(frisjes,fris).
tjes(gewoontjes,gewoon).
tjes(gladjes,glad).
tjes(kampjes,kalm).
tjes(kleintjes,klein).
tjes(koeltjes,koel).
tjes(lichtjes,lucht).
tjes(liefjes,lief).
tjes(losjes,los).
tjes(luchtigjes,luchtig).
tjes(magertjes,mager).
tjes(minnetjes,min).
tjes(netjes,net).
tjes(povertjes,pover).
tjes(slapjes,slap).
tjes(sobertjes,sober).
tjes(soepeltjes,soepel).
tjes(stijfjes,stijf).
tjes(stilletjes,stil).
tjes(vlotjes,vlot).
tjes(warmpjes,warm).
tjes(zachtjes,zacht).
tjes(ziekjes,ziek).
tjes(zuinigjes,zuinig).
tjes(zwakjes,zwak).

%%%% defaults
cgn_postag_c(particle(raak),                  'ADJ(vrij,basis,zonder)').
cgn_postag_c(particle(_),                     'VZ(fin)').

cgn_postag_c(determiner(de),                  'LID(bep,stan,rest)').
cgn_postag_c(determiner(een),                 'LID(onbep,stan,agr)').
cgn_postag_c(determiner(der),                 'LID(bep,gen,rest3)').
cgn_postag_c(determiner(des),                 'LID(bep,gen,evmo)').
cgn_postag_c(determiner(den),                 'LID(bep,dat,evmo)').

cgn_postag_c(determiner(onze),                'VNW(bez,det,stan,vol,1,mv,prenom,met-e,rest)').
cgn_postag_c(determiner(de,nwh,mod,pro,yparg),'ADJ(prenom,basis,zonder)').
cgn_postag_c(determiner(de,nwh,nmod,pro,nparg),'VNW(aanw,det,stan,prenom,met-e,rest)').
cgn_postag_c(determiner(een,nwh,mod,pro,yparg),'ADJ(prenom,basis,zonder)').
cgn_postag_c(determiner(elke,nwh,mod),'VNW(onbep,det,stan,prenom,met-e,evz)').
cgn_postag_c(determiner(geen),'VNW(onbep,det,stan,prenom,zonder,agr)').
cgn_postag_c(determiner(geen,nwh,mod,pro,yparg,nwkpro,geen),'VNW(onbep,det,stan,prenom,zonder,agr)').
cgn_postag_c(determiner(het),'VNW(bez,det,stan,vol,1,mv,prenom,zonder,evon)').
cgn_postag_c(determiner(het,nwh,mod),'VNW(onbep,det,stan,prenom,zonder,evon)').
cgn_postag_c(determiner(het,nwh,mod,pro,yparg),'ADJ(prenom,basis,zonder)').
cgn_postag_c(determiner(het,nwh,nmod,pro,nparg),'VNW(aanw,pron,stan,vol,3o,ev)').
cgn_postag_c(determiner(het,nwh,nmod,pro,nparg,wkpro),'LID(bep,stan,evon)').
cgn_postag_c(determiner(pl),'VNW(aanw,det,stan,prenom,zonder,agr)').
cgn_postag_c(determiner(pl_num),'LID(onbep,stan,agr)').
cgn_postag_c(determiner(pl_num,nwh,nmod,pro,yparg),'VNW(onbep,det,stan,prenom,met-e,rest)').
cgn_postag_c(determiner(wat),'ADJ(prenom,basis,zonder)').
cgn_postag_c(determiner(wat,nwh,mod,pro,nparg,ntopicpro),'VNW(onbep,pron,stan,vol,3o,ev)').
cgn_postag_c(determiner(wat,nwh,mod,pro,nparg),'VNW(onbep,pron,stan,vol,3o,ev)').
cgn_postag_c(determiner(welke),'VNW(onbep,det,stan,prenom,met-e,rest)').
cgn_postag_c(determiner(welke,rwh,nmod,pro,yparg),'VNW(vb,det,stan,prenom,met-e,rest)').
cgn_postag_c(determiner(alle,nwh,mod,pro,nparg),'VNW(onbep,det,stan,prenom,met-e,agr)').
cgn_postag_c(determiner(welk,rwh,nmod,pro,yparg),'VNW(vb,det,stan,prenom,zonder,evon)').
cgn_postag_c(determiner(welk),'VNW(onbep,det,stan,prenom,zonder,evon)').
cgn_postag_c(pre_det_quant(al),'VNW(onbep,det,stan,vrij,zonder)').
cgn_postag_c(pre_det_quant(allebei),'VNW(onbep,det,stan,vrij,zonder)').
cgn_postag_c(determiner(zulke,nwh,nmod,pro,yparg),'VNW(aanw,det,stan,prenom,met-e,rest)').
cgn_postag_c(determiner(wat,nwh,mod,pro,yparg),'TW(hoofd,prenom,stan)').
cgn_postag_c(determiner(sg_num), 'VNW(onbep,det,stan,prenom,zonder,agr)').
cgn_postag_c(determiner(zulk),'VNW(aanw,det,stan,vrij,zonder)').

cgn_postag_c(tmp_determiner,'VNW(onbep,det,stan,prenom,met-e,evz)').

cgn_postag_c(determiner(pron,wh),'VNW(vb,pron,stan,vol,3o,ev)').
cgn_postag_c(determiner(pron),'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)').

cgn_postag_c(gen_determiner(sg),'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)').
cgn_postag_c(gen_determiner(pl),'VNW(bez,det,stan,vol,3,mv,prenom,zonder,agr)').


cgn_postag_c(number(hoofd(pl_num)),'TW(hoofd,prenom,stan)').
cgn_postag_c(number(rang),         'TW(rang,prenom,stan)').
cgn_postag_c(number(hoofd(both)),  'TW(hoofd,prenom,stan)').
cgn_postag_c(number(hoofd(sg_num)),'TW(hoofd,prenom,stan)').

cgn_postag_c(adj_number(enkel),    'VNW(onbep,det,stan,prenom,zonder,evon)').
cgn_postag_c(adj_number(enkele),   'VNW(onbep,det,stan,prenom,met-e,rest)').
cgn_postag_c(adj_number(pl_num),   'VNW(onbep,grad,stan,prenom,met-e,mv,basis)').
cgn_postag_c(adj_number(both),     'VNW(onbep,grad,gen,nom,met-e,mv-n,basis)').

cgn_postag_c(pre_num_adv(both),    'VNW(onbep,grad,stan,vrij,zonder,comp)').
cgn_postag_c(pre_num_adv(pl),      'VNW(onbep,det,stan,prenom,zonder,agr)').
cgn_postag_c(pre_num_adv(pl_indef),'VNW(aanw,det,stan,prenom,zonder,agr)').

cgn_postag_c(punct(is_gelijk),                'SPEC(symb)').
cgn_postag_c(punct(_),                        'LET()').

cgn_postag_c(score_cat,                       'SPEC(symb)').
cgn_postag_c(enumeration,                     'SPEC(symb)').

cgn_postag_c(sbar,                            'BW()').
cgn_postag_c(pp,                              'BW()').
cgn_postag_c(pp(_),                           'BW()').
cgn_postag_c(adverb,                          'BW()').
cgn_postag_c(wh_adverb,                       'BW()').
cgn_postag_c(rwh_loc_adverb,                  'BW()').
cgn_postag_c(modal_adverb,                    'BW()').
cgn_postag_c(dip_sbar_adverb,                 'BW()').
cgn_postag_c(modal_adverb(_),                 'BW()').
cgn_postag_c(predm_adverb,                    'BW()').
cgn_postag_c(tmp_adverb,                      'BW()').
cgn_postag_c(loc_adverb,                      'BW()').
cgn_postag_c(dir_adverb,                      'BW()').
cgn_postag_c(postnp_adverb,                   'BW()').
cgn_postag_c(post_wh_adverb,                  'BW()').
cgn_postag_c(sentence_adverb,                 'BW()').
cgn_postag_c(waar_adverb(_),                  'BW()').
cgn_postag_c(er_adverb(_),                    'BW()').
cgn_postag_c(intensifier,                     'BW()').
cgn_postag_c(intensifier(e),                  'ADJ(prenom,basis,met-e,stan)').
cgn_postag_c(me_intensifier,                  'BW()').
cgn_postag_c(als_me_intensifier,              'BW()').
cgn_postag_c(vp_om_intensifier,               'BW()').
cgn_postag_c(vp_om_me_intensifier,            'BW()').
cgn_postag_c(hoe_adv,                         'BW()').
cgn_postag_c(postadj_adverb,                  'BW()').
cgn_postag_c(postadv_adverb,                  'BW()').
cgn_postag_c(om_postadj_adverb,               'BW()').
cgn_postag_c(num_predm_adverb,                'VNW(onbep,det,stan,prenom,met-e,agr)').
cgn_postag_c(zo_mogelijk_zo,                  'BW()').
cgn_postag_c(zo_mogelijk_mogelijk(no_e),      'ADJ(vrij,basis,zonder)').
cgn_postag_c(zo_mogelijk_mogelijk(e),         'ADJ(prenom,basis,met-e,stan)').
cgn_postag_c(adjective(anders),               'BW()').
cgn_postag_c(comp_adverb(_),                  'BW()').
cgn_postag_c(wh_me_adjective,                 'BW()').
cgn_postag_c(eenmaal_adverb,                  'BW()').
cgn_postag_c(pre_wh_adverb,                   'BW()').
cgn_postag_c(vandaar_adverb,                  'BW()').
cgn_postag_c(postp_adverb,                    'BW()').
cgn_postag_c(postn_adverb,                    'BW()').
cgn_postag_c(zo_van_adverb,                   'BW()').
cgn_postag_c(vp_adverb,                       'BW()').
cgn_postag_c(wk_tmp_adverb,                   'BW()').
cgn_postag_c(pre_np_adverb,                   'BW()').
cgn_postag_c(vp_om_adverb,                    'ADJ(vrij,basis,zonder)').

cgn_postag_c(post_p(_),                       'VZ(fin)').

cgn_postag_c(post_n_n,'N(soort,ev,basis,onz,stan)').

cgn_postag_c(np_me_adjective(no_e(adv)),    'ADJ(vrij,basis,zonder)').
cgn_postag_c(np_me_adjective(no_e(nonadv)), 'ADJ(vrij,basis,zonder)').
cgn_postag_c(np_me_adjective(no_e(locadv)), 'ADJ(vrij,basis,zonder)').
cgn_postag_c(np_me_adjective(no_e(diradv)), 'ADJ(vrij,basis,zonder)').
cgn_postag_c(np_me_adjective(e),            'ADJ(prenom,basis,met-e,stan)').
cgn_postag_c(np_me_adjective(both(oadv)),   'VZ(fin)').  % het hele jaar door
cgn_postag_c(me_adjective(no_e(odet_adv)),  'VNW(onbep,grad,stan,vrij,zonder,basis)').        % teveel
cgn_postag_c(vp_om_me_adjective(no_e(odet_adv)),  'VNW(onbep,grad,stan,vrij,zonder,basis)').  % teveel

cgn_postag_c(adjective(prefix),             'ADJ(prenom,basis,zonder)').

cgn_postag_c(sbar_adjective(no_e(odet_adv)),  'TW(hoofd,vrij)').
cgn_postag_c(sbar_adjective(no_e(adv)),       'ADJ(vrij,basis,zonder)').
cgn_postag_c(sbar_adjective(both(oadv)),      'BW()').
cgn_postag_c(sbar_adjective(both(adv)),       'BW()').
cgn_postag_c(sbar_adjective(e),               'ADJ(prenom,basis,met-e,stan)').
cgn_postag_c(pred_np_me_adjective(dir_locadv),'BW()').
cgn_postag_c(pred_np_me_adjective(tmpadv),    'BW()').
cgn_postag_c(pred_np_me_adjective(adv),       'BW()').
cgn_postag_c(pred_np_me_adjective(nonadv),    'BW()').
cgn_postag_c(pred_np_me_adjective(padv),      'BW()').

cgn_postag_c(adjective(pred(diradv)),         'BW()').

cgn_postag_c(er_loc_adverb,                   'VNW(aanw,adv-pron,obl,vol,3o,getal)').
cgn_postag_c(er_wh_loc_adverb,                'VNW(vb,adv-pron,obl,vol,3o,getal)').
cgn_postag_c(er_vp_adverb,                    'VNW(aanw,adv-pron,stan,red,3,getal)').

cgn_postag_c(tmp_app_noun,                    'N(soort,ev,basis,onz,stan)').

cgn_postag_c(noun(both,_,both),               'N(soort,ev,basis,zijd,stan)').

cgn_postag_c(tmp_np,                          'SPEC(symb)').
cgn_postag_c(np,                              'TW(hoofd,vrij)').
cgn_postag_c(np(year),                        'TW(hoofd,vrij)').


cgn_postag_c(amount_meas_mod_noun(both,count,bare_meas),
	                                    'N(soort,ev,basis,zijd,stan)').
cgn_postag_c(amount_meas_mod_noun(both,count,bare_meas,measure),
	                                    'N(soort,ev,basis,zijd,stan)').

cgn_postag_c(noun(de,count,bare_meas),'N(soort,ev,basis,zijd,stan)').

cgn_postag_c(ge_v_noun(intransitive),'N(soort,ev,basis,onz,stan)').


cgn_postag_c(iets_noun,                       'VNW(onbep,pron,stan,vol,3o,ev)').
cgn_postag_c(iets_adverb,                     'VNW(onbep,adv-pron,obl,vol,3o,getal)').
cgn_postag_c(wh_iets_anders_noun,             'VNW(vb,pron,stan,vol,3p,getal)').
cgn_postag_c(iets_anders_noun,                'VNW(onbep,pron,stan,vol,3p,ev)').

cgn_postag_c(het_noun,                        'VNW(pers,pron,stan,red,3,ev,onz)').

cgn_postag_c(proper_name(Agr,Val),            Postag):-
    name_postag(both,both,Agr,Val,Postag).      
cgn_postag_c(name_determiner(pron),           'N(eigen,ev,basis,gen)').
cgn_postag_c(name_determiner(pron,_),         'N(eigen,ev,basis,gen)').


cgn_postag_c(preposition(_,_),                'VZ(init)').
cgn_postag_c(preposition(te,_,nodet),         'VZ(init)').
cgn_postag_c(preposition(ten,_,nodet),        'VZ(versm)').
cgn_postag_c(preposition(ter,_,nodet),        'VZ(versm)').
cgn_postag_c(preposition(_,_,extracted_np),   'VZ(fin)').
cgn_postag_c(preposition(_,_,_),              'VZ(init)').

cgn_postag_c(conj(_),                         'VG(neven)').
cgn_postag_c(left_conj(_),                    'VG(neven)').
cgn_postag_c(right_conj(als),                 'VG(onder)').
cgn_postag_c(right_conj(eerder_dan),          'VG(onder)').
cgn_postag_c(right_conj(nu_eens_dan),         'BW()').

cgn_postag_c(comp_noun(het,_,sg,_),           'VNW(onbep,pron,stan,vol,3o,ev)').

cgn_postag_c(comparative(_),                  'VG(onder)').

cgn_postag_c(complementizer(a),               'VG(onder)').
cgn_postag_c(complementizer(adv),             'VG(onder)').
cgn_postag_c(complementizer(al),              'VG(onder)').
cgn_postag_c(complementizer(als),             'VG(onder)').
cgn_postag_c(complementizer(alsof),           'VG(onder)').
cgn_postag_c(complementizer(dat),             'VG(onder)').
cgn_postag_c(complementizer(inf),             'VZ(init)').
cgn_postag_c(complementizer(naar),            'VZ(init)').
cgn_postag_c(complementizer(np),              'VG(onder)').
cgn_postag_c(complementizer(of),              'VG(onder)').
cgn_postag_c(complementizer(om),              'VZ(init)').
cgn_postag_c(complementizer(op),              'VZ(init)').
cgn_postag_c(complementizer(pp),              'VG(onder)').
cgn_postag_c(complementizer(root),            'VG(neven)').
cgn_postag_c(complementizer(sbar),            'VG(onder)').
cgn_postag_c(complementizer(start),           'VG(onder)').
cgn_postag_c(complementizer(te),              'VZ(init)').
cgn_postag_c(complementizer(van),             'VZ(init)').
cgn_postag_c(complementizer(vp),              'BW()').
cgn_postag_c(complementizer(zoals),           'VG(onder)').
cgn_postag_c(complementizer,                  'VG(onder)').
cgn_postag_c(complementizer(uit),             'VZ(init)').

cgn_postag_c(complementizer(datti),           'VG(onder,dial)').

%% aant
cgn_postag_c(complementizer(aan_het),         'VZ(init)').


cgn_postag_c(reflexive(u_thi,both),           'VNW(refl,pron,obl,red,3,getal)').
cgn_postag_c(reflexive(fir,pl),               'VNW(pr,pron,obl,vol,1,mv)').
cgn_postag_c(reflexive(fir,sg),               'VNW(pr,pron,obl,red,1,ev)').
cgn_postag_c(reflexive(je,both),              'VNW(pr,pron,obl,red,2v,getal)').

%% vrij/prenom/nom depends on context
cgn_postag_c(adjective(e),                    'ADJ(prenom,basis,met-e,stan)').
cgn_postag_c(adjective(ende(_)),              'WW(od,prenom,met-e)').
cgn_postag_c(adjective(end(_)),               'WW(od,prenom,zonder)').

cgn_postag_c(adjective(ere),                  'ADJ(prenom,comp,met-e,stan)').
cgn_postag_c(adjective(ste),                  'ADJ(prenom,sup,met-e,stan)').

cgn_postag_c(adjective(ge_e),                 'WW(vd,prenom,met-e)').
cgn_postag_c(adjective(ge_no_e(_)),           'WW(vd,prenom,zonder)').
cgn_postag_c(adjective(ge_both(_)),           'WW(vd,prenom,zonder)').

cgn_postag_c(adjective(no_e(_)),              'ADJ(prenom,basis,zonder)').
cgn_postag_c(adjective(er(_)),                'ADJ(prenom,comp,zonder)').
cgn_postag_c(adjective(st(_)),                'ADJ(prenom,sup,zonder)').
cgn_postag_c(adjective(aller_st(_)),          'ADJ(prenom,sup,zonder)').

cgn_postag_c(adjective(both(osentadv)),       'BW()').
cgn_postag_c(adjective(both(_)),              'ADJ(prenom,basis,zonder)').

cgn_postag_c(adjective(stof),                 'ADJ(prenom,basis,zonder)').

cgn_postag_c(post_adjective(no_e),            'ADJ(postnom,basis,met-s)').
cgn_postag_c(post_adjective(er),              'ADJ(postnom,comp,met-s)').
cgn_postag_c(post_adjective_anders(er),       'ADJ(postnom,basis,met-s)').

cgn_postag_c(adjective(het_st(_)),            'ADJ(vrij,sup,zonder)').

cgn_postag_c(nominalized_adjective,           'ADJ(nom,basis,met-e,mv-n)').
cgn_postag_c(nominalized_adjective(_),        'ADJ(nom,basis,met-e,mv-n)').
cgn_postag_c(end_nominalized_adjective,       'WW(od,nom,met-e,mv-n)').
cgn_postag_c(end_nominalized_adjective(_),    'WW(od,nom,met-e,mv-n)').
cgn_postag_c(ge_nominalized_adjective,        'WW(vd,nom,met-e,mv-n)').
cgn_postag_c(ge_nominalized_adjective(_),     'WW(vd,nom,met-e,mv-n)').
cgn_postag_c(nominalized_compar_adjective,    'ADJ(nom,comp,met-e,mv-n)').
cgn_postag_c(nominalized_compar_adjective_sg, 'ADJ(nom,comp,met-e,zonder-n,stan)').
cgn_postag_c(nominalized_super_adjective,     'ADJ(nom,sup,met-e,mv-n)').
cgn_postag_c(nominalized_adjective_sg,        'WW(vd,nom,met-e,zonder-n)').

cgn_postag_c(adjective(pred(adv)),            'BW()').
cgn_postag_c(adjective(pred(sentadv)),        'BW()').
cgn_postag_c(adjective(pred(nonadv)),         'ADJ(vrij,basis,zonder)').
cgn_postag_c(adjective(pred(locadv)),         'ADJ(vrij,basis,zonder)').
cgn_postag_c(adjective(pred(padv)),           'ADJ(vrij,basis,zonder)').
cgn_postag_c(adjective(pred(both)),           'BW()').
cgn_postag_c(adjective(pred(dir_locadv)),     'VZ(init)').


cgn_postag_c(adjective(postn_no_e(_)),   'ADJ(vrij,basis,zonder)').
cgn_postag_c(adjective(postn_pred(_)),   'ADJ(vrij,basis,zonder)').
cgn_postag_c(adjective(postn_both(_)),   'ADJ(vrij,basis,zonder)').

cgn_postag_c(np_adjective,'BW()').
cgn_postag_c(np_adjective(_),'BW()').
cgn_postag_c(het_np_adjective(_),'BW()').
cgn_postag_c(subject_vp_pred_np_adjective,'BW()').
cgn_postag_c(subject_sbar_pred_np_adjective,'ADJ(vrij,basis,zonder)').
cgn_postag_c(subject_sbar_pred_np_me_adjective,'BW()').
cgn_postag_c(clause_np_adjective,          'ADJ(vrij,basis,zonder)').
cgn_postag_c(clause_np_adjective(pp(met)), 'ADJ(vrij,basis,zonder)').

cgn_postag_c(so_np_subject_sbar,'ADJ(vrij,basis,zonder)').
cgn_postag_c(so_np_subject_vp,'ADJ(vrij,basis,zonder)').
cgn_postag_c(so_pp_subject_sbar(_),'ADJ(vrij,basis,zonder)').
cgn_postag_c(so_pp_subject_vp(_),'ADJ(vrij,basis,zonder)').

cgn_postag_c(sbar_pred_adjective(_), 'BW()').
cgn_postag_c(vp_pred_adjective(_),   'BW()').

cgn_postag_c(wh_adjective,'BW()').
cgn_postag_c(wh_adjective(odet_adv),'TW(hoofd,vrij)').

cgn_postag_c(wh_iets_noun,                     'TW(hoofd,prenom,stan)').
cgn_postag_c(e_als_adjective(no_e(odet_adv)),  'TW(hoofd,prenom,stan)').
cgn_postag_c(als_adjective(no_e(odet_adv)),    'TW(hoofd,prenom,stan)').
cgn_postag_c(als_adjective(no_e(nonadv)),      'ADJ(vrij,basis,zonder)').
cgn_postag_c(als_adjective(both(_)),           'BW()').
cgn_postag_c(als_adjective(e),                 'ADJ(prenom,basis,met-e,stan)').

cgn_postag_c(within_word_conjunct,'SPEC(afgebr)').

cgn_postag_c(etc,                 'BW()').

cgn_postag_c(wh_tmp_adverb,       'BW()').

cgn_postag_c(adjective(meer),     'VNW(onbep,grad,stan,vrij,zonder,comp)').

cgn_postag_c(comp_determiner(_,_),'ADJ(prenom,basis,zonder)').

cgn_postag_c(wh_iets_adverb,'VNW(vb,adv-pron,obl,vol,3o,getal)').

cgn_postag_c(verb(_,psp,_),                   'WW(vd,vrij,zonder)').
cgn_postag_c(verb(_,inf_ipp,_),               'WW(inf,vrij,zonder)').
cgn_postag_c(verb(_,inf,_),                   'WW(inf,vrij,zonder)').
cgn_postag_c(verb(_,inf(no_e),_),             'WW(inf,vrij,zonder)').
cgn_postag_c(verb(_,inf(e),_),                'WW(inf,prenom,met-e)').
cgn_postag_c(verb(_,past(sg),_),              'WW(pv,verl,ev)').
cgn_postag_c(verb(_,past(pl),_),              'WW(pv,verl,mv)').
cgn_postag_c(verb(_,both(pl),_),              'WW(pv,tgw,mv)').  % tgw is more frequent than verl
cgn_postag_c(verb(_,modal_inv,_),             'WW(pv,tgw,ev)').
cgn_postag_c(verb(_,sg_hebt,_),               'WW(pv,tgw,met-t)').
cgn_postag_c(verb(_,sg_heeft,_),              'WW(pv,tgw,met-t)').
cgn_postag_c(verb(_,sg_bent,_),               'WW(pv,tgw,met-t)').
cgn_postag_c(verb(_,sg_is,_),                 'WW(pv,tgw,ev)').
cgn_postag_c(verb(_,modal_not_u,_),           'WW(pv,tgw,ev)').
cgn_postag_c(verb(_,imp(sg1),_),              'WW(pv,tgw,ev)').
cgn_postag_c(verb(_,sg1,_),                   'WW(pv,tgw,ev)').
cgn_postag_c(verb(_,sg3,_),                   'WW(pv,tgw,met-t)').
cgn_postag_c(verb(_,subjunctive,_),           'WW(pv,conj,ev)').
cgn_postag_c(verb(_,sg,_),                    'WW(pv,tgw,ev)').
cgn_postag_c(verb(_,imp(sg),_),               'WW(pv,tgw,ev)').
cgn_postag_c(verb(_,pl,_),                    'WW(pv,tgw,mv)').
cgn_postag_c(verb(_,imp(modal_u),_),          'WW(pv,tgw,met-t)').  %%%%% "weest" 

cgn_postag_c(v_noun(_),                       'WW(inf,nom,zonder,zonder-n)').

cgn_postag_c(tag,                             'TSW()').

cgn_postag_c(rel_pronoun(de,no_obl),          'VNW(betr,pron,stan,vol,persoon,getal)').
cgn_postag_c(rel_pronoun(het,no_obl),         'VNW(betr,pron,stan,vol,3,ev)').
cgn_postag_c(rel_pronoun(het,both),           'VNW(vb,pron,stan,vol,3o,ev)').
cgn_postag_c(rel_pronoun(both,obl),           'VNW(vb,pron,stan,vol,3p,getal)').
cgn_postag_c(rel_pronoun(both,no_obl),        'VNW(betr,det,stan,nom,zonder,zonder-n)').

cgn_postag_c(pronoun(nwh,fir,sg,de,nom,def),  'VNW(pers,pron,nomin,vol,1,ev)').

cgn_postag_c(pronoun(nwh,fir,both,de,dat_acc,def),'VNW(pr,pron,obl,nadr,1,ev)').
cgn_postag_c(pronoun(nwh,fir,pl,de,dat_acc,def),'VNW(pr,pron,obl,vol,1,mv)').
cgn_postag_c(pronoun(nwh,fir,pl,de,nom,def),'VNW(pers,pron,nomin,vol,1,mv)').
cgn_postag_c(pronoun(nwh,fir,pl,de,nom,def,wkpro),'VNW(pers,pron,nomin,red,1,mv)').
cgn_postag_c(pronoun(nwh,fir,sg,de,dat_acc,def),'VNW(pr,pron,obl,vol,1,ev)').
cgn_postag_c(pronoun(nwh,fir,sg,de,dat_acc,def,wkpro),'VNW(pr,pron,obl,red,1,ev)').
cgn_postag_c(pronoun(nwh,fir,sg,de,nom,def),'VNW(pers,pron,nomin,vol,1,ev)').
cgn_postag_c(pronoun(nwh,je,both,de,dat_acc,def),'VNW(pr,pron,obl,nadr,2v,getal)').
cgn_postag_c(pronoun(nwh,je,pl,de,both,def),'VNW(pers,pron,stan,nadr,2v,mv)').
cgn_postag_c(pronoun(nwh,je,sg,de,dat_acc,def),'VNW(pers,pron,obl,vol,2v,ev)').
cgn_postag_c(pronoun(nwh,je,sg,de,nom,def),'VNW(pers,pron,nomin,vol,2v,ev)').
cgn_postag_c(pronoun(nwh,thi,both,de,both,def,wkpro),'VNW(pers,pron,stan,red,3,mv)').
cgn_postag_c(pronoun(nwh,thi,both,de,dat_acc,def),'VNW(refl,pron,obl,nadr,3,getal)').
cgn_postag_c(pronoun(nwh,thi,both,de,dat_acc,def,wkpro),'VNW(refl,pron,obl,red,3,getal)').
cgn_postag_c(pronoun(nwh,thi,both,de,nom,def),'VNW(pers,pron,nomin,vol,3p,mv)').
cgn_postag_c(pronoun(nwh,thi,pl,de,both,def,strpro),'VNW(aanw,det,stan,nom,met-e,mv-n)').
cgn_postag_c(pronoun(nwh,thi,pl,de,both,indef),'VNW(onbep,grad,stan,nom,met-e,mv-n,basis)').
cgn_postag_c(pronoun(nwh,thi,pl,de,dat_acc,def),'VNW(pers,pron,obl,vol,3p,mv)').
cgn_postag_c(pronoun(nwh,thi,sg,both,both,indef),'TW(hoofd,vrij)').
cgn_postag_c(pronoun(nwh,thi,sg,both,both,indef,strpro),'TW(hoofd,vrij)').
cgn_postag_c(pronoun(nwh,thi,sg,de,both,def),'VNW(onbep,det,stan,vrij,zonder)').
cgn_postag_c(pronoun(nwh,thi,sg,de,both,def,strpro),'VNW(onbep,pron,stan,vol,3p,ev)').
cgn_postag_c(pronoun(nwh,thi,sg,de,both,indef,strpro),'VNW(onbep,pron,stan,vol,3p,ev)').
cgn_postag_c(pronoun(nwh,thi,sg,het,both,indef,strpro),'VNW(onbep,pron,stan,vol,3o,ev)').
cgn_postag_c(pronoun(nwh,thi,sg,het,both,def,strpro),'VNW(betr,det,stan,nom,zonder,zonder-n)').
cgn_postag_c(pronoun(nwh,u,sg,de,both,def),'VNW(pers,pron,nomin,vol,2b,getal)').
cgn_postag_c(pronoun(nwh,u,sg,de,nom,def),'VNW(pers,pron,nomin,vol,2,getal)').
cgn_postag_c(pronoun(ywh,thi,both,de,both,indef),'VNW(vb,pron,stan,vol,3p,getal)').
cgn_postag_c(pronoun(ywh,thi,sg,het,both,def),'VNW(betr,det,stan,nom,zonder,zonder-n)').
cgn_postag_c(pronoun(ywh,thi,sg,het,both,indef,nparg),'VNW(vb,pron,stan,vol,3o,ev)').
cgn_postag_c(pronoun(nwh,je,sg,de,both,def,wkpro),'VNW(pers,pron,nomin,red,2v,ev)').  % only used in with_dt?

cgn_postag_c(noun(_,_,pl),'N(soort,mv,basis)'). 
cgn_postag_c(noun(het,_,_),'N(soort,ev,basis,onz,stan)').
cgn_postag_c(noun(_,_,_),'N(soort,ev,basis,zijd,stan)').

%% CGN lemma

%% verb: sg1 -> inf
%% adj/psp:  -> inf
%% remove _DIM suffix
%% remove _ compound border, reintroduce -s if necc (how?)
%% word = pos-, add '-' add the end
%% "inflected" determiners/pronouns alle -> al, hoeveelste -> hoeveel, ...

:- use_module(latin1).
starts_with_capital(W) :-
    atom(W),
    atom_codes(W,[F|_T]),
    isupper(F).

det_pron(allerlei,    'ADJ(prenom,basis,zonder)',                       'ADJ(nom,basis,zonder,zonder-n)').
det_pron(allebei,     'ADJ(prenom,basis,zonder)',                       'ADJ(nom,basis,zonder,zonder-n)').
det_pron(die,         'VNW(aanw,det,stan,prenom,zonder,rest)',          'VNW(aanw,pron,stan,vol,3,getal)').
det_pron(dat,         'VNW(aanw,det,stan,prenom,zonder,evon)',          'VNW(aanw,pron,stan,vol,3o,ev)').
det_pron(datzelfde,   'ADJ(prenom,basis,zonder)',                       'ADJ(nom,basis,zonder,zonder-n)').
det_pron(deze,        'VNW(aanw,det,stan,prenom,met-e,rest)',           'VNW(aanw,det,stan,nom,met-e,zonder-n)').
det_pron(dezelve,     'VNW(aanw,det,stan,prenom,met-e,rest)',           'VNW(aanw,det,stan,nom,met-e,zonder-n)').
det_pron(dit,         'VNW(aanw,det,stan,prenom,zonder,evon)',          'VNW(aanw,pron,stan,vol,3o,ev)').
det_pron(ditzelfde,   'ADJ(prenom,basis,zonder)',                       'ADJ(nom,basis,zonder,zonder-n)').
det_pron(elk,         'VNW(onbep,det,stan,prenom,zonder,evon)',         'VNW(onbep,det,stan,vrij,zonder)').
det_pron(geen,        'VNW(onbep,det,stan,prenom,zonder,agr)',          'VNW(onbep,det,stan,nom,zonder,zonder-n)').
det_pron(gene,        'VNW(aanw,det,stan,prenom,met-e,rest)',           'VNW(aanw,det,stan,nom,met-e,zonder-n)').
det_pron(heleboel,    'N(soort,ev,basis,zijd,stan)',                    'N(soort,ev,basis,zijd,stan)').
det_pron(het,         'LID(bep,stan,evon)',                             'VNW(pers,pron,stan,red,3,ev,onz)').
det_pron(hetzelfde,   'ADJ(prenom,basis,zonder)',                       'ADJ(nom,basis,zonder,zonder-n)').
det_pron(dezelfde,    'ADJ(prenom,basis,zonder)',                       'ADJ(nom,basis,zonder,zonder-n)').
det_pron(diezelfde,   'ADJ(prenom,basis,zonder)',                       'ADJ(nom,basis,zonder,zonder-n)').
det_pron(hoeveel,     'TW(hoofd,prenom,stan)',                          'TW(hoofd,vrij)').
det_pron(meer,        'VNW(onbep,grad,stan,prenom,zonder,agr,comp)',    'VNW(onbep,grad,stan,vrij,zonder,comp)').
det_pron(minder,      'VNW(onbep,grad,stan,prenom,zonder,agr,comp)',    'VNW(onbep,grad,stan,vrij,zonder,comp)').
det_pron(onvoldoende, 'ADJ(prenom,basis,zonder)',                       'ADJ(vrij,basis,zonder)').
det_pron(sommig,      'VNW(onbep,det,stan,prenom,met-e,rest)',          'VNW(onbep,det,stan,nom,met-e,zonder-n)').
det_pron(teveel,      'VNW(onbep,grad,stan,prenom,zonder,agr,basis)',   'VNW(onbep,grad,stan,vrij,zonder,basis)').
det_pron(veel,        'VNW(onbep,grad,stan,prenom,zonder,agr,basis)',   'VNW(onbep,grad,stan,vrij,zonder,basis)').
det_pron(voldoende,   'ADJ(prenom,basis,zonder)',                       'ADJ(vrij,basis,zonder)').
det_pron(weinig,      'VNW(onbep,grad,stan,prenom,zonder,agr,basis)',   'VNW(onbep,grad,stan,vrij,zonder,basis)').
det_pron(welk,        'VNW(vb,det,stan,prenom,zonder,evon)',            'VNW(vb,det,stan,nom,met-e,zonder-n)').   % want "zonder,basis" oid bestaat niet in tag-set!!
det_pron(welke,       'VNW(vb,det,stan,prenom,met-e,rest)',             'VNW(vb,det,stan,nom,met-e,zonder-n)').
det_pron(zoveel,      'TW(hoofd,prenom,stan)',                          'TW(hoofd,vrij)').
det_pron(zulk,       'VNW(aanw,det,stan,prenom,met-e,rest)',           'VNW(aanw,det,stan,nom,met-e,zonder-n)'). % want "rest" oid ipv "zonder-n" bestaat niet in tag-set!!
det_pron(zulk,        'VNW(aanw,det,stan,prenom,zonder,evon)',          'VNW(aanw,det,stan,vrij,zonder)').

cgn_postag_proper(Postag,Stem0,Surf,Stem,Q0,Q,Result,SUB) :-
    (   alpino_util:split_atom(Stem0," ",StemEls),
	length(StemEls,Len),
	\+ Len is Q-Q0
    ->  Stem1 = Surf
    ;   Stem1 = Stem0
    ),
    stem_al(Stem1,Stem),
    find_node(Q0,Q,Result,Node),
    find_dehet_name(Node,DeHet,DeHetSg),
    find_sgpl(Node,SgPl),
    name_postag(DeHet,DeHetSg,SgPl,SUB,Stem,Postag,Q0,Q).

stem_al(Atom1,Lem) :-
    (   atom(Atom1),
	atom_concat('Al-',Rest,Atom1)
    ->  atom_concat('al-',Rest,Lem)
    ;   Atom1 = Lem
    ).

find_sgpl(Node,Sg) :-
    alpino_data:agr(Node,Agr),
    alpino_data:sg(Agr2),
    alpino_data:pl(Agr3),
    (   \+ Agr = Agr2
    ->  Sg = pl
    ;   \+ Agr = Agr3
    ->  Sg = sg
    ;   Sg = both
    ).

find_dehet(Node,De,SgDe,Stem) :-
    alpino_data:agr(Node,Agr),
    alpino_data:de(Agr2),
    alpino_data:het(Agr3),
    alpino_data:sg(Agr4),
    (   \+ Agr = Agr2
    ->  De = het, SgDe = het
    ;   \+ Agr = Agr3
    ->  De = de, SgDe = de
    ;   default_dehet(Stem,De1)
    ->  De = De1, SgDe = De1
    ;   \+ (Agr=Agr4, Agr4 = Agr3)		% if singular, then not het
    ->  De = both,
	SgDe = de
    ;   De = both,
	SgDe = both
    ).

find_dehet_name(Node,De,SgDe) :-
    alpino_data:agr(Node,Agr),
    alpino_data:de(Agr2),
    alpino_data:het(Agr3),
    alpino_data:sg(Agr4),
    (   \+ Agr = Agr2   
    ->  De = het, SgDe = het
    ;   \+ Agr = Agr3   
    ->  De = de, SgDe = de
    ;   \+ (Agr=Agr4, Agr4 = Agr3)		% if singular, then not het
    ->  De = both,
	SgDe = de
    ;   De = both,
	SgDe = both
    ).

default_dehet(CompoundStem,DeHet) :-
    atom(CompoundStem),
    alpino_util:split_atom(CompoundStem,"_",[_,W2|Words0]),
    !,
    lists:last([W2|Words0],Last),
    default_dehet(Last,DeHet).

default_dehet(Coma,DeHet) :-
    exc_dehet(Coma,DeHet).
default_dehet(Word,DeHet) :-
    lm_dehet(Word,DeHet).

lm_dehet(Word,DeHet):-
    alpino_cg:phrase_fluency([xx,de,Word,'.'],DeScore),
    alpino_cg:phrase_fluency([xx,het,Word,'.'],HetScore),
    (    HetScore < DeScore
    ->   DeHet = het
    ;    DeHet = de
    ).

exc_dehet(aantal,het).
exc_dehet(affiche,genus).
exc_dehet(aura,de).
exc_dehet(coma,het).
exc_dehet(baldakijn,genus).
exc_dehet(boord,genus).
exc_dehet(doolhof,genus).
exc_dehet(katoen,genus).
exc_dehet(koolmonoxide,genus).
exc_dehet(kwart,het).
exc_dehet(memo,genus).
exc_dehet(miljard,het).
exc_dehet(miljoen,het).
exc_dehet(opzet,genus).
exc_dehet(pas,de).      % lm_dehet fails here
exc_dehet(pedaal,genus).
exc_dehet(percent,het).
exc_dehet(poeder,genus).
exc_dehet(riool,genus).
exc_dehet(soort,genus).
exc_dehet(snoep,de).
exc_dehet(stempel,genus).
exc_dehet(textiel,genus).

nattr(Node) :-
    alpino_data:aform(Node,Aform),
    alpino_data:not_attr(Aform3),
    \+ \+ Aform = Aform3.

noun_postag(genus,  _, sg,        'N(soort,ev,basis,genus,stan)').
noun_postag(de,     _, sg,        'N(soort,ev,basis,zijd,stan)').
noun_postag(het,    _, sg,        'N(soort,ev,basis,onz,stan)').
noun_postag(both,both, sg,        'N(soort,ev,basis,zijd,stan)').
noun_postag(both,de,   sg,        'N(soort,ev,basis,zijd,stan)').
noun_postag(both,het,  sg,        'N(soort,ev,basis,onz,stan)').
noun_postag(de,     _, both,      'N(soort,ev,basis,zijd,stan)').
noun_postag(het,    _, both,      'N(soort,ev,basis,onz,stan)').
noun_postag(both,both, both,      'N(soort,ev,basis,zijd,stan)').
noun_postag(both,de,   both,      'N(soort,ev,basis,zijd,stan)').
noun_postag(both,het,  both,      'N(soort,ev,basis,onz,stan)').
noun_postag(de,     _, bare_meas, 'N(soort,ev,basis,zijd,stan)').
noun_postag(het,    _, bare_meas, 'N(soort,ev,basis,onz,stan)').
noun_postag(both,both, bare_meas, 'N(soort,ev,basis,zijd,stan)').
noun_postag(both,de,   bare_meas, 'N(soort,ev,basis,zijd,stan)').
noun_postag(both,het,  bare_meas, 'N(soort,ev,basis,onz,stan)').
noun_postag(de,     _, meas,      'N(soort,ev,basis,zijd,stan)').
noun_postag(het,    _, meas,      'N(soort,ev,basis,onz,stan)').
noun_postag(both,both, meas,      'N(soort,ev,basis,zijd,stan)').
noun_postag(both,de,   meas,      'N(soort,ev,basis,zijd,stan)').
noun_postag(both,het,  meas,      'N(soort,ev,basis,onz,stan)').
noun_postag(_,      _, pl,        'N(soort,mv,basis)').

name_postag(DeHet0,DeHetSg,SgPl0,Sub,Stem,Tag,Q0,Q) :-
    hdrug_util:hdrug_flag(add_nodes_for_mwu,On),
    (   On == on,
	Q-Q0 > 1
    ->  Tag = 'SPEC(deeleigen)'
    ;   try_sgpl(SgPl0,SgPl,Stem),
	try_dehet(DeHet0,DeHet,Stem),
	name_postag(DeHet,DeHetSg,SgPl,Sub,Tag)
    ).

try_sgpl(both,SgPl,Stem) :-
    (   try_sgpl(Stem,SgPl)
    ->  true
    ;   SgPl = both
    ).
try_sgpl(sg,sg,_).
try_sgpl(pl,pl,_).

try_dehet(both,DeHet,Stem) :-
    (   try_dehet(Stem,DeHet)
    ->  true
    ;   DeHet = both
    ).
try_dehet(de,de,_).
try_dehet(het,het,_).

try_sgpl('Admirals',pl).
try_sgpl('Alpen',pl).
try_sgpl('Ardennen',pl).
try_sgpl('Bahama\'s',pl).
try_sgpl('Balearen',pl).
try_sgpl('B&W',pl).
try_sgpl('B&W.',pl).
try_sgpl('B.&W',pl).
try_sgpl('B.&W.',pl).
try_sgpl('Borg',pl).
try_sgpl('Caraïben',pl).
try_sgpl('Clintons',pl).
try_sgpl('Domobranci',pl).
try_sgpl('Drumbassadors',pl).
try_sgpl('Dukes',pl).
try_sgpl('Filipijnen',pl).
try_sgpl('Filippijnen',pl).
try_sgpl('FNV-Bondgenoten',pl).
try_sgpl('G.S.',pl).
try_sgpl('GS',pl).
try_sgpl('Hoogovens',pl).
try_sgpl('Kamers',pl).
try_sgpl('Kempen',pl).
try_sgpl('Lakers',pl).
try_sgpl('Molukken',pl).
try_sgpl('Mujahedeen',pl).
try_sgpl('Nederlanden',pl).
try_sgpl('Pinkstergemeenten',pl).
%% try_sgpl('PS',pl).  % provinciale staten? maar veel vaker Parti Socialist..
try_sgpl('Pyreneeen',pl).
try_sgpl('Pyreneeën',pl).
try_sgpl('Staten-Generaal',pl).
try_sgpl('Talibaan',pl).
try_sgpl('Taliban',pl).
try_sgpl('Tories',pl).
try_sgpl('Trappers',pl).
try_sgpl('VN',pl).
try_sgpl('Vogezen',pl).
try_sgpl('VS',pl).

try_sgpl('Brattholmeilanden',pl).
try_sgpl('Cetniks',pl).
try_sgpl('Dorsets',pl).
try_sgpl('Flinstones',pl).
try_sgpl('Flintstones',pl).
try_sgpl('Franken',pl).
try_sgpl('Fransen',pl).
try_sgpl('GGD-en',pl).
try_sgpl('Grenslandhallen',pl).
try_sgpl('Habsburgers',pl).
try_sgpl('Hohenstaufen',pl).
try_sgpl('Khmer',pl).
try_sgpl('Klingonen',pl).
try_sgpl('Middeleeuwen',pl).
try_sgpl('Midlands',pl).
try_sgpl('Mon',pl).
try_sgpl('Nomads',pl).
try_sgpl('Noord-Molukken',pl).
try_sgpl('Oostkantons',pl).
try_sgpl('Oscars',pl).
try_sgpl('Pashtun',pl).
try_sgpl('Pruisen',pl).
try_sgpl('Pyu',pl).
try_sgpl('Safaviden',pl).
try_sgpl('Spelen',pl).
try_sgpl('Trekkies',pl).
try_sgpl('USA',pl).
try_sgpl('Veenkoloniën',pl).
try_sgpl('V.S.',pl).
try_sgpl('VN',pl).
try_sgpl('VS.',pl).
try_sgpl('Vikings',pl).
try_sgpl('Vulcans',pl).
try_sgpl('Wadden',pl).
try_sgpl('Waddeneilanden',pl).
try_sgpl('Winterspelen',pl).
try_sgpl('Zomerspelen',pl).

try_sgpl(Word,pl) :-
    alpino_unknowns:decap(Word,Word1),
    atom(Word1),
    alpino_util:split_atom(Word1,"_",Stems),
    lists:last(Stems,Suffix),
    alpino_lex:plural_suffix(Suffix).


try_dehet(L,genus) :-
    genus_naam(L),
    !.

%% N31, A7
try_dehet(Stem,                 de) :-
    atom(Stem),
    (  atom_concat('A',R,Stem)
    ;  atom_concat('N',R,Stem)
    ),
    atom_codes(R,[H|T]),
    digits([H|T]).
try_dehet(L,de) :-
    de_naam(L).
try_dehet(L,het) :-
    het_naam(L).
try_dehet(L,de) :-
    de_heur(L).
try_dehet(L,het) :-
    het_heur(L).

genus_naam('4FM').
genus_naam('ABN-AMRO').
genus_naam('Adecco').
genus_naam('Agusta').
genus_naam('Ahold').
genus_naam('ARJOS').
genus_naam('Beernink').
genus_naam('Belga').
genus_naam('Braet').
genus_naam('C&C').
genus_naam('Comcast').
genus_naam('CTB').
genus_naam('Daimler').
genus_naam('Dassault').
genus_naam('DAT').
genus_naam('Dexia').
genus_naam('DHL').
genus_naam('DNS').
genus_naam('EcoConsult').
genus_naam('Enron').
genus_naam('Euronext').
genus_naam('EWI').
genus_naam('EZ').
genus_naam('Fortis').
genus_naam('Google').
genus_naam('Greenpeace').
genus_naam('IGE').
genus_naam('Interbrew').
genus_naam('Interpay').
genus_naam('IRIS').
genus_naam('JetSky').
genus_naam('Laurus').
genus_naam('LMS').
genus_naam('Mannesmann').
genus_naam('Mobistar').
genus_naam('MOSTforWATER').
genus_naam('Nature').
genus_naam('NED1').
genus_naam('NED2').
genus_naam('NED3').
genus_naam('NUON').
genus_naam('Parmalat').
genus_naam('Prego').
genus_naam('Rituals').
genus_naam('Ryanair').
genus_naam('S&P').
genus_naam('SAR').
genus_naam('SEM').
genus_naam('Smead').
genus_naam('Sobelair').
genus_naam('SOLFA').
genus_naam('Spirit').
genus_naam('STEVIN').
genus_naam('Textkernel').
genus_naam('TBT').
genus_naam('U2').
genus_naam('VHO').
genus_naam('Vivant').
genus_naam('VMF').
genus_naam('Vodafone').
genus_naam('VROM').
genus_naam('VWS').
genus_naam('Wesbank').
genus_naam('Whitehall').
genus_naam('Yukos').

de_naam('Allerheiligen').
de_naam('Allerzielen').
de_naam('Alzheimer').
de_naam('Al-Qaida').
de_naam('al-Qaida').
de_naam('Antenna').
de_naam('Aquarius').
de_naam('Apollo').
de_naam('AR').
de_naam('ARP').
de_naam('As-Sadr').
de_naam('Barabas').
de_naam('Barroso').
de_naam('Batman').
de_naam('Belgica').
de_naam('Betuweroute').
de_naam('Beukwilg').
de_naam('Blitzkrieg').
de_naam('Bono').
de_naam('Boymans').
de_naam('Browning').
de_naam('Buford').
de_naam('Bush').
de_naam('Carter').
de_naam('Che').
de_naam('Cheney').
de_naam('Chevrolet').
de_naam('Chirac').
de_naam('CHU').
de_naam('Corland').
de_naam('Cunningham').
de_naam('D-Day').
de_naam('DDR').
de_naam('Daisy').
de_naam('Davenport').
de_naam('Demer').
de_naam('Dousberg').
de_naam('Dove').
de_naam('EEG').
de_naam('EG').
de_naam('Early').
de_naam('Eikenhorst').
de_naam('Enterprise').
de_naam('Esmeralda').
de_naam('Estienne').
de_naam('Estonia').
de_naam('Ewell').
de_naam('FARC').
de_naam('Farnese').
de_naam('Fere').
de_naam('Fiat').
de_naam('Flanagan').
de_naam('FNB').
de_naam('FNV').
de_naam('Gazastrook').
de_naam('G.R.I.').
de_naam('Gore').
de_naam('Grant').
de_naam('Grigorenko').
de_naam('Grondwet').
de_naam('Haarlemmermeer').
de_naam('HAVO').
de_naam('HBS').
de_naam('HP').
de_naam('HR-beglazing').
de_naam('HTS').
de_naam('Hamilton').
de_naam('Hanekop').
de_naam('Hector').
de_naam('Helmsley').
de_naam('Hoegaarden').
de_naam('Hughes').
de_naam('IJzer').
de_naam('Intertoys').
de_naam('IRA').
de_naam('Ida').
de_naam('IKEA').
de_naam('JFK').
de_naam('JeanJacques').
de_naam('Jiska').
de_naam('KLM').
de_naam('KNSM').
de_naam('K.N.S.M.').
de_naam('Kapp').
de_naam('Kolme').
de_naam('Kongo').
de_naam('Koninginnedag').
de_naam('KVP').
de_naam('LTS').
de_naam('Langefjord').
de_naam('Lay').
de_naam('Ligterink').
de_naam('Lila').
de_naam('Lincoln').
de_naam('Longstreet').
de_naam('MAVO').
de_naam('MEAO').
de_naam('MRI').
de_naam('MTS').
de_naam('Maas').
de_naam('Mangé').
de_naam('Mars').
de_naam('Meade').
de_naam('Mejia').
de_naam('Mercator').
de_naam('Mercurius').
de_naam('Metternich').
de_naam('Meys').
de_naam('Mezen').
de_naam('Moerdijk').
de_naam('Morgan').
de_naam('Mother').
de_naam('Mouret').
de_naam('Muggia').
de_naam('NASA').
de_naam('NAVO').
de_naam('NRC').
de_naam('NT2').
de_naam('Napoleon').
de_naam('Ned').
de_naam('Ney').
de_naam('Nuninga').
de_naam('Opel').
de_naam('OS').
de_naam('Octopus').
de_naam('Oekraïne').
de_naam('Opportunity').
de_naam('Oscar').
de_naam('Paardenmarkt').
de_naam('Pastrana').
de_naam('Pearl').
de_naam('Permeke').
de_naam('PFF').
de_naam('Place').
de_naam('Prince').
de_naam('Prinsjesdag').
de_naam('PvdA').
de_naam('PVV').
de_naam('Renault').
de_naam('Q-Music').
de_naam('Rodenbach').
de_naam('Roosevelt').
de_naam('SIC').
de_naam('Sangha').
de_naam('Schelde').
de_naam('Sharon').
de_naam('Shell').
de_naam('Sherman').
de_naam('Sinaï').
de_naam('Sint-Quintinuskathedraal').
de_naam('Slibreeks').
de_naam('SP.A').
de_naam('Spa-Francorchamps').
de_naam('Spock').
de_naam('St.-Pieter').
de_naam('Swinton').
de_naam('TV-Brussel').
de_naam('Tea').
de_naam('Tipitaka').
de_naam('Titanic').
de_naam('Tongersepoort').
de_naam('Tour').
de_naam('Tricolor').
de_naam('UGent').
de_naam('UMTS').
de_naam('Unità').
de_naam('USSR').
de_naam('Uranus').
de_naam('VVD').
de_naam('Vauban').
de_naam('VLD').
de_naam('Vondeling').
de_naam('Vroenhof').
de_naam('VTM').
de_naam('Vuelta').
de_naam('WAO').
de_naam('WTO').
de_naam('Wellington').
de_naam('Westerbouwing').
de_naam('Wilson').
de_naam('X-10').
de_naam('X-6').
de_naam('X-7').
de_naam('X-9').
de_naam('Zesdaagse').
de_naam('al-Saoed').


de_heur(Atom) :-
    atom(Atom),
    atom_concat(_,Suf,Atom),
    de_suf(Suf).

de_suf(baai).
de_suf(baan).
de_suf(basiliek).
de_suf(brug).
de_suf(gang).
de_suf(gracht).
de_suf(haven).
de_suf(kade).
de_suf(kathedraal).
de_suf(kazerne).
de_suf(kerk).
de_suf(krant).
de_suf(laan).
de_suf(lijn).
de_suf(linie).
de_suf(markt).
de_suf(school).
de_suf(singel).
de_suf(stichting).
de_suf(steeg).
de_suf(straat).
de_suf(streek).
de_suf(toren).
de_suf(transfer).
de_suf(unie).
de_suf(weg).
de_suf(wijk).
de_suf(zee).

het_naam('ABVV').
het_naam('ANP').
het_naam('ASO').
het_naam('ASTRON').
het_naam('AZG').
het_naam('Agalev').
het_naam('Ajax').
het_naam('Al-Arish').
het_naam('Alexandria').
het_naam('Alken-Maes').
het_naam('Apple').
het_naam('Aquafin').
het_naam('Arsenal').
het_naam('BIS').
het_naam('BSO').
het_naam('Bam').
het_naam('Barsela').
het_naam('Bayer').
het_naam('Belgacom').
het_naam('Berlijn').
het_naam('Beverwijk').
het_naam('Bizz').
het_naam('Blokkeer').
het_naam('Bologna').
het_naam('Buitenhof').
het_naam('CBR').
het_naam('CBS').
het_naam('CDA').
het_naam('Celtic').
het_naam('CERN').
het_naam('CHMP').
het_naam('CNV').
het_naam('COX-1').
het_naam('COX-2').
het_naam('CWI').
het_naam('Connexxion').
het_naam('DFC').
het_naam('DOS').
het_naam('DWK').
het_naam('DWS').
het_naam('D\'66').
het_naam('EPA').
het_naam('EPAR').
het_naam('ESA').
het_naam('EUAIN').
het_naam('Ecolo').
het_naam('Engels').
het_naam('Essent').
het_naam('Estudiantes').
het_naam('Excelsior').
het_naam('FAVV').
het_naam('Feijenoord').         
het_naam('Feyenoord').
het_naam('FNV').
het_naam('Frans').
het_naam('GMD').
het_naam('GRUP').
het_naam('GVAV').
het_naam('Georgia').
het_naam('Griffoendor').
het_naam('GroenLinks').
het_naam('Harvard').
het_naam('HBO').
het_naam('Heeswijk').
het_naam('Heineken').
het_naam('Herkenrode').
het_naam('HFC').
het_naam('Hoogkerk').
het_naam('I-City').
het_naam('IBM').
het_naam('IFAD').
het_naam('IMEC').
het_naam('ISBN').
het_naam('ISS').
het_naam('ISSN').
het_naam('IST').
het_naam('Inter').
het_naam('Internet').
het_naam('Israël').
het_naam('Jamathi').
het_naam('Jantje').
het_naam('Journaal').
het_naam('Juda').
het_naam('Juventus').
het_naam('Katwijk').
het_naam('KSO').
het_naam('Keulen').
het_naam('Kluwer').
het_naam('LOP').
het_naam('Levante').
het_naam('Lufthansa').
het_naam('Luxemburg').
het_naam('MGIMO').
het_naam('Mattel').
het_naam('Maagdenhuis').
het_naam('Microsoft').
het_naam('Milaan').
het_naam('Morgan').
het_naam('NEF').
het_naam('NIGZ').
het_naam('NIS').
het_naam('NKV').
het_naam('NRG4SD').
het_naam('NUV').
het_naam('NVV').
het_naam('Nazjaf').
het_naam('Netwerk').
het_naam('Noord-Zuid').
het_naam('Noordwijk').
het_naam('OIE').
het_naam('OLS').
het_naam('OM').
het_naam('Oost-Berlijn').
het_naam('OPB').
het_naam('Oranje-Nassau').
het_naam('Pietje').
het_naam('PSG').
het_naam('PSV').
het_naam('Paradiso').
het_naam('Peer').
het_naam('Peshawar').
het_naam('Petrofina').
het_naam('Philips').
het_naam('Portugees').
het_naam('Quasus').
het_naam('RBC').
het_naam('RCH').
het_naam('Real').
het_naam('Rennes').
het_naam('Ridderkerk').
het_naam('RIZIV').
het_naam('Rijswijk').
het_naam('Sabena').
het_naam('SHO').
het_naam('Siemens').
het_naam('Spaans').
het_naam('Staatsblad').
het_naam('SVV').
het_naam('Sørøy').
het_naam('TNO').
het_naam('TSO').
het_naam('Telstar').
het_naam('Theravada').
het_naam('TVM').
het_naam('U2').
het_naam('UNDP').
het_naam('UWV').
het_naam('Unilever').
het_naam('Verolme').
het_naam('VK').
het_naam('VLAO').
het_naam('VMBO').
het_naam('VWO').
het_naam('Washington').
het_naam('West-Berlijn').
het_naam('Winterswijk').
het_naam('Z33').

het_heur(Atom) :-
    atom(Atom),
    atom_concat(_,Suf,Atom),
    het_suf(Suf).

het_suf(bedrijf).
het_suf(centrum).
het_suf(comité).
het_suf(diep).
het_suf(huis).
het_suf(lyceum).
het_suf(museum).
het_suf(plantsoen).
het_suf(plein).
het_suf(spoor).
het_suf(theater).



digits([]).
digits([H|T]) :-
    alpino_latin1:isdigit(H),
    digits(T).

name_postag(both,both, sg,   'MISC', 'N(eigen,ev,basis,genus,stan)').
name_postag(both,de,   sg,   'MISC', 'N(eigen,ev,basis,zijd,stan)').
name_postag(both,het,  sg,   'MISC', 'N(eigen,ev,basis,onz,stan)').
name_postag(both,both, sg,   'LOC',  'N(eigen,ev,basis,onz,stan)'  ).
name_postag(both,de,   sg,   'LOC',  'N(eigen,ev,basis,zijd,stan)'  ).
name_postag(both,het,  sg,   'LOC',  'N(eigen,ev,basis,onz,stan)'  ).
name_postag(both,both, sg,   'PER',  'N(eigen,ev,basis,zijd,stan)' ).
name_postag(both,de,   sg,   'PER',  'N(eigen,ev,basis,zijd,stan)' ).
name_postag(both,het,  sg,   'PER',  'N(eigen,ev,basis,onz,stan)' ).
name_postag(both,both, sg,   'ORG',  'N(eigen,ev,basis,zijd,stan)'  ).
name_postag(both,de,   sg,   'ORG',  'N(eigen,ev,basis,zijd,stan)'  ).
name_postag(both,het,  sg,   'ORG',  'N(eigen,ev,basis,onz,stan)'  ).
name_postag(de,   _,   sg,   _,      'N(eigen,ev,basis,zijd,stan)' ).
name_postag(het,  _,   sg,   _,      'N(eigen,ev,basis,onz,stan)'  ).
name_postag(genus,_,   sg,   _,      'N(eigen,ev,basis,genus,stan)'  ).
name_postag(both,both, both,   'MISC', 'N(eigen,ev,basis,genus,stan)').
name_postag(both,de,   both,   'MISC', 'N(eigen,ev,basis,zijd,stan)').
name_postag(both,het,  both,   'MISC', 'N(eigen,ev,basis,onz,stan)').
name_postag(both,both, both,   'LOC',  'N(eigen,ev,basis,onz,stan)'  ).
name_postag(both,de,   both,   'LOC',  'N(eigen,ev,basis,zijd,stan)'  ).
name_postag(both,het,  both,   'LOC',  'N(eigen,ev,basis,onz,stan)'  ).
name_postag(both,both, both,   'PER',  'N(eigen,ev,basis,zijd,stan)' ).
name_postag(both,de,   both,   'PER',  'N(eigen,ev,basis,zijd,stan)' ).
name_postag(both,het,  both,   'PER',  'N(eigen,ev,basis,onz,stan)' ).
name_postag(both,both, both,   'ORG',  'N(eigen,ev,basis,zijd,stan)'  ).
name_postag(both,de,   both,   'ORG',  'N(eigen,ev,basis,zijd,stan)'  ).
name_postag(both,het,  both,   'ORG',  'N(eigen,ev,basis,onz,stan)'  ).
name_postag(de,   _, both, _,      'N(eigen,ev,basis,zijd,stan)' ).
name_postag(het,  _, both, _,      'N(eigen,ev,basis,onz,stan)'  ).
name_postag(genus,_, both, _,      'N(eigen,ev,basis,genus,stan)'  ).
name_postag(_,    _, pl,   _,      'N(eigen,mv,basis)'           ).

find_mother_node(Q0,Q,Result,Node):-
    alpino_data:result_term(_,_,_,Tree,_,Result),
    subtree_mother(Tree,Q0,Q,none,Node).

subtree_mother(tree(_,_,lex(ref(_,_,_,_,_,_,U0,U,HIS,_,_)),_),Q0,Q,Mother,Mother) :-
    unskip(HIS,U0,U,Q0,Q).
subtree_mother(tree(Mother,_,List,_),Q0,Q,_,Node) :-
    lists:member(Tree,List),
    subtree_mother(Tree,Q0,Q,Mother,Node).

find_node(Q0,Q,Result,Node) :-
    alpino_data:result_term(_,_,_,Tree,_,Result),
    subtree(Tree,Q0,Q,Node).

subtree(tree(Node,_,lex(ref(_,_,_,_,_,_,U0,U,HIS,_,_)),_),Q0,Q,Node) :-
    unskip(HIS,U0,U,Q0,Q).
subtree(tree(_,_,List,_),Q0,Q,Node) :-
    lists:member(Tree,List),
    subtree(Tree,Q0,Q,Node).

unskip(skip(_,Left,Right,_),U0,U,Q0,Q):-
    !,
    length(Left,L),  Q0 is U0 + L,
    length(Right,R), Q  is U  - R.
unskip(_,U0,U,U0,U).

find_path(Q0,Q,Result,Path) :-
    alpino_data:result_term(_,_,_,Tree,_,Result),
    subtree_path(Tree,Q0,Q,[],Path).

subtree_path(tree(_,_,lex(ref(_,_,_,_,_,_,U0,U,HIS,_,_)),_),Q0,Q,Path,Path) :-
    unskip(HIS,U0,U,Q0,Q).
subtree_path(tree(_,Rule,List,_),Q0,Q,Path0,Path) :-
    lists:nth(N,List,Tree),
    subtree_path(Tree,Q0,Q,[Rule/N|Path0],Path).



mwu_postag(Frame,Stem,Surf,Q0,Q,_Result) -->
    { mwu_postag_frame_stem_surf(Frame,Stem,Surf,Tags) },
    mwu_tags(Tags,Stem,Q0,Q).

mwu_postag(Frame,Stem,Surf,Q0,Q,_Result) -->
    { mwu_postag_frame_stem_surf(Frame,Stem,Surf,Tags,Stems) },
    mwu_tags(Tags,Stems,Q0,Q).

mwu_postag(Frame,Stem,Surf,Q0,Q,_Result) -->
    { mwu_postag_frame_surf(Frame,Surf,Tags) },
    mwu_tags(Tags,Stem,Q0,Q).

mwu_postag(Frame,_Stem,Surf,Q0,Q,_Result) -->
    { mwu_postag_frame_surf(Frame,Surf,Tags,Stems) },
    mwu_tags(Tags,Stems,Q0,Q).

mwu_postag(Frame,Stem,_Surf,Q0,Q,_Result) -->
    { mwu_postag_frame_stem(Frame,Stem,Tags,Stems) },
    mwu_tags_stems(Tags,Stems,Q0,Q).

mwu_postag(Frame,Stem,_Surf,Q0,Q,_Result) -->
    { mwu_postag_frame_stem(Frame,Stem,Tags) },
    mwu_tags(Tags,Stem,Q0,Q).

mwu_postag(_Frame,Stem,Surf,Q0,Q,_Result) -->
    { mwu_postag(Stem,Surf,Tags,Stems) },
    mwu_tags_stems(Tags,Stems,Q0,Q).
mwu_postag(proper_name(_,'MISC'),_,Surf,Q0,Q,_) -->
    {  guess_vreemd(Surf,SurfEls) },
    !,
    mwu_vreemd_tags(SurfEls,Q0,Q).
% in principle, this ought to work, but it harms more than it helps
%mwu_postag(proper_name(M,'MISC'),_,Surf,Q0,Q,_Result) -->
%    { alpino_util:split_atom(Surf," ",Surfs) },
%    guess_tags(Q0,Q,proper_name(M),Surfs),
%    !.
mwu_postag(proper_name(_,_),_,Surf,Q0,Q,_Result) -->
    mwu_name_tags(Surf,Q0,Q).

mwu_postag(punct(_),_,Surf,Q0,Q,_) -->
    mwu_punct_tags(Surf,Q0,Q),
    !.

mwu_postag(preposition(A,B),_Stem,Surf,Q0,Q,_) -->
    {  atom(Surf),
       alpino_util:split_atom(Surf," ",SurfEls),
       length(SurfEls,Len),
       Len is Q-Q0
    },
    guess_tag_list(SurfEls,preposition(A,B),Q0,Q).

mwu_postag(er_adverb(A),_Stem,Surf,Q0,Q,_) -->
    {  atom(Surf),
       alpino_util:split_atom(Surf," ",SurfEls),
       length(SurfEls,Len),
       Len is Q-Q0
    },
    guess_tag_list(SurfEls,er_adverb(A),Q0,Q).


mwu_postag(waar_adverb(A),_Stem,Surf,Q0,Q,_) -->
    {  atom(Surf),
       alpino_util:split_atom(Surf," ",SurfEls),
       length(SurfEls,Len),
       Len is Q-Q0
    },
    guess_tag_list(SurfEls,waar_adverb(A),Q0,Q).

mwu_punct_tags(Stem,Q0,Q) -->
    {  atom(Stem),
       alpino_util:split_atom(Stem," ",Words),
       length(Words,Len),
       Len =:= Q-Q0
    },
    mwu_punct_tags_(Words,Q0,Q).

mwu_punct_tags_([],Q,Q) --> [].
mwu_punct_tags_([W|Words],Q0,Q) -->
    { Q1 is Q0 + 1 },
    [ cgn_postag(Q0,Q1,W,'LET()') ],
    mwu_punct_tags_(Words,Q1,Q).


%% 
mwu_name_tags(Stem,Q0,Q) -->
    {  atom(Stem),
       alpino_util:split_atom(Stem," ",Words),
       length(Words,Len),
       Len =:= Q-Q0
    },
    mwu_name_tags_(Words,Q0,Q).

mwu_name_tags_([],Q,Q) --> [].
mwu_name_tags_([W|Words],Q0,Q) -->
     { Q1 is Q0 + 1 },
     mwu_name_tag(W,Q0,Q1),
     mwu_name_tags_(Words,Q1,Q).

mwu_name_tag(Punct,Q0,Q) -->
    { punct(Punct)},
    !,
    [cgn_postag(Q0,Q,Punct,'LET()')].
mwu_name_tag(Punct,Q0,Q) -->
    [cgn_postag(Q0,Q,Punct,'SPEC(deeleigen)')].

punct(X) :-
    alpino_lex:lexicon(punct(_),_,[X],[],_),
    \+ symb(X,_).  % + &

mwu_tags_stems([],_,Q,Q) --> [].
mwu_tags_stems([H|T],[Stem|Stems],Q0,Q) -->
    [cgn_postag(Q0,Q1,Stem,H)],
    { Q1 is Q0 + 1 },
    mwu_tags_stems(T,Stems,Q1,Q).


mwu_tags(List,Stem,Q0,Q) -->
    {  length(List,Len),
       atom(Stem),
       alpino_util:split_atom(Stem," ",Stems),
       length(Stems,Len)
    },
    !,
    mwu_tags_stems(List,Stems,Q0,Q).

mwu_tags(List,Stem,Q0,Q) -->
    mwu_tags(List,Stem,1,Q0,Q).

mwu_tags([],_,_,Q,Q) --> [].
mwu_tags([H|T],Stem,Pos,Q0,Q) -->
    [cgn_postag(Q0,Q1,Pos/Stem,H)],
    { Q1 is Q0 + 1,
      Pos1 is Pos + 1},
    mwu_tags(T,Stem,Pos1,Q1,Q).

%% ADJ(nom,sup,zonder,zonder-n)

mwu_postag_frame_surf(tag,Surf,['SPEC(symb)','LET()'],Surf) :-
    atom(Surf),
    alpino_util:split_atom(Surf," ",[_,')']).

mwu_postag_frame_surf(with_dt(np(year),_),Surf,[ATag,'LET()',BTag],Stem) :-
    atom(Surf),
    atom_codes(Surf,Codes),
    alpino_util:codes_to_words(Codes,[A,'-',B]),
    num_postag(A,ATag),
    num_postag(B,BTag),
    hdrug_util:concat_all([A,'-',B],Stem,' ').

mwu_postag_frame_surf(number(hoofd(both)),Surf,['SPEC(symb)','LET()','SPEC(symb)']) :-
    atom(Surf),
    atom_codes(Surf,Codes),
    alpino_util:codes_to_words(Codes,[A,'-',B]),
    phone(A),
    atom(B),
    atom_codes(B,BCodes),
    alpino_lex:number_codes_silent(Bnum,BCodes),
    integer(Bnum).

mwu_postag_frame_surf(number(hoofd(both)),Surf,[ATag,'LET()',BTag]) :-
    atom(Surf),
    atom_codes(Surf,Codes),
    alpino_util:codes_to_words(Codes,[A,'-',B]),
    num_postag(A,ATag),
    num_postag(B,BTag).    

mwu_postag_frame_surf(number(hoofd(both)),Surf,[ATag,'SPEC(symb)',BTag]) :-
    atom(Surf),
    atom_codes(Surf,Codes),
    alpino_util:codes_to_words(Codes,[A,'x',B]),
    num_postag(A,ATag),
    num_postag(B,BTag).    

mwu_postag_frame_surf(number(hoofd(_)),Surf,['SPEC(symb)',BTag]) :-
    atom(Surf),
    atom_codes(Surf,Codes),
    alpino_util:codes_to_words(Codes,['+',B]),
    num_postag(B,BTag).    

mwu_postag_frame_surf(number(hoofd(_)),Surf,['LET()',BTag]) :-
    atom(Surf),
    atom_codes(Surf,Codes),
    alpino_util:codes_to_words(Codes,['(',B]),
    num_postag(B,BTag).    

mwu_postag_frame_surf(score_cat,Surf,[ATag,'LET()',BTag]) :-
    atom(Surf),
    atom_codes(Surf,Codes),
    alpino_util:codes_to_words(Codes,[A,'-',B]),
    num_postag(A,ATag),
    num_postag(B,BTag).

mwu_postag_frame_stem_surf(name_determiner(pron,_),_Stem,Surf,PosList) :-
    atom(Surf),
    alpino_util:split_atom(Surf," ",[_|Parts]),
    gen_name_parts(Parts,PosList).

mwu_postag_frame_stem_surf(adjective(het_st(_)),Stem,Surf,['LID(bep,stan,evon)',TweedePos]) :-
    atom_concat('het ',_,Stem),
    atom_concat(_,Gek,Surf),
    st(Gek,TweedePos),
    !.
mwu_postag_frame_stem_surf(adjective(het_st(_)),Stem,Surf,['LID(bep,stan,evon)','ADJ(vrij,sup,zonder)']) :-
    atom(Stem),
    atom_concat('het ',_,Stem),
    atom_concat(_,st,Surf).
mwu_postag_frame_stem_surf(adjective(het_st(_)),Stem,Surf,['LID(bep,stan,evon)','ADJ(nom,sup,met-e,zonder-n,stan)']) :-
    atom(Stem),
    atom_concat('het ',_,Stem),
    atom_concat(_,ste,Surf).
mwu_postag_frame_stem_surf(adjective(het_st(_)),_,Surf,['VZ(init)',Pron,Tag]) :-
    atom(Surf),
    atom_codes(Surf,Codes),
    alpino_util:codes_to_words(Codes,[Op,Zijn,Vroeg]),
    op(Op),
    zijn_tag(Zijn,Pron,_),
    (   Vroeg == minst
    ->  Tag = 'VNW(onbep,grad,stan,vrij,zonder,sup)'
    ;   Tag = 'ADJ(nom,sup,zonder,zonder-n)'
    ).

mwu_postag_frame_stem_surf(fixed_part(op_een_v),v_root(_,Stem),_,
       ['VZ(init)','LID(onbep,stan,agr)','WW(inf,nom,zonder,zonder-n)'],Stem).

mwu_postag_frame_stem_surf(particle(tekort),tekort,'te kort',['BW()','ADJ(vrij,basis,zonder)'],'te kort').

mwu_postag_frame_stem_surf(er_adverb([_In,_Ruil,_Voor]),Stem,Surf,[Atag,Btag,'BW()'],Stems) :-
    mwu_postag(Stem,[Atag,Btag,_],[Astem,Bstem,_]),
    atom(Surf),
    alpino_util:split_atom(Surf," ",[_,_,Cstem]),
    hdrug_util:concat_all([Astem,Bstem,Cstem],Stems,' ').

mwu_postag_frame_stem_surf(waar_adverb([_In,_Ruil,_Voor]),Stem,Surf,[Atag,Btag,'BW()'],Stems) :-
    mwu_postag(Stem,[Atag,Btag,_],[Astem,Bstem,_]),
    atom(Surf),
    alpino_util:split_atom(Surf," ",[_,_,Cstem]),
    hdrug_util:concat_all([Astem,Bstem,Cstem],Stems,' ').

mwu_postag_frame_stem_surf(er_adverb([_In,_Ruil,_Voor,_Van]),Stem,Surf,[Atag,Btag,Ctag,'BW()'],Stems) :-
    mwu_postag(Stem,[Atag,Btag,Ctag,_],[Astem,Bstem,Cstem,_]),
    atom(Surf),
    alpino_util:split_atom(Surf," ",[_,_,_,Dstem]),
    hdrug_util:concat_all([Astem,Bstem,Cstem,Dstem],Stems,' ').

mwu_postag_frame_stem_surf(waar_adverb([_In,_Ruil,_Voor,_Van]),Stem,Surf,[Atag,Btag,Ctag,'BW()'],Stems) :-
    mwu_postag(Stem,[Atag,Btag,Ctag,_],[Astem,Bstem,Cstem,_]),
    atom(Surf),
    alpino_util:split_atom(Surf," ",[_,_,_,Dstem]),
    hdrug_util:concat_all([Astem,Bstem,Cstem,Dstem],Stems,' ').

mwu_postag_frame_stem_surf(complementizer(alsof),alsof,'als of',['VG(onder)','VG(onder)'],'als of').

%mwu_postag_frame_stem_surf(verb(zijn,inf(no_e),ninv(intransitive,part_intransitive('ten onder'))),
%			   v_root(ga_ten_onder,ten_onder_gaan),'ten ondergaan',['VZ(versm)','WW(inf,vrij,zonder)'],'te onder_gaan').

mwu_postag_frame_stem_surf(verb(zijn,INFL,ninv(intransitive,part_intransitive('ten onder'))),
			   v_root(ga_ten_onder,ten_onder_gaan),SURF,['VZ(versm)',POS],'te onder_gaan'):-
    atom(SURF),
    alpino_util:split_atom(SURF," ",[ten,_]),
    cgn_postag_c(verb(_,INFL,intransitive),POS).

st(eerst,'TW(rang,nom,zonder-n)').
st(eerste,'TW(rang,nom,zonder-n)').
st(meest,'VNW(onbep,grad,stan,vrij,zonder,sup)').
st(meeste,'VNW(onbep,grad,stan,nom,met-e,zonder-n,sup)').
st(minst,'VNW(onbep,grad,stan,vrij,zonder,sup)').
st(minste,'VNW(onbep,grad,stan,nom,met-e,zonder-n,sup)').

mwu_postag_frame_stem(noun(both,mass,sg),'een of ander',
		      ['TW(hoofd,nom,zonder-n,basis)','VG(neven)','ADJ(nom,basis,zonder,zonder-n)'],
		      [één,of,ander]).

mwu_postag_frame_stem(adjective(pred_er(_)),Stem,['ADJ(vrij,comp,zonder)','VZ(fin)'],[Kalm,aan]) :-
    atom(Stem),
    alpino_util:split_atom(Stem," ",[AdjEr,aan]),
    alpino_lex:lexicon(adjective(er(_)),Kalm,[AdjEr],[],_).
mwu_postag_frame_stem(np(year),Stem,PosTags,Stems) :-
    atom(Stem),
    alpino_util:split_atom(Stem," ",Words),
    tmp_np(Words,PosTags,Stems).
mwu_postag_frame_stem(tmp_np,Stem,PosTags,Stems) :-
    atom(Stem),
    alpino_util:split_atom(Stem," ",Words),
    tmp_np(Words,PosTags,Stems).

mwu_postag_frame_stem(pre_np_adverb,Stem,['N(soort,ev,basis,onz,stan)','TW(hoofd,vrij)']):-
    atom(Stem),
    atom_concat(nummer,_,Stem).
mwu_postag_frame_stem(pre_np_adverb,Stem,['N(soort,ev,basis,zijd,stan)','TW(hoofd,vrij)']):-
    atom(Stem),
    atom_concat(top,_,Stem).
mwu_postag_frame_stem(with_dt(adverb,_),'des te meer',['BW()','BW()','VNW(onbep,grad,stan,vrij,zonder,comp)']).

mwu_postag_frame_stem(with_dt(determiner(_),_),'des te meer',['BW()','BW()','VNW(onbep,grad,stan,prenom,zonder,agr,comp)']).

mwu_postag_frame_stem(er_adverb(_),Stem,['VNW(aanw,adv-pron,obl,vol,3o,getal)','BW()']) :-
    atom(Stem),
    atom_concat('daar ',_,Stem).
mwu_postag_frame_stem(er_adverb(_),Stem,['VNW(aanw,adv-pron,obl,vol,3o,getal)','BW()']) :-
    atom(Stem),
    atom_concat('hier ',_,Stem).
mwu_postag_frame_stem(er_adverb(_),Stem,['VNW(aanw,adv-pron,stan,red,3,getal)','BW()']) :-
    atom(Stem),
    atom_concat('er ',_,Stem).
mwu_postag_frame_stem(waar_adverb(_),Stem,['VNW(vb,adv-pron,obl,vol,3o,getal)','BW()']) :-
    atom(Stem),
    atom_concat('waar ',_,Stem).
mwu_postag_frame_stem(noun(het,count,pl),gelijkspel,['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)']).

%% 74 jarige
mwu_postag_frame_stem(adjective(e),Stem,['TW(hoofd,vrij)','ADJ(prenom,basis,met-e,stan)']) :-
    atom(Stem),
    alpino_util:split_atom(Stem," ",[Num,Jarig]),
    jarig(Jarig),
    alpino_lex:parse_number(Num,_).

%% 74 jarig (context dependent, really)
mwu_postag_frame_stem(adjective(no_e(_)),Stem,['TW(hoofd,vrij)','ADJ(prenom,basis,zonder)']) :-
    atom(Stem),
    alpino_util:split_atom(Stem," ",[Num,Jarig]),
    jarig(Jarig),
    alpino_lex:parse_number(Num,_).

mwu_postag_frame_stem(noun(DeHet,_,SgPl),Stem,[Tag|Tags]) :-
    atom(Stem),
    alpino_util:split_atom(Stem," ",[P|Parts]),
    (   post_h_word(P,_)
    ;   alpino_util:split_atom(P,"_",[Pref,_]),
	post_h_word(Pref,_)
    ),
    noun_postag(DeHet,DeHet,SgPl,Tag),
    deeleigen(Parts,Tags).

mwu_postag_frame_stem(proper_name(both,_),Stem,[Tag|Tags]) :-
    atom(Stem),
    alpino_util:split_atom(Stem," ",[P|Parts]),
    (   post_h_word(P,DeHet)
    ;   alpino_util:split_atom(P,"-",[Pref,_]),
	post_h_word(Pref,DeHet)
    ),
    noun_postag(DeHet,DeHet,both,Tag),
    deeleigen(Parts,Tags).

jarig(jarig).

rang(Tweede,Twee) :-
    alpino_lex:lexicon(number(rang),Twee,[Tweede],[],_),
    !.
%% prefer the rule above for correct lemma, but this one
%% knows about 16e XVIIIe etc
rang(Tweede,Tweede) :-
    alpino_lex:rang(Tweede).

tmp_np([],[],[]).
tmp_np([Num,'-',Num2,Uur],['TW(hoofd,vrij)','LET()','TW(hoofd,vrij)',UurTag],[Num,'-',Num2,UurStem]) :-
    uur_num(Num),
    uur_num(Num2),
    lists:member(Uur,[uur,'u.']),
    !,
    tmp_np1(Uur,[],UurTag,UurStem).
tmp_np([W|Ws],[P|Ps],[S|Ss]) :-
    tmp_np1(W,Ws,P,S),
    tmp_np(Ws,Ps,Ss).

uur_num(Num) :-
    simple_number(Num,_).
uur_num(Num) :-
    atom(Num),
    alpino_util:split_atom(Num,":",[A,B]),
    simple_number(A,_),
    simple_number(B,_).    

tmp_np1(Num,[Uur|_],'TW(hoofd,vrij)',Num) :-
    lists:member(Uur,[uur,'u.']),
    uur_num(Num).

tmp_np1(Num,[Minuten|_],'TW(hoofd,prenom,stan)',Num) :-
    lists:member(Minuten,[minuten]),
    simple_number(Num,_).

tmp_np1(een,[_,of|_],'LID(onbep,stan,agr)',een).
tmp_np1(een,_,'TW(hoofd,vrij)',één).
tmp_np1(minuut,_,'N(soort,ev,basis,zijd,stan)',minuut).
tmp_np1(minuten,_,'N(soort,mv,basis)',minuut).
tmp_np1(Tweede,Right,Tag,Twee) :-
    rang(Tweede,Twee),
    (   Right == []
    ->  Tag = 'TW(rang,nom,zonder-n)'
    ;   Tag = 'TW(rang,prenom,stan)'
    ).
tmp_np1(Str,_,'SPEC(afk)',Stem) :-
    afkorting(Str,Stem).
tmp_np1(Atom,_,'N(eigen,ev,basis,zijd,stan)',Stem) :-
    alpino_unknowns:decap(Atom,Stem),
    alpino_lex:date_month([Stem],[]).
tmp_np1(W,_Ctxt,P,W) :-
    tmp_np1(W,P).

tmp_np1('+','SPEC(symb)').
tmp_np1(Atom,'N(eigen,ev,basis,zijd,stan)') :-
    alpino_lex:date_month([Atom],[]).
tmp_np1(Atom,'TW(hoofd,vrij)'):-
    simple_number(Atom,Val),
    integer(Val).
tmp_np1(Atom,'SPEC(symb)') :-
    alpino_lex:num_dot_num(_,Atom).
tmp_np1(half,'ADJ(prenom,basis,zonder)').
tmp_np1(kwart,'N(soort,ev,basis,onz,stan)').
tmp_np1(uur,'N(soort,ev,basis,onz,stan)').
tmp_np1(voor,'VZ(init)').
tmp_np1(na,'VZ(init)').
tmp_np1(tot,'VZ(init)').
tmp_np1(met,'VZ(init)').
tmp_np1(en,'VG(neven)').
tmp_np1(over,'VZ(init)').
tmp_np1(de,'LID(bep,stan,rest)').
tmp_np1('Christus','N(eigen,ev,basis,zijd,stan)').
tmp_np1(of,'VG(neven)').
tmp_np1(Atom,'LET()') :-
    punct(Atom).
tmp_np1(Atom,Tag) :-
    atom(Atom),
    atom_codes(Atom,String),
    tmp_np2(String,Tag).
tmp_np1(_,'TW(hoofd,vrij)').

tmp_np2(String,'SPEC(symb)'):-
    alpino_util:split_string(String,"/",[Left,Right]),
    alpino_lex:number_codes_silent(_,Left),
    alpino_lex:number_codes_silent(_,Right).
tmp_np2([48,N|_],'SPEC(symb)') :-	% 010 070 0345
    N > 47, N < 58.
tmp_np2(String,'TW(hoofd,vrij)'):-
    alpino_lex:number_codes_silent(_,String).

afkorting('n.',na).
afkorting('n.C.','na Christus').
afkorting('nov.',november).
afkorting('t/m','tot en met').
afkorting('u.',uur).
afkorting('v.',voor).
afkorting('v.Chr.','voor Christus').
afkorting('v.C.','voor Christus').
afkorting('Chr.','Christus').
afkorting('Chr','Christus').

afkorting('jan.',januari).
afkorting('feb.',februari).
afkorting('febr.',februari).
afkorting('mrt.',maart).
afkorting('aug.',augustus).
afkorting('sep.',september).
afkorting('sept.',september).
afkorting('okt.',oktober).
afkorting('nov.',november).
afkorting('dec.',december).
afkorting(jan,januari).
afkorting(feb,februari).
afkorting(febr,februari).
afkorting(mrt,maart).
afkorting(apr,april).
afkorting(jun,juni).
afkorting(jul,juli).
afkorting(aug,augustus).
afkorting(sep,september).
afkorting(sept,september).
afkorting(okt,oktober).
afkorting(nov,november).
afkorting(dec,december).


op(op).
op('Op').
op('OP').

zijn_tag(zijn,'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)', zijn).
zijn_tag('z\'n','VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)', zijn).
zijn_tag('haar','VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)', haar).
zijn_tag('d\'r','VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)', haar).
zijn_tag(mijn,'VNW(bez,det,stan,vol,1,ev,prenom,zonder,agr)', mijn).
zijn_tag('m\'n','VNW(bez,det,stan,red,1,ev,prenom,zonder,agr)',mijn).
zijn_tag('M\'n','VNW(bez,det,stan,red,1,ev,prenom,zonder,agr)',mijn).
zijn_tag(hun,'VNW(bez,det,stan,vol,3,mv,prenom,zonder,agr)',hum).
zijn_tag(je,'VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)',je).

num_postag(A,'TW(hoofd,vrij)') :-
    atom(A),
    atom_codes(A,ACodes),
    alpino_lex:number_codes_silent(Anum,ACodes),
    integer(Anum),
    !.
num_postag(Pun,'SPEC(symb)'):-
    symb(Pun,_),
    !.
num_postag(Pun,'LET()'):-
    punct(Pun),
    !.
num_postag(_A,'SPEC(symb)').
%    alpino_lex:num_dot_num(_,A).

:- use_module(mwu).

with_dt_tags(Tree,Q0,L0,L) :-
    (   with_dt_tags_(Tree,Q0,L0,L)
    ->  true
    ;   hdrug_util:debug_message(1,"warning: with_dt_tags failed~n",[]),
	hdrug_util:debug_message(2,"~w~n",[Tree]),
	L0 = L
    ).

with_dt_tags_l_l(Stem0,Frame,R0,R,Q0) -->
    [cgn_postag(S0,S,Stem,Tag)],
    {  integer(R0),
       integer(R),
       R is R0 + 1,
       !,
       S0 is Q0 + R0,
       S is Q0 + R,
       cgn_postag_l(Stem0,Stem,Frame,Tag)
    }.
with_dt_tags_l_l(Stem,Tag,R0,R,Q0) -->
    { integer(R0),
      integer(R),
      R - R0 > 1 },
    !,
    { atom(Stem),
      alpino_util:split_atom(Stem," ",Words),
      S0 is Q0 + R0,
      S  is Q0 + R
    },
    guess_tags(S0,S,Tag,Words).
    

with_dt_tags_(dt(_,List),Q0) -->
    with_dt_tags_list(List,Q0).
with_dt_tags_(ix(_,DT),Q0) -->
    with_dt_tags_(DT,Q0).
with_dt_tags_(ix(_),_Q0) --> [].
with_dt_tags_(l(Stem,Frame,R0,R),Q0) -->
    with_dt_tags_l_l(Stem,Frame,R0,R,Q0).
with_dt_tags_(l(Stem,Frame,_Cat,R0,R),Q0) -->
    with_dt_tags_l_l(Stem,Frame,R0,R,Q0).

with_dt_tags_(orig(_),_) --> [].

with_dt_tags_list([],_) --> [].
with_dt_tags_list([_=DT|T],Q0) -->
    with_dt_tags_(DT,Q0),
    with_dt_tags_list(T,Q0).

cgn_postag_l(Stem0,Stem,Frame0,Tag) :-
    frame_map(Frame0,Stem0,Frame),
    cgn_postag_l2(Stem0,Stem,Frame,Tag).

cgn_postag_l2(bepaald,bepalen,adjective(ge_no_e(adv)),'WW(vd,vrij,zonder)').
cgn_postag_l2(al,al,predm_adverb,'VNW(onbep,det,stan,nom,met-e,mv-n)').

cgn_postag_l2(Stem,Stem,Frame,Tag) :-
    stem_dependent_tag(Frame,Stem,Tag), !.
cgn_postag_l2(Stem,Stem2,Frame,Tag) :-
    exceptional_stem_tag(Stem,Frame,Tag,Stem2), !.
cgn_postag_l2(Stem,Stem,Frame,Tag) :-
    exceptional_stem_tag(Stem,Frame,Tag), !.
cgn_postag_l2(Stem,Stem,Frame,Tag) :-
    cgn_postag_c(Frame,Tag), !.
cgn_postag_l2(Stem,Stem,Frame,'NA()') :-
    format(user_error,"no with_dt cgn tag rule for ~w ~w~n",[Stem,Frame]).

number_vrij_path([alle_zes/2|_]).
number_vrij_path([n_n_num_app/2|_]).
number_vrij_path([n_num/1,np_n/1|_]).
number_vrij_path([n_num/1,np_det_n/2|_]).
number_vrij_path([num_num_adv_num/2|Tail]) :-
    number_vrij_path(Tail).
number_vrij_path([n_rang/1|_]).
number_vrij_path([pred_a/1|_]).
number_vrij_path([start_coord(_,_)/1|Path]) :-
    number_vrij_path(Path).
number_vrij_path([end_coord(_,_)/4,start_coord(_,_)/2|Path]) :-
    number_vrij_path(Path).

post_noun_path([a_pp_a/2|Path]) :-
    post_noun_path(Path).
post_noun_path([a_adv_a/2|Path]) :-
    post_noun_path(Path).
post_noun_path([a_a_pp/1|Path]) :-
    post_noun_path(Path).
post_noun_path([a_a_pp_comp/1|Path]) :-
    post_noun_path(Path).
post_noun_path([a_a_np_comp/1|Path]) :-
    post_noun_path(Path).
post_noun_path([a_np_comp_a/2|Path]) :-
    post_noun_path(Path).
post_noun_path([n_n_mod_a/2|_]).

number_nom_path([num_num_adv_num/2|Path]) :-
    number_nom_path(Path).
number_nom_path([n_num/1,n_n_pps/1|_]).
number_nom_path([n_num/1,np_n/1,rel_pp_np_dp/2|_]).
number_nom_path([pron_pron_pps/1|_]).

noun_path([a_num_na_a/2|Path]) :-
    noun_path(Path).
noun_path([n_adj/1|_]).
noun_path([n_rang/1|_]).
noun_path([a_adv_a/2|Path]) :-
    noun_path(Path).

adv_path([a_int_adv_a/2|Path]) :-
    adv_path(Path).
adv_path([a_adv_a/2|Path]) :-
    adv_path(Path).
adv_path([a_a_pp/1|Path]) :-
    adv_path(Path).
adv_path([adj_genoeg/1|_]).
adv_path([adv_a/1|_]).
adv_path([a_a_compp/1|T]) :-
    adv_path(T).
adv_path([a_detadv_a/2|Path]) :-
    adv_path(Path).
adv_path([a_me_comp_a/2|Path]) :-
    adv_path(Path).

pronoun_path([a_num_na_a/2|Path]) :-
    pronoun_path(Path).
pronoun_path([pred_a/1|_]).
pronoun_path([pron_det/1|_]).
pronoun_path([a_a_compp/1|Path]) :-
    pronoun_path(Path).
pronoun_path([a_me_comp_a/2,a_a_compp/1|Path]) :-
    pronoun_path(Path).
pronoun_path([a_me_comp_a/2|Path]) :-
    pronoun_path(Path).
pronoun_path([np_pron_weak/1|_]).
pronoun_path([a_int_adv_a/2|Tail]):-
    pronoun_path(Tail).
pronoun_path([a_detadv_a/2|Tail]):-
    pronoun_path(Tail).
pronoun_path([a_detadv_a/2,a_a_compp/1|Tail]):-
    pronoun_path(Tail).
pronoun_path([det_adj/1,pron_det/1|_]).
pronoun_path([det_adj/1,np_det_n/1|_]):-!,fail.

od_is_adj(v_root(_,X),Y) :-
    od_is_adj(X,Y).
od_is_adj(aan_palen,aanpalend).
od_is_adj(door_gaan,doorgaand).
od_is_adj(aanstaand,aanstaand).

vd_is_adj(v_root(_,X),Y) :-
    vd_is_adj(X,Y).
vd_is_adj(aangeboren,aangeboren).
vd_is_adj(begaan,begaan).
vd_is_adj(behoud_gezind,behoud_gezind).
vd_is_adj(benauwen,benauwd).
vd_is_adj(benieuwen,benieuwd).
vd_is_adj(bereid,bereid).
vd_is_adj(beschamen,beschaamd).
vd_is_adj(bezeten,bezeten).
vd_is_adj(bezorgd,bezorgd).
vd_is_adj(bijgenaamd,bijgenaamd).
vd_is_adj(baren,gebaard).
vd_is_adj(geleden,geleden).
vd_is_adj(geliefd,geliefd).
vd_is_adj(gemiddeld,gemiddeld).
vd_is_adj(genaamd,genaamd).
vd_is_adj(gezind,gezind).
vd_is_adj(goed_gezind,goed_gezind).
vd_is_adj(rimpelen,gerimpeld).
vd_is_adj(geschikt,geschikt).
vd_is_adj(tinten,getint).
vd_is_adj(gewond,gewond).
vd_is_adj(goedgemutst,goedgemutst).
vd_is_adj(inbegrepen,inbegrepen).
vd_is_adj(in_tijgen,ingetogen).
vd_is_adj(ongewenst,ongewenst).
vd_is_adj(ongewild,ongewild).
vd_is_adj(ontstellen,ontsteld).
vd_is_adj(tegenover_stellen,tegenovergesteld).
vd_is_adj(verbijten,verbeten).
vd_is_adj(verleden,verleden).
vd_is_adj(vertrouwd,vertrouwd).
vd_is_adj(volmaakt,volmaakt).
vd_is_adj(wereldberoemd,wereldberoemd).
vd_is_adj(zelfverklaard,zelfverklaard).
vd_is_adj(zogeheten,zogeheten).
vd_is_adj(zogenaamd,zogenaamd).
vd_is_adj(zogenoemd,zogenoemd).

frame_map(Frame0,Lemma,Frame) :-
    (   frame_map_(Frame0,Lemma,Frame1)
    ->  Frame1 = Frame
    ;   Frame0 = Frame
    ).

frame_map_(proper_name(Agr),_,                 proper_name(Agr,'MISC')).
frame_map_(adjective(postn_no_e(X)),_,         adjective(no_e(X))).
frame_map_(adjective(postn_pred(X)),_,         adjective(pred(X))).
frame_map_(adjective(postn_both(X)),_,         adjective(both(X))).
frame_map_(adjective(X,_),_,                   adjective(X)).
frame_map_(adjective(er(A)),    ander,         adjective(no_e(A))).
frame_map_(adjective(er(A),_),  ander,         adjective(no_e(A))).
frame_map_(adjective(ere),      ander,         adjective(e)).
frame_map_(adjective(ere,_),    ander,         adjective(e)).
frame_map_(post_adjective(er),  ander,         post_adjective(no_e)).
frame_map_(post_adjective(er,_),ander,         post_adjective(no_e)).
frame_map_(me_adjective(X),_,                  adjective(X)).
frame_map_(me_adjective(X,_),_,                adjective(X)).
frame_map_(np_me_adjective(_,X),_,             np_me_adjective(X)).
frame_map_(post_adjective(X,_),_,              post_adjective(X)).
frame_map_(noun(A,B,C,_),_,                    noun(A,B,C)).
frame_map_(tmp_noun(A,B,C),_,                  noun(A,B,C)).
frame_map_(tmp_noun(A,B,C,_),_,                noun(A,B,C)).
frame_map_(mod_noun(A,B,C),_,                  noun(A,B,C)).
frame_map_(mod_noun(A,B,C,_),_,                noun(A,B,C)).
frame_map_(meas_mod_noun(both,count,meas),procent,noun(het,count,meas)).
frame_map_(meas_mod_noun(A,B,C),_,             noun(A,B,C)).
frame_map_(meas_mod_noun(A,B,C,_),_,           noun(A,B,C)).

enof_lemma(hij_zij).

vreemd_lemma(alfa).
vreemd_lemma(bèta).
vreemd_lemma(bölke).
vreemd_lemma(bölkes).
vreemd_lemma(bölkje).
vreemd_lemma(capita).
vreemd_lemma(consolatio).
vreemd_lemma('cross-country').
vreemd_lemma('cross-over').
vreemd_lemma('Development').
vreemd_lemma('English').
vreemd_lemma(entarted).
vreemd_lemma(fancy).
vreemd_lemma(fatwa).
vreemd_lemma('high-performance').
vreemd_lemma(home).
vreemd_lemma('Imbiss').
vreemd_lemma(issue).
vreemd_lemma('know-how').
vreemd_lemma(licensee).
vreemd_lemma(onsite).
vreemd_lemma(passphrase).
vreemd_lemma('pop-up').
vreemd_lemma('Spartakusbund').
vreemd_lemma(vice).
vreemd_lemma(versa).
vreemd_lemma('VOXPOP').
vreemd_lemma(voxpop).
vreemd_lemma(warlord).
vreemd_lemma(warlords).

%% added by hand, e.g. used in collocational prepositions
lassy(behulp,behulp,     'N(soort,ev,basis,onz,stan)').
lassy(belope,beloop,     'N(soort,ev,basis,dat)').
lassy(dienste,dienst,    'N(soort,ev,basis,dat)').
lassy(faveure,faveur,    'N(soort,ev,basis,dat)').
lassy(gevolge,gevolg,    'N(soort,ev,basis,dat)').
lassy(gunste,gunst,      'N(soort,ev,basis,dat)').
lassy(hoofde,hoofd,      'N(soort,ev,basis,dat)').
lassy(huize,huis,        'N(soort,ev,basis,dat)').
lassy(inplaats,'in plaats', 'BW()').
lassy(koste,kost,        'N(soort,ev,basis,dat)').
lassy(nadele,nadeel,     'N(soort,ev,basis,dat)').
lassy(name,naam,         'N(soort,ev,basis,dat)').
lassy(nutte,nut,         'N(soort,ev,basis,dat)').
lassy(omwille,omwile,    'BW()').
lassy(opzichte,opzicht,  'N(soort,ev,basis,dat)').
lassy(straffe,straf,     'N(soort,ev,basis,dat)').
lassy(temidden,temidden, 'BW()').
lassy(tijde,tijd,        'N(soort,ev,basis,dat)').
lassy(voordele,voordeel, 'N(soort,ev,basis,dat)').
lassy(wille,wil,         'N(soort,ev,basis,dat)').

lassy(voorbedachten,voorbedacht,'ADJ(prenom,basis,met-e,bijz)').


% first 1500 in frequency, but SPEC(deeleigen) all removed
lassy('de','de','LID(bep,stan,rest)').
lassy('.','.','LET()').
lassy(',',',','LET()').
lassy('van','van','VZ(init)').
lassy('het','het','LID(bep,stan,evon)').
lassy('een','een','LID(onbep,stan,agr)').
lassy('en','en','VG(neven)').
lassy('in','in','VZ(init)').
lassy('is','zijn','WW(pv,tgw,ev)').
lassy('te','te','VZ(init)').
lassy('op','op','VZ(init)').
lassy('De','de','LID(bep,stan,rest)').
lassy('voor','voor','VZ(init)').
lassy('met','met','VZ(init)').
lassy('dat','dat','VG(onder)').
lassy('die','die','VNW(betr,pron,stan,vol,persoon,getal)').
lassy('niet','niet','BW()').
lassy(')',')','LET()').
lassy('(','(','LET()').
lassy('"','"','LET()').
lassy('?','?','LET()').
lassy('om','om','VZ(init)').
lassy('aan','aan','VZ(init)').
lassy('er','er','VNW(aanw,adv-pron,stan,red,3,getal)').
lassy('als','als','VG(onder)').
lassy('zijn','zijn','WW(pv,tgw,mv)').
lassy('door','door','VZ(init)').
lassy(':',':','LET()').
lassy('was','zijn','WW(pv,verl,ev)').
lassy('ook','ook','BW()').
lassy('\'','\'','LET()').
lassy('zijn','zijn','VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)').
lassy('heeft','hebben','WW(pv,tgw,met-t)').
lassy('hij','hij','VNW(pers,pron,nomin,vol,3,ev,masc)').
lassy('-','-','LET()').
lassy('bij','bij','VZ(init)').
lassy('In','in','VZ(init)').
lassy('werd','worden','WW(pv,verl,ev)').
lassy('naar','naar','VZ(init)').
lassy('wordt','worden','WW(pv,tgw,met-t)').
lassy('Het','het','LID(bep,stan,evon)').
lassy('tot','tot','VZ(init)').
lassy('of','of','VG(neven)').
lassy('nog','nog','BW()').
lassy('over','over','VZ(init)').
lassy('zich','zich','VNW(refl,pron,obl,red,3,getal)').
lassy('worden','worden','WW(inf,vrij,zonder)').
lassy('jaar','jaar','N(soort,ev,basis,onz,stan)').
lassy('uit','uit','VZ(init)').
lassy('maar','maar','VG(neven)').
lassy('deze','deze','VNW(aanw,det,stan,prenom,met-e,rest)').
lassy('hun','hun','VNW(bez,det,stan,vol,3,mv,prenom,zonder,agr)').
lassy('ik','ik','VNW(pers,pron,nomin,vol,1,ev)').
lassy('kan','kunnen','WW(pv,tgw,ev)').
lassy('meer','veel','VNW(onbep,grad,stan,vrij,zonder,comp)').
lassy('geen','geen','VNW(onbep,det,stan,prenom,zonder,agr)').
lassy('Een','een','LID(onbep,stan,agr)').
lassy('ze','ze','VNW(pers,pron,stan,red,3,mv)').
lassy('hebben','hebben','WW(pv,tgw,mv)').
lassy('dan','dan','BW()').
lassy('al','al','BW()').
lassy('Hij','hij','VNW(pers,pron,nomin,vol,3,ev,masc)').
lassy('zal','zullen','WW(pv,tgw,ev)').
lassy('moet','moeten','WW(pv,tgw,ev)').
lassy('dat','dat','VNW(betr,pron,stan,vol,3,ev)').
lassy('op','op','VZ(fin)').
lassy('zou','zullen','WW(pv,verl,ev)').
lassy('die','die','VNW(aanw,det,stan,prenom,zonder,rest)').
lassy('wel','wel','BW()').
lassy('nu','nu','BW()').
lassy('u','u','VNW(pers,pron,nomin,vol,2b,getal)').
lassy('worden','worden','WW(pv,tgw,mv)').
lassy('onder','onder','VZ(init)').
lassy('had','hebben','WW(pv,verl,ev)').
lassy('zijn','zijn','WW(inf,vrij,zonder)').
lassy('Maar','maar','VG(neven)').
lassy('nieuwe','nieuw','ADJ(prenom,basis,met-e,stan)').
lassy('zo','zo','BW()').
lassy('En','en','VG(neven)').
lassy('je','je','VNW(pers,pron,nomin,red,2v,ev)').
lassy('andere','ander','ADJ(prenom,basis,met-e,stan)').
lassy('we','we','VNW(pers,pron,nomin,red,1,mv)').
lassy('tegen','tegen','VZ(init)').
lassy('eerste','één','TW(rang,prenom,stan)').
lassy('Ik','ik','VNW(pers,pron,nomin,vol,1,ev)').
lassy('na','na','VZ(init)').
lassy('aan','aan','VZ(fin)').
lassy('dit','dit','VNW(aanw,det,stan,prenom,zonder,evon)').
lassy(';',';','LET()').
lassy('mensen','mens','N(soort,mv,basis)').
lassy('tussen','tussen','VZ(init)').
lassy('uit','uit','VZ(fin)').
lassy('twee','twee','TW(hoofd,prenom,stan)').
lassy('waren','zijn','WW(pv,verl,mv)').
lassy('werden','worden','WW(pv,verl,mv)').
lassy('kunnen','kunnen','WW(pv,tgw,mv)').
lassy('Op','op','VZ(init)').
lassy('Dat','dat','VNW(aanw,pron,stan,vol,3o,ev)').
lassy('wat','wat','VNW(vb,pron,stan,vol,3o,ev)').
lassy('alle','al','VNW(onbep,det,stan,prenom,met-e,agr)').
lassy('waar','waar','VNW(vb,adv-pron,obl,vol,3o,getal)').
lassy('grote','groot','ADJ(prenom,basis,met-e,stan)').
lassy('Wat','wat','VNW(vb,pron,stan,vol,3o,ev)').
lassy('haar','haar','VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)').
lassy('Nederland','Nederland','N(eigen,ev,basis,onz,stan)').
lassy('Er','er','VNW(aanw,adv-pron,stan,red,3,getal)').
lassy('Ook','ook','BW()').
lassy('hem','hem','VNW(pers,pron,obl,vol,3,ev,masc)').
lassy('wil','willen','WW(pv,tgw,ev)').
lassy('weer','weer','BW()').
lassy('daar','daar','VNW(aanw,adv-pron,obl,vol,3o,getal)').
lassy('in','in','VZ(fin)').
lassy('Als','als','VG(onder)').
lassy('zoals','zoals','VG(onder)').
lassy('Hoe','hoe','BW()').
lassy('gaat','gaan','WW(pv,tgw,met-t)').
lassy('ze','ze','VNW(pers,pron,stan,red,3,ev,fem)').
lassy('/','/','LET()').
lassy('land','land','N(soort,ev,basis,onz,stan)').
lassy('omdat','omdat','VG(onder)').
lassy('alleen','alleen','BW()').
lassy('Nederlandse','Nederlands','ADJ(prenom,basis,met-e,stan)').
lassy('veel','veel','VNW(onbep,grad,stan,prenom,zonder,agr,basis)').
lassy('jaren','jaar','N(soort,mv,basis)').
lassy('aantal','aantal','N(soort,ev,basis,onz,stan)').
lassy('tijd','tijd','N(soort,ev,basis,zijd,stan)').
lassy('Dit','dit','VNW(aanw,pron,stan,vol,3o,ev)').
lassy('dus','dus','BW()').
lassy('Voor','voor','VZ(init)').
lassy('Wie','wie','VNW(vb,pron,stan,vol,3p,getal)').
lassy('moeten','moeten','WW(pv,tgw,mv)').
lassy('af','af','VZ(fin)').
lassy('men','men','VNW(pers,pron,nomin,red,3p,ev,masc)').
lassy('hebben','hebben','WW(inf,vrij,zonder)').
lassy('echter','echter','BW()').
lassy('minister','minister','N(soort,ev,basis,zijd,stan)').
lassy('uw','u','VNW(bez,det,stan,vol,2,getal,prenom,zonder,agr)').
lassy('drie','drie','TW(hoofd,prenom,stan)').
lassy('steeds','steeds','BW()').
lassy('Deze','deze','VNW(aanw,det,stan,prenom,met-e,rest)').
lassy('kunnen','kunnen','WW(inf,vrij,zonder)').
lassy('maar','maar','BW()').
lassy('zelf','zelf','BW()').
lassy('veel','veel','VNW(onbep,grad,stan,vrij,zonder,basis)').
lassy('eigen','eigen','ADJ(prenom,basis,zonder)').
lassy('plaats','plaats','N(soort,ev,basis,zijd,stan)').
lassy('Bij','bij','VZ(init)').
lassy('komt','komen','WW(pv,tgw,met-t)').
lassy('goed','goed','ADJ(vrij,basis,zonder)').
lassy('!','!','LET()').
lassy('maken','maken','WW(inf,vrij,zonder)').
lassy('dit','dit','VNW(aanw,pron,stan,vol,3o,ev)').
lassy('«','«','LET()').
lassy('»','»','LET()').
lassy('tijdens','tijdens','VZ(init)').
lassy('toch','toch','BW()').
lassy('meer','veel','VNW(onbep,grad,stan,prenom,zonder,agr,comp)').
lassy('te','te','BW()').
lassy('per','per','VZ(init)').
lassy('deel','deel','N(soort,ev,basis,onz,stan)').
lassy('toe','toe','VZ(fin)').
lassy('regering','regering','N(soort,ev,basis,zijd,stan)').
lassy('die','die','VNW(aanw,pron,stan,vol,3,getal)').
lassy('landen','land','N(soort,mv,basis)').
lassy('België','België','N(eigen,ev,basis,onz,stan)').
lassy('vooral','vooral','BW()').
lassy('Amerikaanse','Amerikaans','ADJ(prenom,basis,met-e,stan)').
lassy('Vlaamse','Vlaams','ADJ(prenom,basis,met-e,stan)').
lassy('zullen','zullen','WW(pv,tgw,mv)').
lassy('stad','stad','N(soort,ev,basis,zijd,stan)').
lassy('Met','met','VZ(init)').
lassy('hoe','hoe','BW()').
lassy('heb','hebben','WW(pv,tgw,ev)').
lassy('uur','uur','N(soort,ev,basis,onz,stan)').
lassy('dat','dat','VNW(aanw,det,stan,prenom,zonder,evon)').
lassy('hier','hier','VNW(aanw,adv-pron,obl,vol,3o,getal)').
lassy('dag','dag','N(soort,ev,basis,zijd,stan)').
lassy('onderzoek','onderzoek','N(soort,ev,basis,onz,stan)').
lassy('altijd','altijd','BW()').
lassy('of','of','VG(onder)').
lassy('kwam','komen','WW(pv,verl,ev)').
lassy('Ze','ze','VNW(pers,pron,stan,red,3,mv)').
lassy('...','...','LET()').
lassy('ten','te','VZ(versm)').
lassy('via','via','VZ(init)').
lassy('enkele','enkel','VNW(onbep,det,stan,prenom,met-e,rest)').
lassy('kinderen','kind','N(soort,mv,basis)').
lassy('1','1','TW(hoofd,vrij)').
lassy('voor','voor','VZ(fin)').
lassy('mogelijk','mogelijk','ADJ(vrij,basis,zonder)').
lassy('hadden','hebben','WW(pv,verl,mv)').
lassy('heel','heel','ADJ(vrij,basis,zonder)').
lassy('eens','eens','BW()').
lassy('zonder','zonder','VZ(init)').
lassy('verschillende','verschillend','ADJ(prenom,basis,met-e,stan)').
lassy('laatste','laat','ADJ(prenom,sup,met-e,stan)').
lassy('euro','euro','N(soort,ev,basis,zijd,stan)').
lassy('onze','ons','VNW(bez,det,stan,vol,1,mv,prenom,met-e,rest)').
lassy('kon','kunnen','WW(pv,verl,ev)').
lassy('vandaag','vandaag','BW()').
lassy('welke','welk','VNW(vb,det,stan,prenom,met-e,rest)').
lassy('gaan','gaan','WW(inf,vrij,zonder)').
lassy('miljoen','miljoen','N(soort,ev,basis,onz,stan)').
lassy('Brussel','Brussel','N(eigen,ev,basis,onz,stan)').
lassy('wereld','wereld','N(soort,ev,basis,zijd,stan)').
lassy('Na','na','VZ(init)').
lassy('elkaar','elkaar','VNW(recip,pron,obl,vol,persoon,mv)').
lassy('binnen','binnen','VZ(init)').
lassy('één','één','TW(hoofd,prenom,stan)').
lassy('terug','terug','BW()').
lassy('staat','staan','WW(pv,tgw,met-t)').
lassy('zouden','zullen','WW(pv,verl,mv)').
lassy('We','we','VNW(pers,pron,nomin,red,1,mv)').
lassy('waarin','waarin','BW()').
lassy('later','laat','ADJ(vrij,comp,zonder)').
lassy('wat','wat','VNW(onbep,pron,stan,vol,3o,ev)').
lassy('komen','komen','WW(inf,vrij,zonder)').
lassy('bijvoorbeeld','bijvoorbeeld','BW()').
lassy('over','over','VZ(fin)').
lassy('volgens','volgens','VZ(init)').
lassy('zelfs','zelfs','BW()').
lassy('zij','zij','VNW(pers,pron,nomin,vol,3p,mv)').
lassy('wie','wie','VNW(vb,pron,stan,vol,3p,getal)').
lassy('procent','procent','N(soort,ev,basis,onz,stan)').
lassy('een','één','TW(hoofd,nom,zonder-n,basis)').
lassy('leven','leven','N(soort,ev,basis,onz,stan)').
lassy('Europese','Europees','ADJ(prenom,basis,met-e,stan)').
lassy('doen','doen','WW(inf,vrij,zonder)').
lassy('Zo','zo','BW()').
lassy('ontwikkeling','ontwikkeling','N(soort,ev,basis,zijd,stan)').
lassy('sinds','sinds','VZ(init)').
lassy('toen','toen','VG(onder)').
lassy('werk','werk','N(soort,ev,basis,onz,stan)').
lassy('zegt','zeggen','WW(pv,tgw,met-t)').
lassy('man','man','N(soort,ev,basis,zijd,stan)').
lassy('week','week','N(soort,ev,basis,zijd,stan)').
lassy('samen','samen','BW()').
lassy('politie','politie','N(soort,ev,basis,zijd,stan)').
lassy('vaak','vaak','ADJ(vrij,basis,zonder)').
lassy('mijn','mijn','VNW(bez,det,stan,vol,1,ev,prenom,zonder,agr)').
lassy('Belgische','Belgisch','ADJ(prenom,basis,met-e,stan)').
lassy('zeer','zeer','BW()').
lassy('naam','naam','N(soort,ev,basis,zijd,stan)').
lassy('politieke','politiek','ADJ(prenom,basis,met-e,stan)').
lassy('mag','mogen','WW(pv,tgw,ev)').
lassy('Volgens','volgens','VZ(init)').
lassy('Welke','welk','VNW(vb,det,stan,prenom,met-e,rest)').
lassy('geld','geld','N(soort,ev,basis,onz,stan)').
lassy('iets','iets','VNW(onbep,pron,stan,vol,3o,ev)').
lassy('Duitse','Duits','ADJ(prenom,basis,met-e,stan)').
lassy('laten','laten','WW(inf,vrij,zonder)').
lassy('%','%','SPEC(symb)').
lassy('ons','ons','VNW(pr,pron,obl,vol,1,mv)').
lassy('lang','lang','ADJ(vrij,basis,zonder)').
lassy('overheid','overheid','N(soort,ev,basis,zijd,stan)').
lassy('verder','ver','ADJ(vrij,comp,zonder)').
lassy('gemaakt','maken','WW(vd,vrij,zonder)').
lassy('tweede','twee','TW(rang,prenom,stan)').
lassy('nodig','nodig','ADJ(vrij,basis,zonder)').
lassy('ligt','liggen','WW(pv,tgw,met-t)').
lassy('partij','partij','N(soort,ev,basis,zijd,stan)').
lassy('mee','mee','VZ(fin)').
lassy('zien','zien','WW(inf,vrij,zonder)').
lassy('Vlaanderen','Vlaanderen','N(eigen,ev,basis,onz,stan)').
lassy('keer','keer','N(soort,ev,basis,zijd,stan)').
lassy('volgende','volgen','WW(od,prenom,met-e)').
lassy('informatie','informatie','N(soort,ev,basis,zijd,stan)').
lassy('Duitsland','Duitsland','N(eigen,ev,basis,onz,stan)').
lassy('gebied','gebied','N(soort,ev,basis,onz,stan)').
lassy('moeten','moeten','WW(inf,vrij,zonder)').
lassy('kunt','kunnen','WW(pv,tgw,met-t)').
lassy('hele','heel','ADJ(prenom,basis,met-e,stan)').
lassy('ter','te','VZ(versm)').
lassy('snel','snel','ADJ(vrij,basis,zonder)').
lassy('nooit','nooit','BW()').
lassy('u','u','VNW(pr,pron,obl,vol,2,getal)').
lassy('vier','vier','TW(hoofd,prenom,stan)').
lassy('Om','om','VZ(init)').
lassy('niets','niets','VNW(onbep,pron,stan,vol,3o,ev)').
lassy('moest','moeten','WW(pv,verl,ev)').
lassy('president','president','N(soort,ev,basis,zijd,stan)').
lassy('grootste','groot','ADJ(prenom,sup,met-e,stan)').
lassy('Europa','Europa','N(eigen,ev,basis,onz,stan)').
lassy('duidelijk','duidelijk','ADJ(vrij,basis,zonder)').
lassy('onderwijs','onderwijs','N(soort,ev,basis,onz,stan)').
lassy('me','me','VNW(pr,pron,obl,red,1,ev)').
lassy('Van','van','VZ(init)').
lassy('bekend','bekend','ADJ(vrij,basis,zonder)').
lassy('oorlog','oorlog','N(soort,ev,basis,zijd,stan)').
lassy('·','·','LET()').
lassy('rond','rond','VZ(init)').
lassy('zei','zeggen','WW(pv,verl,ev)').
lassy('internationale','internationaal','ADJ(prenom,basis,met-e,stan)').
lassy('geleden','geleden','BW()').
lassy('krijgt','krijgen','WW(pv,tgw,met-t)').
lassy('hen','hen','VNW(pers,pron,obl,vol,3p,mv)').
lassy('eeuw','eeuw','N(soort,ev,basis,zijd,stan)').
lassy('dagen','dag','N(soort,mv,basis)').
lassy('geweest','zijn','WW(vd,vrij,zonder)').
lassy('ben','zijn','WW(pv,tgw,ev)').
lassy('wij','wij','VNW(pers,pron,nomin,vol,1,mv)').
lassy('september','september','N(eigen,ev,basis,zijd,stan)').
lassy('want','want','VG(neven)').
lassy('Door','door','VZ(init)').
lassy('even','even','BW()').
lassy('van','van','VZ(fin)').
lassy('blijft','blijven','WW(pv,tgw,met-t)').
lassy('minder','weinig','VNW(onbep,grad,stan,vrij,zonder,comp)').
lassy('sociale','sociaal','ADJ(prenom,basis,met-e,stan)').
lassy('belangrijke','belangrijk','ADJ(prenom,basis,met-e,stan)').
lassy('waarbij','waarbij','BW()').
lassy('gebruik','gebruik','N(soort,ev,basis,onz,stan)').
lassy('alles','alles','VNW(onbep,pron,stan,vol,3o,ev)').
lassy('heet','heten','WW(pv,tgw,ev)').
lassy('opnieuw','opnieuw','BW()').
lassy('film','film','N(soort,ev,basis,zijd,stan)').
lassy('Franse','Frans','ADJ(prenom,basis,met-e,stan)').
lassy('blijkt','blijken','WW(pv,tgw,met-t)').
lassy('huis','huis','N(soort,ev,basis,onz,stan)').
lassy('bijna','bijna','BW()').
lassy('groot','groot','ADJ(prenom,basis,zonder)').
lassy('gebruikt','gebruiken','WW(vd,vrij,zonder)').
lassy('toen','toen','BW()').
lassy('beter','goed','ADJ(vrij,comp,zonder)').
lassy('bedrijf','bedrijf','N(soort,ev,basis,onz,stan)').
lassy('zo\'n','zo\'n','VNW(aanw,det,stan,prenom,zonder,agr)').
lassy('manier','manier','N(soort,ev,basis,zijd,stan)').
lassy('kreeg','krijgen','WW(pv,verl,ev)').
lassy('goede','goed','ADJ(prenom,basis,met-e,stan)').
lassy('2003','2003','TW(hoofd,vrij)').
lassy('vanaf','vanaf','VZ(init)').
lassy('ongeveer','ongeveer','BW()').
lassy('Waar','waar','VNW(vb,adv-pron,obl,vol,3o,getal)').
lassy('staat','staat','N(soort,ev,basis,zijd,stan)').
lassy('kind','kind','N(soort,ev,basis,onz,stan)').
lassy('zowel','zowel','BW()').
lassy('Dit','dit','VNW(aanw,det,stan,prenom,zonder,evon)').
lassy('school','school','N(soort,ev,basis,zijd,stan)').
lassy('groep','groep','N(soort,ev,basis,zijd,stan)').
lassy('willen','willen','WW(pv,tgw,mv)').
lassy('vele','veel','VNW(onbep,grad,stan,prenom,met-e,agr,basis)').
lassy('periode','periode','N(soort,ev,basis,zijd,stan)').
lassy('begin','begin','N(soort,ev,basis,onz,stan)').
lassy('mij','mij','VNW(pr,pron,obl,vol,1,ev)').
lassy('geval','geval','N(soort,ev,basis,onz,stan)').
lassy('2004','2004','TW(hoofd,vrij)').
lassy('zeker','zeker','ADJ(vrij,basis,zonder)').
lassy('Die','die','VNW(aanw,det,stan,prenom,zonder,rest)').
lassy('aanval','aanval','N(soort,ev,basis,zijd,stan)').
lassy('ging','gaan','WW(pv,verl,ev)').
lassy('krijgen','krijgen','WW(inf,vrij,zonder)').
lassy('Britse','Brits','ADJ(prenom,basis,met-e,stan)').
lassy('bestaat','bestaan','WW(pv,tgw,met-t)').
lassy('januari','januari','N(eigen,ev,basis,zijd,stan)').
lassy('boek','boek','N(soort,ev,basis,onz,stan)').
lassy('basis','basis','N(soort,ev,basis,zijd,stan)').
lassy('vindt','vinden','WW(pv,tgw,met-t)').
lassy('2000','2000','TW(hoofd,vrij)').
lassy('eerder','eerder','ADJ(vrij,comp,zonder)').
lassy('afgelopen','af_lopen','WW(vd,prenom,zonder)').
lassy('economische','economisch','ADJ(prenom,basis,met-e,stan)').
lassy('vrouwen','vrouw','N(soort,mv,basis)').
lassy('samenwerking','samenwerking','N(soort,ev,basis,zijd,stan)').
lassy('Frankrijk','Frankrijk','N(eigen,ev,basis,onz,stan)').
lassy('meest','veel','VNW(onbep,grad,stan,vrij,zonder,sup)').
lassy('bij','bij','VZ(fin)').
lassy('water','water','N(soort,ev,basis,onz,stan)').
lassy('rol','rol','N(soort,ev,basis,zijd,stan)').
lassy('z\'n','zijn','VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)').
lassy('Ze','ze','VNW(pers,pron,stan,red,3,ev,fem)').
lassy('maanden','maand','N(soort,mv,basis)').
lassy('Hoeveel','hoeveel','TW(hoofd,prenom,stan)').
lassy('juni','juni','N(eigen,ev,basis,zijd,stan)').
lassy('Zijn','zijn','VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)').
lassy('welk','welk','VNW(vb,det,stan,prenom,zonder,evon)').
lassy('problemen','probleem','N(soort,mv,basis)').
lassy('kleine','klein','ADJ(prenom,basis,met-e,stan)').
lassy('hand','hand','N(soort,ev,basis,zijd,stan)').
lassy('2005','2005','TW(hoofd,vrij)').
lassy('zij','zij','VNW(pers,pron,nomin,vol,3v,ev,fem)').
lassy('Irak','Irak','N(eigen,ev,basis,onz,stan)').
lassy('slechts','slechts','BW()').
lassy('Amsterdam','Amsterdam','N(eigen,ev,basis,onz,stan)').
lassy('terwijl','terwijl','VG(onder)').
lassy('mei','mei','N(eigen,ev,basis,zijd,stan)').
lassy('je','je','VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)').
lassy('erg','erg','ADJ(vrij,basis,zonder)').
lassy('vraag','vraag','N(soort,ev,basis,zijd,stan)').
lassy('maakt','maken','WW(pv,tgw,met-t)').
lassy('weken','week','N(soort,mv,basis)').
lassy('net','net','BW()').
lassy('leger','leger','N(soort,ev,basis,onz,stan)').
lassy('epilepsie','epilepsie','N(soort,ev,basis,zijd,stan)').
lassy('beleid','beleid','N(soort,ev,basis,onz,stan)').
lassy('vrouw','vrouw','N(soort,ev,basis,zijd,stan)').
lassy('ouders','ouder','N(soort,mv,basis)').
lassy('door','door','VZ(fin)').
lassy('weg','weg','N(soort,ev,basis,zijd,stan)').
lassy('U','u','VNW(pers,pron,nomin,vol,2b,getal)').
lassy('dezelfde','dezelfde','ADJ(prenom,basis,zonder)').
lassy('weet','weten','WW(pv,tgw,ev)').
lassy('geven','geven','WW(inf,vrij,zonder)').
lassy('pas','pas','BW()').
lassy('oude','oud','ADJ(prenom,basis,met-e,stan)').
lassy('gisteren','gisteren','BW()').
lassy('belang','belang','N(soort,ev,basis,onz,stan)').
lassy('komen','komen','WW(pv,tgw,mv)').
lassy('houden','houden','WW(inf,vrij,zonder)').
lassy('enige','enig','VNW(onbep,det,stan,prenom,met-e,rest)').
lassy('moment','moment','N(soort,ev,basis,onz,stan)').
lassy('zit','zitten','WW(pv,tgw,ev)').
lassy('iemand','iemand','VNW(onbep,pron,stan,vol,3p,ev)').
lassy('lijkt','lijken','WW(pv,tgw,met-t)').
lassy('gehouden','houden','WW(vd,vrij,zonder)').
lassy('situatie','situatie','N(soort,ev,basis,zijd,stan)').
lassy('blijven','blijven','WW(inf,vrij,zonder)').
lassy('beide','beide','VNW(onbep,grad,stan,prenom,met-e,mv,basis)').
lassy('gezien','zien','WW(vd,vrij,zonder)').
lassy('Aan','aan','VZ(init)').
lassy('Zij','zij','VNW(pers,pron,nomin,vol,3p,mv)').
lassy('gedaan','doen','WW(vd,vrij,zonder)').
lassy('einde','einde','N(soort,ev,basis,onz,stan)').
lassy('juli','juli','N(eigen,ev,basis,zijd,stan)').
lassy('vorm','vorm','N(soort,ev,basis,zijd,stan)').
lassy('kans','kans','N(soort,ev,basis,zijd,stan)').
lassy('zes','zes','TW(hoofd,prenom,stan)').
lassy('vijf','vijf','TW(hoofd,prenom,stan)').
lassy('huidige','huidig','ADJ(prenom,basis,met-e,stan)').
lassy('ontwikkelingslanden','ontwikkeling_land','N(soort,mv,basis)').
lassy('genoemd','noemen','WW(vd,vrij,zonder)').
lassy('gaan','gaan','WW(pv,tgw,mv)').
lassy('eind','eind','N(soort,ev,basis,onz,stan)').
lassy('kader','kader','N(soort,ev,basis,onz,stan)').
lassy('2002','2002','TW(hoofd,vrij)').
lassy('daarom','daarom','BW()').
lassy('meeste','veel','VNW(onbep,grad,stan,prenom,met-e,agr,sup)').
lassy('iedereen','iedereen','VNW(onbep,pron,stan,vol,3p,ev)').
lassy('elke','elk','VNW(onbep,det,stan,prenom,met-e,evz)').
lassy('maart','maart','N(eigen,ev,basis,zijd,stan)').
lassy('Die','die','VNW(aanw,pron,stan,vol,3,getal)').
lassy('doet','doen','WW(pv,tgw,met-t)').
lassy('wet','wet','N(soort,ev,basis,zijd,stan)').
lassy('waarop','waarop','BW()').
lassy('oktober','oktober','N(eigen,ev,basis,zijd,stan)').
lassy('bepaalde','bepalen','WW(vd,prenom,met-e)').
lassy('gevolg','gevolg','N(soort,ev,basis,onz,stan)').
lassy('begon','beginnen','WW(pv,verl,ev)').
lassy('deze','deze','VNW(aanw,det,stan,nom,met-e,zonder-n)').
lassy('belangrijkste','belangrijk','ADJ(prenom,sup,met-e,stan)').
lassy('allemaal','allemaal','BW()').
lassy('Daar','daar','VNW(aanw,adv-pron,obl,vol,3o,getal)').
lassy('zeggen','zeggen','WW(inf,vrij,zonder)').
lassy('zaken','zaak','N(soort,mv,basis)').
lassy('krijgen','krijgen','WW(pv,tgw,mv)').
lassy('bleek','blijken','WW(pv,verl,ev)').
lassy('waarvan','waarvan','BW()').
lassy('hoofdstad','hoofdstad','N(soort,ev,basis,zijd,stan)').
lassy('2001','2001','TW(hoofd,vrij)').
lassy('vorige','vorig','ADJ(prenom,basis,met-e,stan)').
lassy('leden','lid','N(soort,mv,basis)').
lassy('december','december','N(eigen,ev,basis,zijd,stan)').
lassy('andere','ander','ADJ(nom,basis,met-e,zonder-n,stan)').
lassy('sterk','sterk','ADJ(vrij,basis,zonder)').
lassy('soort','soort','N(soort,ev,basis,genus,stan)').
lassy('wilde','willen','WW(pv,verl,ev)').
lassy('premier','premier','N(soort,ev,basis,zijd,stan)').
lassy('hoge','hoog','ADJ(prenom,basis,met-e,stan)').
lassy('bedrijven','bedrijf','N(soort,mv,basis)').
lassy('zaak','zaak','N(soort,ev,basis,zijd,stan)').
lassy('ons','ons','VNW(bez,det,stan,vol,1,mv,prenom,zonder,evon)').
lassy('nemen','nemen','WW(inf,vrij,zonder)').
lassy('achter','achter','VZ(init)').
lassy('2','2','TW(hoofd,vrij)').
lassy('vanuit','vanuit','VZ(init)').
lassy('invloed','invloed','N(soort,ev,basis,zijd,stan)').
lassy('toekomst','toekomst','N(soort,ev,basis,zijd,stan)').
lassy('ervan','ervan','BW()').
lassy('alcohol','alcohol','N(soort,ev,basis,zijd,stan)').
lassy('waardoor','waardoor','BW()').
lassy('strijd','strijd','N(soort,ev,basis,zijd,stan)').
lassy('wanneer','wanneer','VG(onder)').
lassy('je','je','VNW(pr,pron,obl,red,2v,getal)').
lassy('2006','2006','TW(hoofd,vrij)').
lassy('verkiezingen','verkiezing','N(soort,mv,basis)').
lassy('Minister','minister','N(soort,ev,basis,zijd,stan)').
lassy('haar','haar','VNW(pers,pron,obl,vol,3,getal,fem)').
lassy('gemeente','gemeente','N(soort,ev,basis,zijd,stan)').
lassy('augustus','augustus','N(eigen,ev,basis,zijd,stan)').
lassy('weten','weten','WW(inf,vrij,zonder)').
lassy('geworden','worden','WW(vd,vrij,zonder)').
lassy('tien','tien','TW(hoofd,prenom,stan)').
lassy('soms','soms','BW()').
lassy('opdracht','opdracht','N(soort,ev,basis,zijd,stan)').
lassy('mee','met','VZ(fin)').
lassy('heer','heer','N(soort,ev,basis,zijd,stan)').
lassy('ruim','ruim','ADJ(vrij,basis,zonder)').
lassy('kabinet','kabinet','N(soort,ev,basis,onz,stan)').
lassy('geweld','geweld','N(soort,ev,basis,onz,stan)').
lassy('genomen','nemen','WW(vd,vrij,zonder)').
lassy('staan','staan','WW(pv,tgw,mv)').
lassy('paar','paar','N(soort,ev,basis,onz,stan)').
lassy('genoeg','genoeg','BW()').
lassy('buiten','buiten','VZ(init)').
lassy('stond','staan','WW(pv,verl,ev)').
lassy('weinig','weinig','VNW(onbep,grad,stan,vrij,zonder,basis)').
lassy('Tot','tot','VZ(init)').
lassy('geeft','geven','WW(pv,tgw,met-t)').
lassy('duurzame','duurzaam','ADJ(prenom,basis,met-e,stan)').
lassy('nieuw','nieuw','ADJ(prenom,basis,zonder)').
lassy('maakte','maken','WW(pv,verl,ev)').
lassy('voorzitter','voorzitter','N(soort,ev,basis,zijd,stan)').
lassy('meter','meter','N(soort,ev,basis,zijd,stan)').
lassy('bevolking','bevolking','N(soort,ev,basis,zijd,stan)').
lassy('3','3','TW(hoofd,vrij)').
lassy('partijen','partij','N(soort,mv,basis)').
lassy('meteen','meteen','BW()').
lassy('Want','want','VG(neven)').
lassy('steun','steun','N(soort,ev,basis,zijd,stan)').
lassy('eerst','één','TW(rang,nom,zonder-n)').
lassy('zodat','zodat','VG(onder)').
lassy('vrij','vrij','ADJ(vrij,basis,zonder)').
lassy('maatregelen','maatregel','N(soort,mv,basis)').
lassy('woningen','woning','N(soort,mv,basis)').
lassy('helemaal','helemaal','BW()').
lassy('macht','macht','N(soort,ev,basis,zijd,stan)').
lassy('bent','zijn','WW(pv,tgw,met-t)').
lassy('\'t','het','LID(bep,stan,evon)').
lassy('jongeren','jong','ADJ(nom,comp,met-e,mv-n)').
lassy('één','één','TW(hoofd,nom,zonder-n,basis)').
lassy('plan','plan','N(soort,ev,basis,onz,stan)').
lassy('aandacht','aandacht','N(soort,ev,basis,zijd,stan)').
lassy('Wanneer','wanneer','BW()').
lassy('langer','lang','ADJ(vrij,comp,zonder)').
lassy('lange','lang','ADJ(prenom,basis,met-e,stan)').
lassy('gegevens','gegeven','N(soort,mv,basis)').
lassy('Zaken','zaak','N(soort,mv,basis)').
lassy('Toch','toch','BW()').
lassy('gemeenten','gemeente','N(soort,mv,basis)').
lassy('markt','markt','N(soort,ev,basis,zijd,stan)').
lassy('*','*','LET()').
lassy('boven','boven','VZ(init)').
lassy('nationale','nationaal','ADJ(prenom,basis,met-e,stan)').
lassy('laat','laten','WW(pv,tgw,ev)').
lassy('kwamen','komen','WW(pv,verl,mv)').
lassy('inwoners','inwoner','N(soort,mv,basis)').
lassy('Wij','wij','VNW(pers,pron,nomin,vol,1,mv)').
lassy('volledig','volledig','ADJ(vrij,basis,zonder)').
lassy('behandeling','behandeling','N(soort,ev,basis,zijd,stan)').
lassy('al','al','VNW(onbep,det,stan,vrij,zonder)').
lassy('zichzelf','zichzelf','VNW(refl,pron,obl,nadr,3,getal)').
lassy('Tijdens','tijdens','VZ(init)').
lassy('leerlingen','leerling','N(soort,mv,basis)').
lassy('familie','familie','N(soort,ev,basis,zijd,stan)').
lassy('betekent','betekenen','WW(pv,tgw,met-t)').
lassy('recht','recht','N(soort,ev,basis,onz,stan)').
lassy('Je','je','VNW(pers,pron,nomin,red,2v,ev)').
lassy('gegeven','geven','WW(vd,vrij,zonder)').
lassy('april','april','N(eigen,ev,basis,zijd,stan)').
lassy('rekening','rekening','N(soort,ev,basis,zijd,stan)').
lassy('anders','anders','BW()').
lassy('succes','succes','N(soort,ev,basis,onz,stan)').
lassy('of','of','SPEC(vreemd)').
lassy('geldt','gelden','WW(pv,tgw,met-t)').
lassy('contact','contact','N(soort,ev,basis,onz,stan)').
lassy('zie','zien','WW(pv,tgw,ev)').
lassy('reeds','reeds','BW()').
lassy('patiënten','patiënt','N(soort,mv,basis)').
lassy('ooit','ooit','BW()').
lassy('`','`','LET()').
lassy('maand','maand','N(soort,ev,basis,zijd,stan)').
lassy('konden','kunnen','WW(pv,verl,mv)').
lassy('kerk','kerk','N(soort,ev,basis,zijd,stan)').
lassy('elk','elk','VNW(onbep,det,stan,prenom,zonder,evon)').
lassy('1995','1995','TW(hoofd,vrij)').
lassy('zware','zwaar','ADJ(prenom,basis,met-e,stan)').
lassy('november','november','N(eigen,ev,basis,zijd,stan)').
lassy('Italië','Italië','N(eigen,ev,basis,onz,stan)').
lassy('extra','extra','ADJ(prenom,basis,zonder)').
lassy('aanvallen','aanval','N(soort,mv,basis)').
lassy('dood','dood','N(soort,ev,basis,zijd,stan)').
lassy('4','4','TW(hoofd,vrij)').
lassy('Toen','toen','VG(onder)').
lassy('organisatie','organisatie','N(soort,ev,basis,zijd,stan)').
lassy('economie','economie','N(soort,ev,basis,zijd,stan)').
lassy('der','de','LID(bep,gen,rest3)').
lassy('Antwerpen','Antwerpen','N(eigen,ev,basis,onz,stan)').
lassy('tot','tot','VG(neven)').
lassy('mannen','man','N(soort,mv,basis)').
lassy('gevolgen','gevolg','N(soort,mv,basis)').
lassy('gebracht','brengen','WW(vd,vrij,zonder)').
lassy('februari','februari','N(eigen,ev,basis,zijd,stan)').
lassy('sommige','sommig','VNW(onbep,det,stan,prenom,met-e,rest)').
lassy('inzake','inzake','VZ(init)').
lassy('federale','federaal','ADJ(prenom,basis,met-e,stan)').
lassy('scholen','school','N(soort,mv,basis)').
lassy('probleem','probleem','N(soort,ev,basis,onz,stan)').
lassy('Omdat','omdat','VG(onder)').
lassy('grond','grond','N(soort,ev,basis,zijd,stan)').
lassy('name','naam','N(soort,ev,basis,dat)').
lassy('kwaliteit','kwaliteit','N(soort,ev,basis,zijd,stan)').
lassy('hulp','hulp','N(soort,ev,basis,zijd,stan)').
lassy('derde','drie','TW(rang,prenom,stan)').
lassy('eigenlijk','eigenlijk','ADJ(vrij,basis,zonder)').
lassy('buitenlandse','buitenlands','ADJ(prenom,basis,met-e,stan)').
lassy('noemt','noemen','WW(pv,tgw,met-t)').
lassy('lid','lid','N(soort,ev,basis,onz,stan)').
lassy('hoofd','hoofd','N(soort,ev,basis,onz,stan)').
lassy('doel','doel','N(soort,ev,basis,onz,stan)').
lassy('10','10','TW(hoofd,vrij)').
lassy(',,',',,','LET()').
lassy('gesteld','stellen','WW(vd,vrij,zonder)').
lassy('diensten','dienst','N(soort,mv,basis)').
lassy('commissie','commissie','N(soort,ev,basis,zijd,stan)').
lassy('vinden','vinden','WW(inf,vrij,zonder)').
lassy('mogen','mogen','WW(pv,tgw,mv)').
lassy('miljard','miljard','N(soort,ev,basis,onz,stan)').
lassy('EU','EU','N(eigen,ev,basis,zijd,stan)').
lassy('Zie','zien','WW(pv,tgw,ev)').
lassy('weinig','weinig','VNW(onbep,grad,stan,prenom,zonder,agr,basis)').
lassy('Onder','onder','VZ(init)').
lassy('immers','immers','BW()').
lassy('wijze','wijze','N(soort,ev,basis,zijd,stan)').
lassy('vorig','vorig','ADJ(prenom,basis,zonder)').
lassy('uiteindelijk','uiteindelijk','ADJ(vrij,basis,zonder)').
lassy('natuurlijk','natuurlijk','ADJ(vrij,basis,zonder)').
lassy('handen','hand','N(soort,mv,basis)').
lassy('echt','echt','ADJ(vrij,basis,zonder)').
lassy('dienst','dienst','N(soort,ev,basis,zijd,stan)').
lassy('daarvan','daarvan','BW()').
lassy('Niet','niet','BW()').
lassy('groot','groot','ADJ(vrij,basis,zonder)').
lassy('gekomen','komen','WW(vd,vrij,zonder)').
lassy('brengen','brengen','WW(inf,vrij,zonder)').
lassy('Amerikanen','Amerikaan','N(eigen,mv,basis)').
lassy('waarmee','waarmee','BW()').
lassy('troepen','troep','N(soort,mv,basis)').
lassy('producten','product','N(soort,mv,basis)').
lassy('handel','handel','N(soort,ev,basis,zijd,stan)').
lassy('koning','koning','N(soort,ev,basis,zijd,stan)').
lassy('boeken','boek','N(soort,mv,basis)').
lassy('activiteiten','activiteit','N(soort,mv,basis)').
lassy('weg','weg','BW()').
lassy('publiek','publiek','N(soort,ev,basis,onz,stan)').
lassy('moesten','moeten','WW(pv,verl,mv)').
lassy('meestal','meestal','BW()').
lassy('deed','doen','WW(pv,verl,ev)').
lassy('leiding','leiding','N(soort,ev,basis,zijd,stan)').
lassy('The','The','SPEC(vreemd)').
lassy('juist','juist','ADJ(vrij,basis,zonder)').
lassy('daarbij','daarbij','BW()').
lassy('vragen','vraag','N(soort,mv,basis)').
lassy('\'\'','\'\'','LET()').
lassy('korte','kort','ADJ(prenom,basis,met-e,stan)').
lassy('financiële','financieel','ADJ(prenom,basis,met-e,stan)').
lassy('militaire','militair','ADJ(prenom,basis,met-e,stan)').
lassy('helft','helft','N(soort,ev,basis,zijd,stan)').
lassy('binnen','binnen','VZ(fin)').
lassy('ander','ander','ADJ(prenom,basis,zonder)').
lassy('gezegd','zeggen','WW(vd,vrij,zonder)').
lassy('Sinds','sinds','VZ(init)').
lassy('politiek','politiek','N(soort,ev,basis,zijd,stan)').
lassy('gehad','hebben','WW(vd,vrij,zonder)').
lassy('stuk','stuk','N(soort,ev,basis,onz,stan)').
lassy('Rotterdam','Rotterdam','N(eigen,ev,basis,onz,stan)').
lassy('middelen','middel','N(soort,mv,basis)').
lassy('liet','laten','WW(pv,verl,ev)').
lassy('komende','komen','WW(od,prenom,met-e)').
lassy('daarmee','daarmee','BW()').
lassy('beste','goed','ADJ(prenom,sup,met-e,stan)').
lassy('precies','precies','ADJ(vrij,basis,zonder)').
lassy('namelijk','namelijk','BW()').
lassy('Uit','uit','VZ(init)').
lassy('the','the','SPEC(vreemd)').
lassy('staan','staan','WW(inf,vrij,zonder)').
lassy('opgenomen','op_nemen','WW(vd,vrij,zonder)').
lassy('geschiedenis','geschiedenis','N(soort,ev,basis,zijd,stan)').
lassy('Daarom','daarom','BW()').
lassy('werkt','werken','WW(pv,tgw,met-t)').
lassy('ministerie','ministerie','N(soort,ev,basis,onz,stan)').
lassy('China','China','N(eigen,ev,basis,onz,stan)').
lassy('avond','avond','N(soort,ev,basis,zijd,stan)').
lassy('valt','vallen','WW(pv,tgw,met-t)').
lassy('vader','vader','N(soort,ev,basis,zijd,stan)').
lassy('naast','naast','VZ(init)').
lassy('Welk','welk','VNW(vb,det,stan,prenom,zonder,evon)').
lassy('persoon','persoon','N(soort,ev,basis,zijd,stan)').
lassy('nieuws','nieuws','N(soort,ev,basis,onz,stan)').
lassy('Nederlands','Nederlands','N(eigen,ev,basis,onz,stan)').
lassy('militairen','militair','N(soort,mv,basis)').
lassy('lidstaten','lidstaat','N(soort,mv,basis)').
lassy('eerst','eerst','BW()').
lassy('anderen','ander','ADJ(nom,basis,met-e,mv-n)').
lassy('VS','VS','N(eigen,mv,basis)').
lassy('prijs','prijs','N(soort,ev,basis,zijd,stan)').
lassy('nadat','nadat','VG(onder)').
lassy('jonge','jong','ADJ(prenom,basis,met-e,stan)').
lassy('bleef','blijven','WW(pv,verl,ev)').
lassy('systeem','systeem','N(soort,ev,basis,onz,stan)').
lassy('sprake','spraak','N(soort,ev,basis,dat)').
lassy('misschien','misschien','BW()').
lassy('daarna','daarna','BW()').
lassy('Daarnaast','daarnaast','BW()').
lassy('beroep','beroep','N(soort,ev,basis,onz,stan)').
lassy('10','10','TW(hoofd,prenom,stan)').
lassy('woord','woord','N(soort,ev,basis,onz,stan)').
lassy('won','winnen','WW(pv,verl,ev)').
lassy('kilometer','kilo_meter','N(soort,ev,basis,zijd,stan)').
lassy('kant','kant','N(soort,ev,basis,zijd,stan)').
lassy('Dat','dat','VNW(aanw,det,stan,prenom,zonder,evon)').
lassy('betreft','betreffen','WW(pv,tgw,met-t)').
lassy('algemene','algemeen','ADJ(prenom,basis,met-e,stan)').
lassy('20','20','TW(hoofd,prenom,stan)').
lassy('verhaal','verhaal','N(soort,ev,basis,onz,stan)').
lassy('kennis','kennis','N(soort,ev,basis,zijd,stan)').
lassy('gevallen','geval','N(soort,mv,basis)').
lassy('DE','de','LID(bep,stan,rest)').
lassy('wedstrijd','wedstrijd','N(soort,ev,basis,zijd,stan)').
lassy('provincie','provincie','N(soort,ev,basis,zijd,stan)').
lassy('om','om','VZ(fin)').
lassy('gebieden','gebied','N(soort,mv,basis)').
lassy('resultaten','resultaat','N(soort,mv,basis)').
lassy('3','3','TW(hoofd,prenom,stan)').
lassy('programma','programma','N(soort,ev,basis,onz,stan)').
lassy('Nu','nu','BW()').
lassy('na','na','VZ(fin)').
lassy('vond','vinden','WW(pv,verl,ev)').
lassy('Hoewel','hoewel','VG(onder)').
lassy('hebt','hebben','WW(pv,tgw,met-t)').
lassy('Bovendien','bovendien','BW()').
lassy('Italiaanse','Italiaans','ADJ(prenom,basis,met-e,stan)').
lassy('graag','graag','BW()').
lassy('Zij','zij','VNW(pers,pron,nomin,vol,3v,ev,fem)').
lassy('Commissie','commissie','N(eigen,ev,basis,zijd,stan)').
lassy('5','5','TW(hoofd,vrij)').
lassy('waarschijnlijk','waarschijnlijk','ADJ(vrij,basis,zonder)').
lassy('ver','ver','ADJ(vrij,basis,zonder)').
lassy('steden','stad','N(soort,mv,basis)').
lassy('stand','stand','N(soort,ev,basis,zijd,stan)').
lassy('Russische','Russisch','ADJ(prenom,basis,met-e,stan)').
lassy('moeilijk','moeilijk','ADJ(vrij,basis,zonder)').
lassy('bestuur','bestuur','N(soort,ev,basis,onz,stan)').
lassy('zogenaamde','zogenaamd','ADJ(prenom,basis,met-e,stan)').
lassy('\'s','de','LID(bep,gen,evmo)').
lassy('lokale','lokaal','ADJ(prenom,basis,met-e,stan)').
lassy('gaf','geven','WW(pv,verl,ev)').
lassy('terecht','terecht','ADJ(vrij,basis,zonder)').
lassy('minuten','minuut','N(soort,mv,basis)').
lassy('feit','feit','N(soort,ev,basis,onz,stan)').
lassy('druk','druk','N(soort,ev,basis,zijd,stan)').
lassy('artikel','artikel','N(soort,ev,basis,onz,stan)').
lassy('11','11','TW(hoofd,vrij)').
lassy('minder','weinig','VNW(onbep,grad,stan,prenom,zonder,agr,comp)').
lassy('gewoon','gewoon','ADJ(vrij,basis,zonder)').
lassy('titel','titel','N(soort,ev,basis,zijd,stan)').
lassy('regels','regel','N(soort,mv,basis)').
lassy('niveau','niveau','N(soort,ev,basis,onz,stan)').
lassy('hoog','hoog','ADJ(vrij,basis,zonder)').
lassy('auto','auto','N(soort,ev,basis,zijd,stan)').
lassy('opgericht','op_richten','WW(vd,vrij,zonder)').
lassy('Israëlische','Israëlisch','ADJ(prenom,basis,met-e,stan)').
lassy('gevonden','vinden','WW(vd,vrij,zonder)').
lassy('2','2','TW(hoofd,prenom,stan)').
lassy('Israël','Israël','N(eigen,ev,basis,onz,stan)').
lassy('5','5','TW(hoofd,prenom,stan)').
lassy('Rusland','Rusland','N(eigen,ev,basis,onz,stan)').
lassy('reeks','reeks','N(soort,ev,basis,zijd,stan)').
lassy('paus','paus','N(soort,ev,basis,zijd,stan)').
lassy('brief','brief','N(soort,ev,basis,zijd,stan)').
lassy('zag','zien','WW(pv,verl,ev)').
lassy('wist','weten','WW(pv,verl,ev)').
lassy('vast','vast','ADJ(vrij,basis,zonder)').
lassy('studie','studie','N(soort,ev,basis,zijd,stan)').
lassy('verband','verband','N(soort,ev,basis,onz,stan)').
lassy('reden','reden','N(soort,ev,basis,zijd,stan)').
lassy('morgen','morgen','BW()').
lassy('zoon','zoon','N(soort,ev,basis,zijd,stan)').
lassy('woning','woning','N(soort,ev,basis,zijd,stan)').
lassy('werken','werken','WW(inf,vrij,zonder)').
lassy('kosten','kost','N(soort,mv,basis)').
lassy('bezoek','bezoek','N(soort,ev,basis,onz,stan)').
lassy('beslissing','beslissing','N(soort,ev,basis,zijd,stan)').
lassy('belangrijk','belangrijk','ADJ(vrij,basis,zonder)').
lassy('website','web_site','N(soort,ev,basis,zijd,stan)').
lassy('1999','1999','TW(hoofd,vrij)').
lassy('zee','zee','N(soort,ev,basis,zijd,stan)').
lassy('sterke','sterk','ADJ(prenom,basis,met-e,stan)').
lassy('beeld','beeld','N(soort,ev,basis,onz,stan)').
lassy('trein','trein','N(soort,ev,basis,zijd,stan)').
lassy('thuis','thuis','BW()').
lassy('speelt','spelen','WW(pv,tgw,met-t)').
lassy('risico','risico','N(soort,ev,basis,onz,stan)').
lassy('nummer','nummer','N(soort,ev,basis,onz,stan)').
lassy('eveneens','eveneens','BW()').
lassy('willen','willen','WW(inf,vrij,zonder)').
lassy('voorkomen','voorkomen','WW(inf,vrij,zonder)').
lassy('slachtoffers','slachtoffer','N(soort,mv,basis)').
lassy('personen','persoon','N(soort,mv,basis)').
lassy('diverse','divers','ADJ(prenom,basis,met-e,stan)').
lassy('centrale','centraal','ADJ(prenom,basis,met-e,stan)').
lassy('waaronder','waaronder','BW()').
lassy('waarde','waarde','N(soort,ev,basis,zijd,stan)').
lassy('Vlaams','Vlaams','ADJ(prenom,basis,zonder)').
lassy('oog','oog','N(soort,ev,basis,onz,stan)').
lassy('maken','maken','WW(pv,tgw,mv)').
lassy('inmiddels','inmiddels','BW()').
lassy('flo','flo','N(soort,ev,basis,zijd,stan)').
lassy('Brusselse','Brussels','ADJ(prenom,basis,met-e,stan)').
lassy('Over','over','VZ(init)').
lassy('dient','dienen','WW(pv,tgw,met-t)').
lassy('zin','zin','N(soort,ev,basis,zijd,stan)').
lassy('overleg','overleg','N(soort,ev,basis,onz,stan)').
lassy('moeder','moeder','N(soort,ev,basis,zijd,stan)').
lassy('leeftijd','leeftijd','N(soort,ev,basis,zijd,stan)').
lassy('kregen','krijgen','WW(pv,verl,mv)').
lassy('gebeurt','gebeuren','WW(pv,tgw,met-t)').
lassy('Engeland','Engeland','N(eigen,ev,basis,onz,stan)').
lassy('begint','beginnen','WW(pv,tgw,met-t)').
lassy('vlak','vlak','N(soort,ev,basis,onz,stan)').
lassy('slag','slag','N(soort,ev,basis,zijd,stan)').
lassy('project','project','N(soort,ev,basis,onz,stan)').
lassy('leider','leider','N(soort,ev,basis,zijd,stan)').
lassy('Hier','hier','VNW(aanw,adv-pron,obl,vol,3o,getal)').
lassy('burgers','burger','N(soort,mv,basis)').
lassy('vanwege','vanwege','VZ(init)').
lassy('richting','richting','N(soort,ev,basis,zijd,stan)').
lassy('proces','proces','N(soort,ev,basis,onz,stan)').
lassy('positie','positie','N(soort,ev,basis,zijd,stan)').
lassy('medicijnen','medicijn','N(soort,mv,basis)').
lassy('kort','kort','ADJ(vrij,basis,zonder)').
lassy('hoogte','hoogte','N(soort,ev,basis,zijd,stan)').
lassy('grens','grens','N(soort,ev,basis,zijd,stan)').
lassy('doen','doen','WW(pv,tgw,mv)').
lassy('Dat','dat','VG(onder)').
lassy('Afghanistan','Afghanistan','N(eigen,ev,basis,onz,stan)').
lassy('6','6','TW(hoofd,vrij)').
lassy('mens','mens','N(soort,ev,basis,zijd,stan)').
lassy('Vooral','vooral','BW()').
lassy('Veel','veel','VNW(onbep,grad,stan,prenom,zonder,agr,basis)').
lassy('Men','men','VNW(pers,pron,nomin,red,3p,ev,masc)').
lassy('aldus','aldus','BW()').
lassy('waarom','waarom','BW()').
lassy('studenten','student','N(soort,mv,basis)').
lassy('houdt','houden','WW(pv,tgw,met-t)').
lassy('beschikbaar','beschikbaar','ADJ(vrij,basis,zonder)').
lassy('vind','vinden','WW(pv,tgw,ev)').
lassy('vinden','vinden','WW(pv,tgw,mv)').
lassy('openbare','openbaar','ADJ(prenom,basis,met-e,stan)').
lassy('geboren','geboren','WW(vd,vrij,zonder)').
lassy('album','album','N(soort,ev,basis,onz,stan)').
lassy('al','al','VG(onder)').
lassy('zaterdag','zaterdag','N(eigen,ev,basis,zijd,stan)').
lassy('plaatsen','plaats','N(soort,mv,basis)').
lassy('langs','langs','VZ(init)').
lassy('werknemers','werknemer','N(soort,mv,basis)').
lassy('middel','middel','N(soort,ev,basis,onz,stan)').
lassy('licht','licht','N(soort,ev,basis,onz,stan)').
lassy('--','--','LET()').
lassy('bestaande','bestaan','WW(od,prenom,met-e)').
lassy('Alle','al','VNW(onbep,det,stan,prenom,met-e,agr)').
lassy('volgt','volgen','WW(pv,tgw,met-t)').
lassy('ruimte','ruimte','N(soort,ev,basis,zijd,stan)').
lassy('rechter','rechter','N(soort,ev,basis,zijd,stan)').
lassy('plannen','plan','N(soort,mv,basis)').
lassy('leidde','leiden','WW(pv,verl,ev)').
lassy('Justitie','justitie','N(soort,ev,basis,zijd,stan)').
lassy('goud','goud','N(soort,ev,basis,onz,stan)').
lassy('finale','finale','N(soort,ev,basis,zijd,stan)').
lassy('blijven','blijven','WW(pv,tgw,mv)').
lassy('beetje','beetje','N(soort,ev,dim,onz,stan)').
lassy('Alleen','alleen','BW()').
lassy('woorden','woord','N(soort,mv,basis)').
lassy('stellen','stellen','WW(inf,vrij,zonder)').
lassy('direct','direct','ADJ(vrij,basis,zonder)').
lassy('betrekking','betrekking','N(soort,ev,basis,zijd,stan)').
lassy('advies','advies','N(soort,ev,basis,onz,stan)').
lassy('Vanaf','vanaf','VZ(init)').
lassy('termijn','termijn','N(soort,ev,basis,zijd,stan)').
lassy('punten','punt','N(soort,mv,basis)').
lassy('oplossing','oplossing','N(soort,ev,basis,zijd,stan)').
lassy('lucht','lucht','N(soort,ev,basis,zijd,stan)').
lassy('Is','zijn','WW(pv,tgw,ev)').
lassy('controle','controle','N(soort,ev,basis,zijd,stan)').
lassy('8','8','TW(hoofd,vrij)').
lassy('vroeg','vragen','WW(pv,verl,ev)').
lassy('toepassing','toepassing','N(soort,ev,basis,zijd,stan)').
lassy('overigens','overigens','BW()').
lassy('nauwelijks','nauwelijks','BW()').
lassy('Naast','naast','VZ(init)').
lassy('kritiek','kritiek','N(soort,ev,basis,zijd,stan)').
lassy('hard','hard','ADJ(vrij,basis,zonder)').
lassy('groei','groei','N(soort,ev,basis,zijd,stan)').
lassy('dollar','dollar','N(soort,ev,basis,zijd,stan)').
lassy('Buitenlandse','buitenlands','ADJ(prenom,basis,met-e,stan)').
lassy('bovendien','bovendien','BW()').
lassy('wegens','wegens','VZ(init)').
lassy('niemand','niemand','VNW(onbep,pron,stan,vol,3p,ev)').
lassy('half','half','ADJ(prenom,basis,zonder)').
lassy('functie','functie','N(soort,ev,basis,zijd,stan)').
lassy('aanwezig','aanwezig','ADJ(vrij,basis,zonder)').
lassy('15','15','TW(hoofd,prenom,stan)').
lassy('1','1','TW(hoofd,prenom,stan)').
lassy('ziet','zien','WW(pv,tgw,met-t)').
lassy('werking','werking','N(soort,ev,basis,zijd,stan)').
lassy('volledige','volledig','ADJ(prenom,basis,met-e,stan)').
lassy('taal','taal','N(soort,ev,basis,zijd,stan)').
lassy('resultaat','resultaat','N(soort,ev,basis,onz,stan)').
lassy('parlement','parlement','N(soort,ev,basis,onz,stan)').
lassy('Nederlanders','Nederlander','N(eigen,mv,basis)').
lassy('goederen','goed','N(soort,mv,basis)').
lassy('gekregen','krijgen','WW(vd,vrij,zonder)').
lassy('gedurende','gedurende','VZ(init)').
lassy('actief','actief','ADJ(vrij,basis,zonder)').
lassy('1998','1998','TW(hoofd,vrij)').
lassy('tegenover','tegenover','VZ(init)').
lassy('schade','schade','N(soort,ev,basis,zijd,stan)').
lassy('goed','goed','ADJ(prenom,basis,zonder)').
lassy('echte','echt','ADJ(prenom,basis,met-e,stan)').
lassy('stelt','stellen','WW(pv,tgw,met-t)').
lassy('gezet','zetten','WW(vd,vrij,zonder)').
lassy('gevaar','gevaar','N(soort,ev,basis,onz,stan)').
lassy('Europees','Europees','ADJ(prenom,basis,zonder)').
lassy('Deze','deze','VNW(aanw,det,stan,nom,met-e,zonder-n)').
lassy('daarvoor','daarvoor','BW()').
lassy('bedrag','bedrag','N(soort,ev,basis,onz,stan)').
lassy('antwoord','antwoord','N(soort,ev,basis,onz,stan)').
lassy('aanslagen','aanslag','N(soort,mv,basis)').
lassy('speciale','speciaal','ADJ(prenom,basis,met-e,stan)').
lassy('punt','punt','N(soort,ev,basis,onz,stan)').
lassy('jij','jij','VNW(pers,pron,nomin,vol,2v,ev)').
lassy('Dan','dan','BW()').
lassy('weer','weer','N(soort,ev,basis,onz,stan)').
lassy('voormalige','voormalig','ADJ(prenom,basis,met-e,stan)').
lassy('vol','vol','ADJ(vrij,basis,zonder)').
lassy('orde','orde','N(soort,ev,basis,zijd,stan)').
lassy('leiden','leiden','WW(inf,vrij,zonder)').
lassy('-ie','hij','VNW(pers,pron,nomin,red,3,ev,masc)').
lassy('gebruiken','gebruiken','WW(inf,vrij,zonder)').
lassy('betalen','betalen','WW(inf,vrij,zonder)').
lassy('verdere','ver','ADJ(prenom,comp,met-e,stan)').
lassy('uitvoering','uitvoering','N(soort,ev,basis,zijd,stan)').
lassy('mogelijkheden','mogelijkheid','N(soort,mv,basis)').
lassy('gulden','gulden','N(soort,ev,basis,zijd,stan)').
lassy('4','4','TW(hoofd,prenom,stan)').
lassy('vrijwel','vrijwel','BW()').
lassy('voorstel','voorstel','N(soort,ev,basis,onz,stan)').
lassy('stap','stap','N(soort,ev,basis,zijd,stan)').
lassy('schreef','schrijven','WW(pv,verl,ev)').
lassy('mogelijke','mogelijk','ADJ(prenom,basis,met-e,stan)').
lassy('ministers','minister','N(soort,mv,basis)').
lassy('kracht','kracht','N(soort,ev,basis,zijd,stan)').
lassy('groter','groot','ADJ(vrij,comp,zonder)').
lassy('Bush','Bush','N(eigen,ev,basis,zijd,stan)').
lassy('verklaring','verklaring','N(soort,ev,basis,zijd,stan)').
lassy('uitgevoerd','uit_voeren','WW(vd,vrij,zonder)').
lassy('tegen','tegen','VZ(fin)').
lassy('soldaten','soldaat','N(soort,mv,basis)').
lassy('instellingen','instelling','N(soort,mv,basis)').
lassy('helpen','helpen','WW(inf,vrij,zonder)').
lassy('heen','heen','VZ(fin)').
lassy('Amerika','Amerika','N(eigen,ev,basis,onz,stan)').
lassy('aanslag','aanslag','N(soort,ev,basis,zijd,stan)').
lassy('100','100','TW(hoofd,prenom,stan)').
lassy('sector','sector','N(soort,ev,basis,zijd,stan)').
lassy('Ron','Ron','N(eigen,ev,basis,zijd,stan)').
lassy('interne','intern','ADJ(prenom,basis,met-e,stan)').
lassy('campagne','campagne','N(soort,ev,basis,zijd,stan)').
lassy('spelen','spelen','WW(inf,vrij,zonder)').
lassy('serie','serie','N(soort,ev,basis,zijd,stan)').
lassy('productie','productie','N(soort,ev,basis,zijd,stan)').
lassy('justitie','justitie','N(soort,ev,basis,zijd,stan)').
lassy('gericht','richten','WW(vd,vrij,zonder)').
lassy('enorme','enorm','ADJ(prenom,basis,met-e,stan)').
lassy('buitenland','buitenland','N(soort,ev,basis,onz,stan)').
lassy('zuiden','zuiden','N(soort,ev,basis,onz,stan)').
lassy('Wanneer','wanneer','VG(onder)').
lassy('onderhandelingen','onderhandeling','N(soort,mv,basis)').
lassy('omgeving','omgeving','N(soort,ev,basis,zijd,stan)').
lassy('ieder','ieder','VNW(onbep,det,stan,prenom,zonder,evon)').
lassy('eerste','één','TW(rang,nom,zonder-n)').
lassy('Duitsers','Duitser','N(eigen,mv,basis)').
lassy('brand','brand','N(soort,ev,basis,zijd,stan)').
lassy('25','25','TW(hoofd,prenom,stan)').
lassy('zwaar','zwaar','ADJ(vrij,basis,zonder)').
lassy('voorbeeld','voorbeeld','N(soort,ev,basis,onz,stan)').
lassy('straat','straat','N(soort,ev,basis,zijd,stan)').
lassy('rapport','rapport','N(soort,ev,basis,onz,stan)').
lassy('projecten','project','N(soort,mv,basis)').
lassy('gebaseerd','baseren','WW(vd,vrij,zonder)').
lassy('doden','dood','ADJ(nom,basis,met-e,mv-n)').
lassy('buurt','buurt','N(soort,ev,basis,zijd,stan)').
lassy('zitten','zitten','WW(pv,tgw,mv)').
lassy('Waarom','waarom','BW()').
lassy('totale','totaal','ADJ(prenom,basis,met-e,stan)').
lassy('dergelijke','dergelijk','ADJ(prenom,basis,met-e,stan)').
lassy('Daarbij','daarbij','BW()').
lassy('burgemeester','burgemeester','N(soort,ev,basis,zijd,stan)').
lassy('bezig','bezig','ADJ(vrij,basis,zonder)').
lassy('aanpak','aanpak','N(soort,ev,basis,zijd,stan)').
lassy('partner','partner','N(soort,ev,basis,zijd,stan)').
lassy('mogelijkheid','mogelijkheid','N(soort,ev,basis,zijd,stan)').
lassy('mate','mate','N(soort,ev,basis,zijd,stan)').
lassy('jongen','jongen','N(soort,ev,basis,zijd,stan)').
lassy('Iraakse','Iraaks','ADJ(prenom,basis,met-e,stan)').
lassy('zien','zien','WW(pv,tgw,mv)').
lassy('vrienden','vriend','N(soort,mv,basis)').
lassy('Verder','ver','ADJ(vrij,comp,zonder)').
lassy('verantwoordelijk','verantwoordelijk','ADJ(vrij,basis,zonder)').
lassy('veiligheid','veiligheid','N(soort,ev,basis,zijd,stan)').
lassy('samenleving','samenleving','N(soort,ev,basis,zijd,stan)').
lassy('organisaties','organisatie','N(soort,mv,basis)').
lassy('moord','moord','N(soort,ev,basis,zijd,stan)').
lassy('kwestie','kwestie','N(soort,ev,basis,zijd,stan)').
lassy('gang','gang','N(soort,ev,basis,zijd,stan)').
lassy('ervoor','ervoor','BW()').
lassy('daardoor','daardoor','BW()').
lassy('bijzonder','bijzonder','ADJ(vrij,basis,zonder)').
lassy('begonnen','beginnen','WW(vd,vrij,zonder)').
lassy('beginnen','beginnen','WW(inf,vrij,zonder)').
lassy('vormen','vormen','WW(pv,tgw,mv)').
lassy('vervolgens','vervolgens','BW()').
lassy('specifieke','specifiek','ADJ(prenom,basis,met-e,stan)').
lassy('prijzen','prijs','N(soort,mv,basis)').
lassy('lichaam','lichaam','N(soort,ev,basis,onz,stan)').
lassy('gebeuren','gebeuren','WW(inf,vrij,zonder)').
lassy('centrum','centrum','N(soort,ev,basis,onz,stan)').
lassy('betere','goed','ADJ(prenom,comp,met-e,stan)').
lassy('20','20','TW(hoofd,vrij)').
lassy('zomer','zomer','N(soort,ev,basis,zijd,stan)').
lassy('ziekte','ziekte','N(soort,ev,basis,zijd,stan)').
lassy('ziekenhuis','ziekenhuis','N(soort,ev,basis,onz,stan)').
lassy('vrijdag','vrijdag','N(eigen,ev,basis,zijd,stan)').
lassy('rest','rest','N(soort,ev,basis,zijd,stan)').
lassy('rechtbank','rechtbank','N(soort,ev,basis,zijd,stan)').
lassy('milieu','milieu','N(soort,ev,basis,onz,stan)').
lassy('leidt','leiden','WW(pv,tgw,met-t)').
lassy('klanten','klant','N(soort,mv,basis)').
lassy('beweging','beweging','N(soort,ev,basis,zijd,stan)').
lassy('actie','actie','N(soort,ev,basis,zijd,stan)').
lassy('Unie','unie','N(soort,ev,basis,zijd,stan)').
lassy('toegang','toegang','N(soort,ev,basis,zijd,stan)').
lassy('oud','oud','ADJ(vrij,basis,zonder)').
lassy('ogen','oog','N(soort,mv,basis)').
lassy('noorden','noorden','N(soort,ev,basis,onz,stan)').
lassy('hiervoor','hiervoor','BW()').
lassy('enkel','enkel','BW()').
lassy('effect','effect','N(soort,ev,basis,onz,stan)').
lassy('bevat','bevatten','WW(pv,tgw,ev)').
lassy('30','30','TW(hoofd,prenom,stan)').
lassy('officiële','officieel','ADJ(prenom,basis,met-e,stan)').
lassy('koningin','koningin','N(soort,ev,basis,zijd,stan)').
lassy('hetzelfde','hetzelfde','ADJ(prenom,basis,zonder)').
lassy('Een','één','TW(hoofd,nom,zonder-n,basis)').
lassy('betrokken','betrekken','WW(vd,vrij,zonder)').
lassy('winst','winst','N(soort,ev,basis,zijd,stan)').
lassy('slecht','slecht','ADJ(vrij,basis,zonder)').
lassy('keuze','keuze','N(soort,ev,basis,zijd,stan)').
lassy('FNB','FNB','N(eigen,ev,basis,zijd,stan)').
lassy('9','9','TW(hoofd,vrij)').
lassy('7','7','TW(hoofd,vrij)').
lassy('weten','weten','WW(pv,tgw,mv)').
lassy('waarvoor','waarvoor','BW()').
lassy('drank','drank','N(soort,ev,basis,zijd,stan)').
lassy('bang','bang','ADJ(vrij,basis,zonder)').
lassy('aanleiding','aanleiding','N(soort,ev,basis,zijd,stan)').
lassy('Zoals','zoals','VG(onder)').
lassy('Nog','nog','BW()').
lassy('nacht','nacht','N(soort,ev,basis,zijd,stan)').
lassy('meerderheid','meerderheid','N(soort,ev,basis,zijd,stan)').
lassy('gewone','gewoon','ADJ(prenom,basis,met-e,stan)').
lassy('erop','erop','BW()').
lassy('bepaald','bepalen','WW(vd,vrij,zonder)').
lassy('aarde','aarde','N(soort,ev,basis,zijd,stan)').
lassy('telt','tellen','WW(pv,tgw,met-t)').
lassy('tekst','tekst','N(soort,ev,basis,zijd,stan)').
lassy('technische','technisch','ADJ(prenom,basis,met-e,stan)').
lassy('schip','schip','N(soort,ev,basis,onz,stan)').
lassy('hoop','hoop','N(soort,ev,basis,zijd,stan)').
lassy('gebouw','gebouw','N(soort,ev,basis,onz,stan)').
lassy('ander','ander','ADJ(nom,basis,zonder,zonder-n)').
lassy('VLD','VLD','N(eigen,ev,basis,zijd,stan)').
lassy('moderne','modern','ADJ(prenom,basis,met-e,stan)').
lassy('lag','liggen','WW(pv,verl,ev)').
lassy('Japan','Japan','N(eigen,ev,basis,onz,stan)').
lassy('hiervan','hiervan','BW()').
lassy('gemeenschap','gemeenschap','N(soort,ev,basis,zijd,stan)').
lassy('directeur','directeur','N(soort,ev,basis,zijd,stan)').
lassy('allerlei','allerlei','ADJ(prenom,basis,zonder)').
lassy('zoveel','zoveel','TW(hoofd,vrij)').
lassy('vroeger','vroeg','ADJ(vrij,comp,zonder)').
lassy('regio','regio','N(soort,ev,basis,zijd,stan)').
lassy('regelmatig','regelmatig','ADJ(vrij,basis,zonder)').
lassy('Parijs','Parijs','N(eigen,ev,basis,onz,stan)').
lassy('overwinning','overwinning','N(soort,ev,basis,zijd,stan)').
lassy('liggen','liggen','WW(pv,tgw,mv)').
lassy('inkomen','inkomen','N(soort,ev,basis,onz,stan)').
lassy('horen','horen','WW(inf,vrij,zonder)').
lassy('heren','heer','N(soort,mv,basis)').
lassy('groepen','groep','N(soort,mv,basis)').
lassy('discussie','discussie','N(soort,ev,basis,zijd,stan)').
lassy('15','15','TW(hoofd,vrij)').
lassy('zitten','zitten','WW(inf,vrij,zonder)').
lassy('Wel','wel','BW()').
lassy('Twee','twee','TW(hoofd,prenom,stan)').
lassy('muziek','muziek','N(soort,ev,basis,zijd,stan)').
lassy('initiatief','initiatief','N(soort,ev,basis,onz,stan)').
lassy('Britten','Brit','N(eigen,mv,basis)').
lassy('50','50','TW(hoofd,prenom,stan)').
lassy('zat','zitten','WW(pv,verl,ev)').
lassy('raad','raad','N(soort,ev,basis,zijd,stan)').
lassy('open','open','ADJ(vrij,basis,zonder)').
lassy('ontstond','ontstaan','WW(pv,verl,ev)').
lassy('omstandigheden','omstandigheid','N(soort,mv,basis)').
lassy('indien','indien','VG(onder)').
lassy('grotere','groot','ADJ(prenom,comp,met-e,stan)').
lassy('gekozen','kiezen','WW(vd,vrij,zonder)').
lassy('gebeurde','gebeuren','WW(pv,verl,ev)').
lassy('ernstige','ernstig','ADJ(prenom,basis,met-e,stan)').
lassy('biedt','bieden','WW(pv,tgw,met-t)').
lassy('besloot','besluiten','WW(pv,verl,ev)').
lassy('18','18','TW(hoofd,vrij)').
lassy('14','14','TW(hoofd,vrij)').
lassy('vrije','vrij','ADJ(prenom,basis,met-e,stan)').
lassy('volgen','volgen','WW(inf,vrij,zonder)').
lassy('vergadering','vergadering','N(soort,ev,basis,zijd,stan)').
lassy('staten','staat','N(soort,mv,basis)').
lassy('Spanje','Spanje','N(eigen,ev,basis,onz,stan)').
lassy('rechten','recht','N(soort,mv,basis)').
lassy('personeel','personeel','N(soort,ev,basis,onz,stan)').
lassy('mocht','mogen','WW(pv,verl,ev)').
lassy('lijst','lijst','N(soort,ev,basis,zijd,stan)').
lassy('laatste','laat','ADJ(nom,sup,met-e,zonder-n,stan)').
lassy('idee','idee','N(soort,ev,basis,onz,stan)').
lassy('vragen','vragen','WW(inf,vrij,zonder)').
lassy('vaste','vast','ADJ(prenom,basis,met-e,stan)').
lassy('stoffen','stof','N(soort,mv,basis)').
lassy('loopt','lopen','WW(pv,tgw,met-t)').
lassy('Groningen','Groningen','N(eigen,ev,basis,onz,stan)').
lassy('gevraagd','vragen','WW(vd,vrij,zonder)').
lassy('geleid','leiden','WW(vd,vrij,zonder)').
lassy('ene','een','VNW(onbep,det,stan,prenom,met-e,evz)').
lassy('delen','deel','N(soort,mv,basis)').
lassy('arts','arts','N(soort,ev,basis,zijd,stan)').
lassy('17','17','TW(hoofd,vrij)').
lassy('VVD','VVD','N(eigen,ev,basis,zijd,stan)').
lassy('voorwaarden','voorwaarde','N(soort,mv,basis)').
lassy('team','team','N(soort,ev,basis,onz,stan)').
lassy('Spaanse','Spaans','ADJ(prenom,basis,met-e,stan)').
lassy('Pakistan','Pakistan','N(eigen,ev,basis,onz,stan)').
lassy('ontwikkeld','ontwikkelen','WW(vd,vrij,zonder)').
lassy('gezicht','gezicht','N(soort,ev,basis,onz,stan)').
lassy('gewond','gewond','ADJ(vrij,basis,zonder)').
lassy('bereikt','bereiken','WW(vd,vrij,zonder)').
lassy('bekende','bekend','ADJ(prenom,basis,met-e,stan)').
lassy('baan','baan','N(soort,ev,basis,zijd,stan)').
lassy('verleden','verleden','N(soort,ev,basis,onz,stan)').
lassy('verdrag','verdrag','N(soort,ev,basis,onz,stan)').
lassy('universiteit','universiteit','N(soort,ev,basis,zijd,stan)').
lassy('top','top','N(soort,ev,basis,zijd,stan)').
lassy('Palestijnse','Palestijns','ADJ(prenom,basis,met-e,stan)').
lassy('moeite','moeite','N(soort,ev,basis,zijd,stan)').
lassy('....','....','LET()').
lassy('honderd','honderd','TW(hoofd,prenom,stan)').
lassy('gingen','gaan','WW(pv,verl,mv)').
lassy('crisis','crisis','N(soort,ev,basis,zijd,stan)').
lassy('Al','al','BW()').
lassy('werken','werk','N(soort,mv,basis)').
lassy('vrijheid','vrijheid','N(soort,ev,basis,zijd,stan)').
lassy('Vandaag','vandaag','BW()').
lassy('vaker','vaak','ADJ(vrij,comp,zonder)').
lassy('relatie','relatie','N(soort,ev,basis,zijd,stan)').
lassy('nam','nemen','WW(pv,verl,ev)').
lassy('kost','kosten','WW(pv,tgw,ev)').
lassy('halen','halen','WW(inf,vrij,zonder)').
lassy('brochure','brochure','N(soort,ev,basis,zijd,stan)').
lassy('betaald','betalen','WW(vd,vrij,zonder)').
lassy('1994','1994','TW(hoofd,vrij)').
lassy('wonen','wonen','WW(pv,tgw,mv)').
lassy('vrede','vrede','N(soort,ev,basis,zijd,stan)').
lassy('verschil','verschil','N(soort,ev,basis,onz,stan)').
lassy('verkocht','verkopen','WW(vd,vrij,zonder)').
lassy('verantwoordelijkheid','verantwoordelijkheid','N(soort,ev,basis,zijd,stan)').
lassy('ton','ton','N(soort,ev,basis,zijd,stan)').
lassy('studies','studie','N(soort,mv,basis)').
lassy('slachtoffer','slachtoffer','N(soort,ev,basis,onz,stan)').
lassy('ondersteuning','ondersteuning','N(soort,ev,basis,zijd,stan)').
lassy('klein','klein','ADJ(prenom,basis,zonder)').
lassy('Iran','Iran','N(eigen,ev,basis,onz,stan)').
lassy('gesloten','sluiten','WW(vd,vrij,zonder)').
lassy('gebouwd','bouwen','WW(vd,vrij,zonder)').
lassy('Frans','Frans','N(eigen,ev,basis,onz,stan)').
lassy('combinatie','combinatie','N(soort,ev,basis,zijd,stan)').
lassy('bedoeld','bedoelen','WW(vd,vrij,zonder)').
lassy('algemeen','algemeen','ADJ(nom,basis,zonder,zonder-n)').
lassy('wapens','wapen','N(soort,mv,basis)').
lassy('voordat','voordat','VG(onder)').
lassy('vliegtuig','vliegtuig','N(soort,ev,basis,onz,stan)').
lassy('viel','vallen','WW(pv,verl,ev)').
lassy('product','product','N(soort,ev,basis,onz,stan)').
lassy('kun','kunnen','WW(pv,tgw,ev)').
lassy('kent','kennen','WW(pv,tgw,met-t)').
lassy('iedere','ieder','VNW(onbep,det,stan,prenom,met-e,evz)').
lassy('hersenen','hersenen','N(soort,mv,basis)').
lassy('bereiken','bereiken','WW(inf,vrij,zonder)').
lassy('zondag','zondag','N(eigen,ev,basis,zijd,stan)').
lassy('voornamelijk','voornamelijk','ADJ(vrij,basis,zonder)').
lassy('verplicht','verplichten','WW(vd,vrij,zonder)').
lassy('tevens','tevens','BW()').
lassy('=','=','SPEC(symb)').
lassy('Prince','Prince','N(eigen,ev,basis,zijd,stan)').
lassy('partners','partner','N(soort,mv,basis)').
lassy('Ondanks','ondanks','VZ(init)').
lassy('noodzakelijk','noodzakelijk','ADJ(vrij,basis,zonder)').
lassy('meerdere','meerdere','VNW(onbep,det,stan,prenom,met-e,mv)').
lassy('Hun','hun','VNW(bez,det,stan,vol,3,mv,prenom,zonder,agr)').
lassy('honderden','honderd','TW(hoofd,nom,mv-n,basis)').
lassy('hoewel','hoewel','VG(onder)').
lassy('gemiddeld','gemiddeld','ADJ(vrij,basis,zonder)').
lassy('Egypte','Egypte','N(eigen,ev,basis,onz,stan)').
lassy('duizenden','duizend','TW(hoofd,nom,mv-n,basis)').
lassy('doordat','doordat','VG(onder)').
lassy('conflict','conflict','N(soort,ev,basis,onz,stan)').
lassy('belangrijk','belangrijk','ADJ(prenom,basis,zonder)').
lassy('afstand','afstand','N(soort,ev,basis,zijd,stan)').
lassy('Afrika','Afrika','N(eigen,ev,basis,onz,stan)').
lassy('6','6','TW(hoofd,prenom,stan)').
lassy('40','40','TW(hoofd,prenom,stan)').
lassy('12','12','TW(hoofd,vrij)').
lassy('zorg','zorg','N(soort,ev,basis,zijd,stan)').
lassy('verbonden','verbinden','WW(vd,vrij,zonder)').
lassy('toestand','toestand','N(soort,ev,basis,zijd,stan)').
lassy('Londen','Londen','N(eigen,ev,basis,onz,stan)').
lassy('lezen','lezen','WW(inf,vrij,zonder)').
lassy('landbouw','landbouw','N(soort,ev,basis,zijd,stan)').
lassy('initiatieven','initiatief','N(soort,mv,basis)').
lassy('inderdaad','inderdaad','BW()').
lassy('gesproken','spreken','WW(vd,vrij,zonder)').
lassy('denk','denken','WW(pv,tgw,ev)').
lassy('besloten','besluiten','WW(vd,vrij,zonder)').
lassy('afdeling','afdeling','N(soort,ev,basis,zijd,stan)').
lassy('achter','achter','VZ(fin)').
lassy('1996','1996','TW(hoofd,vrij)').
lassy('16','16','TW(hoofd,vrij)').
lassy('zeven','zeven','TW(hoofd,prenom,stan)').
lassy('tanks','tank','N(soort,mv,basis)').
lassy('speelde','spelen','WW(pv,verl,ev)').
lassy('Sommige','sommig','VNW(onbep,det,stan,prenom,met-e,rest)').
lassy('relatief','relatief','ADJ(vrij,basis,zonder)').
lassy('onderdeel','onderdeel','N(soort,ev,basis,onz,stan)').
lassy('Naar','naar','VZ(init)').
lassy('media','medium','N(soort,mv,basis)').
lassy('Geen','geen','VNW(onbep,det,stan,prenom,zonder,agr)').
lassy('bijdrage','bijdrage','N(soort,ev,basis,zijd,stan)').
lassy('behandeld','behandelen','WW(vd,vrij,zonder)').
lassy('Amsterdamse','Amsterdams','ADJ(prenom,basis,met-e,stan)').
lassy('akkoord','akkoord','N(soort,ev,basis,onz,stan)').
lassy('zoek','zoek','ADJ(vrij,basis,zonder)').
lassy('vormen','vorm','N(soort,mv,basis)').
lassy('twintig','twintig','TW(hoofd,prenom,stan)').
lassy('taak','taak','N(soort,ev,basis,zijd,stan)').
lassy('officieel','officieel','ADJ(vrij,basis,zonder)').
lassy('neer','neer','VZ(fin)').
lassy('medewerkers','medewerker','N(soort,mv,basis)').
lassy('leek','lijken','WW(pv,verl,ev)').
lassy('Japanse','Japans','ADJ(prenom,basis,met-e,stan)').
lassy('hogere','hoog','ADJ(prenom,comp,met-e,stan)').
lassy('geheel','geheel','ADJ(vrij,basis,zonder)').
lassy('bouw','bouw','N(soort,ev,basis,zijd,stan)').
lassy('beelden','beeld','N(soort,mv,basis)').
lassy('2007','2007','TW(hoofd,vrij)').
lassy('zon','zon','N(soort,ev,basis,zijd,stan)').
lassy('wilt','willen','WW(pv,tgw,met-t)').
lassy('terrein','terrein','N(soort,ev,basis,onz,stan)').
lassy('soorten','soort','N(soort,mv,basis)').
lassy('praktijk','praktijk','N(soort,ev,basis,zijd,stan)').
lassy('kaart','kaart','N(soort,ev,basis,zijd,stan)').
lassy('India','India','N(eigen,ev,basis,onz,stan)').
lassy('hoeft','hoeven','WW(pv,tgw,met-t)').
lassy('gesprek','gesprek','N(soort,ev,basis,onz,stan)').
lassy('generaal','generaal','N(soort,ev,basis,zijd,stan)').
lassy('geheel','geheel','N(soort,ev,basis,onz,stan)').
lassy('gegaan','gaan','WW(vd,vrij,zonder)').
lassy('besluit','besluit','N(soort,ev,basis,onz,stan)').
lassy('Arabische','Arabisch','ADJ(prenom,basis,met-e,stan)').
lassy('8','8','TW(hoofd,prenom,stan)').
lassy('13','13','TW(hoofd,vrij)').
lassy('12','12','TW(hoofd,prenom,stan)').
lassy('volk','volk','N(soort,ev,basis,onz,stan)').
lassy('verloren','verliezen','WW(vd,vrij,zonder)').
lassy('uitbreiding','uitbreiding','N(soort,ev,basis,zijd,stan)').
lassy('reactie','reactie','N(soort,ev,basis,zijd,stan)').
lassy('o.a.','onder','ander').
lassy('lijn','lijn','N(soort,ev,basis,zijd,stan)').
lassy('goudstandaard','goud_standaard','N(soort,ev,basis,zijd,stan)').
lassy('dingen','ding','N(soort,mv,basis)').
lassy('Dames','dame','N(soort,mv,basis)').
lassy('cijfers','cijfer','N(soort,mv,basis)').
lassy('bijzondere','bijzonder','ADJ(prenom,basis,met-e,stan)').
lassy('zorgen','zorgen','WW(inf,vrij,zonder)').
lassy('zeggen','zeggen','WW(pv,tgw,mv)').
lassy('wilden','willen','WW(pv,verl,mv)').
lassy('verzet','verzet','N(soort,ev,basis,onz,stan)').
lassy('verhalen','verhaal','N(soort,mv,basis)').
lassy('tien','tien','TW(hoofd,vrij)').
lassy('recente','recent','ADJ(prenom,basis,met-e,stan)').
lassy('overal','overal','VNW(onbep,adv-pron,obl,vol,3o,getal)').
lassy('onmiddellijk','onmiddellijk','ADJ(vrij,basis,zonder)').
lassy('Onderwijs','onderwijs','N(soort,ev,basis,onz,stan)').
lassy('krant','krant','N(soort,ev,basis,zijd,stan)').
lassy('journaal','journaal','N(soort,ev,basis,onz,stan)').
lassy('dochter','dochter','N(soort,ev,basis,zijd,stan)').
lassy('denkt','denken','WW(pv,tgw,met-t)').
lassy('bracht','brengen','WW(pv,verl,ev)').
lassy('acht','acht','TW(hoofd,prenom,stan)').
lassy('zover','zover','BW()').
lassy('zoveel','zoveel','TW(hoofd,prenom,stan)').
lassy('werken','werken','WW(pv,tgw,mv)').
lassy('voorzien','voorzien','WW(vd,vrij,zonder)').
lassy('Sosabowski','Sosabowski','N(eigen,ev,basis,zijd,stan)').
lassy('redenen','reden','N(soort,mv,basis)').
lassy('ploeg','ploeg','N(soort,ev,basis,zijd,stan)').
lassy('oudere','oud','ADJ(prenom,comp,met-e,stan)').
lassy('opleiding','opleiding','N(soort,ev,basis,zijd,stan)').
lassy('neemt','nemen','WW(pv,tgw,met-t)').
lassy('Limburg','Limburg','N(eigen,ev,basis,onz,stan)').
lassy('daarop','daarop','BW()').
lassy('boeren','boer','N(soort,mv,basis)').
lassy('beheer','beheer','N(soort,ev,basis,onz,stan)').
lassy('Thailand','Thailand','N(eigen,ev,basis,onz,stan)').
lassy('stonden','staan','WW(pv,verl,mv)').
lassy('slechte','slecht','ADJ(prenom,basis,met-e,stan)').
lassy('optreden','optreden','N(soort,ev,basis,onz,stan)').
lassy('museum','museum','N(soort,ev,basis,onz,stan)').
lassy('juiste','juist','ADJ(prenom,basis,met-e,stan)').
lassy('hoger','hoog','ADJ(prenom,comp,zonder)').
lassy('gisteravond','gisteravond','BW()').
lassy('gehele','geheel','ADJ(prenom,basis,met-e,stan)').
lassy('Externe','extern','ADJ(prenom,basis,met-e,stan)').
lassy('cultuur','cultuur','N(soort,ev,basis,zijd,stan)').
lassy('buiten','buiten','VZ(fin)').
lassy('bank','bank','N(soort,ev,basis,zijd,stan)').
lassy('wetenschappelijke','wetenschappelijk','ADJ(prenom,basis,met-e,stan)').
lassy('wachten','wachten','WW(inf,vrij,zonder)').
lassy('vormt','vormen','WW(pv,tgw,met-t)').
lassy('vond','plaats_vinden','WW(pv,verl,ev)').
lassy('vanmiddag','vanmiddag','BW()').
lassy('uren','uur','N(soort,mv,basis)').
lassy('risico\'s','risico','N(soort,mv,basis)').
lassy('ondanks','ondanks','VZ(init)').
lassy('mening','mening','N(soort,ev,basis,zijd,stan)').
lassy('maatschappij','maatschappij','N(soort,ev,basis,zijd,stan)').
lassy('kunst','kunst','N(soort,ev,basis,zijd,stan)').
lassy('Kamer','kamer','N(soort,ev,basis,zijd,stan)').
lassy('Indien','indien','VG(onder)').
lassy('gouden','gouden','ADJ(prenom,basis,zonder)').
lassy('gebeurd','gebeuren','WW(vd,vrij,zonder)').
lassy('Fransen','Fransen','N(eigen,mv,basis)').
lassy('films','film','N(soort,mv,basis)').
lassy('én','en','VG(neven)').
lassy('bieden','bieden','WW(inf,vrij,zonder)').
lassy('bestaan','bestaan','WW(pv,tgw,mv)').
lassy('bestaan','bestaan','WW(inf,vrij,zonder)').
lassy('beschouwd','beschouwen','WW(vd,vrij,zonder)').
lassy('bescherming','bescherming','N(soort,ev,basis,zijd,stan)').
lassy('Andere','ander','ADJ(prenom,basis,met-e,stan)').
lassy('afspraken','afspraak','N(soort,mv,basis)').
lassy('1993','1993','TW(hoofd,vrij)').
lassy('zwarte','zwart','ADJ(prenom,basis,met-e,stan)').
lassy('verlies','verlies','N(soort,ev,basis,onz,stan)').
lassy('totaal','totaal','N(soort,ev,basis,onz,stan)').
lassy('Terwijl','terwijl','VG(onder)').
lassy('rust','rust','N(soort,ev,basis,zijd,stan)').
lassy('hoeveelheid','hoeveelheid','N(soort,ev,basis,zijd,stan)').
lassy('hart','hart','N(soort,ev,basis,onz,stan)').
lassy('één','één','TW(hoofd,vrij)').
lassy('Chinese','Chinees','ADJ(prenom,basis,met-e,stan)').
lassy('bier','bier','N(soort,ev,basis,onz,stan)').
lassy('belangen','belang','N(soort,mv,basis)').
lassy('banken','bank','N(soort,mv,basis)').
lassy('aanwezigheid','aanwezigheid','N(soort,ev,basis,zijd,stan)').
lassy('voorlichting','voorlichting','N(soort,ev,basis,zijd,stan)').
lassy('verzoek','verzoek','N(soort,ev,basis,onz,stan)').
lassy('vertrouwen','vertrouwen','N(soort,ev,basis,onz,stan)').
lassy('veroorzaakt','veroorzaken','WW(vd,vrij,zonder)').
lassy('Ten','te','VZ(versm)').
lassy('Tegen','tegen','VZ(init)').
lassy('strategie','strategie','N(soort,ev,basis,zijd,stan)').
lassy('spel','spel','N(soort,ev,basis,onz,stan)').
lassy('Sovjet-Unie','Sovjet-Unie','N(eigen,ev,basis,zijd,stan)').
lassy('snelle','snel','ADJ(prenom,basis,met-e,stan)').
lassy('laat','laat','ADJ(vrij,basis,zonder)').
lassy('kort','kort','ADJ(nom,basis,zonder,zonder-n)').
lassy('indruk','indruk','N(soort,ev,basis,zijd,stan)').
lassy('gezag','gezag','N(soort,ev,basis,onz,stan)').
lassy('geschreven','schrijven','WW(vd,vrij,zonder)').
lassy('bevoegdheden','bevoegdheid','N(soort,mv,basis)').
lassy('21','21','TW(hoofd,vrij)').
lassy('voldoende','voldoende','ADJ(prenom,basis,zonder)').
lassy('verkeer','verkeer','N(soort,ev,basis,onz,stan)').
lassy('verboden','verbieden','WW(vd,vrij,zonder)').
lassy('vastgesteld','vast_stellen','WW(vd,vrij,zonder)').
lassy('regionale','regionaal','ADJ(prenom,basis,met-e,stan)').
lassy('principe','principe','N(soort,ev,basis,onz,stan)').
lassy('open','open','ADJ(prenom,basis,zonder)').
lassy('onderzocht','onderzoeken','WW(vd,vrij,zonder)').
lassy('onderneming','onderneming','N(soort,ev,basis,zijd,stan)').
lassy('momenteel','momenteel','ADJ(vrij,basis,zonder)').
lassy('materiaal','materiaal','N(soort,ev,basis,onz,stan)').
lassy('liggen','liggen','WW(inf,vrij,zonder)').
lassy('laten','laten','WW(pv,tgw,mv)').
lassy('jullie','jullie','VNW(pers,pron,stan,nadr,2v,mv)').
lassy('grenzen','grens','N(soort,mv,basis)').
lassy('God','God','N(eigen,ev,basis,zijd,stan)').
lassy('gelegenheid','gelegenheid','N(soort,ev,basis,zijd,stan)').
lassy('CDA','CDA','N(eigen,ev,basis,onz,stan)').
lassy('aanzien','aanzien','N(soort,ev,basis,onz,stan)').
lassy('woont','wonen','WW(pv,tgw,met-t)').
lassy('winnen','winnen','WW(inf,vrij,zonder)').
lassy('staatssecretaris','staatssecretaris','N(soort,ev,basis,zijd,stan)').
lassy('ontstaan','ontstaan','WW(vd,vrij,zonder)').
lassy('Maastricht','Maastricht','N(eigen,ev,basis,onz,stan)').
lassy('klaar','klaar','ADJ(vrij,basis,zonder)').
lassy('industrie','industrie','N(soort,ev,basis,zijd,stan)').
lassy('gewonnen','winnen','WW(vd,vrij,zonder)').
lassy('dacht','denken','WW(pv,verl,ev)').
lassy('bijzonder','bijzonder','ADJ(nom,basis,zonder,zonder-n)').
lassy('aanbod','aanbod','N(soort,ev,basis,onz,stan)').
lassy('Wolder','Wolder','N(eigen,ev,basis,onz,stan)').
lassy('voorlopig','voorlopig','ADJ(vrij,basis,zonder)').
lassy('voordeel','voordeel','N(soort,ev,basis,onz,stan)').
lassy('volgend','volgen','WW(od,prenom,zonder)').
lassy('vermoord','vermoorden','WW(vd,vrij,zonder)').
lassy('Utrecht','Utrecht','N(eigen,ev,basis,onz,stan)').
lassy('praten','praten','WW(inf,vrij,zonder)').
lassy('openbaar','openbaar','ADJ(prenom,basis,zonder)').
lassy('model','model','N(soort,ev,basis,onz,stan)').
lassy('jaarlijks','jaarlijks','ADJ(vrij,basis,zonder)').
lassy('ingezet','in_zetten','WW(vd,vrij,zonder)').
lassy('hoogste','hoog','ADJ(prenom,sup,met-e,stan)').
lassy('heel','heel','ADJ(prenom,basis,zonder)').
lassy('gewerkt','werken','WW(vd,vrij,zonder)').
lassy('gevoel','gevoel','N(soort,ev,basis,onz,stan)').
lassy('Gent','Gent','N(eigen,ev,basis,onz,stan)').
lassy('gehaald','halen','WW(vd,vrij,zonder)').
lassy('gedeelte','gedeelte','N(soort,ev,basis,onz,stan)').
lassy('ervaring','ervaring','N(soort,ev,basis,zijd,stan)').
lassy('Engelse','Engels','ADJ(prenom,basis,met-e,stan)').
lassy('dorp','dorp','N(soort,ev,basis,onz,stan)').
lassy('dieren','dier','N(soort,mv,basis)').
lassy('bloed','bloed','N(soort,ev,basis,onz,stan)').
lassy('Berlijn','Berlijn','N(eigen,ev,basis,onz,stan)').
lassy('afhankelijk','afhankelijk','ADJ(vrij,basis,zonder)').
lassy('24','24','TW(hoofd,vrij)').
lassy('verwacht','verwachten','WW(vd,vrij,zonder)').


%%%
%%% todo
%%% bw gebruikte adj moeten vaak BW zijn
%%% ten ter VZ(versm)
%%% conjunction van unary...
%%%% eerder, ander, ..
%%% onderscheid TW(hoofd,nom,zonder-n,basis) vs TW(hoofd,vrij)
%%%% je
%%% sg/pl de/het voor nouns die both hebben
%% mod_hd_adverb in misc assigns adv ipv adj
%% adverb wat??

particle_tag(erger,      erg,        'ADJ(vrij,comp,zonder)').
particle_tag(betaald,    betalen,    'WW(vd,vrij,zonder)').
particle_tag(gedaan,     doen,       'WW(vd,vrij,zonder)').
particle_tag(gelegen,    liggen,     'WW(vd,vrij,zonder)').
particle_tag(geroepen,   roepen,     'WW(vd,vrij,zonder)').
particle_tag(gevangen,   vangen,     'WW(vd,vrij,zonder)').
particle_tag(gewonnen,   winnen,     'WW(vd,vrij,zonder)').
particle_tag(ingedrukt,  in_drukken, 'WW(vd,vrij,zonder)').
particle_tag(geschreven, schrijven,  'WW(vd,vrij,zonder)').
particle_tag(staande,    staan,      'WW(od,vrij,zonder)').
particle_tag(vergezeld,  vergezellen,'WW(vd,vrij,zonder)').

particle_tag(StemIn,StemIn,Tag) :-
    particle_tag(StemIn,Tag).

particle_tag(aaneen,'BW()').
particle_tag(achterna,'VZ(fin)').
particle_tag(achterover,'BW()').
particle_tag(achteruit,'BW()').
particle_tag(achterwege,'BW()').
particle_tag(actie,'N(soort,ev,basis,zijd,stan)').
particle_tag(adem,'N(soort,ev,basis,zijd,stan)').
particle_tag(af_handig,'ADJ(vrij,basis,zonder)').
particle_tag(afhandig,'ADJ(vrij,basis,zonder)').
particle_tag(beet,'N(soort,ev,basis,zijd,stan)').
particle_tag(bekend,'ADJ(vrij,basis,zonder)').
particle_tag(beschikbaar,'ADJ(vrij,basis,zonder)').
particle_tag(beter,'ADJ(vrij,comp,zonder)').
particle_tag(bezig,'ADJ(vrij,basis,zonder)').
particle_tag(bijeen,'BW()').
particle_tag(binnenskamers,'ADJ(vrij,basis,zonder)').
particle_tag(blank,'ADJ(vrij,basis,zonder)').
particle_tag(blijk,'N(soort,ev,basis,onz,stan)').
particle_tag(blind,'ADJ(vrij,basis,zonder)').
particle_tag(bloot,'ADJ(vrij,basis,zonder)').
particle_tag(bol,'N(soort,ev,basis,zijd,stan)').
particle_tag(deel,'N(soort,ev,basis,onz,stan)').
particle_tag(dicht,'ADJ(vrij,basis,zonder)').
particle_tag(dienst,'N(soort,ev,basis,zijd,stan)').
particle_tag(dik,'ADJ(vrij,basis,zonder)').
particle_tag(dood,'ADJ(vrij,basis,zonder)').
particle_tag(deelgenoot,'N(soort,ev,basis,zijd,stan)').
particle_tag(duidelijk,'ADJ(vrij,basis,zonder)').
particle_tag(dwars,'ADJ(vrij,basis,zonder)').
particle_tag(erg,'ADJ(vrij,basis,zonder)').
particle_tag(eruit,'BW()').
particle_tag(feest,'N(soort,ev,basis,onz,stan)').
particle_tag(fout,'ADJ(vrij,basis,zonder)').
particle_tag(ga,'WW(inf,vrij,zonder)').
particle_tag(gehoor,'N(soort,ev,basis,onz,stan)').
particle_tag(gelijk,'ADJ(vrij,basis,zonder)').
particle_tag(gelukkig,'ADJ(vrij,basis,zonder)').
particle_tag(gemeen,'ADJ(vrij,basis,zonder)').
particle_tag(genoodzaakt,'WW(vd,vrij,zonder)').
particle_tag(gereed,'ADJ(vrij,basis,zonder)').
particle_tag(gerust,'ADJ(vrij,basis,zonder)').
particle_tag(gewaar,'ADJ(vrij,basis,zonder)').
particle_tag(glad,'ADJ(vrij,basis,zonder)').
particle_tag(goed,'ADJ(vrij,basis,zonder)').
particle_tag(goed,'ADJ(vrij,comp,zonder)').
particle_tag(heb,'WW(inf,vrij,zonder)').
particle_tag(hem,'VNW(pers,pron,obl,vol,3,ev,masc)').
particle_tag(hoog,'ADJ(vrij,basis,zonder)').
particle_tag(hoor,'WW(inf,vrij,zonder)').
particle_tag(ineen,'BW()').
particle_tag(kan,'WW(inf,vrij,zonder)').
particle_tag(kandidaat,'N(soort,ev,basis,zijd,stan)').
particle_tag(kapot,'ADJ(vrij,basis,zonder)').
particle_tag(kenbaar,'ADJ(vrij,basis,zonder)').
particle_tag(kennis,'N(soort,ev,basis,zijd,stan)').
particle_tag(klaar,'ADJ(vrij,basis,zonder)').
particle_tag(klem,'N(soort,ev,basis,zijd,stan)').
particle_tag(kwalijk,'ADJ(vrij,basis,zonder)').
particle_tag(kwijt,'ADJ(vrij,basis,zonder)').
particle_tag(lam,'ADJ(vrij,basis,zonder)').
particle_tag(lastig,'ADJ(vrij,basis,zonder)').
particle_tag(leeg,'ADJ(vrij,basis,zonder)').
particle_tag(lek,'ADJ(vrij,basis,zonder)').
particle_tag(les,'N(soort,ev,basis,zijd,stan)').
particle_tag(los,'ADJ(vrij,basis,zonder)').
particle_tag(meester,'N(soort,ev,basis,zijd,stan)').
particle_tag(mis,'ADJ(vrij,basis,zonder)').
particle_tag(moeilijk,'ADJ(vrij,basis,zonder)').
particle_tag(neer,'VZ(fin)').
particle_tag(omver,'BW()').
particle_tag(onbetuigd,'ADJ(vrij,basis,zonder)').
particle_tag(onderuit,'BW()').
particle_tag(onverlet,'ADJ(vrij,basis,zonder)').
particle_tag(open,'ADJ(vrij,basis,zonder)').
particle_tag(opzij,'BW()').
particle_tag(overeen,'BW()').
particle_tag(overhoop,'BW()').
particle_tag(plaats,'N(soort,ev,basis,zijd,stan)').
particle_tag(plat,'ADJ(vrij,basis,zonder)').
particle_tag(post,'N(soort,ev,basis,zijd,stan)').
particle_tag(prat,'ADJ(vrij,basis,zonder)').
particle_tag(prijs,'N(soort,ev,basis,zijd,stan)').
particle_tag(quitte,'ADJ(vrij,basis,zonder)').
particle_tag(raak,'ADJ(vrij,basis,zonder)').
particle_tag(recht,'ADJ(vrij,basis,zonder)').
particle_tag(rijp,'ADJ(vrij,basis,zonder)').
particle_tag(rood,'ADJ(vrij,basis,zonder)').
particle_tag(roodgloeiend,'ADJ(vrij,basis,zonder)').
particle_tag(samen,'BW()').
particle_tag(schemer_door,'WW(inf,vrij,zonder)').
particle_tag(schoon,'ADJ(vrij,basis,zonder)').
particle_tag(schrap,'BW()').
particle_tag(schuil,'BW()').
particle_tag(schuldig,'ADJ(vrij,basis,zonder)').
particle_tag(stand,'N(soort,ev,basis,zijd,stan)').
particle_tag(sterk,'ADJ(vrij,basis,zonder)').
particle_tag(stil,'ADJ(vrij,basis,zonder)').
particle_tag(stop,'N(soort,ev,basis,zijd,stan)').
particle_tag(stuk,'N(soort,ev,basis,onz,stan)').
particle_tag(tegemoet,'BW()').
particle_tag(tegoed,'ADJ(vrij,basis,zonder)').
particle_tag(tekeer,'BW()').
particle_tag(tekort,'N(soort,ev,basis,onz,stan)').
particle_tag(teleur,'BW()').
particle_tag(teniet,'BW()').
particle_tag(tentoon,'BW()').
particle_tag(terecht,'ADJ(vrij,basis,zonder)').
particle_tag(terug,'BW()').
particle_tag(terwille,'ADJ(vrij,basis,zonder)').
particle_tag(teweeg,'BW()').
particle_tag(tewerk,'BW()').
particle_tag(thee,'N(soort,ev,basis,zijd,stan)').
particle_tag(thuis,'BW()').
particle_tag(trouw,'ADJ(vrij,basis,zonder)').
particle_tag(uiteen,'BW()').
particle_tag(uiting,'N(soort,ev,basis,zijd,stan)'). 
particle_tag(vaar,'WW(inf,vrij,zonder)').
particle_tag(val,'WW(inf,vrij,zonder)').
particle_tag(vast,'ADJ(vrij,basis,zonder)').
particle_tag(veil,'ADJ(vrij,basis,zonder)').
particle_tag(veilig,'ADJ(vrij,basis,zonder)').
particle_tag(ver,'ADJ(vrij,basis,zonder)').
particle_tag(ver,'ADJ(vrij,comp,zonder)').
particle_tag(verder,'ADJ(vrij,comp,zonder)').
particle_tag(verzeild,'ADJ(vrij,basis,zonder)').
particle_tag(vij,'ADJ(vrij,basis,zonder)').
particle_tag(vlam,'N(soort,ev,basis,zijd,stan)').
particle_tag(voel,'WW(inf,vrij,zonder)').
particle_tag(vol,'ADJ(vrij,basis,zonder)').
particle_tag(vooraf,'BW()').
particle_tag(voorbij,'ADJ(vrij,basis,zonder)').
particle_tag(voorop,'BW()').
particle_tag(voort,'BW()').
particle_tag(vooruit,'BW()').
particle_tag(vorm,'N(soort,ev,basis,zijd,stan)').
particle_tag(vrij,'ADJ(vrij,basis,zonder)').
particle_tag(waar,'ADJ(vrij,basis,zonder)').
particle_tag(weer,'BW()').
particle_tag(weg,'BW()').
particle_tag(wel,'BW()').
particle_tag(welkom,'ADJ(vrij,basis,zonder)').
particle_tag(wijs,'ADJ(vrij,basis,zonder)').
particle_tag(zaken,'N(soort,mv,basis)').
particle_tag(zorg,'N(soort,ev,basis,zijd,stan)').
particle_tag(zuur,'ADJ(vrij,basis,zonder)').
particle_tag(zwaar,'ADJ(vrij,basis,zonder)').




/*

fout in CGN?:
"een manier van rondkomen"
WR-P-P-H-0000000001.p.2.s.2: rondkomen WW(inf,vrij,zonder)            WW(inf,nom,zonder,zonder-n)
van                      VZ(init)                                     VZ(fin) #PT# WR-P-P-H-0000000014.p.2.s.2

"De ploeg beëindigde het toernooi zelfs met negen man , nadat in de slotfase ook aanvoerder Heitinga na een overtreding ' rood ' zag ."
rood                     ADJ(nom,basis,zonder,zonder-n)               N(soort,ev,basis,onz,stan) #PT# WR-P-P-H-0000000012.p.12.s.3


"Niet door luide demonstraties , maar door vasten en bidden willen de religieuzen hun solidariteit met de armen laten zien ."
vasten                   WW(inf,vrij,zonder)                          WW(inf,nom,zonder,zonder-n) #PT# WR-P-P-H-0000000005.p.1.s.2
bidden                   WW(inf,vrij,zonder)                          WW(inf,nom,zonder,zonder-n) #PT# WR-P-P-H-0000000005.p.1.s.2

allerminst               ADJ(vrij,basis,zonder)                       ADJ(vrij,sup,zonder)

maar                     BW()                                         VG(neven) #PT# WR-P-P-H-0000000014.p.3.s.10


onduidelijk:

Q#WR-P-P-H-0000000019.p.5.s.2|Dusan - midden veertig , kaal , sjofel gekleed - wil het niet goedpraten .|1|1|3.6574679188999992
midden                   BW()                                         N(soort,ev,basis,onz,stan) #PT# WR-P-P-H-0000000019.p.5.s.2


gaan akkoord:    adj of noun?

zijn bestemd:    pv of inf?

gerimpelde:      ADJ(prenom,basis,met-e,stan)      WW(vd,prenom,met-e)
ondergewaardeerde
getinte

geboren          ADJ(prenom,basis,zonder)                     WW(vd,prenom,zonder)
aangeboren

voorgaande       ADJ(prenom,basis,met-e,stan)                 WW(od,prenom,met-e)

teruglopend      ADJ(prenom,basis,zonder)                     WW(od,prenom,zonder)

tekenend         ADJ(vrij,basis,zonder)                       WW(od,vrij,zonder)
overtuigend      ADJ(vrij,basis,zonder)                       WW(od,vrij,zonder)

beschaamd        ADJ(vrij,basis,zonder)                       WW(vd,vrij,zonder)

ABC:  N(eigen,ev,basis,onz,stan)                   N(eigen,ev,basis,genus,stan)

NOS-journaal     N(soort,ev,basis,onz,stan)                   N(eigen,ev,basis,onz,stan)
Volkscongres     N(soort,ev,basis,onz,stan)                   N(eigen,ev,basis,onz,stan)

AOT              N(eigen,ev,basis,onz,stan)                   N(eigen,ev,basis,zijd,stan)
Whalley          N(eigen,ev,basis,onz,stan)                   N(eigen,ev,basis,zijd,stan)
Oranje           N(eigen,ev,basis,onz,stan)                   N(eigen,ev,basis,zijd,stan)

AEX-index:       N(soort,ev,basis,zijd,stan)                  N(eigen,ev,basis,zijd,stan)
Top-tienspeler   N(soort,ev,basis,zijd,stan)                  N(eigen,ev,basis,zijd,stan)
Bijlmerramp      N(soort,ev,basis,zijd,stan)                  N(eigen,ev,basis,zijd,stan)

schudden         WW(pv,tgw,mv)                                WW(pv,verl,mv)

Vilamoura        N(eigen,ev,basis,zijd,stan)                  N(eigen,ev,basis,onz,stan)

Midkap-fondsen   N(eigen,mv,basis)                            N(soort,mv,basis)

gelden als:      VG(onder)                                    VZ(init)
werkte als:      VG(onder)                                    VZ(init)
een woonhuis als:VG(onder)                                    VZ(init)
ontrolt als      VG(onder)                                    VZ(init)

de WK zwemmen    WW(inf,vrij,zonder)                          WW(inf,nom,zonder,zonder-n)

de 50 vrij:
50               TW(hoofd,prenom,stan)                        N(soort,ev,basis,zijd,stan)
vrij             ADJ(vrij,basis,zonder)                       N(soort,ev,basis,zijd,stan)


systematisch:

multi-word-namen krijgen in Alpino altijd SPEC(deeleigen)
Nederlandse Juristen Vereniging
Amsterdam Option Traders
Zusters Missionarissen Consolata
High Noon in Jakarta
Nederland 3

data: namen van maanden worden N(eigen, in CGN

woorden zonder syntactische context:
gelijk           ADJ(vrij,basis,zonder)                       N(soort,ev,basis,onz,stan)



*/

lemma_map(Surf,Lemma) :-
    (    exc_lemma(Surf,Lemma1)
    ->   Lemma = Lemma1
    ;    Surf = Lemma
    ).

exc_lemma('onder meer','onder veel').  % voor 'o.m.'
exc_lemma(sprake,spraak).
exc_lemma(ten,te).
exc_lemma(ter,te).

exc_lemma('Beweging',beweging).
exc_lemma('College',college).
exc_lemma('Comité',comité).
exc_lemma('Commissie',commissie).
exc_lemma('Cultuur',cultuur).
exc_lemma('Europese','Europees').
exc_lemma('Financiën',financiën).
exc_lemma('Hof',hof).
exc_lemma('Hoogheid',hoogheid).
exc_lemma('Journaal',journaal).
exc_lemma('Kerk',kerk).
exc_lemma('Koning',koning).
exc_lemma('Koninklijk',koninklijk).
exc_lemma('Koninklijke',koninklijk).
exc_lemma('Museum',museum).
exc_lemma('Olympische','Olympisch').
exc_lemma('Stichting',stichting).
exc_lemma('Universiteit',universiteit).
exc_lemma('Verbond',verbond).
exc_lemma('Vereniging',vereniging).
exc_lemma('Volkskgezondheid',volksgezondheid).

frequent_tag(Atom,Stem,Result) :-
    findall(Tag/Label,alpino_lex:lexicon(Tag,Label,[Atom],[],_),Tags),
    (   ft(T),
	lists:member(T/L,Tags)
    ->  T = Result,
	(  L = v_root(Stem,_)
        ;  L = Stem
	)        
    ;   Tags = [Result/Stem|_]
    ).

frequent_copy_tag(Q0,Q,Stem,Surf,Result) :-
    findall(Tag/Label,alpino_lexical_analysis:copy_tag(_,_,Q0,Q,Label,Surf,_,Tag),[TagsH|TagsT]),
    (   ( ft(T) ; true ),
	lists:member(T/L,[TagsH|TagsT])
    ->  Result/Stem = T/L
    ;   TagsH = Result/Stem
    ).

ft(noun(_,_,_)).
ft(number(_)).
ft(adjective(_)). % prefer over verb (?)
ft(verb(_,_,_)).  % prefer over v_noun
ft(proper_name(_,_)).
ft(proper_name(_)).

%%% gcnd(Word) tussenwerpsels
%%% lijst van The spoken corpus of Southern-Dutch dialects
%%% van Anne-Sophie Ghyselen
%%% als deze worden geskipd, krijgen ze TSW() als postag
gcnd('ah').
gcnd('aha').
gcnd('awel').
gcnd('allez').
gcnd('ai').
gcnd('au').
gcnd('bah').
gcnd('boe').
gcnd('bwa').
gcnd('bè').
gcnd('dè').
gcnd('ei').
gcnd('eikes').
gcnd('eni').
gcnd('ewaar').
gcnd('goh').
gcnd('ha').
gcnd('haha').
gcnd('hé').
gcnd('hei').
gcnd('ho').
gcnd('hu').
gcnd('hum').
gcnd('jee').
gcnd('mm-hu').
gcnd('mmm').
gcnd('moh').
gcnd('neeë').
gcnd('nou').
gcnd('oeh').
gcnd('oei').
gcnd('oesje').
gcnd('o').
gcnd('oh').
gcnd('oho').
gcnd('poeh').
gcnd('pst').
gcnd(sjonge).
gcnd('sjt').
gcnd('sst').
gcnd('tut').
gcnd('uh').
gcnd('uhm').
gcnd('uhu').
gcnd('wauw').
gcnd('who').
gcnd('wi').
gcnd('zi').
gcnd('zé').
gcnd('zuh').
gcnd('zulle').
gcnd('zunne').
gcnd('zun').
gcnd('goddomme').
gcnd('verdikke').
gcnd('verdorie').
gcnd('godverdomme').
gcnd('goddikke').

eigen_noun('Holocaust').

symb_tags([],P,P) --> [].
symb_tags([Sym|Syms],P0,P) -->
    { P1 is P0 + 1 },
    [cgn_postag(P0,P1,Sym,'SPEC(symb)')],
    symb_tags(Syms,P1,P).

ignore_cap(Word,Word3,Tag,Tag2) :-
    ignore_cap(Word,Word2),
    ignore_tag(Tag,Word2,Word3,Tag2).

ignore_cap(Word,Word2) :-
    ignore_cap(Word),
    alpino_unknowns:decap_first(Word,Word2).

ignore_tag(_,Word,Word2,noun(DeHet,Count,Agr)) :-
    alpino_lex:lexicon(noun(DeHet,Count,Agr),Word2,[Word],[],_),
    !.
ignore_tag(proper_name(Agr,_),W,W,noun(both,both,Agr)).

ignore_cap('Begroting').
ignore_cap('Beweging').
ignore_cap('Bond').
ignore_cap('College').
ignore_cap('Comité').
ignore_cap('Congres').
ignore_cap('Cultuur').
ignore_cap('Defensie').
ignore_cap('Energie').
ignore_cap('Economie').
ignore_cap('Epilepsie').
ignore_cap('Financiën').
ignore_cap('Gemeenschap').
ignore_cap('Gezondheid').
ignore_cap('Hoogheid').
ignore_cap('Industrie').
ignore_cap('Innovatie').
ignore_cap('Justitie').
ignore_cap('Journaal').
ignore_cap('Kamer').
ignore_cap('Koning').
ignore_cap('Landbouw').
ignore_cap('Leefmilieu').
ignore_cap('Luchtmacht').
ignore_cap('Milieu').
ignore_cap('Ministerie').
ignore_cap('Mobiliteit').
ignore_cap('Museum').
ignore_cap('Onderwijs').
ignore_cap('Onderzoek').
ignore_cap('Ontwikkeling').
ignore_cap('Ontwikkelingssamenwerking').
ignore_cap('Overeenkomst').
ignore_cap('Pact').
ignore_cap('Raad').
ignore_cap('Republiek').
ignore_cap('Rijk').
ignore_cap('Senaat').
ignore_cap('Staat').
ignore_cap('Stichting').
ignore_cap('Toerisme').
ignore_cap('Transport').
ignore_cap('Unie').
ignore_cap('Universiteit').
ignore_cap('Verbond').
ignore_cap('Verdrag').
ignore_cap('Verkeer').
ignore_cap('Vereniging').
ignore_cap('Volksgezondheid').
ignore_cap('Volkshuisvesting').
ignore_cap('Welzijn').
ignore_cap('Wetenschap').

guess_vreemd(Surf,SurfEls) :-
    atom(Surf),
    alpino_util:split_atom(Surf," ",SurfEls),
    length(SurfEls,Total),
    findall(_,vreemd_member(SurfEls),Vreemd),
    length(Vreemd,TotalVreemd),
    TotalVreemd/Total > 0.5.

vreemd_member(L) :-
    lists:member(El,L),
    alpino_unknowns:decap_foreign_word(El),
    \+ not_really_vreemd(El).

not_really_vreemd('Alles').
not_really_vreemd('De').
not_really_vreemd('In').
not_really_vreemd('Man').
not_really_vreemd('Of').
not_really_vreemd(alles).
not_really_vreemd(der).
not_really_vreemd(de).
not_really_vreemd(in).
not_really_vreemd(man).
not_really_vreemd(of).
not_really_vreemd(over).

mwu_vreemd_tags([],Q,Q) --> [].
mwu_vreemd_tags([W|Words],Q0,Q) -->
     { Q1 is Q0 + 1 },
     mwu_vreemd_tag(W,Q0,Q1),
     mwu_vreemd_tags(Words,Q1,Q).

mwu_vreemd_tag(Punct,Q0,Q) -->
    { punct(Punct)},
    !,
    [cgn_postag(Q0,Q,Punct,'LET()')].
mwu_vreemd_tag(Punct,Q0,Q) -->
    [cgn_postag(Q0,Q,Punct,'SPEC(vreemd)')].

surf_map(Surf0,Surf,Q0,Q) :-
    Q is Q0 + 1,
    alpino_lexical_analysis:user_skips(List),
    lists:member(alt(Q0,Surf0,Surf),List).

gen_name_parts([],['N(eigen,ev,basis,gen)']).
gen_name_parts([_|T],['SPEC(deeleigen)'|PosList]) :-
    gen_name_parts(T,PosList).

is_a_number(Atom) :-
    alpino_lex:parse_number_simple(Atom).
is_a_number(Atom) :-
    atom(Atom),
    atom_codes(Atom,Codes),
    alpino_lex:roman_number(Codes).

number_tags(Q0,Q,Stem,_,Tag) -->
    {  Len is Q-Q0,
       atom(stem),
       alpino_util:split_atom(Stem," ",Stems),
       length(Stems,Len)
    },
    !,
    number_tags_(Stems,Q0,Q,Tag).

number_tags(Q0,Q,Stem,Surf,Tag) -->
    tags(Q0,Q,Stem,Surf,Tag).

number_tags_([],_,_,_) --> [].
number_tags_([Stem|Stems],Q0,Q,Tag) -->
    {  Q1 is Q0 + 1 },
    number_tag(Stem,Q0,Q1,Tag),
    number_tags_(Stems,Q1,Q,Tag).

number_tag(Stem,Q0,Q,_Tag) -->
    {  symb(Stem,_) },
    !,
    [cgn_postag(Q0,Q,Stem,'SPEC(symb)')].

number_tag(Stem,Q0,Q,_Tag) -->
    {  punct(Stem) },
    !,
    [cgn_postag(Q0,Q,Stem,'LET()')].

number_tag(Stem,Q0,Q,Tag) -->
    [cgn_postag(Q0,Q,Stem,Tag)].

amount_tags(Q0,Q,Stem) -->
    {  Len is Q-Q0,
       atom(stem),
       alpino_util:split_atom(Stem," ",Stems),
       length(Stems,Len)
    },
    !,
    amount_tags_(Stems,Q0,Q).

amount_tags(Q0,Q,Stem) -->
    tags(Q0,Q,Stem,Stem,'TW(hoofd,vrij)').

amount_tags_([],_,_) --> [].
amount_tags_([Amount,Euro],Q0,Q) -->
    {  Q1 is Q0 + 1,
       amount_noun_tag(Euro,EuroTag,EuroLemma),
       \+ amount_noun_tag(Amount,_,_),
       (   symb(Amount,_)
       ->  simple_number(Amount,_)
       ;   true
       ),
       \+ punct(Amount)
    },
    !,
    [cgn_postag(Q0,Q1,Amount,'TW(hoofd,prenom,stan)'),
     cgn_postag(Q1,Q, EuroLemma,  EuroTag)
    ].
amount_tags_([Stem|Stems],Q0,Q) -->
    {  Q1 is Q0 + 1 },
    amount_tag(Stem,Q0,Q1),
    amount_tags_(Stems,Q1,Q).

amount_tag(Stem,Q0,Q) -->
    {  simple_number(Stem,_) },
    !,
    [cgn_postag(Q0,Q,Stem,'TW(hoofd,vrij)')].

amount_tag(Stem,Q0,Q) -->
    {  symb(Stem,_) },
    !,
    [cgn_postag(Q0,Q,Stem,'SPEC(symb)')].

amount_tag(Stem0,Q0,Q) -->
    {  afk(Stem0,Stem,_) },
    !,
    [cgn_postag(Q0,Q,Stem,'SPEC(afk)')].

amount_tag(Stem,Q0,Q) -->
    {  punct(Stem) },
    !,
    [cgn_postag(Q0,Q,Stem,'LET()')].

amount_tag(Euro,Q0,Q) -->
    {  amount_noun_tag(Euro,EuroTag,EuroLemma)
    },
    !,
    [cgn_postag(Q0,Q,EuroLemma,EuroTag)].

amount_tag(Stem,Q0,Q) -->
    [cgn_postag(Q0,Q,Stem,'TW(hoofd,vrij)')].

amount_noun_tag(euro,'N(soort,ev,basis,zijd,stan)',euro).
amount_noun_tag('Euro','N(soort,ev,basis,zijd,stan)','Euro').
amount_noun_tag(miljard,'N(soort,ev,basis,onz,stan)',miljard).
amount_noun_tag(miljoen,'N(soort,ev,basis,onz,stan)',miljoen).
amount_noun_tag(Afk,'SPEC(afk)',miljoen) :-
    afk(Afk,miljoen,_).


symb('???',_).
symb('->',_).
symb('mg/dag',_).
symb(Atom,_) :-
    atom(Atom),
    atom_concat(_,'°',Atom).
symb(Atom,_) :-
    atom(Atom),
    atom_concat(_,'°C',Atom).
symb(Atom,_) :-
    atom(Atom),
    atom_concat(_,'%',Atom).
symb(Atom,_) :-
    atom(Atom),			% 20.000,-
    atom_concat(Number,',-',Atom),
    atom_codes(Number,NumberCodes),
    lists:last(NumberCodes,Digit),
    alpino_latin1:isdigit(Digit).
				
symb(',-',_).
symb('=',_).
symb('#',_).
symb('+',_).
symb('±',_).
symb('&',_).
symb('$',_).
symb('^',_).
symb('©',_).
symb(bis,_).
symb('BF',_).
symb('BEF',_).
symb('CA',_).
symb(cm,_).
symb('cm.',_).
symb('CO2',_).
symb('co2',_).
symb('DLS',_).
symb('DM',_).
symb('§',_).
symb('EUR',_).
symb('€',_).
symb(flo,_).
symb(fr,_).
symb(f,_).
symb(fl,_).
symb('fl.',_).
symb('ƒ',_).
symb(o,tag).
symb('ha.', noun(de,count,meas)).
symb(ha, noun(de,count,meas)).
symb('¤',_).
symb(kg,_).
symb('km²',_).
symb(km,_).
symb('km.',_).
symb(km/u,_).
symb(km2,_).
symb(m,_).
symb(mg,_).
symb('mg.',_).
symb('µg',_).
symb(ml,_).
symb('ml.',_).
symb(mm,_).
symb('mm.',_).
symb(tl,_).
symb('Trb.',_).
symb('Trb',_).
symb(v,_).
symb('×',_).
symb(x,_).

symb(Stem,_) :-
    atom(Stem),
    atom_codes(Stem,[N1,X,N2]),
    lists:member(X,[45,46,47]),
    N1 > 46, N1 < 58,
    N2 > 46, N2 < 58.

real_e(achterste).
real_e(binnenste).
real_e(bovenste).
real_e(buitenste).
real_e(middelste).
real_e(onderste).
real_e(voorste).
real_e(Stem) :-
    atom(Stem),
    \+ atom_concat(_,e,Stem).

meer_lemma(meer,veel).
meer_lemma(minder,weinig).

phone(Atom) :-
    atom_codes(Atom,[48|Tail]),
    alpino_lex:number_codes_silent(Num,Tail),
    integer(Num).

/*
phone('010').
phone('020').
phone('030').
phone('06').
phone('0800').
*/

simple_number(Num,Val) :-
    alpino_lex:simple_convert_number(Num,Val).
simple_number(Num,Val) :-
    atom(Num),
    atom_codes(Num,String),
    alpino_lex:number_codes_silent(Val,String).

deeleigen([],[]).
deeleigen([_|T],['SPEC(deeleigen)'|P]):-
    deeleigen(T,P).