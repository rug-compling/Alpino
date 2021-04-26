:- module(alpino_cgn_postags, [ cgn_postag/9 ]).

cgn_postag(Frame0,Stem,Surf,Q0,Q,Result,His,L0,L) :-
    (   frame_map(Frame0,Stem,Frame)
    ->  true
    ;   Frame0 = Frame
    ),
    cgn_postag_c(Frame,Stem,Surf,Q0,Q,Result,His,L0,L).

cgn_postag_c(Frame,Stem,Surf,Q0,Q,Result,_His) -->
    {   Q > Q0 + 1   },
    mwu_postag(Frame,Stem,Surf,Q0,Q,Result),
    !.

cgn_postag_c(with_dt(_,Tree),_Stem,_Surf,Q0,_,_,_) -->
    !,
    with_dt_tags(Tree,Q0).

cgn_postag_c(Frame,Stem,Surf,Q0,Q,_Cat,_) -->
    {  exceptional_word_tag(Surf,Frame,Tag) },
    !,
    tags(Q0,Q,Stem,Tag).

cgn_postag_c(Frame,Stem0,Surf,Q0,Q,_Cat,_) -->
    {  exceptional_word_tag(Surf,Stem0,Stem,Frame,Tag) },
    !,
    tags(Q0,Q,Stem,Tag).

cgn_postag_c(Frame,Stem,Surf,Q0,Q,Result,His) -->
    history_tags(His,Q0,Q,Stem,Surf,Frame,Result),
    !.

cgn_postag_c(Frame,Stem,_,Q0,Q,_Cat,_) -->
    {  exceptional_stem_tag(Stem,Frame,Tag,Lemma) },
    !,
    tags(Q0,Q,Lemma,Tag).

cgn_postag_c(Frame,Stem,_,Q0,Q,_Cat,_) -->
    {  exceptional_stem_tag(Stem,Frame,Tag) },
    !,
    tags(Q0,Q,Stem,Tag).

cgn_postag_c(particle(_),Stem,_,Q0,Q,_,_) -->
    {  particle_tag(Stem,Tag) },
    !,
    tags(Q0,Q,Stem,Tag).

cgn_postag_c(robust_skip,Surf,Word,Q0,Q,_Cat,_) -->
    {  lassy(Word,Tag) },
    !,
    tags(Q0,Q,guess(Surf),Tag).

cgn_postag_c(robust_skip,_Stem,Word,Q0,Q,Cat,_) -->
    {  alpino_lexical_analysis:tag(_,_,Q0,Q,Stem,Word,His,Frame) },
    !,  % last resort
    cgn_postag(Frame,Stem,Word,Q0,Q,Cat,His).

cgn_postag_c(Frame,Stem,_,Q0,Q,_Cat,_) -->
    {  stem_dependent_tag(Frame,Stem,Tag) },
    !,
    tags(Q0,Q,Stem,Tag).

cgn_postag_c(Frame,Stem0,_,Q0,Q,_Cat,_) -->
    {  stem_dependent_tag(Frame,Stem0,Stem,Tag) },
    !,
    tags(Q0,Q,Stem,Tag).

cgn_postag_c(Frame,Stem,_,Q0,Q,Cat,_) -->
    {  context_dependent_tag_lemma(Frame,Tag,Stem,Stem1,Q0,Q,Cat) },
    !,
    tags(Q0,Q,Stem1,Tag).

cgn_postag_c(Frame,Stem,_,Q0,Q,Cat,_) -->
    {  context_dependent_tag(Frame,Tag,Stem,Q0,Q,Cat) },
    !,
    tags(Q0,Q,Stem,Tag).

cgn_postag_c(amount_meas_mod_noun(A,B,C),Stem,_,Q0,Q,_,_) -->
    {  atom(Stem),
       alpino_util:split_atom(Stem," ",Stems),
       length(Stems,Len),
       Q is Len + Q0
    },
    !,
    guess_tags(Q0,Q,amount_meas_mod_noun(A,B,C),Stems).

cgn_postag_c(Frame,Stem,_,Q0,Q,_Cat,_) -->
    {  cgn_postag_c(Frame,Tag) },
    !,
    tags(Q0,Q,Stem,Tag).

cgn_postag_c(robust_skip,Stem,_,Q0,Q,_,_) -->
    !,
    guess_tags(Q0,Q,robust_skip,Stem).

cgn_postag_c(skip,Stem,_,Q0,Q,_,_) -->
    !,
    guess_tags(Q0,Q,skip,Stem).

cgn_postag_c(fixed_part(P),Stem,_,Q0,Q,_,_) -->
    !,
    guess_tags(Q0,Q,fixed_part(P),Stem).

cgn_postag_c(Frame,Stem,Surf,Q0,Q,_,_) -->
    {  format(user_error,"error: no cgn tag for ~w ~w ~w~n",[Surf,Stem,Frame]) },
    guess_tags(Q0,Q,Frame,Stem).

add_tags([],Q,Q,_Tag) --> [].
add_tags([Stem|Stems],Q0,Q,Tag) -->
    [cgn_postag(Q0,Q1,Stem,Tag)],
    {  Q1 is Q0 + 1 },
    add_tags(Stems,Q1,Q,Tag).

tags(Q0,Q,Stem,Tag,L0,L):-
    hdrug_util:hdrug_flag(add_nodes_for_mwu,On),
    tags(On,Q0,Q,Stem,Tag,L0,L).

tags(off,Q0,Q,Stem0,Tag,[cgn_postag(Q0,Q,Stem,Tag)|L],L) :-
    guess_lemma(Stem0,Stem).

tags(on,Q0,Q,Stem0,Tag,L0,L) :-
    guess_lemma(Stem0,Stem),
    (   Q is Q0 + 1
    ->  L0 = [cgn_postag(Q0,Q,Stem,Tag)|L]
    ;   stags(Q0,Q,Stem,Tag,1,L0,L)
    ).

stags(Q0,Q,Stem,Tag,Pos,L0,L) :-
    (   Q =< Q0
    ->  L0 = L
    ;   Q1 is Q0 + 1,
	m_tag(Q1,Q,Tag,TagN),
	L0 = [cgn_postag(Q0,Q1,Pos/Stem,TagN)|L1],
	Pos1 is Pos + 1,
	stags(Q1,Q,Stem,Tag,Pos1,L1,L)
    ).

m_tag(Q0,Q,Tag0,Tag) :-
    (    Q > Q0
    ->   (   not_last_m_tag(Tag0,Tag)
	 ->  true
	 ;   Tag0 = Tag
	 )
    ;    Tag0 = Tag
    ).

not_last_m_tag('N(eigen,ev,basis,gen)','SPEC(deeleigen)').
not_last_m_tag('N(eigen,ev,dim,gen)',  'SPEC(deeleigen)').

history_tags(normal(decap(X)),Q0,Q,Stem,Surf,Frame,Result) -->
    history_tags(normal(X),Q0,Q,Stem,Surf,Frame,Result).

history_tags(normal('op zijn Belgisch'(normal)),Q0,Q,_,Surf,pp,_) -->
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


history_tags(normal('in mijn eentje'),Q0,Q,_,Surf,pp,_) -->
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


history_tags(normal(ten_xste),Q0,Q,Stem,_Surf,_Frame,_Result) -->
    { Q1 is Q0 + 1,
      atom(Stem),
      alpino_util:split_atom(Stem," ",[ten,Tweede]),
      alpino_lex:lexicon(number(rang),Twee,[Tweede],[],_)
    },
    [ cgn_postag(Q0,Q1,te,'VZ(versm)')],
    [ cgn_postag(Q1,Q,Twee,'TW(rang,nom,zonder-n)')].

history_tags(double_compound,Q0,Q,Stem,_Surf,Frame,Result) -->
    { 2 is Q-Q0,
      starts_with_capital(Stem),
      Q1 is Q0 + 1,
      atom(Stem),
      alpino_util:split_atom(Stem," ",[Stem1,Stem2])
    },
    [ cgn_postag(Q0,Q1,Stem1,'SPEC(deeleigen)')],
    cgn_postag_c(Frame,Stem2,Stem2,Q1,Q,Result,no).
    
history_tags(normal(abbreviation(normal)),Q0,Q,Stem,'\'t',determiner(het,nwh,nmod,pro,nparg,wkpro),_) -->
    { 1 is Q-Q0 }, 
    [ cgn_postag(Q0,Q,Stem,'LID(bep,stan,evon)') ].

history_tags(normal(abbreviation(normal)),Q0,Q,Stem,_,_,_) -->
    { 1 is Q-Q0,
      guess_lemma(Stem,Stem1)}, 
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
    [ cgn_postag(Q1,Q2,Stem2,'LET()')],
    cgn_postag_c(Tag,Stem3,Stem3,Q2,Q ,Result,His2).
    

history_tags(normal(chess),Q0,Q,Stem,_,_,_) -->
    { 1 is Q-Q0 },
    [ cgn_postag(Q0,Q,Stem,'SPEC(symb)') ].

history_tags(part_verb_conjunct,Q0,Q,Stem,_,_,_) -->
    { 1 is Q-Q0 }, 
    [ cgn_postag(Q0,Q,Stem,'SPEC(afgebr)') ].

history_tags(normal(url),Q0,Q,Stem,_,_,_) -->
    { 1 is Q-Q0 }, 
    [ cgn_postag(Q0,Q,Stem,'SPEC(symb)') ].

history_tags(quoted_name(_,_),Q0,Q,Stem,_,_,_) -->
    {  atom(Stem),
       atom_codes(Stem,Codes),
       alpino_util:codes_to_words(Codes,Words),
       length(Words,Len),
       Len is Q - Q0
    },
    guess_tag_list(Words,Q0,Q).

history_tags(normal(enumeration),Q0,Q,Stem,_,_,_) -->
    {  atom(Stem),
       atom_codes(Stem,Codes),
       alpino_util:codes_to_words(Codes,Words),
       length(Words,Len),
       Len is Q - Q0
    },
    guess_tag_list(Words,Q0,Q).

history_tags(normal(variant(variant21(_Lemma,L1,L2),_His)),P0,P,_Stem,_,Frame,_Result) -->
    { cgn_postag_c(Frame,Tag) },
    add_tags([L1,L2],P0,P,Tag).

history_tags(normal(variant(variant31(_Lemma,L1,L2,L3),_His)),P0,P,_Stem,_,Frame,_Result) -->
    { cgn_postag_c(Frame,Tag) },
    add_tags([L1,L2,L3],P0,P,Tag).

history_tags(english_compound(normal),P0,P,Stem,_Surf,Frame,_Result) -->
    { atom(Stem),
      alpino_util:split_atom(Stem,"_",Stems),
      cgn_postag_c(Frame,Tag)
    },
    add_tags(Stems,P0,P,Tag).

history_tags(normal(x_voor_x),P0,P,Stem,_,_,_) -->
    { atom(Stem),
      alpino_util:split_atom(Stem," ",[S,Voor,S]),
      guess_lex(S,Tag),
      P1 is P0 + 1,
      P2 is P1 + 1,
      P  is P2 + 1
    },
    [cgn_postag(P0,P1,S,Tag),
     cgn_postag(P1,P2,Voor,'VZ(init)'),
     cgn_postag(P2,P, S,Tag)
    ].

guess_tag_list([],Q,Q) --> [].
guess_tag_list([H|T],Q0,Q) -->
    guess_tag(H,H,Q0,Q1),
    guess_tag_list(T,Q1,Q).

guess_tags(Q0,Q,Frame,Stem1,L0,L) :-
    (   Q is Q0 + 1, Stem1=[Stem]
    ->  guess_tag(Stem,Frame,Q0,Q,L0,L)
    ;   Q is Q0 + 1, atom(Stem1)
    ->  guess_tag(Stem1,Frame,Q0,Q,L0,L)
    ;   guess_stags(Q0,Q,Frame,Stem1,L0,L)
    ).

guess_tag(Stem,skip,Q0,Q) -->
    { alpino_lexical_analysis:hesitation(Stem),
      Q is Q0 + 1
    },
    !,
    [cgn_postag(Q0,Q,Stem,'TSW()')].

guess_tag(Stem0,_,Q0,Q) -->
    [cgn_postag(Q0,Q,Stem,Tag)],
    { Q is Q0 + 1,
      lassy(Stem0,Tag),
      guess_lemma(Stem0,Stem)},
    !.
guess_tag(Stem,_,Q0,Q) -->
    [cgn_postag(Q0,Q,Stem,Tag)],
    { Q is Q0 + 1,
      guess_lex(Stem,Tag) },
    !.
guess_tag(Stem,_,Q0,Q) -->
    { Q is Q0 + 1 },
    [cgn_postag(Q0,Q,Stem,'NA()')].

guess_stags(Q0,Q,Tag,St,L0,L) :-
    (   Q > Q0
    ->  Q1 is Q0 + 1,
	(   Tag=fixed_part([H|T])
	->  guess_tag(H,H,Q0,Q1,L0,L1),
	    guess_stags(Q1,Q,fixed_part(T),_Stems,L1,L)
	;   St = [Stem|Stems],
	    guess_lex(Stem,POSTAG)
	->  L0 = [cgn_postag(Q0,Q1,Stem,POSTAG)|L1],
	    guess_stags(Q1,Q,Tag,Stems,L1,L)
	;   St = [Stem|Stems]
	->  L0 = [cgn_postag(Q0,Q1,Stem,'NA()')|L1],
	    guess_stags(Q1,Q,Tag,Stems,L1,L)
	;   L0 = [cgn_postag(Q0,Q1,St,'NA()')|L1],    % if St is not a list
	    guess_stags(Q1,Q,Tag,St,L1,L)
	)
    ;   L0 = L
    ).

guess_lex(Stem,Tag) :-
    lassy(Stem,Tag),!.
guess_lex(Stem,Tag) :-
    frequent_tag(Stem,Frame0),
    (   frame_map(Frame0,Stem,Frame)
    ->  true
    ;   Frame0 = Frame
    ),
    cgn_postag_c(Stem,Frame,Tag),!.
guess_lex(Stem1,Tag) :-
    alpino_unknowns:decap_first(Stem1,Stem),
    frequent_tag(Stem,Frame0),
    (   frame_map(Frame0,Stem,Frame)
    ->  true
    ;   Frame0 = Frame
    ),
    cgn_postag_c(Stem1,Frame,Tag),!.
guess_lex(S,'N(eigen,ev,basis,zijd,stan)'):-
    starts_with_capital(S),!.
guess_lex(_,'N(soort,ev,basis,zijd,stan)').

context_dependent_tag_lemma(adjective(er(adv)),'BW()',lief,liever,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    adv_path(Path).

context_dependent_tag_lemma(adjective(meer),PosTag,Stem0,Stem,Q0,Q,Result) :-
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

context_dependent_tag_lemma(adjective(ste),Tag,veel,veel,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    (   nattr(Node)
    ->  Tag = 'VNW(onbep,grad,stan,vrij,zonder,sup)'
    ;   find_path(Q0,Q,Result,Path),
	(   noun_path(Path)
	->  Tag = 'VNW(onbep,grad,stan,nom,met-e,zonder-n,sup)'
	;   Tag = 'VNW(onbep,grad,stan,prenom,met-e,agr,sup)'
	)
    ).

context_dependent_tag_lemma(determiner(pron,rwh),Postag,wier,wie,Q0,Q,Result) :-
    !,
    find_node(Q0,Q,Result,Node),
    (   wh_relagr_pl(Node)
    ->  Postag = 'VNW(betr,pron,gen,vol,3o,mv)'
    ;   Postag = 'VNW(betr,pron,gen,vol,3o,ev)'
    ).

context_dependent_tag_lemma(proper_name(_),Postag,Stem0,Stem,Q0,Q,Result) :-
    !,
    cgn_postag_proper(Postag,Stem0,Stem,Q0,Q,Result,'MISC').

context_dependent_tag_lemma(proper_name(_,SUB),Postag,Stem0,Stem,Q0,Q,Result) :-
    !,
    cgn_postag_proper(Postag,Stem0,Stem,Q0,Q,Result,SUB).

context_dependent_tag_lemma(adjective(ge_e),Tag,Stem0,Stem,_Q0,_Q,_Result) :-
    vd_is_adj(Stem0,Stem),
    !,
    Tag = 'ADJ(prenom,basis,met-e,stan)'.
context_dependent_tag_lemma(adjective(ge_no_e(_)),Tag,Stem0,Stem,Q0,Q,Result) :-
    vd_is_adj(Stem0,Stem),
    find_node(Q0,Q,Result,Node),
    nattr(Node),
    !,
    Tag = 'ADJ(vrij,basis,zonder)'.
context_dependent_tag_lemma(adjective(ge_no_e(_)),Tag,Stem0,Stem,_Q0,_Q,_Result) :-
    vd_is_adj(Stem0,Stem),
    !,
    Tag = 'ADJ(prenom,basis,zonder)'.
context_dependent_tag_lemma(adjective(ge_both(_)),Tag,Stem0,Stem,Q0,Q,Result) :-
    vd_is_adj(Stem0,Stem),
    find_node(Q0,Q,Result,Node),
    nattr(Node),
    !,
    Tag = 'ADJ(vrij,basis,zonder)'.
context_dependent_tag_lemma(adjective(ge_both(_)),Tag,Stem0,Stem,_Q0,_Q,_Result) :-
    vd_is_adj(Stem0,Stem),
    !,
    Tag = 'ADJ(prenom,basis,zonder)'.

context_dependent_tag_lemma(adjective(ende(_)),Tag,v_root(_,Verschil),Stem,_Q0,_Q,_Result) :-
    od_is_adj(Verschil,Stem),
    !,
    Tag = 'ADJ(prenom,basis,met-e,stan)'.
context_dependent_tag_lemma(adjective(end(_)),Tag,v_root(_,Verschil),Stem,Q0,Q,Result) :-
    od_is_adj(Verschil,Stem),
    find_node(Q0,Q,Result,Node),
    nattr(Node),
    !,
    Tag = 'ADJ(vrij,basis,zonder)'.
context_dependent_tag_lemma(adjective(end(_)),Tag,v_root(_,Verschil),Stem,_Q0,_Q,_Result) :-
    od_is_adj(Verschil,Stem),
    !,
    Tag = 'ADJ(prenom,basis,zonder)'.

context_dependent_tag_lemma(adjective(ende(_)),Tag,Verschil,Stem,_Q0,_Q,_Result) :-
    od_is_adj(Verschil,Stem),
    !,
    Tag = 'ADJ(prenom,basis,met-e,stan)'.
context_dependent_tag_lemma(adjective(end(_)),Tag,Verschil,Stem,Q0,Q,Result) :-
    od_is_adj(Verschil,Stem),
    find_node(Q0,Q,Result,Node),
    nattr(Node),
    !,
    Tag = 'ADJ(vrij,basis,zonder)'.
context_dependent_tag_lemma(adjective(end(_)),Tag,Stem,Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node),
    !,
    Tag = 'WW(od,vrij,zonder)'.
context_dependent_tag_lemma(adjective(end(_)),Tag,Verschil,Stem,_Q0,_Q,_Result) :-
    od_is_adj(Verschil,Stem),
    !,
    Tag = 'ADJ(prenom,basis,zonder)'.

context_dependent_tag_lemma(adjective(ende(_)),'WW(od,vrij,zonder)',Stem,Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).

context_dependent_tag_lemma(np_me_adjective(no_e(_)),'WW(od,vrij,zonder)',duren,duren,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node),!.
context_dependent_tag_lemma(np_me_adjective(no_e(_)),'WW(od,prenom,zonder)',duren,duren,_Q0,_Q,_Result).
context_dependent_tag_lemma(np_me_adjective(e),'WW(od,prenom,met-e)',duren,duren,_Q0,_Q,_Result).

meer_lemma(meer,veel).
meer_lemma(minder,weinig).

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

context_dependent_tag(adjective(no_e(odet_adv)),PosTag,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   adv_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   det_pron(Stem,PosTag,_)
    ).

context_dependent_tag(wh_adjective(odet_adv),PosTag,Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    (   pronoun_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   adv_path(Path)
    ->  det_pron(Stem,_,PosTag)
    ;   det_pron(Stem,PosTag,_)
    ).

context_dependent_tag(adjective(ge_no_e(_)),'WW(vd,vrij,zonder)',_Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).

context_dependent_tag(adjective(ge_both(_)),'WW(vd,vrij,zonder)',_Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).

context_dependent_tag(adjective(no_e(_)),'ADJ(vrij,basis,zonder)',_Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).

context_dependent_tag(adjective(er(_)),'ADJ(vrij,comp,zonder)',_Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).

context_dependent_tag(adjective(both(Sub)),'ADJ(vrij,basis,zonder)',_Stem,Q0,Q,Result) :-
    Sub \= osentadv,
    find_node(Q0,Q,Result,Node),
    nattr(Node).

context_dependent_tag(adjective(st(_)),'ADJ(vrij,sup,zonder)',_Stem,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).

context_dependent_tag(adjective(e),'VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)',weinig,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).
context_dependent_tag(adjective(e),'VNW(onbep,grad,stan,prenom,met-e,agr,basis)',weinig,_Q0,_Q,_Result).

context_dependent_tag(adjective(ste),'VNW(onbep,grad,stan,nom,met-e,zonder-n,sup)',weinig,Q0,Q,Result) :-
    find_node(Q0,Q,Result,Node),
    nattr(Node).
context_dependent_tag(adjective(ste),'VNW(onbep,grad,stan,prenom,met-e,agr,sup)',weinig,_Q0,_Q,_Result).

context_dependent_tag(adj_number(enkele),'VNW(onbep,det,stan,nom,met-e,zonder-n)',_,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    number_nom_path(Path).

context_dependent_tag(adj_number(pl_num),'VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)',_,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    number_vrij_path(Path).

context_dependent_tag(adjective(e),'VNW(onbep,det,stan,nom,met-e,zonder-n)',ene,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).
context_dependent_tag(adjective(e),'VNW(onbep,det,stan,prenom,met-e,evz)',ene,_Q0,_Q,_Result).

context_dependent_tag(adjective(e),'ADJ(nom,basis,met-e,zonder-n,stan)',_Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).

context_dependent_tag(adjective(ere),'ADJ(nom,comp,met-e,zonder-n,stan)',_Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).

context_dependent_tag(adjective(ge_e),'WW(vd,nom,met-e,zonder-n)',_Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).

context_dependent_tag(adjective(ende(_)),'WW(od,nom,met-e,zonder-n)',_Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).

context_dependent_tag(adjective(ste),'ADJ(nom,sup,met-e,zonder-n,stan)',_Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).

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

context_dependent_tag(adjective(stof),'VNW(onbep,det,stan,nom,met-e,zonder-n)',enig,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).
context_dependent_tag(adjective(stof),'VNW(onbep,det,stan,prenom,met-e,rest)',enig,_,_,_).

context_dependent_tag(adjective(stof),'ADJ(nom,basis,met-e,zonder-n,stan)',_Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    noun_path(Path).

context_dependent_tag(number(hoofd(_)),'SPEC(symb)',Stem,_,_,_) :-
    atom_codes(Stem,[N1,45,N2]),
    N1 > 47, N2 < 58,
    N2 > 47, N2 < 58.

context_dependent_tag(number(hoofd(_)),'TW(hoofd,nom,zonder-n,basis)',_Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    number_nom_path(Path).

context_dependent_tag(number(hoofd(_)),'TW(hoofd,vrij)',_Stem,Q0,Q,Result) :-
    find_path(Q0,Q,Result,Path),
    number_vrij_path(Path).

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
    ->  find_dehet(Node,DeHetVal,Stem)
    ;   DeHetVal = DeHet
    ),
    (   SgPl == both
    ->  find_sgpl(Node,SgPlVal)
    ;   SgPlVal = SgPl
    ),
    (   is_name(Stem)
    ->  name_postag(DeHetVal,SgPlVal,'MISC',Stem,Tag,Q0,Q)
    ;   noun_postag(DeHetVal,SgPlVal,Tag)
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
    atom_codes(W,Codes),
    alpino_util:split_string(Codes,"_",Words0),
    lists:last(Words0,Last),
    atom_codes(L,Last),
    is_simple_name(L),
    \+ post_h(Words0).

post_h([A,_]) :-
    atom_codes(Regering,A),
    post_h_word(Regering).

post_h_word(bewind).
post_h_word(commissie).
post_h_word(kabinet).
post_h_word(regering).
post_h_word(zaak).

is_simple_name(W) :-
    atom(W),
    atom_codes(W,[F,G|_T]),
    alpino_latin1:isupper(F),
    alpino_latin1:islower(G).

exceptional_stem_tag(Var,_,_,_) :-
    var(Var),
    !,
    fail.
exceptional_stem_tag(meest,nominalized_adjective,                   'VNW(onbep,grad,stan,nom,met-e,mv-n,sup)',veel).
exceptional_stem_tag(meest,nominalized_super_adjective,             'VNW(onbep,grad,stan,nom,met-e,mv-n,sup)',veel).
exceptional_stem_tag(veel,adjective(st(_)),                         'VNW(onbep,grad,stan,vrij,zonder,sup)',   veel).    % meest
exceptional_stem_tag(weinig,adjective(st(_)),                       'VNW(onbep,grad,stan,vrij,zonder,sup)',   weinig).  % minst
exceptional_stem_tag('z\'n',determiner(pron),                       'VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)',zijn).

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

exceptional_stem_tag(twee_DIM,noun(het,count,pl),  'TW(hoofd,nom,mv-n,dim)',   twee).
exceptional_stem_tag(drie_DIM,noun(het,count,pl),  'TW(hoofd,nom,mv-n,dim)',   drie).
exceptional_stem_tag(vier_DIM,noun(het,count,pl),  'TW(hoofd,nom,mv-n,dim)',   vier).
exceptional_stem_tag(vijf_DIM,noun(het,count,pl),  'TW(hoofd,nom,mv-n,dim)',   vijf).
exceptional_stem_tag(zes_DIM, noun(het,count,pl),  'TW(hoofd,nom,mv-n,dim)',   zes).

exceptional_stem_tag('College',_,        'N(soort,ev,basis,onz,stan)', college).
exceptional_stem_tag('Comité',_,         'N(soort,ev,basis,onz,stan)', comité).
exceptional_stem_tag('Congres',_,        'N(soort,ev,basis,onz,stan)', congres).
exceptional_stem_tag('Cultuur',_,        'N(soort,ev,basis,zijd,stan)',cultuur).
exceptional_stem_tag('Defensie',_,       'N(soort,ev,basis,zijd,stan)',defensie).
exceptional_stem_tag('Epilepsie',_,      'N(soort,ev,basis,zijd,stan)',epilepsie).
exceptional_stem_tag('Financiën',_,      'N(soort,mv,basis)',          financiën).
exceptional_stem_tag('Gemeenschap',_,    'N(soort,ev,basis,zijd,stan)',gemeenschap).
exceptional_stem_tag('Hoogheid',_,       'N(soort,ev,basis,zijd,stan)',hoogheid).
exceptional_stem_tag('Justitie',_,       'N(soort,ev,basis,zijd,stan)',justitie).
exceptional_stem_tag('Journaal',_,       'N(soort,ev,basis,onz,stan)', journaal).
exceptional_stem_tag('Kamer',_,          'N(soort,ev,basis,zijd,stan)',kamer).
exceptional_stem_tag('Koning',_,         'N(soort,ev,basis,zijd,stan)',koning).
exceptional_stem_tag('Landbouw',_,       'N(soort,ev,basis,zijd,stan)',landbouw).
exceptional_stem_tag('Leefmilieu',_,     'N(soort,ev,basis,onz,stan)', leefmilieu).
exceptional_stem_tag('Nederlanden',_,    'N(eigen,mv,basis)',          'Nederland').
exceptional_stem_tag('Onderwijs',_,      'N(soort,ev,basis,onz,stan)', onderwijs).
exceptional_stem_tag('Ontwikkeling',_,   'N(soort,ev,basis,zijd,stan)',ontwikkeling).
exceptional_stem_tag('Ontwikkelingssamenwerking',_,
		                         'N(soort,ev,basis,zijd,stan)', ontwikkeling_samenwerking).
exceptional_stem_tag('Raad',_,           'N(soort,ev,basis,zijd,stan)',raad).
exceptional_stem_tag('Rijk',_,           'N(soort,ev,basis,onz,stan)', rijk).
exceptional_stem_tag('Senaat',_,         'N(soort,ev,basis,zijd,stan)',senaat).
exceptional_stem_tag('Staat',_,          'N(soort,ev,basis,zijd,stan)',staat).
exceptional_stem_tag('Stichting',_,      'N(soort,ev,basis,zijd,stan)',stichting).
exceptional_stem_tag('Unie',_,           'N(soort,ev,basis,zijd,stan)',unie).
exceptional_stem_tag('Verbond',_,        'N(soort,ev,basis,onz,stan)', verbond).
exceptional_stem_tag('Verdrag',_,        'N(soort,ev,basis,onz,stan)', verdrag).
exceptional_stem_tag('Vereniging',_,     'N(soort,ev,basis,zijd,stan)',vereniging).
exceptional_stem_tag('Volksgezondheid',_,'N(soort,ev,basis,zijd,stan)',volksgezondheid).

exceptional_stem_tag(aan,adjective(_),                        'VZ(fin)',aan).
exceptional_stem_tag(belang_stellen,nominalized_adjective,    'WW(vd,nom,met-e,mv-n)',belang_stellen).
exceptional_stem_tag(betreffen,preposition(betreffende,[]),   'WW(od,vrij,zonder)',betreffen).
exceptional_stem_tag(derden,noun(both,count,pl),              'TW(rang,nom,mv-n)',drie).
exceptional_stem_tag(detineren,nominalized_adjective,         'WW(vd,nom,met-e,mv-n)',detineren).
exceptional_stem_tag(dode,noun(de,count,pl),                  'ADJ(nom,basis,met-e,mv-n)',dood).
exceptional_stem_tag(dode,noun(de,count,sg),                  'ADJ(nom,basis,met-e,zonder-n,stan)',dood).
exceptional_stem_tag(geestelijke,noun(de,count,pl),           'ADJ(nom,basis,met-e,mv-n)',geestelijk).
exceptional_stem_tag(geestelijke,noun(de,count,sg),           'ADJ(nom,basis,met-e,zonder-n,stan)',geestelijk).
exceptional_stem_tag(gevangen,particle(_),                    'WW(vd,vrij,zonder)',vangen).
exceptional_stem_tag(gezien,complementizer,                   'WW(vd,vrij,zonder)',zien).
exceptional_stem_tag(gezien,preposition(_,_),                 'WW(vd,vrij,zonder)',zien).
exceptional_stem_tag(hoeveelste, wh_number(rang),             'TW(rang,prenom,stan)',hoeveel).
exceptional_stem_tag(in,adjective(_),                         'VZ(fin)',in).
exceptional_stem_tag(jouwe,noun(both,count,both),             'VNW(bez,det,stan,vol,2v,ev,nom,met-e,zonder-n)',jou).
exceptional_stem_tag(jouwe,noun(both,count,pl),               'VNW(bez,det,stan,vol,2v,ev,nom,met-e,mv-n)',jou).
exceptional_stem_tag(leven_lang,noun(_,_,_),                  'ADJ(vrij,basis,zonder)',leven_lang).
exceptional_stem_tag(mee,loc_adverb,                          'VZ(fin)',mee).
exceptional_stem_tag(natuurkundige,noun(de,count,pl),         'ADJ(nom,basis,met-e,mv-n)',natuurkundig).
exceptional_stem_tag(natuurkundige,noun(de,count,sg),         'ADJ(nom,basis,met-e,zonder-n,stan)',natuurkundig).
exceptional_stem_tag(ons,noun(both,count,sg),                 'VNW(bez,det,stan,vol,1,mv,nom,met-e,zonder-n)',ons).
exceptional_stem_tag(op,adjective(_),                         'VZ(fin)',op).
exceptional_stem_tag(over,adjective(_),                       'VZ(fin)',over).
exceptional_stem_tag(sprake,_,                                'N(soort,ev,basis,dat)',spraak).
exceptional_stem_tag(stel,tag,                                'WW(pv,tgw,ev)',stellen).
exceptional_stem_tag(streven,noun(het,mass,sg),               'WW(inf,nom,zonder,zonder-n)',streven).
exceptional_stem_tag(tegen,adjective(_),                      'VZ(fin)',tegen).
exceptional_stem_tag(toe,_,                                   'VZ(fin)',toe).
exceptional_stem_tag(uitgerekend,modal_adverb,                'WW(vd,vrij,zonder)',uit_rekenen).
exceptional_stem_tag(verdenken,noun(de,count,sg),             'WW(vd,nom,met-e,zonder-n)',verdenken).
exceptional_stem_tag(verdenken,noun(de,count,pl),             'WW(vd,nom,met-e,mv-n)',verdenken).

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


exceptional_stem_tag(Var,_,_) :-
    var(Var),
    !,
    fail.

exceptional_stem_tag(v_root(_,Lem),Pos,Tag) :-
    exceptional_stem_tag(Lem,Pos,Tag).

exceptional_stem_tag(Stem,noun(both,both,both),'N(soort,ev,dim,onz,stan)') :-
    atom(Stem),
    atom_concat(_,je,Stem).
exceptional_stem_tag(Stem,_,'SPEC(vreemd)') :-
    vreemd_lemma(Stem).
exceptional_stem_tag('AEX-index',proper_name(sg),                   'N(soort,ev,basis,zijd,stan)').
exceptional_stem_tag('AOW',proper_name(sg),                         'N(soort,ev,basis,zijd,stan)').
exceptional_stem_tag('BTW',proper_name(sg),                         'N(soort,ev,basis,zijd,stan)').
exceptional_stem_tag('ICT',proper_name(sg),                         'N(soort,ev,basis,zijd,stan)').
exceptional_stem_tag('Oranje',proper_name(both,'ORG'),              'N(eigen,ev,basis,onz,stan)').
exceptional_stem_tag('Oranje',proper_name(sg,'ORG'),                'N(eigen,ev,basis,onz,stan)').
exceptional_stem_tag('SRG',meas_mod_noun(_,_,_),                    'N(eigen,ev,basis,zijd,stan)').
exceptional_stem_tag('SRG',meas_mod_noun(_,_,_,_),                  'N(eigen,ev,basis,zijd,stan)').
exceptional_stem_tag('VS',proper_name(both,'LOC'),                  'N(eigen,mv,basis)').
exceptional_stem_tag('\'s',determiner(pron),                        'LID(bep,gen,evmo)').
exceptional_stem_tag('zo\'n',determiner(een),                       'VNW(aanw,det,stan,prenom,zonder,agr)').
exceptional_stem_tag(*,_,                                           'LET()').
exceptional_stem_tag(^,_,                                           'SPEC(symb)').
exceptional_stem_tag(©,_,                                           'SPEC(symb)').
exceptional_stem_tag('CA',_,                                        'SPEC(symb)').
exceptional_stem_tag('#',tag,                                       'SPEC(symb)').
exceptional_stem_tag(aan,complementizer(aan_het),                   'VZ(init)').
exceptional_stem_tag(achter,loc_adverb,                             'VZ(fin)').
exceptional_stem_tag(achter,pred_np_me_adjective(_),                'VZ(fin)').
exceptional_stem_tag(af,adjective(pred(_)),                         'VZ(fin)').
exceptional_stem_tag(af_studeren,nominalized_adjective,             'WW(vd,nom,met-e,mv-n)').
exceptional_stem_tag(af_vaardigen,nominalized_adjective,            'WW(vd,nom,met-e,mv-n)').
exceptional_stem_tag(al,noun(both,both,pl),                         'VNW(onbep,det,stan,nom,met-e,mv-n)').  % in with_dt
exceptional_stem_tag(aldus,_,                                       'BW()').
exceptional_stem_tag(algemeen,noun(het,mass,sg),                    'ADJ(nom,basis,zonder,zonder-n)').
exceptional_stem_tag(alleen,modal_adverb,                           'BW()').
exceptional_stem_tag(alle,pronoun(nwh,thi,pl,de,both,indef),        'VNW(onbep,det,stan,nom,met-e,mv-n)').
exceptional_stem_tag(allebei,_,                                     'VNW(onbep,det,stan,vrij,zonder)').
exceptional_stem_tag(allemaal,_,                                    'BW()').
exceptional_stem_tag(allemachtig,tag,                               'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(aller,determiner(pron),                        'VNW(onbep,det,gen,prenom,met-e,mv)').
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
exceptional_stem_tag(betrekken,nominalized_adjective,               'WW(vd,nom,met-e,mv-n)').
exceptional_stem_tag(betrekken,nominalized_adjective_sg,            'WW(vd,nom,met-e,zonder-n)').
exceptional_stem_tag(beu,np_adjective,                              'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(beu,clause_np_adjective,                       'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(bij,conj(en),                                  'VZ(init)').
exceptional_stem_tag(binnen,loc_adverb,                             'VZ(fin)').
exceptional_stem_tag(blauw, noun(het,mass,sg),                      'ADJ(nom,basis,zonder,zonder-n)').
exceptional_stem_tag(boven,loc_adverb,                              'VZ(fin)').
exceptional_stem_tag(buiten,loc_adverb,                             'VZ(fin)').
exceptional_stem_tag(daarom, _,                                     'BW()').
exceptional_stem_tag(datgeen,_,                                     'VNW(aanw,det,stan,nom,met-e,zonder-n)').
exceptional_stem_tag(deels,adverb,                                  'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(degeen,pronoun(nwh,thi,sg,de,both,def,strpro), 'VNW(aanw,det,stan,nom,met-e,zonder-n)').
exceptional_stem_tag(degeen,pronoun(nwh,thi,pl,de,both,def,strpro), 'VNW(aanw,det,stan,nom,met-e,mv-n)').
exceptional_stem_tag(denk, denk_ik,                                 'WW(pv,tgw,ev)').
exceptional_stem_tag(deskundige,noun(de,count,pl),                  'ADJ(nom,basis,met-e,mv-n)').
exceptional_stem_tag(deskundige,noun(de,count,sg),                  'ADJ(nom,basis,met-e,zonder-n,stan)').
exceptional_stem_tag(destijds,_,                                    'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(deze,determiner(der),                          'VNW(aanw,det,gen,prenom,met-e,rest3)').
exceptional_stem_tag(deze,determiner(de,nwh,nmod,pro,yparg),        'VNW(aanw,det,stan,prenom,met-e,rest)').
exceptional_stem_tag(deze,pronoun(nwh,thi,pl,de,both,def,strpro),   'VNW(aanw,det,stan,nom,met-e,mv-n)').
exceptional_stem_tag(die,determiner(pron),                          'VNW(aanw,pron,gen,vol,3m,ev)').
exceptional_stem_tag(driemaal,noun(both,count,bare_meas),           'BW()').
exceptional_stem_tag(drug,noun(de,mass,sg),                         'N(soort,mv,basis)').
exceptional_stem_tag(duizend,noun(de,count,pl),                     'TW(hoofd,nom,mv-n,basis)').
exceptional_stem_tag(dus,_,                                         'BW()').
exceptional_stem_tag(echt,adverb,                                   'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(eenmaal,tmp_noun(_,_,_),                       'BW()').
exceptional_stem_tag(eens,_,                                        'BW()').
exceptional_stem_tag(eerder,tmp_app_noun,                           'ADJ(vrij,comp,zonder)').
exceptional_stem_tag(eersten,noun(both,count,pl),                   'TW(rang,nom,mv-n)').
exceptional_stem_tag(eindelijk,tag,                                 'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(eindje,noun(het,count,sg),                     'N(soort,ev,dim,onz,stan)').
exceptional_stem_tag(eindje,noun(het,count,pl),                     'N(soort,mv,dim)').
exceptional_stem_tag(elk,predm_adverb,                              'VNW(onbep,det,stan,vrij,zonder)').
exceptional_stem_tag(elkaar,pronoun(nwh,thi,pl,de,dat_acc,def),     'VNW(recip,pron,obl,vol,persoon,mv)').
exceptional_stem_tag(elkaar,determiner(pron),                       'VNW(recip,pron,gen,vol,persoon,mv)').
exceptional_stem_tag(elkander,determiner(pron),                     'VNW(recip,pron,gen,vol,persoon,mv)').
exceptional_stem_tag(één,pronoun(nwh,thi,sg,de,both,indef,strpro),  'TW(hoofd,nom,zonder-n,dim)').
exceptional_stem_tag(een,adjective(e),                              'VNW(onbep,det,stan,prenom,met-e,evz)').
exceptional_stem_tag(ergens,_,                                      'VNW(onbep,adv-pron,obl,vol,3o,getal)').
exceptional_stem_tag(even,adjective(both(tmpadv)),                  'BW()').
exceptional_stem_tag(eventjes,_,                                    'BW()').
exceptional_stem_tag(gaandeweg,_,                                   'BW()').
exceptional_stem_tag(gene,pronoun(nwh,thi,pl,de,both,def,strpro),   'VNW(aanw,det,stan,nom,met-e,mv-n)').
exceptional_stem_tag(genoeg, _,                                     'BW()').
exceptional_stem_tag(ggg,_,                                         'SPEC(onverst)').
exceptional_stem_tag(sneuvelen,nominalized_adjective,               'WW(vd,nom,met-e,mv-n)').
exceptional_stem_tag(betrekken,nominalized_adjective_sg,            'WW(vd,nom,met-e,zonder-n)').
exceptional_stem_tag(geleden,_,                                     'BW()').
exceptional_stem_tag(gelukkig,tag,                                  'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(god,determiner(pron),                          'N(soort,ev,basis,gen)').
exceptional_stem_tag(god,pronoun(nwh,thi,sg,de,gen,def),            'N(soort,ev,basis,gen)').
exceptional_stem_tag(goed,tag,                                      'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(groen, noun(het,mass,sg),                      'ADJ(nom,basis,zonder,zonder-n)').
exceptional_stem_tag(grootmoeder,determiner(pron),                  'N(soort,ev,basis,gen)').
exceptional_stem_tag(grootvader,determiner(pron),                   'N(soort,ev,basis,gen)').
exceptional_stem_tag(haar,determiner(pron),                         'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)').
exceptional_stem_tag(haar,pronoun(nwh,thi,sg,de,dat_acc,def,wkpro), 'VNW(pers,pron,obl,vol,3,getal,fem)').
exceptional_stem_tag(haarzelf,_,                                    'VNW(pers,pron,obl,nadr,3v,getal,fem)').
exceptional_stem_tag(hars,noun(both,count,sg),                      'N(soort,ev,basis,genus,stan)').
exceptional_stem_tag(heel,intensifier,                              'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(hem,pronoun(nwh,thi,sg,de,dat_acc,def,wkpro),  'VNW(pers,pron,obl,vol,3,ev,masc)').
exceptional_stem_tag(hemzelf,_,                                     'VNW(pers,pron,obl,nadr,3m,ev,masc)').
exceptional_stem_tag(henzelf,_,                                     'VNW(pers,pron,obl,nadr,3p,mv)').
exceptional_stem_tag(hij,pronoun(nwh,thi,sg,de,nom,def),            'VNW(pers,pron,nomin,vol,3,ev,masc)').
exceptional_stem_tag(hij,pronoun(nwh,thi,sg,de,nom,def,wkpro),      'VNW(pers,pron,nomin,red,3,ev,masc)').
exceptional_stem_tag(hijzelf,_,                                     'VNW(pers,pron,nomin,nadr,3m,ev,masc)').
exceptional_stem_tag(honderd_duizend,noun(de,count,pl),             'TW(hoofd,nom,mv-n,basis)').
exceptional_stem_tag(honderd,noun(de,count,pl),                     'TW(hoofd,nom,mv-n,basis)').
exceptional_stem_tag(hoogst,intensifier,                            'BW()').
exceptional_stem_tag(hun,determiner(pron),                          'VNW(bez,det,stan,vol,3,mv,prenom,zonder,agr)').
exceptional_stem_tag(ieder,predm_adverb,                            'VNW(onbep,det,stan,vrij,zonder)').
exceptional_stem_tag(ieders,determiner(pron),                       'VNW(onbep,pron,gen,vol,3p,ev)').
exceptional_stem_tag(iemand,determiner(pron),                       'VNW(onbep,pron,gen,vol,3p,ev)').
exceptional_stem_tag(iemand,_,                                      'VNW(onbep,pron,stan,vol,3p,ev)').
exceptional_stem_tag(iets,_,                                        'VNW(onbep,pron,stan,vol,3o,ev)').
exceptional_stem_tag(ikzelf,_,                                      'VNW(pers,pron,nomin,nadr,1,ev)').
exceptional_stem_tag(inderdaad,tag,                                 'BW()').
exceptional_stem_tag(ja,_,                                          'TSW()').
exceptional_stem_tag(je,determiner(pron),                           'VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)').
exceptional_stem_tag(jezelf,_,                                      'VNW(pr,pron,obl,nadr,2v,getal)').
exceptional_stem_tag(jong,noun(de,count,pl),                        'ADJ(nom,comp,met-e,mv-n)').
exceptional_stem_tag(jong,noun(de,count,sg),                        'ADJ(nom,comp,met-e,zonder-n,stan)').
exceptional_stem_tag(jou,determiner(pron),                          'VNW(bez,det,stan,vol,2v,ev,prenom,zonder,agr)').
exceptional_stem_tag(juist,modal_adverb,                            'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(jullie,determiner(pron),                       'VNW(bez,det,stan,nadr,2v,mv,prenom,zonder,agr)').
exceptional_stem_tag(katoen,noun(both,count,sg),                    'N(soort,ev,basis,genus,stan)').
exceptional_stem_tag(keer,noun(de,count,meas),                      'N(soort,ev,basis,genus,stan)').
exceptional_stem_tag(keizer,determiner(pron),                       'N(soort,ev,basis,gen)').
exceptional_stem_tag(kerke,noun(_,_,_),                             'N(soort,ev,basis,dat)').
exceptional_stem_tag(kortom,tag,                                    'BW()').
exceptional_stem_tag(kwijt,_,                                       'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(land,determiner(pron),                         'N(soort,ev,basis,gen)').
exceptional_stem_tag(later,tmp_app_noun,                            'ADJ(vrij,comp,zonder)').
exceptional_stem_tag(liefst,modal_adverb(_),                        'BW()').
exceptional_stem_tag(man,determiner(pron),                          'N(soort,ev,basis,gen)').
exceptional_stem_tag(maximum,adjective(_),                          'N(soort,ev,basis,onz,stan)').
exceptional_stem_tag(veel,postadv_adverb,                           'VNW(onbep,grad,stan,vrij,zonder,comp)').
exceptional_stem_tag('v.j.',_,                                      'SPEC(afk)').
exceptional_stem_tag(meerdere,determiner(pl_num,nwh,nmod,pro,yparg),'VNW(onbep,det,stan,prenom,met-e,mv)').
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
exceptional_stem_tag(nee,_,                                         'TSW()').
exceptional_stem_tag(nergens,_,                                     'VNW(onbep,adv-pron,obl,vol,3o,getal)').
exceptional_stem_tag(net,adjective(no_e(adv)),                      'BW()').
exceptional_stem_tag(net,modal_adverb,                              'BW()').
exceptional_stem_tag(niemand,_,                                     'VNW(onbep,pron,stan,vol,3p,ev)').
exceptional_stem_tag(niemands,determiner(pron),                     'VNW(onbep,pron,gen,vol,3p,ev)').
exceptional_stem_tag(niets,_,                                       'VNW(onbep,pron,stan,vol,3o,ev)').
exceptional_stem_tag(niks,_,                                        'VNW(onbep,pron,stan,vol,3o,ev)').
exceptional_stem_tag(nomineren,nominalized_adjective,               'WW(vd,nom,met-e,mv-n)').
exceptional_stem_tag(nou,tag,                                       'BW()').
exceptional_stem_tag(nummer,noun(both,count,sg),                    'N(soort,ev,basis,onz,stan)').
exceptional_stem_tag(komen,tag,                                     'WW(pv,tgw,ev)').
exceptional_stem_tag(kijken,tag,                                    'WW(pv,tgw,ev)').
exceptional_stem_tag(o,tag,                                         'SPEC(symb)').  % used frequently as an itemizer
exceptional_stem_tag(om,adjective(pred(_)),                         'VZ(fin)').
exceptional_stem_tag(ondanks,_,                                     'VZ(init)').
exceptional_stem_tag(onder,loc_adverb,                              'VZ(fin)').
exceptional_stem_tag(ondervragen,nominalized_adjective,             'WW(vd,nom,met-e,mv-n)').
exceptional_stem_tag(onderweg,adjective(_),                         'BW()').
exceptional_stem_tag(oneens,_,                                      'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(onmiddellijk,modal_adverb(_),                  'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(ons,determiner(pron),                          'VNW(bez,det,stan,vol,1,mv,prenom,zonder,evon)').
exceptional_stem_tag(onszelf,_,                                     'VNW(pr,pron,obl,nadr,1,mv)').
exceptional_stem_tag(onverschillig,preposition(_,_,of_sbar),        'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(op,adjective(pred(_)),                         'VZ(fin)').
exceptional_stem_tag(openbaar,noun(het,mass,sg),                    'ADJ(nom,basis,zonder,zonder-n)').
exceptional_stem_tag(op_leiden,nominalized_adjective,               'WW(vd,nom,met-e,mv-n)').
exceptional_stem_tag(opzet,noun(de,count,sg),                       'N(soort,ev,basis,genus,stan)').
exceptional_stem_tag(overal,_,                                      'VNW(onbep,adv-pron,obl,vol,3o,getal)').
exceptional_stem_tag(overeenkomstig,preposition(_,_),               'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(overleven, nominalized_adjective,              'WW(od,nom,met-e,mv-n)').
exceptional_stem_tag(overstag,_,                                    'BW()').
exceptional_stem_tag(red,   tag,                                    'N(soort,ev,basis,zijd,stan)').
exceptional_stem_tag('red.',tag,                                    'N(soort,ev,basis,zijd,stan)').
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
exceptional_stem_tag(storen,nominalized_adjective,                  'WW(vd,nom,met-e,mv-n)').
exceptional_stem_tag(streven,noun(sg,_,het),                        'WW(inf,nom,zonder,zonder-n)').
exceptional_stem_tag(tal,determiner(wat,nwh,mod,pro,yparg),         'N(soort,ev,basis,onz,stan)').
exceptional_stem_tag(tegemoet,_,                                    'BW()').
exceptional_stem_tag(teneinde,complementizer(inf),                  'VG(onder)').
exceptional_stem_tag(tenminste,tag,                                 'BW()').
exceptional_stem_tag(thuis,_,                                       'BW()').
exceptional_stem_tag(tien_duizend,noun(de,count,pl),                'TW(hoofd,nom,mv-n,basis)').
exceptional_stem_tag(toedoen,_,                                     'WW(inf,nom,zonder,zonder-n)').
exceptional_stem_tag(tot,conj(tot),                                 'VZ(init)').
exceptional_stem_tag(tot,complementizer,                            'VZ(init)').
exceptional_stem_tag(trouwens,_,                                    'BW()').
exceptional_stem_tag(tweemaal,noun(both,count,bare_meas),           'BW()').
exceptional_stem_tag(u,determiner(pron),                            'VNW(bez,det,stan,vol,2,getal,prenom,zonder,agr)').
exceptional_stem_tag(uisluitend,modal_adverb,                       'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(uit_voeren,nominalized_adjective,              'WW(od,nom,met-e,mv-n)').
exceptional_stem_tag(uw,determiner(pron),                           'VNW(bez,det,stan,vol,2,getal,prenom,zonder,agr)').
exceptional_stem_tag(jezelf,_,                                      'VNW(pr,pron,obl,nadr,2,getal)').
exceptional_stem_tag(vader,determiner(pron),                        'N(soort,ev,basis,gen)').
exceptional_stem_tag(veel,adjective(e),                             'VNW(onbep,grad,stan,prenom,met-e,agr,basis)').
exceptional_stem_tag(veel,nominalized_adjective,                    'VNW(onbep,grad,stan,nom,met-e,mv-n,basis)').
exceptional_stem_tag(veevoer,noun(both,mass,sg),                    'N(soort,ev,basis,onz,stan)').
exceptional_stem_tag(verdachte,noun(de,count,sg),                   'WW(vd,nom,met-e,zonder-n)').
exceptional_stem_tag(verdachte,noun(de,count,pl),                   'WW(vd,nom,met-e,mv-n)').
exceptional_stem_tag(veroordelen,nominalized_adjective(_),          'WW(vd,nom,met-e,mv-n)').
exceptional_stem_tag(veroordelen,nominalized_adjective,             'WW(vd,nom,met-e,mv-n)').
exceptional_stem_tag(versus,_,                                      'VZ(init)').
exceptional_stem_tag(vol,preposition(_,_),                          'ADJ(vrij,basis,zonder)').
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
exceptional_stem_tag(wachten,nominalized_adjective,                 'WW(od,nom,met-e,mv-n)').
exceptional_stem_tag(wat,adverb,                                    'VNW(onbep,pron,stan,vol,3o,ev)').
exceptional_stem_tag(watte,pronoun(ywh,thi,sg,het,both,indef,nparg),'VNW(vrag,pron,stan,nadr,3o,ev)').
exceptional_stem_tag(welletjes,_,                                   'BW()').
exceptional_stem_tag(wereld,determiner(pron),                       'N(soort,ev,basis,gen)').
exceptional_stem_tag(wiens,determiner(pron,rwh),                    'VNW(vb,pron,gen,vol,3m,ev)').
exceptional_stem_tag(wit,determiner(pron),                          'N(soort,ev,basis,gen)').
exceptional_stem_tag(wit, noun(het,mass,sg),                        'ADJ(nom,basis,zonder,zonder-n)').
exceptional_stem_tag(xxx,_,                                         'SPEC(onverst)').
exceptional_stem_tag(x,tmp_noun(_,_,_),                             'SPEC(symb)').
exceptional_stem_tag(zat,np_adjective,                              'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(zat,clause_np_adjective,                       'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(zat,postadj_adverb,                            'ADJ(vrij,basis,zonder)').
exceptional_stem_tag(zeggen,tag,                                    'WW(pv,tgw,ev)').
exceptional_stem_tag(zien,nominalized_adjective,                    'WW(od,nom,met-e,mv-n)').
exceptional_stem_tag(zichzelf,_,                                    'VNW(refl,pron,obl,nadr,3,getal)').
exceptional_stem_tag(zijn,determiner(pron),                         'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)').
exceptional_stem_tag(zijn,preposition(zijnde,[]),                   'WW(od,vrij,zonder)').
exceptional_stem_tag(zijzelf,_,                                     'VNW(pers,pron,nomin,nadr,3v,ev,fem)').
exceptional_stem_tag(zo,_,                                          'BW()').
exceptional_stem_tag('zo\'n',_,                                     'VNW(aanw,det,stan,prenom,zonder,agr)').
exceptional_stem_tag(zover,_,                                       'BW()').
exceptional_stem_tag(zwart,determiner(pron),                        'N(soort,ev,basis,gen)').

% hij/zij
exceptional_stem_tag(hij_zij, pronoun(nwh,thi,sg,de,nom,def),         'VNW(pers,pron,nomin,vol,3,ev,zijd)').
exceptional_stem_tag(zij_hij, pronoun(nwh,thi,sg,de,nom,def),         'VNW(pers,pron,nomin,vol,3,ev,zijd)').
% hem/haar
exceptional_stem_tag(hem_haar,pronoun(nwh,thi,sg,de,dat_acc,def),     'VNW(pers,pron,obl,vol,3,ev,zijd)').
exceptional_stem_tag(haar_hem,pronoun(nwh,thi,sg,de,dat_acc,def),     'VNW(pers,pron,obl,vol,3,ev,zijd)').
% zulks
exceptional_stem_tag(zulk,    pronoun(nwh,thi,sg,het,both,indef),     'VNW(aanw,pron,stan,vol,3o,ev)').
% u
exceptional_stem_tag(u,       reflexive(u,sg),                        'VNW(pr,pron,obl,vol,2,getal)').
% wien
exceptional_stem_tag(wie,     pronoun(ywh,thi,both,de,dat,def),       'VNW(vb,pron,dat,vol,3p,getal)').
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


exceptional_word_tag(Word,_,'SPEC(afgebr)') :-
    atom(Word),
    atom_concat(_,'*a',Word).

exceptional_word_tag('o.a.',_,                                       'SPEC(afk)').
exceptional_word_tag('O.a.',_,                                       'SPEC(afk)').
exceptional_word_tag('ca.',_,                                        'SPEC(afk)').
exceptional_word_tag('Ca.',_,                                        'SPEC(afk)').
exceptional_word_tag('o.m.',_,                                       'SPEC(afk)').
exceptional_word_tag('O.m.',_,                                       'SPEC(afk)').
exceptional_word_tag('Chr.',_,                                       'SPEC(afk)').
exceptional_word_tag('v.',_,                                         'SPEC(afk)').
exceptional_word_tag('c.q.',_,                                       'SPEC(afk)').
exceptional_word_tag('t/m',_,                                        'SPEC(afk)').
exceptional_word_tag('nr.',_,                                        'SPEC(afk)').
exceptional_word_tag('n.a.g.',_,                                     'SPEC(afk)').
exceptional_word_tag('b.v.',_,                                       'SPEC(afk)').
exceptional_word_tag('e.d.',_,                                       'SPEC(afk)').
exceptional_word_tag('d.w.z.',_,                                     'SPEC(afk)').
exceptional_word_tag('dwz.',_,                                       'SPEC(afk)').
exceptional_word_tag('t.o.v.',_,                                     'SPEC(afk)').
exceptional_word_tag('n.a.v.',_,                                     'SPEC(afk)').
exceptional_word_tag('t.a.v.',_,                                     'SPEC(afk)').
exceptional_word_tag('i.v.m.',_,                                     'SPEC(afk)').
exceptional_word_tag('i.p.v.',_,                                     'SPEC(afk)').
exceptional_word_tag('e.a.',_,                                       'SPEC(afk)').
exceptional_word_tag('incl.',_,                                      'SPEC(afk)').
exceptional_word_tag('t.z.t.',_,                                     'SPEC(afk)').
exceptional_word_tag('N.v.t.',_,                                     'SPEC(afk)').
exceptional_word_tag('n.v.t.',_,                                     'SPEC(afk)').
exceptional_word_tag('Nr.',_,                                        'SPEC(afk)').
exceptional_word_tag('c.s.',_,                                       'SPEC(afk)').
exceptional_word_tag('blz.',_,                                       'SPEC(afk)').
exceptional_word_tag('Blz.',_,                                       'SPEC(afk)').
exceptional_word_tag('blz',_,                                        'SPEC(afk)').
exceptional_word_tag('Blz',_,                                        'SPEC(afk)').
exceptional_word_tag('m.n.',_,                                       'SPEC(afk)').
exceptional_word_tag('resp.',_,                                      'SPEC(afk)').
exceptional_word_tag('d.d.',_,                                       'SPEC(afk)').
exceptional_word_tag('i.s.m.',_,                                     'SPEC(afk)').
exceptional_word_tag('vs.',_,                                        'SPEC(afk)').
exceptional_word_tag('mln.',_,                                       'SPEC(afk)').
exceptional_word_tag('mg/dag',_,                                     'SPEC(symb)').
exceptional_word_tag(©,_,                                            'SPEC(symb)').
exceptional_word_tag('°C',_,                                         'SPEC(symb)').
exceptional_word_tag('°',_,                                          'SPEC(symb)').
exceptional_word_tag('×',_,                                          'SPEC(symb)').
exceptional_word_tag('=',_,                                          'SPEC(symb)').
exceptional_word_tag('#',_,                                          'SPEC(symb)').
exceptional_word_tag('+',_,                                          'SPEC(symb)').
exceptional_word_tag('±',_,                                          'SPEC(symb)').
exceptional_word_tag('%',_,                                          'SPEC(symb)').
exceptional_word_tag('&',_,                                          'SPEC(symb)').
exceptional_word_tag('$',_,                                          'SPEC(symb)').
exceptional_word_tag('Trb.',_,                                       'SPEC(symb)').
exceptional_word_tag('Trb',_,                                        'SPEC(symb)').
exceptional_word_tag('{',_,                                          'LET()').

exceptional_word_tag('\'m', pronoun(nwh,thi,sg,de,dat_acc,def,wkpro),'VNW(pers,pron,obl,red,3,ev,masc)').
exceptional_word_tag('z\'n',_,                                       'VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)').
exceptional_word_tag('m\'n',determiner(pron),                        'VNW(bez,det,stan,red,1,ev,prenom,zonder,agr)').
exceptional_word_tag('d\'r',determiner(pron),                        'VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)').
exceptional_word_tag(graden,noun(de,count,meas),                     'N(soort,mv,basis)').
exceptional_word_tag('-ie',pronoun(nwh,thi,sg,de,nom,def),           'VNW(pers,pron,nomin,red,3,ev,masc)').
exceptional_word_tag(beide,pronoun(nwh,thi,pl,de,both,indef),        'VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)').
exceptional_word_tag(beide,predm_adverb,                             'VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)').
exceptional_word_tag(beiden,predm_adverb,                            'VNW(onbep,grad,stan,nom,met-e,mv-n,basis)').
exceptional_word_tag(gevangenen,_,                                   'WW(vd,nom,met-e,mv-n)').
exceptional_word_tag('\'k',pronoun(nwh,fir,sg,de,nom,def),           'VNW(pers,pron,nomin,red,1,ev)').

%% kom de gij mee
exceptional_word_tag(de,    pronoun(nwh,inv,sg,both,both,def),       'VNW(pers,pron,dial)').

exceptional_word_tag(allen,  al,al,_,                                'VNW(onbep,det,stan,nom,met-e,mv-n)').
exceptional_word_tag('Allen',al,al,_,                                'VNW(onbep,det,stan,nom,met-e,mv-n)').

exceptional_word_tag(Surf,_,Surf,Tag,Pos) :-
    symb(Surf,Tag,Pos).

symb('ha.', noun(de,count,meas), 'SPEC(symb)').
symb(ha,    noun(de,count,meas), 'SPEC(symb)').
symb('¤',_,   'SPEC(symb)').
symb(mm,_,    'SPEC(symb)').
symb(m,_,     'SPEC(symb)').
symb('mm.',_, 'SPEC(symb)').
symb(cm,_,    'SPEC(symb)').
symb('cm.',_, 'SPEC(symb)').
symb(tl,_,    'SPEC(symb)').
symb('km²',_, 'SPEC(symb)').
symb(km,_,    'SPEC(symb)').
symb('km.',_, 'SPEC(symb)').
symb(km/u,_,  'SPEC(symb)').
symb(km2,_,   'SPEC(symb)').
symb(mg,_,    'SPEC(symb)').
symb('µg',_,  'SPEC(symb)').
symb(ml,_,    'SPEC(symb)').
symb('mg.',_, 'SPEC(symb)').
symb('ml.',_, 'SPEC(symb)').

often_het(afval).
often_het(bestuur).
often_het(commentaar).
often_het(gewicht).
often_het(idee).
often_het(hoogte_punt).
often_het(kwart).
often_het(lucht_toezicht).
often_het(miljard).
often_het(miljoen).
often_het(misbruik).
often_het(moment).
often_het(onderhoud).
often_het(paar).
often_het(percent).
often_het(plan).
often_het(punt).
often_het(raam).
often_het(schilderij).
often_het(toezicht).
often_het(weekend).

genus_genus(baldakijn).
genus_genus(boord).
genus_genus(keer).
genus_genus(koolmonoxide).
genus_genus(soort).

stem_dependent_tag(modal_adverb,enkel,'BW()').
stem_dependent_tag(adjective(no_e(_)),enkel,'BW()').
stem_dependent_tag(noun(both,_,sg),Stem,'N(soort,ev,basis,genus,stan)') :-
    genus_genus(Stem).
stem_dependent_tag(noun(both,_,bare_meas),Stem,'N(soort,ev,basis,onz,stan)') :-
    often_het(Stem).
stem_dependent_tag(noun(both,_,meas),Stem,'N(soort,ev,basis,onz,stan)') :-
    often_het(Stem).
stem_dependent_tag(noun(both,_,sg),Stem,'N(soort,ev,basis,onz,stan)') :-
    often_het(Stem).
stem_dependent_tag(determiner(pron),Cap, 'N(eigen,ev,basis,gen)') :-
    starts_with_capital(Cap). 
stem_dependent_tag('--', Eh, 'TSW()') :-
    alpino_lexical_analysis:hesitation(Eh).
stem_dependent_tag('--', Name, 'SPEC(deeleigen)') :-
    starts_with_capital(Name).
stem_dependent_tag(fixed_part([deelgenoot]),_Word,  'N(soort,ev,basis,zijd,stan)').
stem_dependent_tag(fixed_part([gehoor]),_Word,      'N(soort,ev,basis,onz,stan)').
stem_dependent_tag(fixed_part([kenbaar]),_Word,     'ADJ(vrij,basis,zonder)').
stem_dependent_tag(fixed_part([kandidaat]),_Word,   'N(soort,ev,basis,zijd,stan)').
stem_dependent_tag(fixed_part([klaar]),_Word,       'ADJ(vrij,basis,zonder)').
stem_dependent_tag(fixed_part([rood]),_,            'ADJ(vrij,basis,zonder)').
stem_dependent_tag(fixed_part([uiting]),_Word,      'N(soort,ev,basis,zijd,stan)').
stem_dependent_tag(fixed_part(_),Word,Tag) :-
    lassy(Word,Tag).
stem_dependent_tag(skip,Word,Tag) :-
    lassy(Word,Tag).
    
stem_dependent_tag(modal_adverb,Word,'ADJ(vrij,basis,zonder)') :-
    alpino_lex:lexicon(adjective(_),_,[Word],[],_).
stem_dependent_tag(modal_adverb(_),Word,'ADJ(vrij,basis,zonder)') :-
    alpino_lex:lexicon(adjective(_),_,[Word],[],_).

stem_dependent_tag(pronoun(nwh,thi,sg,de,dat_acc,def),hem,  'VNW(pers,pron,obl,vol,3,ev,masc)').
stem_dependent_tag(pronoun(nwh,thi,sg,de,dat_acc,def),haar, 'VNW(pers,pron,obl,vol,3,getal,fem)').

stem_dependent_tag(verb(_,sg_heeft,_),v_root(Word,_),Tag) :-
    (   Word == heb
    ->  Tag='WW(pv,tgw,met-t)'
    ;   Word == ben
    ->  Tag='WW(pv,tgw,ev)'
    ;   atom_concat(heb_,_,Word)
    ->  Tag='WW(pv,tgw,met-t)'
    ;   atom_concat(ben_,_,Word)
    ->  Tag='WW(pv,tgw,ev)'
    ).

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

stem_dependent_tag(noun(de,count,sg),Stem,'N(eigen,ev,basis,zijd,stan)') :-
    (   alpino_lex:date_month(Stem)
    ;   lists:member(Stem,[zondag,maandag,dinsdag,woensdag,donderdag,vrijdag,zaterdag])
    ).

stem_dependent_tag(noun(het,count,sg),Stem,'N(soort,ev,dim,onz,stan)') :-
    atom(Stem),
    atom_concat(_,'_DIM',Stem).

stem_dependent_tag(noun(het,count,pl),Stem,'N(soort,mv,dim)') :-
    atom(Stem),
    atom_concat(_,'_DIM',Stem).

stem_dependent_tag(tmp_np,middernacht,'N(soort,ev,basis,zijd,stan)').

stem_dependent_tag(sentence_adverb,Stem0,Stem,'ADJ(vrij,dim,zonder)'):-
    tjes(Stem0,Stem).
stem_dependent_tag(adjective(both(tmpadv)),Stem0,Stem,'ADJ(vrij,dim,zonder)'):-
    tjes(Stem0,Stem).
stem_dependent_tag(adjective(pred(_)),Stem0,Stem,'ADJ(vrij,dim,zonder)'):-
    tjes(Stem0,Stem).

tjes(bleekjes,bleek).
tjes(droogjes,droog).
tjes(dunnetjes,dun).
tjes(fijntjes,fijn).
tjes(flauwtjes,flauw).
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


cgn_postag_c(Word,Pos,Tag) :-
    exceptional_word_tag(Word,Pos,Tag).
cgn_postag_c(_,Pos,Tag) :-
    cgn_postag_c(Pos,Tag).

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
cgn_postag_c(cleft_het_noun,                  'VNW(pers,pron,stan,red,3,ev,onz)').

cgn_postag_c(proper_name(_),                  'SPEC(deeleigen)').
cgn_postag_c(proper_name(_,_),                'SPEC(deeleigen)').
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
%% we simple gamble for "vrij"
cgn_postag_c(adjective(e),                    'ADJ(prenom,basis,met-e,stan)').
cgn_postag_c(adjective(ende(_)),              'WW(od,prenom,met-e)').
cgn_postag_c(adjective(ere),                  'ADJ(prenom,comp,met-e,stan)').
cgn_postag_c(adjective(ste),                  'ADJ(prenom,sup,met-e,stan)').

cgn_postag_c(adjective(ge_e),                 'WW(vd,prenom,met-e)').
cgn_postag_c(adjective(ge_no_e(_)),           'WW(vd,prenom,zonder)').
cgn_postag_c(adjective(ge_both(_)),           'WW(vd,prenom,zonder)').

cgn_postag_c(adjective(no_e(_)),              'ADJ(prenom,basis,zonder)').
cgn_postag_c(adjective(er(_)),                'ADJ(prenom,comp,zonder)').
cgn_postag_c(adjective(st(_)),                'ADJ(prenom,sup,zonder)').

cgn_postag_c(adjective(end(_)),               'WW(od,prenom,zonder)').

cgn_postag_c(adjective(both(osentadv)),       'BW()').
cgn_postag_c(adjective(both(_)),              'ADJ(prenom,basis,zonder)').

cgn_postag_c(adjective(stof),                 'ADJ(prenom,basis,zonder)').

cgn_postag_c(post_adjective(no_e),            'ADJ(postnom,basis,met-s)').
cgn_postag_c(post_adjective(er),              'ADJ(postnom,comp,met-s)').
cgn_postag_c(post_adjective_anders(er),       'ADJ(postnom,basis,met-s)').

cgn_postag_c(adjective(het_st(_)),            'ADJ(vrij,sup,zonder)').

cgn_postag_c(nominalized_adjective,           'ADJ(nom,basis,met-e,mv-n)').
cgn_postag_c(nominalized_adjective(_),        'ADJ(nom,basis,met-e,mv-n)').
cgn_postag_c(nominalized_compar_adjective,    'ADJ(nom,comp,met-e,mv-n)').
cgn_postag_c(nominalized_compar_adjective_sg, 'ADJ(nom,comp,met-e,zonder-n)').
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
cgn_postag_c(verb(_,modal_inv,_),             'WW(pv,tgw,ev)').
cgn_postag_c(verb(_,sg_hebt,_),               'WW(pv,tgw,met-t)').
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
det_pron(zulke,       'VNW(aanw,det,stan,prenom,met-e,rest)',           'VNW(aanw,det,stan,nom,met-e,zonder-n)'). % want "rest" oid ipv "zonder-n" bestaat niet in tag-set!!
det_pron(zulk,        'VNW(aanw,det,stan,prenom,zonder,evon)',          'VNW(aanw,det,stan,vrij,zonder)').

cgn_postag_proper(Postag,Stem0,Stem,Q0,Q,Result,SUB) :-
    stem_al(Stem0,Stem),
    find_node(Q0,Q,Result,Node),
    find_dehet(Node,DeHet,Stem),
    find_sgpl(Node,SgPl),
    name_postag(DeHet,SgPl,SUB,Stem,Postag,Q0,Q).

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

find_dehet(Node,De,Stem) :-
    alpino_data:agr(Node,Agr),
    alpino_data:de(Agr2),
    alpino_data:het(Agr3),
    (   \+ Agr = Agr2
    ->  De = het
    ;   \+ Agr = Agr3
    ->  De = de
    ;   default_dehet(Stem,De1)
    ->  De = De1
    ;   De = both
    ).

default_dehet(aantal,het).
default_dehet(geval,het).
default_dehet(medicijn,het).
default_dehet(nummer,het).

nattr(Node) :-
    alpino_data:aform(Node,Aform),
    alpino_data:not_attr(Aform3),
    \+ \+ Aform = Aform3.

noun_postag(de,    sg,        'N(soort,ev,basis,zijd,stan)').
noun_postag(het,   sg,        'N(soort,ev,basis,onz,stan)').
noun_postag(both,  sg,        'N(soort,ev,basis,zijd,stan)').
noun_postag(de,    both,      'N(soort,ev,basis,zijd,stan)').
noun_postag(het,   both,      'N(soort,ev,basis,onz,stan)').
noun_postag(both,  both,      'N(soort,ev,basis,zijd,stan)').
noun_postag(de,    bare_meas, 'N(soort,ev,basis,zijd,stan)').
noun_postag(het,   bare_meas, 'N(soort,ev,basis,onz,stan)').
noun_postag(both,  bare_meas, 'N(soort,ev,basis,zijd,stan)').
noun_postag(de,    meas,      'N(soort,ev,basis,zijd,stan)').
noun_postag(het,   meas,      'N(soort,ev,basis,onz,stan)').
noun_postag(both,  meas,      'N(soort,ev,basis,zijd,stan)').
noun_postag(_,     pl,        'N(soort,mv,basis)').

name_postag(DeHet0,SgPl0,Sub,Stem,Tag,Q0,Q) :-
    hdrug_util:hdrug_flag(add_nodes_for_mwu,On),
    (   On == on,
	Q-Q0 > 1
    ->  Tag = 'SPEC(deeleigen)'
    ;   try_sgpl(SgPl0,SgPl,Stem),
	try_dehet(DeHet0,DeHet,Stem),
	name_postag(DeHet,SgPl,Sub,Tag)
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

try_sgpl('Alpen',pl).
try_sgpl('Angels',pl).
try_sgpl('Antillen',pl).
try_sgpl('Ardennen',pl).
try_sgpl('Balearen',pl).
try_sgpl('Bardi\'s',pl).
try_sgpl('Borg',pl).
try_sgpl('Brattholmeilanden',pl).
try_sgpl('Caraïben',pl).
try_sgpl('Caraïben',pl).
try_sgpl('Cats',pl).
try_sgpl('Cetniks',pl).
try_sgpl('Dorsets',pl).
try_sgpl('FNV-Bondgenoten',pl).
try_sgpl('Filipijnen',pl).
try_sgpl('Flinstones',pl).
try_sgpl('Flintstones',pl).
try_sgpl('Franken',pl).
try_sgpl('Fransen',pl).
try_sgpl('Fransen',pl).
try_sgpl('GGD-en',pl).
try_sgpl('Grenslandhallen',pl).
try_sgpl('Habsburgers',pl).
try_sgpl('Hohenstaufen',pl).
try_sgpl('Hoogovens',pl).
try_sgpl('Kempen',pl).
try_sgpl('Khmer',pl).
try_sgpl('Klingonen',pl).
try_sgpl('Middeleeuwen',pl).
try_sgpl('Molukken',pl).
try_sgpl('Mon',pl).
try_sgpl('Nederlanden',pl).
try_sgpl('Nomads',pl).
try_sgpl('Noord-Molukken',pl).
try_sgpl('Oostkantons',pl).
try_sgpl('Oscars',pl).
try_sgpl('Pashtun',pl).
try_sgpl('Pruisen',pl).
try_sgpl('Pyreneeën',pl).
try_sgpl('Pyu',pl).
try_sgpl('Safaviden',pl).
try_sgpl('Spelen',pl).
try_sgpl('Taliban',pl).
try_sgpl('Tories',pl).
try_sgpl('Trekkies',pl).
try_sgpl('USA',pl).
try_sgpl('V.S.',pl).
try_sgpl('VN',pl).
try_sgpl('Vogezen',pl).
try_sgpl('VS',pl).
try_sgpl('VS.',pl).
try_sgpl('Vikings',pl).
try_sgpl('Vulcans',pl).
try_sgpl('Wadden',pl).
try_sgpl('Waddeneilanden',pl).


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
try_dehet(L,genus) :-
    genus_naam(L).
try_dehet(L,de) :-
    de_heur(L).
try_dehet(L,het) :-
    het_heur(L).

genus_naam('4FM').
genus_naam('ABN-AMRO').
genus_naam('Adecco').
genus_naam('Agusta').
genus_naam('Ahold').
genus_naam('Al-Qaida').
genus_naam('al-Qaida').
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
genus_naam('Estonia').
genus_naam('Euronext').
genus_naam('EWI').
genus_naam('EZ').
genus_naam('Fortis').
genus_naam('Google').
genus_naam('Heineken').
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
genus_naam('Sabena').
genus_naam('SAR').
genus_naam('SEM').
genus_naam('Smead').
genus_naam('Sobelair').
genus_naam('SOLFA').
genus_naam('Spirit').
genus_naam('STEVIN').
genus_naam('Textkernel').
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
de_naam('Alzheimer').
de_naam('Antenna').
de_naam('Aquarius').
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
de_naam('Ewell').
de_naam('FARC').
de_naam('Farnese').
de_naam('Fere').
de_naam('Fiat').
de_naam('Flanagan').
de_naam('FNB').
de_naam('G.R.I.').
de_naam('Gore').
de_naam('Grant').
de_naam('Grigorenko').
de_naam('HP').
de_naam('HR-beglazing').
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
de_naam('Kapp').
de_naam('Kolme').
de_naam('Kongo').
de_naam('Koninginnedag').
de_naam('KVP').
de_naam('Langefjord').
de_naam('Lay').
de_naam('Ligterink').
de_naam('Lila').
de_naam('Lincoln').
de_naam('Longstreet').
de_naam('MRI').
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
de_naam('Morgan').
de_naam('Mother').
de_naam('Mouret').
de_naam('Muggia').
de_naam('NASA').
de_naam('NAVO').
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
het_naam('FAVV').
het_naam('Feijenoord').         
het_naam('Feyenoord').
het_naam('FNV').
het_naam('Frans').
het_naam('GMD').
het_naam('GRUP').
het_naam('GVAV').
het_naam('Georgia').
het_naam('Greenpeace').         
het_naam('Griffoendor').
het_naam('GroenLinks').         
het_naam('Harvard').
het_naam('Heeswijk').
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
het_naam('Israël').
het_naam('Jamathi').
het_naam('Jantje').
het_naam('Journaal').
het_naam('Juda').
het_naam('Juventus').
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
het_naam('SHO').
het_naam('Siemens').
het_naam('Spaans').
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

name_postag(both, sg,   'MISC', 'N(eigen,ev,basis,genus,stan)').
name_postag(both, sg,   'LOC',  'N(eigen,ev,basis,onz,stan)'  ).
name_postag(both, sg,   'PER',  'N(eigen,ev,basis,zijd,stan)' ).
name_postag(both, sg,   'ORG',  'N(eigen,ev,basis,zijd,stan)'  ).
name_postag(de,   sg,   _,      'N(eigen,ev,basis,zijd,stan)' ).
name_postag(het,  sg,   _,      'N(eigen,ev,basis,onz,stan)'  ).
name_postag(genus,sg,   _,      'N(eigen,ev,basis,genus,stan)'  ).
name_postag(both, both, 'MISC', 'N(eigen,ev,basis,genus,stan)').
name_postag(both, both, 'LOC',  'N(eigen,ev,basis,onz,stan)'  ).
name_postag(both, both, 'PER',  'N(eigen,ev,basis,zijd,stan)' ).
name_postag(both, both, 'ORG',  'N(eigen,ev,basis,zijd,stan)'  ).
name_postag(de,   both, _,      'N(eigen,ev,basis,zijd,stan)' ).
name_postag(het,  both, _,      'N(eigen,ev,basis,onz,stan)'  ).
name_postag(genus,both, _,      'N(eigen,ev,basis,genus,stan)'  ).
name_postag(_,    pl,   _,      'N(eigen,mv,basis)'           ).

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
    mwu_tags(Tags,Stem,1,Q0,Q).

mwu_postag(Frame,Stem,Surf,Q0,Q,_Result) -->
    { mwu_postag_frame_stem_surf(Frame,Stem,Surf,Tags,Stems) },
    mwu_tags(Tags,Stems,1,Q0,Q).

mwu_postag(Frame,Stem,Surf,Q0,Q,_Result) -->
    { mwu_postag_frame_surf(Frame,Surf,Tags) },
    mwu_tags(Tags,Stem,1,Q0,Q).

mwu_postag(Frame,Stem,_Surf,Q0,Q,_Result) -->
    { mwu_postag_frame_stem(Frame,Stem,Tags,Stems) },
    mwu_tags_stems(Tags,Stems,Q0,Q).

mwu_postag(Frame,Stem,_Surf,Q0,Q,_Result) -->
    { mwu_postag_frame_stem(Frame,Stem,Tags) },
    mwu_tags(Tags,Stem,1,Q0,Q).

mwu_postag(_Frame,Stem,_Surf,Q0,Q,_Result) -->
    { mwu_postag(Stem,Tags,Stems) },
    mwu_tags_stems(Tags,Stems,Q0,Q).

mwu_postag(_Frame,Stem,Surf,Q0,Q,_Result) -->
    { mwu_postag(Stem,Surf,Tags,Stems) },
    mwu_tags_stems(Tags,Stems,Q0,Q).
mwu_postag(proper_name(_),Stem,_Surf,Q0,Q,_Result) -->
    mwu_name_tags(Stem,Q0,Q).
mwu_postag(proper_name(_,_),Stem,_,Q0,Q,_Result) -->
    mwu_name_tags(Stem,Q0,Q).

%% en/of written as 'en / of'
mwu_postag(_Tag,Stem,Surf,Q0,Q,_) -->
    {  atom(Surf),
       alpino_util:split_atom(Surf," ",SurfEls),
       length(SurfEls,Len),
       (   hdrug_util:concat_all(SurfEls,Stem,'')
       ;   hdrug_util:concat_all(SurfEls,Stem,'-')
       ),
       Len is Q-Q0
    },
    guess_tag_list(SurfEls,Q0,Q).

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
    { punct(Punct) },
    !,
    [cgn_postag(Q0,Q,Punct,'LET()')].
mwu_name_tag(Punct,Q0,Q) -->
    [cgn_postag(Q0,Q,Punct,'SPEC(deeleigen)')].

punct(',').
punct('.').
punct('(').
punct(')').
punct('-').
punct('"').  % "
punct('\'').
punct('/').
punct('!').
punct('?').


mwu_tags_stems([],_,Q,Q) --> [].
mwu_tags_stems([H|T],[Stem|Stems],Q0,Q) -->
    [cgn_postag(Q0,Q1,Stem,H)],
    { Q1 is Q0 + 1 },
    mwu_tags_stems(T,Stems,Q1,Q).

mwu_tags([],_,_,Q,Q) --> [].
mwu_tags([H|T],Stem,Pos,Q0,Q) -->
    [cgn_postag(Q0,Q1,Pos/Stem,H)],
    { Q1 is Q0 + 1,
      Pos1 is Pos + 1},
    mwu_tags(T,Stem,Pos1,Q1,Q).

%% ADJ(nom,sup,zonder,zonder-n)

%% todo: kunnen ook SPEC(symb) zijn etc.
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

mwu_postag_frame_surf(with_dt(np(year),_),Surf,[ATag,'LET()',BTag]) :-
    atom(Surf),
    atom_codes(Surf,Codes),
    alpino_util:codes_to_words(Codes,[A,'-',B]),
    num_postag(A,ATag),
    num_postag(B,BTag).    

mwu_postag_frame_surf(score_cat,Surf,[ATag,'LET()',BTag]) :-
    atom(Surf),
    atom_codes(Surf,Codes),
    alpino_util:codes_to_words(Codes,[A,'-',B]),
    num_postag(A,ATag),
    num_postag(B,BTag).

mwu_postag_frame_surf(fixed_part(op_een_v),_,
       ['VZ(init)','LID(onbep,stan,agr)','WW(inf,nom,zonder,zonder-n)']).


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
mwu_postag_frame_stem_surf(adjective(het_st(_)),_,Surf,['VZ(init)',Pron,'ADJ(nom,sup,zonder,zonder-n)']) :-
    atom(Surf),
    atom_codes(Surf,Codes),
    alpino_util:codes_to_words(Codes,[Op,Zijn,_Vroegst]),
    op(Op),
    zijn_tag(Zijn,Pron,_).

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

mwu_postag_frame_stem(with_dt(adverb,dt(advp,[mod=l(lang,adjective(er(tmpadv)),ap,1,2),
					      hd=l(niet,adverb,0,1)])),
		      'lang niet',['BW()','ADJ(vrij,comp,zonder)'],[niet,lang]).

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
mwu_postag_frame_stem(tmp_np,Stem,PosTags) :-
    atom(Stem),
    atom_codes(Stem,Codes),
    alpino_util:split_string(Codes," ",Words),
    tmp_np(Words,PosTags).
mwu_postag_frame_stem(np(year),Stem,PosTags) :-
    atom(Stem),
    atom_codes(Stem,Codes),
    alpino_util:split_string(Codes," ",Words),
    tmp_np(Words,PosTags).
mwu_postag_frame_stem(noun(het,count,pl),gelijkspel,['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)']).


tmp_np([],[]).
tmp_np([W|Ws],[P|Ps]) :-
    tmp_np1(W,P),
    tmp_np(Ws,Ps).

tmp_np1("+",'SPEC(symb)').
tmp_np1(String,'N(eigen,ev,basis,zijd,stan)') :-
    atom_codes(Atom,String),
    alpino_lex:date_month([Atom],[]).
tmp_np1([48,N|_],'SPEC(symb)') :-  % 010 070 0345
    N > 47, N < 58.
tmp_np1(String,'TW(hoofd,vrij)'):-
    alpino_lex:number_codes_silent(Number,String),
    integer(Number).
tmp_np1(String,'SPEC(symb)') :-
    atom_codes(Atom,String),
    alpino_lex:num_dot_num(_,Atom).
tmp_np1(String,'TW(hoofd,vrij)'):-
    alpino_lex:number_codes_silent(_,String).
tmp_np1("half",'ADJ(prenom,basis,zonder)').
tmp_np1("kwart",'N(soort,ev,basis,onz,stan)').
tmp_np1("uur",'N(soort,ev,basis,onz,stan)').
tmp_np1("voor",'VZ(init)').
tmp_np1("na",'VZ(init)').
tmp_np1("tot",'VZ(init)').
tmp_np1("met",'VZ(init)').
tmp_np1("en",'VG(neven)').
tmp_np1("over",'VZ(init)').
tmp_np1("v.",'SPEC(afk)').
tmp_np1("v.Chr.",'SPEC(afk)').
tmp_np1("v.C.",'SPEC(afk)').
tmp_np1("Chr.",'SPEC(afk)').
tmp_np1("Chr",'SPEC(afk)').
tmp_np1("Christus",'N(eigen,ev,basis,zijd,stan)').
tmp_np1(_,'TW(hoofd,vrij)').


op(op).
op('Op').
op('OP').

zijn_tag(zijn,'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)', zijn).
zijn_tag('z\'n','VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)', zijn).
zijn_tag('haar','VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)', haar).
zijn_tag('d\'r','VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)', haar).
zijn_tag(mijn,'VNW(bez,det,stan,vol,1,ev,prenom,zonder,agr)', mijn).
zijn_tag('m\'n','VNW(bez,det,stan,red,1,ev,prenom,zonder,agr)',mijn).
zijn_tag(hun,'VNW(bez,det,stan,vol,3,mv,prenom,zonder,agr)',hum).
zijn_tag(je,'VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)',je).

num_postag(A,'TW(hoofd,vrij)') :-
    atom(A),
    atom_codes(A,ACodes),
    alpino_lex:number_codes_silent(Anum,ACodes),
    integer(Anum),!.
num_postag(',','LET()').
num_postag(')','LET()').
num_postag('(','LET()').
num_postag('-','LET()').
num_postag('/','LET()').

num_postag(_A,'SPEC(symb)').
%    alpino_lex:num_dot_num(_,A).

%% mwu_postag(lemma,surf,tags,newlemma)
mwu_postag(voorzover,'voor zover',
	   ['VZ(init)','BW()'],
	   [voor,zover]).
mwu_postag('mogelijk zoveel',_,
	   ['TW(hoofd,vrij)','ADJ(vrij,basis,zonder)'],
	   [zoveel,mogelijk]).
mwu_postag('aantal een',_,
	   ['LID(onbep,stan,agr)','N(soort,ev,basis,onz,stan)'],
	   [een,aantal]).
mwu_postag('hard om','om het hardst',
	   ['VZ(init)','LID(bep,stan,evon)','ADJ(vrij,sup,zonder)'],
	   [om,het,hard]).


%% da 's
mwu_postag(v_root(ben,zijn),'da \'s',
	   ['VNW(aanw,pron,stan,vol,3o,ev)','WW(pv,tgw,ev)'],
	   [dat,v_root(ben,zijn)]).
mwu_postag('als het ware','als het ware',
	   ['VG(onder)','VNW(pers,pron,stan,red,3,ev,onz)','WW(pv,conj,ev)'],
	   [als,het,v_root(ben,zijn)]).

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
    (   frame_map(Frame0,Stem0,Frame)
    ->  true
    ;   Frame0 = Frame
    ),
    cgn_postag_l2(Stem0,Stem,Frame,Tag).

cgn_postag_l2(bepaald,bepalen,adjective(ge_no_e(adv)),'WW(vd,vrij,zonder)').

cgn_postag_l2(Stem,Stem,Frame,Tag) :-
    stem_dependent_tag(Frame,Stem,Tag), !.
cgn_postag_l2(Stem,Stem2,Frame,Tag) :-
    exceptional_stem_tag(Stem,Frame,Tag,Stem2), !.
cgn_postag_l2(Stem,Stem,Frame,Tag) :-
    exceptional_stem_tag(Stem,Frame,Tag), !.
cgn_postag_l2(Stem,Stem,Frame,Tag) :-
    cgn_postag_c(Stem,Frame,Tag), !.
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


number_nom_path([num_num_adv_num/2|Path]) :-
    number_nom_path(Path).
number_nom_path([n_num/1,n_n_pps/1|_]).
number_nom_path([n_num/1,np_n/1,rel_pp_np_dp/2|_]).
number_nom_path([pron_pron_pps/1|_]).

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

od_is_adj(aan_palen,aanpalend).
od_is_adj(door_gaan,doorgaand).
od_is_adj(overwegen,overwegend).
od_is_adj(aanstaand,aanstaand).

vd_is_adj(aangeboren,aangeboren).
vd_is_adj(begaan,begaan).
vd_is_adj(benauwen,benauwd).
vd_is_adj(benieuwen,benieuwd).
vd_is_adj(bereid,bereid).
vd_is_adj(beroemd,beroemd).
vd_is_adj(beschamen,beschaamd).
vd_is_adj(bezorgd,bezorgd).
vd_is_adj(bijgenaamd,bijgenaamd).
vd_is_adj(baren,gebaard).
vd_is_adj(geleden,geleden).
vd_is_adj(gemiddeld,gemiddeld).
vd_is_adj(genaamd,genaamd).
vd_is_adj(rimpelen,gerimpeld).
vd_is_adj(geschikt,geschikt).
vd_is_adj(tinten,getint).
vd_is_adj(gewond,gewond).
vd_is_adj(goedgemutst,goedgemutst).
vd_is_adj(in_tijgen,ingetogen).
vd_is_adj(ongewenst,ongewenst).
vd_is_adj(ontstellen,ontsteld).
vd_is_adj(op_luchten,opgelucht).
vd_is_adj(tegenover_stellen,tegenovergesteld).
vd_is_adj(verschuldigd,verschuldigd).
vd_is_adj(verbijten,verbeten).
vd_is_adj(vertrouwd,vertrouwd).
vd_is_adj(wereldberoemd,wereldberoemd).
vd_is_adj(zelfverklaard,zelfverklaard).
vd_is_adj(zogeheten,zogeheten).
vd_is_adj(zogenaamd,zogenaamd).
vd_is_adj(zogenoemd,zogenoemd).

frame_map(adjective(postn_no_e(X)),_,         adjective(no_e(X))).
frame_map(adjective(postn_pred(X)),_,         adjective(pred(X))).
frame_map(adjective(postn_both(X)),_,         adjective(both(X))).
frame_map(adjective(X,_),_,                   adjective(X)).
frame_map(adjective(er(A)),    ander,         adjective(no_e(A))).
frame_map(adjective(er(A),_),  ander,         adjective(no_e(A))).
frame_map(adjective(ere),      ander,         adjective(e)).
frame_map(adjective(ere,_),    ander,         adjective(e)).
frame_map(post_adjective(er),  ander,         post_adjective(no_e)).
frame_map(post_adjective(er,_),ander,         post_adjective(no_e)).
frame_map(me_adjective(X),_,                  adjective(X)).
frame_map(me_adjective(X,_),_,                adjective(X)).
frame_map(np_me_adjective(_,X),_,             np_me_adjective(X)).
frame_map(post_adjective(X,_),_,              post_adjective(X)).
frame_map(noun(A,B,C,_),_,                    noun(A,B,C)).
frame_map(tmp_noun(A,B,C),_,                  noun(A,B,C)).
frame_map(tmp_noun(A,B,C,_),_,                noun(A,B,C)).
frame_map(mod_noun(A,B,C),_,                  noun(A,B,C)).
frame_map(mod_noun(A,B,C,_),_,                noun(A,B,C)).
frame_map(meas_mod_noun(both,count,meas),procent,noun(het,count,meas)).
frame_map(meas_mod_noun(A,B,C),_,             noun(A,B,C)).
frame_map(meas_mod_noun(A,B,C,_),_,           noun(A,B,C)).

vreemd_lemma(consolatio).
vreemd_lemma(fancy).
vreemd_lemma(fatwa).
vreemd_lemma(licensee).
vreemd_lemma(onsite).
vreemd_lemma(passphrase).
vreemd_lemma(vice).
vreemd_lemma(versa).
vreemd_lemma(warlord).
vreemd_lemma(warlords).


lassy('MISSING','SPEC(vreemd)').
lassy('PARA','SPEC(vreemd)').


lassy(de, 'LID(bep,stan,rest)').
lassy('.', 'LET()').
lassy(van, 'VZ(init)').
lassy(',', 'LET()').
lassy(en, 'VG(neven)').
lassy(het, 'LID(bep,stan,evon)').
lassy(een, 'LID(onbep,stan,agr)').
lassy(in, 'VZ(init)').
lassy(is, 'WW(pv,tgw,ev)').
lassy(te, 'VZ(init)').
lassy(op, 'VZ(init)').
lassy('De', 'LID(bep,stan,rest)').
lassy(voor, 'VZ(init)').
lassy(met, 'VZ(init)').
lassy(')', 'LET()').
lassy('(', 'LET()').
lassy(die, 'VNW(betr,pron,stan,vol,persoon,getal)').
lassy(dat, 'VG(onder)').
lassy(niet, 'BW()').
lassy(om, 'VZ(init)').
lassy(aan, 'VZ(init)').
lassy(:, 'LET()').
lassy(door, 'VZ(init)').
lassy(zijn, 'WW(pv,tgw,mv)').
lassy(ook, 'BW()').
lassy(er, 'VNW(aanw,adv-pron,stan,red,3,getal)').
lassy('\'', 'LET()').
lassy(-, 'LET()').
lassy('"', 'LET()').  % "
lassy(was, 'WW(pv,verl,ev)').
lassy(bij, 'VZ(init)').
lassy(zijn, 'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)').
lassy(of, 'VG(neven)').
lassy(tot, 'VZ(init)').
lassy('In', 'VZ(init)').
lassy(werd, 'WW(pv,verl,ev)').
lassy(naar, 'VZ(init)').
lassy(wordt, 'WW(pv,tgw,met-t)').
lassy(het, 'VNW(pers,pron,stan,red,3,ev,onz)').
lassy('Het', 'LID(bep,stan,evon)').
lassy(heeft, 'WW(pv,tgw,met-t)').
lassy(hij, 'VNW(pers,pron,nomin,vol,3,ev,masc)').
lassy(over, 'VZ(init)').
lassy(nog, 'BW()').
lassy(als, 'VZ(init)').
lassy(zich, 'VNW(refl,pron,obl,red,3,getal)').
lassy(worden, 'WW(inf,vrij,zonder)').
lassy(jaar, 'N(soort,ev,basis,onz,stan)').
lassy(maar, 'VG(neven)').
lassy(uit, 'VZ(init)').
lassy(hun, 'VNW(bez,det,stan,vol,3,mv,prenom,zonder,agr)').
lassy(deze, 'VNW(aanw,det,stan,prenom,met-e,rest)').
lassy(kan, 'WW(pv,tgw,ev)').
lassy(u, 'VNW(pers,pron,nomin,vol,2b,getal)').
lassy(meer, 'VNW(onbep,grad,stan,vrij,zonder,comp)').
lassy(ze, 'VNW(pers,pron,stan,red,3,mv)').
lassy(geen, 'VNW(onbep,det,stan,prenom,zonder,agr)').
lassy(als, 'VG(onder)').
lassy('Een', 'LID(onbep,stan,agr)').
lassy(dan, 'BW()').
lassy(al, 'BW()').
lassy(dan, 'VG(onder)').
lassy(hebben, 'WW(pv,tgw,mv)').
lassy('Het', 'VNW(pers,pron,stan,red,3,ev,onz)').
lassy(onder, 'VZ(init)').
lassy(worden, 'WW(pv,tgw,mv)').
lassy(nieuwe, 'ADJ(prenom,basis,met-e,stan)').
lassy('Maar', 'VG(neven)').
lassy(op, 'VZ(fin)').
lassy(dat, 'VNW(betr,pron,stan,vol,3,ev)').
lassy(moet, 'WW(pv,tgw,ev)').
lassy(and, 'SPEC(vreemd)').
lassy(andere, 'ADJ(prenom,basis,met-e,stan)').
lassy(die, 'VNW(aanw,det,stan,prenom,zonder,rest)').
lassy(;, 'LET()').
lassy(zou, 'WW(pv,verl,ev)').
lassy(?, 'LET()').
lassy('En', 'VG(neven)').
lassy(zal, 'WW(pv,tgw,ev)').
lassy(nu, 'BW()').
lassy(dat, 'VNW(aanw,pron,stan,vol,3o,ev)').
lassy(zijn, 'WW(inf,vrij,zonder)').
lassy(na, 'VZ(init)').
lassy(tegen, 'VZ(init)').
lassy(eerste, 'TW(rang,prenom,stan)').
lassy(wel, 'BW()').
lassy(kunnen, 'WW(pv,tgw,mv)').
lassy(tussen, 'VZ(init)').
lassy(werden, 'WW(pv,verl,mv)').
lassy(had, 'WW(pv,verl,ev)').
lassy(zo, 'BW()').
lassy(mensen, 'N(soort,mv,basis)').
lassy(aan, 'VZ(fin)').
lassy(twee, 'TW(hoofd,prenom,stan)').
lassy(dit, 'VNW(aanw,det,stan,prenom,zonder,evon)').
lassy(waren, 'WW(pv,verl,mv)').
lassy('Op', 'VZ(init)').
lassy(uit, 'VZ(fin)').
lassy('Nederland', 'N(eigen,ev,basis,onz,stan)').
lassy(alle, 'VNW(onbep,det,stan,prenom,met-e,agr)').
lassy('Hij', 'VNW(pers,pron,nomin,vol,3,ev,masc)').
lassy(we, 'VNW(pers,pron,nomin,red,1,mv)').
lassy('Ook', 'BW()').
lassy(grote, 'ADJ(prenom,basis,met-e,stan)').
lassy(waar, 'VNW(vb,adv-pron,obl,vol,3o,getal)').
lassy('Vlaamse', 'ADJ(prenom,basis,met-e,stan)').
lassy(gaat, 'WW(pv,tgw,met-t)').
lassy(/, 'LET()').
lassy(wat, 'VNW(vb,pron,stan,vol,3o,ev)').
lassy('Dat', 'VNW(aanw,pron,stan,vol,3o,ev)').
lassy(zoals, 'VG(onder)').
lassy(haar, 'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)').
lassy(ik, 'VNW(pers,pron,nomin,vol,1,ev)').
lassy('Europese', 'ADJ(prenom,basis,met-e,stan)').
lassy('Er', 'VNW(aanw,adv-pron,stan,red,3,getal)').
lassy(in, 'VZ(fin)').
lassy(wil, 'WW(pv,tgw,ev)').
lassy(weer, 'BW()').
lassy(je, 'VNW(pers,pron,nomin,red,2v,ev)').
lassy(jaren, 'N(soort,mv,basis)').
lassy(omdat, 'VG(onder)').
lassy(uw, 'VNW(bez,det,stan,vol,2,getal,prenom,zonder,agr)').
lassy('Nederlandse', 'ADJ(prenom,basis,met-e,stan)').
lassy(veel, 'VNW(onbep,grad,stan,prenom,zonder,agr,basis)').
lassy(alleen, 'BW()').
lassy(aantal, 'N(soort,ev,basis,onz,stan)').
lassy(dus, 'BW()').
lassy('Dit', 'VNW(aanw,pron,stan,vol,3o,ev)').
lassy(land, 'N(soort,ev,basis,onz,stan)').
lassy(«, 'LET()').
lassy(», 'LET()').
lassy(ze, 'VNW(pers,pron,stan,red,3,ev,fem)').
lassy('Deze', 'VNW(aanw,det,stan,prenom,met-e,rest)').
lassy('België', 'N(eigen,ev,basis,onz,stan)').
lassy(moeten, 'WW(pv,tgw,mv)').
lassy(hem, 'VNW(pers,pron,obl,vol,3,ev,masc)').
lassy('Voor', 'VZ(init)').
lassy(daar, 'VNW(aanw,adv-pron,obl,vol,3o,getal)').
lassy(maken, 'WW(inf,vrij,zonder)').
lassy(tijd, 'N(soort,ev,basis,zijd,stan)').
lassy(steeds, 'BW()').
lassy(echter, 'BW()').
lassy(minister, 'N(soort,ev,basis,zijd,stan)').
lassy(eigen, 'ADJ(prenom,basis,zonder)').
lassy('Als', 'VG(onder)').
lassy(zelf, 'BW()').
lassy(komt, 'WW(pv,tgw,met-t)').
lassy(af, 'VZ(fin)').
lassy(landen, 'N(soort,mv,basis)').
lassy('Bij', 'VZ(init)').
lassy(meer, 'VNW(onbep,grad,stan,prenom,zonder,agr,comp)').
lassy(drie, 'TW(hoofd,prenom,stan)').
lassy(tijdens, 'VZ(init)').
lassy(kunnen, 'WW(inf,vrij,zonder)').
lassy(deel, 'N(soort,ev,basis,onz,stan)').
lassy(dit, 'VNW(aanw,pron,stan,vol,3o,ev)').
lassy(regering, 'N(soort,ev,basis,zijd,stan)').
lassy(onderzoek, 'N(soort,ev,basis,onz,stan)').
lassy(per, 'VZ(init)').
lassy(hebben, 'WW(inf,vrij,zonder)').
lassy(euro, 'N(soort,ev,basis,zijd,stan)').
lassy(vooral, 'BW()').
lassy(veel, 'VNW(onbep,grad,stan,vrij,zonder,basis)').
lassy(plaats, 'N(soort,ev,basis,zijd,stan)').
lassy(toe, 'VZ(fin)').
lassy('Amerikaanse', 'ADJ(prenom,basis,met-e,stan)').
lassy('Brussel', 'N(eigen,ev,basis,onz,stan)').
lassy(via, 'VZ(init)').
lassy(goed, 'ADJ(vrij,basis,zonder)').
lassy(gaan, 'WW(inf,vrij,zonder)').
lassy(kwam, 'WW(pv,verl,ev)').
lassy('1', 'TW(hoofd,vrij)').
lassy(verschillende, 'ADJ(prenom,basis,met-e,stan)').
lassy(..., 'LET()').
lassy(mee, 'VZ(fin)').
lassy('Ik', 'VNW(pers,pron,nomin,vol,1,ev)').
lassy(maar, 'BW()').
lassy(mogelijk, 'ADJ(vrij,basis,zonder)').
lassy(te, 'BW()').
lassy(één, 'TW(hoofd,prenom,stan)').
lassy(binnen, 'VZ(init)').
lassy(die, 'VNW(aanw,pron,stan,vol,3,getal)').
lassy('De', 'SPEC(deeleigen)').
lassy('Van', 'SPEC(deeleigen)').
lassy(stad, 'N(soort,ev,basis,zijd,stan)').
lassy('Met', 'VZ(init)').
lassy(zullen, 'WW(pv,tgw,mv)').
lassy(zonder, 'VZ(init)').
lassy(staat, 'WW(pv,tgw,met-t)').
lassy(hoe, 'BW()').
lassy(enkele, 'VNW(onbep,det,stan,prenom,met-e,rest)').
lassy(uur, 'N(soort,ev,basis,onz,stan)').
lassy(bijvoorbeeld, 'BW()').
lassy('Vlaanderen', 'N(eigen,ev,basis,onz,stan)').
lassy(toch, 'BW()').
lassy(dag, 'N(soort,ev,basis,zijd,stan)').
lassy(ten, 'VZ(versm)').
lassy(dat, 'VNW(aanw,det,stan,prenom,zonder,evon)').
lassy('Na', 'VZ(init)').
lassy('Belgische', 'ADJ(prenom,basis,met-e,stan)').
lassy('%', 'SPEC(symb)').
lassy(miljoen, 'N(soort,ev,basis,onz,stan)').
lassy(vandaag, 'BW()').
lassy(onze, 'VNW(bez,det,stan,vol,1,mv,prenom,met-e,rest)').
lassy(laatste, 'ADJ(prenom,sup,met-e,stan)').
lassy('Ze', 'VNW(pers,pron,stan,red,3,mv)').
lassy(wereld, 'N(soort,ev,basis,zijd,stan)').
lassy(ontwikkeling, 'N(soort,ev,basis,zijd,stan)').
lassy(altijd, 'BW()').
lassy(van, 'SPEC(deeleigen)').
lassy(terug, 'BW()').
lassy(zelfs, 'BW()').
lassy(men, 'VNW(pers,pron,nomin,red,3p,ev,masc)').
lassy(kinderen, 'N(soort,mv,basis)').
lassy(later, 'ADJ(vrij,comp,zonder)').
lassy(hadden, 'WW(pv,verl,mv)').
lassy(sinds, 'VZ(init)').
lassy(overheid, 'N(soort,ev,basis,zijd,stan)').
lassy('Zo', 'BW()').
lassy(samen, 'BW()').
lassy(elkaar, 'VNW(recip,pron,obl,vol,persoon,mv)').
lassy(kon, 'WW(pv,verl,ev)').
lassy(zouden, 'WW(pv,verl,mv)').
lassy(voor, 'VZ(fin)').
lassy('Duitsland', 'N(eigen,ev,basis,onz,stan)').
lassy(volgens, 'VZ(init)').
lassy(politieke, 'ADJ(prenom,basis,met-e,stan)').
lassy(informatie, 'N(soort,ev,basis,zijd,stan)').
lassy(procent, 'N(soort,ev,basis,onz,stan)').
lassy(hier, 'VNW(aanw,adv-pron,obl,vol,3o,getal)').
lassy(doen, 'WW(inf,vrij,zonder)').
lassy('Volgens', 'VZ(init)').
lassy('Duitse', 'ADJ(prenom,basis,met-e,stan)').
lassy(leven, 'N(soort,ev,basis,onz,stan)').
lassy(laten, 'WW(inf,vrij,zonder)').
lassy(kunt, 'WW(pv,tgw,met-t)').
lassy(heel, 'ADJ(vrij,basis,zonder)').
lassy(gemaakt, 'WW(vd,vrij,zonder)').
lassy(nodig, 'ADJ(vrij,basis,zonder)').
lassy(volgende, 'WW(od,prenom,met-e)').
lassy(waarin, 'BW()').
lassy(partij, 'N(soort,ev,basis,zijd,stan)').
lassy('Europa', 'N(eigen,ev,basis,onz,stan)').
lassy(zij, 'VNW(pers,pron,nomin,vol,3p,mv)').
lassy(komen, 'WW(inf,vrij,zonder)').
lassy(werk, 'N(soort,ev,basis,onz,stan)').
lassy(onderwijs, 'N(soort,ev,basis,onz,stan)').
lassy(of, 'VG(onder)').
lassy(mag, 'WW(pv,tgw,ev)').
lassy(geld, 'N(soort,ev,basis,onz,stan)').
lassy(gebied, 'N(soort,ev,basis,onz,stan)').
lassy(·, 'LET()').
lassy(!, 'LET()').
lassy(zegt, 'WW(pv,tgw,met-t)').
lassy(eeuw, 'N(soort,ev,basis,zijd,stan)').
lassy(rond, 'VZ(init)').
lassy(oorlog, 'N(soort,ev,basis,zijd,stan)').
lassy(politie, 'N(soort,ev,basis,zijd,stan)').
lassy(tweede, 'TW(rang,prenom,stan)').
lassy(internationale, 'ADJ(prenom,basis,met-e,stan)').
lassy('Om', 'VZ(init)').
lassy(snel, 'ADJ(vrij,basis,zonder)').
lassy(ging, 'WW(pv,verl,ev)').
lassy(de, 'SPEC(deeleigen)').
lassy(naam, 'N(soort,ev,basis,zijd,stan)').
lassy(september, 'N(eigen,ev,basis,zijd,stan)').
lassy('Door', 'VZ(init)').
lassy(zeer, 'BW()').
lassy(hele, 'ADJ(prenom,basis,met-e,stan)').
lassy(over, 'VZ(fin)').
lassy(krijgt, 'WW(pv,tgw,met-t)').
lassy('2003', 'TW(hoofd,vrij)').
lassy(week, 'N(soort,ev,basis,zijd,stan)').
lassy(ter, 'VZ(versm)').
lassy(blijft, 'WW(pv,tgw,met-t)').
lassy(sociale, 'ADJ(prenom,basis,met-e,stan)').
lassy(man, 'N(soort,ev,basis,zijd,stan)').
lassy(grootste, 'ADJ(prenom,sup,met-e,stan)').
lassy(belangrijke, 'ADJ(prenom,basis,met-e,stan)').
lassy(komen, 'WW(pv,tgw,mv)').
lassy(zien, 'WW(inf,vrij,zonder)').
lassy(vaak, 'ADJ(vrij,basis,zonder)').
lassy(nemen, 'WW(inf,vrij,zonder)').
lassy(blijkt, 'WW(pv,tgw,met-t)').
lassy(vindt, 'WW(pv,tgw,met-t)').
lassy(bekend, 'ADJ(vrij,basis,zonder)').
lassy('Unie', 'N(soort,ev,basis,zijd,stan)').
lassy('Franse', 'ADJ(prenom,basis,met-e,stan)').
lassy('2004', 'TW(hoofd,vrij)').
lassy(hen, 'VNW(pers,pron,obl,vol,3p,mv)').
lassy(vier, 'TW(hoofd,prenom,stan)').
lassy(vanaf, 'VZ(init)').
lassy(gebruik, 'N(soort,ev,basis,onz,stan)').
lassy(duidelijk, 'ADJ(vrij,basis,zonder)').
lassy(manier, 'N(soort,ev,basis,zijd,stan)').
lassy(toen, 'VG(onder)').
lassy('2000', 'TW(hoofd,vrij)').
lassy(opnieuw, 'BW()').
lassy(aanval, 'N(soort,ev,basis,zijd,stan)').
lassy(moest, 'WW(pv,verl,ev)').
lassy(eens, 'BW()').
lassy(dagen, 'N(soort,mv,basis)').
lassy(president, 'N(soort,ev,basis,zijd,stan)').
lassy('2005', 'TW(hoofd,vrij)').
lassy(staat, 'N(soort,ev,basis,zijd,stan)').
lassy(geven, 'WW(inf,vrij,zonder)').
lassy(periode, 'N(soort,ev,basis,zijd,stan)').
lassy(maakt, 'WW(pv,tgw,met-t)').
lassy(gebruikt, 'WW(vd,vrij,zonder)').
lassy(basis, 'N(soort,ev,basis,zijd,stan)').
lassy('Irak', 'N(eigen,ev,basis,onz,stan)').
lassy(moeten, 'WW(inf,vrij,zonder)').
lassy(economische, 'ADJ(prenom,basis,met-e,stan)').
lassy(wat, 'VNW(onbep,pron,stan,vol,3o,ev)').
lassy(nooit, 'BW()').
lassy(waarbij, 'BW()').
lassy(geleden, 'BW()').
lassy(houden, 'WW(inf,vrij,zonder)').
lassy(toen, 'BW()').
lassy(juni, 'N(eigen,ev,basis,zijd,stan)').
lassy(school, 'N(soort,ev,basis,zijd,stan)').
lassy(samenwerking, 'N(soort,ev,basis,zijd,stan)').
lassy(januari, 'N(eigen,ev,basis,zijd,stan)').
lassy(epilepsie, 'N(soort,ev,basis,zijd,stan)').
lassy('Britse', 'ADJ(prenom,basis,met-e,stan)').
lassy('We', 'VNW(pers,pron,nomin,red,1,mv)').
lassy(groot, 'ADJ(prenom,basis,zonder)').
lassy(bijna, 'BW()').
lassy(iets, 'VNW(onbep,pron,stan,vol,3o,ev)').
lassy(krijgen, 'WW(inf,vrij,zonder)').
lassy(zowel, 'BW()').
lassy(want, 'VG(neven)').
lassy('Frankrijk', 'N(eigen,ev,basis,onz,stan)').
lassy(lang, 'ADJ(vrij,basis,zonder)').
lassy(minder, 'VNW(onbep,grad,stan,vrij,zonder,comp)').
lassy(kind, 'N(soort,ev,basis,onz,stan)').
lassy(wie, 'VNW(vb,pron,stan,vol,3p,getal)').
lassy(geval, 'N(soort,ev,basis,onz,stan)').
lassy(bestaat, 'WW(pv,tgw,met-t)').
lassy(rol, 'N(soort,ev,basis,zijd,stan)').
lassy(leger, 'N(soort,ev,basis,onz,stan)').
lassy('Wat', 'VNW(vb,pron,stan,vol,3o,ev)').
lassy('U', 'VNW(pers,pron,nomin,vol,2b,getal)').
lassy(ongeveer, 'BW()').
lassy(begin, 'N(soort,ev,basis,onz,stan)').
lassy(afgelopen, 'WW(vd,prenom,zonder)').
lassy('Vlaams', 'ADJ(prenom,basis,zonder)').
lassy(kreeg, 'WW(pv,verl,ev)').
lassy('Dit', 'VNW(aanw,det,stan,prenom,zonder,evon)').
lassy('zo\'n', 'VNW(aanw,det,stan,prenom,zonder,agr)').
lassy(willen, 'WW(pv,tgw,mv)').
lassy(vrouwen, 'N(soort,mv,basis)').
lassy(mijn, 'VNW(bez,det,stan,vol,1,ev,prenom,zonder,agr)').
lassy(ligt, 'WW(pv,tgw,met-t)').
lassy(bedrijf, 'N(soort,ev,basis,onz,stan)').
lassy(vele, 'VNW(onbep,grad,stan,prenom,met-e,agr,basis)').
lassy(beter, 'ADJ(vrij,comp,zonder)').
lassy(beleid, 'N(soort,ev,basis,onz,stan)').
lassy(maanden, 'N(soort,mv,basis)').
lassy(belang, 'N(soort,ev,basis,onz,stan)').
lassy(mei, 'N(eigen,ev,basis,zijd,stan)').
lassy('Van', 'VZ(init)').
lassy(niets, 'VNW(onbep,pron,stan,vol,3o,ev)').
lassy(niks, 'VNW(onbep,pron,stan,vol,3o,ev)').
lassy(meest, 'VNW(onbep,grad,stan,vrij,zonder,sup)').
lassy(gaan, 'WW(pv,tgw,mv)').
lassy(ontwikkelingslanden, 'N(soort,mv,basis)').
lassy('Commissie', 'N(soort,ev,basis,zijd,stan)').
lassy(geweest, 'WW(vd,vrij,zonder)').
lassy(geeft, 'WW(pv,tgw,met-t)').
lassy(zeker, 'ADJ(vrij,basis,zonder)').
lassy(een, 'TW(hoofd,nom,zonder-n,basis)').
lassy(kader, 'N(soort,ev,basis,onz,stan)').
lassy(goede, 'ADJ(prenom,basis,met-e,stan)').
lassy(terwijl, 'VG(onder)').
lassy('2002', 'TW(hoofd,vrij)').
lassy(ouders, 'N(soort,mv,basis)').
lassy(net, 'BW()').
lassy(alles, 'VNW(onbep,pron,stan,vol,3o,ev)').
lassy(kleine, 'ADJ(prenom,basis,met-e,stan)').
lassy(water, 'N(soort,ev,basis,onz,stan)').
lassy(juli, 'N(eigen,ev,basis,zijd,stan)').
lassy(even, 'BW()').
lassy('Die', 'VNW(aanw,det,stan,prenom,zonder,rest)').
lassy(moment, 'N(soort,ev,basis,onz,stan)').
lassy(groep, 'N(soort,ev,basis,zijd,stan)').
lassy('2001', 'TW(hoofd,vrij)').
lassy(situatie, 'N(soort,ev,basis,zijd,stan)').
lassy(slechts, 'BW()').
lassy('Tweede', 'TW(rang,prenom,stan)').
lassy(ons, 'VNW(pr,pron,obl,vol,1,mv)').
lassy(weken, 'N(soort,mv,basis)').
lassy(kwamen, 'WW(pv,verl,mv)').
lassy(problemen, 'N(soort,mv,basis)').
lassy(film, 'N(soort,ev,basis,zijd,stan)').
lassy(einde, 'N(soort,ev,basis,onz,stan)').
lassy('Zijn', 'VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)').
lassy(maart, 'N(eigen,ev,basis,zijd,stan)').
lassy(hand, 'N(soort,ev,basis,zijd,stan)').
lassy(vraag, 'N(soort,ev,basis,zijd,stan)').
lassy(pas, 'BW()').
lassy(huidige, 'ADJ(prenom,basis,met-e,stan)').
lassy('2006', 'TW(hoofd,vrij)').
lassy(wet, 'N(soort,ev,basis,zijd,stan)').
lassy('Die', 'VNW(aanw,pron,stan,vol,3,getal)').
lassy(oude, 'ADJ(prenom,basis,met-e,stan)').
lassy(bij, 'VZ(fin)').
lassy(oktober, 'N(eigen,ev,basis,zijd,stan)').
lassy(doet, 'WW(pv,tgw,met-t)').
lassy(dezelfde, 'ADJ(prenom,basis,zonder)').
lassy(beide, 'VNW(onbep,grad,stan,prenom,met-e,mv,basis)').
lassy(bedrijven, 'N(soort,mv,basis)').
lassy(alcohol, 'N(soort,ev,basis,zijd,stan)').
lassy(vorm, 'N(soort,ev,basis,zijd,stan)').
lassy(duurzame, 'ADJ(prenom,basis,met-e,stan)').
lassy(werken, 'WW(inf,vrij,zonder)').
lassy('3', 'TW(hoofd,vrij)').
lassy(vanuit, 'VZ(init)').
lassy(belangrijkste, 'ADJ(prenom,sup,met-e,stan)').
lassy(vijf, 'TW(hoofd,prenom,stan)').
lassy(staan, 'WW(pv,tgw,mv)').
lassy(meeste, 'VNW(onbep,grad,stan,prenom,met-e,agr,sup)').
lassy(deze, 'VNW(aanw,det,stan,nom,met-e,zonder-n)').
lassy(daarom, 'BW()').
lassy(december, 'N(eigen,ev,basis,zijd,stan)').
lassy(zit, 'WW(pv,tgw,ev)').
lassy(weg, 'N(soort,ev,basis,zijd,stan)').
lassy(huis, 'N(soort,ev,basis,onz,stan)').
lassy('2', 'TW(hoofd,vrij)').
lassy(krijgen, 'WW(pv,tgw,mv)').
lassy(blijven, 'WW(inf,vrij,zonder)').
lassy(van, 'VZ(fin)').
lassy(gevolg, 'N(soort,ev,basis,onz,stan)').
lassy('Minister', 'N(soort,ev,basis,zijd,stan)').
lassy(verkiezingen, 'N(soort,mv,basis)').
lassy(kans, 'N(soort,ev,basis,zijd,stan)').
lassy(iemand, 'VNW(onbep,pron,stan,vol,3p,ev)').
lassy(*, 'LET()').
lassy(maakte, 'WW(pv,verl,ev)').
lassy(invloed, 'N(soort,ev,basis,zijd,stan)').
lassy(zes, 'TW(hoofd,prenom,stan)').
lassy(waarop, 'BW()').
lassy(augustus, 'N(eigen,ev,basis,zijd,stan)').
lassy(andere, 'ADJ(nom,basis,met-e,zonder-n,stan)').
lassy(toekomst, 'N(soort,ev,basis,zijd,stan)').
lassy(geweld, 'N(soort,ev,basis,onz,stan)').
lassy(gehouden, 'WW(vd,vrij,zonder)').
lassy(eind, 'N(soort,ev,basis,onz,stan)').
lassy(sterk, 'ADJ(vrij,basis,zonder)').
lassy('Antwerpen', 'N(eigen,ev,basis,onz,stan)').
lassy(hoge, 'ADJ(prenom,basis,met-e,stan)').
lassy(door, 'VZ(fin)').
lassy(woningen, 'N(soort,mv,basis)').
lassy(steun, 'N(soort,ev,basis,zijd,stan)').
lassy(lijkt, 'WW(pv,tgw,met-t)').
lassy(gegevens, 'N(soort,mv,basis)').
lassy('Ze', 'VNW(pers,pron,stan,red,3,ev,fem)').
lassy('10', 'TW(hoofd,prenom,stan)').
lassy(wanneer, 'VG(onder)').
lassy(strijd, 'N(soort,ev,basis,zijd,stan)').
lassy(patiënten, 'N(soort,mv,basis)').
lassy(opdracht, 'N(soort,ev,basis,zijd,stan)').
lassy(iedereen, 'VNW(onbep,pron,stan,vol,3p,ev)').
lassy('Zij', 'VNW(pers,pron,nomin,vol,3p,mv)').
lassy(elke, 'VNW(onbep,det,stan,prenom,met-e,evz)').
lassy('Hoe', 'BW()').
lassy(leden, 'N(soort,mv,basis)').
lassy(bevolking, 'N(soort,ev,basis,zijd,stan)').
lassy(meteen, 'BW()').
lassy(soms, 'BW()').
lassy(erg, 'ADJ(vrij,basis,zonder)').
lassy(brengen, 'WW(inf,vrij,zonder)').
lassy(premier, 'N(soort,ev,basis,zijd,stan)').
lassy(kabinet, 'N(soort,ev,basis,onz,stan)').
lassy('Want', 'VG(neven)').
lassy(heb, 'WW(pv,tgw,ev)').
lassy(ervan, 'BW()').
lassy('Tijdens', 'VZ(init)').
lassy('Italië', 'N(eigen,ev,basis,onz,stan)').
lassy('Gemeenschap', 'N(soort,ev,basis,zijd,stan)').
lassy(volledig, 'ADJ(vrij,basis,zonder)').
lassy(economie, 'N(soort,ev,basis,zijd,stan)').
lassy(aanvallen, 'N(soort,mv,basis)').
lassy('The', 'SPEC(deeleigen)').
lassy(macht, 'N(soort,ev,basis,zijd,stan)').
lassy(behandeling, 'N(soort,ev,basis,zijd,stan)').
lassy(maatregelen, 'N(soort,mv,basis)').
lassy(federale, 'ADJ(prenom,basis,met-e,stan)').
lassy(begon, 'WW(pv,verl,ev)').
lassy(zodat, 'VG(onder)').
lassy(vorige, 'ADJ(prenom,basis,met-e,stan)').
lassy(ruim, 'ADJ(vrij,basis,zonder)').
lassy(aandacht, 'N(soort,ev,basis,zijd,stan)').
lassy('Zaken', 'N(soort,mv,basis)').
lassy(',,', 'LET()').
lassy(gemeenten, 'N(soort,mv,basis)').
lassy(geldt, 'WW(pv,tgw,met-t)').
lassy(contact, 'N(soort,ev,basis,onz,stan)').
lassy(buiten, 'VZ(init)').
lassy(achter, 'VZ(init)').
lassy(waardoor, 'BW()').
lassy(eerder, 'BW()').
lassy('Daar', 'VNW(aanw,adv-pron,obl,vol,3o,getal)').
lassy(welke, 'VNW(vb,det,stan,prenom,met-e,rest)').
lassy(nationale, 'ADJ(prenom,basis,met-e,stan)').
lassy('Amsterdam', 'N(eigen,ev,basis,onz,stan)').
lassy('z\'n', 'VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)').
lassy(recht, 'N(soort,ev,basis,onz,stan)').
lassy(bleek, 'WW(pv,verl,ev)').
lassy('Zie', 'WW(pv,tgw,ev)').
lassy('Toch', 'BW()').
lassy(zaken, 'N(soort,mv,basis)').
lassy(zaak, 'N(soort,ev,basis,zijd,stan)').
lassy(nieuw, 'ADJ(prenom,basis,zonder)').
lassy('Verenigde', 'WW(vd,prenom,met-e)').
lassy(zij, 'VNW(pers,pron,nomin,vol,3v,ev,fem)').
lassy(markt, 'N(soort,ev,basis,zijd,stan)').
lassy(kwaliteit, 'N(soort,ev,basis,zijd,stan)').
lassy(jongeren, 'ADJ(nom,comp,met-e,mv-n)').
lassy(elk, 'VNW(onbep,det,stan,prenom,zonder,evon)').
lassy(producten, 'N(soort,mv,basis)').
lassy(november, 'N(eigen,ev,basis,zijd,stan)').
lassy(inzake, 'VZ(init)').
lassy(familie, 'N(soort,ev,basis,zijd,stan)').
lassy(extra, 'ADJ(prenom,basis,zonder)').
lassy(eerst, 'BW()').
lassy(april, 'N(eigen,ev,basis,zijd,stan)').
lassy('Wereldoorlog', 'N(soort,ev,basis,zijd,stan)').
lassy(sommige, 'VNW(onbep,det,stan,prenom,met-e,rest)').
lassy(partijen, 'N(soort,mv,basis)').
lassy(laat, 'WW(pv,tgw,ev)').
lassy(houdt, 'WW(pv,tgw,met-t)').
lassy(genomen, 'WW(vd,vrij,zonder)').
lassy('Tot', 'VZ(init)').
lassy(tien, 'TW(hoofd,prenom,stan)').
lassy(je, 'VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)').
lassy(hulp, 'N(soort,ev,basis,zijd,stan)').
lassy(gedaan, 'WW(vd,vrij,zonder)').
lassy(diensten, 'N(soort,mv,basis)').
lassy('EU', 'N(eigen,ev,basis,zijd,stan)').
lassy(waarvan, 'BW()').
lassy(plan, 'N(soort,ev,basis,onz,stan)').
lassy(miljard, 'N(soort,ev,basis,onz,stan)').
lassy(langer, 'ADJ(vrij,comp,zonder)').
lassy('Kamer', 'N(soort,ev,basis,zijd,stan)').
lassy(zware, 'ADJ(prenom,basis,met-e,stan)').
lassy(genoeg, 'BW()').
lassy(stellen, 'WW(inf,vrij,zonder)').
lassy(scholen, 'N(soort,mv,basis)').
lassy(rekening, 'N(soort,ev,basis,zijd,stan)').
lassy(dood, 'N(soort,ev,basis,zijd,stan)').
lassy('\'\'', 'LET()').
lassy(uiteindelijk, 'ADJ(vrij,basis,zonder)').
lassy(troepen, 'N(soort,mv,basis)').
lassy(genoemd, 'WW(vd,vrij,zonder)').
lassy('Amerikanen', 'N(eigen,mv,basis)').
lassy('4', 'TW(hoofd,vrij)').
lassy(immers, 'BW()').
lassy(februari, 'N(eigen,ev,basis,zijd,stan)').
lassy(name, 'N(soort,ev,basis,dat)').
lassy(keer, 'N(soort,ev,basis,genus,stan)').
lassy(ooit, 'BW()').
lassy(neemt, 'WW(pv,tgw,met-t)').
lassy('Den', 'SPEC(deeleigen)').
lassy(succes, 'N(soort,ev,basis,onz,stan)').
lassy(gevolgen, 'N(soort,mv,basis)').
lassy(boek, 'N(soort,ev,basis,onz,stan)').
lassy(activiteiten, 'N(soort,mv,basis)').
lassy(ons, 'VNW(bez,det,stan,vol,1,mv,prenom,zonder,evon)').
lassy(gemeente, 'N(soort,ev,basis,zijd,stan)').
lassy(financiële, 'ADJ(prenom,basis,met-e,stan)').
lassy(wij, 'VNW(pers,pron,nomin,vol,1,mv)').
lassy(stond, 'WW(pv,verl,ev)').
lassy(zie, 'WW(pv,tgw,ev)').
lassy(organisatie, 'N(soort,ev,basis,zijd,stan)').
lassy(maand, 'N(soort,ev,basis,zijd,stan)').
lassy(gaf, 'WW(pv,verl,ev)').
lassy('Aan', 'VZ(init)').
lassy(weten, 'WW(inf,vrij,zonder)').
lassy(vrouw, 'N(soort,ev,basis,zijd,stan)').
lassy(meestal, 'BW()').
lassy(lid, 'N(soort,ev,basis,onz,stan)').
lassy(der, 'LID(bep,gen,rest3)').
lassy(konden, 'WW(pv,verl,mv)').
lassy(geworden, 'WW(vd,vrij,zonder)').
lassy(bent, 'WW(pv,tgw,met-t)').
lassy(mogen, 'WW(pv,tgw,mv)').
lassy(leerlingen, 'N(soort,mv,basis)').
lassy(betekent, 'WW(pv,tgw,met-t)').
lassy(wilde, 'WW(pv,verl,ev)').
lassy(meter, 'N(soort,ev,basis,zijd,stan)').
lassy(hoofdstad, 'N(soort,ev,basis,zijd,stan)').
lassy(handel, 'N(soort,ev,basis,zijd,stan)').
lassy(dienst, 'N(soort,ev,basis,zijd,stan)').
lassy(weet, 'WW(pv,tgw,ev)').
lassy('Toen', 'VG(onder)').
lassy('Daarnaast', 'BW()').
lassy('DE', 'LID(bep,stan,rest)').
lassy(vinden, 'WW(inf,vrij,zonder)').
lassy(nam, 'WW(pv,verl,ev)').
lassy(lidstaten, 'N(soort,mv,basis)').
lassy(keer, 'N(soort,ev,basis,zijd,stan)').
lassy(doel, 'N(soort,ev,basis,onz,stan)').
lassy(daarbij, 'BW()').
lassy('Europees', 'ADJ(prenom,basis,zonder)').
lassy(zei, 'WW(pv,verl,ev)').
lassy('Als', 'VZ(init)').
lassy(derde, 'TW(rang,prenom,stan)').
lassy(der, 'SPEC(deeleigen)').
lassy(boven, 'VZ(init)').
lassy(allemaal, 'BW()').
lassy(probleem, 'N(soort,ev,basis,onz,stan)').
lassy(middelen, 'N(soort,mv,basis)').
lassy('\`', 'LET()').
lassy(politiek, 'N(soort,ev,basis,zijd,stan)').
lassy(gebracht, 'WW(vd,vrij,zonder)').
lassy('VS', 'N(eigen,mv,basis)').
lassy(weinig, 'VNW(onbep,grad,stan,vrij,zonder,basis)').
lassy(vinden, 'WW(pv,tgw,mv)').
lassy(stelt, 'WW(pv,tgw,met-t)').
lassy(militairen, 'N(soort,mv,basis)').
lassy(kerk, 'N(soort,ev,basis,zijd,stan)').
lassy(gezien, 'WW(vd,vrij,zonder)').
lassy(echt, 'ADJ(vrij,basis,zonder)').
lassy('Sinds', 'VZ(init)').
lassy('Raad', 'N(soort,ev,basis,zijd,stan)').
lassy('Onder', 'VZ(init)').
lassy('China', 'N(eigen,ev,basis,onz,stan)').
lassy(wijze, 'N(soort,ev,basis,zijd,stan)').
lassy(mannen, 'N(soort,mv,basis)').
lassy(lange, 'ADJ(prenom,basis,met-e,stan)').
lassy(een, 'TW(hoofd,vrij)').
lassy(anders, 'BW()').
lassy('20', 'TW(hoofd,prenom,stan)').
lassy('1995', 'TW(hoofd,vrij)').
lassy(zeggen, 'WW(inf,vrij,zonder)').
lassy(helemaal, 'BW()').
lassy(handen, 'N(soort,mv,basis)').
lassy(gevallen, 'N(soort,mv,basis)').
lassy(buitenlandse, 'ADJ(prenom,basis,met-e,stan)').
lassy(bleef, 'WW(pv,verl,ev)').
lassy(kort, 'ADJ(vrij,basis,zonder)').
lassy(koning, 'N(soort,ev,basis,zijd,stan)').
lassy('Staten', 'N(soort,mv,basis)').
lassy('Israëlische', 'ADJ(prenom,basis,met-e,stan)').
lassy(voorzitter, 'N(soort,ev,basis,zijd,stan)').
lassy(voorkomen, 'WW(inf,vrij,zonder)').
lassy(systeem, 'N(soort,ev,basis,onz,stan)').
lassy(reeds, 'BW()').
lassy(militaire, 'ADJ(prenom,basis,met-e,stan)').
lassy(daarvan, 'BW()').
lassy(lokale, 'ADJ(prenom,basis,met-e,stan)').
lassy(gegeven, 'WW(vd,vrij,zonder)').
lassy('Israël', 'N(eigen,ev,basis,onz,stan)').
lassy(bepaalde, 'WW(vd,prenom,met-e)').
lassy(verder, 'ADJ(vrij,comp,zonder)').
lassy(geschiedenis, 'N(soort,ev,basis,zijd,stan)').
lassy(gebieden, 'N(soort,mv,basis)').
lassy('Haag', 'SPEC(deeleigen)').
lassy(grond, 'N(soort,ev,basis,zijd,stan)').
lassy(resultaten, 'N(soort,mv,basis)').
lassy(provincie, 'N(soort,ev,basis,zijd,stan)').
lassy(opgenomen, 'WW(vd,vrij,zonder)').
lassy(ministerie, 'N(soort,ev,basis,onz,stan)').
lassy(liet, 'WW(pv,verl,ev)').
lassy(daarmee, 'BW()').
lassy('Niet', 'BW()').
lassy(vorig, 'ADJ(prenom,basis,zonder)').
lassy(regels, 'N(soort,mv,basis)').
lassy(één, 'TW(hoofd,vrij)').
lassy(werkt, 'WW(pv,tgw,met-t)').
lassy(waarmee, 'BW()').
lassy(valt, 'WW(pv,tgw,met-t)').
lassy(publiek, 'N(soort,ev,basis,onz,stan)').
lassy(kennis, 'N(soort,ev,basis,zijd,stan)').
lassy(helft, 'N(soort,ev,basis,zijd,stan)').
lassy('Brusselse', 'ADJ(prenom,basis,met-e,stan)').
lassy('5', 'TW(hoofd,vrij)').
lassy('3', 'TW(hoofd,prenom,stan)').
lassy(website, 'N(soort,ev,basis,zijd,stan)').
lassy(moesten, 'WW(pv,verl,mv)').
lassy(leiding, 'N(soort,ev,basis,zijd,stan)').
lassy(inwoners, 'N(soort,mv,basis)').
lassy(deed, 'WW(pv,verl,ev)').
lassy(anderen, 'ADJ(nom,basis,met-e,mv-n)').
lassy(enige, 'VNW(onbep,det,stan,prenom,met-e,rest)').
lassy(commissie, 'N(soort,ev,basis,zijd,stan)').
lassy(komende, 'WW(od,prenom,met-e)').
lassy(één, 'TW(hoofd,nom,zonder-n,basis)').
lassy(vrij, 'ADJ(vrij,basis,zonder)').
lassy(steden, 'N(soort,mv,basis)').
lassy(persoon, 'N(soort,ev,basis,zijd,stan)').
lassy(niveau, 'N(soort,ev,basis,onz,stan)').
lassy(juist, 'BW()').
lassy(beroep, 'N(soort,ev,basis,onz,stan)').
lassy(ander, 'ADJ(prenom,basis,zonder)').
lassy('Hoewel', 'VG(onder)').
lassy('Daarom', 'BW()').
lassy('Bovendien', 'BW()').
lassy(vlak, 'N(soort,ev,basis,onz,stan)').
lassy(namelijk, 'BW()').
lassy(korte, 'ADJ(prenom,basis,met-e,stan)').
lassy(al, 'VNW(onbep,det,stan,vrij,zonder)').
lassy(zichzelf, 'VNW(refl,pron,obl,nadr,3,getal)').
lassy(staan, 'WW(inf,vrij,zonder)').
lassy(risico, 'N(soort,ev,basis,onz,stan)').
lassy('1999', 'TW(hoofd,vrij)').
lassy('11', 'TW(hoofd,vrij)').
lassy(zetten, 'WW(inf,vrij,zonder)').
lassy(naast, 'VZ(init)').
lassy(kilometer, 'N(soort,ev,basis,zijd,stan)').
lassy(groot, 'ADJ(vrij,basis,zonder)').
lassy(druk, 'N(soort,ev,basis,zijd,stan)').
lassy(woning, 'N(soort,ev,basis,zijd,stan)').
lassy(gesteld, 'WW(vd,vrij,zonder)').
lassy(gekomen, 'WW(vd,vrij,zonder)').
lassy(betreft, 'WW(pv,tgw,met-t)').
lassy(algemene, 'ADJ(prenom,basis,met-e,stan)').
lassy('Nederlands', 'N(eigen,ev,basis,onz,stan)').
lassy('5', 'TW(hoofd,prenom,stan)').
lassy(verhaal, 'N(soort,ev,basis,onz,stan)').
lassy(terecht, 'ADJ(vrij,basis,zonder)').
lassy(maken, 'WW(pv,tgw,mv)').
lassy(diverse, 'ADJ(prenom,basis,met-e,stan)').
lassy(avond, 'N(soort,ev,basis,zijd,stan)').
lassy('Afghanistan', 'N(eigen,ev,basis,onz,stan)').
lassy('2', 'TW(hoofd,prenom,stan)').
lassy(weinig, 'VNW(onbep,grad,stan,prenom,zonder,agr,basis)').
lassy(slachtoffers, 'N(soort,mv,basis)').
lassy(dient, 'WW(pv,tgw,met-t)').
lassy(vragen, 'N(soort,mv,basis)').
lassy(vond, 'WW(pv,verl,ev)').
lassy(studie, 'N(soort,ev,basis,zijd,stan)').
lassy(medicijnen, 'N(soort,mv,basis)').
lassy(centrale, 'ADJ(prenom,basis,met-e,stan)').
lassy(beste, 'ADJ(prenom,sup,met-e,stan)').
lassy(zogenaamde, 'ADJ(prenom,basis,met-e,stan)').
lassy(waarschijnlijk, 'ADJ(vrij,basis,zonder)').
lassy(nadat, 'VG(onder)').
lassy(--, 'LET()').
lassy(reeks, 'N(soort,ev,basis,zijd,stan)').
lassy(misschien, 'BW()').
lassy('Nu', 'BW()').
lassy('Gewest', 'N(soort,ev,basis,onz,stan)').
lassy(stand, 'N(soort,ev,basis,zijd,stan)').
lassy(reden, 'N(soort,ev,basis,zijd,stan)').
lassy(project, 'N(soort,ev,basis,onz,stan)').
lassy(hoofd, 'N(soort,ev,basis,onz,stan)').
lassy(grens, 'N(soort,ev,basis,zijd,stan)').
lassy('Rusland', 'N(eigen,ev,basis,onz,stan)').
lassy('New', 'SPEC(deeleigen)').
lassy(ben, 'WW(pv,tgw,ev)').
lassy('Justitie', 'N(soort,ev,basis,zijd,stan)').
lassy(waarde, 'N(soort,ev,basis,zijd,stan)').
lassy(precies, 'ADJ(vrij,basis,zonder)').
lassy(kosten, 'N(soort,mv,basis)').
lassy(kant, 'N(soort,ev,basis,zijd,stan)').
lassy(eigenlijk, 'ADJ(vrij,basis,zonder)').
lassy(volgt, 'WW(pv,tgw,met-t)').
lassy(jonge, 'ADJ(prenom,basis,met-e,stan)').
lassy(goud, 'N(soort,ev,basis,onz,stan)').
lassy(daarna, 'BW()').
lassy(burgers, 'N(soort,mv,basis)').
lassy(binnen, 'VZ(fin)').
lassy(gebeurt, 'WW(pv,tgw,met-t)').
lassy(feit, 'N(soort,ev,basis,onz,stan)').
lassy(eveneens, 'BW()').
lassy('Veel', 'VNW(onbep,grad,stan,prenom,zonder,agr,basis)').
lassy(waaronder, 'BW()').
lassy(proces, 'N(soort,ev,basis,onz,stan)').
lassy(of, 'SPEC(vreemd)').
lassy(leggen, 'WW(inf,vrij,zonder)').
lassy(gevonden, 'WW(vd,vrij,zonder)').
lassy(beslissing, 'N(soort,ev,basis,zijd,stan)').
lassy(artikel, 'N(soort,ev,basis,onz,stan)').
lassy('Russische', 'ADJ(prenom,basis,met-e,stan)').
lassy('Regering', 'N(soort,ev,basis,zijd,stan)').
lassy(kregen, 'WW(pv,verl,mv)').
lassy(eerst, 'TW(rang,nom,zonder-n)').
lassy(album, 'N(soort,ev,basis,onz,stan)').
lassy(aanslagen, 'N(soort,mv,basis)').
lassy('1998', 'TW(hoofd,vrij)').
lassy('10', 'TW(hoofd,vrij)').
lassy(vanwege, 'VZ(init)').
lassy(vader, 'N(soort,ev,basis,zijd,stan)').
lassy(rechter, 'N(soort,ev,basis,zijd,stan)').
lassy(na, 'VZ(fin)').
lassy(leidde, 'WW(pv,verl,ev)').
lassy(doen, 'WW(pv,tgw,mv)').
lassy(controle, 'N(soort,ev,basis,zijd,stan)').
lassy('Jan', 'SPEC(deeleigen)').
lassy('6', 'TW(hoofd,vrij)').
lassy(toepassing, 'N(soort,ev,basis,zijd,stan)').
lassy(the, 'SPEC(vreemd)').
lassy('Rotterdam', 'N(eigen,ev,basis,onz,stan)').
lassy('Italiaanse', 'ADJ(prenom,basis,met-e,stan)').
lassy('Eerste', 'TW(rang,prenom,stan)').
lassy(speelt, 'WW(pv,tgw,met-t)').
lassy(soort, 'N(soort,ev,basis,genus,stan)').
lassy(programma, 'N(soort,ev,basis,onz,stan)').
lassy(paus, 'N(soort,ev,basis,zijd,stan)').
lassy(openbare, 'ADJ(prenom,basis,met-e,stan)').
lassy(inmiddels, 'BW()').
lassy(blijven, 'WW(pv,tgw,mv)').
lassy(zoon, 'N(soort,ev,basis,zijd,stan)').
lassy(sterke, 'ADJ(prenom,basis,met-e,stan)').
lassy(plaatsen, 'N(soort,mv,basis)').
lassy(beeld, 'N(soort,ev,basis,onz,stan)').
lassy('Vanaf', 'VZ(init)').
lassy('Bush', 'N(eigen,ev,basis,zijd,stan)').
lassy(leiden, 'WW(inf,vrij,zonder)').
lassy(hoogte, 'N(soort,ev,basis,zijd,stan)').
lassy(goederen, 'N(soort,mv,basis)').
lassy(gehad, 'WW(vd,vrij,zonder)').
lassy(betalen, 'WW(inf,vrij,zonder)').
lassy(belangrijk, 'ADJ(vrij,basis,zonder)').
lassy(zee, 'N(soort,ev,basis,zijd,stan)').
lassy(werking, 'N(soort,ev,basis,zijd,stan)').
lassy(natuurlijk, 'ADJ(vrij,basis,zonder)').
lassy(minder, 'VNW(onbep,grad,stan,prenom,zonder,agr,comp)').
lassy(dollar, 'N(soort,ev,basis,zijd,stan)').
lassy('Ron', 'N(eigen,ev,basis,zijd,stan)').
lassy('II', 'SPEC(deeleigen)').
lassy('Dat', 'VNW(aanw,det,stan,prenom,zonder,evon)').
lassy(termijn, 'N(soort,ev,basis,zijd,stan)').
lassy(oog, 'N(soort,ev,basis,onz,stan)').
lassy(groei, 'N(soort,ev,basis,zijd,stan)').
lassy(bezoek, 'N(soort,ev,basis,onz,stan)').
lassy('Uit', 'VZ(init)').
lassy('Naast', 'VZ(init)').
lassy(interne, 'ADJ(prenom,basis,met-e,stan)').
lassy(instellingen, 'N(soort,mv,basis)').
lassy('Buitenlandse', 'ADJ(prenom,basis,met-e,stan)').
lassy(ziet, 'WW(pv,tgw,met-t)').
lassy(verband, 'N(soort,ev,basis,onz,stan)').
lassy(vast, 'ADJ(vrij,basis,zonder)').
lassy(slag, 'N(soort,ev,basis,zijd,stan)').
lassy(productie, 'N(soort,ev,basis,zijd,stan)').
lassy(positie, 'N(soort,ev,basis,zijd,stan)').
lassy(gewoon, 'ADJ(vrij,basis,zonder)').
lassy(finale, 'N(soort,ev,basis,zijd,stan)').
lassy(me, 'VNW(pr,pron,obl,red,1,ev)').
lassy(sector, 'N(soort,ev,basis,zijd,stan)').
lassy(prijs, 'N(soort,ev,basis,zijd,stan)').
lassy(personen, 'N(soort,mv,basis)').
lassy(hoog, 'ADJ(vrij,basis,zonder)').
lassy(haar, 'VNW(pers,pron,obl,vol,3,getal,fem)').
lassy(campagne, 'N(soort,ev,basis,zijd,stan)').
lassy(biedt, 'WW(pv,tgw,met-t)').
lassy('Hier', 'VNW(aanw,adv-pron,obl,vol,3o,getal)').
lassy(soldaten, 'N(soort,mv,basis)').
lassy(parlement, 'N(soort,ev,basis,onz,stan)').
lassy(om, 'VZ(fin)').
lassy(loopt, 'WW(pv,tgw,met-t)').
lassy(je, 'VNW(pr,pron,obl,red,2v,getal)').
lassy(beschikbaar, 'ADJ(vrij,basis,zonder)').
lassy(aanpak, 'N(soort,ev,basis,zijd,stan)').
lassy('Verenigde', 'SPEC(deeleigen)').
lassy('Epilepsie', 'N(soort,ev,basis,zijd,stan)').
lassy(woorden, 'N(soort,mv,basis)').
lassy(volledige, 'ADJ(prenom,basis,met-e,stan)').
lassy(ver, 'ADJ(vrij,basis,zonder)').
lassy(uitvoering, 'N(soort,ev,basis,zijd,stan)').
lassy(partner, 'N(soort,ev,basis,zijd,stan)').
lassy(advies, 'N(soort,ev,basis,onz,stan)').
lassy(actief, 'ADJ(vrij,basis,zonder)').
lassy('8', 'TW(hoofd,vrij)').
lassy(zuiden, 'N(soort,ev,basis,onz,stan)').
lassy(viel, 'WW(pv,verl,ev)').
lassy(leeftijd, 'N(soort,ev,basis,zijd,stan)').
lassy(gingen, 'WW(pv,verl,mv)').
lassy(bestaande, 'WW(od,prenom,met-e)').
lassy(opgericht, 'WW(vd,vrij,zonder)').
lassy(moeilijk, 'ADJ(vrij,basis,zonder)').
lassy(middel, 'N(soort,ev,basis,onz,stan)').
lassy(gebruiken, 'WW(inf,vrij,zonder)').
lassy(bestuur, 'N(soort,ev,basis,onz,stan)').
lassy('Iraakse', 'ADJ(prenom,basis,met-e,stan)').
lassy('Deze', 'VNW(aanw,det,stan,nom,met-e,zonder-n)').
lassy(voeren, 'WW(inf,vrij,zonder)').
lassy(studenten, 'N(soort,mv,basis)').
lassy(speciale, 'ADJ(prenom,basis,met-e,stan)').
lassy(projecten, 'N(soort,mv,basis)').
lassy(officiële, 'ADJ(prenom,basis,met-e,stan)').
lassy(gericht, 'WW(vd,vrij,zonder)').
lassy(aanwezig, 'ADJ(vrij,basis,zonder)').
lassy('Engeland', 'N(eigen,ev,basis,onz,stan)').
lassy('4', 'TW(hoofd,prenom,stan)').
lassy(werknemers, 'N(soort,mv,basis)').
lassy(samenleving, 'N(soort,ev,basis,zijd,stan)').
lassy(richting, 'N(soort,ev,basis,zijd,stan)').
lassy(plannen, 'N(soort,mv,basis)').
lassy(overleg, 'N(soort,ev,basis,onz,stan)').
lassy(ministers, 'N(soort,mv,basis)').
lassy(leidt, 'WW(pv,tgw,met-t)').
lassy(hebt, 'WW(pv,tgw,met-t)').
lassy(gedurende, 'VZ(init)').
lassy(gebaseerd, 'WW(vd,vrij,zonder)').
lassy(bovendien, 'BW()').
lassy('Nederlanders', 'N(eigen,mv,basis)').
lassy('FNB', 'N(eigen,ev,basis,zijd,stan)').
lassy('100', 'TW(hoofd,prenom,stan)').
lassy(stuk, 'N(soort,ev,basis,onz,stan)').
lassy(specifieke, 'ADJ(prenom,basis,met-e,stan)').
lassy(resultaat, 'N(soort,ev,basis,onz,stan)').
lassy(gevaar, 'N(soort,ev,basis,onz,stan)').
lassy(direct, 'ADJ(vrij,basis,zonder)').
lassy(betrekking, 'N(soort,ev,basis,zijd,stan)').
lassy(begint, 'WW(pv,tgw,met-t)').
lassy(aanslag, 'N(soort,ev,basis,zijd,stan)').
lassy('Over', 'VZ(init)').
lassy('Omdat', 'VG(onder)').
lassy('Duitsers', 'N(eigen,mv,basis)').
lassy('20', 'TW(hoofd,vrij)').
lassy(voormalige, 'ADJ(prenom,basis,met-e,stan)').
lassy(veiligheid, 'N(soort,ev,basis,zijd,stan)').
lassy(titel, 'N(soort,ev,basis,zijd,stan)').
lassy(langs, 'VZ(init)').
lassy(hiervoor, 'BW()').
lassy(graag, 'BW()').
lassy('Wie', 'VNW(vb,pron,stan,vol,3p,getal)').
lassy('Vooral', 'BW()').
lassy(taal, 'N(soort,ev,basis,zijd,stan)').
lassy(mogelijke, 'ADJ(prenom,basis,met-e,stan)').
lassy(leider, 'N(soort,ev,basis,zijd,stan)').
lassy(kritiek, 'N(soort,ev,basis,zijd,stan)').
lassy('15', 'TW(hoofd,prenom,stan)').
lassy(zitten, 'WW(pv,tgw,mv)').
lassy(zin, 'N(soort,ev,basis,zijd,stan)').
lassy(vervolgens, 'BW()').
lassy(tegenover, 'VZ(init)').
lassy(sprake, 'N(soort,ev,basis,dat)').
lassy(omgeving, 'N(soort,ev,basis,zijd,stan)').
lassy(lichaam, 'N(soort,ev,basis,onz,stan)').
lassy(enkel, 'BW()').
lassy(daarvoor, 'BW()').
lassy(boeken, 'N(soort,mv,basis)').
lassy('Wanneer', 'VG(onder)').
lassy('Parlement', 'N(soort,ev,basis,onz,stan)').
lassy('Je', 'VNW(pers,pron,nomin,red,2v,ev)').
lassy('Daarbij', 'BW()').
lassy('Alleen', 'BW()').
lassy(verdere, 'ADJ(prenom,comp,met-e,stan)').
lassy(uitgevoerd, 'WW(vd,vrij,zonder)').
lassy(schade, 'N(soort,ev,basis,zijd,stan)').
lassy(ruimte, 'N(soort,ev,basis,zijd,stan)').
lassy(organisaties, 'N(soort,mv,basis)').
lassy(milieu, 'N(soort,ev,basis,onz,stan)').
lassy(klanten, 'N(soort,mv,basis)').
lassy(echte, 'ADJ(prenom,basis,met-e,stan)').
lassy(bracht, 'WW(pv,verl,ev)').
lassy('9', 'TW(hoofd,vrij)').
lassy(wist, 'WW(pv,verl,ev)').
lassy(totale, 'ADJ(prenom,basis,met-e,stan)').
lassy(thuis, 'BW()').
lassy(rest, 'N(soort,ev,basis,zijd,stan)').
lassy(overigens, 'BW()').
lassy(noorden, 'N(soort,ev,basis,onz,stan)').
lassy(morgen, 'BW()').
lassy(minuten, 'N(soort,mv,basis)').
lassy(groter, 'ADJ(vrij,comp,zonder)').
lassy(ervoor, 'BW()').
lassy(effect, 'N(soort,ev,basis,onz,stan)').
lassy(centrum, 'N(soort,ev,basis,onz,stan)').
lassy('Wij', 'VNW(pers,pron,nomin,vol,1,mv)').
lassy('VLD', 'N(eigen,ev,basis,zijd,stan)').
lassy('Palestijnse', 'ADJ(prenom,basis,met-e,stan)').
lassy('Al', 'BW()').
lassy('15', 'TW(hoofd,vrij)').
lassy('1', 'TW(hoofd,prenom,stan)').
lassy(toegang, 'N(soort,ev,basis,zijd,stan)').
lassy(stap, 'N(soort,ev,basis,zijd,stan)').
lassy(mogelijkheden, 'N(soort,mv,basis)').
lassy(doden, 'ADJ(nom,basis,met-e,mv-n)').
lassy(buurt, 'N(soort,ev,basis,zijd,stan)').
lassy('Amerika', 'N(eigen,ev,basis,onz,stan)').
lassy(verklaring, 'N(soort,ev,basis,zijd,stan)').
lassy(moeder, 'N(soort,ev,basis,zijd,stan)').
lassy(justitie, 'N(soort,ev,basis,zijd,stan)').
lassy(enorme, 'ADJ(prenom,basis,met-e,stan)').
lassy(bevat, 'WW(pv,tgw,ev)').
lassy(vormen, 'WW(pv,tgw,mv)').
lassy(mogelijkheid, 'N(soort,ev,basis,zijd,stan)').
lassy('7', 'TW(hoofd,vrij)').
lassy('God','N(eigen,ev,basis,zijd,stan)').
lassy(rondje,'N(soort,ev,dim,onz,stan)').
lassy(afhandig,'ADJ(vrij,basis,zonder)').
lassy('¤','SPEC(symb)').

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

particle_tag(aaneen,'BW()').
particle_tag(achterna,'BW()').
particle_tag(achterover,'BW()').
particle_tag(achteruit,'BW()').
particle_tag(actie,'N(soort,ev,basis,zijd,stan)').
particle_tag(adem,'N(soort,ev,basis,zijd,stan)').
particle_tag(af_handig,'ADJ(vrij,basis,zonder)').
particle_tag(afhandig,'ADJ(vrij,basis,zonder)').
particle_tag(beet,'N(soort,ev,basis,zijd,stan)').
particle_tag(bega,'WW(vd,vrij,zonder)').
particle_tag(begrijp,'WW(vd,vrij,zonder)').
particle_tag(bekend,'ADJ(vrij,basis,zonder)').
particle_tag(beschikbaar,'ADJ(vrij,basis,zonder)').
particle_tag(beter,'ADJ(vrij,comp,zonder)').
particle_tag(bezig,'ADJ(vrij,basis,zonder)').
particle_tag(bijeen,'BW()').
particle_tag(binnenskamers,'ADJ(vrij,basis,zonder)').
particle_tag(blank,'ADJ(vrij,basis,zonder)').
particle_tag(blijf_uit,'WW(inf,vrij,zonder)').
particle_tag(blijk,'N(soort,ev,basis,onz,stan)').
particle_tag(blijk,'WW(inf,vrij,zonder)').
particle_tag(blind,'ADJ(vrij,basis,zonder)').
particle_tag(bloot,'ADJ(vrij,basis,zonder)').
particle_tag(bol,'N(soort,ev,basis,zijd,stan)').
particle_tag(deel,'N(soort,ev,basis,onz,stan)').
particle_tag(dicht,'ADJ(vrij,basis,zonder)').
particle_tag(dienst,'N(soort,ev,basis,zijd,stan)').
particle_tag(dik,'ADJ(vrij,basis,zonder)').
particle_tag(dood,'ADJ(vrij,basis,zonder)').
particle_tag(duidelijk,'ADJ(vrij,basis,zonder)').
particle_tag(dwars,'ADJ(vrij,basis,zonder)').
particle_tag(erg,'ADJ(vrij,basis,zonder)').
particle_tag(erger,'ADJ(vrij,comp,zonder)').
particle_tag(eruit,'BW()').
particle_tag(feest,'N(soort,ev,basis,onz,stan)').
particle_tag(ga,'WW(inf,vrij,zonder)').
particle_tag(gedaan,'WW(vd,vrij,zonder)').
particle_tag(gedwongen,'WW(vd,vrij,zonder)').
particle_tag(gelegen,'WW(vd,vrij,zonder)').
particle_tag(gelijk,'ADJ(vrij,basis,zonder)').
particle_tag(gelukkig,'ADJ(vrij,basis,zonder)').
particle_tag(gemeen,'ADJ(vrij,basis,zonder)').
particle_tag(genoodzaakt,'WW(vd,vrij,zonder)').
particle_tag(gereed,'ADJ(vrij,basis,zonder)').
particle_tag(geroepen,'WW(vd,vrij,zonder)').
particle_tag(gerust,'ADJ(vrij,basis,zonder)').
particle_tag(gevangen,'WW(vd,vrij,zonder)').
particle_tag(gewaar,'ADJ(vrij,basis,zonder)').
particle_tag(gewonnen,'WW(vd,vrij,zonder)').
particle_tag(glad,'ADJ(vrij,basis,zonder)').
particle_tag(goed,'ADJ(vrij,basis,zonder)').
particle_tag(goed,'ADJ(vrij,comp,zonder)').
particle_tag(heb,'WW(inf,vrij,zonder)').
particle_tag(hem,'VNW(pers,pron,obl,vol,3,ev,masc)').
particle_tag(hoog,'ADJ(vrij,basis,zonder)').
particle_tag(hoor,'WW(inf,vrij,zonder)').
particle_tag(ineen,'BW()').
particle_tag(ingedrukt,'WW(vd,vrij,zonder)').
particle_tag(kan,'WW(inf,vrij,zonder)').
particle_tag(kapot,'ADJ(vrij,basis,zonder)').
particle_tag(kenbaar,'ADJ(vrij,basis,zonder)').
particle_tag(kennis,'N(soort,ev,basis,zijd,stan)').
particle_tag(klaar,'ADJ(vrij,basis,zonder)').
particle_tag(klem,'N(soort,ev,basis,zijd,stan)').
particle_tag(kwalijk,'ADJ(vrij,basis,zonder)').
particle_tag(kwijt,'ADJ(vrij,basis,zonder)').
particle_tag(lam,'ADJ(vrij,basis,zonder)').
particle_tag(lastig,'ADJ(vrij,basis,zonder)').
particle_tag(leef,'WW(inf,vrij,zonder)').
particle_tag(leeg,'ADJ(vrij,basis,zonder)').
particle_tag(lek,'ADJ(vrij,basis,zonder)').
particle_tag(les,'N(soort,ev,basis,zijd,stan)').
particle_tag(lig,'WW(vd,vrij,zonder)').
particle_tag(los,'ADJ(vrij,basis,zonder)').
particle_tag(meester,'N(soort,ev,basis,zijd,stan)').
particle_tag(merk,'WW(inf,vrij,zonder)').
particle_tag(mis,'ADJ(vrij,basis,zonder)').
particle_tag(moeilijk,'ADJ(vrij,basis,zonder)').
particle_tag(neer,'VZ(fin)').
particle_tag(omver,'BW()').
particle_tag(onbetuigd,'ADJ(vrij,basis,zonder)').
particle_tag(onderuit,'BW()').
particle_tag(ontglip,'WW(inf,vrij,zonder)').
particle_tag(ontval,'WW(inf,vrij,zonder)').
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
particle_tag(school,'N(soort,ev,basis,zijd,stan)').
particle_tag(schoon,'ADJ(vrij,basis,zonder)').
particle_tag(schrijf,'WW(vd,vrij,zonder)').
particle_tag(schuil,'BW()').
particle_tag(schuldig,'ADJ(vrij,basis,zonder)').
particle_tag(staande,'WW(od,vrij,zonder)').
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
particle_tag(teweeg,'BW()').
particle_tag(tewerk,'BW()').
particle_tag(thee,'N(soort,ev,basis,zijd,stan)').
particle_tag(thuis,'BW()').
particle_tag(trouw,'ADJ(vrij,basis,zonder)').
particle_tag(uiteen,'BW()').
particle_tag(vaar,'WW(inf,vrij,zonder)').
particle_tag(val,'WW(inf,vrij,zonder)').
particle_tag(vast,'ADJ(vrij,basis,zonder)').
particle_tag(veil,'ADJ(vrij,basis,zonder)').
particle_tag(veilig,'ADJ(vrij,basis,zonder)').
particle_tag(ver,'ADJ(vrij,basis,zonder)').
particle_tag(ver,'ADJ(vrij,comp,zonder)').
particle_tag(verder,'ADJ(vrij,comp,zonder)').
particle_tag(vergezeld,'WW(vd,vrij,zonder)').
particle_tag(verzeild,'WW(vd,vrij,zonder)').
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
particle_tag(wacht,'WW(inf,vrij,zonder)').
particle_tag(wandel,'WW(inf,vrij,zonder)').
particle_tag(weer,'BW()').
particle_tag(weet_af,'WW(inf,vrij,zonder)').
particle_tag(weet,'WW(inf,vrij,zonder)').
particle_tag(weg,'BW()').
particle_tag(wel,'BW()').
particle_tag(welkom,'ADJ(vrij,basis,zonder)').
particle_tag(wijs,'ADJ(vrij,basis,zonder)').
particle_tag(zaken,'N(soort,mv,basis)').
particle_tag(zeg,'WW(vd,vrij,zonder)').
particle_tag(zie_aan,'WW(inf,vrij,zonder)').
particle_tag(zien,'WW(inf,vrij,zonder)').
particle_tag(zie,'WW(inf,vrij,zonder)').
particle_tag(zit,'WW(inf,vrij,zonder)').
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

guess_lemma(guess(Surf),Lemma) :-
    !,
    (   lassy_lemma(Surf,Lemma1)
    ->  Lemma = Lemma1
    ;   Lemma = Surf
    ).
guess_lemma(Surf,Lemma) :-
    (    exc_lemma(Surf,Lemma1)
    ->   Lemma = Lemma1
    ;    Surf = Lemma
    ).

exc_lemma('onder meer','onder veel').  % voor 'o.m.'
exc_lemma(sprake,spraak).
exc_lemma(ten,te).
exc_lemma(ter,te).
exc_lemma(wiens,wie).
exc_lemma(wier,wie).

exc_lemma('Beweging',beweging).
exc_lemma('College',college).
exc_lemma('Comité',comité).
exc_lemma('Commissie',commissie).
exc_lemma('Cultuur',cultuur).
exc_lemma('Europese','Europees').
exc_lemma('Financiën',financiën).
exc_lemma('Grondwet',grond_wet).
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
exc_lemma('Strafwetboek',strafwetboek).
exc_lemma('Universiteit',universiteit).
exc_lemma('Verbond',verbond).
exc_lemma('Vereniging',vereniging).
exc_lemma('Volkskgezondheid',volksgezondheid).


lassy_lemma('Aan',aan).
lassy_lemma(aanslagen,aanslag).
lassy_lemma(aanvallen,aanval).
lassy_lemma(activiteiten,activiteit).
lassy_lemma(afgelopen,aflopen).
lassy_lemma(afspraken,afspraak).
lassy_lemma('Al',al).
lassy_lemma(algemene,algemeen).
lassy_lemma(alle,al).
lassy_lemma('Alle',al).
lassy_lemma('Alleen',alleen).
lassy_lemma('Als',als).
lassy_lemma('Amerikaanse','Amerikaans').
lassy_lemma('Amerikanen','Amerikaan').
lassy_lemma(andere,ander).
lassy_lemma('Andere',ander).
lassy_lemma(anderen,ander).
lassy_lemma('Arabische','Arabisch').
lassy_lemma(banken,bank).
lassy_lemma(bedoeld,bedoelen).
lassy_lemma(bedrijven,bedrijf).
lassy_lemma(beelden,beeld).
lassy_lemma(begint,beginnen).
lassy_lemma(begon,beginnen).
lassy_lemma(begonnen,beginnen).
lassy_lemma(behandeld,behandelen).
lassy_lemma(bekende,bekend).
lassy_lemma(belangen,belang).
lassy_lemma(belangrijke,belangrijk).
lassy_lemma(belangrijkste,belangrijk).
lassy_lemma('Belgische','Belgisch').
lassy_lemma(bent,zijn).
lassy_lemma(ben,zijn).
lassy_lemma(bepaald,bepalen).
lassy_lemma(bepaalde,bepalen).
lassy_lemma(beperkt,beperken).
lassy_lemma(bereikt,bereiken).
lassy_lemma(beschouwd,beschouwen).
lassy_lemma(besloot,besluiten).
lassy_lemma(besloten,besluiten).
lassy_lemma(bestaande,bestaan).
lassy_lemma(bestaat,bestaan).
lassy_lemma(beste,goed).
lassy_lemma(best,goed).
lassy_lemma(betaald,betalen).
lassy_lemma(betekent,betekenen).
lassy_lemma(betere,goed).
lassy_lemma(beter,goed).
lassy_lemma(betreffende,betreffen).
lassy_lemma(betreft,betreffen).
lassy_lemma(betrokken,betrekken).
lassy_lemma(bevat,bevatten).
lassy_lemma(bevoegdheden,bevoegdheid).
lassy_lemma(biedt,bieden).
lassy_lemma('Bij',bij).
lassy_lemma(bijzondere,bijzonder).
lassy_lemma(bleef,blijven).
lassy_lemma(bleek,blijken).
lassy_lemma(blijft,blijven).
lassy_lemma(blijkt,blijken).
lassy_lemma('Blz.',bladzij).
lassy_lemma(boeken,boek).
lassy_lemma(boeren,boer).
lassy_lemma('Bovendien',bovendien).
lassy_lemma(bracht,brengen).
lassy_lemma(brengt,brengen).
lassy_lemma('Britse','Brits').
lassy_lemma('Britten','Brit').
lassy_lemma('Brusselse','Brussels').
lassy_lemma(buitenlandse,buitenlands).
lassy_lemma('Buitenlandse',buitenlands).
lassy_lemma(burgers,burger).
lassy_lemma(centrale,centraal).
lassy_lemma('Chinese','Chinees').
lassy_lemma(cijfers,cijfer).
lassy_lemma(concrete,concreet).
lassy_lemma('Daarbij',daarbij).
lassy_lemma('Daar',daar).
lassy_lemma('Daarnaast',daarnaast).
lassy_lemma('Daarom',daarom).
lassy_lemma(dagen,dag).
lassy_lemma('Dames',dame).
lassy_lemma('Dan',dan).
lassy_lemma('Dat',dat).
lassy_lemma('De',de).
lassy_lemma('DE',de).
lassy_lemma(deed,doen).
lassy_lemma(delen,deel).
lassy_lemma(der,de).
lassy_lemma(der,de).
lassy_lemma(derde,drie).
lassy_lemma(dergelijke,dergelijk).
lassy_lemma('Deze',deze).
lassy_lemma('Die',die).
lassy_lemma(diensten,dienst).
lassy_lemma(dient,dienen).
lassy_lemma('Dit',dit).
lassy_lemma(diverse,divers).
lassy_lemma(doden,dood).
lassy_lemma(doet,doen).
lassy_lemma('Door',door).
lassy_lemma('Duitse','Duits').
lassy_lemma('Duitsers','Duitser').
lassy_lemma(duizenden,duizend).
lassy_lemma(duurzame,duurzaam).
lassy_lemma(echte,echt).
lassy_lemma(economische,economisch).
lassy_lemma('Een',een).
lassy_lemma(een,één).
lassy_lemma('Een',één).
lassy_lemma(eerste,één).
lassy_lemma(eerst,één).
lassy_lemma(elke,elk).
lassy_lemma(ene,een).
lassy_lemma('En',en).
lassy_lemma(én,en).
lassy_lemma(enige,enig).
lassy_lemma(enkele,enkel).
lassy_lemma(enorme,enorm).
lassy_lemma('Epilepsie',epilepsie).
lassy_lemma('Er',er).
lassy_lemma(ernstige,ernstig).
lassy_lemma('Europese','Europees').
lassy_lemma('Externe',extern).
lassy_lemma(federale,federaal).
lassy_lemma(financiële,financieel).
lassy_lemma('Franse','Frans').
lassy_lemma('Franstalige','Franstalig').
lassy_lemma(gaat,gaan).
lassy_lemma(gaf,geven).
lassy_lemma(gebaseerd,baseren).
lassy_lemma(gebeurde,gebeuren).
lassy_lemma(gebeurt,gebeuren).
lassy_lemma(gebieden,gebied).
lassy_lemma(gebouwd,bouwen).
lassy_lemma(gebracht,brengen).
lassy_lemma(gebruikt,gebruiken).
lassy_lemma(gedaan,doen).
lassy_lemma(geeft,geven).
lassy_lemma('Geen',geen).
lassy_lemma(gegeven,geven).
lassy_lemma(gegevens,gegeven).
lassy_lemma(gehad,hebben).
lassy_lemma(gehouden,houden).
lassy_lemma(gekomen,komen).
lassy_lemma(gekozen,kiezen).
lassy_lemma(gekregen,krijgen).
lassy_lemma(geldt,gelden).
lassy_lemma(geleid,leiden).
lassy_lemma(gemaakt,maken).
lassy_lemma('Gemeenschap',gemeenschap).
lassy_lemma(gemeenten,gemeente).
lassy_lemma(genoemd,noemen).
lassy_lemma(genomen,nemen).
lassy_lemma(georganiseerd,organiseren).
lassy_lemma(gericht,richten).
lassy_lemma(gesloten,sluiten).
lassy_lemma(gesproken,spreken).
lassy_lemma(gesteld,stellen).
lassy_lemma(getroffen,treffen).
lassy_lemma(gevallen,geval).
lassy_lemma(gevolgen,gevolg).
lassy_lemma(gevonden,vinden).
lassy_lemma(geweest,zijn).
lassy_lemma(gewone,gewoon).
lassy_lemma(geworden,worden).
lassy_lemma(gezegd,zeggen).
lassy_lemma(gezet,zetten).
lassy_lemma(gezien,zien).
lassy_lemma(gingen,gaan).
lassy_lemma(ging,gaan).
lassy_lemma(goede,goed).
lassy_lemma(goederen,goed).
lassy_lemma(grenzen,grens).
lassy_lemma(groepen,groep).
lassy_lemma(grootste,groot).
lassy_lemma(grote,groot).
lassy_lemma(grotere,groot).
lassy_lemma(groter,groot).
lassy_lemma(hadden,hebben).
lassy_lemma(had,hebben).
lassy_lemma(handen,hand).
lassy_lemma(heb,hebben).
lassy_lemma(hebt,hebben).
lassy_lemma(heeft,hebben).
lassy_lemma(hele,heel).
lassy_lemma(heren,heer).
lassy_lemma('Het',het).
lassy_lemma(hield,houden).
lassy_lemma('Hier',hier).
lassy_lemma('Hij',hij).
lassy_lemma(hoeft,hoeven).
lassy_lemma('Hoe',hoe).
lassy_lemma('Hoewel',hoewel).
lassy_lemma(hoge,hoog).
lassy_lemma(hogere,hoog).
lassy_lemma(hoger,hoog).
lassy_lemma(honderden,honderd).
lassy_lemma(houdt,houden).
lassy_lemma(huidige,huidig).
lassy_lemma('Hun',hun).
lassy_lemma(iedere,ieder).
lassy_lemma('Ik',ik).
lassy_lemma('Indien',indien).
lassy_lemma(ingezet,inzetten).
lassy_lemma('In',in).
lassy_lemma(initiatieven,initiatief).
lassy_lemma(instellingen,instelling).
lassy_lemma(internationale,internationaal).
lassy_lemma(interne,intern).
lassy_lemma(inwoners,inwoner).
lassy_lemma('Iraakse','Iraaks').
lassy_lemma('Israëlische','Israëlisch').
lassy_lemma(is,zijn).
lassy_lemma('Is',zijn).
lassy_lemma('Italiaanse','Italiaans').
lassy_lemma('Japanse','Japans').
lassy_lemma(jaren,jaar).
lassy_lemma('Je',je).
lassy_lemma(jonge,jong).
lassy_lemma(jongeren,jong).
lassy_lemma(juiste,juist).
lassy_lemma('Justitie',justitie).
lassy_lemma('Kamer',kamer).
lassy_lemma(kan,kunnen).
lassy_lemma(kent,kennen).
lassy_lemma(kinderen,kind).
lassy_lemma(klanten,klant).
lassy_lemma(kleine,klein).
lassy_lemma(komende,komen).
lassy_lemma(komt,komen).
lassy_lemma(konden,kunnen).
lassy_lemma('Koning',koning).
lassy_lemma(kon,kunnen).
lassy_lemma(korte,kort).
lassy_lemma(kosten,kost).
lassy_lemma(kreeg,krijgen).
lassy_lemma(kregen,krijgen).
lassy_lemma(krijgt,krijgen).
lassy_lemma(kunt,kunnen).
lassy_lemma(kwamen,komen).
lassy_lemma(kwam,komen).
lassy_lemma(laat,laten).
lassy_lemma(laatste,laat).
lassy_lemma(lager,laag).
lassy_lemma(lag,liggen).
lassy_lemma(landelijke,landelijk).
lassy_lemma(landen,land).
lassy_lemma(lange,lang).
lassy_lemma(langer,lang).
lassy_lemma(later,laat).
lassy_lemma(leden,lid).
lassy_lemma(leerlingen,leerling).
lassy_lemma(leidde,leiden).
lassy_lemma(leidt,leiden).
lassy_lemma(lidstaten,lidstaat).
lassy_lemma(liep,lopen).
lassy_lemma(liet,laten).
lassy_lemma(ligt,liggen).
lassy_lemma(lijkt,lijken).
lassy_lemma(lokale,lokaal).
lassy_lemma(loopt,lopen).
lassy_lemma(maakte,maken).
lassy_lemma(maakten,maken).
lassy_lemma(maakt,maken).
lassy_lemma(maanden,maand).
lassy_lemma('Maar',maar).
lassy_lemma(maatregelen,maatregel).
lassy_lemma(mag,mogen).
lassy_lemma(mannen,man).
lassy_lemma(medewerkers,medewerker).
lassy_lemma(medicijnen,medicijn).
lassy_lemma(meer,veel).
lassy_lemma('Meer',veel).
lassy_lemma(meeste,veel).
lassy_lemma(meest,veel).
lassy_lemma('Men',men).
lassy_lemma(mensen,mens).
lassy_lemma('Met',met).
lassy_lemma(middelen,middel).
lassy_lemma(militaire,militair).
lassy_lemma(militairen,militair).
lassy_lemma(minder,weinig).
lassy_lemma('Minister',minister).
lassy_lemma(ministers,minister).
lassy_lemma(minuten,minuut).
lassy_lemma(mocht,mogen).
lassy_lemma(moderne,modern).
lassy_lemma(moesten,moeten).
lassy_lemma(moest,moeten).
lassy_lemma(moet,moeten).
lassy_lemma(mogelijke,mogelijk).
lassy_lemma(mogelijkheden,mogelijkheid).
lassy_lemma('Naast',naast).
lassy_lemma(name,naam).
lassy_lemma(nam,nemen).
lassy_lemma('Na',na).
lassy_lemma(nationale,nationaal).
lassy_lemma('Nederlanders','Nederlander').
lassy_lemma('Nederlandse','Nederlands').
lassy_lemma(neemt,nemen).
lassy_lemma('Niet',niet).
lassy_lemma(nieuwe,nieuw).
lassy_lemma('Nog',nog).
lassy_lemma('nr.',nummer).
lassy_lemma('Nu',nu).
lassy_lemma(officiële,officieel).
lassy_lemma('Of',of).
lassy_lemma(ogen,oog).
lassy_lemma('Omdat',omdat).
lassy_lemma('Om',om).
lassy_lemma(omstandigheden,omstandigheid).
lassy_lemma(onderhandelingen,onderhandeling).
lassy_lemma('Onder',onder).
lassy_lemma('Onderwijs',onderwijs).
lassy_lemma(onderzocht,onderzoeken).
lassy_lemma(ontstond,ontstaan).
lassy_lemma(ontwikkeld,ontwikkelen).
lassy_lemma(ontwikkelingslanden,ontwikkelingsland).
lassy_lemma(onze,ons).
lassy_lemma('Ook',ook).
lassy_lemma(openbare,openbaar).
lassy_lemma(opgenomen,opnemen).
lassy_lemma(opgericht,oprichten).
lassy_lemma('Op',op).
lassy_lemma(organisaties,organisatie).
lassy_lemma(oude,oud).
lassy_lemma(oudere,oud).
lassy_lemma(ouders,ouder).
lassy_lemma(overheden,overheid).
lassy_lemma('Over',over).
lassy_lemma('Palestijnse','Palestijns').
lassy_lemma(partijen,partij).
lassy_lemma(partners,partner).
lassy_lemma(patiënten,patiënt).
lassy_lemma(personen,persoon).
lassy_lemma(plaatsen,plaats).
lassy_lemma(plannen,plan).
lassy_lemma(politieke,politiek).
lassy_lemma(prettige,prettig).
lassy_lemma(prijzen,prijs).
lassy_lemma(problemen,probleem).
lassy_lemma(producten,product).
lassy_lemma(projecten,project).
lassy_lemma(provincies,provincie).
lassy_lemma(punten,punt).
lassy_lemma(raakte,raken).
lassy_lemma(recente,recent).
lassy_lemma(rechten,recht).
lassy_lemma(redenen,reden).
lassy_lemma(regels,regel).
lassy_lemma(regionale,regionaal).
lassy_lemma(resultaten,resultaat).
lassy_lemma('risico\'s',risico).
lassy_lemma('Russische','Russisch').
lassy_lemma(scholen,school).
lassy_lemma(schreef,schrijven).
lassy_lemma('\'s',de).
lassy_lemma('Sinds',sinds).
lassy_lemma(slachtoffers,slachtoffer).
lassy_lemma(sociale,sociaal).
lassy_lemma(soldaten,soldaat).
lassy_lemma(sommige,sommig).
lassy_lemma('Sommige',sommig).
lassy_lemma(soorten,soort).
lassy_lemma('Spaanse','Spaans').
lassy_lemma(speciale,speciaal).
lassy_lemma(specifieke,specifiek).
lassy_lemma(speelt,spelen).
lassy_lemma(staat,staan).
lassy_lemma(staten,staat).
lassy_lemma(steden,stad).
lassy_lemma(stelde,stellen).
lassy_lemma(stelt,stellen).
lassy_lemma(sterke,sterk).
lassy_lemma(stoffen,stof).
lassy_lemma(stond,staan).
lassy_lemma(studenten,student).
lassy_lemma(studies,studie).
lassy_lemma(tanks,tank).
lassy_lemma(technische,technisch).
lassy_lemma(telt,tellen).
lassy_lemma(ten,te).
lassy_lemma(ten,te).
lassy_lemma('Ten',te).
lassy_lemma(ter,te).
lassy_lemma(ter,te).
lassy_lemma('Terwijl',terwijl).
lassy_lemma('\'t',het).
lassy_lemma('Tijdens',tijdens).
lassy_lemma('Toch',toch).
lassy_lemma('Toen',toen).
lassy_lemma(totale,totaal).
lassy_lemma('Tot',tot).
lassy_lemma(troepen,troep).
lassy_lemma(trok,trekken).
lassy_lemma(tweede,twee).
lassy_lemma('Twee',twee).
lassy_lemma(uitgebreid,uitbreiden).
lassy_lemma(uitgevoerd,uitvoeren).
lassy_lemma('Uit',uit).
lassy_lemma('Unie',unie).
lassy_lemma('U',u).
lassy_lemma(vaker,vaak).
lassy_lemma(valt,vallen).
lassy_lemma('Vanaf',vanaf).
lassy_lemma('Vandaag',vandaag).
lassy_lemma('Van',van).
lassy_lemma(vaste,vast).
lassy_lemma('Veel',veel).
lassy_lemma(vele,veel).
lassy_lemma(verboden,verbieden).
lassy_lemma(verbonden,verbinden).
lassy_lemma(verdachte,verdenken).
lassy_lemma(verdere,ver).
lassy_lemma(verder,ver).
lassy_lemma('Verder',verder).
lassy_lemma(verhalen,verhaal).
lassy_lemma(verkiezingen,verkiezing).
lassy_lemma(verkocht,verkopen).
lassy_lemma(verloren,verliezen).
lassy_lemma(vermiste,vermist).
lassy_lemma(veroorzaakt,veroorzaken).
lassy_lemma(verplicht,verplichten).
lassy_lemma(verschillende,verschillend).
lassy_lemma(verwacht,verwachten).
lassy_lemma(vielen,vallen).
lassy_lemma(viel,vallen).
lassy_lemma(vindt,vinden).
lassy_lemma('Vlaamse','Vlaams').
lassy_lemma(volgende,volgen).
lassy_lemma(volgend,volgen).
lassy_lemma('Volgens',volgens).
lassy_lemma(volgt,volgen).
lassy_lemma(volledige,volledig).
lassy_lemma(vonden,vinden).
lassy_lemma(vond,vinden).
lassy_lemma('Vooral',vooral).
lassy_lemma(voormalige,voormalig).
lassy_lemma('Voor',voor).
lassy_lemma(vóór,voor).
lassy_lemma(voorwaarden,voorwaarde).
lassy_lemma(vorige,vorig).
lassy_lemma(vormen,vorm).
lassy_lemma(vormt,vormen).
lassy_lemma(vragen,vraag).
lassy_lemma(vrienden,vriend).
lassy_lemma(vrije,vrij).
lassy_lemma(vrouwen,vrouw).
lassy_lemma('Waarom',waarom).
lassy_lemma('Wanneer',wanneer).
lassy_lemma('Want',want).
lassy_lemma(wapens,wapen).
lassy_lemma(waren,zijn).
lassy_lemma(was,zijn).
lassy_lemma('Wat',wat).
lassy_lemma(weet,weten).
lassy_lemma(weken,week).
lassy_lemma(welke,welk).
lassy_lemma('Wel',wel).
lassy_lemma(wens,wensen).
lassy_lemma(werden,worden).
lassy_lemma(werd,worden).
lassy_lemma(werken,werk).
lassy_lemma(werknemers,werknemer).
lassy_lemma(werkte,werken).
lassy_lemma(werkt,werken).
lassy_lemma(wetenschappelijke,wetenschappelijk).
lassy_lemma('We',we).
lassy_lemma('Wie',wie).
lassy_lemma('Wij',wij).
lassy_lemma(wilden,willen).
lassy_lemma(wilde,willen).
lassy_lemma(wilt,willen).
lassy_lemma(wil,willen).
lassy_lemma(wist,weten).
lassy_lemma(woningen,woning).
lassy_lemma(won,winnen).
lassy_lemma(woorden,woord).
lassy_lemma(wordt,worden).
lassy_lemma(zag,zien).
lassy_lemma(zaken,zaak).
lassy_lemma('Zaken',zaak).
lassy_lemma(zal,zullen).
lassy_lemma(zegt,zeggen).
lassy_lemma(zei,zeggen).
lassy_lemma(zet,zetten).
lassy_lemma('Ze',ze).
lassy_lemma(ziet,zien).
lassy_lemma(zie,zien).
lassy_lemma('Zie',zien).
lassy_lemma('Zijn',zijn).
lassy_lemma('Zij',zij).
lassy_lemma(zit,zitten).
lassy_lemma('z\'n',zijn).
lassy_lemma('Zoals',zoals).
lassy_lemma(zogenaamde,zogenaamd).
lassy_lemma(zouden,zullen).
lassy_lemma(zou,zullen).
lassy_lemma('Zo',zo).
lassy_lemma(zware,zwaar).

frequent_tag(Atom,Result) :-
    findall(Tag,alpino_lex:lexicon(Tag,_,[Atom],[],_),Tags),
    (   ft(T),
	lists:member(T,Tags)
    ->  Result = T
    ;   Tags = [Result|_]
    ).

ft(noun(_,_,_)).
ft(number(_)).

    