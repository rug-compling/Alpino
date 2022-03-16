:- module(alpino_genlex, []).

:- expects_dialect(sicstus).

:- use_module(library(lists)).
:- use_module(hdrug(hdrug_util)).
:- use_module(alpino('src/utils')).
:- use_module(alpino('src/latin1')).
:- use_module(bitcode).
:- use_module('fluency/stem_word_freq').

%% all called from cg.pl
:- public check_conditions/4,
          dict_entry/3, surf_to_list/3, lex_lookup/1.

lex_lookup(AdtNoRefs) :-
    retractall(alpino_cg:lex(_,_,_,_,_,_)),
    create_lex(AdtNoRefs,[]),
    introduce_particles_lex,
    introduce_with_dt(AdtNoRefs,Roots),
    filter_tags,
    debug_call(1,check_missing_roots(Roots)).

%% The create_lex and create_lex_daughters predicates find all leave nodes
%% that are lexical items, and assert them as facts.
create_lex(tree(r(opt(Rel),Node),Ds),Path) :-
    !,
    create_lex(Node,Rel,Ds,Path).
create_lex(tree(r(Rel,Node),Ds),Path) :-
    create_lex(Node,Rel,Ds,Path).

create_lex(p(Cat,_),Rel,Ds,Path) :-
    create_lex_daughters(Ds,[Rel/Cat|Path]).
create_lex(i(_,Node),Rel,Ds,Path) :-
    create_lex_indexed(Node,Rel,Ds,Path).
create_lex(i(_),_,[],_).
create_lex(adt_lex(_,Root,Bc,Frames,Dt,SimpleDt),REL,[],Path) :-
    if(create_frames(Frames,Root,Bc,Dt,SimpleDt,[REL|Path]),true,true).

%% special case for "ik heb gegeten en gedronken"
%% a head word that is co-indexed does not get unified
%% with its DT...
%%
%% no. "ik heb gegeten en gedronken" works without this too.
%% but "En tussendoor worden natuurlijk de verdachten verhoord en familieleden ondervraagd ."
%% runs into an infinite loop
/*
create_lex_indexed(adt_lex(_,Root,Bc,Frames,_Dt,_SimpleDt),hd,[],Path) :-
    !,
    debug_message(2,"ps: co-indexed head spotted (~w); not unifying DT~n",[Root]),
    alpino_data:lexical(Hwrd,Root,_,_,_,_,_,Bc),
    alpino_data:dt(Dt,Hwrd,_,_,_),
    if(create_frames(Frames,Root,Bc,Dt,Dt,[hd|Path]),true,true).
*/
create_lex_indexed(Node,Rel,Ds,Path):-
    create_lex(Node,Rel,Ds,Path).


create_lex_daughters([],_).
create_lex_daughters([Hd|Tl],Path) :-
    create_lex(Hd,Path),
    create_lex_daughters(Tl,Path).

create_frames(FramesSurfs0,Root,Bc,Dt,SimpleDt,REL) :-
    filter_frames(FramesSurfs0,[Fr|FramesSurfs],Dt,SimpleDt,REL),
    !,
    assert_lex([Fr|FramesSurfs],Root,Bc,Dt,SimpleDt).
%% if none survive, keep them all (to be used by robustness)
create_frames(FramesSurfs,Root,Bc,Dt,SimpleDt,_REL) :-
    assert_lex(FramesSurfs,Root,Bc,Dt,SimpleDt).

filter_frames([],[],_,_,_).
filter_frames([Frame-Surfs|T],N,Dt,SimpleDt,REL) :-
    (   check_parse_only(Surfs,Frame)
    ->  N = T2
    ;   check_conditions(Frame,Dt,SimpleDt,REL)
    ->  N = [Frame-Surfs|T2]
    ;   N = T2
    ),
    filter_frames(T,T2,Dt,SimpleDt,REL).

assert_lex([],_,_,_,_).
assert_lex([Frame-Surfs|Tail],Label,Bitcode,Dt,SimpleDt) :-
    assertz(alpino_cg:lex(Frame,Label,Bitcode,Dt,SimpleDt,Surfs)),
    assert_lex(Tail,Label,Bitcode,Dt,SimpleDt).

check_parse_only(List,Frame) :-
    lists:member(Root,List),
    check_parse_only_root(Root,Frame).

check_parse_only_root(eentje,_). % use één; dim should be in adt somehow...
check_parse_only_root(wezen,verb(zijn,inf,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%
% Particle introduction %
%%%%%%%%%%%%%%%%%%%%%%%%%

introduce_particles_lex :-
    (   alpino_cg:lex(Frame,Root,Bitcode,Dt,SimpleDt,_Surfs),
        (  Frame = verb(_,_,Subcat)
        ;  Frame = v_noun(Subcat)
        ),
        Subcat =.. [F|_],
        F \== ninv,
        alpino_postags:postag_of_frame(Frame,_Pos,Attrs),
        attr_particles(Attrs,Particles0),
        alpino_data:dt_svp(SimpleDt,SvpList),
        remove_existing_particles(Particles0,Particles,SvpList),
        assert_particles_lex(Particles,Bitcode,FinalBitcode),
	reassert_lex_bitcode(Frame,Root,Bitcode,Dt,SimpleDt,FinalBitcode),
        fail
    ;   true
    ).

remove_existing_particles([],[],_).
remove_existing_particles([Part|Parts],Rest,SvpList) :-
    svp_has_part(SvpList,Part),
    !,
    remove_existing_particles(Parts,Rest,SvpList).
remove_existing_particles([H|T0],[H|T],SvpList) :-
    remove_existing_particles(T0,T,SvpList).

svp_has_part(SvpList,Part) :-
    member(Svp,SvpList),
    alpino_data:dt(Svp,Hwrd,_,_,_),
    alpino_data:lexical(Hwrd,Part0,_,_,_,_,_,_),
    Part0 == Part.

svp_has_part(SvpList,Part) :-
    member(SvpConj,SvpList),
    alpino_data:dt_cnj_crd(SvpConj,Cnj,_),
    member(Svp,Cnj),
    alpino_data:dt(Svp,Hwrd,_,_,_),
    alpino_data:lexical(Hwrd,Part0,_,_,_,_,_,_),
    Part0 == Part.

reassert_lex_bitcode(Frame,Root,Bitcode,Dt,SimpleDt,NewBitcode) :-
    clause(alpino_cg:lex(Frame,Root,Bitcode,Dt,SimpleDt,Surfs),_,Ref),
    erase(Ref),
    assertz(alpino_cg:lex(Frame,Root,NewBitcode,Dt,SimpleDt,Surfs)).

assert_particles_lex([],Bc,Bc).
assert_particles_lex([Particle|RestParticles],Bc,FinalBc) :-
    next_bitcode(Bc,NextBc,BcRest),
    alpino_data:lexical(Hwrd,Particle,_,_,_,_,_,NextBc),
    alpino_data:dt(Dt,Hwrd,particle(Particle),_,_),
    findall(Surf,dict_entry(Particle,particle(Particle),Surf),Surfs),
    (    \+ alpino_cg:lex(particle(Particle),Particle,NextBc,Dt,Dt,Surfs)
    ->   assertz(alpino_cg:lex(particle(Particle),Particle,NextBc,Dt,Dt,Surfs))
    ;    true
    ),
    assert_particles_lex(RestParticles,BcRest,FinalBc).

:- public attr_particles/2.  % called from adt.pl
:- public frames_and_particles/7.  % called from adt.pl

frames_and_particles(Root,Sense,PosTag,Attrs,Cat,Parts,Frames) :-
    pos2frames(Root,Sense,PosTag,Cat,Attrs,Frames),
    findall(Part,frames_particle(Frames,Part),Parts0),
    sort(Parts0,Parts).

frames_particle(Frames,Part) :-
    member(Frame-_,Frames),
    alpino_postags:postag_of_frame(Frame,_,Attrs),
    member(sc=Term,Attrs),
    nonvar(Term),
    lex_particle(Term,Part).

attr_particles([],[]).
attr_particles([sc=Term|Attrs],[Particle|OtherParticles]) :-
    nonvar(Term),
    lex_particle(Term,Particle),
    !,
    attr_particles(Attrs,OtherParticles).
attr_particles([_|Attrs],OtherParticles) :-
    attr_particles(Attrs,OtherParticles).

lex_particle(Term,Particle) :-          % for verbs, v_noun
    Term =.. [Functor,Particle|_Rest],
    atom_concat(part_,_,Functor).

%%%%%%%%%%%%%%%%%%%%%%%%
% with_dt introduction %
%%%%%%%%%%%%%%%%%%%%%%%%

%% Some entries in the dictionary consist of multiple words. These so-called
%% 'with-dt' items come with a pre-packaged dependency structure.
%% Unfortunately, we cannot detect directly whether the use of with_dt entries
%% is required to realize an abstract dependency structure.
%%
%% The procedure for finding with_dt entries that should be used (if any)
%% as follows:
%%
%% For every interior node, and the list of roots it dominates over, we:
%%
%% - Filter the roots, keeping only the roots that occur in one of the
%%   with dt entries.
%% - Find all possible sublists of roots, containing at least two elements
%%   (since that is the minimum number of words that a with_dt spans).
%% - We sort each sublist, and look up the sublist in the dictionary.
%%   with_dt entries in the dictionary are accessed by the sorted list of
%%   roots.
%% - If bitcoding is used, the bits associated with the individual roots are
%%   OR'ed to retrieve the bitcode of the with_dt item.
%%
%% - complication for with_dt(complex_etc...

introduce_with_dt(FsAdt,Roots) :-
    introduce_with_dt(FsAdt,Roots,[]).

introduce_with_dt(tree(r(_,Node),Ds),Roots,Roots0) :-
    introduce_with_dt(Node,Ds,Roots,Roots0).
introduce_with_dt(p(_,PDt),Ds,Roots,Roots0) :-
    introduce_with_dt_ds(Ds,Roots,Roots0),
    construct_with_dts(Roots,PDt).
introduce_with_dt(i(_,Node),Ds,Roots,Roots0) :-
    introduce_with_dt(Node,Ds,Roots,Roots0).
introduce_with_dt(i(_),[],Roots,Roots).
introduce_with_dt(adt_lex(_,Root,Bc,_,_,_),[],[Root-Bc|Roots0],Roots0).

introduce_with_dt_ds([],Roots,Roots).
introduce_with_dt_ds([H|T],Roots,Roots0) :-
    introduce_with_dt(H,Roots1,Roots0),
    introduce_with_dt_ds(T,Roots,Roots1).

construct_with_dts(Pairs0,Pdt) :-
    findall(R,potential_with_dt_root(Pairs0,R), Pairs1),
    keysort(Pairs1,Pairs2),
    (   find_matching_with_dt(Pairs2,Roots,Codes),
	construct_with_dts_aux(Roots,Codes,Pdt),
	fail
    ;   een_of(Pairs2,Pdt),
	fail
    ;   true
    ).

een_of(Pairs0,Pdt) :-
    lists:select('een of'-C,Pairs0,Pairs1),
    lists:select(Jaar-B,Pairs1,Pairs2),
    lists:select(Dertig-A,Pairs2,Pairs3),
    (   phrasal_entry(Tag,een_N_of_NUM,[een,Jaar,of,Dertig],[]),
	merge_bc_list([A,B,C],Bc),
	hdrug_util:concat_all([een,Jaar,of,Dertig],Surf,' ')
    ;   lists:select(Veertig-D,Pairs3,_),
	merge_bc_list([A,B,C,D],Bc),
	phrasal_entry(Tag,een_N_of_NUM,[een,Jaar,of,Dertig,Veertig],[]),
	hdrug_util:concat_all([een,Jaar,of,Dertig,Veertig],Surf,' ')
    ),
    alpino_lex:generate_with_dt_stem(Tag,Label),
    assert_with_dts([dict_entry(Label,Tag,Surf)],Bc,Pdt).

find_matching_with_dt([Root-Code|Pairs0],[Root|Roots],[Code|Codes]) :-
    alpino_lex:with_dt_all(Root,List),
    find_matching_with_dt_aux(List,Pairs0,Roots,Codes).
find_matching_with_dt([Root-Code|Pairs0],[Root|Roots],[Code|Codes]) :-
    alpino_paraphrase:add_lex(RootConcat,_,with_dt(_,_)),
    alpino_util:split_atom(RootConcat," ",Atoms),
    lists:append(_,[Root|Roots],Atoms),
    find_matching_with_dt_aux(Roots,Pairs0,Roots,Codes).
find_matching_with_dt([_|Pairs],Roots,Codes) :-
    find_matching_with_dt(Pairs,Roots,Codes).

find_matching_with_dt_aux([],_,[],[]).
find_matching_with_dt_aux([Root|Roots],[Root-Code|Tail],[Root|Rs],[Code|Cs]) :-
    find_matching_with_dt_aux(Roots,Tail,Rs,Cs).
find_matching_with_dt_aux([Root|Roots],[_|Tail],Rs,Cs) :-
    find_matching_with_dt_aux([Root|Roots],Tail,Rs,Cs).

potential_with_dt_root(Roots,Root-Code) :-
    member({RootList}-Code,Roots),
    member(Root,RootList),
    is_root(Root).

potential_with_dt_root(Roots,Root-Code) :-
    member(Root-Code,Roots),
    is_root(Root).

potential_with_dt_root(Roots,Root-Code) :-
    member('een of'-_,Roots),
    member(Root-Code,Roots).

is_root('een of').

is_root(Root) :-
    alpino_lex:with_dt_root(Root), !.

is_root(Root) :-
    alpino_paraphrase:add_root(Root), !.

construct_with_dts_aux(CatRoots,Bcs,Pdt) :-
    concat_all(CatRoots,Stem,' '),
    findall(dict_entry(Stem,with_dt(Frame,Dt),Surface),
    	    dict_entry_with_dt(Stem,with_dt(Frame,Dt),Surface),

	    WithDts),
    merge_bc_list(Bcs,Bc),
    assert_with_dts(WithDts,Bc,Pdt).

assert_with_dts([],_,_).
assert_with_dts([dict_entry(Label,Frame,Surface)|T],Bc,Pdt) :-
    copy_pdt(Frame,Pdt,PdtCopy),
    (   alpino_cg:lookup_tag(Frame,Surface,_,_,PdtCopy,Surface),
	\+ alpino_cg:lex(Frame,Label,Bc,PdtCopy,PdtCopy,[Surface])
    ->  assertz(alpino_cg:lex(Frame,Label,Bc,PdtCopy,PdtCopy,[Surface]))
    ;   true
    ),
    assert_with_dts(T,Bc,Pdt).

%% In the case of etc with_dts, the list of the cnj attribute contains
%% too many elements. Do we want to remove entries from the constraints
%% here, and activate the constraints again?
copy_pdt(with_dt(complex_etc,_),Pdt,PdtCopy) :-
    !,
    copy_term(Pdt,PdtCopy,_Cons).
copy_pdt(_,Pdt,PdtCopy) :-
    copy_term(Pdt,PdtCopy).


punct('`', aanhaal_links).
punct('\'', aanhaal_rechts).
punct(:,    dubb_punt).
punct('(',  haak_open).
punct(')',  haak_sluit).
punct(...,  hellip).
punct(=,    is_gelijk).
punct(',',  komma).
punct(-,    ligg_streep).
punct('.',  punt).
punct(;,    punt_komma).
punct('/',  schuin_streep).
punct('!',  uitroep).
punct('?',  vraag).
punct(x,    maal).
punct('×',  maal).
punct(+,    plus).
punct(&,    ampersand).
punct('|',  staand_streep).

%%%%%%%%%%%%%%%%%%%%%
% Inflection lookup %
%%%%%%%%%%%%%%%%%%%%%

dict_entry({L},Frame,Surface) :-
    !,
    lists:member(El,L),
    dict_entry(El,Frame,Surface).

dict_entry(Root,Frame,SurfaceAtom) :-
    root_surface(Root,SurfaceAtom,SurfaceList),
    debug_message(3,"lex lookup for surface form ~w~n",
                             [SurfaceAtom]),
    alpino_lex:lexicon(Frame0,Root0,SurfaceList,[],_),

    simplify_lemma(Root0,Root),
    \+ filter_adj_end(Root,Frame0),
    (   adapt_frame(SurfaceAtom,Frame0,Frame)
    ->  true
    ;   Frame0=Frame
    ).

dict_entry_robust(Root,Frame,SurfaceAtom) :-
    root_surface(Root,SurfaceAtom,SurfaceList),
    debug_message(3,"lex lookup for surface form ~w~n",
		  [SurfaceAtom]),
    alpino_lex:lexicon(Frame0,Root0,SurfaceList,[],_),
    simplify_lemma(Root0,Root),
    \+ filter_adj_end(Root,Frame0),
    add_simple_frames(Frame0,Frame).


%% to paraphrase "de bootjes zijn ver landinwaarts neergekwakt"
add_simple_frames(verb(_,VF,Sc), verb('hebben/zijn',VF,Sc)).

add_simple_frames(verb(_,VF,_),
		  verb('hebben/zijn',VF,intransitive)).
add_simple_frames(verb(_,VF,_),
		  verb('hebben/zijn',VF,transitive)).
add_simple_frames(v_noun(_),v_noun(intransitive)).
add_simple_frames(v_noun(_),v_noun(transitive)).
add_simple_frames(X,X) :-
    \+ X = verb(_,_,_).

dict_entry_with_dt(Root,Frame,SurfaceAtom) :-
    alpino_lex:inv_lex(Root,SurfaceAtom),
    surf_to_list(SurfaceAtom,SurfaceList,[]),
    %% format(user_error,"lexical lookup with_dt: ~w~n",[SurfaceList]),
    alpino_lex:lexicon(Frame,Root0,SurfaceList,[],_),
    simplify_lemma(Root0,Root).

dict_entry_with_dt(Root,with_dt(A,B),SurfaceAtom) :-
    alpino_paraphrase:add_lex(Root,SurfaceAtom,with_dt(A,B)).

%%% hack
filter_adj_end(Root,End) :-
    adj_end_v_amb(Root),
    end_adj(End).

end_adj(adjective(end(_))).
end_adj(adjective(ende(_))).

adj_end_v_amb(internet).
adj_end_v_amb(bruin).
adj_end_v_amb(dicht).
adj_end_v_amb(nodig).
adj_end_v_amb(veel).
adj_end_v_amb(wel).
adj_end_v_amb(zeker).

simplify_lemma(v_root(Root0,_),Root) :-
    !,
    Root0=Root.
simplify_lemma(Root,Root).

root_surface(Root,Surf,[SurfAtom|SurfList]) :-
    alpino_lex:inv_lex(Root,Surf),
    split_atom(Surf," ",[SurfAtom|SurfList]).

root_surface(Root,SurfAtom,SurfList) :-
    split_atom(Root," ",AtomList),
    length(AtomList,Len),
    Len < 6,
    root_surface_list(AtomList,SurfList,[]),
    hdrug_util:concat_all(SurfList,SurfAtom,' ').

root_surface_list([],L,L).
root_surface_list([H|T],L0,L) :-
    root_surface__(H,L0,L1),
    root_surface_list(T,L1,L).

root_surface__(Root,L0,L) :-
    surf_to_list(Root,L0,L).
	
root_surface__(Root,L0,L) :-
    alpino_lex:inv_lex(Root,Surf),
    surf_to_list(Surf,L0,L).

root_surface__(VerNVoudig,[Surf|S],S) :-
    atom_concat(_,voudig,VerNVoudig),
    (  atom_concat(VerNVoudig,d,Surf)
    ;  atom_concat(VerNVoudig,t,Surf)
    ;  atom_concat(VerNVoudig,en,Surf)
    ;  atom_concat(VerNVoudig,de,Surf)
    ;  atom_concat(VerNVoudig,den,Surf)
    ).

root_surface__(Root,L0,L) :-
    atom_concat(Pre,tal,Root),
    atom_concat(Pre,tallen,N),
    surf_to_list(N,L0,L).

%% for genitive inflection of names
root_surface__(Root,[Surf|L],L) :-
    (   Root = Surf0
    ;   alpino_lex:inv_lex(Root,Surf0)
    ),
    alpino_lex:in_names_dictionary(_Name,Root,Surf0,[],[],_),
    add_gen(Surf0,Surf).

surf_to_list(Surf,List0,List) :-
    atom_codes(Surf,Codes),
    alpino_util:split_string(Codes," ",CodesList),
    atom_codes_list(CodesList,List0,List).

atom_codes_list([],L,L).
atom_codes_list([H0|T0],[H|L0],L) :-
    atom_codes(H,H0),
    atom_codes_list(T0,L0,L).

/*
atom_or_list([],H,H).
atom_or_list([H|T],F,[F,H|T]).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% POS tag and attributes to frames %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pos2frames(Root,Sense,Pos,Cat,Attr,Frames) :-
    findall(Frame-Surfs,
            pos2frames_aux(Root,Sense,Pos,Cat,Attr,Frame,Surfs),
            Frames0),
    (  var(Sense)  -> Sense = Root ; true ),
    (  var(Root)   -> Sense = Root ; true ),
    root_mismatch_heuristics(Frames0,Frames1,Root,Sense,Pos,Attr,Cat),
    unknown_root_heuristics(Frames1,Frames2,Root,Sense,Pos,Attr,Cat),
    last_resort_heuristics(Frames2,Frames3,Root,Sense,Pos,Cat,Attr),
    tag_mismatch_heuristics(Frames0,Frames3,Frames4,Root,Sense,Pos,Attr),
    prefer_best_words_in_frames(Root,Frames4,Frames5),
    added_paraphrase_heuristics(Frames5,Frames6,Root,Sense,Pos,Attr),
    remove_redundant_frames(Frames6,Frames).				% add later, so are not removed

remove_redundant_frames(Frames0,Frames):-
    redundant(Frame,GenFrame),
    lists:select(Frame-Surf,Frames0,Frames1),
    lists:member(GenFrame-Surf,Frames1),
    !,
    remove_redundant_frames(Frames1,Frames).
remove_redundant_frames(F,F).

/*
remove_redundant_frames([],[]).
remove_redundant_frames([Frame0|Frames0],Frames):-
    redundant_frame(Frame0,Frames0), !,
    remove_redundant_frames(Frames0,Frames).
remove_redundant_frames([Frame|Frames0],[Frame|Frames]):-
    remove_redundant_frames(Frames0,Frames).

redundant_frame(Frame0-Surf,Frames) :-
    redundant(Frame0,Frame1),
    lists:member(Frame1-Surf,Frames).
*/

redundant(noun(X,Y,_),noun(X,Y,both)).
redundant(noun(X,_,Y),noun(X,both,Y)).
redundant(noun(_,X,Y),noun(both,X,Y)).
redundant(proper_name(sg,X),proper_name(both,X)).
redundant(proper_name(pl,X),proper_name(both,X)).
redundant(verb(hebben,Inf,Sc),verb('hebben/zijn',Inf,Sc)).

pos2frames_aux(Root,Sense,Pos,Cat,Attr,Frame,Surfs) :-
    setof(Surf,dict_entry(Root,Frame,Surf),Surfs),
    alpino_treebank:frame2sense(Root,Frame,Sense),
    alpino_postags:postag_of_frame(Frame,Pos,CheckAttr),
    check_attributes(CheckAttr,Attr,Pos,Frame,Root),
    \+ wrong_cat(Frame,Cat).

pos2frames_aux_robust(Root,Sense,Pos,Cat,Attr,Frame,Surfs) :-
    setof(Surf,dict_entry_robust(Root,Frame,Surf),Surfs),
    alpino_treebank:frame2sense(Root,Frame,Sense),
    alpino_postags:postag_of_frame(Frame,Pos,CheckAttr),
    check_attributes(CheckAttr,Attr,Pos,Frame,Root),
    \+ wrong_cat(Frame,Cat).

root_mismatch_heuristics([H|T],[H|T],_,_,_,_,_).
root_mismatch_heuristics([],Frames,Lemma,_Sense,Pos,Attr,Cat) :-
    alpino_lex:lexicon(_,RealStem,[Lemma],[],_),
    atomic(RealStem),
    findall(Frame-Surfs,
            pos2frames_aux(RealStem,RealStem,Pos,Cat,Attr,Frame,Surfs),
            Frames),
    Frames = [_|_],
    debug_message(2,"assuming root '~w' is specified as lemma '~w' in ADT~n",
		  [RealStem,Lemma]),    
    !.
root_mismatch_heuristics([],[],_,_,_,_,_).

tag_mismatch_heuristics([_|_],Fs,Fs,_,_,_,_).
tag_mismatch_heuristics([],Frames0,Frames,Root,Sense,Pos,Attr) :-
    (   Pos == prefix
    ->  Frames0 = Frames
    ;   findall(Frame-Surfs,pos2frames_aux(Root,Sense,_,_,Attr,Frame,Surfs),Frames1,Frames0),
	(   Pos == noun,
	    \+ (  nonvar(Sense), sub_atom(Sense,_,_,_,'_') ),
	    \+ (  nonvar(Sense), sub_atom(Sense,_,_,_,' ') ),
	    \+ Attr = [_|_]
	->  Frames = [noun(both,both,both)-[Sense]|Frames1]
	;   Frames1 = Frames
	)    
    ).

check_attributes(CheckAtts,Atts,Pos,Frame,Root) :-
    check_attributes1(CheckAtts,Atts,Remainder,Frame,Root),
    ignore_input_atts(Remainder,CheckAtts,Pos,Frame).

%% check_attributes(DictAtts,InputAtts)
%% we must find all InputAtts
%% and there must not be an inconsistency with DictAtts
check_attributes1([],Atts,Atts,_,_).
check_attributes1([Att=Val|T],Attrs0,Attrs,Frame,Root) :-
    (   Att == postag,
	Val == 'NA()'
    ->  Attrs0 = Attrs1
    ;   select(Att=Val1,Attrs0,Attrs1)
    ->  Val = Val1
    ;   ignore_current_att(Root,Att,Val,Frame),
	Attrs0 = Attrs1
    ),
    check_attributes1(T,Attrs1,Attrs,Frame,Root).


%%% these attributes are "obligatory" in the input:
%%% iets  personalized pron
%%% all other attributes in the input *must* be
%%% confirmed in the lexical entries - except for the
%%% two special attributes "rnum" and "stype" which
%%% will be "unified" into the signs of the lexical entries

ignore_current_att(L,pron,_,_):-
    member(L,[ieder,veel,deze,alle]),
    !,
    fail.
ignore_current_att(_,A,B,C) :-
    ignore_current_att(A,B,C).

ignore_current_att(iets,_,_) :-
    !,
    fail.
ignore_current_att(personalized,_,_) :-
    !,
    fail.
ignore_current_att(aform,compar,adjective(meer)) :-
    !.
ignore_current_att(aform,compar,_) :-
    !,
    fail.
ignore_current_att(aform,super,_) :-
    !,
    fail.
%ignore_current_att(pron) :-
%    !,
%    fail.
ignore_current_att(infl,ener,_) :-
    !,
    fail.
ignore_current_att(tense,past,_) :-
    !,
    fail.
ignore_current_att(stype,imparative,_) :-
    !,
    fail.

ignore_current_att(_,_,_).

ignore_input_atts([],_,_,_).
ignore_input_atts([H=V|T],Atts,Pos,Frame) :-
    ignore_input_att(H,V,Atts,Pos,Frame),
    ignore_input_atts(T,Atts,Pos,Frame).

ignore_input_att(H,_,_,Pos,Frame) :-
    ignore_input_att(H,Pos,Frame).

ignore_input_att(tense,_,_).  % after paraphrasing un-passive "para er worden door schepen veel containers overboord gezet
ignore_input_att(rnum,_,_).   % not really ignored, of course, but treated differently
ignore_input_att(stype,_,_).  % not really ignored, of course, but treated differently
ignore_input_att(dropped_agr,_,_). % not really ignored, of course, but treated differently
ignore_input_att(dropped_prs,_,_). % not really ignored, of course, but treated differently
ignore_input_att(lemma,_,_).
ignore_input_att(per,Tag,Frame) :-
    ignore_per(Tag,Frame).

ignore_per(det,_).
ignore_per(noun,_).
ignore_per(tag,_).
ignore_per(adv,_).
ignore_per(pron,rel_pronoun(_,_)).

exc_postag_of_frame(Frame,Pos,Attrs) :-
    (   alpino_postags:postag_of_frame(Frame,Pos,Attrs)
    ->  fail
    ;   Pos=noun,                          % exceptions for old treebanks
        alpino_postags:postag_of_frame(Frame,name,Attrs)
    ;   Pos=noun,
        alpino_postags:postag_of_frame(Frame,pron,Attrs)
    ;   Pos=noun,
        alpino_postags:postag_of_frame(Frame,adv,Attrs)
    ;   Pos=adj,
        alpino_postags:postag_of_frame(Frame,adv,Attrs)
    ;   Pos=adv,
        alpino_postags:postag_of_frame(Frame,adj,Attrs)
    ;   Pos=adv,
        alpino_postags:postag_of_frame(Frame,pp,Attrs)
    ).
    
%%% some frames should only be attempted if certain properties of the
%%% corresponding DT are satisfied and/or certain other tags are present

%% these are *global* checks:
filter_tags :- 
    alpino_filter_tag:initialize_filter_tags,
    (   alpino_cg:clause(lex(Tag,Root,Bc,_,_,_),_,Ref),
        (   alpino_filter_tag:filter_tag(Tag,Root,Bc,Bc)
        ->  true
        ;   erase(Ref)
        ),
        fail
    ;   true
    ).

%% these are *local* checks; there could also be co-occurrences checks.
check_conditions(Frame,Dt,SimpleDt,REL) :-
    findall(Check,condition(Frame,Check),Checks),
    (   \+ apply_checks(Checks,SimpleDt,REL)
    ->  debug_message(3,"frame ~w discarded~n",[Frame]),
	fail
    ;   true
    ),
    \+ \+ lookup(Frame,Dt).

lookup(Frame,Dt) :-
    alpino_lex_types:lex(Cat,Frame,_,_),  % ignore constrains for now, because not all lexical items are present
    alpino_data:dt(Cat,Dt).

apply_checks([],_,_).
apply_checks([H|T],Dt,REL) :-
    apply_check(H,Dt,REL),
    apply_checks(T,Dt,REL).

call_constraints([]).
call_constraints([H|T]) :-
    call_constraint(H),!, % one is enough
    call_constraints(T).

call_constraint(als_word) :-
    alpino_cg:lex(complementizer(als),_,_,_,_,_).

call_constraint(er_word) :-
    er_tag(Tag),
    alpino_cg:lex(Tag,_,_,_,_,_).

call_constraint(tag(Tag)) :-
    alpino_cg:lex(Tag,_,_,_,_,_).

er_tag(er_vp_adverb).     % er
er_tag(er_loc_adverb).    % daar hier ergens nergens overal
er_tag(er_wh_loc_adverb). % waar
er_tag(iets_adverb).      % ergens nergens (anders)
er_tag(wh_iets_adverb).   % waar (anders)

condition(determiner(der),der).
condition(determiner(ener),der).
condition(determiner(des),des).
condition(determiner(pron),rel(det)).
condition(postnp_adverb,rel(mod)).
condition(postadv_adverb,(mcat(advp);mcat(pp))).
condition(postadj_adverb,mcat(ap)).
condition(postnp_adverb,mcat(np)).
condition(postp_adverb,mcat(pp)).
condition(modal_adverb,not_sent_cat).
condition(modal_adverb(_),not_sent_cat).
condition(post_wh_adverb,(mcat(np);mcat(ap);mcat(pp);mcat(advp))).
condition(predm_adverb,(rel(dp);rel(predm))).
condition(eenmaal_adverb,rel(dp)).
condition(reflexive(_,_),rel(se)).
condition(preposition(_,_,me_adj),obj1_me).
condition(preposition(_,_,pp),not_cat(obj1,np)).
condition(preposition(_,_,pp),not_cat(obj1,advp)).
condition(gen_determiner(_),gen_det).
condition(verb(_,_,Sc),Cond) :-
    condition_sc(Sc,Cond).
condition(verb(_,Inf,Sc),Cond) :-
    condition_infl(Inf,Sc,Cond).
condition(v_noun(_),cat(np)).
condition(v_noun(_),not_stype).
condition(v_noun(Sc),Cond) :-
    condition_sc(Sc,Cond).
condition(v_noun(transitive),has_obj1).
condition(v_noun(copula),predc).
condition(v_noun(nonp_copula),predc).
condition(noun(_,_,_,Sc),Cond) :-
    condition_noun_sc(Sc,Cond).
condition(tmp_noun(_,_,_,Sc),Cond) :-
    condition_noun_sc(Sc,Cond).
condition(mod_noun(_,_,_,Sc),Cond) :-
    condition_noun_sc(Sc,Cond).
condition(meas_mod_noun(_,_,_,Sc),Cond) :-
    condition_noun_sc(Sc,Cond).
condition(amount_meas_mod_noun(_,_,_,Sc),Cond) :-
    condition_noun_sc(Sc,Cond).

condition(vandaar_adverb,rel(dp)).
condition(preposition(Prep,_,pc_adv),Cond) :-
    pc_adv_condition(Prep,not_cat(obj1,np),Cond).
condition(preposition(_,_,pc_vp),cat(vc,ti)).
condition(preposition(_,_,voor_pred),rel(predc)).

condition(comp_adverb(_),obcomp).

condition(adjective(Infl),infl_adj(Infl)).
condition(adjective(Infl,_),infl_adj(Infl)).

condition(pronoun(_,_,_,_,gen,_),der).

%% added obj1 "tot op vandaag de dag"
pc_adv_condition(NotVan,Cond0,(Cond0,(rel(pc);rel(whd);rel(rhd);rel(obj1)))) :-  
    NotVan \== van,
    NotVan \== met,
    !.
pc_adv_condition(_NotVan,Cond,Cond).


condition_infl(Infl,_,Cond) :-
    condition_infl(Infl,Cond).
condition_infl(X, SC, not_plural_su) :-
    sg_infl(X),
    \+ short_sbar_subj(SC).

condition_infl(pl,passive,real_su).
condition_infl(sg1,passive,real_su).
condition_infl(sg_hebt,passive,real_su).
%condition_infl(Infl,Subcat,su_not_het) :-
%    pl_infl(Infl),
%    Subcat \= cleft_np,
%    Subcat \= cleft.
%condition_infl(Infl,_,su_not_het) :-
%    not_third_infl(Infl).

condition_infl(psp,cat(ppart)).
condition_infl(psp,not_stype).
condition_infl(inf,cat(inf)).
condition_infl(inf,not_stype).
condition_infl(inf(no_e),cat(inf)).
condition_infl(inf(no_e),not_stype).
condition_infl(inf(e),rel(body)).
condition_infl(inf(e),not_stype).
condition_infl(imp(_),cat(sv1)).
condition_infl(X,( cat(smain)
                 ; cat(ssub)
                 ; cat(whq)
                 ; cat(sv1)
                 )) :-
    finite_infl(X).

condition_infl(X, not_sg_su) :-
    pl_infl(X).

condition_infl(X, not_first_person) :-
    third_infl(X).

condition_infl(X, not_third_person) :-
    not_third_infl(X).

condition_infl(sg_bent,not_first_person).
condition_infl(sg_bent,not_third_person).
condition_infl(sg_bent,not_plural_su).

condition_infl(sg_heeft,not_first_person).
condition_infl(sg_heeft,not_second_person).
condition_infl(sg_heeft,not_plural_su).

condition_infl(sg_is,not_first_person).
condition_infl(sg_is,not_second_person).
condition_infl(sg_is,not_plural_su).

condition_infl(sg1,not_u_person).

condition_infl(sg_hebt,not_third_person).

condition_infl(modal_inv,not_third_person).
condition_infl(modal_inv,not_first_person).

condition_infl(imp(_),not_su).

condition_infl(imp(_),imp).

condition_infl(modal_inv,not_first_person).

not_third_infl(sg1).
not_third_infl(imp(sg1)).
not_third_infl(imp(modal_u)).
not_third_infl(sg_hebt).

third_infl(sg3).
third_infl(sg_heeft).
third_infl(sg_hebt).

sg_infl(sg).
sg_infl(sg1).
sg_infl(sg3).
sg_infl(sg_heeft).
sg_infl(sg_hebt).
sg_infl(past(sg)).
sg_infl(modal_u).
sg_infl(modal_not_u).
sg_infl(modal_inv).
sg_infl(imp(sg1)).
sg_infl(imp(modal_u)).

pl_infl(pl).
pl_infl(past(pl)).
pl_infl(both(pl)).

condition_noun_sc(measure,ne_list(mod)).
condition_noun_sc(app_measure,ne_list(app)).
condition_noun_sc(np_app_measure,ne_list(app)).
condition_noun_sc(start_app_measure,ne_list(app)).

condition_sc(inverted_aux(L),Cond) :-
    condition_sc(aux(L),Cond).
condition_sc(so_aux(L),Cond) :-
    condition_sc(aux(L),Cond).
condition_sc(aux(inf),cat(vc,inf)).
condition_sc(aux_simple(inf),cat(vc,inf)).
condition_sc(aux(te),cat(vc,ti)).
condition_sc(aux(te_inf),( cat(vc,ti)
                         ; cat(vc,inf)
                         )).
condition_sc(aux(psp),cat(vc,ppart)).
condition_sc(aux(_),dt(vc)).
condition_sc(modifier(Sub),Cond) :-
    condition_sc(Sub,Cond).
condition_sc(aux_psp_hebben,(cat(vc,ppart)
                            ;ipp
                            )).                            
condition_sc(aci,cat(vc,inf)).
condition_sc(aci_no_obj,cat(vc,inf)).
condition_sc(passive,cat(vc,ppart)).
condition_sc(passive,passive).
condition_sc(no_subj,no_subj).
condition_sc(aci_simple,(cat(vc,inf),
                         vc_simple)).
condition_sc(aci_simple(_),(dt(vc),
                            vc_simple)).
condition_sc(sbar_obj_opt_het,(cat(vc,cp)
                              ;cat(vc,whsub)
                              )).
condition_sc(sbar,sbar_vc).
condition_sc(np_sbar,sbar_vc).
condition_sc(tr_sbar,sbar_vc).
%condition_sc(copula_sbar,(if_cat(su,cp);if_cat(su,whsub))).  % de vraag is met wie
%condition_sc(so_copula_sbar,(if_cat(su,cp);if_cat(su,whsub))). % de vraag lijkt me met wie
condition_sc(copula_vp,(cat(su,ti)
                       ;cat(su,oti)
                       )).
condition_sc(cleft,not_sup).
condition_sc(copula,not_sup).
condition_sc(nonp_copula,not_sup).
condition_sc(so_copula_vp,(cat(su,ti)
                          ;cat(su,oti)
                          )).
condition_sc(so_pp_sbar,( sbar_vc,
                          cat(obj2,pp)
                        )).
condition_sc(subj_control(pass_te),cat(vc,ti)).
condition_sc(subj_control(te),cat(vc,ti)).
condition_sc(obj_control(pass_te),cat(vc,ti)).
condition_sc(obj_control(te),cat(vc,ti)).
condition_sc(te_passive,cat(vc,ti)).
condition_sc(sbar_subj_te_passive,cat(vc,ti)).
condition_sc(aan_het,cat(vc,ahi)).
condition_sc(np_aan_het,cat(vc,ahi)).
condition_sc(op,cat(vc,ahi)).
condition_sc(pc_pp(Prep),pc(Prep)).
condition_sc(refl_pc_pp(Prep),(pc(Prep),dt(se))).
condition_sc(np_pc_pp(Prep),pc(Prep)).
condition_sc(uit,cat(vc,ahi)).
condition_sc(van_sbar,cat(vc,svan)).
condition_sc(vp,(  cat(vc,ti)
                ;  cat(vc,oti)
                )).
condition_sc(np_vp_subj,(  cat(vc,ti)
                        ;  cat(vc,oti)
                        )).
condition_sc(np_vp_obj,(  cat(vc,ti)
                       ;  cat(vc,oti)
                       )).
condition_sc(so_vp_obj,(  cat(vc,ti)
                       ;  cat(vc,oti)
                       )).
condition_sc(copula_np,predc_obj).
condition_sc(so_copula_np,predc_obj).

% condition_sc(copula,predc).    "keurig als we zijn, ..."

condition_sc(pred_np,predc).
condition_sc(nonp_pred_np,predc).
condition_sc(nonp_pred_np_ndev,predc).
condition_sc(ap_pred_np,predc).
condition_sc(als_pred_np,predc).

%%%% ik verzocht toen op de hoogte te worden gesteld
%condition_sc(pred_np,obj1).
%condition_sc(nonp_pred_np,obj1).
%condition_sc(nonp_pred_np_ndev,obj1).
%condition_sc(ap_pred_np,obj1).
%condition_sc(als_pred_np,obj1).

condition_sc(ninv(L,_),Cond) :-
    condition_sc(L,Cond).
condition_sc(ninv(L,_),ninv) :-
    \+ L = incorporated_subj_topic(_).

condition_sc(incorporated_subj_topic(L),Cond) :-
    condition_sc(L,Cond).
condition_sc(fixed(List,_),Cond) :-
    member(El,List),
    condition_fixed_el(El,Cond).
condition_sc(np_np,np_np).
condition_sc(np_sbar,np_sbar).
condition_sc(np_np_mod_pp(_),np_np).
condition_sc(np_np_mod_pp(Prep),mod_pp(Prep)).
condition_sc(ld_adv,not_cat(ld,pp)).
condition_sc(ld_pp,not_cat(ld,advp)).
condition_sc(np_mod_pp(Prep),mod_pp(Prep)).
condition_sc(mod_pp(Prep),mod_pp(Prep)).


condition_fixed_el({List},Cond) :-
    member(El,List),
    condition_fixed_el(El,Cond).
condition_fixed_el({[acc,dat]},np_np).
condition_fixed_el({[dat,acc]},np_np).
condition_fixed_el(vp,dt(vc)).
%%condition_fixed_el(extra_obj_vp(_,_),dt(vc)).  no, the vc is within the pc
condition_fixed_el(pc(Prep),pc(Prep)).
condition_fixed_el(er_pp(Prep),pc(Prep)).
condition_fixed_el(er_pp(Prep,_),pc(Prep)).
condition_fixed_el(pred,dt(predc)).
condition_fixed_el(pred(Root),pred_root(Root)).
condition_fixed_el(ap_pred(Root),pred_root(Root)).
condition_fixed_el(ap_pred,dt(predc)).
condition_fixed_el(np_pred,dt(predc)).
condition_fixed_el(als_pred,dt(predc)).
condition_fixed_el(subj(Root),su_root(Root)). 
%condition_fixed_el(acc(Root),acc_root(Root)).   % de eer die wij hier aan zullen behalen
condition_fixed_el(pp_refl(Voor),(pc(Voor),
                                  pp_refl)).
condition_fixed_el(vc(Root,_,_),svp_root(Root)).
condition_fixed_el(svp_pp(A,B),svp_pp(A,B)).

:- use_module('../Hdrug/Prolog/hdrug_feature').

apply_check(gen_det,_,[hd,det/detp|_]).

apply_check(obcomp,DT,_) :-
    DT:obcomp => [],
    !,
    fail.
apply_check(obcomp,_,_).

apply_check(imp,DT,_) :-
    DT:attrs <=> Attrs,
    lists:member(stype=Type,Attrs),
    Type = imparative.

apply_check(real_su,DT,_) :-
    DT:attrs <=> Attrs,
    lists:member(stype=Type,Attrs),
    \+ Type=topic_drop,
    DT:su => [],
    !,
    fail.
apply_check(real_su,_,_).

apply_check(not_plural_su,DT,_) :-
    DT:su:attrs <=> List,
    nonvar(List),
    lists:member(num=pl,List),
    !,
    fail.
apply_check(not_plural_su,DT,_) :-
    DT:su:attrs <=> List,
    nonvar(List),
    lists:member(rnum=pl,List),
    !,
    fail.
apply_check(not_plural_su,_,_).

apply_check(not_stype,DT,_) :-
    DT:attrs <=> List,
    nonvar(List),
    lists:member(stype=_,List),
    !,
    fail.
apply_check(not_stype,_,_).

apply_check(ninv,DT,_) :-
    DT:cat <=> Cat,
    \+ Cat == smain,
    \+ Cat == whq,
    \+ Cat == sv1.

apply_check(not_sg_su,DT,_) :-
    DT:su:cat <=> CP,
    CP == cp,!,
    fail.
apply_check(not_sg_su,DT,_) :-
    DT:su:attrs <=> List,
    nonvar(List),
    lists:member(num=sg,List),
    !,
    fail.
apply_check(not_sg_su,DT,_) :-
    DT:attrs <=> List,
    nonvar(List),
    lists:member(stype=imparative,List),
    !,
    fail.
apply_check(not_sg_su,DT,_) :-
    DT:su:attrs <=> List,
    nonvar(List),
    lists:member(rnum=sg,List),
    !,
    fail.
apply_check(not_sg_su,DT,_) :-
    DT:su:attrs <=> List,
    nonvar(List),
    lists:member(verb,List),
    !,
    fail.
apply_check(not_sg_su,DT,_) :-
    DT:su:hwrd:lexical <=> Je,
    nonvar(Je),
    lists:member(Je,[ik,je,jij,hij /*,het*/]),   %het zijn belastingbetalers
    !,
    fail.
apply_check(not_sg_su,_,_).

apply_check(su_not_het,DT,_) :-
    DT:su:hwrd:lexical <=> Het,
    nonvar(Het),
    lists:member(Het,[het,dit,dat]),
    !,
    fail.
apply_check(su_not_het,_,_).

apply_check(not_su,DT,_) :-
    DT:su:hwrd <=> _,
    !,
    fail.
apply_check(not_su,_,_).

apply_check(not_sup,DT,_) :-
    DT:sup:hwrd <=> _,
    !,
    fail.
apply_check(not_sup,_,_).
    

apply_check(not_first_person,DT,_) :-
    DT:su:attrs <=> List,
    nonvar(List),  % how does this happen?
    lists:member(per=fir,List),
    !,
    fail.
apply_check(not_first_person,DT,_) :-
    DT:attrs <=> List,
    nonvar(List),  % how does this happen?
    lists:member(stype=imparative,List),
    !,
    fail.
apply_check(not_first_person,_,_).

%%% conjunctions are "always" third person
apply_check(not_third_person,DT,_) :-
    DT:su:cnj <=> [_|_],
    !,
    fail.

apply_check(not_third_person,DT,_) :-
    DT:su:cat <=> CP,
    CP == cp,
    !,
    fail.

%%% things with a determiner are "always" third person
apply_check(not_third_person,DT,_) :-
    DT:su:det <=> [_|_],
    !,
    fail.
apply_check(not_third_person,DT,_) :-
    DT:su:attrs <=> List,
    nonvar(List),  % how does this happen?
    lists:member(per=thi,List),
    !,
    fail.
apply_check(not_third_person,DT,_) :-
    DT:su:attrs <=> List,
    nonvar(List),  % how does this happen?
    lists:member(verb,List),
    !,
    fail.
apply_check(not_third_person,DT,_) :-
    DT:su:attrs <=> List,
    nonvar(List),  % how does this happen?
    lists:member(pos=noun,List),
    !,
    fail.
apply_check(not_third_person,DT,_) :-
    DT:su:attrs <=> List,
    nonvar(List),  % how does this happen?
    lists:member(pos=name,List),
    !,
    fail.
apply_check(not_third_person,DT,_) :-
    DT:su:attrs <=> List,
    nonvar(List),  % how does this happen?
    lists:member(pos=det,List),
    !,
    fail.
apply_check(not_third_person,DT,_) :-
    DT:su:hwrd:lexical <=> Stem,
    nonvar(Stem),
    lists:member(Stem,[die,dat,wie,wat]),
    !,
    fail.
apply_check(not_third_person,_,_).

apply_check(not_second_person,DT,_) :-
    DT:su:attrs <=> List,
    nonvar(List),  % how does this happen?
    lists:member(per=je,List),
    !,
    fail.
apply_check(not_second_person,_,_).

apply_check(not_u_person,DT,_) :-
    DT:su:attrs <=> List,
    nonvar(List),  % how does this happen?
    lists:member(per=u,List),
    !,
    fail.
apply_check(not_u_person,_,_).

apply_check(dt_part(Cond),_,_) :-
    alpino_adt:dt_part(DT),
    apply_check(Cond,DT,none).
apply_check(true,_,_).
apply_check(ne_list(Path),Dt,_Rel) :-
    Dt:Path <=> [_|_].
apply_check(dt(Path),Dt,_Rel) :-
    Dt:Path => dt.
apply_check(Path<=>Val,Dt,_Rel) :-
    Dt:Path <=> Val.
apply_check(Path==>Val,Dt,_Rel) :-
    Dt:Path ==> Val.
apply_check(Path=>Val,Dt,_Rel) :-
    Dt:Path => Val.
apply_check((A,B),Dt,Rel) :-
    apply_check(A,Dt,Rel),
    apply_check(B,Dt,Rel).
apply_check((A;_B),Dt,Rel) :-
    apply_check(A,Dt,Rel).
apply_check((_;B),Dt,Rel) :-
    apply_check(B,Dt,Rel).
apply_check(su_root(Val),Dt,_Rel) :-
    su(Dt,Su),
    root(Su,Val).
apply_check(acc_root(Val),Dt,_Rel) :-
    obj1(Dt,Su),
    root(Su,Val).
apply_check(pred_root(Val),Dt,_Rel) :-
    predc(Dt,Su),
    root(Su,Val).
apply_check(vc_root(Val),Dt,_Rel) :-
    vc(Dt,Su),
    root(Su,Val).
apply_check(cat(Rel,Cat),Dt,_Path) :-
    dep_cat(Rel,Cat,Dt).
apply_check(not_cat(Rel,Cat),Dt,_Path) :-
    not_dep_cat(Rel,Cat,Dt).
apply_check(if_cat(Rel,Cat),Dt,_Path) :-
    dep_if_cat(Rel,Cat,Dt).
apply_check(cat(Cat),Dt,_Path) :-
    Dt:cat <=> Cat.
%    cat(Cat,Path).
apply_check(rel(Rel),_,Path) :-
    rel(Rel,Path).
apply_check(ipp,Dt,_) :-
    ipp(Dt).
apply_check(vc_simple,Dt,_) :-
    vc_simple(Dt).
apply_check(pc(Prep),Dt,_) :-
    pc(Prep,Dt).
apply_check(predc,Dt,_) :-
    predc(Dt,Predc),
    Predc => dt.
apply_check(obj1,Dt,_) :-
    obj1(Dt,O),
    O => dt.
apply_check(obj2,Dt,_) :-
    obj2(Dt,O),
    O => dt.
apply_check(predc_obj,Dt,_) :-
    predc(Dt,Predc),
    (   obj1(Predc,Obj1),
        Obj1 => dt
    ;  pobj1(Predc,Obj1),
        Obj1 => dt
    ;  se(Predc,Obj1),
        Obj1 => dt
    ).
apply_check(obj1_me,Dt,_) :-
    obj1(Dt,Obj1),
    me(Obj1,Me),
    Me => dt.
apply_check(pp_refl,Dt,_) :-
    dtpc(Dt,Pc),
    Pc:se => dt.
apply_check(nlex(Path),Dt,_) :-
    Dt:Path <=> Arg,
    Arg:hwrd /=> hwrd.

%% problems......
apply_check(svp_root(V),Dt,_):-
    Dt:svp <=> List,
    member(Part,List),
    root(Part,V).

apply_check(svp_pp(P,N),Dt,_) :-
    Dt:svp <=> List,
    member(PP,List),
    root(PP,Prep),
    prep(Prep,P),
    obj1(PP,NP),
    root(NP,N).   

apply_check(des,_,Path) :-
    apply_des(Path).

apply_check(der,_,Path) :-
    apply_der(Path).

apply_check(mcat(Cat),_,[_,_/Cat|_]).

apply_check(not_sent_cat,_,Path) :-
    not_sent_cat(Path).

%% vc is not always there (dip, zoals-cp, ..)
%% normally cp or whsub
%% but it can also be wh-cat,
%% "hij vroeg me waarom"
%% and coordination as usual
apply_check(sbar_vc,Dt,_PATH) :-
    Dt:vc <=> RelDT,
    (   RelDT => []
    ;   RelDT => dt,
        (  RelDT:conj <=> List,
           member(EmDt,List),
           EmDt:cat ==> Cat
        ;  RelDT:cat ==> Cat
        ),
        \+ (nonvar(Cat), member(Cat,[inf,ti,oti,svan]))
    ).


apply_check(passive,Dt,_) :-
    Dt:su <=> SU,
    SU \= [],
    Dt:vc:su <=> SU2,
    SU == SU2,
    !,
    fail.
apply_check(passive,_,_).

apply_check(no_subj,Dt,_) :-
    Dt:su => [].

apply_check(has_obj1,Dt,_) :-
    Dt:obj1 <=> Obj1,
    \+ Obj1 = [].

apply_check(np_np,Dt,_) :-
    Dt:obj1 <=> Obj1,
    Dt:obj2 <=> Obj2,
    \+ (Obj1=[], Obj2=[]).

apply_check(np_sbar,Dt,_) :-
    Dt:vc <=> Obj1,
    Dt:obj2 <=> Obj2,
    \+ (Obj1=[], Obj2=[]).

apply_check(infl_adj(Adj),Dt,Path) :-
    (   ground(Path)
    ->  \+ illegal_infl_adj(Adj,Dt,Path)
    ;   true
    ).

apply_check(mod_pp(Prep),Dt,_Path) :-
    Dt:mod <=> List,
    member(El,List),
    has_prep(El,Prep).

apply_der(Path) :-
    \+ simple_der(Path),
    \+ conj_der(Path).

apply_des(Path) :-
    \+ simple_des(Path),
    \+ conj_des(Path).

simple_des([Rel|_]) :-
    lists:member(Rel,[su,obj1,obj2,body,predc,dp,nucl,sat,tag]).    
simple_des([det,Rel/_|_]) :-
    lists:member(Rel,[su,obj1,obj2,body,dp,nucl,sat,tag]).
simple_des([det,mod/np,_/Cat|_]) :-
    \+ Cat = np.

conj_des([det,cnj/_,Rel/Cat|Rest]) :-
    simple_des([det,Rel/Cat|Rest]).


simple_der([Rel|_]) :-
    lists:member(Rel,[su,obj1,obj2,body,predc,dp,nucl,sat,tag]).    
simple_der([det,Rel/_|_]) :-
    lists:member(Rel,[su,obj1,obj2,body,predc,dp,nucl,sat,tag]).
simple_der([det,mod/np,_/Cat|_]) :-
    \+ Cat = np.

conj_der([det,cnj/_,Rel/Cat|Rest]) :-
    simple_der([det,Rel/Cat|Rest]).

has_prep(El,Prep) :-
    root(El,Prep0),
    prep(Prep,Prep0).
has_prep(El,Prep) :-
    El:cnj <=> [H|_],
    has_prep(H,Prep).

illegal_infl_adj(e,_Dt,Path) :-
    sentential_mod(Path).
illegal_infl_adj(ere,_Dt,Path) :-
    sentential_mod(Path).
illegal_infl_adj(ge_e,_Dt,Path) :-
    sentential_mod(Path).

sentential_mod([Mod,_/S|_]) :-
    mod(Mod),
    sentential(S).
sentential_mod([hd,Mod/ap,_/S|_]) :-
    mod(Mod),
    sentential(S).

mod(mod).
mod(predm).

sentential(ap).
sentential(inf).
sentential(smain).
sentential(ssub).
sentential(sv1).
sentential(ppart).
sentential(ppres).

root(Cmp,Val) :-
    alpino_data:dt(Cmp,Hwrd,_,_,_),
    alpino_data:label(Hwrd,Val,_,_,_,_).

su(Dt,Su) :-
    Dt:su <=> Su.
su(Dt,Su) :-
    Dt:su:cnj <=> SuList,
    member(Su,SuList).

predc(Dt,Predc) :-
    Dt:predc <=> Predc.
predc(Dt,Predc) :-
    Dt:predc:cnj <=> PredcList,
    member(Predc,PredcList).

dtpc(Dt,Pc) :-
    Dt:pc <=> Pc.
dtpc(Dt,Pc) :-
    Dt:pc:cnj <=> PcList,
    member(Pc,PcList).

me(Dt,Me) :-
    Dt:me <=> Me.
me(Dt,Me) :-
    Dt:me:cnj <=> MeList,
    member(Me,MeList).

se(Dt,Se) :-
    Dt:se <=> Se.
se(Dt,Se) :-
    Dt:se:cnj <=> SeList,
    member(Se,SeList).

obj1(Dt,Obj1) :-
    Dt:obj1 <=> Obj1.
obj1(Dt,Obj1) :-
    Dt:obj1:cnj <=> Obj1List,
    member(Obj1,Obj1List).

obj2(Dt,Obj1) :-
    Dt:obj2 <=> Obj1.
obj2(Dt,Obj1) :-
    Dt:obj2:cnj <=> Obj1List,
    member(Obj1,Obj1List).

pobj1(Dt,Obj1) :-
    Dt:pobj1 <=> Obj1.
pobj1(Dt,Obj1) :-
    Dt:pobj1:cnj <=> Obj1List,
    member(Obj1,Obj1List).

rel(Rel,[Rel/_|_]).
rel(Rel,[Rel|_]).
rel(Rel,[hd|Path]) :-
    rel(Rel,Path).
rel(Rel,[cnj/_|Path]) :-
    rel(Rel,Path).

/*
cat(Cat,[hd|Path]) :-
    !,
    cat_continuation(Cat,Path).
cat(_,_).

cat_continuation(Cat1,[_/Cat|_]) :-
    (  Cat=Cat1
    ;  Cat=conj  % tja
    ).
*/

finite_infl(sg).
finite_infl(sg1).
finite_infl(sg3).
finite_infl(sg_heeft).
finite_infl(sg_hebt).
finite_infl(pl).
finite_infl(both(_)).
finite_infl(past(_)).
finite_infl(modal_u).
finite_infl(modal_not_u).
finite_infl(modal_inv).
finite_infl(subjunctive).

dep_cat(Rel,Cat,Dt) :-
    Dt:Rel <=> RelDT,
    RelDT => dt,
    (  RelDT:cat ==> conj
    ;  RelDT:cat ==> Cat
    ;  RelDT:cat ==> du
    ).

not_dep_cat(Rel,Cat,Dt) :-
    Dt:Rel <=> RelDT,
    RelDT => dt,
    RelDT:cat ==> Cat0,
    nonvar(Cat0),
    Cat0==Cat,
    !,
    fail.
not_dep_cat(_,_,_).

%% if there is a Rel, then it must have Cat
dep_if_cat(Rel,Cat,Dt) :-
    Dt:Rel <=> RelDT,
    (   RelDT => []
    ;   RelDT => dt,
        (   RelDT:cat ==> conj
        ;   RelDT:cat ==> Cat
        )
    ).

ipp(Dt) :-
    vc(Dt,VC),
    VC:cat ==> inf,
    ( vc(VC,SubVC)
    ; svp(VC,SubVC)  % we hebben laten zien ..
    ),
    ( SubVC:cat ==> inf
    ; SubVC:cat ==> ti
    ; SubVC:cat ==> conj
    ).

svp(Dt,VC) :-
    Dt:svp <=> LIST,
    lists:member(VC,LIST),
    VC => dt.

vc(Dt,VC) :-
    Dt:vc <=> VC,
    VC => dt.
vc(Dt,VC) :-
    Dt:vc <=> CONJ,
    CONJ:conj <=> List,
    member(VC,List).

vc_simple(Dt) :-
    vc(Dt,VC),
    VC:vc => [].

pc(Prep,Dt) :-
    Dt:pc <=> PC,
    pc_prep(Prep,PC).

pc(Prep,Dt) :-
    Dt:pc <=> PC,
    PC:conj <=> List,
    member(El,List),
    pc_prep(Prep,El).

pc_prep(Prep,DT) :-
    root(DT,Prep0),
    prep(Prep,Prep0).

prep([H|T],Root) :-
    nonvar(H), nonvar(T),
    concat_all([H|T],Root,' ').
prep(uit_op,_).
prep(met,mee).
prep(tot,toe).
prep(van,af).
prep(om,omheen).
prep(Prep,Prep).
prep(Prep,ErPrep) :-
    atom(Prep),
    member(Er,[er,daar,hier,waar]),
    ( atom_concat(Er,Prep,ErPrep)
    ; atom_concat(Er,toe,ErPrep)
    ; atom_concat(Er,mee,ErPrep)
    ; atom_concat(Er,af,ErPrep)
    ).

unknown_root_heuristics([Fr|Frs],[Fr|Frs],_Root,_Sense,_Pos,_Attr,_).
unknown_root_heuristics([],Frames,Root,Sense,Pos,Attr,Cat) :-
    findall(Frame-Surfs,setof(Surf,unknown_root_heuristic(Pos,Root,Sense,Attr,Frame,Cat,Surf),Surfs),Frames).

:- initialize_flag(prefer_best_words,on).
prefer_best_words_in_frames(Root,In,Out) :-
    hdrug_flag(prefer_best_words,OnOff),
    prefer_best_words_in_frames(OnOff,Root,In,Out).

prefer_best_words_in_frames(off,_,X,X).
prefer_best_words_in_frames(on,Root,In,Out) :-
    prefer_best_words_in_frames_(Root,In,Out).

prefer_best_words_in_frames_(_,[],[]).
prefer_best_words_in_frames_(Root,[Frame-Surfs0|Frames0],[Frame-Surfs|Frames]):-
    prefer_best_words(Root,Surfs0,Surfs),
    prefer_best_words_in_frames_(Root,Frames0,Frames).

%%% prefer_best
%%% ensure that order is not changed in case of a tie
%%% since we prefer e.g. zenuwactiviteit over zenuwaktiviteit (activiteit precedes aktiviteit in dict)
prefer_best_words(Root,Surfs0,Surfs) :-
    (   Surfs0 = [_]
    ->  Surfs0 = [Word],Surfs = [Word]
    ;   score_words(Root,Surfs0,Surfs1),
	filter_best(Surfs1,Surfs)
    ).

filter_best([],[]).
filter_best([Score-Word|Words],Result) :-
    filter_best(Words,Score,[Word|Rest],Rest,Result).

filter_best([],_,Result,[],Result).
filter_best([Score-Word|Words],Score0,R0,R,Result):-
    (   Score > Score0
    ->  filter_best(Words,Score,[Word|Rest],Rest,Result)
    ;   Score =:= Score0
    ->  R=[Word|R1],
	filter_best(Words,Score,R0,R1,Result)
    ;   filter_best(Words,Score0,R0,R,Result)
    ).

score_words(_,[],[]).
score_words(Root,[W|Ws],[F-W|Ws1]) :-
    (   lemma_word_freq(Root,W,F0)
    ->  true
    ;   F0 = 0
    ),
    subtract_if_para(Root,W,F0,F),
    score_words(Root,Ws,Ws1).

subtract_if_para(Root,W,F0,F) :-
    alpino_paraphrase:add_lex(Root,W,_),!,
    F is F0 + 1.
subtract_if_para(_,_,F,F).

unknown_root_heuristic(Pos,{RootList},Root,Atts,Pos,Cat,Surfs) :-
    !,
    lists:member(Root0,RootList),
    unknown_root_heuristic(Pos,Root0,Root,Atts,Pos,Cat,Surfs).

unknown_root_heuristic(noun,Root,_,Attr,noun(Gen,both,Num),_,Surf) :-
    \+ sub_atom(Root,_,1,_,'_'),
    (   member(gen=Gen,Attr)
    ->  true
    ;   Gen=both
    ),
    (   member(num=Num,Attr)
    ;   member(rnum=Num,Attr)
    ->  true
    ;   Num=both
    ),
    add_morphology(noun(Gen,both,Num),Root,Surf0),
    realize_surf(Surf0,Surf).

unknown_root_heuristic(prefix,Root,Root,_,within_word_conjunct,_,RootDash) :-
    atom_concat(Root,'-',RootDash).

unknown_root_heuristic(verb,Root,Root,Attrs,verb(HZ,subjunctive,Sc),_,Surf) :-
    lists:member(tense=subjunctive,Attrs),
    dict_entry(Root,verb(HZ,inf,Sc),Inf),
    atom_concat(Inf0,en,Inf),
    atom_concat(Inf0,e,Surf),
    alpino_postags:postag_of_frame(verb(HZ,subjunctive,Sc),verb,CheckAttr),
    check_attributes(CheckAttr,Attrs,verb,verb(HZ,subjunctive,Sc),Root).

unknown_root_heuristic(verb,Root,_,Attr,Frame,Cat,Surf) :-
    atom_concat(Verb,Rest,Root),
    atom_concat('_',Part,Rest),
    dict_entry(Part,particle(Part),_),
    pos2frames_aux_robust(Verb,_,verb,Cat,Attr,Frame0,Surfs0),
    add_bare_prefixes(Frame0,Frame,Surfs0,Part,Surfs),
    lists:member(Surf,Surfs).

unknown_root_heuristic(Pos,Root,_,Attr,Frame,Cat,Surf) :-
    atom_concat(Prefix,Rest,Root),
    atom_concat(Pref,'_',Prefix),
    \+ atom_concat('DIM_',_,Rest),
    pos2frames_aux(Rest,__Sense,Pos,Cat,Attr,Frame,Surfs0),
    add_prefixes(Surfs0,Pref,Surfs,'_'),
    lists:member(Surf,Surfs).

unknown_root_heuristic(Pos,Root,_,Attr,Frame,Cat,Surf) :-
    atom_concat(Prefix,Rest,Root),
    atom_concat(Pref,' ',Prefix),
    pos2frames_aux(Rest,__Sense,Pos,Cat,Attr,Frame,Surfs0),
    add_prefixes(Surfs0,Pref,Surfs,' '),
    lists:member(Surf,Surfs).

unknown_root_heuristic(num,Root,_,[],number(hoofd(pl_num)),_,Root) :-
    \+ (   atom_concat(_Verb,Rest,Root),
	   atom_concat('_',_Part,Rest)
       ).

unknown_root_heuristic(num,Lemma,_Sense,Attr,Frame,_,LemmaDe) :-
    lists:member(numtype=rang,Attr),
    atom(Lemma),
    (  atom_concat(Lemma,de,LemmaDe)
    ;  atom_concat(Lemma,ste,LemmaDe)
    ),
    alpino_lex:lexicon(Frame,Lemma,[LemmaDe],[],_),
    alpino_postags:postag_of_frame(Frame,num,CheckAttr),
    check_attributes(CheckAttr,Attr,num,Frame,Lemma).

unknown_root_heuristic(Pos,Pitloos,_,Attr,Frame,_,Surf) :-
    heur_prefix(Pos,Loos,NoE,Frame),
    atom_concat(Pit,Loos,Pitloos),
    \+ sub_atom(Pit,_,_,_,'_'),
    atom_concat(Pit,NoE,Surf),
    alpino_postags:postag_of_frame(Frame,adj,CheckAttr),
    check_attributes(CheckAttr,Attr,adj,Frame,Pitloos).

%% op een INF (hij zet het op een lopen)
unknown_root_heuristic(fixed,Root,Root,[],fixed_part(op_een_v),_,FinalSurf) :-
    atom_concat('op een ',Loop,Root),
    pos2frames(Loop,Loop,verb,_,[],_,Frames),
    findall(Surfs0,lists:member(verb(_,inf,_)-Surfs0,Frames), SurfsList0),
    sort(SurfsList0,SurfsList1),
    member(Surfs,SurfsList1),
    member(Surf,Surfs),
    concat_all([op,een,Surf], FinalSurf,' ').

/*
unknown_root_heuristic(verb,Iseren,_,_,Frame,_,Surf) :-
    atom_concat(Pref,eren,Iseren),
    atom_concat(Pref,eerd,Surf0),
    atom_concat(ge,Surf0,Surf),
    Frame = verb(hebben,psp,SC),
    last_resort_sc(SC).

unknown_root_heuristic(verb,Iseren,_,_,Frame,_,Surf) :-
    atom_concat(Pref,eren,Iseren),
    atom_concat(Pref,eer,Surf),
    Frame = verb(hebben,sg1,SC),
    last_resort_sc(SC).

unknown_root_heuristic(verb,Iseren,_,_,Frame,_,Surf) :-
    atom_concat(Pref,eren,Iseren),
    atom_concat(Pref,eert,Surf),
    Frame = verb(hebben,sg3,SC),
    last_resort_sc(SC).

unknown_root_heuristic(verb,Iseren,_,_,Frame,_,Surf) :-
    atom_concat(Pref,eren,Iseren),
    atom_concat(Pref,eerde,Surf),
    Frame = verb(hebben,past(sg),SC),
    last_resort_sc(SC).

unknown_root_heuristic(verb,Iseren,_,_,Frame,_,Surf) :-
    atom_concat(Pref,eren,Iseren),
    atom_concat(Pref,eerden,Surf),
    Frame = verb(hebben,past(pl),SC),
    last_resort_sc(SC).

unknown_root_heuristic(verb,Iseren,_,_,Frame,_,Iseren) :-
    atom_concat(_,eren,Iseren),
    Frame = verb(hebben,inf,SC),
    last_resort_sc(SC).

unknown_root_heuristic(verb,Iseren,_,_,Frame,_,Iseren) :-
    atom_concat(_,eren,Iseren),
    Frame = verb(hebben,pl,SC),
    last_resort_sc(SC).
*/
unknown_root_heuristic(Pos,Root,_,Attr,Frame,_,Surf) :-
    dict_entry(Root,Frame,Surf),
    exc_postag_of_frame(Frame,Pos,CheckAttr),
    check_attributes(CheckAttr,Attr,Pos,Frame,Root).

%% input: Surfs0, Part,Surfs, verb(HZ,VF,transitive)
%% two types of output
%% particle added to Surfs -> Frame becomes verb(HZ,VF,ninv(transitive,part_transitive(Part)))
%% particle not added to   -> Frame becomes verb(HZ,VF,part_transitive(Part))

add_bare_prefixes(verb(HZ,VF,SC),
		  verb(HZ,VF,SC2),Surfs0,Part,Surfs) :-
    !,
    SC =.. [Fun|Args],
    \+ Fun = ninv,
    atom_concat(part_,Fun,Fun2),
    T1=.. [Fun2,Part|Args],
    \+ impossible_frame(T1),
    (   SC2 = ninv(SC,T1),
	add_bare_prefixes(Surfs0,Part,Surfs)
    ;   SC2 = T1,
	Surfs0 = Surfs
    ).
add_bare_prefixes(V,V,Surfs0,Part,Surfs) :-
    add_bare_prefixes(Surfs0,Part,Surfs).

add_bare_prefixes([],_,[]).
add_bare_prefixes([S0|S],Part,[PartS0|PartS]) :-
    atom_concat(Part,S0,PartS0),
    add_bare_prefixes(S,Part,PartS).

add_prefixes([],_,[],_).
add_prefixes([H|T],Pref,Results,Sep) :-
    findall(Form,add_prefix(H,Pref,Form,Sep),Results,Results1),
    add_prefixes(T,Pref,Results1,Sep).

add_prefix(Organisatie,SpelDim,Surf,Sep) :-
    atom_concat(_,'_DIM',SpelDim),
    !,
    alpino_lex:inv_lex(SpelDim,Spelletje),
    add_prefix(Organisatie,Spelletje,Surf,Sep).

add_prefix(Organisatie,DierRecht,Surf,Sep) :-
    atom_concat(Dier0,Recht,DierRecht),
    atom_concat(Dier,'_',Dier0),
    add_prefix(Organisatie,Recht,RechtenOrganisatie,Sep),
    add_prefix(RechtenOrganisatie,Dier,Surf,'_').

add_prefix(H,Pref,Surf,Sep) :-
    atom_concat(Pref,Sep,Pref2),
    atom_concat(Pref2,H,Pref3),
    realize_surf(Pref3,Surf).

add_prefix(Affaire,Toeslag,ToeslagenAffaire,'_') :-
    inflected_compound_part(Toeslag,Toeslagen),
    atom_concat(Toeslagen,Affaire,ToeslagenAffaire).

last_resort_heuristics([Fr0|Fr],[Fr0|Fr],_,_,_,_,_).
last_resort_heuristics([],Frames,Root,Sense,Pos,Cat,Attr) :-
    (   Root == 'een of'
    ->  Frames = []
    ;   findall(Frame-Surf,last_resort_heuristic(Pos,Cat,Root,Sense,Attr,Frame,Surf),Frames0),
	sort(Frames0,Frames1),
	filter_specific(Frames1,Frames)
    ).

filter_specific(Frames0,Frames) :-
    lists:select(FrameA-Ws,Frames0,Frames1),
    specific_frames(FrameA,FrameB),
    lists:select(FrameB-Ws,Frames1,Frames2),
    !,
    filter_specific([FrameB-Ws|Frames2],Frames).
filter_specific(F,F).

specific_frames(proper_name(_,_),proper_name(_)).
specific_frames(proper_name(both),proper_name(sg)).
specific_frames(proper_name(both),proper_name(pl)).
specific_frames(proper_name(both,Type),proper_name(sg,Type)).
specific_frames(proper_name(both,Type),proper_name(pl,Type)).

%% todo: adapt surface form for some inflectional variants

last_resort_heuristic(Pos,Cat,{RootList},Root,Atts,Frame,Surfs) :-
    !,
    lists:member(Root0,RootList),
    last_resort_heuristic(Pos,Cat,Root0,Root,Atts,Frame,Surfs).

last_resort_heuristic(Pos,Cat,Root,_,Attr,Frame,Surfs):-
    last_resort_tag(Frame),
    alpino_postags:postag_of_frame(Frame,Pos,CheckAttr),
    check_attributes(CheckAttr,Attr,Pos,Frame,Root),
    add_morphology(Frame,Root,Surf1),
    findall(Surf,realize_surf(Surf1,Surf),Surfs),
    \+ wrong_cat(Frame,Cat).

added_paraphrase_heuristics(Frames0,Frames,Root,Sense,Pos,Attr):-
    findall(Frame-Surf,added_paraphrase_heuristic(Pos,Root,Sense,Attr,Frame,Surf),Frames1),
    add_frames(Frames1,Frames0,Frames).

add_frames([],F,F).
add_frames([Frame|Frames],F0,F) :-
    add_frame(Frame,F0,F1),
    add_frames(Frames,F1,F).

add_frame(Tag-Surfs,F0,F) :-
    add_frame(F0,Tag,Surfs,F).

add_frame([],Tag,Surfs,[Tag-Surfs]).
add_frame([T-S|F0],Tag,Surfs0,F) :-
    (  T == Tag
    ->
	lists:append(S,Surfs0,Surfs1),
	sort(Surfs1,Surfs),
	F = [Tag-Surfs|F0]
    ;   F = [T-S|F2],
	add_frame(F0,Tag,Surfs0,F2)
    ).

%% if we do paraphrasing, and we cannot find a lexical entry,
%% use the same entry as in the input parse
added_paraphrase_heuristic(Pos,Root,_Sense,Attr,Frame,[Surf]):-
    alpino_paraphrase:add_lex(Root,Surf,Frame),
    alpino_postags:postag_of_frame(Frame,Pos,CheckAttr),
    check_attributes(CheckAttr,Attr,Pos,Frame,Root).
    

add_morphology(noun(_,_,pl),Root,Surf) :-
    atom_concat(Pref,Suf,Root),
    en(Suf,Suf2),!,
    atom_concat(Pref,Suf2,Surf).
add_morphology(noun(_,_,pl),Root,Surf) :-
    atom_concat(_,s,Root),
    !,
    Root = Surf.
add_morphology(noun(_,_,pl),Root,Surf) :-
    atom_concat(_,en,Root),
    !,
    Root = Surf.
add_morphology(noun(_,_,pl),Root,Surf) :-
    atom_concat(_,Suf,Root),
    en(Suf),!,
    add_en(Root,Surf).
add_morphology(noun(_,_,pl),Root,Surf) :-
    !,
    atom_concat(Root,s,Surf).

add_morphology(verb(_,psp,_),BelOp,Opgebeld) :-
    atom_concat(Bel,UOp,BelOp),
    atom_concat('_',Op,UOp),
    !,
    add_dt(Bel,Beld),
    atom_concat(Op,ge,Opge),
    atom_concat(Opge,Beld,Opgebeld).

add_morphology(verb(_,psp,_),Bel,Gebeld) :-
    !,
    add_dt(Bel,Beld),
    add_ge(Beld,Gebeld).
%    atom_concat(ge,Beld,Gebeld).

add_morphology(verb(_,past(sg),_),BelOp,OpBelde) :-
    atom_concat(Bel,UOp,BelOp),
    atom_concat('_',Op,UOp),
    !,
    add_dt(Bel,Beld),
    atom_concat(Beld,e,Belde),
    atom_concat(Op,Belde,OpBelde).
    
add_morphology(verb(_,past(sg),_),Bel,Belde) :-
    !,
    add_dt(Bel,Beld),
    atom_concat(Beld,e,Belde).
    
add_morphology(verb(_,past(pl),_),BelOp,OpBelde) :-
    atom_concat(Bel,UOp,BelOp),
    atom_concat('_',Op,UOp),
    !,
    add_dt(Bel,Beld),
    atom_concat(Beld,en,Belde),
    atom_concat(Op,Belde,OpBelde).
    
add_morphology(verb(_,past(pl),_),Bel,Belde) :-
    !,
    add_dt(Bel,Beld),
    atom_concat(Beld,en,Belde).
    
add_morphology(verb(_,inf,_),BelOp,OpBellen) :-
    atom_concat(Bel,UOp,BelOp),
    atom_concat('_',Op,UOp),
    !,
    add_en(Bel,Bellen),
    atom_concat(Op,Bellen,OpBellen).

%% ze zijn te veslaan
add_morphology(verb(_,inf,_),Bel,Bellen) :-
    atom_concat(_,a,Bel),
    !,
    atom_concat(Bel,an,Bellen).
add_morphology(verb(_,inf,_),Bel,Bellen) :-
    atom_concat(Pref,ieer,Bel),
    !,
    atom_concat(Pref,iëren,Bellen).
add_morphology(verb(_,inf,_),Bel,Bellen) :-
    !,
    add_en(Bel,Bellen).
add_morphology(v_noun(_),Bel,Bellen) :-
    !,
    add_morphology(verb(_,inf,_),Bel,Bellen).
add_morphology(verb(_,pl,_),Bel,Bellen) :-
    !,
    add_morphology(verb(_,inf,_),Bel,Bellen).

add_morphology(verb(_,sg3,_),Bel,Belt) :-
    !,
    add_t(Bel,Belt).

add_morphology(name_determiner(pron,_),Root,Roots) :-
    !,
    add_gen(Root,Roots).
add_morphology(name_determiner(pron),Root,Roots) :-
    !,
    add_gen(Root,Roots).

add_morphology(adjective(ende(_)),Root,Surf) :-
    atom_concat(Pref,end,Root),
    !,
    atom_concat(Pref,ende,Surf).
      

add_morphology(_,R,R).

add_gen(Root,Roots) :-
    ends_with_s(Root),
    !,
    atom_concat(Root,'\'',Roots).
add_gen(Root,Roots) :-
    ends_with_c(Root),
    atom_concat(Root,s,Roots).
add_gen(Root,Roots) :-
    ends_with_vv(Root),
    atom_concat(Root,s,Roots).
add_gen(Root,Roots) :-
    atom_concat(Root,'\'s',Roots).

ends_with_c(Root) :-
    atom_codes(Root,Codes),
    append(_,[C],Codes),
    cons(C).

ends_with_vv(Root) :-
    atom_codes(Root,Codes),
    append(_,[V1,V2],Codes),
    vowel(V1),
    vowel(V2).

add_t(Bet,Surf) :-
    atom_concat(_,t,Bet),
    !,
    Bet=Surf.
add_t(Bel,Belt) :-
    atom_concat(Bel,t,Belt).

add_dt(Bel,Bel1) :-
    atom_concat(_,d,Bel),
    !,
    Bel = Bel1.
add_dt(Bel,Bel1) :-
    atom_concat(_,t,Bel),
    !,
    Bel = Bel1.
add_dt(Bel,Belt) :-
    kofschip(Suffix),
    atom_concat(_,Suffix,Bel),
    !,
    atom_concat(Bel,t,Belt).
add_dt(Bel,Beld) :-
    atom_concat(Bel,d,Beld).

add_en(Beer,Beren):-
    atom_concat(Pref,Suffix,Beer),
    open(Suffix,SuffixEn),
    !,
    atom_concat(Pref,SuffixEn,Beren).
add_en(Bel,Bellen) :-
    atom_concat(Pref,Suffix,Bel),
    dubbel(Suffix,Pref,SuffixEn),
    !,
    atom_concat(Pref,SuffixEn,Bellen).
add_en(Toep,Toepen) :-
    atom_concat(Toep,en,Toepen).

ver(ver).

add_ge(Beld,Gebeld) :-
    ver(Ver),
    atom_concat(Ver,_,Beld),
    !,
    Beld=Gebeld.
add_ge(Beld,Gebeld) :-
    atom_concat(ge,Beld,Gebeld).

open(Aar,Aren) :-
    atom_codes(Aar,[V,V,C]),
    vowel(V),
    atom_codes(Aren,[V,C,101,110]).

%% 1. als maar 2 letters
dubbel(Up,[],Uppen) :-
    atom_codes(Up,[V,C]),
    vowel(V),
    cons(C),
    atom_codes(Uppen,[V,C,C,101,110]).
%% 2. als maar 3 letters
dubbel(Bel,[],Bellen) :-
    atom_codes(Bel,[C1,V,C2]),
    vowel(V),
    cons(C1),
    cons(C2),
    atom_codes(Bellen,[C1,V,C2,C2,101,110]).
%% 3. als meer dan 3 letters (klemtoon weten we niet..)
dubbel(Bel,_,Bellen) :-
    atom_codes(Bel,[V1,C1,V2,C2]),
    vowel(V1),
    vowel(V2),
    cons(C1),
    cons(C2),
    atom_codes(Bellen,[V1,C1,V2,C2,C2,101,110]).

kofschip(k).
kofschip(f).
kofschip(s).
kofschip(h).
kofschip(c).
kofschip(p).

ends_with_s(Stem) :-
    alpino_unknowns:genitive_marker(Suf,_),
    atom_concat(_,Suf,Stem).

last_resort_tag(noun(de,count,sg)).
last_resort_tag(noun(de,count,pl)).
last_resort_tag(noun(het,count,pl)).
last_resort_tag(noun(het,count,sg)).
last_resort_tag(adjective(ge_e)).
last_resort_tag(adjective(ge_no_e(adv))).
last_resort_tag(adjective(e)).
last_resort_tag(adjective(no_e(adv))).
last_resort_tag(adjective(ende(padv))).
last_resort_tag(adjective(no_e(adv))).
last_resort_tag(adjective(end(both))).
last_resort_tag(adjective(ere)).
last_resort_tag(adjective(er(adv))).
last_resort_tag(adjective(ste)).
last_resort_tag(adverb).
last_resort_tag(conj(en)).
last_resort_tag(determiner(het,nwh,nmod,pro,nparg)).
last_resort_tag(determiner(de,nwh,nmod,pro,nparg)).
last_resort_tag(modal_adverb).
last_resort_tag(tmp_adverb).
last_resort_tag(verb('hebben/zijn',Fin,Sc)) :-
    last_resort_fin(Fin),
    last_resort_sc(Sc).
last_resort_tag(v_noun(Sc)) :-
    last_resort_sc(Sc).
last_resort_tag(nominalized_adjective).
last_resort_tag(name_determiner(pron)).
last_resort_tag(name_determiner(pron,'PER')).
last_resort_tag(name_determiner(pron,'LOC')).
last_resort_tag(name_determiner(pron,'ORG')).

last_resort_tag(proper_name(both)).
last_resort_tag(proper_name(both,'PER')).
last_resort_tag(proper_name(both,'LOC')).
last_resort_tag(proper_name(both,'ORG')).
last_resort_tag(proper_name(sg)).
last_resort_tag(proper_name(sg,'PER')).
last_resort_tag(proper_name(sg,'LOC')).
last_resort_tag(proper_name(sg,'ORG')).
last_resort_tag(proper_name(pl)).
last_resort_tag(proper_name(pl,'PER')).
last_resort_tag(proper_name(pl,'LOC')).
last_resort_tag(proper_name(pl,'ORG')).

last_resort_fin(psp).
last_resort_fin(inf).
last_resort_fin(sg3).
last_resort_fin(sg1).
last_resort_fin(pl).
last_resort_fin(past(sg)).
last_resort_fin(past(pl)).

last_resort_sc(intransitive).
last_resort_sc(transitive).
last_resort_sc(sbar).
last_resort_sc(np_ld_pp).
last_resort_sc(refl).
last_resort_sc(refl_np).
last_resort_sc(refl_np_ld_pp).

/*
last_resort_heuristic(Pos,Root,_,Attr,Frame,Surfs):-
    set_flag(current_input_sentence,[Root]),
    alpino_lexical_analysis:lexical_analysis_cleanup,
    alpino_lexical_analysis:add_word_forms([Root]),
    alpino_lexical_analysis:guess_names([Root]),
    alpino_lexical_analysis:guess_unknowns([Root],0),
    alpino_lexical_analysis:tag(_,_,_,_,Root,__Root,_,Frame),
    alpino_postags:postag_of_frame(Frame,Pos,CheckAttr),
    check_attributes(CheckAttr,Attr,Pos,Frame,Root),
    findall(Surf,realize_surf(Root,Surf),Surfs).

last_resort_heuristic(noun,Root,_,Attr,noun(Gen,both,Num),Surfs) :-
    (   member(gen=Gen,Attr)
    ->  true
    ;   Gen=both
    ),
    (   member(num=Num,Attr)
    ->  true
    ;   Num=both
    ),
    findall(Surf,realize_surf(Root,Surf),Surfs).

*/

%% TODO: do some inflectional morphology here!

%% get rid of meta '_'
realize_surf(Root,Surf) :-
    atom_codes(Root,RootCodes),
    realize_surf_codes(RootCodes,SurfCodes),
    atom_codes(Surf,SurfCodes).

realize_surf_codes([],[]).
realize_surf_codes([H|T],Result) :-
    realize_surf_codes(T,H,Result).

realize_surf_codes([],H,[H]).
realize_surf_codes([NH|T],H,Result) :-
    realize_surf_codes(T,H,NH,Result).

realize_surf_codes([],A,B,[A,B]).
realize_surf_codes([H|T],A,B,Result) :-
    (   B =:= 95                % "_"
    ->  (   H=68,
            T = [73,77|T2]      % "DIM"
        ->  dim(A,Suffix),
            append(Suffix,NResult,Result),
            realize_surf_codes(T2,106,101,NResult)
        ;   Result = [A|NResult],
            (   islower(A),
                islower(H),
                realize_surf_codes(T,H,NResult)
            ;   allows_s(A), 
                islower(H),
                realize_surf_codes(T,115,H,NResult) % "s"
            ;   realize_surf_codes(T,45,H,NResult) % "-"
            )
        )
    ;   Result = [A|NResult],
        realize_surf_codes(T,B,H,NResult)
    ).

dim(X,Suffix) :-
    (   islower(X)
    ->  dim1(X,Suffix)
    ;   Suffix=[X]
    ).

allows_s(100). % d
allows_s(101). % e  spruitjes
allows_s(102). % f
allows_s(103). % f
allows_s(107). % k
allows_s(108). % l
allows_s(109). % m
allows_s(110). % n
allows_s(112). % p
allows_s(114). % r
allows_s(116). % t

dim1(X,Suffix) :-
    (   (  X =:= 97   % a
        ;  X =:= 101  % e
        ;  X =:= 105  % i
        ;  X =:= 106  % j
        ;  X =:= 108  % l
        ;  X =:= 110  % n
        ;  X =:= 111  % o
        ;  X =:= 114  % r
        ;  X =:= 117  % u
        ;  X =:= 119  % w
        ;  X =:= 121  % y
        )
    ->  Suffix = [X,116]
    ;   X =:= 109     % m
    ->  Suffix = [X,112]
    ;   Suffix = [X]
    ).
    
%% todo: add couple of noun adjective and verb frames per default?

check_missing_roots([]).
check_missing_roots([H-C|T]) :-
    (   alpino_cg:lex(_,_,C1,_,_,_),
        C /\ C1 =\= 0
    ->  true
    ;   hdrug_flag(generate_failsafe,off),
	format(user_error,"warning: missing lex for root ~w~n",[H]),
	fail
    ),
    check_missing_roots(T).

check_missing_surfs(Stems) :-
    findall(Surf-Stem,( alpino_lexical_analysis:tag(_,_,_,_,Stem0,Surf0,His,Tag),
			simplify_lemma(Stem0,Stem),
			lists:member(Stem,Stems),
			\+ Tag = punct(_),
			\+ His = normal(decap(normal)),
			\+ His = skip(_,_,_,_),
			alpino_unknowns:decap(Surf0,Surf)
		      ),Surfs0),
    sort(Surfs0,Surfs),
    check_missing_surfsG(Surfs).

check_missing_surfsG([]).
check_missing_surfsG([H-Stem|T]) :-
    check_missing_surf(H,Stem),
    check_missing_surfsG(T).

check_missing_surf(W,_) :-
    sub_atom(W,_,_,_,' '),
    !.

check_missing_surf(W,Stem) :-
    (   alpino_cg:lex(_,_,_,_,_,Surfs),
	(   member(W,Surfs)
	;   member(W1,Surfs),
	    alpino_unknowns:decap(W1,W)
	;   member(W1,Surfs),
	    alpino_util:split_atom(W1," ",Atoms),
	    member(W1,Atoms),
	    alpino_unknowns:decap(W1,W)
	)
    ->  true
    ;   findall(Surf,( alpino_cg:lex(_,Stem,_,_,_,SurfList),
		       member(Surf,SurfList)
		     ),Surfs0),
	sort(Surfs0,Surfs),
	format(user_error,"~w is not a candidate for generation (use ~w instead)~n",[W,Surfs])
    ).

not_sent_cat([_,_/Cat|_]) :-
    nonvar(Cat),
    member(Cat,[smain,ssub,sv1,ppart,ppres,inf]), !,
    fail.
not_sent_cat(_).

en(heid,heden).
en(jaar,jaren).
en(aan,anen).
en(ees,ezen).
en(tal,tallen).

en(ing).
en(iteit).

%% singular verbs with plural subject!
%% "... maar welke liedjes laat me koud"
short_sbar_subj(sbar_subj).
short_sbar_subj(sbar_subj_np).
short_sbar_subj(sbar_subj_so_np).
short_sbar_subj(sbar_subj_np_np).
short_sbar_subj(pp_sbar_subj(_)).
short_sbar_subj(copula_sbar).
short_sbar_subj(so_copula_sbar).
short_sbar_subj(nonp_copula_sbar).
short_sbar_subj(so_nonp_copula_sbar).
short_sbar_subj(part_so_nonp_copula_sbar(_)).
short_sbar_subj(pp_copula_sbar).
short_sbar_subj(ap_pp_copula_sbar).
short_sbar_subj(part_ap_pp_copula_sbar(_)).
short_sbar_subj(fixed(List,_)) :-
    lists:member(sbar_subj,List).

wrong_cat(adjective(Infl),Cat) :-
    nonvar(Cat),
    wrong_adj_cat(Infl,Cat).
wrong_cat(adjective(Infl,_),Cat) :-
    nonvar(Cat),
    wrong_adj_cat(Infl,Cat).

wrong_adj_cat(end(_),ap).
wrong_adj_cat(ende(_),ap).

wrong_adj_cat(ge_e,ppres).
wrong_adj_cat(ge_no_e(_),ppres).
wrong_adj_cat(ge_both(_),ppres).

wrong_adj_cat(e,ppres).
wrong_adj_cat(no_e(_),ppres).
wrong_adj_cat(both(_),ppres).

%%  adapt_frame(SurfaceAtom,Frame0,Frame).
adapt_frame(maand,tmp_noun(de,count,bare_meas),tmp_noun(de,count,sg)).
adapt_frame(maand,tmp_noun(de,count,bare_meas,SC),tmp_noun(de,count,sg,SC)).

adapt_frame(punt,meas_mod_noun(both,count,bare_meas),meas_mod_noun(both,count,sg)).
adapt_frame(punt,meas_mod_noun(both,count,bare_meas,measure),meas_mod_noun(both,count,sg,measure)).

impossible_frame(part_fixed(_,_,_)).
impossible_frame(part_cleft(_)).
impossible_frame(part_passive(_)).
impossible_frame(part_uit(_)).
impossible_frame(part_sbar_subj_te_passive(_)).
impossible_frame(part_copula(_)).
impossible_frame(part_copula_sbar(_)).
impossible_frame(part_copula_vp(_)).
impossible_frame(part_so_copula(_)).
impossible_frame(part_so_copula_sbar(_)).
impossible_frame(part_so_copula_vp(_)).
impossible_frame(part_aux_simple(_,_)).
impossible_frame(part_aux_psp_zijn(_)).
impossible_frame(part_te_passive(_)).
impossible_frame(part_sbar_subj(_)).
impossible_frame(part_sbar_subj_het(_)).
impossible_frame(part_vp_subj(_)).
impossible_frame(part_vp_subj_het(_)).
impossible_frame(part_pp_vp_subj(_,_)).
impossible_frame(part_pp_sbar_subj(_,_)).
impossible_frame(part_copula_np(_)).
impossible_frame(part_alsof_sbar_subj(_)).
impossible_frame(part_aan_het(_)).
impossible_frame(part_er_pp_sbar(_,_)).
impossible_frame(part_mod_pp(_,_)).
impossible_frame(part_er_er(_)).

%impossible_frame(X) :-
%    format(user_error,"~w~n",[X]).

inflected_compound_part(miljard,miljarden). % miljardenlening
inflected_compound_part(half,halve).	    % halvefinalisten
inflected_compound_part(hoog,hoge).	    % hogedrukgebied
inflected_compound_part(kort,korte).	    % korteafstandraket
inflected_compound_part(vrij,vrije).	    % vrijemarkteconomie
inflected_compound_part(gelijk,gelijke).    % gelijkekansenbudget
inflected_compound_part(sociaal,sociale).   % socialezekerheidstelsel
inflected_compound_part(kind,kinder).
inflected_compound_part(Toeslag,Toeslagen) :-
    root_surface(Toeslag,Toeslagen,ToeslagenList),
    atom_concat(_,n,Toeslagen),
    alpino_lex:lexicon(noun(_,_,pl),Toeslag,ToeslagenList,[],_).

adj_prefix(aal,aal,ale,alen).
adj_prefix(aans,aans,aanse,aansen).
adj_prefix(air,air,aire,airen).
adj_prefix(baar,baar,bare,baren).
adj_prefix('_delig',delig,delige,deligen).
adj_prefix(eerd,eerd,eerde,eerden).
adj_prefix(ees,ees,ese,esen).
adj_prefix(eus,eus,euze,euzen).
adj_prefix(gewijs,gewijs,gewijze,gewijzen).
adj_prefix(half,half,halve,halven).
adj_prefix(ieel,ieel,iële,iëlen).
adj_prefix(iek,iek,ieke,ieken).
adj_prefix(iel,iel,iele,ielen).
adj_prefix(ig,ig,ige,igen).
adj_prefix(isch,isch,ische,ischen).
adj_prefix(loos,loos,loze,lozen).
adj_prefix(lijk,lijk,lijke,lijken).
adj_prefix(oir,oir,oire,oiren).
adj_prefix('_voudig',voudig,voudige,voudigen).

heur_prefix(adj,Stem,Loos,adjective(no_e(adv))):-
    adj_prefix(Stem,Loos,_,_).
heur_prefix(adj,Loos,Loze,adjective(e)):-
    adj_prefix(Loos,_,Loze,_).
heur_prefix(adj,Loos,Lozer,adjective(er(adv))):-
    adj_prefix(Loos,_,Loze,_),
    atom_concat(Loze,r,Lozer).
heur_prefix(adj,Loos,Lozere,adjective(ere)):-
    adj_prefix(Loos,_,Loze,_),
    atom_concat(Loze,re,Lozere).
heur_prefix(adj,Loos,Lozen,nominalized_adjective):-
    adj_prefix(Loos,_,_,Lozen).

heur_prefix(num,half,half,number(hoofd(both))).
heur_prefix(num,half,halve,number(hoofd(both))).

heur_prefix(num,ste,ste,number(rang)).

phrasal_entry(with_dt(CAT,
                      dt(np,[hd=l(Boek,TAG,1,2),
			     det=dt(detp,[hd=l(Tien,number(hoofd(pl_num)),3,4),
					  mod=l('een of',pre_num_adv(pl_indef),[0,2],[1,3])])])),
	      een_N_of_NUM,[een,Boek,of,Tien],[]) :-
    Boek \= stuk,
    alpino_lex:xl(Boek,TAG,_,[],[]),
    alpino_lex:sg_noun(TAG,CAT),
    alpino_lex:number_expression(pl_num,[Tien],[]).

phrasal_entry(with_dt(CAT,
      dt(np,[hd=l(Boek,TAG,1,2),
	     det=dt(conj,[cnj=dt(detp,
				 [hd=l(Tien,number(hoofd(pl_num)),3,4),
				  mod=ix(A,l('een of',pre_num_adv(pl_indef),[0,2],[1,3]))]),
			  cnj=dt(detp,
				 [hd=l(Twintig,number(hoofd(pl_num)),4,5),
				  mod=ix(A)])])])),
	      een_N_of_NUM,[een,Boek,of,Tien,Twintig],[]) :-
    Boek \= stuk,
    alpino_lex:xl(Boek,TAG,_,[],[]),
    alpino_lex:sg_noun(TAG,CAT),
    alpino_lex:number_expression(pl_num,[Tien],[]),
    alpino_lex:number_expression(pl_num,[Twintig],[]).

