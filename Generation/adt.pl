:- module(alpino_adt, [ create_adt/1,
			create_adt/2,
			result_to_adt/2
		      ]).

:- expects_dialect(sicstus).

:- use_module(library(lists)).

:- use_module(hdrug(hdrug_feature)).

:- use_module(bitcode).

%% dynamic preds used by others
%% dt_part/1    for lexical checks

:- dynamic dt_part/1.

:- thread_local dt_part/1.

:- public print_adt_lex/0,
	  show_adt/3,
          bitcode_lookup_frames_adt/4,
          adt_to_fs/4,
          dt_to_adt/2.

print_adt_lex :-
	create_adt(Dt),
	print_lex(Dt).

create_adt(Adt) :-
	hdrug:object(1,o(Cat,_,_)), result_to_adt(Cat,Adt).

create_adt(N,Adt) :-
	hdrug:object(N,o(Cat,_,_)), result_to_adt(Cat,Adt).

show_adt([],Type,Output) :-
    object_adt(1,Adt),
    hdrug_show:show(Type,Output,[value(Adt)]).
show_adt([Ns0|Ns],Type,Output) :-
    findall(value(Adt),(lists:member(N,[Ns0|Ns]),object_adt(N,Adt)),
	    Values),
    hdrug_show:show(Type,Output,Values).

object_adt(Obj,Adt) :-
    hdrug:object(Obj,o(Cat,_,_)),
    result_to_adt(Cat,Adt).

%% Convert a dependency tree to an abstract dependency tree.
result_to_adt(Cat,Adt) :-
    alpino_dt:result_to_dt(Cat, Dt),
    dt_to_adt(Dt,Adt).

dt_to_adt(Dt,Adt) :-
    remove_particles(Dt,NoPartDt),
    remove_idx(NoPartDt,NoIdxDt),
    remove_lemma(NoIdxDt,NoLemmaDt),
    frames2pos(NoLemmaDt,AdtPos),
    remove_punct(AdtPos,Adt).

%% Print lexical items
print_lex(tree(r(_,adt_lex(Cat,Word,Sense,Pos,Attrs)),_)) :-
	!,
	write(Cat),
	tab(1),
	write(Word),
	tab(1),
	write(Sense),
	tab(1),
	write(Pos),
	tab(1),
	write(Attrs),
	nl.

print_lex(tree(_,Ds)) :-
	print_lex_daughters(Ds).

print_lex_daughters([]).
print_lex_daughters([Hd|Tl]) :-
	print_lex(Hd),
	print_lex_daughters(Tl).

%%%%%%%%%%%%%%%%%%%%%%%%
% Removal of particles %
%%%%%%%%%%%%%%%%%%%%%%%%

remove_particles(tree(Rel,Idx,Ds),tree(Rel,Idx,NewDs)) :-
    head_particles(Ds,HeadParticles),
    remove_particles_ds(Ds,NewDs,HeadParticles).

remove_particles_ds([],[],_).
remove_particles_ds([tree(r(svp,l(_,part,Root/_)),_,_)|Tl],NewTl,HeadParticles0) :-
    lists:select(Root,HeadParticles0,HeadParticles),
    !,
    remove_particles_ds(Tl,NewTl,HeadParticles).
remove_particles_ds([H|T],[NewH|NewT],HeadParticles) :-
    remove_particles(H,NewH),
    remove_particles_ds(T,NewT,HeadParticles).

head_particles(Ds,Particles) :-
    lists:memberchk(tree(r(hd,l(Frame,_,_)),_,_),Ds),
    !,
    alpino_postags:postag_of_frame(Frame,_,Attrs),
    alpino_genlex:attr_particles(Attrs,Particles).
head_particles(_,[]).

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Removal of nodes by dependency relation %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remove daughters of a dependency tree node. 
remove_rel(DelRel,tree(Rel,Idx,Ds), tree(Rel,Idx,NewDs)) :-
	remove_rel_daughters(Ds,NewDs,DelRel).

% Remove list items (daughters) with the DelRel dependency relation.
remove_rel_daughters([],[],_).
remove_rel_daughters([tree(r(DelRel,_),_,_)|Tl],NewTl,DelRel) :-
	!,
        remove_rel_daughters(Tl,NewTl,DelRel).
remove_rel_daughters([tree(r(DelRel,_,_),_,_)|Tl],NewTl,DelRel) :-
	!,
        remove_rel_daughters(Tl,NewTl,DelRel).
remove_rel_daughters([Hd|Tl],[NewHd|NewTl],DelRel) :-
	remove_rel(DelRel,Hd,NewHd),
	remove_rel_daughters(Tl,NewTl,DelRel).
*/

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Removal of nodes by category %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Note: a category can either be a phrasal category or a POS tag.

remove_cat(DelCat,tree(Rel,Idx,Ds), tree(Rel,Idx,NewDs)) :-
	remove_cat_daughters(DelCat,Ds,NewDs).

% Remove list items (daughters) with the DelCat category.
remove_cat_daughters(_,[],[]).
remove_cat_daughters(DelCat,[tree(r(_,l(_,DelCat,_)),_,_)|Tl],NewTl) :-
	!, remove_cat_daughters(DelCat,Tl,NewTl).
remove_cat_daughters(DelCat,[tree(r(_,p(DelCat)),_,_)|Tl],NewTl) :-
	!, remove_cat_daughters(DelCat,Tl,NewTl).
remove_cat_daughters(DelCat,[Hd|Tl],[NewHd|NewTl]) :-
	remove_cat(DelCat,Hd,NewHd),
	remove_cat_daughters(DelCat,Tl,NewTl).
*/

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Removal of nodes by their stem %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remove daughters of a dependency tree node. 
remove_stem(Stems,tree(Rel,Idx,Ds), tree(Rel,Idx,NewDs)) :-
	remove_stem_daughters(Stems,Ds,NewDs).

remove_stem_daughters(_,[],[]).
remove_stem_daughters(Stems,[tree(r(_,l(_,_,Stem/_P)),_,_)|Tl],NewTl) :-
	lists:member(Stem,Stems),
	!, remove_stem_daughters(Stems, Tl,NewTl).
remove_stem_daughters(Stems,[Hd|Tl],[NewHd|NewTl]) :-
	remove_stem(Stems,Hd,NewHd),
	remove_stem_daughters(Stems,Tl,NewTl).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% String position removal %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remove string positions from a dependency tree.
remove_idx(tree(Rel,_,Ds),tree(NewRel,NewDs)) :-
    remove_lex_idx(Rel,NewRel),
    remove_daughters_idx(Ds,NewDs).

remove_lex_idx(r(Rel,Rest0),r(Rel,Rest)) :-
    remove_lex_idx_(Rest0,Rest).

remove_lex_idx_(i(Index,Rest0),i(Index,Rest)) :-
    remove_lex_idx_(Rest0,Rest).
remove_lex_idx_(p(Cat),p(Cat)).
remove_lex_idx_(i(I),i(I)).
remove_lex_idx_(l(Pos,Cat,Word/_),l(Pos,Cat,Word)).

% remove_idx on all trees in a (daughter) list.
remove_daughters_idx([],[]).
remove_daughters_idx([Hd|Tl],[NewHd|NewTl]) :-
    remove_idx(Hd,NewHd),
    remove_daughters_idx(Tl,NewTl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simplify root/lemma     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_lemma(tree(Rel,Ds),tree(NewRel,NewDs)) :-
    remove_lex_lemma(Rel,NewRel),
    remove_daughters_lemma(Ds,NewDs).

remove_lex_lemma(r(Rel,Rest0),r(Rel,Rest)) :-
    remove_lex_lemma_(Rest0,Rest).

remove_lex_lemma_(i(Index,Rest0),i(Index,Rest)) :-
    remove_lex_lemma_(Rest0,Rest).
remove_lex_lemma_(p(Cat0),p(Cat)) :-
    remove_lex_lemma__(Cat0,Cat).
remove_lex_lemma_(i(I),i(I)).
remove_lex_lemma_(l(Pos,Cat,Word0),l(Pos,Cat,Word)) :-
    simplify_lemma(Word0,Word).

% remove_lemma on all trees in a (daughter) list.
remove_daughters_lemma([],[]).
remove_daughters_lemma([Hd|Tl],[NewHd|NewTl]) :-
    remove_lemma(Hd,NewHd),
    remove_daughters_lemma(Tl,NewTl).

remove_lex_lemma__(mwu(Lemma0,Sense0),mwu(Lemma,Sense)) :-
    !,
    simplify_lemma(Lemma0,Lemma),
    simplify_lemma(Sense0,Sense).
remove_lex_lemma__(mwu(Lemma0),mwu(Lemma)) :-
    !,
    simplify_lemma(Lemma0,Lemma).
remove_lex_lemma__(Cat,Cat).

simplify_lemma(v_root(Root0,_),Root) :-
    !,
    Root0=Root.
simplify_lemma(Root,Root).

%%%%%%%%%%%%%%%%%%%%%
% Frame to POS tags %
%%%%%%%%%%%%%%%%%%%%%

frames2pos(l(Frame0,Cat,Root),adt_lex(Cat,Root,Sense,PosTag,Attrs)) :-
    (   Frame0 = Extra:Frame
    ->  true
    ;   Frame0 = Frame,
	Extra = []
    ),    
    alpino_postags:postag_of_frame(Extra:Frame,PosTag,Attrs0),
    alpino_treebank:frame2sense(Root,Frame,Sense),
    filter_attributes(PosTag,Attrs0,Attrs).
frames2pos(i(Idx,Lex),i(Idx,NewLex)) :-
    frames2pos(Lex,NewLex).
frames2pos(i(Idx),i(Idx)).
frames2pos(p(Cat),p(Cat)).

frames2pos(tree(r(Rel,Cat),Ds),tree(r(Rel,NewCat),NewDs)) :-
    frames2pos(Cat,NewCat),
    frames2pos_daughters(Ds,NewDs).

frames2pos_daughters([],[]).
frames2pos_daughters([Head|Tail],[NewHead|NewTail]) :-
    frames2pos(Head,NewHead),
    frames2pos_daughters(Tail,NewTail).

/*
filter_attributes(Pos,Atts0,Atts) :-
    filter_attributes_(Pos,Atts0,Atts1),
    (   lists:select(postag='NA()',Atts1,Atts2)
    ->  Atts = Atts2
    ;   Atts = Atts1
    ).

filter_attributes_( Pos,Atts0,Atts) :-
    findall(Ign,ignore_attribute(Pos,Ign),Igns),
    ignore_all(Igns,Atts0,Atts).

ignore_all([],Atts,Atts).
ignore_all([Att|T],Atts0,Atts) :-
    (   lists:select(Att=_,Atts0,Atts1)
    ->  true
    ;   Atts0=Atts1
    ),
    ignore_all(T,Atts1,Atts).

%% ignore attributes *from DT to ADT*
ignore_attribute(_,comparative).
ignore_attribute(_,num).
ignore_attribute(_,vform).
ignore_attribute(_,infl).
ignore_attribute(_,sc).
ignore_attribute(_,special).
ignore_attribute(_,case).
ignore_attribute(_,gen).
ignore_attribute(_,wh).
*/

filter_attributes(_Pos,Atts0,Atts) :-
    filter_attributes(Atts0,Atts).

filter_attributes([],[]).
filter_attributes([Att=Val|T0],T) :-
    filter_attribute(Att,Val,T,T1),
    filter_attributes(T0,T1).

filter_attribute(Att,Val,T0,T) :-
    (   relevant_att(Att)
    ->  T0 = [Att=Val|T]
    ;   T0 = T
    ).


relevant_att(rnum).
relevant_att(stype).
relevant_att(per).
relevant_att(def).
relevant_att(refl).
relevant_att(aform).
relevant_att(personalized).
relevant_att(pron).
relevant_att(iets).
relevant_att(neclass).
relevant_att(tense).
relevant_att(numtype).

%%% not ignored attributes:
%%% rnum (special!)
%%% per=
%%% gen=
%%% def=
%%% refl=refl
%%% aform=base,compar,super
%%% personalized=true (to distinguish "de anderen" from "de andere")
%%% pron=true         (to distinguish "ieders" from "iedere")
%%% iets=true         (to distinguish "iets lekkers" from "lekkere iets")
%%% neclass='LOC','PER','ORG','MISC'
%%% tense=present,past,subjunctive
%%% stype

%%%%%%%%%%%%%%%%%
% ADT bitcoding %
%%%%%%%%%%%%%%%%%

%% bitcode_adt: since we need particle information for bitcoding,
%% this does also first step of lexical lookup. Frames are kept
%% as list in adt_lex(...)
bitcode_lookup_frames_adt(adt_lex(Cat,Root0,Sense0,PosTag,Attrs),
			  adt_lex(Cat,Root,Bc,Frames,Attrs),N,M,Rel) :-
    adapt_case_root(PosTag,Rel,Root0,Sense0,Root,Sense,Attrs),

    %%% check if Root is uninstantiated, in that case attempt to guess it
    %%% from lemma or sense information in Attrs
    alpino_genlex:frames_and_particles(Root,Sense,PosTag,Attrs,Parts,Frames),
    length(Parts,PartsLength),
    NBits is PartsLength + 1,
    give_bits(NBits, Bits),
    Bc is Bits << N,
    M is N + NBits.
bitcode_lookup_frames_adt(i(Index,L),i(Index,L1),N,M,Rel) :-
    bitcode_lookup_frames_adt(L,L1,N,M,Rel).
bitcode_lookup_frames_adt(i(Index),i(Index),N,N,_).
bitcode_lookup_frames_adt(p(Cat),p(Cat),N,N,_).

bitcode_lookup_frames_adt(tree(r(Rel,Cat),Ds),tree(r(Rel,NewCat),NewDs),N,O) :-
    bitcode_lookup_frames_adt(Cat,NewCat,N,M,Rel),
    bitcode_lookup_frames_adt_ds(Ds,NewDs,M,O).

bitcode_lookup_frames_adt_ds([],[],N,N).
bitcode_lookup_frames_adt_ds([Head|Tail],[NewHead|NewTail],N,O) :-
    bitcode_lookup_frames_adt(Head,NewHead,N,M),
    bitcode_lookup_frames_adt_ds(Tail,NewTail,M,O).

%% this does not work yet for pronouns in a conjunction:
%% paraphrase 'hij en zijn vrouw werden benaderd' => men heeft hem en zijn vrouw benaderd
%% nope

%% {}
adapt_case_root(Pron,Rel,Root0,Sense0,Root,Sense,Attrs) :-
    (   adapt_case_root_(Pron,Rel,Root0,Sense0,Root1,Sense1,Attrs)
    ->  Sense1 = Sense, Root1 = Root
    ;   Sense0 = Sense, Root0 = Root
    ).

adapt_case_root_(pron,Rel,Root0,Root0,Root,Root,Attrs) :-
    non_nominative(Rel),
    pers_pronoun1(Root0,Root,Attrs).

adapt_case_root_(pron,Rel,Root0,Root0,Root,Root,Attrs) :-
    nominative(Rel),
    pers_pronoun2(Root0,Root,Attrs).

nominative(su).

non_nominative(obj1).
non_nominative(obj2).
non_nominative(predc).

list_to_root([],R,R).
list_to_root([H|T],F,{[F,H|T]}).

pers_pronoun1(Root0,Root,Attrs) :-
    findall(Root1,pers_pronoun_pair(Root0,Root1,Attrs),List),
    list_to_root(List,Root0,Root).

pers_pronoun2(Root0,Root,Attrs) :-
    findall(Root1,pers_pronoun_pair(Root1,Root0,Attrs),List),
    list_to_root(List,Root0,Root).

pers_pronoun_pair(ik, me, _).
pers_pronoun_pair(ik, mij, _).
pers_pronoun_pair(jij,jou,_).
pers_pronoun_pair(je,jou,_).
pers_pronoun_pair(je,je,_).
pers_pronoun_pair(hij,hem,_).
pers_pronoun_pair(zij,haar,Atts) :-
    member(rnum=sg,Atts).
pers_pronoun_pair(ze,haar,Atts) :-
    member(rnum=sg,Atts).
pers_pronoun_pair(wij,ons,_).
pers_pronoun_pair(we,ons,_).
pers_pronoun_pair(zij,hen,Atts) :-
    member(rnum=pl,Atts).
pers_pronoun_pair(zij,Hun,Atts) :-
    Hun == hun,  % do not generate "hun" for "hen", but allow zij for "hun" if "hun" given
    member(rnum=pl,Atts).
pers_pronoun_pair(ze,hen,Atts) :-
    member(rnum=pl,Atts).
pers_pronoun_pair(ze,Hun,Atts) :-
    Hun == hun,  % do not generate "hun" for "hen", but allow ze for "hun" if "hun" given
    member(rnum=pl,Atts).
pers_pronoun_pair(ze,ze,Atts) :-
    member(rnum=pl,Atts).
%% don't use singular ze for accusative,
%% even if the grammar allows it (the grammar
%% allows it in order to parse Flemish input)
pers_pronoun_pair(Ze0,Ze1,Atts) :-
    member(rnum=sg,Atts),
    (   var(Ze0)
    ->  Ze1 == ze,
	Ze0 == ze
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADT to dt feature structure %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clean :-
    retractall(dt_part(_)).


%% Adt: input
%% NewAdt: simple version of Adt as sign for lexical lookup
%% Fs: Adt as sign
%% Robust: boolean flag
adt_to_fs(Adt,NewAdt,FsAll,Robust) :-
    clean,
    adt_to_fs(Robust,Adt,FsAdt,_:FsAll,[],_Idx,Parts,[],_),
    (   lists:member(Part,Parts),
        assertz(dt_part(Part)),
        fail
    ;   true
    ),
    adt_to_simple_fs(FsAdt,NewAdt,_Simple,[],_).

adt_to_fs(top,tree(r(top,p(Top)),[TopCat]),tree(r(top,p(Top,Fs)),[TopCat1]),_:Fs,
	  Idx,Idx1,Parts0,Parts,_) :-
    alpino_lc_in:top_category_(Fs),
    alpino_data:dt(Fs,Dt),
    adt_to_fs(TopCat,TopCat1,_Rel:Dt,Idx,Idx1,Parts0,Parts,_).


adt_to_fs(robust,tree(r(top,p(Top)),[TopCat]),tree(r(top,p(Top,Fs)),[TopCat1]),_:Fs,
	  Idx,Idx1,Parts0,Parts,_) :-
    alpino_lc_in:robust_top_category_(Fs),
    alpino_data:dt(Fs,Dt),
    adt_to_fs(TopCat,TopCat1,_Rel:Dt,Idx,Idx1,Parts0,Parts,_).

adt_to_fs(tree(r(Rel,p(Cat)),Ds0),tree(r(Rel,p(Cat,Fs)),Ds),Rel:Fs,
	  Idx,Idx1,[Fs|Parts0],Parts,_) :-
    alpino_data:dt(Fs,_,_,Cat,_),
    adt_to_fs_ds(Ds0,Ds,Dts,[],Idx,Idx1,Fs,Parts0,Parts,0,Bc),
    add_dts(Fs,Dts,Bc).

adt_to_fs(tree(r(Rel,adt_lex(Cat,Root0,Bc,Frames,Attrs)),[]),
	  tree(r(Rel,adt_lex(Cat,Root,Bc,Frames,Attrs,Dt)),[]),
	  Rel:Dt,Idx,Idx,[Dt|Parts],Parts,Bc) :-
%%%    (  Root0 = {List},
%%%	when(nonvar(Root),lists:member(Root,List))
%%%     maybe try this at some point, but prettyvars will fail on such structures
    Root0 = Root,
    alpino_data:lexical(Hwrd,Root,_,_,_,_,_,Bc),
    alpino_data:dt(Dt,Hwrd,_,_,Cat,_,Attrs),

    hdrug_util:try_hook(user:dt_apply_attributes(Dt,Attrs)),
    
    %% GvN: if not rel=hd, then this is a node without args, so must
    %% be initialized to have [] values:
    (   Rel == hd
    ->  true
    ;   add_dts(Dt,[],Bc)
    ).

adt_to_fs(tree(r(Rel,i(Index,Lex)),Ds0),
	  tree(r(Rel,i(Index,NewLex)),Ds),
	  Rel:Dt,Idx,[Index:Dt|Idx1],Parts0,Parts,Bc) :-
    adt_to_fs(tree(r(Rel,Lex),Ds0),tree(r(Rel,NewLex),Ds),Rel:Dt,Idx,Idx1,Parts0,Parts,Bc).

adt_to_fs(tree(r(Rel,i(Index)),[]),tree(r(Rel,i(Index)),[]),
	  Rel:Dt,Idx,Idx,Parts,Parts,0) :-
    lists:memberchk(Index:Dt,Idx).

adt_to_fs_ds([],[],Dt,Dt,Idx,Idx,_,Parts,Parts,Bc,Bc).
adt_to_fs_ds([tree(r(_,i(I)),[])|Trees0],Trees,Dt0,Dt,Idx0,Idx,Cat,Parts0,Parts,Bc0,Bc) :-
    \+ lists:member(I:_,Idx0), !,  % in robustness, in parts, nodes may have lost their antecedent
    adt_to_fs_ds(Trees0,Trees,Dt0,Dt,Idx0,Idx,Cat,Parts0,Parts,Bc0,Bc).
adt_to_fs_ds([tree(r(hd,Lex),[])|T],[tree(r(hd,NewLex),[])|NewT],
	     Dt0,Dt,Idx,Idx2,Cat,Parts0,Parts,__,Bc) :-
    !,
    adt_to_fs(tree(r(hd,Lex),[]),tree(r(hd,NewLex),[]),_:Fs,Idx,Idx1,_,_,Bc),
    alpino_data:dt_shared_head_parts(Cat,Fs),
    adt_lex_dt(NewLex,Cat),
    adt_to_fs_ds(T,NewT,Dt0,Dt,Idx1,Idx2,Cat,Parts0,Parts,_,_).
adt_to_fs_ds([H|T],[NewH|NewT],[RelDt|Dt0],Dt,Idx,Idx2,Cat,Parts0,Parts,Bc0,Bc) :-
    adt_to_fs(H,NewH,RelDt,Idx,Idx1,Parts0,Parts1,_),
    adt_to_fs_ds(T,NewT,Dt0,Dt,Idx1,Idx2,Cat,Parts1,Parts,Bc0,Bc).

adt_lex_dt(i(_),_).
adt_lex_dt(i(_,X),Dt) :-
    adt_lex_dt(X,Dt).
adt_lex_dt(adt_lex(_,_,_,_,_,Dt),Dt).

add_dts(Cat,Dts,Bc) :-
    alpino_data:dt_features(NonList,List0),
    %% Subtrees with a 'svp' relation can be a particle. We should not
    %% require them in the dt.
    lists:select(svp,List0,List),
    add_nonlist_dts(NonList,Cat,Dts,Dts1),
    (   single_bitcode(Bc)   % head does not expect svp/particle
    ->  add_list_dts([svp|List],Cat,Dts1)
    ;   add_svp_dts(Cat,Dts1,Dts2),
        add_list_dts(List,Cat,Dts2)
    ).

add_nonlist_dts([],_Cat,Dts,Dts).
add_nonlist_dts([Rel|T],Cat,Dts0,Dts1) :-
    (   lists:select(Rel:Dt,Dts0,Dts)
    ->  Cat:Rel <=> Dt
%    ;   Rel == su
%    ->  Dts0 = Dts   % subjects can be controlled, and this need not be specified in ADT (experimental)
%    ;   Rel == obj1
%    ->  Dts0 = Dts   % objects can be passivized, and this need not be specified in ADT (experimental)
%
% this overgenerates for cases like "zij wil uitrusten en een boek lezen" --> "zij wil een boek uitrusten en lezen"
    
    ;   Dts0 = Dts,
	Cat:Rel ==> []
    ),
    add_nonlist_dts(T,Cat,Dts,Dts1).

% Separate check for svp: allow any particle.
add_svp_dts(Cat,Dts0,Dts1) :-
    select_dts_all(Dts0,svp,List,Dts1),
    Cat:svp <=> RealList,
    alpino_wappend:wcheck_els_svp(RealList,List).

add_list_dts([],_Cat,_).
add_list_dts([Rel|T],Cat,Dts0) :-
    select_dts_all(Dts0,Rel,List,Dts1),
    Cat:Rel <=> RealList,
    RealList = List,
%    (   Rel = mod
%    ->  alpino_wappend:wcheck_els(RealList,List)
%    ;   RealList = List
%    ),
    add_list_dts(T,Cat,Dts1).

select_dts_all([],_,[],[]).
select_dts_all([Rel:Dt|T],Rel,[Dt|List],Dts) :-
    !,
    select_dts_all(T,Rel,List,Dts).
select_dts_all([First|T],Rel,List,[First|Dts]) :-
    select_dts_all(T,Rel,List,Dts).

%%%%%%%%%%%%%%% SIMPLE %%%%%%%%%%%%%%%%%

adt_to_simple_fs(tree(r(top,p(top,D)),[TopCat]),tree(r(top,p(top,D)),[TopCat1]),_:Fs,
	  Idx,Idx1) :-
    !,
    alpino_data:top_cat(Fs),
    alpino_data:dt(Fs,Dt),
    adt_to_simple_fs(TopCat,TopCat1,_Rel:Dt,Idx,Idx1).

adt_to_simple_fs(tree(r(Rel,p(Cat,Dt)),Ds),tree(r(Rel,p(Cat,Dt)),Ds1),Rel:Fs,
	  Idx,Idx1) :-
    alpino_data:dt(Fs,_,_,Cat,_),
    adt_to_simple_fs_ds(Ds,Ds1,[],Dts,Idx,Idx1,Fs),
    add_simple_dts(Fs,Dts).

adt_to_simple_fs(tree(r(Rel,adt_lex(Cat,Root,Bc,Frs,Attrs,Dt)),[]),
                 tree(r(Rel,adt_lex(Cat,Root,Bc,Frs,Dt,SimpleDt)),[]),
                 Rel:SimpleDt,Idx,Idx) :-
    alpino_data:lexical(Hwrd,Root,_,_,_,_,_,Bc),
    alpino_data:dt(SimpleDt,Hwrd,_,_,Cat,_,Attrs),
    (   Rel == hd
    ->  true
    ;   add_simple_dts(SimpleDt,[])
    ).

adt_to_simple_fs(tree(r(Rel,i(Index,Lex)),Ds0),
                 tree(r(Rel,i(Index,NewLex)),Ds),
                 Rel:Dt,Idx,[Index:Dt|Idx1]) :-
    adt_to_simple_fs(tree(r(Rel,Lex),Ds0),tree(r(Rel,NewLex),Ds),Rel:Dt,Idx,Idx1).

adt_to_simple_fs(tree(r(Rel,i(Index)),[]),tree(r(Rel,i(Index)),[]),
	  Rel:Dt,Idx,Idx) :-
    lists:memberchk(Index:Dt,Idx).

adt_to_simple_fs_ds([],[],Dt,Dt,Idx,Idx,_).
adt_to_simple_fs_ds([tree(r(hd,Lex),[])|T],[tree(r(hd,NewLex),[])|NewT],
	     Dt0,Dt,Idx,Idx2,Cat) :-
    !,
    adt_to_simple_fs(tree(r(hd,Lex),[]),tree(r(hd,NewLex),[]),_:Cat2,Idx,Idx1),
    alpino_data:dt_shared_head_parts(Cat,Cat2),
    adt_lex_simple_dt(NewLex,Cat),
    adt_to_simple_fs_ds(T,NewT,Dt0,Dt,Idx1,Idx2,Cat).
adt_to_simple_fs_ds([H|T],[NewH|NewT],Dt0,Dt,Idx,Idx2,Cat) :-
    adt_to_simple_fs(H,NewH,RelDt,Idx,Idx1),
    adt_to_simple_fs_ds(T,NewT,[RelDt|Dt0],Dt,Idx1,Idx2,Cat).


adt_lex_simple_dt(i(_),_).
adt_lex_simple_dt(i(_,X),Dt) :-
    adt_lex_simple_dt(X,Dt).
adt_lex_simple_dt(adt_lex(_,_,_,_,_,Dt),Dt).

add_simple_dts(Cat,Dts) :-
    alpino_data:dt_features(NonList,List),
    %% Subtrees with a 'svp' relation can be a particle. We should not
    %% require them in the dt.
    add_nonlist_simple_dts(NonList,Cat,Dts,Dts1),
    add_list_simple_dts(List,Cat,Dts1).

add_nonlist_simple_dts([],_Cat,Dts,Dts).
add_nonlist_simple_dts([Rel|T],Cat,Dts0,Dts1) :-
    (    lists:select(Rel:Dt,Dts0,Dts)
    ->   Cat:Rel <=> Dt
    ;    Dts0 = Dts,
	 Cat:Rel ==> []
    ),
    add_nonlist_simple_dts(T,Cat,Dts,Dts1).

add_list_simple_dts([],_Cat,_).
add_list_simple_dts([Rel|T],Cat,Dts0) :-
    select_dts_all(Dts0,Rel,List,Dts1),
    Cat:Rel <=> List,
    add_list_simple_dts(T,Cat,Dts1).


%% TMP
%% remove punctuation from ADT, since we cannot generate punctuation
%% marks yet anyway
remove_punct(tree(TOP,Ds0),tree(TOP,Ds)) :-
    remove_punct_ds(Ds0,Ds).

remove_punct_ds([],[]).
remove_punct_ds([H|T],Ds) :-
    remove_punct_d(H,Ds,Ds1),
    remove_punct_ds(T,Ds1).

remove_punct_d(tree(r('--',adt_lex(_,_,_,punct,_)),[]),Ds0,Ds) :-
    !,
    Ds0 = Ds.
remove_punct_d(Tree,[Tree|Ds],Ds).

