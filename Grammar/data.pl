:- module(alpino_data, []).

:- expects_dialect(sicstus).

%% inherit feature library
:- use_module(hdrug(hdrug_feature)).
:- use_module(hdrug(hdrug_util)).

:- multifile user:term_expansion/2.

:- op(1200,xfx,::-).

user:term_expansion((Goal ::- Body),GoalList) :-
    findall(Rule,call_with_constraints(Body,Goal,Rule),GoalList).

user:term_expansion(expand_sem_filter, List) :-
    compile_sem_filter(List).

:- public call_with_constraints/3.
call_with_constraints(Body1,Goal1,Rule) :-
    call(Body1),
    copy_term(Body1/Goal1,_/Goal,ConsList),
    cons_body(ConsList,Goal,Rule).

cons_body([],Head,Head).
cons_body([Goal|T],Head,(Head:-Goals)):-
    cons_body1(T,Goal,Goals).

cons_body1([],Goal,Goal).
cons_body1([Goal|T],PrevGoal,(PrevGoal,Goals)) :-
    cons_body1(T,Goal,Goals).

%% ---------------------------------------------------------------------- %%

cat_if_defined(Var:Path,Val) :-
    cat(Var),
    if_defined(Var:Path,Val).

cat_if_defined(Var:Path,Val,Default) :-
    cat(Var),
    if_defined(Var:Path,Val,Default).

/*

is often incomplete, therefore the robust catch all below is used.

cats([a, aan_het_comp, abs_p, adv, app_n, app_n_app, app_np, app_np_mod,
      call, clist, comp, comparative, comparativep, complex_etc,
      conj, denk_ik, det, dip, dip_adv, ecomp, eenmaal_adv, enumeration,
      etc, etopic, fixed_part, gen_det, hoe_adv, iets_adv, iets_n, imp,
      int_adv, lconj, max, modal_adv, modifier, n, np, num, num_na, op_comp,
      optpunct, p, part, pn, post_adj_adv, post_adv_adv, post_loc_adv_adv,post_n_adv,
      post_n_n, post_np_adv, post_p, post_p_adv, post_pp, post_wh_adv, pp,
      pre_det_quant, pre_num_adv, pre_wh_adv, pred, predm_adv, predm_adv_num,
      pron, punct, redrel, rel, root, rootbar, sbar, score_cat, start,
      start_app_n_app, sv1, tag, te_comp, tmp_app_n, tmp_det, top_cat,
      uit_comp, v, v2_vp, v_noun, vandaar_adv, vb, vc, vc_noun, vp, vproj,
      vpx, vpx_noun, within_word_conjunct, zo_van_adv, zom_a, zom_adv]).

cat(Cat) ::-
    cats(List),
    lists:member(Type,List),
    Cat => Type.
*/

cat(Cat) ::-
    alpino_types:top(List),
    lists:member(Type,List),
    Cat => Type.

%% dt_features(?ListOfAttributes,?ListOfAttributes)
%% first list are the attributes to refer to arguments
%% second list are the attributes to refer to lists of arguments
dt_features([rhd,whd,body,cmp,dlink,hdf,ld,me,nucl,obcomp,obj1,obj2,pc,pobj1,
	     predc,sat,se,su,sup,tag,vc],
	    [app,cnj,crd,det,dp,mod,predm,svp,mwp]).

dt_ds([],_,Ds,Ds).
dt_ds([Att|Atts],Dom,[Att/Arg|List1],List) :-
    Dom:Att <=> Arg,
    dt_ds(Atts,Dom,List1,List).

result(Result,List,Tokens) ::-
    Result => result,
    Result:cat:list <=> List,
    Result:string <=> Tokens.

dt_daughters(DT,L,M) ::-
    dt_features(V,W),
    dt_ds(V,DT,L,[]),
    dt_ds(W,DT,M,[]).

max_dt(Cat) ::-
    Cat => max.

dt(Cat,DT) ::-
    Cat:dt <=> DT.

dt_if_defined(Cat,DT) ::-
    cat(Cat),
    Cat:dt <?=?> DT.

dt_num(DT,Num) ::-
    DT:num <=> Num.

dt(DT,Hwrd,Frame,Cat,Ix) ::-
    DT => dt,
    DT:hwrd <=> Hwrd,
    DT:frame <=> Frame,
    DT:cat <=> Cat,
    DT:ix <=> Ix.

dt(DT,Hwrd,Fwrd,Frame,Cat,Ix) ::-
    DT => dt,
    DT:hwrd <=> Hwrd,
    DT:fwrd <=> Fwrd,
    DT:frame <=> Frame,
    DT:cat <=> Cat,
    DT:ix <=> Ix.

dt(DT,Hwrd,Fwrd,Frame,Cat,Ix,Attrs) ::-
    DT => dt,
    DT:hwrd <=> Hwrd,
    DT:fwrd <=> Fwrd,
    DT:frame <=> Frame,
    DT:cat <=> Cat,
    DT:ix <=> Ix,
    DT:attrs <=> Attrs.

dt_out(Tag,DT) ::-
    Tag:dt_out <=> DT.

dt_svp(DT,SVP) ::-
    DT => dt,
    DT:svp <=> SVP.

punct(P) ::-
    P => punct.

tags(Mother,Tags) ::-
    cat(Mother),
    if_defined(Mother:tags,Tags,[]).

np_agr(NP,Agr) ::-
    NP => np,
    NP:agr <=> Agr.

subj_agr(VP,Agr) ::-
    VP => vproj,
    VP:subj <=> Subj,
    Subj:e_agr <=> Agr.

%% no longer used?
%agr_val(sg,Val) :-
%    Val:agr => sg.
%agr_val(pl,Val) :-
%    Val:agr => pl.

mexs(Mother,Mexs) ::-
    cat(Mother),
    if_defined(Mother:mexs,Mexs,[]).

exs(Mother,Mexs) ::-
    cat(Mother),
    if_defined(Mother:exs,Mexs,[]).

dt_shared_head_parts(DT1,DT2) :-
    DT1 => dt, DT2 => dt,
    DT1:hwrd <=> DT2:hwrd,
    DT1:fwrd <=> DT2:fwrd,
    DT1:frame <=> DT2:frame,
    DT1:cat <=> DT2:cat,
    DT1:lix <=> DT2:lix.

lix(DT,LIX) ::-
    DT => dt,
    DT:lix <=> LIX.

label(DT,Word,Lemma,His,P0,P) ::-
    DT => hwrd,
    DT:lexical <=> Word,
    DT:lemma <=> Lemma,
    DT:his <=> His,
    DT:beginpos <=> P0,
    DT:endpos <=> P.

lexical(DT,Lexical,Lemma,Surface,P0,P,His,Bitcode) ::-
    DT => hwrd,
    DT:lexical <=> Lexical,
    DT:lemma <=> Lemma,
    DT:surface <=> Surface,
    DT:beginpos <=> P0,
    DT:endpos <=> P,
    DT:his <=> His,
    DT:bitcode <=> Bitcode.

cat_to_result(Cat,Result) ::-
    is_defined(Cat:dt,yes),
    Cat:dt <=> _,
    Result => result,
    Result:cat => robust,
    Result:cat:list <=> [Cat].

cat_to_result(Cat,Result) ::-
    Result => result,
    Cat => robust,
    Result:cat <=> Cat.

cat_to_result(Cat,Result) ::-
    cat(Cat),
    Cat /=> robust,
    is_defined(Cat:dt,no),
    Result => result,
    Result:cat => robust,
    Result:cat:list => [].

result_frames(Result,Frames) ::-
    Result => result,
    Result:frames <=> Frames.

lexical_node(Node,Stem,Word,Frame,LexHis) ::-
    Node:dt:fwrd:lexical ==> Stem,
    Node:dt:fwrd:surface ==> Word,
    Node:dt:frame ==> Frame,
    Node:dt:hwrd:his ==> LexHis.

yes_det(Node) ::-
    Node => n,
    Node:hdet => yes.

det_agr(Node,Agr) ::-
    Node => det,
    Node:agr <=> Agr.

n_agr(Node,Agr) ::-
    Node => n,
    Node:agr <=> Agr.

agr(Node,Agr) ::-
    Node:agr <=> Agr.

sg(Agr) ::-
    Agr => agr,
    Agr => sg.

pl(Agr) ::-
    Agr => agr,
    Agr => pl.

de(Node) ::-
    Node => agr,
    Node => de.

het(Node) ::-
    Node => agr,
    Node => het.

def(Node) ::-
    Node => np,
    Node:agr => def.
indef(Node) ::-
    Node => np,
    Node:agr => indef.

plural(Node) ::-
    Node:agr => pl.

plural_noun(Node) ::-
    Node => n,
    Node:agr => pl.

aform(Node,Attr) ::-
    Node:aform <=> Attr.

prep(Node,Prep) ::-
    Node:prep <=> Prep.

hstem(Node,Stem) ::-
    Node:hstem <=> Stem.

not_attr(Node) ::-
    Node => aform,
    Node => ~attr.

nominative(Node) ::-
    Node:case => nom.    

slashed_prep(Node) ::-
    Node => p,
    Node:slash <=> [_].

%% this one could be employed during first phase
%% todo: distinguish wh/non-wh?
syntactic_penalty_cat_d(Cat,non_subj_np_topic) ::-
    Cat => sv1,
    Cat:slash <=> [Slash],
    Slash => np,
    Slash:wh => nwh,
    Slash:case => ~nom.

syntactic_penalty_cat_d(Cat,subj_np_topic) ::-
    Cat => sv1,
    Cat:slash <=> [Slash],
    Slash => np,
    Slash:wh => nwh,
    Slash:case => nom.

%% negation inside does not work in term-expanion????
syntactic_penalty_cat_d(Cat,non_np_topic) ::-
    Cat => sv1,
    Cat:slash <=> [Slash],
    Slash:wh => nwh,
    Slash /=> np.

/*  now treated in the grammar as a constraint
%% ??Er woont men heerlijk
%% Men woont er heerlijk
syntactic_penalty_cat_d(Cat,er_men) :-
    Cat => sv1,
    Cat:slash <=> [Slash],
    Slash => modifier,
    Slash:hstem ==> er, 
    Cat:subj <=> Subj,
    Subj => np,
    Subj:hstem ==> men.
*/

%% ??Er woont de dokter al jaren
%% De dokter woont er al jaren
syntactic_penalty_cat_d(Cat,er_def) :-
    Cat => sv1,
    Cat:slash <=> [Slash],
    Slash => modifier,
    Slash:hstem ==> er, 
    Cat:subj <=> Subj,
    Subj => np,
    Subj:agr => def.

%% this one could be employed during first phase of parsing
syntactic_penalty_cat(Cat,non_subj_np_rel) ::-
    Cat => vp,
    Cat:slash <=> [Slash],
    Slash => np,
    Slash:case => ~nom.

syntactic_penalty_cat(Cat,subj_np_rel) ::-
    Cat => vp,
    Cat:slash <=> [Slash],
    Slash => np,
    Slash:case => nom.

syntactic_penalty_cat(Cat,non_np_rel) ::-
    Cat => vp,
    Cat:slash <=> [Slash],
    Slash /=> np.

%% dis-prefer long-distance dependencies
syntactic_penalty_cat(Cat,long_distance_dep) ::-
    Cat => comp,
    Cat:slash <=> [_].

syntactic_penalty_cat(Cat,non_long_distance_dep) ::-
    Cat => comp,
    Cat:slash => [].

syntactic_penalty_cat(Cat,extraction_from_a_pp) ::-
    Cat => a,
    Cat:sc => [],
    Cat:slash <=> [PP],
    PP => pp.

adjective_er_plural(Cat,Agr,Sg) ::-
    Cat => a,
    Cat:dt => dt,
    Cat:dt:frame ==> adjective(er(_)),
    Cat:agr <=> Agr,
    Sg => agr,
    Sg => sg.

conj(Cat,Conj,Agr) ::-
    Cat => clist,
    Cat:cform <=> Conj,
    Cat:cat => np,
    Cat:cat:e_agr <=> Agr.

conj(Cat,Conj,Agr) ::-
    Cat => clist,
    Cat:cform <=> Conj,
    Cat:cat => n,
    Cat:cat:e_agr <=> Agr.

det_loc(Cat) ::-
    Cat => n,
    Cat:neclass => 'LOC'.

puncttype(Cat,Type) ::-
    Cat => root,
    Cat:puncttype <=> Type.

puncttype(Cat,Type) ::-
    Cat => max,
    Cat:puncttype <=> Type.

vraag_puncttype(Type) ::-
    Type => vraag.

wh_relagr(Cat,Agr) ::-
    Cat => det,
    Cat:wh => rwh,
    Cat:wh:relagr <=> Agr.



%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generation features %%
%%%%%%%%%%%%%%%%%%%%%%%%%

%start_du_dp(Cat,Dp) ::-
%    Cat => start,
%    dt(Cat,Dt),
%    Dt:cat <=> du,
%    Dt:dp <=> Dp.

cat_type(Cat,Type) ::-
    ( Type = modifier ; Type = pp; Type = np),
    Cat => Type.

subject_topic(Slash,su) :-
    Slash:case => nom.
subject_topic(Slash,no_su) :-
    Slash:case /=> nom,
    !.
subject_topic(_,no_su).

%syntactic_feature_cat(Cat,topic(Type,Su)) ::-
%    Cat => sv1,
%    Cat:slash <=> [Slash],
%    cat_type(Slash,Type),
%    subject_topic(Slash,Su).

%% We should also be careful not to penalize a non-standard ordering more than
%% once (e.g. for each level?), so now only at cat=vp, or cat=v2_vp.
%% We then miss: mf constructed in coordination of vproj. Perhaps allow
%% parallel mf?
%% 'dat ik haar kuste en hem uitschold'
%% mf [ np(ik), { [np(haar)],[np(hem)] }])

vp_cat(Cat,Mf) ::-
    Cat => vp,
    Cat:mf <=> Mf.
vp_cat(Cat,Mf) ::-
    Cat => v2_vp,
    Cat:mf <=> Mf.

vp(Cat) ::-
    Cat => vp.

case(Cat,Case) ::-
    Cat => np,
    Cat:case => case,
    Cat:case <=> Case.

postag(Cat,PosTag) ::-
    Cat:dt => dt,
    Cat:dt:frame <=> PosTag.

%postag_dt(Dt,PosTag) ::-
%    Dt => dt,
%    Dt:frame <=> PosTag.

modifier(Cat,Sub) ::-
    Cat => modifier,
    Cat:mcat <=> Sub.

predicative(Cat,Sub) ::-
    Cat => pred,
    Cat:pcat <=> Sub.

/*
np_redrel(NP) ::-
    NP => np,
    NP:redrel => yes.
*/

sv1_extra(Cat,List) ::-
    Cat => sv1,
    Cat:iexs <=> List.

comparative_cat(Cat) ::-
    Cat => comparativep.

sv1_mextra(Cat,List) ::-
    Cat => sv1,
    Cat:imexs <=> List.

result_term(Sc,Str,Cat,Tree,Frames,Result) ::-
    Result => result,
    Result:score <=> Sc,
    Result:string <=> Str,
    Result:cat <=> Cat,
    Result:tree <=> Tree,
    Result:frames <=> Frames.

result_term(Sc,Str,Cat,Tree,Frames,DT,Result) ::-
    Result => result,
    Result:score <=> Sc,
    Result:string <=> Str,
    Result:cat <=> Cat,
    Result:tree <=> Tree,
    Result:frames <=> Frames,
    Result:cdt <=> DT.

result_cdt(Result,DT) :-
    Result:cdt <=> DT.

robust_list_to_cat(List,Cat) ::-
    Cat => robust,
    Cat:list <=> List.

slash(Cat,Slash) ::-
    Cat:slash <=> Slash.

empty_e_deps(Cat) ::-
    cat(Cat),
    if_defined(Cat:e_deps,[],[]).

vslash_empty(Vslash) ::-
    Vslash => [].
vslash_empty(Vslash) ::-
    Vslash => vslash,
    Vslash:exs => [].

context_embed_node(Node) ::-
    Node => sv1.
context_embed_node(Node) ::-
    Node => sbar.
context_embed_node(Node) ::-
    Node => rel.

context_swap_node(Node) ::-
    Node => v,
    Node:vform => fin.
context_swap_node(Node) ::-
    Node => vb,
    Node:vform => fin.

unify_mods(Node) ::-
    Node:mods   <=> Node:cmods,
    Node:predms <=> Node:cpredms,
    Node:dets   <=> Node:cdets,
    Node:apps   <=> Node:capps,
    Node:ccat0  <=> Node:ccat.

percolation_features(Node,Apps,Capps,Dets,Cdets,Mods,Cmods,Predms,Cpredms) ::-
    Node:apps <=> Apps,
    Node:dets <=> Dets,
    Node:mods <=> Mods,
    Node:predms <=> Predms,
    Node:capps <=> Capps,
    Node:cdets <=> Cdets,
    Node:cmods <=> Cmods,
    Node:cpredms <=> Cpredms.

%% precedes_subject_cat(Arg,Subject)
%% Arg can precede Subject in the midfield
%% reflexives
%% R-pronominals
precedes_subject_cat(Arg,Dep) ::-
    Arg => np,
    Arg:sel => to_left, % reduce spur amb
    Dep:sel => to_left, % reduce spur amb
    Dep:prs => ~invje,
    Dep:subn => ~sub_def_pron, % sub_det ok: omdat zich dat wreekt..
                               % indef_pron: omdat zich niemand daarvoor schaamt
    Arg:nform => (refl;er).

%% datives 
precedes_subject_cat(Arg,Dep) ::-
    Arg => np,
    Arg:sel => to_left, % reduce spur amb
    Dep:sel => to_left, % reduce spur amb
    Dep:subn => ~sub_def_pron, % sub_det ok: omdat hem dat/dit/het in de weg zit
    Dep:prs => ~invje,
    Arg:case => dat,
    Arg:nform => ~refl & ~er.

%% omdat me dat niet boeit 
precedes_subject_cat(Arg,Dep) ::-
    Arg => np,
    Arg:sel => to_left, % reduce spur amb
    Dep:sel => to_left, % reduce spur amb
    Dep:subn => (sub_det;sub_indef_pron), % 
    Dep:prs => ~invje,
    Arg:subn => sub_def_pron,
    Arg:case => acc,
    Arg:nform => ~refl & ~er.

%% argument PP
precedes_subject_cat(Arg,Dep) ::-
    Arg => pp,
    Arg:sel => to_left, % reduce spur amb
    Arg:slash => [],    % stranded prepositions should follow subject!
                        % *omdat zich daar toen in kinderen bevonden
    Dep:sel => to_left, % reduce spur amb
    Dep:prs => ~invje,
    Dep:subn => ~sub_def_pron, % sub_det ok: omdat hem dat/dit/het in de weg zit
                               %             omdat hem niemand in de weg zit
    Arg:ld_pc => (pc_pp;ld_pp).

%% er wonen hier mensen
precedes_subject_cat(Arg,Dep) ::-
    Arg => adv,
    Arg:tmploc => loc,
    Dep:sel => to_left, % reduce spur amb
    Dep:prs => ~invje,
    Dep:subn => ~sub_def_pron. % sub_det ok: omdat hem dat/dit/het in de weg zit
                               %             omdat hem niemand in de weg zit


precedes_embedded_subject_cat(Arg,Dep) ::-
    precedes_subject_cat(Arg,Dep).

%% pronoun acc
%% ik zie het hem doen
%% ik zie het Piet doen
%% ik zie dat Piet nooit doen
precedes_embedded_subject_cat(Arg,Dep) ::-
    Arg => np,
    Arg:sel => to_left, % reduce spur amb
    Dep:sel => to_left, % reduce spur amb
    Arg:subn => sub_det, % ik zie het hem doen
    Dep:prs => ~invje,
    Arg:case => acc,
    Arg:nform => ~refl & ~er.



% Derivation tree structure
deriv_tree_struct(Id,Fs,Daughters,Struct) ::-
    Struct => tree,
    Struct:rulename ==> Id,
    Struct:label ==> Fs,
    Struct:ds ==> Daughters.

dt_hwrd_positions(Fs,BeginPos,EndPos) ::-
    Fs:dt:hwrd:beginpos ==> BeginPos,
    Fs:dt:hwrd:endpos ==> EndPos.

dt_fwrd_positions(Fs,BeginPos,EndPos) ::-
    Fs:dt:fwrd:beginpos ==> BeginPos,
    Fs:dt:fwrd:endpos ==> EndPos.

not_ynquestion(Dt) ::-
    Dt:stype => ~ynquestion.
not_whquestion(Dt) ::-
    Dt:stype => ~whquestion.
not_imparative(Dt) ::-
    Dt:stype => ~imparative.
not_declarative(Dt) ::-
    Dt:stype => ~declarative.
not_topic_drop(Dt) ::-
    Dt:stype => ~topic_drop.

ynquestion(Dt) ::-
    Dt:stype => ynquestion.
whquestion(Dt) ::-
    Dt:stype => whquestion.
imparative(Dt) ::-
    Dt:stype => imparative.
declarative(Dt) ::-
    Dt:stype => declarative.
topic_drop(Dt) ::-
    Dt:stype => topic_drop.

inout(H) ::-
    H:mods => [].

mexs_cat_mods(Cat,Out) ::-
    Cat => mexs_cat,
    Cat:mods <=> Out.

top_cat(Cat) ::-
    Cat => top_cat,
    Cat:dt => dt.

clist(Mother,Cats,Conj) ::-
    Mother => clist,
    Mother:cats <=> Cats,
    Mother:conj <=> Conj.

dt_cnj_crd(Dt,Cnj,Crd) ::-
    Dt => dt,
    Dt:cnj <=> Cnj,
    Dt:crd <=> Crd.

ld_pp(Cat) ::-
    Cat => pp,
    Cat:ld_pc => ld_pp.

vproj_without_eps3(Cat) ::-
    Cat => vproj,
    Cat:eps3 => no.

weaken_slash(A,B) ::-
    A => np,
    B => np.
weaken_slash(A,B) ::-
    A => pred,
    B => pred,
    A:slash <=> B:slash.
weaken_slash(A,A) ::-
    A => vc.
weaken_slash(A,B) ::-
    A => pp,
    B => pp,
    A:slash <=> B:slash.
weaken_slash(A,B) ::-
    A => vp,
    B => vp,
    A:slash <=> B:slash.
weaken_slash(A,B) ::-
    A => sbar,
    B => sbar,
    A:slash <=> B:slash.
weaken_slash(A,B) ::-
    A => modifier,
    B => modifier.
weaken_slash(A,B) ::-
    A => adv,
    B => adv.

weaken_vslash([],[]) ::-
    true.
weaken_vslash(A,B) ::-
    A:cj <=> B:cj,
    A:vslashid <=> B:vslashid.

weaken_slash_list([],[]).
weaken_slash_list([El0],[El]) :-
    weaken_slash(El0,El).

sem_paths([dt,cmods,mods,cpredms,predms,mf,cats,relhd,ndt,ccat,ccat0,capps,apps,cdets,dets]).

separate_clause(separate(X,Y)) :-
    hdrug_feature:has_type(_,X,_),
    nonvar(X),
    sem_paths(Atts),
    ignore_paths(Atts,X,Y).

ignore_paths([],X,X).
ignore_paths([H|T],X0,X) :-
    hdrug_feature:is_defined(X0:H,Bool),
    ignore_it(Bool,H,X0,X1),
    ignore_paths(T,X1,X).

ignore_it(no,_,X,X).
ignore_it(yes,Att,X0,X) :-
    hdrug_feature:unify_except(X0,X,Att).

compile_sem_filter(List) :-
    findall(Clause,separate_clause(Clause),List).

expand_sem_filter.
