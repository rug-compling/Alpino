:- module(alpino_tr_tag, [ tr_tag/2 ]).

:- expects_dialect(sicstus).

:- discontiguous
    t/2.

tr_tag(with_dt(A,_),B) :-
    !,
    tr_tag(A,B).
tr_tag(C-A,C-B) :-
    !,
    tr_tag(A,B).
tr_tag(Tag0,Tag) :-
    t(Tag0,Tag),
    !.
tr_tag(Tag,Tag).


% ignore hebben/zijn
t(verb(_,B0,C0),            verb(B,C)) :-
    (   B0 = inf(no_e)
    ->  B  = inf_pl
%    ;   B0 = imp(_)  % tmp
%    ->  B  = imp     % tmp
    ;   B0 = past(Agr)
    ->  B  = Agr
    ;   B0 = both(pl)
    ->  B  = pl
    ;   B0 = pl
    ->  B  = inf_pl
    ;   B0 = inf
    ->  B  = inf_pl
    ;   B0 = B
    ),
    t_subcat(C0,C).

%% adv_tag no longer exists
t(adv_tag,tag).

t(adj_number(enkele),adjective(e)).

t(name_determiner(X),  determiner(X)).
t(name_determiner(X,_),determiner(X)).

t(adjective(Infl0),           adjective(Infl)) :- adj_infl(Infl0,Infl).
t(adjective(Infl0,_),         adjective(Infl)) :- adj_infl(Infl0,Infl).

t(np_adjective,             np_adjective).
t(np_adjective(_),          np_adjective).
t(het_np_adjective(_),      np_adjective).
t(het_np_adjective,         np_adjective).
t(clause_np_adjective,      np_adjective).
t(clause_np_adjective(_),   np_adjective).

t(end_nominalized_adjective(_),  nominalized_adjective).
t(ge_nominalized_adjective(_),  nominalized_adjective).
t(end_nominalized_adjective, nominalized_adjective).
t(ge_nominalized_adjective,  nominalized_adjective).


t(post_adjective(A,_),      post_adjective(A)).

t(noun(DeHet0,_,Num0),             noun(DeHet,Num,[])) :-
    tr_num(Num0,Num),
    tr_dehet(Num,DeHet0,DeHet).

t(noun(DeHet0,_,Num0,Sc0),         noun(DeHet,Num,Sc)) :-
    tr_num(Num0,Num),
    tr_dehet(Num,DeHet0,DeHet),
    tr_noun_subcat(Sc0,Sc).

t(meas_mod_noun(DeHet0,_,Num0),    meas_mod_noun(DeHet,Num,[])) :-
    tr_num(Num0,Num),
    tr_dehet(Num,DeHet0,DeHet).

t(meas_mod_noun(DeHet0,_,Num0,Sc0),meas_mod_noun(DeHet,Num,Sc)) :-
    tr_num(Num0,Num),
    tr_dehet(Num,DeHet0,DeHet),
    tr_noun_subcat(Sc0,Sc).

t(amount_meas_mod_noun(DeHet0,_,Num0),    amount_meas_mod_noun(DeHet,Num,[])) :-
    tr_num(Num0,Num),
    tr_dehet(Num,DeHet0,DeHet).

t(amount_meas_mod_noun(DeHet0,_,Num0,Sc0),amount_meas_mod_noun(DeHet,Num,Sc)) :-
    tr_num(Num0,Num),
    tr_dehet(Num,DeHet0,DeHet),
    tr_noun_subcat(Sc0,Sc).

t(mod_noun(DeHet0,_,Num0),         mod_noun(DeHet,Num,[])) :-
    tr_num(Num0,Num),
    tr_dehet(Num,DeHet0,DeHet).

t(mod_noun(DeHet0,_,Num0,Sc0),     mod_noun(DeHet,Num,Sc)) :-
    tr_num(Num0,Num),
    tr_dehet(Num,DeHet0,DeHet),
    tr_noun_subcat(Sc0,Sc).

t(tmp_noun(DeHet0,_,Num0),         tmp_noun(DeHet,Num,[])) :-
    tr_num(Num0,Num),
    tr_dehet(Num,DeHet0,DeHet).

t(tmp_noun(DeHet0,_,Num0,Sc0),     tmp_noun(DeHet,Num,Sc)) :-
    tr_num(Num0,Num),
    tr_dehet(Num,DeHet0,DeHet),
    tr_noun_subcat(Sc0,Sc).

tr_noun_subcat(Sc0,Sc) :-
    (  ignore_noun_subcat(Sc0)
    -> Sc = []
    ;  Sc0 = Sc
    ).

tr_dehet(pl,_,Out) :-
    !,
    Out = both.
tr_dehet(_,D,D).

tr_num(bare_meas,Out) :-
    !,
    Out = meas.
tr_num(Out,Out).

ignore_noun_subcat(subject_sbar).
ignore_noun_subcat(subject_sbar_no_het).
ignore_noun_subcat(subject_vp).
ignore_noun_subcat(subject_vp_no_het).
ignore_noun_subcat(pred_pp(_)).
ignore_noun_subcat(pred_pp(_,_)).
ignore_noun_subcat(pred_pp_pl(_)).
ignore_noun_subcat(pred_pp_pl(_,_)).

% ignore subcat
t(v_noun(C0),              v_noun(C)) :-
    t_subcat(C0,C).

% ignore subtype of conjunctions
t(left_conj(_),           left_conj).
t(right_conj(_),          right_conj).
t(conj(_),                conj).

% ignore number and netype for names
t(proper_name(_,_),       proper_name).
t(proper_name(_),         proper_name).

% treat cleft 'dit/dat' as determiner/pronoun 'dit/dat'
t(cleft_het_noun,determiner(het,nwh,nmod,pro,nparg)).

% treat special 'het' as determiner/pronoun 'het'
t(het_noun,determiner(het,nwh,nmod,pro,nparg,wkpro)).

% treat special 'wat' as wh-pronoun 'wat'
t(wh_cleft_het_noun,pronoun(ywh,thi,sg,het,both,indef,nparg)).

%% keep form only for potential hdf-postpositions
%% probably these ought to be their own tags,
%% because quite different distribution

t(particle(Form), Tag ) :-
    (   lists:member(Form,
                 [af,aan,[aan,toe],door,[en,al],heen,in,
                  [in,de,plaats],langs,mee,na,om,op,toe,
                  uit,vandaan])
    ->  Tag = particle(Form)
    ;   Tag = particle
    ).

t(preposition(A,_),Tag) :-
    (  lists:member(A,[van,in,op,met,voor,aan,bij,uit,over,naar,door,na,
                       tegen,tot,volgens,tussen,om,onder,tijdens,achter,
                       met,per,binnen,aldus,vanwege,sinds,zonder,via,
                       vanaf])
    ->  Tag = preposition(A)
    ;   Tag = preposition
    ).
t(preposition(A,_,Sc),Tag) :-
    (  lists:member(A,[van,in,op,met,voor,aan,bij,uit,over,naar,door,na,
                       tegen,tot,volgens,tussen,om,onder,tijdens,achter,
                       met,per,binnen,aldus,vanwege,sinds,zonder,via,
                       vanaf])
    ->  Tag = preposition(A,Sc)
    ;   Tag = preposition(Sc)
    ).


t(pp(_),pp).

%% don't distinguish between reflexive and normal pronoun
%% complicated because wk/pro for normal pronouns

t(reflexive(fir,sg),me).
t(pronoun(nwh,fir,sg,de,dat_acc,def,wkpro),me).
t(pronoun(nwh,fir,sg,de,dat_acc,def),me).
t(pronoun(nwh,fir,both,de,dat_acc,def),me).
t(pronoun(nwh,fir,sg,de,dat_acc,def),me).

t(reflexive(u_thi,both),zich).
t(pronoun(nwh,thi,both,de,dat_acc,def,wkpro),zich).
t(pronoun(nwh,thi,both,de,dat_acc,def),zich).

t(reflexive(je,both),je).
t(pronoun(nwh,je,sg,de,both,def,wkpro),je).
t(pronoun(nwh,je,both,de,dat_acc,def),je).

t(reflexive(fir,pl),ons).
t(pronoun(nwh,fir,pl,de,dat_acc,def),ons).

t(reflexive(u,sg),u).
t(pronoun(nwh,u,sg,de,both,def),u).

t(reflexive(je,pl),jullie).
t(pronoun(nwh,je,pl,de,both,def),jullie).

t_subcat(ninv(Frame0,_),Frame):-
    !,
    t_subcat(Frame0,Frame).

t_subcat(fixed_dep(Frame0),Frame):-
    !,
    t_subcat(Frame0,Frame).

t_subcat(part_fixed_dep(_,Frame0),Frame):-
    !,
    t_subcat(Frame0,Frame).

t_subcat(modifier(Frame0),Frame):-
    !,
    t_subcat(Frame0,Frame).

t_subcat(Frame0,Frame) :-
    Frame0 =.. [Func,_Part|Args],
    atom_concat(part_,NewFunc,Func),
    !,
    Frame1 =.. [NewFunc|Args],
    t_subcat(Frame1,Frame).

t_subcat(Frame0,Frame) :-
    (   t_s_r(Frame0,Frame1)
    -> 	Frame = Frame1
    ;   Frame = n
    ).

t_s_r(aux_simple(S),         aux(S)).

t_s_r(aci,                   aux(inf)).
t_s_r(aci_simple,            aux(inf)).
t_s_r(aci_no_obj,            aux(inf)).

t_s_r(fixed([vc(_,psp,_)|_],_),
                             aux(psp)).
t_s_r(fixed([vc(_,inf,_)|_],_),
                             aux(inf)).
t_s_r(fixed([vc(_,te,_)|_],_),
                             aux(te)).
t_s_r(fixed([vc(_,pass_te,_)|_],_),
                             aux(te)).

t_s_r(aux(psp(_)),           aux(psp)).
t_s_r(aux_psp_hebben,        aux(psp)).
t_s_r(aux_psp_zijn,          aux(psp)).
t_s_r(simple_aux_psp_zijn,   aux(psp)).
t_s_r(refl_passive,          aux(psp)).
t_s_r(obj1_passive,          aux(psp)).
t_s_r(sbar_passive,          aux(psp)).
t_s_r(so_passive,            aux(psp)).
t_s_r(norm_passive,          aux(psp)).
t_s_r(passive,               aux(psp)).

t_s_r(sbar_subj_te_passive,  aux(te)).
t_s_r(obj1_te_passive,       aux(te)).
t_s_r(te_passive,            aux(te)).
t_s_r(subj_control(te),      aux(te)).
t_s_r(subj_control(pass_te), aux(te)).
t_s_r(subj_control(wk_te),   aux(te)).

t_s_r(subj_control(te_inf),  aux(te_inf)).

t_s_r(aux(X),aux(X)).

t_s_r(inverted_aux(X),inverted_aux(X)).

adj_infl(aller_st(A),St) :-
    !,
    St = st(A).
adj_infl(X,X).