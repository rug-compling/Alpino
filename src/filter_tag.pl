:- module(alpino_filter_tag, [ filter_tag/4,
			       initialize_filter_tags/0
			     ]).

:- expects_dialect(sicstus).

:- use_module(library(lists)).
:- use_module(hdrug(hdrug_util)).

:- dynamic
    checked/2,
    checked_ok/2.

:- thread_local
    checked/2,
    checked_ok/2.

:- public check_fixed_part/3, is_sc_member/2, check_verb_sc/3, check_voor/2,
    check_op/2, check_vform_right/3, check_tpart/3, check_tpart/4,
    check_of_dat_whsub_sbar/2, check_of_dat_whsub_sbar/2, check_tmp_adv/2,
    check_stem/3, check_ap_pp_copula/2, check_nonp_copula/2,
    check_fixed_word/3, check_alsof/2,
    check_pp_copula/2, check_pp_copula/4, check_ap_copula/2, check_aci_simple/2,
    check_er/2, check_compar_adjective/2, check_subject_sbar/2, check_aux_simple/2,
    check_subject_vp/2,check_er_er/2, fail/2, check_of_dat_whsub_sbar_right/2,
    check_hebben/2, check_zijn/2, check_simple_zijn/2,
    check_part/4, check_part/3, check_ld_prep/3,
    check_ld_prep/2, check_refl/2, check_vform/3, check_hebben_adj/2,
    check_het/2, check_cleft_het/2, check_dir_adv/2, check_loc_adv/2,
    check_subject_pred_sbar/2, check_reduced_relative/2, check_of_dat_whsub_sbar0/2,
    check_of_dat_sbar/2, check_cleft_vform/3,
    check_reduced_relative0/2, check_np_adjective/2,
    check_subject_pred_vp/2, er_prep/3, pprep/3, prep/3, check_part_required/3,
    check_passive/2, check_te/2, check_adj_subject_pred_sbar/2,
    check_adj_subject_pred_vp/2, check_wat/2, check_pl_relative/2,
    check_selects_comparative/3, check_me_adj/2, check_fixed_dep/4,
    check_te_passive/2, check_naar_of_dat_whsub_sbar/2.

initialize_filter_tags :-
    retractall(checked(_,_)),
    retractall(checked_ok(_,_)).

%% done: if we check for certain contexts for tag at P0-P, then that
%% position itself should not be accessible for context requirements
filter_tag(Tag,Root,P0,P) :-
    (   filter_tag_rule(Tag,Root,Conditions)
    ->  apply_conditions(Conditions,P0,P)
    ;   format(user_error,"error: no filter_tag rule for tag ~w~n",[Tag])
    ).

apply_conditions([],_,_).
apply_conditions([H|T],P0,P) :-
    apply_condition(H,P0,P),
    !,
    apply_conditions(T,P0,P).

apply_condition(or(List),P0,P) :-
    !,
    apply_conditions_or(List,P0,P).
apply_condition(Call0,P0,P) :-
    Call0 =.. [Fun|Args0],
    append(Args0,[P0,P],Args),
    Call =.. [Fun|Args],
    once(Call).

apply_conditions_or([H|T],P0,P) :-
    (   apply_condition(H,P0,P)
    ->  true
    ;   apply_conditions_or(T,P0,P)
    ).

filter_tag_rule(verb(_A,B,Sc),Root,        Constraints) :-
    !,
    valid_sc(Sc,Root,B, Constraints).
filter_tag_rule(with_dt(Tag,_),Root,       Constraints) :-
    !,
    filter_tag_rule(Tag,Root,Constraints).
filter_tag_rule(Tag,_,Constraints) :-
    filter_tag_rule(Tag,Constraints).

filter_tag_rule(adjective(_,Sc),         Constraints) :-
    valid_adj_sc(Sc,Constraints).
filter_tag_rule(noun(_,_,_,Sc),          Constraints) :-
    valid_noun_sc(Sc,Constraints).
filter_tag_rule(tmp_noun(_,_,_,Sc),      Constraints) :-
    valid_noun_sc(Sc,Constraints).
filter_tag_rule(mod_noun(_,_,_,Sc),      Constraints) :-
    valid_noun_sc(Sc,Constraints).
filter_tag_rule(meas_mod_noun(_,_,_,Sc), Constraints) :-
    valid_noun_sc(Sc,Constraints).
filter_tag_rule(amount_meas_mod_noun(_,_,_,Sc), Constraints) :-
    valid_noun_sc(Sc,Constraints).

filter_tag_rule(skip,                  []).
filter_tag_rule('UNKNOWN',             []).
filter_tag_rule(longpunct,             []).
filter_tag_rule(within_word_conjunct,  []).
filter_tag_rule(wh_adjective,          []).
filter_tag_rule(wh_adjective(_),       []).
filter_tag_rule(wh_me_adjective,       []).
filter_tag_rule(postp_adverb,          []).
filter_tag_rule(hoe_adv,               [check_compar_adj]).
filter_tag_rule(number(_),             []).
filter_tag_rule(wh_number(_),          []).
filter_tag_rule(np,                    []).
filter_tag_rule(post_p(_),             []).
filter_tag_rule(np(_),                 []).
filter_tag_rule(tmp_np,                []).
filter_tag_rule(te_v_adj,              []).
filter_tag_rule(te_v_adj(Sc),          Cs) :- valid_adj_sc(Sc,Cs).
filter_tag_rule(comparative(Val),      [check_selects_comparative(Val)]).
filter_tag_rule(sbar_pred_adjective(_),[check_tag(complementizer(dat)),
					check_vform(fin)]).
filter_tag_rule(vp_pred_adjective(_),  [check_vform(te)]).
filter_tag_rule(nominalized_adjective, []).
filter_tag_rule(ge_nominalized_adjective, []).
filter_tag_rule(end_nominalized_adjective, []).
filter_tag_rule(nominalized_adjective(Sc), Cs) :- valid_adj_sc(Sc,Cs).
filter_tag_rule(ge_nominalized_adjective(Sc), Cs) :- valid_adj_sc(Sc,Cs).
filter_tag_rule(end_nominalized_adjective(Sc), Cs) :- valid_adj_sc(Sc,Cs).
filter_tag_rule(nominalized_compar_adjective, []).
filter_tag_rule(nominalized_compar_adjective_sg, []).
filter_tag_rule(nominalized_super_adjective, []).
filter_tag_rule(nominalized_super_adjective_sg, []).
filter_tag_rule(nominalized_adjective_sg, []).
filter_tag_rule(sbar_adjective(_),     [check_tag(complementizer(dat)),
					check_vform(fin)]).
filter_tag_rule(comp_pred_adjective,   []).
filter_tag_rule(clause_np_adjective,   []).
filter_tag_rule(clause_np_adjective(Sc),   Cs) :- valid_adj_sc(Sc,Cs).
filter_tag_rule(np_adjective,          []).
filter_tag_rule(np_adjective(Sc),     Cs) :- valid_adj_sc(Sc,Cs).
filter_tag_rule(het_np_adjective,     [check_het]).
filter_tag_rule(het_np_adjective(Sc), [check_het|Cs]) :- valid_adj_sc(Sc,Cs).
filter_tag_rule(np_me_adjective(_),    []).
filter_tag_rule(np_me_adjective(Sc,_),    Cs) :- valid_adj_sc(Sc,Cs).
filter_tag_rule(pred_np_me_adjective(_),           []).
filter_tag_rule(max,[]).
filter_tag_rule(subject_sbar_pred_np_me_adjective, [check_tag(complementizer(dat))]).
filter_tag_rule(subject_sbar_pred_np_adjective, [check_tag(complementizer(dat))]).
filter_tag_rule(subject_vp_pred_np_adjective,   [check_vform(te)]).
filter_tag_rule(tmp_determiner,        [check_tag(number(hoofd(_)))]).
filter_tag_rule(comp_number,           []).
filter_tag_rule(vandaar_adverb,        [check_tag(complementizer(dat))]).
filter_tag_rule(zo_van_adverb,         [check_tag(complementizer(van))]).
filter_tag_rule(loc_adverb,            []).
filter_tag_rule(er_loc_adverb,         []).
filter_tag_rule(pre_num_adv(_),        []).
filter_tag_rule(wh_loc_adverb,         []).
filter_tag_rule(er_wh_loc_adverb,         []).
filter_tag_rule(rwh_loc_adverb,        []).
filter_tag_rule(dip_sbar_adverb,       []).
filter_tag_rule(tmp_adverb,            []).
filter_tag_rule(wk_tmp_adverb,            []).
filter_tag_rule(wh_tmp_adverb,         []).
filter_tag_rule(wh_adverb,             []).
filter_tag_rule(er_adverb(_),          []).
filter_tag_rule(waar_adverb(_),        []).
filter_tag_rule(preposition(_,_,absolute),  []).
filter_tag_rule(preposition(_,_,me_adj),  [check_me_adj]). 
filter_tag_rule(preposition(_,_,adj),  []). 
filter_tag_rule(score_cat,             []).
filter_tag_rule(pre_np_adverb,         []).
filter_tag_rule(pp(_),                 []).
filter_tag_rule(pp,                    []).
filter_tag_rule(denk_ik,               []).
filter_tag_rule(denk_ik_dip,           []).
filter_tag_rule(etc,                   []).
filter_tag_rule(complex_etc,           []).
filter_tag_rule(postnp_adverb,         []).
filter_tag_rule(postn_adverb,          []).
filter_tag_rule(postpron_adverb,       []).
filter_tag_rule(post_wh_adverb,        []).
filter_tag_rule(pre_wh_adverb,         []).
filter_tag_rule(pre_np_adverb,         []).
filter_tag_rule(post_loc_adv_adv,      []).
filter_tag_rule(postadv_adverb,        []).
filter_tag_rule(postadj_adverb,        []).
filter_tag_rule(om_postadj_adverb,     []).
filter_tag_rule(predm_adverb,          []).
filter_tag_rule(eenmaal_adverb,          []).
filter_tag_rule(num_predm_adverb,      [check_tag(number(hoofd(_)))]).
filter_tag_rule(preposition(_,_,of_sbar),     [check_of_dat_whsub_sbar]).
filter_tag_rule(preposition(_,_,mod_sbar),     [or([check_tag(complementizer),
                                                   check_tag(complementizer(als))])]).
filter_tag_rule(preposition(_,_,redrel),      []).
filter_tag_rule(preposition(_,_,extracted_np),[]).
filter_tag_rule(preposition(_,_,pred),        []).
filter_tag_rule(preposition(_,_,nodet),       []).
filter_tag_rule(preposition(_,_,pp),          []).
filter_tag_rule(preposition(_,_,pc_adv),      []).
filter_tag_rule(preposition(_,_,pc_vp),      [check_vform(te)]).
filter_tag_rule(preposition(_,_),             []).
filter_tag_rule(rel_pronoun(_,_),      []).
filter_tag_rule(tmp_noun(_,_,_),       []).
filter_tag_rule(meas_mod_noun(_,_,_),  []).
filter_tag_rule(amount_meas_mod_noun(_,_,_),  []).
filter_tag_rule(mod_noun(_,_,_),       []).
filter_tag_rule(tmp_app_noun,          []).
filter_tag_rule(measure_noun(_),       []).
filter_tag_rule(meas_app_noun(_),      []).
filter_tag_rule(meas_tmp_noun(_,_,_),  []).
filter_tag_rule(modal_adverb,          []).
filter_tag_rule(modal_adverb(_),       []).
filter_tag_rule(conj(_),               []).
filter_tag_rule(reflexive(_,_),        []).
filter_tag_rule(v_noun(Sc),            Conds) :- valid_sc(Sc,Conds).
filter_tag_rule(ge_v_noun(Sc),         Conds) :- valid_sc(Sc,Conds).
filter_tag_rule(proper_name(_),        []).
filter_tag_rule(proper_name(_,_),      []).
filter_tag_rule(punct(_),              []).
filter_tag_rule(tag,                   []).
filter_tag_rule(noun(_,_,_),           []).
filter_tag_rule(post_n_n,              [check_tag(number(hoofd(_)))]).
filter_tag_rule(enumeration,           []).
filter_tag_rule(pronoun(_,_,_,_,_,_),  []).
filter_tag_rule(pronoun(_,_,_,_,_,_,_),[]).
filter_tag_rule(determiner(_),         []).
filter_tag_rule(determiner(_,_),       []).
filter_tag_rule(name_determiner(_),    []).
filter_tag_rule(name_determiner(_,_),  []).
filter_tag_rule(determiner(_,_,_),     []).
filter_tag_rule(determiner(_,_,_,_),   []).
filter_tag_rule(determiner(_,_,_,_,_), []).
filter_tag_rule(determiner(_,_,_,_,_,_), []).
filter_tag_rule(determiner(_,_,_,_,_,_,_), []).
filter_tag_rule(pre_det_quant(_),      []).
filter_tag_rule(gen_determiner(_),     []).
filter_tag_rule(number,                []).
filter_tag_rule(adj_number(_),         []).
filter_tag_rule(adverb,                []).
filter_tag_rule(sentence_adverb,       []).
filter_tag_rule(er_vp_adverb,          []).
filter_tag_rule(intensifier,           []).
filter_tag_rule(intensifier(_),        []).
filter_tag_rule(me_intensifier,        []).
filter_tag_rule(als_me_intensifier,    [check_tag(comparative(als))]).
filter_tag_rule(dir_adverb,            []).
filter_tag_rule(adjective(_),          []).
filter_tag_rule(me_adjective(_),        []).
filter_tag_rule(vp_om_me_adjective(_),  [check_tag(complementizer(om))]).
filter_tag_rule(het_noun,              []).
filter_tag_rule(cleft_het_noun,        [or([check_verb_sc(cleft),
					    check_verb_sc(simple_cleft)])]).
filter_tag_rule(particle(PART),        [check_part_required(PART)]).
filter_tag_rule(num_na,                []).
filter_tag_rule(complementizer,        []).
filter_tag_rule(complementizer(datti),  []).
filter_tag_rule(complementizer(start),  []).
filter_tag_rule(complementizer(root),   []).
filter_tag_rule(complementizer(zoals),  []).
filter_tag_rule(complementizer(als),    []).
filter_tag_rule(complementizer(pp),     []).
filter_tag_rule(complementizer(a),      []).
filter_tag_rule(complementizer(adv),    []).
filter_tag_rule(complementizer(np),     []).
filter_tag_rule(complementizer(naar),   [check_vform_right(fin)]).
filter_tag_rule(complementizer(al),     []).
filter_tag_rule(complementizer(van),    []).
filter_tag_rule(complementizer(vp),     []).
filter_tag_rule(complementizer(sbar),   []).
filter_tag_rule(sbar,                   []).
filter_tag_rule(dip_sbar,               []).
filter_tag_rule(complementizer(alsof),  []).
filter_tag_rule(comp_noun(_,_,_,dan),  [check_tag(comparative(dan))]).
filter_tag_rule(comp_determiner(_,als),[check_tag(comparative(als))]).
filter_tag_rule(comp_determiner(_,dat),[check_tag(complementizer(dat)),
					check_vform(fin)]).
filter_tag_rule(comp_determiner(_,om), [check_tag(complementizer(om))]).
filter_tag_rule(postnp_det_mod,        [check_tag(determiner(_,_,_,_,_,_,geen))]).
filter_tag_rule(als_adjective(_),      [check_tag(comparative(als))]).
filter_tag_rule(e_als_adjective(_),     []).
filter_tag_rule(complementizer(om),     [check_vform_right(te)]).
filter_tag_rule(complementizer(dat),    [check_vform_right(fin)]).
filter_tag_rule(complementizer(te),     [check_te]).
filter_tag_rule(complementizer(inf),    [check_vform_right(te)]).
filter_tag_rule(complementizer(of),     [check_vform_right(fin)]).
filter_tag_rule(complementizer(aan_het),[or([check_verb_sc(aan_het),
					     check_verb_sc(np_aan_het)]),
                                         check_vform_right(inf)]).
filter_tag_rule(complementizer(uit),    [or([check_verb_sc(uit),
					     check_verb_sc(np_uit)]),
                                         check_vform_right(inf)]).
filter_tag_rule(complementizer(op),     [check_op,
                                         check_vform_right(inf)]).
filter_tag_rule(vp_om_adverb,           [check_tag(complementizer(om))]).
filter_tag_rule(vp_om_intensifier,      [check_tag(complementizer(om))]).
filter_tag_rule(vp_om_me_intensifier,   [check_tag(complementizer(om))]).
filter_tag_rule(vp_adverb,              [check_vform(te)]).
filter_tag_rule(sbar_adverb,            [check_tag(complementizer(dat)),
					 check_vform(fin)]).
filter_tag_rule(om_postadj_adverb,      [check_tag(complementizer(om))]).
filter_tag_rule(zo_mogelijk_zo,         [check_tag(zo_mogelijk_mogelijk(_))]).
filter_tag_rule(zo_mogelijk_mogelijk(_),[check_tag(zo_mogelijk_zo)]).
filter_tag_rule(comp_adverb(als),       [check_tag(comparative(als))]).
filter_tag_rule(comp_adverb(dan),       [check_tag(comparative(dan))]).
filter_tag_rule(comp_adverb(e_als),     []).
filter_tag_rule(left_conj(A),           [or([check_tag(right_conj(A)),
					     check_tag(conj(A))])]).
filter_tag_rule(right_conj(A),          [check_tag(left_conj(A))]).
filter_tag_rule(preposition(_,_,sbar),  [check_tag(complementizer(dat)),
					 check_vform(fin)]).
filter_tag_rule(preposition(_,_,loc_adv),[check_loc_adv]).
filter_tag_rule(preposition(_,[],voor_pred),[check_voor]).
filter_tag_rule(preposition(_,_,tmp_adv),[check_tmp_adv]).
filter_tag_rule(iets_noun,            [or([check_tag(post_adjective(_)),
					   check_tag(post_adjective(_,_)),
					   check_vform(te)])]).
filter_tag_rule(wh_iets_noun,         [or([check_tag(post_adjective(_)),
					   check_tag(post_adjective(_,_))])]).
filter_tag_rule(iets_anders_noun,     [check_tag(post_adjective_anders(er))]).
filter_tag_rule(wh_iets_anders_noun,  [check_tag(post_adjective_anders(er))]).
filter_tag_rule(post_adjective(_),    [or([check_tag(iets_noun),
					   check_tag(wh_iets_noun)])]).
filter_tag_rule(post_adjective(_,_),  [or([check_tag(iets_noun),
					   check_tag(wh_iets_noun)])]).
filter_tag_rule(post_adjective_anders(er),
		[or([check_tag(iets_anders_noun),
		     check_tag(wh_iets_anders_noun),
		     check_tag(iets_adverb),
		     check_tag(wh_iets_adverb)])]).

filter_tag_rule(iets_adverb,          [check_tag(post_adjective_anders(er))]).
filter_tag_rule(wh_iets_adverb,       [check_tag(post_adjective_anders(er))]).
filter_tag_rule(fixed_part(Ws),       [check_fixed_part(Ws)]).

valid_adj_sc(als_pred,                [check_tag(complementizer(als))]).
valid_adj_sc(ld_pp,                   [check_ld_prep]).
valid_adj_sc(np_ld_pp,                   [check_ld_prep]).
valid_adj_sc(ld_pp(Prep),             [prep(Prep)]).
valid_adj_sc(pp(Prep),                [prep(Prep)]).
valid_adj_sc(mod_pp(Prep),            [prep(Prep)]).
valid_adj_sc(so_pp(Prep),             [prep(Prep)]).
valid_adj_sc(ld,                      []).
valid_adj_sc(np_ld,                   []).
valid_adj_sc(refl_ld,                 [check_refl]).
valid_adj_sc(er_pp_vp(Prep),          [check_vform(te),er_prep(Prep)]).
valid_adj_sc(er_pp_sbar(Prep),        [er_prep(Prep),check_of_dat_whsub_sbar_right]).
valid_adj_sc(refl,                    [check_refl]).
valid_adj_sc(refl_ld_pp,              [check_refl,check_ld_prep]).
valid_adj_sc(refl_np,                 [check_refl]).
valid_adj_sc(refl_pp(Prep),           [check_refl,prep(Prep)]).
valid_adj_sc(refl_er_pp_vp(Prep),     [check_refl,er_prep(Prep),
                                       check_vform(te)]).
valid_adj_sc(refl_er_pp_sbar(Prep),   [check_refl,er_prep(Prep),
                                       check_of_dat_whsub_sbar_right]).
valid_adj_sc(refl_vp,                 [check_refl,check_vform(te)]).
valid_adj_sc(refl_sbar,               [check_refl]).
valid_adj_sc(refl_dip_sbar,           [check_refl]).
valid_adj_sc(subject_vp,              [check_vform(te)]). % no copula required, because of pred_vp_dp rule
valid_adj_sc(subject_vp_no_het,       [check_vform(te)]). % no copula required: pred_vp_dp rule
valid_adj_sc(subject_vp_sbar,         [check_vform(te)]).
valid_adj_sc(subject_vp_sbar_no_het,  [check_vform(te)]).
valid_adj_sc(object_vp,               [check_vform(te)]).
valid_adj_sc(tr_object_vp,            [check_vform(te)]).
valid_adj_sc(object_sbar,             [check_of_dat_whsub_sbar_right]).
valid_adj_sc(van_sbar,                [check_tag(complementizer(van))]).
valid_adj_sc(subject_sbar,            [check_of_dat_whsub_sbar]). % no copula required: pred_sbar_dp rule
valid_adj_sc(subject_sbar_no_het,     [or([check_tag(dip_sbar_adverb),
					   check_of_dat_whsub_sbar
					  ])
				      ]).
valid_adj_sc(pp_subject_sbar(Prep),   [prep(Prep),check_of_dat_whsub_sbar]).
valid_adj_sc(pp_subject_sbar_no_het(Prep),
	                              [prep(Prep),
	                               or([check_tag(dip_sbar_adverb),
			                   check_of_dat_whsub_sbar
			              ])]).
valid_adj_sc(so_pp_subject_sbar(Prep),[prep(Prep),check_of_dat_whsub_sbar]).
valid_adj_sc(so_pp_subject_sbar_no_het(Prep),
                                      [prep(Prep),
                                       or([check_tag(dip_sbar_adverb),
                                           check_of_dat_whsub_sbar])]).
valid_adj_sc(so_np_subject_sbar,      [check_of_dat_whsub_sbar]).
valid_adj_sc(so_np_subject_vp,        [check_vform(te)]).
valid_adj_sc(transitive,              []).
valid_adj_sc(ld_transitive,           []).
valid_adj_sc(np_ld_transitive,        []).
valid_adj_sc(refl_ld_transitive,      [check_refl]).
valid_adj_sc(so_np,                   []).
valid_adj_sc(np_np,                   []).
valid_adj_sc(pred_so_np,              []).
valid_adj_sc(pred_np,                 []).
valid_adj_sc(pred_refl,               [check_refl]).
valid_adj_sc(refl,                    [check_refl]).
valid_adj_sc(refl_np,                 [check_refl]).
valid_adj_sc(aux_psp_hebben,          [check_hebben_adj]).
valid_adj_sc(fixed(Words),            Conditions) :-
    check_fixed(Words,Conditions,[]).
valid_adj_sc(part(Part),              [check_part(Part)]).
valid_adj_sc(part(Part,Frame),        [check_part(Part)|Cs]) :-
    valid_adj_sc(Frame,Cs).
valid_adj_sc(pred,                    []).
valid_adj_sc(ap_pred,                 []).
valid_adj_sc(ap_pred(Stem),           [check_stem(Stem)]).
valid_adj_sc(pp_pred,                 [check_pp_copula]).
valid_adj_sc(pp_pred(Aan,Slag),       [check_pp_copula(Aan,Slag)]).
valid_adj_sc(nonp_pred,               []).

valid_noun_sc(pp(_Prep),              [fail]).
valid_noun_sc(pp_pp(_,_),             [fail]).
valid_noun_sc(vp,                     [check_vform(te)]).
valid_noun_sc(tr_vp,                  [check_vform(te)]).
valid_noun_sc(pred_pp(Prep),          [prep(Prep)]).
valid_noun_sc(pred_pp(Prep,SubType),  [prep(Prep)|Conds]) :-
    valid_noun_sc(SubType,Conds).
valid_noun_sc(pred_pp_pl(Prep),       [prep(Prep)]).
valid_noun_sc(pred_pp_pl(Prep,Sub),   [prep(Prep)|Conds]) :-
    valid_noun_sc(Sub,Conds).
valid_noun_sc(subject_vp,             [or([check_verb_sc(copula_vp),
					   check_verb_sc(so_copula_vp),
					   check_verb_sc(so_nonp_copula_vp),
					   check_verb_sc(pp_copula_vp),
					   check_verb_sc(pp_copula_vp(_,_)),
					   check_verb_sc(pred_np_vp),
					   check_verb_sc(pp_pred_np_vp),
					   check_verb_sc(pp_pred_np_vp(_,_)),
					   check_verb_sc(als_pred_np_vp),
					   check_verb_sc(voor_pred_np_vp)]),
				       check_vform(te)]).
valid_noun_sc(sbar,                   [check_of_dat_whsub_sbar]).
valid_noun_sc(van_sbar,               [check_tag(complementizer(van))]).
valid_noun_sc(subject_sbar,           []).  % ik vertrek , was zijn eerste reactie 
valid_noun_sc(subject_sbar_no_het,    []).  % ik vertrek , was zijn eerste reactie
valid_noun_sc(measure,                []).
valid_noun_sc(np_measure,             []).
valid_noun_sc(app_measure,            []).
valid_noun_sc(start_app_measure,      []).
valid_noun_sc(np_app_measure,         []).
valid_noun_sc(tmp_app_measure,        []).

valid_sc(fixed_dep(Frame), Root, Infl,
         [check_fixed_part(vc(Root,_,Frame))|Checks]):-
    !,
    valid_sc(Frame,Root,Infl,Checks).
valid_sc(ninv(_,part_fixed_dep(Part,Frame)), Root, Infl,
         [check_fixed_part(vc(Root,_,Frame2))|Checks]):-
    !,
    Frame =.. [F|Args],
    atom_concat(part_,F,F2),
    Frame2 =.. [F2,Part|Args],
    valid_sc(Frame,Root,Infl,Checks).
valid_sc(part_fixed_dep(Part,Frame), Root, Infl,
         [check_fixed_part(vc(Root,_,Frame2))|Checks]):-
    !,
    Frame =.. [F|Args],
    atom_concat(part_,F,F2),
    Frame2 =.. [F2,Part|Args],
    valid_sc(Frame2,Root,Infl,Checks).
valid_sc(ninv(Frame,_),Root,sg1,[check_stem(ik)|Conds]) :-
    \+ Frame = incorporated_subj_topic(_),
    !,
    valid_sc(Frame,Root,sg1,Conds).
valid_sc(ninv(Frame,_),Root,Inf,Conds) :-
    !,
    valid_sc(Frame,Root,Inf,Conds).
valid_sc(Sc,_,B,Constraints) :-
    valid_sc(Sc,B,Constraints).

valid_sc(part_sbar_subj_no_het_tpart(Part),_,[check_tpart(Part)|Conds]) :-
    !,
    valid_sc(sbar_subj_no_het_tpart,Conds).

valid_sc(Term,Vform,[check_part(Vform,Part)|Conds]) :-
    Term =.. [Func,Part|Args],
    atom_concat(part_,NewFunc,Func),
    !,
    NewTerm =.. [NewFunc|Args],
    valid_sc(NewTerm,Conds).

valid_sc(aux(wk_te),         inf,         [check_vform(inf)]). % "hij heeft lopen klieren"
valid_sc(subj_control(wk_te),inf,         [check_vform(inf)]). % "hij heeft durven klieren"
valid_sc(aux(wk_te),         inf(no_e),   [check_vform(inf)]). % "hij heeft staan klieren"
valid_sc(subj_control(wk_te),inf(no_e),   [check_vform(inf)]). %
valid_sc(cleft,Vform,                     [check_cleft_het,
					   check_cleft_vform(Vform)]).
valid_sc(simple_cleft,Vform,              [check_cleft_het,
					   check_cleft_vform(Vform)]).
valid_sc(cleft_np,Vform,                  [check_cleft_het,
					   check_cleft_vform(Vform)]).


valid_sc(Term,_Vform,Conds) :-
    valid_sc(Term,Conds).

valid_sc(part_sbar_subj_no_het_tpart(Part),[check_tpart(Part)|Conds]) :-
    !,
    valid_sc(sbar_subj_no_het_tpart,Conds).

valid_sc(Term,[check_part(Part)|Conds]) :-
    Term =.. [Func,Part|Args],
    atom_concat(part_,NewFunc,Func),
    !,
    NewTerm =.. [NewFunc|Args],
    valid_sc(NewTerm,Conds).

valid_sc(intransitive,                []).
valid_sc(transitive,                  []).
valid_sc(transitive_ndev,             []).
valid_sc(transitive_ndev_ndev,        []).
valid_sc(transitive_ydev,             [check_tag(v_noun(_))]).
valid_sc(num_pred,                    [check_tag(number(hoofd(_)))]).
valid_sc(ld_transitive,               []).
valid_sc(ld_er_transitive,            [or([check_tag(er_loc_adverb),
                                           check_tag(er_vp_adverb),
                                           check_tag(iets_adverb),
                                           check_tag(wh_iets_adverb),
					   check_tag(er_wh_loc_adverb)])]).
valid_sc(pc_er_transitive,            [or([check_tag(er_loc_adverb),
                                           check_tag(er_vp_adverb),
                                           check_tag(iets_adverb),
                                           check_tag(wh_iets_adverb),
					   check_tag(er_wh_loc_adverb)])]).
valid_sc(np_pc_er_transitive,         [or([check_tag(er_loc_adverb),
                                           check_tag(er_vp_adverb),
                                           check_tag(iets_adverb),
                                           check_tag(wh_iets_adverb),
					   check_tag(er_wh_loc_adverb)])]).
valid_sc(svp_er_transitive,           [or([check_tag(er_loc_adverb),
                                           check_tag(er_vp_adverb),
                                           check_tag(iets_adverb),
                                           check_tag(wh_iets_adverb),
					   check_tag(er_wh_loc_adverb)])]).
valid_sc(np_ld_er_transitive,         [or([check_tag(er_loc_adverb),
                                           check_tag(er_vp_adverb),
                                           check_tag(iets_adverb),
                                           check_tag(wh_iets_adverb),
					   check_tag(er_wh_loc_adverb)])]).
valid_sc(me_ld_transitive,            []).
valid_sc(meas,                        []).
valid_sc(adv_meas,                    []).  % TODO
valid_sc(so_adv_meas,                 []).  % TODO
valid_sc(pp_meas(Prep),               [prep(Prep)]).
% valid_sc(ld_pp_meas(Prep),            [check_ld_prep(Prep)]).
valid_sc(ld_pp_meas,                  [check_ld_prep]).
valid_sc(so_meas,                     []).
valid_sc(so_pp_meas,                  [prep(aan)]).
valid_sc(so_pp_refl,                  [check_refl,prep(aan)]).
valid_sc(np_meas,                     []).
valid_sc(refl,                        [check_refl]).
valid_sc(np_pc_pp(Prep),              [prep(Prep)]).
valid_sc(refl_np_pc_pp(Prep),         [check_refl,
                                       prep(Prep)]).
valid_sc(meas_pc_pp(Prep),            [prep(Prep)]).
valid_sc(adv_meas_pc_pp(Prep),        [prep(Prep)]).
valid_sc(np_np_pc_pp(Prep),           [prep(Prep)]).
valid_sc(so_np_pc_pp(Prep),           [prep(Prep)]).
valid_sc(amb_so_np_pass_pc_pp(Prep),  [prep(Prep)]).
valid_sc(np_er_pc_pp(Prep),           [er_prep(Prep)]).
valid_sc(np_mod_pp(Prep),             [prep(Prep)]).
valid_sc(np_np_mod_pp(Prep),          [prep(Prep)]).
valid_sc(refl_er_pc_pp(Prep),         [check_refl,er_prep(Prep)]).
valid_sc(refl_pc_pp(Prep),            [prep(Prep),check_refl]).
valid_sc(np_refl_pc_pp(Prep),         [prep(Prep),check_refl]).
valid_sc(refl_aan_pc_pp,              [prep(aan),check_refl]).
valid_sc(mod_pp(Prep),                [prep(Prep)]).
valid_sc(pc_pp(Prep),                 [prep(Prep)]).
valid_sc(er_pc_pp(Prep),              [er_prep(Prep)]).
valid_sc(pc_pp_refl(Prep),            [prep(Prep),check_refl]).
valid_sc(np_pc_pp_refl(Prep),         [prep(Prep),check_refl]).
valid_sc(np_np,                       []).
valid_sc(refl_np,                     [check_refl]).
valid_sc(copula,                      []).
valid_sc(incorporated_subj_topic(Frame), Checks) :-
    valid_sc(Frame,Checks).
valid_sc(incorporated_subj(Frame), Checks) :-
    valid_sc(Frame,Checks).
valid_sc(als_copula,                  [check_tag(complementizer(als))]).
valid_sc(ap_copula,                   [check_ap_copula]).
valid_sc(ap_copula(Stem),             [check_stem(Stem),check_ap_copula]).
valid_sc(pp_copula,                   [check_pp_copula]).
valid_sc(pp_copula(Aan,Slag),         [check_pp_copula(Aan,Slag)]).
valid_sc(so_nonp_copula,              [check_nonp_copula]).
valid_sc(nonp_copula,                 [check_nonp_copula]).
valid_sc(copula_vp,                   [check_subject_vp,check_vform(te)]).
valid_sc(copula_sbar,                 [check_subject_sbar]).
valid_sc(pp_copula_sbar,              [check_subject_sbar,check_pp_copula]).
valid_sc(pp_copula_vp,                [check_subject_vp,check_vform(te),check_pp_copula]).
valid_sc(pp_copula_sbar(Aan,Slag),    [check_subject_sbar,check_pp_copula(Aan,Slag)]).
valid_sc(pp_copula_vp(Aan,Slag),      [check_subject_vp,check_vform(te),check_pp_copula(Aan,Slag)]).
valid_sc(nonp_copula_vp,              [check_subject_vp,check_vform(te),
                                       check_nonp_copula]).
valid_sc(nonp_copula_sbar,            [check_subject_sbar,
                                       check_nonp_copula]).
valid_sc(ap_copula_sbar,              [check_subject_sbar,
                                       check_ap_copula]).
valid_sc(nonp_copula_np,              [check_np_adjective]).
valid_sc(copula_np,                   [check_np_adjective]).
valid_sc(so_copula_np,                [check_np_adjective]).
valid_sc(so_copula,                   []).
valid_sc(so_copula_vp,                [check_subject_vp,check_vform(te)]).
valid_sc(so_copula_sbar,              [check_subject_sbar]).
valid_sc(so_nonp_copula_vp,           [check_vform(te),check_subject_vp,
                                       check_nonp_copula]).
valid_sc(so_nonp_copula_sbar,         [check_subject_sbar,check_nonp_copula]).
valid_sc(pred_np,                     []).
valid_sc(nonp_pred_np,                [check_nonp_copula]).
valid_sc(nonp_pred_np_ndev,           [check_nonp_copula]).
valid_sc(np_pred_np,                  []).
valid_sc(ap_pred_np,                  [check_ap_copula]).
valid_sc(pp_pred_np,                  [check_pp_copula]).
valid_sc(pp_pred_np(Aan,Slag),        [check_pp_copula(Aan,Slag)]).
valid_sc(als_pred_np,                 [check_tag(complementizer(als))]).
valid_sc(voor_pred_np,                [prep(voor)]).
valid_sc(pred_np_vp,                  [check_subject_pred_vp]).
valid_sc(pp_pred_np_vp,               [check_subject_pred_vp,check_pp_copula]).
valid_sc(pp_pred_np_vp(Aan,Slag),     [check_subject_pred_vp,check_pp_copula(Aan,Slag)]).
valid_sc(als_pred_np_vp,              [check_tag(complementizer(als)),
				       check_subject_pred_vp]).
valid_sc(voor_pred_np_vp,             [prep(voor),
				       check_subject_pred_vp]).
valid_sc(so_pred_np,                  [check_ap_pp_copula]).
valid_sc(pp_so_pred_np,               [prep(aan),check_ap_pp_copula]).
valid_sc(so_pred_vp,                  [check_adj_subject_pred_vp]).
valid_sc(so_pred_sbar,                [check_adj_subject_pred_sbar]).
valid_sc(pp_so_pred_vp,               [prep(aan),check_adj_subject_pred_vp]).
valid_sc(pp_so_pred_sbar,             [prep(aan),check_adj_subject_pred_sbar]).
valid_sc(pred_np_sbar,                [check_subject_pred_sbar]).
valid_sc(pp_pred_np_sbar,             [check_subject_pred_sbar,check_pp_copula]).
valid_sc(pp_pred_np_sbar(Aan,Slag),   [check_subject_pred_sbar,check_pp_copula(Aan,Slag)]).
valid_sc(als_pred_np_sbar,            [check_tag(complementizer(als)),
				       check_subject_pred_sbar]).
valid_sc(voor_pred_np_sbar,           [prep(voor),check_subject_pred_sbar]).
valid_sc(pred_refl,                   [check_refl]).
valid_sc(np_pred_refl,                [check_refl]).
valid_sc(nonp_pred_refl,              [check_refl,check_nonp_copula]).
valid_sc(ap_pred_refl,                [check_refl,check_ap_copula]).
valid_sc(als_pred_refl,               [check_refl,
                                       check_tag(complementizer(als))]).
valid_sc(refl_vp,                     [check_refl,check_vform(te)]).
valid_sc(vp,                          [check_vform(te)]).
valid_sc(tr_vp,                       [check_vform(te)]).
valid_sc(vp_no_control,               [check_vform(te)]).
valid_sc(pp_vp(Prep),                 [prep(Prep),check_vform(te)]).
valid_sc(so_pp_vp,                    [prep(aan),check_vform(te)]).
valid_sc(np_vp_subj,                  [check_vform(te)]).
valid_sc(so_vp_subj,                  [prep(aan),check_vform(te)]).
valid_sc(so_vp_no_control,            [prep(aan),check_vform(te)]).
valid_sc(so_vp_obj,                   [prep(aan),check_vform(te)]).
valid_sc(np_vp_no_control,            [check_vform(te)]).
valid_sc(np_vp_obj,                   [check_vform(te)]).
valid_sc(np_vp_obj1,                  [check_vform(te)]).
valid_sc(dip_sbar,                    []).                
valid_sc(sbar,                        []).                
valid_sc(alsof_sbar,                  [check_alsof]).
valid_sc(tr_sbar,                     []).  
valid_sc(van_sbar,                    [check_tag(complementizer(van))]).
valid_sc(so_van_sbar,                 [check_tag(complementizer(van)),
				       prep(aan)]).
valid_sc(so_pp_sbar,                  [prep(aan)]).
valid_sc(np_sbar,                     []).
valid_sc(np_alsof_sbar,               [check_alsof]).
valid_sc(acc_np_sbar,                 []). 
valid_sc(acc_np_dip_sbar,             []). 
valid_sc(pp_sbar(Prep),               [prep(Prep)]).
valid_sc(ld_pp_sbar,               [check_ld_prep]).
valid_sc(pp_dip_sbar(Prep),           [prep(Prep)]).
valid_sc(pp_refl_sbar(Prep),          [check_refl,prep(Prep)]).
valid_sc(refl_sbar,                   [check_refl]).
valid_sc(refl_dip_sbar,               [check_refl]).
valid_sc(sbar_subj_het,               [check_het,
                                       check_of_dat_sbar]).
valid_sc(sbar_sbar_subj,              [check_of_dat_whsub_sbar]).
valid_sc(sbar_subj,                   [check_of_dat_whsub_sbar]).
valid_sc(sbar_subj_opt_het,           [check_of_dat_whsub_sbar]).
valid_sc(dip_sbar_subj,               [check_het]).
valid_sc(alsof_sbar_subj,             [check_alsof]).
valid_sc(alsof_sbar_subj_so_np,       [check_alsof]).
valid_sc(sbar_subj_meas,              [check_of_dat_whsub_sbar]).
valid_sc(sbar_subj_dat_meas,          [check_of_dat_whsub_sbar]).
valid_sc(sbar_obj,                    [check_of_dat_sbar]).
valid_sc(sbar_obj_no_het,             [check_of_dat_sbar]).
valid_sc(sbar_obj_opt_het,            [check_of_dat_sbar]).
valid_sc(so_pp_sbar_obj,              [prep(aan),check_of_dat_sbar]).
valid_sc(so_np_sbar_obj,              [check_of_dat_sbar]).
valid_sc(pp_sbar_obj(Prep),           [prep(Prep),check_of_dat_sbar]).
valid_sc(refl_pp_vp_obj(Prep),        [prep(Prep),check_refl,check_vform(te)]).
valid_sc(pp_vp_obj(Prep),             [prep(Prep),check_vform(te)]).
valid_sc(sbar_subj_no_het,            [check_of_dat_whsub_sbar]).
valid_sc(er_sbar_subj_no_het,         [check_of_dat_whsub_sbar,check_er]).
valid_sc(dip_sbar_subj_no_het,        []).
valid_sc(dip_sbar_subj_opt_het,       []).
valid_sc(sbar_subj_no_het_tpart,      [check_of_dat_whsub_sbar]).
valid_sc(tpart,                       []).
valid_sc(van_sbar_subj_no_het,        [check_tag(complementizer(van))]).
valid_sc(van_sbar_subj_so_np_no_het,  [check_tag(complementizer(van))]).
valid_sc(pp_sbar_subj(Prep),          [prep(Prep),check_of_dat_whsub_sbar]). 
valid_sc(pp_sbar_subj_no_het(Prep),   [prep(Prep)]). 
valid_sc(pp_sbar_subj_opt_het(Prep),  [prep(Prep),check_of_dat_whsub_sbar]). 
valid_sc(ld_pp_sbar_subj_no_het,      [check_ld_prep,check_of_dat_whsub_sbar]).
valid_sc(ld_adv_sbar_subj_no_het,     [check_loc_adv,check_of_dat_whsub_sbar]).
valid_sc(sbar_subj_so_np,             [check_of_dat_whsub_sbar]).
valid_sc(sbar_subj_so_pp,             [prep(aan),check_of_dat_whsub_sbar]).
valid_sc(sbar_subj_so_np_opt_het,     [check_of_dat_whsub_sbar]).
valid_sc(sbar_subj_so_np_no_het,      [check_of_dat_whsub_sbar]).
valid_sc(dip_sbar_subj_so_np,         []).
valid_sc(dip_sbar_subj_so_np_no_het,  []).
valid_sc(dip_sbar_subj_so_np_opt_het, []).
valid_sc(sbar_subj_refl_no_het,       [check_refl,check_of_dat_whsub_sbar]).
valid_sc(sbar_subj_refl_opt_het,      [check_refl,check_of_dat_whsub_sbar]).
valid_sc(sbar_subj_np,                [check_of_dat_whsub_sbar]).
valid_sc(sbar_subj_np_np,             [check_of_dat_whsub_sbar]).
valid_sc(sbar_subj_np_no_het,         [check_of_dat_whsub_sbar]).
valid_sc(sbar_subj_adv_meas,          [check_of_dat_whsub_sbar]).
valid_sc(vp_subj,                     [check_vform(te)]).
valid_sc(vp_subj_meas,                [check_vform(te)]).
valid_sc(vp_subj_adv_meas,            [check_vform(te)]).
valid_sc(vp_subj_dat_meas,            [check_vform(te)]).
valid_sc(vp_subj_no_het,              [check_vform(te)]).
valid_sc(pp_vp_subj(Prep),            [prep(Prep), check_vform(te)]).
valid_sc(pp_vp_subj_no_het(Prep),     [prep(Prep), check_vform(te)]).
valid_sc(vp_subj_so_np,               [check_vform(te)]).
valid_sc(vp_subj_so_pp,               [prep(aan),  check_vform(te)]).
valid_sc(vp_subj_so_np_no_het,        [check_vform(te)]).
valid_sc(vp_subj_np,                  [check_vform(te)]).
valid_sc(vp_subj_np_np,               [check_vform(te)]).
valid_sc(vp_subj_np_no_het,           [check_vform(te)]).
valid_sc(vp_obj,                      [check_vform(te)]).
valid_sc(so_np_vp_obj,                [check_vform(te)]).
valid_sc(aux(Vform),                  [check_vform(Vform)]).
valid_sc(modifier(X),                 Cs):-
    valid_sc(X,Cs).
valid_sc(aux_simple(inf),             [check_aux_simple]).
valid_sc(inverted_aux(Vform),         [check_vform(Vform)]).
valid_sc(so_aux(Vform),               [check_vform(Vform)]).
valid_sc(subj_control(Vform),         [check_vform(Vform)]).
valid_sc(obj_control(Vform),          [check_vform(Vform)]).
valid_sc(so_control(Vform),           [check_vform(Vform)]).
valid_sc(aux_psp_hebben,              [check_hebben]).
valid_sc(aux_psp_zijn,                [check_zijn]).
valid_sc(simple_aux_psp_zijn,         [check_simple_zijn]).
valid_sc(te_passive,                  [check_te_passive]).
valid_sc(dat_te_passive,              [check_te_passive]).
valid_sc(sbar_subj_te_passive,        [check_vform(te)]).
valid_sc(passive,                     [check_passive]).
valid_sc(sbar_passive,                [check_passive]).
valid_sc(norm_passive,                [check_passive]).
valid_sc(refl_passive,                [check_passive]).
valid_sc(so_passive,                  [check_passive]).
valid_sc(dat_passive,                 [check_passive]).
valid_sc(obj1_passive,                [check_passive]).
valid_sc(obj1_te_passive,             [check_vform(te)]).
valid_sc(op,                          [check_tag(complementizer(op))]).
valid_sc(aan_het,                     [check_tag(complementizer(aan_het))]).
valid_sc(np_aan_het,                  [check_tag(complementizer(aan_het))]).
valid_sc(uit,                         [check_tag(complementizer(uit))]).
valid_sc(np_uit,                      [check_tag(complementizer(uit))]).
valid_sc(aci,                         [check_vform(inf)]).
valid_sc(aci_simple,                  [check_aci_simple]).
valid_sc(aci_no_obj,                  [check_vform(inf)]).
valid_sc(so_np,                       []).
valid_sc(so_np_pass,                  []).
valid_sc(amb_so_np_pass,              []).
valid_sc(so_pp_np,                    [prep(aan)]).
valid_sc(so_pp_np(PREP),              [prep(PREP)]).
valid_sc(so_pp,                       [prep(aan)]).
valid_sc(so_pp(PREP),                 [prep(PREP)]).
valid_sc(het_subj,                    [check_het]).
valid_sc(het_subj_sbar_obcomp,        [check_het,check_tag(complementizer(dat))]).
valid_sc(no_subj,                     []).
valid_sc(er_er,                       [check_er_er,
                                       check_pl_relative]).
valid_sc(obj_er_er,                   [check_er_er,
				       check_tag(pronoun(nwh,je,sg,de,both,def,wkpro)),
                                       check_pl_relative]).
valid_sc(ld_pp(Prep),                 [check_ld_prep(Prep)]).
valid_sc(ld_pp,                       [check_ld_prep]).
valid_sc(so_np_ld_pp,                  [check_ld_prep]).
valid_sc(ld_adv,                      [check_loc_adv]).
valid_sc(refl_ld_adv,                 [check_refl,check_loc_adv]).
valid_sc(ld_dir,                      [check_dir_adv]).
valid_sc(me_ld_dir,                   [check_dir_adv]).
valid_sc(np_ld_dir,                   [check_dir_adv]).
valid_sc(np_ld_transitive,            []).
valid_sc(np_ld_adv,                   [check_loc_adv]).
valid_sc(np_np_ld_adv,                [check_loc_adv]).
valid_sc(refl_ld_dir,                 [check_refl,check_dir_adv]).
valid_sc(refl_ld_transitive,          [check_refl]).
valid_sc(np_ld_pp(Prep),              [check_ld_prep(Prep)]).
valid_sc(np_ld_pp,                    [check_ld_prep]).
valid_sc(np_np_ld_pp,                 [check_ld_prep]).
valid_sc(refl_np_ld_pp,               [check_refl,check_ld_prep]).
valid_sc(refl_ld_pp(Prep),            [check_refl,check_ld_prep(Prep)]).
valid_sc(refl_ld_pp,                  [check_ld_prep,check_refl]).
valid_sc(het_oti_transitive,          [check_het,check_vform(te)]).
valid_sc(er_sbar,                     [check_er,
                                       check_of_dat_whsub_sbar]).
valid_sc(er_vp,                       [check_vform(te),
				       check_er]).
valid_sc(bare_alsof_pred,             [prep(alsof)]).
valid_sc(pred_pc_pp(Prep),            [prep(Prep)]).
valid_sc(su_ap_pred_sbar,             [check_ap_copula,check_of_dat_whsub_sbar]).
valid_sc(su_ap_pred_vp,               [check_ap_copula,check_vform(te)]).
%% next: er not really always there
%% Nooit op gezinspeeld om naar Stuttgart te gaan ?
valid_sc(er_pp_vp(Prep),              [pprep(Prep),check_vform(te)]).
valid_sc(er_pp_vp_no_control(Prep),   [pprep(Prep),check_vform(te)]).
valid_sc(np_er_pp_vp(Prep),           [pprep(Prep),check_vform(te)]).
valid_sc(meas_er_pp_vp(Prep),         [pprep(Prep),check_vform(te)]).
valid_sc(adv_meas_er_pp_vp(Prep),     [pprep(Prep),check_vform(te)]).
valid_sc(obj_np_er_pp_vp(Prep),       [pprep(Prep),check_vform(te)]).
valid_sc(refl_er_pp_vp(Prep),         [pprep(Prep),check_vform(te)]).
valid_sc(er_pp_sbar(Prep),            [pprep(Prep),
                                       check_of_dat_whsub_sbar_right]).
valid_sc(np_er_pp_sbar(Prep),         [pprep(Prep),
                                       check_of_dat_whsub_sbar_right]).
valid_sc(refl_er_pp_sbar(Prep),       [pprep(Prep),
                                       check_of_dat_whsub_sbar_right]).
valid_sc(pred_er_pp_vp(Prep),         [pprep(Prep),check_vform(te)]).
valid_sc(pred_er_pp_sbar(Prep),       [pprep(Prep),
                                       check_of_dat_whsub_sbar_right]).
valid_sc(fixed(Fixed,_),              Conditions) :-
    check_fixed(Fixed,Conditions,[]).

check_fixed([],C,C).
check_fixed([H|T],C0,C) :-
    check_fixed_el(H,C0,C1),
    check_fixed(T,C1,C).

check_fixed_el([H|T],                 [check_fixed_word([H|T])|Cs],Cs).
check_fixed_el(op_een_v,              [check_tag(fixed_part(op_een_v))|Cs],Cs).
check_fixed_el(refl,                  [check_refl|Cs],Cs).
check_fixed_el(acc,                   Cs,Cs).
check_fixed_el(inv(X),Cs0,Cs) :-
    check_fixed_el(X,Cs0,Cs).
check_fixed_el(me,                    Cs,Cs).
check_fixed_el(dat,                   Cs,Cs).
check_fixed_el(pred,                  Cs,Cs).
check_fixed_el(np_pred,               Cs,Cs).
check_fixed_el(nonp_pred,             [check_nonp_copula|Cs],Cs).
check_fixed_el(no_subj,               Cs,Cs).
check_fixed_el(als_pred,              [check_tag(complementizer(als))|Cs],Cs).
check_fixed_el(voor_pred(A),          [check_tag(preposition(_,_,voor_pred)),
				       check_stem(A)|Cs],Cs).
check_fixed_el(svp_pp(V,R),           [prep(V),check_stem(R)|Cs],Cs).
check_fixed_el(ap_pred(Stem),         [check_stem(Stem)|Cs],Cs).
check_fixed_el(ap_svp(Stem),          [check_stem(Stem)|Cs],Cs).
check_fixed_el(adv(Stem),             [check_stem(Stem)|Cs],Cs).
check_fixed_el(np_pred(Stem),         [check_stem(Stem)|Cs],Cs).
check_fixed_el(acc(Stem),             [check_stem(Stem)|Cs],Cs).
check_fixed_el(svp_acc(Stem),         [check_stem(Stem)|Cs],Cs).
check_fixed_el(svp_dat(Stem),         [check_stem(Stem)|Cs],Cs).
check_fixed_el(subj(Stem),            [check_stem(Stem)|Cs],Cs).
check_fixed_el(vc(Stem,Vform,Frame),  [check_fixed_dep(Stem,Frame),
				       check_vform(Vform)|Cs],Cs).
check_fixed_el(ap_pred,               [check_ap_copula|Cs],Cs).
check_fixed_el(pp_pred,               [check_pp_copula|Cs],Cs).
check_fixed_el(pp_pred(Aan,Slag),     [check_pp_copula(Aan,Slag)|Cs],Cs).
check_fixed_el(compar,                [check_compar_adjective|Cs],Cs).
check_fixed_el(rang,                  [check_tag(number(rang))|Cs],Cs).
check_fixed_el(mod_pp(Prep),          [prep(Prep)|Cs],Cs).
check_fixed_el(nor_mod_pp(Prep),      [prep(Prep)|Cs],Cs).
check_fixed_el(pc(Prep),              [prep(Prep)|Cs],Cs).
check_fixed_el(pp(Prep),              [prep(Prep)|Cs],Cs).
check_fixed_el(pp_refl(Prep),         [prep(Prep),check_refl|Cs],Cs).
check_fixed_el(er_pp(Prep,_),         [er_prep(Prep)|Cs],Cs).
check_fixed_el(er_pp(Prep),           [er_prep(Prep)|Cs],Cs).
check_fixed_el(svp_er_pp(Prep),       [er_prep(Prep)|Cs],Cs).
check_fixed_el(svp_er,                [check_er_er|Cs],Cs).
check_fixed_el(ld_pp,                 [check_ld_prep|Cs],Cs).
check_fixed_el(adv_meas,              Cs,Cs).
check_fixed_el(meas,              Cs,Cs).
check_fixed_el(ld_adv,                [check_loc_adv|Cs],Cs).
check_fixed_el(sbar,                  Cs,Cs).
check_fixed_el(dat_sbar,              [check_tag(complementizer(dat))|Cs],Cs).
check_fixed_el(dip_sbar,              Cs,Cs).
check_fixed_el(van_sbar,              [check_tag(complementizer(van))|Cs],Cs).
check_fixed_el(extra_sbar(_),         Cs,Cs).
check_fixed_el(dat_pp(Prep),          [prep(Prep)|Cs],Cs).
check_fixed_el(vp,                    [check_vform(te)|Cs],Cs).
check_fixed_el(vp_no_control,         [check_vform(te)|Cs],Cs).
check_fixed_el(extra_vp(_),           [check_vform(te)|Cs],Cs).
check_fixed_el(extra_vp_no_control(_),[check_vform(te)|Cs],Cs).
check_fixed_el(extra_obj_vp(_,_),     [check_vform(te)|Cs],Cs).
check_fixed_el(obj_vp(_),             [check_vform(te)|Cs],Cs).
check_fixed_el(het_obj1,              [check_het|Cs],Cs).
check_fixed_el(het_svp,               [check_het|Cs],Cs).
check_fixed_el(het_subj,              [check_het|Cs],Cs).
check_fixed_el(het_pobj1,             [check_het|Cs],Cs).
check_fixed_el(naar_sbar_subj,        [check_naar_of_dat_whsub_sbar|Cs],Cs).
check_fixed_el(naar_sbar_subj_no_het, [check_tag(complementizer(naar))|Cs],Cs).
check_fixed_el(sbar_subj,             [check_of_dat_whsub_sbar|Cs],Cs).
check_fixed_el(sbar_subj_opt_het,     [check_of_dat_whsub_sbar|Cs],Cs).
check_fixed_el(subj_het,              [check_het|Cs],Cs).
check_fixed_el(sbar_subj_no_het,      [check_of_dat_whsub_sbar|Cs],Cs).
check_fixed_el(vp_subj,               [check_vform(te)|Cs],Cs).
check_fixed_el(vp_subj_no_het,        [check_vform(te)|Cs],Cs).
check_fixed_el({List},Cs0,Cs) :-
    check_fixed(List,Cs0,Cs).
check_fixed_el(het_pobj1(El),Cs0,Cs) :-
    check_fixed_el(El,Cs0,Cs).
check_fixed_el(opt_het_pobj1(El),Cs0,Cs) :-
    check_fixed_el(El,Cs0,Cs).
check_fixed_el(i(Abb,_),Cs0,Cs):-
    check_fixed_el(Abb,Cs0,Cs).
check_fixed_el(yt(Cat),Cs0,Cs) :-
    check_fixed_el(Cat,Cs0,Cs).
check_fixed_el(nt(Cat),Cs0,Cs) :-
    check_fixed_el(Cat,Cs0,Cs).

check_fixed_part(Ws,P0,P) :-
    hdrug_flag(ignore_fixed_parts,OnOff),
    (    OnOff == on
    ->   true
    ;    check_fixed_part(Ws,_Tag,P0,P)
    ).

check_fixed_part(vc(v_root(Root,_),Iets,Frame),Tag,P0,P) :-
    check_fixed_part(vc(Root,Iets,Frame),Tag,P0,P).

check_fixed_part(Ws,Tag,P0,P) :-
    check_verb_sc(fixed(List,_),Tag,P0,P),
    is_sc_member(Ws,List).
check_fixed_part(Ws,Tag,P0,P) :-
    check_verb_sc(part_fixed(_,List,_),Tag,P0,P),
    is_sc_member(Ws,List).
check_fixed_part(Ws,Tag,P0,P) :-
    check_tag(adjective(_,fixed(List)),Tag,P0,P),
    is_sc_member(Ws,List).
check_fixed_part(Ws,Tag,P0,P) :-
    (  check_tag(nominalized_adjective(fixed(List)),Tag,P0,P)
    ;  check_tag(end_nominalized_adjective(fixed(List)),Tag,P0,P)
    ;  check_tag(ge_nominalized_adjective(fixed(List)),Tag,P0,P)
    ),
    is_sc_member(Ws,List).

check_fixed_word(Part,P0,P) :-
    check_tag(fixed_part(Part),P0,P).

check_fixed_word([Word],P0,P) :-
    alpino_lex:inv_spelling_variant21(Word,W1,W2),
    check_tag(fixed_part([W1,W2]),P0,P).

check_fixed_word([Word],P0,P) :-
    alpino_lex:inv_spelling_variant31(Word,W1,W2,W3),
    check_tag(fixed_part([W1,W2,W3]),P0,P).


is_sc_member(Ws,[H|T]) :-
    is_sc_member(T,H,Ws).

is_sc_member(_,El,El).
is_sc_member(_,{El},Ws) :-
    member(Ws,El).
is_sc_member([H|T],_,Ws) :-
    is_sc_member(T,H,Ws).
    
check_op(P0,P) :-
    check_verb_sc(op,P0,P).

check_op(P0,P) :-
    check_verb_sc(fixed(List,_),P0,P),
    member(vc(_,op,_),List).

check_voor(P0,P) :-
    check_verb_sc(voor_pred_np,P0,P).
check_voor(P0,P) :-
    check_verb_sc(voor_pred_np_sbar,P0,P).
check_voor(P0,P) :-
    check_verb_sc(voor_pred_np_vp,P0,P).
check_voor(P0,P) :-
    check_verb_sc(voor_pred_np_sbar,P0,P).
check_voor(P0,P) :-
    check_verb_sc(fixed(List,_),P0,P),
    member(voor_pred(_),List).

check_verb_sc(Sc,P0,P) :-
    check_verb_sc(Sc,_,P0,P).

check_verb_sc(Sc,Tag,P0,P) :-
    (	check_tag(verb(_,_,Sc),Tag,P0,P)
    ;	check_tag(verb(_,_,ninv(Sc,_)),Tag,P0,P)
    ;	check_tag(verb(_,_,ninv(_,Sc)),Tag,P0,P)
    ;   check_tag(v_noun(Sc),Tag,P0,P)
    ;   check_tag(ge_v_noun(Sc),Tag,P0,P)
    ;   Sc =.. [Fun|Args],
	atom_concat(part_,Fun,Fun2),
	Sc2 =.. [Fun2,_|Args],
	(   check_tag(verb(_,_,Sc2),Tag,P0,P)
	;   check_tag(v_noun(Sc2),Tag,P0,P)
	;   check_tag(ge_v_noun(Sc2),Tag,P0,P)
	)
    ).	 

check_of_dat_whsub_sbar(_,_) :-
    memo(check_of_dat_whsub_sbar0(0,0)).

check_naar_of_dat_whsub_sbar(P0,P) :-
    check_tag(complementizer(naar),P0,P).
check_naar_of_dat_whsub_sbar(P0,P) :-
    check_of_dat_whsub_sbar(P0,P).

check_reduced_relative(_,_) :-
    memo(check_reduced_relative0(0,0)).

check_of_dat_whsub_sbar_right(_P0,P) :-
    check_of_dat_whsub_sbar0(0,P).

:- use_module(library(terms), [ term_hash/4 ]).

memo(Goal) :-
    term_hash(Goal,-1,65025,Index),
    memo(Goal,Index).

memo(Goal,Index) :-
    checked(Index,Goal),
    !,
    checked_ok(Index,Goal).
memo(Goal,Index) :-
    assertz(checked(Index,Goal)),
    call(Goal),
    assertz(checked_ok(Index,Goal)).

check_of_dat_sbar(P0,P) :-
    (	check_tag(complementizer(of),P0,P)
    ;	check_tag(complementizer(dat),P0,P)
    ).

check_of_dat_whsub_sbar0(P0,P) :-
    (	check_tag(complementizer(of),P0,P)
    ;	check_tag(complementizer(dat),P0,P)
    ;	check_reduced_relative0(P0,P)
    ).

check_pl_relative(P0,P) :-
    (   check_reduced_relative(P0,P)
    ;   check_tag(rel_pronoun(de,_),P0,P)
    ;   check_tag(rel_pronoun(both,_),P0,P)
    ).

check_reduced_relative0(P0,P) :-                        % ik vroeg
    (	check_tag(er_wh_loc_adverb,P0,P)	                  % waar je aan dacht
    ;	check_tag(pronoun(ywh,_,_,_,_,_),P0,P)         % wat je zei
    ;	check_tag(pronoun(ywh,_,_,_,_,_,_),P0,P)	  % wat je zei
    ;	check_tag(pronoun(both,_,_,_,_,_),P0,P)        % id (?)
    ;	check_tag(pronoun(both,_,_,_,_,_,_),P0,P)      % id (?)
    ;	check_tag(determiner(_,wh),P0,P)         % welke dingen je zag
    ;	check_tag(determiner(_,rwh,_,_,_),P0,P)     % welke dingen je zag
    ;	check_tag(determiner(_,wh_noq,_,_,_),P0,P)     % zoveel je wilt
    ;	check_tag(determiner(_,rwh),P0,P)     % wiens boeken je zag
    ;	check_tag(wh_loc_adverb,P0,P)            % van waar je kwam
    ;	check_tag(er_wh_loc_adverb,P0,P)            % van waar je kwam
    ;	check_tag(wh_tmp_adverb,P0,P)            % van wanneer de rekening was
    ;	check_tag(wh_adverb,P0,P)                % hoeveel je zag
    ;	check_tag(wh_adjective,P0,P)             % hoe lief je lacht
    ;	check_tag(wh_adjective(_),P0,P)          % hoe lief je lacht
    ;	check_tag(wh_me_adjective,P0,P)          % hoe lief je lacht
    ;	check_tag(waar_adverb(_),P0,P)           % waaraan je dacht
    ).

check_hebben_adj(P0,P) :-
    check_tag(verb(hebben,psp,_SC),P0,P).
check_hebben_adj(P0,P) :-
    check_tag(verb('hebben/zijn',psp,_SC),P0,P).

%% :-( the tag that requires hebben also fulfills it.
check_hebben(P0,P) :-
    check_tag(verb(hebben,PSP,SC),P0,P),
    psp(PSP,SC).
check_hebben(P0,P) :-
    check_tag(verb('hebben/zijn',PSP,SC),P0,P),
    psp(PSP,SC).

psp(psp,_).
psp(inf,SC) :- v_raiser(SC).
psp(inf(no_e),SC) :- v_raiser(SC).
psp(inf_ipp,_).

v_raiser(aux(_)).             % hij is gaan lopen
v_raiser(modifier(X)) :-
    v_raiser(X).
v_raiser(aux_simple(_)).      % hij is wezen vissen
v_raiser(obj_control(_)).     % ik heb hem leren fietsen
v_raiser(subj_control(_)).    % hij heeft staan praten
v_raiser(aci).                % ik heb hem zien vallen
v_raiser(aci_no_obj).         % ik heb de schilderijen laten stelen
v_raiser(fixed([vc(_,_,_)|_],_)).  % ik heb laten zien een beslissing te kunnen nemen

check_zijn(P0,P) :-
    check_tag(verb(zijn,PSP,SC),P0,P),
    psp(PSP,SC).
check_zijn(P0,P) :-
    check_tag(verb(unacc,PSP,SC),P0,P),
    psp(PSP,SC).
check_zijn(P0,P) :-
    check_tag(verb('hebben/zijn',PSP,SC),P0,P),
    psp(PSP,SC).

check_simple_zijn(P0,P) :-
    check_tag(verb(zijn,psp,_SC),P0,P).
check_simple_zijn(P0,P) :-
    check_tag(verb(unacc,psp,_SC),P0,P).
check_simple_zijn(P0,P) :-
    check_tag(verb('hebben/zijn',psp,_SC),P0,P).

check_passive(P0,P) :-
    check_tag(verb(HZ,psp,_),P0,P),
    passive_hz(HZ).

check_te_passive(P0,P) :-
    check_tag(complementizer(te),P0,P,_,Q),
    followed_by_infinitive(Q,HZ),
    passive_hz(HZ).

followed_by_infinitive(A,B) :-
    hdrug_flag(parse_or_generate,FLAG),
    followed_by_infinitive(FLAG,A,B).

followed_by_infinitive(generate,_,_).
followed_by_infinitive(parse,Q1,HZ) :-
    alpino_lexical_analysis:tag(_,_,Q1,_,_,_,_,INF),
    infinitive_tag(INF,HZ).
followed_by_infinitive(parse,Q1,HZ) :-
    alpino_lexical_analysis:tag(_,_,Q1,Q2,_,_,_,punct(_)),
    followed_by_infinitive(Q2,HZ).

infinitive_tag(verb(HZ,inf,_),HZ).
infinitive_tag(verb(HZ,inf(_),_),HZ).

%% not unacc
passive_hz(hebben).
passive_hz(zijn).
passive_hz('hebben/zijn').

finite(sg1).
finite(sg3).
finite(sg).
finite(pl).
finite(sg_hebt).
finite(sg_heeft).
finite(sg_bent).
finite(sg_is).
finite(modal_not_u).
finite(modal_inv).
finite(past(_)).
finite(both(_)).
finite(subjunctive).

%% particles must be to the left, except for finite verbs

check_tpart(Part,P0,P) :-
    hdrug_flag(parse_or_generate,PG),
    check_tpart(PG,Part,P0,P).

check_tpart(generate,Part,P,P) :-
    check_tag(particle(Part),P,P).

check_tpart(parse,Part,P0,P) :-
    check_tag(particle(Part),P0,P).

check_part(Part,P0,P) :-
    hdrug_flag(parse_or_generate,PG),
    check_part(PG,xxx,Part,P0,P).

check_part(Fin,Part,P0,P) :-
    hdrug_flag(parse_or_generate,PG),
    check_part(PG,Fin,Part,P0,P).

check_part(generate,_,Part,P,P) :-
    check_tag(particle(Part),P,P).

check_part(parse,Fin,Part,P0,P) :-
    (   finite(Fin)
    ->  (   check_tag_particle_right(Part,P)
	;   check_tag_particle_left(Part,P0)
	)
    ;   check_tag_particle_left(Part,P0)
    ),
    !.

check_tag_particle_right(Part,Q0) :-
    alpino_lexical_analysis:search_tag_tag(particle(Part),tag(_,_,_,Q,_,_,_,particle(Part))),
    Q > Q0.

%% particle Part should occur left of P0
%% but only if intervening positions are all
%% verbs
check_tag_particle_left(Part,Q) :-
    alpino_lexical_analysis:search_tag_tag(particle(Part),tag(_,_,_,Q,_,_,_,particle(Part))).
check_tag_particle_left(Part,Q) :-
    once(alpino_lexical_analysis:search_tag_tag(verb(_,_,_),tag(_,_,Q0,Q,_,_,_,verb(_,_,_)))),
    check_tag_particle_left(Part,Q0).
check_tag_particle_left(Part,Q) :-
    alpino_lexical_analysis:search_tag_tag(complementizer(te),tag(_,_,Q0,Q,_,_,_,complementizer(te))),
    check_tag_particle_left(Part,Q0).
check_tag_particle_left(Part,Q) :-
    alpino_lexical_analysis:search_tag_tag(complementizer(aan_het),tag(_,_,Q0,Q,_,_,_,complementizer(aan_het))),
    check_tag_particle_left(Part,Q0).
check_tag_particle_left(Part,Q) :-
    once(alpino_lexical_analysis:search_tag_tag(punct(_),tag(_,_,Q0,Q,_,_,_,punct(_)))),
    check_tag_particle_left(Part,Q0).

check_ld_prep(Prep,P0,P) :-
    check_prep(Prep,P0,P).

check_ld_prep(P0,P) :-
    check_prep(Prep,P0,P),
    ld_prep(Prep).

%% prep or adverbial prep; "+R" not obligatory
pprep(Prep,P0,P) :-
    (   Prep \= met, Prep \= tot,
	prep(Prep,P0,P)
    ;   check_tag(preposition(Prep,_,extracted_np),P0,P)
    ;   check_tag(waar_adverb(Prep),P0,P)
    ;   check_tag(er_adverb(Prep),P0,P)
    ).

%% prep/adverbial prep; "+R" obligatory
er_prep(Prep,P0,P) :-
    (   Prep \= met, Prep \= tot,
	prep(Prep,P0,P),
	check_er(P0,P)
    ;   check_tag(preposition(Prep,_,extracted_np),P0,P),
	check_er(P0,P)
    ;   check_tag(waar_adverb(Prep),P0,P)
    ;   check_tag(er_adverb(Prep),P0,P)
    ).

%% preposition occurs as preposition (perhaps with er)
prep(Prep,_,_) :-
    memo(check_prep(Prep,0,0)).

check_prep(Prep,P0,P) :-
    check_tag(preposition(Prep,_),P0,P).
check_prep(Prep,P0,P) :-
    check_tag(preposition(Prep,_,_),P0,P).
check_prep(Prep,P0,P) :-
    check_tag(waar_adverb(Prep),P0,P).
check_prep(Prep,P0,P) :-
    check_tag(er_adverb(Prep),P0,P).
check_prep(Prep,P0,P) :-
    check_tag(pp(Prep),P0,P).

check_refl(P0,P) :-
    check_tag(reflexive(_,_),P0,P).

check_vform_right(te,P0,P) :-
    check_tag(complementizer(te),P0,P,Q0,_Q),
    (   nonvar(P), nonvar(Q0)
    ->  P =< Q0
    ;   true
    ).
check_vform_right(fin,P0,P) :-
    finite(Fin),
    check_tag(verb(_,Fin,_),P0,P,Q0,_Q),
    (   nonvar(P), nonvar(Q0)
    ->  P =< Q0
    ;   true
    ).
check_vform_right(inf,P0,P) :-
    check_vform_inf(P0,P,Q0,_Q),
    (   nonvar(P), nonvar(Q0)
    ->  P =< Q0
    ;   true
    ).

check_vform(wk_te,P0,P) :-
    !,
    P1 is P0 - 1,
    check_tag(complementizer(te),P1,P).
check_vform(pass_te,P0,P) :-
    !,
    P1 is P0 - 1,
    check_tag(complementizer(te),P1,P).
check_vform(te,P0,P) :-
    !,
    P1 is P0 - 1,
    check_tag(complementizer(te),P1,P).
check_vform(te_inf,P0,P) :-
    !,
    (   P1 is P0 - 1,
        check_tag(complementizer(te),P1,P)
    ;   check_vform(inf,P0,P)
    ).
check_vform(fin,P0,P) :-
    !,
    finite(Fin),
    check_vform(Fin,P0,P),
    !.
check_vform(psp(Frame),P0,P) :-
    !,
    check_vform(psp,P0,P),
    check_verb_sc(Frame,P0,P).
check_vform(inf,P0,P) :-
    !,
    check_vform_inf(P0,P,_,_).
check_vform(Vform,P0,P) :-
    check_tag(verb(_,Vform,_),P0,P),
    !.
check_vform(op,P0,P) :-
    check_tag(complementizer(op),P0,P),
    !.


check_te(P0,P) :-
    hdrug_flag(parse_or_generate,FLAG),
    check_te(FLAG,P0,P).

check_te(generate,_,_).
check_te(parse,Q0,Q):-
    alpino_lexical_analysis:search_tag_tag(complementizer(te),tag(_,_,Q0,Q,_,_,_,_)),
    inf_starts(Q).

inf_starts(Q) :-
    (  alpino_lexical_analysis:tag(_,_,Q,_,_,_,_,verb(_,inf,_))
    ;  alpino_lexical_analysis:tag(_,_,Q,_,_,_,_,verb(_,inf(_),_))
    ).
inf_starts(Q) :-
    alpino_lexical_analysis:tag(_,_,Q,Q1,_,_,_,punct(_)),
    inf_starts(Q1).

check_het(P0,P) :-
    check_tag(het_noun,P0,P).

check_cleft_het(P0,P) :-
    check_tag(het_noun,P0,P).
check_cleft_het(P0,P) :-
    check_tag(cleft_het_noun,P0,P).

check_dir_adv(P0,P) :-
    check_tag(dir_adverb,P0,P).
check_dir_adv(P0,P) :-
    check_tag(adjective(Infl),P0,P),
    compound(Infl),   arg(1,Infl,diradv).
check_dir_adv(P0,P) :-
    check_tag(adjective(Infl,_),P0,P),
    compound(Infl),   arg(1,Infl,diradv).
check_dir_adv(P0,P) :-
    check_tag(adjective(Infl),P0,P),
    compound(Infl),   arg(1,Infl,dir_locadv).
check_dir_adv(P0,P) :-
    check_tag(adjective(Infl,_),P0,P),
    compound(Infl),   arg(1,Infl,dir_locadv).

check_tmp_adv(P0,P) :-
    check_tag(tmp_adverb,P0,P).
check_tmp_adv(P0,P) :-
    check_tag(wh_tmp_adverb,P0,P).
check_tmp_adv(P0,P) :-
    check_tag(wh_me_adjective,P0,P).
check_tmp_adv(P0,P) :-
    check_tag(pred_np_me_adjective(tmpadv),P0,P).
check_tmp_adv(P0,P) :-
    check_tag(adjective(Infl),P0,P),
    compound(Infl),    arg(1,Infl,tmpadv).
check_tmp_adv(P0,P) :-
    check_tag(adjective(Infl,_),P0,P),
    compound(Infl),    arg(1,Infl,tmpadv).
check_tmp_adv(P0,P) :-
    check_tag(als_adjective(Infl),P0,P),
    compound(Infl),   arg(1,Infl,tmpadv).
check_tmp_adv(P0,P) :-
    check_tag(sbar_adjective(Infl),P0,P),
    compound(Infl),   arg(1,Infl,tmpadv).
check_tmp_adv(P0,P) :-
    check_tag(sbar_pred_adjective(tmpadv),P0,P).
check_tmp_adv(P0,P) :-
    check_tag(tmp_app_noun,P0,P).

check_loc_adv(P0,P) :-
    check_tag(loc_adverb,P0,P).
check_loc_adv(P0,P) :-
    check_tag(er_loc_adverb,P0,P).
check_loc_adv(P0,P) :-
    check_tag(wh_loc_adverb,P0,P).
check_loc_adv(P0,P) :-
    check_tag(er_wh_loc_adverb,P0,P).
check_loc_adv(P0,P) :-
    check_tag(iets_adverb,P0,P).
check_loc_adv(P0,P) :-
    check_tag(wh_iets_adverb,P0,P).
check_loc_adv(P0,P) :-
    check_tag(adjective(Infl),P0,P),
    compound(Infl),   arg(1,Infl,LOC),
    locadv(LOC).
check_loc_adv(P0,P) :-
    check_tag(adjective(Infl,_),P0,P),
    compound(Infl),   arg(1,Infl,LOC),
    locadv(LOC).
check_loc_adv(P0,P) :-
    check_tag(pred_np_me_adjective(LOC),P0,P),
    locadv(LOC).
check_loc_adv(P0,P) :-
    check_tag(np_me_adjective(Infl),P0,P),
    compound(Infl),   arg(1,Infl,LOC),
    locadv(LOC).
check_loc_adv(P0,P) :-
    check_tag(np_me_adjective(_,Infl),P0,P),
    compound(Infl),   arg(1,Infl,LOC),
    locadv(LOC).

locadv(locadv).
locadv(dir_locadv).

check_subject_pred_sbar(P0,P) :-
    check_of_dat_whsub_sbar(P0,P),
    (   check_adj_subject_pred_sbar(P0,P)
    ;   check_noun_sc(subject_sbar,P0,P)
    ;   check_noun_sc(subject_sbar_no_het,P0,P)
    ;	check_noun_sc(pred_pp(_,subject_sbar),P0,P)
    ;	check_noun_sc(pred_pp_pl(_,subject_sbar),P0,P)
    ;	check_noun_sc(pred_pp(_,subject_sbar_no_het),P0,P)
    ;	check_noun_sc(pred_pp_pl(_,subject_sbar_no_het),P0,P)
    ).

check_subject_pred_vp(P0,P) :-
    (	check_adj_subject_pred_vp(P0,P)
    ;   check_noun_sc(subject_vp,P0,P)
    ;	check_noun_sc(pred_pp(_,subject_vp),P0,P)
    ;	check_noun_sc(pred_pp_pl(_,subject_vp),P0,P)
    ),
    check_vform(te,P0,P).

check_adj_subject_pred_sbar(P0,P) :-
    (	check_tag(adjective(_,subject_vp_sbar_no_het),P0,P)
    ;	check_tag(adjective(_,subject_vp_sbar),P0,P)
    ;	check_tag(adjective(_,subject_sbar),P0,P)
    ;	check_tag(adjective(_,subject_sbar_no_het),P0,P)
    ;	check_tag(adjective(_,pp_subject_sbar(_)),P0,P)
    ;	check_tag(adjective(_,pp_subject_sbar_no_het(_)),P0,P)
    ;	check_tag(adjective(_,so_pp_subject_sbar(_)),P0,P)
    ;	check_tag(adjective(_,so_pp_subject_sbar_no_het(_)),P0,P)
    ;   check_tag(adjective(_,so_np_subject_sbar),P0,P)
    ;   check_tag(post_adjective(_,subject_sbar),P0,P)
    ;   check_tag(post_adjective(_,subject_sbar_no_het),P0,P)
    ;   check_tag(post_adjective(_,pp_subject_sbar(_)),P0,P)
    ;   check_tag(post_adjective(_,pp_subject_sbar_no_het(_)),P0,P)
    ;   check_tag(post_adjective(_,so_pp_subject_sbar(_)),P0,P)
    ;   check_tag(post_adjective(_,so_pp_subject_sbar_no_het(_)),P0,P)
    ;   check_tag(post_adjective(_,so_np_subject_sbar),P0,P)
    ;	check_tag(subject_sbar_pred_np_adjective,P0,P)
    ;	check_tag(subject_sbar_pred_np_me_adjective,P0,P)
    ).

check_adj_subject_pred_vp(P0,P) :-
    (   check_tag(adjective(_,subject_vp),P0,P)
    ;	check_tag(adjective(_,subject_vp_no_het),P0,P)
    ;	check_tag(adjective(_,subject_vp_sbar),P0,P)
    ;	check_tag(adjective(_,subject_vp_sbar_no_het),P0,P)
    ;   check_tag(adjective(_,so_np_subject_vp),P0,P)
    ;	check_tag(post_adjective(_,subject_vp),P0,P)
    ;	check_tag(post_adjective(_,subject_vp_no_het),P0,P)
    ;	check_tag(post_adjective(_,subject_vp_sbar),P0,P)
    ;	check_tag(post_adjective(_,subject_vp_sbar_no_het),P0,P)
    ;   check_tag(post_adjective(_,so_np_subject_vp),P0,P)
    ;   check_tag(subject_vp_pred_np_adjective,P0,P)
    ),
    check_vform(te,P0,P).

a_tag(A,B,C,D,E,F,G,H,P0,P) :-
    hdrug_flag(parse_or_generate,FLAG),
    a_tag(FLAG,A,B,C,D,E,F,G,H,P0,P).

a_tag(generate,_A,_B,_C,_D,_E,_F,_G,TAG,_,_) :-
    alpino_cg:lex(TAG,_,_,_,_,_).
    
a_tag(parse,A,B,C,D,E,F,G,H,P0,P) :-
    alpino_lexical_analysis:search_tag_tag(H,tag(A,B,C,D,E,F,G,H)),
    \+ alpino_lexical_analysis:overlap(A,B,P0,P).

check_stem(Stem,P0,P) :-
    check_stem(Stem,P0,P,_).

check_stem(nominalization(Stem),P0,P) :-
    check_stem(Stem,P0,P,v_noun(_)).

check_stem(Stem,P0,P,H) :-
    atomic(Stem),
    check_stem(v_root(Stem,_),P0,P,H).

check_stem(Stem,P0,P,H) :-
    hdrug_flag(parse_or_generate,PG),
    check_stem(PG,Stem,P0,P,H).

check_stem(parse,Stem,P0,P,H) :-
    alpino_lexical_analysis:search_tag_stem(Stem,tag(A,B,_C,_D,Stem,_F,_G,H)),
    \+ alpino_lexical_analysis:overlap(A,B,P0,P).

check_stem(generate,Stem,_,_,_) :-
    alpino_cg:lex(_,Stem,_,_,_,_).

check_stem(generate,Stem,_,_,_) :-
    alpino_cg:lex(_,{List},_,_,_,_),
    lists:member(Stem,List).

check_tag(Tag,P0,P) :-
    check_tag(Tag,P0,P,_,_).
check_tag(Tag,Tag,P0,P) :-
    check_tag(Tag,P0,P,_,_).

check_tag(Tag,P0,P,Q0,Q) :-
    a_tag(_,_,Q0,Q,_,_,_,Tag,P0,P).
check_tag(Tag,P0,P,Q0,Q) :-
    a_tag(_,_,Q0,Q,_,_,_,with_dt(Tag,_),P0,P).
check_tag(complementizer(dat),P0,P,Q0,Q):-
    check_tag(complementizer(datti),P0,P,Q0,Q).

check_vform_inf(P0,P,Q0,Q) :-
    hdrug_flag(parse_or_generate,PG),
    check_vform_inf(PG,P0,P,Q0,Q).

check_vform_inf(generate,_,_,_,_). % TODO?
check_vform_inf(parse,P0,P,Q0,Q) :-
    (   check_tag(verb(_,inf,_),P0,P,Q,_)
    ;   check_tag(verb(_,inf(no_e),_),P0,P,Q,_)
    ),
    \+ alpino_lexical_analysis:search_tag_tag(complementizer(te),tag(_,_,Q0,Q,_,_,_,_)).

check_aux_simple(P0,P) :-
    hdrug_flag(parse_or_generate,PG),
    check_aux_simple(PG,P0,P).

check_aux_simple(generate,_,_). % TODO?
check_aux_simple(parse,P0,P) :-
    (   check_tag(verb(_,inf,Frame),P0,P,Q,_)
    ;   check_tag(verb(_,inf(no_e),Frame),P0,P,Q,_)
    ),
    (   Frame = intransitive
    ;   Frame = transitive
    ;   Frame = ld_pp
    ),
    \+ alpino_lexical_analysis:search_tag_tag(complementizer(te),tag(_,_,_,Q,_,_,_,_)).

check_aci_simple(P0,P) :-
    hdrug_flag(parse_or_generate,PG),
    check_aci_simple(PG,P0,P).

check_aci_simple(generate,_,_). % TODO?
check_aci_simple(parse,P0,P) :-
    (   check_tag(verb(_,inf,Frame),P0,P,Q,_)
    ;   check_tag(verb(_,inf(no_e),Frame),P0,P,Q,_)
    ),
    aci_simple_frame(Frame),
    \+ alpino_lexical_analysis:search_tag_tag(complementizer(te),tag(_,_,_,Q,_,_,_,_)).

aci_simple_frame(ninv(intransitive,part_intransitive(_))).
aci_simple_frame(intransitive).
aci_simple_frame(ld_pp).
aci_simple_frame(part_intransitive(_)).

ld_prep(P) :-
    alpino_lex_types:r_ld_prep(P).
ld_prep(P) :-
    alpino_lex_types:no_r_ld_prep(P).

check_part_required(Part,P0,P):-
    hdrug_flag(parse_or_generate,PG),
    check_part_required(PG,Part,P0,P).

check_part_required(generate,_,_,_).  % tja, "hij rent de trappen op en af" 
check_part_required(parse,Part,P0,P) :-
    check_part_required(parse,Part,_Tag,P0,P).

check_part_required(parse,Part,Tag,P0,P) :-
    check_tag(verb(_,_,Sc),Tag,P0,P),
    Sc =.. [F,Part|_],
    atom_concat(part_,_,F).
check_part_required(parse,Part,Tag,P0,P) :-
    check_tag(v_noun(Sc),Tag,P0,P),
    Sc =.. [F,Part|_],
    atom_concat(part_,_,F).
check_part_required(parse,Part,Tag,P0,P) :-
    check_tag(ge_v_noun(Sc),Tag,P0,P),
    Sc =.. [F,Part|_],
    atom_concat(part_,_,F).
    
check_part_required(parse,Part,Tag,P0,P) :-
    check_tag(te_v_adj(part(Part)),Tag,P0,P).
check_part_required(parse,Part,Tag,P0,P) :-
    check_tag(te_v_adj(part(Part),_),Tag,P0,P).
check_part_required(parse,Part,Tag,P0,P) :-
    check_tag(adjective(_,part(Part)),Tag,P0,P).
check_part_required(parse,Part,Tag,P0,P) :-
    check_tag(adjective(_,part(Part,_)),Tag,P0,P).
check_part_required(parse,Part,Tag,P0,P) :-
    check_tag(preposition(_,List),Tag,P0,P),
    memberchk(Part,List).
check_part_required(parse,Part,Tag,P0,P) :-
    check_tag(preposition(_,List,_),Tag,P0,P),
    memberchk(Part,List).

check_np_adjective(P0,P) :-
    (  check_tag(np_adjective,P0,P)
    ;  check_tag(np_adjective(_),P0,P)
    ;  check_tag(clause_np_adjective,P0,P)
    ;  check_tag(clause_np_adjective(_),P0,P)
    ;  check_tag(het_np_adjective,P0,P)
    ;  check_tag(adjective(_,refl),P0,P)
    ;  check_tag(adjective(_,refl_np),P0,P)
    ;  check_tag(adjective(_,refl_vp),P0,P)
    ;  check_tag(adjective(_,refl_sbar),P0,P)
    ;  check_tag(adjective(_,refl_pp(_)),P0,P)
    ;  check_tag(adjective(_,refl_er_pp_sbar(_)),P0,P)
    ;  check_tag(adjective(_,refl_er_pp_vp(_)),P0,P)
    ;  check_tag(het_np_adjective(_),P0,P)
    ).

check_pp_copula(P0,P) :-
    (   prep(vol,P0,P)
    ;   check_noun_sc(pred_pp(Prep,_),P0,P),
	check_prep(Prep,P0,P)
    ;   check_noun_sc(pred_pp(Prep),P0,P),
	check_prep(Prep,P0,P)
    ;   check_noun_sc(pred_pp_pl(Prep,_),P0,P),
	check_prep(Prep,P0,P)
    ;   check_noun_sc(pred_pp_pl(Prep),P0,P),
	check_prep(Prep,P0,P)
    ).

check_pp_copula(Prep,Noun,P0,P) :-
    (   check_noun_sc(pred_pp(Prep,_),P0,P),
	check_prep(Prep,P0,P),
	check_stem(Noun,P0,P)
    ;   check_noun_sc(pred_pp(Prep),P0,P),
	check_prep(Prep,P0,P),
	check_stem(Noun,P0,P)
    ;   check_noun_sc(pred_pp_pl(Prep,_),P0,P),
	check_prep(Prep,P0,P),
	check_stem(Noun,P0,P)
    ;   check_noun_sc(pred_pp_pl(Prep),P0,P),
	check_prep(Prep,P0,P),
	check_stem(Noun,P0,P)
    ).

check_ap_copula(P0,P) :-
    (   check_tag(adjective(Infl),P0,P), adj_pred_infl(Infl)
    ;   check_tag(adjective(Infl,_),P0,P), adj_pred_infl(Infl)
    ;   check_tag(wh_adjective,P0,P)
    ;   check_tag(wh_adjective(Infl),P0,P), adj_pred_infl(Infl)
    ;   check_tag(wh_me_adjective,P0,P)
    ;   check_tag(adjective(Infl),P0,P), adj_pred_infl(Infl)
    ;   check_tag(adjective(Infl,_),P0,P), adj_pred_infl(Infl)
    ;   check_tag(clause_np_adjective,P0,P)
    ;   check_tag(np_adjective,P0,P)
    ;   check_tag(np_me_adjective(Infl),P0,P), adj_pred_infl(Infl)
    ;   check_tag(pred_np_me_adjective(_),P0,P)
    ;   check_tag(subject_sbar_pred_np_adjective,P0,P)
    ;   check_tag(subject_sbar_pred_np_me_adjective,P0,P)
    ;   check_tag(subject_vp_pred_np_adjective,P0,P)
    ;   check_tag(np_adjective,P0,P)
    ;   check_tag(clause_np_adjective,P0,P)
    ;   check_tag(het_np_adjective(_),P0,P)
    ;   check_tag(als_adjecive(Infl),P0,P), adj_pred_infl(Infl)
    ;   check_tag(sbar_adjective(_),P0,P), adj_pred_infl(_)
    ;   check_tag(sbar_pred_adjective(_),P0,P)
    ;   check_tag(vp_pred_adjective(_),P0,P)
    ;   check_tag(number(rang),P0,P)
    ;   check_tag(wh_number(rang),P0,P)
    ).

adj_pred_infl(no_e(_)).
adj_pred_infl(pred(_)).
adj_pred_infl(pred_er(_)).
adj_pred_infl(er(_)).
adj_pred_infl(st(_)).
adj_pred_infl(het_st(_)).
adj_pred_infl(meer).
adj_pred_infl(anders).
adj_pred_infl(ge_no_e(_)).
adj_pred_infl(both(_)).
adj_pred_infl(ge_both(_)).
adj_pred_infl(postn_both(_)).
adj_pred_infl(postn_pred(_)).
adj_pred_infl(postn_no_e(_)).
adj_pred_infl(ende(_)).
adj_pred_infl(end(_)).


check_ap_pp_copula(P0,P) :-
    (   check_ap_copula(P0,P)
    ;   check_pp_copula(P0,P)
    ).

check_nonp_copula(P0,P) :-
    (   check_tag(complementizer(als),P0,P)
    ;   check_tag(complementizer(zoals),P0,P)
    ;   check_tag(complementizer(om),P0,P)
    ;   check_ap_pp_copula(P0,P)
    ).

check_er(P0,P) :-
    (   check_tag(er_loc_adverb,P0,P)     % hier daar ergens nergens overal
    ;   check_tag(er_wh_loc_adverb,P0,P)  % waar
    ;   check_tag(er_vp_adverb,P0,P)      % er
    ).

check_compar_adjective(P0,P) :-
    (   check_tag(adjective(er(_)),P0,P)
    ;   check_tag(adjective(anders),P0,P)
    ;   check_tag(adjective(er(_),_),P0,P)
    ;   check_tag(adjective(meer),P0,P)
    ).

check_subject_sbar(P0,P) :-
    (   check_adj_subject_pred_sbar(P0,P)
    ;   check_tag(number(rang),P0,P)
    ;   check_noun_sc(subject_sbar,P0,P)
    ;   check_noun_sc(subject_sbar_no_het,P0,P)
    ;   check_noun_sc(pred_pp(_,subject_sbar),P0,P)
    ;   check_noun_sc(pred_pp_pl(_,subject_sbar),P0,P)
    ;   check_noun_sc(pred_pp(_,subject_sbar_no_het),P0,P)
    ;   check_noun_sc(pred_pp_pl(_,subject_sbar_no_het),P0,P)
    ;   check_reduced_relative(P0,P)
    ).

check_noun_sc(Sc,P0,P) :-
    check_tag(noun(_,_,_,Sc),P0,P).
check_noun_sc(Sc,P0,P) :-
    check_tag(tmp_noun(_,_,_,Sc),P0,P).
check_noun_sc(Sc,P0,P) :-
    check_tag(mod_noun(_,_,_,Sc),P0,P).
check_noun_sc(Sc,P0,P) :-
    check_tag(meas_mod_noun(_,_,_,Sc),P0,P).
check_noun_sc(Sc,P0,P) :-
    check_tag(amount_meas_mod_noun(_,_,_,Sc),P0,P).

check_subject_vp(P0,P) :-
    (   check_adj_subject_pred_vp(P0,P)
    ;   check_noun_sc(subject_vp,P0,P)
    ;   check_noun_sc(pred_pp(_,subject_vp),P0,P)
    ;   check_noun_sc(pred_pp_pl(_,subject_vp),P0,P)
    ;   check_reduced_relative(P0,P)
    ).

check_er_er(P0,P) :-
    a_tag(_,_,_,_,er,_,_,er_vp_adverb,P0,P).

check_wat(P0,P) :-
    a_tag(_,_,_,_,wat,_,_,pronoun(ywh,thi,sg,het,both,indef,nparg),P0,P).

check_alsof(P0,P) :-
    (   check_tag(complementizer(alsof),P0,P)
    ;   check_tag(complementizer(of),P0,P)
    ).

compar_infl(er(_)).
compar_infl(er).
compar_infl(ere).
compar_infl(meer).
compar_infl(anders).

check_selects_comparative(Val,P0,P) :-
    (   check_tag(comp_determiner(_,Val),P0,P)
    ;   check_tag(comp_adverb(Val),P0,P)
    ;   check_tag(comp_noun(_,_,_,Val),P0,P)
    ;   Val=dan,
	(   check_tag(nominalized_compar_adjective,P0,P)
        ;   check_tag(nominalized_compar_adjective_sg,P0,P)
	;   check_tag(post_adjective_anders(er),P0,P)
	;   check_tag(adjective(Infl),P0,P), compar_infl(Infl)
	;   check_tag(adjective(Infl,_),P0,P), compar_infl(Infl)
	;   check_tag(post_adjective(Infl),P0,P), compar_infl(Infl)
	;   check_tag(post_adjective(Infl,_),P0,P), compar_infl(Infl)
	)
    ;   Val=als,
	(   check_tag(als_me_intensifier,P0,P)
	;   check_tag(als_adjective(_),P0,P)
	)
    ).

check_compar_adj(P0,P) :-
    (   check_tag(adjective(Infl),P0,P),
        compar_hoe_infl(Infl)
    ;   check_tag(adjective(Infl,_),P0,P),
        compar_hoe_infl(Infl)
    ).

compar_hoe_infl(er(_)).
compar_hoe_infl(meer).

check_me_adj(P0,P) :-
    (   check_compar_adj(P0,P)
    ;   check_tag(np_me_adjective(_),P0,P)
    ;   check_tag(pred_np_me_adjective(_),P0,P)
    ;   check_tag(me_intensifier,P0,P)
    ).

fail(_,_) :-
    fail.

check_fixed_dep(Stem,Frame,P0,P) :-
    check_stem(Stem,P0,P,verb(_,_,Frame)).
check_fixed_dep(Stem,Frame,P0,P) :-
    check_stem(Stem,P0,P,verb(_,_,fixed_dep(Frame))).
check_fixed_dep(Stem,Frame,P0,P) :-
    Frame =.. [F,Part|Args],
    atom_concat(part_,F2,F),
    Frame2 =.. [F2|Args],
    check_stem(Stem,P0,P,verb(_,_,part_fixed_dep(Part,Frame2))).
check_fixed_dep(Stem,Frame,P0,P) :-
    Frame =.. [F,Part|Args],
    atom_concat(part_,F2,F),
    Frame2 =.. [F2|Args],
    check_stem(Stem,P0,P,verb(_,_,ninv(_,part_fixed_dep(Part,Frame2)))).
check_fixed_dep(Stem,Frame,P0,P) :-
    check_stem(Stem,P0,P,verb(_,_,ninv(_,Frame))).


check_cleft_vform(Fin,P0,P) :-
    (   sg(Fin)
    ->  (  check_tag(rel_pronoun(_,_),P0,P)
	;  check_tag(waar_adverb(_),P0,P)
	)
    ;   true
    ).

sg(sg).
sg(sg1).
sg(sg_heeft).
sg(sg3).
sg(past(sg)).

