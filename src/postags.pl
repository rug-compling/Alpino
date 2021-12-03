:- module(alpino_postags, [ postag_of_frame/2,
			    postag_of_frame/3,
			    postag_of_frame/5,
			    attribute_of_frame/3,
			    attribute_of_frame/4
			  ]).

:- expects_dialect(sicstus).

:- use_module(hdrug(hdrug_util)).

%% this is used for properly formatting frames in automatic XML output
%% cf treebank.pl
%% it is also used for creating/manipulating triples, cf dt.pl

% attribute_of_frame(+Frame,+Att,?Val)
attribute_of_frame(Frame,Att,Val) :-
    attribute_of_frame(Frame,Att,_,Val).

% attribute_of_frame(+Frame,+Att,?Postag,?Val)

attribute_of_frame(Frame,Att,PosTag,Val) :-
    postag_of_frame(Frame,PosTag,AttList),
    lists:memberchk(Att=Val,AttList).

postag_of_frame(Frame,PosTag,AttList) :-
    postag_of_frame(Frame,_,PosTag,AttList,'NA()').

postag_of_frame(Frame,PosTag) :-
    postag_of_frame(Frame,_,PosTag,_,'NA()').

% postag_of_frame(Frame,PosTag,AttValList).
% this should NOT introduce attributes that already exist in xml!!!
% case 1: for "obsolete" xml files
postag_of_frame(Var,_,_,_,_) :-
    var(Var),
    !,
    fail.
postag_of_frame(none,none,none,[],_):-
    !.
postag_of_frame(read_from_treebank(PosTag0),none,PosTag,[],_) :-
    !,
    functor(PosTag0,PosTag,_).
postag_of_frame(read_from_treebank(PosTag,Frame,_Lemma,CgnTag),Frame,PosTag,
                [postag=CgnTag|AttVal],_) :-
    !,
    lassy_postag_atts(CgnTag,AttVal,Atts),
    (   p_of_f(Frame,PosTag0,Atts)
    ->  PosTag0=PosTag
    ;   debug_message(1,
	  "warning: no postag_of_frame for ~w~n",[Frame]),
	functor(Frame,PosTag,_),
	Atts=[]
    ).

postag_of_frame(read_from_treebank(PosTag0,_Lemma,CgnTag),none,PosTag,
                [postag=CgnTag|AttVal],_) :-
    !,
    functor(PosTag0,PosTag,_),
    lassy_postag_atts(CgnTag,AttVal,[]).

postag_of_frame(read_from_treebank(PosTag0,CgnTag),none,PosTag,
                [postag=CgnTag|AttVal],_) :-
    !,
    functor(PosTag0,PosTag,_),
    lassy_postag_atts(CgnTag,AttVal,[]).

postag_of_frame(Atts0:Frame,Fr,Pos,Atts,CgnTag) :-
    !,
    lists:append(Atts0,Atts1,Atts),
    postag_of_frame(Frame,Fr,Pos,Atts1,CgnTag).

postag_of_frame(Frame,Frame,PosTag,[postag=CgnTag|AttVal],CgnTag) :-
    (   p_of_f(Frame,PosTag0,AttValList)
    ->  PosTag0=PosTag,
	lassy_postag_atts(CgnTag,AttVal,AttValList)
    ;   debug_message(1,
	  "warning: no postag_of_frame for ~w~n",[Frame]),
	functor(Frame,PosTag,_),
	AttVal=[]
    ).

%% robust_skip:
%% never used for end results, but only during frame_features in
%% penalties.pl
p_of_f(top,top,[]).

p_of_f(robust_skip,robust_skip,[]).

%% only after time-out??
p_of_f(with_dt(Tag,_),Result,Atts) :-
    p_of_f(Tag,Result,Atts).

p_of_f(pronoun(Wh,Per,Num,Gen,Case,Def),
		pron,
		[wh=Wh,per=Per,num=Num,gen=Gen,case=Case,def=Def]).
p_of_f(pronoun(Wh,Per,Num,Gen,Case,Def,Special),
		pron,
		[wh=Wh,per=Per,num=Num,gen=Gen,case=Case,def=Def,special=Special]).
p_of_f(reflexive(Per,Num),pron,[per=Per,num=Num,refl=refl]).
p_of_f(rel_pronoun(Gen,Case),pron,[gen=Gen,case=Case,wh=rel]).
p_of_f(rel_pred,pron,[wh=rel,special=pred]).
p_of_f(zoals_rel_adverb,pron,[wh=rel,special=adv]).

p_of_f(preposition(_,_),prep,[]).
p_of_f(preposition(_,_,Sc),prep,[sc=Sc]).
p_of_f(mod_postposition,prep,[special=mod_post]).
p_of_f(pp,pp,[]).
p_of_f(pp(_),pp,[]).
p_of_f(waar_adverb(_),pp,[special=waar]).
p_of_f(er_adverb(_),pp,[special=er]).

p_of_f(adverb,adv,[]).
p_of_f(er_adverb,adv,[special=er]).
p_of_f(post_wh_adverb,adv,[special=post_wh]).
p_of_f(pre_wh_adverb,adv,[special=pre_wh]).
p_of_f(comp_adverb(Sub),adv,[special=comp,comparative=Sub]).
p_of_f(intensifier,adv,[special=intensifier]).
p_of_f(intensifier(e),adv,[special=intensifier]).
p_of_f(vp_om_intensifier,adv,[special=intensifier,sc=vp_om]).
p_of_f(me_intensifier,adv,[special=me_intensifier]).
p_of_f(als_me_intensifier,adv,[special=me_intensifier,sc=als]).
p_of_f(vp_om_me_intensifier,adv,[special=me_intensifier,sc=vp_om]).
p_of_f(wh_adverb,adv,[wh=ywh]).
p_of_f(wh_tmp_adverb,adv,[wh=ywh,special=tmp]).
p_of_f(wh_loc_adverb,adv,[wh=ywh,special=loc]).
p_of_f(er_wh_loc_adverb,adv,[wh=ywh,special=er_loc]).
p_of_f(rwh_loc_adverb,adv,[wh=rwh,special=loc]).
p_of_f(predm_adverb,adv,[special=predm]).
p_of_f(eenmaal_adverb,adv,[special=eenmaal]).
p_of_f(postadj_adverb,adv,[special=postadj]).
p_of_f(postadv_adverb,adv,[special=postadv]).
p_of_f(post_loc_adv_adv,adv,[special=postlocadv]).
p_of_f(postnp_adverb,adv,[special=postnp]).
p_of_f(postn_adverb,adv,[special=postn]).
p_of_f(postp_adverb,adv,[special=postp]).
p_of_f(sentence_adverb,adv,[special=sentence]).
p_of_f(er_vp_adverb,adv,[special=er]).
p_of_f(loc_adverb,adv,[special=loc]).
p_of_f(er_loc_adverb,adv,[special=er_loc]).
p_of_f(wk_tmp_adverb,adv,[special=tmp,wk=yes]).
p_of_f(tmp_adverb,adv,[special=tmp]).
p_of_f(dir_adverb,adv,[special=dir]).
p_of_f(modal_adverb,adv,[sc=modal]).
p_of_f(modal_adverb(Sc),adv,[sc=Sc]).
p_of_f(dip_sbar_adverb,adv,[sc=dip_sbar]).
p_of_f(hoe_adv,adv,[special=hoe]).
p_of_f(om_postadj_adverb,adv,[special=postadj,sc=om]).
p_of_f(vp_adverb,adv,[sc=vp]).
p_of_f(vp_om_adverb,adv,[sc=vp_om]).
p_of_f(zo_mogelijk_zo,adv,[sc=zo_mogelijk]).
p_of_f(iets_adverb,adv,[sc=iets]).
p_of_f(wh_iets_adverb,adv,[sc=iets,wh=ywh]).
p_of_f(vandaar_adverb,adv,[sc=vandaar]).
p_of_f(zo_van_adverb,adv,[sc=zo_van]).

p_of_f(post_p(_),prep,[special=post]).

p_of_f(v_noun(Sc),verb,[sc=Sc,special=v_noun]).
p_of_f(ge_v_noun(Sc),noun,[sc=Sc,special=ge_v_noun]).
p_of_f(verb(_,Infl0,Sc0),verb,[sc=Sc,infl=Infl|Rest]) :-
    verb_infl(Infl0,Infl,Rest),
    sc(Sc0,Sc).
p_of_f(denk_ik,verb,[special=denk_ik]).
p_of_f(denk_ik_dip,verb,[special=denk_ik_dip]).

p_of_f(pred_np_me_adjective(_),adj,[sc=pred_np_me]).
p_of_f(subject_sbar_pred_np_me_adjective,adj,[sc=subject_sbar_pred_np_me]).
p_of_f(subject_sbar_pred_np_adjective,adj,[sc=subject_sbar_pred_np]).
p_of_f(subject_vp_pred_np_adjective,adj,[sc=subject_vp_pred_np]).
p_of_f(np_me_adjective(Infl0),adj,[sc=np_me|Atts]) :-
    functor(Infl0,Infl,_),
    adj_form(Infl,Atts).
p_of_f(np_me_adjective(_,Infl0),adj,[sc=np_me|Atts]) :-
    functor(Infl0,Infl,_),
    adj_form(Infl,Atts).
p_of_f(clause_np_adjective,adj,[special=clause_np]).
p_of_f(clause_np_adjective(Sc),adj,[special=clause_np,sc=Sc]).
p_of_f(np_adjective,adj,[special=np]).
p_of_f(np_adjective(Sc),adj,[special=np,sc=Sc]).
p_of_f(het_np_adjective,adj,[special=het_np]).
p_of_f(het_np_adjective(Sc),adj,[special=het_np,sc=Sc]).
p_of_f(adjective(Infl0),adj,Atts) :-
    functor(Infl0,Infl,_),
    adj_form(Infl,Atts).
p_of_f(adjective(Infl0,Sc),adj,[sc=Sc|Atts]) :-
    functor(Infl0,Infl,_),
    adj_form(Infl,Atts).
p_of_f(adjective(Infl0,_,Sc),adj,[sc=Sc|Atts]) :-
    functor(Infl0,Infl,_),
    adj_form(Infl,Atts).
p_of_f(post_adjective(Infl0,Sc),adj,[iets=true,special=iets,sc=Sc|Atts]) :-
    functor(Infl0,Infl,_),
    adj_form(Infl,Atts).
p_of_f(post_adjective(Infl0),adj,[iets=true,special=iets|Atts]) :-
    functor(Infl0,Infl,_),
    adj_form(Infl,Atts).
p_of_f(post_adjective_anders(er),adj,[iets=true,special=anders,infl=er,aform=compar]).
p_of_f(wh_adjective,adj,[wh=ywh]).
p_of_f(wh_adjective(_),adj,[wh=ywh]).
p_of_f(wh_me_adjective,adj,[wh=ywh]).
p_of_f(vp_om_me_adjective(Infl0),adj,[sc=vp_om_me|Atts]) :-
    functor(Infl0,Infl,_),
    adj_form(Infl,Atts).
p_of_f(me_adjective(Infl0),adj,[sc=me|Atts]) :-
    functor(Infl0,Infl,_),
    adj_form(Infl,Atts).
p_of_f(nominalized_adjective,adj,[num=pl,personalized=true,special=a_noun,aform=base]).
p_of_f(nominalized_adjective(Sc),adj,[num=pl,personalized=true,special=a_noun,sc=Sc,aform=base]).
p_of_f(nominalized_compar_adjective,adj,[num=pl,personalized=true,special=a_noun,aform=compar]).
p_of_f(nominalized_compar_adjective_sg,adj,[num=sg,personalized=true,special=a_noun,aform=compar]).
p_of_f(nominalized_super_adjective,adj,[num=pl,personalized=true,special=a_noun,aform=super]).
p_of_f(nominalized_adjective_sg,adj,[num=sg,personalized=true,special=a_noun,aform=base]).
p_of_f(sbar_adjective(Infl0),adj,[sc=sbar|Atts]) :-
    functor(Infl0,Infl,_),
    adj_form(Infl,Atts).
p_of_f(sbar_pred_adjective(_),adj,[sc=sbar,infl=pred]).
p_of_f(vp_pred_adjective(_),adj,[sc=vp,infl=pred]).
p_of_f(zo_mogelijk_mogelijk(Infl0),adj,[sc=zo_mogelijk|Atts]) :-
    functor(Infl0,Infl,_),
    adj_form(Infl,Atts).
p_of_f(als_adjective(Infl0),adj,[sc=als|Atts]) :-
    functor(Infl0,Infl,_),
    adj_form(Infl,Atts).
p_of_f(e_als_adjective(Infl0),adj,[sc=e_als|Atts]) :-
    functor(Infl0,Infl,_),
    adj_form(Infl,Atts).

p_of_f(noun(Gen,_,Num),noun,[gen=Gen,num=Num]).
p_of_f(noun(Gen,_,Num,Sc),noun,[gen=Gen,num=Num,sc=Sc]).

p_of_f(tmp_noun(A,B,C),Pos,[special=tmp|Atts]) :-
    p_of_f(noun(A,B,C),Pos,Atts).
p_of_f(tmp_noun(A,B,C,D),Pos,[special=tmp|Atts]) :-
    p_of_f(noun(A,B,C,D),Pos,Atts).

p_of_f(meas_mod_noun(A,B,C),Pos,[special=meas_mod|Atts]) :-
    p_of_f(noun(A,B,C),Pos,Atts).
p_of_f(meas_mod_noun(A,B,C,D),Pos,[special=meas_mod|Atts]) :-
    p_of_f(noun(A,B,C,D),Pos,Atts).

p_of_f(amount_meas_mod_noun(A,B,C),Pos,[special=amount_meas_mod|Atts]) :-
    p_of_f(noun(A,B,C),Pos,Atts).
p_of_f(amount_meas_mod_noun(A,B,C,D),Pos,[special=amount_meas_mod|Atts]) :-
    p_of_f(noun(A,B,C,D),Pos,Atts).

p_of_f(mod_noun(A,B,C),Pos,[special=mod|Atts]) :-
    p_of_f(noun(A,B,C),Pos,Atts).
p_of_f(mod_noun(A,B,C,D),Pos,[special=mod|Atts]) :-
    p_of_f(noun(A,B,C,D),Pos,Atts).

p_of_f(comp_noun(Gen,_,Num,Dan),noun,[gen=Gen,num=Num,special=comp,
				      comparative=Dan]).
p_of_f(cleft_het_noun,noun,[special=cleft_het]).
p_of_f(wh_cleft_het_noun,noun,[special=cleft_het,wh=ywh]).
p_of_f(het_noun,noun,[special=het]).
p_of_f(iets_noun,noun,[sc=iets]).
p_of_f(wh_iets_noun,noun,[wh=ywh,sc=iets]).
p_of_f(iets_anders_noun,noun,[sc=iets_anders_noun]).
p_of_f(wh_iets_anders_noun,noun,[sc=iets_anders_noun,wh=ywh]).
p_of_f(er_noun,noun,[special=er]).
p_of_f(wh_er_noun,noun,[special=er,wh=ywh]).
p_of_f(tmp_app_noun,adv,[special=tmp_app]).
p_of_f(np,noun,[]).
p_of_f(np_TEMP,noun,[]).
p_of_f(np(Y),noun,[neclass=Y]).
p_of_f(tmp_np,noun,[special=tmp]).
p_of_f(functn,noun,[special=functn]).
p_of_f(post_n_n,noun,[special=post_n_n]).

p_of_f(determiner(pron),det,Atts) :-
    !, Atts=[infl=pron,pron=true].
p_of_f(determiner(Infl),det,[infl=Infl]).
p_of_f(name_determiner(Infl),det,[infl=Infl,special=name]).
p_of_f(name_determiner(Infl,Class),det,[infl=Infl,special=name,neclass=Class]).
p_of_f(determiner(Infl,Wh),det,[infl=Infl,wh=Wh]).
p_of_f(determiner(Infl,Wh,_),det,[infl=Infl,wh=Wh]).
p_of_f(determiner(Infl,Wh,_,_),det,[infl=Infl,wh=Wh]).
p_of_f(determiner(Infl,Wh,_,_,_),det,[infl=Infl,wh=Wh]).
p_of_f(determiner(Infl,Wh,_,_,_,_),det,[infl=Infl,wh=Wh]).
p_of_f(determiner(Infl,Wh,_,_,_,_,_),det,[infl=Infl,wh=Wh]).
p_of_f(determiner(Infl,Wh,_,_,_,_,_,_),det,[infl=Infl,wh=Wh]).
p_of_f(comp_determiner(Infl,Sc),det,[infl=Infl,special=comp,comparative=Sc]).
p_of_f(comp_determiner(Infl),det,[infl=Infl,special=comp]).
p_of_f(pre_num_adv(Infl),det,[infl=Infl,special=pre_num_adv]).
p_of_f(pre_det_quant(Infl),det,[infl=Infl,special=pre_det_quant]).

p_of_f(adj_number(Infl),det,[infl=Infl,special=adj_number]).

p_of_f(gen_determiner(Infl),det,[infl=Infl,special=gen]).
p_of_f(tmp_determiner,det,[special=tmp]).
p_of_f(num_predm_adverb,det,[special=num_predm]).

p_of_f(number(hoofd(Infl)),num,[infl=Infl,numtype=hoofd]).
p_of_f(number(rang),num,[numtype=rang]).
p_of_f(num_na,num,[special=na]). % de drie na mooiste, only with_dt?
p_of_f(wh_number(rang),num,[wh=wh,special=rang]).
p_of_f(score_cat,num,[special=score]).
p_of_f(pre_np_adverb,adv,[special=pre_np]).
p_of_f(enumeration,num,[special=enumeration]).

p_of_f(sbar,comp,[]).
p_of_f(complementizer,comp,[]).
p_of_f(complementizer(Sc),comp,[sc=Sc]).
p_of_f(complementizer(Sc),comp,[sc=Sc]).
p_of_f(conj(_),vg,[]).
p_of_f(left_conj(_),vg,[special=left]).
p_of_f(right_conj(_),vg,[special=right]).
p_of_f(comparative(_),comparative,[]).
p_of_f(etc,vg,[]).
p_of_f(complex_etc,vg,[]).

p_of_f(fixed_part(_),fixed,[]).

p_of_f(particle(_),part,[]).

p_of_f(proper_name(Num),name,[num=Num,neclass='MISC']).
p_of_f(proper_name(Num,Class),name,[num=Num,neclass=Class]).

p_of_f(adv_tag,tag,[]). % no longer used, but occurs in older Treebanks
p_of_f(tag,tag,[]).

p_of_f(within_word_conjunct,prefix,[]).

p_of_f(punct(Type),punct,[special=Type]).
p_of_f(longpunct,punct,[special=long]).

p_of_f(max,max,[]).

p_of_f(read_from_treebank(Pos),Pos,[]).

p_of_f('UNKNOWN','UNKNOWN',[]).

p_of_f('--','--',[]).

%% old
p_of_f(postnp_det_mod,A,B) :-
    p_of_f(adjective(meer),A,B).

p_of_f(meas_tmp_noun(A,B,_),X,Y) :-
    p_of_f(tmp_noun(A,B,meas),X,Y).

p_of_f(subject_vp_pred_np_me_adjective,adj,[sc=subject_vp_pred_np]).

%% per request of Gosse, 2008-07-25
sc(ninv(_,A),B) :-
    !,
    A=B.
sc(A,A).

verb_infl(inf,inf,[]).
verb_infl(inf(no_e),inf(no_e),[]).
verb_infl(inf(e),inf(e),[]).
verb_infl(psp,psp,[]).
verb_infl(past(sg),sg,[tense=past]).
verb_infl(past(pl),pl,[tense=past]).
verb_infl(sg,sg,[tense=present]).
verb_infl(pl,pl,[tense=present]).
verb_infl(both(pl),pl,[]).
verb_infl(sg1,sg1,[tense=present]).
verb_infl(sg3,sg3,[tense=present]).
verb_infl(sg_hebt,sg_hebt,[tense=present]).
verb_infl(sg_heeft,sg_heeft,[tense=present]).
verb_infl(modal_not_u,modal_not_u,[tense=present]).
verb_infl(modal_inv,modal_inv,[tense=present]).
verb_infl(subjunctive,subjunctive,[tense=subjunctive]).
verb_infl(imp(sg),imp(sg),[tense=present]).
verb_infl(imp(sg1),imp(sg1),[tense=present]).
verb_infl(imp(modal_u),imp(modal_u),[tense=present]).
verb_infl(inf_ipp,inf_ipp,[]).

adj_form(stof,        [infl=stof,   aform=base,    vform=adj]).
adj_form(prefix,      [infl=prefix, aform=base,    vform=adj]).
adj_form(e,           [infl=e,      aform=base,    vform=adj]).
adj_form(ge_e,        [infl=e,      aform=base,    vform=psp]).
adj_form(ere,         [infl=e,      aform=compar,  vform=adj]).
adj_form(ste,         [infl=e,      aform=super,   vform=adj]).
adj_form(pred,        [infl=pred,   aform=base,    vform=adj]).
adj_form(pred_er,     [infl=pred,   aform=compar,  vform=adj]).
adj_form(both,        [infl=both,   aform=base,    vform=adj]).
adj_form(ge_both,     [infl=both,   aform=base,    vform=psp]).
adj_form(postn_both,  [infl=both,   aform=base,    vform=adj]).
adj_form(postn_pred,  [infl=pred,   aform=base,    vform=adj]).
adj_form(no_e,        [infl=no_e,   aform=base,    vform=adj]).
adj_form(ge_no_e,     [infl=no_e,   aform=base,    vform=psp]).
adj_form(postn_no_e,  [infl=no_e,   aform=base,    vform=adj]).
adj_form(er,          [infl=no_e,   aform=compar,  vform=adj]).
adj_form(het_st,      [infl=pred,   aform=super,   vform=adj]).
adj_form(het_ste,     [infl=pred,   aform=super,   vform=adj]).
adj_form(st,          [infl=no_e,   aform=super,   vform=adj]).
adj_form(aller_st,    [infl=no_e,   aform=super,   vform=adj]).
adj_form(ende,        [infl=e,      aform=base,    vform=gerund]).
adj_form(end,         [infl=no_e,   aform=base,    vform=gerund]).

adj_form(anders,      [infl=anders, aform=compar,  vform=adj]).
adj_form(meer,        [infl=meer,   aform=compar,  vform=adj]).



%%%%
% lassy_postag_atts(CgnTag,AttVal,AttValList).

%lassy_postag_atts('NA()',Atts0,Atts) :-
%    !,
%    Atts0=Atts.
lassy_postag_atts(Tag,Atts0,Atts) :-
    (   lassy_postag_atts_(Tag,Atts0,Atts)
    ->  true
    ;   format(user_error,"postag not recognized: ~w~n",[Tag]),
	Atts0=Atts
    ).

lassy_postag_atts_('NA()',[pt='na'|AttVal],AttVal).
lassy_postag_atts_('TSW(dial)',[pt='tsw',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('N(soort,dial)',[pt='n',ntype='soort',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('N(eigen,dial)',[pt='n',ntype='eigen',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('ADJ(dial)',[pt='adj',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('WW(dial)',[pt='ww',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('TW(hoofd,dial)',[pt='tw',numtype='hoofd',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('TW(rang,dial)',[pt='tw',numtype='rang',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,dial)',[pt='vnw',vwtype='pers',pdtype='pron',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VNW(refl,pron,dial)',[pt='vnw',vwtype='refl',pdtype='pron',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VNW(recip,pron,dial)',[pt='vnw',vwtype='recip',pdtype='pron',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dial)',[pt='vnw',vwtype='bez',pdtype='det',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VNW(vrag,pron,dial)',[pt='vnw',vwtype='vrag',pdtype='pron',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VNW(vrag,det,dial)',[pt='vnw',vwtype='vrag',pdtype='det',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VNW(betr,pron,dial)',[pt='vnw',vwtype='betr',pdtype='pron',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VNW(betr,det,dial)',[pt='vnw',vwtype='betr',pdtype='det',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VNW(excl,pron,dial)',[pt='vnw',vwtype='excl',pdtype='pron',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VNW(excl,det,dial)',[pt='vnw',vwtype='excl',pdtype='det',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,pron,dial)',[pt='vnw',vwtype='aanw',pdtype='pron',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,det,dial)',[pt='vnw',vwtype='aanw',pdtype='det',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,pron,dial)',[pt='vnw',vwtype='onbep',pdtype='pron',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,dial)',[pt='vnw',vwtype='onbep',pdtype='det',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('LID(bep,dial)',[pt='lid',lwtype='bep',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('LID(onbep,dial)',[pt='lid',lwtype='onbep',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VZ(init,dial)',[pt='vz',vztype='init',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VZ(fin,dial)',[pt='vz',vztype='fin',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VG(neven,dial)',[pt='vg',conjtype='neven',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('VG(onder,dial)',[pt='vg',conjtype='onder',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('BW(dial)',[pt='bw',dial='dial'|AttVal],AttVal).
lassy_postag_atts_('TSW()',[pt='tsw'|AttVal],AttVal).
lassy_postag_atts_('SPEC(afgebr)',[pt='spec',spectype='afgebr'|AttVal],AttVal).
lassy_postag_atts_('SPEC(onverst)',[pt='spec',spectype='onverst'|AttVal],AttVal).
lassy_postag_atts_('SPEC(vreemd)',[pt='spec',spectype='vreemd'|AttVal],AttVal).
lassy_postag_atts_('SPEC(deeleigen)',[pt='spec',spectype='deeleigen'|AttVal],AttVal).
lassy_postag_atts_('SPEC(meta)',[pt='spec',spectype='meta'|AttVal],AttVal).
lassy_postag_atts_('LET()',[pt='let'|AttVal],AttVal).
lassy_postag_atts_('SPEC(comment)',[pt='spec',spectype='comment'|AttVal],AttVal).
lassy_postag_atts_('SPEC(achter)',[pt='spec',spectype='achter'|AttVal],AttVal).
lassy_postag_atts_('SPEC(afk)',[pt='spec',spectype='afk'|AttVal],AttVal).
lassy_postag_atts_('SPEC(symb)',[pt='spec',spectype='symb'|AttVal],AttVal).
lassy_postag_atts_('N(soort,ev,basis,zijd,stan)',[pt='n',ntype='soort',getal='ev',graad='basis',genus='zijd',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('N(soort,ev,basis,onz,stan)',[pt='n',ntype='soort',getal='ev',graad='basis',genus='onz',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('N(soort,ev,dim,onz,stan)',[pt='n',ntype='soort',getal='ev',graad='dim',genus='onz',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('N(soort,ev,basis,gen)',[pt='n',ntype='soort',getal='ev',graad='basis',naamval='gen'|AttVal],AttVal).
lassy_postag_atts_('N(soort,ev,dim,gen)',[pt='n',ntype='soort',getal='ev',graad='dim',naamval='gen'|AttVal],AttVal).
lassy_postag_atts_('N(soort,ev,basis,dat)',[pt='n',ntype='soort',getal='ev',graad='basis',naamval='dat'|AttVal],AttVal).
lassy_postag_atts_('N(soort,mv,basis)',[pt='n',ntype='soort',getal='mv',graad='basis'|AttVal],AttVal).
lassy_postag_atts_('N(soort,mv,dim)',[pt='n',ntype='soort',getal='mv',graad='dim'|AttVal],AttVal).
lassy_postag_atts_('N(eigen,ev,basis,zijd,stan)',[pt='n',ntype='eigen',getal='ev',graad='basis',genus='zijd',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('N(eigen,ev,basis,onz,stan)',[pt='n',ntype='eigen',getal='ev',graad='basis',genus='onz',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('N(eigen,ev,dim,onz,stan)',[pt='n',ntype='eigen',getal='ev',graad='dim',genus='onz',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('N(eigen,ev,basis,gen)',[pt='n',ntype='eigen',getal='ev',graad='basis',naamval='gen'|AttVal],AttVal).
lassy_postag_atts_('N(eigen,ev,dim,gen)',[pt='n',ntype='eigen',getal='ev',graad='dim',naamval='gen'|AttVal],AttVal).
lassy_postag_atts_('N(eigen,ev,basis,dat)',[pt='n',ntype='eigen',getal='ev',graad='basis',naamval='dat'|AttVal],AttVal).
lassy_postag_atts_('N(eigen,mv,basis)',[pt='n',ntype='eigen',getal='mv',graad='basis'|AttVal],AttVal).
lassy_postag_atts_('N(eigen,mv,dim)',[pt='n',ntype='eigen',getal='mv',graad='dim'|AttVal],AttVal).
lassy_postag_atts_('ADJ(prenom,basis,zonder)',[pt='adj',positie='prenom',graad='basis',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('ADJ(prenom,basis,met-e,stan)',[pt='adj',positie='prenom',graad='basis',buiging='met-e',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('ADJ(prenom,basis,met-e,bijz)',[pt='adj',positie='prenom',graad='basis',buiging='met-e',naamval='bijz'|AttVal],AttVal).
lassy_postag_atts_('ADJ(prenom,comp,zonder)',[pt='adj',positie='prenom',graad='comp',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('ADJ(prenom,comp,met-e,stan)',[pt='adj',positie='prenom',graad='comp',buiging='met-e',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('ADJ(prenom,comp,met-e,bijz)',[pt='adj',positie='prenom',graad='comp',buiging='met-e',naamval='bijz'|AttVal],AttVal).
lassy_postag_atts_('ADJ(prenom,sup,zonder)',[pt='adj',positie='prenom',graad='sup',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('ADJ(prenom,sup,met-e,stan)',[pt='adj',positie='prenom',graad='sup',buiging='met-e',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('ADJ(prenom,sup,met-e,bijz)',[pt='adj',positie='prenom',graad='sup',buiging='met-e',naamval='bijz'|AttVal],AttVal).
lassy_postag_atts_('ADJ(nom,basis,zonder,zonder-n)',[pt='adj',positie='nom',graad='basis',buiging='zonder','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('ADJ(nom,basis,zonder,mv-n)',[pt='adj',positie='nom',graad='basis',buiging='zonder','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('ADJ(nom,basis,met-e,zonder-n,stan)',[pt='adj',positie='nom',graad='basis',buiging='met-e','getal-n'='zonder-n',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('ADJ(nom,basis,met-e,zonder-n,bijz)',[pt='adj',positie='nom',graad='basis',buiging='met-e','getal-n'='zonder-n',naamval='bijz'|AttVal],AttVal).
lassy_postag_atts_('ADJ(nom,basis,met-e,mv-n)',[pt='adj',positie='nom',graad='basis',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('ADJ(nom,comp,zonder,zonder-n)',[pt='adj',positie='nom',graad='comp',buiging='zonder','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('ADJ(nom,comp,met-e,zonder-n,stan)',[pt='adj',positie='nom',graad='comp',buiging='met-e','getal-n'='zonder-n',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('ADJ(nom,comp,met-e,zonder-n,bijz)',[pt='adj',positie='nom',graad='comp',buiging='met-e','getal-n'='zonder-n',naamval='bijz'|AttVal],AttVal).
lassy_postag_atts_('ADJ(nom,comp,met-e,mv-n)',[pt='adj',positie='nom',graad='comp',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('ADJ(nom,sup,zonder,zonder-n)',[pt='adj',positie='nom',graad='sup',buiging='zonder','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('ADJ(nom,sup,met-e,zonder-n,stan)',[pt='adj',positie='nom',graad='sup',buiging='met-e','getal-n'='zonder-n',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('ADJ(nom,sup,met-e,zonder-n,bijz)',[pt='adj',positie='nom',graad='sup',buiging='met-e','getal-n'='zonder-n',naamval='bijz'|AttVal],AttVal).
lassy_postag_atts_('ADJ(nom,sup,met-e,mv-n)',[pt='adj',positie='nom',graad='sup',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('ADJ(postnom,basis,zonder)',[pt='adj',positie='postnom',graad='basis',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('ADJ(postnom,basis,met-s)',[pt='adj',positie='postnom',graad='basis',buiging='met-s'|AttVal],AttVal).
lassy_postag_atts_('ADJ(postnom,comp,zonder)',[pt='adj',positie='postnom',graad='comp',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('ADJ(postnom,comp,met-s)',[pt='adj',positie='postnom',graad='comp',buiging='met-s'|AttVal],AttVal).
lassy_postag_atts_('ADJ(vrij,basis,zonder)',[pt='adj',positie='vrij',graad='basis',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('ADJ(vrij,comp,zonder)',[pt='adj',positie='vrij',graad='comp',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('ADJ(vrij,sup,zonder)',[pt='adj',positie='vrij',graad='sup',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('ADJ(vrij,dim,zonder)',[pt='adj',positie='vrij',graad='dim',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('WW(pv,tgw,ev)',[pt='ww',wvorm='pv',pvtijd='tgw',pvagr='ev'|AttVal],AttVal).
lassy_postag_atts_('WW(pv,tgw,mv)',[pt='ww',wvorm='pv',pvtijd='tgw',pvagr='mv'|AttVal],AttVal).
lassy_postag_atts_('WW(pv,tgw,met-t)',[pt='ww',wvorm='pv',pvtijd='tgw',pvagr='met-t'|AttVal],AttVal).
lassy_postag_atts_('WW(pv,verl,ev)',[pt='ww',wvorm='pv',pvtijd='verl',pvagr='ev'|AttVal],AttVal).
lassy_postag_atts_('WW(pv,verl,mv)',[pt='ww',wvorm='pv',pvtijd='verl',pvagr='mv'|AttVal],AttVal).
lassy_postag_atts_('WW(pv,verl,met-t)',[pt='ww',wvorm='pv',pvtijd='verl',pvagr='met-t'|AttVal],AttVal).
lassy_postag_atts_('WW(pv,conj,ev)',[pt='ww',wvorm='pv',pvtijd='conj',pvagr='ev'|AttVal],AttVal).
lassy_postag_atts_('WW(inf,prenom,zonder)',[pt='ww',wvorm='inf',positie='prenom',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('WW(inf,prenom,met-e)',[pt='ww',wvorm='inf',positie='prenom',buiging='met-e'|AttVal],AttVal).
lassy_postag_atts_('WW(inf,nom,zonder,zonder-n)',[pt='ww',wvorm='inf',positie='nom',buiging='zonder','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('WW(inf,vrij,zonder)',[pt='ww',wvorm='inf',positie='vrij',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('WW(vd,prenom,zonder)',[pt='ww',wvorm='vd',positie='prenom',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('WW(vd,prenom,met-e)',[pt='ww',wvorm='vd',positie='prenom',buiging='met-e'|AttVal],AttVal).
lassy_postag_atts_('WW(vd,nom,met-e,zonder-n)',[pt='ww',wvorm='vd',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('WW(vd,nom,met-e,mv-n)',[pt='ww',wvorm='vd',positie='nom',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('WW(vd,vrij,zonder)',[pt='ww',wvorm='vd',positie='vrij',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('WW(od,prenom,zonder)',[pt='ww',wvorm='od',positie='prenom',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('WW(od,prenom,met-e)',[pt='ww',wvorm='od',positie='prenom',buiging='met-e'|AttVal],AttVal).
lassy_postag_atts_('WW(od,nom,met-e,zonder-n)',[pt='ww',wvorm='od',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('WW(od,nom,met-e,mv-n)',[pt='ww',wvorm='od',positie='nom',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('WW(od,vrij,zonder)',[pt='ww',wvorm='od',positie='vrij',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('TW(hoofd,prenom,stan)',[pt='tw',numtype='hoofd',positie='prenom',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('TW(hoofd,prenom,bijz)',[pt='tw',numtype='hoofd',positie='prenom',naamval='bijz'|AttVal],AttVal).
lassy_postag_atts_('TW(hoofd,nom,zonder-n,basis)',[pt='tw',numtype='hoofd',positie='nom','getal-n'='zonder-n',graad='basis'|AttVal],AttVal).
lassy_postag_atts_('TW(hoofd,nom,mv-n,basis)',[pt='tw',numtype='hoofd',positie='nom','getal-n'='mv-n',graad='basis'|AttVal],AttVal).
lassy_postag_atts_('TW(hoofd,nom,zonder-n,dim)',[pt='tw',numtype='hoofd',positie='nom','getal-n'='zonder-n',graad='dim'|AttVal],AttVal).
lassy_postag_atts_('TW(hoofd,nom,mv-n,dim)',[pt='tw',numtype='hoofd',positie='nom','getal-n'='mv-n',graad='dim'|AttVal],AttVal).
lassy_postag_atts_('TW(hoofd,vrij)',[pt='tw',numtype='hoofd',positie='vrij'|AttVal],AttVal).
lassy_postag_atts_('TW(rang,prenom,stan)',[pt='tw',numtype='rang',positie='prenom',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('TW(rang,prenom,bijz)',[pt='tw',numtype='rang',positie='prenom',naamval='bijz'|AttVal],AttVal).
lassy_postag_atts_('TW(rang,nom,zonder-n)',[pt='tw',numtype='rang',positie='nom','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('TW(rang,nom,mv-n)',[pt='tw',numtype='rang',positie='nom','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,vol,1,ev)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='vol',persoon='1',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,nadr,1,ev)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='nadr',persoon='1',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,red,1,ev)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='red',persoon='1',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,vol,1,mv)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='vol',persoon='1',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,nadr,1,mv)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='nadr',persoon='1',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,red,1,mv)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='red',persoon='1',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,vol,2v,ev)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='vol',persoon='2v',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,nadr,2v,ev)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='nadr',persoon='2v',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,red,2v,ev)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='red',persoon='2v',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,nadr,3m,ev,masc)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='nadr',persoon='3m',getal='ev',genus='masc'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,vol,3v,ev,fem)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='vol',persoon='3v',getal='ev',genus='fem'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,nadr,3v,ev,fem)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='nadr',persoon='3v',getal='ev',genus='fem'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,obl,vol,2v,ev)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='obl',status='vol',persoon='2v',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,obl,nadr,3m,ev,masc)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='obl',status='nadr',persoon='3m',getal='ev',genus='masc'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,gen,vol,1,ev)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='gen',status='vol',persoon='1',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,gen,vol,1,mv)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='gen',status='vol',persoon='1',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,gen,vol,3m,ev)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='gen',status='vol',persoon='3m',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,gen,vol,1,ev,prenom,zonder,evmo)',[pt='vnw',vwtype='bez',pdtype='det',naamval='gen',status='vol',persoon='1',getal='ev',positie='prenom',buiging='zonder',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,gen,vol,1,mv,prenom,met-e,evmo)',[pt='vnw',vwtype='bez',pdtype='det',naamval='gen',status='vol',persoon='1',getal='mv',positie='prenom',buiging='met-e',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,gen,vol,3v,ev,prenom,zonder,evmo)',[pt='vnw',vwtype='bez',pdtype='det',naamval='gen',status='vol',persoon='3v',getal='ev',positie='prenom',buiging='zonder',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,1,ev,prenom,met-e,evmo)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='1',getal='ev',positie='prenom',buiging='met-e',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,1,ev,prenom,met-e,evf)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='1',getal='ev',positie='prenom',buiging='met-e',npagr='evf'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,1,mv,prenom,met-e,evmo)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='1',getal='mv',positie='prenom',buiging='met-e',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,1,mv,prenom,met-e,evf)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='1',getal='mv',positie='prenom',buiging='met-e',npagr='evf'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,2v,ev,prenom,met-e,evf)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='2v',getal='ev',positie='prenom',buiging='met-e',npagr='evf'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,3v,ev,prenom,met-e,evmo)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='3v',getal='ev',positie='prenom',buiging='met-e',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,3v,ev,prenom,met-e,evf)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='3v',getal='ev',positie='prenom',buiging='met-e',npagr='evf'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,1,ev,nom,met-e,zonder-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='1',getal='ev',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,1,mv,nom,met-e,zonder-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='1',getal='mv',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,3m,ev,nom,met-e,zonder-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='3m',getal='ev',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,3v,ev,nom,met-e,zonder-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='3v',getal='ev',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(betr,pron,gen,vol,3o,ev)',[pt='vnw',vwtype='betr',pdtype='pron',naamval='gen',status='vol',persoon='3o',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,pron,gen,vol,3m,ev)',[pt='vnw',vwtype='aanw',pdtype='pron',naamval='gen',status='vol',persoon='3m',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,pron,gen,vol,3o,ev)',[pt='vnw',vwtype='aanw',pdtype='pron',naamval='gen',status='vol',persoon='3o',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,det,dat,prenom,met-e,evmo)',[pt='vnw',vwtype='aanw',pdtype='det',naamval='dat',positie='prenom',buiging='met-e',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,det,dat,prenom,met-e,evf)',[pt='vnw',vwtype='aanw',pdtype='det',naamval='dat',positie='prenom',buiging='met-e',npagr='evf'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,det,gen,nom,met-e,zonder-n)',[pt='vnw',vwtype='aanw',pdtype='det',naamval='gen',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,det,dat,nom,met-e,zonder-n)',[pt='vnw',vwtype='aanw',pdtype='det',naamval='dat',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,gen,prenom,met-e,mv)',[pt='vnw',vwtype='onbep',pdtype='det',naamval='gen',positie='prenom',buiging='met-e',npagr='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,dat,prenom,met-e,evmo)',[pt='vnw',vwtype='onbep',pdtype='det',naamval='dat',positie='prenom',buiging='met-e',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,dat,prenom,met-e,evf)',[pt='vnw',vwtype='onbep',pdtype='det',naamval='dat',positie='prenom',buiging='met-e',npagr='evf'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,gen,nom,met-e,mv-n)',[pt='vnw',vwtype='onbep',pdtype='det',naamval='gen',positie='nom',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,gen,nom,met-e,mv-n,basis)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='gen',positie='nom',buiging='met-e','getal-n'='mv-n',graad='basis'|AttVal],AttVal).
lassy_postag_atts_('LID(bep,stan,evon)',[pt='lid',lwtype='bep',naamval='stan',npagr='evon'|AttVal],AttVal).
lassy_postag_atts_('LID(bep,stan,rest)',[pt='lid',lwtype='bep',naamval='stan',npagr='rest'|AttVal],AttVal).
lassy_postag_atts_('LID(bep,gen,evmo)',[pt='lid',lwtype='bep',naamval='gen',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('LID(bep,dat,evmo)',[pt='lid',lwtype='bep',naamval='dat',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('LID(bep,dat,evf)',[pt='lid',lwtype='bep',naamval='dat',npagr='evf'|AttVal],AttVal).
lassy_postag_atts_('LID(bep,dat,mv)',[pt='lid',lwtype='bep',naamval='dat',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('LID(onbep,gen,evf)',[pt='lid',lwtype='onbep',naamval='gen',npagr='evf'|AttVal],AttVal).
lassy_postag_atts_('VZ(init)',[pt='vz',vztype='init'|AttVal],AttVal).
lassy_postag_atts_('VZ(fin)',[pt='vz',vztype='fin'|AttVal],AttVal).
lassy_postag_atts_('VZ(versm)',[pt='vz',vztype='versm'|AttVal],AttVal).
lassy_postag_atts_('VG(neven)',[pt='vg',conjtype='neven'|AttVal],AttVal).
lassy_postag_atts_('VG(onder)',[pt='vg',conjtype='onder'|AttVal],AttVal).
lassy_postag_atts_('BW()',[pt='bw'|AttVal],AttVal).
lassy_postag_atts_('N(soort,ev,basis,genus,stan)',[pt='n',ntype='soort',getal='ev',graad='basis',genus='genus',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('N(eigen,ev,basis,genus,stan)',[pt='n',ntype='eigen',getal='ev',graad='basis',genus='genus',naamval='stan'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,vol,2b,getal)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='vol',persoon='2b',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,nadr,2b,getal)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='nadr',persoon='2b',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,vol,2,getal)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='vol',persoon='2',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,nadr,2,getal)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='nadr',persoon='2',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,red,2,getal)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='red',persoon='2',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,vol,3,ev,masc)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='vol',persoon='3',getal='ev',genus='masc'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,vol,3,ev,zijd)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='vol',persoon='3',getal='ev',genus='zijd'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,red,3,ev,masc)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='red',persoon='3',getal='ev',genus='masc'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,red,3p,ev,masc)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='red',persoon='3p',getal='ev',genus='masc'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,vol,3p,mv)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='vol',persoon='3p',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,nomin,nadr,3p,mv)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='nomin',status='nadr',persoon='3p',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,obl,vol,3,ev,masc)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='obl',status='vol',persoon='3',getal='ev',genus='masc'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,obl,vol,3,ev,zijd)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='obl',status='vol',persoon='3',getal='ev',genus='zijd'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,obl,red,3,ev,masc)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='obl',status='red',persoon='3',getal='ev',genus='masc'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,obl,vol,3,getal,fem)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='obl',status='vol',persoon='3',getal='getal',genus='fem'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,obl,nadr,3v,getal,fem)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='obl',status='nadr',persoon='3v',getal='getal',genus='fem'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,obl,red,3v,getal,fem)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='obl',status='red',persoon='3v',getal='getal',genus='fem'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,obl,vol,3p,mv)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='obl',status='vol',persoon='3p',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,obl,nadr,3p,mv)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='obl',status='nadr',persoon='3p',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,stan,nadr,2v,mv)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='stan',status='nadr',persoon='2v',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,stan,red,3,ev,onz)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='stan',status='red',persoon='3',getal='ev',genus='onz'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,stan,red,3,ev,fem)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='stan',status='red',persoon='3',getal='ev',genus='fem'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,stan,red,3,mv)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='stan',status='red',persoon='3',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,gen,vol,2,getal)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='gen',status='vol',persoon='2',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,gen,vol,3v,getal)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='gen',status='vol',persoon='3v',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(pers,pron,gen,vol,3p,mv)',[pt='vnw',vwtype='pers',pdtype='pron',naamval='gen',status='vol',persoon='3p',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(pr,pron,obl,vol,1,ev)',[pt='vnw',vwtype='pr',pdtype='pron',naamval='obl',status='vol',persoon='1',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(pr,pron,obl,nadr,1,ev)',[pt='vnw',vwtype='pr',pdtype='pron',naamval='obl',status='nadr',persoon='1',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(pr,pron,obl,red,1,ev)',[pt='vnw',vwtype='pr',pdtype='pron',naamval='obl',status='red',persoon='1',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(pr,pron,obl,vol,1,mv)',[pt='vnw',vwtype='pr',pdtype='pron',naamval='obl',status='vol',persoon='1',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(pr,pron,obl,nadr,1,mv)',[pt='vnw',vwtype='pr',pdtype='pron',naamval='obl',status='nadr',persoon='1',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(pr,pron,obl,red,2v,getal)',[pt='vnw',vwtype='pr',pdtype='pron',naamval='obl',status='red',persoon='2v',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(pr,pron,obl,nadr,2v,getal)',[pt='vnw',vwtype='pr',pdtype='pron',naamval='obl',status='nadr',persoon='2v',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(pr,pron,obl,vol,2,getal)',[pt='vnw',vwtype='pr',pdtype='pron',naamval='obl',status='vol',persoon='2',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(pr,pron,obl,nadr,2,getal)',[pt='vnw',vwtype='pr',pdtype='pron',naamval='obl',status='nadr',persoon='2',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(refl,pron,obl,red,3,getal)',[pt='vnw',vwtype='refl',pdtype='pron',naamval='obl',status='red',persoon='3',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(refl,pron,obl,nadr,3,getal)',[pt='vnw',vwtype='refl',pdtype='pron',naamval='obl',status='nadr',persoon='3',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(recip,pron,obl,vol,persoon,mv)',[pt='vnw',vwtype='recip',pdtype='pron',naamval='obl',status='vol',persoon='persoon',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(recip,pron,gen,vol,persoon,mv)',[pt='vnw',vwtype='recip',pdtype='pron',naamval='gen',status='vol',persoon='persoon',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,1,ev,prenom,zonder,agr)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='1',getal='ev',positie='prenom',buiging='zonder',npagr='agr'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,1,ev,prenom,met-e,rest)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='1',getal='ev',positie='prenom',buiging='met-e',npagr='rest'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,red,1,ev,prenom,zonder,agr)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='red',persoon='1',getal='ev',positie='prenom',buiging='zonder',npagr='agr'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,1,mv,prenom,zonder,evon)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='1',getal='mv',positie='prenom',buiging='zonder',npagr='evon'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,1,mv,prenom,met-e,rest)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='1',getal='mv',positie='prenom',buiging='met-e',npagr='rest'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,2,getal,prenom,zonder,agr)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='2',getal='getal',positie='prenom',buiging='zonder',npagr='agr'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,2,getal,prenom,met-e,rest)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='2',getal='getal',positie='prenom',buiging='met-e',npagr='rest'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,2v,ev,prenom,zonder,agr)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='2v',getal='ev',positie='prenom',buiging='zonder',npagr='agr'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='red',persoon='2v',getal='ev',positie='prenom',buiging='zonder',npagr='agr'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,nadr,2v,mv,prenom,zonder,agr)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='nadr',persoon='2v',getal='mv',positie='prenom',buiging='zonder',npagr='agr'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='3',getal='ev',positie='prenom',buiging='zonder',npagr='agr'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,3m,ev,prenom,met-e,rest)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='3m',getal='ev',positie='prenom',buiging='met-e',npagr='rest'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,3v,ev,prenom,met-e,rest)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='3v',getal='ev',positie='prenom',buiging='met-e',npagr='rest'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='red',persoon='3',getal='ev',positie='prenom',buiging='zonder',npagr='agr'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,3,mv,prenom,zonder,agr)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='3',getal='mv',positie='prenom',buiging='zonder',npagr='agr'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,3p,mv,prenom,met-e,rest)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='3p',getal='mv',positie='prenom',buiging='met-e',npagr='rest'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,red,3,getal,prenom,zonder,agr)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='red',persoon='3',getal='getal',positie='prenom',buiging='zonder',npagr='agr'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,gen,vol,1,ev,prenom,met-e,rest3)',[pt='vnw',vwtype='bez',pdtype='det',naamval='gen',status='vol',persoon='1',getal='ev',positie='prenom',buiging='met-e',npagr='rest3'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,gen,vol,1,mv,prenom,met-e,rest3)',[pt='vnw',vwtype='bez',pdtype='det',naamval='gen',status='vol',persoon='1',getal='mv',positie='prenom',buiging='met-e',npagr='rest3'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,gen,vol,2,getal,prenom,zonder,evmo)',[pt='vnw',vwtype='bez',pdtype='det',naamval='gen',status='vol',persoon='2',getal='getal',positie='prenom',buiging='zonder',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,gen,vol,2,getal,prenom,met-e,rest3)',[pt='vnw',vwtype='bez',pdtype='det',naamval='gen',status='vol',persoon='2',getal='getal',positie='prenom',buiging='met-e',npagr='rest3'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,gen,vol,2v,ev,prenom,met-e,rest3)',[pt='vnw',vwtype='bez',pdtype='det',naamval='gen',status='vol',persoon='2v',getal='ev',positie='prenom',buiging='met-e',npagr='rest3'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,gen,vol,3,ev,prenom,zonder,evmo)',[pt='vnw',vwtype='bez',pdtype='det',naamval='gen',status='vol',persoon='3',getal='ev',positie='prenom',buiging='zonder',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,gen,vol,3,ev,prenom,met-e,rest3)',[pt='vnw',vwtype='bez',pdtype='det',naamval='gen',status='vol',persoon='3',getal='ev',positie='prenom',buiging='met-e',npagr='rest3'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,gen,vol,3v,ev,prenom,met-e,rest3)',[pt='vnw',vwtype='bez',pdtype='det',naamval='gen',status='vol',persoon='3v',getal='ev',positie='prenom',buiging='met-e',npagr='rest3'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,gen,vol,3p,mv,prenom,zonder,evmo)',[pt='vnw',vwtype='bez',pdtype='det',naamval='gen',status='vol',persoon='3p',getal='mv',positie='prenom',buiging='zonder',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,gen,vol,3p,mv,prenom,met-e,rest3)',[pt='vnw',vwtype='bez',pdtype='det',naamval='gen',status='vol',persoon='3p',getal='mv',positie='prenom',buiging='met-e',npagr='rest3'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,2,getal,prenom,met-e,evmo)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='2',getal='getal',positie='prenom',buiging='met-e',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,2,getal,prenom,met-e,evf)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='2',getal='getal',positie='prenom',buiging='met-e',npagr='evf'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,3,ev,prenom,met-e,evmo)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='3',getal='ev',positie='prenom',buiging='met-e',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,3,ev,prenom,met-e,evf)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='3',getal='ev',positie='prenom',buiging='met-e',npagr='evf'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,3p,mv,prenom,met-e,evmo)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='3p',getal='mv',positie='prenom',buiging='met-e',npagr='evmo'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,3p,mv,prenom,met-e,evf)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='3p',getal='mv',positie='prenom',buiging='met-e',npagr='evf'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,1,ev,nom,met-e,zonder-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='1',getal='ev',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,1,mv,nom,met-e,zonder-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='1',getal='mv',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,2,getal,nom,met-e,zonder-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='2',getal='getal',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,2v,ev,nom,met-e,zonder-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='2v',getal='ev',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,3m,ev,nom,met-e,zonder-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='3m',getal='ev',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,3v,ev,nom,met-e,zonder-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='3v',getal='ev',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,3p,mv,nom,met-e,zonder-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='3p',getal='mv',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,1,ev,nom,met-e,mv-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='1',getal='ev',positie='nom',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,1,mv,nom,met-e,mv-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='1',getal='mv',positie='nom',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,1,mv,nom,met-e,getal-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='1',getal='mv',positie='nom',buiging='met-e','getal-n'='getal-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,2,getal,nom,met-e,mv-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='2',getal='getal',positie='nom',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,2v,ev,nom,met-e,mv-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='2v',getal='ev',positie='nom',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,3m,ev,nom,met-e,mv-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='3m',getal='ev',positie='nom',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,3v,ev,nom,met-e,mv-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='3v',getal='ev',positie='nom',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,stan,vol,3p,mv,nom,met-e,mv-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='stan',status='vol',persoon='3p',getal='mv',positie='nom',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,2,getal,nom,met-e,zonder-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='2',getal='getal',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(bez,det,dat,vol,3p,mv,nom,met-e,zonder-n)',[pt='vnw',vwtype='bez',pdtype='det',naamval='dat',status='vol',persoon='3p',getal='mv',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(vrag,pron,stan,nadr,3o,ev)',[pt='vnw',vwtype='vrag',pdtype='pron',naamval='stan',status='nadr',persoon='3o',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(betr,pron,stan,vol,persoon,getal)',[pt='vnw',vwtype='betr',pdtype='pron',naamval='stan',status='vol',persoon='persoon',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(betr,pron,stan,vol,3,ev)',[pt='vnw',vwtype='betr',pdtype='pron',naamval='stan',status='vol',persoon='3',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(betr,det,stan,nom,zonder,zonder-n)',[pt='vnw',vwtype='betr',pdtype='det',naamval='stan',positie='nom',buiging='zonder','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(betr,det,stan,nom,met-e,zonder-n)',[pt='vnw',vwtype='betr',pdtype='det',naamval='stan',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(betr,pron,gen,vol,3o,getal)',[pt='vnw',vwtype='betr',pdtype='pron',naamval='gen',status='vol',persoon='3o',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(vb,pron,stan,vol,3p,getal)',[pt='vnw',vwtype='vb',pdtype='pron',naamval='stan',status='vol',persoon='3p',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(vb,pron,dat,vol,3p,getal)',[pt='vnw',vwtype='vb',pdtype='pron',naamval='dat',status='vol',persoon='3p',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(vb,pron,stan,vol,3o,ev)',[pt='vnw',vwtype='vb',pdtype='pron',naamval='stan',status='vol',persoon='3o',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(vb,pron,gen,vol,3m,ev)',[pt='vnw',vwtype='vb',pdtype='pron',naamval='gen',status='vol',persoon='3m',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(vb,pron,gen,vol,3v,ev)',[pt='vnw',vwtype='vb',pdtype='pron',naamval='gen',status='vol',persoon='3v',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(vb,pron,gen,vol,3p,mv)',[pt='vnw',vwtype='vb',pdtype='pron',naamval='gen',status='vol',persoon='3p',getal='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(vb,adv-pron,obl,vol,3o,getal)',[pt='vnw',vwtype='vb',pdtype='adv-pron',naamval='obl',status='vol',persoon='3o',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(excl,pron,stan,vol,3,getal)',[pt='vnw',vwtype='excl',pdtype='pron',naamval='stan',status='vol',persoon='3',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(vb,det,stan,prenom,zonder,evon)',[pt='vnw',vwtype='vb',pdtype='det',naamval='stan',positie='prenom',buiging='zonder',npagr='evon'|AttVal],AttVal).
lassy_postag_atts_('VNW(vb,det,stan,prenom,met-e,rest)',[pt='vnw',vwtype='vb',pdtype='det',naamval='stan',positie='prenom',buiging='met-e',npagr='rest'|AttVal],AttVal).
lassy_postag_atts_('VNW(vb,det,stan,nom,met-e,zonder-n)',[pt='vnw',vwtype='vb',pdtype='det',naamval='stan',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(excl,det,stan,vrij,zonder)',[pt='vnw',vwtype='excl',pdtype='det',naamval='stan',positie='vrij',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,pron,stan,vol,3o,ev)',[pt='vnw',vwtype='aanw',pdtype='pron',naamval='stan',status='vol',persoon='3o',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,pron,stan,nadr,3o,ev)',[pt='vnw',vwtype='aanw',pdtype='pron',naamval='stan',status='nadr',persoon='3o',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,pron,stan,vol,3,getal)',[pt='vnw',vwtype='aanw',pdtype='pron',naamval='stan',status='vol',persoon='3',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,adv-pron,obl,vol,3o,getal)',[pt='vnw',vwtype='aanw',pdtype='adv-pron',naamval='obl',status='vol',persoon='3o',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,adv-pron,stan,red,3,getal)',[pt='vnw',vwtype='aanw',pdtype='adv-pron',naamval='stan',status='red',persoon='3',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,det,stan,prenom,zonder,evon)',[pt='vnw',vwtype='aanw',pdtype='det',naamval='stan',positie='prenom',buiging='zonder',npagr='evon'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,det,stan,prenom,zonder,rest)',[pt='vnw',vwtype='aanw',pdtype='det',naamval='stan',positie='prenom',buiging='zonder',npagr='rest'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,det,stan,prenom,zonder,agr)',[pt='vnw',vwtype='aanw',pdtype='det',naamval='stan',positie='prenom',buiging='zonder',npagr='agr'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,det,stan,prenom,met-e,rest)',[pt='vnw',vwtype='aanw',pdtype='det',naamval='stan',positie='prenom',buiging='met-e',npagr='rest'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,det,gen,prenom,met-e,rest3)',[pt='vnw',vwtype='aanw',pdtype='det',naamval='gen',positie='prenom',buiging='met-e',npagr='rest3'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,det,stan,nom,met-e,zonder-n)',[pt='vnw',vwtype='aanw',pdtype='det',naamval='stan',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,det,stan,nom,met-e,mv-n)',[pt='vnw',vwtype='aanw',pdtype='det',naamval='stan',positie='nom',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(aanw,det,stan,vrij,zonder)',[pt='vnw',vwtype='aanw',pdtype='det',naamval='stan',positie='vrij',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,pron,stan,vol,3p,ev)',[pt='vnw',vwtype='onbep',pdtype='pron',naamval='stan',status='vol',persoon='3p',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,pron,stan,vol,3o,ev)',[pt='vnw',vwtype='onbep',pdtype='pron',naamval='stan',status='vol',persoon='3o',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,pron,gen,vol,3p,ev)',[pt='vnw',vwtype='onbep',pdtype='pron',naamval='gen',status='vol',persoon='3p',getal='ev'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,adv-pron,obl,vol,3o,getal)',[pt='vnw',vwtype='onbep',pdtype='adv-pron',naamval='obl',status='vol',persoon='3o',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,adv-pron,gen,red,3,getal)',[pt='vnw',vwtype='onbep',pdtype='adv-pron',naamval='gen',status='red',persoon='3',getal='getal'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,stan,prenom,zonder,evon)',[pt='vnw',vwtype='onbep',pdtype='det',naamval='stan',positie='prenom',buiging='zonder',npagr='evon'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,stan,prenom,zonder,agr)',[pt='vnw',vwtype='onbep',pdtype='det',naamval='stan',positie='prenom',buiging='zonder',npagr='agr'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,stan,prenom,met-e,evz)',[pt='vnw',vwtype='onbep',pdtype='det',naamval='stan',positie='prenom',buiging='met-e',npagr='evz'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,stan,prenom,met-e,mv)',[pt='vnw',vwtype='onbep',pdtype='det',naamval='stan',positie='prenom',buiging='met-e',npagr='mv'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,stan,prenom,met-e,rest)',[pt='vnw',vwtype='onbep',pdtype='det',naamval='stan',positie='prenom',buiging='met-e',npagr='rest'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,stan,prenom,met-e,agr)',[pt='vnw',vwtype='onbep',pdtype='det',naamval='stan',positie='prenom',buiging='met-e',npagr='agr'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,prenom,zonder,agr,basis)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='stan',positie='prenom',buiging='zonder',npagr='agr',graad='basis'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,prenom,met-e,agr,basis)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='stan',positie='prenom',buiging='met-e',npagr='agr',graad='basis'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,prenom,met-e,mv,basis)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='stan',positie='prenom',buiging='met-e',getal='mv',graad='basis'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,prenom,zonder,agr,comp)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='stan',positie='prenom',buiging='zonder',npagr='agr',graad='comp'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,prenom,met-e,agr,sup)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='stan',positie='prenom',buiging='met-e',npagr='agr',graad='sup'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,prenom,met-e,agr,comp)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='stan',positie='prenom',buiging='met-e',npagr='agr',graad='comp'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,stan,nom,met-e,mv-n)',[pt='vnw',vwtype='onbep',pdtype='det',naamval='stan',positie='nom',buiging='met-e','getal-n'='mv-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,stan,nom,met-e,zonder-n)',[pt='vnw',vwtype='onbep',pdtype='det',naamval='stan',positie='nom',buiging='met-e','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,stan,nom,zonder,zonder-n)',[pt='vnw',vwtype='onbep',pdtype='det',naamval='stan',positie='nom',buiging='zonder','getal-n'='zonder-n'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='stan',positie='nom',buiging='met-e','getal-n'='zonder-n',graad='basis'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,nom,met-e,mv-n,basis)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='stan',positie='nom',buiging='met-e','getal-n'='mv-n',graad='basis'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,nom,met-e,zonder-n,sup)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='stan',positie='nom',buiging='met-e','getal-n'='zonder-n',graad='sup'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,nom,met-e,mv-n,sup)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='stan',positie='nom',buiging='met-e','getal-n'='mv-n',graad='sup'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,nom,zonder,mv-n,dim)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='stan',positie='nom',buiging='zonder','getal-n'='mv-n',graad='dim'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,det,stan,vrij,zonder)',[pt='vnw',vwtype='onbep',pdtype='det',naamval='stan',positie='vrij',buiging='zonder'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,vrij,zonder,basis)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='stan',positie='vrij',buiging='zonder',graad='basis'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,vrij,zonder,sup)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='stan',positie='vrij',buiging='zonder',graad='sup'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,vrij,zonder,comp)',[pt='vnw',vwtype='onbep',pdtype='grad',naamval='stan',positie='vrij',buiging='zonder',graad='comp'|AttVal],AttVal).
lassy_postag_atts_('LID(bep,gen,rest3)',[pt='lid',lwtype='bep',naamval='gen',npagr='rest3'|AttVal],AttVal).
lassy_postag_atts_('LID(onbep,stan,agr)',[pt='lid',lwtype='onbep',naamval='stan',npagr='agr'|AttVal],AttVal).

%% later additions for LASSY Small
lassy_postag_atts_('SPEC(enof)',[pt='spec',spectype='enof'|AttVal],AttVal).
lassy_postag_atts_('VNW(onbep,grad,stan,nom,zonder,zonder-n,sup)',[pt=vnw,vwtype=onbep,pdtype=grad,naamval=stan,positie=nom,buiging=zonder,'getal-n'='zonder-n',graad=sup|AttVal],AttVal).

