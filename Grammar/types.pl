%%           -*-Mode: prolog;-*-

:- module(alpino_types, []).

:- expects_dialect(sicstus).

:- discontiguous
    at/1,
    type/3.

%% no extra argument position to illustrate difference 
%% between intensional and extensional equality.  
%% This way the datastructures are as small as possible.

extensional(_).

%% list type is built-in, but this defines the name of the attributes
%% to refer to the head and tail of the list respectively. 
list_type(h,t).


%% top of the type hierarchy
top(				% first all categories %
     [ v, vb, vc, vproj, v2_vp, vp, vpx,
       aan_het_comp, op_comp, uit_comp, te_comp,
       v_noun, vc_noun, vpx_noun,
       comparative, comparativep,
       det, tmp_det, gen_det,
       n, iets_n, pn, np, pron, 
       app_n, app_np, tmp_app_n, start_app_n_app,app_n_app, app_np_mod,
       a, zom_adv, zom_a, hoe_adv,
       p, pp, abs_p,
       pred,

       call, get_val,put_val,

       left, right,
       num, pre_det_quant, post_n_n, enumeration,
       int_adv,
       post_adv_adv, predm_adv, eenmaal_adv,predm_adv_num, pre_num_adv,
       post_adj_adv, post_wh_adv, pre_wh_adv, post_p_adv, post_loc_adv_adv,
       post_np_adv, post_n_adv, modal_adv, adv, dip_adv, denk_ik,
       vandaar_adv,zo_van_adv, post_p,post_pp,
       root, sv1, sbar, redrel, rel, comp, start, tag, max, rootbar, iets_adv,
       score_cat, pre_np_adv, imp, etopic, ecomp, top_cat, within_word_conjunct,
       num_na,
       part,
       modifier, 
       punct, optpunct,

       veps,

       vslash,
       
       yes,no,
       pp_obj1,pp_pobj1,pp_se,
       ypred,npred,
       clist, last, etc, complex_etc,lconj, conj,
       fixed_part,
       dip,
       no_cat,
       preptype,

       mexs_cat,
       nwh,rywh,

       control_subj, control_obj, control_obj2, control_so, control_none,

       

       hwrd,
       act,pas,obj1_pas,refl_pas,so_pas,sbar_subj_te_pas,norm_pas,sbar_pas, % act
       past,present,subjunctive,% tense
       dt,			% CGN-style DEPENDENCY TREES

       tags_el,

       
       no_passive, norm_passive,imp_passive,

       opt_het, obl_het,
       
       'LOC','ORG', 'TEMP','PER','AMOUNT',

       robust,

       result,tree		% for pretty printing of data-structure
                                % constructed in robustness/parsing
   ]).

%% verbal categories
%%  v: verb in the lexicon
%% vb: either a v or a part+v combination. Needed since you can't have
%%     'ik opbel marie' whereas you can have 'dat ik marie opgebeld heb
%% vc: needed to distinguish left and right daughters in right-braching
%%     verb-clusters 
%% vproj: projections of vc including np, pp, sbar arguments/modifiers.
%% vpx: project of vproj, can combine with `extraposed' stuff to the right
%% vp: saturated verb-phrase, no vslash.
%% v2_vp: saturated verb-phrase in verb-second constructions
%%
%% EXAMPLE:
%%                                    root                                     
%%  _________________________________________                                  
%%  |  |                                    |                                  
%% np  v                                  v2_vp
%%                                          |
%%  |  |                                   vpx
%% ik zou                                   |
%%                                        vproj
%%                             ___________________________________             
%%                             |                                  |            
%%                           vproj                            modifier         
%%         ______________________                                              
%%         |                    |                                |            
%%        np                  vproj                             vp            
%%                ___________________                 ______________          
%%         |      |                  |                |            |          
%%       arie modifier             vproj            comp         vproj        
%%                           _______________                  ___________     
%%                |          |             |          |      |          |     
%%               adv        vb             vc        om      np       vproj   
%%                        ______     ________              _____              
%%                |      |      |   |       |             |    |        |     
%%              niet    part   vb   v       vc           det   n        vc    
%%                                       _______                              
%%                       |      |        |      |         |    |        |     
%%                     terug    v        v     vc        de  trein      vb    
%%                              |        |      |                       |     
%%                           gebeld   willen    v                       v     
%%                                              |                       |     
%%                                           hebben                [te,missen]


%% subj: agreement; maps to deps (for finite verbs)
%% e(xternal)_deps: list; for finite verbs empty, for infinite verbs link
%% to subj for control.
%%
%% difference motivated because of coordination:
%%
%% dat [ik_i vertrek] en [jij_j vertrekt]
%% *no* unification of subj
%%
%% om [PRO_i te vertrekken] en [PRO_i te slapen]
%% *yes* unification of external_arg

type(Cat,[],Atts) :-
    cat_type(Cat,Atts0),
    lists:append(Atts0,
                 [dt,ccat0,ccat,capps,apps,
                  cdets,dets,cmods,mods,cpredms,predms],
                 Atts).

cat_type(v_noun,  [hstem,subn,lex,sc,parts,cj]).
cat_type(vc_noun, [hstem,vc,hasmod,haspre2,exs,mexs,subn,sc,cj]).
cat_type(vpx_noun,[hstem,exs,mexs,subn,sc,cj]).
cat_type(v,       [inv_agr,deps,sc,parts,vform,slash,vslash,subj,imper,
                   pro_deps,e_deps,sv1_mod,cleft,v_noun,
                   vframe,adj_agr,hstem,psprule,fixed_dep,
                   hebben_zijn,act,tense,vtype,exs,mexs,eps1,eps2,eps3,
		   inv,passive,cj,tpart]).
cat_type(vb,      [vtype,inv,deps,sc,parts,vform,slash,vslash,subj,
                   pro_deps,e_deps,vframe,adj_agr,hstem,fixed_dep,vbc,
                   hebben_zijn,act,tense,exs,mexs,eps1,eps2,eps3,tpart,cleft,
		   passive,cj]).
cat_type(vc,      [vtype,vframe,sel,vbc,sc,parts,vform,slash,vslash,pspslash,
                   psprule,subj,fixed_dep,cleft,
                   pro_deps,e_deps,hstem,hebben_zijn,exs,mexs,eps1,eps2,eps3,
                   cj,tpart,
		   passive]).
cat_type(vproj,   [vc,haspre,haspre2,tags,sc,exs,mexs,vform,slash,vslash,subj,
                   rightx,realized_comma,pro_deps,e_deps,eps1,eps2,haswh,hasmod,
                   eps3,mf,cj,cleft]).
cat_type(vp,      [ctype,control,cj,vform,slash,subj,subj_nform,haswh,
		   pro_deps,e_deps,mf,sel,tags,cleft]).
cat_type(vpx,     [rightx,cleft,eps3,haswh,
                   rightm,cj,vform,vslash,tags,slash,subj,pro_deps,e_deps,mf,
                   exs,mexs]).
cat_type(v2_vp,   [vslash,haswh,slash,subj,mf,tags]).
cat_type(num,     [cj,agr,pro,app]).
cat_type(pre_det_quant,[agr]).
cat_type(pred,    [slash,exs,mexs,e_deps,wh,wh_reltmploc,subn,redrel,deverbal,
                   avform,pcat,cform,hstem,nhstem,wkpro,sel,cj]).
cat_type(conj,    [left_conj,cform]).
cat_type(lconj,   [needs_right_cform]).
cat_type(root,    [question,allows_root_imp,puncttype,
                   subj,needs_dip,cj,tags,topic_hstem]).
cat_type(start,   [sv1,branch,puncttype]).
cat_type(max,     [cj,sv1,max_type,mod_np,puncttype]).
cat_type(top_cat, []).
cat_type(sv1,     [iexs,imexs,subj,haswh,slash,tense,cj,sv1_mod,tags,can_be_max]).
cat_type(imp,     [cj,tags]).
cat_type(dip,     []).
cat_type(n,       [wh,wh_reltmploc,case,meas,subn,bmeas,hdet,exs,mexs,haspre,
                   neclass,can_mod,has_app2,
                   pn,agr,e_agr,lex,nform,e_deps,pred,rightm,cj,hstem,amount]).
cat_type(app_n,             [agr,bmeas,meas,amount,nform,e_agr,can_mod]).
cat_type(app_n_app,         [agr,bmeas,meas,amount,nform,e_agr,can_mod]).
cat_type(start_app_n_app,   [agr,bmeas,meas,amount,nform,e_agr,can_mod]).
cat_type(app_np_mod,        [agr,bmeas,meas,amount,nform,e_agr,can_mod]).
cat_type(app_np,  [hstem,agr,meas,nform,e_agr]).
cat_type(tmp_app_n,[agr]).
cat_type(iets_n,  [aform,wh,wh_reltmploc,agr]).
cat_type(aan_het_comp, []).
cat_type(op_comp, []).
cat_type(uit_comp,[]).
cat_type(te_comp, []).
cat_type(pn,      [lex,neclass,agr,cj]).
cat_type(np,      [agr,e_agr,prs,case,wh,wh_reltmploc,meas,nform,parg,
                   wkpro,redrel,can_mod,hdet,
                   neclass,cleft_has_rel,allows_drop,
                   sel,exs,mexs,e_deps,passivizes,subn,has_app,has_app2,pred,
                   hstem,bmeas,cj]).
cat_type(pron,    [agr,prs,hstem,
                   case,wh,wh_reltmploc,nform,parg,wkpro,subn,exs]).
cat_type(det,     [neclass,agr,hstem,case,wh,wh_reltmploc,name,nsubn,
                   subn,modf,nform,sc,pro,parg,has_obcomp,
                   wkpro,exs,cj]).
cat_type(gen_det, [agr]).
cat_type(tmp_det, []).
cat_type(a,       [haspre,adv,slash,e_deps,sc,can_nominalize,can_marked_attr,
                   lex,can_postv,can_postn_with_cform,can_postn,has_obcomp,
                   me_adj,agr,aform,avform,cform,cj,wh,wh_reltmploc,hstem,
                   hasextra,deverbal,exs,tmploc,modifies_a,modifies_v]).
cat_type(p,       [cj,sc,wh,wh_reltmploc,slash,prep,preptype,
                   ppost,nhstem,nnform,pp_role,nagr,ndt,ld_pc]).
cat_type(abs_p,   [sc]).
cat_type(pp,      [hcj,wh,wh_reltmploc,haspart,slash,exs,mexs,prep,preptype,pp_er,
                   sel,vc,allow_modal,pp_role,vhasmod,veps,
                   e_deps,nhstem,nnform,ndt,nagr,cj,ld_pc]).
cat_type(tag,     [adv,np]).
cat_type(enumeration,[cj]).
cat_type(adv,     [haspre,tmploc,redrel,mexs,exs,wh,hstem,wh_reltmploc,cj,pron,
                   nsubn,subadv,can_postv,adj_can_postv,agr,modifies_a,modifies_v,sel,wk]).
cat_type(int_adv, [agr,sc,exs]).
cat_type(dip_adv, []).
cat_type(denk_ik, []).
cat_type(modal_adv,[modal_adv_arg]).
cat_type(vandaar_adv,[]).
cat_type(zo_van_adv, []).
cat_type(post_np_adv,[]).
cat_type(post_n_n,   []).
cat_type(pre_num_adv,[agr]).
cat_type(post_n_adv, [cj]).
cat_type(post_p_adv,   []).
cat_type(post_p,       [sc]).
cat_type(post_pp,      []).
cat_type(post_wh_adv,  []).
cat_type(pre_wh_adv,   []).
cat_type(post_adv_adv, []).
cat_type(post_loc_adv_adv, []).
cat_type(hoe_adv,      []).
cat_type(post_adj_adv, [exs]).
cat_type(predm_adv,    []).
cat_type(eenmaal_adv,  []).
cat_type(predm_adv_num,[]).
cat_type(zom_adv,      []).
cat_type(num_na,       []).
cat_type(zom_a,        [agr,aform,cform]).
cat_type(iets_adv,     [wh,wh_reltmploc]).
cat_type(score_cat,    []).
cat_type(pre_np_adv,   []).
cat_type(sbar,         [wh,e_deps,tmploc,ctype,slash,sel,tags,subj_nform,cj]).
cat_type(redrel,       [sub_wh,haswh,e_deps,tmploc,agr,relhd]).
cat_type(rootbar,      []).
cat_type(rel,          [hstem,nform,tmploc,relform,agr,sel,cj]).
cat_type(comp,         [e_deps,ctype,hstem,sc,wh,wh_reltmploc,tags,cj,slash]).
cat_type(comparative,  [sc,compar_form]).
cat_type(comparativep, [compar_form]).
cat_type(part,         [cj,part]).
cat_type(modifier,     [vc,wh,wh_reltmploc,cj,hstem,exs,mexs,eps,mcat,sel,modifies_v]).
cat_type(within_word_conjunct,[]).
cat_type(etc,          []).	% end conjunction
cat_type(complex_etc,  []).     % end conjunction
cat_type(fixed_part,   [cat,sel,words]).

type(etopic,  [], []).
type(ecomp,   [], []).
type(punct,   [], [puncttype]).
type(optpunct,[], [cat,realized]).
type(no_cat,  [], []).
type(last,    [], [adv,subn,ccat0,ccat,prep,part,vframe,ctype,tmploc,
                    deverbal,pcat,can_postn,wh_reltmploc,cform,tags,hstem]).
type(clist,   [], [left_conj,etc,last,enzlast,cat,cats,conj,cform,within_word]).

%% only for printing the internal datastructure of objects..
type(result,[],[score,string,cat,tree,frames,cdt]).
type(tree,  [],[label,rulename,ds,features]).
type(robust,[],[list]).

%% extraposition of modifiers (relatives, pps)
type(mexs_cat,[],[mods,agr,tmploc,sel,hstem,nform,cat]).

type(tags_el,[],[dt_in,dt_out,lq]).

type(veps,[],[in,out]).

type(vslash,[],[vslashhwrd,vslashfwrd,vslashid,vslashlix,vslashcrd,vslashcnj,vslashparts,exs,mexs,cj]).

%% see Moortgat, Schuurman, vd Wouden, Syntactische Annotatie, aug 2000
%% lexicalist version
%% all head info uniformly under hwrd (head) + postag
%% including complementizers as heads of sbar
%% all modifiers uniformly under mod

%% lexical part of dependency structure
type(hwrd,[],[lexical,lemma,surface,beginpos,endpos,his,bitcode]).

type(dt,
     [],
     [hwrd,			% lexical head word +
      fwrd,                     % as hwrd, for tagger
      frame,			% name of lexical category
      cat,			% phrasal category
      ix,                       % for co-indexed nodes
      lix,                      % for co-indexed head-parts of nodes
      attrs,                    % input/output attributes for generation
      num,                      % number input/output for generation
      stype,                    % type of sentence for generation

      app,			% apposition
      body,			% rompzin
      cmp,                      % complementizer
      cnj,
      crd,
      det,			% determinator element
      dlink,                    % discourse link
      dp,			% discourse part
      hdf,			% sluitend element van circumscriptie
      ld,			% locatief of directioneel complement
      me,			% maat (duur, gewicht....) complement
      mod,			% modifier list
      mwp,                      % multi-word-part
      nucl,			% kernzin
      obcomp,                   % comparative phrase 
      obj1,			% direct object, lijdend voorwerp
      obj2,			% secundair object (indirect, experiencer,
                                % belanghebbend ...)
      pc,			% vast voorzetselvoorwerp
      pobj1,			% voorlopig direct object
      predc,			% predicatief complement
      predm,			% bep van gesteldheid `tijdens de handeling'
      rhd,			% hoofd van (hoofdloze) relatiefzin
      sat,			% satelliet: aan- of uitloop
      se,			% verplicht reflexief object
      su,			% subject, onderwerp
      sup,			% voorlopig subject
      svp,			% scheidbaar deel van werkwoord
      tag,			% aanhangsel, tussenvoegsel
      vc,			% verbaal complement
      whd			% hoofd van whq, whsub
     ]).

at(opt_het).
at(obl_het).

at(yes).
at(no).

at(pp_obj1).
at(pp_pobj1).
at(pp_se).

at('LOC').
at('ORG').
at('TEMP').
at('PER').
at('AMOUNT').

at(nwh).
type(rywh,[[rwh,ywh],[yq,nq]],[rlex,relagr,relhstem]).
at(ywh).    % questions and reduced relatives
at(rwh).    % relatives

type(yq,[[dq,iq]],[]).

at(dq).     % used as direct question
at(iq).     % used as indirect question

at(nq).     % not a question

%% tense
at(past).
at(present).
at(subjunctive).

%% mark V's whether they can occur in (impersonal) passive
at(imp_passive).
at(norm_passive).
at(no_passive).

%% passive
at(act).
at(pas).
at(norm_pas).
at(sbar_pas).
at(so_pas).
at(refl_pas).
at(obj1_pas).
at(sbar_subj_te_pas).

%% real prepositions allow +R extraction
%% some real prepositions allow +LD role
type(preptype,[[ld_prep,no_ld_prep],[r_prep,no_r_prep]],[]).

at(ld_prep).
at(no_ld_prep).

at(r_prep).
at(no_r_prep).

at(control_subj).
at(control_obj).
at(control_obj2).
at(control_so).
at(control_none).

at(left).
at(right).

at(npred).
type(ypred,[],[predp]).

type(call,[],[goal]).
type(get_val,[],[a1,a2]).
type(put_val,[],[a1,a2]).

%% vtype

boolean_type(vtype,[[vaux,vmain,vcontrol,vaanhet]]).

boolean_type(relform,[[relform_inf,
		       relform_fin,
		       relform_np
		      ]]).


%% agreement is a conjunction of number, person, gender, definiteness,
%% Number and person are relevant for subject-verb agreement.
%% Number, gender, and definiteness are relevant for np-internal
%% agreement between determiner, adjective, and noun.

boolean_type(agr,  [[sg,pl],
		    [de,het],
		    [def,indef]  % ,measdef] ??? not used ???
		   ]).

boolean_type(per,  [[fir,invje,ninvje,u,thi]]  ).


boolean_type(mcat,[[mcat_pp,mcat_adv,mcat_vp,mcat_sbar,mcat_imp,
		    mcat_redrel,mcat_rootbar,mcat_bracket,mcat_tag]]).

boolean_type(max_type,[[t_dp,t_xp,t_root,t_rootbar,t_topic_drop,t_part]]).

%% verbal inflection form
boolean_type(vform,[[fin,inf,wk_te,te,psp,om,aan_het,uit,op,pass_te]] ).

%% verbal inflection form
boolean_type(avform,[[avform_ap,        % de erg mooie oplossing
		      avform_ti,        % de niet te missen oplossing
		      avform_aan_ti,    % aan zijn houding te zien ...
		      avform_ppart,     % de gevreesde oplossing
		      avform_ppres]] ). % de werkende oplossing

%% punctuation mark
boolean_type(pun,  [[punt,dubb_punt,komma,uitroep,ampersand,staand_streep,
		     haak_open,haak_sluit,ligg_streep,hellip,schuin_streep,
		     is_gelijk,vraag,maal,punt_komma,plus,aanhaal_links,
		     aanhaal_rechts,begin_naam,end_naam]]).

boolean_type(case, [[nom,gen,obl,dat,acc]]).

boolean_type(nform,[[norm,
		     meas_mod,  % can be used as vp modifier, 'stuk/eind/meter...'
		     temp,	% can be used as vp modifiers
                     temp_meas_mod,
		                % next only for full nps:
		     refl,      % reflexives
		     year,      % 1970
		     er,
		     het_nform,
		     cleft_het,
		     none
		    ]]).

boolean_type(aform,[[attr,	% "lekkere"
		     nattr,	% "lekker"
		     iets,	% "lekkers"
		     only_n,    % "(de hoger) opgeleiden/ de eerst geborene"
		     anders     % "anders"  in ergens/iemand anders
		    ]]).

%% type of subordinate complementizer / subordinate sentence
boolean_type(ctype,[[c_dat,	 % "dat"
		     c_om,       % "om"
                     c_naar,     % consumed by "naar" complementizer
		     c_of,	 % "of"
		     c_of_short, % waarom weet ik niet
		     c_als,      % als
		     c_alsof,    % "alsof"  (het lijkt alsof)
		     c_mod,      % "omdat"
                     c_mod_np,   % inclusief NP
		     c_compare,  % "zoals"
		     c_compare_np, % opmerkingen als "..."
		     c_adv,      % "naar verluidt/zoals verwacht"
		     c_root,     % "want"
		     c_van,      % "van" hij zei van dat kun je niet maken
		     c_redrel,   % reduced relative
		     c_dip,      % quotation,
		     c_none      % e.g. "dan ik dacht t"
		    ]]).

boolean_type(tmploc,[[tmp,tmp_n,meas_n,postp,
		      loc,ld_dir,
                      ntmploc,non_adv,hoe_hoe,sent,me,str_er,wk_er]]).

boolean_type(nr_crd,[[no_crd,one_crd,of_crd,two_crd,ncj,swapped_two_crd]]).

boolean_type(yn_adv,[[yboth,nboth],[yadv,nadv,padv,oadv,detadv]]).

boolean_type(sel,[[to_left,to_right,to_topic]]).

boolean_type(pcat,[[om_pred,np_pred,ap_pred,
                    als_pred,voor_pred,pp_pred,zoals_pred]]).

boolean_type(subn,[[sub_num,sub_indef_adj,sub_def_adj,sub_rang,
                    sub_def_verb,sub_indef_verb,sub_adj_pl,
		    sub_name,sub_noun,sub_indef_pron,sub_def_pron,
                    sub_det,sub_tmp,sub_veel]]).

boolean_type(modal_adv_arg,[[adv_adv,adv_prep,adv_noun,adv_verb,adv_comp]]).

boolean_type(ld_pc,[[ld_pp,pc_pp,mod_pp,pred_pp,n_pp]]).

boolean_type(hz,[[hebben,zijn,unacc]]).

boolean_type(subadv,[[subadv_adj,subadv_adv,subadv_noun,subadv_postp]]).

boolean_type(cform,[[base, compar, super, rang]]).	% cform: groot, groter, grootst

boolean_type(stype,[[imparative, ynquestion, whquestion, declarative, topic_drop ]]).


%% Det/Pron is a weak pronoun (het, me, we, ze, je, zich)
%% no coordination: *de jongen en me
%% no comparative:  *groter dan me
%% no modification: *we op voorhand, *het (is onduidelijk) waarom het kabinet..
%%
%% normal pronouns do not allow extraposed modifiers, except for "strong"
%% pronouns such as "degene"
boolean_type(wkpro,[[strongpro,normpro,ntopicpro,weakpro]]).

boolean_type(comparform,[[cf_als,
                          cf_dan,
                          cf_e_als,
                          cf_om,
                          cf_dat]]).

:- multifile user:term_expansion/2.

:- prolog_load_context(module,Module),
    hdrug_util:set_flag(hdrug_feature_module,Module).

user:term_expansion( expand_type_compiler, List) :-
    prolog_load_context(module,Module),
    hdrug_feature:type_compiler(Module,List).

expand_type_compiler.

