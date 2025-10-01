:- expects_dialect(sicstus).

:- discontiguous
    m/3,
    m/2,
    v/7,
    v/8.

m(Stam,Cat1,WordOrWords) :-
    m(Stam,Cat0),
    t(Cat0,WordOrWords,Cat1).

% for Nicole
frames :-
    m(v_root(_,Root),verb(_,_,Sc),_),
    prettyvars(Sc),
    lists:member(El,Sc),
    format("~w ~w~n",[Root,El]).


mem_eq(W0,W) :-
    (	var(W0)
    ->	fail
    ;   W0 = [_|_]
    ->	lists:member(W,W0)
    ;   W0=W
    ).

%% verbs etc
%% normal verbs:
%% verb(FirstSg,ThirdSg,Inf,PsP,PastSg,PastPl,HebbenZijn,Subcat)
%% the first six fields are the inflected forms of the verb.
%% HebbenZijn: determines which auxiliary is used for `voltooide tijd':
%%   "jan is gekomen"
%%   "jan heeft geslapen"
%% unacc is like "zijn", but in addition passive is not allowed
%%
%% Subcat is a term indicating subcategorization frame
%% Each of the fields (except HebbenZijn) can be a list, in which case
%% each element of the list is a possibility. If a field is a variable,
%% then this is a `not exist'.
%%
%% paradigm for modals and auxiliaries "hebben" and "zijn" is different.
%% They use verb_modal/9, verb_heb/9, verb_ben/9; see the examples:
%%
%% ik  werk  ik      ben       heb      kan      wil      mag
%% je  werkt         bent      hebt     kunt     wil(t)   mag
%%     werk  je      ben       heb      kun      wil      mag
%% u   werkt u       bent/     heeft/   kunt/    wilt/    mag
%%                   ??is      hebt     ?kan     ?wil
%% het werkt         is        heeft    kan      wil      mag
%%  te werken        zijn      hebben   kunnen   willen   mogen
%%   gewerkt         geweest   gehad    gekund   gewild   gemogen
%%     werkte        was       had      kon      wilde    mocht
%%     werkten       waren     hadden   konden   wilden   mochten

t(verb(_,_,_,_,_,_,Ws,Z,Sc),      W,verb(Z,subjunctive,Sc)) :-
    mem_eq(Ws,W).

t(verb(W1,W3,_,_,_,_,_,Z,Sc),     W,verb(Z,sg1,Sc)        ):-
    mem_eq(W1,W),
    \+ mem_eq(W3,W).
    %% 'ik zet, hij zet': create an underspecified entry
    %% so this rule applies only if they are indeed different

t(verb(W1,W3,_,_,_,_,_,Z,Sc),     W,verb(Z,sg3,Sc)        ):-
    mem_eq(W3,W),
    \+ mem_eq(W1,W).

t(verb(W1,W3,_,_,_,_,_,Z,Sc),     W,verb(Z,sg,Sc)         ):-
    mem_eq(W1,W),
    mem_eq(W3,W).

t(verb(_,_,Inf,_,_,PastPl,_,Z,Sc),W,verb(Z,both(pl),Sc)   ):-
    mem_eq(Inf,W),
    mem_eq(PastPl,W).
t(verb(_,_,Inf,_,_,W0,_,Z,Sc),      W,verb(Z,past(pl),Sc)   ):-
    mem_eq(W0,W),
    \+ mem_eq(Inf,W).

t(verb(W,A,B,C),                  W,verb(A,B,C)).

t(verb(_,_,W0,_,_,_,_,Z,Sc),      W,verb(Z,inf,Sc)        ):- mem_eq(W0,W).
t(verb(_,_,inflected(EN,_),_,_,_,_,Z,Sc),
                                  W,verb(Z,inf(no_e),Sc)  ):- mem_eq(EN,W).
t(verb(_,_,inflected(_, E),_,_,_,_,Z,Sc),
                                  W,verb(Z,inf(e),Sc)     ):- mem_eq(E, W).
t(verb(_,_,_,W0,_,_,_,Z,Sc),      W,verb(Z,psp,Sc)        ):- mem_eq(W0,W).
t(verb(_,_,_,_,W0,_,_,Z,Sc),      W,verb(Z,past(sg),Sc)   ):- mem_eq(W0,W).

t(verb_heb(W0,_,_,_,_,_,_,Z,Sc),  W,verb(Z,sg1,Sc)        ):- mem_eq(W0,W).
t(verb_heb(_,W0,_,_,_,_,_,Z,Sc),  W,verb(Z,sg_hebt,Sc)    ):- mem_eq(W0,W).
t(verb_heb(_,_,W0,_,_,_,_,Z,Sc),  W,verb(Z,sg_heeft,Sc)   ):- mem_eq(W0,W).
t(verb_heb(_,_,_,W0,_,_,_,Z,Sc),  W,verb(Z,inf,Sc)        ):- mem_eq(W0,W).
t(verb_heb(_,_,_,_,W0,_,_,Z,Sc),  W,verb(Z,psp,Sc)        ):- mem_eq(W0,W).
t(verb_heb(_,_,_,_,_,W0,_,Z,Sc),  W,verb(Z,past(sg),Sc)   ):- mem_eq(W0,W).
t(verb_heb(_,_,_,_,_,_,W0,Z,Sc),  W,verb(Z,past(pl),Sc)   ):- mem_eq(W0,W).

t(verb_ben(W0,_,_,_,_,_,_,Z,Sc),  W,verb(Z,sg1,Sc)        ):- mem_eq(W0,W).
t(verb_ben(_,W0,_,_,_,_,_,Z,Sc),  W,verb(Z,sg_bent,Sc)    ):- mem_eq(W0,W).
t(verb_ben(_,_,W0,_,_,_,_,Z,Sc),  W,verb(Z,sg_is,Sc)      ):- mem_eq(W0,W).
t(verb_ben(_,_,_,W0,_,_,_,Z,Sc),  W,verb(Z,inf,Sc)        ):- mem_eq(W0,W).
t(verb_ben(_,_,_,_,W0,_,_,Z,Sc),  W,verb(Z,psp,Sc)        ):- mem_eq(W0,W).
t(verb_ben(_,_,_,_,_,W0,_,Z,Sc),  W,verb(Z,past(sg),Sc)   ):- mem_eq(W0,W).
t(verb_ben(_,_,_,_,_,_,W0,Z,Sc),  W,verb(Z,past(pl),Sc)   ):- mem_eq(W0,W).

t(verb_modal(W0,_,_,_,_,_,_,Z,Sc),W,verb(Z,modal_not_u,Sc)):- mem_eq(W0,W).
t(verb_modal(_,W0,_,_,_,_,_,Z,Sc),W,verb(Z,modal_inv,Sc)  ):- mem_eq(W0,W).
t(verb_modal(_,_,W0,_,_,_,_,Z,Sc),W,verb(Z,sg_hebt,Sc)    ):- mem_eq(W0,W).
t(verb_modal(_,_,_,W0,_,_,_,Z,Sc),W,verb(Z,inf,Sc)        ):- mem_eq(W0,W).
t(verb_modal(_,_,_,_,W0,_,_,Z,Sc),W,verb(Z,psp,Sc)        ):- mem_eq(W0,W).
t(verb_modal(_,_,_,_,_,W0,_,Z,Sc),W,verb(Z,past(sg),Sc)   ):- mem_eq(W0,W).
t(verb_modal(_,_,_,_,_,_,W0,Z,Sc),W,verb(Z,past(pl),Sc)   ):- mem_eq(W0,W).

%% special verbs

%% wees: only for imparatives, but we can't express this lexically at the
%% moment
%% weest: also for imparatives, but we don't get that now...

m(v_root(geschied,geschieden),
  verb(geschiede,unacc,subjunctive,[intransitive])).

%% het zij zo
m(v_root(ben,zijn),
  verb(zij,unacc,subjunctive,[copula,       % het zij zo
			      passive,      % dit zij hen vergeven
			      pc_pp(van),   % wat er ook van zij
			      aux_psp_zijn  % ouderwets
			     ])).

m(v_root(ben,zijn),
  verb(weze,unacc,subjunctive,[copula,     % dan weze het zo
			       passive])). % de raad weze gewaarschuwd
                                           % het weze hem gegund

m(v_root(ben,zijn),
  verb(wees,unacc,imp(sg1),[copula,      % wees niet bang
			    ld_pp])).    % wees er snel bij

m(v_root(ben,zijn),
  verb(weest,unacc,imp(modal_u),[copula])).

% m(v_root(heb,hebben),
%   verb(heb,hebben,sg1,[ninv(incorporated_subj_topic(aux_psp_hebben),
% 			    incorporated_subj_topic(aux_psp_hebben)),
% 		       ninv(incorporated_subj_topic(transitive_ndev),
% 			    incorporated_subj_topic(transitive_ndev)),
% 		       ninv(incorporated_subj_topic(nonp_pred_np_ndev),
% 			    incorporated_subj_topic(nonp_pred_np_ndev))
% 		      ])).

% m(v_root(heb,hebben),
%   verb(had,hebben,past(sg),[ninv(incorporated_subj_topic(aux_psp_hebben),
% 				 incorporated_subj_topic(aux_psp_hebben)),
% 			    ninv(incorporated_subj_topic(transitive_ndev),
% 				 incorporated_subj_topic(transitive_ndev)),
% 			    ninv(incorporated_subj_topic(nonp_pred_np_ndev),
% 				 incorporated_subj_topic(nonp_pred_np_ndev))
% 			    ])).

m(v_root(heb,hebben),
  verb(hebde,hebben,sg2,[incorporated_subj(aux_psp_hebben)])).

%% topic drop is not allowed for prs=1 in the grammar
%% therefore special frame for "vraag" to get
%% "vraag me af of hij komt"
% m(v_root(vraag,vragen),
%   verb(vraag,hebben,sg1,[ninv(part_incorporated_subj_topic(af,refl_sbar),
% 			      incorporated_subj_topic(refl_sbar))])).

% m(v_root(vraag,vragen),
%   verb(vroeg,hebben,past(sg),[ninv(part_incorporated_subj_topic(af,refl_sbar),
% 			      incorporated_subj_topic(refl_sbar))])).

%% "kom/kwam er net achter dat hij .."
% m(v_root(kom,komen),
%   verb(kom,hebben,sg1,[ninv(incorporated_subj_topic(er_pp_sbar(achter)),
% 			    incorporated_subj_topic(er_pp_sbar(achter)))])).

% m(v_root(kom,komen),
%   verb(kwam,hebben,past(sg),[ninv(incorporated_subj_topic(er_pp_sbar(achter)),
% 				  incorporated_subj_topic(er_pp_sbar(achter)))])).

%% kvind
m(v_root(vind,vinden),
  verb(kvind,hebben,sg1,[ninv(incorporated_subj_topic(pred_np),
			      incorporated_subj_topic(pred_np)),
			 ninv(incorporated_subj_topic(pred_np_sbar),
			      incorporated_subj_topic(pred_np_sbar)),
			 ninv(incorporated_subj_topic(pred_np_vp),
			      incorporated_subj_topic(pred_np_vp)),
			 ninv(incorporated_subj_topic(tr_sbar),
			     incorporated_subj_topic(tr_sbar))])).



m(v_root(vind,vinden),
  verb(kvin,hebben,sg1,[ninv(incorporated_subj_topic(pred_np),
			     incorporated_subj_topic(pred_np)),
			ninv(incorporated_subj_topic(pred_np_sbar),
			     incorporated_subj_topic(pred_np_sbar)),
			ninv(incorporated_subj_topic(pred_np_vp),
			     incorporated_subj_topic(pred_np_vp)),
			ninv(incorporated_subj_topic(tr_sbar),
			     incorporated_subj_topic(tr_sbar))])).



%% "kvoel mijn benen"
m(v_root(voel,voelen),
  verb(kvoel,hebben,sg1,[ninv(incorporated_subj_topic(pred_refl),
			      incorporated_subj_topic(pred_refl)),
			ninv(incorporated_subj_topic(transitive),
			     incorporated_subj_topic(transitive))])).

%% WHY not simply use the topic_drop analysis???
%% "voel me kut"
% m(v_root(voel,voelen),
%   verb(voel,hebben,sg1,[ninv(incorporated_subj_topic(pred_refl),
% 			     incorporated_subj_topic(pred_refl)),
% 			ninv(incorporated_subj_topic(transitive),
% 			     incorporated_subj_topic(transitive))])).

% m(v_root(voel,voelen),
%   verb(voelde,hebben,past(sg),[ninv(incorporated_subj_topic(pred_refl),
% 				     incorporated_subj_topic(pred_refl)),
% 				ninv(incorporated_subj_topic(transitive),
% 				     incorporated_subj_topic(transitive))])).

%% "heb me verslapen"
%% "heb een kater"
%% WHY not simply use the topic_drop analysis???
% m(v_root(heb,hebben),
%   verb(heb,hebben,sg1,[ninv(incorporated_subj_topic(aux_psp_hebben),
% 			    incorporated_subj_topic(aux_psp_hebben)),
% 		       ninv(incorporated_subj_topic(transitive_ndev),
% 			    incorporated_subj_topic(transitive_ndev))
% 		      ])).

% m(v_root(heb,hebben),
%   verb(had,hebben,past(sg),[ninv(incorporated_subj_topic(aux_psp_hebben),
% 				  incorporated_subj_topic(aux_psp_hebben)),
% 			     ninv(incorporated_subj_topic(transitive_ndev),
% 				  incorporated_subj_topic(transitive_ndev))
% 			    ])).

%% schrok me dood
%% WHY not simply use the topic_drop analysis???
%m(v_root(schrik,schrikken),
%  verb(schrok,unacc,past(sg),[ninv(incorporated_subj_topic(pred_refl),
%				   incorporated_subj_topic(pred_refl))])).
%
%m(v_root(schrik,schrikken),
%  verb(schrik,unacc,sg1,[ninv(incorporated_subj_topic(pred_refl),
%			      incorporated_subj_topic(pred_refl))])).

%% doesn't fit in general scheme for verbs:
%%   ik ben vissen
%%   ik ben wezen vissen
%% * ik ben zijn vissen
%% * ik wil wezen vissen
%% so "wezen" has a special inflectional type which
%% indicates it occurs only in IPP context, ie, it
%% is selected by something which requires a PSP
%%%%% this only seems to produce additional false analyses !?!?!
m(v_root(ben,zijn),verb(ben,   zijn, sg1,      [aux_simple(inf)])).
m(v_root(ben,zijn),verb(bent,  zijn, sg_bent,  [aux_simple(inf)])).
m(v_root(ben,zijn),verb(is,    zijn, sg_is,    [aux_simple(inf)])).
m(v_root(ben,zijn),verb(zijn,  zijn, pl,       [aux_simple(inf)])).
m(v_root(ben,zijn),verb(was,   zijn, past(sg), [aux_simple(inf)])).
m(v_root(ben,zijn),verb(waren, zijn, past(pl), [aux_simple(inf)])).
m(v_root(ben,zijn),verb(wezen, zijn, inf_ipp,  [aux_simple(inf)])).

%% VL: het had beter/mooi/leuk geweest
%%m(v_root(ben,zijn),verb(geweest,hebben,psp,[copula])).
%% now in lex.pl, parse_only 

m(v_root(ben,zijn),verb(wezen, zijn, inf,      List)) :-
    m(v_root(ben,zijn),verb_ben(ben,bent,is,zijn,geweest,was,waren,unacc,List)).

m(v_root(ben,zijn),
  verb_ben(ben,bent,is,zijn,geweest,was,waren,unacc,
	   [copula,		% ik ben dom
            
            % fixed([pred,[me]],no_passive), % hij is me toch kwaad
                                           % het is me daar toch een bende
            % should be mod (?), and is productive
	    % indistinguishable from % het is me een raadsel
            
	    passive,		% ik ben geslagen
	    aux_psp_zijn,	% ik ben gekomen
	    er_er,		% er zijn er die nooit iets doen
                                % TODO er zijn er ook bij die ...
	    intransitive,       % jij bent!
	    copula_sbar,        % het is leuk dat je komt
	    te_passive,         % hij is te verslaan
	    copula_vp,          % het is leuk om je te plagen
	    cleft,              % het zijn schurken
	    aan_het,            % ik ben aan het vissen
	    copula_np,		% ik ben het beu
	    ld_adv,             % ik ben thuis
	    so_copula,          % dat is me te ver
	    so_copula_sbar,     % het is me duidelijk dat ..
	    so_copula_vp,       % het is me teveel om ../ het is me een genoegen ...
	    sbar_subj_te_passive, % het is niet te vermijden dat ..
	    sbar_subj_het,	  % het is dat ik je vader ben
                                  % *dat ik je vader ben is.
	    
	    alsof_sbar_subj,    % het is alsof je naast me staat
	    ld_pp,              % ik ben in Assen
	    ld_dir,		% ik ben het huis uit, etc
            uit,
	    pc_pp(aan),	        % we zijn aan de drank; de beurt is aan ons
                                % also: daar is niets geks aan
	    fixed([{[ap_pred,pc(aan)]}],no_passive), % wat is daar verkeerd aan?
            er_pc_pp(bij),      % luieren was er niet bij
	    pc_pp(van),		% dat is van mij 
	    
	    er_pp_sbar(achter),	% ik ben er wel achter dat ...

            pc_pp(overheen),      % ik ben er nu wel overheen
	    er_pp_sbar(overheen), % ik ben er nog niet overheen dat ...
	
	    pc_pp(voor),        % ik ben voor deze aanpassingen
	    mod_pp(voor),       % daar is geen geld / aanleiding / reden voor
	    er_pp_vp(voor),     % ik ben ervoor om te gaan
	    er_pp_sbar(voor),   % ik ben ervoor dat hij komt
	    pc_pp(tegen),       % ik ben tegen de voorstellen
	    er_pp_vp(tegen),    % ik ben er tegen om
	    er_pp_sbar(tegen),  % ik ben er tegen dat

	    part_er_pp_sbar(uit,op),  % hij is er op uit dat ..
	    part_er_pp_vp(uit,op),    % hij is er op uit om ..
	    part_pc_pp(toe,aan),      % hij is aan een borrel toe
	    part_fixed(toe,[er_pp(aan),ap_pred],no_passive),  % hij is er slecht aan toe
	    part_er_pp_vp(toe,aan),   % hij is er aan toe om..
	    part_intransitive(uit),   % hij is uit
	    part_pc_pp(uit,op),       % hij is uit op de winst
	    part_pc_pp(af,van),       % hij is af van zijn vrouw
	    part_intransitive(weg),   % hij is weg
            part_mod_pp(weg,met),     % hij is er mee weg VL
            part_intransitive(rond),  % hij is rondgeweest met ...
            part_intransitive(langs),
            part_ld_pp(langs),        % ik ben bij hem langs geweest

	    fixed([er_pp(om),compar],no_passive),
                                % de opluchting was er niet minder om

            
            fixed([vc(lig,psp,intransitive),pc(aan),dat],no_passive),
            fixed([vc(lig,psp,intransitive),extra_obj_vp(A,B),er_pp(aan,A),i(dat,B)],no_passive),
            fixed([vc(lig,psp,intransitive),extra_sbar(A),er_pp(aan,A),dat],no_passive),
            fixed([[aan],er_pp(van)],no_passive),
	    fixed([[aan,de,hand]],no_passive), % wat is *er* aan de hand
	    fixed([{[pc(voor),subj(aandacht)]}],no_passive),
	    fixed([{[pc(tot),subj(aanleiding)]}],no_passive),
	    fixed([{[pc(voor),subj(aanleiding)]}],no_passive),
            fixed([{[pc(van),subj(afgeleide)]}],no_passive),
	    fixed([{[pc(voor),subj(afkorting)]}],no_passive),
	    fixed([pc(voor),np_pred(afkorting)],no_passive),
	    fixed([{[pc(van),np_pred]}],no_passive),
	    fixed([pc(aan),subj(behoefte)],no_passive),
	    fixed([pc(voor),subj(belangstelling)],no_passive),
	    fixed([pc(tegen),subj(bezwaar)],no_passive),
	    fixed([subj(bezwaar),er_pp(tegen,A),extra_sbar(A)],no_passive),
	    fixed([subj(bezwaar),er_pp(tegen,A),extra_vp(A)],no_passive),
            fixed([[bijster],[het,spoor]],no_passive),
	    fixed([pc(met),subj(contact)],no_passive),
            fixed([[de,baas],acc],no_passive), % ik ben de stress niet de baas
            fixed([[te,baas],acc],no_passive), % VL

	    fixed([pc(over),subj(discussie)],no_passive),

	    %% daar is geen discussie/twijfel over mogelijk
	    fixed([{[pc(over),ap_pred(mogelijk)]}],no_passive),
	    fixed([{[er_pp(over,A),ap_pred(mogelijk)]},extra_sbar(A)],
		  no_passive),
	    fixed([{[er_pp(van),[eentje]]}],no_passive),
	    fixed([{[er_pp(van),[één]]}],no_passive),
	    fixed([{[er_pp(van),[een]]}],no_passive),
				% hij is er één van
				% *zij zijn er twee van
				% todo: een van de verassingen/wensen/..[subject_sbar] was dat ..

	    %% het is hockeyers eigen veel tijd aan hun sport te besteden
	    fixed([ap_pred(eigen),dat,vp_subj],no_passive),

	    fixed([pc(aan),subj(gebrek)],no_passive),
	    fixed([pc(voor),subj(gevaar)],no_passive),
            fixed([ap_pred(in),{[[het,hart],er_pp(van)]}],no_passive), % VL
            fixed([ap_pred(in),{[[het,hart],er_pp(van,A)]},extra_sbar(A)],no_passive), % VL
            fixed([[hem],het_obj1],no_passive),
	    fixed([pc(van),subj(illustratie)],no_passive),
	    fixed([[in,gebreke]],no_passive),
	    fixed([[in,gebreke],vp],no_passive),
            fixed([pp_pred(in,hand),pc(van)],no_passive),
	    fixed([pc(over),subj(informatie)],no_passive),
	    fixed([pc(voor),subj(interesse)],no_passive),
	    fixed([[af],subj(kous)],no_passive),
	    fixed([pc(op),subj(kritiek)],no_passive),
	    fixed([er_pp(op,C),subj(kritiek),extra_sbar(C)],no_passive),
	    fixed([er_pp(naar,C),np_pred,extra_vp(C)],no_passive), % hij is er de man niet naar om..
            fixed([er_pp(naar,C),extra_vp(C)],no_passive), % zijn aard is er niet naar om ..
	    fixed([{[np_pred(meester),pc(in)]}],no_passive),
	    fixed([{[np_pred(meester),er_pp(in,C)]},extra_vp(C)],no_passive),
	    fixed([pc(aan),subj(nood)],no_passive),
	    fixed([{[pc(van),np_pred(oorzaak)]}],no_passive),
	    fixed([{[er_pp(van,C),np_pred(oorzaak)]},extra_sbar(C)],no_passive),
	    fixed([pc(over),subj(overeenstemming)],no_passive),
	    fixed([vc(schrijf,psp,intransitive),[op,het,lijf],dat],no_passive),
	    fixed([pc(van),ap_pred(op)],no_passive), % hij zou op van de zenuwen zijn / op zijn van de zenuwen
	    fixed([pc(aan),ap_pred(na)],no_passive), % hij was na aan de waarheid 
	    fixed([pc(voor),subj(plaats)],no_passive),
	    fixed([pc(tegen),subj(protest)],no_passive),
	    fixed([pc(voor),subj(reden)],no_passive),
	    fixed([er_pp(voor,C),subj(reden),extra_sbar(C)],no_passive),
	    fixed([er_pp(voor,C),subj(reden),extra_vp(C)],no_passive),
	    fixed([pc(tot),subj(reden)],no_passive),
	    fixed([er_pp(tot,C),subj(reden),extra_sbar(C)],no_passive),
	    fixed([er_pp(tot,C),subj(reden),extra_vp(C)],no_passive),
	    fixed([{[er_pp(van,C),np_pred(reden)]},extra_sbar(C)],no_passive),
	    fixed([{[er_pp(van,C),np_pred(hoofd_reden)]},extra_sbar(C)],no_passive),
	    fixed([subj(rek),pc(uit)],no_passive),
	    fixed([pc(tussen),subj(relatie)],no_passive),
	    fixed([er_pp(tegen,C),subj(protest),extra_sbar(C)],no_passive),
	    fixed([{[er_pp(van,C),np_pred(slachtoffer)]},extra_sbar(C)],no_passive),
	    fixed([vc(spreek,pass_te,intransitive),pc(over)],no_passive),
				% hij is daar helemaal niet over te spreken
	    fixed([vc(spreek,pass_te,intransitive),er_pp(over,C),extra_sbar(C)],no_passive),
				% hij is er helemaal niet over te spreken dat...
	    fixed([{[er_pp(van,C),np_pred(tegenstander)]},extra_sbar(C)],no_passive),
            fixed([pc(van),[ten,koste]],no_passive),
	    fixed([subj(twijfel),pc(over)],no_passive),
            fixed([pp_pred(van,invloed),pc(op)],no_passive),
	    fixed([subj(verschil),pc(tussen)],no_passive),
	    fixed([subj(verschil),pc(met)],no_passive),
	    fixed([{[np_pred(verschil),pc(tussen)]}],no_passive),
	    fixed([{[np_pred(verschil),pc(met)]}],no_passive),
            fixed([er_pp(met),[vet]],no_passive), % VL
	    fixed([{[er_pp(van,C),np_pred(voorstander)]},extra_sbar(C)],no_passive),
	    fixed([{[er_pp(van,C),np_pred(voorstander)]},extra_vp_no_control(C)],no_passive),
	    fixed([pc(van),subj(voorbeeld)],no_passive),
	    fixed([pc(van),subj(schoolvoorbeeld)],no_passive),
	    fixed([pc(op),np_pred(uitzondering)],no_passive),
	    fixed([pc(op),subj(uitzondering)],no_passive),
	    fixed([subj(vraag),pc(naar)],no_passive),
	    fixed([subj(zicht),pc(op)],no_passive),
	    pp_vp_subj(aan),	% het is aan ons om...
	    pp_sbar_subj(aan),  % hoe mensen reageren is niet aan ons
	    fixed([pc(aan),[de,beurt]],no_passive),
                                % nu is het de beurt aan ..
	    fixed([no_subj,pc(aan),[de,beurt]],no_passive),
                                % nu is de beurt aan ..
	    fixed([{[pc(met),svp_pp(in,conflict)]}],no_passive),
	    fixed([{[pc(met),pp_pred(in,contact)]}],no_passive),
	    fixed([{[pc(voor),[als,de,dood]]}],no_passive),
	    fixed([{[np_pred(dupe),er_pp(van,C)]},extra_sbar(C)],no_passive),
            fixed([svp_er_pp(uit)],no_passive),
            part_pc_pp(uit,over),
            part_er_pp_sbar(uit,over),
	    fixed([svp_pp(in,conflict)],no_passive),
	    fixed([{[pc(van),svp_pp(in,greep)]}],no_passive),
	    fixed([[in,omloop]],no_passive),
	    fixed([subj(sprake),pc(van)],no_passive),
	    fixed([subj(sprake),er_pp(van,A),extra_sbar(A)],no_passive),
	    fixed([subj(sprake),sbar],no_passive),
	    fixed([[te,boven],acc],no_passive),
	    fixed([vc(doe,pass_te,intransitive),er_pp(om,A),dat,extra_sbar(A)],no_passive), 
	    fixed([vc(doe,pass_te,intransitive),er_pp(om,A),dat,extra_vp(A)],no_passive), % het is hem er om te doen ..
	    fixed([vc(doe,pass_te,intransitive),pc(om),dat],no_passive), % het is hem te doen om
	    fixed([[te,moede],ap_pred,dat],no_passive),
            fixed([[te,rade],ld_pp],no_passive),
            fixed([[te,rade],ld_adv],no_passive),
	    fixed([[ten,dienste],dat],no_passive),
	    fixed([[ter,wille],dat],no_passive),
	    fixed([[terwille],dat],no_passive),
	    fixed([[thuis],pc(in)],no_passive),
            fixed([[van,dat],het_subj],no_passive), % VL
	    fixed([[van,mening],sbar],no_passive),
	    fixed([[van,oordeel],sbar],no_passive),
            fixed([[van,de,partij]],no_passive),
	    fixed([[van,plan],acc],no_passive),
	    fixed([[van,plan],vp],no_passive),
	    fixed([[van,de,plan],acc],no_passive), % VL
	    fixed([[van,de,plan],vp],no_passive),  % VL
	    fixed([[van,zin],acc],no_passive),
	    fixed([[van,zin],vp],no_passive),
            fixed([ap_pred(ver),er_pp(met)],no_passive),  % hoe ver zijn jullie daar nu mee?
            fixed([ap_pred(waar),er_pp(van)],no_passive),
            fixed([sbar_subj,np_pred(worst),dat],no_passive)
           ])).

m(v_root(heb,hebben),
  verb_heb(heb,hebt,heeft,hebben,gehad,had,hadden,hebben,
       [
	aux_psp_hebben,                 % ik heb geslapen
	transitive_ndev_npas,	        % ik heb al een boek
                                        % SU can be dev: 'vissen heeft voordelen'
	nonp_pred_np_ndev,              % klaar, af, gereed, nodig
			                % beschikbaar, gemeenschappelijk
				        % iets hebben als vervanging
	part_transitive_ndev_npas(uit),	% omdat ik het boek uitheb
        fixed([ap_pred(nodig),adv_meas],no_passive),
                                % ik had niet lang nodig
        fixed([ap_pred(nodig),adv_meas,vp],no_passive),
                                % ik had niet lang nodig om ...
        fixed([ap_pred(nodig),meas,vp],no_passive),
                                % ik had niet lang nodig om ...
        fixed([{[ap_pred(nodig),acc,mod_pp(bij)]}],no_passive),

	part_transitive(nodig),	% VL omdat zij dat zullen nodig hebben

	part_fixed(nodig,[adv_meas],no_passive),
                                % ik zou niet lang hebben nodig gehad
        part_fixed(nodig,[adv_meas,vp],no_passive),
                                % ik zou niet lang hebben nodig gehad om ...
        part_fixed(nodig,[meas,vp],no_passive),
        part_fixed(nodig,[{[acc,mod_pp(bij)]}],no_passive),

	subj_control(pass_te),         % je hebt maar te luisteren ??pass_te??
        obj_control(pass_te),	       % de paus heeft daklozen te eten gehad
	                               % ALLOWS *de paus heeft daklozen gehad te eten
                                       % te eten, slapen, logeren, 
        np_er_pc_pp(bij),              % we hebben er een kindje bij
	np_pc_pp_refl(bij),            % hij heeft een boek bij zich
	np_pc_pp(aan),                 % een hekel, de pest, voldoende, genoeg
        fixed([er_pp(uit),acc],no_passive),  % we hadden de kosten er snel uit
	np_er_pp_sbar(aan),	       % ik heb er een hekel aan, dat...
	np_er_pp_vp(aan),              % ik heb er een hekel/de pest aan om ...
	np_pc_pp(van),                 % dat heb ik van mijn moeder
	                               % we moeten het hebben van onze opslag
	obj_er_er,                     % je hebt er ook die ...
        %%% TODO: je hebt er ook bij die ..
                                       % ik kan/wil het niet hebben dat...
	aci_simple,                    % ik heb een paard in de wei lopen
	np_aan_het,		       % ik heb de kinderen aan het rekenen
        
	part_pc_pp(terug,van),         % daar heb ik niet van terug
	part_pc_pp(weg,van),           % hij heeft iets/veel/wat weg van Piet
        part_fixed(weg,[er_pp(van),sbar_subj],no_passive),
        % het heeft er veel van weg dat ..
	part_transitive(terug),        % ik wil dat terughebben
	part_intransitive(over),       % ik heb nog over
	part_intransitive(plaats),     % de verkiezingen hebben toch plaats
	part_intransitive(vrijaf),
	part_sbar(door),               % ik had door dat hij komt
	part_sbar(voor),               % ik had voor hem tijdig in te lichten
	part_vp(voor),		% ik had voor, hem in te lichten
	fixed([pc(met),[voor],[goed],acc],no_passive), % we zouden het wel goed voor met hem hebben
	fixed([[voor],pc(met),[goed],acc],no_passive), % we zouden het wel goed met hem voor hebben
	fixed([[voor],[goed],pc(met),acc],no_passive), % we zouden het wel met hem goed voor hebben
	part_transitive(aan),          % ze had geen kleren aan
	part_transitive(af),           % we moeten het afhebben
        part_transitive(beet),
	part_transitive(bij),          % zuidelijk: ik heb het bij
	part_transitive(binnen),
	part_transitive(door),         % ze had het zaakje door
	part_transitive(lief),         % ze heeft me lief
	part_intransitive(lief),       % je moet kunnen liefhebben
	part_transitive(mee),          % we hebben de wind / tijdgeest mee
	part_transitive(op),           % ik heb drie borrels op
	part_np_pc_pp(op,met),         % ik heb weinig op met zulke lieden
	part_transitive(over),         % ik heb een borrel over
	part_transitive(overeen),
	part_sbar(overeen),
	part_transitive(tegen),        % we hebben de wind / tijdgeest tegen
        part_sbar(tegen),              % we hebben tegen, dat ...
	part_transitive(voor),         % ik heb zes punten voor
	part_ld_pp(plaats),
	part_np_pc_pp(over,voor),
	part_np_pc_pp(voor,op),
        part_np_ld_adv(vandaan),          % waar heb je die vandaan?
	fixed([[in],[de,pest]],no_passive), % hij heeft de pest in
	fixed([[in],[de,smoor]],no_passive),
	fixed([[in],[de,pest],sbar],no_passive),
	fixed([[in],[de,smoor],sbar],no_passive),
	fixed([[in],{[[de,pest],pc(over)]}],no_passive),
	fixed([[in],{[[de,smoor],pc(over)]}],no_passive),
	fixed([[in],{[[de,pest],pc(over)]},sbar],no_passive),
	fixed([[in],{[[de,smoor],pc(over)]},sbar],no_passive),
	fixed([er_pp(in),[de,pest]],no_passive),
	fixed([er_pp(in),[de,smoor]],no_passive),
	fixed([er_pp(in),[de,pest],sbar],no_passive),
	fixed([er_pp(in),[de,smoor],sbar],no_passive),
	fixed([er_pp(in),[erg]],no_passive),
	fixed([er_pp(in),[geen,erg]],no_passive),
	fixed([er_pp(in),[erg],sbar],no_passive),
	fixed([er_pp(in),[geen,erg],sbar],no_passive),
	fixed([er_pp(in),[de,vaart]],no_passive),    % ze heeft er flink de vaart in
	fixed([pc(over),het_obj1],no_passive),  % ik had het over jou
	fixed([het_subj,er_pp(van,A),acc(schijn),extra_sbar(A)],no_passive),
	fixed([{[acc(aandacht),pc(voor)]}],no_passive),
	fixed([{[acc(aandeel),pc(in)]}],no_passive),
        fixed([pc(tot),acc(aanleiding)],no_passive),
        fixed([pc(voor),acc(aanleiding)],no_passive),
	fixed([{[acc(aanwijzing),pc(voor)]}],no_passive),
        fixed([{[acc(aardigheid),pc(in)]}],no_passive),
	fixed([{[acc(aardigheid),er_pp(in,C)]},extra_vp(C)],no_passive),
	fixed([{[acc(aardigheid),er_pp(in,C)]},extra_sbar(C)],no_passive),
	fixed([{[acc(baal),er_pp(van,A)]},extra_sbar(A)],no_passive),
	fixed([{[acc(baal),er_pp(van,A)]},extra_vp(A)],no_passive),
	fixed([{[acc(baat),pc(bij)]}],no_passive),
	fixed([{[acc(baat),er_pp(bij,X)]},extra_sbar(X)],no_passive),
	fixed([{[acc(baat),er_pp(bij,X)]},extra_vp(X)],no_passive),
	fixed([{[[betrekking],pc(op)]}],no_passive),
        fixed([[begrepen],{[het_obj1,pc(op)]}],no_passive),
	fixed([[goed],{[het_obj1,pc(met)]}],no_passive),
	fixed([{[acc(begrip),pc(voor)]}],no_passive),
	fixed([{[acc(begrip),er_pp(voor,X)]},extra_sbar(X)],no_passive),
	fixed([{[acc(bekomst),pc(van)]}],no_passive),
	fixed([{[acc(belangstelling),pc(voor)]}],no_passive),
	fixed([{[acc(belang),pc(bij)]}],no_passive),
	fixed([{[acc(belang),er_pp(bij,X)]},extra_sbar(X)],no_passive),
	fixed([{[acc(belang),er_pp(bij,X)]},extra_vp(X)],no_passive),
	fixed([{[acc(berouw),pc(over)]}],no_passive),
	fixed([{[acc(bezwaar),pc(tegen)]}],no_passive),
	fixed([{[acc(bezwaar),er_pp(tegen,A)]},extra_sbar(A)],no_passive),
	fixed([{[acc(bezwaar),er_pp(tegen,A)]},extra_vp(A)],no_passive),
        fixed([[bij,het,rechte,eind],het_obj1],no_passive),
	fixed([[vol],{[acc(buik),pc(van)]}],no_passive),
	fixed([{[acc(contact),pc(met)]}],no_passive),
	fixed([{[acc(controle),mod_pp(over)]}],no_passive),
	fixed([[in,de,zeilen],[de,wind]],no_passive),
	fixed([{[acc(dobber),pc(aan)]}],no_passive),  % we hebben daar een zware humanitaire dobber aan
	fixed([er_pp(in),[een,hard,hoofd]],no_passive),
	fixed([er_pp(in,X),[een,hard,hoofd],extra_sbar(X)],no_passive),
	fixed([{[acc(effect),pc(op)]}],no_passive),
	fixed([{[acc(ervaring),pc(met)]}],no_passive),
	fixed([{[acc(gebrek),pc(aan)]}],no_passive),
	fixed([{[acc(geduld),pc(voor)]}],no_passive),
	fixed([{[acc(geduld),er_pp(voor,X)]},extra_vp(X)],no_passive),
	fixed([{[acc(geld),pc(voor)]}],no_passive),
	fixed([{[acc(idee),er_pp(van,A)]},extra_sbar(A)],no_passive), % of/wie
	fixed([acc(invloed),sbar_subj],no_passive),
	fixed([{[acc(gelijk),pc(in)]}],no_passive),  % daar heb je (groot) gelijk in
	fixed([acc(gelijk),sbar],no_passive),  % je hebt (groot) gelijk, dat ..
	fixed([acc(gelijk),vp],no_passive),  % je hebt (groot) gelijk, om ..
	                                     % je hebt het grootste gelijk van de wereld , om / dat ...
	part_intransitive(gelijk),
	part_sbar(gelijk),
	part_vp(gelijk),
	fixed([[gemakkelijk,praten]],no_passive),
	fixed([{[acc(gesprek),pc(met)]}],no_passive),
        fixed([{[ap_pred(vol),pc(aan)]},acc(hand)],no_passive),
        fixed([{[ap_pred(vol),er_pp(aan,X)]},acc(hand),extra_vp(X)],no_passive),
        fixed([ap_pred('in het oog'),acc],no_passive),
	fixed([[in,het,verschiet],acc],no_passive),

        fixed([acc(indruk),yt(sbar)],no_passive), % dat is voorbij, heb ik de indruk
	fixed([{[acc(iets),pc(met)]}],no_passive),  % daar heeft hij iets mee
	fixed([{[acc(informatie),pc(over)]}],no_passive),
	fixed([{[acc(inspraak),pc(bij)]}],no_passive),
	fixed([{[acc(inspraak),pc(in)]}],no_passive),
	fixed([{[acc(interesse),pc(in)]}],no_passive),
	fixed([{[acc(interesse),pc(voor)]}],no_passive),
	fixed([{[acc(invloed),pc(op)]}],no_passive),
	fixed([{[acc(inzicht),pc(in)]}],no_passive),
	fixed([{[acc(kans),pc(op)]}],no_passive),
	fixed([{[acc(kritiek),pc(op)]}],no_passive),
	fixed([{[acc(kritiek),er_pp(op,C)]},extra_sbar(C)],no_passive),
	fixed([{[pc(met),ap_pred(lastig)]},het_obj1],no_passive),
	fixed([er_pp(met,C),ap_pred(lastig),extra_sbar(C),het_obj1],no_passive),
	fixed([{[acc(lol),pc(in)]}],no_passive),
	fixed([{[acc(lol),er_pp(in,C)]},extra_vp(C)],no_passive),
	fixed([{[acc(lol),er_pp(in,C)]},extra_sbar(C)],no_passive),
	fixed([{[acc(lust),pc(tot)]}],no_passive),
	fixed([{[acc(lust),er_pp(tot,C)]},extra_vp(C)],no_passive),
	fixed([{[acc(lust),er_pp(tot,C)]},extra_sbar(C)],no_passive),
	fixed([{[acc(macht),pc(over)]}],no_passive),
	fixed([{[acc(mandaat),pc(voor)]}],no_passive),
	fixed([{[acc(medelijden),pc(met)]}],no_passive),
	fixed([{[acc(compassie),pc(met)]}],no_passive),
	fixed([{[acc(mening),pc(over)]}],no_passive),
	%% todo: heel moeilijk
	fixed([{[pc(met),ap_pred(moeilijk)]},het_obj1],no_passive),
	fixed([er_pp(met,C),ap_pred(moeilijk),extra_sbar(C),het_obj1],no_passive),
	fixed([{[acc(moeite),pc(met)]}],no_passive),
	fixed([{[acc(moeite),er_pp(met,C),extra_sbar(C)]}],no_passive),
	fixed([{[acc(moeite),er_pp(met,C),extra_vp(C)]}],no_passive),
        fixed([{[acc(nominalization(kijk_om)),pc(naar)]}],no_passive),
	fixed([{[acc(mogelijkheid),pc(tot)]}],no_passive),
	fixed([[om,het,lijf],acc],no_passive),
	fixed([[om,handen],acc],no_passive),
	fixed([[omhanden],acc],no_passive),
	fixed([{[acc(oog),er_pp(in)]}],no_passive),
				% VL: ik heb er (geen) goed oog in
	fixed([{[acc(oog),pc(op)]}],no_passive),
	fixed([{[acc(oog),pc(voor)]}],no_passive),
	fixed([{[acc(oog_DIM),pc(op)]}],no_passive),
	fixed([{[acc(oog_DIM),pc(voor)]}],no_passive),
        fixed([{[acc(oor),pc(naar)]}],no_passive),
	fixed([svp_pp(op,naam),acc],no_passive),
        part_fixed(op,[{[acc(pet),pc(van)]}],no_passive),
                                % we hadden er geen hoge pet van op
        fixed([{[acc(plezier),er_pp(van,C)]},extra_sbar(C)],no_passive),
        fixed([{[acc(plezier),pc(in)]}],no_passive),
	fixed([{[acc(plezier),er_pp(in,C)]},extra_vp(C)],no_passive),
	fixed([{[acc(plezier),er_pp(in,C)]},extra_sbar(C)],no_passive),
	fixed([{[acc(probleem),pc(met)]}],no_passive),
	fixed([{[acc(probleem),er_pp(met,C)]},extra_sbar(C)],no_passive),
	fixed([{[acc(probleem),er_pp(met,C)]},extra_vp(C)],no_passive),
	%% recht, het recht, geen recht, het volste recht, ..
	fixed([{[acc(recht),pc(op)]}],no_passive),
	fixed([{[acc(recht),er_pp(op,C)]},extra_vp(C)],no_passive),
        %% daar hebben wij het volste recht toe
	fixed([{[acc(recht),pc(tot)]}],no_passive),
	fixed([{[acc(recht),er_pp(tot,C)]},extra_vp(C)],no_passive),
	fixed([{[acc(recht),er_pp(tot,C)]},extra_sbar(C)],no_passive),
        fixed([{[pc(tot),acc(reden)]}],no_passive),
        fixed([{[er_pp(tot,C),acc(reden)]},extra_sbar(C)],no_passive),
        fixed([{[er_pp(tot,C),acc(reden)]},extra_vp(C)],no_passive),
        fixed([acc(reden),sbar_subj],no_passive),  % dat hij komt heeft praktische redenen
	fixed([{[acc(rol),pc(bij)]}],no_passive),
	fixed([{[acc(rol),pc(in)]}],no_passive),
	fixed([{[acc(schik),pc(in)]}],no_passive),
	fixed([{[acc(schik),er_pp(in,C)]},extra_vp(C)],no_passive),
	fixed([{[acc(schik),er_pp(in,C)]},extra_sbar(C)],no_passive),
	fixed([{[acc(schrik),pc(voor)]}],no_passive),
	fixed([{[acc(spijt),pc(over)]}],no_passive),
	fixed([{[acc(spijt),er_pp(van,X)]},extra_sbar(X)],no_passive),
	fixed([{[acc(spijt),er_pp(van,X)]},extra_vp(X)],no_passive),
        fixed([{[acc(stem),pc(in)]}],no_passive),
	fixed([{[acc(succes),pc(met)]}],no_passive),
        fixed_dep(intransitive),
	fixed([vc(heb,psp,intransitive),[liggen],acc],no_passive), % VL: hij heeft hem liggen gehad
        fixed([vc(heb,psp,intransitive),[cadeau],acc],no_passive), % hij heeft het niet kado gehad
        fixed([vc(heb,psp,intransitive),[kado],acc],no_passive), % hij heeft het niet kado gehad
	fixed([vc(maak,pass_te,intransitive),sbar_subj,pc(met)],no_passive), 
	fixed([vc(maak,pass_te,intransitive),sbar_subj,{[acc,pc(met)]}],no_passive), 
	fixed([vc(maak,pass_te,intransitive),pc(met)],no_passive),
	fixed([vc(maak,pass_te,intransitive),pc(met),acc],no_passive),  % weinig/niets,..
	fixed([vc(vrees,pass_te,intransitive),pc(van),acc],no_passive), % ,,
	fixed([[tegoed],acc],no_passive),
	fixed([[tegoed],{[acc,pc(van)]}],no_passive),
	fixed([[te,goed],acc],no_passive),
	fixed([[te,goed],{[acc,pc(van)]}],no_passive),
	fixed([{[acc(tekort),pc(aan)]}],no_passive),
	fixed([[te,pakken],acc],norm_passive),
	fixed([{[acc(toegang),pc(tot)]}],no_passive),
        fixed([{[acc(twijfel),pc(over)]}],no_passive),
	fixed([[van,doen],pc(met)],no_passive),
	fixed([[van,doen],{[pc(met),acc]}],no_passive),
        fixed([{[acc(trek),pc(in)]}],no_passive),
        fixed([{[acc(trek),er_pp(in,X)]},extra_sbar(X)],no_passive),
        fixed([{[acc(trek),er_pp(in,X)]},extra_vp(X)],no_passive),
	fixed([{[acc(vat),pc(op)]}],no_passive),
	fixed([{[acc(verantwoordelijkheid),pc(voor)]}],no_passive),
	fixed([{[acc(vertrouwen),pc(in)]}],no_passive),
	fixed([{[acc(vertrouwen),er_pp(in,I)]},extra_sbar(I)],no_passive),
        fixed([[voor,ogen],acc],norm_passive),
	fixed([[voor,ogen],sbar],imp_passive),
	fixed([{[acc(vrede),pc(met)]}],no_passive), % ik heb er vrede mee
	fixed([{[acc(vrede),er_pp(met,X)]},extra_sbar(X)],no_passive),
	fixed([{[acc(waardering),pc(voor)]}],no_passive),
	fixed([{[acc(waardering),er_pp(voor,X)]},extra_sbar(X)],no_passive),
        fixed([{[acc(weerslag),pc(op)]}],no_passive),
        fixed([{[acc(zeg),pc(over)]}],no_passive), 
        fixed([{[acc(zeg_DIM),pc(over)]}],no_passive),
	fixed([{[acc(zicht),pc(op)]}],no_passive),
	fixed([pc(in),acc(goesting)],no_passive),  % can be modified...
	fixed([er_pp(in,I),acc(goesting),extra_sbar(I)],no_passive),
	fixed([er_pp(in,I),acc(goesting),extra_vp(I)],no_passive),
	fixed([pc(in),acc(zin)],no_passive),  % can be modified...
	fixed([er_pp(in,I),acc(zin),extra_sbar(I)],no_passive),
	fixed([er_pp(in,I),acc(zin),extra_vp(I)],no_passive),
	fixed([pc(in),acc(lust)],no_passive),  % can be modified...
	fixed([er_pp(in,I),acc(lust),extra_sbar(I)],no_passive),
	fixed([er_pp(in,I),acc(lust),extra_vp(I)],no_passive),
	fixed([als_pred,sbar],no_passive),
	fixed([als_pred,vp],no_passive),
	fixed([[aan,de,hand],acc],no_passive), 
	fixed([[de,baas],acc],no_passive), % ik kan de stress niet de baas
	fixed([[te,baas],acc],no_passive), % VL
        fixed([{[[de,hand],pc(in)]}],no_passive),
        fixed([{[[de,hand],er_pp(in,A)]},extra_sbar(A)],no_passive),
        pc_pp(tot),  % ze hebben tot vrijdag
        fixed([pc(tot), vp],no_passive),  % ze hebben tot vrijdag om ...
	fixed([acc(tijd),me],imp_passive),
	fixed([acc(tijd),me,vp],imp_passive),
	fixed([acc(tijd)],imp_passive),
	fixed([{[acc(tijd),me,pc(voor)]}],imp_passive),
	fixed([{[acc(tijd),pc(voor)]}],imp_passive),
	fixed([acc(tijd),vp],imp_passive),
	fixed([{[[een,broertje,dood],pc(aan)]}],no_passive),
	fixed([er_pp(van,A),[een,handje],extra_vp(A)],no_passive),
	fixed([{[[een,zwak],pc(voor)]}],no_passive),
        fixed([ap_pred(gemeenschappelijk),sbar],no_passive),
	fixed([[gemeen],sbar],no_passive),
	fixed([[gemeen],acc],no_passive),
	fixed([[gemeen],pc(met),sbar],no_passive),
	fixed([[gemeen],{[acc,pc(met)]}],no_passive),
	fixed([[het,laatste,woord]],no_passive),
	fixed([[voor,het,zeggen],het_obj1],no_passive),
	fixed([[iets],van_sbar],no_passive),
	fixed([[zoiets],van_sbar],no_passive),
	fixed([[zo,iets],van_sbar],no_passive),
	fixed([[in,de,gaten],acc],no_passive),
	fixed([[in,de,gaten],sbar],no_passive),
	fixed([[in,de,smiezen],acc],no_passive),
	fixed([[in,de,smiezen],sbar],no_passive),
	fixed([pp_pred(in,hand),acc],no_passive),
	fixed([pp_pred(in,hand),{[acc,mod_pp(van)]}],no_passive),
	fixed([[in,huis],acc],no_passive),
	fixed([[in,petto],acc],no_passive),
	fixed([[in,voorbereiding],acc],no_passive),
	fixed([svp_pp(in,greep),acc],no_passive),
	fixed([svp_pp(in,mars),acc],no_passive),
	fixed([adv(graag),sbar],no_passive),
	fixed([adv(graag),acc],no_passive),
	fixed([ap_pred(lief)],no_passive),        % VL ik heb niet liever dan dat ze komen
	fixed([ap_pred(lief),sbar],no_passive),   % liever , het liefst
	fixed([ap_pred(lief),acc],no_passive),    % liever , het liefst
	fixed([ap_pred('het lief'),sbar],no_passive),   % liever , het liefst
	fixed([ap_pred('het lief'),acc],no_passive),    % liever , het liefst
	fixed([ap_pred(liefst),sbar],no_passive), % ik heb liefst dat ..
	fixed([{[[lak],pc(aan)]}],no_passive),
	fixed([[makkelijk,praten]],no_passive),
	fixed([[onder,de,knie],acc],no_passive),
	fixed([[onder,ogen],acc],no_passive), %? ik had het werkstuk onder ogen
        fixed([svp_pp(op,geweten),acc],no_passive),
	fixed([ap_pred('op het oog'),acc],no_passive),
	fixed([[op,zak],acc],no_passive),
	fixed([[ten,gevolge],acc],no_passive),
	fixed([[ten,gevolge],vp],no_passive),
	fixed([[ten,gevolge],sbar],no_passive),
	fixed([[tengevolge],acc],no_passive),
	fixed([[tengevolge],vp],no_passive),
	fixed([[tengevolge],sbar],no_passive),
	fixed([[ten,gevolge],acc],no_passive),
	fixed([[ten,gevolge],vp],no_passive),
	fixed([[ten,gevolge],sbar],no_passive),
	fixed([[ten,doel],vp],no_passive),
	fixed([[tot,doel],vp],no_passive),
	fixed([svp_pp(tot,gevolg),acc],no_passive),
	fixed([svp_pp(tot,gevolg),vp],no_passive),
	fixed([svp_pp(tot,gevolg),sbar],no_passive),
        fixed([[tot,onderwerp],acc],no_passive),
	fixed([svp_pp(tot,resultaat),acc],no_passive),
	fixed([svp_pp(tot,resultaat),sbar],no_passive),
	fixed([[tot,taak],vp],no_passive),
	fixed([[tot,taak],acc],no_passive),
	fixed([[tuk],dat],no_passive),
	fixed([svp_pp(in,pap),acc(vinger)],no_passive),
				% een flinke vinger in de provinciale pap
	fixed([[in,de,aarde],acc(voet)],no_passive),
	fixed([[vol],[de,mond],pc(van)],no_passive),
	fixed([{[[weet],pc(van)]}],no_passive),
	fixed([{[[weet],er_pp(van,I)]},extra_sbar(I)],no_passive),
	fixed([{[[geen,weet],pc(van)]}],no_passive),
	fixed([{[[geen,weet],er_pp(van,I)]},extra_sbar(I)],no_passive),
	fixed([vp_subj,acc(nut)],no_passive),
	fixed([vp_subj,acc(zin)],no_passive),
        fixed([{[acc(zeggenschap),pc(over)]}],no_passive),
	fixed([[zitting]],no_passive),
	fixed([{[[zitting],pc(in)]}],no_passive),
	fixed([er_pp(over,I),het_obj1,extra_sbar(I)],no_passive), 
	fixed([er_pp(over,I),het_obj1,extra_vp(I)],no_passive),
	fixed([pc(van),[genoeg]],no_passive),
	fixed([er_pp(van,I),[genoeg],extra_vp(I)],no_passive),
	fixed([er_pp(van,I),[genoeg],extra_sbar(I)],no_passive),
	fixed([pc(van),[schoon,genoeg]],no_passive),
	fixed([er_pp(van,I),[schoon,genoeg],extra_vp(I)],no_passive),
	fixed([er_pp(van,I),[schoon,genoeg],extra_sbar(I)],no_passive),
        fixed([[veil],acc],no_passive),
        fixed([[veil],{[acc,pc(voor)]}],no_passive),
	fixed([[vrij]],no_passive),
        fixed([vc(heb,psp,intransitive),sbar],no_passive)
       ])).


m(v_root(kan,kunnen),
  verb_modal(kan,kun,kunt,kunnen,gekund,kon,konden,'hebben/zijn',
             [aux(inf)])).
        % NB: hoe dit tot stand *is* kunnen komen
        %     hoe we dit *hebben* kunnen doen
        %     hoe dit heeft/is kunnen gebeuren

m(v_root(kan,kunnen),
  verb_modal(kan,kun,kunt,kunnen,gekund,kon,konden,hebben,
       [
	intransitive,              % ik kan niet
	transitive_ndev_ndev_npas, % ik kan de tango
	ld_pp,			   % je kunt niet naar Amsterdam
	ld_dir,			   % ik kan niet verder omhoog
        uit,
	sbar_subj,		   % het zou kunnen dat ..
	pp_copula(aan,slag),
	pp_copula(aan,werk),
        %% done: copula (stuk,af,aan,...)
	%% fixed([compar],no_passive),  % de uitslag kan niet beter/harder/mooier          subsumed by ap_copula

	
	
	fixed([no_subj,yt(compar)],no_passive),  % erger kon eenvoudig niet
	fixed([{[[geen,blijf],pc(met)]}],no_passive),
	fixed([[geen,blijf]],no_passive),
	fixed([acc(kwaad),sbar],no_passive),
	fixed([acc(kwaad),vp_no_control],no_passive),
	fixed([vc(blijf_uit,inf,part_intransitive(uit)),sbar_subj],no_passive),  % het kon niet uitblijven dat ...

	     % ik kan (het) niet hebben dat ...
	fixed([vc(heb,inf,intransitive),opt_het_pobj1(dat_sbar)],no_passive),
	
        fixed([vc(lach,inf,intransitive),pc(met)],no_passive),
        fixed([vc(lach,inf,intransitive),er_pp(met,S),extra_sbar(S)],no_passive),
	fixed([vc(leef,inf,intransitive),er_pp(met,S),extra_sbar(S)],no_passive),
	fixed([vc(verhelp,inf,intransitive),sbar],no_passive), % hij kon niet verhelpen dat..
        nonp_pred_np_ndev,       % ik kan geen broek aan / geen pet op / etc
        ap_copula,             % het kan kapot! De pet kan af!
	fixed([ap_pred(kwijt),sbar],no_passive),
        %% ik kan het goed/niet/slecht/wel met hem vinden
        %% wij kunnen het samen goed vinden
        fixed([vc(vind,inf,intransitive),het_obj1],no_passive),
	pc_pp(naast),                % VL
	er_pp_sbar(naast),           % VL we kunnen er niet naast, dat...
	pc_pp(omheen),
	er_pp_sbar(omheen),          % we kunnen er niet omheen, dat...
        er_pp_vp(omheen),            %                         , om...
	part_fixed(om,[er_pp(met)],no_passive),  % VL ik kan er niet mee om
	np_mod_pp(met),              % daar kan ik wat/iets/niets mee
	fixed([[de,baas],acc],no_passive), % ik kan de stress niet de baas
	fixed([[te,baas],acc],no_passive), % VL
	fixed([[door,de,beugel]],no_passive), % dat kan niet door de beugel
	fixed([sbar_subj,[door,de,beugel]],no_passive), % het kan niet door de beugel dat ...
	fixed([vc(ontken,psp,intransitive)],no_passive), % VL de waarde kan niet ontkend.
	fixed([vc(ontken,psp,intransitive),sbar_subj],no_passive), % VL het kan niet ontkend dat ...
	fixed([er_pp(af)],no_passive),   % dat kan er niet af
        part_pc_er_transitive(af),       % dat had er niet afgekund
	fixed([er_pp(vanaf)],no_passive), % dat kan er niet vanaf
	part_fixed(op,[{[pc(met),acc(kant)]}],no_passive),
	fixed([[op,aan],pc(van)],no_passive),
	fixed([[op,aan],er_pp(van,X),extra_sbar(X)],no_passive),
	part_fixed(aan,[pc(vanop)],no_passive),
	part_fixed(aan,[er_pp(vanop,X),extra_sbar(X)],no_passive),
	part_pc_pp(aan,op),  % een vent waar hij op aan kan
        fixed([[overweg]],no_passive),
        fixed([[overweg],pc(met)],no_passive),
        fixed([[overweg],er_pp(met,X),extra_sbar(X)],no_passive),
        fixed([[uit,de,voeten],pc(met)],no_passive),
	fixed([er_pp(van),acc(bal)],no_passive), 
	fixed([er_pp(van),acc(fluit)],no_passive), 
	fixed([er_pp(van),acc(hout)],no_passive), % hij kan er geen hout van
	fixed([er_pp(van),acc(niets)],no_passive), 
	fixed([er_pp(van),acc(wat)],no_passive), 
	fixed([er_pp(van),acc(reet)],no_passive), 
	fixed([er_pp(van),acc(zak)],no_passive), 
	fixed([er_pp(van),acc(zier)],no_passive),
	fixed([[te,lijf],acc],no_passive),
	fixed([vc(maak,inf,intransitive),het_pobj1(vp)],no_passive),  % we kunnen het niet/best maken om ..
        fixed([vc(praat_mee,inf,part_intransitive(mee)),er_pp(van)],no_passive),
        fixed([vc(spreek_mee,inf,part_intransitive(mee)),er_pp(van)],no_passive),
	part_fixed(door,[er_pp(met)],no_passive),  % dat kan er mee door
	part_intransitive(aan),  % de lamp kan aan
	part_transitive(af),     % ik kan deze oefening wel alleen af
	part_pc_pp(af,van),      % hij kon van zijn stukken af
	part_pc_pp(toe,met),     % hij kan wel met minder toe
	part_intransitive(terug),
	er_pp_sbar(onderuit),
	part_ld_pp(terug),
	part_intransitive(uit),  % de lamp kan uit
	part_transitive(aan),    % ik kan haar wel aan
	part_vp_obj(aan),        % jullie kunnen het wel aan om ..
	part_sbar_obj(aan),      % jullie kunnen het wel aan om ..
	fixed([er_pp(in),sbar_subj],no_passive), % het kan er niet in dat ...
	fixed([er_pp(in),nor_mod_pp(bij)],no_passive), % het kan er bij mij niet in dat ...
	fixed([er_pp(in),nor_mod_pp(bij),sbar_subj],no_passive), % het kan er bij mij niet in dat ...
	copula_np(kwijt),
%%%	part_transitive(kwijt),  % ik kan jou niet kwijt
	
%%%	part_np_mod_pp(kwijt,over), % meer kan ik daar niet over kwijt
%%%	part_np_mod_pp(kwijt,bij),  % ik kan mijn agressie daar bij kwijt
	fixed([copula_np(kwijt),ld_pp],no_passive),
	fixed([copula_np(kwijt,ld_pp)],no_passive),
%%%	part_np_pc_pp(kwijt,aan),% je kunt je geld kwijt aan lekkere hapjes
%%%	part_np_pc_pp(kwijt,op), % de verliezers kun je kwijt op de vrije klaveren...
%%%	part_np_pc_pp(kwijt,in)  % daar kan ik veel in kwijt
	fixed([copula_np(kwijt,mod_pp(over))],no_passive),
	fixed([copula_np(kwijt,mod_pp(bij))],no_passive),
	pc_pp(bij),              % ik kan er niet bij (?)
	er_pp_sbar(bij),         % ik kan er niet bij dat mensen hun paarden opeten
	pc_pp(buiten),           % ik kan niet buiten koffie
	pc_pp(tegen),            % ik kan niet tegen thee
	er_pp_sbar(tegen),       % ik kan er niet tegen dat ..
        er_pp_vp(tegen),         % ik kan er niet tegen afgezeken te worden
	part_fixed(uit,[er_pp(aan)],no_passive), % VL hij kan er niet aan uit
        part_pc_pp(uit,over),
        part_er_pp_sbar(uit,over),
	fixed([er_pp(uit),[wijs]],no_passive),
	pc_pp(zonder),           % ik kan niet zonder (koffie)
        part_intransitive(mee),
	part_intransitive(op),   % het kon niet op
	part_transitive(op),     % hij kan zijn geluk/toetje niet op
	part_ld_transitive(op),  % hij kan geen kant op
	part_pc_pp(op,tegen),    % daar kan ik niet tegen op
	part_intransitive(terecht), % er
	part_ld_adv(terecht),    % je kunt daar terecht
	part_ld_pp(terecht),     % je kunt terecht bij ..
        part_intransitive(vooruit),
        part_pc_pp(vooruit,met),
        part_intransitive(verder),
        part_pc_pp(verder,met),
        part_intransitive(weg)
       ])).

m(v_root(wil,willen),
  verb_modal(wil,_,wilt,willen,gewild,[wou,wilde],
	     [wouden,wilden],hebben,
       [
	modifier(aux(inf)),      % hij wil niet slapen;
                                 % ik moet opschieten , wil ik op op tijd komen
	transitive_ndev_ndev_npas,    % ik wil Bolletje
	modifier(tr_sbar),       % wie wilde hij dat de opening zou doen?
                                 % ik moet opschieten , wil ik dat we op tijd k
	ld_pp,                   % ik wil naar huis
	np_ld_pp,                % ik wil hem in de spits
	                         % hij wil jonge honden in het magazine
	ld_dir,                  % ik wil omhoog
        uit,
	intransitive,            % ik wil wel
        nonp_pred_np_ndev,       % kleren aan uit
                                 % pet op af
                                 % een oortje in 
                                 %
                                 % those are LD: de brug over, het land door
                                 %               het land uit/in, de berg op
	part_intransitive(terug),
	part_transitive(terug),  % ik wil mijn geld terug!
	part_ld_pp(terug),
	pp_copula(aan,slag),
	pp_copula(aan,werk),
	pp_copula(in,gesprek),
	np_mod_pp(van),          % hij wil er een deel van
	fixed([[te,lijf],acc],no_passive),
	pc_pp(aan),              % ze willen niet aan de pil
	er_pp_sbar(aan),         % hij wil er niet aan dat ..
        part_pc_er_transitive(aan),    % omdat zij er niet aanwilden

	%% eigenlijk kan dat niet: verplichte modifier...
	%% maar een tweede pc kan óók niet :-(
	fixed([er_pp(in),nor_mod_pp(bij)],no_passive), % deze redenering wil er bij mij niet in
	fixed([er_pp(in),nor_mod_pp(bij),sbar_subj],no_passive), % het wil er bij mij niet in dat ...
	     % ik wil (het) niet hebben dat ...
	fixed([vc(heb,inf,intransitive),opt_het_pobj1(dat_sbar)],no_passive),

	fixed([vc(zeg,inf,intransitive),sbar_subj,sbar],no_passive),  % dat hij komt wil nog niet zeggen dat ...
	
	part_pc_pp(af,van),	% ik wil af van dat gezeur
	fixed([ap_pred(kwijt),sbar],no_passive),
	copula_np(kwijt),
	fixed([{[ap_pred(kwijt),mod_pp(over)]},sbar],no_passive),
	fixed([copula_np(kwijt,mod_pp(over))],no_passive)
%%%     part_pc_pp(kwijt,over),  % hij wilde daar niets over kwijt
%%%     part_np_pc_pp(kwijt,over),  % meer wilde hij daar niet over kwijt;
%%%                                 % hij wilde er geen details over kwijt
%%%     part_er_pp_sbar(kwijt,over), % hij wilde daar alleen over kwijt dat ..
%%%	part_transitive(kwijt),  % ik wil mijn huis kwijt
%%%	part_sbar(kwijt)         % hij wilde alleen kwijt dat ...
       ])).             

m(v_root(zal,zullen),
  verb_modal(zal,zul,zult,zullen,_,zou,zouden,hebben,
             [intransitive,     % dat zal wel
              aux(inf),         % je zult wel moeten
	      sbar_subj,	% het zal toch niet dat ...
              fixed([[best],sbar_subj],no_passive)  % het zal best dat ..
              ])).     

m(Stem,verb(_,_,_,Psp,_,_,_,HZ,Subcat)) :-
    psp_only(Stem,Psp,HZ,Subcat).

m(v_root(placht,plachten),
  verb(_,_,_,_,placht,plachten,_,hebben,
       [aux(te)])).

%% hack: sg3 ought to be sg1, but then it clashes later in argument_realization
%%       it doesn't seem to hurt
m(v_root(ziedaar,ziedaar),verb(ziedaar,hebben,sg3,
       [ninv(incorporated_subj_topic(intransitive),incorporated_subj_topic(intransitive)),
	ninv(incorporated_subj_topic(transitive),  incorporated_subj_topic(transitive))])).

m(v_root(ziehier,ziehier),verb(ziehier,hebben,sg3,
       [ninv(incorporated_subj_topic(intransitive),incorporated_subj_topic(intransitive)),
	ninv(incorporated_subj_topic(transitive),  incorporated_subj_topic(transitive))])).

m(v_root(onderbelicht,onderbelicht),
  verb(_,_,onderbelichten,onderbelicht,_,_,hebben,
       [transitive])).

m(v_root(overbelicht,overbelicht),
  verb(_,_,overbelichten,overbelicht,_,_,hebben,
       [transitive])).

m(v_root(Stem,Inf),
  verb(F0,F1,F2,F3,F4,F5,F6,hebben,Sc)) :-
    vdt(F0,F1,F2,F3,F4,F5,F6,List),
    stem(F0,Stem),
    stem(F2,Inf),
    lists:member(h(Sc),List).

m(v_root(Stem,Inf),
  verb(F0,F1,F2,F3,F4,F5,F6,zijn,Sc)) :-
    vdt(F0,F1,F2,F3,F4,F5,F6,List),
    stem(F0,Stem),
    stem(F2,Inf),
    lists:member(z(Sc),List).

m(v_root(Stem,Inf),
  verb(F0,F1,F2,F3,F4,F5,F6,unacc,Sc)) :-
    vdt(F0,F1,F2,F3,F4,F5,F6,List),
    stem(F0,Stem),
    stem(F2,Inf),
    lists:member(unacc(Sc),List).

m(v_root(Stem,Inf),
  verb(F0,F1,F2,F3,F4,F5,F6,'hebben/zijn',Sc)) :-
    vdt(F0,F1,F2,F3,F4,F5,F6,List),
    stem(F0,Stem),
    stem(F2,Inf),
    lists:member(b(Sc),List).

m(v_root(Lem,Lem),verb(Inf,hebben,inf,[intransitive])) :-
    inf_only(Inf,Lem),
    atomic(Inf).

m(v_root(Inf,Inf),verb(Inf,hebben,inf,[intransitive])) :-
    inf_only(Inf),
    atomic(Inf).

% het skispringen
% ik wil weer gaan skispringen
:- discontiguous inf_only/1, inf_only/2.

inf_only(allroundschaatsen,allround_schaatsen).
inf_only(allrounden).
inf_only(alpineskiën,alpine_skiën).
inf_only(baanwielrennen,baan_wielrennen).
inf_only(backpacken).
inf_only(ballonvaren,ballon_varen).
inf_only(berglopen).
inf_only(bloemlezen).
inf_only(boekhouden).
inf_only(bloemschikken).
inf_only(boogschieten).
inf_only(brandschatten).
inf_only(campagnevoeren,campagne_voeren).
inf_only(carpoolen).
inf_only(crowdsurfen).
inf_only(deeltijdwerken,deeltijd_werken).
inf_only(diepzeeduiken,diepzee_duiken).
inf_only(discuswerpen).
inf_only(doodslaan).	% want tussen droom en daad
inf_only(downhillen).
inf_only(driebanden).
inf_only(dwergwerpen,dwerg_werpen).
inf_only(echtbreken).
inf_only(geitgooien,geit_gooien).
inf_only(handwerken).
inf_only(hardlopen,hard_lopen).
inf_only(haringkaken).
inf_only(hongerstaken).
inf_only(hoofdrekenen,hoofd_rekenen).
inf_only(hoogspringen).
inf_only(ijszeilen,ijs_zeilen).
inf_only(inlineskaten,inline_skaten).
inf_only('inline-skaten',inline_skaten).
inf_only('in-lineskaten',inline_skaten).
inf_only('in-line-skaten',inline_skaten).
inf_only('internet-bankieren',internet_bankieren).
inf_only(internetbankieren,internet_bankieren).
inf_only(kantklossen).
inf_only(kickboksen).
inf_only(kogelstoten).
inf_only(koffiedrinken,koffie_drinken).
inf_only(koorddansen).
inf_only(koppensnellen).
inf_only(kunstrijden).
inf_only(kunstschaatsen).
inf_only(landschapschilderen,landschap_schilderen).
inf_only(langebaanschaatsen,langebaan_schaatsen).
inf_only(linedancen).
inf_only(machineschrijven,machine_schrijven).
inf_only(marathonschaatsen,marathon_schaatsen).
inf_only(maren).		% niets te maren
inf_only(mierenneuken).
inf_only(modderworstelen).
inf_only(motorcrossen).
inf_only(motorrijden).
inf_only(nachtvliegen).
inf_only(oppositievoeren,oppositie_voeren).
inf_only(paalzitten).
inf_only(parachutespringen,parachute_springen).
inf_only('parachute-springen',parachute_springen).
inf_only(pleasen).
inf_only(polsstokhoogspringen).
inf_only(prijsschieten).
inf_only(profwielrennen,prof_wielrennen).
inf_only(rekeningrijden).
inf_only(ruziemaken).
inf_only(schoolverlaten).
inf_only(schoolzwemmen,school_zwemmen).
inf_only(skispringen).
inf_only(skivliegen).
inf_only(skydiven).
inf_only(snowboarden).
inf_only(speerwerpen).
inf_only(sportvissen,sport_vissen).
inf_only(striptekenen,strip_tekenen).
inf_only(stoepranden).
inf_only(tandenpoetsen).
inf_only(teleleren).
inf_only(telewerken).
inf_only(thaiboksen).
inf_only(thuiswerken).
inf_only(tijdrekken).
inf_only(tijdrijden,tijd_rijden).
inf_only(trampolinespringen).
inf_only(treinreizen,trein_reizen).
inf_only('tv-kijken').
inf_only(veldlopen).
inf_only(veldrijden).
inf_only(verdachtmaken,verdacht_maken).
inf_only(vrouwenwielrennen,vrouw_wielrennen).
inf_only(zakenbankieren).
inf_only(zakkenrollen).
inf_only(zandsurfen,zand_surfen).
inf_only(zeezeilen,zee_zeilen).
inf_only(zwanendriften,zwaan_driften).
inf_only(zwartepieten).
inf_only(zwartrijden).
inf_only(zwartwerken).
inf_only(zweefvliegen).

stem(F0,Stem) :-
    (	atom(F0)
    ->	F0 = Stem
    ;	nonvar(F0), F0 = [Stem|_]
    ->	true
    ;   nonvar(F0), F0 = inflected(F1,_)
    ->  stem(F1,Stem)
    ;	format(user_error,"no stem: ~w~n",[F0]),
	fail
    ).

vdt(A,B,C,D,E,F,G,Sc) :-
    v(A,B0,C,D,E,F,G,Sc),
    add_dt(A,B0,B).

/* voor Wikipedia?
add_dt(A,B0,B) :-
    mem_eq(A,Aform),
    atom_concat(_,d,Aform),
    !,
    (   var(B0)
    ->  true
    ;   atom(B0)
    ->  B = [B0,Aform]
    ;   B0 = [_|_],
        lists:append(B0,[Aform],B)
    ).
*/
add_dt(_,B,B).
                


v(A,B,C,D,E,F,_,Sc) :-
    v(A,B,C,D,E,F,Sc).

v(aai,aait,aaien,geaaid,aaide,aaiden,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv])]).

v(aanbid,aanbidt,aanbidden,aanbeden,aanbad,aanbaden,
    [h([transitive,
        als_pred_np])]).

v(aanhoor,aanhoort,aanhoren,aanhoord,aanhoorde,aanhoorden,
    [h([transitive])]).

v(aanschouw,aanschouwt,aanschouwen,aanschouwd,aanschouwde,aanschouwden,
    [h([transitive,
	sbar])]).

v(aanvaard,aanvaardt,aanvaarden,aanvaard,aanvaardde,aanvaardden,
    [h([als_pred_np,
	sbar,
	vp,
	transitive,
	intransitive
       ])]).

v(aanzie,aanziet,aanzien,aanzien,aanzag,aanzagen,
    [h([transitive,
	sbar])]).

v(aap,aapt,apen,geaapt,aapte,aapten,
    [h([part_transitive(na)])]).  

v(aard,aardt,aarden,geaard,aardde,aardden,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	pc_pp(naar)])]).

v(aarzel,aarzelt,aarzelen,geaarzeld,aarzelde,aarzelden,
    [h([intransitive,
	sbar,
        mod_pp(over),
	vp
       ])]).

v(aas,aast,azen,geaasd,aasde,aasden,
    [h([transitive,
	pc_pp(op)])]).

v(abonneer,abonneert,abonneren,geabonneerd,abonneerde,abonneerden,
    [h([transitive,
	np_pc_pp(op)])]).
  

v(aborteer,aborteert,aborteren,geaborteerd,aborteerde,aborteerden,
    [h([intransitive,
	transitive])]).

v(absorbeer,absorbeert,absorberen,geabsorbeerd,absorbeerde,absorbeerden,
    [h([intransitive,
	transitive])]).

v(abstraheer,abstraheert,abstraheren,geabstraheerd,abstraheerde,abstraheerden,
    [h([intransitive,
	transitive,
	pc_pp(van),
	np_pc_pp(van)])]).

v(accelereer,accelereert,accelereren,geaccelereerd,accelereerde,accelereerden,
    [h([intransitive])]).

v(accentueer,accentueert,accentueren,geaccentueerd,accentueerde,accentueerden,
    [h([transitive,
	intransitive])]).

v(accepteer,accepteert,accepteren,geaccepteerd,accepteerde,accepteerden,
    [h([sbar,
        sbar_obj,
	transitive,
        intransitive])]).

v(acclimatiseer,acclimatiseert,acclimatiseren,geacclimatiseerd,acclimatiseerde,acclimatiseerden,
  [unacc([intransitive
	 ]),
   h([transitive  % de vogels moeten geacclimatiseerd worden
     ])]).

m(v_root(acht,achten),
  verb(geacht,hebben,psp,[np_vp_obj1])).

v(acht,acht,achten,geacht,achtte,achtten,
  [h([%% np_vp_obj1,		% verplicht passief? Kunnen we niet afdwingen, maar daarom dit frame alleen als participle
      %% verb raiser? TODO "omdat ik het boek geacht wordt te lezen"
      %% LassySmall: En dat terwijl Egypte het sterkste leger in het Midden-Oosten werd geacht te hebben .
	pred_np,
	pred_np_sbar,		% we achten het bewezen dat ..
	pred_np_vp,
	transitive])]).

v(achterhaal,achterhaalt,achterhalen,achterhaald,achterhaalde,achterhaalden,
    [h([transitive,
	sbar])]).

v(achtervolg,achtervolgt,achtervolgen,achtervolgd,achtervolgde,achtervolgden,
  [h([transitive,
      intransitive,
      np_mod_pp(met)])]).

v(acteer,acteert,acteren,geacteerd,acteerde,acteerden,
    [h([intransitive,
	transitive,
        pc_pp(in),
	sbar])]).

v(activeer,activeert,activeren,geactiveerd,activeerde,activeerden,
  [h([transitive,
      intransitive,
      np_pc_pp(tot)])]).

v(actualiseer,actualiseert,actualiseren,geactualiseerd,actualiseerde,actualiseerden,
    [h([transitive,
	intransitive])]).

v(adel,adelt,adelen,geadeld,adelde,adelden,
  [h([intransitive,             % arbeid ..
      transitive])]).           % iemand adelen

v(adem,ademt,ademen,geademd,ademde,ademden,
    [h([intransitive,
	transitive,
	dip_sbar,  % Riddle:  'O , schatje , ' ademt hij en hij kust me achter mijn oor .
	part_intransitive(in),
	part_intransitive(uit),
	part_transitive(in),
	part_transitive(uit)])]).

v(administreer,administreert,administreren,geadministreerd,administreerde,
  administreerden,
    [h([intransitive,
	transitive])]).

v(adopteer,adopteert,adopteren,geadopteerd,adopteerde,adopteerden,
    [h([intransitive,
	transitive])]).

v(adoreer,adoreert,adoreren,geadoreerd,adoreerde,adoreerden,
    [h([transitive,
	intransitive])]).

v(adresseer,adresseert,adresseren,geadresseerd,adresseerde,adresseerden,
    [h([% refl,
	so_pp_np,
	transitive])]).

v(adstrueer,adstrueert,adstrueren,geadstrueerd,adstrueerde,adstrueerden,
    [h([intransitive,
	transitive])]).

v(adverteer,adverteert,adverteren,geadverteerd,adverteerde,adverteerden,
    [h([intransitive,
	transitive,
	sbar])]).

v(adviseer,adviseert,adviseren,geadviseerd,adviseerde,adviseerden,
    [h([intransitive,
	np_sbar,
	np_vp_obj,
        np_vp_obj1,
	sbar,
	so_pp_np,
	so_pp_sbar,
	np_np,
	transitive,
	vp_no_control,
	np_pc_pp(tot)])]).

v(afficheer,afficheert,afficheren,geafficheerd,afficheerde,afficheerden,
    [h([sbar,
        als_pred_np,
	transitive])]).

v(affirmeer,affirmeert,affirmeren,geaffirmeerd,affirmeerde,affirmeerden,
    [h([intransitive,
	transitive])]).

v(ageer,ageert,ageren,geageerd,ageerde,ageerden,
    [h([intransitive,
        mod_pp(tegen)])]).

v(alarmeer,alarmeert,alarmeren,gealarmeerd,alarmeerde,alarmeerden,
    [h([transitive,
	intransitive])]).

v(ambieer,ambieert,ambiëren,geambieerd,ambieerde,ambieerden,
    [h([transitive,
	vp])]).

v(amendeer,amendeert,amenderen,geamendeerd,amendeerde,amendeerden,
    [h([transitive,
	intransitive])]).

v(amputeer,amputeert,amputeren,geamputeerd,amputeerde,amputeerden,
    [h([np_np,
	transitive])]).

v(amuseer,amuseert,amuseren,geamuseerd,amuseerde,amuseerden,
    [h([% refl,  not refl according to ANS
	transitive,
        intransitive])]).

v(analyseer,analyseert,analyseren,geanalyseerd,analyseerde,analyseerden,
    [h([intransitive,
	sbar,
	transitive])]).

v(analyzeer,analyzeert,analyzeren,geanalyzeerd,analyzeerde,analyzeerden,
    [h([intransitive,
	transitive])]).

v(animeer,animeert,animeren,geanimeerd,animeerde,animeerden,
    [h([intransitive,
        transitive,
	pc_pp(tot)])]).

v(anker,ankert,ankeren,geankerd,ankerde,ankerden,
    [h([transitive,
	intransitive])]).

v(annexeer,annexeert,annexeren,geannexeerd,annexeerde,annexeerden,
    [h([transitive])]).

v(annuleer,annuleert,annuleren,geannuleerd,annuleerde,annuleerden,
    [h([transitive,
	intransitive])]).

v(anticipeer,anticipeert,anticiperen,geanticipeerd,anticipeerde,anticipeerden,
    [h([intransitive,
	transitive,
	pc_pp(op)])]).

v(antwoord,antwoordt,antwoorden,geantwoord,antwoordde,antwoordden,
    [h([np_np,
	intransitive,
	np_sbar,
	so_pp_sbar,
	np_vp_subj,
	sbar,
	van_sbar,
	transitive,
	vp,
	np_pc_pp(op),
	pp_sbar(op),
	pc_pp(op)])]).

v(apaiseer,apaiseert,apaiseren,geapaiseerd,apaiseerde,apaiseerden,
    [h([intransitive,
	transitive])]).

v(aperitief,aperitieft,aperitieven,geaperitiefd,aperitiefde,aperitiefden,
    [h([intransitive])]).

v(app,appt,appen,geappt,appte,appten,
  [h([intransitive,
      part_intransitive(terug),
      transitive,
      sbar,
      np_sbar,
      part_sbar(terug),
      part_np_sbar(terug),
      mod_pp(met)
     ])]).

v(appelleer,appelleert,appelleren,geappelleerd,appelleerde,appelleerden,
    [h([intransitive,
	pc_pp(aan),
	mod_pp(tegen)])]).

v([applaudisseer,applaudiseer],
  [applaudisseert,applaudiseert],
  [applaudisseren,applaudiseren],
  [geapplaudisseerd,geapplaudiseerd],
  [applaudisseerde,applaudiseerde],
  [applaudisseerden,applaudiseerden],
    [h([intransitive,
	mod_pp(voor)])]).

v(apprecieer,apprecieert,appreciëren,geapprecieerd,apprecieerde,apprecieerden,
    [h([transitive,
	sbar,
	sbar_obj
       ])]).

v(arbeid,arbeidt,arbeiden,gearbeid,arbeidde,arbeidden,
    [h([intransitive,
	pc_pp(aan)])]).

v(arbitreer,arbitreert,arbitreren,gearbitreerd,arbitreerde,arbitreerden,
    [h([intransitive,
	transitive])]).

v(archiveer,archiveert,archiveren,gearchiveerd,archiveerde,archiveerden,
    [h([intransitive,
	transitive])]).

v(argumenteer,argumenteert,argumenteren,geargumenteerd,argumenteerde,argumenteerden,
    [h([intransitive,
	sbar,
	transitive,
	mod_pp(tegen),
	mod_pp(voor)])]).

v(arrangeer,arrangeert,arrangeren,gearrangeerd,arrangeerde,arrangeerden,
    [h([transitive,
	intransitive])]).

v(arresteer,arresteert,arresteren,gearresteerd,arresteerde,arresteerden,
    [h([transitive,
        intransitive])]).

v(arriveer,arriveert,arriveren,gearriveerd,arriveerde,arriveerden,
    [unacc([intransitive,
            ld_pp])]).

v(articuleer,articuleert,articuleren,gearticuleerd,articuleerde,articuleerden,
    [h([intransitive,
	transitive])]).

v(assembleer,assembleert,assembleren,geassembleerd,assembleerde,assembleerden,
    [h([transitive])]).

v(assimileer,assimileert,assimileren,geassimileerd,assimileerde,assimileerden,
    [h([intransitive,
	transitive,
	np_pc_pp(aan)])]).

v(assisteer,assisteert,assisteren,geassisteerd,assisteerde,assisteerden,
    [h([intransitive,
	transitive])]).

v(associeer,associeert,associëren,geassocieerd,associeerde,associeerden,
    [h([intransitive,
	transitive,
	np_pc_pp(met),
	pc_pp(met)])]).

v(attaqueer,attaqueert,attaqueren,geattaqueerd,attaqueerde,attaqueerden,
    [h([transitive,
	intransitive])]).

v(attendeer,attendeert,attenderen,geattendeerd,attendeerde,attendeerden,
    [h([np_pc_pp(op),
	np_er_pp_sbar(op),
	transitive,
	pc_pp(op)])]).

v(automatiseer,automatiseert,automatiseren,geautomatiseerd,automatiseerde,automatiseerden,
    [h([intransitive,
	transitive])]).

v(autoriseer,autoriseert,autoriseren,geautoriseerd,autoriseerde,autoriseerden,
    [h([transitive])]).

v(avanceer,avanceert,avanceren,geavanceerd,avanceerde,avanceerden,
  [h([transitive]),
   z([intransitive,
      ld_pp])]).

v(avonturier,avonturiert,avonturieren,geavonturierd,avonturierde,avonturierden,
    [h([intransitive])]).

v(baad,baadt,baden,gebaad,baadde,baadden,
    [h([intransitive,
	% refl,
	transitive
       ])]).

v(baal,baalt,balen,gebaald,baalde,baalden,
    [h([intransitive,
	pc_pp(van),
	er_pp_sbar(van),
	er_pp_vp(van),
	sbar])]).

v(baan,baant,banen,gebaand,baande,baanden,
    [h([np_np,
	refl_np_ld_pp,
	transitive])]).

v(baar,baart,baren,gebaard,baarde,baarden,
    [h([np_np,
	transitive,
	intransitive,
	sbar_subj_np,    % het baart zorgen dat ...
	sbar_subj_np_np, % het baart ons zorgen dat ...
                         % *het baart ons dat ...
	vp_subj_np,
	vp_subj_np_np,
	part_transitive(op)])]).

v(baat,baat,baten,gebaat,baatte,baatten,
  [h([intransitive,
      sbar_subj_so_np,
      sbar_subj_np,		% wat baat het dat ... ?
      sbar_subj_np_np,		% wat baat het ons dat .. ?
      part_transitive(uit),
      vp_subj_so_np,
      so_np
     ])]).

v(babbel,babbelt,babbelen,gebabbeld,babbelde,babbelden,
    [h([intransitive,
        part_intransitive(bij),
        part_intransitive(na),
        mod_pp(over)])]).

v(babysit,babysit,babysitten,gebabysit,babysitte,babysitten,
    [h([intransitive])]).

v(badminton,badmintont,badmintonnen,gebadmintond,badmintonde,badmintonden,
    [h([intransitive])]).

v(bagatelliseer,bagatelliseert,bagatelliseren,gebagatelliseerd,bagatelliseerde,bagatelliseerden,
    [h([transitive,
	intransitive
       ])]).

v(bagger,baggert,baggeren,gebaggerd,baggerde,baggerden,
    [h([intransitive,
        transitive,  % sloten, het veen
        part_transitive(op),
	part_transitive(uit)])]).
  

v(bak,bakt,bakken,gebakken,bakte,bakten,
    [h([transitive,
        part_intransitive(af),
        part_transitive(af),
	ap_pred_np,    % gaar, goudbruin
	mod_pp(in),    % olie of boter om in te bakken
	np_mod_pp(in), % olie of boter om oliebollen in te bakken
        fixed([mod_pp(in),ap_pred,acc],norm_passive), % iets bruin bakken in de olie
	np_np,       % iemand een poets bakken
	np_pc_pp(van)]),
     b([intransitive])]).

v(baken,bakent,bakenen,gebakend,bakende,bakenden,
    [h([transitive,
	part_transitive(af)])]).

v(baker,bakert,bakeren,gebakerd,bakerde,bakerden,
    [h([intransitive,
	part_transitive(in)])]).

v(bakkelei,bakkeleit,bakkeleien,gebakkeleid,bakkeleide,bakkeleiden,
    [h([intransitive,
        pc_pp(over),
	er_pp_sbar(over)
       ])]).

v(bal,balt,ballen,gebald,balde,balden,
    [z([part_intransitive(samen)]),
     h([intransitive,
	transitive,
	part_transitive(samen)])]).

v(balanceer,balanceert,balanceren,gebalanceerd,balanceerde,balanceerden,
    [h([transitive,
	ld_pp,
	ld_adv,
	part_transitive(uit)])]).

v(balk,balkt,balken,gebalkt,balkte,balkten,
    [h([intransitive])]).

v(balsem,balsemt,balsemen,gebalsemd,balsemde,balsemden,
    [h([intransitive,
	transitive])]).  

v(ban,bant,bannen,gebannen,bande,banden,
    [h([transitive,
	np_ld_pp,
	part_transitive(uit)])]).

v(banjer,banjert,banjeren,gebanjerd,banjerde,banjerden,
    [b([intransitive,
	part_intransitive(aan),
	ld_pp])]).  

v(bankier,bankiert,bankieren,gebankierd,bankierde,bankierden,
    [h([intransitive,
	transitive])]).

v(barbecue,barbecuet,barbecuen,gebarbecued,barbecuede,barbecueden,
    [h([intransitive,
	transitive])]).

v(barricadeer,barricadeert,barricaderen,gebarricadeerd,barricadeerde,barricadeerden,
  [h([% refl,
      transitive,
      np_mod_pp(met)])]).

v(barst,barst,barsten,gebarsten,barstte,barstten,
    [unacc([intransitive,
	    part_intransitive(los),
	    part_intransitive(open),
	    part_intransitive(uit),
	    part_dip_sbar(uit),
	    pc_pp(van),
	    part_pc_pp(los,in),
	    part_pc_pp(uit,in)])]).

v(bas,bast,bassen,gebast,baste,basten,
    [h([sbar])]).    % dip

v(baseer,baseert,baseren,gebaseerd,baseerde,baseerden,
    [h([np_pc_pp(op),
	refl_pc_pp(op)   % niet in ANS...?
       ])]).

v(basketbal,basketbalt,basketballen,gebasketbald,basketbalde,basketbalden,
    [h([intransitive])]).

v(bauw,bauwt,bauwen,gebauwd,bauwde,bauwden,
    [h([part_transitive(na)])]).

v(bazel,bazelt,bazelen,gebazeld,bazelde,bazelden,
    [h([intransitive,
	sbar,
	transitive])]).

v(bazuin,bazuint,bazuinen,gebazuind,bazuinde,bazuinden,
    [h([intransitive,
	transitive,
	part_sbar(rond),
	part_vp(rond),
	part_transitive(rond),
	part_sbar(uit),
	part_vp(uit),
	part_transitive(uit)])]).

v(beaam,beaamt,beamen,beaamd,beaamde,beaamden,
    [h([sbar,
	transitive,
	intransitive
       ])]).

v(beadem,beademt,beademen,beademd,beademde,beademden,
    [h([transitive])]).

v(beangstig,beangstigt,beangstigen,beangstigd,beangstigde,beangstigden,
    [h([transitive])]).

v(beantwoord,beantwoordt,beantwoorden,beantwoord,beantwoordde,beantwoordden,
  [h([transitive,
      acc_np_dip_sbar,
      pc_pp(aan)])]). %% beantwoorden aan het verwachtingspatroon

v(beargumenteer,beargumenteert,beargumenteren,beargumenteerd,beargumenteerde,beargumenteerden,
    [h([sbar,
	transitive])]).

v(beboet,beboet,beboeten,beboet,beboette,beboetten,
    [h([transitive])]).

v(bebos,bebost,bebossen,bebost,beboste,bebosten,
    [h([transitive])]).

v(beboter,bebotert,beboteren,beboterd,beboterde,beboterden,
    [h([transitive])]).

v(bebouw,bebouwt,bebouwen,bebouwd,bebouwde,bebouwden,
    [h([transitive])]).

v(becijfer,becijfert,becijferen,becijferd,becijferde,becijferden,
    [h([sbar,
	transitive,
	part_transitive(uit),
	part_transitive(weg),
	np_pc_pp(op)])]).

v(becommentarieer,becommentarieert,becommentariëren,becommentarieerd,becommentarieerde,becommentarieerden,
    [h([transitive])]).

v(beconcurreer,beconcurreert,beconcurreren,beconcurreerd,beconcurreerde,beconcurreerden,
    [h([transitive,
	np_ld_dir])]).

v(bed,bedt,bedden,gebed,bedde,bedden,
    [h([part_transitive(in),
	part_np_ld_pp(in)])]).

v(bedaar,bedaart,bedaren,bedaard,bedaarde,bedaarden,
    [unacc([intransitive]),
     h([transitive])]).  %unacc too?  *Hij werd bedaard

v(bedank,bedankt,bedanken,bedankt,bedankte,bedankten,
    [h([intransitive,
	transitive,
	er_pp_vp(voor),
	np_er_pp_vp(voor),
	np_er_pp_sbar(voor),
	np_sbar,
	fixed([als_pred],no_passive),
	np_pc_pp(voor),
	pc_pp(voor)])]).

v(bedeel,bedeelt,bedelen,bedeeld,bedeelde,bedeelden,
    [h([transitive,
	np_pc_pp(met),
	part_np_np(toe),
	part_np_sbar(toe),
	part_np_vp_obj(toe),
	part_so_pp_np(toe),
	part_transitive(toe)])]).

v(bedek,bedekt,bedekken,bedekt,bedekte,bedekten,
    [h([transitive,
	np_mod_pp(met)])]).

v(bedel,bedelt,bedelen,gebedeld,bedelde,bedelden,
  [h([intransitive,
      part_transitive(bijeen),
      pc_pp(om)])]).

v(bedelf,bedelft,bedelven,bedolven,bedolf,bedolven,
    [h([transitive,
	np_pc_pp(met),
	np_pc_pp(onder)])]).

v(bedenk,bedenkt,bedenken,bedacht,bedacht,bedachten,bedenke,
    [h([refl,
	sbar,
	refl_np,   % de grootste eer die ik me kan bedenken
	refl_sbar, % we moeten ons bedenken dat ...
	transitive,
	np_pc_pp(bij),
	refl_er_pp_sbar(bij), % we moeten ons daar wel bij bedenken dat
	er_pp_sbar(bij),      % we moeten er wel bij bedenken dat
        np_pc_pp(op),  % we moeten daar nog iets op bedenken
	part_np_np(toe),
        np_mod_pp(voor)])]).

v(bederf,bederft,bederven,bedorven,bedierf,bedierven,
    [unacc([intransitive]),
     h([transitive])]).

v(bedien,bedient,bedienen,bediend,bediende,bedienden,
  [h([transitive,
      intransitive,  % er wordt topless bediend
      fixed([svp_pp(op,wenk),acc],norm_passive),
				% np_pc_pp(van),
      refl_pc_pp(van)])]).

v(bedijk,bedijkt,bedijken,bedijkt,bedijkte,bedijkten,
    [h([transitive])]).

v(beding,bedingt,bedingen,bedongen,bedong,bedongen,
    [h([sbar,
	transitive])]).

v(bediscussieer,bediscussieert,bediscussiëren,bediscussieerd,bediscussieerde,bediscussieerden,
    [h([transitive,
	sbar])]).

v(bedissel,bedisselt,bedisselen,bedisseld,bedisselde,bedisselden,
    [h([transitive])]).

v(bedoel,bedoelt,bedoelen,bedoeld,bedoelde,bedoelden,
    [h([nonp_pred_np,
	tr_sbar,
	transitive,
	intransitive, % "ik bedoel maar"
	vp,
	acc_np_om_vp_no_control,  % het speelkwartier is bedoeld om uit te rusten
	np_pc_pp(met),
	er_pp_sbar(met),
	np_pc_pp(voor)])]).

v(bedonder,bedondert,bedonderen,bedonderd,bedonderde,bedonderden,
    [h([transitive])]).

v(bedot,bedot,bedotten,bedot,bedotte,bedotten,
    [h([transitive])]).

v(bedraag,bedraagt,bedragen,bedragen,bedroeg,bedroegen,
    [h([meas])]).

v(bedreig,bedreigt,bedreigen,bedreigd,bedreigde,bedreigden,
    [h([transitive,
	np_pc_pp(met)])]).

v(bedrieg,bedriegt,bedriegen,bedrogen,bedroog,bedrogen,
    [h([intransitive,
	transitive,
	np_mod_pp(met)])]).

v(bedrijf,bedrijft,bedrijven,bedreven,bedreef,bedreven,
    [h([transitive])]).

v(bedrink,bedrinkt,bedrinken,bedronken,bedronk,bedronken,
    [h([refl])]).

v(bedroef,bedroeft,bedroeven,bedroefd,bedroefde,bedroefden,
    [h([intransitive,
	so_np,
	sbar_subj_so_np,
	vp_subj_so_np
       ])]).

v(bedruip,bedruipt,bedruipen,bedropen,bedroop,bedropen,
    [h([refl,
	transitive])]).

v(bedruk,bedrukt,bedrukken,bedrukt,bedrukte,bedrukten,
    [h([transitive,
	so_np  % dan bedrukt ons het gevoel dat ...
       ])]).

v(bedruppel,bedruppelt,bedruppelen,bedruppeld,bedruppelde,bedruppelden,
    [h([transitive])]).

v(beduid,beduidt,beduiden,beduid,beduidde,beduidden,
    [h([np_np,
	np_sbar,
	np_vp_obj,
	sbar,
	transitive])]).

v(beduimel,beduimelt,beduimelen,beduimeld,beduimelde,beduimelden,
    [h([transitive])]).

v(beduvel,beduvelt,beduvelen,beduveld,beduvelde,beduvelden,
    [h([transitive])]).

v(bedwelm,bedwelmt,bedwelmen,bedwelmd,bedwelmde,bedwelmden,
    [h([transitive])]).

v(bedwing,bedwingt,bedwingen,bedwongen,bedwong,bedwongen,
    [h([% refl,
	transitive])]).

v(beëdig,beëdigt,beëdigen,beëdigd,beëdigde,beëdigden,
    [h([transitive,
        als_pred_np,
	intransitive
       ])]).

v(beërf,beërft,beërven,beërfd,beërfde,beërfden,
    [h([transitive])]).  % het koninkrijk van God

v(beef,beeft,beven,gebeefd,beefde,beefden,
    [h([intransitive,
	part_intransitive(na),
	pc_pp(van),
	mod_pp(voor)])]).

v(beeld,beeldt,beelden,gebeeld,beeldde,beeldden,
    [h([part_refl_np(in),
	part_refl_sbar(in),  % hij beeldt zich in, dat ...
	part_refl_vp(in),    % hij beeldt zich in op X te stemmen VL
	part_transitive(af),
	part_nonp_pred_np(af),
	part_transitive(uit),
	part_sbar(uit)
       ])]).

v(beeldhouw,beeldhouwt,beeldhouwen,gebeeldhouwd,beeldhouwde,beeldhouwden,
    [h([intransitive,
	transitive])]).

v(been,beent,benen,gebeend,beende,beenden,
    [z([intransitive,
	part_intransitive(aan),
        part_intransitive(bij),
	part_intransitive(weg),
	ld_dir,
	ld_pp]),
     h([part_transitive(bij),
	part_transitive(uit)])]).

v(beest,beest,beesten,gebeest,beestte,beestten,
    [h([intransitive])]).

v(bef,beft,beffen,gebeft,befte,beften,
    [h([intransitive,
	transitive])]).

v(bega,begaat,inflected(begaan,begane),begaan,beging,begingen,
    [h([intransitive,
	transitive])]).

v(begeef,begeeft,begeven,begeven,begaf,begaven,
    [h([refl_ld_dir,
	fixed([het_obj1],no_passive),
        intransitive,           % VL
        transitive,             % VL
	refl_ld_pp])]).

v(begeer,begeert,begeren,begeerd,begeerde,begeerden,
    [h([transitive])]).

v(begeleid,begeleidt,begeleiden,begeleid,begeleidde,begeleidden,
  [h([transitive,
      np_ld_pp,
      intransitive,
      np_mod_pp(bij)])]).

v(begenadig,begenadigt,begenadigen,begenadigd,begenadigde,begenadigden,
    [h([transitive])]).

v(begiet,begiet,begieten,begoten,begoot,begoten,
    [h([transitive])]).

v(begiftig,begiftigt,begiftigen,begiftigd,begiftigde,begiftigden,
    [h([np_pc_pp(met)])]).

v(begin,begint,beginnen,begonnen,begon,begonnen,
    [z([intransitive,  % ik begin
	transitive,    % hij begon zijn verhaal
	vp,            % dat hij begon een boek te lezen
	aux(te_inf),   % dat hij een boek begon te lezen
                       % we mogen niet beginnen zweven  VLAAMS
        dip_sbar,      % dat is schitterend , begint hij
	np_pc_pp(met), % dat hij zijn verhaal begon met een inleiding
	pc_pp(aan),    % hij begon aan zijn verhaal
	pc_pp(met),    % hij begon met een inleiding
	pc_pp(over),   % hij begon weer over zijn broer
	mod_pp(in),    % hij is er net in begonnen (het nieuwe boek?)
	mod_pp(tegen), % hij begint tegen winnaar Verkerk
	pc_pp(om),     % daar was het om begonnen
	mod_pp(in),    % over boeken. Je kunt niet meer stoppen als je er in begint !
	fixed([pc(om),dat],no_passive),  % daar was het ons om begonnen
        acc_np_dip_sbar, % zo begon hij zijn verhaal
	er_pp_sbar(met)  % het begon er mee dat ...
       ])]).

v(beglaas,beglaast,beglazen,beglaasd,beglaasde,beglaasden,
    [h([transitive])]).

v(begluur,begluurt,begluren,begluurd,begluurde,begluurden,
    [h([transitive])]).

v(begraaf,begraaft,begraven,begraven,begroef,begroeven,
    [h([transitive,
	np_ld_pp,
	intransitive  % in het centrum mag niet begraven worden
	% refl_ld_pp
       ])]).

v(begraas,begraast,begrazen,begraasd,begraasde,begraasden,
    [h([transitive])]).

v(begrens,begrenst,begrenzen,begrensd,begrensde,begrensden,
    [h([transitive])]).

v(begrijp,begrijpt,begrijpen,begrepen,begreep,begrepen,
    [h([intransitive,
	sbar,
	sbar_obj,   % begrijp ik het goed dat hij ...
	transitive,
	part_transitive(mis),
	np_pc_pp(van)])]).

v(begroei,begroeit,begroeien,begroeid,begroeide,begroeiden,
    [h([transitive])]).

v(begroet,begroet,begroeten,begroet,begroette,begroetten,
  [h([transitive,
      acc_np_dip_sbar
     ])]).

v(begroot,begroot,begroten,begroot,begrootte,begrootten,
    [h([transitive,
	intransitive,
	np_pc_pp(op)])]).

v(begunstig,begunstigt,begunstigen,begunstigd,begunstigde,begunstigden,
    [h([transitive])]).

v(behaag,behaagt,behagen,behaagd,behaagde,behaagden,
    [h([so_np,
	sbar_subj_so_np,
	vp_subj_so_np,
	transitive,
	intransitive
       ])]).

v(behaal,behaalt,behalen,behaald,behaalde,behaalden,
    [h([transitive,
	fixed([{[acc(succes),pc(met)]}],no_passive),
        fixed([{[acc(eer),pc(aan)]}],norm_passive)])]).

v(behandel,behandelt,behandelen,behandeld,behandelde,behandelden,
    [h([als_pred_np,
	transitive,
	intransitive,
	np_mod_pp(met),
	np_mod_pp(tegen),
	np_mod_pp(voor)])]).

v(behang,behangt,behangen,behangen,behing,behingen,
    [h([intransitive,
	transitive,
	np_pc_pp(met),
	pc_pp(met)])]).

v(behap,behapt,behappen,behappen,behapte,behapten,
    [h([transitive,
	sbar_obj])]).

v(behartig,behartigt,behartigen,behartigd,behartigde,behartigden,
    [h([transitive])]).

v(beheer,beheert,beheren,beheerd,beheerde,beheerden,
    [h([transitive])]).

v(beheers,beheerst,beheersen,beheerst,beheerste,beheersten,
    [h([transitive])]).

v(beheks,behekst,beheksen,behekst,behekste,beheksten,
    [h([transitive])]).

v(behelp,behelpt,behelpen,beholpen,behielp,behielpen,
    [h([refl,
        intransitive % het blijf behelpen
       ])]).

v(behels,behelst,[behelzen,behelsen],behelsd,behelsde,behelsden,
    [h([sbar,
	transitive])]).

v(behoed,behoedt,behoeden,behoed,behoedde,behoedden,
    [h([transitive,
	np_pc_pp(voor)])]).

v(behoef,behoeft,behoeven,behoefd,behoefde,behoefden,
    [h([intransitive,
	sbar_subj_np, % het behoeft geen betoog dat ..
	transitive,
	aux(te)])]).

v(behoor,behoort,behoren,behoord,behoorde,behoorden,
  [h([intransitive,  % zoals het behoort (ouderwets)
      so_np,
      so_pp,
      pc_pp(bij),
      pc_pp(in),
      pc_pp(tot),
      pp_sbar_subj(tot),     % het behoort tot de mogelijkheden dat...
      pp_vp_subj(tot), % het behoort tot de traditie om zo nu en dan...
      part_so_pp(toe),
      part_so_np(toe),
      aux(te)])]).

v([behoud,behoud],behoudt,behouden,behouden,behield,behielden,
  [h([transitive,
      als_pred_np,
	part_np_np(voor),
	part_transitive(voor),
	part_np_pc_pp(voor,aan),
	part_np_pc_pp(voor,voor)
       ])]).

v(beid,beidt,beiden,gebeid,beidde,beidden,
    [h([intransitive])]).

v(beier,beiert,beieren,gebeierd,beierde,beierden,
    [h([intransitive,
       	mod_pp(doorheen)
       ])]).

v(beijver,beijvert,beijveren,beijverd,beijverde,beijverden,
    [h([refl,
	refl_vp,
	refl_pc_pp(voor)])]).

v(beïnvloed,beïnvloedt,beïnvloeden,beïnvloed,beïnvloedde,beïnvloedden,
  [h([transitive,
      sbar_subj_so_np % het heeft me beinvloed dat ...
     ])]).

v(beitel,beitelt,beitelen,gebeiteld,beitelde,beitelden,
    [h([intransitive,
	transitive])]).

v(beits,beitst,beitsen,gebeitst,beitste,beitsten,
    [h([intransitive,
	transitive])]).

v(bejaag,bejaagt,bejagen,bejaagd,bejaagde,bejaagden,
    [h([transitive])]).

v(bejegen,bejegent,bejegenen,bejegend,bejegende,bejegenden,
    [h([transitive,
	np_mod_pp(met)])]).

v(bejubel,bejubelt,bejubelen,bejubeld,bejubelde,bejubelden,
    [h([transitive])]).

v(bek,bekt,bekken,gebekt,bekte,bekten,
    [h([intransitive,
	part_transitive(af)])]).  

v(bekabel,bekabelt,bekabelen,bekabeld,bekabelde,bekabelden,
    [h([intransitive,
        transitive])]).    

v(bekamp,bekampt,bekampen,bekampt,bekampte,bekampten,
    [h([intransitive,
        transitive])]).  

v(bekeer,bekeert,bekeren,bekeerd,bekeerde,bekeerden,
    [h([refl,               % word order: daar bekeerden zich tienduizenden ..
	refl_pc_pp(tot),
	transitive,
	np_pc_pp(tot)])]).

v(beken,bekent,bekennen,bekend,bekende,bekenden,
    [h([intransitive,
	np_sbar,
	np_np,
	so_pp_np,
	so_pp_sbar,
	sbar,
	transitive,
	fixed([pc(van),acc(spoor)],norm_passive),
	vp
       ])]).

v(beker,bekert,bekeren,gebekerd,bekerde,bekerden,
    [h([intransitive])]).

v(bekeur,bekeurt,bekeuren,bekeurd,bekeurde,bekeurden,
  [h([transitive,
      intransitive])]).

v(bekijk,bekijkt,bekijken,bekeken,bekeek,bekeken,
    [h([sbar,
	transitive,
	pc_pp(op),  % controleren op
        als_pred_np])]).

v(beklaag,beklaagt,beklagen,beklaagd,
  [beklaagde,bekloeg],
  [beklaagden,bekloegen],
    [h([refl,
        refl_np,  % Vlaams "Ik heb me deze overstap niet beklaagd"
	refl_sbar,
        refl_vp,
	transitive,
	refl_pc_pp(over),
	refl_er_pp_sbar(over)])]).

v(beklad,bekladt,bekladden,beklad,bekladde,bekladden,
    [h([transitive])]).

v(bekleed,bekleedt,bekleden,bekleed,bekleedde,bekleedden,
    [h([transitive,
	np_mod_pp(met)])]).

v(beklem,beklemt,beklemmen,beklemd,beklemde,beklemden,
    [h([sbar_subj_so_np,
        so_np,
	transitive,
        intransitive,
        vp_subj_so_np])]).

v(beklemtoon,beklemtoont,beklemtonen,beklemtoond,beklemtoonde,beklemtoonden,
    [h([sbar,
	transitive,
	vp])]).

v(beklijf,beklijft,beklijven,beklijfd,beklijfde,beklijfden,
    [unacc([intransitive])]).

v(beklim,beklimt,beklimmen,beklommen,beklom,beklommen,
    [h([transitive])]).

v(beklink,beklinkt,beklinken,beklonken,beklonk,beklonken,
    [h([transitive])]).

v(beklop,beklopt,bekloppen,beklopt,beklopte,beklopten,
    [h([transitive])]).

v(beknel,beknelt,beknellen,bekneld,beknelde,beknelden,
    [h([sbar_subj_so_np,
        so_np,
        intransitive,
	transitive])]).

v(beknibbel,beknibbelt,beknibbelen,beknibbeld,beknibbelde,beknibbelden,
    [h([intransitive,
	pc_pp(op)])]).

v(beknot,beknot,beknotten,beknot,beknotte,beknotten,
    [h([transitive])]).

v(bekoel,bekoelt,bekoelen,bekoeld,bekoelde,bekoelden,
    [unacc([intransitive])]).

v(bekogel,bekogelt,bekogelen,bekogeld,bekogelde,bekogelden,
    [h([transitive,
	np_mod_pp(met)])]).

v(bekokstoof,bekokstooft,bekokstoven,bekokstoofd,bekokstoofde,bekokstoofden,
    [h([transitive])]).

v(bekom,bekomt,bekomen,bekomen,bekwam,bekwamen,
    [unacc([intransitive,
	    so_np,
	    pc_pp(van)]),
     h([transitive,
	sbar])]).

v(bekommer,bekommert,bekommeren,bekommerd,bekommerde,bekommerden,
    [h([refl,
	% transitive,
	refl_pc_pp(om),
	refl_er_pp_sbar(om),
	refl_pc_pp(over),
	refl_er_pp_sbar(over)])]).

v(bekoop,bekoopt,bekopen,bekocht,bekocht,bekochten,
    [h([transitive,
	np_pc_pp(met)  % met de dood bekopen
       ])]).

v(bekoor,bekoort,bekoren,bekoord,bekoorde,bekoorden,
    [h([transitive,
	intransitive])]).

v(bekort,bekort,bekorten,bekort,bekortte,bekortten,
    [h([transitive])]).

v(bekostig,bekostigt,bekostigen,bekostigd,bekostigde,bekostigden,
    [h([transitive])]).

v(bekrachtig,bekrachtigt,bekrachtigen,bekrachtigd,bekrachtigde,bekrachtigden,
    [h([transitive,
	sbar])]).

v(bekras,bekrast,bekrassen,bekrast,bekraste,bekrasten,
    [h([transitive])]).

v(bekreun,bekreunt,bekreunen,bekreund,bekreunde,bekreunden,
    [h([refl,
	refl_pc_pp(om),
	refl_pc_pp(over)])]).

v(bekrimp,bekrimpt,bekrimpen,bekrompen,bekromp,bekrompen,
    [h([refl])]).

v(bekritiseer,bekritiseert,bekritiseren,bekritiseerd,bekritiseerde,bekritiseerden,
    [h([sbar,
	transitive])]).

v(bekroon,bekroont,bekronen,bekroond,bekroonde,bekroonden,
    [h([transitive,
	np_pc_pp(met)])]).

v(bekruip,bekruipt,bekruipen,bekropen,bekroop,bekropen,
    [h([transitive,
	so_np])]).

v(bekvecht,bekvecht,bekvechten,gebekvecht,bekvechtte,bekvechtten,
    [h([intransitive,
	pc_pp(over),
	er_pp_sbar(over)
       ])]).

v(bekwaam,bekwaamt,bekwamen,bekwaamd,bekwaamde,bekwaamden,
    [h([transitive,
	np_pc_pp(in),
	np_pc_pp(tot)
	%refl_pc_pp(in),
	%refl_pc_pp(tot)
       ])]).

v(bel,belt,bellen,gebeld,belde,belden,
    [h([intransitive,
	sbar,
	acc_np_sbar,  % * mij werd gebeld of ik kwam
	              %    ik werd gebeld of ik kwam
	transitive,
	ld_pp,
	np_ld_dir,    % ik belde hem zijn bed uit
	part_ld_pp(in),
	part_ld_pp(op),
	part_ld_pp(terug),
	part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(in),
	part_intransitive(op),
	part_sbar(op),  % hij belde op dat hij niet kon
	part_intransitive(rond),
	part_intransitive(terug),
	part_transitive(af),
	part_transitive(door),  % hij belde de uitslag door
	part_np_np(door),       % hij belde ons de uitslag door
	part_transitive(op),
	part_transitive(plat),
	part_transitive(terug),
	np_mod_pp(over),
	mod_pp(met)])]).

v(belaad,belaadt,beladen,beladen,belaadde,belaadden,
    [h([transitive,
	np_pc_pp(met)])]).

v(belaag,belaagt,belagen,belaagd,belaagde,belaagden,
    [h([transitive])]).

v(beland,belandt,belanden,beland,belandde,belandden,
    [unacc([ld_adv,
	    ld_pp,
            intransitive, % hack, for "er"
	    part_intransitive(aan),
	    part_ld_pp(aan),
	    part_ld_adv(aan)])]).

v(belang,belangt,belangen,belangd,belangde,belangden,
  [h([so_np,  % wat mij belangt (ouderwets)
      part_transitive(aan)])]).

v(belast,belast,belasten,belast,belastte,belastten,
    [h([transitive,
	np_pc_pp(met)
	% refl_pc_pp(met)
       ])]).

v(belaster,belastert,belasteren,belasterd,belasterde,belasterden,
    [h([transitive])]).

v(belazer,belazert,belazeren,belazerd,belazerde,belazerden,
    [h([transitive])]).

v(beledig,beledigt,beledigen,beledigd,beledigde,beledigden,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(beleef,beleeft,beleven,beleefd,beleefde,beleefden,
    [h([transitive,
	sbar_obj,
	sbar,
	np_pc_pp(aan),          % lol, plezier, weinig, veel, meer
	np_er_pp_sbar(aan),     % lol, plezier, weinig, veel, meer
	np_er_pp_vp(aan)        % lol, plezier, weinig, veel, meer
       ])]).

v(beleen,beleent,belenen,beleend,beleende,beleenden,
    [h([intransitive,
	transitive,
	np_pc_pp(met)
       ])]).

v(beleg,belegt,beleggen,belegd,belegde,belegden,
    [h([intransitive,
	transitive,
	np_pc_pp(in),
	pc_pp(in)])]).

v(beleger,belegert,belegeren,belegerd,belegerde,belegerden,
    [h([transitive])]).

v(belemmer,belemmert,belemmeren,belemmerd,belemmerde,belemmerden,
    [h([np_np,
	np_vp_obj,
	np_vp_obj1,
	sbar_subj_so_np,
	transitive,
	intransitive,
	vp_subj_so_np,
	np_mod_pp(bij),
	np_pc_pp(in),
	pc_pp(in)])]).

v(belet,belet,beletten,belet,belette,beletten,
    [h([np_np,
	np_vp_obj,
	sbar,
	so_np,
	transitive,
	vp])]).

v(belichaam,belichaamt,belichamen,belichaamd,belichaamde,belichaamden,
    [h([transitive])]).

v(belicht,belicht,belichten,belicht,belichtte,belichtten,
    [h([transitive])]).

v(belief,belieft,believen,beliefd,beliefde,beliefden,
    [h([het_subj,  % ?
	vp_subj_so_np,   % tot het hem beliefde naar buiten te komen
	subj_control(te),
	transitive])]).

v(belieg,beliegt,beliegen,belogen,beloog,belogen,
    [h([transitive])]).

v(belijd,belijdt,belijden,beleden,beleed,beleden,
    [h([intransitive,
	sbar,
	transitive])]).

v(beloer,beloert,beloeren,beloerd,beloerde,beloerden,
    [h([transitive])]).

v(beloof,belooft,beloven,beloofd,beloofde,beloofden,
  [h([np_np,                    % hij beloofde ons gouden bergen
      np_sbar,                  % hij beloofde ons dat hij zou komen
      so_pp_sbar,
      np_vp_subj,               % hij beloofde ons te zullen komen
      tr_sbar,                  % hij beloofde dat hij zou komen
      so_pp_np,                 % hij beloofde aan ons gouden bergen
      transitive,               % hij beloofde gouden bergen
      vp,                       % hij beloofde te komen

      aux(te),                  % het beloven mooie dagen te worden
      
      intransitive              % VL: dat belooft voor de toekomst

     
     ])]).

v(beloon,beloont,belonen,beloond,beloonde,beloonden,
    [h([transitive,
        np_mod_pp(met),
	np_mod_pp(voor)])]).

v(beloop,beloopt,belopen,belopen,beliep,beliepen,
    [h([transitive])]).

v(beluister,beluistert,beluisteren,beluisterd,beluisterde,beluisterden,
    [h([transitive,
	sbar])]).

v(bemaal,bemaalt,bemalen,bemaald,bemaalde,bemaalden,
    [h([transitive])]).

v(bemachtig,bemachtigt,bemachtigen,bemachtigd,bemachtigde,bemachtigden,
    [h([transitive])]).

v(beman,bemant,bemannen,bemand,bemande,bemanden,
    [h([transitive])]).

v(bemerk,bemerkt,bemerken,bemerkt,bemerkte,bemerkten,
    [h([sbar,
        np_pc_pp(van),
	transitive])]).

v(bemest,bemest,bemesten,bemest,bemestte,bemestten,
  [h([intransitive,   % er kan nu niet worden bemest
      transitive])]).

v(bemiddel,bemiddelt,bemiddelen,bemiddeld,bemiddelde,bemiddelden,
    [h([intransitive,
        transitive, % de werklozen worden bemiddeld...
	pc_pp(tussen)])]).

v(bemin,bemint,beminnen,bemind,beminde,beminden,
  [h([transitive,
      intransitive])]).

v(bemoedig,bemoedigt,bemoedigen,bemoedigd,bemoedigde,bemoedigden,
    [h([transitive,
	intransitive,
	np_vp_obj1])]).

v(bemoei,bemoeit,bemoeien,bemoeid,bemoeide,bemoeiden,
    [h([refl,
	refl_er_pc_pp(tegenaan),
	refl_pc_pp(met)])]).

v(bemoeilijk,bemoeilijkt,bemoeilijken,bemoeilijkt,bemoeilijkte,bemoeilijkten,
  [h([transitive,
      np_np])]).

v(benadeel,benadeelt,benadelen,benadeeld,benadeelde,benadeelden,
    [h([transitive])]).

v(benader,benadert,benaderen,benaderd,benaderde,benaderden,
    [h([transitive,
	acc_np_sbar,
	np_mod_pp(over)])]).

v(benadruk,benadrukt,benadrukken,benadrukt,benadrukte,benadrukten,
    [h([sbar,
	transitive,
	vp])]).

v(benauw,benauwt,benauwen,benauwd,benauwde,benauwden,
    [h([sbar_subj_so_np,
        so_np,
	transitive,
        intransitive, % dat benauwt
	vp_subj_so_np])]).

v(beneem,beneemt,benemen,benomen,benam,benamen,
    [h([np_np,
	transitive])]).

v(benevel,benevelt,benevelen,beneveld,benevelde,benevelden,
    [h([transitive])]).

v(bengel,bengelt,bengelen,gebengeld,bengelde,bengelden,
    [h([intransitive,
	ld_pp,
	ld_adv])]).

v(benieuw,benieuwt,benieuwen,benieuwd,benieuwde,benieuwden,
    [h([so_np,
	sbar_subj_so_np])]).

v(benijd,benijdt,benijden,benijd,benijdde,benijdden,
    [h([intransitive,
	transitive,
	np_pc_pp(om)])]).

v(benodig,benodigt,benodigen,benodigd,benodigde,benodigden,
    [h([transitive])]).

v(benoem,benoemt,benoemen,benoemd,benoemde,benoemden,
    [h([transitive,
        als_pred_np,
	np_pc_pp(tot)])]).

v(benut,benut,benutten,benut,benutte,benutten,
  [h([transitive,
      np_mod_pp(van)
     ])]).

v(beoefen,beoefent,beoefenen,beoefend,beoefende,beoefenden,
    [h([transitive])]).

v(beoog,beoogt,beogen,beoogd,beoogde,beoogden,
    [h([transitive,
	sbar,
	vp])]).

v(beoordeel,beoordeelt,beoordelen,beoordeeld,beoordeelde,beoordeelden,
    [h([sbar,
	als_pred_np,
	als_pred_np_sbar,
	als_pred_np_vp,
	transitive,
        intransitive,
        pc_pp(op),
	np_pc_pp(op)])]).  % iemand op zijn uiterlijk

v(bepaal,bepaalt,bepalen,bepaald,bepaalde,bepaalden,
    [h([sbar,
	transitive,
	intransitive,
	np_pc_pp(tot),
	np_pc_pp(op),
	refl_pc_pp(bij),
	refl_er_pp_vp(bij),
	refl_pc_pp(tot),
	refl_er_pp_vp(tot)
       ])]).

v(bepantser,bepantsert,bepantseren,bepantserd,bepantserde,bepantserden,
    [h([transitive,
	intransitive
       ])]).

v(beperk,beperkt,beperken,beperkt,beperkte,beperkten,
    [h([refl,
	transitive,
	np_pc_pp(in),
        np_pc_pp(tot),
	refl_er_pp_vp(tot),
	refl_pc_pp(tot)])]).

v(beplak,beplakt,beplakken,beplakt,beplakte,beplakten,
    [h([transitive])]).

v(beplant,beplant,beplanten,beplant,beplantte,beplantten,
    [h([transitive,
	np_mod_pp(met)])]).

v(bepleit,bepleit,bepleiten,bepleit,bepleitte,bepleitten,
  [h([sbar,
      vp,
      transitive])]).

v(bepraat,bepraat,bepraten,bepraat,bepraatte,bepraatten,
    [h([transitive])]).

v(beproef,beproeft,beproeven,beproefd,beproefde,beproefden,
    [h([transitive,
	vp])]).

v(beraad,beraadt,beraden,beraden,[beried,beraadde],[berieden,beraadden],
    [h([refl,
	refl_sbar,  % of ..
	refl_pc_pp(op),
	refl_pc_pp(over)])]).

v(beraadslaag,beraadslaagt,beraadslagen,beraadslaagd,beraadslaagde,beraadslaagden,
    [h([intransitive,
	pc_pp(over),
	er_pp_sbar(over)
       ])]).

v(beraam,beraamt,beramen,beraamd,beraamde,beraamden,
    [h([transitive])]).

v(berecht,berecht,berechten,berecht,berechtte,berechtten,
    [h([transitive])]).

v(beredder,bereddert,beredderen,beredderd,beredderde,beredderden,
    [h([transitive])]).

v(beredeneer,beredeneert,beredeneren,beredeneerd,beredeneerde,beredeneerden,
    [h([transitive,
	sbar,
	ap_pred_np % dat hij zijn bedoelingen soms kapot redeneert .
       ])]).

v(bereid,bereidt,bereiden,bereid,bereidde,bereidden,
    [h([np_np,
	transitive,
	np_mod_pp(van),
	np_mod_pp(met),
	part_transitive(toe),
	part_transitive(voor),
	part_np_er_pp_sbar(voor,op),
	part_np_pc_pp(voor,op),
	part_obj_np_er_pp_vp(voor,op),
	part_np_pc_pp(toe,met),
	part_pc_pp(voor,op)])]).

v(bereik,bereikt,bereiken,bereikt,bereikte,bereikten,
    [h([sbar,
        np_mod_pp(over), % overeenstemming bereiken over
        np_mod_pp(met),  % of je er succes mee bereikt
        np_mod_pp(in),   % of je er succes in bereikt
	transitive,
	so_np            % toen bereikte mij het droeve bericht dat ...
                         % *ik werd bereikt door het droeve bericht
       ])]).

v(bereis,bereist,bereizen,bereisd,bereisde,bereisden,
    [h([transitive])]).

v(bereken,berekent,berekenen,berekend,berekende,berekenden,
    [h([np_np,
	sbar,
	transitive,
	np_pc_pp(op),
	part_transitive(door),
	part_np_pc_pp(door,aan),
	part_np_pc_pp(door,in),
	part_np_pc_pp(door,naar)])]).

v(berg,bergt,bergen,geborgen,borg,borgen,
    [h([refl,
	transitive,
	np_ld_pp,
	part_transitive(op),
	part_np_ld_pp(op),
	part_transitive(weg),
	part_np_ld_pp(weg)])]).

v(bericht,bericht,berichten,bericht,berichtte,berichtten,
    [h([sbar,
	so_pp_np,
        np_np, % dat heeft de minister de Kamer bericht
	transitive,
	vp,
	np_vp_subj,
	so_pp_vp,
	pc_pp(over),
	so_np_pc_pp(over),
	fixed([{[dat_pp(aan),pc(over)]}],no_passive)
       ])]).

v(berijd,berijdt,berijden,bereden,bereed,bereden,
    [h([transitive])]).

v(berisp,berispt,berispen,berispt,berispte,berispten,
  [h([transitive,
      acc_np_dip_sbar,
      intransitive
     ])]).

v(beroem,beroemt,beroemen,beroemd,beroemde,beroemden,
    [h([refl_pc_pp(op),
        refl_er_pp_vp(op),
        refl_er_pp_sbar(op)])]).

v(beroep,beroept,beroepen,beroepen,beriep,beriepen,
    [h([transitive,
	np_pc_pp(tot),
	refl_pc_pp(in),
	refl_pc_pp(op),
	refl_er_pp_sbar(op),
	refl_er_pp_vp(op)])]).

v(beroer,beroert,beroeren,beroerd,beroerde,beroerden,
    [h([so_np  % word-order hoe heeft jou dat beroerd
       ])]).

v(berokken,berokkent,berokkenen,berokkend,berokkende,berokkenden,
    [h([np_np,
	sbar_subj_np_np,  % het berokkent ons schade dat ..
	sbar_subj_np,     % het berokkent schade dat ..
                          % *het berokkent ons dat ..
	transitive])]).

v(beroof,berooft,beroven,beroofd,beroofde,beroofden,
    [h([transitive,
	np_pc_pp(van)])]).

v(berouw,berouwt,berouwen,berouwd,berouwde,berouwden,
    [h([so_np,
        transitive,
        sbar_subj_so_np
       ])]).

v(berust,berust,berusten,berust,berustte,berustten,
    [h([pc_pp(in),
	er_pp_sbar(in),
	pc_pp(bij),
	pc_pp(op),
	sbar,  % dip
	intransitive])]).

v(beschaaf,beschaaft,beschaven,beschaafd,beschaafde,beschaafden,
    [h([transitive])]).

v(beschaam,beschaamt,beschamen,beschaamd,beschaamde,beschaamden,
    [h([transitive,
	np_pc_pp(in)])]).

v(beschadig,beschadigt,beschadigen,beschadigd,beschadigde,beschadigden,
  [unacc([intransitive]),       % ze beschadigen makkelijk
   h([transitive])]).

v(beschaduw,beschaduwt,beschaduwen,beschaduwd,beschaduwde,beschaduwden,
    [h([transitive])]).

v(bescherm,beschermt,beschermen,beschermd,beschermde,beschermden,
  [h([transitive,
      intransitive,
      pc_pp(tegen),
      np_pc_pp(tegen)])]).

v(bescheur,bescheurt,bescheuren,bescheurd,bescheurde,bescheurden,
    [h([refl,
        refl_pc_pp(van)])]).

v(beschiet,beschiet,beschieten,beschoten,beschoot,beschoten,
    [h([transitive,
	np_pc_pp(met)])]).

v(beschijn,beschijnt,beschijnen,beschenen,bescheen,beschenen,
    [h([transitive])]).

v(beschik,beschikt,beschikken,beschikt,beschikte,beschikten,
    [h([transitive,
	intransitive, % anders
	pc_pp(op),
	pc_pp(over)])]).

v(beschilder,beschildert,beschilderen,beschilderd,beschilderde,beschilderden,
    [h([transitive,
	part_transitive(na),
	np_pc_pp(in)])]).

v(beschimmel,beschimmelt,beschimmelen,beschimmeld,beschimmelde,beschimmelden,
  [unacc([intransitive])]).

v(beschimp,beschimpt,beschimpen,beschimpt,beschimpte,beschimpten,
    [h([transitive])]).

v(beschouw,beschouwt,beschouwen,beschouwd,beschouwde,beschouwden,
    [h([als_pred_np,
	als_pred_np_sbar,
	als_pred_np_vp,
	transitive,
        fixed([svp_pp(op,keper)],no_passive)])]).

v(beschrijf,beschrijft,beschrijven,beschreven,beschreef,beschreven,
    [h([np_sbar,
	so_pp_sbar,
	sbar,
	np_mod_pp(in),
	np_mod_pp(van),
	als_pred_np,
	transitive])]).

v(beschuldig,beschuldigt,beschuldigen,beschuldigd,beschuldigde,beschuldigden,
    [h([transitive,
	np_er_pp_sbar(van),
	np_pc_pp(van),
	np_vp_obj,
	acc_np_sbar,
	obj_np_er_pp_vp(van)])]).

v(beschut,beschut,beschutten,beschut,beschutte,beschutten,
    [h([transitive,
	np_pc_pp(tegen)])]).

v(besef,beseft,beseffen,beseft,besefte,beseften,
    [h([sbar,
	transitive,
        %% voetballers zeggen ook:
        refl_sbar,
        refl_np
       ])]).

v(besla,beslaat,beslaan,beslagen,besloeg,besloegen,
    [unacc([intransitive]),
     h([transitive,
	np_pc_pp(met)])]).

v(beslecht,beslecht,beslechten,beslecht,beslechtte,beslechtten,
    [h([transitive])]).

v(beslis,beslist,beslissen,beslist,besliste,beslisten,
  [h([sbar,
      transitive,
      intransitive,		% anders, tijdig
      part_intransitive(mee),
      part_pc_pp(mee,over),
      vp,
      pc_pp(tot),
      pc_pp(over),
      er_pp_sbar(over)
     ])]).

v(besluip,besluipt,besluipen,beslopen,besloop,beslopen,
    [h([transitive])]).

v(besluit,besluit,besluiten,besloten,besloot,besloten,
    [h([sbar,
	transitive,
	vp,
	van_sbar,
	intransitive, % anders; fixed vc
        part_intransitive(naast), % Vlaams voetbal 
        part_intransitive(over),  % Vlaams voetbal
        part_intransitive(raak),  % Vlaams voetbal
        part_intransitive(voorlangs),  % Vlaams voetbal
	pc_pp(tot),
	er_pp_sbar(tot),
	er_pp_vp(tot),
	np_pc_pp(met),
	pc_pp(met),
	pc_pp(over)
       ])]).

v(besmeer,besmeert,besmeren,besmeerd,besmeerde,besmeerden,
  [h([transitive,
      intransitive,   % Rooster het brood en besmeer desgewenst met boter . 
      np_pc_pp(met)])]).

v(besmet,besmet,besmetten,besmet,besmette,besmetten,
    [h([transitive,
	np_pc_pp(met)])]).

v(besmeur,besmeurt,besmeuren,besmeurd,besmeurde,besmeurden,
    [h([transitive,
	np_pc_pp(met)])]).

v(besnijd,besnijdt,besnijden,besneden,besneed,besneden,
    [h([transitive])]).

v(besnuffel,besnuffelt,besnuffelen,besnuffeld,besnuffelde,besnuffelden,
    [h([transitive])]).

v(besodemieter,besodemietert,besodemieteren,besodemieterd,besodemieterde,besodemieterden,
    [h([transitive])]).

v(bespaar,bespaart,besparen,bespaard,bespaarde,bespaarden,
    [h([np_np,
	transitive,
	intransitive,
	pc_pp(op),
	np_pc_pp(van),  % alleen in passief: van blessures bespaard blijven
	np_pc_pp(met),
        np_np_pc_pp(met),
	np_pc_pp(op)])]).

v(bespan,bespant,bespannen,bespannen,bespande,bespanden,
    [h([transitive,
	np_pc_pp(met)])]).

v(bespeel,bespeelt,bespelen,bespeeld,bespeelde,bespeelden,
    [h([transitive])]).

v(bespeur,bespeurt,bespeuren,bespeurd,bespeurde,bespeurden,
    [h([sbar,
        np_pc_pp(van),
	transitive])]).

v(bespied,bespiedt,bespieden,bespied,bespiedde,bespiedden,
    [h([transitive])]).

v(bespioneer,bespioneert,bespioneren,bespioneerd,bespioneerde,bespioneerden,
    [h([transitive])]).

v(bespoedig,bespoedigt,bespoedigen,bespoedigd,bespoedigde,bespoedigden,
    [h([transitive])]).

v(bespot,bespot,bespotten,bespot,bespotte,bespotten,
  [h([transitive,
      np_pc_pp(om)])]).

v(bespreek,bespreekt,bespreken,besproken,besprak,bespraken,
    [h([sbar,
	transitive,
	np_mod_pp(met)])]).

v(besprenkel,besprenkelt,besprenkelen,besprenkeld,besprenkelde,besprenkelden,
    [h([transitive,
	intransitive, % in recepten
	np_pc_pp(met)])]).

v(bespring,bespringt,bespringen,besprongen,besprong,besprongen,
    [h([transitive])]).

v(besproei,besproeit,besproeien,besproeid,besproeide,besproeiden,
    [h([transitive,
	np_pc_pp(met)])]).

v(bespuit,bespuit,bespuiten,bespoten,bespoot,bespoten,
    [h([transitive,
	np_pc_pp(met)])]).

v(bespuug,bespuugt,bespugen,bespuugd,bespuugde,bespuugden,
    [h([transitive])]).

v(besta,bestaat,inflected(bestaan,bestane),bestaan,bestond,bestonden,
    [h([intransitive,
	part_intransitive(voort),
	sbar_subj,   % hoe bestaat het dat ..
	vp_obj,	     % hij bestond het om ..
	meas,        % de vereniging bestaat 10 jaar
	fixed([subj(aandacht),pc(voor)],no_passive),
        fixed([pc(tot),subj(aanleiding)],no_passive),
        fixed([pc(voor),subj(aanleiding)],no_passive),
	fixed([subj(akkoord),pc(over)],no_passive),
	fixed([subj(antwoord),pc(op)],no_passive),
	fixed([subj(behoefte),pc(aan)],no_passive),
	fixed([subj(belangstelling),pc(voor)],no_passive),
        fixed([subj(bereidheid),pc(tot)],no_passive),
	fixed([subj(bezwaar),pc(tegen)],no_passive),
	fixed([subj(bezwaar),er_pp(tegen,A),extra_sbar(A)],no_passive),
	fixed([subj(bezwaar),er_pp(tegen,A),extra_vp(A)],no_passive),
	fixed([subj(consensus),pc(over)],no_passive),
	fixed([subj(controle),pc(op)],no_passive),
	fixed([subj(discussie),pc(over)],no_passive),
	fixed([subj(duidelijkheid),pc(over)],no_passive),
	fixed([subj(draagvlak),pc(voor)],no_passive),
	fixed([subj(eensgezindheid),pc(over)],no_passive),
	fixed([subj(gebrek),pc(aan)],no_passive),
	fixed([subj(gevaar),pc(voor)],no_passive),
	fixed([subj(informatie),pc(over)],no_passive),
	fixed([subj(interesse),pc(voor)],no_passive),
	fixed([subj(jurisprudentie),pc(over)],no_passive),
	fixed([subj(kans),pc(op)],no_passive),
	fixed([subj(kritiek),pc(op)],no_passive),
	fixed([subj(literatuur),pc(over)],no_passive),
	fixed([subj(misverstand),pc(over)],no_passive),
	fixed([subj(misverstand),er_pp(over,X),extra_sbar(X)],no_passive),
	fixed([subj(noodzaak),pc(voor)],no_passive),
	fixed([subj(onduidelijkheid),pc(over)],no_passive),
	fixed([subj(onzekerheid),pc(over)],no_passive),
	fixed([subj(opvatting),pc(over)],no_passive),	
	fixed([subj(overeenstemming),pc(over)],no_passive),
	fixed([subj(reden),pc(voor)],no_passive),
	fixed([subj(relatie),pc(tussen)],no_passive),
        fixed([subj(remedie),pc(tegen)],no_passive),
	fixed([subj(twijfel),pc(over)],no_passive),
	fixed([subj(twijfel),er_pp(over,X),extra_sbar(X)],no_passive),
	fixed([subj(twijfel),er_pp(over,X),extra_vp(X)],no_passive),
	fixed([subj(twijfel),pc(aan)],no_passive),
	fixed([subj(twijfel),er_pp(aan,X),extra_sbar(X)],no_passive),
	fixed([subj(twijfel),er_pp(aan,X),extra_vp(X)],no_passive),
	fixed([subj(variatie),pc(op)],no_passive),
	fixed([subj(verschil),pc(tussen)],no_passive),
	fixed([subj(verschil),pc(over)],no_passive), % verschil van mening over ..
        fixed([subj(verklaring),pc(voor)],no_passive),
        fixed([subj(verplichting),pc(tot)],no_passive),
        fixed([subj(visie),pc(op)],no_passive),
	fixed([subj(vraag),pc(naar)],no_passive),
	fixed([subj(weerstand),pc(tegen)],no_passive),
	fixed([subj(weerzin),pc(tegen)],no_passive),
        fixed([subj(wil),pc(tot)],no_passive),
	fixed([subj(woord),pc(voor)],no_passive),
	fixed([subj(zekerheid),pc(over)],no_passive),
	fixed([subj(zicht),pc(op)],no_passive),
	er_pp_vp_no_control(in),
	er_pp_sbar(in),
	pc_pp(uit),
	er_pp_sbar(uit),
	er_pp_vp_no_control(uit),
	pc_pp(van)])]).

v(besteed,besteedt,besteden,besteed,besteedde,besteedden,
    [h([transitive,
	np_pc_pp(aan),
	part_transitive(aan),
	part_intransitive(aan),  % overal waar wordt aanbesteed, wordt gefraudeerd
	                         % er wordt onderhands aanbesteed
	part_transitive(uit),
        part_intransitive(uit),
	part_np_pc_pp(uit,aan),
	intransitive])]).

v(besteel,besteelt,bestelen,bestolen,bestal,bestalen,
    [h([transitive,
	np_pc_pp(van)])]).

v(bestel,bestelt,bestellen,besteld,bestelde,bestelden,
    [h([intransitive,
	transitive,
        part_transitive(af),
	part_transitive(bij)])]).

v(bestem,bestemt,bestemmen,bestemd,bestemde,bestemden,
    [h([np_pc_pp(tot),
        np_pc_pp(voor),
        als_pred_np,
        transitive,
	part_transitive(voor),
	part_np_pc_pp(voor,tot),
	part_np_vp_obj1(voor)])]).

v(bestempel,bestempelt,bestempelen,bestempeld,bestempelde,bestempelden,
    [h([als_pred_np,
	als_pred_np_sbar,
	als_pred_np_vp,
	transitive,
	np_pc_pp(tot)])]).

v(bestendig,bestendigt,bestendigen,bestendigd,bestendigde,bestendigden,
    [h([transitive])]).

v(besterf,besterft,besterven,bestorven,bestierf,bestierven,
  [unacc([intransitive,
	  so_np_ld_pp, % de woorden bestierven hem op de lippen
	  fixed([het_obj1],no_passive),
	  fixed([pc(van),het_obj1],no_passive)])]).

v(bestier,bestiert,bestieren,bestierd,bestierde,bestierden,
    [h([transitive])]).

v(bestijg,bestijgt,bestijgen,bestegen,besteeg,bestegen,
    [h([transitive])]).

v(bestook,bestookt,bestoken,bestookt,bestookte,bestookten,
    [h([transitive,
	np_pc_pp(met)])]).

v(bestorm,bestormt,bestormen,bestormd,bestormde,bestormden,
    [h([transitive,
	np_pc_pp(met)])]).

v(bestraal,bestraalt,bestralen,bestraald,bestraalde,bestraalden,
    [h([transitive])]).

v(bestraf,bestraft,bestraffen,bestraft,bestrafte,bestraften,
    [h([transitive,
        np_pc_pp(met),
	np_pc_pp(voor)])]).

v(bestrijd,bestrijdt,bestrijden,bestreden,bestreed,bestreden,
    [h([sbar,
        vp, % TCA bestrijdt bepaalde afspraken te hebben gemaakt
	transitive,
	np_pc_pp(met)])]).

v(bestrijk,bestrijkt,bestrijken,bestreken,bestreek,bestreken,
    [h([transitive])]).

v(bestrooi,bestrooit,bestrooien,bestrooid,bestrooide,bestrooiden,
    [h([transitive,
	intransitive, % in recepten
	pc_pp(met),
	np_pc_pp(met)])]).

v(bestudeer,bestudeert,bestuderen,bestudeerd,bestudeerde,bestudeerden,
    [h([transitive,
	sbar])]).

v(bestuif,bestuift,bestuiven,bestoven,bestoof,bestoven,
    [h([transitive])]).

v(bestuur,bestuurt,besturen,bestuurd,bestuurde,bestuurden,
    [h([transitive,
        intransitive])]).

v(bet,bet,betten,gebet,bette,betten,
    [h([transitive])]).

v(betaal,betaalt,betalen,betaald,betaalde,betaalden,
    [h([np_np,
	intransitive,
	transitive,
	amb_so_np_pass,    % ik word/krijg betaald
	so_pp_np,
	pc_pp(voor),       % ik betaal voor deze goederen
	np_pc_pp(voor),    % ik betaal veel geld voor deze goederen
	np_np_pc_pp(voor), % ik betaal hem veel geld voor deze goederen
	amb_so_np_pass_pc_pp(voor), % ik word/krijg betaald voor deze goederen
	np_pc_pp(over),    % ik betaal belasting/btw over deze goederen
	fixed([[leergeld]],imp_passive),
	fixed([{[[leergeld],pc(voor)]}],imp_passive),
	fixed([ap_pred,refl],no_passive), % we betalen ons blauw
	fixed([{[pc(aan),ap_pred]},refl],no_passive),
	part_np_np(door),
	part_so_pp_np(door),
	part_np_np(terug),
	part_so_pp_np(terug),
	part_np_np(uit),
	part_so_pp_np(uit),
	part_intransitive(aan),
	part_intransitive(bij),
	part_transitive(door),
        part_intransitive(mee),
	part_intransitive(terug),
	part_intransitive(uit),
	part_amb_so_np_pass(terug), % ik word/krijg terugbetaald
	part_amb_so_np_pass(uit),   % ik word/krijg uitbetaald
	part_pc_pp(mee,aan),
	np_pc_pp(op),      % belasting betalen op
        np_pc_pp(van),     % waar betaal je dat van?        
	part_transitive(aan),
	part_transitive(af),
	part_transitive(bij),
	part_amb_so_np_pass(door), % word/krijg jij doorbetaald?
	part_transitive(door), % word het loon doorbetaald?
	part_transitive(mee),
	part_transitive(onder),
	part_transitive(terug),
	part_transitive(uit)])]).

v(betaam,betaamt,betamen,betaamd,betaamde,betaamden,
    [h([intransitive,
	transitive, %% mis(?)-used in 'zoals het hem betaamt'
	sbar_subj,
	sbar_subj_so_np
       ])]).

v(betast,betast,betasten,betast,betastte,betastten,
    [h([transitive])]).

v(betegel,betegelt,betegelen,betegeld,betegelde,betegelden,
    [h([transitive])]).

v(beteken,betekent,betekenen,betekend,betekende,betekenden,
    [h([sbar_subj_np, % het betekent oorlog dat ze dat deden (?)
	sbar,
	fixed([sbar_subj,sbar],no_passive), % dat hij komt betekent niet dat ..
	transitive])]).

v(beter,betert,beteren,gebeterd,beterde,beterden,
    [unacc([intransitive,
            fixed([er_pp(op)],no_passive)
           ]),
     h([refl,
	transitive])]).

v(beteugel,beteugelt,beteugelen,beteugeld,beteugelde,beteugelden,
    [h([transitive])]).

v(beticht,beticht,betichten,beticht,betichtte,betichtten,
    [h([np_pc_pp(van),
	np_vp_obj,
	np_er_pp_vp(van),
	np_er_pp_sbar(van)])]).

v(betitel,betitelt,betitelen,betiteld,betitelde,betitelden,
    [h([als_pred_np,
	ap_pred_np, % zo
        sbar, % dip
	np_pc_pp(met)])]).

v(betoog,betoogt,betogen,betoogd,betoogde,betoogden,betoge,
  [h([sbar,
      vp,
	transitive,
	intransitive,  % VL: demonstreert/protesteert
	pc_pp(tegen),
	pc_pp(voor)])]).

v(betoon,betoont,betonen,betoond,betoonde,betoonden,
    [h([np_np,
	pred_refl,
	transitive])]).

v(betover,betovert,betoveren,betoverd,betoverde,betoverden,
    [h([transitive])]).

v(betracht,betracht,betrachten,betracht,betrachtte,betrachtten,
    [h([transitive])]).

v(betrap,betrapt,betrappen,betrapt,betrapte,betrapten,
    [h([transitive,
	np_pc_pp(op),
	np_er_pp_sbar(op)])]).

v(betreed,betreedt,betreden,betreden,betrad,betraden,
    [h([transitive])]).

v(betref,betreft,betreffen,betroffen,betrof,betroffen,
    [h([transitive,
	cleft])]).  %% ?? het betreffen NP[pl].

v(betrek,betrekt,betrekken,betrokken,betrok,betrokken,
    [z([intransitive]),
     h([transitive,
	np_pc_pp(bij),
	np_pc_pp(in),
	np_pc_pp(van),
	np_pc_pp(uit)])]).

v(betreur,betreurt,betreuren,betreurd,betreurde,betreurden,
    [h([sbar_obj,
	sbar,
	transitive,
	vp,
	vp_obj,
	intransitive  % ?VL
       ])]).

%VL; welke bron is het meest te betrouwen?
v(betrouw,betrouwt,betrouwen,betrouwd,betrouwde,betrouwden,
    [h([transitive])]).

v(betuig,betuigt,betuigen,betuigd,betuigde,betuigden,
  [h([np_np,
      sbar,   % dbnl
      so_pp_np,
      transitive])]).

v(betuttel,betuttelt,betuttelen,betutteld,betuttelde,betuttelden,
    [h([transitive])]).

v(betwijfel,betwijfelt,betwijfelen,betwijfeld,betwijfelde,betwijfelden,
    [h([sbar_obj,
	sbar,
	transitive,
        fixed_dep(intransitive)])]).

v(betwist,betwist,betwisten,betwist,betwistte,betwistten,
    [h([np_np,
	sbar,
	transitive])]).

v(beuk,beukt,beuken,gebeukt,beukte,beukten,
    [h([transitive,
	ap_pred_np,
	part_pc_pp(in,op),
        part_intransitive(door),  % de beats beuken door
        part_transitive(in),
	ld_pp,
	ld_adv])]).

v(beul,beult,beulen,gebeuld,beulde,beulden,
    [h([part_transitive(af),
	intransitive])]). 

v(beun,beunt,beunen,gebeund,beunde,beunden,
    [h([part_transitive(bij),
	intransitive])]). 

v(beur,beurt,beuren,gebeurd,beurde,beurden,
    [h([intransitive,
	transitive,
	part_transitive(op)])]).

v(bevaar,bevaart,bevaren,bevaren,bevoer,bevoeren,
    [h([transitive])]).

v(beval,bevalt,bevallen,bevallen,beviel,bevielen,
    [z([intransitive,
	pc_pp(van)]),
     b([so_np,
        sbar_subj_so_np,
	vp_subj_so_np])]).

v(bevang,bevangt,bevangen,bevangen,beving,bevingen,
    [h([transitive])]).

v(bevat,bevat,bevatten,bevat,bevatte,bevatten,
    [h([transitive,
	sbar,
	sbar_obj])]).

v(bevecht,bevecht,bevechten,bevochten,bevocht,bevochten,
    [h([transitive,
	np_np  % zij bevechten elkaar de overwinning
       ])]).

v(beveel,beveelt,bevelen,bevolen,beval,[bevalen,bevolen],
    [h([np_np,
	intransitive,
	np_sbar,
	np_vp_obj,
	sbar,
	part_sbar_obj(aan),
	part_vp_obj(aan),  % het is niet aan te bevelen ..
	transitive,
	vp_no_control,
	part_als_pred_np(aan),
	part_np_vp_obj(aan),
	part_so_pp_np(aan),
	part_np_np(aan),
	part_sbar(aan),
	part_np_sbar(aan),
	part_transitive(aan),
	part_vp_no_control(aan)])]).

v(beveilig,beveiligt,beveiligen,beveiligd,beveiligde,beveiligden,
    [h([transitive,
	np_mod_pp(tegen)])]).

v(bevestig,bevestigt,bevestigen,bevestigd,bevestigde,bevestigden,
    [h([sbar,
	sbar_sbar_subj, % dat hij komt bevestigt dat ...
	np_sbar,
	so_pp_sbar,
	np_np,
	so_pp_np,
	vp,
        np_ld_pp,
	transitive,
	intransitive
       ])]).

v(bevind,bevindt,bevinden,bevonden,bevond,bevonden,
    [h([nonp_pred_np,
	transitive,
        fixed([{[pc(met),svp_pp(in,conflict)]}],no_passive),
        fixed([svp_pp(in,conflict)],no_passive),
        refl, % er bevindt zich een universiteit; moet eigenlijk LD worden...
	refl_ld_pp,
	refl_ld_adv])]).

v(bevis,bevist,bevissen,bevist,beviste,bevisten,
    [h([transitive])]).

v(bevlek,bevlekt,bevlekken,bevlekt,bevlekte,bevlekten,
    [h([transitive])]).

v(bevloei,bevloeit,bevloeien,bevloeid,bevloeide,bevloeiden,
    [h([transitive])]).

v(bevochtig,bevochtigt,bevochtigen,bevochtigd,bevochtigde,bevochtigden,
    [h([transitive])]).

v(bevoel,bevoelt,bevoelen,bevoeld,bevoelde,bevoelden,
    [h([transitive])]).

v(bevolk,bevolkt,bevolken,bevolkt,bevolkte,bevolkten,
    [h([transitive])]).

v(bevoordeel,bevoordeelt,bevoordelen,bevoordeeld,bevoordeelde,bevoordeelden,
    [h([transitive])]).

v(bevoorraad,bevoorraadt,bevoorraden,bevoorraad,bevoorraadde,bevoorraadden,
    [h([transitive])]).

v(bevoorrecht,bevoorrecht,bevoorrechten,bevoorrecht,bevoorrechtte,bevoorrechtten,
    [h([transitive])]).

v(bevorder,bevordert,bevorderen,bevorderd,bevorderde,bevorderden,
    [h([sbar,
	transitive,
	np_pc_pp(tot)])]).

v(bevraag,bevraagt,bevragen,bevraagd,[bevroeg,bevraagde],[bevroegen,bevraagden],
    [h([transitive,
	intransitive  % Te bevragen bij notaris Bloem te Ommen
       ])]).

v(bevredig,bevredigt,bevredigen,bevredigd,bevredigde,bevredigden,
    [h([transitive,
	intransitive  % het antwoord bevredigde niet
       ])]).

v(bevreemd,bevreemdt,bevreemden,bevreemd,bevreemdde,bevreemdden,
    [h([sbar_subj_so_np,
	sbar_subj,
	intransitive,
	so_np
       ])]).

v(bevriend,bevriendt,bevrienden,bevriend,bevriendde,bevriendden,
    [h([intransitive,
        transitive])]).

v(bevries,bevriest,bevriezen,bevroren,bevroor,bevroren,
    [z([intransitive]),
     h([transitive])]).

v(bevrijd,bevrijdt,bevrijden,bevrijd,bevrijdde,bevrijdden,
    [h([transitive,
	np_ld_pp,
	np_pc_pp(van)])]).

v(bevroed,bevroedt,bevroeden,bevroed,bevroedde,bevroedden,
    [h([sbar,
	transitive])]).

v(bevrucht,bevrucht,bevruchten,bevrucht,bevruchtte,bevruchtten,
    [h([transitive])]).

v(bevuil,bevuilt,bevuilen,bevuild,bevuilde,bevuilden,
    [h([transitive])]).

v(bewaak,bewaakt,bewaken,bewaakt,bewaakte,bewaakten,
    [h([transitive])]).

v(bewaar,bewaart,bewaren,bewaard,bewaarde,bewaarden,
    [h([transitive,
	np_ld_pp,
	np_mod_pp(voor),
        intransitive])]).

v(bewaarheid,bewaarheidt,bewaarheiden,bewaarheid,bewaarheidde,bewaarheidden,
    [h([transitive])]).

v(bewandel,bewandelt,bewandelen,bewandeld,bewandelde,bewandelden,
    [h([transitive])]).

v(bewapen,bewapent,bewapenen,bewapend,bewapende,bewapenden,
    [h([transitive,
	np_pc_pp(tegen)])]).

v(bewater,bewatert,bewateren,bewaterd,bewaterde,bewaterden,
  [h([transitive,
      intransitive
     ])]).

v(beweeg,beweegt,bewegen,bewogen,bewoog,bewogen,
    [h([intransitive,
	np_vp_obj1,
	refl,
	transitive,
	ld_pp,
        ld_adv,
        ld_dir,
        np_ld_pp,
	np_pc_pp(tot),
	obj_np_er_pp_vp(tot),
	part_refl(voort),
	part_transitive(voort),
	refl_ld_pp,   % different sense (thematic roles)
        refl_ld_dir                               
       ])]).

v(beween,beweent,bewenen,beweend,beweende,beweenden,
    [h([transitive])]).

v(beweer,beweert,beweren,beweerd,beweerde,beweerden,
    [h([intransitive,
	tr_sbar,
	van_sbar,
	transitive,
	vp])]).

v(bewerk,bewerkt,bewerken,bewerkt,bewerkte,bewerkten,
    [h([sbar,
	transitive,
	np_pc_pp(met)])]).

v(bewerkstellig,bewerkstelligt,bewerkstelligen,bewerkstelligd,bewerkstelligde,bewerkstelligden,
    [h([sbar,
	transitive])]).

v(bewierook,bewierookt,bewieroken,bewierookt,bewierookte,bewierookten,
    [h([transitive])]).

v(bewijs,bewijst,bewijzen,bewezen,bewees,bewezen,
    [h([np_sbar,
	sbar,
	sbar_sbar_subj, % dat hij komt bewijst dat ...
        sbar_obj, % omdat het bewezen is dat hij ...
	so_pp_np,
	so_pp_sbar,
	np_np,  % iemand de laatste eer bewijzen etc.
	transitive,
	refl,   % hij moet zich nog bewijzen
	fixed([acc(dienst),dat],imp_passive),
	fixed([{[acc(dienst),mod_pp(met)]},dat],imp_passive),
	vp])]).

v(bewolk,bewolkt,bewolken,bewolkt,bewolkte,bewolkten,
    [z([intransitive])]).

v(bewonder,bewondert,bewonderen,bewonderd,bewonderde,bewonderden,
    [h([transitive,
        intransitive,
	np_pc_pp(om)])]).

v(bewoon,bewoont,bewonen,bewoond,bewoonde,bewoonden,
    [h([transitive])]).

v(bezaai,bezaait,bezaaien,bezaaid,bezaaide,bezaaiden,
    [h([transitive,
	np_pc_pp(met)])]).

v(bezat,bezat,bezatten,bezat,bezatte,bezatten,
    [h([refl])]).

v(bezeil,bezeilt,bezeilen,bezeild,bezeilde,bezeilden,
    [h([transitive,
	fixed([{[acc(land),pc(met)]}],norm_passive)])]).

v(bezeer,bezeert,bezeren,bezeerd,bezeerde,bezeerden,
  [h([refl,
      refl_pc_pp(aan),
      transitive])]).

v(bezegel,bezegelt,bezegelen,bezegeld,bezegelde,bezegelden,
    [h([transitive,
	np_pc_pp(met)])]).

v(bezet,bezet,bezetten,bezet,bezette,bezetten,
    [h([transitive])]).

v(bezichtig,bezichtigt,bezichtigen,bezichtigd,bezichtigde,bezichtigden,
    [h([transitive])]).

v(bezie,beziet,inflected(bezien,beziene),bezien,bezag,bezagen,
    [h([sbar,
	transitive])]).

v(beziel,bezielt,bezielen,bezield,bezielde,bezielden,
    [h([so_np,
        np_vp_obj,  % wat bezielde je om dat te doen?
	transitive])]).

v(bezig,bezigt,bezigen,gebezigd,bezigde,bezigden,
    [h([transitive])]).

v(bezin,bezint,bezinnen,[bezonnen,bezind],[bezon,bezinde],[bezonnen,bezinden],
    [h([intransitive,  % bezint eer gij begint
	sbar,
	refl,
	refl_pc_pp(op),
	refl_pc_pp(over)])]).

v(bezing,bezingt,bezingen,bezongen,bezong,bezongen,
    [h([transitive])]).

v(bezink,bezinkt,bezinken,bezonken,bezonk,bezonken,
    [z([intransitive])]).

v(bezit,bezit,bezitten,bezeten,bezat,bezaten,
    [h([transitive])]).

v(bezoedel,bezoedelt,bezoedelen,bezoedeld,bezoedelde,bezoedelden,
  [h([transitive,
      refl,
      np_pc_pp(met)])]).

v(bezoek,bezoekt,bezoeken,bezocht,bezocht,bezochten,
    [h([transitive])]).

v(bezoldig,bezoldigt,bezoldigen,bezoldigd,bezoldigde,bezoldigden,
    [h([transitive])]).

v(bezondig,bezondigt,bezondigen,bezondigd,bezondigde,bezondigden,
    [h([refl,
	refl_pc_pp(aan)])]).

v(bezorg,bezorgt,bezorgen,bezorgd,bezorgde,bezorgden,
  [h([np_np,
      so_pp_np,
      sbar_subj_np,		% het bezorgt ellende dat ..
      sbar_subj_np_np,		% het bezorgt ons ellende dat ..
                                % *het bezorgt ons dat ..
      transitive,
      intransitive,		% wij bezorgen niet
      part_transitive(terug),
      part_np_np(terug),
      part_so_pp_np(terug),
      np_ld_adv,
      np_ld_pp,
      np_np_ld_pp,
      np_np_ld_adv,
      part_transitive(thuis),
      part_np_np(thuis)])]).

v(bezuinig,bezuinigt,bezuinigen,bezuinigd,bezuinigde,bezuinigden,
    [h([intransitive,
	transitive,
        part_transitive(weg),
	np_pc_pp(op),
	pc_pp(op)])]).

v(bezuip,bezuipt,bezuipen,bezopen,bezoop,bezopen,
    [h([refl])]).

v(bezuur,bezuurt,bezuren,bezuurd,bezuurde,bezuurden,
    [h([transitive])]).

v(bezwaar,bezwaart,bezwaren,bezwaard,bezwaarde,bezwaarden,
    [h([transitive,
	np_pc_pp(met)])]).

v(bezwanger,bezwangert,bezwangeren,bezwangerd,bezwangerde,bezwangerden,
    [h([transitive])]).

v(bezweer,bezweert,bezweren,bezworen,bezwoer,bezwoeren,
    [h([np_np,
	np_sbar,
	van_sbar,
	so_van_sbar,
	sbar,
	vp,
	np_vp_subj,
	transitive,
        intransitive])]).

v(bezweet,bezweet,bezweten,bezweet,bezweette,bezweetten,
    [h([refl])]).

v(bezwijk,bezwijkt,bezwijken,bezweken,bezweek,bezweken,
    [z([intransitive,
	pc_pp(aan),
	pc_pp(onder),
	pc_pp(voor)])]).

v(bezwijm,bezwijmt,bezijmen,bezwijmde,bezwijmden,bezwijmd,
    [z([intransitive])]).

v(beëdig,beëdigt,beëdigen,beëdigd,beëdigde,beëdigden,
    [h([transitive,
	np_pc_pp(tot)])]).

v(beëindig,beëindigt,beëindigen,beëindigd,beëindigde,beëindigden,
  [h([transitive,
      dip_sbar])]).

v(beïnvloed,beïnvloedt,beïnvloeden,beïnvloed,beïnvloedde,beïnvloedden,
    [h([transitive])]).

v(bibber,bibbert,bibberen,gebibberd,bibberde,bibberden,
    [h([intransitive,
	part_intransitive(na),
        pc_pp(van)  % bibberen van de kou
       ])]).

v(bid,bidt,bidden,gebeden,bad,baden,
    [h([intransitive,
	sbar,
        np_sbar,
	so_np,
	transitive,
	vp,
	part_transitive(aan),
	pc_pp(om),
	pc_pp(voor),
	np_pc_pp(voor)])]).

v(biecht,biecht,biechten,gebiecht,biechtte,biechtten,
    [h([intransitive,
	so_pp_np,
	transitive,
	part_sbar(op),
	part_so_pp_np(op),
	part_np_np(op),
	part_transitive(op),
	part_vp(op)])]).

v(bied,biedt,bieden,geboden,bood,boden,
    [h([np_np,
	intransitive,
	sbar_subj_np,		% het biedt voordelen dat
	sbar_subj_np_np,	% het biedt ons voordelen dat
                                % * het biedt ons dat
	so_pp_np,
	transitive,
	part_np_np(aan),
%	part_refl(aan),
	part_so_pp_np(aan),
	part_transitive(aan),
	part_sbar(aan),
	part_so_pp_sbar(aan),
	part_np_sbar(aan),
	part_vp(aan),
	part_so_pp_vp(aan),
	part_np_vp_subj(aan),
	fixed([[uitkomst]],imp_passive),
	fixed([{[acc(bescherming),pc(tegen)]}],norm_passive),
	fixed([{[acc(ruimte),pc(tot)]}],norm_passive),
        fixed([[het,hoofd],dat],imp_passive),
        fixed([{[[het,hoofd],dat_pp(aan)]}],imp_passive),
	fixed([{[acc(oplossing),pc(voor)]}],norm_passive),
	fixed([{[acc(toegang),pc(tot)]}],no_passive),
	part_fixed(aan,[[te,koop],acc],norm_passive),
	part_fixed(aan,[[te,ruil],acc],norm_passive),
	part_fixed(aan,[[te,huur],acc],norm_passive),
	part_fixed(aan,[[te,koop],{[acc,dat]}],norm_passive),
	part_fixed(aan,[[te,ruil],{[acc,dat]}],norm_passive),
	part_fixed(aan,[[te,huur],{[acc,dat]}],norm_passive)
       ])]).

v(biets,bietst,bietsen,gebietst,bietste,bietsten,
    [h([intransitive,
	transitive])]).

v(biggel,biggelt,biggelen,gebiggeld,biggelde,biggelden,
    [h([ld_pp,
	intransitive])]).			% tranen over wangen

v(bijt,bijt,bijten,gebeten,beet,beten,
    [h([ap_pred_np,
	intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv,
        fixed([[stuk],{[acc(tand),pc(op)]},refl],no_passive),
        fixed([[stuk],{[acc(tand),pc(op)]}],no_passive),
        fixed([[stuk],acc(tand),refl],no_passive),
        fixed([[stuk],acc(tand)],no_passive),
                                % hij beet zich de tanden stuk
	part_transitive(dood),
	part_transitive(door),
	part_intransitive(door),
	part_intransitive(uit),
	part_transitive(uit),
	part_intransitive(toe),
	part_np_np(toe), 
	part_np_sbar(toe),
	part_np_np(af),
	part_pc_pp_refl(af,van),  % van zich af bijten
	part_refl(vast),
	part_refl_ld_pp(vast),
	part_transitive(af)])]).

v(biljart,biljart,biljarten,gebiljart,biljartte,biljartten,
    [h([intransitive])]).

v(billijk,billijkt,billijken,gebillijkt,billijkte,billijkten,
    [h([sbar,
        sbar_obj,
	transitive])]).

v(bind,bindt,binden,gebonden,bond,bonden,
    [h([intransitive,
	transitive,
	np_pc_pp(aan),
	part_np_np(aan),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(om),
	part_np_np(om),
	part_transitive(vast),
%	refl_pc_pp(aan),
	part_np_pc_pp(aan,met),
	part_np_pc_pp(aan,tegen),
	part_intransitive(in),
	part_transitive(in),
	part_transitive(onder),
	part_np_np(onder),
	part_np_ld_pp(vast)])]).

v(biologeer,biologeert,biologeren,gebiologeerd,biologeerde,biologeerden,
    [h([transitive])]).

v(bits,bitst,bitsen,gebitst,bitste,bitsten,
    [h([sbar,
	transitive,
	part_sbar(toe),
	part_transitive(toe),
	part_sbar(terug),
	part_transitive(terug),
	intransitive])]).
  

v(bivakkeer,bivakkeert,bivakkeren,gebivakkeerd,bivakkeerde,bivakkeerden,
    [h([intransitive,
	ld_pp,
	ld_adv])]).

v(blaak,blaakt,blaken,geblaakt,blaakte,blaakten,
    [h([intransitive])]).

m(v_root(blaas,blazen),
  verb(geblazen,zijn,psp,[transitive_ydev])).

v(blaas,blaast,blazen,geblazen,blies,bliezen,
    [h([intransitive,
	transitive,
	np_ld_pp,  % de wind blies hem in de sloot / hij werd de sloot in geblazen
	np_ld_dir,
        ap_pred_np,		% blies zijn wangen bol
	part_intransitive(mee),
	part_intransitive(uit),
%	part_sbar_subj_so_np(in), ??
	part_transitive(aan),
	part_transitive(af),
	part_transitive(in),
	part_transitive(mee),   % Candy Dulfer blaast een nummertje mee
	part_transitive(op),
	part_transitive(uit),
	part_transitive(weg),
				% hem werd nieuw leven ingeblazen
				% also sometimes acc and passivizable?
	part_fixed(in,[[nieuw,leven],acc],norm_passive), 
	part_fixed(in,[[nieuw,leven],dat],imp_passive)])]). 

v(blaat,blaat,blaten,geblaat,blaatte,blaatten,
    [h([intransitive,
	part_transitive(na)])]).

v(bladder,bladdert,bladderen,gebladderd,bladderde,bladderden,
    [z([part_intransitive(af),
        ld_pp,
	part_pc_pp(af,van),
	intransitive])]).

v(blader,bladert,bladeren,gebladerd,bladerde,bladerden,
    [h([intransitive,
	mod_pp(in),
        mod_pp(door),
        mod_pp(doorheen),
	part_transitive(door)])]).

v(blaf,blaft,blaffen,geblaft,blafte,blaften,
    [h([intransitive,
        part_transitive(aan),  % ?
	part_transitive(af),
        part_transitive(toe),
        part_sbar(toe),
	transitive,  % bevelen
	sbar])]).    % dip

v(blaker,blakert,blakeren,geblakerd,blakerde,blakerden,
    [h([intransitive,
	transitive])]).

v(blancheer,blancheert,blancheren,geblancheerd,blancheerde,blancheerden,
    [h([intransitive,
	transitive])]).

v(bleek,bleekt,bleken,gebleekt,bleekte,bleekten,
    [z([intransitive]),
     h([transitive])]).

v(blend,blendt,blenden,geblend,blendde,blendden,
    [h([intransitive,
	transitive])]).  

v(blèr,blèrt,blèren,geblèrd,blèrde,blèrden,
    [h([intransitive,
	transitive,
	sbar])]).

v(blesseer,blesseert,blesseren,geblesseerd,blesseerde,blesseerden,
    [h([transitive])]).

v(blief,blieft,blieven,gebliefd,bliefde,bliefden,
    [h([transitive])]).

v(blief,bliept,bliepen,gebliept,bliepte,bliepten,
    [h([intransitive])]).

v(blijf,blijft,blijven,gebleven,bleef,bleven,blijve,
    [unacc([aan_het,
	cleft,
	copula,
	copula_np,
	copula_sbar,
	copula_vp,
	intransitive,
	so_np,		% dan blijft er mij geen keus
	so_copula,      % ik blijf je trouw
	ld_adv,
        norm_passive,                % omdat .. blijft voorbehouden/aangewezen/behouden
	te_passive,
	er_pp_sbar(bij),        % ik blijf erbij dat ...
	er_pp_vp(bij),
	ld_pp,
        fixed([vc(lig,psp,intransitive),pc(aan),dat],no_passive),
        fixed([vc(lig,psp,intransitive),extra_obj_vp(A,B),er_pp(aan,A),i(dat,B)],no_passive),
        fixed([vc(lig,psp,intransitive),extra_sbar(A),er_pp(aan,A),dat],no_passive),
	part_intransitive(aan),
	part_als_copula(aan),
	part_intransitive(achter),
	part_intransitive(binnen),
	part_intransitive(bij),
	part_intransitive(na),    % op school
	part_intransitive(op),
	part_intransitive(open),
	part_intransitive(opzij), % VL, De voetballer bleef geschorst opzij
	part_intransitive(over),
	part_intransitive(thuis),
	part_intransitive(uit),
	part_intransitive(weg),
	part_ld_pp(weg),    
	part_so_np(bij),           % dat blijft me bij
        part_so_np(over),          % ons blijft niets anders over dan ...
                                   % overblijven = resten
	part_sbar_subj_so_np(bij), % het is me bijgebleven dat...
	part_sbar_subj_no_het(over),  % blijft over dat we gaan
%	part_transitive(bij),
	part_transitive(voor),
	pc_pp(bij),		% het blijft bij woorden
	aux(inf),
	pp_vp_subj(aan),    % het is aan ons om...
 	fixed([[achterwege]],no_passive),
 	fixed([[achterwege],sbar_subj],no_passive),
 	fixed([[buiten,beschouwing]],no_passive),
 	fixed([[buiten,beschouwing],sbar_subj],no_passive),
	fixed([{[pc(bij),[ten,achter]]}],no_passive),
	fixed([{[pc(met),svp_pp(in,conflict)]}],no_passive),
	fixed([{[pc(met),pp_pred(in,contact)]}],no_passive),
	fixed([{[pc(voor),[als,de,dood]]}],no_passive),
	fixed([{[[thuis],pc(in)]}],no_passive),
	fixed([{[np_pred(dupe),pc(van)]}],no_passive),
	fixed([{[np_pred(dupe),er_pp(van,C)]},extra_sbar(C)],no_passive),
	fixed([[de,baas],acc],no_passive), % ik kan de stress niet de baas
	fixed([[te,baas],acc],no_passive), % VL
	fixed([[in,omloop]],no_passive),
	fixed([svp_pp(in,conflict)],no_passive),
	fixed([[in,gebreke]],no_passive),
	fixed([[in,gebreke],vp],no_passive),
        fixed([{[pp_pred(in,hand),pc(van)]}],no_passive),
	fixed([[in,stand]],no_passive),
        part_intransitive(instand),
	    %% daar is geen discussie/twijfel over mogelijk
	    fixed([{[pc(over),ap_pred(mogelijk)]}],no_passive),
	    fixed([{[er_pp(over,A),ap_pred(mogelijk)]},extra_sbar(A)],
		  no_passive),
	fixed([[overeind],sbar_subj_no_het],no_passive),
	fixed([subj(sprake),pc(van)],no_passive),
	fixed([subj(sprake),er_pp(van,A),extra_sbar(A)],no_passive),
	fixed([[te,boven],acc],no_passive),
	fixed([[te,moede],ap_pred,dat],no_passive),
	fixed([[ten,achter]],no_passive),
        fixed([[terwille],dat],no_passive),
	fixed([[ter,wille],dat],no_passive),
        fixed([pp_pred(van,invloed),pc(op)],no_passive),
	fixed([[van,mening],sbar],no_passive),
	fixed([[van,oordeel],sbar],no_passive),
        fixed([[van,de,partij]],no_passive),
	fixed([[van,plan],acc],no_passive),
	fixed([[van,plan],vp],no_passive),
	fixed([[van,de,plan],acc],no_passive), % VL
	fixed([[van,de,plan],vp],no_passive),  % VL
	fixed([[van,zin],acc],no_passive),
	fixed([[van,zin],vp],no_passive),
	part_ld_adv(weg),
	part_pc_pp(achter,bij),
	er_pc_pp(af),	   % "je moet er wel af blijven"
        part_pc_er_transitive(af), % hack, voor: "je moet er wel afblijven"
	part_pc_pp(af,van),
	part_pc_pp(over,van)])]).

v(blijk,blijkt,blijken,gebleken,bleek,bleken,
    [unacc([aan_het,
	    cleft,
	    copula,
	    copula_np,
	    copula_sbar,
	    copula_vp,
	    passive,              % omdat hij bleek gevonden
                       % also imp:  er bleek gefraudeerd met toegangskaarten
                       % ad19990126-97-4-2
                       % also sbar: omdat hiermee bleek aangetoond dat...
            %% ALSO: aux_psp_zijn?? VL: er bleek ruzie ontstaan...
            %%                          omdat het geld bleek verdwenen
            simple_aux_psp_zijn,
	    te_passive,
	    intransitive,
	    sbar_subj,		  % het bleek dat hij alles kwijt was
	    dip_sbar_subj_no_het, % later bleek dat hij alles kwijt was
                                  % hij sliep , bleek later
	    van_sbar_subj_no_het, % later bleek van niet
	    van_sbar_subj_so_np_no_het, % later bleek ons van wel
	    sbar_subj_so_np,      % het bleek ons dat hij alles kwijt was
	    dip_sbar_subj_so_np_no_het, % later bleek ons dat hij alles kwijt was
                                        % hij sliep , bleek ons later
	                                % het bleek uit de stukken dat ..
	    pp_sbar_subj(uit),   % later bleek uit de stukken dat ..
	    pp_sbar_subj_no_het(uit),   % later bleek uit de stukken dat ..
	    pc_pp(uit),
            pc_pp(aan),
	    pc_pp(van),       % daar bleek echter niets van
	    aux(te),
            fixed([vc(lig,psp,intransitive),pc(aan),dat],no_passive),
            fixed([vc(lig,psp,intransitive),extra_obj_vp(A,B),er_pp(aan,A),i(dat,B)],no_passive),
            fixed([vc(lig,psp,intransitive),extra_sbar(A),er_pp(aan,A),dat],no_passive),
	    fixed([[aan,de,hand]],no_passive),
	    fixed([pc(tot),subj(aanleiding)],no_passive),
	    fixed([pc(voor),subj(aanleiding)],no_passive),
	    fixed([pc(van),dat],no_passive), % daar is ons niets van gebleken
	    pp_vp_subj(aan), % het is aan ons om...
	    fixed([[de,baas],acc],no_passive), % ik bleek stress niet de baas
	    fixed([[te,baas],acc],no_passive), % VL
	    fixed([pc(aan),[de,beurt]],no_passive),
	    fixed([{[pc(met),svp_pp(in,conflict)]}],no_passive),
	    fixed([{[pc(met),pp_pred(in,contact)]}],no_passive),
	    fixed([{[pc(voor),[als,de,dood]]}],no_passive),
	    fixed([{[np_pred(dupe),pc(van)]}],no_passive),
	    fixed([{[np_pred(dupe),er_pp(van,C)]},extra_sbar(C)],no_passive),
	    fixed([{[[thuis],pc(in)]}],no_passive),

	    /* ????
	    fixed([{acc(gelijk),dat},sbar],no_passive), % je hebt (groot) gelijk, dat ..
	    fixed([{acc(gelijk),dat},vp],no_passive),  % je hebt (groot) gelijk, om ..
	                                     % je hebt het grootste gelijk van de wereld , om / dat ...
	    part_so_np(gelijk),
	    part_np_sbar(gelijk),
	    part_np_vp_obj(gelijk),
	    */
            
	    %fixed([[aan,toe],ap_pred,svp_er],no_passive),
	    %part_fixed([[aan,toe],ap_pred,svp_er],no_passive),
            part_fixed(toe,[er_pp(aan),ap_pred],no_passive),
            fixed([pp_pred(in,hand),pc(van)],no_passive),
	    fixed([[in,omloop]],no_passive),
	    fixed([svp_pp(in,conflict)],no_passive),
	    fixed([[in,gebreke]],no_passive),
	    fixed([[in,gebreke],vp],no_passive),
	    %% daar is geen discussie/twijfel over mogelijk
	    fixed([pc(van),ap_pred(op)],no_passive), % hij zou op van de zenuwen zijn / op zijn van de zenuwen
	    fixed([pc(aan),ap_pred(na)],no_passive), % hij was na aan de waarheid 
	    fixed([{[pc(over),ap_pred(mogelijk)]}],no_passive),
	    fixed([{[er_pp(over,A),ap_pred(mogelijk)]},extra_sbar(A)],
		  no_passive),
	    fixed([subj(rek),pc(uit)],no_passive),
	    fixed([subj(sprake),pc(van)],no_passive),
	    fixed([subj(sprake),er_pp(van,A),extra_sbar(A)],no_passive),
	    fixed([[te,boven],acc],no_passive),
	    fixed([vc(doe,pass_te,intransitive),pc(om),dat],no_passive),
	    fixed([vc(spreek,pass_te,intransitive),pc(over)],no_passive),
				% hij is daar helemaal niet over te spreken
	    fixed([vc(spreek,pass_te,intransitive),er_pp(over,C),extra_sbar(C)],no_passive),
				% hij is daar helemaal niet over te spreken
	    fixed([[te,moede],ap_pred,dat],no_passive),
	    fixed([[terwille],dat],no_passive),
            part_fixed(toe,[er_pp(aan),ap_pred],no_passive),
	    part_pc_pp(toe,aan),
            fixed([pp_pred(van,invloed),pc(op)],no_passive),
	    fixed([[van,mening],sbar],no_passive),
	    fixed([[van,oordeel],sbar],no_passive),
            fixed([[van,de,partij]],no_passive),
	    fixed([[van,plan],acc],no_passive),
	    fixed([[van,plan],vp],no_passive),
	    fixed([[van,de,plan],acc],no_passive),  %VL
	    fixed([[van,de,plan],vp],no_passive),   %VL
	    fixed([[van,zin],acc],no_passive),
	    fixed([[van,zin],vp],no_passive)])]).

v(blik,blikt,blikken,geblikt,blikte,blikten,
    [h([intransitive,
	part_intransitive(terug),
	part_intransitive(vooruit),
	part_transitive(aan),  % aanblikken = aankijken
	part_transitive(in),
	pc_pp(naar),
	part_sbar(terug),
	part_sbar(vooruit),
	part_pc_pp(terug,op),
	part_pc_pp(vooruit,op)])]).

v(blikker,blikkert,blikkeren,geblikkerd,blikkerde,blikkerden,
    [h([intransitive])]).

v(bliksem,bliksemt,bliksemen,gebliksemd,bliksemde,bliksemden,
    [h([intransitive])]).

v(blinddoek,blinddoekt,blinddoeken,geblinddoekt,blinddoekte,blinddoekten,
    [h([transitive])]).

v(blindeer,blindeert,blinderen,geblindeerd,blindeerde,blindeerden,
    [h([transitive])]).

v(blink,blinkt,blinken,geblonken,blonk,blonken,
    [h([intransitive,
	transitive,
	part_transitive(op),  % VL oppoetsen
	part_intransitive(uit),
	part_pc_pp(uit,in)])]).

v(bloed,bloedt,bloeden,gebloed,bloedde,bloedden,
    [h([intransitive,
	mod_pp(voor)]),
     b([part_intransitive(dood)])]).

v(bloei,bloeit,bloeien,gebloeid,bloeide,bloeiden,
    [z([part_intransitive(op),
	part_intransitive(open),
	part_intransitive(uit)]),
     h([intransitive])]).

v(bloes,bloest,bloezen,gebloesd,bloesde,bloesden,
    [h([intransitive])]).

v(blog,blogt,bloggen,geblogd,blogde,blogden,
    [h([intransitive,
	mod_pp(over),
	sbar
       ])]).

v(blok,blokt,blokken,geblokt,blokte,blokten,
    [h([intransitive,
	transitive])]).

v(blokkeer,blokkeert,blokkeren,geblokkeerd,blokkeerde,blokkeerden,
    [h([intransitive,
	transitive,
	np_np])]).

v(bloos,bloost,blozen,gebloosd,bloosde,bloosden,
    [h([intransitive,
	pc_pp(van)])]).

v(blow,blowt,blowen,geblowd,blowde,blowden,
    [h([intransitive])]).

v(bluf,bluft,bluffen,gebluft,blufte,bluften,
    [h([intransitive,
	sbar,
	part_transitive(af)])]).

v(blunder,blundert,blunderen,geblunderd,blunderde,blunderden,
    [h([intransitive])]).

v(blus,blust,blussen,geblust,bluste,blusten,
    [h([intransitive,
        transitive,
	part_intransitive(af),
	part_transitive(af),
	part_transitive(uit),
        part_intransitive(na),
	part_transitive(na)])]).

v(boei,boeit,boeien,geboeid,boeide,boeiden,
    [h([intransitive,
	sbar_subj_so_np,
	transitive])]).

v(boek,boekt,boeken,geboekt,boekte,boekten,
    [h([transitive,
	fixed([mod_pp(met),acc(succes)],no_passive),
	als_pred_np,  %% boeken als winst
        part_transitive(af),
        part_intransitive(af),
        part_transitive(bij),
        part_intransitive(bij),
	part_transitive(door),
        part_transitive(in),
        part_transitive(om),
        part_intransitive(om),
	part_transitive(over),
	intransitive])]).

v(boekstaaf,boekstaaft,boekstaven,geboekstaafd,boekstaafde,boekstaafden,
    [h([transitive])]).

v(boemel,boemelt,boemelen,geboemeld,boemelde,boemelden,
    [b([intransitive])]).

v(boen,boent,boenen,geboend,boende,boenden,
    [h([intransitive,
	part_transitive(schoon),
	transitive])]).

v(boer,boert,boeren,geboerd,boerde,boerden,
  [z([ld_dir]),
   h([intransitive])]).

v(boet,boet,boeten,geboet,boette,boetten,
    [h([intransitive,
	transitive,
        pc_pp(voor),
	part_transitive(in),
	part_pc_pp(in,in),
	part_pc_pp(in,aan)])]).

v(boetseer,boetseert,boetseren,geboetseerd,boetseerde,boetseerden,
    [h([intransitive,
	transitive])]).

v(boezem,boezemt,boezemen,geboezemd,boezemde,boezemden,
    [h([part_np_np(in),
	part_sbar_subj_so_np(in),
	part_transitive(in)])]).

v(bof,boft,boffen,geboft,bofte,boften,
    [h([intransitive,
	sbar,
	mod_pp(met)])]).

v(bok,bokt,bokken,gebokt,bokte,bokten,
    [h([intransitive])]).

v([boks,box],[bokst,boxt],[boksen,boxen],[gebokst,geboxt],[bokste,boxte],
  [boksten,boxten],
    [h([intransitive,
	transitive,
	mod_pp(met),
	mod_pp(tegen),
	part_pc_pp(op,tegen)])]).

v(bol,bolt,bollen,gebold,bolde,bolden,
    [z([part_intransitive(op)]),
     h([intransitive,
	transitive,
	part_intransitive(uit), %VL
        part_transitive(af) %VL
       ])]).

v(bolwerk,bolwerkt,bolwerken,gebolwerkt,bolwerkte,bolwerkten,
    [h([transitive])]).

v(bom,bomt,bommen,gebomd,bomde,bomden,
  [h([np_np,                    % dat kan me niet bommen
      sbar_subj_so_np           % het kan me niet bommen, dat
     ])]).

v(bombardeer,bombardeert,bombarderen,gebombardeerd,bombardeerde,bombardeerden,
    [h([ap_pred_np,
	transitive,
	part_transitive(plat),
	intransitive,
	np_pc_pp(met),
	np_pc_pp(tot)])]).

v(bonjour,bonjourt,bonjouren,gebonjourd,bonjourde,bonjourden,
    [h([np_ld_pp,
        np_ld_dir])]).

v(bonk,bonkt,bonken,gebonkt,bonkte,bonkten,
    [h([intransitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv])]).

v(bons,bonst,bonzen,gebonsd,bonsde,bonsden,
    [h([intransitive,
	so_np]),
     b([ld_pp,
	np_ld_pp])]).

v(boog,boogt,bogen,geboogd,boogde,boogden,
    [h([pc_pp(op),     % hij kan bogen op...
        np_ld_pp])]).  % hij boogde de bal in het doel

v(boom,boomt,bomen,geboomd,boomde,boomden,
    [h([intransitive])]).

%% as in 'boom town'
v(boom,boomt,boomen,geboomd,boomde,boomden,
    [h([intransitive])]).

v(boor,boort,boren,geboord,boorde,boorden,
    [h([np_np,
	intransitive,
	transitive,
	np_ld_dir, % de grond in boren
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv,
	part_transitive(aan),
	pc_pp(naar)
	%refl_ld_pp
       ])]).

v(boots,bootst,bootsen,gebootst,bootste,bootsten,
    [h([part_transitive(na)])]).

v(borduur,borduurt,borduren,geborduurd,borduurde,borduurden,
    [h([intransitive,
	part_pc_pp(voort,op),
	transitive])]).

v(borg,borgt,borgen,geborgd,borgde,borgden,
    [h([transitive,
	intransitive])]).

v(borrel,borrelt,borrelen,geborreld,borrelde,borrelden,
  [z([part_intransitive(op),
      part_ld_pp(op),
      part_intransitive(over), % VL
      part_pc_pp(over,van),    % VL  De film borrelt over van dergelijke metaforen .
      ld_pp,
      ld_dir]),
   h([intransitive])]).

v(borstel,borstelt,borstelen,geborsteld,borstelde,borstelden,
    [h([intransitive,
	transitive])]).

v(bot,bot,botten,gebot,botte,botten,
  [b([part_intransitive(uit)])]).

v(boter,botert,boteren,geboterd,boterde,boterden,
    [h([intransitive,
	het_subj])]).

v(bots,botst,botsen,gebotst,botste,botsten,
    [z([intransitive,
	part_ld_pp(in)
       ]),
     h([transitive]),
     b([ld_pp,
        pc_pp(met),
	part_ld_pp(aan),
	part_ld_pp(op)])]).

v(bottel,bottelt,bottelen,gebotteld,bottelde,bottelden,
    [h([transitive])]).

%% also vieren + bot particle...
v(botvier,botviert,botvieren,gebotvierd,botvierde,botvierden,
    [h([transitive,
	np_pc_pp(op)])]).

v(bouw,bouwt,bouwen,gebouwd,bouwde,bouwden,
    [h([intransitive,
	transitive,
	part_als_pred_np(om),
	part_sbar(in),
	part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(bij),
	part_intransitive(op),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(bij),
	part_transitive(in),
	part_transitive(na),
	part_transitive(om),
	part_transitive(op),
	part_transitive(uit),
	part_transitive(vol),
	part_pc_pp(voort,op),
	part_refl(op),
	pc_pp(aan),
	pc_pp(op),
	np_ld_pp,  % je moet er hekken omheen bouwen
	part_np_ld_pp(in),
	part_np_pc_pp(af,met),
	part_np_pc_pp(om,tot),
	part_np_pc_pp(op,uit),
	part_np_pc_pp(uit,naar),
	part_np_pc_pp(uit,tot)])]).

v(bowl,bowlt,bowlen,gebowld,bowlde,bowlden,
    [h([intransitive,
	transitive])]).

v(boycot,boycot,boycotten,geboycot,boycotte,boycotten,
    [h([transitive])]).

v(braad,braadt,braden,gebraden,braadde,braadden,
    [h([ap_pred_np,
	intransitive,
	transitive])]).

v(braak,braakt,braken,gebraakt,braakte,braakten,
    [h([intransitive,
	transitive,
	pc_pp(van),
	part_transitive(uit)])]).

v(brabbel,brabbelt,brabbelen,gebrabbeld,brabbelde,brabbelden,
    [h([intransitive,
	sbar,
	transitive])]).

v(brainstorm,brainstormt,brainstormen,gebrainstormd,brainstormde,brainstormden,
    [h([intransitive])]).

v(bral,bralt,brallen,gebrald,bralde,bralden,
    [h([intransitive,
	transitive,
	sbar])]).

v(brand,brandt,branden,gebrand,brandde,brandden,
    [z([part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(door),
	part_intransitive(los),
	part_intransitive(op),
	part_intransitive(uit)]),
     h([intransitive,
	transitive,
	np_pc_pp(aan),
	np_ld_dir,           % de zon brandt ons de tent uit
        refl_np_pc_pp(aan),  % zij branden er zich de vingers niet aan
	part_transitive(plat),
	part_transitive(weg),
	pc_pp(op)]),
     b([part_transitive(af),
	part_transitive(op),
	part_transitive(uit),
	pc_pp(van)])]).  % hebzucht, verlangen ...

v(brandmerk,brandmerkt,brandmerken,gebrandmerkt,brandmerkte,brandmerkten,
    [h([als_pred_np,
	transitive])]).

v(breek,breekt,breken,gebroken,brak,braken,
    [z([intransitive,
	ld_pp,
        part_fixed(uit,[subj(pleuris)],no_passive),
        part_fixed(uit,[dat,subj(zweet)],no_passive),
        fixed([[zuur],dat],no_passive),
	part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(los),
	part_intransitive(op),
	part_intransitive(open),
	part_intransitive(uit),
	part_ld_pp(door)]),
     h([np_np,
	transitive,
	part_intransitive(in),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(door),
	part_transitive(los),
	part_transitive(op),
	part_transitive(open),
	part_transitive(uit),
	fixed([{[acc(hoofd),pc(over)]},refl],no_passive),
	fixed([{[acc(hoofd),er_pp(over,X)]},refl,extra_sbar(X)],no_passive),
	fixed([{[acc(hoofd),pc(over)]}],no_passive),
	fixed([{[acc(hoofd),er_pp(over,X)]},extra_sbar(X)],no_passive),
	fixed([{[acc(nek),pc(over)]}],no_passive),
	pc_pp(met)]),
     b([part_so_np(op),
	% part_so_np(uit),  ??
	part_np_ld_pp(af)])]).

v(brei,breit,breien,gebreid,breide,breiden,
    [h([intransitive,
	transitive,
	np_pc_pp(aan)])]).

v(breid,breidt,breiden,gebreid,breidde,breidden,
    [h([part_intransitive(uit),
	part_pc_pp(uit,met),
	part_ld_pp(uit),
	part_transitive(uit),
	part_refl(uit),		% word order: refl<su
	part_refl_ld_pp(uit),
	part_np_pc_pp(uit,met),
	part_np_ld_pp(uit)])]).

v(breng,brengt,brengen,gebracht,bracht,brachten,
    [h([np_np,
	np_aan_het,
	np_ld_dir,
	so_pp_np,
	transitive,
	pp_pred_np, % ik breng hem in gevaar / onder controle etc
        pp_copula(aan,kook),  % recepten: breng aan de kook en ...
        fixed([ap_pred('op de hoogte'),acc],norm_passive),
        fixed([ap_pred('in orde'),acc],norm_passive),
	fixed([ap_pred('van de wijs'),acc],norm_passive),
	fixed([ap_pred('van de wijs'),mod_pp(door),acc],norm_passive),
	fixed([ap_pred('van streek'),acc],norm_passive),
	fixed([ap_pred('van streek'),mod_pp(door),acc],norm_passive),
	np_ld_pp,
	np_ld_adv,  % ik bracht haar thuis ...
	obj_np_er_pp_vp(tot),
	np_er_pp_sbar(tot),
	part_np_np(bij),
	part_np_np(over),
	part_np_ld_transitive(over),  % ik breng hem de grens over
	part_np_np(toe),
	part_so_pp_np(toe),
	part_so_pp_np(over),
	part_np_sbar(bij),
	part_np_sbar(over),
	part_sbar_subj_np(op),
	part_sbar_subj_np(teweeg),
	part_sbar_subj_np(toe),
	part_sbar(in),
	part_sbar(mede),
	part_sbar(mee),
	part_sbar(op),
	part_sbar(over),
	part_sbar(teweeg),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(bij),
	part_transitive(bijeen),
	part_transitive(binnen),
	part_transitive(door),
	part_transitive(groot),
	part_transitive(langs),
	part_transitive(in),
	part_np_ld_pp(in),
	part_transitive(mede), 
	part_transitive(mee), % NB dat ik de spullen mee naar huis breng
	part_np_np(mede),
	part_np_np(mee),      % welke spullen breng je me mee ?
	part_transitive(om),
	part_transitive(onder),
	part_intransitive(op),  % VL: en dat brengt op
	part_transitive(op),
	part_transitive(over),
	part_transitive(rond),
	part_transitive(samen),
        part_np_pc_pp(terecht,van),
	part_transitive(terug),
	part_transitive(teweeg),
	part_transitive(thuis),
	part_transitive(toe),
	part_transitive(uit),
	part_dip_sbar(uit),
	part_np_np(uit),
	part_so_pp_np(uit),
	part_np_mod_pp(uit,over),   % verslag, rapport, ..
	part_np_np_mod_pp(uit,over),   % verslag, rapport, ..
	part_np_pc_pp(uit,op),   % stem / plaat / bod
	part_transitive(voort),
	part_transitive(weg),
	part_transitive(zoek),
	part_vp_subj_np(op),
	part_vp_obj(op),  % ik kan het niet opbrengen haar te bellen
	part_vp(over),
	fixed([[aan,het,daglicht],acc],norm_passive),
	fixed([[aan,het,daglicht],sbar],imp_passive),
	fixed([[aan,het,licht],acc],norm_passive),
	fixed([[aan,het,licht],sbar],imp_passive),
	fixed([[aan,het,verstand],acc,dat],norm_passive),
	fixed([[aan,het,verstand],sbar,dat],imp_passive),
	fixed([{[acc(bezoek),dat_pp(aan)]}],imp_passive),
	fixed([{[acc(bezoek),pc(bij)]}],imp_passive),
	fixed([acc(bezoek),dat],imp_passive),
	fixed([svp_pp(in,conflict),acc],norm_passive),
	fixed([{[svp_pp(in,conflict),pc(met)]},acc],norm_passive),
	fixed([pp_pred(in,contact),acc],norm_passive),
	fixed([{[pp_pred(in,contact),pc(met)]},acc],norm_passive),
	fixed([{[[in,verband],pc(met)]},acc],norm_passive),
	fixed([{[[in,mindering],pc(op)]},acc],norm_passive),
	fixed([[aan,de,man],acc],norm_passive),
	part_np_pc_pp(op,voor),  % waardering, begrip
	part_np_er_pp_sbar(op,voor), % ,,
	fixed([pp_pred(in,beeld),sbar],norm_passive),  % in beeld brengen hoe ...
	fixed([[in,kaart],acc],norm_passive),
	fixed([[in,kaart],sbar],imp_passive),  % in kaart brengen hoe ...
	fixed([[in,omloop],acc],norm_passive),
        fixed([[in,rekening],{[dat,acc]}],norm_passive),
        fixed([[in,rekening],acc],norm_passive),
        fixed([[in,rekening],sbar],norm_passive),
        fixed([svp_pp(in,stelling),acc],norm_passive),
	fixed([ap_pred('in de war'),acc],norm_passive),
	fixed([[naar,buiten],acc],norm_passive),
	fixed([[naar,buiten],sbar],imp_passive),
	fixed([[naar,voren],acc],norm_passive),
	fixed([[naar,voren],sbar],imp_passive),
	fixed([svp_pp(in,praktijk),acc],norm_passive),
	fixed([svp_pp(in,praktijk),sbar],norm_passive),
	fixed([svp_pp(in,openbaarheid),acc],norm_passive),
	fixed([svp_pp(in,openbaarheid),sbar],norm_passive),
	fixed([svp_pp(onder,aandacht),acc],norm_passive),
	fixed([svp_pp(onder,aandacht),sbar],imp_passive),
	fixed([svp_pp(onder,aandacht),acc,dat],norm_passive),
	fixed([svp_pp(onder,aandacht),sbar,dat],imp_passive),
	fixed([[om,het,leven],acc],norm_passive),
	fixed([ap_pred('op de been'),acc],norm_passive),
        fixed([[op,andere,gedachten],acc],norm_passive),
	fixed([[op,de,markt],acc],norm_passive),
	fixed([svp_pp(op,idee),acc],norm_passive),
	fixed([svp_pp(op,naam),acc],norm_passive),
	fixed([[op,gang],acc],norm_passive),
	fixed([[op,smaak]],no_passive),
	fixed([[op,smaak],acc],norm_passive),
	fixed([[te,berde],acc],norm_passive),
	fixed([[te,berde],sbar],imp_passive),
	fixed([[te,binnen],refl],norm_passive),  % ouderwets
	fixed([[te,binnen],refl,sbar],norm_passive),  % ouderwets
	fixed([[te,gelde],acc],norm_passive),
	fixed([[ten,einde],acc],norm_passive),
	fixed([[teneinde],acc],norm_passive),
	fixed([[ten,gehore],acc],norm_passive),
	fixed([[ten,grave],acc],norm_passive),
	fixed([[te,laste],acc],norm_passive),
	fixed([{[[te,laste],pc(van)]},acc],norm_passive),
	fixed([[ten,laste],acc],norm_passive),
	fixed([{[[ten,laste],pc(van)]},acc],norm_passive),
	fixed([[te,last],acc],norm_passive),
	fixed([{[[te,last],pc(van)]},acc],norm_passive),
	fixed([[tenlaste],acc],norm_passive),
	fixed([{[[tenlaste],pc(van)]},acc],norm_passive),
	fixed([[tenlast],acc],norm_passive),
	fixed([{[[tenlast],pc(van)]},acc],norm_passive),
	fixed([[telast],acc],norm_passive),
	fixed([{[[telast],pc(van)]},acc],norm_passive),
	fixed([[ten,onder],acc],norm_passive),
	part_transitive('ten onder'),
	part_transitive(tenonder),
	fixed([[ten,tonele],acc],norm_passive),
	fixed([[ten,uitvoer],acc],norm_passive),
	fixed([[te,val],acc],norm_passive),
	fixed([[ten,val],acc],norm_passive),
        fixed([[ter,dood],acc],norm_passive),
	fixed([[ter,ore],acc],norm_passive),
	fixed([[ter,sprake],acc],norm_passive),
	fixed([[ter,sprake],sbar],norm_passive),
	fixed([[te,sprake],acc],norm_passive),
	fixed([[te,sprake],sbar],norm_passive),
        fixed([[te,weeg],acc],norm_passive),
        fixed([[tot,bedaren],acc],norm_passive), % tot INF brengen? productive?
	fixed([[tot,leven],acc],norm_passive),
        fixed([svp_pp(tot,ontploffing),acc],norm_passive),
        fixed([[tot,staan],acc],norm_passive),
        fixed([[tot,zinken],acc],norm_passive),
	fixed([[tot,stand],acc],norm_passive),
	fixed([[tot,uitdrukking],acc],norm_passive),
	fixed([[tot,uitdrukking],sbar],norm_passive),
	fixed([[tot,uiting],acc],norm_passive),
	fixed([[tot,uiting],sbar],norm_passive),
	fixed([svp_pp(in,herinnering),acc],norm_passive),
	fixed([svp_pp(in,herinnering),sbar],norm_passive),
	fixed([svp_pp(in,herinnering),{[acc,dat]}],norm_passive),
	fixed([svp_pp(in,herinnering),dat,sbar],norm_passive),
	fixed([{[acc(verbetering),pc(in)]}],imp_passive),
        fixed([{[[voor,rekening],pc(van),acc]}],norm_passive),
	part_np_ld_pp(aan),
	part_np_ld_pp(door),
	part_np_ld_adv(door),   % Napoleon bracht daar de winter door
	part_np_ld_pp(groot),
	part_np_ld_pp(onder),
	part_np_ld_adv(onder),
	part_np_ld_pp(over),
	part_np_ld_pp(samen),
	part_np_ld_pp(terug),
	part_np_ld_pp(thuis),
	part_np_pc_pp_refl(mede,met),
	part_np_pc_pp_refl(mee,met),
	np_pc_pp_refl(met),
	pp_refl_sbar(met),
	part_np_pc_pp(af,van),
	part_np_er_pp_vp(af,van),
	part_np_er_pp_sbar(af,van),
	part_np_pc_pp(door,met),
	part_np_pc_pp(in,tegen),
	part_er_pp_sbar(in,tegen),
	%%	er_pp_sbar(tegenin),  % automatically added % todo: +DIP
	part_pp_refl_sbar(mede,met),
	part_pp_refl_sbar(mee,met)])]).

v(bridge,bridget,bridgen,[gebridget,gebridged],
                         [bridgete,bridgede],
                         [bridgeten,bridgeden],
    [h([intransitive,
	transitive])]).

v(brief,brieft,brieven,gebriefd,
  briefde,briefden,
    [h([part_transitive(over),
        part_transitive(door),
        part_sbar(over),
        transitive])]).

v(brief,brieft,briefen,gebrieft,
  briefte,brieften,
    [h([transitive])]).

v(bries,briest,briesen,gebriest,brieste,briesten,
    [h([intransitive,
	sbar])]).  % mostly dip

v(broed,broedt,broeden,gebroed,broedde,broedden,
    [h([intransitive,
	part_transitive(uit),
	pc_pp(op),
        er_pp_vp(op),
        er_pp_sbar(op)])]).

v(broei,broeit,broeien,gebroeid,broeide,broeiden,
    [h([intransitive,
	transitive])]).

v(brok,brokt,brokken,gebrokt,brokte,brokten,
    [h([transitive,
	np_ld_pp])]).

v(brokkel,brokkelt,brokkelen,gebrokkeld,brokkelde,brokkelden,
    [z([part_intransitive(af)]),
     h([intransitive,
	transitive,
	np_ld_pp,
	part_transitive(af)])]).

v(brom,bromt,brommen,gebromd,bromde,bromden,
    [h([intransitive,
	sbar,
	transitive,
	np_np  % wat ik u brom
       ])]).

v(brons,bronst,bronzen,gebronsd,bronsde,bronsden,
    [h([transitive])]).

v(brouilleer,brouilleert,brouilleren,gebrouilleerd,brouilleerde,brouilleerden,
  [h([intransitive,
      np_pc_pp(met),
      pc_pp(met),
      transitive])]).

v(brouw,brouwt,brouwen,gebrouwen,brouwde,brouwden,
    [h([intransitive,
	transitive,
	np_pc_pp(van)  % ze brouwen er niets van
       ])]).

v(bruin,bruint,bruinen,gebruind,bruinde,bruinden,
    [z([intransitive]),
     h([transitive])]).

v(bruis,bruist,bruisen,gebruist,bruiste,bruisten,
    [h([intransitive])]).

v(brul,brult,brullen,gebruld,brulde,brulden,
    [h([intransitive,
	sbar,
	transitive,
        part_intransitive(mee),
        part_transitive(mee),
	pc_pp(van)])]).

v([bruuskeer,bruskeer],
  [bruuskeert,bruskeert],
  [bruuskeren,bruskeren],
  [gebruuskeerd,gebruskeerd],
  [bruuskeerde,bruskeerde],
  [bruuskeerden,bruskeerden],
    [h([transitive,
	intransitive])]).

v(budgetteer,budgetteert,budgetteren,gebudgetteerd,budgetteerde,budgetteerden,
    [h([intransitive,
	transitive])]).

v(buffel,buffelt,buffelen,gebuffeld,buffelde,buffelden,
  [h([intransitive,
      transitive,
      np_ld_pp,
      np_ld_dir
     ])]).

v(buig,buigt,buigen,gebogen,boog,bogen,
    [z([ld_dir,
	intransitive,
	part_intransitive(af),
	part_intransitive(neer),
	part_intransitive(om),
	part_ld_pp(af)]),
     h([refl,
	transitive,
	part_refl(neer),
	part_transitive(af),
	part_transitive(neer),
	part_transitive(om),
	part_np_ld_pp(om),
	np_ld_dir,
	np_ld_pp,
	pc_pp(voor),
	refl_pc_pp(over)]),
     b([ld_pp,
	refl_ld_dir])]).

v(buik,buikt,buiken,gebuikt,buikte,buikten,
  [h([part_intransitive(uit)])]).

v(buit,buit,buiten,gebuit,buitte,buitten,
    [h([part_transitive(uit),
        part_sbar(uit)])]).

v(buitel,buitelt,buitelen,gebuiteld,buitelde,buitelden,
  [b([intransitive,
      ld_pp])]).

v(buk,bukt,bukken,gebukt,bukte,bukten,
    [h([intransitive,
	refl,
	mod_pp(voor)])]).

v(bulder,buldert,bulderen,gebulderd,bulderde,bulderden,
    [h([intransitive,
	sbar])]).

v(bulk,bulkt,bulken,gebulkt,bulkte,bulkten,
    [h([intransitive])]).

v(bundel,bundelt,bundelen,gebundeld,bundelde,bundelden,
    [h([transitive])]).

v(bungel,bungelt,bungelen,gebungeld,bungelde,bungelden,
    [h([intransitive,
	ld_adv,
	ld_pp])]).

v(burger,burgert,burgeren,geburgerd,burgerde,burgerden,
    [z([part_intransitive(in)]),
     h([part_transitive(in)])]).  % mensen die kunnen worden ingeburgerd !?!??

v(bus,bust,bussen,gebust,buste,busten,
    [h([intransitive,
        transitive, % VL: de kranten/post bussen
	ld_pp])]).

v(buurt,buurt,buurten,gebuurt,buurtte,buurtten,
    [h([intransitive])]).  

v(calculeer,calculeert,calculeren,gecalculeerd,calculeerde,calculeerden,
    [h([transitive,
	part_sbar(in),
	part_transitive(in)])]).

v(camoufleer,camoufleert,camoufleren,gecamoufleerd,camoufleerde,camoufleerden,
    [h([transitive,
	sbar])]).

v(cancel,cancelt,cancelen,gecanceld,cancelde,cancelden,
    [h([transitive,
        intransitive])]).

v(canvas,canvast,canvassen,gecanvast,canvaste,canvasten,
    [h([transitive])]).

v(capituleer,capituleert,capituleren,gecapituleerd,capituleerde,capituleerden,
    [h([intransitive])]).

v(carjack,carjackt,carjacken,gecarjackt,carjackte,carjackten,
    [h([transitive,
        intransitive])]).

v(cash,casht,cashen,gecasht,cashte,cashten,
    [h([transitive,
        intransitive])]).

v(cast,cast,casten,gecast,castte,castten,
    [h([transitive,
	als_pred_np])]).

v(castreer,castreert,castreren,gecastreerd,castreerde,castreerden,
    [h([transitive])]).

v(categoriseer,categoriseert,categoriseren,gecategoriseerd,categoriseerde,categoriseerden,
    [h([transitive,
	intransitive,
	als_pred_np])]).

v(celebreer,celebreert,celebreren,gecelebreerd,celebreerde,celebreerden,
    [h([transitive])]).

v(censureer,censureert,censureren,gecensureerd,censureerde,censureerden,
  [h([transitive,
      intransitive])]).

v(centraliseer,centraliseert,centraliseren,gecentraliseerd,centraliseerde,centraliseerden,
  [h([transitive,
      intransitive])]).

v(centreer,centreert,centreren,gecentreerd,centreerde,centreerden,
  [h([transitive,
      np_ld_pp,
      np_pc_pp(op)])]).

v(certificeer,certificeert,certificeren,gecertificeerd,certificeerde,
  certificeerden,
  [h([transitive,
      intransitive])]).

v(chanteer,chanteert,chanteren,gechanteerd,chanteerde,chanteerden,
    [h([transitive,
	np_pc_pp(met),
	intransitive])]).

v(chargeer,chargeert,chargeren,gechargeerd,chargeerde,chargeerden,
    [h([intransitive,
	transitive])]).

v(charter,chartert,charteren,gecharterd,charterde,charterden,
    [h([transitive])]).

v(chat,chat,chatten,gechat,chatte,chatten,
    [h([intransitive,
	mod_pp(met),
	mod_pp(over)
       ])]).

v(check,checkt,checken,gecheckt,checkte,checkten,
    [h([transitive,
	part_intransitive(in),
	part_intransitive(uit),
	part_transitive(af),  % :-(
	part_transitive(in),
	part_transitive(uit),
	sbar,
	intransitive
       ])]).

v(chill,chillt,chillen,gechilld,chillde,chillden,
    [h([intransitive])]).

v([chinees,'Chinees'],[chineest,'Chineest'],
  ['Chinezen',chinezen],gechineesd,[chineesde,'Chineesde'],
  [chineesden,'Chineesden'],
    [h([intransitive])]).

v(chip,chipt,chippen,gechipt,chipte,chipten,
    [h([intransitive,
        transitive  % van een chip voorzien, o.a. huisdieren en gevangenen
       ])]).

v(choqueer,choqueert,choqueren,gechoqueerd,choqueerde,choqueerden,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np,
	np_mod_pp(door),
        intransitive])]).

v(cijfer,cijfert,cijferen,gecijferd,cijferde,cijferden,
    [h([intransitive,
	part_transitive(weg),
        part_sbar(weg),
	part_refl(weg)])]).

v(circuleer,circuleert,circuleren,gecirculeerd,circuleerde,circuleerden,
    [h([intransitive,
	ld_pp,
	ld_adv])]).

v(cirkel,cirkelt,cirkelen,gecirkeld,cirkelde,cirkelden,
    [h([intransitive,
	ld_pp,
	ld_adv])]).

v(citeer,citeert,citeren,geciteerd,citeerde,citeerden,
    [h([transitive,
	intransitive,
	sbar,    % dip...
	acc_np_dip_sbar, % dip...
	pc_pp(uit)])]).

v(claim,claimt,claimen,geclaimd,claimde,claimden,
    [h([sbar,
	transitive,
	vp])]).

v(classificeer,classificeert,classificeren,geclassificeerd,classificeerde,classificeerden,
    [h([als_pred_np,
	transitive])]).

v(cloon,cloont,clonen,gecloond,cloonde,cloonden,
    [h([intransitive])]).

v(coach,coacht,coachen,gecoacht,coachte,coachten,
    [h([transitive,
        intransitive])]).

v(coat,coat,coaten,gecoat,coatte,coatten,
    [h([transitive,
        intransitive])]).

v(codeer,codeert,coderen,gecodeerd,codeerde,codeerden,
    [h([intransitive,
	transitive])]).

v(collaboreer,collaboreert,collaboreren,gecollaboreerd,collaboreerde,
  collaboreerden,
    [h([intransitive,
        pc_pp(met)])]).

v(collecteer,collecteert,collecteren,gecollecteerd,collecteerde,
  collecteerden,
    [h([intransitive])]).

v(combineer,combineert,combineren,gecombineerd,combineerde,combineerden,
    [h([intransitive,
	transitive,
	np_pc_pp(met),
	pc_pp(met)])]).

v(commandeer,commandeert,commanderen,gecommandeerd,commandeerde,commandeerden,
    [h([intransitive,
	transitive,
	dip_sbar,
	acc_np_dip_sbar,
        np_ld_pp,
        np_ld_dir,
	vp_no_control,
	np_vp_obj1])]).

v(commiteer,commiteert,commiteren,gecommiteerd,commiteerde,commiteerden,
    [h([%refl,
        transitive,
	%refl_pc_pp(aan),
        np_pc_pp(aan)])]).

v(committeer,committeert,committeren,gecommitteerd,committeerde,committeerden,
    [h([%refl,
	%refl_pc_pp(aan),
        transitive,
        np_pc_pp(aan)])]).

v(communiceer,communiceert,communiceren,gecommuniceerd,communiceerde,communiceerden,
    [h([intransitive,
        transitive,
        sbar,
	mod_pp(met),
	pc_pp(over)])]).

v(compenseer,compenseert,compenseren,gecompenseerd,compenseerde,compenseerden,
    [h([transitive,
        np_np,
	np_pc_pp(voor)])]).

v(compileer,compileert,compileren,gecompileerd,compileerde,compileerden,
    [h([transitive,
        intransitive])]).

v(complementeer,complementeert,complementeren,gecomplementeerd,
  complementeerde,complementeerden,
    [h([transitive,
        intransitive])]).

v(completeer,completeert,completeren,gecompleteerd,completeerde,completeerden,
    [h([transitive])]).

v(compliceer,compliceert,compliceren,gecompliceerd,compliceerde,compliceerden,
    [h([transitive])]).

v(complimenteer,complimenteert,complimenteren,gecomplimenteerd,complimenteerde,complimenteerden,
    [h([transitive,
        acc_np_dip_sbar,
	np_pc_pp(met)])]).

v(componeer,componeert,componeren,gecomponeerd,componeerde,componeerden,
    [h([intransitive,
	transitive])]).

v(comprimeer,comprimeert,comprimeren,gecomprimeerd,comprimeerde,comprimeerden,
    [h([intransitive,
	transitive])]).

v(compromitteer,compromitteert,compromitteren,gecompromitteerd,compromitteerde,compromitteerden,
    [h([transitive,
	np_pc_pp(met)])]).

v(concentreer,concentreert,concentreren,geconcentreerd,concentreerde,concentreerden,
    [h([refl,
	transitive,
	np_ld_pp,
	np_pc_pp(op),
	refl_ld_pp,
	refl_ld_adv,
	refl_pc_pp(op)])]).

v(concipieer,concipieert,concipiëren,geconcipieerd,concipieerde,concipieerden,
    [h([transitive])]).

v(concludeer,concludeert,concluderen,geconcludeerd,concludeerde,concludeerden,
    [h([sbar,
	transitive,
	intransitive,
	np_pc_pp(uit),
	pp_sbar(uit),
	pc_pp(tot)])]).

v(concretiseer,concretiseert,concretiseren,geconcretiseerd,concretiseerde,concretiseerden,
    [h([transitive])]).

v(concurreer,concurreert,concurreren,geconcurreerd,concurreerde,concurreerden,
    [h([intransitive,
	pc_pp(met),
	pc_pp(tegen),
	np_ld_dir,
        ap_pred_np % bedrijven concurreren elkaar dood
       ])]).

v(condenseer,condenseert,condenseren,gecondenseerd,condenseerde,condenseerden,
    [z([intransitive]),
     h([transitive])]).

v(conditioneer,conditioneert,conditioneren,geconditioneerd,conditioneerde,conditioneerden,
    [h([transitive])]).

v(condoleer,condoleert,condoleren,gecondoleerd,condoleerde,condoleerden,
    [h([intransitive,
	transitive,
	np_pc_pp(met),
	pc_pp(met)])]).

v(configureer,configureert,configureren,geconfigureerd,configureerde,configureerden,
    [h([transitive,
	intransitive])]).

v(confisqueer,confisqueert,confisqueren,geconfisqueerd,confisqueerde,
  confisqueerden,
    [h([transitive])]).

v(conformeer,conformeert,conformeren,geconformeerd,conformeerde,conformeerden,
    [h([transitive,
	refl_pc_pp(aan)])]).

v(confronteer,confronteert,confronteren,geconfronteerd,confronteerde,confronteerden,
    [h([transitive,
	np_pc_pp(met),
	np_er_pp_sbar(met)])]).

v(congresseer,congresseert,congresseren,gecongresseerd,congresseerde,congresseerden,
    [h([intransitive])]).

v(conserveer,conserveert,conserveren,geconserveerd,conserveerde,conserveerden,
    [h([transitive])]).

v(consolideer,consolideert,consolideren,geconsolideerd,consolideerde,consolideerden,
    [h([transitive])]).

v(constateer,constateert,constateren,geconstateerd,constateerde,constateerden,
  [h([sbar,
      vp,
      transitive,
      intransitive		% ik constateer slechts...
     ])]).

v(constitueer,constitueert,constitueren,geconstitueerd,constitueerde,constitueerden,
    [h([transitive])]).

v(construeer,construeert,construeren,geconstrueerd,construeerde,construeerden,
    [h([transitive])]).

v(consulteer,consulteert,consulteren,geconsulteerd,consulteerde,consulteerden,
    [h([transitive])]).

v(consumeer,consumeert,consumeren,geconsumeerd,consumeerde,consumeerden,
    [h([transitive,
	intransitive])]).

v(continueer,continueert,continueren,gecontinueerd,continueerde,continueerden,
    [h([intransitive,
	transitive])]).

v(contacteer,contacteert,contacteren,gecontacteerd,contacteerde,contacteerden,
    [h([transitive])]).

v(contracteer,contracteert,contracteren,gecontracteerd,contracteerde,contracteerden,
    [h([transitive])]).

v(contrasteer,contrasteert,contrasteren,gecontrasteerd,contrasteerde,contrasteerden,
  [h([intransitive,
      transitive,
      np_pc_pp(met),
      pc_pp(met)])]).

v(controleer,controleert,controleren,gecontroleerd,controleerde,controleerden,
    [h([sbar,
	transitive,
	intransitive,
        pc_pp(op),
	np_pc_pp(op),
	intransitive])]).

v(converseer,converseert,converseren,geconverseerd,converseerde,converseerden,
    [h([intransitive,
	pc_pp(over),
	mod_pp(met)])]).

v(converteer,converteert,converteren,geconverteerd,converteerde,converteerden,
    [h([intransitive,
	transitive,
	np_ld_pp])]).

v(copieer,copieert,copiëren,gecopieerd,copieerde,copieerden,
    [h([intransitive,
	transitive])]).

v(copuleer,copuleert,copuleren,gecopuleerd,copuleerde,copuleerden,
    [h([intransitive])]).

v(correspondeer,correspondeert,corresponderen,gecorrespondeerd,correspondeerde,correspondeerden,
    [h([intransitive,
	mod_pp(met),
        pc_pp(over)])]).

v(corrigeer,corrigeert,corrigeren,gecorrigeerd,corrigeerde,corrigeerden,
    [h([transitive,
	intransitive,
	sbar % dip
       ])]).

v(corrumpeer,corrumpeert,corrumperen,gecorrumpeerd,corrumpeerde,corrumpeerden,
    [h([transitive,
	intransitive % macht
       ])]).

v(counsel,counselt,counselen,gecounseld,counselde,counselden,
    [h([transitive])]).

v(counter,countert,counteren,gecounterd,counterde,counterden,
    [h([intransitive,
	transitive])]).

v(cover,covert,coveren,gecoverd,coverde,coverden,
    [h([transitive,
	intransitive])]).

v(coördineer,coördineert,coördineren,gecoördineerd,coördineerde,coördineerden,
    [h([transitive,
	intransitive])]).

v(crash,crasht,crashen,gecrasht,crashte,crashten,
    [z([intransitive]),
     h([transitive])]).

v(cremeer,cremeert,cremeren,gecremeerd,cremeerde,cremeerden,
    [h([transitive])]).

v(crepeer,crepeert,creperen,gecrepeerd,crepeerde,crepeerden,
    [z([intransitive])]).

v(creëer,creëert,creëren,gecreëerd,creëerde,creëerden,
  [h([transitive,
      np_mod_pp(voor),
      np_np,		      % de Paus creeerde hem kardinaal
      refl_np])]).	      % Feyenoord creeerde zich talloze kansen

v(cricket,cricket,cricketten,gecricket,crickette,cricketten,
    [h([intransitive])]).

v(criminaliseer,criminaliseert,criminaliseren,gecriminaliseerd,
  criminaliseerde,criminaliseerden,
    [h([transitive])]).

v(croon,croont,cronen,gecroond,croonde,croonden,
  [h([intransitive,
      transitive])]).

v(cross,crosst,crossen,gecrosst,crosste,crossten,
    [z([ld_pp,
        ld_dir]),
     h([intransitive])]).

v(culmineer,culmineert,culmineren,geculmineerd,culmineerde,culmineerden,
    [h([intransitive])]).

v(cultiveer,cultiveert,cultiveren,gecultiveerd,cultiveerde,cultiveerden,
    [h([transitive])]).

v(cumuleer,cumuleert,cumuleren,gecumuleerd,cumuleerde,cumuleerden,
  [h([intransitive,
      transitive])]).

v(daag,daagt,dagen,gedaagd,daagde,daagden,
    [z([part_intransitive(op)]),
     h([intransitive,
	transitive,
        so_np,  % Maar nu begon me iets te dagen
	sbar_subj,
	sbar_subj_so_np,
	np_pc_pp(voor),
	part_intransitive(uit),
	part_transitive(uit),
        part_np_vp_obj1(uit),
	part_np_pc_pp(uit,tot)])]).

v(daal,daalt,dalen,gedaald,daalde,daalden,
    [z([intransitive,
	ld_dir,
	ld_pp,
	meas,
        fixed([[ten,grave]],no_passive),
	part_intransitive(af),
	part_intransitive(in),
	part_intransitive(neer),
	part_ld_pp(af),
	part_ld_pp(neer)])]).

v(dagdroom,dagdroomt,dagdromen,gedagdroomd,dagdroomde,dagdroomden,
    [h([intransitive,
	sbar])]).

v(dagvaard,dagvaardt,dagvaarden,gedagvaard,dagvaardde,dagvaardden,
    [h([transitive])]).

v(dam,damt,dammen,gedamd,damde,damden,
  [h([intransitive,
      transitive,  % op het NK werden 182 partijen gedamd
	part_transitive(af),
	part_transitive(in)
       ])]).

v(damp,dampt,dampen,gedampt,dampte,dampten,
    [h([intransitive])]).

v(dank,dankt,danken,gedankt,dankte,dankten,
    [h([intransitive,
	transitive,
	np_sbar, % ik dank God op mijn blote knieën dat ...
	np_pc_pp(aan),
	np_pc_pp(voor),
	np_er_pp_sbar(voor),
	np_er_pp_vp(voor),
	part_transitive(af),
        fixed([[de,koekoek],[je]],no_passive),
	pp_sbar_obj(aan),  % we hebben het aan hem te danken dat ...
	pp_sbar(aan),      % we hebben     aan hem te danken dat ...
	pc_pp(voor),
	er_pp_sbar(voor),
	er_pp_vp(voor)])]).

v(dans,danst,dansen,gedanst,danste,dansten,
    [h([intransitive,
	transitive,
	pc_pp(op),
	ld_pp,			% 
	ld_dir,                 %                   
	np_pc_pp(op)])]).

v(dar,dart,darren,gedard,darde,darden,
    [h([intransitive,
	transitive])]).

v(dart,dart,darten,gedart,dartte,dartten,
    [h([intransitive])]).

v(dartel,dartelt,dartelen,gedarteld,dartelde,dartelden,
    [h([intransitive])]).

v(date,datet,daten,gedate,datete,dateten,
    [h([intransitive])]).

v(dateer,dateert,dateren,gedateerd,dateerde,dateerden,
    [z([intransitive]),
     h([transitive]),
     b([pc_pp(uit),
	pc_pp(van),
	pp_sbar_subj(van)   % het dateert van twintig jaar geleden dat Belgie nog eens won
       ])]).

v(daver,davert,daveren,gedaverd,daverde,daverden,
    [h([intransitive,
	ld_adv,
	ld_pp])]).

v(deactiveer,deactiveert,deactiveren,gedeactiveerd,deactiveerde,deactiveerden,
    [h([transitive])]).

v(deïnstalleer,deïnstalleert,deïnstalleren,gedeïnstalleerd,deïnstalleerde,deïnstalleerden,
    [h([transitive])]).

v(deal,dealt,dealen,gedeald,dealde,dealden,
    [h([intransitive,
	transitive,
	pc_pp(in)])]).

v(debatteer,debatteert,debatteren,gedebatteerd,debatteerde,debatteerden,
    [h([intransitive,
	pc_pp(over),
	er_pp_sbar(over)])]).

v(debiteer,debiteert,debiteren,gedebiteerd,debiteerde,debiteerden,
    [h([np_np,
	transitive,
	np_pc_pp(voor)])]).

v(deblokkeer,deblokkert,deblokkeren,gedeblokkeerd,deblokkeerde,deblokkeerden,
    [h([intransitive,
	transitive])]).

v(debuteer,debuteert,debuteren,gedebuteerd,debuteerde,debuteerden,
    [h([intransitive,
	pc_pp(met)])]).

v(decanteer,decanteert,decanteren,gedecanteerd,decanteerde,decanteerden,
    [h([transitive])]).

v(decentraliseer,decentraliseert,decentraliseren,gedecentraliseerd,decentraliseerde,decentraliseerden,
    [h([transitive,
        intransitive])]).

v(decideer,decideert,decideren,gedecideerd,decideerde,decideerden,
    [h([transitive])]).

v(decimeer,decimeert,decimeren,gedecimeerd,decimeerde,decimeerden,
    [h([transitive]),
     z([intransitive])]).

v(declameer,declameert,declameren,gedeclameerd,declameerde,declameerden,
    [h([intransitive,
	transitive,
	dip_sbar
       ])]).

v(declareer,declareert,declareren,gedeclareerd,declareerde,declareerden,
    [h([intransitive,
	transitive])]).

v(decodeer,decodeert,decoderen,gedecodeerd,decodeerde,decodeerden,
    [h([transitive])]).

v(decoreer,decoreert,decoreren,gedecoreerd,decoreerde,decoreerden,
    [h([transitive])]).

v(decreteer,decreteert,decreteren,gedecreteerd,decreteerde,decreteerden,
    [h([transitive,
	sbar])]).

v(deel,deelt,delen,gedeeld,deelde,deelden,
    [h([intransitive,
	transitive,
	np_pc_pp(door),
	np_pc_pp(in),
	np_pc_pp(met),
	np_pc_pp(op),
	part_np_np(mee),
	part_np_np(mede),
	part_np_np(uit),
        part_intransitive(mee),
	part_intransitive(mede),
	part_np_sbar(mede),
	part_np_sbar(mee),
	part_sbar(mede),
	part_sbar(mee),
	part_so_pp_np(mede),
	part_so_pp_np(mee),
	part_so_pp_sbar(mee),
	part_so_pp_sbar(mede),
	part_so_pp_np(uit),
	part_transitive(in),
	part_np_pc_pp(in,naar), 
	part_transitive(mede),
	part_transitive(mee),
	part_transitive(op),
	part_transitive(rond),
	part_transitive(uit),
	part_np_np(toe),
	part_so_pp_np(toe),
	part_vp(mede),
	part_vp(mee),
	part_np_vp_subj(mede),
	part_np_vp_subj(mee),
	pc_pp(door),
	pc_pp(in),
	pc_pp(op),
	part_np_ld_pp(in),
	part_np_pc_pp(op,in),
	part_pc_pp(mede,in),
	part_pc_pp(mee,in)])]).

v(deemster,deemstert,deemsteren,gedeemsterd,deemsterde,deemsterden,
    [unacc([part_intransitive(weg)])]).

v(deer,deert,deren,gedeerd,deerde,deerden,
    [h([intransitive,
	sbar_subj_so_np,
        sbar_subj,  % maar dat hij niet komt , deert niet
	transitive,
	vp_subj_so_np])]).

v(definieer,definieert,definiëren,gedefinieerd,definieerde,definieerden,
  [h([transitive,
      als_pred_np,
      sbar
     ])]).

v(degenereer,degenereert,degenereren,gedegenereerd,degenereerde,degenereerden,
    [z([intransitive])]).

v(degradeer,degradeert,degraderen,gedegradeerd,degradeerde,degradeerden,
    [z([intransitive]),
     h([transitive,
	np_pc_pp(tot)])]).

v(dein,deint,deinen,gedeind,deinde,deinden,
    [h([intransitive])]).

v(deins,deinst,deinzen,gedeinsd,deinsde,deinsden,
    [z([intransitive,
	part_intransitive(achteruit),
	part_intransitive(terug),
	part_pc_pp(terug,voor),
	part_er_pp_vp(terug,voor)])]).

v(deïnstalleer,deïnstalleert,deïnstalleren,gedeïnstalleerd,deïnstalleerde,deïnstalleerden,
    [h([transitive,
	intransitive])]).

v(dek,dekt,dekken,gedekt,dekte,dekten,
    [h([transitive,
	intransitive, % van Gobbel dekte onjuist
	part_transitive(in), % de verzekering dekt ons in tegen claims
        part_np_pc_pp(in,tegen),
%	part_refl(in),
%	part_refl_pc_pp(in,tegen),
	part_intransitive(af),
	part_transitive(af),
	part_transitive(op),
	part_transitive(toe),
	part_np_pc_pp(af,met)])]).

v(delegeer,delegeert,delegeren,gedelegeerd,delegeerde,delegeerden,
    [h([transitive,
	intransitive,
	np_pc_pp(aan)])]).

v(delete,deletet,deleten,gedeletet,deletete,deleteten,
    [h([intransitive,
	transitive])]).

v(delf,delft,delven,gedolven,[dolf,delfde],[dolven,delfden],
    [h([intransitive,
	transitive])]).

v(delg,delgt,delgen,gedelgd,delgde,delgden,
    [h([transitive])]).

v(demarreer,demarreert,demarreren,gedemarreerd,demarreerde,demarreerden,
    [b([intransitive])]).

v(dementeer,dementeert,dementeren,gedementeerd,dementeerde,dementeerden,
    [h([intransitive,
	transitive])]).

v(demilitariseer,demilitariseert,demilitariseren,gedemilitariseerd,
  demilitariseerde,demilitariseerden,
    [h([transitive])]).

v(democratiseer,democratiseert,democratiseren,gedemocratiseerd,democratiseerde,democratiseerden,
    [z([intransitive]),
     h([transitive])]).

v(demoniseer,demoniseert,demoniseren,gedemoniseerd,demoniseerde,demoniseerden,
    [h([transitive])]).

v(demonstreer,demonstreert,demonstreren,gedemonstreerd,demonstreerde,demonstreerden,
    [h([intransitive,
	sbar,
	np_sbar,
	transitive,
	np_np,
	so_pp_np,
	pc_pp(tegen),
	pc_pp(voor)])]).

v(demonteer,demonteert,demonteren,gedemonteerd,demonteerde,demonteerden,
    [h([transitive])]).

v(demoraliseer,demoraliseert,demoraliseren,gedemoraliseerd,demoraliseerde,
  demoraliseerden,
    [h([intransitive,
	transitive])]).

v(demp,dempt,dempen,gedempt,dempte,dempten,
    [h([transitive])]).

v(dender,dendert,denderen,gedenderd,denderde,denderden,
    [z([ld_pp,
        ld_dir,
	part_intransitive(langs),
	part_ld_pp(langs)]),
     h([intransitive,
	part_intransitive(aan),
        part_intransitive(na)
       ])]).

v(denk,denkt,denken,gedacht,dacht,dachten,denke,
    [h([intransitive,
	tr_sbar,
	van_sbar,
	transitive,
	vp,
	refl_np,  % Zijn carrière zou niet de glorieuze afsluiting krijgen die hij zich had gedacht .
	er_pp_sbar(aan),
	er_pp_vp(aan),
	er_pp_sbar(om),
	er_pp_vp(over),
	part_np_np(toe),
        part_intransitive(mee),
	part_intransitive(na),
	part_intransitive(terug),
	part_intransitive(uit),
	part_intransitive(vooruit),
	part_np_sbar(toe),
	part_np_vp_obj(toe),
	part_refl(terug),
	part_refl_np(in),
	part_refl(in),
	part_refl_sbar(in),
	part_transitive(in),
	part_transitive(uit),
	part_transitive(vooruit),  % stappen, zetten (or are these mod?)
	part_transitive(weg),
        part_np_pc_pp(weg,uit),  % hij is niet meer weg te denken uit...
	ld_dir,         % wij denken een andere kant op
	pc_pp(aan),
	pc_pp(om),
	pc_pp(over),
	np_pc_pp(over),		% hij dacht er hetzelfde over
	pc_pp(van),
	np_pc_pp(van),		% hij denkt er het zijne van
	subj_control(te),       % ik denk wel te willen blijven
	part_sbar(na),		% ik moet nadenken of ik wil
	part_er_pp_sbar(na,over),
	part_er_pp_vp(na,over),
	part_pc_pp(door,op),
	part_pc_pp(door,over),
	part_pc_pp(mee,met),
	part_pc_pp(na,bij),
	part_pc_pp(na,over),
	part_pc_pp(terug,aan)])]).

v(dep,dept,deppen,gedept,depte,depten,
    [h([ap_pred_np,
	transitive])]).

v(depanneer,depanneert,depanneren,gedepanneerd,depanneerde,depanneerden,
    [h([intransitive,
	transitive])]).

v(deponeer,deponeert,deponeren,gedeponeerd,deponeerde,deponeerden,
    [h([transitive,
	np_ld_pp])]).

v(deporteer,deporteert,deporteren,gedeporteerd,deporteerde,deporteerden,
    [h([transitive])]).

v(deprimeer,deprimeert,deprimeren,gedeprimeerd,deprimeerde,deprimeerden,
    [h([transitive,
	sbar_subj])]).

v(deputeer,deputeert,deputeren,gedeputeerd,deputeerde,deputeerden,
    [h([transitive])]).

v(dereguleer,dereguleert,dereguleren,gedereguleerd,dereguleerde,dereguleerden,
    [h([intransitive,
        transitive])]).

v(derf,derft,derven,gederfd,derfde,derfden,
    [h([transitive])]).

v(desavoueer,desavoueert,desavoueren,gedesavoueerd,desavoueerde,desavoueerden,
    [h([transitive])]).

v(deselecteer,deselecteert,deselecteren,gedeselecteerd,deselecteerde,deselecteerden,
    [h([transitive,
	intransitive])]).

v(deserteer,deserteert,deserteren,gedeserteerd,deserteerde,deserteerden,
    [z([intransitive])]).

v(desinfecteer,desinfecteert,desinfecteren,gedesinfecteerd,desinfecteerde,desinfecteerden,
    [h([transitive])]).

v(desintegreer,desintegreert,desintegreren,gedesintegreerd,desintegreerde,desintegreerden,
    [z([intransitive])]).

v(destabiliseer,destabiliseert,destabiliseren,gedestabiliseerd,destabiliseerde,destabiliseerden,
    [h([transitive])]).

v(destilleer,destilleert,destilleren,gedestilleerd,destilleerde,destilleerden,
    [h([transitive,
	np_pc_pp(uit),
        pp_sbar(uit)])]).

v(detacheer,detacheert,detacheren,gedetacheerd,detacheerde,detacheerden,
    [h([transitive])]).

v(detailleer,detailleert,detailleren,gedetailleerd,detailleerde,detailleerden,
    [h([transitive])]).

v(detecteer,detecteert,detecteren,gedetecteerd,detecteerde,detecteerden,
    [h([transitive])]).

v(determineer,determineert,determineren,gedetermineerd,determineerde,determineerden,
    [h([transitive])]).

v(detoneer,detoneert,detoneren,gedetoneerd,detoneerde,detoneerden,
    [h([intransitive])]).

v(detineer,detineert,detineren,gedetineerd,detineerde,detineerden,
    [h([intransitive])]).

v(deug,deugt,deugen,gedeugd,deugde,deugden,
  [h([intransitive,
      pc_pp(van),  % daar deugt niets/weinig/geen reet van
      pc_pp(voor)])]).

v(deuk,deukt,deuken,gedeukt,deukte,deukten,
    [h([intransitive,
	transitive,
	part_transitive(in),
	part_transitive(uit)])]).

%% iets chemisch
v(deutereer,deutereert,deutereren,gedeutereerd,deutereerde,deutereerden,
    [h([intransitive,
	transitive
       ])]).

v(devalueer,devalueert,devalueren,gedevalueerd,devalueerde,devalueerden,
    [z([intransitive]),
     h([transitive])]).

v(diagnosticeer,diagnosticeert,diagnosticeren,gediagnosticeerd,diagnosticeerde,diagnosticeerden,
    [h([transitive])]).

v(diagnostiseer,diagnostiseert,diagnostiseren,gediagnostiseerd,diagnostiseerde,diagnostiseerden,
    [h([intransitive,
	transitive])]).

v(dicht,dicht,dichten,gedicht,dichtte,dichtten,
    [h([np_np,
	intransitive,
	transitive,
	sbar,
        part_so_pp_np(toe),
	part_np_np(toe)])]).

v(dicteer,dicteert,dicteren,gedicteerd,dicteerde,dicteerden,
    [h([intransitive,
        sbar,
        np_sbar,
	transitive,
        np_np])]).

v(dieet,dieet,diëten,gedieet,diëte,diëten,
    [h([intransitive])]).

v(dien,dient,dienen,gediend,diende,dienden,diene,
    [h([intransitive,
	transitive,
	aux(te),
	ld_pp,
	ld_adv,
	vp_no_control, % dat dient om ...
	so_np_pc_pp(van),		% iemand van repliek dienen
	fixed([{[pc(van),dat]},dip_sbar],no_passive),
        norm_passive,  % VL de schilderijen dienen overgebracht naar 't Museum
	sbar_passive,  % Daarbij dient aangetekend dat ' Barney ' een reeks toernooien aan zich voorbij liet gaan .
	part_np_np(toe),
	part_intransitive(op),
	part_refl(aan),
	part_transitive(aan),
	part_transitive(in),
	part_transitive(op),
	part_np_np(op), % iemand een maaltijd opdienen
	part_transitive(toe),
        part_intransitive(toe),
	part_transitive(uit),
        als_copula,
	fixed([[ter,zake]],no_passive),
        fixed([vc(zeg,psp,intransitive),sbar_subj_opt_het],no_passive),
	pc_pp(ter),
	pc_pp(tot),
	er_pp_vp(tot),
	pc_pp(voor),
	part_np_ld_pp(aan),
	part_np_ld_pp(in),
	part_np_pc_pp(toe,aan)])]).

v(diep,diept,diepen,gediept,diepte,diepten,
    [h([part_transitive(op),
	part_transitive(uit)])]).

v(differentieer,differentieert,differentiëren,gedifferentieerd,differentieerde,differentieerden,
    [h([intransitive,
	transitive,
	pc_pp(op),
	pc_pp(tussen)])]).

v(digitaliseer,digitaliseert,digitaliseren,gedigitaliseerd,digitaliseerde,digitaliseerden,
    [h([intransitive,
	transitive])]).  

v(dij,dijt,dijen,gedijd,dijde,dijden,
    [z([intransitive,
	ld_pp,
	part_intransitive(uit),
	part_pc_pp(uit,tot)])]).

v(dijk,dijkt,dijken,gedijkt,dijkte,dijkten,
    [h([part_transitive(in),
	part_transitive(om)])]).

v(dik,dikt,dikken,gedikt,dikte,dikten,
    [z([part_intransitive(in),
	part_intransitive(aan)]),
     h([part_transitive(in),
	part_transitive(aan)])]).

v(dikteer,dikteert,dikteren,gedikteerd,dikteerde,dikteerden,
    [h([intransitive,
	transitive])]).

v(dim,dimt,dimmen,gedimd,dimde,dimden,
    [h([transitive]),
     z([intransitive])]).

v(dineer,dineert,dineren,gedineerd,dineerde,dineerden,
    [h([intransitive])]).

v(ding,dingt,dingen,gedongen,dong,dongen,
    [h([intransitive,
	part_intransitive(af),
	part_np_pc_pp(af,op), % daar valt het nodige op af te dingen
	part_pc_pp(af,op), % daar valt op af te dingen
	part_np_pc_pp(af,van),
        part_intransitive(mee),
	part_pc_pp(mee,naar),
	pc_pp(naar),
	pc_pp(om)])]).

v(dip,dipt,dippen,gedipt,dipte,dipten,
    [h([intransitive,
	transitive,
	np_ld_pp])]).

v(diplomeer,diplomeert,diplomeren,gediplomeerd,diplomeerde,diplomeerden,
    [h([intransitive,
	transitive])]).

v(dirigeer,dirigeert,dirigeren,gedirigeerd,dirigeerde,dirigeerden,
    [h([intransitive,
	transitive,
	np_ld_pp,
	np_ld_dir])]).

v(dirk,dirkt,dirken,gedirkt,dirkte,dirkten,
    [h([part_refl(op)])]).

v(dis,dist,dissen,gedist,diste,disten,
    [h([transitive,   % new: afzeiken
	intransitive, % id
	part_transitive(op)])]).

v(disciplineer,disciplineert,disciplineren,gedisciplineerd,disciplineerde,disciplineerden,
    [h([transitive])]).

v(discrimineer,discrimineert,discrimineren,gediscrimineerd,discrimineerde,discrimineerden,
    [h([intransitive,
	transitive,
	pc_pp(naar)])]).

v(discussieer,discussieert,discussiëren,gediscussieerd,discussieerde,discussieerden,
    [h([intransitive,
	mod_pp(met),
	sbar,
	pc_pp(over)])]).

v(diskussieer,diskussieert,diskussiëren,gediskussieerd,diskussieerde,diskussieerden,
    [h([intransitive,
	mod_pp(met),
	sbar,
	pc_pp(over)])]).

v(diskwalificeer,diskwalificeert,diskwalificeren,gediskwalificeerd,diskwalificeerde,diskwalificeerden,
    [h([transitive,
        refl,
        refl_pc_pp(voor),
	np_pc_pp(voor)])]).

v(distantieer,distantieert,distantiëren,gedistantieerd,distantieerde,distantieerden,
    [h([refl,
	pc_pp(van),
	refl_pc_pp(van)])]).

v(distilleer,distilleert,distilleren,gedistilleerd,distilleerde,distilleerden,
    [h([transitive,
	np_pc_pp(uit)])]).

v(distingeer,distingeert,distingeren,gedistingeerd,distingeerde,distingeerden,
    [h([transitive])]).

v(distribueer,distribueert,distribueren,gedistribueerd,distribueerde,distribueerden,
    [h([transitive])]).

v(diversificeer,diversificeert,diversificeren,gediversificeerd,diversificeerde,
  diversificeerden,
    [h([intransitive,
	transitive])]).
  
v(dobbel,dobbelt,dobbelen,gedobbeld,dobbelde,dobbelden,
    [h([intransitive])]).

v(dobber,dobbert,dobberen,gedobberd,dobberde,dobberden,
    [h([intransitive,
	part_intransitive(aan),
	ld_pp,
	ld_adv])]).

v(doceer,doceert,doceren,gedoceerd,doceerde,doceerden,
    [h([intransitive,
	transitive,
	sbar])]).  % mostly dip

v(documenteer,documenteert,documenteren,gedocumenteerd,documenteerde,documenteerden,
    [h([transitive])]).

v(doe,doet,inflected(doen,doene),gedaan,deed,deden,
    [h([transitive,     
	aci,
	aci_no_obj,
	np_ld_pp,		% ik deed suiker in de koffie
				% no LD for "wat deed je in Amsterdam == terwijl je in Amsterdam was"
	np_ld_adv,		% ik deed het raampje omhoog
	sbar,
	% so_np,
	nonp_copula,		% hij doet gek / raar / misselijk
	so_nonp_copula,         % de vakantie deed hem goed
	part_so_np(goed),       % riddle: de vakantie zou hem goeddoen
	pp_pred_np(op,slot),    % hij doet de deur op slot
        % so_pp_np,    
	fixed([ap_pred,het_obj1],no_passive),
	np_er_pp_vp(aan),
	np_er_pp_sbar(aan),
	np_pc_pp(aan), % we doen iets/niets/alles/wat aan de criminaliteit
        er_pp_sbar(aan), % VL ik kan er niet aan doen dat ...
	part_so_np_sbar_obj(aan), % ik kan het hun niet aandoen dat ...
	part_so_np_vp_obj(aan), % ik kan het hun niet aandoen om ...
	np_pc_pp(met),
	np_pc_pp(tegen),
	pc_pp(tot),              % deze verandering doet er niet toe
	pp_sbar_subj(tot),       % het doet er niet toe dat hij komt
	meas_pc_pp(over),        % ik deed er tien dagen over
	adv_meas_pc_pp(over),    % ik deed er heel lang over
	meas_er_pp_vp(over),     % ik deed er tien dagen over om ...
	adv_meas_er_pp_vp(over), % ik deed er lang over om ...
	np_mod_pp(voor),         % daar doe je het voor
	intransitive,            % ze doen maar...
	part_nonp_copula(aan),
	part_fixed(aan,[sbar_subj,nonp_pred],no_passive),  % het doet vreemd aan dat hij komt
	part_so_nonp_copula(aan),
	part_np_np(aan),
	part_np_np(na),
	part_np_np(om),
	part_np_np(onder),
	part_np_np(over),
	part_np_np(voor),
	part_intransitive(aan),
	part_intransitive(af),  % socialisme heeft afgedaan
	part_intransitive(dienst),
	part_als_copula(dienst),
        part_intransitive(mee),
        part_transitive(mee),   % de wedstrijden die ik heb meegedaan
                                % de wedstrijd *dat ik heb meegedaan
                                % so real arg, not tmp
	part_intransitive(open),
	part_intransitive(zaken),
	part_refl(op),
	part_refl(voor),
        part_nonp_pred_refl(voor),
	part_sbar(voor),
	part_so_pp_np(over),
	part_so_pp_np(toe),
	part_transitive(aan),
	part_transitive(af),
	part_fixed(af,[sbar_subj_no_het,pc(aan)],no_passive),
	%% Hieraan doet niet af dat verzoekers het niet eens zijn met dit oordeel .
	part_transitive(dicht),
	part_transitive(in), % oordopjes
	part_transitive(na),
	part_intransitive(na),
	part_transitive(om),
	part_transitive(onder),
	part_transitive(op),
%%%	part_fixed(op,[{[acc(ervaring),pc(met)]}],norm_passive),
%%%  you should not use acc(STEM) if instead of STEM a pronoun can
%%%  be used. "We hebben veel ervaring. Die hebben we opgedaan tijdens ..."
	part_np_pc_pp(op,met),
	part_transitive(open),
	part_transitive(over),
	part_transitive(teniet),
        part_so_np(tekort),
        part_so_pp(tekort),
        part_transitive(tekort), %% because: hij voelde zich tekort gedaan (???)
	part_transitive(terug),
	part_transitive(toe),
	part_transitive(uit),
	part_transitive(voor),
	part_transitive(weg),
	pc_pp(aan),  % we doen aan sport
	pc_pp(in),
	pc_pp(tegen),
	alsof_sbar,                   % omdat hij deed alsof ...
	                              % omdat hij deed alsof .
	fixed([{[acc(aangifte),pc(van)]}],norm_passive),
	fixed([{[acc(aangifte),er_pp(van,A)]},extra_sbar(A)],norm_passive),
	fixed([{[acc(aanval),pc(op)]}],norm_passive),
	fixed([{[acc(afstand),pc(van)]}],imp_passive),
	fixed([{[acc(alles),pc(voor)]}],norm_passive),
	fixed([{[acc(alles),er_pp(voor,C)]},extra_vp(C)],norm_passive),
	fixed([{[acc(beklag),pc(over)]}],norm_passive),
	fixed([acc(beklag)],norm_passive),
	fixed([{[acc(beroep),pc(op)]}],norm_passive),
	fixed([{[acc(beroep),pc(op)]},vp_no_control],norm_passive),
	fixed([acc(best)],imp_passive),  % hij doet wel zijn best
	fixed([acc(best),vp],imp_passive),
	% ik doe mijn best op de achtergrond te blijven
	fixed([[aan,de,hand],{[acc,dat]}],norm_passive),
	fixed([[cadeau],{[acc,dat]}],norm_passive),
	fixed([[cadeau],{[acc,dat_pp(aan)]}],norm_passive),
	fixed([[kado],{[acc,dat]}],norm_passive),
	fixed([[kado],{[acc,dat_pp(aan)]}],norm_passive),
        fixed([{[acc(concessie),dat]}],norm_passive),
        fixed([{[acc(concessie),dat_pp(aan)]}],norm_passive),
	part_fixed(uit,[[de,deur],acc],norm_passive),
	fixed([[de,ronde]],no_passive),
        fixed([[gestand],dat],norm_passive),
        fixed([[gestand],dat_pp(aan)],norm_passive),
	fixed([{[acc(mededeling),pc(over)]}],imp_passive),
	fixed([{[acc(melding),pc(over)]}],imp_passive),
	fixed([{[acc(melding),pc(van)]}],imp_passive),
	fixed([{[ap_pred,pc(over)]}],imp_passive), % hij doet moeilijk over..
	fixed([{[ap_pred,er_pp(over,A)]},extra_sbar(A)],imp_passive),
	fixed([{[acc(moeite),pc(voor)]}],norm_passive),
	fixed([{[acc(moeite),er_pp(voor,C)]},extra_sbar(C)],norm_passive),
	fixed([{[acc(moeite),er_pp(voor,C)]},extra_vp(C)],norm_passive),
	fixed([{[acc(onderzoek),pc(naar)]}],norm_passive),
	fixed([{[acc(onderzoek),er_pp(naar,C)]},extra_sbar(C)],norm_passive),
	fixed([[opgeld]],no_passive),
        fixed([pc(met),acc(plezier),dat],norm_passive),
       	fixed([inv(acc(plezier)),pc(met),dat],norm_passive),
       	fixed([acc(plezier),inv(dat),pc(met)],norm_passive),
        fixed([{[acc(recht),dat]}],norm_passive),
        fixed([{[acc(recht),dat_pp(aan)]}],norm_passive),
	fixed([{[acc(studie),pc(naar)]}],norm_passive),
	fixed([{[acc(studie),er_pp(naar,C)]},extra_sbar(C)],norm_passive),
        fixed([{[[tegoed],refl,pc(aan)]}],no_passive),
	fixed([[te,niet],acc],norm_passive),
	fixed([[terzake]],no_passive),
	fixed([sbar_subj,[terzake]],no_passive),
        fixed([{[acc(oproep),dat_pp(aan)]}],norm_passive),
        fixed([{[acc(toezegging),dat]}],norm_passive),
        fixed([{[acc(toezegging),dat_pp(aan)]}],norm_passive),
        fixed([[uit,de,doeken],acc],norm_passive),
        fixed([[uit,de,doeken],{[acc,dat]}],norm_passive),
        fixed([[uit,de,doeken],sbar],imp_passive),
        fixed([[uit,de,doeken],dat,sbar],imp_passive),
        fixed([{[acc(verzoek),dat]}],norm_passive),
        fixed([{[acc(verzoek),dat_pp(aan)]}],norm_passive),
	fixed([[van,de,hand],acc],norm_passive),
	fixed([{[acc(verslag),pc(van)]}],imp_passive),
	fixed([{[acc(voordeel),pc(met)]}],no_passive),
	fixed([{[acc(voordeel),er_pp(met,A)]},extra_sbar(A)],no_passive),
        fixed([{[acc(zeg),pc(over)]}],norm_passive),
        fixed([{[acc(zeg_DIM),pc(over)]}],norm_passive),
				% je doet er verstandig aan te verdwijnen
	fixed([er_pp(aan),ap_pred,vp],no_passive),
	part_np_ld_pp(over),
	part_pc_pp(af,aan),  % dat doet daar niet aan af 
	part_np_pc_pp(af,aan),  % dat doet niets af aan de overwinning
	part_fixed(af,[sbar_subj,{[pc(aan),acc]}],no_passive),
				% het doet niets af aan de overw, dat ..
	part_np_pc_pp(af,met),
	part_np_pc_pp(af,van),
	part_als_pred_np(af),
	part_np_pc_pp(open,voor),
	part_pc_pp(mee,aan),
	part_pc_pp(mee,met),
	part_pc_pp(onder,voor),
	%% het doet me deugd/plezier/geen goed dat hij komt
	fixed([sbar_subj,{[acc,dat]}],no_passive),
	%% het doet me deugd te horen dat hij komt
	fixed([vp_subj,{[acc,dat]}],no_passive),
	%% het doet deugd/plezier/geen goed dat hij komt
	fixed([sbar_subj,acc],no_passive),
	%% het doet deugd te horen dat hij komt
	fixed([vp_subj,acc],no_passive),
	%% de verbouwing deed ons huis geen goed
	np_np,
	part_fixed(aan,[[geweld],acc],norm_passive)])]).

v(doek,doekt,doeken,gedoekt,doekte,doekten,
  [h([part_transitive(op),
      part_intransitive(op)])]).

v(doel,doelt,doelen,gedoeld,doelde,doelden,
    [h([intransitive,
        pc_pp(op),
        pp_dip_sbar(op),
	er_pp_sbar(op),
	er_pp_vp(op)])]).

v(doem,doemt,doemen,gedoemd,doemde,doemden,
    [z([part_intransitive(op)]),
     h([transitive,
	np_pc_pp(ter),
	np_pc_pp(tot),
	pc_pp(tot)])]).

v(doezel,doezelt,doezelen,gedoezeld,doezelde,doezelden,
  [z([part_intransitive(in),
      part_intransitive(weg)])]).

v(dof,doft,doffen,gedoft,dofte,doften,
    [h([part_refl(op),
	intransitive,
	transitive])]).

v(dok,dokt,dokken,gedokt,dokte,dokten,
    [h([transitive,
	intransitive])]).

v(dokter,doktert,dokteren,gedokterd,dokterde,dokterden,
    [h([pc_pp(aan),
	pc_pp(op),
        intransitive,
        part_transitive(uit)])]).

v(dokumenteer,dokumenteert,dokumenteren,gedokumenteerd,dokumenteerde,dokumenteerden,
    [h([transitive])]).

v(dol,dolt,dollen,gedold,dolde,dolden,
    [h([intransitive,
        acc_np_dip_sbar,
	transitive,
	pc_pp(met)])]).

v(domineer,domineert,domineren,gedomineerd,domineerde,domineerden,
  [h([intransitive,
      mod_pp(in),
      mod_pp(bij),
      transitive])]).

v(dommel,dommelt,dommelen,gedommeld,dommelde,dommelden,
    [z([intransitive,
	part_intransitive(in)])]).

v(dompel,dompelt,dompelen,gedompeld,dompelde,dompelden,
    [h([np_ld_pp,
        transitive,
	part_transitive(onder),
	part_np_ld_pp(onder)])]).

v(donder,dondert,donderen,gedonderd,donderde,donderden,
    [z([ld_dir,
	part_intransitive(op),
	ld_pp]),
     h([intransitive,
	np_ld_dir,
	np_ld_pp])]).

v(doneer,doneert,doneren,gedoneerd,doneerde,doneerden,
    [h([transitive,
	intransitive,  % help mee en doneer!
	so_pp_np])]).

v(dood,doodt,doden,gedood,doodde,doodden,
    [h([transitive,
	intransitive])]).  % gij zult niet doden

v(doof,dooft,doven,gedoofd,doofde,doofden,
    [z([part_intransitive(uit),
	intransitive]),
     h([transitive,
	part_transitive(uit)])]).

v(dooi,dooit,dooien,gedooid,dooide,dooiden,
    [h([het_subj,
	part_intransitive(weg)])]).

v(dool,doolt,dolen,gedoold,doolde,doolden,
    [h([intransitive,
	ld_pp,
	ld_adv,
	part_intransitive(rond),
	part_ld_pp(rond)])]).

v(doop,doopt,dopen,gedoopt,doopte,doopten,
    [h([np_pred_np,
	transitive,
	np_pc_pp(in),   % is this a real PC?
	part_transitive(om),
	part_np_pc_pp(om,in),
	part_np_pc_pp(om,tot)])]).

v(doorblader,doorbladert,doorbladeren,doorbladerd,doorbladerde,doorbladerden,
    [h([transitive])]).

v(doorboor,doorboort,doorboren,doorboord,doorboorde,doorboorden,
    [h([transitive,
	np_pc_pp(met)])]).

v(doorbreek,doorbreekt,doorbreken,doorbroken,doorbrak,doorbraken,
  [h([transitive,
      acc_np_dip_sbar   % ze kunnen niet ver weg zijn, doorbrak Benitez de stilte
     ])]).

v(doordenk,doordenkt,doordenken,doordacht,doordacht,doordachten,
    [h([transitive])]).

v(doordrenk,doordrenkt,doordrenken,doordrenkt,doordrenkte,doordrenkten,
    [h([transitive,
	np_pc_pp(met)])]).

v(doordring,doordringt,doordringen,doordrongen,doordrong,doordrongen,
    [h([transitive,
	np_er_pp_sbar(van),
	np_pc_pp(van)])]).

v(doorgroef,doorgroeft,doorgroeven,doorgroefd,doorgroefde,doorgroefden,
    [h([transitive])]).

v(doorflits,doorflitst,doorflitsen,doorflitst,doorflitste,doorflitsten,
    [h([transitive])]).

v(doorgrond,doorgrondt,doorgronden,doorgrond,doorgrondde,doorgrondden,
    [h([transitive])]).

v(doorkerf,doorkerft,doorkerven,doorkerfd,doorkerfde,doorkerfden,
    [h([transitive])]).

v(doorklief,doorklieft,doorklieven,doorkliefd,doorkliefde,doorkliefden,
    [h([transitive])]).

v(doorkruis,doorkruist,doorkruisen,doorkruist,doorkruiste,doorkruisten,
    [h([transitive])]).

v(doorleef,doorleeft,doorleven,doorleefd,doorleefde,doorleefden,
    [h([transitive])]).

v(doorlees,doorleest,doorlezen,doorlezen,doorlas,doorlazen,
    [h([intransitive,
	transitive])]).

v(doorlicht,doorlicht,doorlichten,doorgelicht,doorlichtte,doorlichtten,
    [h([intransitive,
	transitive])]).

v(doorloop,doorloopt,doorlopen,doorlopen,doorliep,doorliepen,
    [h([transitive])]).

v(doorprik,doorprikt,doorprikken,doorprikt,doorprikte,doorprikten,
    [h([transitive])]).

v(doorsnijd,doorsnijdt,doorsnijden,doorsneden,doorsneed,doorsneden,
    [h([np_np,
	transitive])]).

%% VL
v(doorsnuffel,doorsnuffelt,doorsnuffelen,doorsnuffeld,doorsnuffelde,doorsnuffelden,
    [h([transitive])]).

%% VL
v(doorspartel,doorspartelt,doorspartelen,doorsparteld,doorspartelde,doorspartelden,
    [h([transitive])]).

v(doorspek,doorspekt,doorspekken,doorspekt,doorspekte,doorspekten,
    [h([transitive,
	np_pc_pp(met)])]).

v(doorsta,doorstaat,inflected(doorstaan,doorstane),doorstaan,doorstond,doorstonden,
    [h([transitive])]).

v(doorsteek,doorsteekt,doorsteken,doorstoken,doorstak,doorstaken,
    [h([transitive])]).

v(doorstroom,doorstroomt,doorstromen,doorstroomd,doorstroomde,doorstroomden,
    [h([transitive])]).

v(doortrek,doortrekt,doortrekken,doortrokken,doortrok,doortrokken,
    [h([transitive,
	np_pc_pp(met)])]).

v(doorvoel,doorvoelt,doorvoelen,doorvoeld,doorvoelde,doorvoelden,
    [h([transitive])]).

v(doorvors,doorvorst,doorvorsen,doorvorst,doorvorste,doorvorsten,
    [h([transitive])]).

v(doorwaad,doorwaadt,doorwaden,doorwaad,doorwaadde,doorwaadden,
    [h([transitive])]).

v(doorweek,doorweekt,doorweken,doorweekt,doorweekte,doorweekten,
    [h([transitive])]).

v(doorworstel,doorworstelt,doorworstelen,doorworsteld,doorworstelde,doorworstelden,
    [h([transitive])]).

v(doorzeef,doorzeeft,doorzeven,doorzeefd,doorzeefde,doorzeefden,
    [h([transitive])]).

v(doorzie,doorziet,inflected(doorzien,doorziene),doorzien,doorzag,doorzagen,
    [h([sbar,
	transitive])]).

v(doorzoek,doorzoekt,doorzoeken,doorzocht,doorzocht,doorzochten,
    [h([transitive,
	np_pc_pp(op)])]).

v(doorzwem,doorzwemt,doorzwemmen,doorzwommen,doorzwom,doorzwommen,
    [h([transitive])]).

v(dop,dopt,doppen,gedopt,dopte,dopten,
    [h([intransitive,
	transitive])]).

v(dos,dost,dossen,gedost,doste,dosten,
    [h([part_transitive(uit)])]).

v(doseer,doseert,doseren,gedoseerd,doseerde,doseerden,
    [h([transitive,
	intransitive])]).

v(dotter,dottert,dotteren,gedotterd,dotterde,dotterden,
    [h([intransitive,
	transitive])]).

v(doubleer,doubleert,doubleren,gedoubleerd,doubleerde,doubleerden,
    [h([intransitive,
	transitive])]).

v(douch,doucht,douchen,gedoucht,douchte,douchten,
    [h([intransitive,
%	refl,
        transitive])]).  % ik wil mijn kind kunnen douchen

v(download,downloadt,downloaden,gedownload,downloadde,downloadden,
    [h([transitive,
        intransitive])]).

v(draaf,draaft,draven,gedraafd,draafde,draafden,
    [z([part_intransitive(op),
	part_intransitive(aan),
	part_intransitive(door),
	part_ld_pp(op),
	part_transitive(op),
	ld_pp,
        ld_dir % het bos in, het veld af
       ]),
     h([intransitive,
        part_intransitive(rond)
       ])]).

v(draag,draagt,dragen,gedragen,droeg,droegen,
    [h([intransitive,
	transitive,
	np_ld_pp,
	np_ld_dir,
	fixed([{[acc(gevolg),pc(van)]}],no_passive),
	fixed([{[acc(spoor),pc(van)]}],no_passive),
	fixed([[ten,grave],acc],norm_passive),
	fixed([{[acc(schande),pc(van)]}],norm_passive),
	fixed([{[acc(schande),er_pp(van,X)]},extra_sbar(X)],norm_passive),
	part_als_pred_np(voor),
	part_np_np(op),
	part_dip_sbar(op),
	part_acc_np_dip_sbar(op),
	part_np_np(over),
	part_np_np(toe),
	part_intransitive(bij),
	part_intransitive(voor),
	part_np_vp_obj(op),
	part_so_vp_obj(op),
	part_sbar(voor),
	part_so_pp_np(af),
	part_np_sbar(na),
	part_np_np(na),
	part_so_pp_np(op),
	part_so_pp_np(over),
	part_so_pp_np(voor),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(bij),
	part_transitive(binnen),
	part_transitive(mee), % NB dat ik de spullen mee naar huis draag
	part_np_pc_pp_refl(mee,met),  % de herinnering die ik met me meedraag
	part_transitive(op),
	part_transitive(over),
        part_transitive(rond),
	part_transitive(uit),
        part_sbar(uit),
	part_transitive(voor),
	part_transitive(weg),
	part_vp(op),
	part_np_pc_pp(af,van),
	part_np_pc_pp(voor,bij),
	part_np_pc_pp(bij,aan),
	part_np_pc_pp(bij,in),
	part_np_pc_pp(bij,tot),
	part_np_pc_pp(op,aan),
	part_np_pc_pp(voor,aan),
	part_np_pc_pp(voor,ter),
	part_pc_pp(bij,aan),
	part_er_pp_sbar(bij,aan),
	part_pp_sbar_subj(bij,aan),  % dat het zo rustig bleef heeft bijgedragen aan ...
	part_pc_pp(bij,tot),
	part_er_pp_sbar(bij,tot),
	part_er_pp_vp_no_control(bij,tot),
	part_pc_pp(zorg,voor),
	part_er_pp_sbar(zorg,voor),   % dat hij er zorg voor draagt
	                              % cannot be particle in that order
	fixed([pc(voor),[zorg]],imp_passive),
	fixed([er_pp(voor,A),[zorg],extra_sbar(A)],imp_passive)])]).

v(draai,draait,draaien,gedraaid,draaide,draaiden,
    [z([part_intransitive(bij),  % hij draait wel bij
	part_intransitive(om),
        part_intransitive(weg),
        part_ld_pp(weg),
	part_ld_pp(om),
	part_ld_pp(af),
        part_intransitive(uit),
	part_pc_pp(uit,op),
	part_er_pp_sbar(uit,op)]),
     h([np_ld_dir,
	transitive,
	intransitive,  % u vraagt en wij draaien
	nonp_pred_np,
	pc_pp(om),   % het draait allemaal om de poen
	er_pp_vp_no_control(om),
	er_pp_sbar(om),
	er_pp_sbar(omheen), % we willen er niet omheen draaien dat ..
	pc_pp(op),   % het station draait op reclameinkomsten
	part_fixed(om,[{[acc(hand),pc(voor)]}],no_passive),
	part_fixed(aan,[acc(duimschroef),dat],norm_passive),
	part_fixed(aan,[acc(duimschroef)],norm_passive),
        part_fixed(uit,[[een,poot],dat],imp_passive),
	fixed([acc(loer),dat],imp_passive),
	fixed([[voor,ogen],[een,rad],dat],imp_passive),
	fixed([[voor,de,ogen],[een,rad],dat],imp_passive),
	fixed([ap_pred(grijs),acc],norm_passive),      % ik heb die plaat grijs gedraaid
	part_np_np(toe),
	part_np_np(om),
        part_fixed(om,[[de,nek],acc],norm_passive), % iemand/iets de nek omdraaien
	part_intransitive(open),
	part_intransitive(proef),
	%part_refl(om),
        part_intransitive(mee),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(dicht),
	part_transitive(door),
	part_transitive(om),
	part_transitive(open),
	part_transitive(terug),
        part_transitive(weg),
        part_np_ld_pp(weg),
	part_np_ld_pp(af),
	part_np_pc_pp(open,met)]),
     b([intransitive,
	ld_dir,
	ld_pp,
	np_ld_pp,
	part_ld_pp(rond),
	part_intransitive(rond),
%% 	part_sbar_subj_so_np(om), ??
	part_transitive(op),  % je moet het horloge opdraaien
	part_ld_transitive(op), % je moet die kant opdraaien
	part_transitive(rond),
	part_transitive(uit),
	fixed([[quitte]],imp_passive),
	part_pc_pp(op,voor)])]).

v(draal,draalt,dralen,gedraald,draalde,draalden,
    [h([intransitive])]).

v(draineer,draineert,draineren,gedraineerd,draineerde,draineerden,
    [h([intransitive,
        transitive])]).

v(dram,dramt,drammen,gedramd,dramde,dramden,
    [h([intransitive,
	sbar,
	part_transitive(door)])]).

v(dramatiseer,dramatiseert,dramatiseren,gedramatiseerd,dramatiseerde,dramatiseerden,
    [h([transitive])]).

v(drapeer,drapeert,draperen,gedrapeerd,drapeerde,drapeerden,
    [h([transitive,
	np_ld_pp])]).

v(dreg,dregt,dreggen,gedregd,dregde,dregden,
    [h([transitive,
        intransitive,
        part_transitive(op)])]).

v(dreig,dreigt,dreigen,gedreigd,dreigde,dreigden,
    [h([intransitive,
	sbar,
	transitive,
        part_transitive(af),  %VL
	vp,
	er_pp_sbar(met),
	er_pp_vp(met),
	np_pc_pp(met),
	pc_pp(met),
	aux(te)])]).

v(drein,dreint,dreinen,gedreind,dreinde,dreinden,
    [h([intransitive])]).

v(drenk,drenkt,drenken,gedrenkt,drenkte,drenkten,
    [h([transitive,
	np_pc_pp(in)])]).

v(drentel,drentelt,drentelen,gedrenteld,drentelde,drentelden,
    [b([intransitive,
        ld_pp])]).

v(dresseer,dresseert,dresseren,gedresseerd,dresseerde,dresseerden,
    [h([transitive,
	np_pc_pp(op)])]).

v(dreun,dreunt,dreunen,gedreund,dreunde,dreunden,
    [h([intransitive,
	part_intransitive(na),
	part_transitive(op),
	np_ld_pp,
	sbar, % dip
	part_sbar(op)])]).

v(dribbel,dribbelt,dribbelen,gedribbeld,dribbelde,dribbelden,
    [h([intransitive]),
     b([ld_pp,
        ld_dir,
        np_ld_pp, % hij dribbelde de bal in de Duitse loopgraven
	part_intransitive(aan)])]).

v(drijf,drijft,drijven,gedreven,dreef,dreven,
    [z([ld_dir,
	part_intransitive(af),
	part_intransitive(boven),
	part_intransitive(door),
	part_intransitive(over),
	part_intransitive(terug),
	part_intransitive(uit),
	part_intransitive(weg),
	part_ld_pp(af),
	part_ld_pp(terug),
	part_ld_pp(weg),
	part_mod_pp(weg,met)]),
     h([intransitive,
	transitive,
	np_ld_dir,  % de paal de grond in drijven
	part_np_ld_pp(door),
	np_ld_pp,
	fixed([acc(handel)],norm_passive),
	fixed([{[acc(handel),pc(in)]}],norm_passive),
        pp_pred_np(op,vlucht),
	part_np_ld_pp(terug),
	part_sbar_subj_np(op),  % het dreef de prijs op dat .. (?)
	part_transitive(aan),
	part_transitive(af),
	part_transitive(door),
	part_transitive(op),
	part_transitive(terug),
	part_transitive(uit),
	part_transitive(voort),
	part_transitive(weg),
	part_np_pc_pp(aan,tot)]),
     b([ld_pp,
	part_intransitive(rond),
	part_ld_pp(op)])]).

v(dril,drilt,drillen,gedrild,drilde,drilden,
    [h([intransitive,
	transitive,
	np_pc_pp(in)])]).

v(dring,dringt,dringen,gedrongen,drong,drongen,
    [z([ld_pp,
	part_intransitive(binnen),
	part_ld_pp(binnen),
	part_ld_pp(in),
	part_transitive(binnen),
	part_ld_pp(door),
	part_sbar_subj_opt_het(door),
	part_ld_pp(op)]),
     h([intransitive,
	np_ld_dir,
	np_ld_pp,
	part_np_np(op),
	part_intransitive(aan),
	part_dip_sbar(aan),
	part_so_pp_np(op),
        part_so_pp_refl(op), % word order
	part_refl(in),
	part_refl(op),
	part_so_pp_np(op),
	part_transitive(op),
	part_transitive(terug),
	dip_sbar,
	pc_pp(om),
	part_er_pp_sbar(aan,op),
	part_er_pp_vp_no_control(aan,op),
	part_vp_no_control(aan),
	part_pc_pp(aan,op),
	part_refl_ld_pp(in)]),
     b([part_intransitive(op),
        part_intransitive(voor)])]).

v(drink,drinkt,drinken,gedronken,dronk,dronken,
    [h([ap_pred_refl,
	intransitive,
	transitive,
	np_pc_pp(op),
	np_pc_pp(uit),   % dit glas is om cognac uit te drinken
	np_pc_pp(van),
	part_intransitive(in),
	part_transitive(in),  % wij dronken zijn woorden in als nectar
	part_refl_np(in),  % hij drinkt zich moed in
	np_mod_pp(bij),
	pc_pp(uit),       % dit glas is om uit te drinken
	part_intransitive(tee),
	part_intransitive(thee),
	part_transitive(leeg),
	part_intransitive(leeg),  % drink leeg!
	part_transitive(op),
	part_intransitive(op),  % drink op!
	pc_pp(op),
        pc_pp(van)])]).

v(drogeer,drogeert,drogeren,gedrogeerd,drogeerde,drogeerden,
  [h([transitive,
      intransitive])]).

v(drom,dromt,drommen,gedromd,dromde,dromden,
    [z([ld_dir,
	ld_pp
       ]),
     h([intransitive])]).

v(droog,droogt,drogen,gedroogd,droogde,droogden,
    [z([part_intransitive(op),
	part_intransitive(uit)]),
     h([transitive,
	part_transitive(af),
	part_intransitive(af), % er moet afgedroogd worden
	part_transitive(op),
	part_transitive(uit)]),
     b([intransitive])]).

v(droom,droomt,dromen,gedroomd,droomde,droomden,
    [h([intransitive,
	sbar,
	transitive,
        refl_np,    % zich geen betere start kunnen dromen
	pc_pp(over),
	pc_pp(van),
	er_pp_sbar(van),
	er_pp_vp(van)])]).

v(drom,dromt,drommen,gedromd,dromde,dromden,
    [z([part_intransitive(samen)])]).

v(drop,dropt,droppen,gedropt,dropte,dropten,
    [h([intransitive,
	transitive,
	np_ld_pp])]).

v(druip,druipt,druipen,gedropen,droop,dropen,
    [z([part_intransitive(af)]),
     h([intransitive]),
     b([ld_pp,
	part_intransitive(uit)])]).

v(druis,druist,druisen,gedruist,druiste,druisten,
    [b([part_pc_pp(in,tegen)])]).

v(druk,drukt,drukken,gedrukt,drukte,drukten,
    [h([nonp_pred_np,
	intransitive,
	np_ld_dir,
	refl,
	transitive,
	ld_adv,
	ld_pp,
	np_ld_pp,
	np_ld_adv,
	np_np_ld_pp,
	part_intransitive(af),
	part_intransitive(neer),
	part_sbar_subj_so_np(in),
	part_sbar_subj_so_np(neer),
	part_sbar(uit),
	part_transitive(aan),
	part_transitive(achterover),
	part_transitive(af),
	part_transitive(bij),
	part_transitive(dood),
	part_transitive(door),
	part_transitive(in),
	part_transitive(neer),
	part_transitive(op),
	part_np_np(op), % ik druk hem een stempel op
	er_pp_sbar(op),  % VL = legde er de nadruk op dat
	part_transitive(plat),
	part_transitive(terug),
	part_transitive(uit),
	part_transitive(weg),
        part_fixed(in,[[de,kop],acc],norm_passive),
	fixed([[op,het,hart],dat,vp],imp_passive),
	fixed([[op,het,hart],dat,sbar],imp_passive),
	fixed([pp_pred(in,hand),{[acc,dat]}],no_passive),
	fixed([[de,hand],dat],imp_passive),
	part_ld_pp(af),
	part_np_ld_pp(aan),
	part_np_ld_pp(af),
	part_np_ld_pp(plat),
	part_np_ld_pp(terug),
	part_np_ld_pp(uit),
	part_np_ld_pp(weg)])]).

v(drum,drumt,drummen,gedrumd,drumde,drumden,
    [h([intransitive])]).

v(drup,drupt,druppen,gedrupt,drupte,drupten,
    [b([intransitive,
	ld_pp])]).

v(druppel,druppelt,druppelen,gedruppeld,druppelde,druppelden,
    [h([intransitive,
	transitive]),
     b([ld_pp,
	np_ld_pp,
	part_intransitive(door),
        part_intransitive(aan)])]).

v(dub,dubt,dubben,gedubd,dubde,dubden,
    [h([intransitive,
	transitive, % je kunt nummers dubben
	mod_pp(over),
	sbar])]).  

v(dubbel,dubbelt,dubbelen,gedubbeld,dubbelde,dubbelden,
    [h([pc_pp(met),
	intransitive,
	transitive])]).

v(dubbelklik,dubbelklikt,dubbelklikken,gedubbelklikt,dubbelklikte,dubbelklikten,
    [h([pc_pp(op),
	intransitive])]).

v(ducht,ducht,duchten,geducht,duchtte,duchtten,
    [h([transitive,
	np_pc_pp(van)])]).

v(duelleer,duelleert,duelleren,geduelleerd,duelleerde,duelleerden,
    [h([intransitive])]).

v(duid,duidt,duiden,geduid,duidde,duidden,
    [h([np_np,
	intransitive,
	transitive,
	part_als_pred_np(aan),
	part_np_np(aan),
	part_np_vp_obj(aan),
	part_sbar(aan),
	part_transitive(aan),
	part_vp(aan),
	fixed([[euvel],{[acc,dat]}],norm_passive),
	fixed([[euvel],dat,sbar],imp_passive),
	pc_pp(op),
	er_pp_sbar(op),
	part_np_pc_pp(aan,met)])]).

v(duik,duikt,duiken,gedoken,dook,doken,
    [z([part_intransitive(onder),
	part_intransitive(op),
	part_intransitive(weg),
	part_transitive(in),
	part_ld_pp(op),
	part_pc_pp(weg,achter),
	part_pc_pp(weg,in),
	part_pc_pp(weg,voor)]),
     h([part_transitive(op),
        transitive % je moet een schoppen duiken (bridge)
       ]),
     b([intransitive,
	ld_dir,   % iemand achterna duiken
	ld_adv,
	ld_pp])]).

v(duikel,duikelt,duikelen,geduikeld,duikelde,duikelden,
    [z([ld_pp,
	ld_dir]),
     h([part_transitive(op)]),
     b([intransitive])]).

v(duim,duimt,duimen,geduimd,duimde,duimden,
    [h([intransitive,
	sbar])]).  

v(duizel,duizelt,duizelen,geduizeld,duizelde,duizelden,
    [h([intransitive,
	fixed([het_subj,dat],no_passive),
	fixed([het_subj,dat],no_passive),
        fixed([sbar_subj,dat],no_passive),
        pc_pp(van)  % om van te duizelen
       ])]).

v(duld,duldt,dulden,geduld,duldde,duldden,
    [h([transitive,
	sbar_obj,
	sbar
       ])]).

v(dump,dumpt,dumpen,gedumpt,dumpte,dumpten,
    [h([transitive])]).

v(dun,dunt,dunnen,gedund,dunde,dunden,
    [h([part_intransitive(uit),
	part_transitive(uit),
	intransitive,
	transitive  % van fruitbomen
       ])]).

v(dunk,dunkt,dunken,gedocht,docht,dochten,
    [h([sbar_subj_so_np,                              % het dunkt me dat ..
	dip_sbar_subj_so_np_no_het,                   % mij dunkt dat ..
	fixed([no_subj,dat],no_passive),              % me dunkt !
	fixed([no_subj,yt(acc),nt(dat)],no_passive),  % een probleem dunkt me
	fixed([no_subj,nt(acc),yt(dat)],no_passive),  % mij dunkt een probleem
	                                              % todo: me dunkt een probleem
	part_so_np(goed)])]).

%% de basketbal-lezing
v(dunk,dunkt,dunken,gedunkt,dunkte,dunkten,
  [h([intransitive,
      transitive   % de bal
     ])]).

v(dupeer,dupeert,duperen,gedupeerd,dupeerde,dupeerden,
    [h([transitive])]).

v(dupliceer,dupliceert,dupliceren,gedupliceerd,dupliceerde,dupliceerden,
    [h([transitive])]).

v(durf,durft,durven,gedurfd,durfde,durfden,
    [h([intransitive,
	transitive,
	vp,
	ld_pp,
	ld_dir,
        uit,
	part_transitive(aan),
	part_sbar_obj(aan),
	part_vp_obj(aan),
	subj_control(te_inf)  % VL: ik durf beweren ...
       ])]).

v(dut,dut,dutten,gedut,dutte,dutten,
    [h([intransitive]),
     unacc([part_intransitive(in)])]).

v(duur,duurt,duren,geduurd,duurde,duurden,
    [h([intransitive,
	meas,
	adv_meas,
	so_adv_meas,
	vp_subj_adv_meas,  %% todo: het duurde even voor(dat)/eer/tot hij kwam
        sbar_subj_adv_meas,  %% hoe lang duurt het dat ...
	pc_pp(tot)])]).

v([duw,douw],[duwt,douwt],[duwen,douwen],[geduwd,gedouwd],[duwde,douwde],[duwden,douwden],
    [h([intransitive,
	np_ld_dir,
	transitive,
	ld_adv,
	ld_pp,
	np_ld_pp,
	np_np_ld_pp,
	np_ld_adv,
	part_transitive(naast),
	part_transitive(over),
	part_intransitive(in),
	part_intransitive(naast),
	part_intransitive(over),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(in),
	part_transitive(om),
	part_transitive(vast),
	part_transitive(voort),
	part_transitive(weg)])]).

v(dwaal,dwaalt,dwalen,gedwaald,dwaalde,dwaalden,
    [z([part_intransitive(af),
	part_ld_pp(af)]),
     h([intransitive,
	ld_pp,
	ld_adv,
	part_intransitive(rond),
	part_pc_pp(rond,met)])]).

v(dwarrel,dwarrelt,dwarrelen,gedwarreld,dwarrelde,dwarrelden,
  [z([ld_pp,
      ld_dir]),
   b([intransitive])]).

v(dwars,dwarst,dwarsen,gedwarst,dwarste,dwarsten,
    [h([transitive])]).  % VL

v(dwarsboom,dwarsboomt,dwarsbomen,gedwarsboomd,dwarsboomde,dwarsboomden,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(dweep,dweept,dwepen,gedweept,dweepte,dweepten,
    [h([pc_pp(met),
        sbar])]). %dip

v(dweil,dweilt,dweilen,gedweild,dweilde,dweilden,
    [h([intransitive,
	transitive,
        part_transitive(af),
	part_transitive(op)
       ])]).

v(dwing,dwingt,dwingen,gedwongen,dwong,dwongen,
    [h([intransitive,
	np_ld_dir,
        np_uit,
	np_vp_obj1,
	transitive,
	np_ld_pp,
	pc_pp(tot),
	np_pc_pp(tot),
	obj_np_er_pp_vp(tot),
	part_sbar(af),
	part_pp_sbar(af,van),
	part_transitive(af)])]).

v(eb,ebt,ebben,geëbd,ebde,ebden,
    [z([part_intransitive(weg)]),
     h([intransitive])]).

v(echo,echoot,echoën,geëchood,echode,echoden,
  [h([intransitive,
      dip_sbar,
      part_intransitive(na),
      transitive])]).

v(edit,edit,[editten,editen],[geëdit,'ge-edit'],[editte,edite],[editten,editen],
    [h([intransitive,
	transitive])]).

v(eer,eert,eren,geëerd,eerde,eerden,
    [h([transitive])]).

v(eerbiedig,eerbiedigt,eerbiedigen,geëerbiedigd,eerbiedigde,eerbiedigden,
    [h([transitive])]).

v(eet,eet,eten,gegeten,at,aten,
  [h([ap_pred_np,		% mijn buikje rond?
                                % hommelnesten leeg
      refl_np,  % hij eet zich een hartvervetting
      intransitive,
      transitive,
      part_intransitive(mee),
      pc_pp(van),
      mod_pp(bij), %% eet er geroosterd brood bij
      np_pc_pp(van),
      part_transitive(op),
      part_intransitive(op)  % eet maar lekker op!
     ])]).

v(effectueer,effectueert,effectueren,geëffectueerd,effectueerde,effectueerden,
    [h([transitive])]).

v(effektueer,effektueert,effektueren,geëffektueerd,effektueerde,effektueerden,
    [h([transitive])]).

v(effen,effent,effenen,geëffend,effende,effenden,
  [h([transitive,
      intransitive  % VL: gelijkmaker scoren
     ])]).

v(egaliseer,egaliseert,egaliseren,geëgaliseerd,egaliseerde,egaliseerden,
    [h([transitive])]).

v(eigen,eigent,eigenen,geëigend,eigende,eigenden,
    [h([part_refl_np(toe),
	part_transitive(toe)])]).

v(eindig,eindigt,eindigen,geëindigd,eindigde,eindigden,
    [z([intransitive,
	fixed([rang],no_passive), % VL we eindigden derde
	als_copula,      % ze eindigde als derde
	np_pc_pp(met),
	er_pp_sbar(met),
	er_pp_vp(met),
	pc_pp(in),   % eindigde in een overwinning
	pc_pp(met),  % eindigde met de vrijlating van X.P.
	pc_pp(op)]),
     b([transitive])]).

v(eis,eist,eisen,geëist,eiste,eisten,
    [h([intransitive,
	sbar,
	vp,
	transitive,
	np_pc_pp(van),
        np_mod_pp(voor),
	part_transitive(terug),
	part_np_pc_pp(terug,van),
	part_sbar_subj_np(op),  % het eist onze aandacht op dat ..(?)
	part_transitive(op),
	part_vp_subj_so_np(op),
	pp_sbar(van)])]).

v(ekskuseer,ekskuseert,ekskuseren,geëkskuseerd,ekskuseerde,ekskuseerden,
    [h([transitive])]).

v(elimineer,elimineert,elimineren,geëlimineerd,elimineerde,elimineerden,
    [h([transitive])]).

v(emancipeer,emancipeert,emanciperen,geëmancipeerd,emancipeerde,emancipeerden,
    [h([refl,
	sbar_subj_so_np,  %% ?? het emancipeert ons dat ..
        intransitive,
	transitive])]).

v(email,emailt,emailen,geëmaild,emailde,emailden,
    [h([transitive,
	intransitive,
	np_np, % ik heb hem foto's geëmaild
      sbar,
      np_sbar])]).

v('e-mail','e-mailt','e-mailen','ge-emaild','e-mailde','e-mailden',
    [h([transitive,
	intransitive,
	np_np, % ik heb hem foto's ge-emaild
      sbar,
      np_sbar])]).

v(emigreer,emigreert,emigreren,geëmigreerd,emigreerde,emigreerden,
    [z([intransitive,
	ld_pp])]).

v(emmer,emmert,emmeren,geëmmerd,emmerde,emmerden,
    [h([intransitive,
	er_pp_sbar(over),
	pc_pp(over)])]).

v(emotioneer,emotioneert,emotioneren,geëmotioneerd,emotioneerde,emotioneerden,
    [h([transitive,
	intransitive,
	sbar_subj_so_np,
	vp_subj_so_np])]).  

v(engageer,engageert,engageren,geëngageerd,engageerde,engageerden,
    [h([refl,
	transitive,
	np_pc_pp(voor),
        refl_vp,
        refl_pc_pp(tot),
	refl_er_pp_sbar(tot),
	refl_er_pp_vp(tot),
	refl_ld_pp,
	refl_pc_pp(met)])]).

v(ensceneer,ensceneert,ensceneren,geënsceneerd,ensceneerde,ensceneerden,
    [h([transitive,
	intransitive])]).

v(ent,ent,enten,geënt,entte,entten,
    [h([transitive,
        intransitive,
	np_pc_pp(op),
	part_transitive(in),
	part_intransitive(in), % we moeten gaan inenten
	part_np_pc_pp(in,tegen)])]).

v(enter,entert,enteren,geënterd,enterde,enterden,
    [h([transitive])]).

v(entertain,entertaint,entertainen,[entertaind,geëntertaind],entertainde,entertainden,
  [h([transitive,
      intransitive])]).

v(enthousiasmeer,enthousiasmeert,enthousiasmeren,geënthousiasmeerd,
  enthousiasmeerde,enthousiasmeerden,
    [h([transitive,
	intransitive])]).

v(erf,erft,erven,geërfd,erfde,erfden,
    [z([part_intransitive(over)]),
     h([intransitive,
	transitive,
	pc_pp(van),
	np_pc_pp(van),
	part_transitive(over)])]).

v(erger,ergert,ergeren,geërgerd,ergerde,ergerden,
  [h([refl,
      refl_sbar,  % hij ergerde zich dat de zaak nog niet geregeld was
	sbar_subj_so_np,
	transitive, %% ? of obj2?
	vp_subj_so_np,
	np_pc_pp(door),
	refl_pc_pp(aan),
	refl_pc_pp(over),  % is dat grammaticaal?
	fixed([ap_pred,refl],no_passive),
	%% hij ergert zich dood / groen en geel
	fixed([{[pc(aan),ap_pred]},refl],no_passive),
	fixed([{[er_pp(aan,A),ap_pred]},extra_sbar(A),refl],no_passive),
	refl_er_pp_sbar(aan)])]).

v(erken,erkent,erkennen,erkend,erkende,erkenden,
    [h([als_pred_np,
	fixed([{[acc(meerdere),pc(in)]}],no_passive),
	sbar,
	vp,
	transitive])]).

v(ervaar,ervaart,ervaren,ervaren,[ervaarde,ervoer],[ervaarden,ervoeren],
    [h([als_pred_np,
	sbar,
	transitive,
	fixed([[aan,den,lijve],acc],norm_passive),
	fixed([[aan,den,lijve],sbar],imp_passive)])]).

v(escaleer,escaleert,escaleren,geëscaleerd,escaleerde,escaleerden,
    [z([intransitive])]).

v(escorteer,escorteert,escorteren,geëscorteerd,escorteerde,escorteerden,
    [h([transitive])]).

v(etaleer,etaleert,etaleren,geëtaleerd,etaleerde,etaleerden,
    [h([sbar,
	transitive])]).

v(etiketteer,etiketteert,etiketteren,geëtiketteerd,etiketteerde,etiketteerden,
    [h([transitive])]).

v(ets,etst,etsen,geëtst,etste,etsten,
    [h([intransitive,
	transitive])]).

v(etter,ettert,etteren,geëtterd,etterde,etterden,
    [h([intransitive,
	pc_pp(met)])]).

v(evacueer,evacueert,evacueren,geëvacueerd,evacueerde,evacueerden,
    [h([intransitive,
	transitive,
	np_ld_pp])]).

v(evalueer,evalueert,evalueren,geëvalueerd,evalueerde,evalueerden,
    [h([intransitive,
	transitive])]).

v(evangeliseer,evangeliseert,evangeliseren,geëvangeliseerd,evangeliseerde,evangeliseerden,
    [h([intransitive])]).

v(evenaar,evenaart,evenaren,geëvenaard,evenaarde,evenaarden,
    [h([sbar_subj_np,  % het evenaart de stoutste verwachtingen dat ..
	transitive])]).

v(evolueer,evolueert,evolueren,geëvolueerd,evolueerde,evolueerden,
  [z([intransitive,
      ld_pp])]).

v(examineer,examineert,examineren,geëxamineerd,examineerde,examineerden,
    [h([intransitive,
	transitive])]).

v(excelleer,excelleert,excelleren,geëxcelleerd,excelleerde,excelleerden,
    [h([intransitive,
        pc_pp(in)])]).

v(excuseer,excuseert,excuseren,geëxcuseerd,excuseerde,excuseerden,
    [h([refl,
	intransitive,
	transitive,
	refl_sbar,
        sbar,  % Excuseer dat ik even ...
	refl_pc_pp(voor),
	refl_er_pp_sbar(voor)])]).

v(executeer,executeert,executeren,geëxecuteerd,executeerde,executeerden,
    [h([% refl,
	transitive,
	intransitive])]).

v(expandeer,expandeert,expanderen,geëxpandeerd,expandeerde,expandeerden,
    [b([intransitive]),
     h([transitive])]).

v(experimenteer,experimenteert,experimenteren,geëxperimenteerd,experimenteerde,experimenteerden,
    [h([intransitive,
	pc_pp(met),
	pc_pp(op)])]).

v(expliciteer,expliciteert,expliciteren,geëxpliciteerd,expliciteerde,expliciteerden,
    [h([sbar,
	transitive])]).

v(explodeer,explodeert,exploderen,geëxplodeerd,explodeerde,explodeerden,
    [z([intransitive])]).

v(exploiteer,exploiteert,exploiteren,geëxploiteerd,exploiteerde,exploiteerden,
    [h([transitive])]).

v(exploreer,exploreert,exploreren,geëxploreerd,exploreerde,exploreerden,
    [h([transitive])]).

v(exporteer,exporteert,exporteren,geëxporteerd,exporteerde,exporteerden,
    [h([intransitive,
	np_ld_pp,
	transitive])]).

v(exposeer,exposeert,exposeren,geëxposeerd,exposeerde,exposeerden,
    [h([transitive,
	intransitive])]).

v(extraheer,extraheert,extraheren,geëxtraheerd,extraheerde,extraheerden,
    [h([transitive,
	intransitive])]).

v(extrapoleer,extrapoleert,extrapoleren,geëxtrapoleerd,extrapoleerde,
  extrapoleerden,
    [h([transitive])]).

v(faal,faalt,falen,gefaald,faalde,faalden,
  [h([intransitive,
      pc_pp(in)])]).

v(fabriceer,fabriceert,fabriceren,gefabriceerd,fabriceerde,fabriceerden,
    [h([sbar,
	transitive])]).

v(faciliteer,faciliteert,faciliteren,gefaciliteerd,faciliteerde,faciliteerden,
    [h([transitive,
	intransitive])]).

v(fake,faket,faken,gefaket,fakete,faketen,
    [h([transitive,  % orgasme
	intransitive])]).

v(faliciteer,faliciteert,faliciteren,gefaliciteerd,faliciteerde,faliciteerden,
    [h([transitive])]).

v(fantaseer,fantaseert,fantaseren,gefantaseerd,fantaseerde,fantaseerden,
    [h([intransitive,
	sbar,
	transitive,
	refl_np, % ik fantaseer me een prachtige toekomst
	pc_pp(over)])]).

v(fascineer,fascineert,fascineren,gefascineerd,fascineerde,fascineerden,
    [h([sbar_subj_so_np,
	transitive,
	intransitive,
	vp_subj_so_np])]).

v(faseer,faseert,faseren,gefaseerd,faseerde,faseerden,
  [h([transitive,
      part_transitive(uit)])]).

v(fax,faxt,faxen,gefaxt,faxte,faxten,
    [h([transitive,
	intransitive,
	np_np,
        part_transitive(door),
	part_np_np_ld_pp(toe),
	np_pc_pp(naar),
	part_np_pc_pp(door,naar),
	pc_pp(naar)])]).

v(feest,feest,feesten,gefeest,feestte,feestten,
    [h([intransitive])]).  

v(feliciteer,feliciteert,feliciteren,gefeliciteerd,feliciteerde,feliciteerden,
    [h([transitive,
	np_pc_pp(met),
	np_sbar,
	intransitive])]).

v(field,fieldt,fielden,gefield,fieldde,fieldden,
    [h([intransitive])]).

v(fiets,fietst,fietsen,gefietst,fietste,fietsten,
    [z([ld_dir]),
     h([part_intransitive(rond),
	part_intransitive(aan),
	part_transitive(aan), % de motoren moeten worden aangefietst
	transitive,
	np_ld_dir,            % we hebben hem de stad rondgefietst
	np_ld_pp              % ik fiets jullie er wel even heen
       ]),
     b([ld_pp,
        part_intransitive(langs),
	part_intransitive(verder),  % HACK VL: ik ben op mijn elan blijven verder fietsen
	part_transitive(rond),
	intransitive])]).

v(figureer,figureert,figureren,gefigureerd,figureerde,figureerden,
    [h([intransitive,
	transitive,
	als_copula,
	ld_adv,
	ld_pp])]).

v(fik,fikt,fikken,gefikt,fikte,fikten,
    [h([intransitive])]).

v(fiks,fikst,fiksen,gefikst,fikste,fiksten,
    [h([transitive])]).

v(fileer,fileert,fileren,gefileerd,fileerde,fileerden,
    [h([transitive])]).

v(film,filmt,filmen,gefilmd,filmde,filmden,
    [h([intransitive,
	transitive])]).

v(filosofeer,filosofeert,filosoferen,gefilosofeerd,filosofeerde,filosofeerden,
    [h([intransitive,
	sbar,  % dip
	pc_pp(over),
	er_pp_sbar(over)])]).

v(filter,filtert,filteren,gefilterd,filterde,filterden,
    [z([intransitive]),
     h([transitive,
	np_ld_pp,
	part_transitive(uit)])]).

v(filtreer,filtreert,filtreren,gefiltreerd,filtreerde,filtreerden,
    [h([intransitive,
	transitive])]).

v(financier,financiert,financieren,gefinancierd,financierde,financierden,
    [h([transitive,
	intransitive])]).

v(fingeer,fingeert,fingeren,gefingeerd,fingeerde,fingeerden,
    [h([transitive])]).

v(finish,finisht,finishen,gefinisht,finishte,finishten,
    [z([intransitive,
	fixed([rang],no_passive)  % VL wij finishte derde
       ])]).

v(fitness,fitnesst,fitnessen,gefitnesst,fitnesste,fitnessten,
    [h([intransitive])]).

v(fix,fixt,fixen,gefixt,fixte,fixten,
    [h([transitive])]).

v(fixeer,fixeert,fixeren,gefixeerd,fixeerde,fixeerden,
    [h([sbar,
	transitive,
        intransitive,
	np_pc_pp(op),
	refl_pc_pp(op)])]).

v(fladder,fladdert,fladderen,gefladderd,fladderde,fladderden,
    [b([intransitive,
	part_intransitive(aan),
	part_intransitive(op),
	part_intransitive(weg),
	ld_dir,
	ld_pp])]).

v(flakker,flakkert,flakkeren,geflakkerd,flakkerde,flakkerden,
    [h([intransitive]),
     z([part_intransitive(op)])]).

v(flambeer,flambeert,flamberen,geflambeerd,flambeerde,flambeerden,
    [h([intransitive,
	transitive])]).

v(flaneer,flaneert,flaneren,geflaneerd,flaneerde,flaneerden,
    [h([intransitive])]).

v(flankeer,flankeert,flankeren,geflankeerd,flankeerde,flankeerden,
    [h([intransitive,
	transitive])]).

v(flans,flanst,flansen,geflanst,flanste,flansten,
    [h([intransitive,
	np_pc_pp(in)])]).  

v(flap,flapt,flappen,geflapt,flapte,flapten,
    [z([pc_pp(uit),                    % het flapte eruit
        pp_dip_sbar(uit),              %
        part_pc_er_transitive(uit)]),  % dat het er uitflapte
     h([transitive,
	np_pc_pp(uit),
        part_np_pc_er_transitive(uit),  % ik mag het er niet uitflappen
	er_pp_sbar(uit)])]).


v(flapper,flappert,flapperen,geflapperd,flapperde,flapperden,
    [h([intransitive])]).

v(flatteer,flatteert,flatteren,geflatteerd,flatteerde,flatteerden,
    [h([intransitive,
	sbar_subj_so_np,  % het flatteert de uitslag dat ..
	transitive])]).

v(fleem,fleemt,flemen,gefleemd,fleemde,fleemden,
    [h([intransitive,
	sbar])]).

v(fles,flest,flessen,geflest,fleste,flesten,
    [h([transitive])]).

v(fleur,fleurt,fleuren,gefleurd,fleurde,fleurden,
    [h([part_transitive(op)]),
     z([part_intransitive(op),
	part_pc_pp(op,van)])]).

v(flexibiliseer,flexibiliseert,flexibiliseren,geflexibiliseerd,
  flexibiliseerde,flexibiliseerden,
    [b([intransitive]),
     h([transitive])]).

v(flik,flikt,flikken,geflikt,flikte,flikten,
    [h([np_np,
	transitive])]).

v(flikker,flikkert,flikkeren,geflikkerd,flikkerde,flikkerden,
    [z([part_intransitive(op),
        part_ld_pp(op),
	ld_pp]),
     h([intransitive,
        ld_dir,
	np_ld_pp])]).

v(flip,flipt,flippen,geflipt,flipte,flipten,
    [h([intransitive])]).

v(flipper,flippert,flipperen,geflipperd,flipperde,flipperden,
    [h([intransitive,
	np_ld_pp,
	np_ld_dir])]).

v(flirt,flirt,flirten,geflirt,flirtte,flirtten,
    [h([intransitive,
	pc_pp(met)])]).

v(flits,flitst,flitsen,geflitst,flitste,flitsten,
  [z([ld_pp,
      ld_adv,  % heen en weer bv
      part_intransitive(aan),
      part_intransitive(langs),
      part_intransitive(uit)]),
   h([intransitive,
      transitive])]).

v(floep,floept,floepen,gefloept,floepte,floepten,
    [z([ld_pp,
	part_intransitive(aan),
	part_intransitive(uit)])]).

v(flonker,flonkert,flonkeren,geflonkerd,flonkerde,flonkerden,
    [h([intransitive])]).

v(flop,flopt,floppen,geflopt,flopte,flopten,
    [b([intransitive])]).

v(floreer,floreert,floreren,gefloreerd,floreerde,floreerden,
    [h([intransitive])]).

v(flos,flost,flossen,geflost,floste,flosten,
  [h([intransitive,		% je moet elke dag flossen
      transitive])]).		% tanden alleen zeker?

v(fluctueer,fluctueert,fluctueren,gefluctueerd,fluctueerde,fluctueerden,
    [h([intransitive])]).

v(fluister,fluistert,fluisteren,gefluisterd,fluisterde,fluisterden,
    [h([intransitive,
	sbar,
	transitive,
	vp,
	np_np_ld_pp,   % ik fluister hem iets in het oor
	part_np_np(in),
	part_np_np(toe),
	part_np_sbar(in),
	part_np_sbar(toe),
	part_np_vp_obj(in),
	part_np_vp_obj(toe),
	part_sbar(in),
	part_transitive(in),
	part_vp(in)])]).

v(fluit,fluit,fluiten,gefloten,floot,floten,
    [h([intransitive,
	transitive,
	part_intransitive(af),
	part_transitive(af),
	part_transitive(uit),
	part_transitive(terug)])]).

v(fnuik,fnuikt,fnuiken,gefnuikt,fnuikte,fnuikten,
    [h([transitive])]).

v(focus,focust,focussen,[gefocust,gefocused],focuste,focusten,
    [h([intransitive,
	refl,
	refl_pc_pp(op),
	np_pc_pp(op),  % wij focussen onze aandacht op...
	pc_pp(op)])]).

v(foeter,foetert,foeteren,gefoeterd,foeterde,foeterden,
    [h([intransitive,
	sbar, % dip
        part_transitive(uit),
	pc_pp(over),
	pc_pp(tegen)])]).

v(fok,fokt,fokken,gefokt,fokte,fokten,
    [h([intransitive,
	transitive,
	part_sbar_subj_so_np(op), % het fokt ons op dat ..
	part_transitive(op),
	part_vp_subj_so_np(op)])]).

v(folder,foldert,folderen,gefolderd,folderde,folderden,
    [h([intransitive])]).

v(follow,followt,followen,gefollowd,followde,followden,
    [h([transitive])]).

v(unfollow,unfollowt,unfollowen,geunfollowd,unfollowde,unfollowden,
    [h([transitive])]).

v(folter,foltert,folteren,gefolterd,folterde,folterden,
    [h([intransitive,
	transitive])]).

v(fonkel,fonkelt,fonkelen,gefonkeld,fonkelde,fonkelden,
    [h([intransitive])]).

v(fop,fopt,foppen,gefopt,fopte,fopten,
    [h([transitive])]).

v(forceer,forceert,forceren,geforceerd,forceerde,forceerden,
    [h([% refl,
	transitive,
	sbar,
        refl_np, % VL: de daders forceerden zich toegang...
        intransitive
       ])]).

v(forens,forenst,[forensen,forenzen],[geforensd,geforenst],[forensde,forenste],
  [forensden,forensten],
    [h([intransitive])]).

v(formaliseer,formaliseert,formaliseren,geformaliseerd,formaliseerde,formaliseerden,
    [h([% refl,
	sbar_subj_np, % ?
	transitive])]).

v(formatteer,formatteert,formatteren,geformatteerd,formatteerde,formatteerden,
    [h([transitive,
	intransitive])
    ]).

v(formeer,formeert,formeren,geformeerd,formeerde,formeerden,
    [h([transitive,
	intransitive,
	refl])]).

v(formuleer,formuleert,formuleren,geformuleerd,formuleerde,formuleerden,
    [h([intransitive,
	sbar,
	fixed([{[acc(antwoord),pc(op)]}],norm_passive),
	transitive
       ])]).

v(fotografeer,fotografeert,fotograferen,gefotografeerd,fotografeerde,fotografeerden,
    [h([intransitive,
	transitive])]).

v(fouilleer,fouilleert,fouilleren,gefouilleerd,fouilleerde,fouilleerden,
    [h([intransitive,
	transitive])]).

v(fourneer,fourneert,fourneren,gefourneerd,fourneerde,fourneerden,
    [h([transitive])]).

v(frappeer,frappeert,frapperen,gefrappeerd,frappeerde,frappeerden,
    [h([intransitive,
	so_np,
	sbar_subj,
	sbar_subj_so_np])]).

v(fraudeer,fraudeert,frauderen,gefraudeerd,fraudeerde,fraudeerden,
    [h([intransitive])]).

v(freewheel,freewheelt,freewheelen,gefreewheeld,freewheelde,freewheelden,
    [h([intransitive])]).

v([frekwenteer,frequenteer],[frekwenteert,frequenteert],
  [frekwenteren,frequenteren],[gefrekwenteerd,gefrequenteerd],
  [frekwenteerde,frequenteerde],[frekwenteerden,frequenteerden],
    [h([transitive])]).

v(friemel,friemelt,friemelen,gefriemeld,friemelde,friemelden,
    [h([intransitive,
	ld_adv,
	ld_pp,
	np_ld_pp])]).

v(fris,frist,frissen,gefrist,friste,fristen,
    [z([part_intransitive(op),
        part_pc_pp(op,van)]),
     h([% part_refl(op),
	part_transitive(op)])]).

v(frituur,frituurt,frituren,gefrituurd,frituurde,frituurden,
    [h([transitive,
        intransitive,
	ap_pred_np])]).

v(fröbel,fröbelt,fröbelen,gefröbeld,fröbelde,fröbelden,
    [h([intransitive])]).

v(frommel,frommelt,frommelen,gefrommeld,frommelde,frommelden,
    [h([intransitive,
	np_ld_pp,
	pc_pp(aan)])]).

v(frons,fronst,fronsen,gefronst,fronste,fronsten,
    [h([% refl,
	transitive,
        intransitive])]).

v(fruit,fruit,fruiten,gefruit,fruitte,fruitten,
    [h([transitive,
	ap_pred_np])]).  % fruit de uitjes goudblond/glazig/bruin

v(frummel,frummelt,frummelen,gefrummeld,frummelde,frummelden,
    [h([intransitive])]).

v(frunnik,frunnikt,frunniken,gefrunnikt,frunnikte,frunnikten,
    [h([intransitive,
	ld_pp,
	ld_adv,
	np_ld_pp])]).

v(frustreer,frustreert,frustreren,gefrustreerd,frustreerde,frustreerden,
    [h([sbar_subj_so_np,
        sbar_subj,
	transitive,   % so_np?
        intransitive  % dat frustreert
       ])]).

v(frutsel,frutselt,frutselen,gefrutseld,frutselde,frutselden,
    [h([intransitive,
	transitive,
	ld_dir])]).

v(fuif,fuift,fuiven,gefuifd,fuifde,fuifden,
    [h([intransitive])]).

v(fulmineer,fulmineert,fulmineren,gefulmineerd,fulmineerde,fulmineerden,
    [h([intransitive,
	sbar, % dip
	pc_pp(tegen)])]).

v(functioneer,functioneert,functioneren,gefunctioneerd,functioneerde,functioneerden,
    [h([intransitive,
        als_copula])]).

v(fundeer,fundeert,funderen,gefundeerd,fundeerde,fundeerden,
    [h([sbar,
	transitive,
	np_pc_pp(op)])]).

v(fungeer,fungeert,fungeren,gefungeerd,fungeerde,fungeerden,
    [h([intransitive,
        als_copula])]).

v(fuseer,fuseert,fuseren,gefuseerd,fuseerde,fuseerden,
  [z([intransitive,
      pc_pp(met)]),
   h([transitive])
  ]).

v(fusilleer,fusilleert,fusilleren,gefusilleerd,fusilleerde,fusilleerden,
    [h([transitive])]).

v(föhn,föhnt,föhnen,geföhnd,föhnde,föhnden,
    [h([transitive,
	intransitive])]).

v(ga,gaat,inflected(gaan,gane),gegaan,ging,gingen,ga,
    [z([aux(inf),                       % ik ga fietsen
	ld_pp,                          % ik ga naar huis
	ld_adv,                         % terug in de tijd / terug naar huis / onderlangs
	ap_copula,                      % het horloge ging kapot/stuk/dood/..
	pp_copula(aan,leiding),
	pp_copula(aan,slag),
	pp_copula(aan,werk),
	pp_copula(in,ballingschap),
	pp_copula(in,dienst),
	pp_copula(in,gesprek),
	pp_copula(in,première),
	pp_copula(in,productie),
	pp_copula(in,staking),
	pp_copula(in,honger_staking),
        pp_copula(in,vervulling),
	pp_copula(op,bon),     % vlees gaat op de bon
	pp_copula(op,fles),    % het bedrijf ging op de fles
	pp_copula(op,schop),
        pp_copula(uit,kleed),  % meervoud alleen
        aan_het,
	ld_dir,                         % het bos uit, achteruit
        uit,
	intransitive,                   % ik ga!
	transitive_ndev_ndev,		% hij gaat zijn eigen weg
	                                % er is nog een lange weg te gaan

	fixed([pc(om),dat],no_passive), % het gaat ons om de knikkers
	pc_pp(om),		        % het gaat om de knikkers
	er_pp_sbar(om),                 % het gaat erom dat ..
	er_pp_vp_no_control(om),        % het gaat erom de boeken te lezen
	fixed([er_pp(om,A),dat,extra_sbar(A)],no_passive),
	fixed([er_pp(om,A),i(dat,B),extra_obj_vp(A,B)],no_passive),

	pc_pp(met),                     % we gaan met vakantie/verlof/emiraat
	pc_pp(op),                      % we gaan op vakantie/reis/..

	pc_pp(over),		        % het gaat over iets leuks
	er_pp_sbar(over),               % het gaat erover dat ..
	er_pp_vp_no_control(over),      % het gaat erover de boeken te lezen

	pc_pp(tussen),                  % het gaat nu tussen jou en mij
        pc_pp(voor),                    % we gaan voor goud; we gaan ervoor

        fixed([subj(verhaal),sbar],no_passive),
					% het verhaal gaat dat hij komt

	part_intransitive(aan),         
	part_np_pc_pp(aan,met),         % een verbintenis aangaan met..
	part_vp_subj(aan),              % het gaat niet aan, om VP
	part_sbar_subj(aan),            % het gaat niet aan, dat

	part_so_np(aan),	        % dat gaat mij (niets) aan
	part_np_np(aan),                % wat gaat dat mij aan
	part_sbar_subj_so_np(aan),      % het gaat mij (niets) aan dat ...
	part_sbar_subj_np_np(aan),      % het gaat mij een heleboel aan dat ...
	
	part_intransitive(achteruit),
	part_intransitive(vooruit),
                                        % we gaan er op achteruit
        part_fixed(achteruit,[er_pp(op)],no_passive),
        part_fixed(vooruit,[er_pp(op)],no_passive),
	part_intransitive(af),          % dat ik wel ben afgegaan
	part_pc_pp(af,op),              % ik ga af op zijn verklaring
	part_ap_pred_np(af),            % dat gaat ons goed af
	part_intransitive(binnen),
	part_ld_pp(binnen),
	part_intransitive(dicht),
	part_intransitive(dood),
	part_pc_pp(dood,aan),
        part_intransitive(door),
	part_ld_pp(door),
	part_pc_pp(door,met),
	part_pc_pp(door,op),
	part_pc_pp(door,over),
	part_pc_pp(door,voor),
	part_vp(door),
	part_intransitive(fout), % wat er is fout gegaan
	part_intransitive(goed),      % wat er is goed gegaan
	part_intransitive(heen),        
	part_intransitive(in),
	part_ld_pp(in),
	part_pc_pp(in,op),
	part_pc_pp(in,tegen),
	%% eigenlijk kan dat niet: verplichte modifier...
	fixed([er_pp(in),nor_mod_pp(bij)],no_passive), % deze redenering gaat er bij mij niet in
	fixed([er_pp(in),nor_mod_pp(bij),sbar_subj],no_passive), % het gaat er bij mij niet in dat ...
%	part_pc_pp(in,bij),     
%	part_pp_sbar_subj(in,bij), % het gaat er bij mij niet in, dat ...
        part_ld_pp(langs),      % bij iemand langs gaan
        part_ld_adv(langs),     % thuis langs gaan
        part_intransitive(mee),
	part_ld_pp(mee),
	part_pc_pp(mee,met),
	part_intransitive(mis),
	part_intransitive(om),
	part_pc_pp(om,met),
	part_ld_pp(om),		   % in deze business gaat veel geld om
	part_intransitive(onder),
	part_ld_pp(onder),
	part_intransitive(op), 	   % de redenering gaat op
	part_pc_pp(op,aan),        % het geld ging op aan snoep
	part_pc_pp(op,in),         % hij ging helemaal op in zijn boek
	part_pc_pp(op,voor),       % ik ga op voor mijn examen
	part_so_np(op),		   % toen ging mij een lichtje op
	part_sbar_subj(op),        % VL het gaat niet op dat ...
	part_intransitive(open),
	part_so_np(open),          % toen gingen mij de ogen open
	part_intransitive(over),
	part_pc_pp(over,in),
	part_pc_pp(over,op),
	part_pc_pp(over,tot),
	part_ld_pp(over),   % overgaan naar; overgaan in andere handen
	part_er_pp_vp(over,tot),
	part_intransitive(rond),
	part_pc_pp(rond,met),
	part_intransitive(samen),
	part_pc_pp(samen,in),
	part_pc_pp(samen,met),
	part_intransitive(scheep),
	part_ld_pp(scheep),
	part_ld_pp(schuil),
        part_ld_adv(schuil),
	part_so_np(tegemoet),
	part_sbar(tegen),          % het medicijn ging tegen dat de kwaal...
	part_transitive(tegen),    % dat gaat bijwerkingen tegen
	part_intransitive(tekeer),
	part_pc_pp(tekeer,tegen),
	part_intransitive(teloor),
	part_intransitive(teniet),
	part_intransitive(terug),
	part_ld_pp(terug),
	part_pc_pp(terug,op),
	part_intransitive(toe),
	part_intransitive(uit),
	part_ld_pp(uit),
	part_so_pp(uit),   % er gaan brieven uit aan alle betrokkenen
	part_pc_pp(uit,naar),
	part_pc_pp(uit,op),
	part_pc_pp(uit,van),
	part_er_pp_sbar(uit,van),
	part_er_pp_vp(uit,van),
	%% van+uit is written as one word...:
	% er_pp_sbar(vanuit),
	% er_pp_vp(vanuit),
        part_intransitive(verder),
	part_ld_pp(verder),
	part_dip_sbar(verder),
        part_pc_pp(verder,met),
	part_intransitive(voor),
	part_ld_pp(voor),
	part_transitive(voor),     % ik ga hem voor
        part_np_pc_pp(voor,in),    % hij ging ons voor in de dienst
	part_intransitive(vooraf), % de soep ging de maaltijd vooraf
	part_transitive(vooraf),
	part_pc_pp(vooraf,aan),
	part_intransitive(voorbij),
	part_ld_pp(voorbij),
	part_pc_pp(voorbij,aan),
	part_er_pp_sbar(voorbij,aan),
	part_so_np(voorbij),       % hij gaat mij voorbij
	part_ld_pp(voort),
        part_vp(voort),
	part_pc_pp(voort,met),
        part_sbar(voort),  % dip
	part_intransitive(vooruit),
	part_so_np(vooruit),       % ik ging hun vooruit
	part_pc_pp(vooruit,op),	   % ik ga er op vooruit
	part_np_pc_pp(vooruit,op), % ik ga er honderd gulden op vooruit
	part_intransitive(vreemd),
	part_intransitive(weg),
	part_ld_pp(weg),

	fixed([{[ap_pred('aan de gang'),pc(met)]}],no_passive),
	part_er_pc_pp(vandoor,met), % hij ging er mee vandoor; as if "er" has two roles...
	fixed([svp_er_pp(vandoor),pc(met)],no_passive), % hij ging er met de titel vandoor

	fixed([subj(discussie),pc(over)],no_passive),
	fixed([subj(verhaal),pc(over)],no_passive),

	fixed([het_subj,ap_pred,dat],no_passive), % het gaat hem goed
	fixed([het_subj,{[ap_pred,pc(met)]}],no_passive), % het gaat goed/slecht/matig met hem
	fixed([het_subj,[voor,de,wind],dat],no_passive),
        part_fixed(toe,[er_pp(aan),ap_pred],no_passive),
				% het ging er wild aan toe
        part_fixed(af,[ap_pred,dat],no_passive),
                                % fietsen gaat hem goed af
        fixed([[aan,de,haal],pc(met)],no_passive),
	fixed([[aan,het,hart],dat],no_passive),
	fixed([[aan,het,hart],dat,sbar],no_passive),
        fixed([[dagga]],no_passive), % in tweets "ik ga dagga" = ik ga slapen/naar huis?
        fixed([[door,het,leven],nonp_pred],no_passive),
	fixed([[hand,in,hand]],no_passive),
	fixed([{[[hand,in,hand],pc(met)]}],no_passive),
	fixed([svp_pp(in,beroep)],no_passive),
	fixed([svp_pp(in,cassatie)],no_passive),
	fixed([{[svp_pp(in,beroep),pc(tegen)]}],no_passive),
	fixed([[in,de,fout]],no_passive),
	part_fixed(in,[[de,fout]],no_passive), % hij ging toen de fout weer in
	fixed([[in,discussie]],no_passive),
	fixed([{[[in,discussie],pc(met)]}],no_passive),
	fixed([{[[in,zee],pc(met)]}],no_passive),
	part_fixed(op,[[in,vlammen]],no_passive),
	fixed([[kopje,onder]],no_passive),
	part_fixed(op,[subj(licht),dat],no_passive),
%	fixed([{[[onderdoor],pc(aan)]}],no_passive),
        part_pc_pp(onderdoor,aan),                          % omdat hij er aan onderdoor zou gaan
        part_fixed(door,[[onder],pc(aan)],no_passive),      % omdat hij er aan onder doorgaat
	fixed([[naar,de,knoppen]],no_passive),
	fixed([[door,de,knieën]],no_passive),
        fixed([[overkop]],no_passive),
        fixed([[over,de,kop]],no_passive),
	fixed([[over,de,schreef]],no_passive),
	fixed([[over,de,toonbank]],no_passive),
	fixed([pc(op),[prat]],no_passive), % hij gaat prat op zijn succes
	fixed([er_pp(op,A),[prat],extra_sbar(A)],no_passive),
	fixed([er_pp(op,A),[prat],extra_vp(A)],no_passive),
	part_fixed(op,[subj(stem),vp],no_passive),
	part_fixed(op,[subj(stem),sbar],no_passive),
        fixed([[op,de,loop]],no_passive),
	fixed([[te,boven],dat],norm_passive),
	fixed([ap_pred,pc(aan)],no_passive),  % hij gaat te gronde/kapot/dood/.. aan
	fixed([[te,keer]],no_passive),
        fixed([[te,rade],ld_pp],no_passive),
        fixed([[te,rade],ld_adv],no_passive),
	fixed([[te,werk]],imp_passive),
        part_intransitive(tewerk),


        fixed([[ten,onder]],no_passive),
        fixed([[ten,onder],pc(aan)],no_passive),

        part_intransitive(tenonder),
        part_pc_pp(tenonder,aan),

        part_intransitive('ten onder'),
        part_pc_pp('ten onder',aan),

	fixed([[ter,perse]],no_passive),
	fixed([[uit,de,weg],acc],norm_passive),
	fixed([ap_pred(ver)],no_passive),
	fixed([ap_pred(ver),dat],no_passive),
	fixed([ap_pred(ver),sbar_subj],no_passive),
	fixed([ap_pred(ver),dat,sbar_subj],no_passive),
	fixed([ap_pred(ver),vp_subj],no_passive),
	fixed([ap_pred(ver),dat,vp_subj],no_passive),
	fixed([pc(van),[ten,koste]],no_passive),
	fixed([{[[te,buiten],pc(aan)]},refl],no_passive),
	fixed([[te,buiten],acc],no_passive),
	fixed([{[[te,laste],pc(van)]}],no_passive),
	fixed([{[[te,last],pc(van)]}],no_passive),
	fixed([{[[ten,laste],pc(van)]}],no_passive),
	fixed([{[[tenlaste],pc(van)]}],no_passive),
	fixed([{[[tenlast],pc(van)]}],no_passive),
	fixed([{[[telast],pc(van)]}],no_passive),
	fixed([[in,zijn,werk]],no_passive),  % hoe gaat dat in zijn werk?
	fixed([[naar,de,klote]],no_passive),
	fixed([[te,lijf],acc],norm_passive),  % hij werd te lijf gegaan
	fixed([[ten,aanval]],no_passive),
	fixed([[ten,gronde]],no_passive),
	fixed([[ten,gronde],pc(aan)],no_passive),
	fixed([[ter,harte],dat],no_passive),
	fixed([[van,start]],imp_passive),
	fixed([[van,au]],no_passive),
	fixed([[van,een,leien,dakje]],no_passive),
        fixed([[voor,anker]],imp_passive),
	fixed([[voor,de,bijl]],no_passive),
        fixed([ap_pred(vrijuit)],no_passive),
	fixed([[te,moede],ap_pred,dat],no_passive),
	fixed([[zijns,weegs]],no_passive),
	fixed([[huns,weegs]],no_passive),
	fixed([[ons,weegs]],no_passive),
	fixed([[jouws,weegs]],no_passive),
	fixed([[haar,weegs]],no_passive)
    ]),
     b([part_transitive(aan), % de verplcihtignen die we hebben/zijn aangegaan
	part_sbar(na),        % ik ging na of hij de waarheid sprak
	part_transitive(na)   % ik ga zijn gangen na,
       ])]).

v(gaap,gaapt,gapen,gegaapt,gaapte,gaapten,
    [h([intransitive,
	part_transitive(aan),
        ld_pp, % daar gaapt een groot gat tussen
	pc_pp(naar)])]).

v(gaar,gaart,garen,gegaart,gaarde,gaarden,
    [h([transitive])]).

v(gak,gakt,gakken,gegakt,gakte,gakten,
    [h([intransitive])]).

v(galm,galmt,galmen,gegalmd,galmde,galmden,
    [h([intransitive,
	transitive,
	part_intransitive(na),
	sbar % dip
       ])]).

v(galoppeer,galoppeert,galopperen,gegaloppeerd,galoppeerde,galoppeerden,
    [b([intransitive])]).

v(game,gamet,gamen,gegamed,gamede,gameden,
    [h([intransitive])]).

v(gap,gapt,gappen,gegapt,gapte,gapten,
    [h([transitive])]).

v(garandeer,garandeert,garanderen,gegarandeerd,garandeerde,garandeerden,
  [h([np_np,
      so_pp_np,
      np_sbar,
      so_pp_sbar,
      np_vp_subj,
      sbar_subj_so_np,
      sbar,
      transitive,
      vp])]).

v(garneer,garneert,garneren,gegarneerd,garneerde,garneerden,
    [h([transitive,
	np_pc_pp(met),
        pc_pp(met)])]).

v(gebaar,gebaart,gebaren,gebaard,gebaarde,gebaarden,
    [h([np_vp_obj,
	sbar,
	np_sbar,
	intransitive,
	vp])]).

v(gebeur,gebeurt,gebeuren,gebeurd,gebeurde,gebeurden,
    [unacc([intransitive,
	    sbar_subj,
	    sbar_subj_so_np,	% het zal je maar gebeuren dat ..
	    so_np,
	    fixed([[waar]],no_passive),  % het is waar gebeurd
            mod_pp(omheen),		 % er gebeurde veel omheen
	    mod_pp(in),                  % daar moet nog veel in gebeuren
	    pc_pp(aan),                  % ?
	    pc_pp(met),                  % ?
            no_subj   % en zo gebeurde .
           ])]).

v(gebied,gebiedt,gebieden,geboden,gebood,geboden,
    [h([np_np,
	intransitive,
	np_sbar,
	np_vp_obj,
	sbar_subj_so_np,
	vp_subj_so_np,
	sbar,
	transitive,
	vp])]).

v(gebruik,gebruikt,gebruiken,gebruikt,gebruikte,gebruikten,
    [h([als_pred_np,
	intransitive,
	transitive,
	fixed([acc,rel_om],norm_passive),
	np_pc_pp(voor),
	np_mod_pp(bij),  % daar kunt u wel wat hulp bij gebruiken
	np_mod_pp(van),  % daar heb ik nog niets van gebruikt
	part_transitive(op)])]).

v(gedenk,gedenkt,gedenken,gedacht,gedacht,gedachten,
    [h([sbar,
	transitive])]).

v(gedij,gedijt,gedijen,gedijd,gedijde,gedijden,
    [h([intransitive])]).

v(gedoog,gedoogt,gedogen,gedoogd,gedoogde,gedoogden,
    [h([sbar,
	transitive,
        intransitive,
	vp,
	np_ld_pp])]).

v(gedraag,gedraagt,gedragen,gedragen,gedroeg,gedroegen,
    [h([refl,
	nonp_pred_refl])]).

v(geef,geeft,geven,gegeven,gaf,gaven,
    [h([np_np,
	intransitive,  % het geeft niet
	so_pp_np,
%% 	so_np,  ??
	transitive,
	so_np_pc_pp(tot),	% ik geef jullie tot vrijdag
	fixed([pc(tot),vp_no_control],no_passive),
	                        % ik geef tot vrijdag om ...
	fixed([{[i(dat,I),pc(tot)]},obj_vp(I)],no_passive),
	                        % ik geef jullie tot vrijdag om ...
        so_control(pass_te),  % hij geeft ons dat te doen
        part_so_control(mee,pass_te),  % hij geeft ons dat boek mee te lezen / te lezen mee
%	np_pc_pp(op),  % ik geef hem op zijn donder/flikker/...
	np_mod_pp(bij), % geef er stokbrood bij
        fixed([als_pred,sbar],imp_passive),
        fixed([als_pred,vp],imp_passive),
	fixed([{[als_pred,acc,dat]}],norm_passive),
	fixed([{[als_pred,acc,dat_pp(aan)]}],norm_passive),
        als_pred_np,
	sbar_subj,     % het geeft niet dat je wat later bent
	sbar_subj_np,  % het geeft een kick / een goed gevoel / ... dat ...
        np_mod_pp(voor), 
	part_als_pred_np(op),
	part_intransitive(aan),	% tenzij anders is aangegeven
	part_sbar_sbar_subj(aan),
	part_np_np(aan),
	part_np_np(door),
	part_np_np(in),
	part_np_np(mee), % NB dat ik hem de spullen mee naar huis geef
	part_sbar(mee),  % VL
	part_np_sbar(mee),
	part_so_pp_np(mee),
	part_np_np(na),
	part_np_np(op),
	part_np_np(prijs),
	part_np_np(terug),
	part_np_np(toe),
	part_intransitive(af),
	part_intransitive(les),
	part_so_np(les),
	part_so_pp(les),
        part_intransitive(mee),
	part_intransitive(op),  % schaker Kris: ik geef niet op!
        part_pc_pp(op,van),     % waar hij (hoog) van opgeeft
	part_intransitive(over),
	part_intransitive(toe),
        part_pc_pp(uit,op),     % uitzicht geven op... Vlaams?
	part_intransitive(voor),
	part_intransitive(vrij),
	part_np_sbar(aan),
	part_np_sbar(na),
	part_np_sbar(op),
	part_np_sbar(prijs),
	part_np_sbar(toe),
	part_np_vp_obj(in),
	part_np_vp_obj(op),
	part_refl(bloot),
	part_transitive(bloot),
	%part_refl(op),
	part_refl(over),
	part_sbar(aan),
        part_pp_sbar(aan,met),  % ik wil hier mee aangeven dat...
	part_sbar(op),
	part_sbar(prijs),
	part_sbar(toe),
	part_so_pp_np(af),
	part_so_pp_np(door),
	part_so_pp_np(op),
	part_so_pp_np(over),
	part_so_pp_np(terug),
	np_pc_pp(van),    % overzicht, definitie, schets, voorbeeld, ...
	np_np_pc_pp(van), % ik geef jullie een voorbeeld van ... 
	part_fixed(uit,[{[pc(aan),acc(belasting_geld)]}],norm_passive),
	part_fixed(uit,[{[pc(voor),acc(belasting_geld)]}],norm_passive),
	part_fixed(uit,[{[pc(aan),acc(geld)]}],norm_passive),
	part_fixed(uit,[{[pc(voor),acc(geld)]}],norm_passive),
	part_fixed(uit,[{[er_pp(voor,X),acc(geld)]},extra_vp(X)],norm_passive),
	part_so_np(toe),
	part_so_np(vrij),
	part_so_np(vrijaf),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(door),
	part_sbar(door),
	part_np_sbar(door),
	part_so_pp_sbar(door),
	part_transitive(bij), % De jonge chauffeur gaf echter nog gas bij .
	part_transitive(in),
	part_transitive(mee),
	part_transitive(na),
	part_transitive(op),
	part_transitive(over),
	part_transitive(prijs),
	part_transitive(terug),
	part_transitive(toe),
	part_transitive(uit),
	part_intransitive(uit),  % Drentenaren geven het minst uit
	part_als_pred_refl(uit), % zich uitgeven als iemand anders
	part_ld_pp(uit),         % VL de weg geeft uit in de Gildenstraat
	part_transitive(voor),
	part_transitive(vorm),
	part_transitive(vrij),
	part_transitive(weer),
	part_sbar(weer),
	part_transitive(weg),
	part_vp(aan),
	part_vp(op),
	part_vp(toe),
	part_vp(voor),
	pc_pp(om),
	fixed([{[pc(tot),acc(aanleiding)]}],norm_passive),
	fixed([{[pc(op),acc(antwoord)]}],norm_passive),
       	fixed([pc(op),acc(antwoord),dat],norm_passive),
       	fixed([inv(acc(antwoord)),pc(op),dat],norm_passive),
       	fixed([acc(antwoord),inv(dat),pc(op)],norm_passive),
	fixed([{[acc(bevel),pc(tot)]}],norm_passive),
	fixed([{[pc(aan),[gehoor]]}],imp_passive),
	fixed([{[pc(aan),[gevolg]]}],imp_passive),
	fixed([[cadeau],{[acc,dat]}],norm_passive),
	fixed([[cadeau],{[acc,dat_pp(aan)]}],norm_passive),
	fixed([{[pc(op),acc(commentaar)]}],norm_passive),
       	fixed([pc(op),acc(commentaar),dat],norm_passive),
       	fixed([inv(acc(commentaar)),pc(op),dat],norm_passive),
       	fixed([acc(commentaar),inv(dat),pc(op)],norm_passive),
	fixed([[kado],{[acc,dat]}],norm_passive),
	fixed([[kado],{[acc,dat_pp(aan)]}],norm_passive),
        fixed([{[[de,brui],er_pp(aan)]}],no_passive),
        fixed([{[[de,brui],er_pp(van)]}],no_passive),  % VL Buitenbeen geeft er voor dit seizoen de brui van .
	fixed([{[acc(kritiek),pc(op)]}],norm_passive),
	fixed([sbar_subj_no_het,acc(doorslag)],no_passive),
	fixed([[te,leen],{[acc,dat]}],norm_passive),
	fixed([[te,leen],{[acc,dat_pp(aan)]}],norm_passive),
	fixed([acc(tijd),me,dat],imp_passive),
	fixed([acc(tijd),me,i(dat,X),obj_vp(X)],imp_passive),
	fixed([acc(tijd),dat],imp_passive),
	fixed([acc(tijd),i(dat,X),obj_vp(X)],imp_passive),
	fixed([{[[gestalte],pc(in)]},dat],imp_passive),
	fixed([[gewonnen],refl],no_passive),
	fixed([{[acc(moeite)]},refl],imp_passive),
	fixed([{[acc(moeite),pc(voor)]},refl],imp_passive),
	fixed([{[[opdracht],pc(tot)]}],imp_passive),
        fixed([[present]],imp_passive),	% Vlaams
	fixed([{[acc(recht),pc(op)]}],no_passive),
	fixed([{[acc(rekenschap),pc(van)]},refl],imp_passive),
	fixed([{[acc(rekenschap),er_pp(van,B)]},extra_sbar(B),refl],imp_passive),
	fixed([{[acc(rekenschap),er_pp(van,B)]},extra_vp(B),refl],imp_passive),
	fixed([{[acc(schuld),pc(van)]},dat],norm_passive),
	fixed([{[acc(schuld),er_pp(van,B)]},dat,extra_sbar(B)],norm_passive),
	fixed([{[acc(schuld),er_pp(van,B)]},dat,extra_vp(B)],norm_passive),
	fixed([acc(schuld),dat,sbar],no_passive),
	fixed([ap_pred('ter beschikking'),{[acc,dat]}],norm_passive),  
	fixed([pp_pred(uit,hand),{[acc,dat]}],norm_passive),  
	fixed([pp_pred(uit,hand),acc],norm_passive), 
	fixed([{[acc(uiting),pc(aan)]}],norm_passive),
	fixed([{[er_pp(aan,A),acc(voorkeur)]},extra_sbar(A)],norm_passive),
	fixed([{[er_pp(aan,A),acc(voorkeur)]},extra_vp(A)],norm_passive),
	part_fixed(langs,[er_pp(van),dat],no_passive),
	fixed([er_pp(vanlangs),dat],no_passive),
	fixed([[geen,pas],sbar_subj],no_passive),
	fixed([[geen,pas]],no_passive),
	fixed([[geen,pas],vp_subj],no_passive),
	fixed([[geen,krimp]],imp_passive),
	fixed([[gestalte],dat],imp_passive),
	fixed([[gestalte],dat_pp(aan)],imp_passive),
        fixed([{[acc(kennis),pc(van)]}],norm_passive),
	fixed([{[acc(kennis),er_pp(van,X)]},extra_sbar(X)],norm_passive),
        fixed([{[acc(kennis),dat,pc(van)]}],norm_passive),
	fixed([{[acc(kennis),dat,er_pp(van,X)]},extra_sbar(X)],norm_passive),
        fixed([{[acc(kennis),dat_pp(aan),pc(van)]}],norm_passive),
	fixed([{[acc(kennis),dat_pp(aan),er_pp(van,X)]},extra_sbar(X)],norm_passive),
	fixed([pp_pred(in,bewaring),acc],norm_passive),
	fixed([pp_pred(in,bewaring),{[acc,dat_pp(aan)]}],norm_passive),
	fixed([pp_pred(in,bewaring),{[acc,dat]}],norm_passive),
	fixed([[het,nakijken],dat],imp_passive),
	fixed([pp_pred(in,hand),{[acc,dat_pp(aan)]}],norm_passive),
	fixed([pp_pred(in,hand),{[acc,dat]}],norm_passive),
	fixed([pp_pred(in,hand),{[acc,pc(van)]}],norm_passive),
	fixed([[in,overweging],acc],norm_passive),
	fixed([[in,overweging],vp_no_control],imp_passive),
	fixed([[in,overweging],vp_no_control,dat],imp_passive),
	fixed([[in,overweging],vp_no_control,dat_pp(aan)],imp_passive),
	fixed([[in,overweging],sbar],imp_passive),
	fixed([[in,overweging],sbar,dat],imp_passive),
	fixed([[in,overweging],sbar,dat_pp(aan)],imp_passive),
	fixed([[ter,overweging],acc],norm_passive),
	fixed([[ter,overweging],vp_no_control],imp_passive),
	fixed([[ter,overweging],vp_no_control,dat],imp_passive),
	fixed([[ter,overweging],vp_no_control,dat_pp(aan)],imp_passive),
	fixed([[ter,overweging],sbar],imp_passive),
	fixed([[ter,overweging],sbar,dat],imp_passive),
	fixed([[ter,overweging],sbar,dat_pp(aan)],imp_passive),
        fixed([acc(toegang),pc(tot)],norm_passive),
        fixed([acc(toegang),{[dat,pc(tot)]}],norm_passive),
	fixed([[partij],dat],imp_passive),
        fixed([vc(denk,pass_te,intransitive),sbar_subj],no_passive),
        fixed([vc(denk,pass_te,intransitive),dat,sbar_subj],no_passive),

	fixed([vc(ken,pass_te,intransitive),acc],norm_passive),
	fixed([vc(ken,pass_te,intransitive),dat,sbar],imp_passive),
	fixed([vc(ken,pass_te,intransitive),dat,vp],imp_passive),
	fixed([vc(ken,pass_te,intransitive),sbar],imp_passive),
	fixed([vc(ken,pass_te,intransitive),vp],imp_passive),
	fixed([vc(ken,pass_te,intransitive),{[acc,dat]}],imp_passive),

	fixed([vc(versta,pass_te,intransitive),acc],no_passive),
	fixed([vc(versta,pass_te,intransitive),dat,sbar],imp_passive),
	fixed([vc(versta,pass_te,intransitive),dat,vp],imp_passive),
	fixed([vc(versta,pass_te,intransitive),{[acc,dat_pp(aan)]}],imp_passive),
	fixed([vc(versta,pass_te,intransitive),dat_pp(aan),sbar],imp_passive),
	fixed([vc(versta,pass_te,intransitive),dat_pp(aan),vp],imp_passive),
	fixed([vc(versta,pass_te,intransitive),sbar],imp_passive),
	fixed([vc(versta,pass_te,intransitive),vp],imp_passive),
	fixed([vc(versta,pass_te,intransitive),{[acc,dat]}],no_passive),
        %% also fixed? "te zien geven"
	fixed([[ten,antwoord],dat,sbar],imp_passive),
	fixed([[ten,beste],acc],norm_passive),
	fixed([[ten,gehore],acc],norm_passive),
	fixed([{[pc(over),acc(informatie)]}],norm_passive),

        fixed([pc(over),acc(informatie),dat],norm_passive),
       	fixed([inv(acc(informatie)),pc(over),dat],norm_passive),
       	fixed([acc(informatie),inv(dat),pc(over)],norm_passive),

        fixed([pc(van),acc(beeld),dat],norm_passive),
       	fixed([inv(acc(beeld)),pc(van),dat],norm_passive),
       	fixed([acc(beeld),inv(dat),pc(van)],norm_passive),
        fixed([pc(van),acc(beeld)],norm_passive),
       	fixed([inv(acc(beeld)),pc(van)],norm_passive),

        fixed([{[acc(voorbeeld),pc(van)]}],no_passive),
        fixed([{[acc(voorbeeld),dat,pc(van)]}],no_passive),
	fixed([{[acc(blijk),pc(van)]}],norm_passive),
	fixed([{[acc(blijk),er_pp(van,B)]},extra_sbar(B)],imp_passive),
	fixed([{[acc(blijk),er_pp(van,B)]},extra_vp(B)],imp_passive),
	fixed([acc(blijk),vp],imp_passive),
	part_er_pp_sbar(blijk,van),
	part_er_pp_vp(blijk,van),  % wordorder!
	part_pc_pp(blijk,van),
	part_np_pc_pp(af,op),
	part_np_pc_pp(af,aan),
	part_np_pc_pp(mee,met),
	part_np_pc_pp(op,voor),
	part_np_pc_pp(prijs,aan),
	part_np_pc_pp(terug,aan),
	part_np_pc_pp(toe,op),
	part_np_pc_pp(uit,aan),
	part_np_pc_pp(weer,in),
	part_np_pc_pp(weg,aan),
	part_pc_pp(af,op),
	part_so_np(les),
        part_so_pp(les),
	part_pc_pp(les,in),
	part_pc_pp(terug,van),
	part_pc_pp(toe,aan),
	part_pc_pp(toe,op),
	part_pc_pp(toe,tegenover),
	part_pc_pp(vorm,aan),
	part_refl_pc_pp(af,met),
%	part_refl_pc_pp(op,bij),
%	part_refl_pc_pp(op,voor),
	part_refl_pc_pp(over,aan),
	part_refl_pc_pp(uit,voor)])]).

v(geeuw,geeuwt,geeuwen,gegeeuwd,geeuwde,geeuwden,
    [h([intransitive,
        sbar % dip
       ])]).

v(gehoorzaam,gehoorzaamt,gehoorzamen,gehoorzaamd,gehoorzaamde,gehoorzaamden,
    [h([intransitive,
	transitive,
	pc_pp(aan)])]).

v(geil,geilt,geilen,gegeild,geilde,geilden,
    [h([pc_pp(op),
        part_transitive(op)])]).

v(geit,geit,geiten,gegeit,geitte,geitten,
    [h([intransitive])]).

v(gekscheer,gekscheert,gekscheren,gegekscheerd,gekscheerde,gekscheerden,
  [h([intransitive,
      dip_sbar,
      pc_pp(met)])]).

v(gelast,gelast,gelasten,gelast,gelastte,gelastten,
    [h([np_np,
	np_sbar,
	sbar,
	transitive,
	vp_no_control,
	np_vp_obj,
	np_pc_pp(tot),
	part_transitive(af)])]).

v(geld,geldt,gelden,gegolden,gold,golden,
    [h([intransitive,
	sbar_subj_no_het,
        dip_sbar_subj_no_het,  % ook hier geldt: bla bla
	so_np,
	fixed([sbar_subj_no_het,als_pred],no_passive),
        als_copula,
	pc_pp(voor),
	pp_sbar_subj_no_het(voor)
       ])]).

v(geleid,geleidt,geleiden,geleid,geleidde,geleidden,
    [h([intransitive,
	transitive,
	part_np_ld_pp(voor),
	np_ld_pp])]).

v(gelief,gelieft,gelieven,geliefd,geliefde,geliefden,gelieve,
    [h([vp])]).

v(gelijk,gelijkt,gelijken,geleken,geleek,geleken,
    [h([pc_pp(naar),
	pc_pp(op),
	transitive])]).

v(geloof,gelooft,geloven,geloofd,geloofde,geloofden,
    [h([intransitive,
	sbar,
	np_sbar,                % en geloof me dat dit beter is voor iedereen
        sbar_obj,               % VL ik kan het niet geloven dat ..
	van_sbar,		% ik geloof van niet
	transitive,
	vp,
	np_pc_pp(van),		% ?? ik geloof er niets van
	np_er_pp_sbar(van),	% ik geloof er niets van dat ..
	pc_pp(aan),
	pc_pp(in),
	er_pp_sbar(in)])]).

%% remove gelukt, that one is psp of lukken anyway
v(geluk,gelukt,gelukken,_,gelukte,gelukten,
  [unacc([intransitive,
	  so_np,
	  vp_subj
	 ])]).

v(geneer,geneert,generen,gegeneerd,geneerde,geneerden,
    [h([refl,
	sbar_subj_so_np, % ?
	np_sbar,
	np_vp_subj,
	refl_sbar,
	refl_vp
	% transitive
       ])]).

v(genees,geneest,genezen,genezen,genas,genazen,
    [unacc([intransitive,
	pc_pp(van)]),
     h([transitive,
	np_mod_pp(met),   % om er ziektes mee te genezen
	np_pc_pp(van)])]).

v(generaliseer,generaliseert,generaliseren,gegeneraliseerd,generaliseerde,generaliseerden,
    [h([intransitive,
	transitive])]).

v(genereer,genereert,genereren,gegenereerd,genereerde,genereerden,
    [h([intransitive,
	transitive])]).

v(geniet,geniet,genieten,genoten,genoot,genoten,
    [h([intransitive,
	transitive,
        part_intransitive(mee),
        part_pc_pp(mee,van),
        part_intransitive(na),
        part_pc_pp(na,van),
	pc_pp(van),
	er_pp_sbar(van),
	er_pp_vp(van)])]).

%% komt niet echt meer voor...
%%v(genoeg,genoegt,genoegen,genoegd,genoegde,genoegden,
%%    [h([so_np])]).

v(geraak,geraakt,geraken,geraakt,geraakte,geraakten,
    [unacc([intransitive,
            part_intransitive(binnen),
            part_transitive(binnen),
            part_intransitive(in),
            part_transitive(in),
	    part_intransitive(bekend),
	    part_sbar_subj_no_het(bekend),
            fixed([[in,de,vergetelheid]],no_passive),
	    nonp_copula,
            ld_adv,
            ld_pp])]).

v(gerief,gerieft,gerieven,geriefd,geriefde,geriefden,
    [h([transitive])]).

% en zo geschiedde.  NO subject, nor any complements!
v(geschied,geschiedt,geschieden,geschied,geschiedde,geschiedden,
    [unacc([intransitive,
            so_np,
            no_subj])]).

v(gesel,geselt,geselen,gegeseld,geselde,geselden,
    [h([transitive])]).

v(gesp,gespt,gespen,gegespt,gespte,gespten,
    [h([intransitive,
	transitive,
	part_transitive(om),
	part_np_np(om),  % hij gespte de wandelaars een hartslagmeter om
	np_ld_pp,
	part_transitive(aan),
	part_transitive(af),
	part_np_np(aan)
       ])]).

v(gesticuleer,gesticuleert,gesticuleren,gegesticuleerd,gesticuleerde,gesticuleerden,
    [h([intransitive])]).

v(gestikuleer,gestikuleert,gestikuleren,gegestikuleerd,gestikuleerde,gestikuleerden,
    [h([intransitive])]).

v(getroost,getroost,getroosten,getroost,getroostte,getroostten,
    [h([refl_np])]).

v(getuig,getuigt,getuigen,getuigd,getuigde,getuigden,getuige,
    [h([intransitive,
	sbar,
	transitive,
	vp,
	pc_pp(naar),
	pc_pp(tegen),
	pc_pp(van),
	pc_pp(voor)])]).

v(geur,geurt,geuren,gegeurd,geurde,geurden,
    [h([intransitive,
	pc_pp(met),
	pc_pp(naar)])]).

v(geval,gevalt,gevallen,gevallen,geviel,gevielen,
  [h([sbar_subj,   % ouderwets "toen geviel het dat ..."
      part_transitive(wel),
      part_fixed_dep(wel,intransitive)])]).

v(gevoel,gevoelt,gevoelen,gevoeld,gevoelde,gevoelden,
    [h([sbar,
	transitive])]).

v(gewaag,gewaagt,gewagen,gewaagd,gewaagde,gewaagden,
    [h([pc_pp(van)])]).

v(gids,gidst,gidsen,gegidst,gidste,gidsten,
    [h([intransitive,
	np_ld_dir,
	np_ld_pp])]).

v(giechel,giechelt,giechelen,gegiecheld,giechelde,giechelden,
    [h([intransitive,
	sbar, % dip,
	pc_pp(om)])]).

v(gier,giert,gieren,gegierd,gierde,gierden,
    [h([intransitive,
	part_fixed(uit,[het_obj1],no_passive)]),
     z([ld_pp,
        part_intransitive(aan),
        so_np_ld_pp,
	fixed([subj(zenuw),svp_pp(door,keel),dat],no_passive)])]).

v(giet,giet,gieten,gegoten,goot,goten,
    [h([ap_pred_np,
	het_subj,
	transitive,
	part_transitive(uit),
	np_ld_pp,
        np_np_ld_pp,
	part_np_np(in),
	part_np_sbar(in),     % met de paplepel ingieten
	part_transitive(af),  % aardappelen
        part_intransitive(af),
	part_transitive(over),
	part_np_ld_pp(over)])]).

v(gijzel,gijzelt,gijzelen,gegijzeld,gijzelde,gijzelden,
    [h([transitive])]).

v(gil,gilt,gillen,gegild,gilde,gilden,
    [h([intransitive,
	sbar,
	part_transitive(uit),  % ze gilde het uit
	transitive])]).

v(gireer,gireert,gireren,gegireerd,gireerde,gireerden,
    [h([transitive])]).

v(gis,gist,gissen,gegist,giste,gisten,
    [h([intransitive,
	transitive,
        sbar,
	pc_pp(naar),
	er_pp_sbar(naar)])]).

v(gist,gist,gisten,gegist,gistte,gistten,
    [h([intransitive])]).

v(glaceer,glaceert,glaceren,geglaceerd,glaceerde,glaceerden,
    [h([transitive])]).

v(glans,glanst,glanzen,geglansd,glansde,glansden,
    [h([intransitive,
	part_transitive(toe)])]).

v(glazuur,glazuurt,glazuren,geglazuurd,glazuurde,glazuurden,
    [h([transitive])]).

v(glibber,glibbert,glibberen,geglibberd,glibberde,glibberden,
    [z([intransitive,
	part_intransitive(aan),
	ld_pp,
        ld_dir])]).

v([glijd,glij],glijdt,glijden,gegleden,gleed,gleden,
    [z([ld_dir,
	ld_pp,
	part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(uit),
        part_pc_pp(uit,over),
	part_intransitive(weg),
	part_ld_pp(af),
	part_ld_pp(weg),
	part_pc_pp(af,naar)]),
     h([np_ld_pp,
	np_ld_dir,
        intransitive   % het ijs glijdt niet.
       ])]).

v(glim,glimt,glimmen,geglommen,glom,glommen,
    [h([intransitive,
        part_so_np(tegemoet),
	pc_pp(van)])]).

v(glimlach,glimlacht,glimlachen,geglimlacht,glimlachte,glimlachten,
    [h([intransitive,
	dip_sbar,
	mod_pp(bij),
	part_transitive(toe),
	part_transitive(weg),
	transitive,
	pc_pp(om)])]).

v(glinster,glinstert,glinsteren,geglinsterd,glinsterde,glinsterden,
    [h([intransitive])]).

v(glip,glipt,glippen,geglipt,glipte,glipten,
  [unacc([intransitive,
          ld_dir,
          ld_pp,
	  so_np_ld_pp, % het geld glipt me door de vingers
          part_intransitive(binnen)])]).

v(glitter,glittert,glitteren,geglitterd,glitterde,glitterden,
    [h([intransitive])]).

v(globaliseer,globaliseert,globaliseren,geglobaliseerd,globaliseerde,globaliseerden,
    [h([intransitive])]).

v(gloei,gloeit,gloeien,gegloeid,gloeide,gloeiden,
    [h([intransitive,
	part_intransitive(op),
	pc_pp(van)])]).

v(glooi,glooit,glooien,geglooid,glooide,glooiden,
    [h([intransitive])]).

v(gloor,gloort,gloren,gegloord,gloorde,gloorden,
    [h([intransitive])]).

v(glorieer,glorieert,gloriëren,geglorieerd,glorieerde,glorieerden,
    [h([intransitive])]).

v(glunder,glundert,glunderen,geglunderd,glunderde,glunderden,
    [h([intransitive,
	sbar % dip
       ])]).

v(gluur,gluurt,gluren,gegluurd,gluurde,gluurden,
  [h([intransitive,
      	ld_dir,  % ze gluurde de gang in
	ld_pp])]).

v(gniffel,gniffelt,gniffelen,gegniffeld,gniffelde,gniffelden,
    [h([intransitive,
	sbar,
        mod_pp(om),
	mod_pp(over)])]).

v(gnuif,gnuift,gnuiven,gegnuifd,gnuifde,gnuifden,
    [h([intransitive,
	sbar,
	transitive])]).

v(gok,gokt,gokken,gegokt,gokte,gokten,
    [h([intransitive,
	sbar,
	part_intransitive(mis),
	part_intransitive(raak),
	pc_pp(op),
	er_pp_sbar(op),
	er_pp_vp(op),
	fixed([pc(op),het_obj1],no_passive), % ik gok het er op
	fixed([er_pp(op,A),het_obj1,extra_sbar(A)],no_passive) % we gokken het erop dat ..
       ])]).

v(golf,golft,golven,gegolfd,golfde,golfden,
    [h([intransitive]),
     z([ld_pp,
	part_intransitive(door),
	part_intransitive(aan)])]).

v(golf,golft,golfen,gegolft,golfte,golften,
    [h([intransitive])]).

v(gom,gomt,gommen,gegomd,gomde,gomden,
    [h([transitive,
        intransitive,
        part_transitive(uit),
        part_transitive(weg)])]).

v(gons,gonst,gonzen,gegonsd,gonsde,gonsden,
    [h([intransitive,
	part_intransitive(na)])]).

v(goochel,goochelt,goochelen,gegoocheld,goochelde,goochelden,
  [h([intransitive,
      np_ld_dir,  % hij goochelde het konijn tevoorschijn
      pc_pp(met)])]).

v([google,googel],[googlet,googelt],[googleën,googelen],[gegoogled,gegoogeld],[googlede,googelde],[googleden],[googelden],
  [h([intransitive,
      transitive])]).

v(gooi,gooit,gooien,gegooid,gooide,gooiden,
    [h([np_np,
        nonp_pred_np, % ik gooi alles overhoop / in de war / stuk / kapot ...
	intransitive,
	np_ld_dir,
	transitive,
	ld_adv,
	ld_pp,
	np_ld_pp,
	np_np_ld_pp,
	part_intransitive(in),
	part_transitive(aan),
	part_transitive(af), % bridge: harten afgooien
	part_transitive(in),
	part_transitive(kapot),
        part_transitive(leeg),
	part_np_ld_pp(leeg),
	part_transitive(los),
	part_transitive(neer),
	part_transitive(om),
	part_transitive(op),
	part_transitive(open),
	part_np_np(toe),
	part_transitive(uit),
	part_transitive(weg),
	part_intransitive(mis),
	part_transitive(mis),
	part_intransitive(raak),
	part_transitive(raak),
	part_np_np(aan),
	pc_pp(met),
	pc_pp(om),
	part_fixed(neer,[er_pp(bij),acc(bijl_DIM)],norm_passive),
	fixed([[voor,de,voeten],{[dat,acc]}],imp_passive),
	fixed([[voor,de,voeten],dat,sbar],imp_passive),
        fixed([[op,een,accoordje],het_obj1],no_passive),
	fixed([[in,het,honderd],acc],norm_passive),
	fixed([er_pp(naar),svp_pp(met,pet)],imp_passive),
	fixed([[te,grabbel],acc],norm_passive)])]).

v(gorgel,gorgelt,gorgelen,gegorgeld,gorgelde,gorgelden,
    [h([intransitive])]).

v(gourmet,gourmet,gourmetten,gegourmet,gourmette,gourmetten,
    [h([intransitive])]).

v(graaf,graaft,graven,gegraven,groef,groeven,
    [h([intransitive,
	transitive,
	part_intransitive(af),
	ld_pp,
	refl_ld_pp,
	refl_np_ld_pp,	       % hij groef zich een weg naar buiten
	ld_adv,
	np_ld_pp, % een gracht graven om de stad; er een gracht omheen graven
	mod_pp(met),
%	part_refl(in),
	part_transitive(af),
	part_transitive(door),
	part_transitive(in),
	part_transitive(op),
	part_transitive(uit)])]).

v(graai,graait,graaien,gegraaid,graaide,graaiden,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv])]).

v(graas,graast,grazen,gegraasd,graasde,graasden,
    [h([intransitive,
	part_transitive(af),
	transitive])]).

v(grabbel,grabbelt,grabbelen,gegrabbeld,grabbelde,grabbelden,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv])]).

v(grap,grapt,grappen,gegrapt,grapte,grapten,
    [h([intransitive,
	sbar])]).

v(grasduin,grasduint,grasduinen,gegrasduind,grasduinde,grasduinden,
    [h([intransitive])]).

v(grauw,grauwt,grauwen,gegrauwd,grauwde,grauwden,
    [h([intransitive,
	transitive,
	sbar,
	np_sbar,
	part_np_sbar(toe),
	part_np_np(toe)])]).

v(graveer,graveert,graveren,gegraveerd,graveerde,graveerden,
    [h([transitive])]).

v(grendel,grendelt,grendelen,gegrendeld,grendelde,grendelden,
    [h([transitive,
	part_transitive(af)])]).

v(grens,grenst,grenzen,gegrensd,grensde,grensden,
    [h([pc_pp(aan),
        intransitive,
	part_transitive(af)])]).

v(grief,grieft,grieven,gegriefd,griefde,griefden,
    [h([intransitive,
	sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(grien,grient,grienen,gegriend,griende,grienden,
    [h([intransitive,
        mod_pp(om)])]).

v(griezel,griezelt,griezelen,gegriezeld,griezelde,griezelden,
  [h([intransitive,
      pc_pp(van)])]).

v(grif,grift,griffen,gegrift,grifte,griften,
    [h([transitive,
	np_ld_pp])]).

v(gril,grilt,grillen,gegrild,grilde,grilden,
    [h([intransitive,
      transitive,
      ap_pred_np])]).

v(grill,grillt,grillen,gegrilld,grillde,grillden,
    [h([intransitive,
      transitive,
      ap_pred_np])]).

v(grijns,grijnst,grijnzen,gegrijnsd,grijnsde,grijnsden,
    [h([intransitive,
	sbar,			% dip only?
	mod_pp(bij),  % hij grijnst er gemeen bij
	part_transitive(aan),
	part_transitive(toe),
	pc_pp(naar)])]).

v(grijp,grijpt,grijpen,gegrepen,greep,grepen,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv,
	fixed([[bij,de,keel],acc],norm_passive),
	fixed([[naar,de,keel],acc],norm_passive),
        fixed([[om,zich,heen]],no_passive),
	part_intransitive(in),
	part_intransitive(mis),
	part_intransitive(plaats),
	part_transitive(aan),
	part_transitive(beet),
	part_transitive(vast),
	part_ld_pp(in),
	part_ld_pp(plaats),
        part_ld_pp(vast),
	part_transitive(terug),
	part_pc_pp(aan,op),
	part_pc_pp(terug,naar),
	part_pc_pp(terug,op)])]).

v(grijs,grijst,grijzen,gegrijsd,grijsde,grijsden,
    [z([intransitive])]).

v(grinnik,grinnikt,grinniken,gegrinnikt,grinnikte,grinnikten,
    [h([intransitive,
	sbar, % dip
	pc_pp(over),
	pc_pp(om),
	pc_pp(tegen)])]).

v(gris,grist,grissen,gegrist,griste,gristen,
    [h([np_np,
	transitive])]).

v(groei,groeit,groeien,gegroeid,groeide,groeiden,
    [unacc([intransitive,
            ld_dir,
            ld_pp,
            fixed([[boven,het,hoofd],dat],no_passive),
            part_intransitive(aan),
            part_intransitive(op),
	    part_mod_pp(op,in),   % een mooi land om in op te groeien
	    part_mod_pp(op,met),  % daar zijn we mee opgegroeid
            part_intransitive(toe),
            part_intransitive(uit),
            part_ld_pp(door),
            part_ld_pp(toe),
            part_ld_pp(uit),
            part_pc_pp(uit,tot)])]).

v(groep,groept,groepen,gegroept,groepte,groepten,
    [h([part_intransitive(samen),
        intransitive])]).

v(groepeer,groepeert,groeperen,gegroepeerd,groepeerde,groepeerden,
    [h([intransitive,
	refl,
	transitive,
	refl_pc_pp(in),
        np_pc_pp(in),
	refl_pc_pp(tot)])]).

v(groet,groet,groeten,gegroet,groette,groetten,
    [h([transitive,
        intransitive,
        part_intransitive(terug),
        part_transitive(terug)])]).

v(grol,grolt,grollen,gegrold,grolde,grolden,
    [h([intransitive])]).

v(grom,gromt,grommen,gegromd,gromde,gromden,
    [h([intransitive,
	sbar,
	transitive])]).

v(grond,grondt,gronden,gegrond,grondde,grondden,
    [h([transitive,
	np_ld_pp])]).

%%% wat sommige eenden doen:
v(grondel,grondelt,grondelen,gegrondeld,grondelde,grondelden,
    [h([intransitive])]).

v(grondvest,grondvest,grondvesten,gegrondvest,grondvestte,grondvestten,
    [h([transitive,
	np_ld_pp])]).

v(grossieer,grossiert,grossieren,gegrossierd,grossierde,grossierden,
    [h([pc_pp(in)])]).

v(gruw,gruwt,gruwen,gegruwd,gruwde,gruwden,
    [h([intransitive,
	pc_pp(van)])]).

v(gruwel,gruwelt,gruwelen,gegruweld,gruwelde,gruwelden,
    [h([intransitive,
	pc_pp(van)])]).

v(gulp,gulpt,gulpen,gegulpt,gulpte,gulpten,
    [b([intransitive,
	ld_dir,
	ld_pp])]).

v(gum,gumt,gummen,gegumd,gumde,gumden,
    [h([transitive,
        intransitive,
        part_transitive(uit),
        part_transitive(weg)])]).

v(gun,gunt,gunnen,gegund,gunde,gunden,
  [h([np_np,
      so_pp_np,
      transitive,
      so_np_sbar_obj,
      so_np_vp_obj])]).

v(guts,gutst,gutsen,gegutst,gutste,gutsten,
    [z([ld_pp]),
     h([intransitive,
	transitive
       ])]).

v(gym,gymt,gymen,gegymd,gymde,gymden,
    [h([intransitive])]).

v(haak,haakt,haken,gehaakt,haakte,haakten,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv,
	part_intransitive(af),
	part_intransitive(in),
	part_transitive(af),
	part_transitive(in),
	part_intransitive(aan),
	part_transitive(aan),
	part_ld_pp(aan),
	part_np_ld_pp(aan),
	pc_pp(naar),
	part_np_ld_pp(in),
	part_pc_pp(in,op)])]).

v(haal,haalt,halen,gehaald,haalde,haalden,
    [h([intransitive,
	np_ld_dir,
        np_np_ld_pp,  % u haalt me de woorden uit de mond...
	transitive,
	np_ld_pp,
	part_als_pred_np(aan),
	part_intransitive(aan),
        part_sbar(aan),
	part_intransitive(adem),
	part_intransitive(in),
	part_intransitive(op),
	part_intransitive(over),
	part_intransitive(uit),
	part_np_vp_obj1(over),
	part_sbar_subj_np(op), % ? het haalt herinneringen op dat ..
	part_transitive(aan),
	part_transitive(af),
	part_transitive(bij),
	part_transitive(binnen),
	part_transitive(door),
	part_transitive(in),
	part_transitive(leeg),
	part_transitive(neer),
	part_transitive(omver),
	part_transitive(om),  % de bal omhalen
	part_transitive(onderuit),
	part_transitive(op),
	part_transitive(over),
	part_transitive(overhoop),
	part_transitive(terug),
	part_transitive(uit),
	part_transitive(weg),
	pc_pp(aan),
	fixed([[bakzeil]],no_passive),
	fixed([[geen,bakzeil]],no_passive),
%	pp_vp_obj(in),
	pp_sbar(uit), % we halen daar uit of iemand gedronken heeft
	part_fixed(op,[{[pc(voor),acc(neus)]}],norm_passive),
	part_fixed(op,[{[mod_pp(bij),acc(schouder)]}],norm_passive),
	part_fixed(op,[{[pc(over),acc(schouder)]}],norm_passive),
	part_fixed(op,[acc(schouder),dip_sbar],norm_passive),
	fixed([[door,het,slijk],acc],norm_passive),
	fixed([[uit,het,slop],acc],norm_passive),
	fixed([svp_pp(op,hals),acc],norm_passive),
	fixed([svp_pp(op,hals),{[acc,refl]}],norm_passive),
	fixed([ap_pred('uit elkaar'),acc],norm_passive),
	fixed([[voor,de,geest],{[acc,refl]}],norm_passive),
	fixed([[voor,de,geest],refl,sbar],norm_passive),
        fixed([[de,koekoek],[je]],no_passive),
	part_fixed(vandaan,[[onder,de,nagels],{[[het,bloed],dat]}],imp_passive),
	fixed([svp_pp(in,hoofd),acc],no_passive),
	fixed([svp_pp(in,hoofd),het_pobj1(vp)],no_passive),
				% hij haalde het in zijn hoofd om ..
	part_ld_pp(uit),
	part_obj_np_er_pp_vp(over,tot),
	part_np_ld_pp(weg),
	part_np_pc_pp(op,aan),
	part_np_pc_pp(over,tot),
	part_np_pc_pp(uit,met),
	part_pc_pp(uit,met),
	part_pc_pp(uit,naar)])]).

v(haast,haast,haasten,gehaast,haastte,haastten,
    [h([intransitive,
	refl,
	refl_ld_dir,
	refl_vp,  % hij haast zich te zeggen dat het een vergissing was
%	transitive,  % ?
	refl_ld_pp,
	refl_pc_pp(met)])]).

v(haat,haat,haten,gehaat,haatte,haatten,
    [h([transitive,
	sbar_obj,
	vp_obj])]).

v(hack,hackt,hacken,gehackt,hackte,hackten,
    [h([intransitive,
	transitive])]).
	
v(hagel,hagelt,hagelen,gehageld,hagelde,hagelden,
      [h([het_subj,
          fixed([het_subj,acc],no_passive)])]).
				%  het hagelt grote korrels Venz

v(hak,hakt,hakken,gehakt,hakte,hakten,
    [h([nonp_pred_np,
	intransitive,
	transitive,
	ld_pp,
	ld_adv,
        ld_dir,
	np_ld_pp,
	np_ld_adv,
	part_np_np(af),
	part_transitive(af),
	part_transitive(door),
	part_transitive(fijn),
	part_transitive(om),
	part_transitive(open),
	part_transitive(uit),
        part_transitive(weg),
	part_pc_pp(in,op),
	part_fixed(door,[{[mod_pp(over),acc(knoop)]}],norm_passive),  % daar zijn geen knopen over doorgehakt
	pc_pp(op),
	part_np_ld_pp(af)])]).

v(hakkel,hakkelt,hakkelen,gehakkeld,hakkelde,hakkelden,
    [h([intransitive,
	sbar,
	transitive])]).

v(hallucineer,hallucineert,hallucineren,gehallucineerd,hallucineerde,hallucineerden,
    [h([intransitive,
	sbar_subj_so_np,
	transitive])]).

v(halveer,halveert,halveren,gehalveerd,halveerde,halveerden,
    [h([transitive]),
     z([intransitive])]).

v(hamer,hamert,hameren,gehamerd,hamerde,hamerden,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	part_transitive(af),
	part_transitive(in),
	part_np_np(in),  % hij heeft dat er ingehamerd gekregen; ??er??
	er_pp_vp(op),
	er_pp_sbar(op),
	np_ld_pp,
	np_ld_adv
       ])]).

v(hamster,hamstert,hamsteren,gehamsterd,hamsterde,hamsterden,
    [h([intransitive,
	transitive])]).

v(handbal,handbalt,handballen,gehandbald,handbalde,handbalden,
    [h([intransitive])]).  

v(handel,handelt,handelen,gehandeld,handelde,handelden,
    [h([intransitive,
	part_transitive(af),
	pc_pp(in),
	pc_pp(naar),	      % we moeten handelen naar onze principes
	                      % als .. dan moeten we daar ook naar handelen
	pc_pp(met),
	pc_pp(op),
	pc_pp(over)])]).

v(handhaaf,handhaaft,handhaven,gehandhaafd,handhaafde,handhaafden,
    [h([%refl,
	transitive,
	intransitive  % ik zal handhaven
	%refl_pc_pp(tegen)
       ])]).

v(handicap,handicapt,handicappen,gehandicapt,handicapte,handicapten,
    [h([sbar_subj_so_np,  % ???
	transitive])]).

v(hang,hangt,hangen,gehangen,hing,hingen,
    [z([part_intransitive(uit)]),
     h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
        ld_dir,  % omlaag hangen
	np_ld_pp,
	np_ld_adv,
	subj_control(pass_te),	% ?omdat het wasgoed hangt te drogen
	                        % omdat het wasgoed te drogen hangt
				% het wasgoed heeft daar te drogen gehangen
	obj_control(pass_te),   % ik hang het wasgoed te drogen
	                        % TODO het wasgoed wordt te drogen gehangen
%	                        % ALLOWS *omdat ik het wasgoed hang te drogen
%	                        % omdat ik het wasgoed te drogen hang
	fixed([ap_pred('te kijk')],no_passive), % VL
	fixed([ap_pred('te kijk'),acc],norm_passive), % VL
	fixed([ap_pred('halfstok')],no_passive), 
	fixed([ap_pred('halfstok'),acc],norm_passive), 
        norm_passive,           % omdat de kunstwerken hangen uitgestald
        sbar_passive,           % er hing bijvermeld dat ...
	fixed([[boven,het,hoofd],dat],no_passive),
	part_intransitive(aan),
	part_intransitive(af),
        part_ld_er_transitive(bij), % zoals ze er bijhangen
	part_intransitive(los),
	part_intransitive(neer),
	part_intransitive(op),
	part_intransitive(over),
	part_intransitive(rond),
	part_intransitive(samen),
        part_intransitive(tentoon),
	part_ld_adv(uit),  % waar heb jij uitgehangen?
	part_pc_pp(vast,aan),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(in),
	part_transitive(neer),
	part_np_np(om),
	part_transitive(om),
	part_transitive(op),
	part_transitive(over),
	part_transitive(uit),
	pc_pp(naar),
	fixed([[in,de,lucht]],no_passive),
	part_fixed(uit,[[de,beest]],no_passive),
	part_fixed(uit,[[de,keel],dat],no_passive),
	part_fixed(uit,[sbar_subj,[de,keel],dat],no_passive),
	fixed([svp_pp(aan,klok),sbar],no_passive),
	fixed([svp_pp(aan,klok),acc],no_passive),
	part_er_pp_sbar(af,van),
	part_ld_pp(rond),
	part_np_ld_pp(op),
	part_pp_sbar_subj(af,van),  % het hangt van jou af of hij komt 
	part_pc_pp(af,van),
	part_pc_pp(samen,met),
	part_fixed(samen,[pc(met),sbar],no_passive)]),
     b([part_ld_pp(uit)])]).

v(hannes,hannest,hannesen,gehannest,hanneste,hannesten,
    [h([transitive])]).

v(hanteer,hanteert,hanteren,gehanteerd,hanteerde,hanteerden,
    [h([transitive])]).

v(hap,hapt,happen,gehapt,hapte,hapten,
    [h([intransitive,
	transitive,
	part_intransitive(af),  % Een Bijtschildpad kan een kinderhand afhappen
	part_intransitive(toe),
        part_intransitive(weg), % hapt zo heerlijk weg
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv])]).

v(haper,hapert,haperen,gehaperd,haperde,haperden,
    [h([intransitive,
	pc_pp(aan)])]).

v(hard,hardt,harden,gehard,hardde,hardden,
  [unacc([intransitive,
	  part_intransitive(uit) % van lijm
	 ]),
     h([transitive,
	np_pc_pp(tegen)])]).

v(hark,harkt,harken,geharkt,harkte,harkten,
    [h([intransitive,
	transitive,
	part_transitive(aan),
	np_ld_dir,
	np_ld_pp])]).

v(harmonieer,harmonieert,harmoniëren,geharmonieerd,harmonieerde,harmonieerden,
    [h([intransitive,
	pc_pp(met)])]).

v(harmoniseer,harmoniseert,harmoniseren,geharmoniseerd,harmoniseerde,harmoniseerden,
    [h([transitive])]).

v(hars,harst,harsen,geharst,harste,harsten,
    [h([transitive,
        intransitive])]).

v(haspel,haspelt,haspelen,gehaspeld,haspelde,haspelden,
    [h([part_transitive(af)])]).

v(haven,havent,havenen,gehavend,havende,havenden,
    [h([transitive])]).

v(hecht,hecht,hechten,gehecht,hechtte,hechtten,
    [h([intransitive,
	refl,
	transitive,
	part_transitive(aan),
	np_pc_pp(aan), % belang, waarde, ..?
	pc_pp(aan),
	er_pp_sbar(aan),
	np_er_pp_sbar(aan),
	er_pp_vp(aan),
	np_er_pp_vp(aan),
	refl_pc_pp(aan),
	part_refl_pc_pp(aan,aan)   % ... waaraan zich geen tRNA kan aanhechten
       ])]).

v(heel,heelt,helen,geheeld,heelde,heelden,
    [unacc([intransitive]),
     h([transitive])]).

v(heers,heerst,heersen,geheerst,heerste,heersten,
    [h([intransitive,
	ld_pp,
	ld_adv,
	pc_pp(over)])]).

v(heet,heet,heten,geheten,heette,heetten,
    [h([cleft,
	copula,
	copula_np,
	copula_sbar,
	copula_vp,
	np_pred_np,
	dip_sbar_subj,   % het heet dat ..; we moeten , heet het , naar huis
	intransitive, % naar het heet, ...
	pc_pp(naar),
	aux(te),
	fixed([[welkom],acc],norm_passive)])]).

v(hef,heft,heffen,geheven,hief,hieven,
    [h([transitive,
	np_ld_pp,
	np_ld_dir,
	fixed([pc(op),acc(belasting)],norm_passive),
	fixed([pc(over),acc(belasting)],norm_passive),
	part_intransitive(aan),
	part_sbar_subj_so_np(op),  %? het heft ons op dat we naar de kerk gaan
	part_transitive(aan),
	part_transitive(op)])]).

v(hei,heit,heien,geheid,heide,heiden,
    [h([intransitive,
	np_ld_dir  % ze heien de palen de grond in
       ])]).
  

v(heilig,heiligt,heiligen,geheiligd,heiligde,heiligden,
    [h([np_np,
	sbar_subj_so_np,
	transitive])]).

v(hekel,hekelt,hekelen,gehekeld,hekelde,hekelden,
    [h([transitive,
	sbar % VL
       ])]).

v(hel,helt,hellen,geheld,helde,helden,
    [b([intransitive,
	ld_dir,
	ld_pp,
	part_intransitive(over),
	part_ld_pp(over)])]).

v(helder,heldert,helderen,gehelderd,helderde,helderden,
    [unacc([part_intransitive(op)]),
     h([part_transitive(op),
	part_sbar(op)])]).

v(help,helpt,helpen,geholpen,hielp,hielpen,
    [h([aci,
	aci_no_obj,
	intransitive,
	np_ld_dir,
	np_vp_obj1,
	vp_no_control,
	sbar_subj_so_np,
                                % het helpt dat hij..
        sbar_subj_opt_het,      % Ook helpt dat ...
	transitive_ndev,
	vp_subj_so_np,
        vp_subj,
	sbar_obj, % wij kunnen het niet helpen dat ...
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv,
	np_pc_pp(aan),  % ik help hem aan een baan
	np_mod_pp(bij),
	np_mod_pp(met),
        part_intransitive(mee),
        part_transitive(mee),
        part_transitive(vooruit),
	part_vp(mee),
	pc_pp(aan),
	pc_pp(tegen),
	mod_pp(bij),
	mod_pp(met),
        pp_pred_np(aan,slag),
        pp_pred_np(aan,werk),
	pp_pred_np(in,zadel),
	fixed([[naar,de,klote],acc],norm_passive),
	fixed([[om,zeep],acc],norm_passive),
	fixed([ap_pred('op de been'),acc],norm_passive),
	fixed([ap_pred('op weg'),acc],norm_passive),
	part_np_pc_pp(af,van),
	part_pc_pp(mee,aan),
	part_pc_pp(mee,tegen),
	part_er_pp_sbar(mee,aan),
	part_mod_pp(mee,bij),
	part_mod_pp(mee,in),
	part_mod_pp(mee,met)])]).

v(hemel,hemelt,hemelen,gehemeld,hemelde,hemelden,
    [h([part_transitive(op)])]).

v(hengel,hengelt,hengelen,gehengeld,hengelde,hengelden,
  [h([intransitive,
      np_ld_pp,
      np_ld_adv,
      pc_pp(naar)])]).

v(heradem,herademt,herademen,herademd,herademde,herademden,
    [h([intransitive])]).

v(herbegin,herbegint,herbeginnen,herbegonnen,herbegon,herbegonnen,
    [b([transitive,
        intransitive,
	vp])]).

v(herbekijk,herbekijkt,herbekijken,herbekeken,herbekeek,herbekeken,
    [h([sbar,
	transitive])]).

v(herbeleef,herbeleeft,herbeleven,herbeleefd,herbeleefde,herbeleefden,
    [h([transitive])]).

v(herbenoem,herbenoemt,herbenoemen,herbenoemd,herbenoemde,herbenoemden,
    [h([transitive,
        als_pred_np,
	np_pc_pp(tot)])]).

v(herberg,herbergt,herbergen,geherbergd,herbergde,herbergden,
    [h([transitive])]).

v(herbevestig,herbevestigt,herbevestigen,herbevestigd,herbevestigde,herbevestigden,
    [h([sbar,
	np_sbar,
	so_pp_sbar,
	vp,
	transitive,
	intransitive
       ])]).

v(herbewapen,herbewapent,herbewapenen,herbewapend,herbewapende,herbewapenden,
    [h([transitive,
	np_pc_pp(tegen)])]).

v(herbezin,herbezint,herbezinnen,herbezonnen,herbezon,herbezonnen,
    [b([refl,
        refl_pc_pp(op),
        refl_pc_pp(over)])]).

v(herbouw,herbouwt,herbouwen,herbouwd,herbouwde,herbouwden,
    [h([transitive])]).

v(herbron,herbront,herbronnen,herbrond,herbonde,herbronden,
    [h([refl])]).

v(herbruik,herbruikt,herbruiken,herbruikt,herbruikte,herbruikten,
    [h([transitive])]).

v(herdenk,herdenkt,herdenken,herdacht,herdacht,herdachten,
    [h([intransitive,
	sbar,
	transitive])]).

v(herdruk,herdrukt,herdrukken,herdrukt,herdrukte,herdrukten,
    [h([transitive])]).

v(herenig,herenigt,herenigen,herenigd,herenigde,herenigden,
    [h([transitive,
	refl,  % daar herenigde zich het peleton
	np_pc_pp(met)])]).

v(hergeef,hergeeft,hergeven,hergeven,hergaf,hergaven,
    [h([np_np,
	transitive,
	intransitive])]).

v(hergebruik,hergebruikt,hergebruiken,hergebruikt,hergebruikte,hergebruikten,
    [h([intransitive,
        transitive])]).

v(hergroepeer,hergroepeert,hergroeperen,gehergroepeerd,hergroepeerde,
  hergroepeerden,
    [h([intransitive,
	%refl,
	transitive
       ])]).

v(herhaal,herhaalt,herhalen,herhaald,herhaalde,herhaalden,
    [h([refl,  % tot twee maal toe herhaalde zich dit ritueel
	sbar,
	np_np,  % ik herhaal het u (ouderwets)
	transitive,
	vp,
        intransitive])]).

v(herijk,herijkt,herijken,herijkt,herijkte,herijkten,
    [h([transitive])]).

v(herinner,herinnert,herinneren,herinnerd,herinnerde,herinnerden,
    [h([refl_np,
	refl_vp,
	refl_sbar,
	refl,             % als ik me goed herinner, ...
	np_refl_pc_pp(van),  % daar herinner ik me niets/nog enkele details/weinig van
	sbar,
	transitive,       % hij wordt herinnerd vanwege/door/als ...
	er_pp_sbar(aan),
	np_er_pp_sbar(aan),
	er_pp_sbar(aan),
	np_pc_pp(aan),
	pc_pp(aan)])]).

v(herkauw,herkauwt,herkauwen,herkauwd,herkauwde,herkauwden,
    [h([intransitive,
	transitive])]).

v(herkans,herkanst,herkansen,herkanst,herkanste,herkansten,
    [h([intransitive,    % ga je herkansen = proefwerk overdoen
	transitive])]).  % ik ga wiskunde herkansen

v(herken,herkent,herkennen,herkend,herkende,herkenden,
    [h([transitive,
        part_transitive(terug), % ik herken je niet meer terug
        np_pc_pp(van), % ik herken daar niets meer van
	np_pc_pp(aan),
	np_pc_pp(in)])]).

v(herkeur,herkeurt,herkeuren,herkeurd,herkeurde,herkeurden,
    [h([transitive])]).

v(herkies,herkiest,herkiezen,herkozen,herkoos,herkozen,
    [h([transitive])]).

v(herkrijg,herkrijgt,herkrijgen,herkregen,herkreeg,herkregen,
    [h([transitive])]).

v(herleef,herleeft,herleven,herleefd,herleefde,herleefden,
    [unacc([intransitive,
	pc_pp(in)])]).

v(herlees,herleest,herlezen,herlezen,herlas,herlazen,
    [h([transitive])]).

v(herleid,herleidt,herleiden,herleid,herleidde,herleidden,
    [h([als_pred_np,
	transitive,
	sbar,
        np_pc_pp(op),
	np_pc_pp(tot)])]).

v(herneem,herneemt,hernemen,hernomen,hernam,hernamen,
  [h([intransitive,
      dip_sbar,
      transitive])]).

v(hernieuw,hernieuwt,hernieuwen,hernieuwd,hernieuwde,hernieuwden,
    [h([transitive])]).

v(hernoem,hernoemt,hernoemen,hernoemd,hernoemde,hernoemden,
    [h([transitive,
        np_pc_pp(naar),
	np_pc_pp(tot)])]).

v(herontdek,herontdekt,herontdekken,herontdekt,herontdekte,herontdekten,
    [h([transitive])]).

v(heropen,heropent,heropenen,heropend,heropende,heropenden,
    [h([transitive])]).

v(heropleeft,heropleeft,heropleven,heropleefd,heropleefde,heropleefden,
    [h([intransitive])]).

v(herover,herovert,heroveren,heroverd,heroverde,heroverden,
    [h([transitive,
	np_pc_pp(op)])]).

v(heroverweeg,heroverweegt,heroverwegen,heroverwogen,heroverwoog,heroverwogen,
    [h([sbar,
	transitive])]).

v(herpak,herpakt,herpakken,herpakt,herpakte,herpakten,
    [h([refl,
	transitive])]).

v(herplaats,herplaatst,herplaatsen,herplaatst,herplaatsten,herplaatste,
    [h([transitive])]).

v(herrijs,herrijst,herrijzen,herrezen,herrees,herrezen,
    [unacc([intransitive,
	    ld_pp])]).

v(herroep,herroept,herroepen,herroepen,herriep,herriepen,
    [h([sbar,
	transitive,
	vp,
	intransitive])]).

v(herschep,herschept,herscheppen,herschapen,herschiep,herschiepen,
    [h([als_pred_np,
	transitive,
	pc_pp(in),
	intransitive])]).

v(herschik,herschikt,herschikken,herschikt,herschikte,herschikten,
    [h([transitive])]).

v(herschrijf,herschrijft,herschrijven,herschreven,herschreef,herschreven,
    [h([transitive])]).

v(herspeel,herspeelt,herspelen,herspeeld,herspeelde,herspeelden,
    [h([transitive])]).

v(herstart,herstart,herstarten,herstart,herstartte,herstartten,
    [h([transitive]),
     z([intransitive])
    ]).

v(herstel,herstelt,herstellen,hersteld,herstelde,herstelden,
  [z([intransitive,
      pc_pp(van)
     ]),
   h([refl,
      transitive,
      refl_pc_pp(van)])
  ]).

v(herstructureer,herstructureert,herstructureren,geherstructureerd,herstructureerde,herstructureerden,
    [h([transitive,
	intransitive])]).

v(herteken,hertekent,hertekenen,hertekend,hertekende,hertekenden,
    [h([transitive])]).

%% stemmen in Miami
v(hertel,hertelt,hertellen,herteld,hertelde,hertelden,
    [h([transitive,
	intransitive])]).

v(hertrouw,hertrouwt,hertrouwen,hertrouwd,hertrouwde,hertrouwden,
    [z([intransitive,
	pc_pp(met)])]).

v(herval,hervalt,hervallen,hervallen,herviel,hervielen,
    [z([intransitive,
	pc_pp(in)])]).

v(hervat,hervat,hervatten,hervat,hervatte,hervatten,
    [h([sbar,
	transitive,
        intransitive, % VL Aanvaller Pauwels hervatte met de reserven .
	vp])]).

v(herverdeel,herverdeelt,herverdelen,herverdeeld,herverdeelde,herverdeelden,
    [h([transitive])]).

v(herverzeker,herverzekert,herverzekeren,herverzekerd,herverzekerde,herverzekerden,
    [h([transitive,
	intransitive])]).

v(hervind,hervindt,hervinden,hervonden,hervond,hervonden,
    [h([refl,
	np_ld_pp,
	np_ld_adv,
	transitive])]).

v(hervorm,hervormt,hervormen,hervormd,hervormde,hervormden,
    [h([transitive,
	intransitive])]).

v(herwin,herwint,herwinnen,herwonnen,herwon,herwonnen,
    [h([%refl,
	transitive])]).

v(herzie,herziet,inflected(herzien,herziene),herzien,herzag,herzagen,
    [h([transitive])]).

v(heug,heugt,heugen,geheugd,heugde,heugden,
    [h([so_np,
        refl_np,  % ik kan me dat niet heugen
        refl_vp,  % ik kan me niet heugen ooit van Mark te hebben verloren
	refl_sbar % ik kan me niet heugen dat
       ])]).

v(heul,heult,heulen,geheuld,heulde,heulden,
    [h([pc_pp(met)])]).

v(hevel,hevelt,hevelen,geheveld,hevelde,hevelden,
    [h([part_transitive(over),
	part_np_ld_pp(over),
        part_np_np(over),
        np_ld_pp])]).

v(highlight,highlight,highlighten,gehighlight,highlightte,highlightten,
    [h([transitive,
	intransitive])
    ]).

v(hijg,hijgt,hijgen,gehijgd,hijgde,hijgden,
    [h([intransitive,
	sbar,
	pc_pp(naar),
	fixed([svp_pp(in,nek),dat],no_passive), % @ Hij moet weten dat wij hem in zijn nek hijgen .
	part_intransitive(uit),
	part_intransitive(na)])]).

v(hijs,hijst,hijsen,gehesen,hees,hesen,
    [h([intransitive,
	np_ld_dir,
	transitive,
        part_transitive(op),
        refl_pc_pp(in),  % jaarlijks hijsen zich honderden mensen in ...
	np_ld_pp])]).

v(hik,hikt,hikken,gehikt,hikte,hikten,
    [h([intransitive,
	part_pc_pp(aan,tegen),
	part_er_pp_sbar(aan,tegen)])]).

v(hinder,hindert,hinderen,gehinderd,hinderde,hinderden,
    [h([intransitive,
	sbar_subj_so_np,
        sbar_subj,  % dat Feyenoord de oefenwedstrijden verloor, hindert niet
	transitive,
	vp_subj_so_np])]).

v(hink,hinkt,hinken,gehinkt,hinkte,hinkten,
    [h([intransitive]),
     b([ld_pp,
        part_intransitive(aan),
	ld_dir])]).

v(hinkel,hinkelt,hinkelen,gehinkeld,hinkelde,hinkelden,
    [h([intransitive]),
     b([ld_pp,
	ld_dir])]).

v(hinnik,hinnikt,hinniken,gehinnikt,hinnikte,hinnikten,
    [h([intransitive])]).

v(hint,hint,hinten,gehint,hintte,hintten,
    [h([intransitive,
	pc_pp(op),
	sbar])]).

v(hip,hipt,hippen,gehipt,hipte,hipten,
    [h([intransitive,
	ld_pp,
	ld_dir])]).

v(hits,hitst,hitsen,gehitst,hitste,hitsten,
    [h([part_sbar_subj_so_np(op),
	part_transitive(op),
	part_vp_subj_so_np(op),
	part_np_pc_pp(op,tegen)])]).

v(hobbel,hobbelt,hobbelen,gehobbeld,hobbelde,hobbelden,
    [h([intransitive]),
     z([ld_dir,
	ld_pp,
        part_intransitive(aan)])]).

v(hockey,hockeyt,hockeyen,gehockeyd,hockeyde,hockeyden,
    [h([intransitive,
        part_intransitive(mee)])]).

v(hoed,hoedt,hoeden,gehoed,hoedde,hoedden,
    [h([transitive,
	pc_pp(voor),
	refl_pc_pp(voor),
	refl_er_pp_sbar(voor),
	refl_er_pp_vp(voor)])]).

%% zonder iets te hoeven missen
%% omdat hij niet hoeft na te blijven
%% ?omdat hij niet hoeft nablijven
%% *hij hoeft niet nablijven
%% ?omdat wij niet hoeven nablijven
%% *wij hoeven niet nablijven
v(hoef,hoeft,hoeven,[gehoefd,gehoeven],hoefde,hoefden,
    [h([intransitive,
	transitive,
        part_transitive(aan),	% we hoeven die kleding toch niet aan?
	part_intransitive(op),  % ik hoef nog niet op
	ld_dir,
        uit,
	ld_pp,
	ap_copula('op zoek'),
	np_ld_pp,        % we hoeven er niets/geen slagroom bij/in/op
	fixed([sbar_subj,acc(betoog)],no_passive),
        %% Het hoeft niet gezegd dat ..
        fixed([vc(zeg,psp,intransitive),sbar_subj_opt_het],no_passive),
	aux_pc_pp(te_inf,van),     % ik hoef niet te trainen van mijn trainer
	aux(te_inf)
       ])]).

v(hoepel,hoepelt,hoepelen,gehoepeld,hoepelde,hoepelden,
    [h([intransitive]),
     z([part_intransitive(op)])]).

v(hoest,hoest,hoesten,gehoest,hoestte,hoestten,
    [h([intransitive,
        mod_pp(doorheen),
	so_np_ld_pp,  % iemand in zijn gezicht hoesten
	part_transitive(op),
	part_transitive(uit),
	transitive])]).

v(hok,hokt,hokken,gehokt,hokte,hokten,
    [h([intransitive,
	transitive,
	part_transitive(op),  % als er vogelgriep heerst
	ld_pp,
	ld_adv
       ])]).

v(hol,holt,hollen,gehold,holde,holden,
    [z([ld_pp,
	ld_dir,
	part_intransitive(aan)]),
     h([intransitive,
	transitive,
	part_transitive(uit)])]).

v(honger,hongert,hongeren,gehongerd,hongerde,hongerden,
    [h([intransitive,
	part_intransitive(dood),
	%part_refl(dood),
	part_transitive(dood),
	part_transitive(uit),
	pc_pp(naar)])]).

v(honkbal,honkbalt,honkballen,gehonkbald,honkbalde,honkbalden,
    [h([intransitive])]).

v(honoreer,honoreert,honoreren,gehonoreerd,honoreerde,honoreerden,
    [h([sbar,
	transitive])]).

v(hoog,hoogt,hogen,gehoogd,hoogde,hoogden,
    [h([part_transitive(op)])]).

v(hooi,hooit,hooien,gehooid,hooide,hooiden,
    [h([intransitive])]).

v(hoon,hoont,honen,gehoond,hoonde,hoonden,
    [h([intransitive,
	part_transitive(uit),
	part_transitive(weg),
	dip_sbar,
	transitive])]).

v(hoop,hoopt,hopen,gehoopt,hoopte,hoopten,
    [h([tr_sbar,
	van_sbar,
	transitive,
	intransitive,		% je blijft hopen, he
	vp,			% omdat ik hoop hem daar te vinden
	subj_control(te),	% omdat ik hem daar hoop te vinden
	np_pc_pp(van),
	part_intransitive(op),
	part_refl(op),
	part_transitive(op),
        er_pp_sbar(op),
	pc_pp(op)
       ])]).

v(hoor,hoort,horen,gehoord,hoorde,hoorden,
    [h([aci,
	aci_no_obj,
	%np_np,  % ?
	intransitive,
	ld_adv,
	sbar,
        pp_sbar(van),
	transitive_ndev_ndev,
	ld_pp,
	pc_pp(van),  % daar heb ik nog nooit van gehoord
        part_pc_pp(op,van),	% daar hoor ik van op...
	np_pc_pp(in),	      % ik hoor er een duidelijke aarzeling in
	pc_pp(over),    % daar heb ik over gehoord
	np_pc_pp(over), % daar hoorden we niemand over
	np_pc_pp(van),
	pp_sbar_subj_no_het(bij), % hier hoort nog bij, dat ..
				% part_np_np(aan),
	part_transitive(terug),
	part_intransitive(toe),
	part_transitive(toe),
	part_intransitive(thuis),
	part_ld_pp(thuis),
	part_ld_adv(thuis),
	part_np_sbar(aan),
        part_sbar(aan), % we hebben moeten aanhoren dat
	part_transitive(aan),
	part_transitive(uit),
	part_np_er_pc_pp(af,aan),  % dat hoor je er niet aan af
	part_pp_sbar(af,aan),
	aux(te),          % behoren: omdat ik een vriend (be)hoor te zijn
	subj_control(te), % luister: omdat ik hoor (zeggen) een vriend te zijn
	                  %          omdat ik te horen kreeg een boef te zijn
	part_ld_adv(thuis)])]).

v(hop,hopt,hoppen,gehopt,hopte,hopten,
    [h([transitive,
        intransitive,
        ld_pp])]).

v(hort,hort,horten,gehort,hortte,hortten,
    [h([intransitive])]).

v(hos,host,hossen,gehost,hoste,hosten,
    [h([intransitive])]).

v(host,host,hosten,gehost,hoste,hosten,
    [h([transitive])]).

v(hots,hotst,hotsen,gehotst,hotste,hotsten,
    [h([intransitive])]).

v([houd,hou],houdt,houden,gehouden,hield,hielden,
    [b([part_intransitive(op),
	part_pc_pp(op,met),
	part_pc_pp(op,over)]),
     h([nonp_pred_np,
	np_aan_het,
	transitive,
	intransitive,  % ik moet nog zien of de lijm houdt
	np_ld_pp,
	np_ld_adv,
	np_ld_dir,  % we houden de beker omhoog
        np_np_ld_pp,  % ze hielden hem een brandende kaars onder de oksel
	np_pc_pp_refl(voor),
	np_pc_pp(aan),
	part_np_np(voor),
	voor_pred_np,
	voor_pred_np_sbar,
	voor_pred_np_vp,
	np_vp_obj1, % we zijn gehouden de begroting uit te voeren
	part_intransitive(aan),
	part_sbar(aan),
	part_intransitive(af),
	part_intransitive(halt),
	part_intransitive(huis),
	part_intransitive(in),
	part_intransitive(stand), % ook "geen stand houden"
	part_intransitive(stil),
	part_intransitive(vast),
	part_pc_pp(vast,aan),
	part_er_pp_sbar(vast,aan),
	part_intransitive(vol),
	part_np_sbar(voor),
        part_np_vp_obj(voor),
	part_transitive(vrij),
	part_refl(in),
	fixed([[schrap],refl],no_passive),
	part_refl(schuil),
	part_transitive(schuil),  % hij houdt de kinderen schuil
	part_refl(verborgen),     % daar houden zich Joden verborgen
	part_transitive(verborgen),  % hij houdt de papieren verborgen
	part_sbar_obj(stil),
	part_sbar_obj(tegen),
	part_sbar(tegen),  % we moeten tegenhouden dat ...
	part_sbar(bij),
	part_sbar(in),
	part_vp(in),
	part_sbar(stil),
	part_sbar(vol),
	part_sbar(voor),
	part_sbar_subj_so_np(op), % het houdt ons op dat we moeten komen
	part_transitive(aan),
	part_transitive(achter),
	part_transitive(af),
	part_transitive(bezig),
	part_transitive(bij),
	part_transitive(bijeen),
	part_transitive(binnen),
	part_transitive(geheim),
	part_transitive(hoog),  %omdat wij een naam hebben hoog te houden
	part_transitive(in),
	part_transitive(na),
	part_transitive(onder),
	part_transitive(op),
	part_transitive(open),
	part_transitive(over),
	part_intransitive(over),  % het houdt niet over
	part_transitive(schoon),
	part_transitive(stil),
	part_transitive(tegen),
        part_transitive(terug),
        part_transitive(toe),  % VL  ogen toehouden
	part_transitive(thuis),
	part_transitive(uit),
	part_transitive(vast),
	part_transitive(vol),
	part_transitive(voor),
        part_transitive(wakker),
	part_transitive(weg),
        part_vp(op),  % we zijn maar opgehouden op de deur te kloppen
	part_vp(vol),
	part_vp(voor),
	pc_pp(van),
	er_pp_vp(van),
	er_pp_sbar(van),
	%% spelling
	fixed([[boven,het,hoofd],{[acc(hand),dat]}],imp_passive),
	fixed([{[[boek],pc(van)]}],imp_passive),
	fixed([{[acc(contact),pc(met)]}],norm_passive),
        fixed([{[pc(aan),[de,hand]]}],imp_passive),
        fixed([er_pp(in),[de,moed]],imp_passive),
	fixed([{[acc(rekening),pc(met)]}],norm_passive),
	fixed([{[er_pp(met,A),acc(rekening)]},extra_sbar(A)],norm_passive),
	fixed([{[acc(verband),pc(met)]}],no_passive),
	fixed([pc(bij),het_obj1],no_passive),  % ik houd het bij ...
	fixed([er_pp(bij,A),het_obj1,extra_sbar(A)],no_passive), %% VL ik hou het erbij dat
	fixed([pc(op),het_obj1],no_passive),
	fixed([er_pp(op,A),het_obj1,extra_sbar(A)],no_passive), %% ik hou het erop dat
	fixed([er_pp(voor,A),het_obj1_pass,extra_sbar(A)],norm_passive),
	% Het wordt ervoor gehouden dat vervolgens de doorgang is versperd ... (VL?)
        fixed([[in,het,zeil],[een,oogje]],imp_passive),
        fixed([[in,het,zeil],[geen,oogje]],imp_passive),
        fixed([[gestand],dat],norm_passive),
        fixed([[gestand],dat_pp(aan)],norm_passive),
        fixed([[gestand],acc],norm_passive),
	fixed([[gezelschap],acc],norm_passive),
	fixed([{[acc(huis),pc(met)]}],norm_passive),
	fixed([[in,bedwang],acc],norm_passive),
	fixed([[in,de,gaten],acc],norm_passive),
	fixed([[in,de,gaten],sbar],norm_passive),
	fixed([[in,de,smiezen],acc],no_passive),
	fixed([[in,de,smiezen],sbar],no_passive),
        fixed([ap_pred('in het oog'),acc],no_passive),
	fixed([svp_pp(in,achterhoofd),acc],norm_passive),
	fixed([svp_pp(in,achterhoofd),sbar],norm_passive),
	fixed([svp_pp(in,gedachte),acc],norm_passive),
	fixed([svp_pp(in,gedachte),sbar],norm_passive),
	fixed([[in,ogenschouw],acc],norm_passive),
	fixed([[in,ogenschouw],sbar],norm_passive),
	fixed([svp_pp(in,greep),acc],no_passive),
	fixed([[in,petto],acc],norm_passive),
	fixed([[voor,ogen],acc],norm_passive),
	fixed([[voor,ogen],sbar],imp_passive),
	fixed([pp_pred(in,hand),acc],no_passive),
	fixed([[in,het,midden],acc],norm_passive), 
	fixed([[in,het,midden],sbar],imp_passive), 
	fixed([[in,stand],acc],norm_passive),
	part_transitive(instand),
	fixed([[in,toom],acc],norm_passive),
	fixed([[onder,de,duim],acc],norm_passive),
	fixed([ap_pred('op de been'),acc],norm_passive),
	fixed([[op,gang],acc],norm_passive),
	fixed([[overeind],acc],norm_passive),
	fixed([[overeind],sbar],norm_passive),
	fixed([[staande],acc],norm_passive),
	fixed([[staande],sbar],norm_passive),
	fixed([[te,vriend],acc],norm_passive),
	fixed([[tegoed],acc],no_passive),
	fixed([[tegoed],{[acc,pc(van)]}],no_passive),
	fixed([[te,goed],acc],no_passive),
	fixed([[te,goed],{[acc,pc(van)]}],no_passive),
	fixed([{[[toezicht],pc(op)]}],imp_passive),
	fixed([[van,het,lijf],acc,refl],no_passive),
	fixed([[van,de,domme],refl],no_passive),
	fixed([[voet,bij,stuk]],no_passive),
        fixed([[voor,gezien],acc],no_passive),
        fixed([[voor,het,lapje],acc],no_passive),
	fixed([pc(van),[verre],refl],no_passive),
	fixed([[ap_pred(gedeisd)],refl],no_passive), % houdt zich heel gedeisd
	fixed([[voor,de,gek],acc],norm_passive),
	part_fixed(na,[er_pp(op),acc],norm_passive),
	fixed([[eropna],acc],norm_passive),
%	fixed([[na],[op],{[acc,svp_er]}],no_passive),
				% we houden er bediendes op na
	                        % ze hielden dat er op na
	part_ld_pp(huis),
	part_ld_pp(vast),
	part_np_ld_pp(vast),
	part_np_pc_pp(aan,tot),
	part_np_pc_pp(af,van),
	part_np_pc_pp(bezig,met),
	part_np_pc_pp(open,voor),
	part_np_pc_pp(over,aan),
	part_pc_pp(over,aan), % daar houden we aan over
	part_np_pc_pp(over,van),
	part_np_pc_pp(terug,van),
	part_pc_pp(vol,tot),
	part_refl_ld_adv(op),
	part_refl_ld_pp(op),
	part_refl_pc_pp(bezig,met), % met die vraag hielden zich SU bezig
	part_refl_er_pp_vp(bezig,met),
	part_refl_pc_pp(op,met)]),
     b([part_vp(op)])]).

v(houw,houwt,houwen,gehouwen,hieuw,hieuwen,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv,
	np_pc_pp(uit),
	part_transitive(af),
	part_np_np(af),
	part_transitive(uit)])]).

v(huichel,huichelt,huichelen,gehuicheld,huichelde,huichelden,
    [h([intransitive,
	transitive])]).

v(huil,huilt,huilen,gehuild,huilde,huilden,
    [h([intransitive,
	transitive,		% krokodillentranen, tranen met tuiten
	sbar,			% dip
        mod_pp(om),
        pc_pp(van),   % een imago om van te huilen
	part_intransitive(uit),
	part_pc_pp(uit,op),   % een schouder om op uit te huilen
	part_transitive(uit)])]).

v(huis,huist,huizen,gehuisd,huisde,huisden,
    [h([ld_adv,
	ld_pp,
	intransitive,
	transitive
       ])]).

v(huisvest,huisvest,huisvesten,gehuisvest,huisvestte,huisvestten,
    [h([transitive,
	np_ld_pp])]).

v(huiver,huivert,huiveren,gehuiverd,huiverde,huiverden,
  [h([intransitive,
      pc_pp(van),
      pc_pp(voor)])]).

v(hul,hult,hullen,gehuld,hulde,hulden,
    [h([np_ld_pp
	% refl_ld_pp
       ])]).

v(huldig,huldigt,huldigen,gehuldigd,huldigde,huldigden,
    [h([transitive,
        part_transitive(in)])]).

v(hum,humt,hummen,gehumd,humde,humden,
    [h([intransitive,
	transitive])]).

v(hunker,hunkert,hunkeren,gehunkerd,hunkerde,hunkerden,
  [h([intransitive,
      er_pp_sbar(naar),
      er_pp_vp(naar),
      pc_pp(naar)])]).

v(hup,hupt,huppen,gehupt,hupte,hupten,
    [b([intransitive,
	ld_dir,
	ld_pp])]).

v(huppel,huppelt,huppelen,gehuppeld,huppelde,huppelden,
    [b([intransitive,
	part_intransitive(aan),
	ld_dir,
	ld_pp])]).

v(hurk,hurkt,hurken,gehurkt,hurkte,hurkten,
    [z([part_intransitive(neer)]),
     h([intransitive])]).

v(hussel,husselt,husselen,gehusseld,husselde,husselden,
    [h([fixed([[door,elkaar],acc],norm_passive),
	transitive,
	intransitive,
	np_ld_pp])]).

v(huur,huurt,huren,gehuurd,huurde,huurden,
    [h([transitive,
	intransitive, % heeft u een eigen huis, of huurt u?
	np_pc_pp(van),
	np_pc_pp(voor),
	part_transitive(af),
	part_transitive(in)])]).

v(huw,huwt,huwen,gehuwd,huwde,huwden,
    [z([intransitive,
	pc_pp(met)]),
     h([transitive])]).

v(huwelijk,huwelijkt,huwelijken,gehuwelijkt,huwelijkte,huwelijkten,
    [h([part_transitive(uit)])]).

v(hypnotiseer,hypnotiseert,hypnotiseren,gehypnotiseerd,hypnotiseerde,hypnotiseerden,
    [h([transitive,
	intransitive % de taal hypnotiseert
       ])]).

v(hypothekeer,hypothekeert,hypothekeren,gehypothekeerd,hypothekeerde,hypothekeerden,
    [h([transitive])]).

v(idealiseer,idealiseert,idealiseren,geïdealiseerd,idealiseerde,idealiseerden,
    [h([transitive])]).

v(identificeer,identificeert,identificeren,geïdentificeerd,identificeerde,identificeerden,
    [h([transitive,
	np_pc_pp(met),
        als_pred_np
	%refl_pc_pp(met)
       ])]).

v(ijk,ijkt,ijken,geijkt,ijkte,ijkten,
    [h([transitive])]).

v(ijl,ijlt,ijlen,geijld,ijlde,ijlden,
    [z([ld_pp,
	part_intransitive(aan)]),
     h([intransitive,
	part_intransitive(na)])]).

v(ijs,ijst,ijzen,geijsd,ijsde,ijsden,
    [h([pc_pp(van)])]).

v(ijsbeer,ijsbeert,ijsberen,geijsbeerd,ijsbeerde,ijsbeerden,
    [h([intransitive,
	ld_pp,
	ld_adv])]).

v(ijshockey,ijshockeyt,ijshockeyen,geijshockeyd,ijshockeyde,ijshockeyden,
    [h([intransitive,
        part_intransitive(mee)])]).

v(ijver,ijvert,ijveren,geijverd,ijverde,ijverden,
    [h([vp,
	pc_pp(tegen),
        pc_pp(met),
	pc_pp(voor),
	er_pp_vp(voor),
	er_pp_sbar(voor)
       ])]).

v(ijzel,ijzelt,ijzelen,geijzeld,ijzelde,ijzelden,
    [h([het_subj])]).  

v(illustreer,illustreert,illustreren,geïllustreerd,illustreerde,illustreerden,
    [h([transitive,
        intransitive,
	sbar])]).

v(imiteer,imiteert,imiteren,geïmiteerd,imiteerde,imiteerden,
    [h([sbar,
	transitive,
	intransitive
       ])]).

v(immuniseer,immuniseert,immuniseren,geïmmuniseerd,immuniseerde,immuniseerden,
    [h([transitive])]).

v(implanteer,implanteert,implanteren,geïmplanteerd,implanteerde,implanteerden,
    [h([transitive,
        np_np])]).

v(implementeer,implementeert,implementeren,geïmplementeerd,implementeerde,implementeerden,
    [h([transitive])]).

v(impliceer,impliceert,impliceren,geïmpliceerd,impliceerde,impliceerden,
    [h([sbar,
	transitive])]).

v(imponeer,imponeert,imponeren,geïmponeerd,imponeerde,imponeerden,
    [h([intransitive,
	sbar_subj_so_np,
	transitive,
	np_pc_pp(door),
	np_pc_pp(met),
	pc_pp(door),
	pc_pp(met)])]).

v(importeer,importeert,importeren,geïmporteerd,importeerde,importeerden,
  [h([transitive,
      intransitive])]).

v(improviseer,improviseert,improviseren,geïmproviseerd,improviseerde,improviseerden,
    [h([intransitive,
	transitive,
	dip_sbar,
	np_pc_pp(op),
	pc_pp(op)
       ])]).

v(in,int,innen,geïnd,inde,inden,
    [h([transitive,
	intransitive])]).

v(incasseer,incasseert,incasseren,geïncasseerd,incasseerde,incasseerden,
    [h([transitive,
	intransitive % je moet kunnen incasseren
       ])]).

v(incorporeer,incorporeert,incorporeren,geïncorporeerd,incorporeerde,incorporeerden,
    [h([transitive,
        np_ld_pp])]).

v(indiceer,indiceert,indiceren,geïndiceerd,indiceerde,indiceerden,
    [h([sbar,
	transitive,
	np_pc_pp(op)])]).

v(individualiseer,individualiseert,individualiseren,geïndividualiseerd,individualiseerde,individualiseerden,
    [h([intransitive,
	transitive])]).

v(indoctrineer,indoctrineert,indoctrineren,geïndoctrineerd,indoctrineerde,indoctrineerden,
    [h([intransitive,
	transitive])]).

v(induceer,induceert,induceren,geïnduceerd,induceerde,induceerden,
    [h([sbar,
	transitive])]).

v(industrialiseer,industrialiseert,industrialiseren,geïndustrialiseerd,industrialiseerde,industrialiseerden,
    [h([intransitive,
	transitive])]).

v(infecteer,infecteert,infecteren,geïnfecteerd,infecteerde,infecteerden,
    [h([transitive,
	np_pc_pp(met)])]).

v(infiltreer,infiltreert,infiltreren,geïnfiltreerd,infiltreerde,infiltreerden,
    [h([transitive]),
     b([intransitive,
	ld_pp])]).

v(informeer,informeert,informeren,geïnformeerd,informeerde,informeerden,
    [h([sbar,
        acc_np_sbar,
	transitive,
	intransitive,
	np_pc_pp(over),
	np_er_pp_sbar(over),
	pc_pp(naar)])]).

v(inhaleer,inhaleert,inhaleren,geïnhaleerd,inhaleerde,inhaleerden,
    [h([intransitive,
	transitive])]).

v(initieer,initieert,initiëren,geïnitieerd,initieerde,initieerden,
    [h([transitive,
	intransitive])]).

v(injecteer,injecteert,injecteren,geïnjecteerd,injecteerde,injecteerden,
    [h([transitive,
        intransitive,
	np_ld_pp])]).

v(inkasseer,inkasseert,inkasseren,geïnkasseerd,inkasseerde,inkasseerden,
    [h([transitive])]).

v(inkt,inkt,inkten,geïnkt,inkte,inkten,
    [h([transitive,
	intransitive])]).

v(innoveer,innoveert,innoveren,geïnnoveerd,innoveerde,innoveerden,
  [h([intransitive,
      transitive])]).

v(insinueer,insinueert,insinueren,geïnsinueerd,insinueerde,insinueerden,
  [h([sbar,
      intransitive,
      transitive,
      vp])]).

v(inspecteer,inspecteert,inspecteren,geïnspecteerd,inspecteerde,inspecteerden,
    [h([intransitive,
	sbar,
	transitive])]).

v(inspireer,inspireert,inspireren,geïnspireerd,inspireerde,inspireerden,
    [h([transitive,
	intransitive,
	np_vp_obj1,
	vp_no_control,
	np_pc_pp(op),
	np_pc_pp(tot),
	pc_pp(tot)])]).

v(installeer,installeert,installeren,geïnstalleerd,installeerde,installeerden,
    [h([als_pred_np,
	transitive,
	intransitive,
	np_ld_pp])]).

v(institutionaliseer,institutionaliseert,institutionaliseren,geïnstitutionaliseerd,institutionaliseerde,institutionaliseerden,
    [unacc([intransitive]),
     h([transitive])]).

v(instrueer,instrueert,instrueren,geïnstrueerd,instrueerde,instrueerden,
    [h([transitive,
	np_vp_obj1,
	vp_no_control
       ])]).

v(integreer,integreert,integreren,geïntegreerd,integreerde,integreerden,
    [z([intransitive,
	pc_pp(met)]),
     h([transitive,
	np_ld_pp,
	np_pc_pp(met)])]).

v(intensiveer,intensiveert,intensiveren,geïntensiveerd,intensiveerde,intensiveerden,
    [h([sbar_subj_np, % het intensiveert de contacten dat we naar recepties ..
	transitive]),
     z([intransitive])]).

v(interesseer,interesseert,interesseren,geïnteresseerd,interesseerde,interesseerden,
    [h([so_np,              % de problemen interesseren ons
	sbar_subj_so_np,    % het interesseert ons dat
	vp_subj_so_np,      %                      om
	transitive,         % ik ben niet geinteresseerd/wij hebben hem kunnen interesseren
	np_np,              % die zaken interesseren ons een heleboel
        np_pc_pp(in),       %                            daarin
	np_pc_pp(voor)      % ik interesseer de kinderen daarvoor
       ])]).

v(interneer,interneert,interneren,geïnterneerd,interneerde,interneerden,
    [h([transitive])]).

v(internet,internet,internetten,geïnternet,internette,internetten,
    [h([intransitive])]).

v(interpreteer,interpreteert,interpreteren,geïnterpreteerd,interpreteerde,interpreteerden,
    [h([nonp_pred_np,
	sbar,
	transitive,
	intransitive
       ])]).

v(interrumpeer,interrumpeert,interrumperen,geïnterrumpeerd,interrumpeerde,interrumpeerden,
    [h([transitive,
        intransitive,
        sbar %dip
       ])]).

v(intervenieer,intervenieert,interveniëren,geïntervenieerd,intervenieerde,intervenieerden,
    [h([intransitive])]).

v(interview,interviewt,interviewen,geïnterviewd,interviewde,interviewden,
    [h([transitive,
	np_mod_pp(over)
       ])]).

v(intimideer,intimideert,intimideren,geïntimideerd,intimideerde,intimideerden,
    [h([sbar_subj_so_np,
	intransitive,
	transitive])]).

v(intrigeer,intrigeert,intrigeren,geïntrigeerd,intrigeerde,intrigeerden,
    [h([intransitive,
	sbar_subj_so_np,
	transitive,
	vp_subj_so_np,
	pc_pp(tegen)])]).

v(introduceer,introduceert,introduceren,geïntroduceerd,introduceerde,introduceerden,
    [h([transitive,
	np_ld_pp])]).

v(inventariseer,inventariseert,inventariseren,geïnventariseerd,inventariseerde,inventariseerden,
    [h([sbar,
	transitive,
        intransitive])]).

v(investeer,investeert,investeren,geïnvesteerd,investeerde,investeerden,
    [h([transitive,
	intransitive,
	np_pc_pp(in),
	pc_pp(in)])]).

v(inviteer,inviteert,inviteren,geïnviteerd,inviteerde,inviteerden,
    [h([transitive])]).

v(ioniseer,ioniseert,ioniseren,geïoniseerd,ioniseerde,ioniseerden,
    [h([transitive])]).

v(ironiseer,ironiseert,ironiseren,geironiseerd,ironiseerde,ironiseerden,
    [h([intransitive,
	transitive])]).

v(irrigeer,irrigeert,irrigeren,geïrrigeerd,irrigeerde,irrigeerden,
    [h([intransitive,
	transitive])]).

v(irriteer,irriteert,irriteren,geïrriteerd,irriteerde,irriteerden,
    [h([intransitive,
	sbar_subj_so_np,
	transitive,
	vp_subj_so_np,
	refl_pc_pp(aan)])]).

v(isoleer,isoleert,isoleren,geïsoleerd,isoleerde,isoleerden,
    [h([intransitive,
        np_pc_pp(van),
	transitive])]).

v(jaag,jaagt,jagen,gejaagd,[joeg,jaagde],[joegen,jaagden],
    [h([intransitive,
	np_ld_dir,
	transitive,
	np_ld_adv,
	np_ld_pp,
	part_np_np(aan),
	part_sbar_subj_so_np(op),
	part_sbar_subj_so_np(weg),
 	part_transitive(aan),
 	part_transitive(achterna),
	part_transitive(af),
	part_transitive(na),
	part_transitive(op),
	part_transitive(weg),
	part_vp_subj_so_np(op),
	pc_pp(naar),
	pc_pp(op),
        pp_pred_np(op,vlucht),
	fixed([[op,de,kast],acc],no_passive),
	fixed([[in,het,harnas],pp_refl(tegen),acc],no_passive),
	fixed([[de,stuipen,op,het,lijf],dat],imp_passive),
	fixed([pc(over),acc(rilling),dat],norm_passive),
        np_np_ld_pp,  % ik jaag hem een kogel door het hoofd
	part_np_ld_pp(weg),
	part_np_ld_pp(weg),
	part_np_pc_pp(aan,tot)])]).

v(jakker,jakkert,jakkeren,gejakkerd,jakkerde,jakkerden,
    [h([intransitive,
        part_intransitive(af),
        part_transitive(af),
	ld_pp])]).

v(jam,jamt,jammen,gejamd,jamde,jamden,
    [h([intransitive])]).

v(jammer,jammert,jammeren,gejammerd,jammerde,jammerden,
    [h([intransitive,
	sbar, % dip
        transitive,
        part_transitive(uit),
	pc_pp(om),
	pc_pp(over)])]).

v(jank,jankt,janken,gejankt,jankte,jankten,
    [h([intransitive,
	sbar,
	pc_pp(om)])]).

v(jas,jast,jassen,gejast,jaste,jasten,
    [h([np_pc_pp(door),
	transitive])]).		% piepers

v(jat,jat,jatten,gejat,jatte,jatten,
    [h([intransitive,
	transitive])]).

v(jen,jent,jennen,gejend,jende,jenden,
    [h([intransitive,
	transitive])]).

v(jengel,jengelt,jengelen,gejengeld,jengelde,jengelden,
  [h([intransitive,
      dip_sbar,
      pc_pp(om)])]).

v(jeuk,jeukt,jeuken,gejeukt,jeukte,jeukten,
    [h([intransitive,
	so_np])]).

v(jodel,jodelt,jodelen,gejodeld,jodelde,jodelden,
    [h([intransitive,
	transitive])]).

v(joel,joelt,joelen,gejoeld,joelde,joelden,
    [h([intransitive,
        transitive,
	part_transitive(uit),
        sbar,  % dip
	pc_pp(van)])]).

v(jog,jogt,joggen,[gejogd,gejogt],[jogde,jogte],[jogden,jogten],
    [h([intransitive])]).

v(join,joint,joinen,gejoind,joinde,joinden,
    [h([intransitive,
	transitive])]).

v(jojo,jojoot,jojoën,gejojood,[jojode,jojoode],[jojoden,jojooden],
    [h([intransitive])]).

v(jok,jokt,jokken,gejokt,jokte,jokten,
    [h([intransitive,
        sbar
       ])]).

v(jong,jongt,jongen,gejongd,jongde,jongden,
    [h([intransitive])]).

v(jongleer,jongleert,jongleren,gejongleerde,jongleerde,jongleerden,
    [h([intransitive])]).

v(jouw,jouwt,jouwen,gejouwd,jouwde,jouwden,
    [h([part_transitive(uit),
        intransitive])]).  

v(jubel,jubelt,jubelen,gejubeld,jubelde,jubelden,
    [h([intransitive,
	sbar,  % mostly dip
	pc_pp(over)])]).

v(judo,jodoot,judoën,gejudood,judode,judoden,
    [h([intransitive])]).

v(juich,juicht,juichen,gejuicht,juichte,juichten,
    [h([intransitive,
	sbar,
        mod_pp(bij), % we staan er niet bij te juichen
        mod_pp(over), % we staan er niet over te juichen
	part_sbar_obj(toe),
	part_sbar(toe),
	part_transitive(toe)])]).

v(jureer,jureert,jureren,gejureerd,jureerde,jureerden,
    [h([intransitive,
	transitive])]).

v(juridiseer,juridiseert,juridiseren,gejuridiseerd,juridiseerde,juridiseerden,
    [h([intransitive])]).

v(jut,jut,jutten,gejut,jutte,jutten,
    [h([intransitive,
	part_transitive(op),
	part_np_vp_obj1(op)
       ])]).   

v(kaak,kaakt,kaken,gekaakt,kaakte,kaakten,
    [h([intransitive,
	transitive])]).

v(kaal,kaalt,kalen,gekaald,kaalde,kaalden,
    [h([intransitive,
	transitive])]).

v(kaap,kaapt,kapen,gekaapt,kaapte,kaapten,
    [h([intransitive,
	part_transitive(weg),
	transitive])]).

v(kaart,kaart,kaarten,gekaart,kaartte,kaartten,
    [h([intransitive,
 	part_transitive(aan),
	part_sbar(aan),
	part_transitive(af),
	pc_pp(met),
	part_intransitive(na),
	part_mod_pp(na,over),
	pc_pp(om)])]).

v(kaats,kaatst,kaatsen,gekaatst,kaatste,kaatsten,
    [z([part_intransitive(terug)]),
     h([intransitive,
	transitive,
	part_transitive(terug)])]).

v(kabbel,kabbelt,kabbelen,gekabbeld,kabbelde,kabbelden,
    [h([intransitive,
	ld_pp])]).

v(kachel,kachelt,kachelen,gekacheld,kachelde,kachelden,
    [b([ld_pp,
	ld_dir,
	part_intransitive(aan),
        intransitive])]).  

v(kader,kadert,kaderen,gekaderd,kaderde,kaderden,
    [h([part_transitive(af),
	part_transitive(in),
        ld_pp
       ])]).

v(kaffer,kaffert,kafferen,gekafferd,kafferde,kafferden,
  [h([part_transitive(uit),
      pc_pp(op)
     ])
  ]).

v(kak,kakt,kakken,gekakt,kakte,kakten,
    [h([intransitive]),
     z([part_intransitive(in),
	part_transitive(uit)])]).

v(kakel,kakelt,kakelen,gekakeld,kakelde,kakelden,
    [h([intransitive])]).

v(kalefater,kalefatert,kalefateren,gekalefaterd,kalefaterde,kalefaterden,
    [h([part_transitive(op)])]).

v(kalf,kalft,kalven,gekalfd,kalfde,kalfden,
    [unacc([part_intransitive(af)]),
     h([intransitive,
        part_transitive(af)])]).

v(kalk,kalkt,kalken,gekalkt,kalkte,kalkten,
    [h([ap_pred_np,
	transitive,
	np_ld_pp])]).

v(kalmeer,kalmeert,kalmeren,gekalmeerd,kalmeerde,kalmeerden,
    [unacc([intransitive]),
     h([sbar_subj_so_np,
	transitive])]).

v(kam,kamt,kammen,gekamd,kamde,kamden,
    [h([np_np,
	transitive,
	part_transitive(af),
	part_transitive(uit)])]).

v(kamp,kampt,kampen,gekampt,kampte,kampten,
    [h([intransitive,
        pc_pp(met)])]).

v(kampeer,kampeert,kamperen,gekampeerd,kampeerde,kampeerden,
    [h([intransitive])]).

v(kanaliseer,kanaliseert,kanaliseren,gekanaliseerd,kanaliseerde,kanaliseerden,
    [h([transitive])]).

v(kandideer,kandideert,kandideren,gekandideerd,kandideerde,kandideerden,
    [h([als_pred_np,
	transitive,
	intransitive])]).

v(kanker,kankert,kankeren,gekankerd,kankerde,kankerden,
    [h([intransitive,
	pc_pp(op),
	pc_pp(over)])]).

v(kano,kanoot,kanoën,gekanood,kanode,kanoden,
    [h([intransitive,
	ld_dir,
	ld_pp])]).

v(kant,kant,kanten,gekant,kantte,kantten,
    [h([transitive,
	refl_pc_pp(tegen)])]).

v(kantel,kantelt,kantelen,gekanteld,kantelde,kantelden,
    [unacc([intransitive]),
     h([transitive])]).

v(kap,kapt,kappen,gekapt,kapte,kapten,
    [h([intransitive,
	nonp_pred_np,
	transitive,
	part_transitive(af),
	part_acc_np_dip_sbar(af),
	part_transitive(uit),  % zijn tegenstander
	pc_pp(met)])]).

v(kapitaliseer,kapitaliseert,kapitaliseren,gekapitaliseerd,kapitaliseerde,
  kapitaliseerden,
    [h([intransitive,
	transitive])]).

v(kapittel,kapittelt,kapittelen,gekapitteld,kapittelde,kapittelden,
    [h([transitive])]).

v(kapseis,kapseist,kapseizen,gekapseisd,kapseisde,kapseisden,
    [b([intransitive])]).

v(kapsel,kapselt,kapselen,gekapseld,kapselde,kapselden,
    [h([part_transitive(in)])]).

v(karakteriseer,karakteriseert,karakteriseren,gekarakteriseerd,karakteriseerde,karakteriseerden,
    [h([als_pred_np,
	sbar_subj_np,
	transitive])]).

v(kastijd,kastijdt,kastijden,gekastijd,kastijdde,kastijdden,
    [h([transitive])]).  

v(kat,kat,katten,gekat,katte,katten,
  [h([pc_pp(op),
      np_pc_pp(af),
      part_transitive(om),
      intransitive])]).

v(kauw,kauwt,kauwen,gekauwd,kauwde,kauwden,
    [h([intransitive,
	pc_pp(op),
	transitive])]).

v(kayak,kayakt,kayakken,gekayakt,kayakte,kayakten,
    [z([ld_dir]),
     h([intransitive])]).

v(keel,keelt,kelen,gekeeld,keelde,keelden,
    [h([transitive])]).

v([keep,kiep],[keept,kiept],[keepen,kiepen],
  [gekeept,gekiept],[kiepte,keepte],[kiepten,keepten],
    [h([intransitive,
	transitive])]).

v(keer,keert,keren,gekeerd,keerde,keerden,
    [z([intransitive,
	ld_pp,
	ld_dir,
	part_intransitive(om),
	part_intransitive(terug),
	part_ld_pp(terug),
	part_intransitive(weer),
	part_transitive(af),
	part_ld_pp(terug)]),
     h([np_ld_dir,
	transitive,
	np_ld_pp,
	part_np_np(toe),  % ik keer hem de rug toe
	part_intransitive(uit),
	part_transitive(uit),
	part_np_np(uit),
	part_so_np_pass(uit),
	part_sbar_subj_np(om),
	part_so_pp_np(uit),
	part_transitive(om),
	refl_pc_pp(tegen),
	part_np_ld_pp(om),
	part_np_ld_pp(af)])]).

v(keet,keet,keten,gekeet,keette,keetten,
    [h([intransitive])]).

v(kef,keft,keffen,gekeft,kefte,keften,
    [h([intransitive])]).

v(kegel,kegelt,kegelen,gekegeld,kegelde,kegelden,
    [h([intransitive,
	part_transitive(om),
	np_ld_pp,
	np_ld_dir])]).

v(kelder,keldert,kelderen,gekelderd,kelderde,kelderden,
    [unacc([intransitive]),
     h([transitive])]).

v(ken,kent,kennen,gekend,kende,kenden,
    [h([transitive,
	fixed_dep(intransitive),   % hij geeft te kennen dat .. cf. geven
        als_pred_np,  % without np???
	np_pc_pp(in),
	np_pc_pp(van),  % ik ken hem van volleybal
	part_np_np(toe),
	part_so_pp_np(toe),
	part_transitive(toe)])]).

v(kenmerk,kenmerkt,kenmerken,gekenmerkt,kenmerkte,kenmerkten,
    [h([sbar_subj_np,
	transitive,
	vp_subj_np,
	refl_pc_pp(door),
        als_pred_refl])]).

v(kenschets,kenschetst,kenschetsen,gekenschetst,kenschetste,kenschetsten,
    [h([sbar_subj_np,
	transitive])]).

v(kenter,kentert,kenteren,gekenterd,kenterde,kenterden,
    [z([intransitive])]).

v(kerf,kerft,kerven,[gekorven,gekerfd],[korf,kerfde],[korven,kerfden],
    [z([intransitive,
	ld_pp]),
     h([transitive,
        part_transitive(in)])]).

v(kerk,kerkt,kerken,gekerkte,kerkte,kerkten,
    [h([intransitive])]).

v(kerm,kermt,kermen,gekermd,kermde,kermden,
    [h([intransitive,
	sbar,
	transitive,
	pc_pp(over)])]).

v(kersten,kerstent,kerstenen,gekerstend,kerstende,kerstenden,
    [h([transitive])]).

v(keten,ketent,ketenen,geketend,ketende,ketenden,
    [h([transitive,
	part_transitive(vast),
	part_np_ld_pp(vast),
	np_ld_pp])]).

v(kets,ketst,ketsen,geketst,ketste,ketsten,
    [z([part_intransitive(af),
	part_pc_pp(af,op)]),
     h([transitive,
	part_transitive(af)]),
     b([intransitive])]).

v(keur,keurt,keuren,gekeurd,keurde,keurden,
    [h([transitive,
	intransitive,
        fixed([[waardig],acc(blik),dat],norm_passive),
	part_transitive(af),
        part_sbar_obj(af),
        part_sbar(af),
	part_sbar_obj(goed),
	part_sbar(goed),
	part_transitive(goed)])]).

v(keurmerk,keurmerkt,keurmerken,gekeurmerkt,keurmerkte,keurmerkten,
    [h([intransitive,
	transitive])]).

v(keuvel,keuvelt,keuvelen,gekeuveld,keuvelde,keuvelden,
    [h([intransitive,
	pc_pp(over),
	er_pp_sbar(over)])]).

v(kibbel,kibbelt,kibbelen,gekibbeld,kibbelde,kibbelden,
    [h([intransitive,
	pc_pp(over),
	er_pp_sbar(over)])]).

v(kick,kickt,kicken,gekickt,kickte,kickten,
    [z([part_intransitive(af),
	part_pc_pp(af,van)]),
     h([intransitive,
	transitive,  % in games op internet, blijkbaar
	pc_pp(op)])]).

v(kidnap,kidnapt,kidnappen,gekidnapt,kidnapte,kidnapten,
    [h([transitive])]).

v(kiek,kiekt,kieken,gekiekt,kiekte,kiekten,
    [h([intransitive,
	transitive])]).

v(kiem,kiemt,kiemen,gekiemd,kiemde,kiemden,
    [unacc([intransitive])]).

v(kien,kient,kienen,gekiend,kiende,kienden,
    [h([part_transitive(uit),
	part_sbar(uit),
	intransitive])]).

v(kiep,kiept,kiepen,gekiept,kiepte,kiepten,
    [z([part_intransitive(om)]),
     h([np_ld_pp,
	part_transitive(leeg),
	part_transitive(om)])]).

v(kieper,kiepert,kieperen,gekieperd,kieperde,kieperden,
    [unacc([intransitive,
	ld_pp]),
     h([np_ld_dir,
	part_transitive(om),
	np_ld_pp])]).

v(kier,kiert,kieren,gekierd,kierde,kierden,
    [h([intransitive])]).

v(kies,kiest,kiezen,gekozen,koos,kozen,
    [h([als_pred_np,
	intransitive, % u mag kiezen; je moet kiezen
	transitive,
	sbar,   % je mag kiezen of je mee wilt
	np_ld_pp,   % we kiezen hem in de regering /
        np_pc_pp(tot),
	np_pc_pp(uit),
	np_mod_pp(voor),
	part_transitive(uit),
	pc_pp(tussen),
	pc_pp(uit),
	pc_pp(voor),
	er_pp_vp(voor),
        vp, % gekozen is hem te lozen
	fixed([{[[partij],pc(voor)]}],imp_passive),
	fixed([[het,hazepad]],no_passive),
	fixed([[partij]],imp_passive),
	part_np_pc_pp(uit,voor)])]).

v(kietel,kietelt,kietelen,gekieteld,kietelde,kietelden,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv])]).

v(kijf,kijft,kijven,[gekijfd,gekeven],[kijfde,keef],[kijfden,keven],
    [h([intransitive])]).

v(kijk,kijkt,kijken,gekeken,keek,keken,
  [z([	part_pc_pp(uit,op)     % omdat we daar snel op zijn uitgekeken (order suggests this is a VC)
     ]),
   h([  intransitive,
        sbar,
	transitive,
	ld_pp,
	ld_dir,  % hij kijkt de weg af
	np_ld_pp,		% haar in de ogen / hem in de rug
	np_np_ld_pp,  % Ze keken je de kleren van het lijf .
	mod_pp(bij),
	part_np_np(af),
	part_intransitive(af),
	part_ld_pp(af),
        part_intransitive(mee),
	part_ld_pp(mee),
	part_intransitive(om),
	part_intransitive(op),
	part_intransitive(rond),
	part_intransitive(terug),
	part_transitive(terug),  % we kijken de opnames terug
	part_intransitive(toe),
        part_intransitive(vooruit),
	part_ld_pp(in),
	part_ld_pp(op),
	part_sbar(na),
	part_sbar(toe),
	part_sbar(uit),
	part_vp(uit),
	part_mod_pp(uit,met),       % daar moet je mee uitkijken
	part_intransitive(aan),	% kijk eens aan ,
	part_intransitive(weg),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(in),
	part_transitive(na),
	part_so_np(tegemoet),
	part_fixed(uit,[acc(oog)],no_passive),
	part_fixed(uit,[acc(oog),refl],no_passive),
	fixed([[koffiedik]],no_passive),
	fixed([[televisie]],imp_passive),
	fixed([[tv]],imp_passive),
	fixed([[op,of,om]],imp_passive),
	part_pc_pp_refl(uit,voor),  % hij kijkt voor zich uit
	part_pc_pp_refl(heen,voor),  % hij kijkt voor zich heen; ouderwets
	part_pc_pp(uit,naar),
	part_er_pp_sbar(uit,naar),  % ik kijk er naar uit dat ze promoveren
	part_er_pp_sbar(toe,op),    % we moeten er op toekijken dat   VL
	part_ld_pp(aan),
	part_ld_pp(neer),
	part_ld_pp(om),
	part_ld_pp(rond),
	part_ld_pp(terug),
	part_np_pc_pp(aan,op),
        part_np_er_pp_sbar(aan,op),
	part_np_pc_pp(na,op),
	part_pc_pp(op,van),
        part_pc_pp(toe,op),
	part_er_pp_sbar(op,van),
	part_sbar(op)  % dan moet men niet raar opkijken dat hij komt
       ]),
     b([part_intransitive(uit),
	part_ld_pp(uit)])]).

v(kik,kikt,kikken,gekikt,kikte,kikten,
    [h([intransitive])]).

v(kikker,kikkert,kikkeren,gekikkerd,kikkerde,kikkerden,
  [h([part_transitive(op)]),
   unacc([part_intransitive(op),
	  part_pc_pp(op,van)])
  ]).

v(kill,killt,killen,gekilld,killde,killden,
  [h([transitive])]).

v(kir,kirt,kirren,gekird,kirde,kirden,
  [h([intransitive,
      acc_np_dip_sbar,
      sbar])]).

v(kist,kist,kisten,gekist,kistte,kistten,
  [h([transitive])]).

v(kit,kit,kitten,gekit,kitte,kitten,
  [h([part_transitive(af),
      part_transitive(dicht),
      part_transitive(vast),
      transitive,
      intransitive])]).

v(klaag,klaagt,klagen,geklaagd,[klaagde,kloeg],[klaagden,kloegen],
    [h([intransitive,
	sbar,
	transitive,
	pc_pp(over),
	er_pp_sbar(over),
	er_pp_vp(over),
	part_transitive(aan),
        part_sbar(aan),
	part_intransitive(aan)])]).

v(klaar,klaart,klaren,geklaard,klaarde,klaarden,
    [unacc([part_intransitive(op)]),
     h([intransitive,
	transitive,
        part_transitive(in), % douane
        part_transitive(uit), % VL
	part_transitive(op)])]).

v(klad,kladt,kladden,geklad,kladde,kladden,
  [h([transitive,
      intransitive,
      part_transitive(onder)])]).

v(klak,klakt,klakken,geklakt,klakte,klakten,
    [h([intransitive,
	pc_pp(met)])]).

v(klamp,klampt,klampen,geklampt,klampte,klampten,
    [h([transitive,
	ld_pp,
	np_ld_pp,
	part_intransitive(aan),
	part_transitive(aan),
	part_np_ld_pp(vast),
	part_transitive(vast)
       ])]).

v(klap,klapt,klappen,geklapt,klapte,klapten,
    [unacc([part_intransitive(dicht),
	    part_intransitive(neer),
	    part_intransitive(in),
	    part_intransitive(om),
	    part_intransitive(open),
	    part_intransitive(uit),
            ld_dir,
	    ld_pp]),
     h([intransitive,
	part_transitive(om),
	fixed([[stuk],acc(hand)],no_passive),
	fixed([[stuk],acc(hand),refl],no_passive),
	part_transitive(in),
	part_transitive(dicht),
	part_transitive(neer),
        part_transitive(open),
	part_transitive(over),
	part_transitive(om),
	part_transitive(op),
	part_transitive(toe),
	part_transitive(uit),
	part_transitive(weg),
	pc_pp(met)])]).

v(klapper,klappert,klapperen,geklapperd,klapperde,klapperden,
    [h([intransitive,
	pc_pp(met)])]).

v(klappertand,klappertandt,klappertanden,geklappertand,klappertandde,klappertandden,
    [h([intransitive])]).

v(klapwiek,klapwiekt,klapwieken,geklapwiekt,klapwiekte,klapwiekten,
    [h([intransitive])]).

v(klasseer,klasseert,klasseren,geklasseerd,klasseerde,klasseerden,
    [h([refl,
	transitive,
	refl_pc_pp(voor)])]).

v(klater,klatert,klateren,geklaterd,klaterde,klaterden,
    [h([intransitive,
	part_intransitive(op)])]).

v(klauter,klautert,klauteren,geklauterd,klauterde,klauterden,
    [z([ld_pp,
	ld_dir,
	part_intransitive(aan)]),
     b([intransitive])]).

v(klauw,klauwt,klauwen,geklauwd,klauwde,klauwden,
    [h([intransitive,
	refl_ld_dir,
	transitive]),
     z([ld_pp
       ])]).

v(klaverjas,klaverjast,klaverjassen,geklaverjast,klaverjaste,klaverjasten,
    [h([intransitive])]).

v(kleed,kleedt,kleden,gekleed,kleedde,kleedden,
    [h([intransitive,
	%refl,
	transitive,
	np_ld_pp,
	%part_refl(om),
        part_intransitive(af),
	fixed([[in,het,nieuw],acc],norm_passive),
	part_transitive(aan),
	part_transitive(in),
	part_transitive(om),
	part_transitive(uit)
	%refl_pc_pp(voor)
       ])]).

v(kleef,kleeft,kleven,gekleefd,kleefde,kleefden,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv,
	part_intransitive(aan),
	part_so_np(aan)])]).

v(klei,kleit,kleien,gekleid,kleide,kleiden,
    [h([intransitive,
	transitive])]).

v(kleineer,kleineert,kleineren,gekleineerd,kleineerde,kleineerden,
    [h([transitive])]).

v(klem,klemt,klemmen,geklemd,klemde,klemden,
    [h([intransitive,
	part_transitive(af),
	np_ld_pp,
	part_transitive(in),
	part_transitive(opeen),
	part_transitive(vast),
        part_np_ld_pp(vast),
	part_np_ld_pp(in)])]).

v(klep,klept,kleppen,geklept,klepte,klepten,
    [h([intransitive,
	transitive, % de bel oid
	pc_pp(met)])]).

v(klepper,kleppert,klepperen,geklepperd,klepperde,klepperden,
    [h([intransitive,
	pc_pp(met)])]).

v(klets,kletst,kletsen,gekletst,kletste,kletsten,
    [z([ld_pp]),
     h([intransitive,
	part_intransitive(af),
	part_intransitive(bij),
	part_transitive(na),
        part_pc_pp(aan,tegen),
	np_ld_pp,
	np_ld_adv,
	mod_pp(met),
	pc_pp(over)])]).

v(kletter,klettert,kletteren,gekletterd,kletterde,kletterden,
    [z([ld_pp,
        ld_dir  % de trap af
       ]),
     h([intransitive,
	part_intransitive(neer),
	np_ld_pp,
	np_ld_adv
       ])]).

v(kleum,kleumt,kleumen,gekleumd,kleumde,kleumden,
    [h([intransitive])]).

v(kleun,kleunt,kleunen,gekleund,kleunde,kleunden,
    [h([intransitive,
        fixed([er_pp(in)],imp_passive),% er keihard in kleunen?
        part_intransitive(mis)])]).

v(kleur,kleurt,kleuren,gekleurd,kleurde,kleurden,
    [z([ap_copula]),  % het stadion kleurde rood
     h([ap_pred_np,
	intransitive,
	transitive,
        part_transitive(bij),
	part_transitive(in)])]).

v(klief,klieft,klieven,gekliefd,kliefde,kliefden,
    [h([intransitive,
	transitive])]).

v(klier,kliert,klieren,geklierd,klierde,klierden,
    [h([intransitive,
	transitive])]).

v(klik,klikt,klikken,geklikt,klikte,klikten,
    [h([intransitive,
	transitive,
	part_transitive(aan),
	pc_pp(op),
	pc_pp(met),
	pc_pp(over),
	pc_pp(tussen)])]).

v(klim,klimt,klimmen,geklommen,klom,klommen,
    [z([ld_pp,
	ld_dir,
	meas,
	part_intransitive(op),
	part_transitive(op),
	part_ld_pp(op)]),
     b([intransitive])]).

v(klingel,klingelt,klingelen,geklingeld,klingelde,klingelden,
  [h([intransitive,
      transitive		% zijn belletje
     ])]).

v(klink,klinkt,klinken,geklonken,klonk,klonken,
  [z([part_intransitive(in)]),
   h([intransitive,
      transitive,
      nonp_copula,	      % dat klinkt aannemelijk; als onzin
      so_nonp_copula,	      % dat klink mij onaannemelijk in de oren
      mod_pp(doorheen),
      part_ld_pp(door),
      part_transitive(door),
      part_intransitive(na),
      pc_pp(op),		     % daar moet op geklonken worden
      part_transitive(vast),	     % spijkeren
      part_np_ld_pp(vast),	     % ,,
      part_sbar_subj_no_het(door),   % toen klonk al door dat ...
      sbar,			     % dip
      alsof_sbar])]).

v(klis,klist,klissen,geklist,kliste,klisten,
    [h([transitive])]).

v(klit,klit,klitten,geklit,klitte,klitten,
    [b([intransitive,
	part_intransitive(samen),
	pc_pp(bij),
	part_intransitive(vast),
	part_refl(vast)])]).

v(klok,klokt,klokken,geklokt,klokte,klokten,
    [h([intransitive,
        part_intransitive(af),
        part_transitive(af),
        part_np_pc_pp(af,op),
	transitive])]).

v(kloneer,kloneert,kloneren,gekloneerd,kloneerde,kloneerden,
    [h([intransitive,
	transitive])]).

v(klonter,klontert,klonteren,geklonterd,klonterde,klonterden,
    [b([intransitive,
        part_intransitive(samen)])]).

v(kloof,klooft,kloven,gekloofd,kloofde,kloofden,
    [h([transitive,
        intransitive])]).

v(klooi,klooit,klooien,geklooid,klooide,klooiden,
    [h([intransitive])]).

v(kloon,kloont,klonen,gekloond,kloonde,kloonden,
    [h([intransitive,
	transitive])]).

v(kloot,kloot,kloten,gekloot,klootte,klootten,
    [h([intransitive])]).

v(klop,klopt,kloppen,geklopt,klopte,klopten,
    [h([ap_pred_np,
	intransitive,
	sbar_subj,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_np_ld_pp,  % de regering probeert alleen de burgers geld uit de zak te kloppen
	np_ld_adv,
	part_intransitive(aan),
	part_transitive(aan),
	part_intransitive(af),
	part_transitive(af),
	part_transitive(in),  % VL: de ruit inkloppen
	part_transitive(op),
        part_transitive(uit),
	fixed([[op,de,borst],refl],no_passive),
	fixed([[op,de,borst],refl,sbar],no_passive),
	pc_pp(van),  % er klopt geen zak van
	pc_pp(met),
	part_ld_pp(aan)])]).

v(klos,klost,klossen,geklost,kloste,klosten,
    [h([transitive]),
     b([intransitive])]).

v(klots,klotst,klotsen,geklotst,klotste,klotsten,
    [z([ld_pp,
	ld_dir]),
     h([intransitive
       ])]).

v(kluif,kluift,kluiven,gekloven,kloof,kloven,
    [h([part_transitive(af),
	pc_pp(aan),
	transitive,
	intransitive])]).

v(kluister,kluistert,kluisteren,gekluisterd,kluisterde,kluisterden,
    [h([transitive,
	np_ld_pp])]).

v(klungel,klungelt,klungelen,geklungeld,klungelde,klungelden,
    [h([intransitive])]).  

v(klus,klust,klussen,geklust,kluste,klusten,
    [h([intransitive,
	part_intransitive(bij), % hij klust wat bij
	er_pc_pp(bij)  % we klussen er wat bij
       ])]).

v(kluun,kluunt,klunen,gekluund,kluunde,kluunden,
  [h([intransitive]),
   b([ld_pp])
  ]).

v(knaag,knaagt,knagen,geknaagd,knaagde,knaagden,
    [h([intransitive,
	transitive,
        part_transitive(aan),
	ap_pred_np,
	pc_pp(aan),  % dat knaagde aan hem
	fixed([pc(aan),sbar_subj_opt_het],no_passive),
	        % het knaagde aan hem dat ...
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv])]).

v(knabbel,knabbelt,knabbelen,geknabbeld,knabbelde,knabbelden,
    [h([intransitive,
	transitive,
        part_transitive(af),
	part_np_pc_pp(af,van),
	ld_pp,
	ld_adv])]).

v(knak,knakt,knakken,geknakt,knakte,knakten,
    [unacc([intransitive,
            part_intransitive(af)]),
     h([transitive])]).

v(knal,knalt,knallen,geknald,knalde,knalden,
    [h([np_ld_pp,
        np_ld_dir,
	part_intransitive(mis),
	part_intransitive(raak),
	part_intransitive(naast),
	part_intransitive(over),
	part_transitive(mis),
	part_transitive(af),
        part_intransitive(binnen),
        part_transitive(binnen),
	part_transitive(dicht),  % de deur
	part_transitive(neer),
	part_transitive(naast),
	part_transitive(over),
	part_transitive(raak)]),
     b([ld_pp,
        np_ld_pp,
        ap_copula('uit elkaar'),
        ap_copula('in elkaar'),
        ap_copula('uit mekaar'),
        ap_copula('in mekaar'),
	intransitive])]).

v(knap,knapt,knappen,geknapt,knapte,knapten,
    [z([part_intransitive(af),
	part_intransitive(op),
	part_pc_pp(af,op),
	part_pc_pp(op,van)]),
     h([% part_refl(op),
	part_transitive(af),
	part_transitive(op),
	fixed([[een,uiltje]],imp_passive),
	pc_pp(door),  % door midden?
	part_np_pc_pp(op,met)]),
     b([intransitive])]).

v(knapper,knappert,knapperen,geknapperd,knapperde,knapperden,
    [h([intransitive])]).

v(knars,knarst,knarsen,geknarst,knarste,knarsten,
    [h([intransitive])]).

v(knarsetand,knarsetandt,knarsetanden,geknarsetand,knarsetandde,knarsetandden,
    [h([intransitive,
        sbar])]).

v(knecht,knechten,knechten,geknecht,knechtte,knechtten,
    [h([transitive])]).

v(kneed,kneedt,kneden,gekneed,kneedde,kneedden,
    [h([transitive,
        part_transitive(door),
	intransitive,   % in recepten: kneed totdat ..
	part_intransitive(door),     % kneed door tot een soepel ...
        np_pc_pp(van)])]).

v(knel,knelt,knellen,gekneld,knelde,knelden,
    [h([intransitive,
	transitive,
	part_transitive(af),
	np_ld_pp])]).

v(knerp,knerpt,knerpen,geknerpt,knerpte,knerpten,
    [h([intransitive])]).

v(knetter,knettert,knetteren,geknetterd,knetterde,knetterden,
    [h([intransitive])]).

v(kneus,kneust,kneuzen,gekneusd,kneusde,kneusden,
    [h([transitive])]).

v(knevel,knevelt,knevelen,gekneveld,knevelde,knevelden,
    [h([transitive])]).

v(kniel,knielt,knielen,geknield,knielde,knielden,
    [h([part_intransitive(neer),
	pc_pp(voor),
	part_ld_pp(neer)]),
     b([intransitive])]).

v(knies,kniest,kniezen,gekniesd,kniesde,kniesden,
    [h([intransitive])]).

v(knijp,knijpt,knijpen,geknepen,kneep,knepen,
    [b([ld_pp,
        ld_adv]),
     h([intransitive,
	transitive,
	np_ld_pp,
	np_ld_adv,
	part_fixed(dicht,[sbar,acc(hand)],no_passive),
	part_fixed(dicht,[sbar,acc(hand_DIM)],no_passive),
	fixed([pp_pred(in,hand),refl,sbar],no_passive),
	fixed([pp_pred(in,hand),sbar],no_passive),
	fixed([svp_pp(in,hand_DIM),refl,sbar],no_passive),
	fixed([svp_pp(in,hand_DIM),sbar],no_passive),
	part_np_np(toe),
	part_transitive(af),
	part_transitive(dicht),
        part_transitive(in),
        part_transitive(leeg),
	part_transitive(samen),
	part_transitive(uit),
	part_transitive(toe),
	pc_pp(op)])]).

v(knik,knikt,knikken,geknikt,knikte,knikten,
    [h([transitive,
        part_transitive(toe),
        sbar,
        van_sbar  % hij knikte van wel
       ]),
     b([intransitive])]).

v(knikkebol,knikkebolt,knikkebollen,geknikkebold,knikkebolde,knikkebolden,
    [h([intransitive])]).

v(knikker,knikkert,knikkeren,geknikkerd,knikkerde,knikkerden,
    [h([intransitive,
        np_ld_pp,
	np_ld_dir])]).

v(knip,knipt,knippen,geknipt,knipte,knipten,
    [h([ap_pred_np,
	intransitive,
	transitive,
	np_pc_pp(uit),
	np_pc_pp(van),
	part_transitive(aan), % het licht
	part_transitive(op),
	part_transitive(af),
	part_transitive(door),
	part_transitive(kaal),
	part_transitive(uit),
	pc_pp(met),
	mod_pp(in),  % er is in de uitzending geknipt
	part_np_ld_pp(af)])]).

v(knipoog,knipoogt,knipogen,geknipoogd,knipoogde,knipoogden,
    [h([intransitive])]).

v(knipper,knippert,knipperen,geknipperd,knipperde,knipperden,
  [h([intransitive,
      part_transitive(weg),  % Riddle: ze knipperde haar tranen weg
	pc_pp(met),
	pc_pp(tegen)])]).

v(knisper,knispert,knisperen,geknisperd,knisperde,knisperden,
    [h([intransitive])]).

v(knoei,knoeit,knoeien,geknoeid,knoeide,knoeiden,
    [h([intransitive,
	pc_pp(met),
        transitive])]).

v(knok,knokt,knokken,geknokt,knokte,knokten,
    [h([intransitive,
	refl_ld_dir,
        part_intransitive(af),  % er werd veel afgeknokt
	part_transitive(uit),
	part_refl(terug),  % hij knokte zich terug
        part_intransitive(terug), % ik heb teruggeknokt, en...
	refl_ld_pp,        % hij knokte zich naar de overwinning
	transitive,        % een stevig robbertje
	pc_pp(om),
	pc_pp(tegen),
	pc_pp(voor)])]).

v(knoop,knoopt,knopen,geknoopt,knoopte,knoopten,
    [h([transitive,
	np_ld_pp,
	part_transitive(aan),
	part_transitive(af),
	part_transitive(dicht),
	part_transitive(los),
	part_transitive(vast),
	part_fixed(vast,[pc(aan),acc(touw)],norm_passive),
	part_fixed(aan,[pc(met),acc],norm_passive),
	part_pc_pp(aan,met),
	fixed([svp_pp(in,oor),acc],norm_passive),
	fixed([svp_pp(in,oor),refl,acc],norm_passive),
	fixed([svp_pp(in,oor),sbar],norm_passive),
	fixed([svp_pp(in,oor),refl,sbar],norm_passive),
	fixed([[aan,elkaar],acc(eindje)],norm_passive),  % de eindjes moeten aan elkaar worden geknoopt
	part_np_ld_pp(vast),
	part_pc_pp(aan,bij)])]).

v(knor,knort,knorren,geknord,knorde,knorden,
    [h([intransitive,
	sbar,
	pc_pp(op),
	pc_pp(over),
	pc_pp(tegen)])]).

v(knot,knot,knotten,geknot,knotte,knotten,
    [h([transitive  % bomen
       ])]).

v(knuffel,knuffelt,knuffelen,geknuffeld,knuffelde,knuffelden,
    [h([intransitive,
	transitive,
	ld_dir,  % zij knuppelen ons de bus in
	pc_pp(met)])]).

v(knuppel,knuppelt,knuppelen,geknuppeld,knuppelde,knuppelden,
    [h([part_transitive(dood),
	transitive  % homeruns
       ])]).

v(knutsel,knutselt,knutselen,geknutseld,knutselde,knutselden,
    [h([intransitive,
	transitive,
	pc_pp(aan)])]).

v(kodeer,kodeert,koderen,gekodeerd,kodeerde,kodeerden,
    [h([intransitive,
	transitive])]).

v(koek,koekt,koeken,gekoekt,koekte,koeten,
  [b([part_intransitive(aan)])]).

v(koel,koelt,koelen,gekoeld,koelde,koelden,
    [unacc([part_intransitive(af)]),
     h([transitive,
	np_pc_pp(met),
	np_pc_pp(op),
	part_transitive(af)]),
     b([intransitive])]).

v(koer,koert,koeren,gekoerd,koerde,koerden,
  [h([intransitive,
      dip_sbar
     ])]).

v(koers,koerst,koersen,gekoerst,koerste,koersten,
    [b([intransitive,
	transitive,  % een heldere lijn
        part_ld_pp(aan),
	part_ld_pp(af),
	part_intransitive(aan),
	ld_dir,
	ld_pp])]).

v(koester,koestert,koesteren,gekoesterd,koesterde,koesterden,
    [h([%refl,
	transitive,
	refl_ld_pp])]).

v(kogel,kogelt,kogelen,gekogeld,kogelde,kogelden,
    [h([part_intransitive(raak),
	part_intransitive(mis),
	part_transitive(raak),
	part_transitive(mis),
	np_ld_pp
       ])]).


v(koketteer,koketteert,koketteren,gekoketteerd,koketteerde,koketteerden,
    [h([pc_pp(met),
	er_pp_sbar(met),
	er_pp_vp(met),
	intransitive])]).

v(kokhals,kokhalst,kokhalzen,gekokhalsd,kokhalsde,kokhalsden,
    [h([intransitive])]).

v(kokkerel,kokkerelt,kokkerellen,gekokkereld,kokkerelde,kokkerelden,
    [h([intransitive])]).

v(kolf,kolft,kolven,gekolfd,kolfde,kolfden,
    [h([intransitive,
        transitive,
        part_intransitive(af),
        part_transitive(af)])]).

v(kolk,kolkt,kolken,gekolkt,kolkte,kolkten,
  [h([intransitive,
      ld_pp,
      ld_dir
     ])]).

v(koloniseer,koloniseert,koloniseren,gekoloniseerd,koloniseerde,koloniseerden,
    [h([intransitive,
	transitive])]).

v(kom,komt,komen,gekomen,kwam,kwamen,
  [z([part_so_np_pass(tegemoet),
      part_so_pp(tegemoet),
      part_transitive(na),
      part_intransitive(overeen),
      part_transitive(overeen),
      part_pc_pp(overeen,in),
      part_pc_pp(overeen,met),
      part_np_pc_pp(overeen,met),
      part_sbar_tpart(overeen),
      part_vp_tpart(overeen),
      pc_pp(aan),		% hoe kom je aan wormen?
                                % een eind(e) komen aan
                                 % hij komt er eindelijk aan ==> er aan LD?
      
      pc_pp(achter),		% hoe kom je achter de waarheid
      pc_pp(door)		% dat kwam door jullie

      ]),
   unacc([intransitive,			% ik kom!
	  ld_adv,			% ik kwam thuis
	  ld_dir,			% ik kwam het huis in
	  sbar_subj,			% hoe komt het, dat ..
	  aux(te_inf),			% hij komt logeren
                                % dat komt te vervallen
	  aux(psp(part_intransitive(aan))), % hij komt aangelopen
	  aux(psp(ld_pp)),	       % hij komt naar huis gelopen
	  aux(psp(part_ld_transitive(_))), % hij komt het bos in gelopen
	  aux(psp(part_ld_pp(_,_))),	   % hij komt op ons afgerend
	  er_pp_sbar(achter),		   % ik kwam erachter dat ..
	  er_pp_sbar(bij),		   % hoe komt u er bij dat ..
	  mod_pp(bij),          % er komt een ton bij
	  er_pp_sbar(in),		   % ik kan er in komen dat ..
	  er_pp_sbar(tot),	% VL we moeten ertoe komen dat ..
	  pp_copula,
	  pp_copula_sbar,
	  ap_copula('onder de indruk'),
	  ap_copula('aan de beurt'),
	  ap_copula('in gebruik'),
	  ap_copula('in orde'),
	  ap_copula('ter beschikking'),
	  ap_copula('op dreef'),
	  ld_pp,		% ik kom naar de opening
	  pp_sbar_subj_no_het(bij), % hier komt nog bij, dat ..
	  pp_sbar_subj_no_het(bovenop), 
	  part_er_sbar_subj_no_het(bij), % waar nog bijkomt dat ..
	  pc_pp(met),		% hij komt met een wetsontwerp
	  pc_pp(om),		% hij kwam om suiker
	  pc_pp(onderuit),
	  er_pp_sbar(onderuit),
	  pc_pp(op),	 % hij komt op de koffie/op vreemde gedachten/
	                        % op ideeën; dat komt op veertig euro
	  pc_pp(tot),		% het kwam tot ..
	  er_pp_vp(tot),   % we kwamen er niet toe om het werk te doen
	  fixed([svp_pp(tot,ontploffing)],no_passive),
	  pc_pp(van),	% dat komt van de nachtvorst
                                % het zal er nooit van komen
	  so_np_ld_pp,		% het water komt ons tot de lippen
                                % "het kwijl komt mediaondernemers in de mond"
	  fixed([[bij,elkaar]],no_passive),
	  fixed([[bij,elkaar],ld_pp],no_passive),
	  fixed([[bij,elkaar],ld_adv],no_passive),
	  fixed([[bij,mekaar]],no_passive),
	  fixed([[bij,mekaar],ld_pp],no_passive),
	  fixed([[bij,mekaar],ld_adv],no_passive),
	  part_intransitive(aan),
	  part_ld_pp(aan),
	  part_so_np(aan),
	  part_pc_pp(aan,met),
	  part_pc_pp(aan,op),
	  part_er_pp_sbar(aan,op),
	  part_er_pp_vp_no_control(aan,op),
	  part_sbar(af),	% Vlaams; Kom nu niet af dat ...
	  part_pc_pp(af,op),
	  part_pc_pp(af,van),   % we komen niet van hem af
	  part_num_pred(voor),	% we komen 3-0 voor
	  part_num_pred(achter),
	  part_intransitive(achter), % omdat we toen heel vlug zijn achtergekomen
	  part_intransitive(af),
	  part_intransitive(bij),
	  part_intransitive(bijeen),
	  part_intransitive(binnen),
	  part_ld_pp(binnen),
	  part_intransitive(boven),
	  part_intransitive(buiten),
	  part_transitive(door),
	  part_ld_pp(door),
	  %% perhaps 'gereed, klaar, af, los, ...' are predc?
	  part_intransitive(gereed),
	  part_transitive(gereed),
	  part_pc_pp(gelijk,met),
	  part_intransitive(goed), % Oboema!
	  part_intransitive(in),   % hij kwam in met gestrekt been
				%      part_ld_pp(in),           % ???
				%      part_transitive(in),      % ??? we kunnen de kluis inkomen / should be LD
	  part_ld_adv(in),	% we kunnen daar inkomen
	  part_fixed(in,[er_pp(van),subj(niets)],no_passive),
	  part_fixed(in,[er_pp(van),subj(niks)],no_passive),
	  part_intransitive(klaar),
	  part_pc_pp(klaar,met),
	  part_intransitive(langs),
	  part_ld_pp(langs),
	  part_intransitive(los),
	  part_intransitive(mee),
	  part_ld_pp(mee),
	  part_intransitive(na),
	  part_intransitive(neer),
	  part_pc_pp(neer,op),
	  part_er_pp_sbar(neer,op),
	  part_ld_pp(neer),
	  part_intransitive(om),
	  part_intransitive(onder),
	  part_intransitive(op),
	  part_ld_pp(op),
	  part_pc_pp(op,in),	 % dat kwam niet in me op
	  part_pp_sbar_subj(op,bij), % het kwam niet bij me op dat
	  part_pp_vp_subj(op,bij),   % het kwam niet bij me op om
	  part_pp_sbar_subj(op,in),  % het kwam niet in me op dat
	  part_pp_vp_subj(op,in),    % het kwam niet in me op om
	  part_pc_pp(op,tegen),
	  part_pc_pp(op,voor),
	  part_intransitive(over),
	  part_ap_copula(over),	 % dat komt zuinig over
	  part_ap_copula_sbar(over), % het komt raar over dat ...
	  part_sbar_subj(over),	     % het komt over dat ...
	  part_fixed(over,[sbar_subj,pc(op)],no_passive),
	  part_ld_pp(over),
	  pc_pp(overheen),	% ik kom er wel overheen
	  er_pp_sbar(overheen),	% ik kom er wel weer overheen dat ...
	  part_intransitive(rond),
	  part_pc_pp(rond,met),
	  part_pc_pp(rond,van),
	  part_intransitive(samen),
	  part_transitive(tegen),
	  part_sbar_obj(tegen), % ik kom wel tegen dat ...
	  part_intransitive(terecht),
	  part_ld_adv(terecht),
	  part_ld_pp(terecht),
	  part_pc_pp(terecht,van),
	  part_intransitive(terug),
	  part_ld_pp(terug),
	  part_pc_pp(terug,op),
	  part_pc_pp(terug,van),
	  part_intransitive(thuis),
	  part_intransitive(toe),
	  part_so_np(toe),
	  part_so_pp(toe),
	  part_vp_subj_so_np(toe),
	  part_pc_pp(toe,aan),
	  part_er_pp_vp(toe,aan),  % ik kom er niet aan toe de boeken te openen
	  part_pc_pp(toe,met),
	  part_ld_pp(toe),
	  part_intransitive(tussen),
	  part_ld_pp(tussen),
	  part_ld_pp(uit),
	  part_intransitive(uit),
	  part_fixed(uit,[[de,neus],dat],no_passive),
	  part_pc_pp(uit,in),
	  part_pc_pp(uit,met),
	  part_pc_pp(uit,op),
	  part_pc_pp(uit,tegen),
	  part_pc_pp(uit,voor),
	  part_er_pp_sbar(uit,voor),
	  part_er_pp_vp(uit,voor),
				% het komt niet uit dat je nu komt
	  part_sbar_subj_opt_het(uit), % vandaag kwam uit dat je sliep
	  part_so_np(uit),             % omdat hem dat niet uit komt
	  part_sbar_subj_so_np(uit), % omdat het ons niet uit komt dat ...
	  part_vp_subj_so_np(uit), % omdat het ons niet uit komt om ...
	  part_intransitive(vooruit),
	  part_pc_pp(vooruit,met), % ik kom er niet mee vooruit
	  fixed([subj(klad),pc(in)],no_passive),
	  fixed([[als,geroepen]],no_passive),
	  als_copula,	     % als een (geweldige) schok/verrassing/..
	  fixed([svp_pp(aan,einde)],no_passive),
	  fixed([ap_pred(gelegen)],no_passive),
	  fixed([ap_pred(ongelegen)],no_passive),
	  fixed([ap_pred(gelegen),dat],no_passive),
	  fixed([ap_pred(ongelegen),dat],no_passive),
	  fixed([sbar_subj,ap_pred(gelegen)],no_passive),
	  fixed([sbar_subj,ap_pred(ongelegen)],no_passive),
          fixed([sbar_subj,ap_pred(gelegen),dat],no_passive),
          fixed([sbar_subj,ap_pred(ongelegen),dat],no_passive),
          fixed([[op,andere,gedachten]],norm_passive),
          fixed([[op,de,proppen],pc(met)],no_passive),
          fixed([[naar,buiten],sbar_subj_no_het],no_passive),
	  fixed([pc(van),ap_pred(op)],no_passive), % hij zou op van de zenuwen zijn / op zijn van de zenuwen
	  fixed([pc(aan),ap_pred(na)],no_passive), % hij was na aan de waarheid 
          fixed([[te,koop]],no_passive),
          fixed([[ten,ijs],ap_pred(beslaan)],no_passive),
          fixed([[tot,leven]],norm_passive),
          fixed([[tot,uiting]],norm_passive),
          fixed([[tot,uiting],sbar_subj_no_het],norm_passive),

          fixed([vc(sta,pass_te,intransitive),pc(op)],no_passive),
                                                    % dat komt op een boete te staan
          fixed([vc(sta,pass_te,intransitive),pc(op),dat],no_passive),
                                                    % dat komt hem op een boete te staan
          fixed([vc(sta,pass_te,intransitive),ap_pred(duur),dat],no_passive),
				% dat komt ons duur te staan
          fixed([vc(sta,pass_te,intransitive),sbar_subj,ap_pred(duur),dat],no_passive),
				% het komt ons duur te staan dat..

          fixed([vc(waai_aan,psp,part_intransitive(aan)),dat],no_passive),
          %% het komt me niet aangewaaid
          fixed([vc(waai_aan,inf,part_intransitive(aan)),dat],no_passive),
          %% het komt me niet aanwaaien

          fixed([vc(val,inf,intransitive),[uit,de,lucht]],no_passive),               % de ontknoping komt uit de lucht vallen
          fixed([vc(val,inf,intransitive),sbar_subj,[uit,de,lucht]],no_passive), % het komt niet uit de lucht vallen dat hij komt
          
          part_fixed(uit,[sbar_subj,ap_pred],no_passive),
				% het kwam goed uit dat hij kwam
          part_fixed(uit,[sbar_subj,ap_pred,dat],no_passive),
				% het kwam ons goed uit dat ..
          part_fixed(uit,[ap_pred],no_passive),
          part_fixed(uit,[ap_pred,dat],no_passive),
          part_fixed(uit,[[de,keel],dat],no_passive),
          part_fixed(uit,[sbar_subj,[de,keel],dat],no_passive),
          part_intransitive(voor), % problemen komen voor
          part_ld_pp(voor),            % koningskaars komt in de duinen veel voor

          part_dip_sbar_subj_so_np_opt_het(voor),     % het komt mij voor dat ..

          part_so_nonp_copula(voor),          % hij komt me bekend voor
          part_so_nonp_copula_vp(voor),   % het komt me overdreven voor om ..
          part_so_nonp_copula_sbar(voor),
          part_sbar_subj(voor),	% het komt voor, dat ..
          part_intransitive(voorbij),
          part_transitive(voorbij),
          part_ld_pp(voorbij),
          part_pc_pp(voort,uit),
          part_pp_sbar_subj_no_het(voort,uit),
          part_intransitive(vrij),
          part_intransitive(weg),
          part_ld_pp(weg),
          part_pc_pp(weg,met),
          fixed([pc(op),subj(kritiek)],no_passive),
          fixed([pc(op),subj(reactie)],no_passive), % er kwamen leuke reacties op
          fixed([er_pp(op,C),subj(kritiek),extra_sbar(C)],no_passive),
          fixed([[aan,de,bak]],no_passive),
          fixed([[aan,de,weet],acc],no_passive),
          fixed([[aan,de,weet],{[acc,pc(over)]}],no_passive),
          fixed([[aan,de,weet],sbar],no_passive),
          fixed([{[[voor,rekening],pc(van)]}],no_passive),
          fixed([{[pc(in),[tot,uitdrukking]]}],no_passive),
          fixed([[tot,uitdrukking]],no_passive),
          fixed([{[pc(in),[tot,uitdrukking]]},sbar_subj_no_het],no_passive),
          fixed([[tot,uitdrukking],sbar_subj_no_het],no_passive),
          fixed([{[pc(met),svp_pp(in,conflict)]}],no_passive),
          fixed([{[pc(met),pp_pred(in,contact)]}],no_passive),
          fixed([{[pc(voor),svp_pp(in,aanmerking)]}],no_passive),
          fixed([{[er_pp(voor,A),svp_pp(in,aanmerking)]},extra_vp(A)],no_passive),
          fixed([svp_pp(in,aanmerking)],no_passive),
          fixed([svp_pp(in,aanmerking),vp],no_passive),
          fixed([{[[in,aanraking],pc(met)]}],no_passive),
          fixed([{[svp_pp(in,aanvaring)]}],no_passive),
          fixed([{[svp_pp(in,botsing)]}],no_passive),
          fixed([{[svp_pp(in,aanvaring),pc(met)]}],no_passive),
          fixed([{[svp_pp(in,botsing),pc(met)]}],no_passive),
          fixed([{[pp_pred(in,actie),pc(tegen)]}],no_passive),
          fixed([pp_pred(in,actie)],no_passive),  % PP PREDC?
          fixed([{[[in,botsing],pc(met)]}],no_passive),
          fixed([pp_pred(in,hand),pc(van)],no_passive),
          fixed([[in,opstand]],no_passive),
          fixed([{[pc(tegen),[in,opstand]]}],no_passive),
          fixed([ap_pred('op de been')],no_passive),
          fixed([[op,het,spoor],acc],no_passive),
          fixed([{[[over,de,brug],pc(met)]}],no_passive),
          fixed([subj(schot),pc(in)],no_passive),
          fixed([[te,pas]],no_passive),
          fixed([{[[te,pas],pc(aan)]}],no_passive),
          fixed([{[[te,pas],pc(bij)]}],no_passive),
          fixed([{[[te,last],pc(van)]}],no_passive),
          fixed([[te,rade],ld_pp],no_passive),
          fixed([[te,rade],ld_adv],no_passive),
          fixed([{[[te,laste],pc(van)]}],no_passive),
          fixed([{[[ten,laste],pc(van)]}],no_passive),
          fixed([{[[tenlast],pc(van)]}],no_passive),
          fixed([{[[tenlaste],pc(van)]}],no_passive),
          fixed([{[[telast],pc(van)]}],no_passive),
          fixed([[voor,de,dag]],no_passive),
          fixed([{[[voor,de,dag],pc(met)]}],no_passive),
          fixed([[aan,bod]],no_passive),
          fixed([[aan,bod],ld_pp],no_passive),
          fixed([[aan,de,leiding]],no_passive),
          fixed([[aan,het,licht]],no_passive),
          fixed([sbar_subj_no_het,[aan,het,licht]],no_passive),
          fixed([[aan,het,daglicht]],no_passive),
          fixed([sbar_subj_no_het,[aan,het,daglicht]],no_passive),
          fixed([[aan,het,rollen]],no_passive),
          fixed([[beschikbaar]],no_passive), %% predc?
          fixed([er_pp(af),ap_pred],no_passive),
          fixed([er_pp(vanaf),ap_pred],no_passive),
          part_fixed(uit,[acc(strot),dat],no_passive),	% dat komt me/ons de/mijn/onze strot uit
          fixed([[in,omloop]],no_passive), % PP PREDC?
          fixed([ap_pred('in zwang')],no_passive),
          fixed([svp_pp(in,conflict)],no_passive), % PP PREDC?
          fixed([[in,het,geweer],pc(tegen)],no_passive),
          fixed([[in,het,nieuws]],no_passive), % PP PREDC?
          fixed([sbar_subj_no_het,[naar,voren]],no_passive),
          fixed([{[[naar,voren],pc(uit)]}],no_passive),
          fixed([sbar_subj_no_het,{[[naar,voren],pc(uit)]}],no_passive),
          fixed([[naar,voren],als_pred],no_passive),
          fixed([{[[naar,voren],pc(uit),als_pred]}],no_passive),
          fixed([[om,het,leven]],no_passive),
          fixed([[onder,ogen],dat],no_passive),
          fixed([svp_pp(onder,vuur)],norm_passive),
          fixed([[op,gang]],no_passive),  % PP PREDC?
          fixed([[op,verhaal]],no_passive), % PP PREDC?
          fixed([svp_pp(op,naam)],no_passive),
          fixed([svp_pp(op,naam),pc(van)],no_passive),
          fixed([[over,de,brug]],no_passive),
          fixed([[te,baat]],no_passive),
          fixed([[te,baat],dat],no_passive),
          fixed([[te,boven],acc],no_passive),
          fixed([[te,hulp]],no_passive),
          fixed([[te,hulp],dat],no_passive),
          fixed([[te,stade]],no_passive),
          fixed([[te,stade],dat],no_passive),
          fixed([[te,na],dat],no_passive),
          part_intransitive(tekort),
          part_transitive(tekort),
          fixed([{[[ten,goede],dat_pp(aan)]}],no_passive),
          fixed([{[[ten,goede],dat_pp(aan)]},vp_subj],no_passive),
          fixed([{[[ten,goede],dat_pp(aan)]},sbar_subj],no_passive),
          fixed([{[[ten,goede],pc(van)]}],no_passive),
          fixed([{[[ten,goede],pc(van)]},vp_subj],no_passive),
          fixed([{[[ten,goede],pc(van)]},sbar_subj],no_passive),
          fixed([[ten,goede],dat],no_passive),
          fixed([[ten,goede],dat,vp_subj],no_passive),
          fixed([[ten,goede],dat,sbar_subj],no_passive),
          fixed([[tengoede]],no_passive),
          part_intransitive(tengoede),
          part_so_np(tengoede),
          part_so_pp(tengoede),
          part_pc_pp(tengoede,van),
          part_vp_subj_so_np(tengoede),
          part_vp_subj_so_pp(tengoede),
          part_sbar_subj_so_np(tengoede),
          part_sbar_subj_so_pp(tengoede),
          fixed([[ten,einde]],no_passive),
          fixed([[teneinde]],no_passive),
          fixed([[ten,tonele]],no_passive),
          fixed([[te,val]],no_passive),
          fixed([[ten,val]],no_passive),
          fixed([[ter,sprake]],no_passive),
          fixed([sbar_subj_opt_het,[ter,sprake]],no_passive),
          fixed([[te,sprake]],no_passive),
          fixed([sbar_subj_opt_het,[te,sprake]],no_passive),
          fixed([[ter,ore],dat],no_passive),
          fixed([[ter,ore],pc(van)],no_passive),
          fixed([sbar_subj_opt_het,[ter,ore],dat],no_passive),
          fixed([sbar_subj_opt_het,[ter,ore],pc(van)],no_passive),
          fixed([[terzake]],no_passive),
          fixed([[tot,stand]],no_passive),
          part_intransitive(totstand),
          fixed([[uit,de,bus]],no_passive),
          fixed([[uit,de,bus],sbar_subj_no_het],no_passive),
          fixed([[uit,de,bus],als_pred],no_passive),
          fixed([[uit,de,verf]],no_passive),
          fixed([[van,de,grond]],no_passive),
          fixed([[van,pas]],no_passive),
          fixed([[van,pas],dat],no_passive),
          fixed([[voor,de,dag],ap_pred],no_passive),
          fixed([subj(vraag),pc(naar)],no_passive),
          %% we zijn dat te weten gekomen
          fixed([[te,weten],acc],no_passive),
          fixed([[te,weten],mod_pp(over),acc],no_passive),
          fixed([[te,weten],sbar],no_passive),
          aan_het   % "toen deze aan het wankelen kwam"
          %% ?? %we zijn dat komen te weten
          % fixed([vc(weet,te,intransitive),acc],no_passive),
          % fixed([vc(weet,te,intransitive),mod_pp(over),acc],no_passive),
          % fixed([vc(weet,te,intransitive),sbar],no_passive)
     ])]).

v(kondig,kondigt,kondigen,gekondigd,kondigde,kondigden,
    [h([part_als_pred_np(aan),
	part_sbar(aan),
	part_np_sbar(aan),
	part_transitive(aan),
	part_refl(aan),  % refl want volgorde: er kondigt zich een nieuwe X aan
	part_transitive(af),
	part_vp(aan)])]).

v(konkel,konkelt,konkelen,gekonkeld,konkelde,konkelden,
    [h([intransitive])]).

v(kook,kookt,koken,gekookt,kookte,kookten,
  [z([part_intransitive(in),
      part_intransitive(over),
      part_intransitive(uit)]),
   h([intransitive,
      ap_pred_np,
      transitive,
      pc_pp(op),
      np_mod_pp(in),  % het water waar ze de aardappelen in koken
      part_transitive(door),
      part_transitive(in),
      part_transitive(mee),
      part_transitive(in)])]).

v(koop,koopt,kopen,gekocht,kocht,kochten,
    [h([intransitive,
	transitive,
	np_pc_pp(met),
	np_pc_pp(van),	     % hij heeft er een buitenhuis van gekocht
	np_mod_pp(voor),     % ik koop er een boek voor
        np_np,
	part_intransitive(bij),
	part_intransitive(in),
	part_transitive(aan),
	part_transitive(af),
	part_np_np(af),
	part_np_pc_pp(af,van),
	part_transitive(bij),
	part_transitive(in),
	part_transitive(om),
	part_transitive(op),
        part_transitive(over),
	part_transitive(terug),
	part_transitive(uit),
	part_transitive(weg),
	part_refl_pc_pp(in,bij),
	part_refl_pc_pp(in,in)])]).

v(kop,kopt,koppen,gekopt,kopte,kopten,
    [h([transitive,
	intransitive,  % Ajaxieden koppen slecht
	part_intransitive(in),
	part_intransitive(over),
	part_intransitive(naast),
	part_intransitive(raak),
	part_intransitive(weg),
	part_intransitive(binnen),  % VL
	part_transitive(binnen),       
	part_transitive(in),       
	part_transitive(door),
	part_transitive(over),
	part_transitive(naast),
	part_transitive(raak),
	part_transitive(weg),
	np_ld_pp,   % hij kopt de bal over de zijlijn
	np_ld_dir,
	sbar])]).   % de krant kopte dat ...

v(kopieer,kopieert,kopiëren,gekopieerd,kopieerde,kopieerden,
    [h([intransitive,
	np_ld_pp,
	transitive])]).

v(koppel,koppelt,koppelen,gekoppeld,koppelde,koppelden,
    [h([intransitive,
	transitive,
	np_pc_pp(aan),
	part_transitive(los),
        part_transitive(terug),
	part_np_ld_pp(los),
        part_np_ld_pp(terug)])]).

v(korf,korft,korven,gekorfd,korfde,korfden,
    [h([part_intransitive(in),
        part_transitive(in)])]).

v(korfbal,korfbalt,korfballen,gekorfbald,korfbalde,korfbalden,
    [h([intransitive])]).

v(kort,kort,korten,gekort,kortte,kortten,
    [h([transitive,
	np_pc_pp(met),
	np_pc_pp(op),
	part_intransitive(in),
	part_transitive(af),
	part_transitive(in),
	pc_pp(op)]),
     b([intransitive])]).

v(kortwiek,kortwiekt,kortwieken,gekortwiekt,kortwiekte,kortwiekten,
    [h([transitive])]).

%% in CGN transitive: de kop kosten, het leven kosten
v(kost,kost,kosten,gekost,kostte,kostten,
  [h([intransitive,
      so_np,                % dat gaat me kosten, het kost me maar even (of is even me?)
	meas,               % todo: also with SO_PP?
	sbar_subj_dat_meas, % het kost ons veel geld dat ..
	sbar_subj_meas,     % het kost een fortuin dat ..
	so_meas,
        so_pp_meas,
	vp_subj_dat_meas,
	vp_subj_meas])]).

v(kots,kotst,kotsen,gekotst,kotste,kotsten,
    [h([intransitive,
        np_ld_pp,
	part_transitive(uit),
	part_transitive(onder),
	pc_pp(van)])]).

v(kraai,kraait,kraaien,gekraaid,kraaide,kraaiden,
    [h([intransitive,
	transitive,
	sbar,
	fixed([subj(haan),pc(naar)],no_passive),
	fixed([[victorie]],imp_passive)])]).

v(kraak,kraakt,kraken,gekraakt,kraakte,kraakten,
    [h([intransitive,
        fixed([{[acc(noot),pc(over)]}],norm_passive),
	part_transitive(af),
	transitive])]).

v(kraam,kraamt,kramen,gekraamd,kraamde,kraamden,
    [h([part_transitive(uit)])]).

v(krab,krabt,krabben,gekrabd,krabde,krabden,
    [h([np_np,
	intransitive,
%	refl,
	transitive,
        part_transitive(af),
	ld_pp,
	ld_adv,
        ld_dir,
	np_ld_pp,
	np_ld_adv
%	refl_ld_pp
       ])]).

v(krabbel,krabbelt,krabbelen,gekrabbeld,krabbelde,krabbelden,
    [h([intransitive,
	transitive,
	pc_pp(aan)]),
     z([ld_pp,
	ld_dir,
	part_intransitive(op),
	part_intransitive(terug)])]).

v(kras,krast,krassen,gekrast,kraste,krasten,
    [h([intransitive,
	transitive,
	part_transitive(door),
	part_transitive(in),
	part_np_ld_pp(in),
	vp,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv]),
     b([part_intransitive(op)])]).

v(krenk,krenkt,krenken,gekrenkt,krenkte,krenkten,
    [h([sbar_subj_so_np,
	transitive,
        fixed([acc(haar),dat],norm_passive), % geen haar werd hem gekrenkt
	vp_subj_so_np])]).

v(krepeer,krepeert,kreperen,gekrepeerd,krepeerde,krepeerden,
    [unacc([intransitive])]).

v(kreun,kreunt,kreunen,gekreund,kreunde,kreunden,
    [h([intransitive,
	sbar,
        transitive,  % een liedje kreunen (?!)
	part_intransitive(na)])]).

v(kreuk,kreukt,kreuken,gekreukt,kreukte,kreukten,
  [h([intransitive,
      transitive
     ])]).

v(kriebel,kriebelt,kriebelen,gekriebeld,kriebelde,kriebelden,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv])]).

v(krijg,krijgt,krijgen,gekregen,kreeg,kregen,
  [h([ap_pred_np,
      fixed([acc,{[ap_pred,pc(voor)]}],no_passive),
      pp_pred_np(onder,controle),
      pp_pred_np(onder,hoede),
      pp_pred_np(aan,slag),
      pp_pred_np(aan,werk),
      pp_pred_np(in,bezit),
      pp_pred_np(in,eigendom),
      pp_pred_np(op,peil),
      %% fixed([ap_pred,het_obj1],no_passive), ??? ik krijg het warm, zie ap_pred_np
      np_aan_het,
      so_passive,
      obj1_passive, % ik krijg dat allemaal niet gedaan, ik krijg het niet op tijd opgeruimd
				% treated as obj1_passive, because obj1 "dat" "het" are obligatory
      transitive,
      np_ld_pp,
      np_ld_adv,
      np_ld_dir,
      np_mod_pp(bovenop),
      pc_pp(met),	    % met de wapenstok
				% pc_pp(op),  % op mijn donder/flikker/...
      np_pc_pp(aan),		% ik krijg een hekel/de pest aan
      np_er_pp_sbar(aan),
      np_er_pp_vp(aan),
        fixed([svp_er_pp(door),acc],no_passive),  % we moeten die veranderingen erdoor krijgen
	fixed([svp_er_pp(bij),acc],no_passive),	% we krijgen er een kindje / 10 procent / .. bij
	part_np_pc_er_transitive(bij), % we hebben er een ton bijgekregen
	fixed([als_pred,acc],no_passive),
	fixed([als_pred,vp],no_passive),          % ik kreeg als opdracht om ..
	fixed([als_pred,sbar],no_passive), % ik krijg als opdracht dat ..
	part_fixed(mee,[als_pred,vp],no_passive),          % ik kreeg als opdracht mee om ..
	part_fixed(mee,[als_pred,sbar],no_passive), % ik krijg als opdracht mee dat ..
	part_intransitive(gelijk),
	part_pc_pp(gelijk,van),
	np_pc_pp(van),
	np_pc_pp(voor),
        np_pc_pp(op),  % we krijgen mensen op bezoek/de koffie
	part_sbar(door),
	part_sbar(terug),
	part_intransitive(vrij),
	part_transitive(vrij),   % hij heeft de vrouwen vrij gekregen
	part_intransitive(vrijaf),
	part_transitive(aan),   % we kregen een overall aan
	part_transitive(binnen),
	part_transitive(door),
	part_transitive(los),
	part_sbar(mee),  % hij heeft meegekregen dat ...
	part_transitive(mee), % NB dat ik de spullen mee naar huis krijg
	part_pp_sbar(mee,van), % hij heeft daarvan meegekregen dat ..
	part_np_pc_pp(mee,van), % daar krijg je iets van mee
        part_transitive(rond),
	part_transitive(terug),
        part_transitive(toe),
	subj_control(pass_te),
        obj_control(pass_te), % de paus heeft daklozen te eten gekregen
	                      % ALLOWS *de paus heeft daklozen gekregen te eten
        sbar_obj_opt_het,       % dan krijg je (het) al snel dat 
	fixed([[in],[de,pest]],no_passive), % hij heeft de pest in
	fixed([[in],[de,smoor]],no_passive),
	fixed([[in],[de,pest],sbar],no_passive),
	fixed([[in],[de,smoor],sbar],no_passive),
	fixed([[in],{[[de,pest],pc(over)]}],no_passive),
	fixed([[in],{[[de,smoor],pc(over)]}],no_passive),
	fixed([[in],{[[de,pest],pc(over)]},sbar],no_passive),
	fixed([[in],{[[de,smoor],pc(over)]},sbar],no_passive),
	fixed([er_pp(in),[de,pest]],no_passive),
	fixed([er_pp(in),[de,smoor]],no_passive),
	fixed([er_pp(in),[de,pest],sbar],no_passive),
	fixed([er_pp(in),[de,smoor],sbar],no_passive),
        fixed([[aan,de,stok],pc(met),het_obj1],no_passive),
	fixed([{[pc(over),acc(informatie)]}],norm_passive),
        fixed([{[pc(in),acc(aardigheid)]}],norm_passive),
	fixed([{[er_pp(in,C),acc(aardigheid)]},extra_sbar(C)],norm_passive),
	fixed([{[er_pp(in,C),acc(aardigheid)]},extra_vp(C)],norm_passive),
	fixed([[cadeau],acc],no_passive),
	fixed([[kado],acc],no_passive),
	fixed([[te,leen],acc],no_passive),
	fixed([{[acc(bekomst),pc(van)]}],no_passive),
	fixed([{[acc(beweging),pc(in)]}],no_passive),
	fixed([{[acc(contact),pc(met)]}],no_passive),
	fixed([{[acc(hulp),mod_pp(bij)]}],no_passive),
	fixed([{[[gestalte],pc(in)]}],no_passive),
	fixed([{[acc(lucht),pc(van)]}],no_passive),
	fixed([acc(tijd),me],imp_passive),
	fixed([acc(tijd),me,vp],imp_passive),
	fixed([acc(tijd)],imp_passive),
	fixed([acc(tijd),vp],imp_passive),
	fixed([[van,voren],[de,wind]],no_passive),
	fixed([[gedaan],het_pobj1(sbar)],no_passive),
	fixed([[gedaan],nt(sbar)],no_passive),
        fixed([{[[het,schijt],pc(van)]}],no_passive),
	fixed([[voor,elkaar],acc],no_passive),
	fixed([[voor,elkaar],het_pobj1(sbar)],no_passive),
	fixed([[voor,elkaar],nt(sbar)],no_passive),
	fixed([[voor,mekaar],het_pobj1(sbar)],no_passive),
	fixed([[voor,mekaar],nt(sbar)],no_passive),
	fixed([pc(van),[genoeg]],no_passive),
	fixed([pc(van),[geen,genoeg]],no_passive),
	fixed([{[er_pp(van,A),[genoeg]]},extra_vp(A)],no_passive),
	fixed([{[er_pp(van,A),[genoeg]]},extra_sbar(A)],no_passive),
	fixed([{[er_pp(van,A),acc(lucht)]},extra_sbar(A)],no_passive),
        fixed([{[er_pp(van,A),[weet]]},extra_sbar(A)],no_passive),
      	fixed([{[[weet],pc(van)]}],no_passive),
        fixed([{[er_pp(van,A),[weet]]},extra_sbar(A)],no_passive),
      	fixed([{[[geen,weet],pc(van)]}],no_passive),
	fixed([{[er_pp(van,A),[geen,weet]]},extra_sbar(A)],no_passive),
	fixed([[gestalte]],no_passive),
	fixed([[voor,het,zeggen],het_obj1],no_passive),
	fixed([[in,de,gaten],acc],no_passive),
	fixed([[in,de,gaten],sbar],no_passive),
	fixed([[in,de,smiezen],acc],no_passive),
	fixed([[in,de,smiezen],sbar],no_passive),
	fixed([pp_pred(in,hand),acc],no_passive),
	fixed([svp_pp(in,greep),acc],no_passive),
	fixed([{[acc(last),pc(met)]}],no_passive),
	fixed([[onder,ogen],acc],no_passive),
	fixed([[onder,de,duim],acc],norm_passive),
	fixed([[onder,de,knie],acc],no_passive),
	fixed([[onder,uit,de,zak]],no_passive),
	fixed([[onderuit,de,zak]],no_passive),
	fixed([{[acc(oog),pc(voor)]}],no_passive),
	fixed([[op,punt],acc],norm_passive),  % Vlaams
	fixed([ap_pred('op de been'),acc],no_passive),
	fixed([[op,de,knieën],acc],no_passive),
	fixed([{[acc(reactie),pc(op)]}],no_passive), % ik kreeg er leuke reacties op
	fixed([{[acc(schuld),pc(van)]}],no_passive),
	fixed([{[acc(schuld),er_pp(van,C)]},extra_sbar(C)],no_passive),
	fixed([{[acc(schuld),er_pp(van,C)]},extra_vp(C)],no_passive),
	fixed([acc(schuld),sbar],no_passive),
	fixed([vc(maak,pass_te,intransitive),pc(met)],no_passive),
	fixed([vc(maak,pass_te,intransitive),{[pc(met),acc]}],no_passive),
	fixed([[te,pakken],acc],norm_passive),
	fixed([[ten,antwoord],acc],no_passive),
	fixed([[ten,antwoord],sbar],no_passive),
	fixed([acc(telefoon),sbar],no_passive), % VL
	fixed([[ten,gehore],acc],norm_passive),
	fixed([[tot,taak],vp],no_passive),
	fixed([[tot,taak],acc],no_passive),
	fixed([[voor,elkaar],het_pobj1(vp)],no_passive),
	fixed([[voor,mekaar],het_pobj1(vp)],no_passive),
	fixed([acc(greep),pc(op)],norm_passive),
	fixed([acc(vat),pc(op)],norm_passive),
	fixed([acc(zicht),pc(op)],norm_passive),
	part_fixed(langs,[er_pp(van)],no_passive),
	part_np_pc_pp(terug,van),
	part_np_pc_pp(terug,voor),
	part_transitive(weg),
	part_np_ld_pp(weg)])]).

v(krijs,krijst,krijsen,[gekresen,gekrijst],[krijste,krees],[krijsten,kresen],
  [h([intransitive,
      part_transitive(uit),   % ze krijste het uit
	sbar])]).

v(krijt,krijt,krijten,gekreten,kreet,kreten,
  [h([intransitive,
      part_transitive(uit),
      part_sbar(uit)
     ])]).

v(krik,krikt,krikken,gekrikt,krikte,krikten,
    [h([part_transitive(op),
        np_ld_dir,
        np_ld_pp,
        transitive])]).

v(krimp,krimpt,krimpen,gekrompen,kromp,krompen,
    [b([part_intransitive(in),
	part_transitive(in),
	intransitive,
	part_intransitive(ineen)])]).

v(kringel,kringelt,kringelen,gekringeld,kringelde,kringelden,
  [z([ld_pp,
      ld_dir,
      ld_adv]),
   h([intransitive,
      refl_ld_pp,
      refl_ld_adv
     ])]).

v(krioel,krioelt,krioelen,gekrioeld,krioelde,krioelden,
    [h([intransitive,
	ld_adv,
	ld_pp])]).

v(kristalliseer,kristalliseert,kristalliseren,gekristalliseerd,kristalliseerde,kristalliseerden,
  [h([part_refl(uit)]),
   unacc([intransitive,
	  transitive,
          part_intransitive(uit)])]).

v(kritiseer,kritiseert,kritiseren,gekritiseerd,kritiseerde,kritiseerden,
    [h([transitive,
	sbar])]).

v(krom,kromt,krommen,gekromd,kromde,kromden,
    [unacc([intransitive]),
     h([%refl,
	transitive])]).

v(kronkel,kronkelt,kronkelen,gekronkeld,kronkelde,kronkelden,
    [z([ld_pp]),
     h([intransitive,
	% refl,
	refl_ld_pp,
	transitive])]).

v(kroon,kroont,kronen,gekroond,kroonde,kroonden,
    [h([pred_np,
	transitive,
	np_pc_pp(tot),
	pc_pp(tot)])]).

v(krop,kropt,kroppen,gekropt,kropte,kropten,
    [h([part_transitive(op)])]).

v(krui,kruit,kruien,gekruid,kruide,kruiden,
  [h([transitive,
      np_ld_pp])]).

v(kruid,kruidt,kruiden,gekruid,kruidde,kruidden,
    [h([transitive,
	np_pc_pp(met)])]).

v(kruip,kruipt,kruipen,gekropen,kroop,kropen,
    [z([ld_dir,
	ld_pp,
	part_intransitive(aan),
	part_intransitive(rond),
	part_intransitive(weg),
	pc_pp(voor),
	part_ld_pp(weg)]),
     b([intransitive,
	part_transitive(rond)])]).

v(kruis,kruist,kruisen,gekruist,kruiste,kruisten,
    [h([intransitive,
	transitive,
	part_transitive(aan),
	part_transitive(in),  % van de tomaten
	np_pc_pp(met)])]).

v(kruisig,kruisigt,kruisigen,gekruisigd,kruisigde,kruisigden,
    [h([transitive])]).

v(krul,krult,krullen,gekruld,krulde,krulden,
    [h([intransitive,
	transitive,
	ld_dir,  % haar mondhoeken krullen omhoog
	refl_pc_pp(om)])]).

v(kuch,kucht,kuchen,gekucht,kuchte,kuchten,
  [h([intransitive,
      dip_sbar  % ahum, kuchte hij voorzichtig
     ])]).

v(kuier,kuiert,kuieren,gekuierd,kuierde,kuierden,
    [z([ld_dir,
	part_intransitive(aan),
        part_intransitive(door),
        part_intransitive(langs),
        part_intransitive(verder),
        part_intransitive(voorbij),
	ld_pp]),
     h([intransitive])]).

v(kuil,kuilt,kuilen,gekuild,kuilde,kuilden,
    [h([part_transitive(in)])]).

v(kuis,kuist,kuisen,gekuist,kuiste,kuisten,
    [h([intransitive,
	transitive,
        part_intransitive(af), %VL
        part_transitive(af),   %VL
	part_intransitive(op),
	part_transitive(op)])]).

v(kukel,kukelt,kukelen,gekukeld,kukelde,kukelden,
    [h([intransitive]),
     z([ld_pp,
	ld_dir])]).

v(kus,kust,kussen,gekust,kuste,kusten,
    [h([intransitive,
	transitive,
	fixed([[gedag]],imp_passive),
	fixed([[gedag],dat],imp_passive),
	fixed([acc(hand),dat],imp_passive),
	np_ld_pp])]).

v(kut,kut,kutten,gekut,kutte,kutten,
    [h([intransitive])]).  

v(kuur,kuurt,kuren,gekuurd,kuurde,kuurden,
    [h([intransitive])]).  

v(kwaak,kwaakt,kwaken,gekwaakt,kwaakte,kwaakten,
    [h([intransitive])]).

v(kwak,kwakt,kwakken,gekwakt,kwakte,kwakten,
    [z([ld_pp]),
     h([np_ld_pp,
	np_ld_dir])]).

v(kwakkel,kwakkelt,kwakkelen,gekwakkeld,kwakkelde,kwakkelden,
    [h([intransitive])]).

v(kwalificeer,kwalificeert,kwalificeren,gekwalificeerd,
  kwalificeerde,kwalificeerden,
    [h([als_pred_np,
	refl,
	transitive,
	refl_pc_pp(voor)])]).

v(kwantificeer,kwantificeert,kwantificeren,gekwantificeerd,
  kwantificeerde,kwantificeerden,
    [h([transitive])]).

v(kwartet,kwartet,kwartetten,gekwartet,kwartette,kwartetten,
    [h([intransitive])]).

v(kwartier,kwartiert,kwartieren,gekwartierd,kwartierde,kwartierden,
    [h([part_transitive(in),
	part_np_ld_pp(in)])]).

v(kwast,kwast,kwasten,gekwast,kwastte,kwastten,
    [h([part_transitive(in)])]).

v(kwebbel,kwebbelt,kwebbelen,gekwebbeld,kwebbelde,kwebbelden,
    [h([intransitive])]).

v(kweek,kweekt,kweken,gekweekt,kweekte,kweekten,
    [h([transitive,
        intransitive,
        mod_pp(met),   % je kweekt er goodwill mee
	part_transitive(aan),
	part_transitive(op)])]).

v(kweel,kweelt,kwelen,gekweeld,kweelde,kweelden,
    [h([intransitive,
	transitive])]).

v(kwek,kwekt,kwekken,gekwekt,kwekte,kwekten,
    [h([intransitive])]).

v(kwel,kwelt,kwellen,gekweld,kwelde,kwelden,
    [h([intransitive,
	transitive])]).

v(kwets,kwetst,kwetsen,gekwetst,kwetste,kwetsten,
    [h([%refl,
	sbar_subj_so_np,
	transitive,
        intransitive,
	vp_subj_so_np])]).

v(kwetter,kwettert,kwetteren,gekwetterd,kwetterde,kwetterden,
  [h([intransitive,
      sbar
     ])]).

v(kwijl,kwijlt,kwijlen,gekwijld,kwijlde,kwijlden,
    [h([intransitive])]).

v(kwijn,kwijnt,kwijnen,gekwijnd,kwijnde,kwijnden,
    [unacc([part_intransitive(weg)]),
     h([intransitive])]).

v(kwijt,kwijt,kwijten,gekweten,kweet,kweten,
    [h([transitive,
	refl_pc_pp(van)])]).

v(kwispel,kwispelt,kwispelen,gekwispeld,kwispelde,kwispelden,
    [h([intransitive,
	transitive  % ? hij kwispelde zijn staart
       ])]).

v(laad,laadt,laden,geladen,laadde,laadden,
    [h([transitive,
	np_ld_pp,
	np_pc_pp(met),
	intransitive,  % een plek om te laden en lossen
	part_transitive(af),
	part_transitive(door),
	part_transitive(in),
	part_transitive(op),
	part_transitive(over),
	part_transitive(uit),
	part_transitive(vol),
	part_np_pc_pp(over,in),
	part_np_pc_pp(over,op)])]).

v(laaf,laaft,laven,gelaafd,laafde,laafden,
    [h([refl,
	transitive,
	refl_pc_pp(aan)])]).

v(laai,laait,laaien,gelaaid,laaide,laaiden,
    [unacc([part_intransitive(op),
	intransitive])]).

v(laak,laakt,laken,gelaakt,laakte,laakten,
    [h([transitive,
	np_pc_pp(in)])]).

%% CANDIDATES for svp?
%% zich laten vertellen SBAR
%% het laten aankomen op
%% laten liggen LD

v(laat,laat,laten,gelaten,liet,lieten,
    [h([aci,            % ik liet de kinderen het liedje zingen
	aci_no_obj,     % ik liet de schilderijen stelen
	% Gerlof Bouma: ik laat de boeken aan hem zien / ik laat hem de boeken zien
	% isn't this obj2? Yes. Further evidence because of unexpected vp and sbar arg's
	fixed([vc(hoor,inf,intransitive),vp],no_passive),
	fixed([vc(hoor,inf,intransitive),dat,vp],no_passive),
	fixed([vc(hoor,inf,intransitive),dat_pp(aan),vp],no_passive),
	fixed([vc(hoor,inf,intransitive),sbar],no_passive),
	fixed([vc(hoor,inf,intransitive),dat,sbar],no_passive),
	fixed([vc(hoor,inf,intransitive),dat_pp(aan),sbar],no_passive),
	fixed([vc(hoor,inf,intransitive),acc],no_passive),
	fixed([vc(hoor,inf,intransitive),acc,dat],no_passive),
	fixed([vc(hoor,inf,intransitive),{[acc,dat_pp(aan)]}],no_passive),
        fixed([vc(hoor,inf,intransitive),pp_refl(van)],no_passive),  % hij heeft niets van zich laten horen
	fixed([vc(zie,inf,intransitive),acc],no_passive),
	fixed([vc(zie,inf,intransitive),acc,dat],no_passive),
	fixed([vc(zie,inf,intransitive),{[acc,dat_pp(aan)]}],no_passive),
	fixed([vc(zie,inf,intransitive),{[acc(voorbeeld),pc(van)]}],no_passive),
	fixed([vc(zie,inf,intransitive),{[acc(voorbeeld),dat,pc(van)]}],no_passive),
	fixed([vc(zie,inf,intransitive),{[acc(voorbeeld),dat_pp(aan),pc(van)]}],no_passive),
	fixed([vc(zie,inf,intransitive),vp],no_passive),
	fixed([vc(zie,inf,intransitive),dat,vp],no_passive),
	fixed([vc(zie,inf,intransitive),dat_pp(aan),vp],no_passive),
	fixed([vc(zie,inf,intransitive),sbar],no_passive),
	fixed([vc(zie,inf,intransitive),dat,sbar],no_passive),
	fixed([vc(zie,inf,intransitive),dat_pp(aan),sbar],no_passive),
	fixed([vc(blijk,inf,intransitive),vp],no_passive),
	fixed([vc(blijk,inf,intransitive),dat,vp],no_passive),
	fixed([vc(blijk,inf,intransitive),dat_pp(aan),vp],no_passive),
	fixed([vc(blijk,inf,intransitive),sbar],no_passive),
	fixed([vc(blijk,inf,intransitive),dat,sbar],no_passive),
	fixed([vc(blijk,inf,intransitive),dat_pp(aan),sbar],no_passive),
	fixed([vc(blijk,inf,intransitive),dat_pp(aan),sbar],no_passive),
	fixed([vc(blijk,inf,intransitive),acc],no_passive),
	fixed([vc(blijk,inf,intransitive),acc,dat],no_passive),
	fixed([vc(blijk,inf,intransitive),{[acc,dat_pp(aan)]}],no_passive),
	fixed([vc(merk,inf,intransitive),vp],no_passive),
	fixed([vc(merk,inf,intransitive),dat,vp],no_passive),
	fixed([vc(merk,inf,intransitive),dat_pp(aan),vp],no_passive),
	fixed([vc(merk,inf,intransitive),sbar],no_passive),
	fixed([vc(merk,inf,intransitive),dat,sbar],no_passive),
	fixed([vc(merk,inf,intransitive),dat_pp(aan),sbar],no_passive),
	fixed([vc(merk,inf,intransitive),acc],no_passive),
	fixed([vc(merk,inf,intransitive),acc,dat],no_passive),
	fixed([vc(merk,inf,intransitive),{[acc,dat_pp(aan)]}],no_passive),
        fixed([vc(ruik,inf,intransitive),[een,poepje],acc],no_passive),
	fixed([vc(voel,inf,intransitive),vp],no_passive),
	fixed([vc(voel,inf,intransitive),dat,vp],no_passive),
	fixed([vc(voel,inf,intransitive),dat_pp(aan),vp],no_passive),
	fixed([vc(voel,inf,intransitive),sbar],no_passive),
	fixed([vc(voel,inf,intransitive),dat,sbar],no_passive),
	fixed([vc(voel,inf,intransitive),dat_pp(aan),sbar],no_passive),
	fixed([vc(voel,inf,intransitive),acc],no_passive),
	fixed([vc(voel,inf,intransitive),acc,dat],no_passive),
	fixed([vc(voel,inf,intransitive),{[acc,dat_pp(aan)]}],no_passive),
	fixed([vc(weet,inf,intransitive),vp],no_passive),
	fixed([vc(weet,inf,intransitive),dat,vp],no_passive),
	fixed([vc(weet,inf,intransitive),dat_pp(aan),vp],no_passive),
	fixed([vc(weet,inf,intransitive),sbar],no_passive),
	fixed([vc(weet,inf,intransitive),dat,sbar],no_passive),
	fixed([vc(weet,inf,intransitive),dat_pp(aan),sbar],no_passive),
	fixed([vc(weet,inf,intransitive),acc],no_passive),
	fixed([vc(weet,inf,intransitive),acc,dat],no_passive),
	fixed([vc(weet,inf,intransitive),{[acc,dat_pp(aan)]}],no_passive),
	%
	fixed([vc(versta,inf,intransitive),vp],no_passive),
	fixed([vc(versta,inf,intransitive),dat,vp],no_passive),
	fixed([vc(versta,inf,intransitive),dat_pp(aan),vp],no_passive),
	fixed([vc(versta,inf,intransitive),sbar],no_passive),
	fixed([vc(versta,inf,intransitive),dat,sbar],no_passive),
	fixed([vc(versta,inf,intransitive),dat_pp(aan),sbar],no_passive),
	fixed([vc(versta,inf,intransitive),acc],no_passive),
	fixed([vc(versta,inf,intransitive),acc,dat],no_passive),
	fixed([vc(versta,inf,intransitive),{[acc,dat_pp(aan)]}],no_passive),
	%
        fixed([vc(eet,inf,intransitive),[van,het,brood],[de,kaas],refl],no_passive),
	fixed([vc(ga,inf,intransitive),[verstek]],no_passive),
	fixed([vc(kijk,inf,intransitive),[in,de,kaart],refl],no_passive),
	fixed([vc(leun_aan,inf,part_intransitive(aan)),{[acc,refl]}],no_passive),
	fixed([vc(leun_aan,inf,part_intransitive(aan)),refl,sbar],no_passive),
	fixed([vc(leun_aan,inf,part_intransitive(aan)),refl,het_obj1,sbar],no_passive),
        fixed([vc(ontga,inf,intransitive),{[refl,acc]}],no_passive), % hij liet zich deze buitenkans niet ontgaan
        fixed([vc(ontval,inf,intransitive),{[refl,acc]}],no_passive), % hij liet zich/dat ontvallen
        fixed([vc(ontval,inf,intransitive),refl,sbar],no_passive), % hij liet zich ontvallen dat ..
        fixed([vc(ontval,inf,intransitive),refl,vp],no_passive), % hij liet zich ontvallen hem te missen
        fixed([vc(ontglip,inf,intransitive),{[refl,acc]}],no_passive), 
        fixed([vc(ontglip,inf,intransitive),refl,sbar],no_passive), 
        fixed([vc(ontglip,inf,intransitive),refl,vp],no_passive), % 
        fixed([vc(raad,inf,intransitive),refl,sbar_subj],no_passive), % het laat zich raden dat hij komt
        part_fixed(over,[vc(raad,pass_te,intransitive)],no_passive), % dat jurkje laat niets te raden over
        part_fixed(over,[vc(raad,pass_te,intransitive),acc],norm_passive), % die zaak laat een heleboel te raden over
				                                           % er wordt heel wat te raden overgelaten
	fixed([vc(schrik_af,inf,part_intransitive(af)),{[pc(door),refl]}],no_passive),
        fixed([vc(schemer_door,inf,part_intransitive(door)),sbar],no_passive),
        fixed([vc(schemer_door,inf,part_intransitive(door)),vp],no_passive),
	fixed([vc(sla,inf,intransitive),[uit,het,veld],refl],no_passive),
	fixed([vc(sla,inf,intransitive),{[mod_pp(door),[uit,het,veld]]},refl],no_passive),
	fixed([vc(val,inf,intransitive),sbar],no_passive),  % hij heeft laten vallen dat...
        fixed([vc(val,inf,intransitive),acc(steek)],no_passive),
        fixed([vc(val,inf,intransitive),acc(steek_DIM)],no_passive),
	fixed([vc(versta,inf,intransitive),dat,sbar],no_passive),
        fixed([vc(versta,inf,intransitive),refl,sbar_subj],no_passive), % het laat zich verstaan dat hij komt
        fixed([vc(volg,inf,intransitive),pc(op),sbar],no_passive),
        fixed([vc(voorspel,inf,intransitive),refl,sbar_subj],no_passive), % het laat zich voorspellen dat hij komt
        fixed([vc(wacht,inf,intransitive),pp_refl(op)],no_passive),       % de nieuwe generatie liet op zich wachten
	fixed([vc(weet_af,inf,part_intransitive(af)),het_obj1],no_passive),
        fixed([vc(geval_wel,inf,part_intransitive(wel)),{[refl,acc]}],no_passive), 
        fixed([vc(geval_wel,inf,part_intransitive(wel)),refl,sbar],no_passive), 
        fixed([vc(zit,inf,intransitive),er_pp(bij),het_obj1],no_passive),
        fixed([vc(zie_aan,inf,part_intransitive(aan)),refl,naar_sbar_subj],no_passive), % het laat zich aanzien dat hij komt
        fixed([vc(vaar,inf,intransitive),acc],no_passive),
        fixed([vc(piepel,inf,intransitive),refl],no_passive),
	pred_np,	        % dat laat me koud
				% ik liet dat in goede staat
				% hij liet de boel de boel
	np_np,			% wij lieten hem geen keus
	np_ld_dir,              % ik liet hem de deur uit
	so_pp_np,		% ik laat het aan hem
	vp_obj,		        % hij kan het niet laten om haar te plagen
        part_np_vp_obj(toe),    % PPI 03.p.1.s.4
	part_vp_obj(toe),       % het wordt toegelaten om .. ; het laat toe om ..
	part_np_np(toe),        % we laten het hem niet toe...
	transitive_ndev_ndev_npas,	% laat dat !
        intransitive,           % laat maar ...
	inverted_aux(inf),	% laten we gaan; * we laten gaan
	np_ld_pp,               % ik liet hem binnen / in de kamer
	np_ld_adv,              % ik liet hem binnen / in de kamer
	np_pc_pp_refl(achter),  % ik heb het achter me gelaten
	np_pc_pp(bij),          % daar laat ik het bij
	part_np_np(na),
	part_np_np(over),
	part_intransitive(af),
	part_intransitive(los),
	part_sbar_subj_so_np(na), % ?
	part_sbar_subj_so_np(open), % ?
	part_sbar(los),
	part_sbar(toe),
	part_sbar(weg),
        part_transitive(aan),  % ik had het gas aangelaten
	part_transitive(achter),
	part_transitive(af),
	part_transitive(binnen),
	part_transitive(daar),
	part_transitive(door),
	part_transitive(in),
	part_transitive(los),
	part_transitive(na),
	part_transitive(neer),
	part_transitive(op),
	part_transitive(open),
	part_transitive(over),
	part_transitive(thuis),
	part_transitive(toe),
	part_transitive(uit),
	part_transitive(vrij),
	part_transitive(weg),
	part_vp(af), % Natuurlijk laat Gillette niet af de zegeningen van de Venus te benadrukken .
	part_vp(los),
	part_vp(na),
        part_vp_obj(na),
	part_vp_no_control(toe),
	part_vp(weg),
	fixed([[aan,het,woord],acc],norm_passive),
	fixed([ap_pred,dat,sbar_subj],no_passive),
				% het laat ons koud dat ..
	fixed([[achterwege],acc],norm_passive),
	fixed([[achterwege],sbar],norm_passive),
 	fixed([[buiten,beschouwing],acc],norm_passive),
 	fixed([[buiten,beschouwing],sbar],norm_passive),
        fixed([ap_pred(heel),pc(van),acc],norm_passive), % niets/weinig/geen spaan
	fixed([[in,de,steek],acc],norm_passive),
	fixed([[in,het,onzekere],acc],norm_passive),
	fixed([[in,het,midden],acc],norm_passive), 
	fixed([[in,het,midden],sbar],norm_passive), 
	fixed([[in,'\'t',onzekere],acc],norm_passive),
	fixed([[met,rust],acc],norm_passive),
	fixed([[onbetuigd],refl],no_passive),
	fixed([[onverlet],sbar],no_passive), % maar dat laat onverlet dat ..
	fixed([[onvermeld],sbar],no_passive),
	part_np_ld_pp(toe),
	part_np_pc_pp(los,op),
	part_np_pc_pp(los,over),
	part_np_pc_pp(over,aan),
	part_pp_sbar(over,aan),
        part_pp_vp_obj(over,aan),        % ik laat het aan jullie over om ...
        part_fixed(over,[pc(aan),vp_no_control],no_passive),  % ik laat aan jullie over om ...
	part_np_pc_pp(vrij,op),
	part_refl_pc_pp(in,met),
	part_refl_pc_pp(uit,over),
	part_refl_er_pp_sbar(uit,over),
	part_fixed(over,[vc(wens,pass_te,intransitive)],no_passive),
	part_fixed(over,[vc(wens,pass_te,intransitive),acc],norm_passive) % dat laat een heleboel te wensen over
                                                                          % er wordt heel wat te wensen over gelaten
       ])]).

v(label,labelt,labelen,gelabeld,labelde,labelden,
    [h([transitive,
	als_pred_np
       ])]).

v(lach,lacht,lachen,gelachen,lachte,lachten,
    [h([% np_np,  ??? lach me een hoedje -> pred,refl below
	intransitive,
	pred_np,
	transitive,
 	fixed([pred,refl],no_passive),
	sbar, %% DIP
	part_sbar(uit), %% DIP
	part_intransitive(uit),
	part_transitive(toe),
	part_transitive(uit),
	part_np_mod_pp(uit,om),
	part_transitive(weg),
	mod_pp(bij),		% ze lacht er lief bij
	mod_pp(met),            % ze kan er niet mee lachen
	pc_pp(om),
	pc_pp(over),
	pc_pp(tegen)])]).

v(lak,lakt,lakken,gelakt,lakte,lakten,
    [h([ap_pred_np,
	transitive])]).

v(lal,lalt,lallen,gelald,lalde,lalden,
    [h([intransitive,
	transitive])]).

v(lammer,lammert,lammeren,gelammerd,lammerde,lammerden,
    [h([intransitive,
	part_intransitive(af)])]).

v(lanceer,lanceert,lanceren,gelanceerd,lanceerde,lanceerden,
    [h([transitive,
        als_pred_np,
        sbar,
	np_ld_pp,
	intransitive
       ])]).

v(land,landt,landen,geland,landde,landden,
    [z([intransitive,
	ld_pp,
	ld_adv,
	part_intransitive(aan),
	part_ld_pp(aan)]),
     h([transitive,
	np_ld_pp])]).

v(langlauf,langlauft,langlaufen,gelanglauft,langlaufte,langlauften,
    [h([intransitive])]).

v(lap,lapt,lappen,gelapt,lapte,lapten,
    [h([np_np,
	intransitive,
	transitive,
	fixed([svp_pp(aan,laars),acc],norm_passive),
	fixed([svp_pp(aan,laars),sbar],norm_passive),
	%% Dat het bestemmingsplan geen woonfunctie toestond lapte de ambtsbekleder aan zijn laars .
	part_transitive(op)])]).

v(lardeer,lardeert,larderen,gelardeerd,lardeerde,lardeerden,
    [h([transitive,
        np_pc_pp(met)])]).

v(las,last,lassen,gelast,laste,lasten,
    [h([intransitive,
	transitive,
	np_ld_pp,
	part_sbar(in),
        part_transitive(af),  % "finish lassen", but used as "afgelasten"
	part_transitive(in)])]).

v(last,last,lasten,gelast,lastte,lastten,
    [h([part_transitive(af),
        part_transitive(in)])]).

v(lauwer,lauwert,lauweren,gelauwerd,lauwerde,lauwerden,
    [h([transitive,
        intransitive])]).

v(laveer,laveert,laveren,gelaveerd,laveerde,laveerden,
    [h([intransitive])]).

v(lazer,lazert,lazeren,gelazerd,lazerde,lazerden,
    [z([ld_dir,
        part_intransitive(op),
	ld_pp]),
     h([np_ld_dir,
	np_ld_pp])]).

v(leas,least,leasen,[geleast,geleasd],[leaste,leasde],[leasten,leasden],
    [h([intransitive,
	transitive])]).

v(ledig,ledigt,ledigen,geledigd,ledigde,ledigden,
    [h([transitive,
	np_pc_pp(op)])]).

v(leef,leeft,leven,geleefd,leefde,leefden,leve,
    [unacc([part_intransitive(op)]),
     h([intransitive,
	ld_adv,
	transitive,
	ld_pp,
        part_intransitive(mee),
	part_intransitive(samen),
	part_refl(in),
	part_refl(uit),
	part_transitive(na),
	pc_pp(met),
	pc_pp(op),
	pc_pp(van),
        fixed([pp_pred(in,onmin)],no_passive),
        fixed([pp_pred(in,onmin),pc(met)],no_passive),
	er_pp_sbar(met),   % we moeten er mee leren leven dat ...
	er_pp_vp(met),     % we moeten er mee leren leven om ... 
	pc_pp(voor),
	part_ld_pp(voort),
	part_pc_pp(mee,met),
	part_pc_pp(samen,met),
	part_transitive(uit),
	part_refl_pc_pp(in,in),
	part_refl_pc_pp(uit,in),
	part_refl_pc_pp(uit,op)])]).

v(leeg,leegt,legen,geleegd,leegde,leegden,
    [h([transitive,
	np_ld_pp])]).

v(leen,leent,lenen,geleend,leende,leenden,
    [h([np_np,
	intransitive,
	so_pp_np,
	transitive,
	np_pc_pp(van),
	part_np_np(uit),
	part_so_pp_np(uit),
	part_transitive(uit),
	pc_pp(van),
        refl_pc_pp(tot),
        refl_er_pp_vp(tot),
	refl_pc_pp(voor),
        refl_er_pp_vp(voor)])]).

v(leer,leert,leren,geleerd,leerde,leerden,
    [h([np_np,
	intransitive,
	np_sbar,
	np_vp_obj,            % ik heb hem geleerd om goed uit te kijken
	so_control(te_inf),   % ik heb hem leren fietsen
	                      % not aci: * ik leer het sneeuwen
	                      %     aci:   ik zie het sneeuwen
                              % omdat ik de kinderen beter heb leren uit te kijken
	subj_control(te_inf), % ik heb leren fietsen
                              % ik moet het leren toe te laten
	sbar,
	transitive,
	vp,
	np_pc_pp(uit),
	np_pc_pp(van),
	np_mod_pp(over),
	part_np_np(aan),
	part_np_vp_obj(aan),    % ik heb hem aangeleerd om goed uit te kijken
	part_np_np(af),
	part_intransitive(aan),
	part_intransitive(bij),
	part_np_sbar(aan),
	part_sbar(aan),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(bij),
	pc_pp(van),
	pc_pp(voor)])]).

v(lees,leest,lezen,gelezen,las,lazen,leze,
    [h([np_np,
	intransitive,
	sbar,
	transitive,
	mod_pp(doorheen),     % een boek waar je gretig doorheen leest
	mod_pp(in),           % dat kun je er allemaal in lezen
	part_np_er_pc_pp(af,aan),  % dat lees je er niet aan af
	part_pp_sbar(af,aan),      % je leest er niet aan af dat ..
	part_pp_sbar(af,van),      
	part_sbar(af),
	part_transitive(af),
	part_np_pc_pp(af,van),
	part_intransitive(voor),
	part_transitive(voor),
	part_so_pp_np(voor,aan),
	part_so_np(voor),
	part_np_np(voor),
	part_so_pp(voor,aan),
	part_amb_so_np_pass(voor),  % ik word/krijg voorgelezen
	part_intransitive(kaart),
	part_sbar(na),
	part_transitive(door),
	part_transitive(over),
        part_transitive(in),
	part_transitive(na),
        part_intransitive(na),
	part_transitive(op),
	part_np_pc_pp(na,op),
	part_intransitive(terug),
	part_transitive(terug),
	part_transitive(uit),
	fixed([{[acc,pc(over),mod_pp(in)]}],norm_passive),
	fixed([{[pc(over),mod_pp(in)]}],imp_passive),
	np_mod_pp(van),
	pc_pp(over),
        np_pc_pp(over)])]).

v(leg,legt,leggen,gelegd,legde,legden,
    [h([ap_pred_np,
        pp_pred_np(in,as),
        pp_pred_np(in,puin),
	np_np,      % ?? dat heeft hem geen windeieren gelegd; hem een strobreed in de weg leggen
	transitive,
	np_ld_pp,
	np_np_ld_pp,
	np_ld_adv,		% opzij boven onder buiten  ...
%%% TODO
%%% ik leg de appels te rijpen
%%% de appels worden te rijpen gelegd
	obj_control(pass_te),
	part_np_np(aan),
	part_np_np(op),
	part_np_np(uit),
        part_so_pp_np(uit),
	part_np_np(voor),
	part_so_pp_np(voor),
	part_pc_pp(toe,op),
	part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(bij),
	part_intransitive(in),
 	part_intransitive(vast),
	part_intransitive(voor),
	part_ld_pp(terug),
	part_np_ld_pp(terug),
	part_np_sbar(uit),
	part_pred_np(uit),
	part_refl(vast),   % different sense
	part_refl_pc_pp(vast,op),
	part_sbar(vast),
	part_refl_vp(vast),
	% part_sbar_subj_so_np(op), ???
	part_sbar(neer),
	part_sbar(uit),
	part_sbar(voor),
	part_so_pp_np(op),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(bij),
	part_transitive(bloot),
        part_transitive(droog),
	part_transitive(in),
	part_transitive(klaar),
        part_transitive(lam),
	part_transitive(neer),
        part_intransitive(neer),
	part_transitive(om),
	part_transitive(op),
        part_transitive(opzij),
	part_transitive(over),
	part_transitive(plat),
	part_transitive(stil),
	part_transitive(terug),
	part_transitive(toe),
	part_intransitive(uit),
	part_transitive(uit),
 	part_transitive(vast),
	part_als_pred_np(vast),
	part_transitive(voor),
	part_transitive(weg),
	part_vp(op),
	part_vp(vast),
	part_vp(voor),
	refl_ld_pp,
	fixed([[beslag],pc(op)],imp_passive),
	fixed([[geen,beslag],pc(op)],imp_passive),
        fixed([[aan,banden],acc],norm_passive),
	fixed([[aan,de,dag],acc],norm_passive),
	fixed([[aan,den,dag],acc],norm_passive),
	fixed([[aan,de,dag],pc(voor),acc],norm_passive),
	fixed([[aan,den,dag],pc(voor),acc],norm_passive),
        fixed([[de,hand],pc(op)],imp_passive),
	fixed([[in,de,weg],[geen,strobreed],dat],imp_passive),
	fixed([{[pc(voor),acc(fundament)]}],norm_passive),
	fixed([pp_pred(in,as),acc],norm_passive),
	fixed([[in,de,luren],acc],norm_passive),
	fixed([{[pp_pred(in,hand),pc(van)]},acc],norm_passive),
	fixed([[te,ruste],acc],norm_passive),
	fixed([[te,rusten],acc],norm_passive),
	fixed([[te,slapen],acc],norm_passive),
	fixed([[te,vondeling],acc],norm_passive),
	fixed([[te,laste],acc],norm_passive),
	fixed([[te,laste],{[acc,dat]}],norm_passive),
	fixed([[te,laste],dat,sbar],imp_passive),
	fixed([[te,last],acc],norm_passive),
	fixed([[te,last],{[acc,dat]}],norm_passive),
	fixed([[te,last],dat,sbar],imp_passive),
	fixed([[ten,laste],acc],norm_passive),
	fixed([[ten,laste],{[acc,dat]}],norm_passive),
	fixed([[ten,laste],dat,sbar],imp_passive),
	fixed([[tenlaste],acc],norm_passive),
	fixed([[tenlaste],{[acc,dat]}],norm_passive),
	fixed([[tenlaste],dat,sbar],imp_passive),
	fixed([[telaste],acc],norm_passive),
	fixed([[telaste],{[acc,dat]}],norm_passive),
	fixed([[telaste],dat,sbar],imp_passive),
	fixed([[telast],acc],norm_passive),
	fixed([[telast],{[acc,dat]}],norm_passive),
	fixed([[telast],dat,sbar],imp_passive),
	fixed([[ten,uitvoer],acc],norm_passive),
	fixed([{[pc(op),acc(nadruk)]}],norm_passive),
	fixed([{[er_pp(op,C),acc(nadruk)]},extra_sbar(C)],norm_passive),
        fixed([[aan,de,schenen],ap_pred(na),{[[het,vuur],dat]}],imp_passive),  % also "te na"
	fixed([[te,luister],acc(oor)],norm_passive),
	fixed([[te,luisteren],acc(oor)],norm_passive),
	fixed([svp_pp(in,schaal),acc(gewicht)],no_passive),
        part_fixed(af,[acc(rekenschap)],no_passive),
        part_fixed(af,[acc(rekenschap),pc(van)],no_passive),
        part_fixed(af,[acc(rekenschap),pc(over)],no_passive),
        part_fixed(af,[acc(verantwoording),dat_pp(aan)],no_passive),
        part_fixed(af,[acc(verantwoordelijkheid),dat_pp(aan)],no_passive),
        part_fixed(af,[{[acc(verantwoording),dat]}],no_passive),
        part_fixed(af,[{[acc(verantwoordelijkheid),dat]}],no_passive),
        fixed([[voor,anker],acc],norm_passive),
	part_np_ld_pp(af),
	part_np_ld_pp(neer),
	part_np_ld_pp(vast),
	part_np_pc_pp(aan,op),
	part_np_pc_pp(af,tegen),
	part_np_pc_pp(in,met),
	part_np_pc_pp(toe,op),
	part_np_pc_pp(weg,voor),
	part_pc_pp(aan,op),
	part_refl_pc_pp(neer,bij),
	part_refl_er_pp_sbar(neer,bij),
	part_refl_er_pp_vp(toe,op),  % hij legt zich er op toe om ..
	part_refl_pc_pp(toe,op)])]).

v(legaliseer,legaliseert,legaliseren,gelegaliseerd,legaliseerde,legaliseerden,
    [h([transitive])]).

v(leger,legert,legeren,gelegerd,legerde,legerden,
    [z([intransitive,
	ld_pp]),
     h([%refl,
	transitive,
	np_ld_pp
	%refl_ld_pp
       ])]).

v(legitimeer,legitimeert,legitimeren,gelegitimeerd,legitimeerde,legitimeerden,
    [h([refl,
	sbar_subj_np,
	transitive
       ])]).

v(leid,leidt,leiden,geleid,leidde,leidden,
    [h([intransitive,
	ld_dir,
	np_ld_dir,
	transitive,
	pc_pp(tot),
	er_pp_sbar(tot),
	er_pp_vp_no_control(tot),
        fixed([sbar_subj,pc(tot)],no_passive),  
					% het leidt tot twijfel dat..
	                                % dat ik slaap leidt tot twijfel
        fixed([sbar_subj,er_pp(tot,X),extra_sbar(X)],no_passive),
					% dat ik slaap leidt ertoe dat ..
        fixed([sbar_subj,acc(twijfel)],no_passive),
                                        % het leidt geen/weinig/.. twijfel dat
	part_np_ld_pp(door),
	ld_pp,
	np_ld_pp,
	obj_np_er_pp_vp(tot),
	part_als_pred_np(op),
	% part_sbar_subj_so_np(in), ???
	part_intransitive(af),
	part_transitive(af),
	part_sbar(af),
	part_pc_pp(af,van),
	part_np_pc_pp(af,uit),
	part_pp_sbar(af,uit),
	part_np_pc_pp(af,van),
	part_transitive(binnen),
	part_transitive(in),
	part_transitive(om),
	part_transitive(op),
	part_np_pc_pp(op,in),
	part_intransitive(op),
	part_transitive(rond),
	part_transitive(uit),
	part_transitive(weg),
	part_np_ld_pp(weg),
	part_np_ld_pp(rond),
	part_transitive(voor),
	part_np_ld_pp(voor),
        part_np_np(voor),
        part_so_pp_np(voor),
	part_np_pc_pp(in,met),
	part_np_pc_pp(op,tot),
	part_np_pc_pp(op,voor),
	part_pc_pp(op,voor)])]).

v(lek,lekt,lekken,gelekt,lekte,lekten,
  [unacc([part_intransitive(uit),
	  part_sbar_subj(uit),
	  part_sbar(uit), 
	  part_sbar_subj_no_het_tpart(uit),
	  part_mod_pp(uit,over),
	  part_pc_pp(uit,naar)]),
   h([intransitive,
      sbar,
      transitive]),
   b([ld_pp,
      part_intransitive(weg)])]).

v(leng,lengt,lengen,gelengd,lengde,lengden,
    [h([intransitive, % de dagen lengen
	part_intransitive(aan),
	part_transitive(aan)])]).

v(lenig,lenigt,lenigen,gelenigd,lenigde,lenigden,
    [h([transitive])]).

v(lepel,lepelt,lepelen,gelepeld,lepelde,lepelden,
    [h([intransitive,
	transitive,
	part_transitive(op),
	part_transitive(uit),
	np_ld_pp,
	np_ld_dir,
	pc_pp(op)])]).

v(les,lest,lessen,gelest,leste,lesten,
    [h([intransitive,
	transitive])]).

v(let,let,letten,gelet,lette,letten,
    [h([transitive,
	part_intransitive(op),
	part_sbar(op),
	pc_pp(op),
	er_pp_sbar(op),
	er_pp_vp(op),
	part_mod_pp(op,met),
	part_pc_pp(op,op)])]).

v(leuk,leukt,leuken,geleukt,leukte,leukten,
    [h([part_transitive(op)])]).

v(leun,leunt,leunen,geleund,leunde,leunden,
    [h([ld_pp,
	ld_adv,
	intransitive,
	part_intransitive(achterover),
	part_ld_pp(aan),
	part_fixed_dep(aan,intransitive)  % wij laten ons het aanleunen
       ])]).

v(leur,leurt,leuren,geleurd,leurde,leurden,
    [h([intransitive,
	pc_pp(met)])]).


v(leuter,leutert,leuteren,geleuterd,leuterde,leuterden,
    [h([intransitive])]).

v(lever,levert,leveren,geleverd,leverde,leverden,
    [h([np_np,
	so_pp_np,
        np_mod_pp(bij),
	transitive,
	intransitive,
	fixed([{[acc(bijdrage),pc(tot)]}],norm_passive),
	fixed([{[acc(kritiek),pc(op)]}],norm_passive),
        fixed([{[pc(van),np_pred(illustratie)]}],no_passive),
	part_np_np(aan),
	part_so_pp_np(aan),
	part_np_np(op),
	part_np_np(over),
	part_intransitive(in),
	part_sbar(op),
	part_transitive(aan),
	part_transitive(af),
        part_transitive(door),
	part_transitive(in),
	part_transitive(mee),
	part_transitive(op),
	part_intransitive(op),
	part_transitive(over),
	part_transitive(uit),
	part_so_pp_np(over,aan),
	part_so_pp_sbar(over),
	part_np_pc_pp(uit,aan)])]).

v(liberaliseer,liberaliseert,liberaliseren,geliberaliseerd,liberaliseerde,liberaliseerden,
    [h([transitive,
	intransitive])]).

v(licht,licht,lichten,gelicht,lichtte,lichtten,
    [z([intransitive,
	ld_pp]),
     h([transitive,
	np_ld_pp,
	np_ld_dir,
	part_transitive(aan),
	part_intransitive(op),
	part_intransitive(voor), % we gaan vandaag voorlichten
	part_sbar(toe),
	fixed([[beentje],acc],norm_passive),
	fixed([[pootje],acc],norm_passive),
	fixed([[een,beentje],acc],norm_passive),
	fixed([[de,hand],pc(met)],imp_passive),
	part_transitive(door),
	part_transitive(in),
	part_np_pc_pp(in,over),
	part_acc_np_sbar(in),
	part_np_sbar(toe),
	part_transitive(op),
	part_transitive(toe),
	part_np_np(toe), % ik wil het u toelichten
	part_transitive(uit),
	part_transitive(voor),
	part_np_pc_pp(op,voor),
	part_np_pc_pp(voor,over)])]).


v(lieer,lieert,liëren,gelieerd,lieerde,lieerden,
    [z([refl_pc_pp(aan),
        refl_pc_pp(met)])]).
  

v(liefkoos,liefkoost,liefkozen,geliefkoosd,liefkoosde,liefkoosden,
    [h([transitive])]).

v(lieg,liegt,liegen,gelogen,loog,logen,
    [h([intransitive,
	sbar,
	part_np_np(voor),
	part_transitive(voor),
	part_np_sbar(voor),
	pc_pp(om),
	pc_pp(over),
	pc_pp(tegen),
        fixed([er_pp(aan),acc],norm_passive), % daar is geen letter aan gelogen
        fixed([er_pp(van),acc],norm_passive), % daar is geen letter aan gelogen
        transitive % de waarheid
       ])]).

v(lift,lift,liften,gelift,liftte,liftten,
    [b([intransitive,
        part_intransitive(mee),
	part_pc_pp(mee,op)
       ])]).

v(lig,ligt,liggen,gelegen,lag,lagen,
    [h([intransitive,
	so_np,
	nonp_copula,		% dat ligt gevoelig
	fixed([{[ap_pred(wakker),pc(van)]}],no_passive),
	nonp_copula_sbar,   % het ligt voor de hand dat ..
	nonp_copula_vp,     % het ligt voor de hand ..
	norm_passive,       % omdat de spulletjes in de etalage liggen uitgestald

        fixed([vc(lig,psp,intransitive),{[pc(aan),acc,dat]}],no_passive),
        fixed([vc(lig,psp,intransitive),extra_obj_vp(A,B),er_pp(aan,A),i(dat,B)],no_passive),
        fixed([vc(lig,psp,intransitive),extra_sbar(A),er_pp(aan,A),dat],no_passive),

        fixed([vc(sterf,op,intransitive)],no_passive),
        fixed([er_pp(bij),nonp_pred],no_passive), % het ligt er mooi bij
        part_fixed(bij,[nonp_pred,svp_er],no_passive),  % dat het er mooi bijlag
        part_intransitive(aan), % Toon: aanliggen aan ..
	part_intransitive(achter),
	part_intransitive(dwars),
	part_intransitive(klaar),
	part_intransitive(om),
	part_intransitive(onder),
	part_intransitive(vast),
	part_intransitive(voor),
	pc_pp(aan),  % dat ligt aan jou
	er_pp_sbar(in),
	subj_control(wk_te),    % hij ligt te klieren / hij komt liggen klieren (?)
	subj_control(pass_te),  % VL: omdat hij te slapen lag
        part_ld_er_transitive(bij), % zoals ze er bijliggen
        pp_sbar_subj(bovenop),  % het ligt er dik bovenop dat ...
	fixed([{[[ten,grondslag],pc(aan)]}],no_passive),
	fixed([sbar_subj,{[[ten,grondslag],pc(aan)]}],no_passive),
        fixed([vc(besluit,psp,intransitive),pc(in),sbar_subj_opt_het],no_passive),


	fixed([[braak]],no_passive),
	fixed([[gereed]],no_passive),
	fixed([[gereed],vp],no_passive),
	fixed([svp_pp(in,bedoeling)],no_passive),
	fixed([svp_pp(in,bedoeling),vp_subj],no_passive),
	fixed([svp_pp(in,bedoeling),sbar_subj],no_passive),
	fixed([[in,de,lijn,der,verwachting]],no_passive),
	fixed([[in,de,lijn,der,verwachting],sbar_subj],no_passive),
	fixed([[in,de,lijn,der,verwachting],vp_subj],no_passive),
	fixed([[in,de,lijn,der,verwachtingen]],no_passive),
	fixed([[in,de,lijn,der,verwachtingen],sbar_subj],no_passive),
	fixed([[in,de,lijn,der,verwachtingen],vp_subj],no_passive),
	fixed([[in,de,lijn]],no_passive),
	fixed([[in,de,lijn],sbar_subj],no_passive),
	fixed([[in,de,lijn],vp_subj],no_passive),	
	fixed([[in,de,verwachting],sbar_subj],no_passive),
	fixed([[in,de,verwachting],vp_subj],no_passive),	
        fixed([pp_pred(in,hand),pc(van)],no_passive), % zijn lot ligt in handen van ...
	fixed([[aan,het,hart],ap_pred(na),dat],no_passive),  % also "te na"
	fixed([[aan,het,hart],ap_pred(nauw),dat],no_passive),  % VL
	fixed([[aan,het,hart],dat],no_passive),
        fixed([er_pp(op),[vingerdik]],no_passive),
        fixed([er_pp(op,A),[vingerdik],extra_sbar(A)],no_passive),
	fixed([sbar_subj,[aan,het,hart],ap_pred(na),dat],no_passive), % also "te na"
	fixed([sbar_subj,[aan,het,hart],dat],no_passive),
	fixed([sbar_subj,pc(aan)],no_passive),
	fixed([sbar_subj,svp_pp(in,rede)],no_passive),
	fixed([vp_subj,svp_pp(in,rede)],no_passive),
	fixed([sbar_subj_no_het,yt(svp_pp(in,rede))],no_passive),
	fixed([vp_subj_no_het,yt(svp_pp(in,rede))],no_passive),
%	fixed([sbar_subj,svp_pp(voor,hand)],no_passive),
%	fixed([vp_subj,svp_pp(voor,hand)],no_passive),
%	fixed([sbar_subj_no_het,yt(svp_pp(voor,hand))],no_passive),
%	fixed([vp_subj_no_het,yt(svp_pp(voor,hand))],no_passive),
        fixed([vp_subj_no_het,yt(svp_pp(op,weg))],no_passive),
	%%% Alleen vond de rechter dat het niet op zijn weg lag een einde te maken aan deze discriminatie .
	fixed([{[pc(op),subj(nadruk)]}],norm_passive),
	fixed([{[er_pp(op,C),subj(nadruk)]},extra_sbar(C)],norm_passive),
	fixed([[in,het,verschiet]],no_passive),
	fixed([[klaar]],no_passive),
	fixed([[klaar],vp],no_passive),
	fixed([svp_pp(onder,vuur)],no_passive),
	fixed([[op,de,loer]],no_passive),
        fixed([[voor,anker]],no_passive),
	fixed([[wakker],pc(van)],no_passive),
	fixed([er_pp(van,C),[wakker],extra_sbar(C)],no_passive),
        part_intransitive(stil),
	part_ld_pp(vast),
	part_pc_pp(overhoop,met),
	part_pc_pp(voor,op)]),
     b([ld_adv,
	ld_pp
       ])]).

v(lijd,lijdt,lijden,geleden,leed,leden,
    [h([intransitive,
	% sbar_subj_np, % het lijdt geen twijfel dat ?
	fixed([acc(twijfel),sbar_subj],no_passive),
	fixed([{[acc(verlies),pc(op)]}],norm_passive),
	sbar,
	transitive,
	pc_pp(aan),
	pc_pp(onder),
	pc_pp(van)])]).

v(lijf,lijft,lijven,gelijfd,lijfde,lijfden,
    [h([part_transitive(in)])]).

v(lijk,lijkt,lijken,geleken,leek,leken,
    [h([aan_het,
	cleft,
	cleft_np,    % het lijken mij mooie schoenen
	copula,
	copula_np,
	copula_sbar,
	copula_vp,
	simple_aux_psp_zijn,  % omdat zijn dagen lijken weergekeerd
	passive,       % omdat zijn dagen lijken geteld
                       % omdat gefraudeerd leek met toegangskaarten
                       % omdat het wat ver lijkt gezocht dat ...
		       % omdat hiermee lijkt bewezen dat ..
	dat_passive,   % omdat zijn dagen mij lijken geteld
	te_passive,    % omdat hij lijkt te verslaan
	dat_te_passive,    % omdat hij mij lijkt te verslaan (ambigu!)
	intransitive,
	dip_sbar_subj, % we moeten , leek het , naar huis
	dip_sbar_subj_so_np_opt_het,
	alsof_sbar_subj,
	alsof_sbar_subj_so_np,
	van_sbar_subj_so_np_no_het, % Mij leek van wel
	so_copula,
	so_copula_sbar,
	so_copula_vp,
	so_copula_np,
	so_np,    % dat lijkt me niet
	er_pp_sbar(op),
        pc_pp(aan),
	pc_pp(naar),
	pc_pp(op),
        pc_pp(overheen),        % ik ben er nu wel overheen
        er_pp_sbar(overheen),   % ik ben er nog niet overheen dat ...
	aux(te),     % hij lijkt te slapen
	so_aux(te),  % hij lijkt me te slapen
        fixed([vc(lig,psp,intransitive),pc(aan),dat],no_passive),
        fixed([vc(lig,psp,intransitive),extra_obj_vp(A,B),er_pp(aan,A),i(dat,B)],no_passive),
        fixed([vc(lig,psp,intransitive),extra_sbar(A),er_pp(aan,A),dat],no_passive),
	fixed([[aan,de,hand]],no_passive),
        pp_vp_subj(aan),        % het is aan ons om...
        fixed([pc(tot),subj(aanleiding)],no_passive),
        fixed([pc(voor),subj(aanleiding)],no_passive),
	fixed([[de,baas],acc],no_passive), % ik lijk de stress niet de baas
	fixed([[te,baas],acc],no_passive), % VL
	fixed([pc(aan),[de,beurt]],no_passive),
        fixed([[bijster],[het,spoor]],no_passive),
        fixed([svp_er_pp(uit)],no_passive),
	fixed([{[pc(met),svp_pp(in,conflict)]}],no_passive),
	fixed([{[pc(met),pp_pred(in,contact)]}],no_passive),
	fixed([{[pc(voor),[als,de,dood]]}],no_passive),
	fixed([{[np_pred(dupe),pc(van)]}],no_passive),
	fixed([{[np_pred(dupe),er_pp(van,C)]},extra_sbar(C)],no_passive),
	fixed([{[[thuis],pc(in)]}],no_passive),
        part_fixed(toe,[er_pp(aan),ap_pred],no_passive),
        fixed([pp_pred(in,hand),pc(van)],no_passive),
	fixed([[in,omloop]],no_passive),
	fixed([svp_pp(in,conflict)],no_passive),
	fixed([[af],subj(kous)],no_passive),
        fixed([{[pc(van),np_pred(illustratie)]}],no_passive),
	    %% daar is geen discussie/twijfel over mogelijk
	fixed([{[pc(over),ap_pred(mogelijk)]}],no_passive),
	fixed([{[er_pp(over,A),ap_pred(mogelijk)]},extra_sbar(A)],no_passive),
	fixed([pc(van),ap_pred(op)],no_passive), % hij zou op van de zenuwen zijn / op zijn van de zenuwen
	fixed([pc(aan),ap_pred(na)],no_passive), % hij was na aan de waarheid 
        fixed([vc(schrijf,psp,intransitive),[op,het,lijf],dat],no_passive), 
        fixed([vc(schrijf,psp,intransitive),{[[op,het,lijf],pc(van)]}],no_passive), 
        fixed([pc(tot),subj(reden)],no_passive),
        fixed([er_pp(tot,C),subj(reden),extra_sbar(C)],no_passive),
        fixed([er_pp(tot,C),subj(reden),extra_vp(C)],no_passive),
	fixed([subj(rek),pc(uit)],no_passive),
	fixed([subj(sprake),pc(van)],no_passive),
	fixed([subj(sprake),er_pp(van,A),extra_sbar(A)],no_passive),
	fixed([[te,boven],acc],no_passive), 
	fixed([vc(doe,pass_te,intransitive),pc(om),dat],no_passive),
	fixed([vc(spreek,pass_te,intransitive),pc(over)],no_passive),
				% hij is daar helemaal niet over te spreken
	fixed([vc(spreek,pass_te,intransitive),er_pp(over,C),extra_sbar(C)],no_passive),
				% hij is daar helemaal niet over te spreken
	fixed([[terwille],dat],no_passive),
        fixed([pp_pred(van,invloed),pc(op)],no_passive),
        fixed([pp_pred(van,invloed),pc(op),dat],no_passive),
	fixed([[van,mening],sbar],no_passive),
	fixed([[van,mening],dat,sbar],no_passive),
	fixed([[van,oordeel],sbar],no_passive),
	fixed([[van,oordeel],dat,sbar],no_passive),
        fixed([[van,de,partij]],no_passive),
	fixed([[van,plan],acc],no_passive),
	fixed([[van,plan],vp],no_passive),
	fixed([[van,plan],{[acc,dat]}],no_passive),
	fixed([[van,plan],dat,vp],no_passive),
	fixed([[van,de,plan],acc],no_passive), % VL
	fixed([[van,de,plan],vp],no_passive), % VL
	fixed([[van,de,plan],{[acc,dat]}],no_passive), % VL
	fixed([[van,de,plan],dat,vp],no_passive), % VL
	fixed([[van,zin],acc],no_passive),
	fixed([[van,zin],vp],no_passive),
	fixed([[van,zin],acc],no_passive),
	fixed([[van,zin],dat,vp],no_passive)])]).

v(lijm,lijmt,lijmen,gelijmd,lijmde,lijmden,
    [h([intransitive,
	transitive,
	part_transitive(vast),
        part_ld_er_transitive(aan), % die heb ik er aangelijmd
	part_ld_pp(vast),
	np_ld_pp])]).

v(lijn,lijnt,lijnen,gelijnd,lijnde,lijnden,
    [h([part_transitive(aan),
	intransitive,
	part_transitive(uit),
	part_transitive(op)])]).

v(lijst,lijst,lijsten,gelijst,lijstte,lijstten,
    [h([transitive,
	part_intransitive(in),
	part_transitive(in)])]).

v(lik,likt,likken,gelikt,likte,likten,
    [h([intransitive,
	transitive,
	part_transitive(af),
	part_np_pc_pp(af,bij),  % hij likt zijn vingers er bij af
        part_refl_np_pc_pp(af,bij),  % zij likken zich daar de vingers bij af
	part_np_pc_pp(af,van),  % Vlaams: om duimen en vingers van af te likken
	part_transitive(op),
	pc_pp(aan)])]).

%% kan ik in X een foto liken?
v(like,liket,liken,geliket,likete,liketen,
    [h([transitive])]).

v(likkebaard,likkebaardt,likkebaarden,gellikebaard,likkebaarde,likkebaarden,
    [h([intransitive,
	pc_pp(van)])]).

v(likwideer,likwideert,likwideren,gelikwideerd,likwideerde,likwideerden,
    [h([transitive])]).

v(liquideer,liquideert,liquideren,geliquideerd,liquideerde,liquideerden,
    [h([transitive])]).

v(limiteer,limiteert,limiteren,gelimiteerd,limiteerde,limiteerden,
    [h([transitive,
	intransitive])]).

v(link,linkt,linken,gelinkt,linkte,linkten,
    [h([transitive,
	intransitive,
	ld_pp,
 	part_ld_pp(door),
	np_ld_pp,
	np_pc_pp(met),
 	part_np_ld_pp(door)
       ])]).

v(lispel,lispelt,lispelen,gelispeld,lispelde,lispelden,
    [h([intransitive,
	sbar,
	transitive,
	vp])]).

v(lob,lobt,lobben,gelobd,lobde,lobden,
    [h([transitive,
	np_ld_pp,
	np_ld_dir])]).

v(lobby,[lobbyt,lobbiet],[lobbyen,lobbieën],[gelobbyd,gelobbied],
    [lobbyde,lobbiede],[lobbyden,lobbieden],
    [h([intransitive,
	mod_pp(voor)])]).

v([localiseer, lokaliseer],
  [localiseert,lokaliseert],
  [localiseren,lokaliseren],
  [gelocaliseerd,gelokaliseerd],
  [localiseerde,lokaliseerde],
  [localiseerden,lokaliseerden],
    [h([transitive,
        intransitive])]).

v(loei,loeit,loeien,geloeid,loeide,loeiden,
    [h([intransitive])]).

v(loens,loenst,loenzen,geloensd,loensde,loensden,
    [h([intransitive])]).

v(loer,loert,loeren,geloerd,loerde,loerden,
    [h([intransitive,
	ld_pp,
	ld_adv,
	pc_pp(op)])]).

%% 
v(log,logt,loggen,[gelogd,gelogt],[logde,logte],[logden,logten],
    [h([transitive,
	part_transitive(in),
	part_pc_pp(in,op),
	part_transitive(uit)]),
     b([pc_pp(op),
        part_intransitive(in),
	part_intransitive(uit)])]).			% ik heb/ben ingelogd

v(logeer,logeert,logeren,gelogeerd,logeerde,logeerden,
    [h([transitive]),  % ouderwets "huisvesten"...; ook voor: "we krijgen de neefjes te logeren"
     b([ld_pp,
	ld_adv,
	intransitive])]).

v(logenstraf,logenstraft,logenstraffen,gelogenstraft,logenstrafte,logenstraften,
    [h([sbar_subj_np,
	transitive])]).

v(lok,lokt,lokken,gelokt,lokte,lokten,
    [h([intransitive,
	np_ld_dir,
	transitive,
	np_ld_pp,
	part_np_ld_pp(terug),
	part_transitive(terug),
	part_np_ld_pp(weg),
	part_transitive(weg),
	part_sbar_subj_np(uit), % het lokt reacties uit dat ...
        part_sbar_obj(uit),     % men lokt het uit dat ...
        part_sbar(uit),         % zo lokt men uit dat ...
	part_transitive(aan),
	part_transitive(uit)])]).

v(lonk,lonkt,lonken,gelonkt,lonkte,lonkten,
    [h([intransitive,
	pc_pp(naar)])]).

v(loochen,loochent,loochenen,geloochend,loochende,loochenden,
    [h([transitive,
	sbar])]).

v(loods,loodst,loodsen,geloodst,loodste,loodsten,
    [h([np_ld_dir,
	transitive,
        part_transitive(binnen),
	np_ld_pp,
	np_pc_pp(door)])]).

v(loof,looft,loven,geloofd,loofde,loofden,
    [h([intransitive,
	transitive,
        acc_np_dip_sbar,
	part_transitive(uit),
	part_np_pc_pp(uit,voor)])]).

v(loog,loogt,logen,geloogd,loogde,loogden,
    [h([transitive,
	part_transitive(uit)])]).

v(looi,looit,looien,gelooid,looide,looiden,
    [h([transitive])]).

v(loon,loont,lonen,geloond,loonde,loonden,
    [h([np_np,
	intransitive,
	transitive,
	vp_subj_np,
        vp_subj,
        sbar_subj_np,
        sbar_subj])]).

v(loop,loopt,lopen,gelopen,liep,liepen,
    [z([ld_dir,
        ap_copula,         % het liep anders; dat loopt fout
	pc_pp(op),         % de motor loopt op benzine
	fixed([subj(onderzoek),pc(naar)],no_passive),
	fixed([subj(rilling),pc(over),dat],no_passive),
				% de rillingen lopen ons over de rug/het lijf
	part_intransitive(aan),
	part_ld_transitive(af),  % want afloop is ook een noun, "als ik de mogelijkheden afloop"
	part_fixed(aan,[[rood]],no_passive),
	part_intransitive(achteruit),
	part_intransitive(af),
        part_ld_er_transitive(bij), % zoals ze er bijlopen
	part_intransitive(binnen),
	part_ld_pp(binnen),
	part_intransitive(dood),
	part_ld_pp(door),
	part_intransitive(heen),
	part_intransitive(in),
	part_ld_pp(in),
	part_pc_pp(in,op),  % we lopen in op de koploper
	part_intransitive(langs),
	part_ld_pp(langs),
	part_intransitive(leeg),
	part_intransitive(los),
	part_ld_pp(mee), % NB dat ik mee naar huis loop
	part_intransitive(mis),
	part_intransitive(om),  % ik ga niet omlopen
	part_intransitive(onder),
	part_intransitive(op),
	part_ld_pp(op),
	part_intransitive(over),
	part_ld_pp(over),
	part_pc_pp(over,van),        % hij liep over van enthousiasme
	part_intransitive(stuk),
	part_pc_pp(stuk,op),         % het plan liep stuk op geldgebrek
	part_so_np(tegemoet),
	part_intransitive(terug),
	part_ld_pp(terug),
	part_intransitive(toe),
	part_ld_pp(toe),
	part_intransitive(uit),
	part_ld_pp(uit),
	part_pc_pp(uit,op),          % het plan liep op niets uit
	part_er_pp_sbar(uit,op),          % het plan liep er op uit dat ..
	part_intransitive(uiteen),
	part_intransitive(vast),
	part_mod_pp(vast,in),
	part_intransitive(vol),
	part_intransitive(voorbij),
	part_transitive(voorbij),
	part_intransitive(vooruit),
	part_pc_pp(vooruit,op),      % hij liep vooruit op de beslissing
        part_intransitive(warm),
	part_intransitive(weg),
	part_pc_pp(weg,voor),        % hij loopt weg voor de problemen
	fixed([[tegen,het,lijf],dat],no_passive),
	fixed([[in,de,gaten]],no_passive),
	fixed([[in,de,gaten],sbar_subj],no_passive),
	fixed([[in,de,smiezen]],no_passive),
	fixed([[in,de,smiezen],sbar_subj],no_passive),
	fixed([[in,het,honderd]],no_passive),
	fixed([[op,de,klippen]],no_passive),
	fixed([[in,de,papieren]],no_passive),
	fixed([[hand,in,hand]],no_passive),
	part_fixed(af,[[met,een,sisser]],no_passive),
	fixed([[spaak]],no_passive),
        fixed([[te,hoop]],no_passive),
        fixed([[te,hoop],pc(tegen)],no_passive),
	fixed([[ten,einde]],no_passive),
	fixed([[teneinde]],no_passive),
	fixed([[van,een,leien,dakje]],no_passive),
	fixed([[uit,de,pas]],no_passive),
	fixed([[uit,de,hand]],no_passive)]),
     h([ap_pred_np,                  % hij loopt zijn schoenen stuk
	transitive,                  % ik heb de marathon gelopen
	part_intransitive(achter),   % het horloge heeft achtergelopen
	part_ld_pp(aan,achter),      % we hebben achter de feiten aangelopen
	part_so_np(af),              % 
	part_refl(dood),             % ik heb me doodgelopen
	part_transitive(kapot),	% hij heeft zijn schoenen kapotgelopen
	part_intransitive(school),   % VLAAMS ik heb school gelopen met hem
        part_intransitive(trap),
	part_refl(vast),             % Robben liep zich weer vast
        part_refl(warm),
	part_intransitive(hard),     % ik heb hardgelopen
	part_pc_pp(rond,met),        % ik heb lang rondgelopen met dat plan
	part_intransitive(voorop),   % ik heb vooropgelopen
	part_transitive(omver),      % ik heb hem omvergelopen
	subj_control(wk_te),         % hij loopt te vervelen
	part_transitive(op),         % we lopen hondenbeten op

        fixed([svp_acc(storm)],no_passive),  % het loopt nog geen storm
        fixed([{[svp_acc(storm),pc(tegen)]}],no_passive),
                                % het loopt nog geen storm
        
        part_intransitive(storm),    % het zal wel stormlopen
        part_pc_pp(storm,tegen),     % Chirac heeft stormgelopen tegen Europa
        
	fixed([acc(gevaar)],no_passive),
	fixed([acc(gevaar),vp],no_passive),
	fixed([acc(gevaar),sbar],no_passive),
        fixed([[spitsroeden]],no_passive),
	fixed([acc(risico)],no_passive),
	fixed([acc(risico),vp],no_passive),
	fixed([acc(risico),sbar],no_passive),
	fixed([[uit,de,sloffen],[het,vuur],refl],no_passive),
	fixed([[in,de,weg],dat],no_passive),
	fixed([{[[in,de,pas],pc(met)]}],imp_passive),
	fixed([{[[te,koop],pc(met)]}],imp_passive),
	fixed([{[[te,koop],er_pp(met,C)]},extra_sbar(C)],imp_passive),
	fixed([[in,de,weg]],no_passive),
	fixed([{[[de,kantjes],er_pp(vanaf)]}],imp_passive),
	fixed([[onder,de,voet],acc],norm_passive),
	fixed([[voor,de,voeten],dat],no_passive)]),
     b([intransitive,                % ik heb gelopen / ?hij is gelopen
	ld_adv,			     % we hebben daar gelopen
	ld_pp,
	part_transitive(af),         % ik ben/heb het parcours afgelopen
	part_transitive(in),         % ik heb de schoenen ingelopen
        part_intransitive(mee),
	part_transitive(mis),        % ik heb/ben haar misgelopen
	part_transitive(na),         % ik heb/ben haar nagelopen
	part_transitive(over),       % ik ben/heb de brug overgelopen
	part_intransitive(rond),     % ik ben/heb rondgelopen
	part_ld_pp(rond),	% ik ben/heb rondgelopen in het bos
	part_intransitive(school),
	part_ld_pp(school),
	part_refl(stuk),             % hij loopt zich stuk
        part_intransitive(stuk),     % de aanvallen liepen stuk
	part_refl_pc_pp(stuk,op),    % hij loopt zich stuk op de verdediging
        part_pc_pp(stuk,op),         % de aanvallen liepen stuk op
	part_transitive(uit),        % ik heb de marathon uitgelopen
	part_transitive(door)])]).   % ik heb/ben alles doorgelopen

v(loos,loost,lozen,geloosd,loosde,loosden,
  [h([intransitive,
      np_ld_pp,
      transitive])]).

v(loot,loot,loten,geloot,lootte,lootten,
    [h([intransitive,
	transitive,
	part_transitive(in),
	part_transitive(uit),
	pc_pp(om)])]).

v(los,lost,lossen,gelost,loste,losten,
    [unacc([part_intransitive(op),
            part_pc_pp(op,in)]),
     h([intransitive,
	transitive,
	part_refl(op),
        np_pc_pp(over),     % VL: een woord lossen over ...
	part_transitive(af),
	part_intransitive(af),
	part_transitive(in),
	part_transitive(op),
        part_mod_pp(op,met), % hier kunnen we de problemen goed mee oplossen
	part_np_pc_pp(op,in)])]).

v(louter,loutert,louteren,gelouterd,louterde,louterden,
    [h([transitive,
        intransitive])]).

v(lucht,lucht,luchten,gelucht,luchtte,luchtten,
    [h([intransitive,
	transitive,
	part_intransitive(op),
	part_sbar_subj_so_np(op),
	part_transitive(op)])]).

v(luid,luidt,luiden,geluid,luidde,luidden,
    [h([intransitive,
	transitive,
	ap_copula,  % hoe luidt de naam van ...
	sbar, % only dips?
	part_sbar_subj_np(in),  % ??
	part_transitive(in),
        part_transitive(uit)])]).

v(luier,luiert,luieren,geluierd,luierde,luierden,
    [h([intransitive])]).

v(luis,luist,luizen,geluisd,luisde,luisden,
    [h([np_pc_pp(in),        % ik ben erin geluisd; hij heeft ons erin geluisd
        part_np_pc_er_transitive(in)])]).  % ik ben er ingeluisd

v(luister,luistert,luisteren,geluisterd,luisterde,luisterden,
    [h([intransitive,
        part_intransitive(mee),
        part_intransitive(toe),
	part_transitive(af),
	part_intransitive(af),
	part_transitive(op),
	pc_pp(naar),
	part_pc_pp(mee,naar),
        transitive])]).

v(luk,lukt,lukken,gelukt,lukte,lukten,
  [unacc([intransitive,
          pc_pp(in),		% VL: daar ben ik aardig in gelukt
	  er_pp_vp(in),         % VL: We zijn er nooit in gelukt ons eigen spel op te dringen .
          so_np,
	  sbar_subj,
	  sbar_subj_so_np,
          vp_subj,
          vp_subj_so_np])]).

v(lul,lult,lullen,geluld,lulde,lulden,
    [h([intransitive,
        mod_pp(doorheen),
        pc_pp(tegen),
	part_transitive(verder),  % als je het niet verder lult, ...
	refl_ld_pp,  % hoe hij zich daar uitlult, hij lulde zich naar binnen
	ap_pred_np,  % iemand murw lullen
        part_pc_pp(aan,tegen),
	part_pc_pp(in,op),
	pc_pp(over)])]).

v(lummel,lummelt,lummelen,gelummeld,lummelde,lummelden,
    [h([intransitive])]).

v(lunch,luncht,lunchen,geluncht,lunchte,lunchten,
    [h([intransitive])]).

v(lurk,lurkt,lurken,gelurkt,lurkte,lurkten,
    [h([intransitive])]).

v(lust,lust,lusten,gelust,lustte,lustten,
  [h([transitive,
      fixed([er_pp(van)],no_passive), % ze zullen ervan lusten!
      fixed([subj(hond),er_pp(van),[geen,brood]],no_passive),
      fixed([{[[pap],er_pp(van)]}],no_passive)
     ])]).

v(luw,luwt,luwen,geluwd,luwde,luwden,
  [unacc([intransitive,
	  transitive  % we luwen de storm van onze manager 
	 ])]).

v(lynch,lyncht,lynchen,gelyncht,lynchte,lynchten,
    [h([transitive])]).

v(maai,maait,maaien,gemaaid,maaide,maaiden,
    [h([intransitive,
	transitive,
	part_transitive(af),
	np_ld_pp,
	pc_pp(met)])]).

v(maak,maakt,maken,gemaakt,maakte,maakten,
    [h([transitive,
	fixed_dep(intransitive),  % we hebben met de koningin te maken
	ap_pred_refl,  % hij maakt zich kwaad
	np_aan_het,    % iemand aan het lachen/huilen maken
	pred_np,       % ik maak de deur groen
	pred_np_sbar,  % ik maak het acceptabel dat je komt
	pred_np_vp,    % ik maak het acceptabel om de boeken te lezen
        so_pred_np,    % ik maak jullie het leven zuur
	su_ap_pred_sbar,  % het maakt ons vrolijk dat je komt
	su_ap_pred_vp,    % het maakt ons bang de wedstrijd te verliezen
	ap_copula,        % liefde maakt blind
	refl_np,          % zich zorgen/illusies maken 
	sbar,
	sbar_obj,         % anglicisme? De illustraties van Sarah Horne maken het dat de personages tot leven komen
	so_pred_sbar,
	so_pred_vp,
        pp_so_pred_np,   % ik maak aan jullie de beslissing duidelijk
	pp_so_pred_sbar, % ik maak aan jullie duidelijk dat...
	pp_so_pred_vp,   % ik maak aan juliie duidelijk VP
        part_transitive(blij),
        part_transitive(duidelijk), % omdat ik de belissing zou duidelijk maken
        part_vp(duidelijk),         % omdat ik wilde duidelijk maken te komen
        part_sbar(duidelijk),       % omdat ik wilde duidelijk maken dat...
        part_np_np(duidelijk),
        part_np_vp_obj(duidelijk),
        part_np_sbar(duidelijk),
        part_so_pp_np(duidelijk), % ik zou aan jullie de beslissing hebben d g
        part_so_pp_sbar(duidelijk), 
        part_so_pp_vp(duidelijk), % ik zou aan jullie hebben duidelijk gemaakt..
	np_mod_pp(bij),   % hij heeft er tekeningen/muziek/... bij gemaakt
	np_pc_pp(tot),
	np_pc_pp(uit),
	np_pc_pp(van),
	fixed([{[acc(gebruik),er_pp(van,C)]},extra_sbar(C)],norm_passive),
	fixed([{[acc(misbruik),er_pp(van,C)]},extra_sbar(C)],norm_passive),	
	fixed([{[acc(punt),er_pp(van,C)]},extra_sbar(C)],norm_passive),	
	% ik maak het er niet beter op
	fixed([er_pp(op),compar,acc],norm_passive),
	% hij heeft het zichzelf er niet makkelijker op gemaakt
	fixed([er_pp(op),compar,{[acc,dat]}],norm_passive),
	% de resultaten maken het er (voor) Berlusconi niet gemakkelijk(er) op
	fixed([er_pp(op),ap_pred,{[acc,dat_pp(voor)]}],norm_passive),
	fixed([ap_pred(sterk),refl,sbar],no_passive),  % ik maak me sterk dat...
	fixed([ap_pred(sterk),refl,vp],no_passive),  % VL ik maak me sterk om...
	part_np_np(bekend),
	part_np_mod_pp(bekend,over),
        part_refl_vp(op),  % hij maakt zich op om...
	part_np_np(over),
	part_np_np(uit),
	part_np_np(wijs),
	part_intransitive(buit),  %VL
        part_intransitive(gelijk),
	part_transitive(gelijk),
        part_fixed(gelijk,[[met,de,grond],acc],norm_passive),
	part_intransitive(kennis),
        part_intransitive(mee),  %we hebben nooit anders meegemaakt
	part_intransitive(plaats),
	part_intransitive(schoon),
	part_intransitive(uit),
	part_intransitive(vast),
	part_intransitive(voort),
	part_np_sbar(wijs),
	part_pc_pp(gebruik,van),  % je moet van de gelegenheid gebruikmaken
	part_fixed(gebruik,[[geen],pc(van)],imp_passive),
	                          % er werd geen gebruikgemaakt van ...
%%	fixed([{[acc(gebruik),pc(van)]}],norm_passive), % geen gebruik werd ..
        %% subsumed by np_pc_pp(van)

	fixed([{[pc(over),ap_pred(druk)]},refl],no_passive),
				% hij maakt zich er niet druk over
	fixed([{[er_pp(over,C),ap_pred(druk)]},refl,extra_sbar(C)],no_passive),
				% hij maakt zich er niet druk over of hij komt
        part_refl(druk),
	fixed([{[pc(om),ap_pred(druk)]},refl],no_passive),
				% hij maakt zich er niet druk om
	fixed([{[er_pp(om,C),ap_pred(druk)]},refl,extra_sbar(C)],no_passive),
				% hij maakt zich er niet druk om of hij komt
	part_fixed(druk,[pc(over),refl],no_passive),
				% hij wil zich er niet over drukmaken
	part_fixed(druk,[er_pp(over,C),refl,extra_sbar(C)],no_passive),
				% hij wil zich er niet over drukmaken of hij komt
	fixed([{[pc(over),ap_pred(sappel)]},refl],no_passive),
	fixed([{[er_pp(over,C),ap_pred(sappel)]},refl,extra_sbar(C)],no_passive),
	fixed([{[pc(om),ap_pred(sappel)]},refl],no_passive),
	fixed([{[er_pp(om,C),ap_pred(sappel)]},refl,extra_sbar(C)],no_passive),
        fixed([{[er_pp(van,C),acc(geheim)]},extra_sbar(C)],norm_passive),
        fixed([{[er_pp(van,C),acc(geheim)]},extra_vp(C)],norm_passive),
        fixed([{[[komaf],pc(met)]}],imp_passive),
	fixed([er_pp(naar),het_obj1],no_passive),
	fixed([{[acc(verwijt),dat]}],norm_passive),
	part_refl(gereed),
	part_refl(klaar),
	part_refl(waar),
	part_sbar_subj_so_np(kapot),
	part_sbar_subj_so_np(uit),
	part_sbar_subj(uit),        % het maakt niet uit of je komt
        part_sbar_subj_np(uit),     % het maakt een groot verschil uit of ...
        part_sbar_subj_np_np(uit),  % wat maakt het mij uit of ...
	part_sbar(bekend),
	part_sbar(goed),
	part_sbar(mee),
	part_sbar_obj(mee),   % we maken het nog mee dat ..
	part_sbar(uit),
	part_sbar(waar),
	part_sbar(wijs),
	part_so_pp_np(wijs),
	part_so_np(uit),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(bekend),
	part_transitive(buit),
	part_transitive(dood),
	part_transitive(door),
	part_transitive(gereed),
	part_transitive(goed),
        part_transitive(hard),
        part_sbar(hard),
	part_transitive(kapot),
	part_transitive(klaar),
	part_transitive(los),
	part_transitive(mee),
        part_np_pc_pp(mee,van),  % daar heb ik staaltjes van meegemaakt
	part_transitive(na),
	part_transitive(nat),
	part_transitive(op),
	part_np_mod_pp(op,over),  % verslag opmaken over
	part_np_mod_pp(op,van),  % verslag opmaken van
	part_transitive(open),
	part_transitive(over),
	part_transitive(schoon),
	part_transitive(uit),
	part_transitive(vast),
	part_transitive(vuil),
	part_fixed(vuil,[{[acc(vuil),pc(aan)]}],norm_passive),
	part_transitive(vrij),
	part_transitive(waar),
        part_transitive(wakker),
	part_transitive(wijs),
	part_vp_subj_so_np(kapot),
	part_vp(bekend),
	part_vp(wijs),
	fixed([{[acc(aanmerking),pc(op)]}],norm_passive),
	fixed([{[acc(aanspraak),pc(op)]}],norm_passive),
	fixed([[afhandig],{[acc,dat]}],imp_passive),
	fixed([[afhandig],{[acc,dat_pp(van)]}],imp_passive),
	fixed([{[acc(afspraak),pc(over)]}],norm_passive),
	fixed([{[acc(begin),pc(met)]}],norm_passive),
	fixed([{[pc(tegen),acc(bezwaar)]}],imp_passive),
	fixed([{[er_pp(tegen,C),acc(bezwaar)]},extra_vp(C)],imp_passive),
	fixed([{[er_pp(tegen,C),acc(bezwaar)]},extra_sbar(C)],imp_passive),
	fixed([[de,blits]],imp_passive),
        fixed([{[[deelgenoot],acc,pc(van)]}],norm_passive),
	part_refl_np(eigen),
	fixed([{[acc(eind),pc(aan)]}],norm_passive),
	fixed([{[acc(einde),pc(aan)]}],norm_passive),
	fixed([[furore]],imp_passive),
	fixed([{[acc(haast),pc(met)]}],norm_passive),
	fixed([{[acc(herrie),pc(om)]}],imp_passive),
	fixed([{[acc(herrie),pc(over)]}],imp_passive),
        fixed([[het,hof],dat],norm_passive),
	fixed([{[pc(op),acc(jacht)]}],imp_passive),
	fixed([{[pc(op),acc(kans)]}],imp_passive),
	fixed([{[er_pp(op,X),acc(kans)]},extra_sbar(X)],imp_passive),
	fixed([{[er_pp(op,X),acc(kans)]},extra_vp(X)],imp_passive),
	fixed([{[acc(inbreuk),pc(op)]}],no_passive),
	fixed([{[acc(indruk),pc(op)]}],no_passive),
        fixed([ap_pred('in orde'),acc],norm_passive),
	fixed([[kenbaar],acc],norm_passive),
	fixed([[kenbaar],{[acc,dat]}],norm_passive),
	fixed([[kenbaar],dat,sbar],imp_passive),
	fixed([[kenbaar],dat,vp],imp_passive),
	fixed([[kenbaar],sbar],imp_passive),
	fixed([[kenbaar],vp],imp_passive),
	fixed([[kenbaar],dat_pp(aan),sbar],imp_passive),
	fixed([[kenbaar],dat_pp(aan),vp],imp_passive),
	fixed([[kenbaar],{[acc,dat_pp(aan)]}],norm_passive),
	part_transitive(kenbaar),
	part_np_np(kenbaar),
	part_np_sbar(kenbaar),
	part_vp(kenbaar),
	part_np_vp_subj(kenbaar),
	part_sbar(kenbaar),
	part_so_pp_sbar(kenbaar),
	part_so_pp_vp(kenbaar),
	part_so_pp_np(kenbaar),
	fixed([{[pc(met),acc(kennis)]}],imp_passive),
	fixed([[korte,metten]],imp_passive),
	fixed([{[pc(met),[korte,metten]]}],imp_passive),
	part_refl_pc_pp(meester,van),  % ... heeft meester gemaakt
	fixed([[meester],pc(van),refl],no_passive), % ... maakt zich er snel meester van 
%	fixed([{[acc(melding),pc(van)]}],norm_passive),subsumed: np_pc_pp(van)
	fixed([{[acc(voorstelling),pc(van)]},refl],no_passive),
	fixed([[naam]],imp_passive),
	fixed([[naam],als_pred],imp_passive),
	fixed([{[acc(inbreuk),pc(op)]}],no_passive),
	fixed([{[acc(onderscheid),pc(tussen)]}],no_passive),
	fixed([{[acc(opmerking),pc(over)]}],norm_passive),
	fixed([[een,pas,op,de,plaats]],imp_passive),
	fixed([[pas,op,de,plaats]],imp_passive),
	fixed([{[[plaats],pc(voor)]}],imp_passive),
	fixed([[plaats]],imp_passive),
        fixed([[rechtsomkeer]],imp_passive),
        fixed([[rechtsomkeert]],imp_passive),
	fixed([{[acc(ruzie),pc(met)]}],imp_passive),
	fixed([{[acc(ruzie),pc(over)]}],imp_passive),
        % fixed([{[[schuldig],pc(aan)]},refl],no_passive),
        % particle order: dat hij zich heeft schuldig gemaakt ...
        part_refl_pc_pp(druk,om),
        fixed([[schoon,schip],pc(met)],imp_passive),
        part_refl_pc_pp(schuldig,aan),
	fixed([{[acc(sier),mod_pp(met)]}],norm_passive),  % hij maakte er goede sier mee ...
	fixed([{[[staat],pc(op)]}],imp_passive),
	fixed([[te,gelde],acc],norm_passive),
	fixed([[te,niet],acc],norm_passive),
	fixed([[te,schande],acc],norm_passive),
	fixed([{[er_pp(bij),acc(tekening)]}],norm_passive),
	fixed([{[er_pp(bij),acc(tekening)]},vp_no_control],norm_passive),
	fixed([{[er_pp(bij),acc(tekening)]},sbar],norm_passive),
	fixed([{[er_pp(bij),acc(tekening_DIM)]}],norm_passive),
	fixed([{[er_pp(bij),acc(tekening_DIM)]},vp_no_control],norm_passive),
	fixed([{[er_pp(bij),acc(tekening_DIM)]},sbar],norm_passive),
	fixed([{[acc(tijd),mod_pp(voor)]}],norm_passive),
	fixed([{[acc(toespeling),pc(op)]}],norm_passive),
	fixed([[tot,voorwerp],{[acc,pc(van)]}],norm_passive),
	fixed([[uit,de,voeten],refl],no_passive),
        fixed([{[acc(uitzondering),pc(op)]}],norm_passive),
        fixed([pc(vanaf),refl],no_passive),
	fixed([{[acc(zorg),pc(over)]},refl],no_passive),
	fixed([{[acc(zorg),er_pp(over,X)]},refl,extra_sbar(X)],no_passive),
	fixed([{[acc(zorg),pc(om)]},refl],no_passive),
	fixed([{[acc(zorg),er_pp(om,X)]},refl,extra_sbar(X)],no_passive),
	fixed([acc(zorg),refl,sbar],no_passive),
	part_ld_pp(vast),
	part_np_pc_pp(los,van),
	part_np_ld_pp(vast),
	part_np_pc_pp(af,op),
	part_np_pc_pp(bekend,met),
	part_np_pc_pp(gereed,voor),
	part_np_pc_pp(op,uit),
	part_pp_sbar(op,uit),
	part_so_pp_np(over,aan),
	part_np_pc_pp(over,naar),
	part_np_pc_pp(over,op),
	part_np_pc_pp(uit,van),
	part_np_pc_pp(uit,voor),
	part_np_pc_pp(vrij,van),
	part_np_pc_pp(vrij,voor),
	part_pc_pp(kennis,met),
	part_pc_pp(plaats,voor),
	part_refl_pc_pp(af,van),
	part_refl_pc_pp(gereed,voor),
%	part_refl_pc_pp(los,van),
	part_refl_pc_pp(op,voor),
	part_refl_pc_pp(vrij,voor),
	part_transitive(zwart), % omdat ze mij heeft zwart gemaakt bij ...
        part_fixed(uit,[{[acc(deel),pc(van)]}],norm_passive)])
    ]).

v(maal,maalt,malen,[gemaald,gemalen],maalde,maalden,
    [h([intransitive,
	pc_pp(om),
        er_pp_vp(om),
	pc_pp(op),
	pc_pp(over),
	transitive,
	part_ld_pp(uit),  % de molen maalde uit op het IJsselmeer
	part_np_ld_pp(uit),  % de molen maalde uit op het IJsselmeer
	part_transitive(uit),
	part_transitive(fijn),
	np_ld_pp  % peper over het gerecht
       ])]).

v(maan,maant,manen,gemaand,maande,maanden,
    [h([np_vp_obj1,
	np_pc_pp(om),
	pc_pp(tot),
        part_transitive(aan),
        part_intransitive(aan),
        part_np_vp_obj1(aan),
        part_vp_no_control(aan),
        sbar,    % dip
        np_sbar, % dip
	np_pc_pp(tot)])]).

v(machtig,machtigt,machtigen,gemachtigd,machtigde,machtigden,
    [h([transitive,
	np_vp_obj1,
	np_pc_pp(tot)])]).

v(mag,mag,mogen,gemogen,mocht,mochten,moge,
    [h([intransitive,
	ld_dir,
        uit,
        sbar_subj, % VL: het mag niet dat de zakken zo vlug scheuren
	pp_copula(aan,slag),
	pp_copula(aan,werk),
        part_intransitive(aan),
        part_intransitive(af),
        part_intransitive(uit),  % de Dijk: mag de muziek uit?
	transitive_ndev_ndev_npas,
	part_intransitive(terug),
	part_ld_pp(terug),
	part_transitive(terug),
        nonp_pred_np_ndev,       % ik mag een uniform aan / een pet op / etc
	modifier(aux(inf)),
	aux_pc_pp(inf,van),     % ik mag niet zeuren van mijn vrouw
        passive,		% daar mag niet aan getornd (VLAAMS?)
	pc_pp(van),		% dat mag niet van de directeur
	np_pc_pp(van),          % ik mag dat niet van de directeur
	ld_adv,
	ld_pp])]).

v(mail,mailt,mailen,gemaild,mailde,mailden,
  [h([transitive,
      np_np, % ik heb hem de adressen gemaild
      intransitive,
      sbar,
      mod_pp(over),
      np_mod_pp(over),
      part_transitive(door),
      part_intransitive(terug),
      part_transitive(terug),
      part_sbar(terug),
      part_acc_np_sbar(terug),
      acc_np_sbar])]).

%% we hebben geen cent te makken
v(mak,makt,makken,gemakt,makte,makten,
    [h([transitive])]).  

v(manage,managet,managen,gemanaged,managede,manageden,
    [h([intransitive,
	transitive])]).

v(mangel,mangelt,mangelen,gemangeld,mangelde,mangelden,
  [h([transitive,
      intransitive,		% ik wil weten wat er schort
      er_pp_sbar(aan),
      pc_pp(aan)])]).

v(manifesteer,manifesteert,manifesteren,gemanifesteerd,manifesteerde,manifesteerden,
    [h([refl,
	refl_pc_pp(in),
        transitive,  % (*) ze wilden hun vrijheid manifesteren 
        intransitive])]).

v(manipuleer,manipuleert,manipuleren,gemanipuleerd,manipuleerde,manipuleerden,
    [h([intransitive,
	transitive,
	np_pc_pp(met),
	pc_pp(met)])]).

v(mank,mankt,manken,gemankd,mankte,mankten,
    [h([intransitive,
	transitive])]).

v(mankeer,mankeert,mankeren,gemankeerd,mankeerde,mankeerden,
    [h([intransitive,
	sbar,
	so_np,
	transitive,
	np_pc_pp(aan),
	pc_pp(aan)])]).

v(manoeuvreer,manoeuvreert,manoeuvreren,gemanoeuvreerd,manoeuvreerde,manoeuvreerden,
    [h([intransitive,
	transitive,
	np_ld_pp,
	np_ld_dir,
	pc_pp(met)])]).

v(marcheer,marcheert,marcheren,gemarcheerd,marcheerde,marcheerden,
    [h([part_transitive(af)]), % Dan werden de gevangenen weer naar het kamp afgemarcheerd
     b([intransitive,
        ld_pp,
        ld_dir, % Ondertussen marcheren ME-troepen het stadion binnen .
        part_intransitive(aan), % ze komen aangemarcheerd
        part_intransitive(af),
        part_intransitive(op),
	part_ld_pp(op)])]).

v(marginaliseer,marginaliseert,marginaliseren,gemarginaliseerd,marginaliseerde,
  marginaliseerden,
    [h([transitive,
	intransitive])]).

v(marineer,marineert,marineren,gemarineerd,marineerde,marineerden,
    [h([transitive])]).

v(markeer,markeert,markeren,gemarkeerd,markeerde,markeerden,
    [h([transitive,
        als_pred_np,
	np_pc_pp(met)])]).

v(market,market,marketen,gemarket,markette,marketten,
    [h([transitive,
        intransitive])]).

v(martel,martelt,martelen,gemarteld,martelde,martelden,
    [h([intransitive,
	transitive,
	part_transitive(dood),
	pc_pp(met),
	pc_pp(op)])]).

v(maskeer,maskeert,maskeren,gemaskeerd,maskeerde,maskeerden,
    [h([transitive,
	sbar])]).

v(masker,maskert,maskeren,gemaskerd,maskerde,maskerden,
    [h([transitive])]).

v(masseer,masseert,masseren,gemasseerd,masseerde,masseerden,
    [h([intransitive,
	part_transitive(weg),
	transitive])]).

v(master,mastert,masteren,gemasterd,masterde,masterden,
    [h([transitive])]).

v(masturbeer,masturbeert,masturberen,gemasturbeerd,masturbeerde,masturbeerden,
    [h([intransitive,
        refl   %Vlaams
       ])]).

v(mat,mat,matten,gemat,matte,matten,
    [h([part_transitive(af),
	transitive])]).

v(match,matcht,matchen,gematcht,matchte,matchten,
    [h([intransitive,
	transitive,
	np_pc_pp(met)])]).

v(matig,matigt,matigen,gematigd,matigde,matigden,
    [h([intransitive,
	transitive,
	part_refl_np(aan)])]).

v(mats,matst,matsen,gematst,matste,matsten,
    [h([transitive])]).

v(mauw,mauwt,mauwen,gemauwd,mauwde,mauwden,
    [h([intransitive])]).

v(maximaliseer,maximaliseert,maximaliseren,gemaximaliseerd,maximaliseerde,maximaliseerden,
    [h([transitive])]).

v(meander,meandert,meanderen,gemeanderd,meanderde,meanderden,
    [h([intransitive])]).

v(mechaniseer,mechaniseert,mechaniseren,gemechaniseerd,mechaniseerde,mechaniseerden,
    [h([intransitive,
	transitive])]).

v(mediteer,mediteert,mediteren,gemediteerd,mediteerde,mediteerden,
    [h([intransitive,
	pc_pp(over)])]).

v(meediscussieer,meediscussieert,meediscussiëren,meegediscussieerd,meediscussieerde,meediscussieerden,
    [h([intransitive,
	mod_pp(met),
	sbar,
	pc_pp(over)])]).

v(meen,meent,menen,gemeend,meende,meenden,
    [h([sbar,
	transitive,
	van_sbar,
        np_pc_pp(van), % daar menen we niets/geen woord van
	vp,
	subj_control(te)])]).  % ?

v(meer,meert,meren,gemeerd,meerde,meerden,
    [h([intransitive,
	part_intransitive(aan),
	part_transitive(aan),
	part_intransitive(af),
	part_transitive(af),
	transitive
       ])]).

v(meesmuil,meesmuilt,meesmuilen,gemeesmuild,meesmuilde,meesmuilden,
    [h([intransitive,
	sbar])]).

v(meet,meet,meten,gemeten,[mat,meette],[maten,meetten],
    [h([intransitive,
	sbar,
	meas,   % hij meette 2.10
	transitive,
	part_transitive(aan),
	part_transitive(door),
	part_np_np(aan),
	part_np_np(toe),
	part_refl_np(aan),
	part_transitive(af),
	part_sbar(af),
	part_transitive(op),
	part_transitive(uit),
	part_sbar(uit),  % breed uitmeten in de pers dat ...
	refl_pc_pp(met),
	part_np_pc_pp(af,aan),
	part_np_pc_pp(af,van),
	part_np_pc_pp(af,naar)])]).

v(mekker,mekkert,mekkeren,gemekkerd,mekkerde,mekkerden,
    [h([intransitive])]).

v(meld,meldt,melden,gemeld,meldde,meldden,
    [h([ap_pred_np,
        ap_pred_refl,
        als_pred_refl,
        als_pred_np,
	refl, % necc. refl because: "er hebben zich 10 leden gemeld"
        refl_ld_pp,
        part_refl(ziek),
        part_transitive(ziek),
	sbar,
	vp,
	np_vp_subj,
	sbar,
	np_sbar,
	so_pp_sbar,
	transitive,
	np_np,
	so_pp_np,
        np_mod_pp(over),   % daar heb ik niets over te melden
	part_als_pred_np(aan),
	part_als_pred_np(af),
        part_refl(aan), % necc. refl: "er hebben zich 10 leden aangemeld"
        part_refl(af), % necc. refl: "er hebben zich 10 leden afgemeld"
	part_transitive(aan),
	part_intransitive(aan),
	part_transitive(af),
	part_transitive(na),  % juridisch: De minister dient op na te melden wijze in de proceskosten te worden veroordeeld .
	part_intransitive(af)  % VL X meldde ziek af
       ])]).

v(melk,melkt,melken,gemolken,[molk,melkte],[molken,melkten],
    [h([intransitive,
        part_transitive(uit),
	transitive])]).

v(memoreer,memoreert,memoreren,gememoreerd,memoreerde,memoreerden,
    [h([sbar,
	transitive])]).

v(men,ment,mennen,gemend,mende,menden,
    [h([transitive])]).

v(meng,mengt,mengen,gemengd,mengde,mengden,
    [h([transitive,
        part_transitive(aan),
	np_ld_pp,
	refl_ld_pp,
	intransitive  % hun stemmen mengen zeer goed
       ])]).

v(menstrueer,menstrueert,menstrueren,gemenstrueerd,menstrueerde,menstrueerden,
    [h([intransitive])]).

v(mep,mept,meppen,gemept,mepte,mepten,
    [h([transitive,
        intransitive,
        np_ld_pp,
        np_ld_dir])]).

v(merge,merget,mergen,gemerged,mergede,mergeden,
    [h([transitive])]).

v(merk,merkt,merken,gemerkt,merkte,merkten,
  [h([sbar,
      sbar_obj, % Je zult het echt merken dat de coureurs er zijn
	transitive,
	fixed_dep(intransitive),
	np_pc_pp(aan),
	np_pc_pp(van),
	part_als_pred_np(aan),
	part_sbar(aan),
	part_sbar(op),
	part_transitive(aan),
	part_transitive(op),
	part_np_mod_pp(op,over),
	part_vp(op),
	part_np_pc_pp(aan,op)])]).

v(mest,mest,mesten,gemest,mestte,mestten,
    [h([ap_pred_np,
	transitive,
	part_transitive(uit),
        part_transitive(vet)])]).

v(metsel,metselt,metselen,gemetseld,metselde,metselden,
    [h([intransitive,
	transitive])]).

v(meubileer,meubileert,meubileren,gemeubileerd,meubileerde,meubileerden,
    [h([transitive])]).

v(miauw,miauwt,miauwen,gemiauwd,miauwde,miauwden,
    [h([intransitive])]).

v(middel,middelt,middelen,gemiddeld,middelde,middelden,
    [h([intransitive,
	transitive])]).

v(mier,miert,mieren,gemierd,mierde,mierden,
    [h([intransitive,
	pc_pp(over)])]).

v(miezer,miezert,miezeren,gemiezerd,miezerde,miezerden,
    [h([het_subj])]).

v(migreer,migreert,migreren,gemigreerd,migreerde,migreerden,
  [h([transitive,
      np_ld_pp]),
   z([intransitive,
      ld_pp])]).

v(mijd,mijdt,mijden,gemeden,meed,meden,
    [h([transitive])]).

v(mijmer,mijmert,mijmeren,gemijmerd,mijmerde,mijmerden,
    [h([intransitive,
	sbar,
	pc_pp(over)])]).

v(mik,mikt,mikken,gemikt,mikte,mikten,
    [h([intransitive,
	part_intransitive(raak),
	part_transitive(raak),
	part_intransitive(naast),
	part_intransitive(over),
	np_ld_dir,
	np_ld_pp,
	er_pp_sbar(op),
	er_pp_vp(op),
	ld_pp,
        ld_dir,
	ld_adv
       ])]).

v(milder,mildert,milderen,gemilderd,milderde,milderden,
    [h([intransitive,
        transitive])]).

v(mime,mimet,mimen,gemimed,mimede,mimeden,
  [h([sbar,
      transitive,
      intransitive
     ])]).

%% en te plussen
v(min,mint,minnen,gemind,minde,minden,
    [h([intransitive,
        transitive])]).

v(minacht,minacht,minachten,geminacht,minachtte,minachtten,
    [h([transitive])]).

v(minder,mindert,minderen,geminderd,minderde,minderden,
  [unacc([intransitive,
          pc_pp(met)]),
   h([transitive])]).

v(minimaliseer,minimaliseert,minimaliseren,geminimaliseerd,
  minimaliseerde,minimaliseerden,
  [h([sbar_subj_np,
      transitive,
      intransitive])]).

v(mis,mist,missen,gemist,miste,misten,
  [h([intransitive,
      transitive
     ])]).

v(misbruik,misbruikt,misbruiken,misbruikt,misbruikte,misbruikten,
    [h([als_pred_np,
	transitive])]).

v(misdoe,misdoet,inflected(misdoen,misdoene),misdaan,misdeed,misdeden,
    [h([intransitive,
	transitive,
	np_np  % wat heeft hij ons misdaan?
       ])]).

v(misgun,misgunt,misgunnen,misgund,misgunde,misgunden,
    [h([np_np])]).

v(misdraag,misdraagt,misdragen,misdragen,misdroeg,misdroegen,
    [h([refl])]).

v(misgrijp,misgrijpt,misgrijpen,misgrepen,misgreep,misgrepen,
    [h([refl])]).

v(mishaag,mishaagt,mishagen,mishaagd,mishaagde,mishaagden,
    [h([intransitive,
	so_np])]).

v(mishandel,mishandelt,mishandelen,mishandeld,mishandelde,mishandelden,
    [h([transitive])]).

v(misken,miskent,miskennen,miskend,miskende,miskenden,
    [h([sbar,
	transitive])]).

v(miskijk,miskijkt,miskijken,miskeken,miskeek,miskeken,
    [h([refl])]).  

v(misleid,misleidt,misleiden,misleid,misleidde,misleidden,
    [h([sbar_subj_so_np,
	transitive,
	intransitive])]).

v(misloop,misloopt,mislopen,misgelopen,misliep,misliepen,
    [b([transitive])]).

v(misluk,mislukt,mislukken,mislukt,mislukte,mislukten,
    [unacc([intransitive])]).

v(mismaak,mismaakt,mismaken,mismaakt,mismaakte,mismaakten,
    [h([transitive])]).

v(misprijs,misprijst,misprijzen,misprezen,misprees,misprezen,
    [h([transitive])]).

v(misreken,misrekent,misrekenen,misrekend,misrekende,misrekenden,
  [h([refl,
      pc_pp_refl(op)
     ])]).

v(missta,misstaat,inflected(misstaan,misstane),misstaan,misstond,misstonden,
    [h([intransitive,
	so_np])]).

v(mistrap,mistrapt,mistrappen,mistrapt,mistrapte,mistrapten,
    [h([refl])]).

v(mist,mist,misten,gemist,mistte,mistten,
    [h([het_subj])]).

v(misvorm,misvormt,misvormen,misvormd,misvormde,misvormden,
    [h([transitive])]).

v(mix,mixt,mixen,gemixt,mixte,mixten,
  [h([transitive,
      intransitive, % Els van Nieuw Zeer
      part_transitive(in)])]).

v(mobiliseer,mobiliseert,mobiliseren,gemobiliseerd,mobiliseerde,mobiliseerden,
  [h([transitive,
      intransitive])]).

v(modder,moddert,modderen,gemodderd,modderde,modderden,
    [h([intransitive,
	part_intransitive(aan)])]).

v(modelleer,modelleert,modelleren,gemodelleerd,modelleerde,modelleerden,
    [h([transitive])]).

v(moderniseer,moderniseert,moderniseren,gemoderniseerd,moderniseerde,moderniseerden,
    [h([transitive,
	intransitive])]).

v(moedig,moedigt,moedigen,gemoedigd,moedigde,moedigden,
    [h([part_intransitive(aan),
	part_transitive(aan),
	part_dip_sbar(aan),
        part_np_vp_obj1(aan), % hij werd aangemoedigd de stap te zetten
	part_np_pc_pp(aan,tot)])]).

v(moei,moeit,moeien,gemoeid,moeide,moeiden,
  [h([transitive,
      refl,  % VL?
      refl_pc_pp(met)])]).

v(moet,moet,moeten,gemoeten,moest,moesten,
  [b([modifier(aux(inf)),       % VL: hij is diep moeten gaan
                                %     hij heeft diep moeten gaan
				% vind je het erg moest je dochter thuiskomen met...
      aux_pc_pp(inf,van)]),	

   h([intransitive,
      transitive_ndev_ndev_npas,
      ld_pp,
      ld_adv,
      ld_dir,
      uit,
      pp_copula(aan,slag),
      pp_copula(aan,werk),
      part_intransitive(af),
      part_intransitive(over),
      part_intransitive(terug),
      part_ld_pp(terug),
      nonp_copula,              % het boek moet af/kapot/weg/open...
      fixed([ap_pred(kwijt),sbar],no_passive),
      nonp_pred_np_ndev,  % ik moet een uniform aan / een pet op / etc
      part_pc_pp(af,van),
      fixed([{[ap_pred('aan de gang'),pc(met)]}],no_passive),
      fixed([[van,het,hart],dat],no_passive),  % mij moet vanavond iets van het hart
      fixed([[van,het,hart],dat,sbar_subj],no_passive),
                                               % het moet me van het hart dat je zeurt
      fixed([[te,lijf],acc],no_passive),
      fixed([[te,rade],ld_pp],no_passive),
      fixed([[te,rade],ld_adv],no_passive),
	     % ik moet (het) niet hebben dat ...
      fixed([vc(heb,inf,intransitive),opt_het_pobj1(dat_sbar)],no_passive),
                                % het moet gezegd dat je zeurt
      fixed([vc(zeg,psp,intransitive),sbar_subj_opt_het],no_passive),
                                % je moet zien (om) de aandacht vast te houden
      fixed([vc(zie,inf,intransitive),vp],no_passive),
      np_pc_pp(met),            % wat moeten wij daar mee?
      part_transitive(aan),     % wat moet ik aan?
      part_np_pc_pp(aan,met)    % wat moeten wij daar mee aan?
     ])]).

v(moffel,moffelt,moffelen,gemoffeld,moffelde,moffelden,
    [h([part_sbar(weg),
	intransitive,
	np_ld_pp,
	np_ld_dir,
	part_transitive(weg)])]).

v(mok,mokt,mokken,gemokt,mokte,mokten,
    [h([intransitive,
	part_intransitive(na),
	sbar,  % dip
	pc_pp(over)])]).

v(mol,molt,mollen,gemold,molde,molden,
    [h([transitive])]).

v(molesteer,molesteert,molesteren,gemolesteerd,molesteerde,molesteerden,
    [h([transitive])]).

v(mompel,mompelt,mompelen,gemompeld,mompelde,mompelden,
    [h([intransitive,
	sbar,
	transitive,
	pc_pp(over)])]).

v(mond,mondt,monden,gemond,mondde,mondden,
    [unacc([part_ld_pp(uit),
        part_ld_adv(uit)])]).

v(monkel,monkelt,monkelen,gemonkeld,monkelde,monkelden,
    [h([intransitive,
	sbar,
	transitive])]).

v(monitoor,monitoort,monitoren,gemonitoord,monitoorde,monitoorden,
    [h([transitive,
	intransitive])]).

v(monitor,monitort,monitoren,gemonitord,monitorde,monitorden,
    [h([transitive,
	intransitive])]).

v(monopoliseer,monopoliseert,monopoliseren,gemonopoliseerd,monopoliseerde,monopoliseerden,
    [h([transitive])]).

v(monster,monstert,monsteren,gemonsterd,monsterde,monsterden,
    [h([intransitive,
	part_intransitive(aan),  % matrozen
	transitive])]).

v(monteer,monteert,monteren,gemonteerd,monteerde,monteerden,
    [h([intransitive,
	transitive,
	part_transitive(af),
	np_ld_pp])]).

v(monter,montert,monteren,gemonterd,monterde,monterden,
    [h([part_transitive(op)]),
     b([part_intransitive(op)])]).

v(moord,moordt,moorden,gemoord,moordde,moordden,
    [h([intransitive,
	part_transitive(uit)])]).

v(mopper,moppert,mopperen,gemopperd,mopperde,mopperden,
    [h([intransitive,
	sbar,
	pc_pp(op),
	pc_pp(over)])]).

v(mor,mort,morren,gemord,morde,morden,
    [h([intransitive,
	sbar,
	pc_pp(over)])]).

v(moraliseer,moraliseert,moraliseren,gemoraliseerd,moraliseerde,moraliseerden,
  [h([intransitive,
      transitive
     ])]).

v(morrel,morrelt,morrelen,gemorreld,morrelde,morrelden,
    [h([intransitive,
	pc_pp(aan)])]).

v(mors,morst,morsen,gemorst,morste,morsten,
    [h([intransitive,
	transitive,
	mod_pp(over),
	np_mod_pp(over),
	pc_pp(met)])]).

v(mot,mot,motten,gemot,motte,motten,
    [h([transitive])]).

v(motiveer,motiveert,motiveren,gemotiveerd,motiveerde,motiveerden,
    [h([sbar_subj_np,
        acc_np_dip_sbar,
	transitive,
	intransitive,
	np_vp_obj1,
	vp_no_control,
	np_pc_pp(voor)])]).

v(motregen,motregent,motregenen,gemotregend,motregende,motregenden,
    [h([het_subj])]).

v(mousseer,mousseert,mousseren,gemousseerd,mousseerde,mousseerden,
    [h([intransitive])]).

v(muilkorf,muilkorft,muilkorven,gemuilkorfd,muilkorfde,muilkorfden,
    [h([transitive])]).

v(muis,muist,muizen,gemuisd,muisde,muisden,
    [z([fixed([er_pp(onderuit)],no_passive),
        fixed([er_pp(vanonder)],no_passive),
        fixed([er_pp([van,onder])],no_passive),
        fixed([er_pp(vandoor)],no_passive),
        fixed([er_pp(tussenuit)],no_passive)
       ])]).

v(mummel,mummelt,mummelen,gemummeld,mummelde,mummelden,
    [h([intransitive,
	transitive,
	sbar])]).

v(munt,munt,munten,gemunt,muntte,muntten,
    [h([transitive,
	part_intransitive(uit),
	np_pc_pp(op),  % hij heeft het op mij gemunt
	part_pc_pp(uit,in)])]).

v(murmel,murmelt,murmelen,gemurmeld,murmelde,murmelden,
    [h([intransitive,
	sbar,
	transitive])]).

v(musiceer,musiceert,musiceren,gemusiceerd,musiceerde,musiceerden,
    [h([intransitive])]).

v(naai,naait,naaien,genaaid,naaide,naaiden,
    [h([intransitive,
	transitive,
	part_transitive(aan),
	part_transitive(af),
        part_transitive(op),
        part_transitive(toe),
	part_np_ld_pp(in),
	part_np_np(aan),
	np_ld_pp])]).

v(naak,naakt,naken,genaakt,naakte,naakten,
    [h([intransitive])]).  

v(nader,nadert,naderen,genaderd,naderde,naderden,
    [z([intransitive,
	transitive])]).

v(nagel,nagelt,nagelen,genageld,nagelde,nagelden,
    [h([np_ld_pp,
	part_transitive(vast),
	part_np_ld_pp(vast)])]).

v(nationaliseer,nationaliseert,nationaliseren,genationaliseerd,nationaliseerde,nationaliseerden,
    [h([transitive])]).

v(naturaliseer,naturaliseert,naturaliseren,genaturaliseerd,naturaliseerde,naturaliseerden,
  [z([intransitive,
      pc_pp(tot)]),
     h([transitive])]).

v(navigeer,navigeert,navigeren,genavigeerd,navigeerde,navigeerden,
    [h([intransitive,
	transitive,
	ld_pp,
	np_ld_pp])]).

v(neem,neemt,nemen,genomen,nam,namen,neme,
    [z([part_intransitive(af),
        part_pc_pp(af,tot),  % de wind neemt af tot matig
        part_pc_pp(toe,tot), % de wind neemt toe tot krachtig
	part_mod_pp(af,door), % maar de problemen nemen er niet door af
	part_fixed(toe,[[hand,over,hand]],no_passive),
	part_intransitive(toe)]),
     h([nonp_pred_np,
	% np_np,  ??
        fixed([[uit,de,zeilen],{[acc(wind),dat]}],norm_passive),
        fixed([pp_pred(uit,hand),{[acc,dat]}],norm_passive),
        fixed([pp_pred(uit,hand),[acc]],norm_passive),
        fixed([acc(maat),dat],norm_passive),
	fixed([pp_pred(in,bewaring),acc],norm_passive),
	transitive,
	intransitive,
        sbar_obj,
        pp_sbar(bij), % Vlaams; Neem daarbij dat ...
	np_ld_pp,
        np_np_ld_pp,  % je neemt hem de woorden uit de mond
	np_pc_pp_refl(op),
	refl_pp_vp_obj(op),  % hij nam het op zich de kandidaten te informeren
	np_pc_pp_refl(tot),
	np_pc_pp(tegen),
	np_pc_pp(ter),
	np_pc_pp(tot),
	part_np_np(af),
	part_np_np(over),
	part_intransitive(deel),
        part_intransitive(in),  % bij het feest werd flink ingenomen
	part_intransitive(op),
	part_intransitive(over),  % fietsen/VL
	part_intransitive(plaats),
	part_intransitive(vrij),
	part_intransitive(vrijaf),
	part_intransitive(waar),
	part_refl_np(voor),
	part_sbar(aan),
        part_van_sbar(aan),  % ik neem aan van wel
	part_sbar(op),
	part_sbar(terug),
	part_sbar(waar),
	part_sbar(weg),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(door),
	part_transitive(gevangen),
	part_transitive(in),
	part_transitive(mee),
	part_transitive(op),
        part_np_pc_pp(op,tegen),
	part_transitive(over),
	part_transitive(terug),
	part_transitive(uit),
	part_transitive(waar),
	part_als_pred_np(waar),
	part_transitive(weg),
	part_sbar(gewaar),
	part_transitive(gewaar),
	part_refl_vp(voor),
	part_refl_sbar(voor),
	part_fixed(op,[het_obj1,pc(voor)],no_passive),
	part_fixed(op,[het_obj1,pc(tegen)],no_passive),
	fixed([[stelling]],imp_passive),
	fixed([pc(tegen),[stelling]],imp_passive),
	fixed([{[acc(aanstoot),pc(aan)]}],norm_passive),
	fixed([{[acc(aanstoot),er_pp(aan,X)]},extra_sbar(X)],norm_passive),
	fixed([{[acc(afscheid),pc(van)]}],imp_passive),
	fixed([{[acc(afstand),pc(van)]}],imp_passive),
	fixed([{[[een,loopje],pc(met)]}],imp_passive),
	fixed([{[acc(genoegen),pc(met)]}],norm_passive),
	fixed([{[acc(genoegen),er_pp(met,X)]},extra_sbar(X)],norm_passive),
	fixed([{[acc(genoegen),er_pp(met,X)]},extra_vp(X)],norm_passive),
        fixed([[in,bescherming],acc],norm_passive),
        fixed([[in,bescherming],{[acc,pc(tegen)]}],norm_passive),
        fixed([[in,de,maling],acc],norm_passive),
	fixed([{[acc(initiatief),pc(tot)]}],norm_passive),
        fixed([pp_pred(in,hand),acc],norm_passive),
	fixed([{[acc(nota),pc(van)]}],norm_passive),
	fixed([{[acc(kennis),pc(van)]}],norm_passive),
	fixed([{[acc(kennis),er_pp(van,X)]},extra_sbar(X)],norm_passive),
	part_pc_pp(kennis,van),
	part_er_pp_sbar(kennis,van),
	fixed([{[acc(nota),er_pp(van,X)]},extra_sbar(X)],norm_passive),
	fixed([{[acc(maatregel),pc(tegen)]}],norm_passive),
	fixed([{[[poolshoogte],ld_pp]}],no_passive),
	fixed([[poolshoogte],ld_adv],no_passive),
	fixed([{[[poolshoogte],pc(van)]}],no_passive),
	fixed([[in,acht],acc],norm_passive),
	fixed([[in,acht],sbar],norm_passive),
	fixed([svp_pp(in,aanmerking),acc],norm_passive),
	fixed([svp_pp(in,aanmerking),sbar],imp_passive),
	fixed([[in,beslag],acc],norm_passive),
	fixed([[in,de,arm],acc],norm_passive),
	fixed([[in,gebruik],acc],norm_passive),
	fixed([[in,het,ootje],acc],norm_passive),
	fixed([[in,ogenschouw],acc],norm_passive),
	fixed([[in,ogenschouw],sbar],norm_passive),
	fixed([[in,overweging],acc],norm_passive),
	fixed([[in,overweging],sbar],norm_passive),
	fixed([[in,ontvangst],acc],norm_passive),
	part_so_np(kwalijk),
        part_so_np_sbar_obj(kwalijk),
        part_np_np(kwalijk),
        part_np_sbar(kwalijk),
	fixed([ap_pred(kwalijk),dat],norm_passive),  % neem me niet kwalijk
	fixed([ap_pred(kwalijk),{[acc,dat]}],norm_passive),
	fixed([ap_pred(kwalijk),dat,het_pobj1(sbar)],imp_passive),
	fixed([ap_pred(kwalijk),dat,sbar],imp_passive),
	part_fixed(af,[[in,dank],{[acc,dat]}],norm_passive),
	part_fixed(af,[[in,dank],dat,het_pobj1(sbar)],imp_passive),
	part_fixed(af,[[in,dank],dat,sbar],imp_passive),
	fixed([[onder,de,loep],acc],norm_passive),
	fixed([[onder,handen],acc],norm_passive),
	fixed([svp_pp(onder,vuur),acc],norm_passive),
	fixed([[op,de,korrel],acc],norm_passive),
	fixed([[op,de,koop,toe],acc],norm_passive),
	fixed([[op,de,koop,toe],sbar],imp_passive),
	fixed([[op,sleeptouw],acc],norm_passive),
	fixed([[poolshoogte]],no_passive),
	fixed([[poolshoogte],ld_adv],no_passive),
	fixed([[poolshoogte],ld_pp],no_passive),
	fixed([[te,baat],acc],norm_passive),
	fixed([[te,baat],acc,vp],norm_passive),
	fixed([[te,grazen],acc],norm_passive),
	fixed([[te,pakken],acc],norm_passive),
	fixed([[ter,hand],acc],norm_passive),
	fixed([[ter,harte],acc,refl],norm_passive),
	fixed([[ter,harte],acc],norm_passive),
	fixed([acc(tijd),me],imp_passive),
	fixed([acc(tijd),me,vp],imp_passive),
	fixed([acc(tijd)],imp_passive),
	fixed([acc(tijd),vp],imp_passive),
	fixed([[voor,lief],acc],norm_passive),
	fixed([[voor,lief],sbar],norm_passive),
	fixed([[voor,lief],opt_het_pobj1(dat_sbar)],norm_passive),
        fixed([acc(voortouw),vp],norm_passive),
        fixed([{[acc(vrede),pc(met)]}],norm_passive),
	fixed([[zitting],pc(in)],no_passive),
	fixed([svp_pp(voor,rekening),acc],norm_passive),
	part_fixed(toe,[[op,de,koop],acc],norm_passive),
	part_fixed(in,[er_pp(op),[gif]],no_passive),
	part_fixed(in,[er_pp(op),[vergif]],no_passive),
	fixed([er_pp(op),[gif]],no_passive),
	fixed([er_pp(op),[vergif]],no_passive),
	part_fixed(in,[er_pp(op,X),[vergif],extra_sbar(X)],imp_passive),
	part_fixed(in,[er_pp(op,X),[gif],extra_sbar(X)],imp_passive),
	fixed([er_pp(op,X),[gif],extra_sbar(X)],imp_passive),
	fixed([er_pp(op,X),[vergif],extra_sbar(X)],imp_passive),
        part_fixed(in,[{[acc(standpunt),er_pp(over)]}],norm_passive),
	fixed([{[acc(vrede),pc(met)]}],no_passive), % ik nemen er vrede mee
	fixed([{[acc(vrede),er_pp(met,X)]},extra_sbar(X)],no_passive),
	part_np_ld_pp(af),
	part_np_ld_pp(mee),  % NB dat ik de spullen mee naar huis neem
                             % ?? dat ik de spullen naar huis mee neem
        part_np_ld_dir(mee), % NB dat ik de spullen mee het bos in neem
                             % !! dat ik de spullen het bos mee in neem
                             % ?? dat ik de spullen het bos in mee neem
	part_np_ld_pp(op),
	part_np_ld_pp(over),
	part_np_pc_pp(aan,van),
	part_np_pc_pp(aan,voor),
	part_np_pc_pp(door,met),
	part_np_pc_pp(in,met),
	part_np_pc_pp(in,tegen),
	part_np_pc_pp(op,met),
	part_np_pc_pp(over,van),
	part_pp_sbar(aan,van),
	part_pc_pp(deel,aan),
	part_pc_pp(deel,in),
	part_ld_pp(plaats),
	part_fixed(op,[{[acc(contact),pc(met)]}],imp_passive)])]).

v(negeer,negeert,negeren,genegeerd,negeerde,negeerden,
    [h([transitive,
	sbar])]).

v(neig,neigt,neigen,geneigd,neigde,neigden,
    [h([transitive,
	intransitive,
	er_pp_vp(naar),
	er_pp_vp(tot),
	pc_pp(naar),
	pc_pp(tot)])]).

v(nek,nekt,nekken,genekt,nekte,nekten,
    [h([transitive])]).

v(nep,nept,neppen,genept,nepte,nepten,
    [h([transitive])]).

v(nest,nest,nesten,genest,nestte,nestten,
    [h([intransitive,
        transitive])]).

v(nestel,nestelt,nestelen,genesteld,nestelde,nestelden,
    [h([intransitive,
	refl,
	part_refl(in),
	part_refl_ld_pp(in),
	refl_ld_pp,
	refl_ld_adv])]).

%% een treffer netten: doelpunt maken VLAAMS
v(net,net,netten,genet,nette,netten,
    [h([intransitive,
        transitive])]).

v(netwerk,netwerkt,netwerken,genetwerkt,netwerkte,netwerkten,
    [h([intransitive])]).

v(neuk,neukt,neuken,geneukt,neukte,neukten,
    [h([intransitive,
	transitive,
	pc_pp(met)])]).

v(neus,neust,neuzen,geneusd,neusde,neusden,
    [h([part_intransitive(rond),
	part_transitive(door),
	ld_pp,
	ld_adv,
	intransitive])]).

v(neurie,neuriet,[neuriën,neurieën],geneuried,neuriede,neurieden,
    [h([intransitive,
	transitive,
	mod_pp(bij),
	np_mod_pp(bij)])]).

v(neutraliseer,neutraliseert,neutraliseren,geneutraliseerd,neutraliseerde,neutraliseerden,
    [h([sbar_subj_np,
	transitive])]).

v(nies,niest,[niesen,niezen],[geniest,geniesd],[nieste,niesde],[niesten,niesden],
    [h([intransitive])]).

v(nijg,nijgt,nijgen,genegen,neeg,negen,
    [h([intransitive,
	transitive])]).

v(nijp,nijpt,nijpen,genepen,neep,nepen,
    [h([intransitive])]).

v(niks,nikst,niksen,genikst,nikste,niksten,
    [h([intransitive])]).

v(nip,nipt,nippen,genipt,nipte,nipten,
    [h([pc_pp(aan),
	pc_pp(van),
        intransitive])]).

v(nivelleer,nivelleert,nivelleren,genivelleerd,nivelleerde,nivelleerden,
    [h([intransitive,
	sbar_subj_np,
	transitive])]).

v(nodig,nodigt,nodigen,genodigd,nodigde,nodigden,
  [h([transitive,
      np_pc_pp(aan),
      np_pc_pp(op),
      np_pc_pp(voor),
      part_vp_no_control(uit),	% de maatregel nodigt uit om ...
      part_np_vp_obj1(uit),
      np_vp_obj1,
      vp_no_control,
      part_vp(uit),		% VL
      part_transitive(uit),
      part_intransitive(uit),	% VL
      part_np_pc_pp(uit,op),
      part_np_pc_pp(uit,voor),
      part_np_pc_pp(uit,tot),
      part_pc_pp(uit,tot),
      part_er_pp_vp_no_control(uit,tot),
      part_obj_np_er_pp_vp(uit,tot)])]).

v(noem,noemt,noemen,genoemd,noemde,noemden,
    [h([pred_np,
	pred_np_sbar,
	pred_np_vp,
	transitive,
        sbar,
	np_pc_pp(naar),
        fixed([{[acc(voorbeeld),pc(van)]}],no_passive),
        fixed([{[acc(voorbeeld),dat,pc(van)]}],no_passive),
	part_transitive(op),
	fixed([[bij,name],acc],norm_passive)])]).

v(nok,nokt,nokken,genokt,nokte,nokten,
    [z([part_intransitive(af)])]).

v(nomineer,nomineert,nomineren,genomineerd,nomineerde,nomineerden,
  [h([transitive,
      als_pred_np,
      np_pc_pp(voor),
      pc_pp(voor)])]).

v(nood,noodt,noden,genood,noodde,noodden,
    [h([transitive,
	pc_pp(tot)])]).

v(noodzaak,noodzaakt,noodzaken,genoodzaakt,noodzaakte,noodzaakten,
    [h([np_vp_obj1,
	er_pp_vp_no_control(tot),
	obj_np_er_pp_vp(tot),
	np_pc_pp(tot),
	transitive,
	pc_pp(tot)])]).

v(noop,noopt,nopen,genoopt,noopte,noopten,
  [h([np_vp_obj,
      er_pp_vp_no_control(tot),
	obj_np_er_pp_vp(tot),
	np_pc_pp(tot),
	pc_pp(tot)])]).

v(normaliseer,normaliseert,normaliseren,genormaliseerd,
  normaliseerde,normaliseerden,
    [h([sbar_subj_np,
	refl,  % word order: nu heeft zich dat genormaliseerd
	transitive,
        intransitive])]).

v(normeer,normeert,normeren,genormeerd,normeerde,normeerden,
    [h([transitive])]).

v(noteer,noteert,noteren,genoteerd,noteerde,noteerden,
    [h([sbar,
	transitive,
	intransitive,
	vp,
	als_pred_np,
	np_ld_pp])]).

v(notueleer,notuleert,notuleren,genotuleerd,notuleerde,notuleerden,
    [h([intransitive,
	transitive])]).

v(nuanceer,nuanceert,nuanceren,genuanceerd,nuanceerde,nuanceerden,
    [h([transitive,
	intransitive,
	sbar])]).  % dip

v(nummer,nummert,nummeren,genummerd,nummerde,nummerden,
    [h([intransitive,
	part_transitive(door),
	transitive])]).

v(nuttig,nuttigt,nuttigen,genuttigd,nuttigde,nuttigden,
    [h([transitive])]).

v(objectiveer,objectiveert,objectiveren,geobjectiveerd,objectiveerde,objectiveerden,
    [h([transitive])]).

v(objektiveer,objektiveert,objektiveren,geobjektiveerd,objektiveerde,objektiveerden,
    [h([transitive])]).

v(obsedeer,obsedeert,obsederen,geobsedeerd,obsedeerde,obsedeerden,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(observeer,observeert,observeren,geobserveerd,observeerde,observeerden,
    [h([transitive,
	intransitive])]).

v(octroieer,octroieert,octroieren,geoctroieerd,octroieerde,octroieerden,
    [h([transitive])]).

v(oefen,oefent,oefenen,geoefend,oefende,oefenden,
    [h([intransitive,
	transitive,
	pc_pp(op),
	np_pc_pp(op),        % kritiek oefenen op
	np_pc_pp(in),
	part_transitive(in),  % VL: OBJ1 in te studeren
	part_transitive(uit),
	part_np_pc_pp(uit,op)])]).

v(offer,offert,offeren,geofferd,offerde,offerden,
    [h([so_pp_np,
	transitive,
	intransitive,
	part_transitive(op),
	part_np_pc_pp(op,aan),
	part_np_pc_pp(op,voor)])]).

v(offreer,offreert,offreren,geoffreerd,offreerde,offreerden,
    [h([transitive,
	np_np])]).

v(olie,oliet,oliën,geolied,oliede,olieden,
    [h([transitive])]).

v(omarm,omarmt,omarmen,omarmd,omarmde,omarmden,
    [h([transitive])]).

v(omcirkel,omcirkelt,omcirkelen,omcirkeld,omcirkelde,omcirkelden,
    [h([transitive])]).

v(omdijk,omdijkt,omdijken,omdijkt,omdijkte,omdijkten,
    [h([transitive])]).

v(omgeef,omgeeft,omgeven,omgeven,omgaf,omgaven,
    [h([transitive,
	np_pc_pp(met)])]).

v(omhein,omheint,omheinen,omheind,omheinde,omheinden,
    [h([transitive])]).

v(omhels,omhelst,omhelzen,omhelsd,omhelsde,omhelsden,
    [h([transitive,
	intransitive])]).

v(omhul,omhult,omhullen,omhuld,omhulde,omhulden,
    [h([transitive])]).

v(omkleed,omkleedt,omkleden,omkleed,omkleedde,omkleedden,
    [h([transitive,
	np_pc_pp(met)])]).

v(omklem,omklemt,omklemmen,omklemd,omklemde,omklemden,
    [h([transitive,
	np_pc_pp(met)])]).

v(omlijn,omlijnt,omlijnen,omlijnd,omlijnde,omlijnden,
    [h([sbar_subj_np,
	transitive])]).

v(omlijst,omlijst,omlijsten,omlijst,omlijstte,omlijstten,
    [h([transitive])]).

v(ommuur,ommuurt,ommuren,ommuurd,ommuurde,ommuurden,
    [h([transitive])]).

v(omring,omringt,omringen,omringd,omringde,omringden,
    [h([transitive,
	np_pc_pp(met)])]).

v(omschrijf,omschrijft,omschrijven,omschreven,omschreef,omschreven,
    [h([als_pred_np,
	sbar,
	transitive])]).

v(omsingel,omsingelt,omsingelen,omsingeld,omsingelde,omsingelden,
    [h([transitive])]).

v(omsluit,omsluit,omsluiten,omsloten,omsloot,omsloten,
    [h([transitive])]).

v(omspan,omspant,omspannen,omspannen,omspande,omspanden,
    [h([transitive])]).

v(omspeel,omspeelt,omspelen,omspeeld,omspeelde,omspeelden,
    [h([transitive])]).

v(omspoel,omspoelt,omspoelen,omspoeld,omspoelde,omspoelden,
    [h([transitive])]).

v(omstrengel,omstrengelt,omstrengelen,omstrengeld,omstrengelde,omstrengelden,
    [h([transitive])]).

v(omstuw,omstuwt,omstuwen,omstuwd,omstuwde,omstuwden,
    [h([transitive])]).

v(omtrek,omtrekt,omtrekken,omtrokken,omtrok,omtrokken,
    [h([transitive])]).

v(omvat,omvat,omvatten,omvat,omvatte,omvatten,
    [h([sbar,
	transitive])]).

v(omwikkel,omwikkelt,omwikkelen,omwikkeld,omwikkelde,omwikkelden,
    [h([transitive,
	np_pc_pp(met)])]).

v(omzeil,omzeilt,omzeilen,omzeild,omzeilde,omzeilden,
    [h([sbar_subj_np,
	transitive])]).

v(omzoom,omzoomt,omzomen,omzoomd,omzoomde,omzoomden,
    [h([transitive,
	np_pc_pp(met)])]).

v(onderbelicht,onderbelicht,onderbelichten,onderbelicht,onderbelichtte,onderbelichtten,
    [h([transitive])]).

v(onderbouw,onderbouwt,onderbouwen,onderbouwd,onderbouwde,onderbouwden,
    [h([sbar_subj_np,
	transitive])]).

v(onderbreek,onderbreekt,onderbreken,onderbroken,onderbrak,onderbraken,
  [h([transitive,
      intransitive,
      acc_np_dip_sbar])]).

v(onderdruk,onderdrukt,onderdrukken,onderdrukt,onderdrukte,onderdrukten,
    [h([transitive])]).

v(onderga,ondergaat,inflected(ondergaan,ondergane),ondergaan,onderging,ondergingen,
    [h([transitive])]).

v(ondergraaf,ondergraaft,ondergraven,ondergraven,ondergroef,ondergroeven,
    [h([sbar_subj_np,
	transitive])]).

v(onderhandel,onderhandelt,onderhandelen,onderhandeld,onderhandelde,onderhandelden,
    [h([intransitive,
        part_transitive(uit),
	pc_pp(met),   %% ???
	pc_pp(over)])]).

v([onderhoud,onderhou],onderhoudt,onderhouden,onderhouden,onderhield,onderhielden,
    [h([sbar_subj_so_np,
	transitive,
	np_pc_pp(met),
	np_pc_pp(over),
	fixed([{[acc(contact),pc(met)]}],norm_passive),
	refl_pc_pp(met)])]).

v(onderken,onderkent,onderkennen,onderkend,onderkende,onderkenden,
    [h([sbar,
	transitive,
	vp])]).

v(onderlijn,onderlijnt,onderlijnen,onderlijnd,onderlijnde,onderlijnden,
  [h([transitive,
      sbar,
      intransitive])]).

v(ondermijn,ondermijnt,ondermijnen,ondermijnd,ondermijnde,ondermijnden,
    [h([transitive])]).

v(onderneem,onderneemt,ondernemen,ondernomen,ondernam,ondernamen,
    [h([transitive,
        intransitive,
	np_pc_pp(op),
	np_pc_pp(tegen)])]).

v(onderricht,onderricht,onderrichten,onderricht,onderrichtte,onderrichtten,
    [h([transitive,
	np_pc_pp(in)])]).

v(onderschat,onderschat,onderschatten,onderschat,onderschatte,onderschatten,
    [h([transitive,
	sbar % +hoe X
       ])]).

v(onderscheid,onderscheidt,onderscheiden,onderscheiden,onderscheidde,onderscheidden,
    [h([refl,
	sbar,
	transitive,
	np_pc_pp(in),
	np_pc_pp(met),
	np_pc_pp(van),
	refl_pc_pp(door),
	refl_pc_pp(van)])]).

v(onderschep,onderschept,onderscheppen,onderschept,onderschepte,onderschepten,
    [h([transitive])]).

v(onderschrijf,onderschrijft,onderschrijven,onderschreven,onderschreef,onderschreven,
    [h([sbar,
	transitive])]).

v(onderstel,onderstelt,onderstellen,ondersteld,onderstelde,onderstelden,
    [h([sbar,
	transitive,
	vp])]).

v(ondersteun,ondersteunt,ondersteunen,ondersteund,ondersteunde,ondersteunden,
    [h([sbar_subj_so_np,
	transitive,
	intransitive,
	pc_pp(bij),
	pc_pp(in)])]).

v(onderstreep,onderstreept,onderstrepen,onderstreept,onderstreepte,onderstreepten,
    [h([sbar,
	transitive,
	vp,
	np_pc_pp(met)])]).

v(onderteken,ondertekent,ondertekenen,ondertekend,ondertekende,ondertekenden,
    [h([transitive,
        intransitive,
	sbar])]).

v(ondertitel,ondertitelt,ondertitelen,ondertiteld,ondertitelde,ondertitelden,
    [h([transitive])]).  

v(ondertunnel,ondertunnelt,ondertunnelen,ondertunneld,ondertunnelde,ondertunnelden,
    [h([transitive])]).  

v(ondervang,ondervangt,ondervangen,ondervangen,onderving,ondervingen,
    [h([sbar_subj_so_np,
	transitive])]).

%v(onderverdeel,onderverdeelt,onderverdelen,onderverdeeld,onderverdeelde,onderverdeelden,
%    [h([transitive,
%	np_pc_pp(in)])]).

v(ondervind,ondervindt,ondervinden,ondervonden,ondervond,ondervonden,
    [h([sbar,
	transitive,
	np_pc_pp(van),  % hinder/last/schade/overlast/...
	fixed([{[acc(baat),pc(bij)]}],no_passive),
	fixed([{[acc(baat),er_pp(bij,X)]},extra_sbar(X)],no_passive),
	fixed([{[acc(baat),er_pp(bij,X)]},extra_vp(X)],no_passive),
	fixed([[aan,den,lijve],acc],norm_passive),
	fixed([[aan,den,lijve],sbar],imp_passive)])]).

v(ondervraag,ondervraagt,ondervragen,ondervraagd,[ondervroeg,ondervraagde],[ondervroegen,ondervraagden],
  [h([transitive,
      acc_np_sbar])]).

v(onderwerp,onderwerpt,onderwerpen,onderworpen,onderwierp,onderwierpen,
    [h([transitive,
	np_pc_pp(aan)
	% refl_pc_pp(aan)
       ])]).

v(onderwijs,onderwijst,onderwijzen,onderwezen,onderwees,onderwezen,
    [h([np_np,
	so_pp_np,
	so_np,
	sbar,
	transitive,
	intransitive])]).

v(onderzoek,onderzoekt,onderzoeken,onderzocht,onderzocht,onderzochten,
    [h([sbar,
	transitive,
	intransitive,
	np_pc_pp(op)])]).

v(ontaard,ontaardt,ontaarden,ontaard,ontaardde,ontaardden,
    [unacc([intransitive,
	pc_pp(in)])]).

v(ontbeer,ontbeert,ontberen,ontbeerd,ontbeerde,ontbeerden,
    [h([transitive])]).

v(ontbied,ontbiedt,ontbieden,ontboden,ontbood,ontboden,
    [h([transitive,
	np_ld_pp,
	np_ld_adv])]).

v(ontbijt,ontbijt,ontbijten,ontbeten,ontbeet,ontbeten,
    [h([intransitive,
	pc_pp(met)])]).

v(ontbind,ontbindt,ontbinden,ontbonden,ontbond,ontbonden,
    [h([transitive])]).

v(ontbloot,ontbloot,ontbloten,ontbloot,ontblootte,ontblootten,
    [h([transitive])]).

v(ontbos,ontbost,ontbossen,ontbost,ontboste,ontbosten,
    [h([transitive])]).

v(ontbrand,ontbrandt,ontbranden,ontbrand,ontbrandde,ontbrandden,
    [unacc([intransitive,
	pc_pp(in)])]).

v(ontbreek,ontbreekt,ontbreken,ontbroken,ontbrak,ontbraken,
    [h([intransitive,
        so_np,                  % omdat ons de kennis ontbrak
	pc_pp(aan),
	fixed([het_subj,{[pc(aan),dat]}],no_passive),
                                % het ontbrak ons aan lef
	fixed([het_subj,er_pp(aan,C),extra_sbar(C)],no_passive)])]).
                                % het ontbrak er nog maar aan dat ...

v(ontcijfer,ontcijfert,ontcijferen,ontcijferd,ontcijferde,ontcijferden,
    [h([transitive])]).

v(ontdek,ontdekt,ontdekken,ontdekt,ontdekte,ontdekten,
    [h([sbar,
        vp,
	np_ld_pp,   % wij ontdekken geen lijn in dit stuk
	np_ld_adv,
	np_pc_pp(aan), % ik kan er geen vrolijke noot aan ontdekken
	transitive])]).

v(ontdoe,ontdoet,inflected(ontdoen,ontdoene),ontdaan,ontdeed,ontdeden,
    [h([np_pc_pp(van)
	%refl_pc_pp(van)
        ])]).

v(ontdooi,ontdooit,ontdooien,ontdooid,ontdooide,ontdooiden,
    [unacc([intransitive]),
     h([transitive])]).

v(ontduik,ontduikt,ontduiken,ontdoken,ontdook,ontdoken,
    [h([transitive])]).

v(onteer,onteert,onteren,onteerd,onteerde,onteerden,
    [h([sbar_subj_so_np,
	transitive])]).

v(onteigen,onteigent,onteigenen,onteigend,onteigende,onteigenden,
    [h([transitive,
	intransitive])]).

v(onterf,onterft,onterven,onterfd,onterfde,onterfden,
    [h([transitive])]).  

v(ontferm,ontfermt,ontfermen,ontfermd,ontfermde,ontfermden,
    [h([refl_pc_pp(over),
	refl])]).

v(ontfutsel,ontfutselt,ontfutselen,ontfutseld,ontfutselde,ontfutselden,
    [h([np_np,
        so_pp_np,
	transitive])]).

v(ontga,ontgaat,inflected(ontgaan,ontgane),ontgaan,ontging,ontgingen,
  [unacc([so_np,
          transitive,           % het ontgaan van belasting
	  sbar_subj_so_np,
          fixed_dep(intransitive)          % for svp
         ])]).

v(ontgeld,ontgeldt,ontgelden,ontgolden,ontgold,ontgolden,
    [h([transitive % wij moeten het/dat/zijn woede ontgelden
       ])]).

v(ontgin,ontgint,ontginnen,ontgonnen,ontgon,ontgonnen,
  [h([transitive,
      intransitive
     ])]).

v(ontglip,ontglipt,ontglippen,ontglipt,ontglipte,ontglipten,
    [unacc([intransitive,
            so_np,
            sbar_subj_so_np])]).

v(ontgoochel,ontgoochelt,ontgoochelen,ontgoocheld,ontgoochelde,ontgoochelden,
    [h([np_np,
	sbar_subj_so_np,
	so_np,
	transitive,
        intransitive])]).

v(ontgrendel,ontgrendelt,ontgrendelen,ontgrendeld,
  ontgrendelde,ontgrendelden,
    [h([transitive,
        intransitive])]).

v(ontgroei,ontgroeit,ontgroeien,ontgroeid,ontgroeide,ontgroeiden,
    [unacc([so_np,
	transitive])]).

v(ontgroen,ontgroent,ontgroenen,ontgroend,ontgroende,ontgroenden,
    [h([transitive]),
     z([intransitive])]).

v(onthaal,onthaalt,onthalen,onthaald,onthaalde,onthaalden,
    [h([transitive,
        als_pred_np,
	np_pc_pp(op)])]).

v(onthaar,onthaart,ontharen,onthaard,onthaarde,onthaarden,
    [h([transitive,
	intransitive])]).

v(onthaast,onthaast,onthaasten,onthaast,onthaastte,onthaastten,
    [h([intransitive])]).

v(onthecht,onthecht,onthechten,onthecht,onthechtte,onthechtten,
    [h([refl_pc_pp(van),
	intransitive])]).

v(onthef,ontheft,ontheffen,ontheven,onthief,onthieven,
    [h([transitive,
	np_pc_pp(uit),
	np_pc_pp(van)])]).

v(ontheilig,ontheiligt,ontheiligen,ontheiligd,ontheiligde,ontheiligden,
    [h([transitive])]).

v(onthoofd,onthoofdt,onthoofden,onthoofd,onthoofdde,onthoofdden,
    [h([transitive])]).

v([onthoud,onthou],onthoudt,onthouden,onthouden,onthield,onthielden,
    [h([np_np,
	sbar,
	so_pp_np,
	transitive,
	intransitive,  % onthoud , ik ben de baas
	refl_pc_pp(van)])]).

v(onthul,onthult,onthullen,onthuld,onthulde,onthulden,
  [h([sbar,
      vp,
      np_mod_pp(van),
      transitive,
      np_np,
      so_pp_np
     ])]).

v(onthuts,onthutst,onthutsen,onthutst,onthutste,onthutsten,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(ontkalk,ontkalkt,ontkalken,ontkalkt,ontkalkte,ontkalkten,
  [h([transitive,
      intransitive])]).

v(ontken,ontkent,ontkennen,ontkend,ontkende,ontkenden,
    [h([intransitive,
	sbar,
	transitive,
	vp])]).

v(ontketen,ontketent,ontketenen,ontketend,ontketende,ontketenden,
    [h([sbar_subj_np,
	transitive])]).

v(ontkiem,ontkiemt,ontkiemen,ontkiemd,ontkiemde,ontkiemden,
    [unacc([intransitive])]).

v(ontkleed,ontkleedt,ontkleden,ontkleed,ontkleedde,ontkleedden,
    [h([transitive])]).

v(ontkom,ontkomt,ontkomen,ontkomen,ontkwam,ontkwamen,
    [z([intransitive,
	er_pp_sbar(aan),
	er_pp_vp(aan),
	pc_pp(aan)])]).

v(ontkoppel,ontkoppelt,ontkoppelen,ontkoppeld,ontkoppelde,ontkoppelden,
    [h([transitive])]).

v(ontkracht,ontkracht,ontkrachten,ontkracht,ontkrachtte,ontkrachtten,
    [h([sbar_subj_np,
	transitive])]).

v(ontkurk,ontkurkt,ontkurken,ontkurkt,ontkurkte,ontkurkten,
    [h([transitive])]).

v(ontlaad,ontlaadt,ontladen,ontladen,ontlaadde,ontlaadden,
    [h([intransitive,           % ??
	refl,
	transitive 		% je moet de batterij eens per drie maanden geheel ontladen
                                % refl_ld_pp,
                                % refl_ld_adv
        ])]).

v(ontlast,ontlast,ontlasten,ontlast,ontlastte,ontlastten,
    [h([% refl,
	sbar_subj_np,
	transitive,
	np_pc_pp(van)])]).

v(ontleed,ontleedt,ontleden,ontleed,ontleedde,ontleedden,
  [h([transitive]),
   unacc([intransitive])]).

v(ontleen,ontleent,ontlenen,ontleend,ontleende,ontleenden,
    [h([transitive,
	np_pc_pp(aan)])]).

v(ontlok,ontlokt,ontlokken,ontlokt,ontlokte,ontlokten,
    [h([np_np,
	so_pp_np,
	transitive])]).

v(ontloop,ontloopt,ontlopen,ontlopen,ontliep,ontliepen,
    [z([transitive])]).

v(ontluik,ontluikt,ontluiken,ontloken,ontlook,ontloken,
    [unacc([intransitive]),
     h([transitive])]).

v(ontluister,ontluistert,ontluisteren,ontluisterd,ontluisterde,ontluisterden,
    [h([transitive])]).

v(ontmaagd,ontmaagdt,ontmaagden,ontmaagd,ontmaagde,ontmaagden,
    [h([transitive])]).

v(ontman,ontmant,ontmannen,ontmand,ontmande,ontmanden,
    [h([transitive])]).

v(ontmantel,ontmantelt,ontmantelen,ontmanteld,ontmantelde,ontmantelden,
    [h([transitive])]).

v(ontmasker,ontmaskert,ontmaskeren,ontmaskerd,ontmaskerde,ontmaskerden,
    [h([sbar_subj_np,
	transitive])]).

v(ontmenselijk,ontmenselijkt,ontmenselijken,ontmenselijkt,ontmenselijkte,ontmenselijkten,
  [unacc([intransitive]),
   h([transitive])]
 ).

v(ontmijn,ontmijnt,ontmijnen,ontmijnd,ontmijnde,ontmijnden,
    [h([transitive])]).

v(ontmoedig,ontmoedigt,ontmoedigen,ontmoedigd,ontmoedigde,ontmoedigden,
    [h([sbar_subj_np,
	transitive])]).

v(ontmoet,ontmoet,ontmoeten,ontmoet,ontmoette,ontmoetten,
    [h([transitive])]).

v(ontneem,ontneemt,ontnemen,ontnomen,ontnam,ontnamen,
    [h([np_np,
	transitive,
	so_pp_np])]).

v(ontnuchter,ontnuchtert,ontnuchteren,ontnuchterd,ontnuchterde,ontnuchterden,
    [h([sbar_subj_np,
	transitive,
        intransitive])]).

v(ontpit,ontpit,ontpitten,ontpit,ontpitte,ontpitten,
    [h([transitive])]).

v(ontplof,ontploft,ontploffen,ontploft,ontplofte,ontploften,
    [unacc([intransitive])]).

v(ontplooi,ontplooit,ontplooien,ontplooid,ontplooide,ontplooiden,
    [h([refl,
	transitive])]).

v(ontpop,ontpopt,ontpoppen,ontpopt,ontpopte,ontpopten,
  [h([refl,
      refl_pc_pp(tot)])]).

v(ontraad,ontraadt,ontraden,ontraden,[ontried,ontraadde],[ontrieden,ontraadden],
    [h([np_np,
	np_vp_obj,
        vp_obj,       % het wordt ontraden om ...
	transitive])]).

v(ontraadsel,ontraadselt,ontraadselen,ontraadseld,ontraadselde,ontraadselden,
    [h([transitive])]).

v(ontrafel,ontrafelt,ontrafelen,ontrafeld,ontrafelde,ontrafelden,
  [h([transitive,
      refl % word order
     ])]).

v(ontregel,ontregelt,ontregelen,ontregeld,ontregelde,ontregelden,
    [h([sbar_subj_np,
	transitive,
	intransitive,
	vp_subj_np])]).

v(ontrief,ontrieft,ontrieven,ontriefd,ontriefde,ontriefden,
    [h([transitive])]).

v(ontroer,ontroert,ontroeren,ontroerd,ontroerde,ontroerden,
    [h([sbar_subj_so_np,
	transitive,
	intransitive,
	vp_subj_so_np])]).

v(ontrol,ontrolt,ontrollen,ontrold,ontrolde,ontrolden,
    [z([intransitive]),
     h([np_np,
	transitive,
        refl,   % word order
	np_ld_pp])]).

v(ontruim,ontruimt,ontruimen,ontruimd,ontruimde,ontruimden,
  [h([transitive,
      intransitive])]).

v(ontruk,ontrukt,ontrukken,ontrukt,ontrukte,ontrukten,
    [h([np_np,
        fixed([[aan,de,vergetelheid],acc],norm_passive),
	transitive])]).

v(ontschiet,ontschiet,ontschieten,ontschoten,ontschoot,ontschoten,
  [z([so_np,
      sbar_subj_so_np
     ])]).

v(ontsier,ontsiert,ontsieren,ontsierd,ontsierde,ontsierden,
    [h([sbar_subj_so_np,
	transitive])]).

v(ontsla,ontslaat,inflected(ontslaan,ontslane),ontslagen,ontsloeg,ontsloegen,
    [h([transitive,
	np_pc_pp(uit),
	np_pc_pp(van)])]).

v(ontsluier,ontsluiert,ontsluieren,ontsluierd,ontsluierde,ontsluierden,
    [h([sbar_subj_so_np,
	transitive])]).

v(ontsluit,ontsluit,ontsluiten,ontsloten,ontsloot,ontsloten,
    [h([transitive,
	np_pc_pp(voor)])]).

v(ontsmet,ontsmet,ontsmetten,ontsmet,ontsmette,ontsmetten,
    [h([transitive,
        intransitive])]).

v(ontsnap,ontsnapt,ontsnappen,ontsnapt,ontsnapte,ontsnapten,
    [z([intransitive,
	so_np,
	ld_pp,
	pp_sbar_subj(aan), % het is aan onze aandacht ontsnapt dat ..
	pc_pp(aan)])]).

v(ontspan,ontspant,ontspannen,ontspannen,ontspande,ontspanden,
    [unacc([intransitive]),
     h([refl,
	transitive])]).

v(ontspin,ontspint,ontspinnen,ontsponnen,ontspon,ontsponnen,
  [h([refl,
      intransitive])]).

v(ontspoor,ontspoort,ontsporen,ontspoord,ontspoorde,ontspoorden,
    [unacc([intransitive])]).

v(ontspring,ontspringt,ontspringen,ontsprongen,ontsprong,ontsprongen,
    [unacc([so_np,
	intransitive,
	transitive,
	pc_pp(aan),
	pc_pp(uit)])]).

v(ontspruit,ontspruit,ontspruiten,ontsproten,ontsproot,ontsproten,
  [unacc([intransitive,
          so_np,
          np_pc_pp(uit),
          pc_pp(aan),
          pc_pp(uit)])]).

v(ontsta,ontstaat,inflected(ontstaan,ontstane),ontstaan,ontstond,ontstonden,
  [unacc([intransitive,
	  fixed([subj(weerstand),pc(tegen)],no_passive),
	  fixed([subj(vraag),pc(naar)],no_passive),
	  pc_pp(uit),
	  pc_pp(over)]          % ruzie/rumoer/stampei/...
        )]).

v(ontsteek,ontsteekt,ontsteken,ontstoken,ontstak,ontstaken,
    [unacc([intransitive,
	pc_pp(in)]),
     h([transitive])]).

v(ontsteel,ontsteelt,ontstelen,ontstolen,ontstal,ontstalen,
    [h([np_np,
	transitive])]).

v(ontstel,ontstelt,ontstellen,ontsteld,ontstelde,ontstelden,
    [unacc([intransitive]),
     h([sbar_subj_so_np,
	so_np,
	transitive]),
     b([vp_subj_so_np])]).

v(ontstem,ontstemt,ontstemmen,ontstemd,ontstemde,ontstemden,
    [h([intransitive,
	sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(ontstijg,ontstijgt,ontstijgen,ontstegen,ontsteeg,ontstegen,
    [unacc([intransitive,
	so_np,
	transitive,
	pc_pp(aan)])]).

v(onttakel,onttakelt,onttakelen,onttakeld,onttakelde,onttakelden,
    [h([transitive])]).

v(onttrek,onttrekt,onttrekken,onttrokken,onttrok,onttrokken,
    [h([np_np,
	transitive,  % esp. in nominalizations
	np_pc_pp(aan),
        refl_pc_pp(aan)   % word order "omdat zich hieraan vele bedrijven..."
        ])]).

v(onttroon,onttroont,onttronen,onttroond,onttroonde,onttroonden,
    [h([transitive])]).

v(ontval,ontvalt,ontvallen,ontvallen,ontviel,ontvielen,
    [unacc([so_np,
            fixed_dep(intransitive)
           ])]).

v(ontvang,ontvangt,ontvangen,ontvangen,ontving,ontvingen,
    [h([intransitive,
	transitive,
        np_pc_pp(van),  % iets ontvangen van
	fixed([{[acc(reactie),pc(op)]}],norm_passive)])]).

v(ontveins,ontveinst,ontveinzen,ontveinsd,ontveinsde,ontveinsden,
  [h([transitive,
      np_np,
      refl_sbar,
      np_sbar])]).

v(ontvel,ontvelt,ontvellen,ontveld,ontvelde,ontvelden,
  [h([transitive])]).

v(ontvlam,ontvlamt,ontvlammen,ontvlamd,ontvlamde,ontvlamden,
  [unacc([intransitive])]).

v(ontvlucht,ontvlucht,ontvluchten,ontvlucht,ontvluchtte,ontvluchtten,
  [unacc([intransitive,
          so_np,
          transitive])]).

v(ontvoer,ontvoert,ontvoeren,ontvoerd,ontvoerde,ontvoerden,
    [h([transitive])]).

v(ontvolg,ontvolgt,ontvolgen,ontvolgd,ontvolgde,ontvolgden,
    [h([transitive])]).

v(ontvouw,ontvouwt,ontvouwen,[ontvouwd,ontvouwen],ontvouwde,ontvouwden,
    [h([np_np,
	refl,
	transitive])]).

v(ontvreemd,ontvreemdt,ontvreemden,ontvreemd,ontvreemdde,ontvreemdden,
    [h([transitive,
	intransitive])]).

v(ontvriend,ontvriendt,ontvrienden,ontvriend,ontvriendde,ontvriendden,
    [h([transitive,
	intransitive])]).

v(ontwaak,ontwaakt,ontwaken,ontwaakt,ontwaakte,ontwaakten,
    [unacc([intransitive,
	pc_pp(uit)])]).

v(ontwaar,ontwaart,ontwaren,ontwaard,ontwaarde,ontwaarden,
    [h([sbar,
	transitive])]).

%% ongeldig maken (bv van strippenkaart)
v(ontwaard,ontwaardt,ontwaarden,ontwaard,ontwaardde,ontwaardden,
    [h([transitive])]).

v(ontwapen,ontwapent,ontwapenen,ontwapend,ontwapende,ontwapenden,
    [h([intransitive,
	transitive])]).

v(ontwar,ontwart,ontwarren,ontward,ontwarde,ontwarden,
    [h([transitive])]).

v(ontwater,ontwatert,ontwateren,ontwaterd,ontwaterde,ontwaterden,
    [h([intransitive,
	transitive])]).

v(ontwerp,ontwerpt,ontwerpen,ontworpen,ontwierp,ontwierpen,
  [h([transitive,
      intransitive])]).

v(ontwijk,ontwijkt,ontwijken,ontweken,ontweek,ontweken,
    [h([transitive])]).

v(ontwikkel,ontwikkelt,ontwikkelen,ontwikkeld,ontwikkelde,ontwikkelden,
    [h([refl,    % word order "waarin zich een kankercel ontwikkelde"
	refl_pc_pp(tot),
	part_refl(door),
	transitive])]).

v(ontworstel,ontworstelt,ontworstelen,ontworsteld,ontworstelde,ontworstelden,
    [h([np_np,
	refl_pc_pp(aan)])]).

v(ontwortel,ontwortelt,ontwortelen,ontworteld,ontwortelde,ontwortelden,
    [h([intransitive,
	transitive])]).

v(ontwricht,ontwricht,ontwrichten,ontwricht,ontwrichtte,ontwrichtten,
    [h([sbar_subj_so_np,
	transitive,
        intransitive])]).

v(ontzeg,ontzegt,ontzeggen,ontzegd,[ontzegde,ontzei],[ontzegden,ontzeiden],
    [h([np_np,
	refl_np,
	sbar_subj_so_np,
	so_pp_np,
	transitive])]).

v(ontzenuw,ontzenuwt,ontzenuwen,ontzenuwd,ontzenuwde,ontzenuwden,
    [h([sbar_subj_so_np,
	transitive])]).

v(ontzet,ontzet,ontzetten,ontzet,ontzette,ontzetten,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np,
	np_pc_pp(uit),
	np_pc_pp(van)])]).

v(ontzie,ontziet,inflected(ontzien,ontziene),ontzien,ontzag,ontzagen,
    [h([transitive])]).

v(ontzilt,ontzilt,ontzilten,ontzilt,ontziltte,ontziltten,
    [h([transitive])]).

v(onweer,onweert,onweren,geönweerd,onweerde,onweerden,
    [h([het_subj])]).  

v(oog,oogt,ogen,geoogd,oogde,oogden,
    [h([intransitive,
	nonp_copula,
	alsof_sbar,
	pc_pp(naar),
	pc_pp(op)])]).

v(oogst,oogst,oogsten,geoogst,oogstte,oogstten,
    [h([transitive,
	mod_pp(van), % er kan het hele jaar van geoogst worden
	intransitive])]).

v(oor,oort,oren,geoord,oorde,oorden,
    [h([intransitive,
	nonp_copula,
	alsof_sbar])]).

v(oordeel,oordeelt,oordelen,geoordeeld,oordeelde,oordeelden,oordele,
    [h([intransitive,
	sbar,
	transitive,
	pc_pp(over)])]).

v(oormerk,oormerkt,oormerken,geoormerkt,oormerkte,oormerkten,
    [h([transitive,
	intransitive])]).

v(open,opent,openen,geopend,opende,openden,
    [h([intransitive,
	refl,
        dip_sbar,
	fixed([[de,ogen],dat],imp_passive),
	transitive,
	pc_pp(met)])]).

v(openbaar,openbaart,openbaren,geopenbaard,openbaarde,openbaarden,
    [h([np_np,
	np_sbar,
	refl,
	sbar,
	transitive,
	vp])]).

v(operationaliseer,operationaliseert,operationaliseren,geoperationaliseerd,operationaliseerde,operationaliseerden,
    [h([transitive])]).

v(opereer,opereert,opereren,geopereerd,opereerde,opereerden,
    [h([intransitive,
	transitive,
	pc_pp(aan),
	np_pc_pp(aan)])]).

v(opper,oppert,opperen,geopperd,opperde,opperden,
    [h([intransitive,
	sbar,
	transitive,
	vp_no_control
       ])]).

v(opteer,opteert,opteren,geopteerd,opteerde,opteerden,
    [h([intransitive,
	pc_pp(voor),
        er_pp_vp(voor)])]).

v(optimaliseer,optimaliseert,optimaliseren,geoptimaliseerd,optimaliseerde,optimaliseerden,
    [h([transitive])]).

v(orakel,orakelt,orakelen,orakeld,orakelde,orakelden,
    [h([sbar,
	intransitive,
	transitive])]).

v(orden,ordent,ordenen,geordend,ordende,ordenden,
    [h([transitive])]).

v(oreer,oreert,oreren,georeerd,oreerde,oreerden,
    [h([sbar,
	intransitive,
	pc_pp(over)])]).

v(organiseer,organiseert,organiseren,georganiseerd,organiseerde,organiseerden,
    [h([sbar,
	transitive,
        np_mod_pp(in)])]).

v(oriënteer,oriënteert,oriënteren,georiënteerd,oriënteerde,oriënteerden,
    [h([transitive,
	np_pc_pp(naar),
	np_pc_pp(op),
        refl,                   % word-order:
	refl_pc_pp(op)          % want daar orienteren zich vele gegadigden
       ])]).

v(orkestreer,orkestreert,orkestreren,georkestreerd,orkestreerde,orkestreerden,
    [h([intransitive,
	transitive])]).  

%VL: zich outen = uit de kast komen?
v(out,out,outen,geout,outte,outten,
    [h([refl])]).

v(ouwehoer,ouwehourt,ouwehoeren,geouwehoerd,ouwehoerde,ouwehoerden,
    [h([intransitive])]).  

v(overbelast,overbelast,overbelasten,overbelast,overbelastte,overbelastten,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(overbluf,overbluft,overbluffen,overbluft,overblufte,overbluften,
    [h([sbar_subj_so_np,
	transitive,
	np_pc_pp(met)])]).

v(overboek,overboekt,overboeken,overboekt,overboekte,overboekten,
    [h([intransitive,
	transitive])]).  

v(overbrug,overbrugt,overbruggen,overbrugd,overbrugde,overbrugden,
    [h([sbar_subj_so_np,
	transitive])]).

v(overdek,overdekt,overdekken,overdekt,overdekte,overdekten,
    [h([transitive])]).

v(overdenk,overdenkt,overdenken,overdacht,overdacht,overdachten,
    [h([sbar,
	transitive])]).

v(overdonder,overdondert,overdonderen,overdonderd,overdonderde,overdonderden,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(overdrijf,overdrijft,overdrijven,overdreven,overdreef,overdreven,
    [h([intransitive,
	transitive,
	sbar])]).

v(overgiet,overgiet,overgieten,overgoten,overgoot,overgoten,
    [h([transitive,
	np_pc_pp(met)])]).

v(overhaal,overhaalt,overhalen,overhaald,overhaalde,overhaalden,
    [h([transitive,
        np_vp_obj1])]).

v(overhaast,overhaast,overhaasten,overhaast,overhaastte,overhaastten,
    [h([transitive])]).

v(overhandig,overhandigt,overhandigen,overhandigd,overhandigde,overhandigden,
    [h([np_np,
	so_pp_np,
	transitive])]).

v(overheers,overheerst,overheersen,overheerst,overheerste,overheersten,
    [h([intransitive,
	transitive])]).

v(overhoor,overhoort,overhoren,overhoord,overhoorde,overhoorden,
    [h([transitive])]).

v(overkap,overkapt,overkappen,overkapt,overkapte,overkapten,
    [h([transitive])]).

v(overklas,overklast,overklassen,overklast,overklaste,overklasten,
    [h([transitive])]).

v(overkoepel,overkoepelt,overkoepelen,overkoepeld,overkoepelde,overkoepelden,
    [h([transitive])]).

v(overkom,overkomt,overkomen,overkomen,overkwam,overkwamen,
    [unacc([so_np,
	sbar_subj_so_np])]).

v(overlaad,overlaadt,overladen,overladen,overlaadde,overlaadden,
    [h([refl_np,
	transitive,
	np_pc_pp(met)])]).

v(overlap,overlapt,overlappen,overlapt,overlapte,overlapten,
    [h([intransitive,
	transitive])]).

v(overleef,overleeft,overleven,overleefd,overleefde,overleefden,
    [h([transitive,
	intransitive])]).

v(overleg,overlegt,overleggen,overlegd,overlegde,overlegden,
    [h([intransitive,
	sbar,
	transitive,
	mod_pp(over),
	np_mod_pp(met),
	mod_pp(met)])]).

v(overlijd,overlijdt,overlijden,overleden,overleed,overleden,
    [unacc([intransitive,
	pc_pp(aan)])]).

v(overloop,overloopt,overlopen,overlopen,overliep,overliepen,
    [h([transitive])]).

v(overmag,overmag,overmogen,overmocht,overmocht,overmochten,
    [h([intransitive,
	transitive])]).

v(overman,overmant,overmannen,overmand,overmande,overmanden,
    [h([transitive])]).

v(overmeester,overmeestert,overmeesteren,overmeesterd,overmeesterde,overmeesterden,
    [h([transitive])]).

v(overnacht,overnacht,overnachten,overnacht,overnachtte,overnachtten,
    [h([intransitive])]).

v(overreed,overreedt,overreden,overreed,overreedde,overreedden,
    [h([transitive,
	np_vp_obj])]).

v(overrijd,overrijdt,overrijden,overreden,overreed,overreden,
    [h([transitive])]).

v(overroep,overroept,overroepen,overroepen,overriep,overriepen,
    [h([transitive])]).

v(overrompel,overrompelt,overrompelen,overrompeld,overrompelde,overrompelden,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(overrule,overrulet,overrulen,overruled,overrulede,overruleden,
    [h([transitive])]).

v(overschaduw,overschaduwt,overschaduwen,overschaduwd,overschaduwde,overschaduwden,
    [h([sbar_subj_so_np,
	transitive])]).

v(overschat,overschat,overschatten,overschat,overschatte,overschatten,
    [h([transitive])]).

v(overschilder,overschildert,overschilderen,overschilderd,overschilderde,overschilderden,
    [h([transitive,
	intransitive])]).

v(overschreeuw,overschreeuwt,overschreeuwen,overschreeuwd,overschreeuwde,overschreeuwden,
    [h([transitive])]).

v(overschrijd,overschrijdt,overschrijden,overschreden,overschreed,overschreden,
    [h([sbar_subj_so_np,
	transitive])]).

v(overschrijf,overschrijft,overschrijven,overschreven,overschreef,overschreven,
    [h([transitive])]).

v(overspan,overspant,overspannen,overspannen,overspande,overspanden,
    [h([transitive])]).

v(overspeel,overspeelt,overspelen,overspeeld,overspeelde,overspeelden,
    [h([transitive])]).

v(overspoel,overspoelt,overspoelen,overspoeld,overspoelde,overspoelden,
    [h([transitive])]).

v(overspuit,overspuit,overspuiten,overgespoten,overspoot,overspoten,
    [h([transitive])]).

v(overstelp,overstelpt,overstelpen,overstelpt,overstelpte,overstelpten,
  [h([sbar_subj_so_np,
      transitive,
      vp_subj_so_np,
      np_pc_pp(met)])]).

v(overstem,overstemt,overstemmen,overstemd,overstemde,overstemden,
    [h([transitive])]).

v(overstijg,overstijgt,overstijgen,overstegen,oversteeg,overstegen,
    [h([sbar_subj_so_np,
	transitive])]).

v(overstroom,overstroomt,overstromen,overstroomd,overstroomde,overstroomden,
  [b([transitive,
      intransitive,
      np_pc_pp(met)])]).

%% een beursemissie overtekenen
v(overteken,overtekent,overtekenen,overtekend,overtekende,overtekenden,
    [h([transitive])]).

v(overtij,overtijt,overtijen,overtijd,overtijdde,overtijdden,
    [h([intransitive])]).

v(overtreed,overtreedt,overtreden,overtreden,overtrad,overtraden,
    [h([transitive])]).

v(overtref,overtreft,overtreffen,overtroffen,overtrof,overtroffen,
    [h([sbar_subj_so_np,
	transitive])]).

v(overtrek,overtrekt,overtrekken,overtrokken,overtrok,overtrokken,
    [h([transitive,
	np_pc_pp(met)])]).

v(overtroef,overtroeft,overtroeven,overtroefd,overtroefde,overtroefden,
    [h([transitive])]).

v(overtuig,overtuigt,overtuigen,overtuigd,overtuigde,overtuigden,
    [h([intransitive,
	%refl,
	transitive,
	np_er_pp_sbar(van),
	acc_np_sbar,  % passive: omdat ik niet ben overtuigd dat..
	np_vp_obj1,
	obj_np_er_pp_vp(van),
	np_pc_pp(van)
	% refl_pc_pp(van)
       ])]).

v(overvaar,overvaart,overvaren,overvaren,overvoer,overvoeren,
    [h([transitive,
        sbar])]).  

v(overval,overvalt,overvallen,overvallen,overviel,overvielen,
    [h([sbar_subj_so_np,
	so_np,  % er overviel mij een raar gevoel
	transitive,
	vp_subj_so_np,
	np_pc_pp(met)])]).

v(overvleugel,overvleugelt,overvleugelen,overvleugeld,overvleugelde,overvleugelden,
    [h([sbar_subj_so_np,
	transitive])]).

v(overvlieg,overvliegt,overvliegen,overvlogen,overvloog,overvlogen,
    [h([transitive])]).  

v(overvoer,overvoert,overvoeren,overvoerd,overvoerde,overvoerden,
    [h([intransitive,
	transitive])]).

v(overvraag,overvraagt,overvragen,overvraagd,overvroeg,overvroegen,
    [h([intransitive,
	transitive])]).

v(overweeg,overweegt,overwegen,overwogen,overwoog,overwogen,
    [h([intransitive,
	sbar,
	transitive,
	vp])]).

v(overweldig,overweldigt,overweldigen,overweldigd,overweldigde,overweldigden,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(overwerk,overwerkt,overwerken,overwerkt,overwerkte,overwerkten,
    [h([refl])]).

v(overwin,overwint,overwinnen,overwonnen,overwon,overwonnen,
    [h([intransitive,
	transitive])]).

v(overwinter,overwintert,overwinteren,overwinterd,overwinterde,overwinterden,
  [h([intransitive,
      transitive  % deze plant moet overwinterd worden bij temperaturen van ...
     ])]).

v(overwoeker,overwoekert,overwoekeren,overwoekerd,overwoekerde,overwoekerden,
    [h([transitive,
	np_pc_pp(met)])]).

v(overzie,overziet,inflected(overzien,overziene),overzien,overzag,overzagen,
    [h([transitive,
	np_mod_pp(van),  % de gevolgen/consequenties overzien van
	sbar])]).

v(overzomer,overzomert,overzomeren,overzomerd,overzomerde,overzomerden,
    [h([intransitive])]).

v(paai,paait,paaien,gepaaid,paaide,paaiden,
    [h([intransitive,
	transitive])]).

v(paal,paalt,palen,gepaald,paalde,paalden,
  [h([intransitive,
      ld_pp,
      transitive])]).

v(paar,paart,paren,gepaard,paarde,paarden,
    [h([intransitive,
	transitive,
	np_pc_pp(aan),
	pc_pp(met)])]).

v(pacht,pacht,pachten,gepacht,pachtte,pachtten,
    [h([transitive])]).

v(pacificeer,pacificeert,pacificeren,gepacificeerd,pacificeerde,pacificeerden,
    [h([transitive])]).

v(paf,paft,paffen,gepaft,pafte,paften,
    [h([intransitive,
        transitive])]).  % een sjekkie

v(pak,pakt,pakken,gepakt,pakte,pakten,
    [h([intransitive,
	transitive,
	np_ld_pp,
	np_pc_pp(op),
	part_np_np(af),
	part_refl(samen),  % donkere wolken pakken zich samen
	part_intransitive(aan),
	part_intransitive(in),
	part_transitive(aan),
	part_np_pc_pp(aan,op),
	part_transitive(af),
	part_transitive(beet),
	part_transitive(in),
	part_transitive(mee), 
	part_transitive(op),
	part_transitive(samen),
        part_transitive(terug),
	part_transitive(uit),
	part_mod_pp(uit,met),  % VL
        part_fixed(uit,[nonp_pred],no_passive), % dat pakt goed uit
	part_transitive(vast),
        part_np_ld_pp(vast),
	part_np_pc_pp(af,van),
	part_pc_pp(uit,tegen)]),
     b([part_intransitive(uit)])]).

v(palm,palmt,palmen,gepalmd,palmde,palmden,
    [h([part_transitive(in)])]).

v(panikeer,panikeert,panikeren,gepanikeerd,panikeerde,panikeerden,
    [h([intransitive])]).

v(pantser,pantsert,pantseren,gepantserd,pantserde,pantserden,
    [h([transitive,
	np_pc_pp(tegen)])]).

v(pap,papt,pappen,gepapt,papte,papten,
    [h([intransitive,  % en nathouden
	part_pc_pp(aan,met)])]).

v(papegaai,papegaait,papegaaien,gepapegaaid,papegaaide,papegaaiden,
    [h([intransitive,
	part_transitive(na)
       ])]).

v(paradeer,paradeert,paraderen,geparadeerd,paradeerde,paradeerden,
    [h([intransitive,
	pc_pp(met)])]).

v(parafraseer,parafraseert,parafraseren,geparafraseerd,
  parafraseerde,parafraseerden,
    [h([transitive,
        intransitive])]).

v(pareer,pareert,pareren,gepareerd,pareerde,pareerden,
    [h([intransitive,
	transitive,
        sbar,
	np_pc_pp(met),
	pc_pp(met)])]).

v(parel,parelt,parelen,gepareld,parelde,parelden,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv])]).

v(parfumeer,parfumeert,parfumeren,geparfumeerd,parfumeerde,parfumeerden,
  [h([transitive,
      intransitive])]).

v(parkeer,parkeert,parkeren,geparkeerd,parkeerde,parkeerden,
    [h([intransitive,
	transitive,
	part_intransitive(in),
	part_transitive(in), % de auto kan zichzelf inparkeren
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv])]).

v(parodieer,parodieert,parodieren,geparodieerd,parodieerde,parodieerden,
  [h([transitive,
      intransitive])]).

v(parse,parset,parsen,geparset,parsete,parseten,
    [h([intransitive,
	transitive])]).

v(participeer,participeert,participeren,geparticipeerd,participeerde,participeerden,
    [h([intransitive,
	pc_pp(in)])]).

v(pas,past,passen,gepast,paste,pasten,
    [h([intransitive,
	so_np,
	transitive,
	ld_pp,
	ld_adv,
	sbar_subj,
	np_ld_pp,
	part_intransitive(bij),
	part_intransitive(op),
	% part_refl(aan),
	part_sbar(op),
	part_transitive(aan),
	part_np_pc_pp(aan,op),
	part_pc_pp(aan,op),
	part_transitive(af),
	part_transitive(bij),  % wij passen het verschil bij
	part_transitive(in),
	part_transitive(op),
	part_transitive(toe),
        part_als_pred_np(toe),
	part_transitive(uit),  % bridge
	part_vp(op),
	pc_pp(bij),
	pc_pp(op),
	part_np_ld_pp(in),
	part_np_pc_pp(aan,aan),
	part_np_pc_pp(toe,in),
	part_np_pc_pp(toe,op),
	part_pc_pp(op,met),
	part_pc_pp(op,op),
	part_pc_pp(op,voor),
	er_pp_vp(voor),
	part_er_pp_sbar(op,voor)
	% part_refl_pc_pp(aan,aan)
       ])]).

v(pass,passt,passen,gepasst,passte,passten,
    [h([intransitive,
	transitive,
	np_ld_dir,
	np_ld_adv,
	np_ld_pp])]).

v(passeer,passeert,passeren,gepasseerd,passeerde,passeerden,
    [z([intransitive,
	pc_pp(voor)]),
     h([np_pc_pp(voor)]),
     b([ld_pp,  % ik passeer langs het stadion (???)
	transitive])]).		% het koufront is Nederland gepasseerd

v(pasteuriseer,pasteuriseert,pasteuriseren,gepasteuriseerd,pasteuriseerde,pasteuriseerden,
    [h([transitive])]).

v(patenteer,patenteert,patenteren,gepatenteerd,patenteerde,patenteerden,
  [h([transitive,
      intransitive])]).

v(patrouilleer,patrouilleert,patrouilleren,gepatrouilleerd,patrouilleerde,patrouilleerden,
    [h([intransitive])]).

v(pauzeer,pauzeert,pauzeren,gepauzeerd,pauzeerde,pauzeerden,
  [h([intransitive,
      transitive])]).

v(peddel,peddelt,peddelen,gepeddeld,peddelde,peddelden,
    [z([ld_dir,
	part_intransitive(aan),
	ld_pp]),
     h([transitive]),
     b([intransitive])]).

v(peer,peert,peren,gepeerd,peerde,peerden,
    [z([fixed([[hem]],no_passive)]),
     h([np_ld_pp,
        np_ld_dir,
        part_transitive(binnen),
	part_intransitive(raak),
	part_intransitive(mis)
       ])]).

v(pees,peest,pezen,gepeesd,peesde,peesden,
    [h([intransitive])]).

v(peiger,peigert,peigeren,gepeigerd,peigerde,peigerden,
  [h([part_refl(af),  
      part_transitive(af)
     ])]).

v(peil,peilt,peilen,gepeild,peilde,peilden,
    [h([sbar,
	transitive,
	intransitive,
	pc_pp(naar)])]).

v(peins,peinst,peinzen,gepeinsd,peinsde,peinsden,
    [h([intransitive,
	sbar,
	ap_pred_refl,  % me suf
        transitive,  % VL "ik peins het ook"
	pc_pp(op),
	pc_pp(over),
        er_pp_sbar(over),
        er_pp_vp(over)])]).

v(peis,peist,peizen,gepeisd,peisde,peisden,
    [h([intransitive,
	sbar,
	transitive
       ])]).

v(pel,pelt,pellen,gepeld,pelde,pelden,
    [h([transitive,
	part_transitive(af),
	part_np_pc_pp(af,van),
	intransitive])]).

v(pen,pent,pennen,gepend,pende,penden,
    [h([transitive,
	part_transitive(neer),
        intransitive])]).

v(pendel,pendelt,pendelen,gependeld,pendelde,pendelden,
    [h([intransitive,
	ld_pp,
	ld_adv])]).

v(penetreer,penetreert,penetreren,gepenetreerd,penetreerde,penetreerden,
    [z([ld_pp]),
     h([transitive,
	np_pc_pp(met)])]).

v(pensioneer,pensioneert,pensioneren,gepensioneerd,pensioneerde,pensioneerden,
    [z([intransitive])]).

v(pep,pept,peppen,gepept,pepte,pepten,
    [h([part_sbar_subj_so_np(op),
	part_transitive(op),
	part_intransitive(op),
	part_vp_subj_so_np(op)])]).

v(peper,pepert,peperen,gepeperd,peperde,peperden,
    [h([transitive,
	part_transitive(in),
	part_np_sbar(in),
	part_np_np(in), % hij peperde ons de regels in
	part_fixed(in,[{[dat,het_pobj1]},nt(vp)],norm_passive),
	part_fixed(in,[dat,yt(vp)],norm_passive),
	part_fixed(in,[{[dat,het_pobj1]},nt(sbar)],norm_passive),
	part_fixed(in,[dat,yt(sbar)],norm_passive),
	pc_pp(in)])]).

v(perfectioneer,perfectioneert,perfectioneren,geperfectioneerd,perfectioneerde,perfectioneerden,
  [h([transitive,
      intransitive])]).

v(perform,performt,performen,geperformd,performde,performden,
    [h([intransitive,
	transitive])]).

v(perk,perkt,perken,geperkt,perkte,perkten,
    [h([part_sbar_subj_so_np(in),
	part_transitive(in)])]).

v(permitteer,permitteert,permitteren,gepermitteerd,permitteerde,permitteerden,
    [h([np_np,
	np_vp_obj,
	refl_np,
	fixed([refl,het_pobj1(vp)],no_passive),
	fixed([refl,het_pobj1(sbar)],no_passive),
	fixed([acc,het_pobj1(vp)],no_passive),
	fixed([acc,het_pobj1(sbar)],no_passive),
	transitive,
	refl_vp])]).

v(pers,perst,persen,geperst,perste,persten,
    [h([intransitive,
	transitive,
	np_ld_pp,
	refl_ld_pp,
	np_ld_dir,
	part_transitive(af),
	part_transitive(samen),
	part_transitive(uit)])]).

v(personifieer,personifieert,personifiëren,gepersonifieerd,personifieerde,
  personifieerden,
    [h([transitive])]).

v(pest,pest,pesten,gepest,pestte,pestten,
    [h([intransitive,
	transitive,
	np_mod_pp(met),
	mod_pp(met),  % ze worden gebruikt om mee te pesten
	part_transitive(weg)])]).

v(peur,peurt,peuren,gepeurd,peurde,peurden,
    [h([np_pc_pp(uit),
	ld_pp])]).

v(peuter,peutert,peuteren,gepeuterd,peuterde,peuterden,
    [h([intransitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	part_transitive(los),
        part_transitive(af),
        part_np_pc_pp(af,van),
	pc_pp(aan)])]).

v(peuzel,peuzelt,peuzelen,gepeuzeld,peuzelde,peuzelden,
    [h([intransitive,
	part_transitive(op)])]).  

v(picknick,picknickt,picknicken,gepicknickt,picknickte,picknickten,
    [h([intransitive])]).

v(piek,piekt,pieken,gepiekt,piekte,piekten,
    [h([intransitive,
	pred_np])]).

v(piel,pielt,pielen,gepield,pielde,pielden,
    [h([intransitive])]).

v(pieker,piekert,piekeren,gepiekerd,piekerde,piekerden,
    [h([intransitive,
	ap_pred_refl,  % ik pieker me suf/rot/gek
	pc_pp(over),
	er_pp_sbar(over),
	er_pp_vp(over)])]).

v(piep,piept,piepen,gepiept,piepte,piepten,
    [z([ld_pp]),
     h([intransitive,
	sbar,
	part_transitive(op),
        transitive
       ])]).

v(piepel,piepelt,piepelen,gepiepeld,piepelde,piepelden,
    [h([fixed_dep(intransitive),
	transitive])]).

v(pierce,piercet,piercen,gepiercet,piercete,pierceten,
    [h([transitive])]).

%% 'wijzen' met muis?
v(pijl,pijlt,pijlen,gepijld,pijlde,pijlden,
    [h([ld_pp,
        part_transitive(uit),   % de route uitpijlen VL
        part_transitive(af),    % de route afpijlen VL
        ld_adv])]).

v(pijnig,pijnigt,pijnigen,gepijnigd,pijnigde,pijnigden,
    [h([transitive])]).

v(pijp,pijpt,pijpen,[gepepen,gepijpt],[peep,pijpte],[pepen,pijpten],
    [h([intransitive,
	transitive])]).

v(pik,pikt,pikken,gepikt,pikte,pikten,
    [h([intransitive,
	sbar,
        sbar_obj,
        vp_obj,
	transitive,
	np_ld_pp,
	part_sbar(op),
	part_intransitive(aan),
	part_fixed(aan,[acc(wagon_DIM)],norm_passive),
	part_transitive(in),
	part_intransitive(in), % VL
	part_transitive(op),
	part_np_pc_pp(mee,van),
	part_transitive(af),
	part_transitive(mee),
	part_transitive(uit),
	pc_pp(naar),
	pc_pp(tegen),
	pc_pp(van),
	part_np_ld_pp(op),
	part_pc_pp(in,op)])]).

v(pin,pint,pinnen,gepind,pinde,pinden,
    [h([intransitive,
	transitive,
	part_transitive(vast),
	part_np_pc_pp(vast,op)])]).

v(ping,pingt,pingen,gepingd,pingde,pingden,
    [h([intransitive,
	transitive,
	np_sbar,
	sbar])]).

v(pingel,pingelt,pingelen,gepingeld,pingelde,pingelden,
    [h([intransitive])]).

v(pingpong,pingpongt,pingpongen,gepingpongd,pingpongde,pingpongden,
    [h([intransitive])]).

v(pink,pinkt,pinken,gepinkt,pinkte,pinkten,
    [h([part_transitive(weg),
	np_ld_pp])]).

v(pionier,pioniert,pionieren,gepionierd,pionierde,pionierden,
    [h([intransitive])]).

v(pis,pist,pissen,gepist,piste,pisten,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp])]).

v(pies,piest,piesen,gepiest,pieste,piesten,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp])]).

v(pit,pit,pitten,gepit,pitte,pitten,
    [h([intransitive,
	transitive])]).

v(pitch,pitcht,pitchen,gepitcht,pitchte,pitchten,
    [h([intransitive])]).

v(plaag,plaagt,plagen,geplaagd,plaagde,plaagden,
    [h([intransitive,
	sbar_subj_so_np,
	dip_sbar,
	np_sbar,
	transitive,
	np_er_pp_sbar(met),
	np_pc_pp(met)])]).

v(plaats,plaatst,plaatsen,geplaatst,plaatste,plaatsten,
    [h([np_ld_adv,
	np_ld_pp,
	np_ld_dir,
	refl,
	refl_ld_pp,  % achter de bal plaatste zich Robin van Persie
	transitive,
        fixed([[op,'non-actief'],acc],norm_passive),   %VL
        fixed([[op,non,actief],acc],norm_passive),   %VL
	fixed([{[pc(van),pp_pred(onder,bevel)]},acc],norm_passive),
	fixed([pp_pred(onder,bevel),acc],norm_passive),
	part_transitive(door),  % van aandelen
	part_transitive(over),
	part_np_ld_pp(over),
	part_transitive(terug),
	part_np_ld_pp(terug),
	refl_pc_pp(voor)])]).

v(plag,plagt,plaggen,geplagd,plagde,plagden,
    [h([part_transitive(af),
        intransitive,
	transitive])]).

v(plak,plakt,plakken,geplakt,plakte,plakten,
    [unacc([part_intransitive(vast),
	part_ld_pp(vast)]),
     h([intransitive,
	transitive,
	np_ld_pp,
	part_intransitive(aan),
	part_transitive(af),
	part_transitive(aan),
	part_transitive(in),
	part_transitive(op),
	part_np_np(af),
	part_np_np(op), % wij plakken hem een etiket op
	part_transitive(dicht),
	part_transitive(vast),
	pc_pp(aan),
	part_np_ld_pp(vast)])]).

v(plamuur,plamuurt,plamuren,geplamuurd,plamuurde,plamuurden,
  [h([transitive,
      intransitive,
      ap_pred_np
     ])]).

v(plan,plant,plannen,gepland,plande,planden,
  [h([transitive,
      sbar,
      part_transitive(in)])]).

v(plant,plant,planten,geplant,plantte,plantten,
  [h([transitive,
      intransitive, % we willen oogsten zonder te planten
      np_ld_pp,
      part_transitive(in),
      part_transitive(over),
      part_transitive(uit),
      part_np_np(in),
      part_refl(voort),
      part_transitive(aan),
      part_transitive(neer),
      part_transitive(voort)])]).

v(plas,plast,plassen,geplast,plaste,plasten,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	part_transitive(uit),
	np_ld_pp,
	pc_pp(met)])]).

v(plat,plat,platten,geplat,platte,platten,
  [unacc([part_intransitive(af)]),
   h([part_transitive(af)])
  ]).

v(plavei,plaveit,plaveien,geplaveid,plaveide,plaveiden,
    [h([intransitive,
	transitive])]).

v(playback,playbackt,playbacken,geplaybackt,playbackte,playbackten,
    [h([intransitive,
	transitive])]).

v(pleeg,pleegt,plegen,gepleegd,pleegde,pleegden,
    [h([transitive,
	aux(te),
        fixed([{[acc(onderhoud),pc(aan)]}],norm_passive),
	fixed([{[acc(overleg),mod_pp(met)]}],norm_passive),
	fixed([{[acc(overleg),mod_pp(over)]}],norm_passive)
       ])
    ]).

v(pleister,pleistert,pleisteren,gepleisterd,pleisterde,pleisterden,
    [h([intransitive,
	transitive])]).

v(pleit,pleit,pleiten,gepleit,pleitte,pleitten,
    [h([intransitive,
	sbar,
	vp,
	transitive,
        part_transitive(vrij),
	er_pp_sbar(voor),
	er_pp_vp(voor),
                                    % het pleit voor hem dat ...
	pp_sbar_subj_opt_het(voor), % voor deze zienswijze pleit dat ...
	pp_sbar_subj_opt_het(tegen),
	pc_pp(tegen),
	pc_pp(voor)])]).

v(pleng,plengt,plengen,geplengd,plengde,plengden,
    [h([intransitive,
	transitive %tranen
       ])]).

v(plens,plenst,plenzen,geplensd,plensde,plensden,
    [h([het_subj,
	ld_pp,
        part_intransitive(neer),
	np_ld_pp])]).

v(plet,plet,pletten,geplet,plette,pletten,
    [h([intransitive,
	transitive])]).

v(pleur,pleurt,pleuren,gepleurd,pleurde,pleurden,
    [h([np_ld_pp,
	ld_pp,
	np_ld_dir,
	part_intransitive(op)])]).

v(plezier,pleziert,plezieren,geplezierd,plezierde,plezierden,
  [h([so_np,
      transitive,
      sbar_subj_so_np,
      np_pc_pp(met)])]).

v(ploeg,ploegt,ploegen,geploegd,ploegde,ploegden,
    [b([ld_pp]),
     h([intransitive,
	transitive,
	part_transitive(door),
	part_transitive(om)])]).

v(ploeter,ploetert,ploeteren,geploeterd,ploeterde,ploeterden,
    [b([ld_pp]),
     h([intransitive,
        part_intransitive(aan), % ze ploeteren maar wat aan
	pc_pp(aan),
	pc_pp(op)])]).

v(plof,ploft,ploffen,geploft,plofte,ploften,
    [z([ld_pp,
	ld_adv,
	ld_dir,
	part_intransitive(neer),
	part_ld_pp(neer),
	part_ld_adv(neer)]),
     h([intransitive,
	np_ld_pp,
	np_ld_dir])]).

v(plons,plonst,plonzen,geplonsd,plonsde,plonsden,
    [z([ld_pp,
	ld_adv,
	ld_dir]),
     h([np_ld_pp,
	np_ld_adv])]).

v(plooi,plooit,plooien,geplooid,plooide,plooiden,
    [h([ap_pred_np,
	intransitive,
	transitive])]).

v(plug,plugt,pluggen,geplugd,plugde,plugden,
    [h([part_transitive(in),
	part_intransitive(in),
	np_ld_pp])]).

v(pluim,pluimt,pluimen,gepluimd,pluimde,pluimden,
    [h([intransitive,
	transitive])]).

v(pluis,pluist,pluizen,[geplozen,gepluisd],[ploos,pluisde],[plozen,pluisden],
    [h([transitive,
	part_transitive(uit),
	part_transitive(na)])]).
  
v(pluk,plukt,plukken,geplukt,plukte,plukten,
    [h([ap_pred_np,
	transitive,
%	np_pc_pp(van), LD
	fixed([{[acc(vrucht),pc(van)]}],norm_passive),
	np_ld_pp,
	pc_pp(aan)])]).

v(plunder,plundert,plunderen,geplunderd,plunderde,plunderden,
    [h([transitive,
	intransitive])]).

%% en te minnen
v(plus,plust,plussen,geplust,pluste,plusten,
  [h([intransitive,
      meas  % de koers pluste 3%
     ])]).

v(poch,pocht,pochen,gepocht,pochte,pochten,
    [h([intransitive,
	sbar,
	pc_pp(op)])]).

v(pocheer,pocheert,pocheren,gepocheerd,pocheerde,pocheerden,
    [h([transitive])]).

v(poedel,poedelt,poedelen,gepoedeld,poedelde,poedelden,
    [h([intransitive])]).

v(poeder,poedert,poederen,gepoederd,poederde,poederden,
    [h([np_np,
	transitive])]).

v(poeier,poeiert,poeieren,gepoeierd,poeierde,poeierden,
    [h([part_transitive(af),
        ld_pp,
        ld_dir,
        np_ld_pp,
        ap_pred_np])]).

v(poep,poept,poepen,gepoept,poepte,poepten,
    [h([intransitive,
        part_transitive(uit)])]).

v(poets,poetst,poetsen,gepoetst,poetste,poetsten,
    [h([ap_pred_np,
	transitive,
	intransitive,
	part_transitive(weg),
	part_sbar_subj_so_np(op),
	part_transitive(op)])]).

v(pof,poft,poffen,gepoft,pofte,poften,
    [h([transitive,
	ap_pred_np])]).

v(poker,pokert,pokeren,gepokerd,pokerde,pokerden,
    [h([intransitive])]).

v(polariseer,polariseert,polariseren,gepolariseerd,polariseerde,polariseerden,
    [h([intransitive,
	sbar_subj_so_np,
	transitive])]).

v(polder,poldert,polderen,gepolderd,polderde,polderden,
    [h([part_transitive(in),
	intransitive])]).
  
v(polijst,polijst,polijsten,gepolijst,polijstte,polijstten,
  [h([transitive,
      intransitive])]).

v(politiseer,politiseert,politiseren,gepolitiseerd,politiseerde,politiseerden,
    [z([intransitive]),
     h([transitive])]).

v(pols,polst,polsen,gepolst,polste,polsten,
    [h([transitive,
	sbar,     % polsen of ..
	np_sbar,  % hem polsen of ..
	np_pc_pp(om),
	np_pc_pp(over),
	np_pc_pp(ten)])]).

v(pomp,pompt,pompen,gepompt,pompte,pompten,
    [h([intransitive,
	transitive,
	part_transitive(door),
	part_np_ld_pp(door),
	part_transitive(in),
	part_np_np(in),		% hij krijgt lucht ingepompt
	part_transitive(leeg),
	part_transitive(op),
	part_transitive(over),
	part_transitive(rond),
	part_transitive(vol),
	part_transitive(weg),
	np_ld_pp,
	part_np_ld_pp(op),
	part_np_ld_pp(over),
	part_np_ld_pp(weg)])]).


v(pond,pondt,ponden,gepond,pondde,pondden,
    [h([part_transitive(uit)])]).

v(poneer,poneert,poneren,geponeerd,poneerde,poneerden,
    [h([transitive,
	sbar])]).

v(poog,poogt,pogen,gepoogd,poogde,poogden,
    [h([vp,
	subj_control(te),
	transitive])]).

v(pook,pookt,poken,gepookt,pookte,pookten,
    [h([part_transitive(op)])]).

% "poelen"
v(pool,poolt,poolen,gepoold,poolde,poolden,
    [h([intransitive])]).

v(poort,poort,poorten,gepoort,poorte,poorten,
    [h([transitive])]).

v(poot,poot,poten,gepoot,pootte,pootten,
    [h([transitive,
	part_np_ld_pp(neer),
	part_np_ld_adv(neer)])]).

v(popel,popelt,popelen,gepopeld,popelde,popelden,
    [h([intransitive,
        vp])]).  % ze staan te popelen om...

v(populariseer,populariseert,populariseren,gepopulariseerd,populariseerde,
  populariseerden,
    [h([intransitive,
	transitive])]).

v(por,port,porren,gepord,porde,porden,
    [h([intransitive,
	transitive,
        part_transitive(aan),   % Vlaams
        part_np_vp_obj1(aan),
        part_transitive(op),
        part_np_vp_obj1(op),  % je moet hem opporren om ...
        np_pc_pp(voor),    % hij is er niet voor te porren
        np_er_pp_vp(voor), % hij is er niet voor te porren om ...
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv])]).

v(portretteer,portretteert,portretteren,geportretteerd,portretteerde,portretteerden,
    [h([transitive])]).

v(poseer,poseert,poseren,geposeerd,poseerde,poseerden,
    [h([intransitive,
	pc_pp(voor)])]).

v(positioneer,positioneert,positioneren,gepositioneerd,positioneerde,
  positioneerden,
    [h([transitive,
        np_ld_pp,
        als_pred_np])]).
    

v(post,post,posten,gepost,postte,postten,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv])]).

v(posteer,posteert,posteren,geposteerd,posteerde,posteerden,
    [h([transitive,
        refl  % word order
               % daar hebben zich twee agenten geposteerd
       ])]).

v(postuleer,postuleert,postuleren,gepostuleerd,postuleerde,postuleerden,
    [h([intransitive,
	sbar,
	transitive])]).

v(pot,pot,potten,gepot,potte,potten,
    [h([transitive,
	part_transitive(op)])]).

v(praat,praat,praten,gepraat,praatte,praatten,
    [h([ap_pred_np,
	np_np,
	intransitive,
	transitive,
	np_ld_dir,
	np_ld_pp,
        mod_pp(doorheen),
	part_np_np(aan),
	part_np_sbar(aan),     % we hebben hem aangepraat dat ..
	part_intransitive(af), % Er werd wat afgepraat
        part_intransitive(mee),
	part_intransitive(na),
        part_transitive(bij),
        part_np_pc_pp(bij,over),
	part_transitive(door),
	part_dip_sbar(door),
	part_transitive(mee),  % een woordje meepraten x
	part_transitive(na),
	part_acc_np_dip_sbar(na),
	part_transitive(om),
	part_transitive(uit),
	part_transitive(goed),
	part_sbar_obj_opt_het(goed),
	mod_pp(met),
        part_intransitive(bij),
        part_mod_pp(bij,met),
	pc_pp(over),
	part_pc_pp(door,over),
	er_pp_sbar(over),
	pc_pp(tegen),
        part_pc_pp(aan,tegen),  % iemand om tegen aan te praten (?)
	pc_pp(van),
	part_pc_pp(in,op),
	part_np_mod_pp(uit,met),
	part_mod_pp(mee,met),
	part_fixed(heen,[svp_pp(over,hoofd)],no_passive),
	part_pc_pp(mee,over),
	part_pc_pp(na,over)])]).

v(prak,prakt,prakken,geprakt,prakte,prakten,
    [h([transitive,
	intransitive])]).  

v(prakkiseer,prakkiseert,prakkiseren,geprakkiseerd,prakkiseerde,prakkiseerden,
    [h([intransitive,
        transitive,
        pc_pp(over)])]).

v(praktiseer,praktiseert,praktiseren,gepraktiseerd,praktiseerde,praktiseerden,
    [h([intransitive,
	transitive])]).

%% een prangende vraag is een vraag die prangt
v(prang,prangt,prangen,geprangd,prangde,prangden,
    [h([intransitive])]).

v(preciseer,preciseert,preciseren,gepreciseerd,preciseerde,preciseerden,
    [h([sbar,
	transitive])]).

v(predik,predikt,[prediken,predikken],gepredikt,predikte,predikten,
    [h([intransitive,
	sbar,
	transitive,
	pc_pp(over)])]).

v(preek,preekt,preken,gepreekt,preekte,preekten,
    [h([intransitive,
	sbar,
	transitive,
	mod_pp(over),
	pc_pp(tegen)])]).

v(prefereer,prefereert,prefereren,geprefereerd,prefereerde,prefereerden,
    [h([sbar,
	transitive,
	vp,
	vp_obj,
	np_pc_pp(boven)])]).

v(preludeer,preludeert,preluderen,gepreludeerd,preludeerde,preludeerden,
    [h([intransitive,
	pc_pp(op)])]).

v(prent,prent,prenten,geprent,prentte,prentten,
    [h([np_np,
	transitive,
        part_transitive(in),
	np_pc_pp(in),
	part_np_np(in),
	part_np_sbar(in)])]).

v(prepareer,prepareert,prepareren,geprepareerd,prepareerde,prepareerden,
    [h([transitive,
	np_pc_pp(op),
	np_pc_pp(voor)])]).

v(pres,prest,pressen,geprest,preste,presten,
    [h([transitive,
	np_pc_pp(tot)])]).

v(presenteer,presenteert,presenteren,gepresenteerd,presenteerde,presenteerden,
    [h([als_pred_np,
	np_np,
	so_pp_np,
	transitive,
        intransitive])]).

v(presideer,presideert,presideren,gepresideerd,presideerde,presideerden,
    [h([intransitive,
      transitive,
      pc_pp(over)])]).

v(presteer,presteert,presteren,gepresteerd,presteerde,presteerden,
    [h([intransitive,
        vp_obj, % ze presteren het om ...
	transitive])]).

v(pretendeer,pretendeert,pretenderen,gepretendeerd,pretendeerde,pretendeerden,
    [h([sbar,
	transitive,
	vp])]).

v(prevaleer,prevaleert,prevaleren,geprevaleerd,prevaleerde,prevaleerden,
    [h([intransitive,
	pc_pp(boven),
	transitive
	% refl_pc_pp(van) ???
       ])]).

v(prevel,prevelt,prevelen,gepreveld,prevelde,prevelden,
    [h([intransitive,
	sbar,
	transitive])]).

v(priem,priemt,priemen,gepriemd,priemde,priemden,
    [h([intransitive,
        ld_dir,  % de lucht in
	transitive])]).

v(prijk,prijkt,prijken,geprijkt,prijkte,prijkten,
    [h([ld_adv,
	ld_pp,
	pc_pp(met),
        intransitive])]).

v(prijs,prijst,prijzen,geprezen,prees,prezen,
    [h([als_pred_np,
	transitive,
	part_transitive(in),
	sbar,
	pp_sbar(in), % het valt in hem te prijzen dat ...
	np_ld_dir,  % ik prijs hem de hemel in
        fixed([[gelukkig],acc,sbar],no_passive),
        fixed([{[[gelukkig],pc(met)]},acc],no_passive),
	part_transitive(aan),
        part_als_pred_np(aan),
        part_als_pred_np_sbar(aan),
        part_als_pred_np_vp(aan)])]).

v(prijs,prijst,prijzen,geprijsd,prijsde,prijsden,
  [h([transitive,
      part_transitive(af),
      part_transitive(om)	% naar euro's
     ])]).

v(prik,prikt,prikken,geprikt,prikte,prikten,
    [h([ap_pred_np,
	intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	refl_pc_pp(aan),
	part_intransitive(tegen),  % VL tegenscoren
	part_transitive(aan),
	part_transitive(door),
	part_transitive(in),
	pc_pp(naar)])]).

v(prikkel,prikkelt,prikkelen,geprikkeld,prikkelde,prikkelden,
    [h([intransitive,
	sbar_subj_so_np,
	transitive,
	np_vp_obj1,
	np_pc_pp(tot)])]).

v(primeer,primeert,primeren,geprimeerd,primeerde,primeerden,
  [h([transitive,
      intransitive])]).

v(print,print,printen,geprint,printte,printten,
    [h([intransitive,
	part_transitive(af),
	part_transitive(uit),
	transitive])]).

v(privatiseer,privatiseert,privatiseren,geprivatiseerd,privatiseerde,privatiseerden,
  [h([transitive,
      intransitive])]).

v(probeer,probeert,proberen,geprobeerd,probeerde,probeerden,
    [h([transitive,
	vp,
	sbar, % of
	np_pc_pp(met),
	np_er_pp_vp(aan), % ik probeerde er alles aan om ...
	part_sbar(uit),
	part_transitive(uit),
	subj_control(te),
	part_np_pc_pp(uit,op),
        intransitive
       ])]).

v(procedeer,procedeert,procederen,geprocedeerd,procedeerde,procedeerden,
    [h([intransitive,
	mod_pp(over),
	pc_pp(tegen),
	pc_pp(voor)])]).

v(proclameer,proclameert,proclameren,geproclameerd,proclameerde,proclameerden,
    [h([transitive,
        pred_np])]).

v(produceer,produceert,produceren,geproduceerd,produceerde,produceerden,
    [h([transitive,
	intransitive])]).

v(proef,proeft,proeven,geproefd,proefde,proefden,
    [h([intransitive,
	transitive,
	sbar,			% ik proef of het gaar is
	np_pc_pp(in),		% ik proef arrogantie in zijn woorden / er arrogantie in
	pc_pp(van)])]).

v(proest,proest,proesten,geproest,proestte,proestten,
    [h([intransitive,
	part_transitive(uit),
	sbar
       ])]).

v(professionaliseer,professionaliseert,professionaliseren,
  geprofessionaliseerd,professionaliseerde,professionaliseerden,
    [h([intransitive,
	transitive])]).

v(profeteer,profeteert,profeteren,geprofeteerd,profeteerde,profeteerden,
    [h([intransitive,
	sbar,
	transitive])]).

v(profileer,profileert,profileren,geprofileerd,profileerde,profileerden,
    [h([transitive,
	refl,
	als_pred_np,
	als_pred_refl])]).

v(profiteer,profiteert,profiteren,geprofiteerd,profiteerde,profiteerden,
    [h([intransitive,
        part_intransitive(mee),
	part_pc_pp(mee,van),
	pc_pp(van),
	er_pp_sbar(van)])]).

v(prognotiseer,prognotiseert,prognotiseren,geprognotiseerd,prognotiseerde,
  prognotiseerden,
    [h([sbar,
        transitive])]).

v(programmeer,programmeert,programmeren,geprogrammeerd,programmeerde,programmeerden,
    [h([intransitive,
	transitive])]).

v(projecteer,projecteert,projecteren,geprojecteerd,projecteerde,projecteerden,
    [h([transitive,
	np_pc_pp(op)])]).

v(proklameer,proklameert,proklameren,geproklameerd,proklameerde,proklameerden,
    [h([transitive])]).

v(prolifereer,prolifereert,prolifereren,geprolifereerd,prolifereerde,
  prolifereerden,
    [h([intransitive,
	transitive])]).

v(prolongeer,prolongeert,prolongeren,geprolongeerd,prolongeerde,prolongeerden,
    [h([transitive])]).

v(promoot,promoot,promoten,gepromoot,promootte,promootten,
    [h([transitive,
        sbar])]).

v(promoveer,promoveert,promoveren,gepromoveerd,promoveerde,promoveerden,
    [z([intransitive,
	ld_pp,
	pc_pp(op),
	pc_pp(tot)]),
     h([transitive,
	np_pc_pp(tot)]),
     b([ap_pred_np])]).

v(pronk,pronkt,pronken,gepronkt,pronkte,pronkten,
    [h([intransitive,
	ld_pp,
	ld_adv,
	pc_pp(met)])]).

v(proost,proost,proosten,geproost,proostte,proostten,
    [h([sbar,
	intransitive,
	pc_pp(op)])]).

v(prop,propt,proppen,gepropt,propte,propten,
    [h([intransitive,
	transitive,
	np_np_ld_pp,
	np_ld_pp,
	np_ld_dir,
        part_transitive(op),
	part_transitive(vol)])]).

v(propageer,propageert,propageren,gepropageerd,propageerde,propageerden,
    [h([sbar,
	transitive,
	vp])]).

v(prostitueer,prostitueert,prostitueren,geprostitueerd,
  prostitueerde,prostitueerden,
    [h([transitive])]).

v(protesteer,protesteert,protesteren,geprotesteerd,protesteerde,protesteerden,
    [h([intransitive,
	transitive,
	sbar,
	pc_pp(tegen),
        er_pp_sbar(tegen)])]).

v(provoceer,provoceert,provoceren,geprovoceerd,provoceerde,provoceerden,
    [h([intransitive,
	sbar_subj_so_np,
	transitive,
	vp_subj_so_np,
	np_pc_pp(tot),
	pc_pp(tot)])]).

v(pruil,pruilt,pruilen,gepruild,pruilde,pruilden,
    [h([intransitive,
        sbar])]).

v(pruim,pruimt,pruimen,gepruimd,pruimde,pruimden,
    [h([intransitive,
	transitive])]).

v(pruts,prutst,prutsen,geprutst,prutste,prutsten,
    [h([intransitive,
	np_ld_pp,
	np_ld_dir,
	pc_pp(aan)])]).

v(pruttel,pruttelt,pruttelen,geprutteld,pruttelde,pruttelden,
    [h([intransitive,
        part_intransitive(na),
        part_intransitive(tegen),
	sbar,
	part_sbar(tegen),
	transitive])]).

v(puber,pubert,puberen,gepuberd,puberde,puberden,
    [h([intransitive])]).

v(publiceer,publiceert,publiceren,gepubliceerd,publiceerde,publiceerden,
    [h([intransitive,
	sbar,
	transitive,
	vp,
	pc_pp(over),
	np_pc_pp(over)])]).

v(puf,puft,puffen,gepuft,pufte,puften,
    [h([intransitive,
	sbar,
	part_intransitive(uit)])]).

v(puil,puilt,puilen,gepuild,puilde,puilden,
    [z([ld_pp,
	part_pc_pp(uit,van)]),
     h([part_intransitive(uit)]),
     b([part_transitive(uit)])]).

v(pulk,pulkt,pulken,gepulkt,pulkte,pulkten,
    [h([ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_dir,
	part_transitive(los)])]).

v(pulseer,pulseert,pulseren,gepulseerd,pulseerde,pulseerden,
    [h([intransitive])]).

v(punt,punt,punten,gepunt,puntte,puntten,
    [h([transitive])]).

v(punter,puntert,punteren,gepunterd,punterde,punterden,
    [h([intransitive,
        np_ld_pp,
        np_ld_dir])]).

v(pureer,pureert,pureren,gepureerd,pureerde,pureerden,
    [h([transitive])]).

v(push,pusht,pushen,gepusht,pushte,pushten,
    [h([transitive,
	np_vp_obj1,
	intransitive])]).

v(put,put,putten,geput,putte,putten,
    [h([intransitive,
	transitive,
	pc_pp(uit),
	np_pc_pp(uit),
	part_sbar_subj_so_np(uit),
	part_transitive(uit),
	part_refl_pc_pp(uit,in)])]).

v(puur,puurt,puren,gepuurd,puurde,puurden,
    [h([transitive,
        np_pc_pp(uit)])]).

v(puzzel,puzzelt,puzzelen,gepuzzeld,puzzelde,puzzelden,
  [h([intransitive,
      part_transitive(uit),
      part_sbar(uit)])]).

v(raad,raadt,raden,geraden,[ried,raadde],[rieden,raadden],
    [h([intransitive,  % raden maar!
	sbar,
	so_np,
	transitive,
	part_np_np(aan),
	part_np_np(af),
	part_np_vp_obj(aan),
	part_np_vp_obj(af),
	part_transitive(aan),
	part_transitive(af),
	part_vp_no_control(af),
	part_vp_no_control(aan),
	part_sbar_obj(af),
	part_sbar_obj(aan), 
	part_sbar(af),  % afgeraden wordt om ...
	part_sbar(aan), % aangeraden wordt om ...
	part_so_np_sbar_obj(af),
	part_so_np_sbar_obj(aan), 
        part_vp_obj(af),  % het wordt afgeraden om ...
        part_vp_obj(aan), % het wordt aangeraden om ...
        part_so_np_vp_obj(af),  % het wordt jullie afgeraden om ...
        part_so_np_vp_obj(aan), % het wordt jullie aangeraden om ...
	pc_pp(naar)])]).

v(raadpleeg,raadpleegt,raadplegen,geraadpleegd,raadpleegde,raadpleegden,
    [h([transitive,
	np_pc_pp(over)])]).

v(raak,raakt,raken,geraakt,raakte,raakten,
    [unacc([aan_het,
	    nonp_copula,
	    copula_np,
%%%	    nonp_copula_np,
	    ld_pp,
	    ld_adv,
            pc_pp(aan),   % aan de drank/diarree/...
	    part_intransitive(aan),
	    part_intransitive(af),
	    part_intransitive(bekend),
	    part_sbar_subj_no_het(bekend),
	    part_intransitive(los),
            part_intransitive(op),  % geduld etc
	    part_intransitive(uit),
	    part_intransitive(zoek),
%%%	    part_so_pp_np(kwijt),
%%%	    part_transitive(kwijt),
            pc_pp(overheen),      % ik ben er nu wel overheen
	    er_pp_sbar(overheen), % ik ben er nog niet overheen dat ...
	    pred_er_pp_sbar(van),
	    pred_er_pp_vp(van),
	    pred_pc_pp(van),
            fixed([[bijster],[het,spoor]],no_passive),
            fixed([{[[in,botsing],pc(met)]}],no_passive),
	    fixed([{[pc(met),svp_pp(in,conflict)]}],no_passive),
	    fixed([svp_pp(in,conflict)],no_passive),
	    fixed([{[pc(met),pp_pred(in,contact)]}],no_passive),
	    fixed([{[pc(met),[in,gevecht]]}],no_passive),
	    fixed([{[pc(met),[slaags]]}],no_passive),
	    fixed([{[[thuis],pc(in)]}],no_passive),
	    fixed([{[[verzeild],ld_pp]}],no_passive),
	    fixed([{[[verzeild],ld_adv]}],no_passive),
            fixed([[in,de,vergetelheid]],no_passive),
	    fixed([[in,omloop]],no_passive),
            fixed([ap_pred(bekend),sbar_subj_opt_het],no_passive),
	    fixed([ap_pred('in zwang')],no_passive),
	    fixed([[in,gevecht]],no_passive),
	    fixed([[slaags]],no_passive),
	    fixed([copula_np(kwijt,mod_pp(van))],no_passive),
%%%	    part_np_pc_pp(kwijt,van),
				% daar raakte ik de kluts/tel/.. van kwijt
	    part_pc_pp(los,van)]),
     h([intransitive,
	sbar_subj_so_np,
	pc_pp(aan),		% dat raakt aan bedrog
	                        % daar mag niet aan geraakt worden
	transitive,
	part_transitive(aan),
	part_np_pc_pp(aan,met)])]).

v(raam,raamt,ramen,geraamd,raamde,raamden,
    [h([transitive,
	sbar,
	np_pc_pp(op)])]).

v(raap,raapt,rapen,geraapt,raapte,raapten,
    [h([transitive,
	np_ld_pp,
	part_transitive(op),
	part_np_ld_pp(op),
        part_intransitive(op) % raap op!
       ])]).

v(raas,raast,razen,geraasd,raasde,raasden,
    [b([ld_pp,
        ld_dir, % de berg af
        part_intransitive(aan),  % hij komt aanrazen
	part_intransitive(langs)]),
     h([dip_sbar,
	intransitive])]).

v(raaskal,raaskalt,raaskallen,geraaskald,raaskalde,raaskalden,
    [h([intransitive,
        sbar])]).

v(race,racet,racen,geracet,racete,raceten,
    [b([ld_pp,
	ld_dir,
        part_intransitive(aan)]),
     h([intransitive
       ])]).

v(radicaliseer,radicaliseert,radicaliseren,geradicaliseerd,radicaliseerde,radicaliseerden,
  [b([intransitive])]).

v(rafel,rafelt,rafelen,gerafeld,rafelde,rafelden,
  [h([transitive]),
   b([intransitive])]).

v(raffel,raffelt,raffelen,geraffeld,raffelde,raffelden,
  [h([part_transitive(af),
      ld_pp,
      intransitive])]).

v(raffineer,raffineert,raffineren,geraffineerd,raffineerde,raffineerden,
    [h([transitive])]).

v(rakel,rakelt,rakelen,gerakeld,rakelde,rakelden,
    [h([part_transitive(op)])]).

v(ram,ramt,rammen,geramd,ramde,ramden,
    [b([ld_pp]),
     h([ap_pred_np,
	transitive,
        intransitive,
	np_np_ld_pp, % dat kregen wij er vroeger in geramd
	np_ld_pp,
	np_ld_dir])]).

v(rammel,rammelt,rammelen,gerammeld,rammelde,rammelden,
    [h([intransitive,
	transitive,
	pc_pp(aan),
	pc_pp(op)])]).

v(rand,randt,randen,gerand,randde,randden,
    [h([part_transitive(aan)])]).

v(randomiseer,randomiseert,randomiseren,gerandomiseerd,randomiseerde,randomiseerden,
  [h([intransitive,
      transitive])]).

v(rangeer,rangeert,rangeren,gerangeerd,rangeerde,rangeerden,
    [h([intransitive,
	transitive,
	part_transitive(uit)])]).
 
v(rangschik,rangschikt,rangschikken,gerangschikt,rangschikte,rangschikten,
    [h([transitive,
	np_pc_pp(in),
	np_pc_pp(naar),
	np_pc_pp(onder),
	np_pc_pp(op)])]).

v(ransel,ranselt,ranselen,geranseld,ranselde,ranselden,
    [h([ap_pred_np,
	intransitive,
	transitive,
	part_transitive(af)])]).

v(rap,rapt,rappen,gerapt,rapte,rapten,
    [h([intransitive,
	sbar,
	transitive,
        part_intransitive(mee),
	part_transitive(mee),
	part_sbar(mee)])]).

v(rapporteer,rapporteert,rapporteren,gerapporteerd,rapporteerde,rapporteerden,
    [h([sbar,
	so_pp_np,
	so_np,
	transitive,
	intransitive,
	np_np,  % mij is iets anders gerapporteerd
	vp,
	pc_pp(aan)])]).

v(rasp,raspt,raspen,geraspt,raspte,raspten,
    [h([intransitive,
	np_mod_pp(over),  % rasp er wat kaas over
	transitive])]).

v(ratel,ratelt,ratelen,gerateld,ratelde,ratelden,
    [h([intransitive,
	part_transitive(af),
	sbar])]).

v(ratificeer,ratificeert,ratificeren,geratificeerd,ratificeerde,ratificeerden,
    [h([transitive,
	intransitive])]).

v(rationaliseer,rationaliseert,rationaliseren,gerationaliseerd,rationaliseerde,rationaliseerden,
    [h([transitive])]).

v(ravot,ravot,ravotten,geravot,ravotte,ravotten,
  [h([intransitive,
      mod_pp(in)
     ])]).

%% often combines with nonadv adjective?
v(reageer,reageert,reageren,gereageerd,reageerde,reageerden,
    [h([intransitive,
	sbar,   % mostly dip
	part_transitive(af),
	pc_pp(met),
	pc_pp(op),
	pc_pp(tegen),
	fixed([als_pred],no_passive), % als door een wesp gestoken
	part_np_pc_pp(af,op),
	part_pc_pp(af,op)   % ik heb iemand nodig om op af te reageren
	% part_refl_pc_pp(af,op)
       ])]).

v(realiseer,realiseert,realiseren,gerealiseerd,realiseerde,realiseerden,
    [h([refl_sbar,
	refl_np,
	transitive])]).

v(reanimeer,reanimeert,reanimeren,gereanimeerd,reanimeerde,reanimeerden,
    [h([transitive,
        intransitive])]).

v(rebelleer,rebelleert,rebelleren,gerebelleerd,rebelleerde,rebelleerden,
    [h([intransitive,
	pc_pp(tegen)])]).

v(recenseer,recenseert,recenseren,gerecenseerd,recenseerde,recenseerden,
    [h([intransitive,
	transitive])]).

v(rechercheer,rechercheert,rechercheren,gerechercheerd,rechercheerde,
  rechercheerden,
    [h([intransitive,
	transitive])]).

v(recht,recht,rechten,gerecht,rechtte,rechtten,
    [h([transitive])]).

v(rechtig,rechtigt,rechtigen,gerechtigd,rechtigde,rechtigden,
    [h([np_np,
	np_vp_obj,
	transitive,
	vp])]).

v(rechtvaardig,rechtvaardigt,rechtvaardigen,gerechtvaardigd,rechtvaardigde,rechtvaardigden,
    [h([sbar_subj_so_np,
	sbar,
	transitive])]).

v(reciteer,reciteert,reciteren,gereciteerd,reciteerde,reciteerden,
  [h([intransitive,
      dip_sbar,
      acc_np_dip_sbar,
      transitive])]).

v(reclameer,reclameert,reclameren,gereclameerd,reclameerde,reclameerden,
    [h([sbar,
	intransitive])]).

v(reconstrueer,reconstrueert,reconstrueren,gereconstrueerd,reconstrueerde,reconstrueerden,
    [h([sbar,
	transitive,
	intransitive])]).

v(recreëer,recreëert,recreëren,gerecreëerd,recreëerde,recreëerden,
    [h([intransitive])]).

v(recruteer,recruteert,recruteren,gerecruteerd,recruteerde,recruteerden,
    [h([transitive,
        intransitive])]).

v(rectificeer,rectificeert,rectificeren,gerectificeerd,rectificeerde,rectificeerden,
    [h([intransitive,
	transitive])]).

v(recupereer,recupereert,recupereren,gerecupereerd,recupereerde,recupereerden,
    [h([intransitive,
	transitive])]).  

v(recycle,recyclet,recyclen,gerecycled,recyclede,recycleden,
    [h([intransitive,
	transitive])]).

v(red,redt,redden,gered,redde,redden,
    [h([np_np,
	intransitive,
	refl,
        np_pc_pp(van), % iemand redden van de ondergang
	transitive,
	np_ld_pp])]).

v(redder,reddert,redderen,geredderd,redderde,redderden,
    [h([transitive])]).

v(redekavel,redekavelt,redekavelen,geredekaveld,redekavelde,redekavelden,
    [h([intransitive])]).

v(redeneer,redeneert,redeneren,geredeneerd,redeneerde,redeneerden,
    [h([intransitive,
	part_transitive(weg),
	sbar])]).

v(redetwist,redetwist,redetwisten,geredetwist,redetwistte,redetwistten,
    [h([intransitive])]).

v(redigeer,redigeert,redigeren,geredigeerd,redigeerde,redigeerden,
    [h([transitive])]).

v(redirect,redirect,redirecten,geredirect,redirecte,redirecten,
    [h([intransitive,
	transitive,
	pc_pp(naar),
	np_pc_pp(naar)])]).

v(redoubleer,redoubleert,redoubleren,geredoubleerd,redoubleerde,redoubleerden,
    [h([intransitive,
	transitive])]).

v(reduceer,reduceert,reduceren,gereduceerd,reduceerde,reduceerden,
    [h([sbar_subj_so_np,
	transitive])]).

v(refereer,refereert,refereren,gerefereerd,refereerde,refereerden,
    [h([pc_pp(aan),
	pc_pp(naar),
	pc_pp(over)
	% refl_pc_pp(aan)
       ])]).

v(reflecteer,reflecteert,reflecteren,gereflecteerd,reflecteerde,reflecteerden,
  [h([intransitive,
      dip_sbar,
      transitive,
      pc_pp(op)])]).

v(reformeer,reformeert,reformeren,gereformeerd,reformeerde,reformeerden,
    [h([transitive])]).

v(regeer,regeert,regeren,geregeerd,regeerde,regeerden,
    [h([intransitive,
	transitive,
        part_intransitive(mee),
	pc_pp(met),
	pc_pp(over)])]).

v(regel,regelt,regelen,geregeld,regelde,regelden,
    [h([transitive,
	part_transitive(in),
	fixed([[in,der,minne],acc],norm_passive),
	sbar])]).

v(regen,regent,regenen,geregend,regende,regenden,
    [h([het_subj,
	het_subj_sbar_obcomp,
        part_intransitive(binnen),
	part_intransitive(in),
	part_transitive(in), % het inregenen van mest
	part_transitive(weg),
	ld_pp,     % granaatscherven regenden op de daken
	part_ld_pp(neer),  % brokken regenden neer op de dorpjes
	np_ld_pp,  % In Brabant zijn we uit onze tentjes geregend .
	np_ld_dir,    % we zijn de tent uit geregend
	ap_copula(nat),  % de broek is nat/*doorweekt geregend
	fixed([het_subj,acc],no_passive)])]). %  het regende kansen

v(regisseer,regisseert,regisseren,geregisseerd,regisseerde,regisseerden,
    [h([intransitive,
	transitive])]).

v(registreer,registreert,registreren,geregistreerd,registreerde,registreerden,
    [h([intransitive,
	sbar,
        als_pred_np,
	transitive,
	np_ld_pp])]).

v(reguleer,reguleert,reguleren,gereguleerd,reguleerde,reguleerden,
    [h([transitive,
        intransitive])]).

v(rehabiliteer,rehabiliteert,rehabiliteren,gerehabiliteerd,rehabiliteerde,rehabiliteerden,
    [h([transitive])]).

v(reik,reikt,reiken,gereikt,reikte,reikten,
    [h([intransitive,
	so_pp_np,
	np_np,
	part_np_np(aan),
	part_np_np(over),
	part_np_np(toe),
	part_np_np(uit),
	part_intransitive(toe),
	part_so_pp_np(uit),
	part_transitive(aan),
	part_transitive(over),
	part_transitive(toe),
	part_transitive(uit),
	ld_pp,
	ld_dir,
	ld_adv,
	np_ld_pp,
	np_ld_adv])]).

v(reikhals,reikhalst,reikhalzen,gereikhalsd,reikhalsde,reikhalsden,
    [h([ld_pp])]).

v(reil,reilt,reilen,gereild,reilde,reilden,
    [h([het_subj,
        intransitive])]).

v(reinig,reinigt,reinigen,gereinigd,reinigde,reinigden,
    [h([transitive,
	intransitive,
	np_pc_pp(van)])]).

v(reïntegreer,reïntegreert,reïntegreren,gereïntegreerd,reïntegreerde,
  reïntegreerden,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv])]).

v(reis,reist,reizen,gereisd,reisde,reisden,
    [z([ld_dir,
	ld_pp,
	part_intransitive(door),
	part_ld_pp(door),
	part_intransitive(af),
	part_ld_pp(af),
	part_intransitive(aan),
	part_intransitive(in),
	part_intransitive(mee),
	part_pc_pp(mee,met),
	part_transitive(na),    % Dilan Yeşilgöz
	part_intransitive(om),
	part_intransitive(rond),
	part_intransitive(terug),
	part_intransitive(uit),
	part_ld_pp(terug)]),
     h([intransitive]),
     b([part_ld_pp(rond),
	part_transitive(af),
	part_ld_pp(rond)])]).

v(rek,rekt,rekken,[gerekt,gerokken],rekte,rekten,  % gerokken: VL
    [unacc([part_intransitive(uit)]),
     h([transitive,
	%%% part_refl(uit),
	part_intransitive(tijd),
	part_transitive(op),
	part_transitive(uit)]),
     b([intransitive])]).

v(reken,rekent,rekenen,gerekend,rekende,rekenden,
    [h([ap_pred_np,
	intransitive,
	sbar,
        van_sbar,   % reken maar van yes
	transitive,
	er_pp_sbar(op),
	er_pp_vp(op),
	np_ld_pp,  % tot/onder/bij
	np_pc_pp(voor),
        pc_pp(aan),
	part_np_np(aan),
	part_fixed(aan,[dat,het_pobj1(sbar)],imp_passive),
	part_fixed(aan,[dat,sbar],imp_passive),
	part_transitive(aan),
	part_np_np(aan),
	part_intransitive(af),
	part_transitive(af),
	part_np_pc_pp(af,met),
	part_pc_pp(af,met),
	part_np_pc_pp(af,op),
	part_transitive(door),
	part_transitive(in),
	part_transitive(mee),
        part_transitive(na),
        part_intransitive(na),
	part_intransitive(om),
	part_transitive(om),
	part_pc_pp(om,in),
	part_pc_pp(om,naar),
	part_np_pc_pp(om,in),
	part_np_pc_pp(om,naar),
	part_np_np(toe),
	part_pred_np(toe),
	part_transitive(toe),
	part_sbar(uit),
        part_intransitive(uit),
	part_transitive(uit),
	part_sbar(voor),
	part_np_sbar(voor),
	part_transitive(voor),
	part_np_np(voor),
	pc_pp(in),
	pc_pp(met),
	pc_pp(op),
	np_pc_pp(tot),
	part_np_pc_pp(toe,aan),
	fixed([pc(tot),het_pobj1(vp_no_control)],imp_passive)])]).

v(rekruteer,rekruteert,rekruteren,gerekruteerd,rekruteerde,rekruteerden,
    [h([transitive,
	intransitive])]).

v(rel,relt,rellen,gereld,relde,relden,
    [h([intransitive])]).

v(relateer,relateert,relateren,gerelateerd,relateerde,relateerden,
    [h([sbar,
	transitive,
	np_pc_pp(aan)])]).

v(relativeer,relativeert,relativeren,gerelativeerd,relativeerde,relativeerden,
    [h([intransitive,
	sbar_subj_so_np,
	sbar, % dip
	transitive])]).

v(relax,relaxt,relaxen,gerelaxt,relaxte,relaxten,
    [h([intransitive])]).

v(releveer,releveert,releveren,gereleveerd,releveerde,releveerden,
    [h([intransitive,
	sbar,
	transitive])]).

v(rem,remt,remmen,geremd,remde,remden,
    [h([intransitive,
	transitive,
	part_intransitive(af),
	part_transitive(af)])]).

v(remigreer,remigreert,remigreren,geremigreerd,remigreerde,remigreerden,
  [h([transitive,
      np_ld_pp]),
   z([intransitive,
      ld_pp])]).

v(remix,remixt,remixen,geremixt,remixte,remixten,
    [h([intransitive,
	transitive])]).

v(ren,rent,rennen,gerend,rende,renden,
    [z([ld_dir,
	ld_pp,
	part_intransitive(aan),
	part_intransitive(weg),
        part_mod_pp(weg,met)
       ]),
     b([intransitive,
        part_intransitive(rond),
	ap_pred_refl,
	pc_pp(om)])]).

v(rendeer,rendeert,renderen,gerendeerd,rendeerde,rendeerden,
    [h([intransitive])]).

v(renoveer,renoveert,renoveren,gerenoveerd,renoveerde,renoveerden,
    [h([intransitive,
	transitive])]).

v(rentenier,renteniert,rentenieren,gerentenierd,rentenierde,rentenierden,
    [h([intransitive])]).

v(reorganiseer,reorganiseert,reorganiseren,gereorganiseerd,reorganiseerde,reorganiseerden,
    [h([intransitive,
	transitive])]).

v(rep,rept,reppen,gerept,repte,repten,
    [h([refl,
	pc_pp(over),
	pc_pp(van),
	refl_ld_pp])]).

v(repareer,repareert,repareren,gerepareerd,repareerde,repareerden,
    [h([intransitive,
	transitive])]).

v(repatrieer,repatrieert,repatriëren,gerepatrieerd,repatrieerde,repatrieerden,
  [h([intransitive,
      transitive])]).

v(repeteer,repeteert,repeteren,gerepeteerd,repeteerde,repeteerden,
    [h([intransitive,
	sbar,
	pc_pp(op), % we hebben er weken op gerepeteerd
	transitive])]).

v(repliceer,repliceert,repliceren,gerepliceerd,repliceerde,repliceerden,
    [h([sbar,
	transitive,
	vp])]).

v(representeer,representeert,representeren,gerepresenteerd,representeerde,representeerden,
    [h([transitive])]).

v(reproduceer,reproduceert,reproduceren,gereproduceerd,reproduceerde,reproduceerden,
    [h([refl,
	transitive])]).

v(reserveer,reserveert,reserveren,gereserveerd,reserveerde,reserveerden,
    [h([intransitive,
	% refl_np,
	transitive])]).

v(reset,reset,resetten,gereset,resette,resetten,
    [h([transitive,
	intransitive])]).

v(resoneer,resoneert,resoneren,geresoneerd,resoneerde,resoneerden,
    [h([intransitive,
	pc_pp(met),
	pc_pp(op)])]).

v(respecteer,respecteert,respecteren,gerespecteerd,respecteerde,respecteerden,
  [h([transitive,
      sbar_obj,
      sbar])]).

v(ressorteer,ressorteert,ressorteren,geressorteerd,ressorteerde,ressorteerden,
    [h([intransitive,
	pc_pp(onder)])]).

v(rest,rest,resten,gerest,restte,restten,
    [h([so_np,
        fixed([vp_subj_no_het,dat],no_passive), % rest mij u te feliciteren
        pc_pp(van),  % het enige dat er nog van rest, is ..
	intransitive])]).

v(restaureer,restaureert,restaureren,gerestaureerd,restaureerde,restaureerden,
    [h([transitive,
        intransitive])]).

v(resteer,resteert,resteren,geresteerd,resteerde,resteerden,
    [unacc([intransitive,
	    so_np])]).

v(restitueer,restitueert,restitueren,gerestitueerd,restitueerde,restitueerden,
    [h([transitive])]).

v(restyl,restylt,restylen,gerestyld,restylde,retylden,
    [h([transitive,
        intransitive])]).

v(resulteer,resulteert,resulteren,geresulteerd,resulteerde,resulteerden,
    [h([intransitive,
	pc_pp(in),
	er_pp_sbar(in),
	pc_pp(uit)])]).

v(resumeer,resumeert,resumeren,geresumeerd,resumeerde,resumeerden,
    [h([transitive])]).

% paardensport: terugdeinzen/gaan/stappen (voor een hindernis)
v(retireer,retireert,retireren,geretireerd,retireerde,retireerden,
    [h([intransitive])]).

v(retourneer,retourneert,retourneren,geretourneerd,retourneerde,retourneerden,
    [h([transitive,
	intransitive % de speler retourneerde best goed
       ])]).

v(retweet,retweet,retweeten,geretweet,retweette,retweetten,
    [h([intransitive,
	sbar,
	np_sbar,
	transitive])]).

v(reutel,reutelt,reutelen,gereuteld,reutelde,reutelden,
    [h([intransitive,
        transitive])]).

v(revalideer,revalideert,revalideren,gerevalideerd,revalideerde,revalideerden,
    [b([intransitive])]).

v(revancheer,revancheert,revancheren,gerevancheerd,revancheerde,revancheerden,
    [h([refl,
	transitive % de nederlaag
       ])]).

v(revatiliseer,revatiliseert,revatiliseren,gerevatiliseerd,revatiliseerde,
  revatiliseerden,
    [h([transitive])]).

v(reviseer,reviseert,reviseren,gereviseerd,reviseerde,reviseerden,
    [h([transitive,
	intransitive])]).

v(revitaliseer,revitaliseert,revitaliseren,gerevitaliseerd,revitaliseerde,
  revitaliseerden,
    [h([transitive,
	intransitive])]).

v(revolteer,revolteert,revolteren,gerevolteerd,revolteerde,revolteerden,
    [h([intransitive])]).

v(richt,richt,richten,gericht,richtte,richtten,
    [h([intransitive,
	np_ld_dir,
	refl,
	transitive,
	np_pc_pp(aan),
	np_pc_pp(naar),
	np_pc_pp(op),
	np_er_pp_vp(op),     % de werkwijze die erop is gericht om ...
	part_np_mod_pp(op,voor),  % een monument oprichten voor ...
	np_er_pp_sbar(op),
	np_pc_pp(tegen),
	np_pc_pp(tot),
	part_refl(op),
	part_refl(tegen),
 	part_sbar_subj_so_np(uit),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(in),
	part_np_pc_pp(in,op),
	part_np_er_pp_vp(in,op),
	part_np_er_pp_sbar(in,op),
	part_transitive(op),
	part_transitive(uit),
	part_np_mod_pp(uit,met),
	pc_pp(naar),
	pc_pp(op),
	refl_pc_pp(naar),
	refl_pc_pp(op),
	refl_pc_pp(tegen),
        refl_er_pp_vp(op),
        refl_er_pp_sbar(op),
	refl_pc_pp(tot),
	fixed([{[[het,woord],pc(tot)]}],imp_passive),
	fixed([[te,gronde],acc],norm_passive),
	fixed([[ten,gronde],acc],norm_passive),
	part_np_ld_pp(af),
	part_np_pc_pp(aan,bij),
	part_np_pc_pp(aan,onder),
	part_np_pc_pp(uit,tegen)]),
     b([part_als_pred_np(in)])]).

v(ridder,riddert,ridderen,geridderd,ridderde,ridderden,
    [h([transitive,
        np_pc_pp(tot)])]).

v(ridiculiseer,ridiculiseert,ridiculiseren,geridiculiseerd,
  ridiculiseerde,ridiculiseerden,
    [h([intransitive,
	transitive])]).

v([rijd,rij],rijdt,rijden,gereden,reed,reden,
    [z([ld_dir,
	part_intransitive(door),
	part_intransitive(aan),
	part_intransitive(achteruit),
        part_intransitive(binnen),
	part_intransitive(langs),
	part_intransitive(proef),
	part_intransitive(terug),
	part_intransitive(uit),
	part_intransitive(verder),  % HACK VL: ik ben op mijn elan blijven verder rijden
	part_intransitive(voor),
	part_intransitive(weg),
	part_ld_pp(aan),
	part_ld_pp(af),
	part_ld_pp(door),
	part_ld_pp(in),
	part_ld_pp(op),
	part_ld_pp(rond),
	part_ld_pp(terug),
	part_ld_pp(uit),
        part_intransitive(binnen),
	part_so_np(tegemoet),
	part_mod_pp(weg,met),
	part_mod_pp(weg,per),
	part_pc_pp(aan,tegen),
	part_pc_pp(in,op),
	part_pc_pp(mee,met)]),  % NB dat ik met hem mee naar huis rijd
     h([ap_pred_np,
	intransitive,
	np_ld_dir,
	transitive,
	np_ld_pp,
	part_np_np(af),
	part_intransitive(af),
	part_intransitive(auto),
	part_intransitive(in),   % je moet eerst goed inrijden
	part_intransitive(paard),
	fixed([[klem],acc],norm_passive),
	part_ld_pp(rond),
	part_intransitive(voorbij),
	part_transitive(aan),
        part_transitive(binnen),
	part_transitive(dood),
	part_transitive(klem),
	part_transitive(omver),
	part_transitive(terug),
	part_transitive(uit),
	part_transitive(voorbij),
	part_transitive(weg),
	pc_pp(op)]),   % op benzine
     b([ld_pp,
	part_intransitive(lek), % VL
	part_intransitive(mee), % NB dat ik mee naar huis rijd
	part_intransitive(om),
	part_intransitive(rond),
	part_transitive(af),
	part_transitive(door),
	part_transitive(in),
	part_transitive(rond),
	part_transitive(uit)])]).

%% VL: harken
v(rijf,rijft,rijven,[gereven,gerijfd],[reef,rijfde],[rijfden,reven],
    [h([part_transitive(bijeen),
	part_transitive(binnen)
       ])]).

v(rijg,rijgt,rijgen,geregen,reeg,regen,
    [h([intransitive,
	transitive,
	part_transitive(aaneen),
	part_refl(aaneen),  % de dagen
	% part_refl_np(aaneen),
	np_pc_pp(aan)])]).

v(rijm,rijmt,rijmen,gerijmd,rijmde,rijmden,
    [h([intransitive,
	transitive,  % hoe valt dit te rijmen?
	sbar,        % dat hij komt is niet te rijmen ...
	np_pc_pp(met),
	pc_pp(met),
	pc_pp(op)])]).

v(rijp,rijpt,rijpen,gerijpt,rijpte,rijpten,
    [unacc([pc_pp(tot)]),
     h([transitive]),
     b([intransitive])]).

v(rijs,rijst,rijzen,gerezen,rees,rezen,
    [unacc([intransitive,
	ld_pp,
	ld_dir,  % de kosten rijzen de pan uit
	fixed([subj(haar),[te,berge]],no_passive),  % subject: haren
	fixed([subj(haar),[te,berge],dat],no_passive),
	fixed([subj(haar),[ten,berge]],no_passive),  % subject: haren
	fixed([subj(haar),[ten,berge],dat],no_passive),
        fixed([subj(bezwaar),pc(tegen)],no_passive),
	part_intransitive(op),
	part_ld_pp(op)])]).

v(rijt,rijt,rijten,gereten,reet,reten,
    [h([np_np,
	np_pc_pp(aan),
        part_transitive(uiteen),
	np_ld_pp,  % uit elkaar rijten
	part_sbar_subj_so_np(open),
	part_transitive(open)])]).

v(ril,rilt,rillen,gerild,rilde,rilden,
  [h([intransitive,
      pc_pp(van),
      transitive])]).

v(rimpel,rimpelt,rimpelen,gerimpeld,rimpelde,rimpelden,
    [unacc([intransitive]),
     h([transitive])]).

v(ring,ringt,ringen,geringd,ringde,ringden,  % van vogels
    [h([intransitive,
	transitive])]).

v(ringeloor,ringeloort,ringeloren,geringeloord,ringeloorde,ringeloorden,
    [h([transitive])]).

v(rinkel,rinkelt,rinkelen,gerinkeld,rinkelde,rinkelden,
    [h([intransitive,
	pc_pp(met)])]).

v(rip,ript,rippen,geript,ripte,ripten,
    [h([transitive,
        np_pc_pp(van)])]).

v(riposteer,riposteert,riposteren,geriposteerd,riposteerde,riposteerden,
    [h([intransitive,
	sbar])]).

v(ris,rist,rissen,gerist,riste,risten,
    [h([transitive,
        np_pc_pp(van)])]).

v(riskeer,riskeert,riskeren,geriskeerd,riskeerde,riskeerden,
    [h([transitive,
	sbar_obj,
	sbar,
	vp_obj,
	vp])]).

v(risqueer,risqueert,risqueren,gerisqueerd,risqueerde,risqueerden,
    [h([transitive,
	vp])]).

v(rits,ritst,ritsen,geritst,ritste,ritsten,
    [h([intransitive,
        part_transitive(aan),
        part_transitive(af),
        part_transitive(los),
	transitive
       ])]).

v(ritsel,ritselt,ritselen,geritseld,ritselde,ritselden,
    [h([intransitive,
	transitive,
	pc_pp(met)])]).

v(rochel,rochelt,rochelen,gerocheld,rochelde,rochelden,
    [h([intransitive])]).

v(rock,rockt,rocken,gerockt,rockte,rockten,
    [h([intransitive])]).

v(roddel,roddelt,roddelen,geroddeld,roddelde,roddelden,
    [h([intransitive,
	pc_pp(over)])]).

v(rodeer,rodeert,roderen,gerodeerd,rodeerde,rodeerden,
    [h([intransitive])]).

v(rodel,rodelt,rodelen,gerodeld,rodelde,rodelden,
    [h([intransitive])]).

v(roei,roeit,roeien,geroeid,roeide,roeiden,
    [z([ld_dir,
	ld_pp,
	part_intransitive(aan),
	part_intransitive(uit),
	part_ld_pp(uit)]),
     h([intransitive,
	transitive,
	np_ld_pp,
	part_transitive(uit)])]).

v(roem,roemt,roemen,geroemd,roemde,roemden,
    [h([intransitive,
	transitive,
	als_pred_np,
	pc_pp(op),
	pc_pp(over)])]).

v(roep,roept,roepen,geroepen,riep,riepen,
    [h([intransitive,
	sbar,
	transitive,
	vp,
	np_ld_dir,
	np_ld_pp,
	part_als_pred_np(aan),
	part_als_pred_np(op),
	part_als_pred_np(uit),
	part_np_np(na),
	part_np_np(toe),
	part_np_sbar(toe),
	part_np_vp_obj1(op),
	part_vp_no_control(op),
	part_np_vp_obj(toe),
	part_sbar_subj_so_np(op),
	part_sbar(in), % VL De raadsman riep ter verdediging in dat ...
	part_sbar(om),
	part_sbar(terug),
	part_sbar(uit),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(bijeen),
	part_transitive(binnen),
	part_transitive(in),
	part_transitive(na),
	part_np_sbar(na),
	part_intransitive(om),  % de NS belooft vaker om te roepen bij vertragingen
	part_transitive(om),
	part_transitive(op),
	part_transitive(terug),
	part_transitive(uit),
	part_vp(uit),
	pc_pp(om),
	fixed([svp_pp(in,herinnering),acc],norm_passive),
	fixed([svp_pp(in,herinnering),sbar],norm_passive),
	fixed([svp_pp(in,herinnering),{[acc,dat]}],norm_passive),
	fixed([svp_pp(in,herinnering),dat,sbar],norm_passive),
        fixed([svp_pp(tot,orde),acc],norm_passive),
	fixed([[in,leven],acc],norm_passive),
	fixed([[in,het,leven],acc],norm_passive),
	fixed([[ach,en,wee]],imp_passive),
	fixed([{[acc(hoera),pc(tegen)]}],imp_passive),
	fixed([[te,hulp],acc],norm_passive),
	part_fixed(in,[{[acc(hulp),pc(van)]}],norm_passive),
	part_fixed(op,[{[acc(associatie),pc(met)]}],norm_passive),
	part_fixed(toe,[{[acc(halt),pc(aan)]}],norm_passive),
	part_np_ld_pp(terug),
	part_np_pc_pp(af,over),
	part_np_pc_pp(op,tot),
	part_np_pc_pp(uit,tot),
	part_pc_pp(op,tot)])]).

v(roer,roert,roeren,geroerd,roerde,roerden,
    [h([intransitive,
	refl,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	part_transitive(door),
	part_transitive(los),
	part_transitive(om),
	part_transitive(aan)])]).

v(roerbak,roerbakt,roerbakken,geroerbakt,roerbakte,roerbakten,
    [h([intransitive,
	transitive])]).

v(roest,roest,roesten,geroest,roestte,roestten,
    [unacc([part_intransitive(vast),
	    part_ld_pp(vast),
            fixed([svp_pp(aan,reet)],no_passive),
            fixed([svp_pp(aan,reet),dat],no_passive),
            fixed([acc(reet),dat],no_passive),  % het zal me m'n reet roesten
            part_intransitive(door)]),
     b([intransitive])]).


v(roezemoes,roezemoest,roezemoezen,geroezemoesd,roezemoesde,roezemoesden,
    [h([intransitive,
	transitive,
        sbar
       ])
    ]).

v(roffel,roffelt,roffelen,geroffeld,roffelde,roffelden,
    [h([intransitive,
	transitive])]).

v(rol,rolt,rollen,gerold,rolde,rolden,
    [z([ld_dir,
	ld_pp,
	part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(om),
	part_intransitive(op),
	part_intransitive(weg)]),
     h([np_ld_dir,
	transitive,
	np_ld_pp,
	part_refl(op),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(in),
	part_transitive(om),
	part_transitive(op),
	part_transitive(uit),
        part_transitive(voort)]),
     b([ap_pred_np,
	intransitive])]).

v(rolschaats,rolschaatst,rolschaatsen,gerolschaatst,rolschaatste,rolschaatsten,
    [b([ld_pp,
	part_intransitive(aan)]),
     h([intransitive,
	ld_dir,
	transitive
       ])]).

v(romantiseer,romantiseert,romantiseren,geromantiseerd,
  romantiseerde,romantiseerden,
    [h([transitive,
        intransitive])]).

v(rommel,rommelt,rommelen,gerommeld,rommelde,rommelden,
    [h([intransitive,
	part_intransitive(aan),
	part_intransitive(af),
	ld_pp,
	ld_adv,
	pc_pp(met)])]).

v(rond,rondt,ronden,gerond,rondde,rondden,
    [h([intransitive,
	transitive,
	part_intransitive(af),
	part_transitive(af),
	part_pc_pp(af,met),  % Bakellende: ik rond af met u even mee terug te nemen ...
	part_np_pc_pp(af,naar),
	part_np_pc_pp(af,op)])]).

v(ronk,ronkt,ronken,geronkt,ronkte,ronkten,
    [h([intransitive,
	sbar])]).

v(ronsel,ronselt,ronselen,geronseld,ronselde,ronselden,
    [h([intransitive,
	transitive])]).

v(roof,rooft,roven,geroofd,roofde,roofden,
    [h([intransitive,
        part_transitive(leeg),
	transitive])]).

v(rooi,rooit,rooien,gerooid,rooide,rooiden,
  [h([transitive,
      intransitive])]).

v(rook,rookt,roken,gerookt,rookte,rookten,
    [h([intransitive,
	transitive,
        part_intransitive(mee),
	part_transitive(op),
	part_transitive(uit),
        part_intransitive(af),
	np_ld_dir])]).

v(room,roomt,romen,geroomd,roomde,roomden,
    [h([part_transitive(af)])]).

v(rooster,roostert,roosteren,geroosterd,roosterde,roosterden,
    [h([intransitive,
        ap_pred_np,  % pijnboompitten goudbruin
	part_transitive(in),
	transitive])]).

v(ros,rost,rossen,gerost,roste,rosten,
    [b([transitive,
	intransitive,
	ld_dir,
	ld_pp,
	np_ld_dir,
	np_ld_pp,
	part_transitive(af)])]).

v(rot,rot,rotten,gerot,rotte,rotten,
    [unacc([part_intransitive(door),
            part_intransitive(op),
            part_intransitive(weg)
       ]),
     h([intransitive,
	transitive])]).

v(roteer,roteert,roteren,geroteerd,roteerde,roteerden,
    [h([intransitive,
	transitive])]).

v(rotzooi,rotzooit,rotzooien,gerotzooid,rotzooide,rotzooiden,
    [h([intransitive])]).

v(rouleer,rouleert,rouleren,gerouleerd,rouleerde,rouleerden,
    [h([intransitive])]).

v(rouw,rouwt,rouwen,gerouwd,rouwde,rouwden,
    [h([intransitive,
	pc_pp(om),
	pc_pp(over)])]).

v(royeer,royeert,royeren,geroyeerd,royeerde,royeerden,
    [h([transitive])]).

v(rubriceer,rubriceert,rubriceren,gerubriceerd,rubriceerde,rubriceerden,
    [h([transitive,
	intransitive])]).

v(rugby,rugbyt,rugbyen,[gerugbyd,gerugbied],[rugbyde,rugbiede],[rugbyden,rugbieden],
    [h([intransitive])]).


v(rui,ruit,ruien,geruid,ruide,ruiden,
    [h([part_transitive(op)])]).
  

v([ruik,riek],[ruikt,riekt],[ruiken,rieken],geroken,[rook,riekte],[roken,riekten],
    [h([intransitive,  % hij kan niet meer ruiken
	nonp_copula,
	sbar,
	transitive,
	pc_pp(uit),
	pc_pp(aan),
	pc_pp(naar)])]).

v(ruil,ruilt,ruilen,geruild,ruilde,ruilden,
    [h([intransitive,
	transitive,
	np_pc_pp(tegen),
	np_pc_pp(voor),
	part_transitive(af),
        part_transitive(om),
        part_np_pc_pp(om,tegen),
        part_np_pc_pp(om,voor),
	part_transitive(in),
        part_np_pc_pp(in,tegen),
        part_np_pc_pp(in,voor),
	part_transitive(uit),
        part_np_pc_pp(uit,tegen),
        part_np_pc_pp(uit,voor),
	pc_pp(met)])]).

v(ruim,ruimt,ruimen,geruimd,ruimde,ruimden,
    [h([intransitive,
	transitive,
	part_intransitive(af),
	part_transitive(af),
	part_transitive(in),
        part_intransitive(puin),
	part_transitive(uit),
	part_intransitive(op),  % dat ruimt op; C&A ruimt op!
	part_transitive(op),
	fixed([[het,veld]],imp_passive),
	fixed([[uit,de,weg],acc],norm_passive),
	part_np_pc_pp(op,tegen)])]).

v(ruis,ruist,ruisen,geruist,ruiste,ruisten,
    [h([intransitive])]).

v(ruit,ruit,ruiten,geruit,ruitte,ruitten,
    [h([transitive])]).

v(ruk,rukt,rukken,gerukt,rukte,rukten,
    [z([part_intransitive(aan),
	part_intransitive(op),
	part_ld_pp(op),
	ld_dir,  % we zijn de stad binnen gerukt
	part_pc_pp(los,op),
	part_pc_pp(uit,naar),
	part_pc_pp(uit,voor)]),
     h([np_np,
	intransitive,
	transitive,
	np_ld_pp,
	np_ld_dir,
	part_np_np(af),
	part_np_np(uit),
	part_intransitive(af),
	part_intransitive(los),
	part_intransitive(weg),
        part_transitive(aan), % we rukken versterking aan (tja)
	part_transitive(af),
	part_transitive(los),
	part_transitive(uit),
	part_transitive(weg),
	pc_pp(aan),
	part_np_ld_pp(af),
	part_np_ld_pp(los),
	part_np_ld_pp(weg)]),
     b([part_intransitive(in),
	part_intransitive(uit)])]).

v(run,runt,runnen,gerund,runde,runden,
    [h([transitive])]).

v(rust,rust,rusten,gerust,rustte,rustten,
    [h([intransitive,
	ld_pp,
	ld_adv,
	%% ban, taboe, vloek
	pc_pp(op),
	%% fixed([subj(taboe),pc(op)],no_passive),
	part_intransitive(uit),
	part_transitive(toe),
	part_transitive(uit),
	part_np_pc_pp(toe,met),
	part_np_pc_pp(toe,op),
	part_np_er_pp_vp(toe,op),
	part_np_pc_pp(toe,voor),
	part_np_er_pp_vp(toe,voor),
	part_np_pc_pp(uit,met),
	part_pc_pp(uit,van)])]).

v(ruzie,ruziet,[ruzieën,ruziën],geruzied,ruziede,ruzieden,
    [h([intransitive,
	mod_pp(met),
	pc_pp(over)])]).

v(ruïneer,ruïneert,ruïneren,geruïneerd,ruïneerde,ruïneerden,
    [h([transitive])]).

v(sabbel,sabbelt,sabbelen,gesabbeld,sabbelde,sabbelden,
    [h([intransitive,
	pc_pp(aan),
	pc_pp(op)])]).

v(sabel,sabelt,sabelen,gesabeld,sabelde,sabelden,
    [z([intransitive]),
     h([part_transitive(neer)])]).

v(saboteer,saboteert,saboteren,gesaboteerd,saboteerde,saboteerden,
    [h([intransitive,
	transitive])]).

v(sakker,sakkert,sakkeren,gesakkerd,sakkerde,sakkerden,
    [h([intransitive,
	sbar,
        part_intransitive(na),
        part_sbar(na),
        part_intransitive(door),
        part_sbar(door)])]).

v(salueer,salueert,salueren,gesalueerd,salueerde,salueerden,
    [h([intransitive])]).

v(sampel,sampelt,sampelen,gesampeld,sampelde,sampelden,
    [h([intransitive,
	transitive])]).

v(sanctioneer,sanctioneert,sanctioneren,gesanctioneerd,sanctioneerde,sanctioneerden,
    [h([sbar_subj_so_np,
	transitive])]).

v(saneer,saneert,saneren,gesaneerd,saneerde,saneerden,
    [h([transitive,
        intransitive])]).

v(sar,sart,sarren,gesard,sarde,sarden,
    [h([intransitive,
	transitive])]).

v(savoureer,savoureert,savoureren,gesavoureerd,savoureerde,savoureerden,
    [h([intransitive,
	transitive])]).

v(scan,scant,scannen,gescand,scande,scanden,
    [h([transitive,
	intransitive,
	pc_pp(op),
	np_pc_pp(op),
	part_transitive(af),
        part_transitive(in)])]).

v(scandeer,scandeert,scanderen,gescandeerd,scandeerde,scandeerden,
  [h([transitive,
      intransitive,
      sbar])]).

v(schaad,schaadt,schaden,geschaad,schaadde,schaadden,
    [h([intransitive,
	transitive])]).

v(schaaf,schaaft,schaven,geschaafd,schaafde,schaafden,
    [h([intransitive,
	transitive,
        part_transitive(af),
	part_transitive(bij),
        np_ld_pp,
	pc_pp(aan)])]).

v(schaak,schaakt,schaken,geschaakt,schaakte,schaakten,
    [h([intransitive,
	transitive])]).

v(schaal,schaalt,schalen,geschaald,schaalde,schaalden,
    [h([transitive,
        part_transitive(in),
	part_intransitive(op),
        part_transitive(op),
	part_pc_pp(in,in)])]).

v(schaam,schaamt,schamen,geschaamd,schaamde,schaamden,
    [h([refl,
	refl_sbar,
	refl_vp,
	refl_pc_pp(voor),
	refl_er_pp_sbar(voor),
	refl_er_pp_vp(voor),
	refl_pc_pp(over),
	refl_er_pp_sbar(over),
	refl_er_pp_vp(over),
	fixed([[rot],refl],no_passive)])]).

v(schaar,schaart,scharen,geschaard,schaarde,schaarden,
  [b([intransitive]),		% de vrachtwagen was geschaard
   h([refl_ld_pp,		% onder hen schaarde zich de sopraan X
      np_ld_pp])]).

v(schaats,schaatst,schaatsen,geschaatst,schaatste,schaatsten,
    [b([ld_pp,
	part_intransitive(aan)]),
     h([intransitive,
	ld_dir,
	transitive
       ])]).

v(schaduw,schaduwt,schaduwen,geschaduwd,schaduwde,schaduwden,
    [h([transitive])]).

v(schaf,schaft,schaffen,geschaft,schafte,schaften,
    [h([transitive,
	part_np_np(aan),
	part_transitive(aan),
	part_transitive(af)])]).

v(schaft,schaft,schaften,geschaft,schaftte,schaftten,
    [h([intransitive,
	transitive])]).

v(schakel,schakelt,schakelen,geschakeld,schakelde,schakelden,
    [h([intransitive,
	transitive,
	np_ld_pp,
	part_sbar_subj_so_np(uit),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(in),
	part_transitive(om),
	part_transitive(uit),
	part_np_ld_pp(terug)]),
     b([part_intransitive(om),
	part_intransitive(over),
	ld_pp,
	part_intransitive(aan),
	part_intransitive(in),
	part_intransitive(uit),
	part_intransitive(terug),
	part_ld_pp(over),
	part_pc_pp(over,op)])]).

v(schal,schalt,schallen,geschald,schalde,schalden,
    [h([intransitive,
	transitive,
	part_so_np(tegemoet),
	sbar_subj,
	sbar % dip
       ])]).

v(schamper,schampert,schamperen,geschamperd,schamperde,schamperden,
    [h([intransitive,
        mod_pp(over),
	sbar])]).

v(schamp,schampt,schampen,geschampt,schampte,schampten,
    [h([transitive,
	intransitive, % tegen de zijkant
	part_intransitive(af),
	part_ld_pp(af)
       ])]).

v(scharnier,scharniert,scharnieren,gescharnierd,scharnierde,scharnierden,
    [h([intransitive,
	ld_pp])]).

v(scharrel,scharrelt,scharrelen,gescharreld,scharrelde,scharrelden,
    [h([part_intransitive(rond),
	part_transitive(op),
	pc_pp(met),
	fixed([[bij,elkaar],acc],norm_passive),
	fixed([[bij,mekaar],acc],norm_passive),
	part_transitive(bijeen),
	part_ld_pp(rond)]),
     b([intransitive,
        part_intransitive(af),
	ld_pp])]).

v(schat,schat,schatten,geschat,schatte,schatten,
    [h([sbar,
	transitive,
	intransitive,
	np_pc_pp(op),
	part_ap_pred_np(in),
	part_transitive(in),
        part_np_pc_pp(in,op),
	part_sbar(in),
	pc_pp(naar)])]).

v(schater,schatert,schateren,geschaterd,schaterde,schaterden,
  [h([intransitive,
      pc_pp(om),
      sbar,			% dip
      part_transitive(uit)])]).

v(schaterlach,schaterlacht,schaterlachen,geschaterlacht,schaterlachte,schaterlachten,
    [h([intransitive])]).

v(scheel,scheelt,schelen,gescheeld,scheelde,scheelden,
    [h([np_np,
	intransitive,
	sbar_subj,          % het scheelt, dat ...
	sbar_subj_so_np,    % het kan me niet schelen, dat
	sbar_subj_np,       % het scheelt een slok op een borrel, dat
	sbar_subj_np_np,    % het kan me heel veel geld schelen, dat
	pc_pp(aan),  % wat scheelt er aan?
	so_np,
	transitive,
	pc_pp(in)])]).

v(scheep,scheept,schepen,gescheept,scheepte,scheepten,
    [h([part_transitive(af),
	part_transitive(in),
        part_intransitive(in),
	np_ld_pp,
	part_np_pc_pp(af,met),
	part_np_pc_pp(op,met)])]).

v(scheer,scheert,scheren,gescheerd,scheerde,scheerden,
    [z([ld_pp,
	part_refl(weg)])]).  % scheer je weg

v(scheer,scheert,scheren,[geschoren,gescheerd],
  [schoor,scheerde],[schoren,scheerden],
    [h([np_np,
	transitive,
	fixed([[over,een,kam],acc],norm_passive),
	fixed([[over,één,kam],acc],norm_passive),
	part_transitive(af),
	part_transitive(kaal),
	part_transitive(weg),
	intransitive
       ])]).

v(schei,scheit,scheien,gescheden,schee,scheet,
    [h([part_pc_pp(uit,met),
	part_intransitive(uit)])]).

v(scheid,scheidt,scheiden,gescheiden,scheidde,scheidden,
    [z([intransitive,
	pc_pp(van)]),
     h([transitive,
	so_np,  % aanvankelijk scheidden ons slechts luttele meters
	np_pc_pp(van),
	part_refl(af),
	refl,  % hier scheiden zich de wegen
	part_transitive(af),
	part_transitive(uit),
	refl_pc_pp(van),
	part_np_pc_pp(af,met),
	part_np_pc_pp(af,van),
	part_refl_pc_pp(af,van)])]).

v(scheit,scheit,scheiten,gescheten,scheet,scheten,
    [h([intransitive])]).

%% bellen
v(schel,schelt,schellen,gescheld,schelde,schelden,
    [h([intransitive])]).

v(scheld,scheldt,schelden,gescholden,schold,scholden,
    [h([intransitive,
        part_transitive(kwijt),
        part_np_np(kwijt),
        part_transitive(vrij),
        part_np_np(vrij),
	part_als_pred_np(uit),
	part_transitive(uit),
	dip_sbar,
	pc_pp(op),
	ap_pred_np, % ik schold hem verrot
	part_transitive(verrot), % ik heb hem verrotgescholden
	part_fixed(vol,[[de,huid],dat],imp_passive),
	part_np_pc_pp(uit,voor)])]).

v(schemer,schemert,schemeren,geschemerd,schemerde,schemerden,
    [h([intransitive,
        part_intransitive(door),  % cf +laten
        mod_pp(doorheen)
       ])]).

v(schend,schendt,schenden,geschonden,schond,schonden,
    [h([transitive])]).

v(schenk,schenkt,schenken,geschonken,schonk,schonken,
    [h([np_np,
        so_pp_np,
	transitive,
	intransitive,  % je mag belastingvrij schenken tot 100000 euro
	np_ld_pp,
	part_intransitive(bij),
	part_so_np(bij),
        part_intransitive(door),
	part_transitive(uit),
	part_np_np(in),
	part_transitive(in),
	part_intransitive(in),
        part_so_pp_np(in,voor),
	part_transitive(weg),
	part_so_pp_np(weg),
	part_intransitive(weg)
       ])]).

v(schep,schept,scheppen,geschapen,schiep,schiepen,
    [h([transitive,
	refl_np,
	part_transitive(om),
	np_ld_pp,
        part_transitive(af),
        part_transitive(weg),
	part_intransitive(door),
	part_transitive(door),
	part_transitive(over),
        part_np_ld_pp(over),
	part_transitive(om),
        part_intransitive(om),
        part_transitive(uit),
        part_intransitive(uit)])]).

v(schep,schept,scheppen,geschept,[schiep,schepte],[schiepen,schepten],
    [h([fixed([{[pc(in),acc(genoegen)]}],norm_passive),
	fixed([{[er_pp(in,C),acc(genoegen)]},extra_sbar(C)],norm_passive),
	fixed([{[er_pp(in,C),acc(genoegen)]},extra_vp(C)],norm_passive),
        fixed([{[pc(in),acc(plezier)]}],norm_passive),
	fixed([{[er_pp(in,C),acc(plezier)]},extra_sbar(C)],norm_passive),
	fixed([{[er_pp(in,C),acc(plezier)]},extra_vp(C)],norm_passive),
	fixed([{[acc(behagen),pc(in)]}],norm_passive),
	fixed([{[acc(behagen),er_pp(in,C)]},extra_vp(C)],norm_passive),
	fixed([{[acc(behagen),er_pp(in,C)]},extra_sbar(C)],norm_passive)
       ])]).
  

v(schep,schept,scheppen,geschept,schepte,schepten,
    [h([transitive,
	np_ld_pp,
	part_intransitive(op),
	part_transitive(op),
	part_sbar(op),
	part_transitive(om),
	part_transitive(om),
	fixed([acc(genoegen),pc(in)],no_passive),
	fixed([acc(genoegen),er_pp(in,C),extra_vp(C)],no_passive),
	part_pc_pp(op,over),
	part_er_pp_sbar(op,over)])]).

v(scherm,schermt,schermen,geschermd,schermde,schermden,
    [h([intransitive,
	part_transitive(af),
	pc_pp(met),
	er_pp_sbar(met),	% hij schermde ermee dat
	er_pp_vp(met),          % hij schermde ermee een deal te hebben met ...
	part_np_pc_pp(af,van)])]).

v(scherp,scherpt,scherpen,gescherpt,scherpte,scherpten,
    [h([intransitive,
	transitive,
	part_transitive(aan)])]).

v(scherts,schertst,schertsen,geschertst,schertste,schertsten,
    [h([intransitive,
	sbar, % dip
	pc_pp(met)])]).

v(schets,schetst,schetsen,geschetst,schetste,schetsten,
    [h([np_np,
	sbar,
	np_sbar,
	intransitive,
	transitive])]).

v(schetter,schettert,schetteren,geschetterd,schetterde,schetterden,
    [h([intransitive])]).

v(scheur,scheurt,scheuren,gescheurd,scheurde,scheurden,
    [h([np_np,
	intransitive,
	transitive,
	part_intransitive(af),
	part_intransitive(los),
	part_intransitive(open),
	% part_refl(los),
	part_transitive(af),
        part_transitive(door),
	part_transitive(in),
	part_transitive(los),
	part_transitive(open),
	part_transitive(uit),
	part_np_ld_pp(af),
	part_np_ld_pp(los)]),
     z([part_intransitive(aan),
	part_intransitive(in)]),
     b([nonp_pred_np,
        part_intransitive(langs),
        part_intransitive(door),
	ld_pp,
	ld_dir % ik ben heel London door gescheurd
       ])]).

m(v_root(schiet_te_binnen,te_binnen_schieten),
  verb(zijn,sg,[ninv(intransitive,intransitive),
                ninv(so_np,so_np),
                ninv(sbar_subj_so_np_opt_het,sbar_subj_so_np_opt_het)
               ]),
  [te,binnenschiet]).

m(v_root(schiet_te_binnen,te_binnen_schieten),
  verb(zijn,pl,[ninv(intransitive,intransitive),
                ninv(so_np,so_np),
                ninv(sbar_subj_so_np_opt_het,sbar_subj_so_np_opt_het)
               ]),
  [te,binnenschieten]).

m(v_root(schiet_te_binnen,te_binnen_schieten),
  verb(zijn,inf,[ninv(intransitive,intransitive),
                ninv(so_np,so_np),
                ninv(sbar_subj_so_np_opt_het,sbar_subj_so_np_opt_het)
               ]),
  [te,binnenschieten]).

m(v_root(schiet_te_binnen,te_binnen_schieten),
  verb(zijn,psp,[ninv(intransitive,intransitive),
                ninv(so_np,so_np),
                ninv(sbar_subj_so_np_opt_het,sbar_subj_so_np_opt_het)
               ]),
  [te,binnengeschoten]).

m(v_root(schiet_te_binnen,te_binnen_schieten),
  verb(zijn,past(sg),[ninv(intransitive,intransitive),
                ninv(so_np,so_np),
                ninv(sbar_subj_so_np_opt_het,sbar_subj_so_np_opt_het)
               ]),
  [te,binnenschoot]).

m(v_root(schiet_te_binnen,te_binnen_schieten),
  verb(zijn,past(pl),[ninv(intransitive,intransitive),
                ninv(so_np,so_np),
                ninv(sbar_subj_so_np_opt_het,sbar_subj_so_np_opt_het)
               ]),
  [te,binnenschoten]).

v(schiet,schiet,schieten,geschoten,schoot,schoten,
    [z([part_intransitive(aan),
	part_intransitive(door),
	part_intransitive(in),
	part_intransitive(neer),
	part_intransitive(omhoog),
	part_intransitive(op),
	part_intransitive(over),
	part_intransitive(toe),
	part_intransitive(uit),
	part_intransitive(voorbij),
	part_intransitive(weg),
	part_ld_pp(voorbij),
	part_ld_pp(af),
	part_ld_pp(in),
	part_ld_pp(neer),
	part_ld_pp(uit),
        so_np_ld_pp,  % het schiet ons in de keel
        fixed([[te,binnen]],no_passive),
        fixed([[te,binnen],dat],no_passive),
        fixed([[te,binnen],dat,sbar_subj_opt_het],no_passive),
	fixed([ld_pp,sbar_subj],no_passive), % het schoot door mijn gedachten dat ...
			  	             % het schoot door me heen dat ...
	                                     % het schoot in het verkeerde keelgat dat ...
	fixed([{[pp_pred(in,actie),pc(tegen)]}],no_passive),
	fixed([pp_pred(in,actie)],no_passive),	
	part_np_ld_pp(af),
	part_np_ld_pp(weg),
	part_np_pc_pp(af,met),
	part_pc_pp(op,met),
	part_np_pc_pp(op,met),
	part_pc_pp(uit,tegen)]),
     h([np_np,
	intransitive,
	transitive,
	np_ld_dir,
	np_ld_pp,
	part_np_np(af),
	part_np_np(uit),
	part_intransitive(mis),
	part_intransitive(raak),
	part_intransitive(terug),
	part_intransitive(af),
	part_intransitive(naast),
	part_intransitive(over),
	part_transitive(raak),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(dood),
	part_transitive(in),
	part_transitive(neer),
	part_transitive(omhoog),
	part_transitive(voor),
	part_transitive(voorbij),
	part_np_np(voor),
	part_so_pp_np(voor),
	part_transitive(weg)]),
     b([nonp_pred_np,  % raak, naast
        pp_pred_np(in,brand),
	ld_dir,
	ld_pp,
	part_ld_pp(in),
	part_ld_pp(over),
	part_transitive(op),
	part_transitive(over),
	part_transitive(uit),
	fixed([[te,hulp]],no_passive),
	fixed([[te,hulp],dat],no_passive),
        part_intransitive(tekort),
        part_pc_pp(tekort,in),
	fixed([[wakker]],no_passive)])]).

v(schift,schift,schiften,geschift,schiftte,schiftten,
    [h([transitive,
	np_pc_pp(van)]),
     b([intransitive])]).

v(schijn,schijnt,schijnen,geschenen,scheen,schenen,
    [h([copula,
	copula_np,
	copula_sbar,
	copula_vp,
	intransitive,
	                       % Naar het schijnt moeten we naar huis
        dip_sbar_subj_opt_het, % Er ligt een recessie op de loer , schijnt .
	so_copula,
	so_copula_sbar,
	so_copula_vp,
	so_copula_np,
	part_dip_sbar_subj_so_np(toe),   % het schijnt mij toe dat ...; we moeten weg , naar het mij toeschijnt
        part_so_copula(toe),         % de flora schijnt mij rijker toe
	pc_pp(met),
        pc_pp(aan),
        mod_pp(doorheen),
        pc_pp(overheen),        % ik ben er nu wel overheen
        er_pp_sbar(overheen),   % ik ben er nog niet overheen dat ...
        part_sbar(uit),         % VL
	part_van_sbar(uit),     % VL
	part_transitive(uit),   % VL
	aux(te),
	passive,                % het boek scheen voor hem klaargelegd
        fixed([vc(lig,psp,intransitive),pc(aan),dat],no_passive),
        fixed([vc(lig,psp,intransitive),extra_obj_vp(A,B),er_pp(aan,A),i(dat,B)],no_passive),
        fixed([vc(lig,psp,intransitive),extra_sbar(A),er_pp(aan,A),dat],no_passive),
        fixed([pc(tot),subj(aanleiding)],no_passive),
        fixed([pc(voor),subj(aanleiding)],no_passive),
	fixed([[de,baas],acc],no_passive), % ik kan de stress niet de baas
	fixed([[te,baas],acc],no_passive), % VL
	fixed([pc(aan),[de,beurt]],no_passive),
	fixed([{[np_pred(dupe),pc(van)]}],no_passive),
	fixed([{[np_pred(dupe),er_pp(van,C)]},extra_sbar(C)],no_passive),
        fixed([pp_pred(in,hand),pc(van)],no_passive),
	fixed([pc(van),ap_pred(op)],no_passive), % hij zou op van de zenuwen zijn / op zijn van de zenuwen
	fixed([pc(aan),ap_pred(na)],no_passive),
	fixed([[in,omloop]],no_passive),
	    %% daar is geen discussie/twijfel over mogelijk
	    fixed([{[pc(over),ap_pred(mogelijk)]}],no_passive),
	    fixed([{[er_pp(over,A),ap_pred(mogelijk)]},extra_sbar(A)],
		  no_passive),
	fixed([subj(sprake),pc(van)],no_passive),
	fixed([subj(sprake),er_pp(van,A),extra_sbar(A)],no_passive),
	fixed([[te,boven],acc],no_passive),
	fixed([vc(doe,pass_te,intransitive),pc(om),dat],no_passive),
	fixed([vc(spreek,pass_te,intransitive),pc(over)],no_passive),
				% hij is daar helemaal niet over te spreken
	fixed([vc(spreek,pass_te,intransitive),er_pp(over,C),extra_sbar(C)],no_passive),
				% hij is daar helemaal niet over te spreken
	fixed([[terwille],dat],no_passive),
        part_fixed(toe,[er_pp(aan),ap_pred],no_passive),
	part_so_copula_vp(toe), % ouderwets: het had ons heerlijk toegeschenen om te blijven
	part_np_vp_subj(toe),   % ouderwets: het buro had me toegeschenen een paardenstal te zijn
        fixed([pp_pred(van,invloed),pc(op)],no_passive),
        fixed([[van,de,partij]],no_passive),
	fixed([[van,plan],acc],no_passive),
	fixed([[van,plan],vp],no_passive),
	fixed([[van,de,plan],acc],no_passive), % VL
	fixed([[van,de,plan],vp],no_passive)   % VL
       ])]).

v(schijt,schijt,schijten,gescheten,scheet,scheten,
    [h([intransitive,
	transitive])]).

v(schik,schikt,schikken,geschikt,schikte,schikten,
    [h([sbar_subj_so_np,
	intransitive,
	refl,
        refl_pc_pp(in),
        refl_pc_pp(naar),
	so_np,
	transitive,
	fixed([[in,der,minne],acc],norm_passive),
	%refl_ld_pp,
	np_ld_pp  % schik er wat sla op; schik er wat kaas op
	]),
     b([part_intransitive(in)
	])]).

v(schil,schilt,schillen,geschild,schilde,schilden,
    [h([transitive,
	part_transitive(af)])]).

v(schilder,schildert,schilderen,geschilderd,schilderde,schilderden,
    [h([ap_pred_np,
	np_np,
	intransitive,
	part_intransitive(over),
	transitive,
	mod_pp(over),
	np_mod_pp(over),
	part_mod_pp(heen,over),
	part_np_mod_pp(heen,over),
	part_pred_np(af),
	part_transitive(af),
	part_transitive(over),
	part_np_pc_pp(af,naar)])]).

v(schimmel,schimmelt,schimmelen,geschimmeld,schimmelde,schimmelden,
    [b([intransitive])]).

v(schimp,schimpt,schimpen,geschimpt,schimpte,schimpten,
    [h([intransitive,
	sbar])]).

v(schipper,schippert,schipperen,geschipperd,schipperde,schipperden,
    [h([intransitive])]).

v(schitter,schittert,schitteren,geschitterd,schitterde,schitterden,
    [h([intransitive])]).

v(schmier,schmiert,schmieren,geschmierd,schmierde,schmierden,
    [h([intransitive])]).

v(schmink,schminkt,schminken,geschminkt,schminkte,schminkten,
    [h([transitive])]).

v(schnabbel,schnabbelt,schnabbelen,geschnabbeld,schnabbelde,schnabbelden,
    [h([intransitive,
	part_intransitive(rond),
	part_intransitive(bij)])]).

v(schoei,schoeit,schoeien,geschoeid,schoeide,schoeiden,
    [h([transitive,
	fixed([svp_pp(op,leest),acc],norm_passive)])]).

v(schoffeer,schoffeert,schofferen,geschoffeerd,schoffeerde,schoffeerden,
    [h([transitive])]).

v(schoffel,schoffelt,schoffelen,geschoffeld,schoffelde,schoffelden,
    [h([intransitive,
	transitive, % de perkjes
	part_transitive(onderuit)])]).

v(schok,schokt,schokken,geschokt,schokte,schokten,
    [h([intransitive,
	part_intransitive(na),
	so_np,
	sbar_subj_so_np,
	transitive])]).

v(schokschouder,schokschoudert,schokschouderen,geschokschouderd,schokschouderde,schokschouderden,
    [h([intransitive,
        dip_sbar])]).

v(schommel,schommelt,schommelen,geschommeld,schommelde,schommelden,
    [h([intransitive])]).

v(schooi,schooit,schooien,geschooid,schooide,schooiden,
    [h([intransitive])]).

v(school,schoolt,scholen,geschoold,schoolde,schoolden,
    [h([intransitive,
	transitive,
	part_intransitive(bij),
	part_transitive(bij),
	part_transitive(om)])]).

v(schoon,schoont,schonen,geschoond,schoonde,schoonden,
    [h([transitive,
	np_pc_pp(van),
	part_transitive(op)])]).

v(schop,schopt,schoppen,geschopt,schopte,schopten,
    [h([intransitive,
	np_ld_dir,
	transitive,
	ld_pp,
	np_ld_pp,
	np_pc_pp(tot),
        np_np, % schop de mensen een geweten!
        part_transitive(in),  % de deur...
        part_transitive(om),  % een stoel...
        part_transitive(uit), % haar schoenen...
        part_pc_pp(in,op),    % op iemand inschoppen
	fixed([{[pc(over),acc(herrie)]}],imp_passive),
	fixed([{[acc(herrie),pc(om)]}],imp_passive),
	fixed([ap_pred('in de war'),acc],norm_passive)])]).

v(schors,schorst,schorsen,geschorst,schorste,schorsten,
    [h([intransitive,
	transitive])]).

v(schort,schort,schorten,geschort,schortte,schortten,
    [h([transitive,
        intransitive, % ik wil weten wat er schort
	er_pp_sbar(aan),
	part_transitive(op),
	pc_pp(aan),
	part_np_pc_pp(op,tot)])]).

v(schotel,schotelt,schotelen,geschoteld,schotelde,schotelden,
  [h([part_np_np(voor),
      part_transitive(voor)])]).

v(schouw,schouwt,schouwen,geschouwd,schouwde,schouwden,
    [h([sbar,
	transitive,
	part_transitive(aan),
	part_np_pc_pp(aan,met)])]).

v(schraag,schraagt,schragen,geschraagd,schraagde,schraagden,
    [h([transitive])]).

v(schraap,schraapt,schrapen,geschraapt,schraapte,schraapten,
    [h([intransitive,
	part_transitive(af),
	part_transitive(uit),
	transitive])]).

v(schrap,schrapt,schrappen,geschrapt,schrapte,schrapten,
    [h([intransitive,
	part_transitive(door),
	transitive,
	ld_pp,  % daar moet in geschrapt worden
	np_ld_pp])]).

v(schreeuw,schreeuwt,schreeuwen,geschreeuwd,schreeuwde,schreeuwden,
    [h([intransitive,
	sbar,
	transitive,
	vp,
	part_np_np(in),  % hij schreeuwde hem moed in
	part_transitive(na),
	part_sbar(na),
	part_np_sbar(na),
	part_transitive(uit),
	part_transitive(toe),
	part_np_np(toe),
	part_np_sbar(toe),	
	pc_pp(om),
	pc_pp(tegen)])]).

v(schrei,schreit,schreien,geschreid,schreide,schreiden,
    [h([intransitive,
	transitive,
	pc_pp(om)])]).

v(schrijd,schrijdt,schrijden,geschreden,schreed,schreden,
  [b([intransitive,
      ld_pp,
      ld_dir])]).

v(schrijf,schrijft,schrijven,geschreven,schreef,schreven,
    [h([np_np,
	intransitive,
	np_sbar,
	np_vp_obj,
	sbar,
	so_pp_np,
	so_np,
	so_pp_sbar,
	so_vp_no_control,
	np_ld_dir, % iemand de grond in schrijven
	transitive,
	vp,
	np_ld_pp,          % een brief schrijven naar
	pc_pp(over),
	np_pc_pp(over),
	part_intransitive(terug),
	part_transitive(terug),
	part_np_np(bij),  % hij kreeg een miljoen op zijn rekening bijgeschreven
	part_np_np(terug),
	part_np_sbar(terug),
	part_np_vp_obj(terug),
	part_sbar(terug),
	part_so_pp_np(terug),
	part_so_np(terug),
	part_als_pred_np(in),
        part_fixed(in,[{[als_pred,acc,ld_pp]}],norm_passive),
	part_als_pred_np(uit),
	part_np_np(aan),
	part_np_np(toe),
	part_transitive(toe),
	part_np_np(voor),
	part_intransitive(in),
	part_intransitive(over),
	part_np_sbar(voor),
	part_np_vp_obj(aan),
	part_refl(in),  % volgorde: @ Er schreven zich 25 mensen in .
	part_refl(uit),
	part_sbar(op),
	part_sbar(voor),
	part_so_pp_np(toe),
	part_so_pp_np(voor),
	part_transitive(aan),
	part_intransitive(af),
	part_transitive(af),
	part_transitive(bij),
	part_transitive(in),
        part_np_ld_pp(in),
        part_np_ld_adv(in),
	part_transitive(neer),
	part_transitive(op),
	part_intransitive(op),  % schrijf op
	part_transitive(over),
	part_transitive(uit),
	part_transitive(vol),
        part_np_pc_pp(vol,over),
	part_transitive(voor),
	part_intransitive(voor), % dokters moeten zuiniger voorschrijven
	fixed([svp_pp(op,naam),acc],norm_passive),
	fixed([svp_pp(op,buik),acc],norm_passive),
	part_vp(op),
	part_vp(voor),
	part_np_ld_pp(over),
	part_np_pc_pp(af,met),
	part_np_pc_pp(af,op),
	part_np_pc_pp(af,van),
	part_pc_pp(in,op)
       ])]).

v(schrijn,schrijnt,schrijnen,geschrijnd,schrijnde,schrijnden,
    [h([intransitive])]).

v(schrik,schrikt,schrikken,[geschrokken,geschrikt],[schrok,schrikte],[schrokken,schrikten],
  [unacc([intransitive,
	  sbar, % vooral VL
	  part_intransitive(terug),
	  part_intransitive(op),
	  part_mod_pp(op,van),
	  pc_pp(van),
	  er_pp_sbar(van),
	  er_pp_vp(van),
	  pc_pp(voor),
	  pred_refl,
	  part_refl(dood),
	  part_pc_pp(op,uit),
	  part_pc_pp(terug,van),
	  part_er_pp_vp(terug,van),
	  part_pc_pp(terug,voor),
	  part_er_pp_vp(terug,voor)]),
   h([part_intransitive(af),
      part_transitive(op),
      part_transitive(af),
      part_np_vp_obj1(af),
      part_sbar_subj_np(af)])]). % dat Koller meedeed heeft ons niet afges.

v(schrob,schrobt,schrobben,geschrobd,schrobde,schrobden,
    [h([intransitive,
	transitive])]).

v(schroef,schroeft,schroeven,geschroefd,schroefde,schroefden,
    [h([ap_pred_np,
	intransitive,
	transitive,
	np_ld_pp,
	part_transitive(af),
	part_transitive(op),
	part_transitive(terug)])]).

v(schroei,schroeit,schroeien,geschroeid,schroeide,schroeiden,
    [h([intransitive,
	transitive])]).

v(schrok,schrokt,schrokken,geschrokt,schrokte,schrokten,
    [h([intransitive,
	np_ld_pp])]).

v(schrompel,schrompelt,schrompelen,geschrompeld,schrompelde,schrompelden,
    [unacc([intransitive,
	    part_intransitive(ineen)])]).

v(schroom,schroomt,schromen,geschroomd,schroomde,schroomden,
    [h([transitive,
	intransitive, % schroom niet!
	vp])]).

v(schud,schudt,schudden,geschud,schudde,schudden,
    [h([nonp_pred_np,
	np_np,
	intransitive,
	transitive,
	np_ld_pp,
	np_ld_dir,
	van_sbar, % hij schudde van nee
	part_refl(af),
	part_transitive(af),
	part_transitive(op),
	part_transitive(om),
	part_transitive(uit),
        part_transitive(wakker),  % ze moeten even worden wakker geschud
	part_np_ld_pp(af)])]).

v(schuddebuik,schuddebuikt,schuddebuiken,geschuddebuikt,schuddebuikte,schuddebuikten,
    [h([intransitive])]).

v(schuif,schuift,schuiven,geschoven,schoof,schoven,
    [z([ld_dir,
	ld_pp,
	part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(bij),
	part_intransitive(door),
	part_intransitive(in),  % de verdedigers moeten inschuiven
	part_intransitive(op),
	part_intransitive(open),
	part_intransitive(uit), % VL uitglijden
	part_intransitive(weg),
	pc_pp(met),
	part_ld_pp(af),
	part_ld_pp(op)]),
     h([intransitive,
        transitive, % wat schuift dat?
	np_ld_dir,
	np_np_ld_pp,
	transitive,
	np_ld_pp,
	part_np_np(toe),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(bij),
	part_transitive(door),
	part_transitive(op),
	part_transitive(open),
	part_transitive(toe),
	part_transitive(vooruit),
	part_transitive(weg),
        fixed([[naar,voren],acc,als_pred],no_passive),
	fixed([[op,de,lange,baan],acc],norm_passive),
	fixed([[in,de,schoenen],{[acc,dat]}],norm_passive),
	part_np_ld_pp(af),
	part_np_pc_pp(toe,naar)])]).

v(schuifel,schuifelt,schuifelen,geschuifeld,schuifelde,schuifelden,
    [z([ld_dir,
	ld_pp]),
     b([intransitive,
	part_intransitive(aan)])]).

v(schuil,schuilt,schuilen,[geschuild,gescholen],[schuilde,school],[schuilden,scholen],
    [h([intransitive,
	ld_pp,
	ld_adv,
	fixed([{[ld_pp,[hem]]}],no_passive),
	er_pp_sbar(in),
	pc_pp(voor)])]).

v(schuim,schuimt,schuimen,geschuimd,schuimde,schuimden,
    [h([intransitive,
	part_transitive(af),
	transitive])]).

v(schurk,schurkt,schurken,geschurkt,schurkte,schurken,
    [h([part_pc_pp(aan,tegen),
	part_refl_pc_pp(aan,tegen),
	pc_pp(tegen),
	intransitive,
	refl_pc_pp(tegen)])]).

v(schut,schut,schutten,geschut,schutte,schutten,
    [h([transitive])]).

v(schutter,schuttert,schutteren,geschutterd,schutterde,schutterden,
    [h([intransitive])]).

v(schuur,schuurt,schuren,geschuurd,schuurde,schuurden,
    [h([intransitive,
	transitive,
	part_transitive(uit)
       ])]).

v(schuw,schuwt,schuwen,geschuwd,schuwde,schuwden,
    [h([transitive,
        vp_obj,  % hij schuwt het niet (om) mee naar voren te komen
        vp       % hij schuwt niet (om) naar het net te komen
       ])]).

v(scoor,scoort,scoren,gescoord,scoorde,scoorden,
    [h([intransitive,
	part_intransitive(tegen),
	pc_pp(op),  % hij scoort op immigratie en buitenlanders
	transitive])]).

v(scout,scout,scouten,gescout,scoutte,scoutten,
    [h([intransitive,
	transitive])]).

v(screen,screent,screenen,gescreend,screende,screende,
    [h([transitive,
        sbar])]).  % hij screende of alles wel klopte

v([scroll,scrol],[scrollt,scrolt],scrollen,[gescrolld,gescrold],[scrollde,scrolde],[scrollden,scrolden],
  [h([intransitive,
      ld_dir
     ])]).

v(seal,sealt,sealen,geseald,sealde,sealden,
    [h([intransitive,
	transitive])]).

v(sein,seint,seinen,geseind,seinde,seinden,
    [h([intransitive,
	sbar,
	so_pp_np,
	transitive,
        part_intransitive(door),
        part_transitive(door),
	part_transitive(in),
	vp,
	pc_pp(om)])]).

v(seks,sekst,seksen,gesekst,sekste,seksten,
    [h([transitive,
	intransitive
       ])]).

v(selecteer,selecteert,selecteren,geselecteerd,selecteerde,selecteerden,
    [h([transitive,
	pc_pp(op),
	np_pc_pp(op),
	intransitive,
	part_transitive(uit)])]).

v(seponeer,seponeert,seponeren,geseponeerd,seponeerde,seponeerden,
    [h([transitive])]).

v(serveer,serveert,serveren,geserveerd,serveerde,serveerden,
    [h([intransitive,
        part_transitive(af),
	so_pp_np,
        np_np,
        part_intransitive(uit),  % wil je helpen uit te serveren
        part_transitive(uit),  % de partij uit serveren; het eten uitserveren
	np_pc_pp(bij), % serveer er friet bij!
	transitive])]).

v(settel,settelt,settelen,gesetteld,settelde,settelden,
  [h([intransitive,  % ik zie mij niet settelen
      refl])]).      % ik zie mij me niet settelen

v(shockeer,shockeert,shockeren,geshockeerd,shockeerde,shockeerden,
    [h([sbar_subj_so_np,
	transitive,
	intransitive,
	vp_subj_so_np])]).

v(shop,shopt,shoppen,geshopt,shopte,shopten,
    [h([intransitive,
	transitive  % leuke dingen shoppen (??)
       ])]).

v(show,showt,showen,geshowd,showde,showden,
  [h([transitive,
      np_np,
      so_pp_np,
      np_sbar,
      so_pp_sbar,
      sbar
     ])]).

v(sidder,siddert,sidderen,gesidderd,sidderde,sidderden,
    [h([intransitive,
	pc_pp(voor),
	pc_pp(van)  % om van te sidderen
       ])]).

v(sier,siert,sieren,gesierd,sierde,sierden,
    [h([sbar_subj_so_np,
	transitive,
	part_transitive(op),
	vp_subj_so_np,
	np_pc_pp(met)])]).

v(signaleer,signaleert,signaleren,gesignaleerd,signaleerde,signaleerden,
    [h([sbar,
	transitive,
        intransitive, % Pim Fortuyn signaleerde
	np_ld_adv,
	np_ld_pp])]).

v(signeer,signeert,signeren,gesigneerd,signeerde,signeerden,
    [h([transitive,
        intransitive])]).

v(sijpel,sijpelt,sijpelen,gesijpeld,sijpelde,sijpelden,
    [b([ld_pp,
	ld_dir,
	part_ld_pp(binnen),
	part_ld_pp(door),
	part_intransitive(weg),
	part_ld_pp(weg),
	intransitive])]).

v(simuleer,simuleert,simuleren,gesimuleerd,simuleerde,simuleerden,
    [h([transitive,
        intransitive,
        sbar,
	vp])]).

v(sip,sipt,sippen,gesipt,sipte,sipten,
    [h([transitive,
        intransitive,
        sbar
       ])]).

v(sis,sist,sissen,gesist,siste,sisten,
    [h([intransitive,
	sbar,
	part_np_sbar(toe), % Ik siste mijn vriend nijdig toe dat we ...
	transitive,
	vp])]).

v(situeer,situeert,situeren,gesitueerd,situeerde,situeerden,
    [h([np_ld_pp,
	np_ld_adv,
	als_pred_refl,
	refl_ld_pp, % VL?
	intransitive,
	transitive
       ])]).

v(sjoel,sjoelt,sjoelen,gesjoeld,sjoelde,sjoelden,
    [h([intransitive])]).

v(sjoemel,sjoemelt,sjoemelen,gesjoemeld,sjoemelde,sjoemelden,
    [h([intransitive])]).

v(sjees,sjeest,sjezen,gesjeesd,sjeesde,sjeesden,
    [b([intransitive,
        part_intransitive(aan),
        ld_pp,
        ld_dir])]).

v(sjok,sjokt,sjokken,gesjokt,sjokte,sjokten,
    [b([intransitive,
        ld_pp,
        ld_dir,
        part_intransitive(aan)])]).

v(sjor,sjort,sjorren,gesjord,sjorde,sjorden,
    [h([intransitive,
	transitive,
	pc_pp(aan)])]).

v(sjouw,sjouwt,sjouwen,gesjouwd,sjouwde,sjouwden,
    [z([ld_dir]),
     b([ld_pp,
	part_intransitive(aan)]),
     h([intransitive,
	part_intransitive(rond),
	np_ld_dir,
	np_ld_pp,
	transitive,
	pc_pp(met)])]).

v(skandeer,skandeert,skanderen,geskandeerd,skandeerde,skandeerden,
    [h([transitive,
	sbar])]).

v(skate,skatet,skaten,geskated,skatede,skateden,
    [h([intransitive,
	transitive]),
     b([ld_pp,
	part_intransitive(aan),
	ld_dir])]).

v(skateboard,skateboardt,skateboarden,geskateboard,skateboardde,skateboardden,
    [h([intransitive])]).

v(skeeler,skeelert,skeeleren,geskeelerd,skeelerde,skeelerden,
    [b([ld_pp,
	part_intransitive(aan),
	ld_dir]),
     h([intransitive,
	transitive
       ])]).

v([ski,skie],skiet,skiën,geskied,skiede,skieden,
    [h([intransitive,
        transitive % de afdaling
       ]),
     z([ld_pp,
        ld_dir])]).

v(skip,skipt,skippen,geskipt,skipte,skipten,
    [h([transitive])]).

v([skype,skyp],[skypet,skypt],skypen,[geskypet,geskypt],[skypete,skypte],[skypeten,skypten],
    [h([intransitive])]).  

v(sla,slaat,slaan,geslagen,sloeg,sloegen,
    [z([aan_het,  % hij slaat aan het rekenen
        ld_pp,
	fixed([[om,het,hart],{[subj(schrik),dat]}],no_passive),
	part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(linksaf),
	part_intransitive(rechtsaf),
	part_intransitive(dicht),
	part_intransitive(door),
	part_intransitive(in),
	part_intransitive(lek),
	part_intransitive(los),
	part_intransitive(neer),
	part_intransitive(om),
	part_intransitive(op),
	part_intransitive(open),
	part_intransitive(over),
	part_intransitive(stuk),
	part_intransitive(weg),
	part_ld_pp(in),
	part_ld_pp(om),
        fixed([svp_pp(op,tilt)],no_passive),
        pp_copula(op,vlucht),
	fixed([[op,hol]],no_passive),
        fixed([[overkop]],no_passive),
        fixed([[over,de,kop]],no_passive),
	fixed([[overboord]],no_passive),
	fixed([[te,pletter]],no_passive),
	fixed([subj(vlam),svp_pp(in,pan)],no_passive),
	part_ld_pp(aan),
	part_ld_pp(af),
	part_ld_pp(door),
	part_ld_pp(in),
	part_ld_pp(neer),
	part_ld_pp(over),
	part_ld_pp(stuk),
	part_pc_pp(af,naar),
	part_pc_pp(om,in),
	part_pc_pp(terug,op)]),
     h([np_np,
	intransitive,
	np_ld_dir,
	transitive,
	np_ld_pp,
	pc_pp(op),   % dat slaat nergens op; dat slaat op X
	             % dat slaat als een tang op een varken
	pp_sbar_subj(op),  % het slaat nergens op dat ...
	pp_vp_subj(op),    % het slaat nergens op om ...
	part_np_np(toe),
	part_np_np(uit),
	part_intransitive(toe),
        % part_refl(in),
        nonp_pred_np,
	part_sbar_subj_so_np(terneer),
	part_sbar(gade),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(dicht),
	part_intransitive(dood),  % gij zult niet doodslaan
	part_transitive(dood),
	part_transitive(door),
	part_transitive(gade),
	part_transitive(in),
	part_transitive(ineen),
	part_transitive(kapot),
	part_transitive(los),
	part_transitive(mis),
	part_np_pc_pp(na,op),
	part_transitive(na),
	part_transitive(neer),
	part_transitive(om),
	part_transitive(op),
	part_np_ld_pp(op),
	part_transitive(open),
	part_transitive(over),
	part_transitive(stuk),
	part_transitive(terneer),
	part_intransitive(terug),
	part_transitive(terug),
	part_transitive(toe),
	part_transitive(uit),
	part_transitive(weg),
	fixed([{[pc(op),acc(acht)]}],norm_passive),
        fixed([svp_acc(alarm)],norm_passive),  % groot alarm werd geslagen
 	fixed([{[pc(uit),[een,slaatje]]}],imp_passive),
 	fixed([{[pc(uit),[geen,slaatje]]}],imp_passive),
	fixed([{[pc(uit),acc(munt)]}],imp_passive),
	fixed([[in,elkaar],acc(hand)],norm_passive),
	fixed([[inelkaar],acc(hand)],norm_passive),
	fixed([[in,mekaar],acc(hand)],norm_passive),
	fixed([[in,de,wind],acc],norm_passive),
	fixed([[in,de,boeien],acc],norm_passive),
	fixed([[op,de,borst],refl],no_passive),
	fixed([[op,de,borst],refl,sbar],no_passive),
	fixed([[te,pletter],refl],no_passive),
	fixed([refl,svp_pp(voor,kop)],no_passive),
	fixed([svp_pp(voor,kop),refl,sbar],no_passive),
	part_np_pc_pp(af,van),
	part_np_pc_pp(op,met)]),
     b([part_np_ld_pp(af),
	part_np_ld_pp(weg),
	part_intransitive(uit)])]).

v(slaag,slaagt,slagen,geslaagd,slaagde,slaagden,
    [unacc([intransitive,
	    er_pp_vp(in),
	    er_pp_sbar(in),
	    pc_pp(in),
	    pc_pp(met),
	    pc_pp(voor)])]).

v(slaak,slaakt,slaken,geslaakt,slaakte,slaakten,
    [h([transitive])]).

v(slaap,slaapt,slapen,geslapen,sliep,sliepen,
    [z([part_intransitive(in),
        mod_pp(doorheen)
       ]),
     h([intransitive,
	transitive,
	part_intransitive(uit),  %'dat hij uitgeslapen is --> adjective'
	part_transitive(uit),    % zijn roes
	mod_pp(op),              % een matras is om op te slapen
	mod_pp(om),              % hij slaapt er niet/geen minuut minder om
	mod_pp(tussen),          % daar slaapt de duivel tussen
	pc_pp(van),              % (niet) kunnen slapen van de kiespijn/zenuwen
	pc_pp(over)])]).         % er een nachtje over slapen

v(slaapwandel,slaapwandelt,slaapwandelen,geslaapwandeld,slaapwandelde,slaapwandelden,
    [b([ld_dir,
	part_intransitive(aan),
	ld_pp]),
     h([intransitive
       ])]).

v(slacht,slacht,slachten,geslacht,slachtte,slachtten,
    [h([transitive,
        intransitive,
	part_transitive(af)])]).

v(slachtoffer,slachtoffert,slachtofferen,geslachtofferd,slachtofferde,slachtofferden,
    [h([transitive])]).

v(slalom,slalomt,slalommen,geslalomd,slalomde,slalomden,
    [b([ld_dir,
	part_intransitive(aan),
	ld_pp]),
     h([intransitive])]).

v(slank,slankt,slanken,geslankt,slankte,slankten,
    [h([part_intransitive(af),
	part_transitive(af)  % het personeelsbestand moet worden afgeslankt
       ])]).
  

v(slecht,slecht,slechten,geslecht,slechtte,slechtten,
    [h([transitive])]).

v(slee,sleet,sleeën,gesleed,sleede,sleeden,gesleed,
    [b([ld_dir,
	part_intransitive(aan),
	ld_pp]),
     h([intransitive])]).

v(sleep,sleept,slepen,gesleept,sleepte,sleepten,
    [h([intransitive,
	transitive,
	ld_pp,
	np_ld_pp,
	np_ld_dir,
	fixed([[in,de,wacht],acc],norm_passive),
	part_intransitive(aan),
	part_transitive(af),
	part_transitive(binnen),
	part_refl(voort),
	part_transitive(aan),
	part_transitive(mee), % NB dat ik de spullen mee naar huis sleep
	part_transitive(voort),
	part_transitive(weg),
	pc_pp(met),
	part_np_ld_pp(mee)])]).

v(slenter,slentert,slenteren,geslenterd,slenterde,slenterden,
    [z([ld_dir]),
     b([ld_pp,
	part_intransitive(aan),
	ld_adv]),
     h([intransitive,
	part_intransitive(rond)])]).

v(sleur,sleurt,sleuren,gesleurd,sleurde,sleurden,
    [h([intransitive,
	part_transitive(mee), % NB dat ik de spullen mee naar huis sleur
	np_ld_dir,
	np_ld_pp])]).

v(sleutel,sleutelt,sleutelen,gesleuteld,sleutelde,sleutelden,
    [h([intransitive,
	pc_pp(aan),
        np_ld_pp                % "in elkaar"
       ])]).

v(slib,slibt,slibben,geslibd,slibde,slibden,
    [b([part_intransitive(aan),
        part_intransitive(dicht),
	ld_pp])]).

v(slijm,slijmt,slijmen,geslijmd,slijmde,slijmden,
    [h([intransitive])]).

v(slijp,slijpt,slijpen,geslepen,sleep,slepen,
    [h([intransitive,
	part_transitive(bij),
	part_np_pc_pp(af,van),
	transitive])]).

v(slijt,slijt,slijten,gesleten,sleet,sleten,
    [unacc([intransitive,
	part_intransitive(af),
	part_intransitive(uit),
	part_ld_pp(af)]),
     h([so_pp_np,
	transitive,
	part_transitive(af),
	part_transitive(uit)])]).

v(slik,slikt,slikken,geslikt,slikte,slikten,
    [h([intransitive,
	sbar,
        sbar_obj,
        vp_obj,
	transitive,
	part_transitive(door),
	part_transitive(in)])]).

v(slinger,slingert,slingeren,geslingerd,slingerde,slingerden,
    [z([ld_dir]),
     b([ld_pp,
	part_intransitive(aan),
	ld_adv]),
     h([intransitive,
	np_ld_dir,
	refl_ld_dir,
	transitive,
	np_ld_pp,
	np_np_ld_pp,
	part_transitive(aan),  % het debat aanslingeren
	part_np_np(toe),
	part_intransitive(rond),
	part_ld_pp(rond),
	part_transitive(rond),
	part_refl(voort),
	pc_pp(met),
	refl_ld_pp])]).

v(slink,slinkt,slinken,geslonken,slonk,slonken,
    [unacc([intransitive])]).

v(slip,slipt,slippen,geslipt,slipte,slipten,
    [z([ld_pp]),
     b([intransitive])]).

v(slis,slist,slissen,geslist,sliste,slisten,
    [h([intransitive])]).

v(slobber,slobbert,slobberen,geslobberd,slobberde,slobberden,
    [h([intransitive,
	transitive,
	ld_pp])]).

v(slof,sloft,sloffen,gesloft,slofte,sloften,
    [z([ld_dir]),
     h([intransitive,
	part_transitive(bij),
	pc_pp(met)]),
     b([ld_pp,
	part_intransitive(aan),
        part_intransitive(rond),
	ld_adv])]).

v(slok,slokt,slokken,geslokt,slokte,slokten,
    [h([intransitive,
	transitive,
	part_sbar_subj_so_np(op),
	part_transitive(op)])]).

v(sloof,slooft,sloven,gesloofd,sloofde,sloofden,
  [h([intransitive,
      part_refl(af),
      part_refl(uit),
      part_refl_pc_pp(uit,voor)])]).

v(sloop,sloopt,slopen,gesloopt,sloopte,sloopten,
    [h([transitive,
        intransitive,
	np_ld_pp])]).

v(slorp,slorpt,slorpen,geslorpt,slorpte,slorpten,
    [h([part_sbar_subj_so_np(op),
        transitive,
	part_transitive(op)])]).

v(sluimer,sluimert,sluimeren,gesluimerd,sluimerde,sluimerden,
    [b([intransitive,
        part_intransitive(in),
	ld_pp,
	ld_adv])]).

v(sluip,sluipt,sluipen,geslopen,sloop,slopen,
    [z([intransitive,
	ld_dir,
	ld_pp,
	part_intransitive(aan),
	part_intransitive(binnen),
	part_intransitive(uit),  % dat doen insekten geloof ik
	part_intransitive(weg),
	part_ld_pp(binnen)]),
     b([part_intransitive(rond)])]).

v(sluis,sluist,sluizen,gesluisd,sluisde,sluisden,
    [h([np_ld_pp,
	np_ld_dir,
	part_np_ld_pp(door),
	part_transitive(door),
	part_transitive(terug),
	part_np_ld_pp(terug),
	part_np_ld_pp(terug),
	part_transitive(weg),
	part_np_ld_pp(weg)
       ])]).

v(sluit,sluit,sluiten,gesloten,sloot,sloten,
    [unacc([part_intransitive(aan),
	    pc_pp(met),
	    part_ld_pp(aan),
	    part_pc_pp(aan,bij)]),
     h([refl,
	intransitive,
	transitive,
	np_ld_pp,
	part_np_np(af),
	part_intransitive(aaneen),
	part_refl(aan),            % refl < su
	part_refl_pc_pp(aan,bij),  % refl < su
                        % toen sloot zich de liberaal Joekes aan
%	part_refl(aaneen),
%	part_refl(op),
	part_sbar_subj_so_np(uit),
	part_sbar(in),
	part_sbar(uit),
	part_transitive(aan),
	part_transitive(aaneen),
	part_intransitive(af),
	part_transitive(af),
        part_transitive(bij),
	part_transitive(buiten),
	part_transitive(in),
	part_transitive(op),
	part_transitive(over),
	part_transitive(uit),
	part_vp(uit),
	% refl_ld_pp,
	fixed([{[pc(voor),acc(oog)]}],norm_passive),
	part_np_ld_pp(in),
	part_np_ld_pp(op),
	part_np_pc_pp(aan,bij),
	part_np_pc_pp(aan,op),
	part_np_pc_pp(af,met),
	part_np_pc_pp(af,van),
	part_np_pc_pp(uit,van),
	part_pc_pp(af,met)])]).

v(slurp,slurpt,slurpen,geslurpt,slurpte,slurpten,
    [h([intransitive,
	transitive,
	part_transitive(op),
	np_ld_pp])]).

v(smaak,smaakt,smaken,gesmaakt,smaakte,smaakten,
    [h([intransitive,
	so_np,
	transitive,
	pc_pp(naar),
	alsof_sbar,
	mod_pp(bij),
	np_alsof_sbar,
	so_nonp_copula,
	nonp_copula])]).

v(smaal,smaalt,smalen,gesmaald,smaalde,smaalden,
    [h([intransitive,
	sbar,
	pc_pp(op)])]).

v(smacht,smacht,smachten,gesmacht,smachtte,smachtten,
    [h([intransitive,
	pc_pp(naar)])]).

v(smak,smakt,smakken,gesmakt,smakte,smakten,
    [z([ld_pp]),
     h([np_ld_pp,
	np_ld_dir]),
     b([intransitive])]).

v(smeed,smeedt,smeden,gesmeed,smeedde,smeedden,
    [h([transitive,
	part_transitive(om),
	part_transitive(samen),
	part_np_pc_pp(samen,tot),
	np_pc_pp(tot),
	np_ld_pp])]).

v(smeek,smeekt,smeken,gesmeekt,smeekte,smeekten,
    [h([intransitive,
	np_vp_obj,
	so_np,
        np_np, % ik smeek het je
	vp,
	sbar,
	np_sbar,
	part_transitive(af),
	pc_pp(om)])]).

v(smeer,smeert,smeren,gesmeerd,smeerde,smeerden,
    [h([intransitive,
	transitive,
	np_ld_pp,
	part_transitive(door),
	part_transitive(aan),
	part_np_np(aan),
	part_transitive(in),
	part_np_pc_pp(in,met),
	part_transitive(uit),
	part_np_pc_pp(uit,over)])]).

v(smelt,smelt,smelten,gesmolten,smolt,smolten,
    [unacc([intransitive,
	part_intransitive(af),
	part_intransitive(samen),
	part_intransitive(weg),
	part_pc_pp(samen,met)]),
     h([transitive,
	part_transitive(om),
	part_transitive(samen),
	part_np_pc_pp(samen,met),
	part_np_pc_pp(samen,tot)])]).

v(smeul,smeult,smeulen,gesmeuld,smeulde,smeulden,
    [h([intransitive,
	part_intransitive(na)])]).

v(smijt,smijt,smijten,gesmeten,smeet,smeten,
    [h([nonp_pred_np,
	intransitive,
	np_ld_dir,
	transitive,
	np_ld_pp,
	pc_pp(met)])]).

v(smoes,smoest,smoezen,gesmoesd,smoesde,smoesden,
    [h([intransitive,
	transitive])]).

v(smokkel,smokkelt,smokkelen,gesmokkeld,smokkelde,smokkelden,
    [h([intransitive,
	np_ld_dir,
	np_ld_pp,
	part_transitive(binnen),
	transitive])]).

v(smoor,smoort,smoren,gesmoord,smoorde,smoorden,
    [unacc([intransitive]),
     h([transitive,
        fixed([[in,de,kiem],acc],norm_passive)])]).

v(sms,[smst,'sms-t','sms\'t'],['sms-en','sms\'en',smsen],
  [gesmst,'ge-sms-t','gesms-t'],[smste,'sms-te','sms\'te'],
  [smsten,'sms-ten','sms\'ten'],
    [h([intransitive,
	transitive,
	sbar,
	np_sbar])]).

v(smuk,smukt,smukken,gesmukt,smukte,smukten,
    [h([part_transitive(op)])]).

v(smul,smult,smullen,gesmuld,smulde,smulden,
    [h([intransitive,
	pc_pp(aan),
	pc_pp(van)])]).

v(smurf,smurft,smurfen,gesmurft,smurfte,smurften,
    [h([intransitive,
        transitive])]).

v(snack,snackt,snacken,gesnackt,snackte,snackten,
    [h([intransitive])]).

v(snak,snakt,snakken,gesnakt,snakte,snakten,
    [h([intransitive,
	pc_pp(naar),
	er_pp_sbar(naar),
	er_pp_vp(naar)])]).

v(snap,snapt,snappen,[gesnapt,gesnopen],snapte,snapten,
    [h([sbar,
	transitive,
        intransitive, % snap je?
	np_er_pp_sbar(van),
	np_pc_pp(van)])]).

v(snater,snatert,snateren,gesnaterd,snaterde,snaterden,
    [h([intransitive])]).

v(snauw,snauwt,snauwen,gesnauwd,snauwde,snauwden,
    [h([intransitive,
	sbar,
	transitive,
        part_transitive(af),
	part_np_np(toe),
	part_np_sbar(toe),
	pc_pp(tegen)])]).

v(sneef,sneeft,sneven,gesneefd,sneefde,sneefden,
    [h([intransitive])]).  

v(sneer,sneert,sneren,gesneerd,sneerde,sneerden,
    [h([sbar,
	intransitive])]).  

v(sneeuw,sneeuwt,sneeuwen,gesneeuwd,sneeuwde,sneeuwden,
    [h([het_subj,
	part_transitive(onder),
	fixed([het_subj,acc],no_passive)]),
     z([part_intransitive(binnen),
        part_intransitive(dicht),
	part_intransitive(in)]),
     b([part_intransitive(onder)])]).

v(snel,snelt,snellen,gesneld,snelde,snelden,
    [z([intransitive,
	ld_dir,
	ld_pp,
	refl,
	refl_ld_dir,
	refl_ld_pp,
	part_intransitive(heen),
	part_transitive(voorbij),
        fixed([[te,hulp]],no_passive),
        fixed([[te,hulp],dat],no_passive),
	part_intransitive(aan),
	part_intransitive(toe),
	part_ld_pp(toe)])]).

v(snelwandel,snelwandelt,snelwandelen,gesnelwandeld,snelwandelde,snelwandelden,
    [h([intransitive])]).

v(snerp,snerpt,snerpen,gesnerpt,snerpte,snerpten,
    [h([intransitive,
	sbar,
	transitive])]).

v(sneuvel,sneuvelt,sneuvelen,gesneuveld,sneuvelde,sneuvelden,
    [unacc([intransitive])]).

v([snij,snijd],snijdt,snijden,gesneden,sneed,sneden,
    [h([intransitive,
	ld_pp,
	ld_adv,
	transitive,
	np_ld_pp,
	nonp_pred_np,
	part_np_np(af),		% iemand de pas afsnijden; but also "om niet door de oprukkende Russen de pas afgesneden te worden . " ????
	part_np_np(door),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(bij),
	part_transitive(door),
	part_transitive(fijn),
	part_transitive(in),
	part_transitive(uit),
	part_transitive(weg),
	pc_pp(op),
        part_np_np(over), % iemand de polsen/keel over snijden
	% refl_ld_pp,
	part_np_ld_pp(af),
	part_np_ld_pp(uit),
	part_np_pc_pp(toe,op)])]).

v(snik,snikt,snikken,gesnikt,snikte,snikten,
    [h([intransitive,
	sbar %dip
       ])]).

%% snipper de ui
v(snipper,snippert,snipperen,gesnipperd,snipperde,snipperden,
    [h([transitive,
	ap_pred_np % ragfijn
       ])]).

v(snoef,snoeft,snoeven,gesnoefd,snoefde,snoefden,
    [h([intransitive,
	sbar])]).

v(snoei,snoeit,snoeien,gesnoeid,snoeide,snoeiden,
    [h([intransitive,
	part_transitive(bij),
	mod_pp(in),
	transitive])]).

v(snoep,snoept,snoepen,gesnoept,snoepte,snoepten,
    [h([intransitive,
	transitive,
	part_transitive(af),
	part_transitive(weg),
	pc_pp(van)])]).

v(snoer,snoert,snoeren,gesnoerd,snoerde,snoerden,
    [h([transitive,
	part_transitive(aan),
	fixed([[de,mond],dat],imp_passive)])]).

v(snor,snort,snorren,gesnord,snorde,snorden,
    [z([ld_pp]),
     h([intransitive,
	transitive])]).

v(snotter,snottert,snotteren,gesnotterd,snotterde,snotterden,
    [h([intransitive])]).

v(snuf,snuft,snuffen,gesnuft,snufte,snuften,
    [h([intransitive])]).

v(snuffel,snuffelt,snuffelen,gesnuffeld,snuffelde,snuffelden,
    [h([intransitive,
        part_transitive(af),
	part_transitive(door),
	ld_pp,
	ld_adv])]).

v(snuif,snuift,snuiven,gesnoven,snoof,snoven,
    [h([intransitive,
	transitive,
	sbar, % dip
	part_transitive(op),
	pc_pp(aan)])]).

v(snuit,snuit,snuiten,gesnoten,snoot,snoten,
    [h([np_np,
	transitive])]).

v(snurk,snurkt,snurken,gesnurkt,snurkte,snurkten,
    [h([intransitive])]).

v(socialiseer,socialiseert,socialiseren,gesocialiseerd,socialiseerde,socialiseerden,
    [z([intransitive]),
     h([transitive])]).

v(sodemieter,sodemietert,sodemieteren,gesodemieterd,sodemieterde,sodemieterden,
    [z([intransitive,
	part_intransitive(op),
	ld_dir,
	ld_pp]),
     h([np_ld_dir,
	transitive,
	np_ld_pp])]).

v(soebat,soebat,soebatten,gesoebat,soebatte,soebatten,
    [h([intransitive])]).

v(soes,soest,soezen,gesoesd,soesde,soesden,
    [h([intransitive])]).

v(soigneer,soigneert,soigneren,gesoigneerd,soigneerde,soigneerden,
    [h([transitive,
        intransitive])]).

v(sol,solt,sollen,gesold,solde,solden,
    [h([intransitive,
	transitive,
	pc_pp(met)])]).

v(soldeer,soldeert,solderen,gesoldeerd,soldeerde,soldeerden,
    [h([intransitive,
	transitive])]).

v(soleer,soleert,soleren,gesoleerd,soleerde,soleerden,
    [h([intransitive])]).

v(solliciteer,solliciteert,solliciteren,gesolliciteerd,solliciteerde,solliciteerden,
  [h([intransitive,
      transitive, % VL We worden druk gesolliciteerd
	pc_pp(op),
	pc_pp(naar)])]).

v(som,somt,sommen,gesomd,somde,somden,
    [h([part_sbar(op),
	part_transitive(op),
        part_intransitive(op),
        part_dip_sbar(op)])]).

v(somber,sombert,somberen,gesomberd,somberde,somberden,
    [h([intransitive,
	sbar])]).

v(sommeer,sommeert,sommeren,gesommeerd,sommeerde,sommeerden,
    [h([np_vp_obj1,
	np_vp_obj,
	np_ld_pp,
	np_ld_dir,
	transitive,
	intransitive])]).

v(sondeer,sondeert,sonderen,gesondeerd,sondeerde,sondeerden,
    [h([intransitive])]).

v(sop,sopt,soppen,gesopt,sopte,sopten,
    [h([intransitive,
	transitive])]).

v(sorteer,sorteert,sorteren,gesorteerd,sorteerde,sorteerden,
  [h([transitive,
      intransitive,
      np_pc_pp(op)])]).

v(soupeer,soupeert,souperen,gesoupeerd,soupeerde,soupeerden,
    [h([intransitive,
	part_transitive(op)])]).

v(spaar,spaart,sparen,gespaard,spaarde,spaarden,
    [h([np_np,
	intransitive,
	transitive,
	part_transitive(op),
	part_transitive(uit),
	part_pc_pp(uit,op),
	part_np_pc_pp(uit,op),
	np_pc_pp(van),  % hij bleef er van/voor gespaard
	np_pc_pp(voor),
	pc_pp(voor)])]).

v(space,spacet,spacen,gespacet,spacete,spaceten,
    [h([intransitive,
	transitive])]).

v(spam,spamt,spammen,gespamd,spamde,spamden,
    [h([intransitive,
	part_transitive(vol)])]).

v(span,spant,spannen,gespannen,spande,spanden,
    [unacc([part_intransitive(aan)]),
     h([intransitive,
	transitive,
	np_ld_pp,
	pc_pp(om), % het spant erom
	er_pp_sbar(om),		% het spant erom of hij komt
	fixed([[de,kroon]],no_passive),
	part_refl(in),
	part_refl_vp(in),
	part_transitive(aan),
	part_np_mod_pp(aan,over), % een rechtzaak/.. aanspannen over
	part_transitive(in),
	part_intransitive(in),
	part_np_pc_pp(aan,tegen),
	part_np_pc_pp(in,tegen),
	part_intransitive(samen),
	part_pc_pp(samen,met),
	part_pc_pp(samen,tegen),
	part_refl_pc_pp(in,voor),
	part_refl_er_pp_sbar(in,voor)])]).

v(spar,spart,sparren,gespard,sparde,sparden,
    [h([intransitive])]).  

v(spartel,spartelt,spartelen,gesparteld,spartelde,spartelden,
    [h([intransitive,
	part_intransitive(tegen),
	ld_pp,
	ld_adv])]).

v(spat,spat,spatten,gespat,spatte,spatten,
    [z([ld_pp,
        part_intransitive(af),
	part_intransitive(op),
	so_np_ld_pp,  % het zeepsop spat haar in het gezicht
        ap_copula('uit elkaar'),
        fixed([ap_pred('uit elkaar'),pc(in)],no_passive),
	part_intransitive(uiteen),
        part_pc_pp(uiteen,in)]),
     h([pred_np,        % ik spat jou nat
	pc_pp(met)]),
     b([intransitive,
	part_pc_pp(af,van)])]).

v(spatel,spatelt,spatelen,gespateld,spatelde,spatelden,
    [h([np_ld_pp,
        part_transitive(door)])]).

v(specialiseer,specialiseert,specialiseren,gespecialiseerd,specialiseerde,specialiseerden,
    [h([transitive,
        pc_pp(in),
	refl_pc_pp(in)])]).

v(specificeer,specificeert,specificeren,gespecificeerd,specificeerde,specificeerden,
    [h([transitive])]).

v(speculeer,speculeert,speculeren,gespeculeerd,speculeerde,speculeerden,
    [h([intransitive,
	sbar,
	er_pp_sbar(op),
	er_pp_sbar(over),
	pc_pp(in),
	pc_pp(op),
	pc_pp(over)])]).

v(speech,speecht,speechen,gespeecht,speechte,speechten,
  [h([intransitive])]).

v(speel,speelt,spelen,gespeeld,speelde,speelden,
    [unacc([copula_np(kwijt)]),
     h([intransitive,		% hij speelt heel lief
	transitive,             % we spelen tikkertje
	np_np_ld_pp,            % ik speel hem een boek in handen
	np_ld_pp,               % ik speel de bal de hoek in / de pannen van het dak
	ap_pred_np,             % zoek spelen; kapot spelen
	sbar_subj_no_het,       % hierbij speelt ook , dat ...
 	ld_pp,			% de film speelt in Venetie; (why is passive ruled out?)
	ld_adv,                 % de film speelt boven
	pc_pp(in),              % hij speelt in de film
	np_pc_pp(in),           % hij speelt Hamlet in Hamlet
	pc_pp(met),             % het idee waar hij mee speelde; iets om mee te spelen
	pc_pp(op),              % we spelen op buitenspel
	pc_pp(tegen),           % de ploeg waar we tegen speelden
	pc_pp(voor),            % hij speelt voor directeur
	sbar,                   % hij speelde dat ...
	part_np_np(toe),
	part_so_pp_np(toe),
	part_intransitive(af),
	part_intransitive(gelijk),
	part_intransitive(in),
	part_intransitive(kaart),
        part_intransitive(mee),
	part_intransitive(op),
	part_intransitive(over),  % speel nu eens over!
	part_intransitive(samen),
        part_intransitive(terug),
	part_intransitive(toneel),
	part_intransitive(voor),
        part_intransitive(vooruit),
	part_refl(af),
	part_refl_ld_pp(af),
	part_so_pp_np(door),
	part_transitive(aan),
	part_np_np(aan),   % ik heb hem geen goeie ballen aangespeeld
	part_transitive(af),
	part_transitive(door),
	part_transitive(gelijk),
	part_transitive(in),
	part_transitive(klaar),
	part_transitive(mee),
	part_sbar_subj_no_het(mee),  % er speelt mee dat ..
	part_transitive(na),
	part_transitive(op),	% iets hoog opspelen
	part_transitive(over),
	part_transitive(samen),
        part_transitive(terug),
	part_transitive(uit),
	part_transitive(vol),  % we spelen de tijd vol
	part_transitive(voor),
        part_transitive(vooruit),
        part_transitive(vrij),
	part_transitive(weg), % Feyenoord heeft Ajax helemaal weggespeeld
        fixed([[in,de,kaart],dat],no_passive),
        fixed([[in,de,kaart],pc(van)],no_passive),
	fixed([pp_pred(in,hand),{[acc,dat_pp(aan)]}],norm_passive),
	fixed([pp_pred(in,hand),{[acc,dat]}],norm_passive),
	fixed([pp_pred(in,hand),{[acc,pc(van)]}],norm_passive),
	fixed([[parten],dat],no_passive),
	fixed([[geen,parten],dat],no_passive),
	fixed([[quitte]],imp_passive),
	fixed([sbar_subj_opt_het,acc(rol)],no_passive),
	fixed([sbar_subj_opt_het,{[acc(rol),mod_pp(bij)]}],no_passive),
        fixed([{[acc(rol),mod_pp(bij)]}],no_passive),
	part_np_ld_pp(af),
	part_np_ld_pp(door),
	part_np_np(door), % (aan) iemand iets doorspelen
	part_np_pc_pp(door,aan),
	part_np_pc_pp(mee,met),
	part_np_pc_pp(uit,tegen),
	part_pc_pp(gelijk,tegen),
	part_pc_pp(in,op),
	part_pc_pp(mee,in),
	part_pc_pp(mee,met),
	part_pc_pp(mee,tegen)])]).

v(speen,speent,spenen,gespeend,speende,speenden,
    [h([transitive,
	np_pc_pp(van),
	pc_pp(van)])]).

v(spek,spekt,spekken,gespekt,spekte,spekten,
    [h([transitive])]).

v(spekuleer,spekuleert,spekuleren,gespekuleerd,spekuleerde,spekuleerden,
    [h([intransitive,
	pc_pp(in),
	pc_pp(op),
	pc_pp(over)])]).

v(spel,spelt,spellen,gespeld,spelde,spelden,
    [h([intransitive,
	transitive])]).

v(speld,speldt,spelden,gespeld,speldde,speldden,
    [h([np_np,
        fixed([[op,de,mouw],{[acc,dat]}],norm_passive),
        fixed([[op,de,mouw],acc,sbar],imp_passive),
	part_transitive(op),
	part_np_np(op),
	transitive])]).

v(spendeer,spendeert,spenderen,gespendeerd,spendeerde,spendeerden,
    [h([transitive,
	np_pc_pp(aan)])]).

v(sper,spert,sperren,gesperd,sperde,sperden,
    [h([transitive,
	part_transitive(open)])]).

v(spetter,spettert,spetteren,gespetterd,spetterde,spetterden,
  [h([intransitive]),
   b([part_pc_pp(af,van)])]).

v(speur,speurt,speuren,gespeurd,speurde,speurden,
  [h([intransitive,
      transitive,
      part_transitive(af),
      part_transitive(op),
      part_np_pc_pp(af,op),
      pc_pp(naar)])]).

v(spied,spiedt,spieden,gespied,spiedde,spiedden,
  [h([intransitive,
      ld_dir,
      ld_pp
     ])]).

v(spiegel,spiegelt,spiegelen,gespiegeld,spiegelde,spiegelden,
    [h([intransitive,
	refl,
	transitive,
	part_np_np(voor),
	part_np_sbar(voor),
	part_np_vp_obj(voor),
	part_sbar(voor),
	part_transitive(voor),
	part_vp(voor),
	refl_pc_pp(aan)])]).

v(spiek,spiekt,spieken,gespiekt,spiekte,spiekten,
    [h([intransitive])]).

v(spiets,spietst,spietsen,gespietst,spietste,spietsten,
    [h([transitive])]).

v(spijbel,spijbelt,spijbelen,gespijbeld,spijbelde,spijbelden,
    [h([intransitive])]).

v(spijker,spijkert,spijkeren,gespijkerd,spijkerde,spijkerden,
    [h([ap_pred_np,
	intransitive,
        part_transitive(aan),
	part_transitive(bij),
	part_np_np(bij),
	np_ld_pp,
	np_ld_pp])]).

v(spijs,spijst,spijzen,gespijsd,spijsde,spijsden,
    [h([transitive])]).

v(spijt,spijt,spijten,gespeten,speet,speten,
    [h([sbar_subj_so_np,
	so_np,
	vp_subj_so_np])]).

v(spin,spint,spinnen,[gesponnen,gespind],[spon,spinde],[sponnen,spinden],
    [h([intransitive,
	transitive,
	fixed([{[acc(garen),pc(bij)]}],imp_passive),
	part_transitive(uit),
	part_transitive(in)])]).

v(spioneer,spioneert,spioneren,gespioneerd,spioneerde,spioneerden,
    [h([intransitive,
	pc_pp(bij),
	pc_pp(voor)])]).

v(spit,spit,spitten,gespit,spitte,spitten,
    [h([intransitive,
	transitive,
        part_transitive(door),
	part_transitive(om),
	part_transitive(uit)])]).

v(spits,spitst,spitsen,gespitst,spitste,spitsten,
    [h([transitive,
	part_transitive(toe),
	refl_pc_pp(op),
	part_np_pc_pp(toe,op),
	part_refl_pc_pp(toe,op)])]).

v(splijt,splijt,splijten,gespleten,spleet,spleten,
    [unacc([intransitive]),
     h([transitive])]).

v(split,split,splitten,gesplit,splitte,splitten,
    [b([intransitive])]).

v(splits,splitst,splitsen,gesplitst,splitste,splitsten,
  [h([refl,
      refl_pc_pp(in),
      transitive,
      intransitive,
      fixed([[in,de,maag],{[acc,dat]}],norm_passive),
      part_transitive(af),
      part_transitive(in),
      part_transitive(op),
      part_transitive(uit),
      np_pc_pp(in),
      part_np_pc_pp(uit,in),
      part_np_pc_pp(uit,naar),
      part_np_pc_pp(op,in)])]).

v(spoed,spoedt,spoeden,gespoed,spoedde,spoedden,
    [z([intransitive]),
     h([refl,
	refl_ld_dir,
	refl_ld_pp])]).

v(spoel,spoelt,spoelen,gespoeld,spoelde,spoelden,
    [z([ld_dir,
	ld_pp,
	part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(uit),
	part_intransitive(weg),
	part_ld_pp(aan)]),
     h([np_np,
	np_ld_dir, % de onderstroom spoelt de kwallen het zand op
	intransitive,
	transitive,
	part_transitive(aan),
	part_transitive(af),
	part_transitive(door),
	part_transitive(om),
	part_transitive(uit),
	part_intransitive(terug),
	part_transitive(terug),
	part_intransitive(vooruit),
	part_transitive(vooruit),
	part_transitive(weg),
	part_np_ld_pp(af)])]).

v(spons,sponst,sponzen,gesponsd,sponsde,sponsden,
  [h([part_transitive(af)])]).

v(sponsor,sponsort,sponsoren,gesponsord,sponsorde,sponsorden,
  [h([transitive,
      intransitive])]).

v(spook,spookt,spoken,gespookt,spookte,spookten,
    [h([intransitive,
        part_intransitive(rond),
	part_transitive(uit)])]).

v(spoor,spoort,sporen,gespoord,spoorde,spoorden,
    [z([ld_pp,
	part_transitive(aan),
	part_pc_pp(aan,tot)]),
     h([transitive,
	part_np_vp_obj1(aan),
	part_dip_sbar(aan),
	part_acc_np_dip_sbar(aan),
	part_transitive(op),
        part_intransitive(op),
	pc_pp(met),
	part_np_pc_pp(aan,tot),
	part_er_pp_vp_no_control(aan,tot),
	part_obj_np_er_pp_vp(aan,tot)]),
     b([intransitive])]).

v(sport,sport,sporten,gesport,sportte,sportten,
    [h([intransitive,
	ap_pred_refl])]).

v(spot,spot,spotten,gespot,spotte,spotten,
    [h([intransitive,
	transitive,
	dip_sbar,
	pc_pp(met)])]).

v(sprankel,sprankelt,sprankelen,gesprankeld,sprankelde,sprankelden,
    [h([intransitive])]).

v(spreek,spreekt,spreken,gesproken,sprak,spraken,
    [h([intransitive,
	sbar,
	transitive,
	np_mod_pp(met),
	np_pc_pp(van),
	part_als_pred_np(aan),
	part_np_np(in),  % hij sprak hem moed in
	part_intransitive(aan),
	part_intransitive(af),
        part_intransitive(mee),
	part_sbar_subj_so_np(in),
	part_sbar_subj_so_np(tegen),
	part_sbar(af),
	part_sbar(tegen),
	part_sbar(uit),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(door),
	part_transitive(in),
	part_transitive(tegen),
	part_transitive(toe),
	part_transitive(uit),
	part_transitive(vrij),
	part_sbar_subj_so_np(aan),  % het spreekt me aan dat je komt
	part_vp(af),
%	pc_pp(met),
        mod_pp(met),
	part_np_mod_pp(af,over), % daar is niets over afgesproken
	pc_pp(namens),
	pc_pp(over),
        er_pp_sbar(over),
        er_pp_vp(over),
	np_pc_pp(over),   % we zijn niet te spreken over deze situatie (???)
	pc_pp(tegen),
	pc_pp(tot),
	pc_pp(uit),
	pc_pp(van),
	fixed([{[acc(schande),pc(van)]}],norm_passive),
	fixed([{[acc(schande),er_pp(van,X)]},extra_sbar(X)],norm_passive),
	fixed([[te,na],dat],imp_passive),
	fixed([sbar_subj,[vanzelf]],no_passive),
	fixed([sbar_subj,pp_refl(voor)],no_passive),
	fixed([sbar_subj,svp_pp(in,voordeel)],no_passive),
	fixed([sbar_subj,[boekdelen]],no_passive),
	part_fixed(mee,[acc(woord_DIM)],norm_passive),
	part_np_er_pp_sbar(uit,over),
	part_np_pc_pp(aan,met),
	part_np_pc_pp(aan,op),
	part_np_er_pp_sbar(aan,op),
	part_np_pc_pp(aan,over),
	part_np_pc_pp(uit,over),
	part_np_pc_pp(vrij,van),
	part_pp_sbar(af,met),  % ?
	part_pc_pp(af,met),
	part_np_pc_pp(af,met), % ?
	part_pc_pp(mee,in),
	part_pc_pp(mee,over),
	part_pp_vp(af,met),    % ?
        part_refl(uit),
	part_refl_pc_pp(uit,over),
	part_refl_pc_pp(uit,tegen),
	part_refl_pc_pp(uit,voor)]),
     b([part_intransitive(in),
	part_intransitive(uit)   % ouderwets: nadat hij uitgesproken had
       ])]).

v(spreid,spreidt,spreiden,gespreid,spreidde,spreidden,
    [h([transitive,
	np_ld_pp,
	part_transitive(tentoon),
	part_transitive(uit),
	fixed([[ten,toon],acc],norm_passive),
	part_np_ld_pp(uit)])]).

v(sprenkel,sprenkelt,sprenkelen,gesprenkeld,sprenkelde,sprenkelden,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_pc_pp(met),
	pc_pp(met)])]).

v(spring,springt,springen,gesprongen,sprong,sprongen,
    [z([ld_dir,
	transitive,
	part_intransitive(aan),
	part_intransitive(bij),
	part_intransitive(binnen),
	part_intransitive(in),
	part_intransitive(op),
	part_intransitive(open),
	part_intransitive(over),
	part_intransitive(uit),
	part_intransitive(weg),
	part_ld_pp(in),
	part_ld_pp(op),
	part_ld_pp(over),
	part_ld_pp(terug),
	part_so_np(bij),
	part_transitive(na),
	part_transitive(om),
	part_transitive(over),
	ap_copula('in het oog'),
        fixed([svp_pp(op,tilt)],no_passive),
	fixed([sbar_subj,ap_pred('in het oog')],no_passive),
	fixed([[te,hulp]],no_passive),
	fixed([[te,hulp],dat],no_passive),
	part_pc_pp(in,op),
	part_pc_pp(op,van)]),
     h([part_intransitive(bij)]),
     b([ld_pp,
	ld_adv,
	part_pc_pp(om,met),  % VL hadden omgesprongen met
	intransitive])]).

v(sprint,sprint,sprinten,gesprint,sprintte,sprintten,
  [h([intransitive
     ]),
   b([pred_refl,                % VL hij sprintte zich derde
      refl_ld_pp                % VL hij sprintte zich naar de tweede plek
     ]),
   z([ld_pp,
      ld_dir,			% het veld af; de bocht om
      ld_adv,			% cdb 3637: hij sprintte binnen
      part_so_np(voorbij),
      part_intransitive(aan)
     ])]).

v(sproei,sproeit,sproeien,gesproeid,sproeide,sproeiden,
    [h([intransitive,
	transitive])]).

v(sprokkel,sprokkelt,sprokkelen,gesprokkeld,sprokkelde,sprokkelden,
    [h([transitive,
        np_pc_pp(bij),
	part_transitive(bij),
	part_transitive(bijeen)])]).

v(spruit,spruit,spruiten,gesproten,sproot,sproten,
    [unacc([intransitive,
            pc_pp(uit),
            part_pc_pp(voort,uit)])]).

v(spui,spuit,spuien,gespuid,spuide,spuiden,
    [h([intransitive,
	transitive])]).

v(spuit,spuit,spuiten,gespoten,spoot,spoten,
    [z([ld_pp,
        ld_dir,
	part_intransitive(op),
	part_transitive(op)]),
     h([transitive,
	np_ld_pp,
	np_ld_dir,
	np_np_ld_pp,
	part_ld_pp(in),
	part_np_ld_pp(in),
	part_transitive(in),
        part_transitive(uit)]),
     b([ap_pred_np,
	intransitive,
	part_so_np(in)])]).

v(spurt,spurt,spurten,gespurt,spurtte,spurtten,
    [h([intransitive,
        refl,
	ld_dir,
	pred_refl,  % VL hij spurtte zich derde
        refl_ld_pp
       ])]).

v(sputter,sputtert,sputteren,gesputterd,sputterde,sputterden,
    [h([intransitive,
	part_intransitive(tegen),
	part_intransitive(na),
	part_sbar(tegen),
	pc_pp(tegen)])]).

v(spuug,spuugt,spugen,[gespuugd,gespogen],[spuugde,spoog],[spuugden,spogen],
    [h([np_np,
	intransitive,
	transitive,
	part_transitive(uit),
	ld_pp])]).

v(spuw,spuwt,spuwen,gespuwd,spuwde,spuwden,
    [h([intransitive,
	transitive,
	np_ld_dir,  % we spuwen boodschappen de wereld in
	part_transitive(uit)])]).

v(squash,squasht,squashen,gesquasht,squashte,squashten,
    [h([intransitive])]).

v(sta,staat,inflected(staan,stane),gestaan,stond,stonden,
    [z([part_intransitive(op),
	part_intransitive(recht),
	part_ld_pp(op)]),
     h([intransitive,
	ld_adv_sbar_subj_no_het,
	ld_adv,
	ld_pp_sbar_subj_no_het,
	ld_pp,
        op,               % de dam staat op doorbreken
	so_np_ld_pp,      % het water staat ons tot de lippen
	so_np,            % die kleding staat mij niet/wel
	ld_dir,           % de bladeren staan recht/schuin omhoog
	te_passive,       % staat te bezien, ..
	norm_passive,     % omdat het beeld prachtig stond opgesteld
	sbar_passive,     % omdat er staat geschreven dat ...
	nonp_copula,      % de versieringen staan prachtig, je staat buiten spel
        num_pred,         % het staat 3-0
	part_num_pred(voor),   % we staan 3-0 voor
	part_num_pred(achter),
	fixed([np_pred(nummer)],no_passive),  % hij staat nummer 1
	nonp_copula_sbar, % het staat buiten kijf dat hij komt
	nonp_copula_vp,   % het staat goed om ..
	so_nonp_copula,   % die kleding staat mij goed
	alsof_sbar,
	np_alsof_sbar,
	np_pc_pp(naar),
	part_sbar_obj(uit), % ik kan het niet uitstaan dat je komt
	part_sbar(uit),     % ik kan niet uitstaan dat je komt    
	dip_sbar_subj_no_het, % er stond dat je moest betalen
	fixed([{[[symbool],pc(voor)]}],no_passive),
	fixed([er_pp(voor),ap_pred],no_passive),  % hij staat er niet goed voor
	pc_pp(naar),
	pc_pp(op),
        fixed([svp_pp(op,naam)],no_passive),              % de woning staat op zijn naam
        fixed([{[svp_pp(op,naam),pc(van)]}],no_passive),  % de woning staat op naam van ...
	er_pp_sbar(op),
	er_pp_vp(op),
	fixed([vc(kijk,te,intransitive),er_pp(van,C),extra_sbar(C)],no_passive),
	pc_pp(tegenover),
	fixed([pc(tegenover),sbar_subj_no_het],no_passive),
	fixed([pc(tegenover),ap_pred],no_passive),
	fixed([er_pp(tegenover,C),ap_pred,extra_sbar(C)],no_passive),
	fixed([er_pp(tegenover,C),ap_pred,extra_vp(C)],no_passive),
	pc_pp(tot),
	pc_pp(voor),
        subj_control(wk_te),    % dat wij staan te lummelen
	subj_control(pass_te),  % dat ons de problemen te wachten staan

        fixed([[tot,aan,de,lippen],dat,subj(water)],no_passive),
        fixed([[aan,de,lippen],dat,subj(water)],no_passive),
        fixed([[tot,aan,de,lippen],subj(water)],no_passive),
        fixed([[aan,de,lippen],subj(water)],no_passive),
	fixed([[blank]],no_passive),  % de vloer
	fixed([{[pc(van),[ten,dienste]]}],no_passive),
	fixed([ap_svp('aan het hoofd')],no_passive),

        part_vp_obj(toe),
        part_so_np_vp_obj(toe),
        part_sbar_obj(toe),
	part_sbar(toe),
        part_so_np_sbar_obj(toe),
        
	part_np_np(af),
	part_np_np(toe),
	part_intransitive(aan),
	part_intransitive(bij),
        part_ld_er_transitive(bij), % zoals ze er bijstaan
	part_intransitive(in),
	part_intransitive(klaar),
	part_intransitive(leeg),
	part_intransitive(open),
	part_intransitive(stil),
	part_intransitive(tegen),
        part_intransitive(tentoon),
	part_intransitive(terecht),
	part_intransitive(uit),
	part_intransitive(vast),
	part_intransitive(voor),
	part_intransitive(voorop),
	part_intransitive(los),
	part_pc_pp(los,van),
	part_intransitive(gelijk),
	part_pc_pp(gelijk,met),
	part_np_sbar(aan),
	part_np_sbar(toe),
	part_np_vp_obj(toe),
	part_so_vp_obj(toe),  % ik sta aan hem_i toe om PRO_i mij te zoenen
	part_sbar_subj_no_het_tpart(vast),
	part_tpart(voorop),
	part_sbar_subj_no_het_tpart(voorop),
	part_sbar(toe),
	fixed([[overeind],sbar_subj_no_het],no_passive),
	part_sbar_subj(vast),
	part_sbar_subj(voorop),
	part_so_np(aan),
	part_so_np(bij),
	part_sbar_subj_so_np(bij), % het staat me bij dat ...
	%% er staat me iets van bij dat ...
	%% er staat me niets meer bij van deze vergadering
	part_fixed(bij,[{[mod_pp(van),dat]}],no_passive),
	part_fixed(bij,[{[mod_pp(van),dat,sbar]}],no_passive),
	part_so_np(tegen),
	part_so_np(voor),
	part_transitive(af),
	part_transitive(bij),
	part_ld_transitive(op),  % de goede kant op staan (de neuzen bv)
	part_transitive(toe),
	part_transitive(uit),
	part_transitive(voor),
	part_intransitive(vrij), % Ik sta vrij!
	part_so_np(vrij),
	part_vp_subj_so_np(vrij),
	part_als_copula(bekend),
	part_ap_copula(bekend),
        part_er_pp_sbar(bekend,om),
        part_pc_pp(bekend,om),
        part_pc_pp(bekend,onder),
        part_intransitive(bekend),  % vanwege, omdat
	fixed([{[[bol],pc(van)]}],no_passive),
	fixed([{[[borg],pc(voor)]}],no_passive),
	fixed([{[[borg],er_pp(voor,C)]},extra_sbar(C)],no_passive),
	fixed([{[[garant],pc(voor)]}],no_passive),
	fixed([{[[garant],er_pp(voor,C)]},extra_sbar(C)],no_passive),
        fixed([{[pc(met),pp_pred(in,contact)]}],no_passive),
	fixed([{[[in,het,teken],pc(van)]}],no_passive),
	fixed([{[[in,verband],pc(met)]}],no_passive),
        fixed([{[pp_pred(in,verhouding),pc(met)]}],no_passive),
        fixed([{[pp_pred(in,verhouding),pc(tot)]}],no_passive),
	fixed([acc(man_DIM)],no_passive),
	fixed([{[svp_pp(onder,gezag),pc(van)]}],no_passive),
	fixed([{[svp_pp(onder,invloed),pc(van)]}],no_passive),
	fixed([{[svp_pp(onder,leiding),pc(van)]}],no_passive),
	fixed([{[svp_pp(onder,voorzitterschap),pc(van)]}],no_passive),
	fixed([[op,wacht]],no_passive),
	fixed([[blank]],no_passive),
	fixed([[gereed]],no_passive),
	fixed([[gereed],vp],no_passive),
	fixed([[in,vuur,en,vlam]],no_passive),
        fixed([[in,lichterlaaie]],no_passive),
        fixed([[in,lichterlaaien]],no_passive),
	fixed([[in,de,weg],dat],no_passive),
	fixed([[in,de,weg]],no_passive),
	fixed([[klaar]],no_passive),
	fixed([[klaar],vp],no_passive),
	fixed([[hun,mannetje]],no_passive),
	fixed([[mijn,mannetje]],no_passive),
	fixed([[je,mannetje]],no_passive),
        fixed([np_pred(model)],no_passive),
        fixed([np_pred(model),pc(voor)],no_passive),
	fixed([[op,de,nominatie]],no_passive),
	fixed([[op,de,nominatie],vp],no_passive),
	fixed([[onder,contract]],no_passive),
	fixed([[onder,water]],no_passive),
	fixed([pp_pred(onder,druk),vp],no_passive),
	fixed([{[pc(van),pp_pred(onder,bevel)]}],no_passive),
	fixed([ap_pred('op de been')],no_passive),
	fixed([[op,gespannen,voet]],no_passive),
	fixed([[op,gespannen,voet],pc(met)],no_passive),
	fixed([[op,punt],vp],no_passive),
        fixed([[op,punt]],no_passive),
	fixed([[op,het,punt],vp],no_passive),
	fixed([[op,punt],pc(van)],no_passive),
	fixed([[op,het,punt],pc(van)],no_passive),
	fixed([[op,het,spel]],no_passive),
	fixed([[op,zichzelf]],no_passive),
        fixed([[op,het,getouw]],no_passive),
	fixed([[op,losse,schroeven]],no_passive),
	fixed([[op,stapel]],no_passive),
	fixed([[paf]],no_passive),
	fixed([[paf],mod_pp(van)],no_passive),
	fixed([[perplex]],no_passive),
	fixed([[perplex],mod_pp(van)],no_passive),
	fixed([[roodgloeiend]],no_passive),  % de telefoon
        fixed([vc(schrijf,psp,intransitive),sbar_subj,[in,de,sterren]],
              no_passive),
	fixed([[op,standby]],no_passive),
	fixed([[op,stand,by]],no_passive),
	fixed([[op,'stand-by']],no_passive),
	fixed([{[[te,boek],als_pred]}],no_passive),
	fixed([[te,boek]],no_passive),
	fixed([[te,schande]],no_passive),
        fixed([vc(trappel,te,intransitive),vp],no_passive),
	fixed([[te,woord],acc],norm_passive),
	fixed([[ten,dienste],dat],no_passive),
	fixed([[ter,discussie]],no_passive),
	fixed([[ter,discussie],sbar_subj],no_passive),
	fixed([[terzijde],acc],norm_passive),
	fixed([[voor,de,deur]],no_passive),
	fixed([[voor,ogen],dat],no_passive),
	fixed([[voor,ogen],dat,sbar_subj_no_het],no_passive),
	fixed([[voor,ogen],dat,vp_subj_no_het],no_passive),
	fixed([[voor,schut]],no_passive),
	part_pc_pp(bloot,aan),
	part_pc_pp(in,voor),
	part_er_pp_sbar(in,voor),  % ik sta er voor in dat hij komt
	part_pc_pp(klaar,met),
	part_pc_pp(klaar,voor),
	part_er_pp_sbar(klaar,voor),
	part_er_pp_vp(klaar,voor),
	part_pc_pp(open,voor),
	part_er_pp_sbar(open,voor),
	part_er_pp_vp(open,voor),
	part_pc_pp(stil,bij),
        part_er_pp_sbar(stil,bij),
	part_pc_pp(stil,in),
	part_pc_pp(terecht,voor),
	part_np_pc_pp(voor,op),  % hij laat zich voorstaan op
	part_np_er_pp_sbar(voor,op),  % hij laat zich voorstaan op
	part_np_er_pp_vp(voor,op),  % hij laat zich voorstaan op
                                    % or svp with laat?
	part_pc_pp(uit,tegen),
%	part_intransitive(vol),   confusion with "dat volstaat"
	part_pc_pp(vol,met)]),
     b([part_np_np(af),
	part_so_pp_np(af)])]).

v(staaf,staaft,staven,gestaafd,staafde,staafden,
    [h([transitive])]).

v(staak,staakt,staken,gestaakt,staakte,staakten,
    [h([intransitive,
	transitive])]).

v(staar,staart,staren,gestaard,staarde,staarden,
    [h([intransitive,
	part_transitive(aan),
	part_transitive(af),
	part_transitive(na),
	so_np_ld_pp,  % hij staarde haar in de ogen
	ld_pp,
	ld_dir,  % ze staarde omlaag / het raam uit / de duisternis in
        part_pc_pp_refl(uit,voor),
	part_refl(blind),
	part_refl_pc_pp(blind,op),
        fixed([ap_pred(blind),refl],no_passive),
        fixed([{[ap_pred(blind),pc(op)]},refl],no_passive),
				% not always particle, word order:
				% dat ik me me daar blind op staar
	                        % predc: je moet je niet *te* blind staren daarop
	part_np_pc_pp(aan,met)])]).

v(stabiliseer,stabiliseert,stabiliseren,gestabiliseerd,stabiliseerde,stabiliseerden,
    [h([refl,
	sbar_subj_so_np,
	transitive]),
     unacc([intransitive])]).

v(stagneer,stagneert,stagneren,gestagneerd,stagneerde,stagneerden,
    [unacc([intransitive,
	    transitive  % dat stagneert de gang naar de sporthal
	   ])]).

v(stal,stalt,stallen,gestald,stalde,stalden,
    [h([intransitive,
	transitive,
	part_transitive(uit)])]).

v(stalk,stalkt,stalken,gestalkt,stalkte,stalkten,
    [h([intransitive,
	transitive])]).

v(stam,stamt,stammen,gestamd,stamde,stamden,
    [unacc([pc_pp(uit),
            pc_pp(van),
            part_pc_pp(af,van)])]).

v(stamel,stamelt,stamelen,gestameld,stamelde,stamelden,
    [h([intransitive,
	sbar,
	transitive,
	vp])]).

v(stamp,stampt,stampen,gestampt,stampte,stampten,
    [h([intransitive,
	transitive,
	np_ld_dir,
	fixed([[uit,de,grond],acc],norm_passive),
	part_transitive(aan),
	part_transitive(samen),
	ld_pp,
	ld_adv,
	np_ld_pp])]).

v(stampvoet,stampvoet,stampvoeten,gestampvoet,stampvoette,stampvoetten,
    [h([intransitive])]).

v(standaardiseer,standaardiseert,standaardiseren,gestandaardiseerd,standaardiseerde,standaardiseerden,
    [h([transitive])]).

v(stang,stangt,stangen,gestangd,stangde,stangden,
    [h([intransitive,
	transitive])]).

v(stap,stapt,stappen,gestapt,stapte,stapten,
    [z([ld_dir,
	ld_pp,
	part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(binnen),
	part_intransitive(in),
	part_intransitive(op),
	part_intransitive(over),
        part_intransitive(toe),
	part_intransitive(uit),
	part_pc_pp(af,van),
	part_ld_pp(binnen),
	part_ld_pp(in),
	part_ld_pp(op),
	part_ld_pp(over),
	part_ld_pp(uit),
	part_ld_pp(af),
	part_ld_pp(op),
	part_ld_pp(over)]),
     b([intransitive])]).

v(stapel,stapelt,stapelen,gestapeld,stapelde,stapelden,
    [h([transitive,
	np_ld_pp,
	part_refl(op),
	part_transitive(op)])]).

v(start,start,starten,gestart,startte,startten,
    [z([intransitive,
	pc_pp(met),
	ld_pp,
	part_intransitive(op)]),
     b([transitive]),
     h([part_transitive(op)])]).

v(stationeer,stationeert,stationeren,gestationeerd,stationeerde,stationeerden,
    [z([intransitive]),
     h([transitive,
	np_ld_pp])]).

v(steek,steekt,steken,gestoken,stak,staken,
    [z([part_intransitive(op),
	part_intransitive(over),
	part_ld_pp(over),
        fixed([[van,wal]],no_passive)
       ]),
     h([ap_pred_np,
        pp_pred_np(in,brand),
	intransitive,
	sbar_subj,
	sbar_subj_so_np,
	transitive,
	vp_subj,
	vp_subj_so_np,
	np_ld_dir,
	ld_pp,
	ld_dir,
	ld_adv,
	np_ld_pp,
	np_ld_adv,
	np_np_ld_pp, % ik steek hem een dolk in de rug
	part_np_np(toe),
	part_np_np(uit),
	part_intransitive(in),
	part_intransitive(toe),
	part_intransitive(uit),
	part_transitive(aan),
	part_transitive(af),
        part_transitive(bij),
	part_transitive(dood),
	part_transitive(door),
	part_transitive(in),
	part_transitive(neer),
	part_transitive(op),
	part_transitive(over),
	part_transitive(toe),
	part_transitive(weg),	% VL
	part_sbar(weg),
	part_pc_pp(toe,aan), % VL maar we willen er ook niet aan toesteken .
	part_transitive(uit),
	pc_pp(naar),
	% refl_pc_pp(in),
	fixed([[in,elkaar]],no_passive),
	fixed([[inelkaar]],no_passive),
	fixed([[in,het,nieuw],acc],norm_passive),
	fixed([[onder,de,riem],[een,hart],dat],imp_passive),
	fixed([[onder,stoelen,of,banken],acc],norm_passive),
	fixed([[onder,stoelen,of,banken],het_pobj1(sbar)],imp_passive),
	fixed([[onder,stoelen,of,banken],het_pobj1(vp)],imp_passive),
	fixed([[onder,stoelen,of,banken],sbar],imp_passive),
	fixed([[onder,stoelen,of,banken],vp],imp_passive),
	fixed([[in,eigen,boezem],[de,hand]],no_passive),
	fixed([[de,draak],pc(met)],imp_passive),
	fixed([[de,loftrompet],pc(over)],imp_passive),
	fixed([[de,loftrompet]],imp_passive),
	fixed([{[[een,stokje],pc(voor)]}],imp_passive),
	fixed([[in,het,vuur],{[acc(hand),pc(voor)]}],norm_passive),
	fixed([[in,het,vuur],{[acc(hand),er_pp(voor,S)]},extra_sbar(S)],norm_passive),
	part_fixed(af,[[de,loef],dat],imp_passive),
        part_fixed(bij,[[een,tandje]],imp_passive),  % Vlaams
        part_fixed(op,[[de,kop]],no_passive),
        part_fixed(op,[{[acc,pc(van)]}],no_passive),
        fixed([[in,een,wespennest],refl],no_passive),
	part_ld_pp(uit),
	part_np_ld_pp(uit),
	part_transitive(weg),
	part_sbar(weg),
	part_np_pc_pp(aan,met),
	part_np_pc_pp(op,van)]),
     b([part_intransitive(af),
	part_pc_pp(af,bij),
	part_pc_pp(af,tegen),
	part_pc_pp(op,van)])]).

v(steel,steelt,stelen,gestolen,stal,stalen,
    [h([intransitive,
	transitive,
	fixed([[de,show]],norm_passive)
       ])]).

v(steggel,steggelt,steggelen,gesteggeld,steggelde,steggelden,
    [h([intransitive,
        pc_pp(over)])]).

v(steiger,steigert,steigeren,gesteigerd,steigerde,steigerden,
    [h([intransitive])]).

v(stek,stekt,stekken,gestekt,stekte,stekten,
    [h([intransitive,
	transitive])]).

v(stel,stelt,stellen,gesteld,stelde,stelden,
    [h([np_np,    % ons een vraag/voorwaarde
	so_pp_np,
	pred_np,
	sbar,
	refl,     % VL er stelt zich wel een probleem
	transitive,
	np_ld_pp,
	np_pc_pp(op),
        pp_pred_np(in,bedrijf),
	er_pp_sbar(tegenover),
	np_pc_pp(met),
        fixed([als_pred,sbar],imp_passive),
        fixed([als_pred,vp],imp_passive),
	fixed([pc(zonder),het_obj1],no_passive), 
	vp,            % hij stelt niet te kunnen komen
	part_intransitive(gelijk), % Vlaams, sport: gelijkmaker scoren
	part_intransitive(teleur),
	part_nonp_pred_refl(op),
	
	part_transitive(voor),
	part_np_np(voor),
	part_so_pp_np(voor),
	part_np_vp_no_control(voor),
	part_so_vp_no_control(voor),
	part_vp_no_control(voor),
	part_refl_sbar(voor),
	part_refl_np(voor),
	part_refl_pc_pp(voor,aan),
	part_pred_np(voor),
	part_sbar(voor),
	part_np_ld_pp(voor),
	part_fixed(voor,[{[acc,pc(bij)]},refl],no_passive),
	   % daar kan ik me wel iets bij voorstellen
	part_fixed(voor,[{[acc,er_pp(bij,X)]},refl,extra_sbar(X)],no_passive),

	part_pred_np(aan),
	part_refl(aan),
%	part_refl(op),
	part_sbar_subj_so_np(gerust),
	part_sbar_subj_so_np(teleur),
	part_np_sbar(gerust), % ik kan u geruststellen dat ik niet kom
	part_sbar(in),   % je kunt instellen dat hij elk uur een berichtje stuurt
	part_sbar(vast),
	part_sbar(voorop),
	part_transitive(aan),
	part_transitive(achter),
	part_transitive(af),
	part_transitive(bij),
	part_transitive(gelijk),
	part_transitive(gerust),
	part_intransitive(gerust),
	part_transitive(in),
	part_refl(in), % er stelt zich een evenwicht in
	part_transitive(op),
	part_transitive(open),
	part_transitive(samen),
	part_transitive(schadeloos),
	part_transitive(tegenover),
	part_transitive(teleur),
	part_transitive(tentoon),
	part_intransitive(tentoon), % Vlaams
	part_als_pred_np(tentoon),
	part_transitive(terecht),
        part_transitive(tewerk),
	part_intransitive(uit),
	part_transitive(uit),
	part_transitive(vast),
	part_transitive(veilig),
	part_transitive(voorop),
	part_transitive(vrij),
	part_vp(vast),
	fixed([[in,vraag],acc],norm_passive),
	fixed([{[pc(aan),acc(eis)]}],norm_passive),
	part_fixed(in,[{[acc(onderzoek),pc(naar)]}],
		   norm_passive),
	part_fixed(in,[{[acc(onderzoek),er_pp(naar,C)]},extra_sbar(C)],
		   norm_passive),
%	fixed([{[dat,acc(vraag)]}],norm_passive),   subsumed by np_np
%	fixed([{[dat_pp(aan),acc(vraag)]}],norm_passive), subsumed by so_pp_np
	fixed([{[pc(over),acc(vraag)]}],norm_passive),
        
       	fixed([pc(over),acc(vraag),dat],norm_passive),
       	fixed([inv(acc(vraag)),pc(over),dat],norm_passive),
       	fixed([acc(vraag),inv(dat),pc(over)],norm_passive),
        fixed([{[acc(vraag),pc(bij)]},refl],norm_passive),
        
	fixed([{[pc(over),dat_pp(aan),acc(vraag)]}],norm_passive),
	fixed([{[[te,boek],acc]}],norm_passive),
	fixed([{[[te,schrift],acc]}],norm_passive),
	fixed([{[[te,werk],acc]}],norm_passive),
	fixed([{[[tewerk],acc]}],norm_passive),
        part_transitive(tewerk),
	fixed([[ten,dienste]],norm_passive),
	fixed([{[pc(aan),[ten,dienste]]}],norm_passive),
	fixed([{[pc(van),[ten,dienste]]}],norm_passive),
	fixed([[ten,dienste],acc],norm_passive),
	fixed([{[pc(aan),[ten,dienste]]},acc],norm_passive),
	fixed([{[pc(van),[ten,dienste]]},acc],norm_passive),
	fixed([[aan,de,kaak],acc],norm_passive),
	fixed([pp_pred(aan,orde),sbar],norm_passive),  
	fixed([[als,doel],refl,vp],no_passive),
	fixed([{[acc(belang),pc(in)]}],no_passive),
	fixed([{[acc(einde),pc(aan)]}],no_passive),        
	fixed([{[acc(eer),pc(in)]}],no_passive),
	fixed([{[acc(eer),er_pp(in,C)]},extra_sbar(C)],no_passive),
	fixed([{[acc(eer),er_pp(in,C)]},extra_vp(C)],no_passive),
	fixed([[beschikbaar],acc],norm_passive),
	fixed([[beschikbaar],{[acc,dat]}],norm_passive),
	fixed([[beschikbaar],{[acc,dat_pp(aan)]}],norm_passive),
	fixed([[kandidaat],refl],imp_passive),
	fixed([[kandidaat],pc(voor),refl],imp_passive),
	fixed([[in,beschuldiging],acc],norm_passive),
	fixed([pp_pred(in,bewaring),acc],norm_passive),
	fixed([[in,staat,van,beschuldiging],acc],norm_passive),
	fixed([[in,de,gelegenheid],i(acc,A),obj_vp(A)],norm_passive),
	fixed([[in,gebreke],acc],norm_passive),
	fixed([[in,het,gelijk],acc],norm_passive),
	fixed([[in,het,ongelijk],acc],norm_passive),
	fixed([[in,het,werk],acc],norm_passive),
	fixed([[in,het,werk],acc,vp],norm_passive),
	fixed([[in,kennis],{[acc,pc(van)]}],norm_passive),
	fixed([[in,kennis],{[acc,er_pp(van,C)]},extra_sbar(C)],norm_passive),
	fixed([[in,uitzicht],{[acc,dat]}],norm_passive),
        fixed([[op,de,proef],acc],norm_passive),
	fixed([[op,het,standpunt],refl,sbar],no_passive),
	fixed([[op,'non-actief'],acc],norm_passive),  
	fixed([[op,non,actief],acc],norm_passive),  
	fixed([[op,punt],acc],norm_passive),  % Vlaams
	fixed([svp_pp(op,prijs),acc],norm_passive),
	fixed([svp_pp(op,prijs),het_pobj1(sbar)],norm_passive),
	fixed([svp_pp(op,prijs),het_pobj1(vp)],norm_passive),
	fixed([[orde,op,zaken]],imp_passive),
	fixed([{[[paal,en,perk],pc(aan)]}],imp_passive),
	fixed([{[[geen,paal,en,perk],pc(aan)]}],imp_passive),
	fixed([[ten,doel],refl,vp],no_passive),
	fixed([[tot,doel],refl,vp],no_passive),
	fixed([[ter,discussie],acc],norm_passive),
	fixed([[ter,discussie],sbar],norm_passive),
	fixed([[teweer],refl],imp_passive),
	fixed([[teweer],pc(tegen),refl],imp_passive),
	fixed([[te,weer],refl],no_passive),
	fixed([[te,weer],pc(tegen),refl],no_passive),
	fixed([{[[te,werk],acc]}],norm_passive),
	fixed([{[[tewerk],acc]}],norm_passive),
        part_transitive(tewerk),
	fixed([{[acc(prijs),pc(op)]}],norm_passive),
	fixed([er_pp(op,C),acc(prijs),extra_sbar(C)],imp_passive),
	fixed([er_pp(op,C),acc(prijs),extra_vp(C)],imp_passive),
	fixed([{[pc(in),acc(vertrouwen)]}],norm_passive),
	fixed([{[pc(aan),acc(voorwaarde)]}],norm_passive),
        fixed([ap_pred('ter beschikking'),acc],norm_passive),
        fixed([ap_pred('ter beschikking'),{[acc,dat]}],norm_passive),
        fixed([{[ap_pred('ter beschikking'),acc,dat_pp(aan)]}],norm_passive),
	fixed([{[acc(vertrouwen),pc(in)]}],no_passive),
	part_np_pc_pp(aan,tot),
        part_als_pred_np(aan),
	part_np_pc_pp(af,op),
        part_transitive(bloot),
	part_np_pc_pp(bloot,aan),
	part_np_pc_pp(gelijk,aan),
	part_np_pc_pp(gelijk,met),
	part_np_pc_pp(in,op),
	part_als_pred_np(in),
	part_np_er_pp_sbar(in,op),
	part_np_er_pp_vp(in,op),
	part_np_pc_pp(samen,uit),
	part_np_pc_pp(teleur,in),
	part_np_pc_pp(teleur,over),
	part_np_pc_pp(vast,op),
	part_np_pc_pp(vrij,van),
	part_pc_pp(teleur,in),
	part_pc_pp(teleur,over),
	part_transitive(tevreden),
	part_refl_ld_pp(op),
	part_refl_pc_pp(in,op)  % or accidental?
       ])]).

v(stelp,stelpt,stelpen,gestelpt,stelpte,stelpten,
    [h([transitive])]).

v(stem,stemt,stemmen,gestemd,stemde,stemden,
  [h([ap_pred_np,
      so_ap_copula,           % de solidariteit stemt me hoopvol
	su_ap_pred_sbar,      % het stemt ons tevreden dat je komt
	pp_sbar_subj(tot),    % het stemt tot tevredenheid dat je komt
	part_np_sbar(toe),    % u zult me toestemmen dat (ouderwets)
	intransitive,
        transitive,
        pc_pp(tot), % dat stemt tot nadenken
	np_pc_pp(tot),
	part_intransitive(in),
	part_dip_sbar(in),
	part_intransitive(mee),
	part_intransitive(overeen),
	part_intransitive(tegen),
	part_intransitive(voor),
	part_transitive(af),
        part_transitive(weg),
	pc_pp(met),
	pc_pp(op),
	pc_pp(over),
	pc_pp(tegen),
	pc_pp(voor),
	er_pp_sbar(voor),
	er_pp_vp(voor),
	fixed([[blanco]],imp_passive),
	part_pc_pp(af,op),
	part_np_pc_pp(af,op),
	part_pc_pp(in,met),
	part_er_pp_sbar(in,met),
	part_er_pp_vp(in,met),
	part_pc_pp(overeen,met),
	part_intransitive(teleur),
	part_so_np(teleur),
	part_intransitive(toe),
	part_dip_sbar(toe),
	part_pc_pp(toe,in),
	part_er_pp_sbar(toe,in),
	part_er_pp_vp(toe,in)])]).

v(stempel,stempelt,stempelen,gestempeld,stempelde,stempelden,
    [h([ap_pred_np,
	part_intransitive(af),
	part_transitive(af),
	intransitive,
	transitive])]).

v(stenig,stenigt,stenigen,gestenigd,stenigde,stenigden,
  [h([transitive,
      intransitive])]).

v(step,stept,steppen,gestept,stepte,stepten,
    [h([intransitive])]).

v(sterf,sterft,sterven,gestorven,stierf,stierven,
    [unacc([intransitive,
            transitive,
            part_intransitive(af),
            part_intransitive(uit),
            part_intransitive(weg),
            pc_pp(aan)])]).

v(steriliseer,steriliseert,steriliseren,gesteriliseerd,steriliseerde,steriliseerden,
    [h([transitive])]).

v(sterk,sterkt,sterken,gesterkt,sterkte,sterkten,
  [h([transitive,
      sbar_subj_so_np,
      refl_pc_pp(in),
      refl_pc_pp(met)]),
   b([part_intransitive(aan)])]).

v(steun,steunt,steunen,gesteund,steunde,steunden,
    [h([intransitive,
	sbar,
	transitive,
	np_mod_pp(in),
% 	mod_pp(op),  LD?
	vp,
	ld_pp,
	np_ld_pp])]).

v(steven,stevent,stevenen,gestevend,stevende,stevenden,
    [z([intransitive,
	ld_dir,
	ld_pp,
	part_ld_pp(af)])]).

v(sticht,sticht,stichten,gesticht,stichtte,stichtten,
    [h([intransitive,
	transitive])]).

v(stier,stiert,stieren,gestierd,stierde,stierden,
    [z([intransitive,
	ld_dir,
	ld_pp])]).

v(stift,stift,stiften,gestift,stiftte,stiftten,
    [h([intransitive,
	transitive,
        part_transitive(bij),
	np_ld_pp,
	np_ld_dir])]).

v(stigmatiseer,stigmatiseert,stigmatiseren,gestigmatiseerd,
  stigmatiseerde,stigmatiseerden,
    [h([transitive,
	intransitive])]).

v(stijf,stijft,stijven,[gestijfd,gesteven],[stijfde,steef],[stijfden,steven],
    [h([transitive]),
     z([part_intransitive(op),
        intransitive])]).

v(stijg,stijgt,stijgen,gestegen,steeg,stegen,
    [z([intransitive,
	ld_dir,
	ld_pp,
	meas,
        so_np_ld_pp,
	part_intransitive(af),
	part_intransitive(in),
	part_intransitive(op),
	part_intransitive(uit),
	part_ld_pp(uit)])]).

v(stik,stikt,stikken,gestikt,stikte,stikten,
    [unacc([intransitive]),
     h([transitive]),
     b([pc_pp(in),
	pc_pp(van)])]).

v(stil,stilt,stillen,gestild,stilde,stilden,
    [h([transitive])]).

v(stileer,stileert,stileren,gestileerd,stileerde,stileerden,
    [h([transitive])]).

v(stimuleer,stimuleert,stimuleren,gestimuleerd,stimuleerde,stimuleerden,
    [h([sbar_subj_so_np,
	sbar,
	np_vp_obj1,
	np_pc_pp(in),
	np_pc_pp(tot),
	pc_pp(tot),
	np_er_pp_sbar(tot),
	np_er_pp_vp(tot),
	intransitive,
	transitive])]).

v(stink,stinkt,stinken,gestonken,stonk,stonken,
    [h([intransitive,
	pc_pp(naar)]),
     z([pc_pp(in),                % we zijn er in gestonken; we stonken in zijn geintje
        part_pc_er_transitive(in) % we zijn er ingestonken
       ])]).

v(stip,stipt,stippen,gestipt,stipte,stipten,
    [h([part_sbar(aan),
	part_transitive(aan),
        transitive])]).

v(stippel,stippelt,stippelen,gestippeld,stippelde,stippelden,
    [h([transitive,
	part_transitive(uit)])]).

v(stipuleer,stipuleert,stipuleren,gestipuleerd,stipuleerde,stipuleerden,
    [h([sbar,
	transitive])]).

v(stoei,stoeit,stoeien,gestoeid,stoeide,stoeiden,
    [h([intransitive,
	pc_pp(met)])]).

v(stoel,stoelt,stoelen,gestoeld,stoelde,stoelden,
    [h([intransitive,
	np_pc_pp(op),
	pc_pp(op)])]).

v(stof,stoft,stoffen,gestoft,stofte,stoften,
    [h([transitive,
	part_transitive(af)])]).

v(stofzuig,stofzuigt,stofzuigen,gestofzuigd,stofzuigde,stofzuigden,
    [h([transitive,
        intransitive])]).

v(stok,stokt,stokken,gestokt,stokte,stokten,
    [unacc([intransitive]),
     h([transitive])]).

v(stol,stolt,stollen,gestold,stolde,stolden,
    [unacc([intransitive])]).

v(stommel,stommelt,stommelen,gestommeld,stommelde,stommelden,
    [z([ld_dir]),
     b([ld_pp,
	part_intransitive(aan)]),
     h([intransitive])]).

v(stomp,stompt,stompen,gestompt,stompte,stompten,
    [z([part_intransitive(af)]),
     h([intransitive,
	transitive,
	part_transitive(af)])]).

v(stoof,stooft,stoven,gestoofd,stoofde,stoofden,
    [z([intransitive]),
     h([transitive,
        part_transitive(aan)]),
     b([ap_pred_np])]).

v(stook,stookt,stoken,gestookt,stookte,stookten,
    [h([intransitive,
	transitive,
	part_transitive(op),
	part_np_pc_pp(op,tegen)])]).

v(stoom,stoomt,stomen,gestoomd,stoomde,stoomden,
  [z([ld_pp,
      ld_adv,
	part_ld_pp(door),
	part_ld_pp(op),
	part_intransitive(op),
        part_intransitive(uit)]),
     h([transitive,
        part_transitive(af), % behang
	ap_pred_np,
	part_transitive(klaar),
	part_np_pc_pp(klaar,voor)]),
     b([intransitive,
        part_intransitive(aan)])]).

v(stoor,stoort,storen,gestoord,stoorde,stoorden,
    [h([intransitive,
	sbar_subj_so_np_opt_het,  % het meest stoort haar dat ..
	sbar_subj_opt_het,        % het meest stoort dat ..
        so_np,                    % dan stoort mij zijn aanwezigheid
	transitive,
	vp_subj_so_np,
	vp_subj,
	refl_pc_pp(aan),
	refl_er_pp_sbar(aan)])]).

v(stoot,stoot,stoten,gestoten,[stootte,stiet],[stootten,stieten],
    [z([ld_pp,
	part_intransitive(af),
	part_transitive(aan),
	part_pc_pp(door,naar)]),
     h([intransitive,
	nonp_pred_np,
	refl,
	transitive,
	np_ld_pp,
	fixed([[uit,de,mond],[het,brood],dat],norm_passive),
	part_transitive(af),
	part_transitive(door),
        part_intransitive(in),   % sport: bij het instoten voelde ik ...
	part_transitive(omver),
	part_transitive(op), % in de vaart der volken
	refl_ld_pp]),
     b([part_transitive(uit)])]).

v(stop,stopt,stoppen,gestopt,stopte,stopten,
    [z([intransitive,
	ld_adv,
	ld_pp,
	vp,
	pc_pp(met)]),
     h([transitive,
	np_ld_pp,
	np_np_ld_pp,
	part_np_np(toe),
	part_transitive(af),
	part_transitive(in),
	part_transitive(toe),
	part_transitive(weg)])]).

v(storm,stormt,stormen,gestormd,stormde,stormden,
    [z([ld_dir,
	ld_pp,
	part_intransitive(aan),
	part_intransitive(binnen),
	part_ld_pp(binnen),
	part_ld_pp(aan),
	part_ld_pp(af)]),
     h([het_subj])]).

v(stort,stort,storten,gestort,stortte,stortten,
  [z([nonp_copula,
      ld_pp,			% hij is naar beneden gestort
      ld_dir,			% hij stortte de diepte in
      intransitive,
      part_intransitive(in),
      part_intransitive(ineen),
      part_intransitive(neer),
      part_ld_pp(in),
      part_ld_pp(neer),
      part_np_ld_pp(in),
      part_np_ld_pp(neer),
      part_np_ld_pp(uit)]),
   h([transitive,
      np_ld_pp,
      np_np_ld_pp,	      % hij kreeg alles over zich heen gestort
      part_intransitive(over),
      part_transitive(af),	% je kunt geld "af"storten blijkbaar
      part_transitive(in),
      part_transitive(neer),
      part_transitive(terug),
      part_transitive(door),
      part_np_np(door),
      part_so_pp_np(door),
      part_transitive(over),
      part_transitive(uit),
      refl_ld_pp,
      refl_ld_dir]),
   b([ld_pp])]).

v(stortregen,stortregent,stortregenen,gestortregend,stortregende,stortregenden,
    [h([het_subj])]). %  het stortregent

v(stotter,stottert,stotteren,gestotterd,stotterde,stotterden,
    [h([intransitive,
	sbar,
	transitive,
	vp])]).

v(stouw,stouwt,stouwen,gestouwd,stouwde,stouwden,
    [h([part_transitive(vol),
	np_ld_pp,
        transitive,
	part_transitive(weg)])]).

v(straal,straalt,stralen,gestraald,straalde,straalden,
  [h([part_transitive(uit),
      part_transitive(over),
      part_transitive(door),
      part_transitive(af),	% het geluid wordt afgestraald
      fixed([[iets],van_sbar],no_passive), % ze straalden iets uit van : ...
      mod_pp(bij),		% maar ze straalt er een beetje bij
      part_sbar(uit)]),
   b([intransitive,
      part_pc_pp(af,op),
      pc_pp(van),
      er_pc_pp(af),		   % waar de tevredenheid af straalt
      part_pc_er_transitive(af), % voor: "waar de tevredenheid afstraalt"
      part_pc_pp(af,van),
      part_so_np(tegemoet),
      part_intransitive(uit)])]).

v(straf,straft,straffen,gestraft,strafte,straften,
    [h([intransitive,
	transitive,
	np_pc_pp(voor),
	part_transitive(af),
	part_intransitive(af), % VL
	pc_pp(voor)])]).

v(strand,strandt,stranden,gestrand,strandde,strandden,
    [unacc([intransitive,
            ld_pp,
            ld_adv])]).

v(stream,streamt,streamen,gestreamd,streamde,streamden,
  [h([intransitive,
      transitive])]).

v(streef,streeft,streven,gestreefd,streefde,streefden,
    [z([part_transitive(voorbij)]),
     h([part_transitive(na),
	pc_pp(naar),
	pc_pp(voor), % VL?
	er_pp_sbar(naar),
	er_pp_vp(naar),
        vp   % gestreefd wordt , de cijfers te verbeteren
       ])]).

v(streel,streelt,strelen,gestreeld,streelde,streelden,
  [h([transitive,
      intransitive,
      np_ld_pp,
      sbar_subj_np,		% het streelt hem, dat ...
      np_ld_adv])]).

v(streep,streept,strepen,gestreept,streepte,streepten,
    [h([transitive,
	part_transitive(aan),
	part_transitive(af),
	part_transitive(door),
	part_transitive(weg),
	ld_adv,
	ld_pp])]).

v(strek,strekt,strekken,gestrekt,strekte,strekten,
    [h([intransitive,
	% refl,
	transitive,
	np_ld_pp,
	part_transitive(uit),
	pc_pp(tot),
	er_pp_vp(tot),
	er_pp_sbar(tot),
	% refl_ld_pp,
	fixed([[tot,eer],dat],no_passive),
	fixed([sbar_subj,[tot,eer],dat],no_passive),
	part_np_ld_pp(uit),
        part_refl(uit),
	part_refl_ld_pp(uit)  % daar strekt zich een landschap uit...
       ])]).

v(strem,stremt,stremmen,gestremd,stremde,stremden,
    [unacc([intransitive]),
     h([transitive])]).

v(strengel,strengelt,strengelen,gestrengeld,strengelde,strengelden,
    [h([% refl,
	transitive])]).

v(stress,stresst,stressen,gestresst,stresste,stressten,
    [h([refl,
	refl_pc_pp(aan),   % ik stress me aan X
	transitive])]).

v(stribbel,stribbelt,stribbelen,gestribbeld,stribbelde,stribbelden,
    [h([intransitive,
	part_intransitive(tegen)])]).

v(striem,striemt,striemen,gestriemd,striemde,striemden,
    [h([intransitive,
	transitive])]).

v(strijd,strijdt,strijden,gestreden,streed,streden,
    [h([intransitive,
	transitive,
	part_intransitive(mee),
	part_pc_pp(mee,met),
	part_pc_pp(mee,tegen),
	part_pc_pp(mee,voor),
	part_pc_pp(mee,om),
	pc_pp(met),
	pc_pp(om),
	pc_pp(over),
	pc_pp(tegen),
	pc_pp(voor),
	er_pp_sbar(voor)])]).

v(strijk,strijkt,strijken,gestreken,streek,streken,
    [z([ap_pred_np,
	ld_pp,
	part_intransitive(neer),
	part_ld_pp(neer)]),
     h([intransitive,
	% refl,
	transitive,
	np_ld_pp,
	part_transitive(aan),
	part_transitive(af),
	part_transitive(glad),
        part_transitive(in),
	part_transitive(neer),
	part_transitive(op),
	part_transitive(uit)])]).

v(strik,strikt,strikken,gestrikt,strikte,strikten,
    [h([intransitive,
	transitive,
	np_pc_pp(voor)])]).

v(strip,stript,strippen,gestript,stripte,stripten,
    [h([intransitive,
	transitive])]).

v(strompel,strompelt,strompelen,gestrompeld,strompelde,strompelden,
    [z([ld_dir,
	ld_pp]),
     b([intransitive])]).

v(strooi,strooit,strooien,gestrooid,strooide,strooiden,
    [h([intransitive,
	transitive,
	part_transitive(bij),
	part_transitive(in),
	part_transitive(rond),
	part_transitive(uit),
	pc_pp(met),
	np_ld_pp,
	np_np_ld_pp,
	np_ld_dir,
	part_np_ld_pp(uit)])]).

v(strook,strookt,stroken,gestrookt,strookte,strookten,
    [h([pc_pp(met),
	intransitive])]).

v(stroom,stroomt,stromen,gestroomd,stroomde,stroomden,
    [z([ld_dir,
	ld_pp,
	so_np_ld_pp, % de tranen stromen haar over de wangen
	part_intransitive(binnen),
	part_intransitive(leeg),
	part_intransitive(toe),
	part_intransitive(over),
	part_intransitive(uit),
	part_intransitive(vol),
	part_intransitive(in),
	part_ld_pp(binnen),
	part_ld_pp(uit),
	part_ld_pp(in),
	part_so_np(toe),
	part_vp(toe),
	part_ld_pp(toe),
	part_ld_pp(uit),
	part_pc_pp(door,naar)]),
     h([intransitive,
	part_intransitive(weg)])]).

v(stroomlijn,stroomlijnt,stroomlijnen,gestroomlijnd,stroomlijnde,stroomlijnden,
    [h([transitive])]).

v(stroop,stroopt,stropen,gestroopt,stroopte,stroopten,
    [h([intransitive,
	transitive,
	np_ld_pp,
	part_np_np(af),
	part_transitive(af),
	part_transitive(op)])]).

v(structureer,structureert,structureren,gestructureerd,structureerde,structureerden,
    [h([sbar_subj_so_np,
	transitive])]).

v(struikel,struikelt,struikelen,gestruikeld,struikelde,struikelden,
    [z([intransitive,
	pc_pp(over)])]).

v(struin,struint,struinen,gestruind,struinde,struinden,
    [z([ld_pp,
        ld_dir]),
     h([intransitive,
	part_transitive(door),
	part_transitive(af)])]).

v(struktureer,struktureert,struktureren,gestruktureerd,struktureerde,struktureerden,
    [h([sbar_subj_so_np,
	transitive])]).

v(studeer,studeert,studeren,gestudeerd,studeerde,studeerden,
    [z([part_intransitive(af),
	part_pc_pp(af,in),
	part_pc_pp(af,op)]),
     h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv,
	part_transitive(in),
	pc_pp(op)]),
     b([part_transitive(af)])]).

v(stuif,stuift,stuiven,gestoven,stoof,stoven,
    [z([part_intransitive(weg),
	ld_dir,
	part_intransitive(aan),
	part_intransitive(op),
	ld_pp]),
     h([intransitive,
	transitive])]).

v(stuiptrek,stuiptrekt,stuiptrekken,gestuiptrekt,stuiptrekte,stuiptrekten,
    [h([intransitive])]).

v(stuit,stuit,stuiten,gestuit,stuitte,stuitten,
    [z([pc_pp(op),
	part_intransitive(af),
	part_pc_pp(af,op)]),
     h([intransitive,
	fixed([[tegen,de,borst]],no_passive),
	fixed([[tegen,de,borst],dat],no_passive),
	fixed([sbar_subj,[tegen,de,borst],dat],no_passive),
	transitive])]).

v(stuiter,stuitert,stuiteren,gestuiterd,stuiterde,stuiterden,
    [b([ld_dir,
	part_intransitive(aan),
	ld_pp]),
     h([transitive,
	np_ld_dir,
	np_ld_pp])]).

v(stulp,stulpt,stulpen,gestulpt,stulpte,stulpten,
    [h([transitive,
        part_intransitive(uit),
	part_transitive(uit)])]).

v(stunt,stunt,stunten,gestunt,stuntte,stuntten,
    [h([intransitive])]).

v(stuntel,stuntelt,stuntelen,gestunteld,stuntelde,stuntelden,
    [h([intransitive])]).

v(stut,stut,stutten,gestut,stutte,stutten,
    [h([transitive])]).

v(stuuk,stuukt,stuken,gestuukt,stukte,stukten,
    [h([intransitive,
	transitive])]).

v(stuur,stuurt,sturen,gestuurd,stuurde,stuurden,
    [h([np_np,
	intransitive,
	np_ld_dir,
        np_uit,
	so_pp_np,
	transitive,
	np_ld_pp,
        np_ld_adv,
	np_pc_pp(om),
	part_np_np(toe),
	part_np_np(mee),
	part_np_np(op),
	part_np_np(terug),
	part_intransitive(af),
	part_intransitive(bij),
	part_transitive(aan),
	part_pc_pp(aan,op),
	part_er_pp_sbar(aan,op),
	part_er_pp_vp(aan,op),
	part_transitive(af),
	part_transitive(bij),
	part_transitive(door),
        part_transitive(heen),  % van bolletjesslikkers
	part_transitive(in),
	part_transitive(langs),  % ik stuur al mijn vrienden langs
	part_transitive(mee), % NB dat ik de spullen mee naar huis stuur
        part_transitive(na),
        part_np_np(na),
	part_transitive(op),
	part_np_ld_pp(op),
	part_intransitive(op),
	part_transitive(over),
	part_transitive(rond),
	part_transitive(terug),
	part_np_mod_pp(terug,met), % mijn vriendin krijg ik er niet mee terug
	part_transitive(toe),
        part_np_np(thuis),   % ik krijg de boeken thuisgestuurd
	part_transitive(uit),
	part_transitive(weg),
	fixed([ap_pred('in de war'),acc],norm_passive),
	part_ld_pp(aan),
	part_np_ld_pp(aan),
	part_np_ld_pp(af),
	part_np_ld_pp(door),
	part_np_ld_pp(terug),
	part_np_ld_pp(uit),
	part_np_ld_pp(weg),
	part_np_pc_pp(uit,op)])]).

v(stuw,stuwt,stuwen,gestuwd,stuwde,stuwden,
    [h([np_ld_dir,
	transitive,
	np_ld_pp,
	part_transitive(op),
	part_transitive(voort)])]).

v(sublimeer,sublimeert,sublimeren,gesublimeerd,sublimeerde,sublimeerden,
    [h([intransitive,
	transitive])]).

v(subsidieer,subsidieert,subsidiëren,gesubsidieerd,subsidieerde,subsidieerden,
    [h([transitive,
	intransitive])]).

v(substitueer,substitueert,substitueren,gesubstitueerd,substitueerde,substitueerden,
    [h([pc_pp(voor),
	intransitive])]).

v(sudder,suddert,sudderen,gesudderd,sudderde,sudderden,
  [h([intransitive,
      ap_pred_np,		% sudder het vlees gaar
      transitive,
      part_transitive(na),
      part_transitive(mee),
      part_intransitive(na)])]).

v(suf,suft,suffen,gesuft,sufte,suften,
    [h([intransitive])]).

v(suggereer,suggereert,suggereren,gesuggereerd,suggereerde,suggereerden,
    [h([np_np,
	np_sbar,
	np_vp_obj,
	sbar,
	transitive,
	vp])]).

v(suis,suist,suizen,gesuisd,suisde,suisden,
    [z([ld_dir,
	ld_pp]),
     h([intransitive])]).

v(sukkel,sukkelt,sukkelen,gesukkeld,sukkelde,sukkelden,
    [z([ld_dir,
	part_intransitive(aan),
	ld_pp]),
     h([intransitive,
	pc_pp(met)])]).

v(supporter,supportert,supporteren,gesupporterd,supporterde,supporterden,
    [h([intransitive,
        transitive])]).

v(surf,surft,surfen,[gesurfd,gesurft],[surfde,surfte],[surfden,surften],
    [h([intransitive,
        part_transitive(af)  %internet
       ])]).

v(surveilleer,surveilleert,surveilleren,gesurveilleerd,surveilleerde,
  surveilleerden,
    [h([intransitive])]).

v(sus,sust,sussen,gesust,suste,susten,
    [h([intransitive,
	transitive,
	sbar])]).

v(swing,swingt,swingen,geswingd,swingde,swingden,
    [h([intransitive,
        part_fixed(uit,[[de,pan]],no_passive)])
    ]).

v(swipe,swipet,swipen,geswipet,swipete,swipeten,
    [h([intransitive])]).

v(switch,switcht,switchen,geswitcht,switchte,switchten,
    [b([intransitive])]).

v(symboliseer,symboliseert,symboliseren,gesymboliseerd,symboliseerde,symboliseerden,
    [h([sbar,
	transitive])]).

v(sympathiseer,sympathiseert,sympathiseren,gesympathiseerd,sympathiseerde,sympathiseerden,
    [h([pc_pp(met),
        intransitive])]).

v(sympatiseer,sympatiseert,sympatiseren,gesympatiseerd,sympatiseerde,sympatiseerden,
    [h([pc_pp(met),
        intransitive])]).

v(synchroniseer,synchroniseert,synchroniseren,gesynchroniseerd,
  synchroniseerde,synchroniseerden,
    [h([part_transitive(na),
	transitive,
	intransitive])]).

v(systematiseer,systematiseert,systematiseren,gesystematiseerd,systematiseerde,systematiseerden,
    [h([transitive])]).

v(taai,taai,taaien,getaaid,taaide,taaiden,
  [z([part_intransitive(af)])]).

v(taal,taalt,talen,getaald,taalde,taalden,
    [h([pc_pp(naar)])]).

v(taan,taant,tanen,getaand,taande,taanden,
    [h([intransitive,   % de belangstelling taant (?)
	transitive])]).

v(tackel,tackelt,tackelen,getackeld,tackelde,tackelden,
    [h([intransitive,
	transitive])]).

v(tafel,tafelt,tafelen,getafeld,tafelde,tafelden,
    [h([intransitive,
	part_intransitive(na)])]).

v(tafeltennis,tafeltennist,tafeltennissen,getafeltennist,tafeltenniste,tafeltennisten,
    [h([intransitive])]).

v(tak,takt,takken,getakt,takte,takten,
    [unacc([part_intransitive(af)]),
     h([part_transitive(af)])]).

v(takel,takelt,takelen,getakeld,takelde,takelden,
    [h([part_transitive(op),
	part_transitive(toe),
	part_transitive(weg),
	np_ld_pp,
	np_ld_dir,
	transitive]),
     unacc([part_intransitive(af)])]).

v(talm,talmt,talmen,getalmd,talmde,talmden,
    [h([intransitive,
	pc_pp(met)])]).

%% je haren tangen
v(tang,tangt,tangen,getangd,tangde,tangden,
    [h([transitive])]).

v(tank,tankt,tanken,getankt,tankte,tankten,
    [h([intransitive,
	part_intransitive(bij),
	part_transitive(bij),
	transitive])]).

v(tap,tapt,tappen,getapt,tapte,tapten,
    [h([intransitive,
	transitive,
	ld_pp,
	np_ld_pp,
	part_np_np(af),
	part_transitive(af),
	part_np_pc_pp(af,van)])]).

v(tape,tapet,tapen,getapet,tapete,tapeten,
  [h([intransitive,
      transitive,
      part_transitive(vast),
      part_np_ld_pp(vast)])]).

v(tart,tart,tarten,getart,tartte,tartten,
    [h([transitive])]).

v(tas,tast,tassen,getast,taste,tasten,
    [h([part_transitive(op)])]).

v(tast,tast,tasten,getast,tastte,tastten,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv,
	part_transitive(aan),
	part_np_mod_pp(aan,door),
	part_transitive(af),
	part_sbar(af),
	part_intransitive(mis),
	pc_pp(naar),
	part_intransitive(toe),
	fixed([[in,het,duister]],imp_passive),
	fixed([[in,het,duister],pc(naar)],imp_passive)])]).

v(tatoeëer,tatoeëert,tatoeëren,getatoeëerd,tatoeëerde,tatoeëerden,
    [h([transitive])]).

v(taxeer,taxeert,taxeren,getaxeerd,taxeerde,taxeerden,
    [h([transitive,
	sbar,
        intransitive,
	np_pc_pp(op)])]).

v(taxi,taxiet,taxiën,getaxied,taxiede,taxieden,
  [h([intransitive,
      ld_pp,
      np_ld_pp
     ])]).  

v(teel,teelt,telen,geteeld,teelde,teelden,
  [h([transitive,
      intransitive  % de smokkel teelde welig
     ])]).

v(teem,teemt,temen,geteemd,teemde,teemden,
    [h([intransitive,
	sbar
       ])
    ]).

v(teer,teert,teren,geteerd,teerde,teerden,
    [h([intransitive,
	transitive,
	part_pc_pp(in,op),
        part_intransitive(in),
	part_intransitive(uit),
	part_transitive(uit),
	pc_pp(op)])]).

v(tegel,tegelt,tegelen,getegeld,tegelde,tegelden,
    [h([intransitive])]).  

v(teister,teistert,teisteren,geteisterd,teisterde,teisterden,
    [h([transitive])]).

v(teken,tekent,tekenen,getekend,tekende,tekenden,
    [h([ap_pred_np,
	intransitive,
	transitive,
	sbar_subj_np,
	np_pc_pp(naar),
	part_intransitive(aan),
	part_refl(af),
	part_sbar_subj_refl_no_het(af), 
	part_sbar(aan),
	part_pp_sbar(aan,bij),  % hierbij moet worden aangetekend dat...
	part_sbar(op),
	part_vp(aan),
	part_transitive(aan),
	part_transitive(af),
	part_intransitive(bij),
	part_intransitive(in),
	part_transitive(in),
	part_intransitive(na),
	part_transitive(bij),
	part_transitive(na),
	part_transitive(op),
        part_transitive(over),
	part_transitive(uit),
	pc_pp(naar),
	pc_pp(voor),
	part_fixed(aan,[{[acc(bezwaar),pc(tegen)]}],no_passive),
	part_fixed(aan,[{[acc(bezwaar),er_pp(tegen,A)]},extra_sbar(A)],
		   no_passive),
	part_fixed(aan,[{[acc(bezwaar),er_pp(tegen,A)]},extra_vp(A)],
		   no_passive),
	part_fixed(aan,[{[acc(protest),pc(tegen)]}],no_passive),
	part_fixed(aan,[{[acc(protest),er_pp(tegen,A)]},extra_sbar(A)],
		   no_passive),
        fixed([[present]],imp_passive),  % Vlaams
	part_np_ld_pp(op),
	part_np_pc_pp(aan,tegen),
	part_np_pc_pp(in,voor),
	part_pc_pp(in,op),
	part_pc_pp(in,voor)
       ])]).

v(tel,telt,tellen,geteld,telde,telden,
    [h([ap_pred_np,
	intransitive,
	sbar,             % we moeten tellen hoeveel ...
	transitive,       
        sbar_subj_no_het, % voor ons telt hoe het publiek reageert
                          % wat telt is of hij komt
	part_intransitive(af),
        part_intransitive(mee),
	part_sbar(op),
	part_transitive(af),
	part_transitive(bij),
	part_transitive(mee),
	part_transitive(na),
        part_sbar(na),  % op je vingers
	part_transitive(neer),
	part_transitive(op),
	part_intransitive(op), % juf Ida: vandaag gaan we optellen
        part_transitive(uit),
        part_sbar(uit),
	pc_pp(tot),
	part_pc_pp(op,tot),
	pc_pp(vanaf),
	pc_pp(voor),
	part_np_pc_pp(neer,voor),
	part_np_pc_pp(op,bij),
	part_pc_pp(mee,bij),
	part_pc_pp(mee,met),
	part_pc_pp(mee,voor)])]).

v(telefoneer,telefoneert,telefoneren,getelefoneerd,telefoneerde,telefoneerden,
    [h([np_np,
	intransitive,
	np_sbar,
	ld_pp,
	transitive,
	part_intransitive(door),
	part_transitive(door),
	pc_pp(met)])]).

v(telegrafeer,telegrafeert,telegraferen,getelegrafeerd,telegrafeerde,telegrafeerden,
  [h([transitive,
      sbar,
      np_sbar
     ])]).

v(tem,temt,temmen,getemd,temde,temden,
    [h([transitive])]).

v(temper,tempert,temperen,getemperd,temperde,temperden,
    [h([transitive,
        acc_np_dip_sbar])]).

v(temporiseer,temporiseert,temporiseren,getemporiseerd,temporiseerde,
  temporiseerden,
    [h([intransitive])]).

v(tendeer,tendeert,tenderen,getendeerd,tendeerde,tendeerden,
    [h([intransitive,
	pc_pp(naar)])]).

v(tennis,tennist,tennissen,getennist,tenniste,tennisten,
    [h([intransitive])]).

v(terg,tergt,tergen,getergd,tergde,tergden,
    [h([transitive])]).

v(terroriseer,terroriseert,terroriseren,geterroriseerd,terroriseerde,terroriseerden,
    [h([transitive])]).

v(test,test,testen,getest,testte,testten,
    [h([sbar,
	transitive,
        intransitive,
	mod_pp(met),
	np_mod_pp(met),
	np_pc_pp(op),
	part_np_pc_pp(uit,op),
	part_sbar(uit),
	part_transitive(uit),
	pc_pp(op)])]).

v(tetter,tettert,tetteren,getetterd,tetterde,tetterden,
    [h([intransitive,
        sbar,
        pc_pp(door)])]).

v(theologiseer,theologiseert,theologiseren,getheologiseerd,theologiseerde,
  theologiseerden,
    [h([intransitive])]).

v(tien,tient,tienen,getiend,tiende,tienden,
    [h([intransitive
       ])]).

v(tier,tiert,tieren,getierd,tierde,tierden,
    [h([intransitive,
	sbar])]).

v(tijg,tijgt,tijgen,getogen,toog,togen,
    [z([ld_pp,
	fixed([[ten,strijde]],imp_passive),
	fixed([{[[ten,strijde],pc(tegen)]}],imp_passive),
	pc_pp(aan)])]).

v(tik,tikt,tikken,getikt,tikte,tikten,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv,
	part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(binnen),
	part_intransitive(in),
	part_intransitive(naast),
	part_intransitive(over),
	part_intransitive(raak),
	part_intransitive(weg),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(binnen),
	part_transitive(door),
	part_transitive(in),
	part_transitive(om),
	part_transitive(over),
	part_transitive(uit),
	part_transitive(weg),
	fixed([svp_pp(op,vinger),acc],norm_passive),
	fixed([[op,de,kop],acc],norm_passive)])]).

v(til,tilt,tillen,getild,tilde,tilden,
    [h([intransitive,
	transitive,
	np_ld_pp,
        np_ld_dir,
	%% wrong: zwaar can be modified... 'al te' 'zo zwaar dat'
	fixed([{[ap_pred(zwaar),pc(aan)]}],imp_passive),
	fixed([{[ap_pred(zwaar),er_pp(aan,C)]},extra_sbar(C)],imp_passive),
	part_transitive(op),
	part_transitive(uit),
	part_np_ld_pp(uit)])]).

v(time,timet,timen,getimed,timede,timeden,
    [h([transitive])]).

v(timmer,timmert,timmeren,getimmerd,timmerde,timmerden,
    [h([transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv,
	part_transitive(af),
	part_transitive(dicht),
	part_transitive(in),
	intransitive, % ik timmer graag
	fixed([[aan,de,weg]],imp_passive)])]).

v(tinkel,tinkelt,tinkelen,getinkeld,tinkelde,tinkelden,
    [h([intransitive])]).

v(tint,tint,tinten,getint,tintte,tintten,
    [h([transitive])]).

v(tintel,tintelt,tintelen,getinteld,tintelde,tintelden,
    [h([intransitive])]).

v(tip,tipt,tippen,getipt,tipte,tipten,
    [h([als_pred_np,
	intransitive,
	transitive,
        acc_np_sbar,
	np_pc_pp(over),
	np_pc_pp(voor),
	pc_pp(aan)])]).

v(tippel,tippelt,tippelen,getippeld,tippelde,tippelden,
    [h([intransitive,
	pc_pp(op)])]).

v(toast,toast,toasten,getoast,toastte,toastten,
    [h([intransitive,
	pc_pp(op)])]).

v(tob,tobt,tobben,getobd,tobde,tobden,
  [h([intransitive,
      part_refl(af),
	pc_pp(met),
	pc_pp(over)])]).

v(tocht,tocht,tochten,getocht,tochtte,tochtten,
    [h([intransitive])]).

v(toef,toeft,toeven,getoefd,toefde,toefden,
    [h([intransitive,
	ld_pp,
	ld_adv])]).

v([toer,tour],[toert,tourt],[toeren,touren],[getoerd,getourd],[toerde,tourde],[toerden,tourden],
    [h([intransitive,
        ld_pp,
	ld_dir   % de wereld rond
       ])]).

v(toeter,toetert,toeteren,getoeterd,toeterde,toeterden,
    [h([np_np,
	intransitive,
	transitive])]).

v(toets,toetst,toetsen,getoetst,toetste,toetsten,
    [h([transitive,
	part_transitive(in),
	sbar,
	np_pc_pp(aan),
	part_np_pc_pp(af,op),
	part_transitive(af),
	np_pc_pp(op)])]).

v(tokkel,tokkelt,tokkelen,getokkeld,tokkelde,tokkelden,
    [h([intransitive,
	transitive])]).

v(tol,tolt,tollen,getold,tolde,tolden,
    [z([ld_pp,
	ld_dir]),
     h([intransitive,
	part_intransitive(na)])]).

v(tolereer,tolereert,tolereren,getolereerd,tolereerde,tolereerden,
    [h([sbar,
	sbar_obj,
	transitive])]).

v(tolk,tolkt,tolken,getolkt,tolkte,tolkten,
    [h([intransitive])]).

v(tong,tongt,tongen,getongd,tongde,tongden,
    [h([intransitive])]).

v(tongzoen,tongzoent,tongzoenen,getongzoend,tongzoende,tongzoenden,
    [h([intransitive,
	transitive])]).

v(tooi,tooit,tooien,getooid,tooide,tooiden,
    [h([transitive,
	refl_pc_pp(met)])]).

v(toom,toomt,tomen,getoomd,toomde,toomden,
    [h([part_transitive(in)])]).

v(toon,toont,tonen,getoond,toonde,toonden,
    [h([np_np,
	np_sbar,
	vp,	      % Henin toonde onverslaanbaar te zijn
	np_vp_subj,   % Henin toonde het publiek onverslaanbaar te zijn op gravel
	pred_refl,
	refl,  % daar toont zich de meester
        refl_pc_pp(in), % daar toonde hij zich een (groot)meester in
	sbar,
	fixed([{[acc(belangstelling),pc(voor)]}],no_passive),
	fixed([{[acc(begrip),pc(voor)]}],no_passive),
	fixed([{[acc(begrip),er_pp(voor,X)]},extra_sbar(X)],no_passive),
	fixed([{[acc(interesse),pc(voor)]}],no_passive),
	fixed([{[acc(interesse),pc(in)]}],no_passive),
	so_pp_np,
	so_pp_sbar,
	transitive,
	intransitive,
	part_np_np(aan), % ik kan het je aantonen
	part_vp(aan),
	part_sbar(aan),
	part_sbar_sbar_subj(aan),
	part_transitive(aan)])]).

v(toost,toost,toosten,getoost,toostte,toostten,
    [h([intransitive,
	pc_pp(op)])]).

v(top,topt,toppen,getopt,topte,topten,
  [h([transitive,
      part_transitive(af)
     ])]).

v(toren,torent,torenen,getorend,torende,torenden,
    [h([ld_pp,
        part_ld_pp(uit),
        intransitive])]).

v(torn,tornt,tornen,getornd,tornde,tornden,
    [z([intransitive]),
     h([transitive,
	np_ld_pp,
        part_pc_pp(op,tegen),
	pc_pp(aan)])]).

v(torpedeer,torpedeert,torpederen,getorpedeerd,torpedeerde,torpedeerden,
    [h([transitive])]).

v(tors,torst,torsen,getorst,torste,torsten,
    [h([transitive])]).

v(totaliseer,totaliseert,totaliseren,getotaliseerd,totaliseerde,totaliseerden,
    [h([transitive])]).
  

v(toucheer,toucheert,toucheren,getoucheerd,toucheerde,toucheerden,
    [h([transitive])]).

v(touwtrek,trouwtrekt,touwtrekken,getouwtrekt,touwtrekte,touwtrekten,
    [h([intransitive])]).

v(tover,tovert,toveren,getoverd,toverde,toverden,
    [h([np_np,
	intransitive,
	transitive,
	np_ld_pp,
	np_ld_dir,
	part_transitive(voor),
	part_np_np(voor),
	part_als_pred_np(om),
	part_transitive(om),
	part_np_pc_pp(om,in),
	part_np_pc_pp(om,tot)])]).

v(traan,traant,tranen,getraand,traande,traanden,
    [h([intransitive])]).

v(traceer,traceert,traceren,getraceerd,traceerde,traceerden,
    [h([transitive])]).

v(tracht,tracht,trachten,getracht,trachtte,trachtten,
    [h([vp,
        subj_control(te)])]).

v(tracteer,tracteert,tracteren,getracteerd,tracteerde,tracteerden,
    [h([intransitive,
	transitive,
	np_pc_pp(op),
	pc_pp(op)])]).

v(trade,tradet,traden,getraded,tradede,tradeden,
    [h([intransitive,
	transitive])]).

v(train,traint,trainen,getraind,trainde,trainden,
    [h([intransitive,
	transitive,
        part_intransitive(mee),
	ap_pred_refl, % hij trainde zich wezenloos
        np_vp_obj1,
	pc_pp(in),
	np_pc_pp(in),
	obj_np_er_pp_vp(in),  % hij werd er in getraind assertiever te zijn
	np_er_pp_sbar(in),    % we trainen hem erin dat hij op zulke momenten assertiever moet zijn...
	np_pc_pp(op),
	pc_pp(op)])]).

v(traineer,traineert,traineren,getraineerd,traineerde,traineerden,
    [h([transitive])]).

v(trakteer,trakteert,trakteren,getrakteerd,trakteerde,trakteerden,
    [h([intransitive,
	transitive,
	np_pc_pp(op),
	pc_pp(op)])]).

v(transcendeer,transcendeert,transcenderen,getranscendeerd,transcendeerde,transcendeerden,
    [h([sbar_subj_so_np,
	transitive])]).

v(transcribeer,transcribeert,transcriberen,getranscribeerd,transcribeerde,transcribeerden,
    [h([transitive])]).

v(transformeer,transformeert,transformeren,getransformeerd,transformeerde,transformeerden,
    [h([transitive,
	np_pc_pp(in),
	np_pc_pp(tot)]),
     z([intransitive,
        pc_pp(in),
        pc_pp(tot)])]).

v(transpireer,transpireert,transpireren,getranspireerd,transpireerde,transpireerden,
    [h([intransitive])]).

v(transplanteer,transplanteert,transplanteren,getransplanteerd,
  transplanteerde,transplanteerden,
    [h([intransitive,
	transitive])]).

v(transporteer,transporteert,transporteren,getransporteerd,transporteerde,transporteerden,
    [h([intransitive,
	transitive,
	np_mod_pp(in)
       ])]).

v(trap,trapt,trappen,getrapt,trapte,trapten,
    [z([ld_pp]),
     h([intransitive,
	np_ld_dir,
	nonp_pred_np,
	transitive,
	part_intransitive(af),  % Feyenoord trapt af
	part_intransitive(binnen), % VL scoren
	part_transitive(binnen),   % een strafschop binnentrappen
	part_intransitive(na),
	part_intransitive(over),
	part_intransitive(mis),
	part_intransitive(naast),
	part_intransitive(raak),
	part_transitive(aan),   % de brommer aantrappen
	part_transitive(na),
	part_intransitive(uit),
	part_transitive(uit),
	part_transitive(plat),
	np_ld_pp,
	part_pc_pp(in,op),  % er werd hard op hem in getrapt
	part_transitive(in)])]).

v(trappel,trappelt,trappelen,getrappeld,trappelde,trappelden,
    [h([intransitive,
	pc_pp(met)])]).

v(treed,treedt,treden,getreden,trad,traden,
    [z([ld_dir,
	ld_pp,
	pp_copula(in,contact),
	pp_copula(in,dienst),
	pp_copula(in,werking),
        pp_copula(op,voorgrond),
	part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(binnen),
	part_intransitive(in),
	part_intransitive(terug),
	part_intransitive(toe),
	part_intransitive(uit),
	part_ld_pp(binnen),
	part_ld_pp(in),
	part_transitive(aan),
        part_transitive(bij),  % Vlaams
	part_transitive(binnen),
	part_np_pc_pp(bij,in),  % europarl: ik moet u bijtreden in uw aanklachten
	pc_pp(aan),
	fixed([[aan,het,licht]],no_passive),
        fixed([svp_pp(buiten,oever)],no_passive),
        fixed([{[pc(van),svp_pp(buiten,oever)]}],no_passive),
        fixed([{[pc(met),pp_pred(in,contact)]}],no_passive),
        fixed([{[pc(met),svp_pp(in,communicatie)]}],no_passive),
        fixed([svp_pp(in,communicatie)],no_passive),
        fixed([{[pc(met),svp_pp(in,debat)]}],no_passive),
        fixed([svp_pp(in,debat)],no_passive),
        fixed([{[pc(met),svp_pp(in,dialoog)]}],no_passive),
        fixed([svp_pp(in,dialoog)],no_passive),
        fixed([{[pc(met),svp_pp(in,huwelijk)]}],no_passive),
        fixed([svp_pp(in,huwelijk)],no_passive),
        fixed([{[pc(met),svp_pp(in,voetspoor)]}],no_passive),
        fixed([svp_pp(in,voetspoor)],no_passive),
	fixed([[in,werking]],no_passive),
	fixed([[inwerking]],no_passive),
	fixed([[naar,buiten]],no_passive),
	fixed([[naar,buiten],mod_pp(met)],no_passive),
	part_so_np(tegemoet),
	part_ld_pp(toe),
	part_pc_pp(aan,tegen),
	part_pc_pp(toe,tot),
	part_pc_pp(uit,uit),
	part_pc_pp(uit,van)]),
     b([part_intransitive(op),
        part_intransitive(wederop),
        part_als_copula(op),
	part_pc_pp(op,tegen),
        part_ld_pp(op),
        part_ld_adv(op)
       ]),
     h([fixed([[met,de,voeten],acc],norm_passive),
	fixed([[met,voeten],acc],norm_passive),
	part_transitive(tegemoet)])]).

v(tref,treft,treffen,getroffen,trof,troffen,
    [h([intransitive,
	transitive,  % bij jullie treft men nog ... (aan)
	             % ik werd getroffen door zijn buitengewone stijl
	so_np,       % toen trof mij zijn betrokkenheid
	sbar_subj,   % het treft dat je thuis bent
	sbar_subj_so_np,  % het treft me dat je thuis bent
	sbar_obj,	  % je treft het dat hij thuis is
	np_ld_pp,
	np_ld_adv,
        fixed([subj(blaam),dat],no_passive),
	part_ap_pred_np(aan),
	part_transitive(aan),
	part_np_ld_pp(aan),
	part_np_ld_adv(aan)])]).

v(treiter,treitert,treiteren,getreiterd,treiterde,treiterden,
    [h([intransitive,
	transitive])]).

v(trek,trekt,trekken,getrokken,trok,trokken,
    [z([ld_dir,
	ld_pp,
	part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(binnen),
	part_intransitive(bij),
	part_intransitive(dicht),
	part_intransitive(door),
	part_intransitive(in),
	part_intransitive(langs),
        part_intransitive(mee),
        part_intransitive(om),
	part_intransitive(over),
	part_intransitive(samen),
	part_intransitive(terug),
	part_intransitive(uit),
	part_intransitive(voorbij),
	part_intransitive(weg),
	part_ld_pp(in), 
	part_ld_pp(om), 
	part_ld_pp(rond),
	part_ld_adv(rond),
	pc_pp(van),
	fixed([{[[van,leer],pc(tegen)]}],imp_passive),
	fixed([[in,de,aanval]],no_passive),
	fixed([[ten,aanval]],no_passive),
	fixed([{[[ten,oorlog],pc(tegen)]}],imp_passive),
	fixed([{[[ten,strijde],pc(tegen)]}],imp_passive),
	fixed([{[[ten,velde],pc(tegen)]}],imp_passive),
	fixed([[ten,oorlog]],imp_passive),
	fixed([[ten,strijde]],imp_passive),
	fixed([[ten,velde]],imp_passive),
	fixed([[teweer],pc(tegen)],imp_passive),
	fixed([[te,weer],pc(tegen)],no_passive),
	fixed([[van,leer]],imp_passive),
	part_ld_pp(terug),
	part_ld_pp(voorbij),
	part_ld_pp(weg),
	part_pc_pp(af,van),
	part_pc_pp(mee,met),
	part_pc_pp(op,met),
	part_ld_pp(op),  % optrekken naar
	part_pc_pp(samen,tot),
	part_ld_pp(weg),
	part_pc_pp(uit,op)	% erop uittrekken
       ]),
     h([np_np,
	intransitive,
	np_ld_dir,
	nonp_pred_np,
        pp_pred_np_sbar, % wij trekken (het) in twijfel of hij komt
	transitive,
	fixed([{[acc(conclusie),pc(uit)]}],norm_passive),
	fixed([{[acc(lering),pc(uit)]}],norm_passive),
        fixed([{[acc(les),pc(uit)]}],norm_passive),
        fixed([[op,gang],acc],no_passive), % PP PREDC?
	fixed([{[acc(pijl),pc(op)]}],norm_passive),
	fixed([{[acc(profijt),pc(uit)]}],norm_passive),
	fixed([{[acc(profijt),pc(van)]}],norm_passive),
	fixed([{[acc(wissel),pc(op)]}],norm_passive),
	fixed([acc(aandacht),sbar_subj],no_passive),  % het trekt de aandacht, dat...
	np_ld_pp,
	np_pc_pp(van),
	part_np_np(af),
	part_np_np(uit),
	part_refl(af),
	part_refl(samen),
%%	part_refl(terug),    % or accidental reflexive??
	part_refl_np(aan),
	part_sbar(na),
	part_sbar_subj_so_np(op),
	part_transitive(aan),
	part_np_pc_pp(aan,tot), % dat trok hem aan tot ...
	                        % ik voelde me aangetrokken tot..
	part_np_mod_pp(aan,in),  % dat is wat me er zo in aan trekt
        part_np_np(aan),  % een kind kleren aantrekken
	part_transitive(af),
	part_transitive(bij),  % riddle: hij trok een stoel bij
	part_transitive(dicht),
	part_transitive(door),
	part_transitive(gelijk),
	part_transitive(mee),
	part_transitive(na),
	part_transitive(om),
	part_transitive(op),
	part_transitive(open),
	part_transitive(over),
	part_transitive(recht),
	part_transitive(samen),
	part_transitive(terug),
	part_transitive(toe),
	part_transitive(uit),
        part_transitive(vlot),
	part_transitive(voor),
	part_transitive(voort),
	part_transitive(weg),
	pc_pp(aan),
	pc_pp(met),
	part_mod_pp(aan,voor),
	part_np_ld_pp(door),
	part_np_ld_pp(terug),
	part_np_ld_pp(weg),
	part_np_pc_pp(op,uit),
	part_np_pc_pp(toe,naar),
	part_np_pc_pp(uit,voor),
%	part_refl_ld_pp(terug),
	part_fixed(aan,[{[pc(van),acc]},refl],no_passive),
	part_refl_pc_pp(aan,van),
	part_refl_pc_pp(op,aan)]),
     b([part_intransitive(rond),
	part_transitive(in),
	part_intransitive(op)])]).  % we hebben/zijn samen opgetrokken

v(treur,treurt,treuren,getreurd,treurde,treurden,
    [h([intransitive,
	sbar,  % mostly dip
	part_intransitive(na),
	pc_pp(om),
	pc_pp(over),
	part_pc_pp(na,om),
	part_pc_pp(na,over)])]).

v(treuzel,treuzelt,treuzelen,getreuzeld,treuzelde,treuzelden,
    [h([intransitive,
	pc_pp(met)])]).

v(trigger,triggert,triggeren,getriggerd,triggerde,triggerden,
    [h([transitive])]).

v(tril,trilt,trillen,getrild,trilde,trilden,
    [h([intransitive,
	pc_pp(van),
        part_intransitive(mee),
	part_intransitive(los),
	part_intransitive(na)])]).

v(trim,trimt,trimmen,getrimd,trimde,trimden,
    [h([intransitive,
	transitive])]).

v(triomfeer,triomfeert,triomferen,getriomfeerd,triomfeerde,triomfeerden,
    [h([intransitive,
	pc_pp(over)])]).

v(trip,tript,trippen,getript,tripte,triptedn,
  [h([intransitive,
      pc_pp(op)])]).

v(trippel,trippelt,trippelen,getrippeld,trippelde,trippelden,
    [b([intransitive])]).

v(troef,troeft,troeven,getroefd,troefde,troefden,
    [h([intransitive,
	transitive,
	part_intransitive(af),
	part_intransitive(in),
	part_transitive(af),
	part_intransitive(over),
	part_transitive(over)
       ])]).

v(troggel,troggelt,troggelen,getroggeld,troggelde,troggelden,
    [h([part_transitive(af),
	part_transitive(los),
	part_transitive(weg),
	part_np_np(af)])]).

v(trommel,trommelt,trommelen,getrommeld,trommelde,trommelden,
    [h([intransitive,
	transitive,
	part_transitive(op),
	part_np_mod_pp(op,voor),
	part_np_vp_obj1(op)])]).

v(troon,troont,tronen,getroond,troonde,troonden,
    [h([intransitive,
	np_ld_dir,
	ld_pp,
	ld_adv,
	np_ld_pp,
	part_transitive(mee)])]). % NB dat ik hem mee naar huis troon

v(troost,troost,troosten,getroost,troostte,troostten,
    [h([transitive,
	sbar_subj_so_np,	% het troost ons dat ...
	dip_sbar,
	acc_np_dip_sbar,
	np_pc_pp(met),
        intransitive,
	refl_er_pp_sbar(met) % ik troost me ermee dat ...
	])]).

v(trotseer,trotseert,trotseren,getrotseerd,trotseerde,trotseerden,
    [h([transitive])]).

v(trouw,trouwt,trouwen,getrouwd,trouwde,trouwden,
    [z([pc_pp(met)]),
     b([transitive,  % zij zijn een Hollandse vrouw getrouwd
	intransitive])]).

v(tsjirp,tsjirpt,tsjirpen,getsjirpt,tsjirpte,tsjirpten,
    [h([intransitive])]).

v(tuchtig,tuchtigt,tuchtigen,getuchtigd,tuchtigde,tuchtigden,
    [h([transitive])]).

v(tuf,tuft,tuffen,getuft,tufte,tuften,
    [h([intransitive]),
     z([ld_pp,
	ld_dir
       ])]).

v(tuig,tuigt,tuigen,getuigd,tuigde,tuigden,
    [h([part_transitive(af),
	part_transitive(op)])]).

v(tuimel,tuimelt,tuimelen,getuimeld,tuimelde,tuimelden,
    [unacc([intransitive,
            part_intransitive(om),
            ld_pp,
            ld_dir])]).

v(tuin,tuint,tuinen,getuind,tuinde,tuinden,
    [z([pc_pp(in),
        part_pc_er_transitive(in)])]).

v(tuinier,tuiniert,tuinieren,getuinierd,tuinierde,tuinierden,
    [h([intransitive])]).

v(tuit,tuit,tuiten,getuit,tuitte,tuitten,
    [h([intransitive,
	transitive])]).

v(turf,turft,turven,geturfd,turfde,turfden,
    [h([transitive,
        intransitive,
	sbar  % we turven hoeveel mensen binnen komen
       ])]).

v(turn,turnt,turnen,geturnd,turnde,turnden,
      [h([intransitive,
	  transitive])]).

v(tut,tut,tutten,getut,tutte,tutten,
    [h([part_transitive(op),
	intransitive])]).  

v(tutoyeer,tutoyeert,tutoyeren,getutoyeerd,tutoyeerde,tutoyeerden,
    [h([intransitive,
	transitive])]).

v(tuur,tuurt,turen,getuurd,tuurde,tuurden,
    [h([intransitive,
	ld_dir,  % we turen het bos in ; de weg af
	ld_pp])]).

v(tweet,tweet,tweeten,getweet,tweette,tweetten,
    [h([intransitive,
	sbar,
	np_sbar,
	transitive])]).

v(twijfel,twijfelt,twijfelen,getwijfeld,twijfelde,twijfelden,
    [h([intransitive,
	sbar,
	pc_pp(aan),
	er_pp_sbar(aan),
	pc_pp(over),
	er_pp_sbar(over),
	pc_pp(tussen)])]).

v(twinkel,twinkelt,twinkelen,getwinkeld,twinkelde,twinkelden,
    [h([intransitive])]).

v(twist,twist,twisten,getwist,twistte,twistten,
    [h([intransitive,
	pc_pp(om),
	er_pp_sbar(om),
	pc_pp(over),
	er_pp_sbar(over)])]).

v(twitter,twittert,twitteren,getwitterd,twitterde,twitterden,
    [h([intransitive,
	sbar,
	np_sbar,
	mod_pp(over),
	transitive])]).

v(typ,typt,typen,getypt,typte,typten,
    [h([intransitive,
	transitive,
	mod_pp(op),
	np_mod_pp(op),
	part_transitive(in),
	part_transitive(na),
	part_transitive(uit),
	part_transitive(over),
	sbar])]).

v(typeer,typeert,typeren,getypeerd,typeerde,typeerden,
    [h([sbar_subj_so_np,
	transitive])]).

v(uit,uit,uiten,geuit,uitte,uitten,
    [h([refl,
	fixed([{[acc(kritiek),pc(op)]}],norm_passive),
	fixed([{[acc(kritiek),er_pp(op,C)]},extra_sbar(C)],norm_passive),
        np_pc_pp(over),  % ongenoegen misprijzen bezwaren meningen bedenkingen
	transitive,
        sbar,
	refl_pc_pp(in)])]).

v(uniformeer,uniformeert,uniformeren,geüniformeerd,uniformeerde,uniformeerden,
    [h([sbar_subj_so_np,
	transitive])]).

v(update,[updatet,update],updaten,[geüpdate,'ge-update',geupdate,geüpdatet,'ge-updatet',geupdatet],[updatete,update],[updateten,updaten],
    [h([intransitive,
	transitive])]).

v(upgrade,upgradet,upgraden,geüpgraad,[upgrade,upgradede],[upgraden,upgradeden],
    [h([intransitive,
	transitive,
	ld_pp])]).

v(upload,uploadt,uploaden,[geüpload,'ge-upload'],uploadde,uploadden,
    [h([transitive,
        intransitive,
        np_ld_pp])]).

v(urineer,urineert,urineren,geürineerd,urineerde,urineerden,
    [h([intransitive])]).

v(vaag,vaagt,vagen,gevaagd,vaagde,vaagden,
    [h([np_ld_pp,
        part_transitive(weg),
	part_np_ld_pp(weg)])]).

v(vaar,vaart,varen,gevaren,[voer,vaarde],[voeren,vaarden],
    [z([part_pc_pp(uit,tegen),
	part_intransitive(aan),	% with "komen"
	part_intransitive(af),  % we varen pas morgen af
	part_intransitive(over),
	part_ld_pp(over)]),
     h([transitive,
	np_ld_pp,   % we hebben de punt er helemaal af gevaren
        pc_pp(op),  % varen op stookolie/kernenergie
	part_transitive(aan),
	part_transitive(over)]),
     b([intransitive,
        part_intransitive(mee),
	part_intransitive(om),  
	part_intransitive(rond),
	part_intransitive(uit),
	part_ld_pp(uit),
	ld_dir,
        fixed([svp_er_pp(bij),[wel]],no_passive),
	fixed([[blind],pc(op)],imp_passive),
	ld_pp
       ])]).

v(vaardig,vaardigt,vaardigen,gevaardigd,vaardigde,vaardigden,
    [h([part_transitive(af),
	part_transitive(uit),
	part_np_ld_pp(af)])]).

v(vaccineer,vaccineert,vaccineren,gevaccineerd,vaccineerde,vaccineerden,
    [h([transitive,
        intransitive,
	np_pc_pp(tegen),
        pc_pp(tegen)])]).

v(val,valt,vallen,gevallen,viel,vielen,
    [unacc(
       [intransitive,
        ld_dir,
        te_passive,
        sbar_subj_te_passive,
	ld_adv,
        ld_pp,
	mod_pp(bij), % daar zijn veel slachtoffers bij gevallen
        so_np_ld_pp,
                                % valt ons (rauw) op het dak
                                %      elkaar in de armen
	fixed([ap_pred,sbar_subj],no_passive), % het valt slecht dat ..
        part_intransitive(af),
	part_intransitive(binnen),
	part_intransitive(dood),
	part_intransitive(droog),
	part_intransitive(flauw),
	part_intransitive(in),
        part_intransitive(mee),
	part_intransitive(neer),
	part_intransitive(om),
	part_intransitive(op),
	part_intransitive(open),
	part_intransitive(samen),
	part_intransitive(stil),
	part_intransitive(tegen),
	part_intransitive(terug),
	part_intransitive(uit),
	part_dip_sbar(uit),
	part_ap_copula(uit),
	part_intransitive(bij),
	part_fixed(uit,[ap_pred,pc(voor)],no_passive),
	part_intransitive(uiteen),
	part_intransitive(voor),
	part_intransitive(weg),
	part_ld_transitive(af),	% hij valt de trap af
	part_transitive(af),    % we vallen hem af (of is dit obj2?)
	part_so_pp(toe),  % de prijs is aan hem toegevallen
	part_ld_pp(in),
	part_so_np(op),
	part_so_pp(op),
        part_mod_pp(op,tussen),       % zie daar maar tussen op te vallen
	part_sbar_subj(op),
	part_sbar_subj_no_het(op),
	part_dip_sbar_subj_no_het(op),   % we moeten , valt mij op , steeds langer wachten
	part_sbar_subj_so_np_opt_het(op),
	part_so_np(mee),
	part_np_np(mee), % het viel hem ALLES mee
	part_sbar_subj(mee),
	part_sbar_subj_so_np(mee),
	part_vp_subj(mee),
	part_vp_subj_so_np(mee),
	part_so_np(tegen),
	part_sbar_subj(tegen),
	part_sbar_subj_so_np(tegen),
	part_vp_subj(tegen),
	part_vp_subj_so_np(tegen),
	part_so_np(in),
	part_so_np(toe),
	pc_pp(aan),
	pc_pp(op),    % ik val op blond; de keuze viel op X
	pc_pp(voor),
	fixed([{[pp_pred(in,hand),pc(van)]}],no_passive),
	fixed([{[pp_pred(in,hand),dat]}],no_passive),
	fixed([[inslaap]],no_passive),  % really should be "in slaap"
	                                % which is treated as PREDC
        fixed([[in,de,smaak]],no_passive),
	fixed([[ten,prooi]],no_passive),
	fixed([[ten,prooi],dat],no_passive),
	fixed([[ten,prooi],dat_pp(aan)],no_passive),
	fixed([[door,de,mand]],no_passive),
	fixed([[te,beurt],dat],no_passive),
	fixed([[te,beurt],dat_pp(aan)],no_passive),
        fixed([[te,binnen],dat],no_passive),
        fixed([[te,binnen],dat,sbar_subj_opt_het],no_passive),
	fixed([acc(buil),refl],no_passive),
	fixed([{[acc(buil),pc(aan)]},refl],no_passive),

        nonp_copula,

        so_nonp_copula,    % het schrijven van deze brief valt mij zwaar
        so_nonp_copula_vp, % het valt mij niet gemakkelijk om deze brief te schrijven

	fixed([[te,moede],ap_pred,dat],no_passive),
	fixed([[ten,deel],dat],no_passive),
	fixed([[in,de,prijzen]],no_passive),
	fixed([[in,het,niet]],no_passive),
	fixed([{[mod_pp(bij),[in,het,niet]]}],no_passive),
	part_ld_pp(binnen),
	part_ld_pp(neer),
	part_ld_pp(terug),
	part_ld_pp(voor),
	part_pc_pp(af,van),
	part_pc_pp(dood,op),
	part_pc_pp(mee,met),
	part_pc_pp(mee,van),
	part_pc_pp(open,van),
	part_pc_pp(samen,met),
	part_pc_pp(tegen,van),
	part_pc_pp(uit,in),
	part_pc_pp(uit,tegen),
	part_pc_pp(uiteen,in),
        fixed([ap_pred('uit elkaar'),pc(in)],no_passive),
	part_pc_pp(weg,tegen)]),
     z([part_transitive(bij), % we zijn hem bijgevallen / hij werd bijgev.
        fixed([[uit,de,toon]],no_passive),
	part_transitive(binnen),
	part_pc_pp(terug,op)]),	
     h([part_intransitive(aan),
	part_transitive(aan),
	part_np_pc_pp(aan,op),  % iemand aanvallen op
	part_transitive(lastig),
	part_np_pc_pp(lastig,met),
	fixed([[in,de,rede],dip_sbar],norm_passive),
	fixed([[in,de,rede],acc,dip_sbar],norm_passive),
	fixed([[in,de,rede],acc],norm_passive)])]).

v(vang,vangt,vangen,gevangen,ving,vingen,
    [h([intransitive,
	transitive,
	np_ld_pp,
	part_sbar(op),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(in),
	part_transitive(op),
        part_np_mod_pp(op,in),
        part_np_mod_pp(op,van),
	part_transitive(uit),
        part_np_np(af),  % iemand vliegen afvangen
	part_np_pc_pp(aan,met),
	part_vp(aan),  % dat hij aanving een boek te lezen
	part_pc_pp(aan,met)]),
     b([part_intransitive(aan)])]).

v(valideer,valideert,valideren,gevalideerd,valideerde,valideerden,
    [h([transitive,
	intransitive])]).

v(varieer,varieert,variëren,gevarieerd,varieerde,varieerden,
    [h([transitive,
	pc_pp(op)]),
     b([intransitive])]).

v(vast,vast,vasten,gevast,vastte,vastten,
    [h([intransitive])]).

v(vat,vat,vatten,gevat,vatte,vatten,
    [h([transitive,
	np_ld_pp,
        sbar_obj,  % het is niet te vatten dat hij komt
	part_als_pred_np(op),
	part_als_pred_np_sbar(op),  % ik vat het als een beleding op dat ...
	part_als_pred_np_vp(op),  % ik vat het als een belemmering op om ...
	part_intransitive(post),
        part_intransitive(vlam),
	part_transitive(aan),
	part_transitive(op),
	part_transitive(samen),
	part_sbar(samen), % dip
	part_np_pc_pp(aan,met),
	part_np_pc_pp(samen,in)])]).

v(vecht,vecht,vechten,gevochten,vocht,vochten,
    [h([intransitive,
	part_transitive(aan),
	part_transitive(uit),
	part_intransitive(mee),
	part_intransitive(terug),
	part_refl(terug),  % hij vocht zich terug
	refl_ld_pp,        % hij vocht zich naar de overwinning
	refl_ld_dir,
	np_ld_dir,
	part_refl(in),
	part_refl_ld_pp(in),
	transitive,  % wedstrijd / robbertje
	pc_pp(met),
	pc_pp(om),
	pc_pp(over),
	pc_pp(tegen),
	pc_pp(voor),
	er_pp_sbar(voor),
	er_pp_vp(voor),
	part_np_pc_pp(uit,met)])]).

v(veeg,veegt,vegen,geveegd,veegde,veegden,
    [h([nonp_pred_np,
	np_np,
	intransitive,
	np_ld_dir,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	part_np_ld_pp(weg),
	part_fixed(uit,[[de,mantel],dat],imp_passive),
	part_transitive(aan),
	part_transitive(af),
        part_transitive(op),
	part_transitive(schoon),
	part_transitive(uit),
	part_transitive(weg)])]).

v(veel,veelt,velen,geveeld,veelde,veelden,
    [h([transitive,
	sbar_obj,
	np_pc_pp(van)])]).

v(veer,veert,veren,geveerd,veerde,veerden,
    [b([ld_pp,
        ld_dir, % overeind
	intransitive,
        part_intransitive(in),
	part_intransitive(op),
	part_intransitive(terug)])]).

v(veil,veilt,veilen,geveild,veilde,veilden,
    [h([transitive,
        intransitive])]).

v(veins,veinst,veinzen,geveinsd,veinsde,veinsden,
    [h([intransitive,
	sbar,
	transitive,
	vp])]).

v(vel,velt,vellen,geveld,velde,velden,
    [h([transitive,
	fixed([{[acc(oordeel),pc(over)]}],norm_passive)])]).

v(vent,vent,venten,gevent,ventte,ventten,
    [h([intransitive,
	part_intransitive(uit),
	part_transitive(uit),
	transitive])]).

v(ventileer,ventileert,ventileren,geventileerd,ventileerde,ventileerden,
    [h([intransitive,
	sbar,
	transitive,
	vp])]).

v(veraangenaam,veraangenaamt,veraangenamen,veraangenaamd,veraangenaamde,
  veraangenaamden,
    [h([transitive])]).

v(veracht,veracht,verachten,veracht,verachtte,verachtten,
    [h([transitive,
	sbar_obj])]).

v(verafgood,verafgoodt,verafgoden,verafgood,verafgoodde,verafgoodden,
    [h([transitive])]).

v(verafschuw,verafschuwt,verafschuwen,verafschuwd,verafschuwde,verafschuwden,
    [h([sbar_obj,
	transitive])]).

v(verander,verandert,veranderen,veranderd,veranderde,veranderden,
    [unacc([intransitive,
	als_pred_np,
	sbar_subj_so_np,
	pc_pp(in),
	pc_pp(van)]),
     h([transitive,
	np_pc_pp(in),
        pc_pp(aan),  % daar verandert niets aan
	np_pc_pp(aan)])]).  % daar verandert hij helemaal niets aan

v(veranker,verankert,verankeren,verankerd,verankerde,verankerden,
    [h([transitive,
	np_ld_pp])]).

v(verantwoord,verantwoordt,verantwoorden,verantwoord,verantwoordde,verantwoordden,
    [h([% refl,
	transitive,
	sbar_obj,
	sbar,
        so_pp_np,
        np_np,
	np_pc_pp(tegenover),
	% refl_pc_pp(tegenover),
	refl_pc_pp(voor)])]).

v(verarm,verarmt,verarmen,verarmd,verarmde,verarmden,
    [unacc([intransitive]),
     h([transitive])]).

v(verbaas,verbaast,verbazen,verbaasd,verbaasde,verbaasden,
    [h([sbar_subj_so_np,
	sbar_subj,
	so_np,
	intransitive,  % hij blijft verbazen
	vp_subj_so_np,
	refl_sbar,
	refl_er_pp_sbar(over),
	refl_pc_pp(over)])]).

v(verban,verbant,verbannen,verbannen,verbande,verbanden,
    [h([transitive,
	np_ld_pp])]).

v(verbaster,verbastert,verbasteren,verbasterd,verbasterde,verbasterden,
    [z([intransitive]),
     h([transitive,
	np_pc_pp(tot)])]).

v(verbeeld,verbeeldt,verbeelden,verbeeld,verbeeldde,verbeeldden,
    [h([refl_np,
	refl_sbar,
	refl_vp,
	transitive])]).

v(verberg,verbergt,verbergen,verborgen,verborg,verborgen,
    [h([sbar,
	transitive,
	vp,
	refl,
	refl_ld_pp,
	refl_ld_adv,  % also refl, since refl < su: daarachter verbergen zich de X
	np_ld_pp,
	np_ld_adv])]).

v(verbeter,verbetert,verbeteren,verbeterd,verbeterde,verbeterden,
    [unacc([intransitive,
	    fixed([er_pp(op)],no_passive),
		% de situatie is er niet op verbeterd
	    sbar_subj_so_np]),
     h([np_np,
	dip_sbar,
	acc_np_dip_sbar,
        np_pc_pp(aan),
	transitive])]).

v(verbied,verbiedt,verbieden,verboden,verbood,verboden,
    [h([np_np,
	np_vp_obj,     % ik verbied     hem om te komen
        so_np_vp_obj,  % ik verbied het hem om te komen
        vp_obj,
	sbar,
	np_sbar,
	transitive,
	vp])]).

v(verbijster,verbijstert,verbijsteren,verbijsterd,verbijsterde,verbijsterden,
    [h([sbar_subj_so_np,
	transitive,
        intransitive,
	vp_subj_so_np])]).

v(verbijt,verbijt,verbijten,verbeten,verbeet,verbeten,
    [h([refl,
	transitive,
	refl_sbar,
	refl_pc_pp(over)])]).

v(verbind,verbindt,verbinden,verbonden,verbond,verbonden,
    [h([transitive,
	intransitive, % Bea: cultuur verbindt
	np_pc_pp(aan),
	np_pc_pp(met),
	pc_pp(met),   % hoe kan ik verbinden met een draadloos netwerk
        part_transitive(door), % ik verbind u door!
	refl_pc_pp(tot),
	refl_er_pp_sbar(tot),
	refl_er_pp_vp(tot)])]).

v(verbitter,verbittert,verbitteren,verbitterd,verbitterde,verbitterden,
    [b([intransitive]),
     h([so_np,
	intransitive,
	sbar_subj_so_np,
	vp_subj_so_np,
	np_pc_pp(tegen)])]).

v(verbleek,verbleekt,verbleken,verbleekt,verbleekte,verbleekten,
    [unacc([intransitive,
	pc_pp(bij)])]).

v(verblijd,verblijdt,verblijden,verblijd,verblijdde,verblijdden,
    [h([sbar_subj_so_np,
	transitive,
	np_pc_pp(met),
	refl_pc_pp(over)])]).

v(verblijf,verblijft,verblijven,verbleven,verbleef,verbleven,
    [b([intransitive,
	ld_pp,
	ld_adv])]).

v(verblik,verblikt,verblikken,verblikt,verblikte,verblikten,
    [h([intransitive,
        pc_pp(van)])]).

v(verblind,verblindt,verblinden,verblind,verblindde,verblindden,
    [h([sbar_subj_so_np,
	transitive,
        intransitive
       ])]).

v(verbloem,verbloemt,verbloemen,verbloemd,verbloemde,verbloemden,
    [h([sbar,
	transitive])]).

v(verbloos,verbloost,verblozen,verbloosd,verbloosde,verbloosden,
    [h([intransitive,
        pc_pp(van)])]).

v(verbluf,verbluft,verbluffen,verbluft,verblufte,verbluften,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(verbouw,verbouwt,verbouwen,verbouwd,verbouwde,verbouwden,
    [h([transitive,
	np_mod_pp(op),  % tarwe verbouwen op
        intransitive])]).

v(verbrand,verbrandt,verbranden,verbrand,verbrandde,verbrandden,
    [unacc([intransitive]),
     h([transitive])]).

v(verbras,verbrast,verbrassen,verbrast,verbraste,verbrasten,
    [h([transitive])]).

v(verbreed,verbreedt,verbreden,verbreed,verbreedde,verbreedden,
    [h([refl,
	transitive,
	intransitive]),
     b([sbar_subj_so_np])]).

v(verbreek,verbreekt,verbreken,verbroken,verbrak,verbraken,
  [h([transitive,
      acc_np_dip_sbar % stilte, gepeins, impasse, trance
     ])]).

v(verbreid,verbreidt,verbreiden,verbreid,verbreidde,verbreidden,
    [h([refl,
	transitive])]).

v(verbrijzel,verbrijzelt,verbrijzelen,verbrijzeld,verbrijzelde,verbrijzelden,
  [h([transitive,
      intransitive])]).

v(verbroeder,verbroedert,verbroederen,verbroederd,verbroederde,verbroederden,
    [z([intransitive]),
     h([refl])]).

v(verbrod,verbrodt,verbrodden,verbrod,verbrodde,verbrodden,
    [h([transitive])]).

v(verbrokkel,verbrokkelt,verbrokkelen,verbrokkeld,verbrokkelde,verbrokkelden,
    [unacc([intransitive]),
     h([transitive])]).

v(verbrui,verbruit,verbruien,verbruid,verbruide,verbruiden,
    [h([transitive])]).

v(verbruik,verbruikt,verbruiken,verbruikt,verbruikte,verbruikten,
    [h([transitive])]).

v(verbuig,verbuigt,verbuigen,verbogen,verboog,verbogen,
    [h([transitive])]).

v(verchroom,verchroomt,verchromen,verchroomd,verchroomde,verchroomden,
    [h([transitive])]).

v(verdaag,verdaagt,verdagen,verdaagd,verdaagde,verdaagden,
    [h([transitive])]).

v(verdamp,verdampt,verdampen,verdampt,verdampte,verdampten,
    [unacc([intransitive]),
     h([transitive])]).

v(verdedig,verdedigt,verdedigen,verdedigd,verdedigde,verdedigden,
    [h([transitive,
	intransitive,
	part_intransitive(uit),
	part_transitive(uit),
        pc_pp(op),
        np_pc_pp(op),
	sbar,
        acc_np_dip_sbar,
	np_pc_pp(tegen)])]).

v(verdeel,verdeelt,verdelen,verdeeld,verdeelde,verdeelden,
    [h([refl,
	sbar_subj_so_np,
	transitive,
        intransitive,
	np_pc_pp(in),
	np_pc_pp(onder),
	np_pc_pp(over),
        part_np_pc_pp(onder,in),
	refl_pc_pp(in)])]).

v(verdelg,verdelgt,verdelgen,verdelgd,verdelgde,verdelgden,
    [h([transitive])]).

v(verdenk,verdenkt,verdenken,verdacht,verdacht,verdachten,
    [h([transitive,
	np_er_pp_sbar(van),
	np_pc_pp(van),
	np_vp_obj1,  % hij werd verdacht een spion te zijn
	obj_np_er_pp_vp(van),
	np_er_pp_sbar(van)])]).

v(verderf,verderft,verderven,verdorven,verdierf,verdierven,
    [h([sbar_subj_so_np,
	transitive])]).

v(verdicht,verdicht,verdichten,verdicht,verdichtte,verdichtten,
    [h([refl,
	transitive,
	refl_pc_pp(tot)])]).

v(verdien,verdient,verdienen,verdiend,verdiende,verdienden,
    [h([transitive,
	intransitive,
	np_pc_pp(aan),
        pc_pp(aan), % we verdienen er niet aan
	np_pc_pp(met),
	np_pc_pp(op),
	pc_pp(op),  % dat wil niet zeggen dat ik er op verdien
	vp,
	vp_obj,
	sbar_obj,
	fixed([sbar_subj,acc(lof)],no_passive),	% het verdient alle lof, dat...
	fixed([vp_subj,acc(aanbeveling)],no_passive), % het verdient aanbeveling om altijd ...
	part_intransitive(bij),
	part_transitive(bij),
	part_transitive(in),
	part_transitive(terug)])]).

v(verdiep,verdiept,verdiepen,verdiept,verdiepte,verdiepten,
    [h([transitive,
        refl,
	refl_pc_pp(in)])]).

v(verdik,verdikt,verdikken,verdikt,verdikte,verdikten,
    [unacc([intransitive]),
     h([refl,
	transitive])]).

v(verdisconteer,verdisconteert,verdisconteren,verdisconteerd,verdisconteerde,verdisconteerden,
    [h([sbar,
	transitive])]).

v(verdiskonteer,verdiskonteert,verdiskonteren,verdiskonteerd,verdiskonteerde,verdiskonteerden,
    [h([sbar,
	transitive])]).

v(verdobbel,verdobbelt,verdobbelen,verdobbeld,verdobbelde,verdobbelden,
    [h([transitive])]).

v(verdoe,verdoet,inflected(verdoen,verdoene),verdaan,verdeed,verdeden,
    [h([% refl,
	transitive,
	np_pc_pp(met)])]).

v(verdoem,verdoemt,verdoemen,verdoemd,verdoemde,verdoemden,
    [h([transitive])]).

v(verdoezel,verdoezelt,verdoezelen,verdoezeld,verdoezelde,verdoezelden,
    [h([intransitive,
	sbar,
	transitive,
	vp])]).

v(verdom,verdomt,verdommen,verdomd,verdomde,verdomden,
    [h([np_np,
	sbar_subj_so_np,
	transitive,
	vp_obj,
	vp])]).

v(verdonker,verdonkert,verdonkeren,verdonkerd,verdonkerde,verdonkerden,
    [unacc([intransitive]),
     h([% refl,
	transitive])]).

v(verdonkeremaan,verdonkeremaant,verdonkeremanen,verdonkeremaand,verdonkeremaande,verdonkeremaanden,
    [h([transitive])]).

v(verdoof,verdooft,verdoven,verdoofd,verdoofde,verdoofden,
    [unacc([intransitive]),
     h([transitive])]).

v(verdor,verdort,verdorren,verdord,verdorde,verdorden,
    [unacc([intransitive]),
     h([transitive])]).

v(verdraag,verdraagt,verdragen,verdragen,verdroeg,verdroegen,
    [h([sbar,
	transitive,
	sbar_obj,
	vp_obj,
	vp,
	refl_pc_pp(met)])]).

v(verdraai,verdraait,verdraaien,verdraaid,verdraaide,verdraaiden,
    [z([intransitive]),
     h([vp,
	transitive])]).

v(verdriet,verdriet,verdrieten,verdroten,verdroot,verdroten,
    [h([sbar_subj_so_np,
	transitive]),
     b([vp_subj_so_np])]).

v(verdrijf,verdrijft,verdrijven,verdreven,verdreef,verdreven,
    [h([transitive,
	np_ld_pp,
	np_pc_pp(met)])]).

v(verdring,verdringt,verdringen,verdrongen,verdrong,verdrongen,
    [h([refl,
	sbar_subj_so_np,
	transitive,
	np_ld_pp,
	refl_ld_pp,
	refl_ld_adv])]).

v(verdrink,verdrinkt,verdrinken,verdronken,verdronk,verdronken,
  [unacc([intransitive,
          ld_pp,
          ld_adv]),
   h([transitive,
      np_ld_pp,
      np_ld_adv])]).

v(verdroog,verdroogt,verdrogen,verdroogd,verdroogde,verdroogden,
    [unacc([intransitive]),
     h([transitive])]).

v(verdruk,verdrukt,verdrukken,verdrukt,verdrukte,verdrukten,
    [h([transitive])]).

v(verdubbel,verdubbelt,verdubbelen,verdubbeld,verdubbelde,verdubbelden,
    [unacc([intransitive]),
     h([transitive])]).

v(verduidelijk,verduidelijkt,verduidelijken,verduidelijkt,verduidelijkte,verduidelijkten,
    [h([sbar,
	transitive,
	intransitive])]).

v(verduister,verduistert,verduisteren,verduisterd,verduisterde,verduisterden,
    [unacc([intransitive]),
     h([transitive])]).

v(verdun,verdunt,verdunnen,verdund,verdunde,verdunden,
    [unacc([intransitive]),
     h([transitive,
	np_pc_pp(met)])]).

v(verduur,verduurt,verduren,verduurd,verduurde,verduurden,
    [h([transitive])]).

v(verduurzaam,verduurzaamt,verduurzamen,verduurzaamd,
  verduurzaamde,verduurzaamden,
    [h([transitive])]).

v(verdwaal,verdwaalt,verdwalen,verdwaald,verdwaalde,verdwaalden,
    [unacc([intransitive,
	ld_pp,
	ld_adv])]).

v(verdwaas,verdwaast,verdwazen,verdwaasd,verdwaasde,verdwaasden,
    [unacc([intransitive])]).

v(verdwijn,verdwijnt,verdwijnen,verdwenen,verdween,verdwenen,
    [unacc([intransitive,
	    fixed([[in,het,niet]],no_passive),
	    ld_pp,
	    ld_adv,
	    ld_dir])]).

v(veredel,veredelt,veredelen,veredeld,veredelde,veredelden,
    [h([transitive])]).

v(vereen,vereent,verenen,vereend,vereende,vereenden,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(vereenvoudig,vereenvoudigt,vereenvoudigen,vereenvoudigd,vereenvoudigde,vereenvoudigden,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(vereenzaam,vereenzaamt,vereenzamen,vereenzaamd,vereenzaamde,vereenzaamden,
    [unacc([intransitive])]).

v(vereenzelvig,vereenzelvigt,vereenzelvigen,vereenzelvigd,vereenzelvigde,vereenzelvigden,
    [h([intransitive,
	transitive,
	% refl,
	np_pc_pp(met)
	% refl_pc_pp(met)
       ])]).

v(vereer,vereert,vereren,vereerd,vereerde,vereerden,
    [h([transitive,
	np_pc_pp(met)])]).

v(vereeuwig,vereeuwigt,vereeuwigen,vereeuwigd,vereeuwigde,vereeuwigden,
    [h([transitive,
	np_pc_pp(met)])]).

v(vereffen,vereffent,vereffenen,vereffend,vereffende,vereffenden,
    [h([sbar_subj_so_np,
	transitive])]).

v(vereis,vereist,vereisen,vereist,vereiste,vereisten,
    [h([sbar,
	transitive,
	vp_subj_so_np])]).

v(vereng,verengt,verengen,verengd,verengde,verengden,
    [h([refl,
	transitive,
	np_pc_pp(tot),
	refl_pc_pp(tot)])]).

v(verenig,verenigt,verenigen,verenigd,verenigde,verenigden,
    [unacc([intransitive]),
     h([sbar_subj_so_np,
	vp_subj_so_np,
	transitive,
	refl,
	np_pc_pp(met),
	refl_pc_pp(met)])]).

v(vererger,verergert,verergeren,verergerd,verergerde,verergerden,
    [unacc([intransitive]),
     h([sbar_subj_so_np,
	transitive,
	refl])]).

v(verf,verft,verven,geverfd,verfde,verfden,
    [h([ap_pred_np,
	intransitive,
	transitive])]).

v(verfijn,verfijnt,verfijnen,verfijnd,verfijnde,verfijnden,
    [h([transitive])]).

v(verfilm,verfilmt,verfilmen,verfilmd,verfilmde,verfilmden,
    [h([transitive])]).

v(verflauw,verflauwt,verflauwen,verflauwd,verflauwde,verflauwden,
    [unacc([intransitive])]).

v(verfoei,verfoeit,verfoeien,verfoeid,verfoeide,verfoeiden,
    [h([transitive])]).

v(verfomfaai,verfomfaait,verfomfaaien,verfomfaaid,verfomfaaide,verfomfaaiden,
    [unacc([intransitive]),
     h([transitive])]).

v(verfraai,verfraait,verfraaien,verfraaid,verfraaide,verfraaiden,
    [h([sbar_subj_so_np,
	transitive])]).

v(verfrans,verfranst,verfransen,verfranst,verfranste,verfransten,
  [unacc([intransitive]),
   h([transitive])  % hij heeft zijn naam verfranst
  ]).

v(verfris,verfrist,verfrissen,verfrist,verfriste,verfristen,
    [h([transitive])]).

v(verfrommel,verfrommelt,verfrommelen,verfrommeld,verfrommelde,verfrommelden,
    [h([transitive])]).

v(verg,vergt,vergen,gevergd,vergde,vergden,
    [h([transitive,
        np_np,
	np_pc_pp(van),
	sbar, % de urgentie van de situatie vergt dat ...
	pp_sbar(van)])]).  % je kunt van niemand vergen dat ...

v(verga,vergaat,inflected(vergaan,vergane),vergaan,verging,vergingen,
    [unacc([intransitive,
	fixed([het_subj,ap_pred,dat],no_passive), % het verging hem goed
	so_np])]).

v(vergaap,vergaapt,vergapen,vergaapt,vergaapte,vergaapten,
    [h([transitive,
	refl_pc_pp(aan)])]).

v(vergaar,vergaart,vergaren,vergaard,vergaarde,vergaarden,
    [h([transitive])]).

v(vergader,vergadert,vergaderen,vergaderd,vergaderde,vergaderden,
    [b([intransitive,  % waren vergaderd -> ouderwets
        mod_pp(met),
        mod_pp(over)])]).

v(vergal,vergalt,vergallen,vergald,vergalde,vergalden,
    [h([np_np,
	transitive])]).

v(vergaloppeer,vergaloppeert,vergalopperen,vergaloppeerd,
  vergaloppeerde,vergaloppeerden,
    [h([refl])]).

v(vergas,vergast,vergassen,vergast,vergaste,vergasten,
    [z([intransitive]),
     h([transitive])]).

v(vergast,vergast,vergasten,vergast,vergastte,vergastten,
    [h([np_pc_pp(aan),
	np_pc_pp(op)])]).

v(vergeef,vergeeft,vergeven,vergeven,vergaf,vergaven,
    [h([np_np,
	np_sbar,
	so_np_sbar_obj,   % ik vergeef het hem nooit dat ...
	sbar,
	transitive,
        intransitive  % je moet kunnen vergeven
       ])]).

v(vergeel,vergeelt,vergelen,vergeeld,vergeelde,vergeelden,
    [unacc([intransitive])]).

v(vergeet,vergeet,vergeten,vergeten,vergat,vergaten,vergete,
    [b([sbar,
	vp,			% omdat ze vergeten dat te zeggen
	fixed([mod_pp(bij),sbar],no_passive),  % VL
	subj_control(te),  % omdat ze dat vergeten te zeggen
	refl,
	transitive,
	np_mod_pp(bij),  % Men vergeet er zelfs de gepeperde rekening bij .
	intransitive % je moet leren te vergeten
       ])]).

v(vergeld,vergeldt,vergelden,vergolden,vergold,vergolden,
    [h([transitive,
        np_np % de heer zal het u vergelden
       ])]).

v(vergelijk,vergelijkt,vergelijken,vergeleken,vergeleek,vergeleken,vergelijke,
    [h([transitive,
        intransitive,
	np_pc_pp(met),
        pc_pp(met)])]).

v(vergemakkelijk,vergemakkelijkt,vergemakkelijken,vergemakkelijkt,vergemakkelijkte,vergemakkelijkten,
  [h([sbar_subj_so_np,
      transitive]),
   unacc([fixed([er_pp(op)],no_passive)
	 ])]).

v(vergenoeg,vergenoegt,vergenoegen,vergenoegd,vergenoegde,vergenoegden,
    [h([refl_pc_pp(met)])]).

v(vergewis,vergewist,vergewissen,vergewist,vergewiste,vergewisten,
    [h([refl_pc_pp(van),
	refl_er_pp_sbar(van),
	refl_er_pp_vp(van)])]).

v(vergezel,vergezelt,vergezellen,vergezeld,vergezelde,vergezelden,
    [h([transitive,
	np_pc_pp(op)])]).

v(vergiet,vergiet,vergieten,vergoten,vergoot,vergoten,
    [h([transitive])]).

v(vergiftig,vergiftigt,vergiftigen,vergiftigd,vergiftigde,vergiftigden,
    [h([transitive])]).

v(vergis,vergist,vergissen,vergist,vergiste,vergisten,
    [h([refl,
	refl_pc_pp(bij),
	refl_pc_pp(in)])]).

v(vergist,vergist,vergisten,vergist,vergistte,vergistten,
    [h([transitive])]).  % van afval

v(verglijd,verglijdt,verglijden,vergleden,vergleed,vergleden,
  [unacc([intransitive,
	  ld_pp])]).

v(vergoed,vergoedt,vergoeden,vergoed,vergoedde,vergoedden,
    [h([np_np,
	sbar_subj_so_np,
	transitive])]).

v(vergoelijk,vergoelijkt,vergoelijken,vergoelijkt,vergoelijkte,vergoelijkten,
    [h([transitive,
        acc_np_dip_sbar
       ])]).

v(vergok,vergokt,vergokken,vergokt,vergokte,vergokten,
    [h([transitive])]).  

v(vergooi,vergooit,vergooien,vergooid,vergooide,vergooiden,
    [h([transitive])]).

v(vergrendel,vergrendelt,vergrendelen,vergrendeld,vergrendelde,vergrendelden,
    [h([transitive])]).

v(vergrijp,vergrijpt,vergrijpen,vergrepen,vergreep,vergrepen,
    [h([refl,
	refl_pc_pp(aan)])]).

v(vergrijs,vergrijst,vergrijzen,vergrijsd,vergrijsde,vergrijsden,
    [unacc([intransitive])]).

v(vergroei,vergroeit,vergroeien,vergroeid,vergroeide,vergroeiden,
    [unacc([intransitive,
	pc_pp(met)])]).

v(vergroot,vergroot,vergroten,vergroot,vergrootte,vergrootten,
    [h([intransitive,
	sbar_subj_so_np,
	transitive,
	part_transitive(uit)])]).

v(verguis,verguist,verguizen,verguisd,verguisde,verguisden,
    [h([transitive])]).

v(verguld,verguldt,vergulden,verguld,verguldde,verguldden,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np,
	np_pc_pp(met)])]).

v(vergun,vergunt,vergunnen,vergund,vergunde,vergunden,
    [h([np_np,
        so_np_vp_obj,  % ik vergun het hem om
	so_vp_obj,     % ik vergun hem om
        vp_obj,
        % het zij (mij) vergund nog enige opmerkingen te maken
	transitive])]).

v(verhaal,verhaalt,verhalen,verhaald,verhaalde,verhaalden,
    [h([transitive,
	np_pc_pp(op),
	np_pc_pp(over), % hoewel daar een stroom anecdotes over te verhalen is
	pc_pp(over),
	sbar,
	pc_pp(van)])]).

v(verhaar,verhaart,verharen,verhaard,verhaarde,verhaarden,
    [z([transitive])]).

v(verhaast,verhaast,verhaasten,verhaast,verhaastte,verhaastten,
    [h([sbar_subj_so_np,
	transitive])]).

v(verhakkel,verhakkelt,verhakkelen,verhakkeld,verhakkelde,verhakkelden,
    [h([transitive])]).

v(verhandel,verhandelt,verhandelen,verhandeld,verhandelde,verhandelden,
    [h([transitive])]).

v(verhang,verhangt,verhangen,verhangen,verhing,verhingen,
    [h([refl,
	transitive])]).

v(verhapstuk,verhapstukt,verhapstukken,verhapstukt,verhapstukte,verhapstukten,
    [h([intransitive,
	transitive])]).

v(verhard,verhardt,verharden,verhard,verhardde,verhardden,
    [unacc([intransitive]),
     h([refl,
	sbar_subj_so_np,
	transitive,
	np_pc_pp(tegen)])]).

v(verhaspel,verhaspelt,verhaspelen,verhaspeld,verhaspelde,verhaspelden,
    [h([transitive])]).

v(verheel,verheelt,verhelen,verheeld,verheelde,verheelden,
    [h([np_np,
	np_sbar,
	sbar,
	transitive,
	vp])]).

v(verheerlijk,verheerlijkt,verheerlijken,verheerlijkt,verheerlijkte,verheerlijkten,
    [h([transitive,
	np_pc_pp(met)])]).

v(verhef,verheft,verheffen,verheven,verhief,verhieven,
    [h([refl,
	transitive,
	np_pc_pp(tegen),
	np_ld_pp,
	refl_ld_pp,
	refl_pc_pp(op)])]).

v(verhelder,verheldert,verhelderen,verhelderd,verhelderde,verhelderden,
    [unacc([intransitive]),
     h([transitive])]).

v(verhelp,verhelpt,verhelpen,verholpen,verhielp,verhielpen,
    [h([transitive,
	fixed_dep(intransitive),
	np_pc_pp(aan),
	pc_pp(aan)		% VLAAMS Camerabewaking moet daaraan verhelpen .
       ])]).

v(verheug,verheugt,verheugen,verheugd,verheugde,verheugden,
    [h([sbar_subj_so_np,
	vp_subj_so_np,
	so_np,
	refl_pc_pp(in),
	refl_pc_pp(op),
	refl_er_pp_sbar(op),
	refl_er_pp_vp(op),
	refl_pc_pp(over),
	refl_er_pp_sbar(over)])]).

v(verhevig,verhevigt,verhevigen,verhevigd,verhevigde,verhevigden,
    [unacc([intransitive]),
     h([sbar_subj_so_np,
	transitive])]).

v(verhinder,verhindert,verhinderen,verhinderd,verhinderde,verhinderden,
    [h([np_vp_obj,
	sbar_subj_so_np,
	sbar,
	transitive])]).

v(verhit,verhit,verhitten,verhit,verhitte,verhitten,
    [h([sbar_subj_so_np,
	transitive,
	intransitive])]).

v(verhoed,verhoedt,verhoeden,verhoed,verhoedde,verhoedden,
    [h([transitive,
	sbar])]).

v(verhonger,verhongert,verhongeren,verhongerd,verhongerde,verhongerden,
    [unacc([intransitive]),
     h([transitive])]).

v(verhoog,verhoogt,verhogen,verhoogd,verhoogde,verhoogden,
  [h([transitive,
      ld_pp,
      np_ld_pp,
      sbar_subj,	      % het verhoogde de spanning dat ...
      intransitive	      % west verhoogde, en oost bood de manche
     ])]).

v(verhoop,verhoopt,verhopen,verhoopt,verhoopte,verhoopten,
    [h([transitive,
        intransitive,
        np_pc_pp(van),
        sbar,
	vp])]).

v(verhoor,verhoort,verhoren,verhoord,verhoorde,verhoorden,
    [h([transitive])]).

v([verhoud,verhou],verhoudt,verhouden,verhouden,verhield,verhielden,
    [h([refl,
	refl_pc_pp(tot)])]).

v(verhuis,verhuist,verhuizen,verhuisd,verhuisde,verhuisden,
    [z([ld_dir,
	ld_pp]),
     h([np_ld_dir,
	transitive]),
     b([intransitive])]).

v(verhul,verhult,verhullen,verhuld,verhulde,verhulden,
    [h([sbar,
	np_sbar,  % ik zal u niet verhullen dat ... (europarl)
	transitive,
        intransitive,
	vp])]).

v(verhuur,verhuurt,verhuren,verhuurd,verhuurde,verhuurden,
    [h([np_np,
	part_so_pp_np(door),
	part_transitive(door),   % hij koopt woningen om ze door te verhuren (aan arme mensen)
	intransitive,
	so_pp_np,
	transitive])]).

v(verifieer,verifieert,verifiëren,geverifieerd,verifieerde,verifieerden,
    [h([sbar,
	transitive])]).

v(verijdel,verijdelt,verijdelen,verijdeld,verijdelde,verijdelden,
    [h([sbar_subj_so_np,
        sbar,
	transitive])]).

v(verjaag,verjaagt,verjagen,verjaagd,[verjoeg,verjaagde],[verjoegen,verjaagden],
    [h([np_ld_dir,
	transitive,
	np_ld_pp])]).

v(verjaar,verjaart,verjaren,verjaard,verjaarde,verjaarden,
    [unacc([intransitive])]).  

v(verjong,verjongt,verjongen,verjongd,verjongde,verjongden,
    [h([transitive]),
     z([intransitive])]).

v(verkanker,verkankert,verkankeren,verkankerd,verkankerde,verkankerden,
    [h([transitive])]).

v(verkas,verkast,verkassen,verkast,verkaste,verkasten,
    [h([intransitive,
	ld_pp,
	transitive])]).

v(verkeer,verkeert,verkeren,verkeerd,verkeerde,verkeerden,
    [h([ld_pp,
        ap_copula('onder de indruk'),  % vanwege "erg onder de indruk"
        ap_copula('in staat'),
        pp_copula,
	sbar_subj, % zo kan het verkeren dat ...
	ld_adv]),
     b([intransitive])]).

v(verken,verkent,verkennen,verkend,verkende,verkenden,
  [h([transitive])]).

v(verkerf,verkerft,verkerven,verkorven,verkof,verkorven,
  [h([transitive])]).

v(verketter,verkettert,verketteren,verketterd,verketterde,verketterden,
    [h([transitive])]).

v(verkies,verkiest,verkiezen,[verkoren,verkozen],verkoos,verkozen,
    [h([als_pred_np,
	transitive,
	vp,
	np_pc_pp(boven),   % as in: "ze verkiezen vlak boven golvend terrein" how???
	np_pc_pp(tot),
	part_als_pred_np(uit),
	part_transitive(uit),
	part_np_pc_pp(uit,tot)])]).

v(verkijk,verkijkt,verkijken,verkeken,verkeek,verkeken,
    [h([refl,
	transitive,  % VL
	refl_pc_pp(bij),
	refl_pc_pp(op)])]).

v(verkil,verkilt,verkillen,verkild,verkilde,verkilden,
    [unacc([intransitive]),
     h([transitive])]).

v(verklaar,verklaart,verklaren,verklaard,verklaarde,verklaarden,
    [h([pred_np,
	refl,
	pred_refl,   % zij verklaarden zich eens met ...
	sbar,
	so_pp_sbar,
	transitive,
	part_transitive(dood),
        intransitive,
	vp,
        so_pp_vp,
	np_pc_pp(tot),	      % iemand tot ongewenst persoon verklaren
	refl_pc_pp(tegen),
	refl_pc_pp(voor),
	voor_pred_np,		% ik verklaar de zitting voor geopend
	fixed([voor_pred(gek),acc,sbar],norm_passive),   %de mensen verklaarden me voor gek dat ...
	
        fixed([vc(ben,te,passive),acc],no_passive) % only for Europarl:
                     % wij verklaren de zitting te zijn geopend
       ]),
     b([np_np,
	so_pp_np,
	pred_refl])]).

v(verklank,verklankt,verklanken,verklankt,verklankte,verklankten,
    [h([transitive])]).

v(verklap,verklapt,verklappen,verklapt,verklapte,verklapten,
    [h([np_np,
	np_sbar,
	sbar,
	transitive,
	vp])]).

v(verkleed,verkleedt,verkleden,verkleed,verkleedde,verkleedden,
    [h([als_pred_np,
	transitive])]).

v(verklein,verkleint,verkleinen,verkleind,verkleinde,verkleinden,
    [h([intransitive,
	sbar_subj_so_np,
	transitive])]).

v(verkleum,verkleumt,verkleumen,verkleumd,verkleumde,verkleumden,
    [unacc([intransitive])]).

v(verkleur,verkleurt,verkleuren,verkleurd,verkleurde,verkleurden,
    [unacc([intransitive])]).

v(verklik,verklikt,verklikken,verklikt,verklikte,verklikten,
    [h([transitive])]).

v(verkloot,verkloot,verkloten,verkloot,verklootte,verklootten,
    [h([transitive])]).

v(verknal,verknalt,verknallen,verknald,verknalde,verknalden,
    [h([transitive])]).

v(verkneukel,verkneukelt,verkneukelen,verkneukeld,verkneukelde,verkneukelden,
    [h([refl])]).

v(verknip,verknipt,verknippen,verknipt,verknipte,verknipten,
    [h([transitive])]).

v(verknoei,verknoeit,verknoeien,verknoeid,verknoeide,verknoeiden,
    [h([transitive])]).

v(verkoker,verkokert,verkokeren,verkokerd,verkokerde,verkokerden,
  [h([intransitive,
      transitive])]).

v(verkommer,verkommert,verkommeren,verkommerd,verkommerde,verkommerden,
    [unacc([intransitive])]).

v(verkondig,verkondigt,verkondigen,verkondigd,verkondigde,verkondigden,
  [h([sbar,
      np_mod_pp(over),
      transitive,
      so_pp_np,
      so_pp_sbar,
      vp])]).

v(verkool,verkoolt,verkolen,verkoold,verkoolde,verkoolden,
    [unacc([intransitive]),
     h([transitive])]).

v(verkoop,verkoopt,verkopen,verkocht,verkocht,verkochten,
    [h([np_np,
	so_pp_np,
	so_pp,          % China verkocht wel aan Europa, maar had zelf geen behoefte aan Europese zaken
	transitive,
	intransitive,   % dat verkoopt niet
	sbar_obj,       % we kunnen het niet verkopen, dat..
	so_pp_sbar_obj, % we kunnen het niet aan de achterban verkopen, dat
	so_np_sbar_obj, % we kunnen het onze achterban niet verkopen, dat
	np_pc_pp(voor),
	np_pc_pp(van),
	part_transitive(door),
        part_np_np(door),
	part_so_pp_np(door),
	part_transitive(uit),
	part_np_pc_pp(door,aan)])]).

v(verkort,verkort,verkorten,verkort,verkortte,verkortten,
    [z([intransitive]),
     h([transitive]),
     b([sbar_subj_so_np])]).

v(verkracht,verkracht,verkrachten,verkracht,verkrachtte,verkrachtten,
    [h([transitive,
        intransitive])]).

v(verkramp,verkrampt,verkrampen,verkrampt,verkrampte,verkrampten,
    [h([intransitive])]).

v(verkrap,verkrapt,verkrappen,verkrapt,verkrapte,verkrapten,
    [h([intransitive,
        transitive])]).

v(verkreukel,verkreukelt,verkreukelen,verkreukeld,verkreukelde,verkreukelden,
    [unacc([intransitive]),
     h([transitive])]).

v(verkrijg,verkrijgt,verkrijgen,verkregen,verkreeg,verkregen,
    [h([transitive,
        fixed([svp_pp(over,hart),het_pobj1(vp)],no_passive),
	sbar,			% VL ze verzet hemel en aarde om te verkrijgen dat daar iets aan gedaan wordt .
	pp_sbar(van),           % VL De VS willen nu van de Russen verkrijgen dat
        np_ld_pp
       ])]).

v(verkrop,verkropt,verkroppen,verkropt,verkropte,verkropten,
    [h([transitive,
	sbar,
	sbar_obj,
	vp_obj])]).

v(verkruimel,verkruimelt,verkruimelen,verkruimeld,verkruimelde,verkruimelden,
    [h([transitive]),
     b([intransitive])]).

v(verkwansel,verkwanselt,verkwanselen,verkwanseld,verkwanselde,verkwanselden,
    [h([transitive])]).

v(verkwik,verkwikt,verkwikken,verkwikt,verkwikte,verkwikten,
    [unacc([intransitive,
	pc_pp(met)]),
     h([transitive,
	np_pc_pp(met)])]).

v(verkwist,verkwist,verkwisten,verkwist,verkwistte,verkwistten,
    [h([transitive])]).

v(verlaag,verlaagt,verlagen,verlaagd,verlaagde,verlaagden,
    [h([transitive,
	intransitive,
	ld_pp,
	np_ld_pp
       ])]).

v(verlaat,verlaat,verlaten,verlaat,verlaatte,verlaatten,
    [h([refl])]).

v(verlaat,verlaat,verlaten,verlaten,verliet,verlieten,
    [h([transitive,
	refl_pc_pp(op)])]).

v(verlam,verlamt,verlammen,verlamd,verlamde,verlamden,
    [unacc([intransitive]),
     h([transitive])]).

v(verlang,verlangt,verlangen,verlangd,verlangde,verlangden,
    [h([sbar,
	transitive,
	vp,			% su control is not correct
	                        % we verlangen van de EU de democratie te stimuleren
	np_pc_pp(van),
	part_intransitive(terug),
	part_pc_pp(terug,naar),
	pc_pp(naar)])]).

v(verleen,verleent,verlenen,verleend,verleende,verleenden,
    [h([np_np,
	so_pp_np,
	transitive,
	np_pc_pp(bij)])]).

v(verleer,verleert,verleren,verleerd,verleerde,verleerden,
    [b([transitive])]).

v(verleg,verlegt,verleggen,verlegd,verlegde,verlegden,
    [h([transitive,
	np_ld_pp])]).

v(verleid,verleidt,verleiden,verleid,verleidde,verleidden,
    [h([transitive,
	intransitive,
        np_vp_obj,    % hij liet zich verleiden om te gaan slapen
        np_vp_obj1,   % hij werd verleid
	np_pc_pp(met),
	np_pc_pp(tot)])]).

v(verlekker,verlekkert,verlekkeren,verlekkerd,verlekkerde,verlekkerden,
  [h([refl,
      refl_pc_pp(bij),
      refl_pc_pp(over)
     ])]).

v(verleng,verlengt,verlengen,verlengd,verlengde,verlengden,
    [h([intransitive,
	sbar_subj_so_np,
	transitive])]).

v(verlevendig,verlevendigt,verlevendigen,verlevendigd,verlevendigde,verlevendigden,
    [z([intransitive]),
     h([sbar_subj_so_np,
	transitive])]).

v(verlicht,verlicht,verlichten,verlicht,verlichtte,verlichtten,
    [h([np_np,
	sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(verlies,verliest,verliezen,verloren,verloor,verloren,verlieze,
    [h([np_pc_pp(aan),		% veel geld verliezen aan deze crisis
        np_pc_pp(op),		% veel geld verliezen op
	pc_pp(aan),		% aan invloed verliezen
	mod_pp(bij),            % we kunnen er niets bij verliezen
	np_mod_pp(bij),         % we kunnen er geen cent bij verliezen
        pc_pp(op),              % we hebben op punten verloren
	pc_pp(van),             % we hebben van Feyenoord verloren
	np_pc_pp(van)		% de wedstrijd verliezen van Feyenoord
       ]),
     b([intransitive,           % we zijn/hebben verloren
	transitive,             % ik ben een schilderij verloren (cdb5827)
	refl_pc_pp(in),         % hij verliest zich in details
	                        % we hebben/zijn dat uit het oog verloren
	                        % we hebben/zijn uit het oog verloren dat...
	fixed([[uit,het,oog],acc],norm_passive),
	fixed([[uit,het,oog],sbar],imp_passive)])]).

%v(verlig,verligt,verliggen,verlegen,verlag,verlagen,
%    [b([intransitive])]).

v(verlijd,verlijdt,verlijden,verleden,verleed,verleden,
    [h([transitive])]).

v(verlijm,verlijmt,verlijmen,verlijmd,verlijmde,verlijmden,
    [h([transitive])]).

v(verlink,verlinkt,verlinken,verlinkt,verlinkte,verlinkten,
    [h([transitive])]).

v(verloeder,verloedert,verloederen,verloederd,verloederde,verloederden,
    [h([intransitive,
	transitive])]).

v(verlok,verlokt,verlokken,verlokt,verlokte,verlokten,
    [h([np_pc_pp(tot),
	transitive,
	np_vp_obj])]).

v(verloochen,verloochent,verloochenen,verloochend,verloochende,verloochenden,
    [h([% refl,
	transitive])]).

v(verloof,verlooft,verloven,verloofd,verloofde,verloofden,
    [h([intransitive,
	refl,
	refl_pc_pp(met)])]).

v(verloop,verloopt,verlopen,verlopen,verliep,verliepen,
    [unacc([intransitive])]).

v(verloot,verloot,verloten,verloot,verlootte,verlootten,
    [h([transitive,
	np_pc_pp(onder)])]).

v(verlos,verlost,verlossen,verlost,verloste,verlosten,
    [h([sbar_subj_so_np,
	transitive,
	np_pc_pp(uit),
	np_pc_pp(van)])]).

v(verlucht,verlucht,verluchten,verlucht,verluchtte,verluchtten,
    [h([transitive])]).

v(verluchtig,verluchtigt,verluchtigen,verluchtigd,verluchtigde,verluchtigden,
    [h([transitive])]).

v(verluid,verluidt,verluiden,verluid,verluidde,verluidden,
    [h([intransitive,
	transitive]),
     b([dip_sbar_subj_opt_het])]).

v(verlustig,verlustigt,verlustigen,verlustigd,verlustigde,verlustigden,
    [h([refl,
	refl_pc_pp(aan),
	refl_pc_pp(in)])]).

v(vermaak,vermaakt,vermaken,vermaakt,vermaakte,vermaakten,
    [h([np_np,
	sbar_subj_so_np,
	so_pp_np,
	transitive,
	vp_subj_so_np,
	np_pc_pp(met)
%	refl,  % ik vermaak me uitstekend!
%	refl_pc_pp(met)
       ])]).

v(vermaal,vermaalt,vermalen,vermalen,vermaalde,vermaalden,
    [h([transitive,
	np_pc_pp(tot)])]).

v(vermaan,vermaant,vermanen,vermaand,vermaande,vermaanden,
  [h([transitive,
      np_pc_pp(tot),
      pc_pp(tot), % de directeur vermaande (ons) tot kalmte
      acc_np_dip_sbar,
      dip_sbar])]).

v(vermag,vermag,vermogen,vermocht,vermocht,vermochten,
    [h([transitive,
	subj_control(te)])]).

v(vermager,vermagert,vermageren,vermagerd,vermagerde,vermagerden,
    [unacc([intransitive]),
     h([sbar_subj_so_np,
	transitive])]).

v(verman,vermant,vermannen,vermand,vermande,vermanden,
    [h([refl])]).

v(vermark,vermarkt,vermarken,vermarkt,vermarkte,vermarkten,
    [h([transitive])]).

v(vermarkt,vermarkt,vermarkten,vermarkt,vermarktte,vermarktten,
    [h([transitive])]).

v(vermeerder,vermeerdert,vermeerderen,vermeerderd,vermeerderde,vermeerderden,
    [h([intransitive,
	refl,
	transitive])]).

v(vermei,vermeit,vermeien,vermeid,vermeide,vermeiden,
    [h([refl,
	refl_ld_pp])]).
  

v(vermeld,vermeldt,vermelden,vermeld,vermeldde,vermeldden,
    [h([sbar,
	transitive,
        als_pred_np,
	np_mod_pp(over),
	np_mod_pp(in),
	np_pc_pp(bij),  % maar dat wordt er niet bij vermeld
	pp_sbar(bij),   % hij vermeldde er niet bij, dat..
	vp])]).

v(vermeng,vermengt,vermengen,vermengd,vermengde,vermengden,
    [h([refl,
	transitive,
	np_pc_pp(met),
	refl_pc_pp(met)])]).

v(vermenigvuldig,vermenigvuldigt,vermenigvuldigen,vermenigvuldigd,vermenigvuldigde,vermenigvuldigden,
    [h([refl,
	transitive,
	np_pc_pp(met),
	intransitive])]).

v(vermijd,vermijdt,vermijden,vermeden,vermeed,vermeden,
    [h([sbar_subj_so_np,
	sbar,
	transitive,
	vp])]).

v(verminder,vermindert,verminderen,verminderd,verminderde,verminderden,
  [unacc([intransitive
	 ]),
     h([sbar_subj_so_np,
	transitive])]).

v(vermink,verminkt,verminken,verminkt,verminkte,verminkten,
    [h([transitive])]).

v(vermis,vermist,vermissen,vermist,vermiste,vermisten,
    [h([transitive])]).

v(vermoed,vermoedt,vermoeden,vermoed,vermoedde,vermoedden,
    [h([tr_sbar,
	van_sbar,
	transitive,
	intransitive, % "zijn gedrag deed anders vermoeden"
	vp])]).

v(vermoei,vermoeit,vermoeien,vermoeid,vermoeide,vermoeiden,
    [h([sbar_subj_so_np,
	transitive,
	np_pc_pp(met)])]).

v(vermom,vermomt,vermommen,vermomd,vermomde,vermomden,
    [h([als_pred_np,
	% refl,
	transitive])]).

v(vermoord,vermoordt,vermoorden,vermoord,vermoordde,vermoordden,
    [h([transitive])]).

v(vermorzel,vermorzelt,vermorzelen,vermorzeld,vermorzelde,vermorzelden,
    [h([transitive,
	np_pc_pp(met)])]).

v(vermurw,vermurwt,vermurwen,vermurwd,vermurwde,vermurwden,
    [h([intransitive,
	np_pc_pp(met),
	np_vp_obj,
	sbar_subj_so_np,
	vp_subj_so_np,
	transitive])]).

v(vernauw,vernauwt,vernauwen,vernauwd,vernauwde,vernauwden,
    [unacc([intransitive]),
     h([refl,
	transitive])]).

v(verneder,vernedert,vernederen,vernederd,vernederde,vernederden,
    [h([transitive])]).

v(vernederlands,vernederlandst,vernederlandsen,vernederlandst,vernederlandste,vernederlandsten,
    [h([transitive])]).

v(verneem,verneemt,vernemen,vernomen,vernam,vernamen,
    [h([sbar,
	np_pc_pp(van),
	pp_sbar(van),
        np_mod_pp(over),
        pc_pp(van),
	transitive])]).

v(verneuk,verneukt,verneuken,verneukt,verneukte,verneukten,
    [h([transitive])]).

v(vernevel,vernevelt,vernevelen,verneveld,vernevelde,vernevelden,
  [h([transitive,
      intransitive])]).

v(verniel,vernielt,vernielen,vernield,vernielde,vernielden,
  [h([transitive,
      intransitive])]).

v(vernietig,vernietigt,vernietigen,vernietigd,vernietigde,vernietigden,
  [h([transitive,
      intransitive])]).

v(vernieuw,vernieuwt,vernieuwen,vernieuwd,vernieuwde,vernieuwden,
    [h([transitive,
        intransitive])]).

v(vernis,vernist,vernissen,gevernist,verniste,vernisten,
    [h([transitive])]).

v(vernoem,vernoemt,vernoemen,vernoemd,vernoemde,vernoemden,
    [h([transitive,
	np_pc_pp(naar)])]).

v(veronachtzaam,veronachtzaamt,veronachtzamen,veronachtzaamd,veronachtzaamde,veronachtzaamden,
    [h([transitive])]).

v(veronderstel,veronderstelt,veronderstellen,verondersteld,veronderstelde,veronderstelden,
    [h([ap_pred_np,
	sbar,
	van_sbar,    % ik veronderstel van niet
	np_vp_obj1,  % only in passive? Zij worden verondersteld te komen
	transitive])]).

v(verongeluk,verongelukt,verongelukken,verongelukt,verongelukte,verongelukten,
    [unacc([intransitive])]).

v(verontreinig,verontreinigt,verontreinigen,verontreinigd,verontreinigde,verontreinigden,
    [h([transitive])]).

v(verontrust,verontrust,verontrusten,verontrust,verontrustte,verontrustten,
    [h([% refl,
	sbar_subj_so_np,
	transitive,
	vp_subj_so_np
	% refl_pc_pp(over)
       ])]).

v(verontschuldig,verontschuldigt,verontschuldigen,verontschuldigd,verontschuldigde,verontschuldigden,
    [h([% refl,
	% refl_sbar,  % mostly dip
        np_sbar,      % dip:  ik versliep me, verontschuldigde hij zijn late aankomst
	sbar_subj_so_np,  % ?
	transitive,
	refl_pc_pp(voor)])]).

v(verontwaardig,verontwaardigt,verontwaardigen,verontwaardigd,verontwaardigde,verontwaardigden,
    [h([transitive])]).

v(veroordeel,veroordeelt,veroordelen,veroordeeld,veroordeelde,veroordeelden,
    [h([sbar_obj,
	sbar,
	transitive,
	intransitive,
	np_pc_pp(tot),
	fixed([{[pc(tot),mod_pp(voor),acc]}],norm_passive),
	obj_np_er_pp_vp(tot),
	fixed([[ter,dood],acc],norm_passive)])]).

v(veroorloof,veroorlooft,veroorloven,veroorloofd,veroorloofde,veroorloofden,
    [h([np_np,
	np_vp_obj,
	fixed([dat,het_pobj1(vp)],imp_passive),
	fixed([dat,het_pobj1(sbar)],imp_passive),
        np_sbar,
	transitive,
	vp
       ])]).

v(veroorzaak,veroorzaakt,veroorzaken,veroorzaakt,veroorzaakte,veroorzaakten,
  [h([sbar,
      np_np,			% ik heb u leed veroorzaakt
      transitive])]).

v(verorber,verorbert,verorberen,verorberd,verorberde,verorberden,
    [h([transitive])]).

v(verordonneer,verordonneert,verordonneren,verordonneerd,verordonneerde,verordonneerden,
  [h([transitive,
      np_vp_obj1,
      acc_np_sbar,
      sbar])]).

v(verouder,veroudert,verouderen,verouderd,verouderde,verouderden,
    [unacc([intransitive])]).

v(verover,verovert,veroveren,veroverd,veroverde,veroverden,
  [h([transitive,
      refl_np, % hij veroverde zich een mooie positie
      part_transitive(terug),
      part_np_pc_pp(terug,op),
      np_pc_pp(op)])]).

v(verpacht,verpacht,verpachten,verpacht,verpachtte,verpachtten,
    [h([transitive,
	so_pp_np])]).

v(verpak,verpakt,verpakken,verpakt,verpakte,verpakten,
    [h([intransitive,
	transitive,
	np_pc_pp(in)])]).

v(verpand,verpandt,verpanden,verpand,verpandde,verpandden,
  [h([np_np,
      np_pc_pp(aan),  % mijn hart
      transitive])]).

v(verpats,verpatst,verpatsen,verpatst,verpatste,verpatsten,
    [h([transitive])]).

v(verpauper,verpaupert,verpauperen,verpauperd,verpauperde,verpauperden,
    [unacc([intransitive])]).

v(verpersoonlijk,verpersoonlijkt,verpersoonlijken,verpersoonlijkt,
  verpersoonlijkte,verpersoonlijkten,
    [h([transitive])]).

v(verpest,verpest,verpesten,verpest,verpestte,verpestten,
    [h([np_np,
	transitive])]).

v(verpieter,verpietert,verpieteren,verpieterd,verpieterde,verpieterden,
    [unacc([intransitive])]).

v(verplaats,verplaatst,verplaatsen,verplaatst,verplaatste,verplaatsten,
    [h([refl,
	transitive,
	np_ld_pp,
	refl_pc_pp(in),
	refl_ld_pp])]).

v(verplant,verplant,verplanten,verplant,verplantte,verplantten,
    [h([transitive])]).

v(verpleeg,verpleegt,verplegen,verpleegd,verpleegde,verpleegden,
    [h([transitive])]).

v(verpletter,verplettert,verpletteren,verpletterd,verpletterde,verpletterden,
    [h([transitive,
	vp_subj_so_np])]).

v(verplicht,verplicht,verplichten,verplicht,verplichtte,verplichtten,
    [h([np_vp_obj1,
	sbar_subj_so_np,
	sbar,
	transitive,
	np_pc_pp(aan),
	np_pc_pp(tot),
	so_np_pc_pp(tot),  % Daartoe verplicht ons het subsidiariteitsbeginsel .
	np_er_pp_sbar(tot),
	obj_np_er_pp_vp(tot),
	pc_pp(tot)])]).

v(verpoos,verpoost,verpozen,verpoosd,verpoosde,verpoosden,
    [h([refl,
        intransitive])]).

v(verpop,verpopt,verpoppen,verpopt,verpopte,verpopten,
    [h([refl,
        intransitive])]).

v(verpot,verpot,verpotten,verpot,verpotte,verpotten,
    [h([transitive])]).

v(verpruts,verprutst,verprutsen,verprutst,verprutste,verprutsten,
    [h([transitive])]).

v(verpulver,verpulvert,verpulveren,verpulverd,verpulverde,verpulverden,
    [unacc([intransitive]),
     h([transitive,
	np_pc_pp(tot)])]).

v(verraad,verraadt,verraden,verraden,[verraadde,verried],[verraadden,verrieden],
    [h([np_np,
	sbar_subj_so_np,
	sbar,
	vp,
	np_vp_subj,
	so_pp_np,
	transitive])]).

v(verras,verrast,verrassen,verrast,verraste,verrasten,
    [h([intransitive,
	transitive,
	sbar_subj_so_np,
	sbar_subj])]).

v(verregen,verregent,verregenen,verregend,verregende,verregenden,
    [unacc([intransitive])]).

v(verrek,verrekt,verrekken,verrekt,verrekte,verrekten,
    [unacc([intransitive]),
     h([transitive])]).

v(verreken,verrekent,verrekenen,verrekend,verrekende,verrekenden,
    [h([refl,
	pc_pp(met),
	np_pc_pp(met),
	transitive])]).

v(verrem,verremt,verremmen,verremd,verremde,verremden,
    [h([refl])]).

v(verricht,verricht,verrichten,verricht,verrichtte,verrichtten,
    [h([transitive,
	fixed([{[acc(onderzoek),pc(naar)]}],norm_passive),
	fixed([{[acc(onderzoek),er_pp(naar,C)]},extra_sbar(C)],
	      norm_passive)
       ])]).

v(verrijd,verrijdt,verrijden,verreden,verreed,verreden,
    [h([transitive])]).

v(verrijk,verrijkt,verrijken,verrijkt,verrijkte,verrijkten,
    [h([refl,
	sbar_subj_so_np,
	transitive,
	intransitive % bea: cultuur verrijkt.
       ])]).

v(verrijs,verrijst,verrijzen,verrezen,verrees,verrezen,
    [unacc([intransitive,
	ld_pp])]).

v(verroer,verroert,verroeren,verroerd,verroerde,verroerden,
  [h([transitive,
      intransitive		% VL
     ])]).

v(verroest,verroest,verroesten,verroest,verroestte,verroestten,
    [unacc([intransitive])]).

v(verrot,verrot,verrotten,verrot,verrotte,verrotten,
    [unacc([np_np,
	intransitive])]).

v(verruil,verruilt,verruilen,verruild,verruilde,verruilden,
    [h([np_pc_pp(voor),
	transitive,
	pc_pp(van)])]).

v(verruim,verruimt,verruimen,verruimd,verruimde,verruimden,
    [unacc([intransitive]),
     h([sbar_subj_so_np,
	transitive])]).

v(verruk,verrukt,verrukken,verrukt,verrukte,verrukten,
    [h([sbar_subj_so_np,
	transitive])]).

v(verruw,verruwt,verruwen,verruwd,verruwde,verruwden,
    [h([intransitive,
	transitive])]).

v(versaag,versaagt,versagen,versaagd,versaagde,versaagden,
    [h([intransitive,
	transitive])]).

v(verschaal,verschaalt,verschalen,verschaald,verschaalde,verschaalden,
    [unacc([intransitive])]).

v(verschaf,verschaft,verschaffen,verschaft,verschafte,verschaften,
    [h([np_np,
	so_pp_np,
	%% informatie zekerheid opheldering uitleg over ..
	np_np_mod_pp(over),
	fixed([{[pc(tot),acc(toegang)]}],norm_passive),
        fixed([pc(tot),acc(toegang),dat],norm_passive),
       	fixed([inv(acc(toegang)),pc(tot),dat],norm_passive),
       	fixed([acc(toegang),inv(dat),pc(tot)],norm_passive),
	transitive])]).

v(verschakel,verschakelt,verschakelten,verschakeld,verschakelde,verschakelden,
  [h([refl])]).

v(verschalk,verschalkt,verschalken,verschalkt,verschalkte,verschalkten,
    [h([transitive])]).

v(verschans,verschanst,verschansen,verschanst,verschanste,verschansten,
    [h([refl,
	refl_ld_adv,
	refl_ld_pp,
	transitive])]).

v(verscheep,verscheept,verschepen,verscheept,verscheepte,verscheepten,
    [h([transitive,
	np_pc_pp(naar)])]).

v(verscherp,verscherpt,verscherpen,verscherpt,verscherpte,verscherpten,
    [unacc([intransitive]),
     h([sbar_subj_so_np,
	transitive])]).

v(verscheur,verscheurt,verscheuren,verscheurd,verscheurde,verscheurden,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(verschiet,verschiet,verschieten,verschoten,verschoot,verschoten,
    [unacc([intransitive,
            pc_pp(van)]),
     h([transitive])]).

v(verschijn,verschijnt,verschijnen,verschenen,verscheen,verschenen,
    [unacc([intransitive,
	    so_np,
	    so_pp,
	    transitive,
	    fixed([[ten,tonele]],no_passive),
	    ld_pp,
	    ld_adv])]).

v(verschil,verschilt,verschillen,verschild,verschilde,verschilden,
    [h([intransitive,
	meas,
	sbar_subj,  % hoe je dit moet oplossen, verschilt per situatie
	fixed([svp_pp(van,mening)],no_passive),
	fixed([{[mod_pp(over),svp_pp(van,mening)]}],no_passive),
	fixed([svp_pp(van,inzicht)],no_passive),
	fixed([{[mod_pp(over),svp_pp(van,inzicht)]}],no_passive),
	fixed([svp_pp(van,opvatting)],no_passive),
	fixed([{[mod_pp(over),svp_pp(van,opvatting)]}],no_passive),
%	pc_pp(in),
	pc_pp(met),
%	pc_pp(over),  % daar verschillen de meningen/inzichten over
	mod_pp(over), % daar verschillen de meningen/inzichten over
%	pc_pp(per),
	pc_pp(van)
       ])]).

v(verschoon,verschoont,verschonen,verschoond,verschoonde,verschoonden,
    [h([transitive,
	np_pc_pp(van)])]).

v(verschraal,verschraalt,verschralen,verschraald,verschraalde,verschraalden,
    [unacc([intransitive]),
     h([transitive])]).

v(verschrik,verschrikt,verschrikken,verschrikt,verschrikte,verschrikten,
  [h([transitive,
      intransitive])]).

v(verschrik,verschrikt,verschrikken,verschrokken,verschrok,verschrokken,
    [unacc([intransitive])]).

v(verschroei,verschroeit,verschroeien,verschroeid,verschroeide,verschroeiden,
    [unacc([intransitive]),
     h([transitive])]).

v(verschrompel,verschrompelt,verschrompelen,verschrompeld,verschrompelde,verschrompelden,
    [unacc([intransitive]),
     h([transitive])]).

v(verschuif,verschuift,verschuiven,verschoven,verschoof,verschoven,
  [unacc([intransitive,
	  ld_pp]),
   h([refl,
      transitive,
      np_ld_pp])]).

v(verschuil,verschuilt,verschuilen,verscholen,[verschuilde,verschool],[verschuilden,verscholen],
    [h([refl,
	intransitive,
	refl_ld_pp,
	refl_ld_adv])]).

v(versier,versiert,versieren,versierd,versierde,versierden,
    [h([transitive,
	np_pc_pp(met)])]).

v(versimpel,versimpelt,versimpelen,versimpeld,versimpelde,versimpelden,
    [h([transitive])]).  

v(versjouw,versjouwt,versjouwen,versjouwd,versjouwde,versjouwden,
    [h([transitive])]).  

v(versla,verslaat,verslaan,verslagen,versloeg,versloegen,
    [unacc([intransitive]),
     h([transitive])]).

v(verslaaf,verslaaft,verslaven,verslaafd,verslaafde,verslaafden,
    [h([refl_pc_pp(aan),
        intransitive])]).

v(verslaap,verslaapt,verslapen,verslapen,versliep,versliepen,
    [h([refl,
	transitive])]).

v(verslap,verslapt,verslappen,verslapt,verslapte,verslapten,
    [unacc([intransitive,
	pc_pp(in)]),
     h([transitive])]).

v(verslechter,verslechtert,verslechteren,verslechterd,verslechterde,verslechterden,
    [unacc([intransitive,
	transitive])]).

v(versleep,versleept,verslepen,versleept,versleepte,versleepten,
    [h([transitive])]).  

v(versleutel,versleutelt,versleutelen,versleuteld,versleutelde,versleutelden,
    [h([transitive])]).

v(verslijt,verslijt,verslijten,versleten,versleet,versleten,
    [unacc([intransitive]),
     h([transitive,
	voor_pred_np
       ])]).

v(verslik,verslikt,verslikken,verslikt,verslikte,verslikten,
    [h([refl,
	refl_pc_pp(aan),
	refl_pc_pp(in)])]).

v(verslind,verslindt,verslinden,verslonden,verslond,verslonden,
    [h([transitive])]).

v(verslinger,verslingert,verslingeren,verslingerd,verslingerde,verslingerden,
    [h([refl,
	refl_pc_pp(aan)])]).

v(verslof,versloft,versloffen,versloft,verslofte,versloften,
    [h([intransitive])]).

v(verslons,verslonst,verslonzen,verslonsd,verslonsde,verslonsden,
    [h([intransitive,
        transitive])]).  

v(versluier,versluiert,versluieren,versluierd,versluierde,versluierden,
    [h([sbar,
	transitive,
	vp])]).

v(versmaad,versmaadt,versmaden,versmaad,versmaadde,versmaadden,
    [h([transitive])]).

v(versmal,versmalt,versmallen,versmald,versmalde,versmalden,
    [unacc([intransitive,
	pc_pp(tot)]),
     h([refl,
	transitive,
	refl_pc_pp(tot)])]).

v(versmelt,versmelt,versmelten,versmolten,versmolt,versmolten,
    [unacc([intransitive,
	np_pc_pp(met)]),
     h([transitive])]).

v(versnel,versnelt,versnellen,versneld,versnelde,versnelden,
    [unacc([intransitive]),
     h([refl,
	transitive])]).

v(versnijd,versnijdt,versnijden,versneden,versneed,versneden,
    [h([transitive])]).

v(versnipper,versnippert,versnipperen,versnipperd,versnipperde,versnipperden,
    [unacc([intransitive]),
     h([transitive])]).

v(versnoep,versnoept,versnoepen,versnoept,versnoepte,versnoepten,
    [h([transitive])]).

v(versober,versobert,versoberen,versoberd,versoberde,versoberden,
    [h([transitive]),
     unacc([intransitive])]).

v(versoepel,versoepelt,versoepelen,versoepeld,versoepelde,versoepelden,
    [h([transitive])]).

v(verspeel,verspeelt,verspelen,verspeeld,verspeelde,verspeelden,
    [h([transitive])]).

v(versper,verspert,versperren,versperd,versperde,versperden,
    [h([np_np,
	transitive])]).

v(verspil,verspilt,verspillen,verspild,verspilde,verspilden,
    [h([transitive,
	np_pc_pp(aan),
	intransitive])]).

v(versplinter,versplintert,versplinteren,versplinterd,versplinterde,versplinterden,
    [unacc([intransitive]),
     h([transitive])]).

v(verspreek,verspreekt,verspreken,versproken,versprak,verspraken,
  [h([refl])]).

v(verspreid,verspreidt,verspreiden,verspreid,verspreidde,verspreidden,
    [h([refl,
	transitive,
        np_ld_pp,
        np_ld_adv,
	refl_ld_pp,
	refl_ld_adv])]).

v(verspring,verspringt,verspringen,versprongen,versprong,versprongen,
    [unacc([intransitive,
	    ld_pp])]).

v(versta,verstaat,inflected(verstaan,verstane),verstaan,verstond,verstonden,versta,
    [h([refl,
	sbar,
        fixed_dep(intransitive),
	np_pc_pp(van), % ik verstond er geen woord van
	transitive,
	fixed_dep(intransitive),  % heeft laten verstaan VP/SBAR
	part_transitive(mis),  % dat is niet mis te verstaan
	np_pc_pp(onder),
	refl_pc_pp(met)])]).

v(verstap,verstapt,verstappen,verstapt,verstapte,verstapten,
    [h([refl])]).

v(verstar,verstart,verstarren,verstard,verstarde,verstarden,
    [unacc([intransitive]),
     h([transitive])]).

v(verstedelijk,verstedelijkt,verstedelijken,verstedelijkt,verstedelijkte,verstedelijkten,
    [unacc([intransitive])]).

v(versteen,versteent,verstenen,versteend,versteende,versteenden,
    [unacc([intransitive]),
     h([transitive])]).

v(verstel,verstelt,verstellen,versteld,verstelde,verstelden,
    [h([transitive])]).

v(versterk,versterkt,versterken,versterkt,versterkte,versterkten,
    [h([sbar_subj_so_np,
	transitive,
	np_pc_pp(met)])]).

v(versterf,versterft,versterven,versterfd,versterfde,versterfden,
    [unacc([intransitive])]).

v(verstevig,verstevigt,verstevigen,verstevigd,verstevigde,verstevigden,
    [h([refl,
	sbar_subj_so_np,
	transitive])]).

v(verstier,verstiert,verstieren,verstierd,verstierde,verstierden,
    [h([transitive])]).

v(verstijf,verstijft,verstijven,verstijfd,verstijfde,verstijfden,
    [unacc([intransitive,
	pc_pp(van)]),
     h([transitive])]).

v(verstik,verstikt,verstikken,verstikt,verstikte,verstikten,
    [unacc([intransitive]),
     h([transitive])]).

v(verstil,verstilt,verstillen,verstild,verstilde,verstilden,
    [unacc([intransitive])]).

v(verstof,verstoft,verstoffen,verstoft,verstofte,verstoften,
    [unacc([intransitive])]).

v(verstom,verstomt,verstommen,verstomd,verstomde,verstomden,
    [unacc([intransitive]),
     h([transitive])]).

v(verstook,verstookt,verstoken,verstookt,verstookte,verstookten,
    [h([transitive])]).

v(verstoor,verstoort,verstoren,verstoord,verstoorde,verstoorden,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np,
	intransitive])]).

v(verstoot,verstoot,verstoten,verstoten,[verstootte,verstiet],[verstootten,verstieten],
    [h([transitive])]).

v(verstop,verstopt,verstoppen,verstopt,verstopte,verstopten,
    [h([transitive,
        sbar, % VL hij kon niet verstoppen dat...
	np_ld_pp,
	np_ld_adv,
	intransitive  % de Randstad dreigt te verstoppen = verstopt te raken
       ])]).

v(verstout,verstout,verstouten,verstoutte,verstoutten,verstouwt,
  [h([refl,
      refl_vp,
      refl_sbar
     ])]).

v(verstouw,verstouwt,verstouwen,verstouwd,verstouwde,verstouwden,
    [h([transitive])]).

v(verstrak,verstrakt,verstrakken,verstrakt,verstrakte,verstrakten,
    [unacc([intransitive]),
     h([transitive])]).

v(verstrek,verstrekt,verstrekken,verstrekt,verstrekte,verstrekten,
    [h([np_np,
	fixed([{[pc(over),acc(informatie)]}],norm_passive),
        fixed([pc(over),acc(informatie),dat],norm_passive),
       	fixed([inv(acc(informatie)),pc(over),dat],norm_passive),
       	fixed([acc(informatie),inv(dat),pc(over)],norm_passive),
	fixed([{[pc(over),acc(gegeven)]}],norm_passive),
        fixed([pc(over),acc(gegeven),dat],norm_passive),
       	fixed([inv(acc(gegeven)),pc(over),dat],norm_passive),
       	fixed([acc(gegeven),inv(dat),pc(over)],norm_passive),
	np_mod_pp(bij),
	np_np_mod_pp(bij),
	so_pp_np,
	transitive])]).

v(verstrengel,verstrengelt,verstrengelen,verstrengeld,verstrengelde,verstrengelden,
    [h([transitive,
	np_pc_pp(met)
       ])]).

v(verstrijk,verstrijkt,verstrijken,verstreken,verstreek,verstreken,
    [unacc([intransitive])]).

v(verstrik,verstrikt,verstrikken,verstrikt,verstrikte,verstrikten,
    [h([transitive,
	refl_pc_pp(in)])]).

v(verstrooi,verstrooit,verstrooien,verstrooid,verstrooide,verstrooiden,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np])]).

v(verstuik,verstuikt,verstuiken,verstuikt,verstuikte,verstuikten,
    [h([transitive])]).

v(verstuur,verstuurt,versturen,verstuurd,verstuurde,verstuurden,
    [h([transitive,
	np_ld_pp
       ])]).

v(versuf,versuft,versuffen,versuft,versufte,versuften,
    [unacc([intransitive]),
     h([transitive])]).

v(vertaal,vertaalt,vertalen,vertaald,vertaalde,vertaalden,
    [h([transitive,
	np_pc_pp(in),
	pc_pp(in),
	np_pc_pp(naar),
	part_transitive(door),
	part_np_pc_pp(door,naar),
	pc_pp(naar),
	als_pred_np,
	sbar, % dip
        acc_np_dip_sbar,
        intransitive,
	refl_pc_pp(in)])]).

v(vertak,vertakt,vertakken,vertakt,vertakte,vertakten,
  [h([refl,
      intransitive,
      transitive])]).

v(verteder,vertedert,vertederen,vertederd,vertederde,vertederden,
    [h([sbar_subj_so_np,
	transitive]),
     b([intransitive,
	vp_subj_so_np])]).

v(verteer,verteert,verteren,verteerd,verteerde,verteerden,
    [unacc([intransitive]),
     h([transitive,
	sbar_obj,  % ik kan het niet verteren dat ..
	sbar       
       ])]).			

v(vertegenwoordig,vertegenwoordigt,vertegenwoordigen,vertegenwoordigd,vertegenwoordigde,vertegenwoordigden,
    [h([transitive])]).

v(verteken,vertekent,vertekenen,vertekend,vertekende,vertekenden,
    [h([intransitive,
	sbar_subj_so_np,
	transitive])]).

v(vertel,vertelt,vertellen,verteld,vertelde,vertelden,
    [h([np_np,
        so_pp_np,
	intransitive,
	np_sbar,
	so_pp_sbar,
	np_vp_subj,
	np_pc_pp(bij),	    % dat vertelde hij er niet bij
	pp_sbar(bij),       % hij vertelde er niet bij dat ...
        mod_pp(over),	    % en hij vertelt daar zelf over
	
        so_np_mod_pp(over), % hij heeft me daar nooit over verteld ...
	np_mod_pp(over),    % hij heeft daar niets geks over verteld ...
        np_np_mod_pp(over), % hij heeft me daar nooit het fijne over verteld ...
	mod_pp(van),	    % daar heeft hij nooit van verteld
	np_mod_pp(van),     % hij heeft daar niets geks van verteld ...
	so_np_mod_pp(van),  % daar heeft hij mij nooit van verteld
	np_np_mod_pp(van),  % daar heeft hij mij nooit het fijne van verteld
	refl,		    % fout tellen ....
	sbar,
	transitive,
	vp,
	np_pc_pp(omheen),
	part_sbar(na),
	part_transitive(na),
	part_np_np(na),
	part_transitive(door),
	part_np_np(door),
	part_so_pp_np(door),
	part_intransitive(op)   % alleen imparative: "vertel op!"
       ])]).

v(vertik,vertikt,vertikken,vertikt,vertikte,vertikten,
    [h([refl,   % fout tikken
	transitive,
        vp_obj, % ze vertikken het, om ...
	vp])]).

v(vertil,vertilt,vertillen,vertild,vertilde,vertilden,
    [h([refl,
	refl_pc_pp(aan)])]).

v(vertimmer,vertimmert,vertimmeren,vertimmerd,vertimmerde,vertimmerden,
    [h([transitive]),
     b([mod_pp(aan)])]).

v(vertoef,vertoeft,vertoeven,vertoefd,vertoefde,vertoefden,
    [h([intransitive,
	ld_pp,
	ld_adv])]).

v(vertolk,vertolkt,vertolken,vertolkt,vertolkte,vertolkten,
    [h([transitive])]).

v(vertoon,vertoont,vertonen,vertoond,vertoonde,vertoonden,
    [h([refl,
	transitive,
	sbar_obj,  % het is nog nooit vertoond dat ...
	fixed([{[acc(spoor),pc(van)]}],no_passive),
	refl_pc_pp(met)])]).

v(vertraag,vertraagt,vertragen,vertraagd,vertraagde,vertraagden,
    [unacc([intransitive]),
     h([sbar_subj_so_np,
	transitive]),
     b([vp_subj_so_np])]).

v(vertrap,vertrapt,vertrappen,vertrapt,vertrapte,vertrapten,
    [h([transitive])]).

v(vertrek,vertrekt,vertrekken,vertrokken,vertrok,vertrokken,
  [unacc([ld_pp,
	  ld_adv  % vertrekken op weg naar X
	 ]),
     z([intransitive]),         % VL: er wordt vertrokken vanaf het plein
     h([transitive])]).		% hij vertrekt zijn gezicht/geen spier

v(vertroebel,vertroebelt,vertroebelen,vertroebeld,vertroebelde,vertroebelden,
    [h([intransitive,
	sbar_subj_so_np,
	transitive])]).

v(vertroetel,vertroetelt,vertroetelen,vertroeteld,vertroetelde,vertroetelden,
    [h([transitive])]).

v(vertrouw,vertrouwt,vertrouwen,vertrouwd,vertrouwde,vertrouwden,
    [h([transitive,
	part_np_np(toe),
	part_so_pp_np(toe),
	part_np_sbar(toe),
	sbar,
	pc_pp(op),
	er_pp_sbar(op),
	er_pp_vp(op)])]).

v(vertwijfel,vertwijfelt,vertwijfelen,vertwijfeld,vertwijfelde,vertwijfelden,
    [h([intransitive])]).

v(vervaag,vervaagt,vervagen,vervaagd,vervaagde,vervaagden,
    [unacc([intransitive,
	pc_pp(tot)]),
     h([transitive])]).

v(vervaardig,vervaardigt,vervaardigen,vervaardigd,vervaardigde,vervaardigden,
    [h([transitive,
	np_pc_pp(uit),
        np_pc_pp(van)])]).

v(verval,vervalt,vervallen,vervallen,verviel,vervielen,
    [unacc([intransitive,
	pc_pp(aan),
	pc_pp(in),
	pc_pp(tot)])]).

v(vervals,vervalst,vervalsen,vervalst,vervalste,vervalsten,
  [h([transitive,
      intransitive])]).

v(vervang,vervangt,vervangen,vervangen,verving,vervingen,
    [h([transitive,
	np_pc_pp(door)])]).

v(vervat,vervat,vervatten,vervat,vervatte,vervatten,
    [h([transitive,
	np_pc_pp(in)])]).

v(vervel,vervelt,vervellen,verveld,vervelde,vervelden,
  [unacc([intransitive])]).

v(verveel,verveelt,vervelen,verveeld,verveelde,verveelden,
    [h([intransitive,
	refl,
	sbar_subj_so_np,
	so_np,
	transitive,
	vp_subj_so_np])]).

v(verveelvoudig,verveelvoudigt,verveelvoudigen,verveelvoudigd,
  verveelvoudigde,verveelvoudigden,
    [unacc([intransitive]),
     h([transitive])]).

v(ververs,ververst,verversen,ververst,ververste,verversten,
    [h([transitive])]).

v(vervlak,vervlakt,vervlakken,vervlakt,vervlakte,vervlakten,
    [unacc([intransitive]),
     h([transitive])]).

v(vervlecht,vervlecht,vervlechten,vervlochten,vervlocht,vervlochten,
    [h([transitive,
	np_pc_pp(met)])]).

v(vervlieg,vervliegt,vervliegen,vervlogen,vervloog,vervlogen,
    [unacc([intransitive])]).

v(vervloei,vervloeit,vervloeien,vervloeid,vervloeide,vervloeiden,
    [unacc([intransitive])]).

v(vervloek,vervloekt,vervloeken,vervloekt,vervloekte,vervloekten,
  [h([transitive,
      np_sbar])]).  % ik vervloekte mezelf dat ...

v(vervluchtig,vervluchtigt,vervluchtigen,vervluchtigd,vervluchtigde,vervluchtigden,
    [unacc([intransitive])]).

v(vervoeg,vervoegt,vervoegen,vervoegd,vervoegde,vervoegden,
    [h([transitive,
	refl_ld_pp])]).

v(vervoer,vervoert,vervoeren,vervoerd,vervoerde,vervoerden,
  [h([transitive,
      intransitive,
      np_mod_pp(in)])]).

v(vervolg,vervolgt,vervolgen,vervolgd,vervolgde,vervolgden,
    [h([transitive,
	sbar,			% mostly in dip: 'Hij vervolgt: ....'
	acc_np_sbar,            % .. , vervolgde hij zijn verhaal
	pc_pp(met),
        intransitive  % justitie vervolgt niet
       ])]).

v(vervolledig,vervolledigt,vervolledigen,vervolledigd,vervolledigde,vervolledigden,
    [h([transitive])]).

v(vervolmaak,vervolmaakt,vervolmaken,vervolmaakt,vervolmaakte,vervolmaakten,
    [h([sbar_subj_so_np,
	transitive])]).

v(vervorm,vervormt,vervormen,vervormd,vervormde,vervormden,
    [h([intransitive,
	transitive])]).

v(vervreemd,vervreemdt,vervreemden,vervreemd,vervreemdde,vervreemdden,
    [unacc([intransitive,
	pc_pp(van)]),
     h([sbar_subj_so_np,
	transitive,
	np_pc_pp(van)])]).

v(vervroeg,vervroegt,vervroegen,vervroegd,vervroegde,vervroegden,
    [h([sbar_subj_so_np,
	transitive])]).

v(vervuil,vervuilt,vervuilen,vervuild,vervuilde,vervuilden,
    [unacc([intransitive]),
     h([transitive])]).

v(vervul,vervult,vervullen,vervuld,vervulde,vervulden,
    [h([transitive,
	np_mod_pp(in),   % De Europese Unie heeft hier ook een belangrijke rol in te vervullen .
	fixed([{[acc,pc(met)]},sbar_subj],no_passive),
        % het vervult ons met grote blijdschap dat ..
	np_pc_pp(met)])]).

v(verwaai,verwaait,verwaaien,verwaaid,[verwoei,verwaaide],[verwaaiden,verwoeien],
    [h([transitive]),
     unacc([intransitive])]).

v(verwaardig,verwaardigt,verwaardigen,verwaardigd,verwaardigde,verwaardigden,
    [h([transitive,
	refl_vp,
	np_pc_pp(met)])]).

v(verwaarloos,verwaarloost,verwaarlozen,verwaarloosd,verwaarloosde,verwaarloosden,
    [h([transitive])]).

v(verwacht,verwacht,verwachten,verwacht,verwachtte,verwachtten,
    [h([tr_sbar,
	van_sbar,
	transitive,
	intransitive, % ik had niet anders verwacht
        refl_pc_pp(aan), % VL: zich verwachten aan
	fixed([{[acc(duidelijkheid),pc(over)]}],no_passive),
	vp,
        fixed([{[pc(in),acc(verandering)]}],norm_passive),
	np_pc_pp(van)])]).

v(verwar,verwart,verwarren,verward,verwarde,verwarden,
    [h([sbar_subj_so_np,
	transitive,
	vp_subj_so_np,
	np_pc_pp(met),
        intransitive,
        pc_pp(met)])]).

v(verwarm,verwarmt,verwarmen,verwarmd,verwarmde,verwarmden,
  [h([transitive,
      intransitive,
      part_transitive(voor)])]).

v(verwater,verwatert,verwateren,verwaterd,verwaterde,verwaterden,
    [unacc([intransitive])]).

v(verwed,verwedt,verwedden,verwed,verwedde,verwedden,
    [h([transitive,
	np_pc_pp(om),
	np_er_pp_sbar(om),
	np_pc_pp(op),
	np_er_pp_sbar(op),
	np_pc_pp(onder),
	np_er_pp_sbar(onder)])]).

v(verweef,verweeft,verweven,verweven,verweefde,verweefden,
  [h([transitive,
      np_pc_pp(tot),
      np_pc_pp(met)])]).

v(verweek,verweekt,verweken,verweekt,verweekte,verweekten,
    [unacc([intransitive]),
     h([transitive])
    ]).

v(verweer,verweert,verweren,verweerd,verweerde,verweerden,
    [unacc([intransitive]),
     h([refl,
	refl_dip_sbar,
	refl_pc_pp(met),
	refl_pc_pp(tegen)])]).

v(verwek,verwekt,verwekken,verwekt,verwekte,verwekten,
    [h([transitive])]).

v(verwelk,verwelkt,verwelken,verwelkt,verwelkte,verwelkten,
    [unacc([intransitive])]).

v(verwelkom,verwelkomt,verwelkomen,verwelkomd,verwelkomde,verwelkomden,
    [h([transitive])]).

v(verwen,verwent,verwennen,verwend,verwende,verwenden,
    [h([transitive])]).

v(verwens,verwenst,verwensen,verwenst,verwenste,verwensten,
    [h([transitive])]).

v(verwerf,verwerft,verwerven,verworven,[verwierf,verworf],[verwierven,verworven],
    [h([refl_np,
	fixed([{[pc(in),acc(aandeel)]}],norm_passive),
	transitive,
	sbar])]).

v(verwerk,verwerkt,verwerken,verwerkt,verwerkte,verwerkten,
    [h([transitive,
        intransitive, % je moet leren verwerken
	np_pc_pp(in),
	np_pc_pp(tot),
	pp_sbar(in),
	sbar])]).

v(verwerkelijk,verwerkelijkt,verwerkelijken,verwerkelijkt,verwerkelijkte,verwerkelijkten,
    [unacc([intransitive]),
     h([transitive])]).

v(verwerp,verwerpt,verwerpen,verworpen,verwierp,verwierpen,
    [h([transitive])]).

v(verwester,verwestert,verwesteren,verwesterd,verwesterde,verwesterden,
    [unacc([intransitive])]).

v(verwezenlijk,verwezenlijkt,verwezenlijken,verwezenlijkt,verwezenlijkte,verwezenlijkten,
    [h([transitive])]).

v(verwijd,verwijdt,verwijden,verwijd,verwijdde,verwijdden,
    [h([transitive])]).

v(verwijder,verwijdert,verwijderen,verwijderd,verwijderde,verwijderden,
    [h([refl,
	sbar_subj_so_np,
	transitive,
	np_ld_pp,
	np_pc_pp(met),
	refl_ld_pp
        ])]).

v(verwijs,verwijst,verwijzen,verwezen,verwees,verwezen,
    [h([transitive,
	pc_pp(naar),
	er_pp_sbar(naar),
	np_pc_pp(naar),
	part_transitive(door),
	part_pc_pp(door,naar),
	part_np_pc_pp(door,naar),
	part_transitive(terug),
	part_np_pc_pp(terug,naar)])]).

v(verwijt,verwijt,verwijten,verweten,verweet,verweten,
    [h([np_np,
	np_sbar,
	np_vp_obj,
	sbar,
	transitive,
	vp])]).

v(verwikkel,verwikkelt,verwikkelen,verwikkeld,verwikkelde,verwikkelden,
  [h([np_pc_pp(in),
      transitive])]).

v(verwilder,verwildert,verwilderen,verwilderd,verwilderde,verwilderden,
    [unacc([intransitive]),
     h([sbar_subj_so_np,
	transitive])]).

v(verwissel,verwisselt,verwisselen,verwisseld,verwisselde,verwisselden,
    [unacc([np_pc_pp(met),
	pc_pp(van)]),
     h([transitive,
	np_pc_pp(tegen),
	np_pc_pp(voor)])]).

v(verwittig,verwittigt,verwittigen,verwittigd,verwittigde,verwittigden,
    [h([transitive,
        intransitive,
        np_sbar,
	acc_np_sbar,
	np_pc_pp(van)])]).

v(verwoest,verwoest,verwoesten,verwoest,verwoestte,verwoestten,
    [h([sbar_subj_so_np,
	transitive])]).

v(verwond,verwondt,verwonden,verwond,verwondde,verwondden,
    [h([transitive,
	refl_pc_pp(aan)])]).

v(verwonder,verwondert,verwonderen,verwonderd,verwonderde,verwonderden,
    [h([refl,
	sbar_subj_so_np,
        sbar_subj,   % dat hij nog slaapt, hoeft niet te verwonderen
	transitive,
	vp_subj_so_np,
	np_pc_pp(van),
	refl_pc_pp(over),
	refl_er_pp_sbar(over)])]).

v(verwoord,verwoordt,verwoorden,verwoord,verwoordde,verwoordden,
    [h([np_sbar, % dip
	transitive])]).

v(verword,verwordt,verworden,verworden,verwerd,verwerden,
  [unacc([transitive,
	  pc_pp(tot)])]).

v(verwring,verwringt,verwringen,verwrongen,verwrong,verwrongen,
    [h([transitive])]).

v(verzaak,verzaakt,verzaken,verzaakt,verzaakte,verzaakten,
    [h([intransitive,
	transitive,
	vp,
	pc_pp(aan)])]).

v(verzacht,verzacht,verzachten,verzacht,verzachtte,verzachtten,
    [h([sbar_subj_so_np,
	transitive,
	intransitive,
	vp_subj_so_np])]).

v(verzadig,verzadigt,verzadigen,verzadigd,verzadigde,verzadigden,
    [h([refl,
	transitive,
	refl_pc_pp(aan)])]).

v(verzak,verzakt,verzakken,verzakt,verzakte,verzakten,
    [unacc([intransitive])]).

v(verzakelijk,verzakelijkt,verzakelijken,verzakelijkt,verzakelijkte,
  verzakelijkten,
    [unacc([intransitive])]).

v(verzamel,verzamelt,verzamelen,verzameld,verzamelde,verzamelden,
    [z([ld_pp]),
     h([intransitive,
	transitive,
	refl,
	refl_ld_pp,
	refl_ld_adv,
	np_ld_pp,
	np_ld_adv])]).

v(verzand,verzandt,verzanden,verzand,verzandde,verzandden,
    [unacc([intransitive,
	pc_pp(in)])]).

v(verzegel,verzegelt,verzegelen,verzegeld,verzegelde,verzegelden,
    [h([transitive,
	np_pc_pp(met)])]).

v(verzeil,verzeilt,verzeilen,verzeild,verzeilde,verzeilden,
    [z([pc_pp(in)]),
     h([transitive])]).

v(verzeker,verzekert,verzekeren,verzekerd,verzekerde,verzekerden,
    [h([np_np,
	np_sbar,
	np_vp_subj,
	sbar,
	transitive,
        intransitive,
	part_transitive(bij),
	vp,
	np_pc_pp(tegen),
	np_pc_pp(van),
	refl_er_pp_sbar(van)])]).

v(verzelfstandig,verzelfstandigt,verzelfstandigen,verzelfstandigd,verzelfstandigde,verzelfstandigden,
    [h([transitive,
        intransitive])]).

v(verzend,verzendt,verzenden,verzonden,verzond,verzonden,
    [h([transitive,
	np_ld_pp])]).

v(verzeng,verzengt,verzengen,verzengd,verzengde,verzengden,
    [unacc([intransitive]),
     h([transitive])]).

v(verzet,verzet,verzetten,verzet,verzette,verzetten,
    [h([refl,
	transitive,
	fixed([{[acc(werk),pc(voor)]}],norm_passive),
	fixed([{[acc(werk),er_pp(voor,X)]},extra_vp(X)],norm_passive),
	refl_pc_pp(tegen),
	refl_er_pp_sbar(tegen)])]).

v(verziek,verziekt,verzieken,verziekt,verziekte,verziekten,
    [unacc([intransitive]),
     h([transitive,
	np_pc_pp(voor)])]).

v(verzilt,verzilt,verzilten,verzilt,verziltte,verziltten,
    [unacc([intransitive])]).

v(verzilver,verzilvert,verzilveren,verzilverd,verzilverde,verzilverden,
    [h([transitive])]).

v(verzin,verzint,verzinnen,verzonnen,verzon,verzonnen,
    [h([sbar,
	transitive,
	%% er is van alles bij verzonnen
	fixed([{[acc,pc(bij)]}],norm_passive), 
        np_pc_pp(omheen),  % er een verhaal omheen verzinnen
	np_pc_pp(op)
       ])]).

v(verzink,verzinkt,verzinken,verzonken,verzonk,verzonken,
  [unacc([intransitive,
	  fixed([[in,het,niet]],no_passive),
	  ld_pp
	 ])]).

v(verzit,verzit,verzitten,verzeten,verzat,verzaten,
    [h([intransitive,
	transitive])]).

v(verzoek,verzoekt,verzoeken,verzocht,verzocht,verzochten,
    [h([np_vp_obj,   % hen wordt verzocht te komen
	np_vp_obj1,  % zij worden verzocht te komen
	transitive,  % so_np?  dat is de goden verzoeken
	intransitive, % tenzij de partners anders verzoeken
	vp_no_control,
	sbar,
	acc_np_sbar,
	pc_pp(om)])]).

v(verzoen,verzoent,verzoenen,verzoend,verzoende,verzoenden,
    [h([refl,
	transitive,
	intransitive,
	np_pc_pp(met),
	refl_pc_pp(met),
	refl_er_pp_sbar(met)])]).

v(verzorg,verzorgt,verzorgen,verzorgd,verzorgde,verzorgden,
    [h([transitive,
	intransitive])]).

v(verzucht,verzucht,verzuchten,verzucht,verzuchtte,verzuchtten,
    [h([intransitive,
	sbar,
	transitive])]).

v(verzuim,verzuimt,verzuimen,verzuimd,verzuimde,verzuimden,
    [h([intransitive,
	transitive,
	vp])]).

v(verzuip,verzuipt,verzuipen,verzopen,verzoop,verzopen,
    [unacc([intransitive,
            ld_pp,
            ld_adv]),
     h([transitive,
        np_ld_pp,
        np_ld_adv])]).

v(verzuur,verzuurt,verzuren,verzuurd,verzuurde,verzuurden,
    [unacc([intransitive]),
     h([np_np,
	sbar_subj_so_np,
	transitive])]).

v(verzwaar,verzwaart,verzwaren,verzwaard,verzwaarde,verzwaarden,
    [unacc([intransitive]),
     h([transitive,
	np_pc_pp(met)])]).

v(verzwak,verzwakt,verzwakken,verzwakt,verzwakte,verzwakten,
    [unacc([intransitive]),
     h([sbar_subj_so_np,
	transitive])]).

v(verzwelg,verzwelgt,verzwelgen,verzwolgen,verzwolg,verzwolgen,
    [h([transitive])]).

v(verzwijg,verzwijgt,verzwijgen,verzwegen,verzweeg,verzwegen,
  [h([sbar,
      np_np,  % je hebt me iets verzwegen
      transitive,
      vp])]).

v(verzwik,verzikt,verzwikken,verzwikt,verzwikte,verzwikten,
    [h([transitive])]).

v(verzwind,verzwindt,verzwinden,verzwonden,verzwond,verzwonden,
    [z([intransitive])]).

v(vestig,vestigt,vestigen,gevestigd,vestigde,vestigden,
    [h([np_ld_adv,
	transitive,
	refl,          % "twee jaar later vestigden zich ook Franse kolonisten"
        refl_ld_pp,    % se < su, therefore real se???
	refl_ld_adv,
	fixed([{[acc(aandacht),pc(op)]}],norm_passive),
	fixed([{[acc(aandacht),er_pp(op,C)]},extra_sbar(C)],norm_passive),
	fixed([{[acc(hoop),pc(op)]}],norm_passive),
	fixed([{[acc(hoop),er_pp(op,C)]},extra_sbar(C)],norm_passive),
	np_ld_pp])]).

v(vet,vet,vetten,gevet,vette,vetten,
    [h([part_transitive(in)])]).

v(veto,vetoot,vetoën,gevetood,vetode,vetoden,
    [h([intransitive,
	transitive])]).

v(vibreer,vibreert,vibreren,gevibreerd,vibreerde,vibreerden,
    [h([intransitive,
	transitive])]).

v(vier,viert,vieren,gevierd,vierde,vierden,
    [h([sbar,
	transitive,
	intransitive, % VL
        als_pred_np,
	part_intransitive(feest),
        part_intransitive(mee),
	part_transitive(mee),
	part_transitive(bot),
	part_intransitive(uit),
	part_transitive(uit),
	part_np_pc_pp(bot,op)])]).

v(vigeer,vigeert,vigeren,gevigeerd,vigeerde,vigeerden,
    [h([intransitive])]).

v(vijl,vijlt,vijlen,gevijld,vijlde,vijlden,
    [h([intransitive,
	part_transitive(bij),
	part_transitive(weg),
	part_transitive(af),
	transitive])]).

v(vijzel,vijzelt,vijzelen,gevijzeld,vijzelde,vijzelden,
    [h([part_transitive(op),
	transitive,
	ap_pred_np])]).  % vijzel de lavendel fijn

v(vil,vilt,villen,gevild,vilde,vilden,
    [h([transitive])]).

v(vind,vindt,vinden,gevonden,vond,vonden,
    [h([pred_np,
	pred_np_sbar,
	pred_np_vp,
	tr_sbar,
	van_sbar,
	transitive_ndev_ndev,
        % fixed_dep(intransitive),
	intransitive,  % .. en gij zult vinden
	aci,        % dat ik ze goed vind spelen
        obj1_te_passive,  % ik vond die oplossing te verantwoorden
	np_ld_pp,
	np_ld_adv,
	np_pc_pp(aan),
	np_pc_pp(op),   % we vonden er iets op
	np_pc_pp(van),
	refl_pc_pp(in),
	refl_er_pp_sbar(in),
	refl_er_pp_vp(in),
	np_er_pp_sbar(van),	% wat vindt u er van dat ...
	np_mod_pp(voor),        % een oplossing/remedie/... vinden voor
        % ik vind het niet kunnen dat ...
        fixed([{[acc(antwoord),pc(op)]}],norm_passive),
	fixed([{[acc(begrip),pc(voor)]}],norm_passive),
	fixed([{[acc(begrip),er_pp(voor,X)]},extra_sbar(X)],norm_passive),
        fixed([vc(kan,inf,intransitive),het_pobj1(sbar)],no_passive),
        fixed([vc(ben,psp,intransitive),ap_pred(mooi),het_obj1],no_passive),
        fixed([[in,gebreke],acc],norm_passive),
	fixed([{[[geen,graten],er_pp(in)]}],no_passive), % VL
	fixed([{[[geen,graten],er_pp(in,C)]},extra_sbar(C)],no_passive), % VL
	fixed([{[[geen,graten],er_pp(in,C)]},extra_vp(C)],no_passive),  % VL
	% VL de uitbaatster vond er niet beter op dan de brandweer op te trommelen
	fixed([er_pp(op),compar],no_passive),
	np_mod_pp(over),  % ik vond daar niets over
	part_np_mod_pp(terug,over),  % ik vond daar niets over terug
	part_np_mod_pp(terug,van),  % ik vond daar niets van terug
	np_mod_pp(voor),  % ik vond daar geen oplossing voor
        part_transitive(goed),
        part_sbar(goed),
	part_intransitive(plaats),
	part_sbar(uit),
	part_transitive(goed),
	part_transitive(terug),
	part_np_ld_pp(terug),
	part_np_ld_adv(terug),
	part_transitive(uit),
	part_vp(goed),
	part_ld_pp(plaats),
	part_ld_adv(plaats)])]).

v(vinger,vingert,vingeren,gevingerd,vingerde,vingerden,
    [h([intransitive,
	transitive])]).

v(vink,vinkt,vinken,gevinkt,vinkte,vinkten,
    [h([part_transitive(aan),
	part_transitive(uit),
	part_transitive(af)])]).

v(vis,vist,vissen,gevist,viste,visten,
    [h([intransitive,
	transitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	part_transitive(af),
	part_transitive(op),
	part_transitive(uit),
	part_sbar(uit),
	pc_pp(met),
	pc_pp(naar),
	pc_pp(op),
	part_np_ld_pp(op)])]).

v(viseer,viseert,viseren,geviseerd,viseerde,viseerden,
    [h([transitive])]).  

v(visualiseer,visualiseert,visualiseren,gevisualiseerd,visualiseerde,
  visualiseerden,
    [h([transitive])]).  

v(vit,vit,vitten,gevit,vitte,vitten,
    [h([intransitive,
	pc_pp(op)])]).

v(vlag,vlagt,vlaggen,gevlagd,vlagde,vlagden,
    [h([intransitive,
        part_intransitive(af),
	part_transitive(af),
	part_intransitive(uit),
	part_transitive(uit)])]).

v(vlak,vlakt,vlakken,gevlakt,vlakte,vlakten,
    [h([part_intransitive(af),
        part_transitive(af),
	part_transitive(uit),
        part_transitive(weg)])]).

v(vlam,vlamt,vlammen,gevlamd,vlamde,vlamden,
    [h([intransitive,
	transitive]),
     b([part_intransitive(op)])]).

v(vlas,vlast,vlassen,gevlast,vlaste,vlasten,
  [h([pc_pp(op),
      er_pp_vp(op)])]).

v(vlecht,vlecht,vlechten,gevlochten,vlocht,vlochten,
    [h([transitive,
	part_intransitive(door),
        part_transitive(in),
	np_ld_pp])]).

v(vlei,vleit,vleien,gevleid,vleide,vleiden,
    [h([intransitive,
	transitive,
	refl_pc_pp(met)])]).

v(vlek,vlekt,vlekken,gevlekt,vlekte,vlekten,
    [h([intransitive])]).

v(vlieg,vliegt,vliegen,gevlogen,vloog,vlogen,
    [z([ld_dir,
	ld_pp,
        so_np_ld_pp,  % hij is hem naar de keel gevlogen
        pp_copula(in,brand),
	part_intransitive(aan),
	part_intransitive(door),
	part_intransitive(in),
	part_intransitive(langs),
	part_intransitive(om),
	part_intransitive(op),
	part_intransitive(over),  % ik kwam speciaal over vliegen
	part_intransitive(terug),
	part_intransitive(uit),
	part_intransitive(weg),
	part_ld_pp(door),
	part_ld_pp(over),
	part_ld_pp(terug),
	part_transitive(aan),
	part_transitive(over),
	fixed([[in,de,haren],dat],no_passive),
	fixed([[over,de,toonbank]],no_passive),
        fixed([[overkop]],no_passive),
        fixed([[over,de,kop]],no_passive),
	fixed([[over,de,schreef]],no_passive),
	part_ld_pp(aan)]),
     h([intransitive,
        np_ld_pp,  %% ik heb hem naar Duitsland gevlogen
	np_ld_dir, %% ik heb hem het land uit gevlogen
 	transitive,
	part_transitive(in),
	part_transitive(over),  % ik heb hem over gevlogen
	part_np_ld_pp(over),    
	part_transitive(terug)]),
     b([part_intransitive(rond),
	part_ld_pp(rond),
	part_ld_adv(rond)])]).

v(vlieger,vliegert,vliegeren,gevliegderd,vliegerde,vliegerden,
    [h([intransitive])]).

v(vlij,vlijt,vlijen,gevlijd,vlijde,vlijden,
    [h([transitive,
	part_np_ld_adv(neer),
	part_np_ld_pp(neer)])]).

v(vloei,vloeit,vloeien,gevloeid,vloeide,vloeiden,
    [z([ld_dir,
	ld_pp,
	part_intransitive(over),
	part_intransitive(samen),
	part_intransitive(terug),
	part_intransitive(weg),
	part_intransitive(af),
	part_ld_pp(over),
	part_ld_pp(terug),
	part_pp_sbar_subj_opt_het(voort,uit),
	part_pc_pp(voort,uit)]),
     b([intransitive])]).

v(vloek,vloekt,vloeken,gevloekt,vloekte,vloekten,
    [h([intransitive,
	sbar,   % dip
	ap_pred_np, % stijf
	pc_pp(bij),
	pc_pp(met),
	pc_pp(op)])]).

v(vloer,vloert,vloeren,gevloerd,vloerde,vloerden,
    [h([transitive])]).

v(vlog,vlogt,vloggen,gevlogd,vlogde,vlogden,
    [h([intransitive])]).

v(vlooi,vlooit,vlooien,gevlooid,vlooide,vlooiden,
    [h([intransitive,
	transitive, % de apen vlooien elkaar
	part_transitive(door),
	part_transitive(na),
	part_transitive(uit),
	part_sbar(uit)])]).

v(vlot,vlot,vlotten,gevlot,vlotte,vlotten,
    [h([intransitive])]).

v(vlucht,vlucht,vluchten,gevlucht,vluchtte,vluchtten,
    [z([intransitive,
	ld_dir,
	ld_pp,
        pc_pp(voor),
	part_intransitive(weg),
        part_pc_pp(weg,voor),
	part_ld_pp(weg)])]).

v(voed,voedt,voeden,gevoed,voedde,voedden,
    [h([intransitive,
	transitive,
	np_np,
	part_transitive(op),
        part_intransitive(op),
%	refl_pc_pp(met),
	part_np_pc_pp(op,tot)])]).

v(voeder,voedert,voederen,gevoederd,voederde,voederden,
  [h([transitive,
      intransitive,
      part_transitive(bij),
      part_intransitive(bij)])]).

v(voeg,voegt,voegen,gevoegd,voegde,voegden,
    [h([% so_np,  %?
	transitive,
	np_ld_pp,
        refl_ld_pp,      % daar heeft zich ook SU bij gevoegd
	er_pp_sbar(bij), % hij voegde erbij dat ...
	part_sbar(bij),  % er wordt bijgevoegd
	part_transitive(toe),
        part_intransitive(toe),
	part_sbar(toe),
	part_vp(toe),
	part_np_np(toe),
	part_so_pp_np(toe),  % aan iemand iets vertellen
        part_np_pc_pp(toe,aan), % iets ergens aan toevoegen / bijdoen
        part_pc_pp(toe,aan), % ergens aan toevoegen / bijdoen
	part_intransitive(in),
	part_intransitive(uit),
	part_np_sbar(toe),   % hij voegde ons toe dat ..
	part_np_vp_obj(toe), % hij voegde ons toe te ..
	part_pp_sbar(toe,aan), 
	part_pp_vp(toe,aan),   
	part_sbar(in),
	part_transitive(bij),
	part_transitive(in),
	part_transitive(samen),
	part_vp(in),
	refl_pc_pp(naar),
	part_ld_pp(in),
	part_np_ld_pp(in),
	part_np_pc_pp(samen,met),
	part_np_pc_pp(samen,tot),
        part_np_er_pp_sbar(toe,aan)])]).

v(voel,voelt,voelen,gevoeld,voelde,voelden,
    [h([aci,
	fixed([vc(kom_aan,inf,part_intransitive(aan)),sbar],no_passive),   % ik voel aankomen dat hij blijft
	fixed([vc(kom_aan,inf,part_intransitive(aan)),het_pobj1(sbar)],no_passive),   % VL? ik voel het aankomen dat hij blijft
	pred_refl,
	fixed([{[mod_pp(bij),ap_pred]},refl],no_passive),
	fixed([{[mod_pp(door),ap_pred]},refl],no_passive), % ik voel me er lekkerder door
	fixed([{[mod_pp(in),ap_pred]},refl],no_passive),  % je moet je er lekker in voelen
	fixed([{[mod_pp(over),ap_pred]},refl],no_passive),
	nonp_copula,		% het voelt goed

	fixed([ap_pred,sbar_subj],no_passive), % hoe voelt het dat ..
	fixed([ap_pred,vp_subj],no_passive),   % hoe voelt het om ..
	
	refl_passive,  % ik voel me in de kou gezet

        fixed([[genoopt],refl,vp],no_passive),
 	fixed([[geroepen],refl,vp],no_passive),

	alsof_sbar,
	part_nonp_copula(aan),
	part_alsof_sbar(aan),
	sbar,
	als_pred_np,
	als_pred_np_sbar,
	als_pred_np_vp,
	transitive_ndev_ndev,
	intransitive,  % voel maar!
	er_pp_sbar(voor),
	er_pp_vp(voor),
	np_pc_pp(voor),
        part_intransitive(mee),
	part_sbar(aan),
	part_transitive(aan),
	part_transitive(mee),
	part_transitive(na),
	pc_pp(aan),
	pc_pp(voor),
	np_pc_pp(van),  % ik heb er weinig/niets van gevoeld
	fixed([[aan,de,tand],acc],norm_passive),
        fixed([vc(spreek_aan,psp,part_intransitive(aan)),pc(door),refl],no_passive),
%	fixed([[genoopt],refl,vp],no_passive),
%	fixed([[geroepen],refl,vp],no_passive),
%	fixed([[gedwongen],refl,vp],no_passive),
%	fixed([[verplicht],refl,vp],no_passive),
	fixed([{[acc(behoefte),pc(tot)]}],no_passive),
 	fixed([{[acc(behoefte),er_pp(tot,C)]},extra_sbar(C)],no_passive),
 	fixed([{[acc(behoefte),er_pp(tot,C)]},extra_vp(C)],no_passive),
	part_pc_pp(mee,met),
	part_refl(thuis),
	part_refl_ld_adv(thuis),
	part_refl_ld_pp(thuis)])]).

v(voer,voert,voeren,gevoerd,voerde,voerden,
    [h([ap_pred_np,
	np_np,
	ld_dir,
	np_ld_dir,
	so_pp_np,
	transitive,
	intransitive,  % die voert niet ver
	fixed([ap_pred(ver)],no_passive),
	fixed([ap_pred(ver),sbar_subj],no_passive),
	fixed([ap_pred(ver),dat,sbar_subj],no_passive),
	fixed([ap_pred(ver),vp_subj],no_passive),
	fixed([ap_pred(ver),dat,vp_subj],no_passive),
	fixed([[ten,tonele],acc],norm_passive),
	np_mod_pp(met),
	np_mod_pp(over),
%	fixed([{[pc(over),acc(discussie)]}],norm_passive),
%	fixed([{[pc(over),acc(onderhandeling)]}],norm_passive),
%	fixed([{[pc(met),acc(gesprek)]}],norm_passive),
%	fixed([{[pc(over),acc(gesprek)]}],norm_passive),
	ld_pp,
	np_ld_pp,
	part_als_pred_np(op),
	part_ld_pp(door),
	part_sbar_subj_so_np(op),
	part_sbar(aan),
	part_sbar(in),
	part_so_pp_np(op),
	part_intransitive(actie),
	part_intransitive(in),
	part_intransitive(uit),
	part_transitive(aan),
        part_np_np(aan), %% de havens krijgen vis aangevoerd.
                         %% ??? wij voeren de havens vis aan
        part_als_pred_np(aan),  % als bewijs werd XX aangevoerd
        part_fixed(aan,[als_pred,sbar],imp_passive),
	part_transitive(af),
        part_np_ld_pp(af),
	part_transitive(bij),  % wel/niet in de Oostvaardersplassen
	part_transitive(door),
	part_transitive(in),
	part_transitive(mee),
        part_transitive(oorlog),
	part_transitive(op),
	part_transitive(terug),
	part_transitive(toe),
	part_transitive(uit),
	part_transitive(weg),
	part_np_ld_pp(in),
	part_np_ld_pp(uit),
	part_ld_pp(terug),
	part_np_ld_pp(terug),
	part_np_pc_pp(terug,op),
	part_np_pc_pp(terug,tot),
	part_np_pc_pp(uit,in),
	part_np_pc_pp(weg,naar),
	part_np_pc_pp(weg,uit)])]).

v(voetbal,voetbalt,voetballen,gevoetbald,voetbalde,voetbalden,
    [h([intransitive,
	refl_ld_pp,	    % hij voetbalde zich in de nationale ploeg
	mod_pp(op),         % het was geen veld om op te voetballen
	pc_pp(met)])]).

v(vogel,vogelt,vogelen,gevogeld,vogelde,vogelden,
    [h([intransitive,
	part_transitive(uit),
	part_sbar(uit)
       ])
    ]).

v(volbreng,volbrengt,volbrengen,volbracht,volbracht,volbrachten,
    [h([transitive])]).

v(voldoe,voldoet,inflected(voldoen,voldoene),voldaan,voldeed,voldeden,
    [h([intransitive,
	so_np,
	transitive,
	pc_pp(aan),
	pc_pp(in)])]).

v(voleindig,voleindigt,voleindigen,voleindigd,voleindigde,voleindigden,
    [h([intransitive,
	transitive])]).

v(volg,volgt,volgen,gevolgd,volgde,volgden,
    [z([intransitive,
        er_er,  % er zullen er nog volgen
	part_intransitive(opeen),
	pp_sbar_subj_no_het(uit), % Hieruit volgt dat ..
	sbar_subj_no_het,  % dan volgt ogenblikkelijk dat er ...
	pc_pp(op),
	pc_pp(uit)]),
     b([np_ld_pp,
	np_ld_dir,
	part_transitive(na),
        transitive,
        fixed([[op,de,voet],acc],norm_passive),
	part_transitive(in),
	part_transitive(op),
	part_als_pred_np(op)])]).

v(volhard,volhardt,volharden,volhard,volhardde,volhardden,
    [h([intransitive,
	pc_pp(bij),
	pc_pp(in)])]).

v(volleer,volleer,volleren,gevolleerd,volleerde,volleerden,
    [h([intransitive,
        transitive,
        np_ld_pp,
        np_ld_dir])]).

v(volley,volleyt,volleyen,gevolleyd,volleyde,volleyden,
    [h([intransitive,
        transitive,
        np_ld_pp,
        np_ld_dir])]).

v(volleybal,volleybalt,volleyballen,gevolleybald,volleybalde,volleybalden,
    [h([intransitive])]).

v(volmaak,volmaakt,volmaken,volmaakt,volmaakte,volmaakten,
    [h([sbar_subj_so_np,
	transitive])]).

v(volsta,volstaat,inflected(volstaan,volstane),volstaan,volstond,volstonden,
    [h([intransitive,
	sbar_subj,
        vp_subj,
	pc_pp(met),
        er_pp_vp(met)])]).

v(voltooi,voltooit,voltooien,voltooid,voltooide,voltooiden,
    [h([transitive])]).

v(voltrek,voltrekt,voltrekken,voltrokken,voltrok,voltrokken,
    [h([refl,
	transitive])]).

v(volvoer,volvoert,volvoeren,volvoerd,volvoerde,volvoerden,
    [h([transitive])]).

v(vonk,vonkt,vonken,gevonkt,vonkte,vonkten,
    [h([intransitive])]).

v(vonnis,vonnist,vonnissen,gevonnist,vonniste,vonnisten,
    [h([intransitive,
	transitive,
	sbar])]).

v(voorkom,voorkomt,voorkomen,voorkomen,voorkwam,voorkwamen,
    [h([sbar_subj_so_np_opt_het, % voorkomen moet worden dat...
	sbar,
	fixed([[erger]],imp_passive),
	transitive])]).

v(vooronderstel,vooronderstelt,vooronderstellen,voorondersteld,vooronderstelde,vooronderstelden,
    [h([sbar,
	transitive])]).

v(voorspel,voorspelt,voorspellen,voorspeld,voorspelde,voorspelden,
    [h([np_np,
	np_sbar,
	sbar_subj_so_np,
	sbar,
	transitive,
	intransitive
       ])]).

v(voorvoel,voorvoelt,voorvoelen,voorvoeld,voorvoelde,voorvoelden,
    [h([sbar,
	transitive])]).

v(voorzeg,voorzegt,voorzeggen,voorzegd,voorzegde,voorzegden,
    [h([sbar,
	transitive])]).

v(voorzie,voorziet,inflected(voorzien,voorziene),voorzien,voorzag,voorzagen,
    [h([sbar,
	transitive,
	fixed([pc(op),het_obj1],no_passive),
				% hij had het op de dochter voorzien
	np_pc_pp(van),
	np_pc_pp(voor),
	pc_pp(in),
        er_pp_sbar(in)])]).

v(vorder,vordert,vorderen,gevorderd,vorderde,vorderden,
    [z([intransitive,
	pc_pp(met)]),
     h([transitive,
	part_transitive(in),
	np_pc_pp(van),
	pp_sbar(van),
	sbar,
	part_transitive(op),  % VL
	part_transitive(terug),
        part_intransitive(terug)])]).

v(vorm,vormt,vormen,gevormd,vormde,vormden,
    [h([np_np,
	transitive,
	refl, % er vormde zich een file
	%  TODO "een inbreuk vormen op"
	np_pc_pp(naar),
	np_pc_pp(uit),
	np_pc_pp(van),
	np_pc_pp(over),
        fixed([pc(op),acc(uitzondering)],no_passive),
        fixed([{[pc(van),acc(illustratie)]}],no_passive),
	fixed([{[acc(mening),refl,pc(over)]}],no_passive),
	fixed([{[acc(oordeel),refl,pc(over)]}],no_passive),
	part_als_pred_np(om),
	part_transitive(om),
	part_np_pc_pp(om,tot)])]).

v(vors,vorst,vorsen,gevorst,vorste,vorsten,
    [h([intransitive,
	pc_pp(naar)])]).

v(vouw,vouwt,vouwen,gevouwen,vouwde,vouwden,
  [unacc([part_intransitive(uit),
	  part_intransitive(open)]),
   h([ap_pred_np,
	transitive,
	part_transitive(uit),
	part_transitive(op),
	part_transitive(open)])]).

v(vraag,vraagt,vragen,gevraagd,vroeg,vroegen,
    [h([np_np,
	np_sbar,     % mij werd gevraagd of ik kwam
	acc_np_sbar, % ik werd gevraagd of ik wilde komen
	np_vp_obj,   % mij werd gevraagd te komen
	np_vp_obj1,  % ik werd gevraagd te komen
	%%% ?? np_vp_no_control,  % ik vroeg de commisie te mogen komen
	so_vp_obj,   % +aan
	vp_subj_np,  % het vraagt een flinke investering om ...
	fixed([{[pc(van),vp]}],imp_passive),
		     % van mij werd gevraagd om .. (lijkt wel een obj2)
	             % ik werd te eten gevraagd =/= ik werd gevraagd te eten
	fixed([vc(eet,pass_te,intransitive),acc],norm_passive),
	sbar,
	van_sbar,
	so_van_sbar,
	so_pp_np,
        so_pp,       % vraag maar aan je vader...
	so_pp_sbar,
	part_transitive(uit),  % de gegevens kunnen worden uitgevraagd
	transitive,
	intransitive,  % u vraagt maar
	vp,			% ik vraag te mogen vertrekken
	vp_no_control,          % je zou moeten vragen om deze berichten te verwijderen
	np_pc_pp(ten),
	np_pc_pp(van),
	als_pred_np,
	fixed([{[acc(aandacht),pc(voor)]}],norm_passive),
	fixed([svp_pp(van,lijf),[de,hemd],dat],imp_passive),
	fixed([svp_pp(van,lijf),[het,hemd],dat],imp_passive),
	fixed([[te,hulp],acc],norm_passive),
	part_refl_sbar(af),
	part_refl_np(af),    % also 'zichzelf'
	part_transitive(aan),
	part_np_mod_pp(aan,voor),    %  daar een hoofdelijke stemming voor aanvragen
        fixed([{[acc(octrooi),pc(op)]}],norm_passive),
        fixed([{[acc(patent),pc(op)]}],norm_passive),
        part_fixed(aan,[{[acc(octrooi),pc(op)]}],norm_passive),
        part_fixed(aan,[{[acc(patent),pc(op)]}],norm_passive),
        part_fixed(aan,[{[acc(patent),pc(op)]}],norm_passive),
	part_transitive(na),
	part_sbar(na),
	part_transitive(op),
	part_transitive(terug),
	pc_pp(naar),
	so_np_pc_pp(naar),
	so_np_pc_pp(over),
        fixed([{[dat_pp(aan),pc(naar)]}],imp_passive),
        fixed([{[dat_pp(aan),pc(over)]}],imp_passive),
	pc_pp(om),
	np_pc_pp(om), 
	so_np_pc_pp(om),
        fixed([{[dat_pp(aan),pc(om)]}],imp_passive)])]).

v(vrees,vreest,vrezen,gevreesd,vreesde,vreesden,
    [h([intransitive,
	sbar,
	van_sbar,
	transitive,
	vp,
	pc_pp(voor)])]).

v(vreet,vreet,vreten,gevreten,vrat,vraten,
    [h([intransitive,
	transitive,
	part_transitive(aan),
	part_transitive(op),
        part_transitive(uit),	% wat heeft hij uitgevreten?
	ap_pred_np,   % hommelnesten leeg
	pc_pp(van),
	np_pc_pp(van),
	pc_pp(aan),
        sbar_subj,          % het vreet dat ...
        pp_sbar_subj(aan),  % het vreet aan me dat ..
	part_refl_pc_pp(op,van)])]).

v(vries,vriest,vriezen,gevroren,vroor,vroren,
  [z([part_intransitive(aan),
      ap_copula,		% de plas vroor dicht; hij vroor dood
      part_intransitive(op),
      part_intransitive(vast),
      part_intransitive(kapot),
      part_intransitive(dood)
     ]),
   h([intransitive,	  % de ijsmeester is toen hard gaan vriezen...
      het_subj_sbar_obcomp,
      part_transitive(door),
      part_transitive(kapot),  % de vorst heeft de mais kapot gevroren
      part_transitive(in)])]).

v(vrij,vrijt,vrijen,[gevrijd,gevreeën],[vrijde,vree],[vrijden,vreeën],
    [h([intransitive,
	part_transitive(op),
	pc_pp(met),
	pc_pp(naar)])]).

v(vrijwaar,vrijwaart,vrijwaren,gevrijwaard,vrijwaarde,vrijwaarden,
    [h([transitive,
	np_pc_pp(tegen),
	np_pc_pp(van),
	np_pc_pp(voor)])]).

v(vrolijk,vrolijkt,vrolijken,gevrolijkt,vrolijkte,vrolijkten,
    [h([part_sbar_subj_so_np(op),
	part_transitive(op),
	part_intransitive(op),
	part_vp_subj_so_np(op)])]).

v(vul,vult,vullen,gevuld,vulde,vulden,
    [h([np_np,
	transitive,
	intransitive, % aardappelpuree vult
        np_pc_pp(met),
	part_sbar(aan),  % dip
	part_sbar(in),
	part_transitive(aan),
	part_intransitive(aan),
	part_transitive(bij),
	part_transitive(in),
        part_intransitive(in), % vul zelf maar in
	part_transitive(op),
	part_vp(in),
	part_np_pc_pp(aan,met),
	part_np_pc_pp(op,met)])]).

v(vuur,vuurt,vuren,gevuurd,vuurde,vuurden,
    [h([intransitive,
        transitive,
	ld_pp,
	ld_dir,
	np_ld_pp,
	np_ld_dir,
	part_transitive(aan),
	part_transitive(af),
	pc_pp(op),
	part_intransitive(mis),
	part_intransitive(raak),
	part_np_pc_pp(af,op)])]).

v(waad,waadt,waden,gewaad,waadde,waadden,
  [z([ld_pp,
      ld_dir,
      intransitive]),
     h([part_transitive(door)])]).

v(waag,waagt,wagen,gewaagd,waagde,waagden,
    [h([refl_ld_dir,
	refl_ld_pp,
	refl_ld_adv,
	transitive,
	vp_obj,
	fixed([er_pp(op),het_obj1],no_passive),
        % dit waag ik te betwijfelen; not a real VP, because of extraction
        % fixed([vc(betwijfel,te,intransitive),acc],no_passive),
        % fixed([vc(betwijfel,te,intransitive),sbar],no_passive),
	tr_vp,   % "bridge verb"   ik kom niet ' waagde hij op te merken
	np_pc_pp(aan),
	refl_pc_pp(aan),
        intransitive])]).

v(waai,waait,waaien,gewaaid,[waaide,woei],[waaiden,woeien],
  [unacc([part_intransitive(aan),
	  part_intransitive(af),
	  part_intransitive(binnen),
	  part_intransitive(om),
	  part_intransitive(op),
	  part_intransitive(over),
	  part_ld_pp(over),
	  part_transitive(op),
	  part_so_np(aan),
	  part_so_np(tegemoet),
	  ld_pp,
	  ld_dir,
	  part_ld_pp(over)]),
   b([part_intransitive(uit),
      fixed([[met,alle,winden,mee]],imp_passive)]),
   h([intransitive,
      part_np_np(toe),		% iemand koelte toe waaien
      part_transitive(op)])]).

v(waaier,waaiert,waaieren,gewaaierd,waaierde,waaierden,
    [h([intransitive,
	refl,
	part_intransitive(uit),
	part_ld_pp(uit)])]).

v(waak,waakt,waken,gewaakt,waakte,waakten,
    [h([intransitive,
	pc_pp(over),
	er_pp_sbar(over),
	er_pp_vp(over),
	pc_pp(tegen),
	pc_pp(voor),
	er_pp_sbar(voor),
	er_pp_vp(voor)])]).

v(waan,waant,wanen,gewaand,waande,waanden,
  [h([pred_np,
      sbar,			% ouderwets
      vp,			% ouderwets
      refl_ld_pp,
      refl_ld_adv])]).

% de baardmijt waart rond!
% en het Iglo-spook waart nog altijd door de levensmiddelenindustrie
v(waar,waart,waren,gewaard,waarde,waarden,
    [unacc([part_intransitive(rond),
	    ld_pp])]).

v(waarborg,waarborgt,waarborgen,gewaarborgd,waarborgde,waarborgden,
    [h([np_np,
	np_sbar,
	np_vp_subj,
	sbar,
	transitive,
	np_pc_pp(tegen),
	np_pc_pp(voor)])]).

v(waardeer,waardeert,waarderen,gewaardeerd,waardeerde,waardeerden,
    [h([transitive,
	sbar_obj,
	sbar,
	part_transitive(op),
	part_transitive(onder),
	part_transitive(over),
	part_transitive(af),
	np_pc_pp(op)])]).

v(waarschuw,waarschuwt,waarschuwen,gewaarschuwd,waarschuwde,waarschuwden,
    [h([acc_np_sbar,
	intransitive,
	sbar,
	np_vp_obj1,
        vp_no_control,
	transitive,
	np_pc_pp(voor),
	pc_pp(voor),
	er_pp_sbar(voor),
	er_pp_vp_no_control(voor),
	np_er_pp_sbar(voor),
	obj_np_er_pp_vp(voor),
	np_pc_pp(tegen),
	er_pp_sbar(tegen),
	er_pp_vp_no_control(tegen),
	np_er_pp_sbar(tegen),
	obj_np_er_pp_vp(tegen),
	pc_pp(tegen)
       ])]).

v(wacht,wacht,wachten,gewacht,wachtte,wachtten,
    [h([intransitive,
	so_np,
	transitive,
	part_intransitive(af),
	part_sbar(af),
	sbar,			% we wachten of de heren wel komen
	vp,			% ik kan niet wachten (om) het te vertellen
	part_transitive(af),
	part_transitive(op),
	pc_pp(met),
	pc_pp(op),
	er_pp_sbar(op),  % je kunt erop wachten dat het mis gaat
	refl_pc_pp(voor),
	refl_er_pp_vp(voor), 
	part_pc_pp(af,tot)])]).

v(waggel,waggelt,waggelen,gewaggeld,waggelde,waggelden,
    [z([ld_dir,
	ld_pp]),
     b([intransitive])]).

v(wakker,wakkert,wakkeren,gewakkerd,wakkerde,wakkerden,
    [unacc([part_intransitive(aan),
	    part_intransitive(op)]),
     h([part_transitive(aan),
	intransitive])]).

v(walg,walgt,walgen,gewalgd,walgde,walgden,
    [h([pc_pp(van),
	intransitive])]).

v(walm,walmt,walmen,gewalmd,walmde,walmden,
    [h([intransitive,
	part_so_np(tegemoet)])]).

v(wals,walst,walsen,gewalst,walste,walsten,
    [h([intransitive,
	transitive,
	part_pc_pp(heen,over),
	pc_pp(over)])]).

v(wandel,wandelt,wandelen,gewandeld,wandelde,wandelden,
  [h([intransitive,
      transitive, % route zus en zo
      part_transitive(in)	% VL
     ]),
   b([part_intransitive(rond),
      part_intransitive(aan),	% komen
      part_ld_pp(rond),
      part_intransitive(weg),
      ld_adv,
      ld_pp,
      ld_dir])]).

v(wanhoop,wanhoopt,wanhopen,gewanhoopt,wanhoopte,wanhoopten,
    [h([intransitive,
	pc_pp(aan)])]).

v(wankel,wankelt,wankelen,gewankeld,wankelde,wankelden,
    [z([ld_dir,
	ld_pp]),
     h([intransitive])]).

v(wantrouw,wantrouwt,wantrouwen,gewantrouwd,wantrouwde,wantrouwden,
    [h([transitive])]).

%% ??
v(wap,wapt,wappen,gewapt,wapte,wapten,
    [h([intransitive])]).

v(wapen,wapent,wapenen,gewapend,wapende,wapenden,
    [h([transitive,
	refl, % refl < su
	np_pc_pp(tegen),
	refl_pc_pp(tegen), % refl < su
	np_pc_pp(voor),
	refl_pc_pp(voor)
       ])]).

v(wapper,wappert,wapperen,gewapperd,wapperde,wapperden,
    [h([intransitive,
	part_np_np(toe)	% iemand koelte toe wapperen
    ])]).

v(warm,warmt,warmen,gewarmd,warmde,warmden,
    [z([part_intransitive(op)]),
     h([transitive,
        part_transitive(door),
	refl_pc_pp(aan),
	%% part_refl(op),
	part_transitive(op)])]).

v(was,wast,wassen,gewassen,waste,wasten,
  [h([ap_pred_np,
      np_ld_pp, % ik moet de schimmel van mijn stramme voeten wassen
	np_np,
	transitive,
	intransitive,
	part_intransitive(af),
	part_transitive(af),
        part_transitive(uit),
	part_transitive(wit)])]).

v(wasem,wasemt,wasemen,gewasemd,wasemde,wasemden,
    [z([intransitive,
	ld_pp,
	part_intransitive(uit)
       ])]).

v(water,watert,wateren,gewaterd,waterde,waterden,
    [h([intransitive,
	part_intransitive(af),
	part_transitive(af),
	transitive])]).

v(watertand,watertandt,watertanden,gewatertand,watertande,watertanden,
    [h([intransitive,
	pc_pp(van)])]).

%% van was voorzien (ski's etc)
v(wax,waxt,waxen,gewaxt,waxte,waxten,
    [h([intransitive,
	transitive])]).

v(wed,wedt,wedden,gewed,wedde,wedden,
    [h([intransitive,
	sbar,
	pc_pp(om),
	pc_pp(op),
	er_pp_sbar(om),
	er_pp_sbar(op)
       ])]).

v(wedervaar,wedervaart,wedervaren,wedervaren,wedervoer,wedervoeren,
    [unacc([so_np])]).

v(wedijver,wedijvert,wedijveren,gewedijverd,wedijverde,wedijverden,
    [h([intransitive,
	pc_pp(met),
	pc_pp(voor)])]).

v(weef,weeft,weven,geweven,weefde,weefden,
    [h([intransitive,
	transitive,
	part_transitive(in),
	np_ld_pp])]).

v(weeg,weegt,wegen,gewogen,woog,wogen,
    [h([intransitive,
	transitive,
	meas,
	part_sbar(af),
	part_transitive(af),
        part_intransitive(mee),
	part_transitive(mee),
	part_sbar(mee),
	part_sbar_subj_no_het(mee), % daarbij speelt mee dat hij ...
	part_transitive(op),
	part_transitive(uit),
	part_np_pc_pp(af,tegen),
	part_pc_pp(op,tegen),
        part_transitive(weg),
        part_np_ld_pp(weg)])]).

v(week,weekt,weken,geweekt,weekte,weekten,
    [unacc([intransitive]),
     h([transitive,
        part_transitive(los),
        part_np_pc_pp(los,van),
	part_transitive(af)])]).

v(weeklaag,weeklaagt,weeklagen,geweeklaagd,weeklaagde,weeklaagden,
    [h([intransitive])]).

v(ween,weent,wenen,geweend,weende,weenden,
    [h([intransitive,
	transitive, % bittere tranen
	pc_pp(om)])]).

v(weer,weert,weren,geweerd,weerde,weerden,
    [h([refl,
	transitive,
	np_ld_pp,
	np_pc_pp(van),
	part_transitive(af),
	part_dip_sbar(af),
	part_acc_np_dip_sbar(af),
        part_intransitive(af),
	refl_pc_pp(tegen)])]).

v(weergalm,weergalmt,weergalmen,weergalmd,weergalmde,weergalmden,
    [h([intransitive])]).

v(weerhoud,weerhoudt,weerhouden,weerhouden,weerhield,weerhielden,
    [h([transitive,
	np_pc_pp(van),
        np_vp_obj1,
	obj_np_er_pp_vp(van)])]).

v(weerkaats,weerkaatst,weerkaatsen,weerkaatst,weerkaatste,weerkaatsten,
    [unacc([intransitive]),
     h([transitive]),
     b([ld_pp])]).

v(weerklink,weerklinkt,weerklinken,weerklonken,weerklonk,weerklonken,
  [h([intransitive,
      sbar,
      pc_pp(van)])]).

v(weerlicht,weerlicht,weerlichten,geweerlicht,weerlichtte,weerlichtten,
  [h([het_subj])]).

v(weerleg,weerlegt,weerleggen,weerlegd,weerlegde,weerlegden,
  [h([transitive,
      acc_np_dip_sbar])]).

v(weerspiegel,weerspiegelt,weerspiegelen,weerspiegeld,weerspiegelde,weerspiegelden,
    [h([sbar,
	transitive,
        % np_pc_pp(in),
	refl_pc_pp(in)])]).

v(weerspreek,weerspreekt,weerspreken,weersproken,weersprak,weerspraken,
    [h([sbar,
	transitive])]).

v(weersta,weerstaat,inflected(weerstaan,weerstane),weerstaan,weerstond,weerstonden,
    [h([transitive,
	pc_pp(aan)])]).  % aan de verleiding weerstaan

v(weet,weet,weten,geweten,wist,wisten,wete,
  [h([refl_passive,             % omdat hij zich weet gesteund / gesteund weet...
	tr_sbar,
	transitive,
	aci_simple, % VL: je weet me wonen, je weet het staan
	van_sbar,  % ik weet zeker van wel
	intransitive, % ik denk niet, ik weet
	np_pc_pp(op),   % de chauffeur wist er wel iets op
	np_pc_pp(van),  % ik wist iets van hem
        pp_sbar(van),   % ik wist van hem dat ...
	np_pc_pp(over),  % ik wil er alles over weten
	part_fixed_dep(af,intransitive),
	pc_pp(van),
        %% part_pc_pp(weg,met), % hij weet er wel weg mee; ook "geen weg weten met"
	fixed([{[acc(weg),pc(met)]}],no_passive),        
	subj_control(te),   % dat hij de mensen wist te overtuigen
        vp,
        refl_vp,		       % daarvan wist zich eentje te handhaven
        np_pred_refl,            % hij weet zich het middelpunt ...
	fixed([{[acc(raad),pc(met)]},refl],no_passive),
	fixed([{[acc(raad),pc(met)]}],no_passive),
	fixed([acc(raad),refl],no_passive),
	fixed([{[[geen,blijf],pc(met)]},refl],no_passive),
	fixed([{[[geen,blijf],pc(met)]}],no_passive),
	fixed([[geen,blijf],refl],no_passive),
        fixed([[beter]],no_passive), % je weet toch wel beter
        fixed([[beter],het_obj1],no_passive), % hij wist het weer beter
	fixed([[nooit]],no_passive), % je weet (maar) nooit
        part_pc_pp(af,van),  % ze heeft er van afgeweten
	part_np_pc_pp(af,van)])]).

v(weid,weidt,weiden,geweid,weidde,weidden,
    [h([intransitive,
	transitive,
	part_intransitive(uit),
	part_pc_pp(uit,over)])]).

v(weifel,weifelt,weifelen,geweifeld,weifelde,weifelden,
    [h([intransitive,
	pc_pp(over)])]).

v(weiger,weigert,weigeren,geweigerd,weigerde,weigerden,
    [h([np_np,
	intransitive,
	np_vp_subj,
	transitive,
	vp,
	subj_control(te)  %% omdat hij zijn medewerking weigert te verlenen
       ])]).

v(wek,wekt,wekken,gewekt,wekte,wekten,
    [h([transitive,
	np_pc_pp(tot),
	sbar_subj_np,  % het wekt onze interesse/belangstelling, dat ...
	fixed([[tot,leven],acc],norm_passive),
	part_sbar_subj_so_np(op),
	part_transitive(op),
	part_vp_subj_so_np(op),
	part_np_pc_pp(op,tot),
	part_np_pc_pp(op,uit)])]).

v(wel,welt,wellen,geweld,welde,welden,
    [b([ld_pp,
	intransitive,
        part_intransitive(op)])]).

v(welf,welft,welven,gewelfd,welfde,welfden,
    [h([refl,
        intransitive])]).

v(wemel,wemelt,wemelen,gewemeld,wemelde,wemelden,
    [h([pc_pp(van)])]).

v(wen,went,wennen,gewend,wende,wenden,
    [unacc([intransitive,
	pc_pp(aan),
	er_pp_sbar(aan)]),
     h([np_pc_pp(aan),
	np_er_pp_sbar(aan),
	part_np_np(af),
	part_transitive(af)])]).

v(wend,wendt,wenden,gewend,wendde,wendden,
    [h([intransitive,
	np_ld_dir,
	transitive,
	np_ld_pp,
	part_sbar(voor),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(voor),
	part_vp(voor),
	refl_pc_pp(tot),
	part_refl(af),
	part_refl(om),
	part_refl_pc_pp(af,van)])]).

v(wenk,wenkt,wenken,gewenkt,wenkte,wenkten,
    [h([intransitive,
	transitive,
	sbar,		       % ze wenkt dat ik dichterbij moet komen
	np_vp_obj1,
	np_ld_pp])]).

v(wens,wenst,wensen,gewenst,wenste,wensten,
    [h([np_np,
	tr_sbar,
	transitive,
	vp,
	np_ld_pp,
	np_ld_adv,
	part_np_np(toe),
	part_np_sbar(toe),
	part_np_vp_obj(toe),
	part_transitive(geluk),
	subj_control(te),
        intransitive,
        
        fixed([pc(met),acc(succes),dat],norm_passive),
       	fixed([inv(acc(succes)),pc(met),dat],norm_passive),
       	fixed([acc(succes),inv(dat),pc(met)],norm_passive),

        fixed([pc(met),acc(geluk),dat],norm_passive),
       	fixed([inv(acc(geluk)),pc(met),dat],norm_passive),
       	fixed([acc(geluk),inv(dat),pc(met)],norm_passive),

	part_np_pc_pp(geluk,met)])]).

v(wentel,wentelt,wentelen,gewenteld,wentelde,wentelden,
    [z([ld_pp]),
     h([intransitive,
	transitive,
	part_transitive(af),
	part_np_pc_pp(af,op),
	np_ld_pp])]).

v(werf,werft,werven,geworven,wierf,wierven,
    [h([intransitive,
	transitive,
	part_transitive(aan),
        part_intransitive(aan)])]).

v(werk,werkt,werken,gewerkt,werkte,werkten,
    [z([part_intransitive(op),
	part_als_pred_np(uit)]),
     h([intransitive,
        part_refl(binnen),  % omdat hij zich heeft weten binnen te werken
	ap_pred_refl,
	ap_copula,  % het werkt koortsverlagend
        part_ap_copula(uit),  % het werkte goed uit
	refl_ld_dir,
	np_ld_pp,  % hij werkt de bal naar voren
	np_ld_dir, % hij werkt de bal de achterlijn over
        fixed([[in,de,hand],acc],norm_passive),
        fixed([[in,de,hand],sbar],imp_passive),
	fixed([ap_pred,vp_subj],no_passive),   % het werkt goed om naar de jongeren toe te gaan
	%% dat werkt ons op de lachspieren/het gemoed
	so_np_pc_pp(op),
	part_intransitive(af),  % voetbal, afwerken op doel
        part_intransitive(mee),
	part_intransitive(over),
	part_intransitive(samen),
	part_intransitive(tegen),
	part_intransitive(terug),
	part_refl(op),
	part_sbar_subj_so_np(tegen),
	part_transitive(af),
	part_transitive(bij),
	part_intransitive(bij),
	part_transitive(bol),  % 'het kunnen bolwerken'
	part_transitive(door),
	part_transitive(in),
	part_transitive(om),
	part_transitive(op),
	part_transitive(tegen),
	part_transitive(uit),
	part_transitive(weg),
	pc_pp(aan),
	part_pc_pp(door,aan),
	part_pc_pp(voort,aan),
	er_pp_sbar(aan),
	er_pp_vp(aan),
	pc_pp(met),
	part_pc_pp(toe,naar),
	part_np_pc_pp(op,tot),
	part_pc_pp(door,in),
	part_pc_pp(door,op),
	part_pc_pp(in,op),
	part_pc_pp(mee,aan),
	part_er_pp_sbar(mee,aan),
	part_pc_pp(samen,aan),
	part_pc_pp(samen,met),
	part_pc_pp(terug,op),
	part_refl_pc_pp(op,tot)])]).

v(werp,werpt,werpen,geworpen,wierp,wierpen,
    [h([np_np,
	intransitive,
	np_ld_dir,
	transitive,
	ld_pp,
	np_ld_pp,
	np_np_ld_pp,
	part_np_np(tegen),
	part_np_np(toe),
	part_intransitive(uit),
	part_ld_pp(uit),
	part_sbar_subj_so_np(omver),
	part_sbar_subj_so_np(op),
	part_sbar(op),
	part_np_sbar(tegen),
	part_sbar(tegen),
	part_transitive(af),
	part_transitive(in),   % de hengel inwerpen
	part_transitive(omver),
	part_transitive(op),
	part_transitive(tegen),
	part_transitive(terug),
	part_transitive(toe),
	part_transitive(uit),
	part_transitive(weg),
	part_vp(op),
	fixed([[voor,de,voeten],dat,sbar],imp_passive),
        fixed([{[acc(licht),pc(op)]}],norm_passive),
	pc_pp(met),
	part_np_ld_pp(terug),
        part_fixed(op,[als_pred,refl],no_passive),
	part_fixed(af,[acc(vrucht)],no_passive)])]).

v(wervel,wervelt,wervelen,gewerveld,wervelde,wervelden,
  [h([intransitive,
      part_intransitive(op)
     ])]).

v(wettig,wettigt,wettigen,gewettigd,wettigde,wettigden,
    [h([transitive])]).

v(wiebel,wiebelt,wiebelen,gewiebeld,wiebelde,wiebelden,
    [h([intransitive])]).

v(wied,wiedt,wieden,gewied,wiedde,wiedden,
    [h([intransitive,
	transitive])]).

v(wieg,wiegt,wiegen,gewiegd,wiegde,wiegden,
    [h([transitive,
	intransitive])]).

v(wiegel,wiegelt,wiegelen,gewiegeld,wiegelde,wiegelden,
    [h([intransitive])]).

v(wiek,wiekt,wieken,gewiekt,wiekte,wiekten,
    [h([part_transitive(kort),
        intransitive])]).

v(wielren,wielrent,wielrennen,gewielrend,wielrende,wielrenden,
    [h([intransitive])]).

v(wijd,wijdt,wijden,gewijd,wijdde,wijdden,
    [h([transitive,
	np_pc_pp(aan),
	np_pc_pp(tot),
	part_transitive(in),
        fixed([[priester],acc],norm_passive), % hij werd priester gewijd
	part_intransitive(uit),
	part_pc_pp(uit,over),
	part_transitive(uit),
	refl_pc_pp(aan),
	part_np_pc_pp(in,in),
	part_np_pc_pp(toe,aan)])]).

v(wijk,wijkt,wijken,geweken,week,weken,
    [z([intransitive,
	part_intransitive(af),
	part_intransitive(terug),
	part_intransitive(uit),
	pc_pp(voor),
	part_pc_pp(af,van),
	part_ld_pp(uit)])]).

v(wijs,wijst,wijzen,gewezen,wees,wezen,
    [h([np_np,
	intransitive,
	ld_dir,
	np_ld_dir,
	np_sbar,
	sbar,
	so_np,
	transitive,
	ld_pp,
        np_pc_pp(op),    % de huiseigenaren *ZIJN* gewezen op het risico
	so_np_pc_pp(op), % de huiseigenaren *IS* gewezen op het risico
	pc_pp(op),
	er_pp_sbar(op),
	er_pp_vp(op),
        pp_sbar_subj_no_het(op),
	np_er_pp_sbar(op),
	np_er_pp_vp(op),
	part_als_pred_np(aan),
	part_als_pred_np(af),
	part_np_np(aan),
	part_intransitive(toe),
	part_transitive(toe),
	part_np_np(toe),
	part_sbar(aan),
	part_sbar(uit),
	part_so_pp_np(aan),
	part_so_pp_np(toe),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(na),
	part_transitive(terecht),
	part_transitive(terug),
	part_intransitive(uit), % +anders
	part_transitive(uit),
	fixed([[van,de,hand],acc],norm_passive),
	part_np_pc_pp(aan,voor)])]).

v(wijt,wijt,wijten,geweten,[weet,wijtte],[weten,wijtten],
    [h([np_pc_pp(aan),
	pp_sbar_obj(aan)])]).  % dat hij komt is te wijten aan jouw gezag

v(wijzig,wijzigt,wijzigen,gewijzigd,wijzigde,wijzigden,
  [h([transitive,
      refl,           % meteen wijzigt zich zijn thematiek
      np_mod_pp(aan), % er kunnen nog dingen aan gewijzigd worden
      intransitive])]).

v(wik,wikt,wikken,gewikt,wikte,wikten,
    [h([intransitive,  % de mens wikt
	transitive])]).

v(wikkel,wikkelt,wikkelen,gewikkeld,wikkelde,wikkelden,
    [h([transitive,
	np_ld_pp,
	part_intransitive(af),
	part_transitive(af),
	part_transitive(in)])]).

v(willig,willigt,willigen,gewilligd,willigde,willigden,
    [h([part_transitive(in)])]).

v(wimpel,wimpelt,wimpelen,gewimpeld,wimpelde,wimpelden,
    [h([part_transitive(af),
	part_transitive(weg),
	np_ld_dir,  % de lof opzij wimpelen ..
	fixed([[van,de,hand],acc],norm_passive),
	part_sbar(weg)])]).

v([win,gewin],[wint,gewint],[winnen,gewinnen],gewonnen,[won,gewon],wonnen,
    [h([intransitive,
	transitive,
	np_pc_pp(op),
        np_mod_pp(met),
	np_pc_pp(uit),
	np_pc_pp(van),  % de wedstrijd, het, alles
	np_pc_pp(voor),
	part_intransitive(bij),
	part_transitive(bij),
	np_mod_pp(bij), % we hebben daar niets bij te winnen
	mod_pp(bij),    % daar kunnen we alleen maar bij winnen
	part_transitive(in),
	part_transitive(terug),
	pc_pp(aan),
	fixed([{[mod_pp(door),pc(aan)]}],no_passive), % het verslag zou er aan geloofwaardigheid door winnen
	pc_pp(aan), % winnen aan kracht
	pc_pp(op),
	pc_pp(van),
	part_np_ld_pp(in)])]).

v(wind,windt,winden,gewonden,wond,wonden,
    [h([transitive,
	np_ld_pp,
	part_transitive(op),
	part_refl(op),
	part_refl_er_pp_sbar(op,over),
	fixed([er_pp(om),[geen,doekjes]],imp_passive),
	fixed([er_pp(om),[geen,doekjes],sbar],no_passive),
	fixed([er_pp(rond),[geen,doekjes]],imp_passive),          %VL
	fixed([er_pp(rond),[geen,doekjes],sbar],no_passive),	  %VL
	fixed([er_pp(rond),[geen,doekjes],dip_sbar],no_passive),  %VL
	part_refl_pc_pp(op,over)])]).

v(winkel,winkelt,winkelen,gewinkeld,winkelde,winkelden,
    [h([intransitive])]).

v(winter,wintert,winteren,gewinterd,winterde,winterden,
    [h([het_subj])]).  

v(wip,wipt,wippen,gewipt,wipte,wipten,
  [z([ld_pp,
      ld_dir,
      part_intransitive(aan),
      part_intransitive(binnen),
      part_intransitive(langs)]),
   h([intransitive,
      transitive,
      part_transitive(op),
      np_ld_pp,
      np_ld_dir,
      pc_pp(met)])]).

v(wis,wist,wissen,gewist,wiste,wisten,
  [h([transitive,
      np_ld_pp,
      refl_np_ld_pp,  % hij wiste zich een traain uit de ogen
      part_np_np(uit),
      part_transitive(af),
      part_transitive(uit)])]).

v(wissel,wisselt,wisselen,gewisseld,wisselde,wisselden,
    [h([intransitive,
	transitive,
	np_pc_pp(met),
	np_pc_pp(van),
	part_intransitive(af),
	part_transitive(af),
	part_transitive(in),
	part_transitive(om),
	part_intransitive(uit),
	part_transitive(uit),
	pc_pp(met),
	pc_pp(van),
	pc_pp(voor),
	part_np_pc_pp(af,door),
	part_np_pc_pp(af,met),
	part_np_pc_pp(in,voor),
	part_pc_pp(af,met)])]).

v(wit,wit,witten,gewit,witte,witten,
    [h([intransitive,
	transitive])]).

v(woed,woedt,woeden,gewoed,woedde,woedden,
    [h([intransitive,
	fixed([subj(discussie),pc(over)],no_passive),
        part_intransitive(uit),
	ld_pp,
	ld_adv,
	pc_pp(tegen)])]).

v(woeker,woekert,woekeren,gewoekerd,woekerde,woekerden,
    [h([intransitive,
	pc_pp(met)])]).

v(woel,woelt,woelen,gewoeld,woelde,woelden,
    [b([ld_pp]),
     h([ap_pred_np,
	intransitive,
	transitive,
	np_ld_pp,
	part_transitive(om)])]).

v(wok,wokt,wokken,gewokt,wokte,wokten,
    [h([transitive,
	intransitive])]).

v(wolk,woklt,wolken,gewolkt,wolkte,wolkten,
    [h([intransitive])]).

v(wond,wondt,wonden,gewond,wondde,wondden,
    [h([transitive,
	np_ld_pp])]).

v(woon,woont,wonen,gewoond,woonde,woonden,
    [h([intransitive,
	ld_adv,
	ld_pp,
	part_intransitive(in),
	part_intransitive(samen),
	part_transitive(bij),
	part_ld_pp(in),
	part_ld_pp(adv),
	part_pc_pp(samen,met)])]).

v(word,wordt,worden,geworden,werd,werden,worde,
  [unacc([passive,
	  te_passive,           % de bron wordt steeds moeilijker te traceren
	    copula,
	    so_nonp_copula,	% het werd hem te erg

				% dat werd me toen toch een bende!
				% Dat zal me een vertoning worden .  --> 
	  fixed([pred,[me]],no_passive),
				% hij werd me toch kwaad
				% het werd me daar een bende
				% should be mod (?), and is productive
	    
	    copula_sbar,
	    copula_vp,
	    simple_cleft,           % het worden tropenjaren
	    copula_np,
	    so_nonp_copula_sbar,
	    so_nonp_copula_vp,
	    part_sbar(gewaar),
	    part_transitive(gewaar),
	    part_vp(gewaar),
	    pred_er_pp_sbar(van),
	    pred_er_pp_vp(van),
	    pred_pc_pp(door),
	    pred_pc_pp(om),  % ik kan er nog boos/pissig/kwaad om worden
	    pred_pc_pp(van), % daar word ik misselijk van
	    pc_pp(van),      % wat is er van hem geworden?
	                     % niets is er van hem geworden
	    %% fixed([subj(wat),pc(van)],no_passive), 

	    pc_pp(tot),	% zij zijn geworden tot conservatieve organisaties

            
	    fixed([er_pp(op),compar],no_passive),  % de situatie wordt er niet beter op
	    fixed([[de,baas],acc],no_passive), % ik word de stress niet de baas
	    fixed([[te,baas],acc],no_passive), % VL
	    fixed([{[np_pred(dupe),pc(van)]}],no_passive),
	    fixed([{[np_pred(dupe),er_pp(van,C)]},extra_sbar(C)],no_passive),
	    fixed([{[pc(van),np_pred(slachtoffer)]}],no_passive),
	    fixed([{[pc(met),ap_pred(oud)]}],no_passive),

	    fixed([ap_pred(duidelijk),naar_sbar_subj_no_het],imp_passive),
	    fixed([ap_pred(bekend),naar_sbar_subj_no_het],imp_passive),

	    %% die kunnen me gestolen worden
	    fixed([vc(steel,psp,intransitive),dat],no_passive),
	    
	    fixed([{[mod_pp(met),ap_pred(rijk)]}],no_passive),
	    fixed([{[mod_pp(met),ap_pred(groot)]}],no_passive),
	    fixed([{[er_pp(van,C),np_pred(slachtoffer)]},extra_sbar(C)],no_passive),
	    fixed([{[np_pred(lid),pc(van)]}],no_passive),
	    fixed([[te,moede],ap_pred,dat],no_passive),
  	    fixed([[vergezeld],pc(van)],no_passive),
	    fixed([er_pp(uit),[geen,wijs]],no_passive),
	    fixed([er_pp(uit),[wijs]],no_passive)])]).

v(worstel,worstelt,worstelen,geworsteld,worstelde,worstelden,
    [h([intransitive,
	part_transitive(door),
	pc_pp(met),
	pc_pp(tegen),
	ap_pred_refl,  % hij worstelde zich vrij/los
	refl_ld_pp,
	refl_ld_dir])]).

v(wortel,wortelt,wortelen,geworteld,wortelde,wortelden,
    [unacc([intransitive,
	ld_pp,
	ld_adv])]).

v(wraak,wraakt,wraken,gewraakt,wraakte,wraakten,
    [h([transitive])]).

v(wreek,wreekt,wreken,gewroken,wreekte,wreekten,
    [h([refl,
	transitive,
	sbar_subj_refl_opt_het, % dan wreekt zich dat ...
	refl_pc_pp(op)])]).

v(wriemel,wriemelt,wriemelen,gewriemeld,wriemelde,wriemelden,
    [h([intransitive,
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_dir,
	pc_pp(aan)])]).

v(wrijf,wrijft,wrijven,gewreven,wreef,wreven,
    [h([ap_pred_np,
	np_np,
	intransitive,
	transitive,
	ld_pp,
	ld_adv,
	part_np_np(aan),
	part_np_sbar(aan),
	part_np_np(in),
	part_np_sbar(in),
	part_np_vp_obj(in),
	part_so_np(in),
	part_transitive(af),
	part_transitive(aan),
	part_transitive(in),
	refl_np_ld_pp,   % hij wrijft zich de slaap uit de ogen
	np_ld_pp,        % zo wrijf je het vuil in de stof
	fixed([[onder,de,neus],dat,sbar],imp_passive),
	fixed([[onder,de,neus],{[acc,dat]}],norm_passive),
	part_transitive(uit),	% hij wrijft zijn ogen uit
	part_refl_np(uit),	% hij wrijft zich de ogen uit
	part_np_pc_pp(in,met)])]).

v(wrik,wrikt,wrikken,gewrikt,wrikte,wrikten,
    [h([intransitive,
	transitive,
	np_ld_pp,
	pc_pp(aan)])]).

v(wring,wringt,wringen,gewrongen,wrong,wrongen,
    [h([intransitive,
	transitive,
	part_transitive(uit),
        part_fixed(om,[[de,nek],acc],norm_passive), % kansen de nek om wringen VL
        part_fixed(om,[acc(nek)],norm_passive), % ik ga Bruno zijn nek omwringen VL
	fixed([no_subj,[de,schoen],ld_adv,[hem]],no_passive),  % daar wringt hem de schoen
	part_intransitive(tegen),  % VL
	np_ld_pp,
	np_ld_dir
	% refl_ld_pp
        ])]).

v(wroet,wroet,wroeten,gewroet,wroette,wroetten,
    [h([nonp_pred_np,
	intransitive,
	transitive,
	part_transitive(door),
	ld_pp,
	ld_adv,
	np_ld_pp,
	np_ld_adv])]).

v(wuif,wuift,wuiven,[gewuifd,gewoven],[wuifde,woof],[wuifden,woven],
    [h([intransitive,
	ld_pp,
	np_ld_pp,  % hij wuift dit van tafel
	np_ld_dir,
	fixed([[gedag]],imp_passive),
	fixed([[gedag],dat],imp_passive),
	part_np_np(toe),	% iemand koelte toe wuiven
	part_transitive(na),
	part_transitive(toe),
	part_transitive(uit),
	part_transitive(weg),
	pc_pp(met)])]).

v(wurg,wurgt,wurgen,gewurgd,wurgde,wurgden,
  [h([transitive,
      intransitive])]).

v(wurm,wurmt,wurmen,gewurmd,wurmde,wurmden,
    [h([intransitive,
	np_ld_pp,
	np_ld_dir,
	part_refl(los),
	part_refl(vast)])]).

v(zaag,zaagt,zagen,gezaagd,zaagde,zaagden,
    [h([nonp_pred_np,
	intransitive,
	transitive,
	np_ld_pp,
	np_np,  % de oren van het lijf
        part_transitive(weg),	% de poten onder X stoel
	part_transitive(uit),
	part_transitive(af),
	part_transitive(door),
        part_np_pc_pp(door,over), %% iemand doorzagen over ..
	part_transitive(om)])]).

v(zaai,zaait,zaaien,gezaaid,zaaide,zaaiden,
    [h([transitive,
	fixed([sbar_subj,acc(verwarring)],no_passive),
	intransitive, % wie zaait zal oogsten
	part_intransitive(in),
	part_transitive(in),
	part_refl(uit),
	part_intransitive(uit),
	part_transitive(uit)])]).

v(zadel,zadelt,zadelen,gezadeld,zadelde,zadelden,
    [h([transitive,
	part_transitive(op),
	part_np_pc_pp(op,met),
	part_pc_pp(op,met)])]).

v(zak,zakt,zakken,gezakt,zakte,zakten,
    [z([intransitive,
	ld_pp,
	ld_dir,
	meas,
	fixed([[in,de,schoenen],dat,subj(moed)],no_passive),  % cf zink
        fixed([{[acc(broek),dat,pc(van)]}],no_passive),
	fixed([[in,elkaar]],no_passive),
	fixed([[inelkaar]],no_passive),
	part_intransitive(af),
	part_intransitive(in),
	part_intransitive(ineen),
	part_intransitive(onderuit),
	part_intransitive(terug),
	part_intransitive(weg),
	part_ld_pp(af),
	part_ld_pp(in),
	part_ld_pp(terug),
	pc_pp(voor),
	part_ld_pp(weg),
	part_pc_pp(af,in)])]).

v(zalf,zalft,zalven,gezalfd,zalfde,zalfden,
    [h([transitive,
	intransitive % VL de heer slaat en zalft
       ])]).

v(zamel,zamelt,zamelen,gezameld,zamelde,zamelden,
    [h([part_intransitive(in),
	part_transitive(in)])]).

v(zanik,zanikt,zaniken,gezanikt,zanikte,zanikten,
    [h([intransitive,
	pc_pp(om),
	sbar,
	pc_pp(over),
	er_pp_sbar(over)])]).

v(zap,zapt,zappen,gezapt,zapte,zapten,
    [h([intransitive])]).

v(zeef,zeeft,zeven,gezeefd,zeefde,zeefden,
    [h([transitive])]).

v(zeem,zeemt,zemen,gezeemd,zeemde,zeemden,
    [h([transitive,
	intransitive])]).

v(zeep,zeept,zepen,gezeept,zeepte,zeepten,
    [h([transitive,
        part_transitive(in)])]).

v(zeg,zegt,zeggen,gezegd,zei,zeiden,zegge,
    [h([tr_sbar,
	van_sbar,
	sbar_obj,         % het moet gezegd worden dat ...
	vp,
        subj_control(te),  % het boek waar hij aan zegt te werken
                           %                   een hekel aan zegt te hebben
	np_sbar,
        so_pp_sbar,
	np_vp_subj,  % hij zei mij de oplossing te hebben
	             % also obj-control in 'beveel' reading:
	             % hij zei mij het boek te pakken
        so_pp_vp,
	np_vp_obj, % hij zei me naast hem te gaan zitten
	transitive,
	np_np,
	intransitive,   % ??
	np_pc_pp(met),  % wat wil je daar nou mee zeggen
	                % daar was alles mee gezegd
	part_intransitive(voor),
	part_transitive(voor),
	part_np_np(voor),
	np_pc_pp(voor),  % er valt veel voor te zeggen
	np_er_pp_vp(voor),  % er valt veel voor te zeggen om ..
	part_np_np(aan),
        part_np_sbar(aan),  % we hebben hem aangezegd dat ze uiterlijk..
	part_transitive(aan),
	part_np_np(af),
	part_intransitive(af),
	part_transitive(af),
        part_transitive(dank),
	np_pc_pp(bij),		% dat zei hij er niet bij
	pp_sbar(bij),   % hij zei er niet bij dat ...
	pp_sbar(met),   % hij wilde daar mee zeggen dat ..
	pp_sbar(van),   % er werd van gezegd dat het door omkoping was verkregen
	np_pc_pp(met),  % hij wilde daar verder niets mee zeggen
        np_pc_pp(in),   % je hebt er niets in te zeggen
	part_np_np(na),
	part_transitive(na),
	%% np_pc_pp(op),  ??
	part_transitive(op),
	part_intransitive(op),  % zeg op , of ik knal je kop er af
	part_np_np(op),  % we zeggen hem de vriendschap op
	np_mod_pp(over), % daar heeft hij nooit iets over gezegd
	np_np_mod_pp(over), % daar heeft hij mij nooit iets over gezegd
	fixed([{[mod_pp(over),sbar]}],imp_passive),   % ik vertrek niet , zei hij daar toen over
	np_mod_pp(van),    % daar heeft hij nooit iets van gezegd
	np_np_mod_pp(van), % daar heeft hij mij nooit iets van gezegd
	fixed([er_pp(op),[donder]],imp_passive),
	fixed([er_pp(op,X),[donder],extra_sbar(X)],imp_passive),
	fixed([er_pp(bij),sbar],imp_passive),
	% hij kan er geen nee tegen zeggen
	fixed([[gedag]],imp_passive),
	fixed([[gedag],dat],imp_passive),
	fixed([pc(tegen),acc(ja)],imp_passive),
	fixed([pc(tegen),acc(nee)],imp_passive),
	fixed([pc(tegen),acc(neen)],imp_passive),
	fixed([pc(tegen),acc(u)],imp_passive),  % een carriere om u tegen te zeggen
	part_np_np(toe),
	part_intransitive(toe),
	part_np_sbar(toe),
	part_np_vp_subj(toe),
	part_sbar(toe),
	part_transitive(toe),
	part_transitive(terug),
	part_sbar(terug),
	part_vp(toe),
	np_pc_pp(van)])]).

v(zegen,zegent,zegenen,gezegend,zegende,zegenden,
    [h([transitive,
	part_transitive(in)])]).

v(zegevier,zegeviert,zegevieren,gezegevierd,zegevierde,zegevierden,
    [h([intransitive,
	pc_pp(over)])]).

v(zeik,zeikt,zeiken,[gezeikt,gezeken],[zeikte,zeek],[zeikten,zeken],
    [h([intransitive,
	sbar,  % hij zat te zeiken dat...
	mod_pp(over),
	part_transitive(af),
	part_intransitive(af)  % er moet afgezeken worden.
       ])]).

v(zeil,zeilt,zeilen,gezeild,zeilde,zeilden,
    [z([ld_dir,
	ld_adv,
	part_intransitive(aan),
        part_intransitive(uit),
        part_intransitive(weg),
	ld_pp]),
     h([intransitive,
	het_subj])]).  % het reilt en zeilt

v(zeker,zekert,zekeren,gezekerd,zekerde,zekerden,
  [h([transitive])]).

v(zend,zendt,zenden,gezonden,zond,zonden,
    [h([np_np,
	intransitive,
	so_pp_np,
	transitive,
	np_ld_pp,
	ld_pp,
	np_ld_dir,
	np_pc_pp(om),
	part_np_np(toe),
	part_intransitive(uit),
	part_transitive(heen),
	part_transitive(door),
	part_np_ld_pp(door),
	part_np_ld_pp(in),
	part_np_ld_pp(uit),
	part_transitive(in),
	part_transitive(over),
	part_transitive(toe),
	part_transitive(uit)])]).

v(zet,zet,zetten,gezet,zette,zetten,
    [z([part_intransitive(aan),
	part_intransitive(af),
	part_intransitive(op),
	part_intransitive(uit),
	part_ld_pp(af),
	part_np_ld_pp(af)]),
     h([nonp_pred_np,
	np_ld_dir,
	np_ld_pp,
	np_ld_adv,
	np_np_ld_pp,
	ld_pp,  % zet maar op de rekening
	refl,
	transitive,
	aan_het,
	np_aan_het,
        pp_pred_np(aan,slag),
        pp_pred_np(aan,werk),
	part_np_np(aan),
	part_np_vp_obj1(aan),
	part_np_np(af),
	part_np_np(voor),
	part_intransitive(door),
	part_intransitive(in),
	part_intransitive(om),
	part_intransitive(voor),
	part_np_ld_pp(om),
	part_refl(af),
	part_refl(in),
        part_refl_vp(in),  
	part_refl(uit),
	part_refl(verder),  %VL
	part_intransitive(voort),
	part_refl(voort),
	part_sbar(uiteen),
	part_np_sbar(uiteen),
	part_so_pp_sbar(uiteen),
	part_so_pp_np(bij),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(apart),
	part_transitive(bij),
	part_transitive(buiten),
	part_transitive(door),
	part_transitive(gelijk),
        part_transitive(gevangen),
	part_transitive(in),
	part_np_mod_pp(in,voor),
        part_np_ld_pp(in),
	part_transitive(klaar),
	part_transitive(klem),
	part_transitive(neer),
        part_np_ld_pp(neer),
	part_als_pred_np(neer),
	part_transitive(om),
	np_pc_pp(op),   % ik zet hem op stil
 	part_transitive(op),
	part_transitive(open),
	part_transitive(opzij),
	part_transitive(over),
	part_transitive(recht),
	part_transitive(stil),
	part_transitive(stop),
	part_transitive(terug),
	part_intransitive(terug),
	so_np_pc_pp(tot),       % ik kon me er niet toe zetten.
				% ik kon me er niet toe zetten om ..
	fixed([er_pp(tot,C),dat,extra_vp(C)],no_passive),
	part_transitive(uit),
	part_transitive(uiteen),
	part_transitive(vast),
        part_transitive(verder),
	part_transitive(voor),
	part_so_control(voor,pass_te),	% wat zetten we hem te eten voor?  dit werkt nog niet, woord-volgorde
	part_transitive(voorop),
	part_transitive(voort),
	part_transitive(weg),
	ld_pp_sbar,  % ik wil in de krant/op het etiket zetten dat ...
	refl_ld_pp,
        fixed([op_een_v,het_obj1],no_passive),
                                % hij zette het op een krijsen
        fixed([op_een_v,{[[me],het_obj1]}],no_passive),
                                % hij zette het me toch op een krijsen
	fixed([pc(naar),[koers]],imp_passive),
	fixed([{[pc(op),acc(druk)]}],norm_passive),
	part_fixed(aan,[acc(duimschroef),dat],norm_passive),
	part_fixed(aan,[acc(duimschroef)],norm_passive),
	fixed([[alles,op,alles]],imp_passive),
	fixed([[klem],acc],norm_passive),
	fixed([[in,gang],acc],norm_passive),
	fixed([[in,het,pak],acc],norm_passive), % Vlaams
	fixed([[in,de,verf],acc],norm_passive), 
        fixed([[in,lichterlaaie],acc],norm_passive),
        fixed([[in,lichterlaaien],acc],norm_passive),
	fixed([[op,het,spel],acc],norm_passive),
	fixed([[op,het,spoor],acc],norm_passive),
	fixed([[op,losse,schroeven],acc],norm_passive),
	fixed([[op,poten],acc],norm_passive),
	fixed([[op,punt],acc],norm_passive),  % Vlaams
	fixed([[op,stelten],acc],norm_passive),
	fixed([[op,touw],acc],norm_passive),
	fixed([[op,het,getouw],acc],norm_passive), % Vlaams
        fixed([[reserve],acc],norm_passive),
	fixed([[in,scene],acc],norm_passive),
	fixed([[in,scène],acc],norm_passive),
	fixed([[schrap],refl],no_passive),
	fixed([[te,kakken],acc],norm_passive),
	fixed([[te,schande],acc],norm_passive),
	fixed([{[[te,boek],als_pred]}],no_passive),
	fixed([[te,boek]],no_passive),
	fixed([{[[voet],ld_pp]}],imp_passive),   % voet aan wal zetten?
	fixed([[voor,schut],acc],norm_passive),
	fixed([svp_pp(op,rij_DIM),acc],norm_passive),
	fixed([svp_pp(op,rij_DIM),sbar],imp_passive),
        fixed([svp_pp(naar,hand),acc],norm_passive),
	fixed([[een,hak],dat],imp_passive),
	fixed([pp_pred(in,verf),acc],norm_passive),
        fixed([pp_pred(onder,druk),i(acc,X),obj_vp(X)],norm_passive),
	part_fixed(dwars,[acc(voet),dat],imp_passive),
	part_np_ld_pp(af),
	part_np_ld_pp(door),
	part_np_ld_pp(terug),
	part_np_ld_pp(vast),
        part_pc_pp(aan,tot), % de media hebben aangezet tot haat tegen P.F.
	part_np_pc_pp(aan,tot),
	part_obj_np_er_pp_vp(aan,tot),
	part_np_pc_pp(af,met),
	part_np_pc_pp(af,tegen),
	part_np_pc_pp(in,op),
	part_np_pc_pp(om,in),
	part_np_pc_pp(op,tegen),
	part_np_pc_pp(vast,met),
	part_np_pc_pp(vast,op),
	part_pc_pp(in,op),
	part_refl_pc_pp(af,tegen),
	part_refl_pc_pp(in,voor),
	part_refl_er_pp_sbar(in,voor),
	part_refl_pc_pp(vast,in),
	fixed([[betaald],{[acc,dat]}],norm_passive),
	fixed([{[[te,werk],acc]}],norm_passive),
	fixed([{[[tewerk],acc]}],norm_passive),
        part_transitive(tewerk),
	part_fixed(bij,[{[[luister],pc(aan)]}],imp_passive),
	part_fixed(bij,[[kracht],acc],norm_passive), 
					% het betoog werd kracht bijgezet
	part_fixed(bij,[[luister],acc],norm_passive),
	part_fixed(om,[[in,klinkende,munt],acc],norm_passive)]),
     b([fixed([[op,de,keel],[het,mes],dat],imp_passive)])]).

v(zetel,zetelt,zetelen,gezeteld,zetelde,zetelden,
    [b([ld_adv,
	ld_pp,
	intransitive % er
       ])]).

v(zeul,zeult,zeulen,gezeuld,zeulde,zeulden,
  [h([transitive,
      part_transitive(mee),
      part_pc_pp(mee,met),
      pc_pp(met)])]).

v(zeur,zeurt,zeuren,gezeurd,zeurde,zeurden,
    [h([intransitive,
	sbar,
	fixed([svp_pp(aan,kop),dat],no_passive),
				% hem aan zijn kop gezeurd
        ap_pred_np,		% ik heb haar gek gezeurd (om ...)
	np_np,			% de oren van de kop/het lijf
	pc_pp(om),
	er_pp_sbar(over),
	pc_pp(over)])]).

v(zever,zevert,zeveren,gezeverd,zeverde,zeverden,
    [h([intransitive])]).

%% Men zie e2-e4 met mat in twee zetten
%% m(zie,verb(zie,hebben,subjunctive,transitive_ndev)).
v(zie,ziet,inflected(zien,ziene),gezien,zag,zagen,
    [h([aci,
	fixed([vc(kom_aan,inf,part_intransitive(aan)),sbar],no_passive),   % ik zie aankomen dat hij blijft
	fixed([vc(kom_aan,inf,part_intransitive(aan)),het_pobj1(sbar)],no_passive),   % VL? ik zie het aankomen dat hij blijft
	fixed([vc(kom,inf,intransitive),er_pp(van,C),het_obj1,extra_sbar(C) ],no_passive),
				% ik zie het er nog van komen dat hij blijft
	fixed([vc(zit,inf,intransitive),het_pobj1(sbar)],no_passive),
	                        % ik zie het niet zitten dat hij komt
	fixed([vc(zit,inf,intransitive),het_pobj1(vp)],no_passive),
	                        % ik zie het niet zitten om te gaan
	fixed([vc(zit,inf,intransitive),acc],no_passive),
	                        % ik zie het niet zitten om te gaan
	als_pred_np,
        ap_copula,    % je ziet bleek
	intransitive,
	sbar,
	sbar_obj,     % ik wil het niet zien dat hij een tegenstander een elleboog geeft .
	transitive_ndev_ndev,
	ld_pp,        % zie onder EVD
	np_pc_pp(aan),
        np_pc_pp(van),    % je ziet er helemaal niets van
        np_mod_pp(op),    % er is niets op te zien (film/foto)
        np_mod_pp(over),  % ik heb er iets over gezien (op tv)
	obj1_passive,     % ziet zich voor problemen gesteld
        refl_passive,

        %%% todo: is this refl_passive; or is refl_passive===obj1_passive?
% 	fixed([[gedwongen],refl,vp],no_passive),
 	fixed([[genoopt],refl,vp],no_passive),
 	fixed([[geroepen],refl,vp],no_passive),
% 	fixed([[verplicht],refl,vp],no_passive),

	part_ap_pred_np(in),
	part_np_np(aan),
        part_fixed_dep(aan,intransitive),  % in: het laat zich aanzien dat..
	part_intransitive(af),
	part_np_er_pc_pp(af,aan),  % dat zie je er niet aan af
	part_pp_sbar(af,aan),
	part_intransitive(in),
	part_intransitive(om),  % in wrok ...
	part_intransitive(op),
	part_intransitive(terug),
	part_intransitive(toe),
	part_intransitive(vooruit),
        np_pc_pp(in),  % we zien er niets in
                       % we zien er een voordeel in ..
        np_er_pp_vp(in),
	part_sbar_subj_so_np(aan),  % het is hem niet aan te zien dat..
	part_sbar(in),
	part_sbar(vooruit),
	part_transitive(aan),
	part_transitive(af),
	part_transitive(in),
	part_pc_pp(in,van),  % er de lol/humor/belang inzien van
	part_transitive(tegemoet),
	part_np_mod_pp(tegemoet,op), 
	part_transitive(terug),
	part_np_pc_pp(terug,van),
	part_transitive(uit),
	part_transitive(vooruit),
	part_transitive(weer),
	part_vp(in),
	pp_sbar(aan),
	pc_pp(op),         %%% ???
	subj_control(te),  % je moet de huurders zien te interesseren
                           % zie dan maar eens ...
	fixed([{[acc(aanleiding),pc(in)]},vp],norm_passive),
	fixed([{[[brood],pc(in)]}],imp_passive),
	fixed([[door,de,vingers],acc],norm_passive),
	fixed([{[[geen,been],pc(in)]},vp],imp_passive),
	fixed([{[[geen,graten],er_pp(in,C)]},extra_sbar(C)],no_passive), % VL
	fixed([{[[geen,graten],er_pp(in,C)]},extra_vp(C)],no_passive),  % VL
	fixed([[het,licht]],no_passive),
	fixed([acc(kans),vp],imp_passive),
	fixed([{[acc(kans),er_pp(tot,C)]},extra_sbar(C)],no_passive),
	fixed([{[acc(kans),er_pp(tot,C)]},extra_vp(C)],no_passive),
	fixed([[schoon],acc(kans)],no_passive),
	fixed([[schoon],acc(kans),vp],no_passive),
	part_fixed(in,[{[pc(van),acc(nut)]}],norm_passive),
	fixed([[onder,ogen],acc],norm_passive),
	fixed([[onder,ogen],sbar],norm_passive),
        fixed([[over,het,hoofd],acc],norm_passive),
        fixed([[over,het,hoofd],sbar],imp_passive),
        fixed([{[pc(tot),acc(reden)]}],no_passive),
        fixed([{[er_pp(tot,C),acc(reden)]},extra_sbar(C)],no_passive),
        fixed([{[er_pp(tot,C),acc(reden)]},extra_vp(C)],no_passive),

        fixed([[rijp],acc(kans),vp],no_passive),
        
	part_er_pp_sbar(toe,op),
	part_sbar(toe),  % ik moest toezien hoe/dat ...
	part_er_pp_vp(af,van),
	part_er_pp_sbar(af,van),
	part_er_pp_sbar(op,tegen),
	part_er_pp_vp(op,tegen),
	part_ld_pp(op),
	part_ld_pp(uit),
	part_np_pc_pp(aan,voor),
	part_pp_sbar_subj_opt_het(uit,naar),
	part_pc_pp(af,van),
	part_np_pc_pp(in,van),  % we zien er de lol van in
	part_pc_pp(op,tegen),
	part_pc_pp(terug,op),
	part_pc_pp(toe,op),
	part_pc_pp(uit,naar),
	fixed([svp_er_pp(uit)],no_passive),
	fixed([svp_er_pp(uit),nonp_pred],no_passive),
	fixed([svp_er_pp(uit),alsof_sbar],no_passive),
	part_intransitive(eruit), % zoals hij eruitziet /
	part_fixed(eruit,[nonp_pred],no_passive), % hoe hij eruitziet /
	                                          % die eruitziet als..
	part_alsof_sbar(eruit),  % hij ziet eruit alsof..
	part_fixed(uit,[svp_er],no_passive), % hij ziet er niet uit; hij zag er uit!!!
	part_fixed(uit,[nonp_pred,svp_er],no_passive)])]).

v(zied,ziedt,zieden,gezoden,ziedde,ziedden,
    [h([intransitive,
	transitive])]).

v(ziek,ziekt,zieken,geziekt,ziekte,ziekten,
    [h([intransitive,
	part_intransitive(uit),
	transitive])]).

v(zieltoog,zieltoogt,zieltogen,gezieltoogd,zieltoogde,zieltoogden,
    [h([intransitive])]).

v(zigzag,zigzagt,zigzaggen,gezigzagd,zigzagde,zigzagden,
    [h([intransitive])]).

v(zijg,zijgt,zijgen,gezegen,zeeg,zegen,
    [b([intransitive,
	part_intransitive(neer),
	part_intransitive(weg),
	part_intransitive(ineen)])]).  

v(zin,zint,zinnen,gezind,zinde,zinden,
    [h([so_np,
	sbar_subj_so_np])]).  % het zint ons niet dat ...

v(zin,zint,zinnen,[gezonnen,gezind],[zon,zinde],[zonnen,zinden],
    [h([pc_pp(op),
	er_pp_sbar(op),
	er_pp_vp(op)])]).

v(zinder,zindert,zinderen,gezinderd,zinderde,zinderden,
    [h([intransitive,
	part_intransitive(na),
	pc_pp(van)])]).

v(zing,zingt,zingen,gezongen,zong,zongen,
    [h([intransitive,
	transitive,
	sbar,
	np_pc_pp(in),
        part_intransitive(mee),
	part_transitive(mee),
	pc_pp(over),
	mod_pp(bij),           % er wordt wel bij gezongen
        mod_pp(doorheen),
	part_intransitive(in),
	part_transitive(in),
        part_transitive(toe),
	part_transitive(uit),  % 'het' uitzingen/ but also full NPs
	part_np_pc_pp(mee,met),
	part_pc_pp(mee,met)])]).

v(zink,zinkt,zinken,gezonken,zonk,zonken,
    [h([part_transitive(af)]),
     z([intransitive,
	ld_pp,
	fixed([[in,de,schoenen],dat,subj(hart)],no_passive),
	fixed([[in,de,schoenen],dat,subj(moed)],no_passive),
	fixed([[in,de,tenen],dat,subj(hart)],no_passive),
	fixed([[in,de,tenen],dat,subj(moed)],no_passive),
	fixed([[in,het,niet]],no_passive),
        part_intransitive(in),
	part_intransitive(weg),
	part_ld_pp(weg)])]).

v(zinspeel,zinspeelt,zinspelen,gezinspeeld,zinspeelde,zinspeelden,
    [h([pc_pp(op),
	er_pp_sbar(op),
        er_pp_vp(op),
	intransitive])]).

%% TODO:
%% er zit iets tegenstrijdigs in dat ..
%% Er zit bovendien iets onmenselijks in van een kind te eisen dat het zich van zijn ouders distantieert .
v(zit,zit,zitten,gezeten,zat,zaten,
    [h([intransitive,
	ld_adv,
	%so_np,
	ld_pp,
	nonp_copula,     % het zit goed; we zitten opgesloten; ze zaten nog maar net in het zadel
	so_nonp_copula,  % die kleding zit me goed
	fixed([ap_pred,dat,sbar_subj],no_passive),  % hoog, niet lekker, dwars
        fixed([[tot,hier],dat],no_passive),
        fixed([[tot,hier],dat,sbar_subj],no_passive),

        so_np_ld_pp,     % het is me in de genen gaan zitten
        
	part_intransitive(dwars),  % dat kan dwars zitten
	part_transitive(dwars),  % hij wordt dwars gezeten
	part_fixed(dwars,[dat,sbar_subj],no_passive),
	part_sbar_subj_np(dwars),
        
	alsof_sbar,
	np_alsof_sbar,
	np_pc_pp(achter),  % achter de broek; passivizes
	np_pc_pp(op),      % op de huid, op de hielen, op de kop; passivizes
	norm_passive,        % er zitten spekjes door verwerkt

	%%% klopt niet: woordvolgorde
	%%% omdat hij zit ondergedoken
        part_intransitive(ondergedoken),
        part_intransitive(opgesloten),
        part_intransitive(gevangen),

	%%% klopt ook niet: woordvolgorde with participles
	%%% hij heeft ondergedoken gezeten (participle gezeten cannot select a participle)
	%%% *hij heeft gezeten ondergedoken

	fixed([vc(duik_onder,psp,part_intransitive(onder))],no_passive),
	fixed([vc(sluit_op,psp,part_intransitive(op))],no_passive),
	fixed([vc(vang,psp,intransitive)],no_passive),

	%%% daarom beide opties tegelijks :-(
	
        part_intransitive(klem),
%	part_intransitive(in),
	part_intransitive(neer),
        part_intransitive(mee),
	part_intransitive(op),
	part_intransitive(rond),  % omdat de hartens rondzitten
	part_intransitive(stil),
	part_intransitive(tegen),
	part_intransitive(vast),
	part_intransitive(vol),
	part_intransitive(voor),
	part_so_np(mee),
	part_transitive(achterna),
	part_transitive(op),
	part_transitive(uit),
	part_transitive(voor),
	pc_pp(aan),
	pc_pp(achter),
	pc_pp(met),
	pc_pp(onder),
	part_intransitive(onder), % hij zat helemaal onder
	er_pp_sbar(met),  % ik kan er niet mee zitten dat ...
        so_np_pc_pp(tot), % dat zit me tot hier
        
	pc_pp(voor),   % ? ik ga er eens goed voor zitten
	pc_pp(zonder),
	subj_control(wk_te),  % hij zit te zeuren
	fixed([subj(schot),pc(in)],no_passive),
	fixed([subj(verschil),pc(tussen)],no_passive),
	fixed([{[[in,de,knoop],pc(met)]}],no_passive),
	fixed([{[[omhoog]]}],no_passive),
	fixed([{[[omhoog],pc(met)]}],no_passive),


	% dat zit er dik in
	fixed([{[[dik],svp_er_pp(in)]}],no_passive),
	% de kans zit er dik in dat ..
	fixed([{[[dik],svp_er_pp(in)]},sbar],no_passive),
	% het zit er dik in dat ..
	fixed([sbar_subj,{[[dik],svp_er_pp(in)]}],no_passive),
	% dat zit er wel in
	fixed([svp_er_pp(in)],no_passive),
	% de kans zit er in dat hij komt
	fixed([svp_er_pp(in),sbar],no_passive),
	% het zit er in dat hij komt
	fixed([sbar_subj,svp_er_pp(in)],no_passive),
	% omdat het er inzit
        part_fixed(in,[svp_er],no_passive),
	% omdat de kans er inzit dat hij komt
        part_fixed(in,[svp_er,sbar],no_passive),
	% omdat het er inzit dat hij komt
        part_fixed(in,[sbar_subj,svp_er],no_passive),
	% omdat het er dik inzit
        part_fixed(in,[{[[dik],svp_er]}],no_passive),
	% omdat de kans er dik inzit dat hij komt
        part_fixed(in,[{[[dik],svp_er]},sbar],no_passive),
	% omdat het er dik inzit dat hij komt
        part_fixed(in,[sbar_subj,{[[dik],svp_er]}],no_passive),

	% het zat er aan te komen dat hij komt
	fixed([vc(kom,te,intransitive),sbar_subj,er_pp(aan)],no_passive),
	
	fixed([svp_er_pp(op)],no_passive),  % het zit er weer op = it is finished
	fixed([svp_er_pp(op),[bovenarms]],no_passive), % VLAAMS ruzie maken?
	part_fixed(op,[[bovenarms],svp_er],no_passive), % idem
	
	fixed([[in,de,haren],dat],no_passive),
	fixed([[in,de,weg],dat],no_passive),
	fixed([[in,de,weg]],no_passive),
	fixed([subj(klad),pc(in)],no_passive),
	fixed([[snor]],no_passive),
	fixed([{[ld_adv,svp_dat(hem)]}],no_passive),  % het zit 'm in de details
	fixed([{[ld_pp,svp_dat(hem)]}],no_passive),  % het zit 'm in de details
                                % NB 'm < SU
                                % daarin zit 'm de charme
	fixed([{[er_pp(in,C),svp_dat(hem)]},extra_sbar(C)],no_passive),
	fixed([[in,de,koude,kleren],dat],no_passive),
	fixed([[in,de,kouwe,kleren],dat],no_passive),
	fixed([[in,het,nieuw]],no_passive),
	fixed([svp_pp(in,maag),pc(met)],no_passive),
	fixed([[op,de,hielen],acc],norm_passive),
	fixed([{[pc(met),ap_pred(vervelen)]}],no_passive), % VLAAMS
	fixed([{[er_pp(met,C),ap_pred(vervelen)]},extra_sbar(C)],no_passive), % VLAAMS
	fixed([[in,het,bloed],dat],no_passive),
	part_ld_pp(aan),
	part_ld_pp(neer),
	part_ld_pp(vast),
	part_pc_pp(in,over),
	part_pc_pp(in,met), % VL ik zit er soms mee in
        part_er_pp_sbar(in,over)]),
     b([part_intransitive(aan)])]).

v(zoef,zoeft,zoeven,gezoefd,zoefde,zoefden,
    [b([ld_pp,
	part_intransitive(aan),
	ld_dir]),
     h([intransitive])]).

v(zoek,zoekt,zoeken,gezocht,zocht,zochten,
    [h([intransitive,
	transitive,
	vp,   % hij zocht mensen te overtuigen
	np_ld_pp,
	part_als_pred_np(aan),
	part_sbar(op),
	part_sbar(uit),
	part_transitive(aan),
	part_np_vp_obj1(aan), % hij werd aangezocht door de koningin om ...
	part_transitive(af),
	part_pc_pp(af,naar),
	part_transitive(op),
	part_transitive(uit),
	part_np_pc_pp(uit,op),
	np_mod_pp(voor),  % uitlaatklep,oplossing,alternatief zoeken voor ...
	pc_pp(naar),
        pc_pp(op),  % zoekterm
        sbar, % zoeken of er nog iets is
	fixed([{[acc(antwoord),pc(op)]}],norm_passive),
	fixed([{[acc(contact),pc(met)]}],norm_passive),
	part_np_ld_pp(op),
	part_np_ld_adv(op),
	part_np_pc_pp(aan,voor),
	part_np_pc_pp(af,naar)])]).

v(zoem,zoemt,zoemen,gezoemd,zoemde,zoemden,
    [h([intransitive,
	part_intransitive(in),
	part_pc_pp(in,op),
        sbar])]).

v(zoen,zoent,zoenen,gezoend,zoende,zoenden,
    [h([intransitive,
	fixed([[gedag]],imp_passive),
	fixed([[gedag],dat],imp_passive),
	transitive])]).

v(zomer,zomert,zomeren,gezomerd,zomerde,zomerden,
      [h([het_subj])]).

v(zoet,zoet,zoeten,gezoet,zoette,zoetten,
    [unacc([intransitive]),
     h([transitive])]).

v(zon,zont,zonnen,gezond,zonde,zonden,
    [h([intransitive])]).

v(zonder,zondert,zonderen,gezonderd,zonderde,zonderden,
    [h([part_refl(af),
	part_transitive(af),
	part_transitive(uit),
	part_np_ld_pp(af),
	part_np_pc_pp(af,van),
	part_np_pc_pp(uit,van),
	part_refl_ld_pp(af),
	part_refl_pc_pp(af,van)])]).

v(zondig,zondigt,zondigen,gezondigd,zondigde,zondigden,
    [h([intransitive,
	pc_pp(tegen)])]).

v(zonnebaad,zonnebaadt,zonnebaden,gezonnebaad,zonnebaadde,zonnebaadden,
    [h([intransitive])]).

v(zoog,zoogt,zogen,gezoogd,zoogde,zoogden,
    [h([intransitive,
        transitive])]).

v(zoom,zoomt,zoomen,gezoomd,zoomde,zoomden,
    [h([intransitive,
	part_intransitive(in),
	part_intransitive(uit),
	part_pc_pp(in,op)])]).

v(zorg,zorgt,zorgen,gezorgd,zorgde,zorgden,
    [h([intransitive,
	sbar,
	pp_sbar_subj(voor),  % het zorgde voor veel oproer, dat ...
	vp, % Vliegensvlug zorgden ze hier weg te komen
	er_pp_sbar(voor),
	er_pp_vp(voor),
	pc_pp(voor)])]).

v(zout,zout,zouten,gezouten,zoutte,zoutten,
    [h([transitive,
        part_intransitive(op),
        part_transitive(op)])]).

v(zucht,zucht,zuchten,gezucht,zuchtte,zuchtten,
    [h([intransitive,
	sbar,  % dip...
	pc_pp(naar),
	pc_pp(van)])]).

v(zuig,zuigt,zuigen,gezogen,zoog,zogen,
    [h([intransitive,
	np_ld_dir,
	transitive,
	np_ld_pp,
	ap_pred_np,
	part_transitive(aan),
	part_transitive(af),
        part_transitive(mee),
	part_transitive(op),
	part_transitive(uit),
	part_transitive(weg),
	part_np_ld_pp(af),
	part_np_ld_pp(mee), % je werd er in meegezogen
	part_np_ld_pp(op),
	part_np_ld_pp(weg),
	pc_pp(aan),
	pc_pp(op)])]).

v(zuip,zuipt,zuipen,gezopen,zoop,zopen,
    [h([ap_pred_refl,
	intransitive,
	transitive])]).

v(zuiver,zuivert,zuiveren,gezuiverd,zuiverde,zuiverden,
    [h([transitive,
	np_pc_pp(van),
	part_transitive(aan)])]).

v(zwaai,zwaait,zwaaien,gezwaaid,zwaaide,zwaaiden,
     [z([part_intransitive(af)]),
      h([intransitive,
	 ld_dir,
	 np_ld_dir,
	 transitive,
	 fixed([[gedag]],imp_passive),
	 fixed([[gedag],dat],imp_passive),
	 ld_pp,
	 np_ld_pp,
	 part_transitive(na),
	 part_transitive(uit),
	 pc_pp(met),
	 part_so_np(toe),	% de koningin zwaaide ons toe
	 part_np_np(toe)	% lof toezwaaien
	])]).

v(zwabber,zwabbert,zwabberen,gezwabberd,zwabberde,zwabberden,
    [h([intransitive,
	transitive])]).

v(zwak,zwakt,zwakken,gezwakt,zwakte,zwakten,
    [unacc([part_intransitive(af)]),
     h([part_transitive(af)])]).

v(zwalk,zwalkt,zwalken,gezwalkt,zwalkte,zwalkten,
    [b([ld_dir,
	part_intransitive(aan),
	part_intransitive(rond),
	ld_pp]),
     h([intransitive])]).

v(zwalp,zwalpt,zwalpen,gezwalpt,zwalpte,zwalpten,
    [b([ld_dir,
	part_intransitive(aan),
	part_intransitive(rond),
	ld_pp]),
     h([intransitive])]).

v(zwam,zwamt,zwammen,gezwamd,zwamde,zwamden,
    [h([intransitive,
        sbar])]).

v(zweef,zweeft,zweven,gezweefd,zweefde,zweefden,
    [h([intransitive,
	part_intransitive(rond)
       ]),
     b([ld_dir,
	ld_adv,
	ld_pp,
	part_intransitive(aan)])]).

v(zweem,zweemt,zwemen,gezweemd,zweemde,zweemden,
    [h([pc_pp(naar),
	intransitive])]).

v(zweep,zweept,zwepen,gezweept,zweepte,zweepten,
    [h([transitive,
	part_sbar_subj_so_np(op),
	part_transitive(op),
	part_vp_subj_so_np(op),
	pc_pp(met),
	intransitive
       ])]).

v(zweer,zweert,zweren,gezworen,[zweerde,zwoer,zwoor],[zweerden,zwoeren,zworen],
    [h([intransitive,
	np_sbar,
	np_vp_subj,
	sbar,
	so_pp_np,
	np_np,
	transitive,
	vp,
	part_transitive(af),
	part_transitive(in),
	pc_pp(bij),
	pc_pp(op)])]).

v(zweet,zweet,zweten,gezweet,zweette,zweetten,
    [h([intransitive,
	transitive,
	pc_pp(op)])]).

v(zwel,zwelt,zwellen,[gezwollen,gezweld],[zwol,zwelde],[zwollen,zwelden],
    [unacc([intransitive,
	    part_intransitive(aan),
	    part_intransitive(op)]),
     h([transitive])]).

v(zwelg,zwelgt,zwelgen,gezwolgen,zwolg,zwolgen,
    [h([intransitive])]).

v(zwem,zwemt,zwemmen,gezwommen,zwom,zwommen,
  [h([intransitive,
      part_intransitive(rond),  % de dolfijnen die daar rondzwemmen
      transitive]),    % een record; een programmanummer,de 100 meter;
   b([ld_dir,	       % het kanaal over
      ld_adv,
      ld_pp,
      part_intransitive(aan),
      part_intransitive(terug)
     ])]).

v(zwengel,zwengelt,zwengelen,gezwengeld,zwengelde,zwengelden,
  [z([part_intransitive(aan)]),
   h([part_transitive(aan),
	transitive])]).

v(zwenk,zwenkt,zwenken,gezwenkt,zwenkte,zwenkten,
    [z([ld_dir,
	part_ld_pp(af),
	ld_pp]),
     h([intransitive,
	transitive])]).

v(zwerf,zwerft,zwerven,gezworven,zwierf,zwierven,
    [h([intransitive,
	ld_pp,
	ld_adv,
	part_intransitive(af),
	part_intransitive(rond),
	part_intransitive(uit),
	part_ld_pp(uit),
	part_ld_adv(uit),
	part_ld_pp(rond),
	part_ld_adv(rond)])]).

v(zwerm,zwermt,zwermen,gezwermd,zwermde,zwermden,
    [b([intransitive,
	part_intransitive(uit),
	part_ld_pp(uit),
	part_ld_adv(uit),
	ld_adv,
	ld_pp])]).

v(zwets,zwetst,zwetsen,gezwetst,zwetste,zwetsten,
    [h([intransitive,
	part_intransitive(af),
	part_intransitive(bij),
	part_transitive(na),
        part_pc_pp(aan,tegen),
	np_ld_pp,
	np_ld_adv,
	mod_pp(met),
	pc_pp(over)])]).

v(zwicht,zwicht,zwichten,gezwicht,zwichtte,zwichtten,
    [unacc([intransitive,
	    pc_pp(onder),
	    pc_pp(voor)])]).

v(zwiep,zwiept,zwiepen,gezwiept,zwiepte,zwiepten,
    [h([intransitive,
	np_ld_dir,
	transitive,
	np_ld_pp,
	pc_pp(met)])]).

v(zwier,zwiert,zwieren,gezwierd,zwierde,zwierden,
    [b([ld_pp]),
     h([intransitive,
        np_ld_pp,
        np_ld_dir  % hij zwierde beide handen in de hoogte / de hoogte in (VL)
       ])]).

v(zwijg,zwijgt,zwijgen,gezwegen,zweeg,zwegen,
    [h([intransitive,
        so_np_pc_pp(van),   % VL: zwijg me ervan
        so_np_pc_pp(over),  % VL: zwijg me over ..
	part_intransitive(stil),
	part_transitive(dood),
	pc_pp(op),
	pc_pp(over),
	pc_pp(van)])]).

v(zwijmel,zwijmelt,zwijmelen,gezwijmeld,zwijmelde,zwijmelden,
    [h([intransitive,
	sbar,
	part_intransitive(weg)])]).

v(zwijn,zwijnt,zwijnen,gezwijnd,zwijnde,zwijnden,
    [h([intransitive])]).

v(zwik,zwikt,zwikken,gezwikt,zwikte,zwikten,
    [h([intransitive])]).

v(zwoeg,zwoegt,zwoegen,gezwoegd,zwoegde,zwoegden,
    [h([intransitive,
	pc_pp(op)])]).

psp_only(gelieerd,
	 gelieerd,
         zijn,
         [pc_pp(aan),
          pc_pp(met)]).

psp_only(v_root(moei,moeien),
	 gemoeid,
         zijn,
         [pc_pp(met)]).

psp_only(v_root(bedenk,bedenken),
	 bedacht,
         zijn,
         [pc_pp(op),
          er_pp_sbar(op),
          er_pp_vp(op)]).

psp_only(behept,
	 behept,
	 zijn,
	 [pc_pp(met),
	  er_pp_sbar(met),
	  er_pp_vp(met)
	  ]).

%% dat hem geen lang leven is beschoren (toebedeeld)
psp_only(beschoren,
	 beschoren,
         zijn,
         [so_np]).

psp_only(v_root(baat,baten),
	 gebaat,
         zijn,
         [pc_pp(bij),
          er_pp_vp(bij),
          er_pp_sbar(bij),
          pc_pp(met),
          er_pp_vp(met),
          er_pp_sbar(met)]).

%% transitive: 'het doelpunt werd geboren uit een briljante actie'/zonnegloren
psp_only(geboren,
	 geboren,
         hebben,
         [transitive,
	  als_pred_np,
          np_pc_pp(uit)]).

psp_only(gecharmeerd,
	 gecharmeerd,
         zijn,
         [pc_pp(van),
          er_pp_sbar(van),
          er_pp_vp(van)]).

psp_only(geoorloofd,
	 geoorloofd,
         zijn,
         [vp_subj,
          vp_subj_so_np]).

psp_only(inbegrepen,
	 inbegrepen,
         zijn,
         [intransitive,
          pc_pp(in),
          er_pp_sbar(in)]).

psp_only(opgewassen,
	 opgewassen,
         zijn,
         [pc_pp(tegen)]).

psp_only(toegedaan,
	 toegedaan,
         zijn,
         [so_np]).

psp_only(geruggesteund,
	 geruggesteund,
	 hebben,
	 [transitive]).

%% omdat we hem dankbaarheid zijn verschuldigd
%% ook passief: omdat er dankbaarheid aan hem verschuldigd is
psp_only(verschuldigd,
	 verschuldigd,
         zijn,
         [transitive,
	  so_pp_np,
	  np_np]).

