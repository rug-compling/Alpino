:- module(alpino_guides, [ check_predict/2,
			   check_connect/2,
			   tr_tag/2
			 ]).

:- expects_dialect(sicstus).

:- use_module(library(terms)).
:- use_module(library(lists)).

check_connect(F,PREV) :-
    create_index(g(F,PREV),Index),
    bool_vector_member(Index).

check_predict(F,PREV) :-
    create_index(g(F,[PREV]),Index),
   bool_vector_member(Index).

%%    format(user_error,"predict succeeds: ~w ~w~n",[g(F,[PREV]),Index]).

% check_connect(F,PREV) :-
%    create_index(g(F,PREV),Index),
%    (   bool_vector_member(Index)
%    ->  true
%    ;   format(user_error,"connect failed: ~w ~w~n",[F,PREV]),
% 	fail
%    ).

% check_predict(F,PREV) :-
%    create_index(g(F,[PREV]),Index),
%    (   bool_vector_member(Index)
%    ->  true
%    ;   format(user_error,"predict failed: ~w ~w~n",[F,PREV]),
% 	fail
%    ).


%% Term is almost always ground.
%% therefore, only numbervars if
%% we realize it was not.
create_index(Term,Index) :-
    term_hash(Term,Index),
    (   var(Index)
    ->  numbervars(Term,0,_),
	term_hash(Term,Index)
    ;   true
    ).

foreign_resource(guides, [ bool_vector_member ]).

foreign(bool_vector_member,c,bool_vector_member(+integer,[-integer])).

bool_vector_member(Int) :-
    bool_vector_member(Int,1).

:- load_foreign_resource(guides).

tr_tag(with_dt(A,_X),B) :-
    !,
    tr_tag(A,B).

%% keep form only for potential hdf-postpositions
%% probably these ought to be their own tags,
%% because quite different distribution
tr_tag(particle(Form), Tag ) :-
    !,
    (   member(Form,
	       [af,aan,[aan,toe],door,[en,al],heen,in,
		[in,de,plaats],langs,mee,na,om,op,toe,
		uit,vandaan])
    ->  Tag = particle(Form)
    ;   Tag = particle
    ).

tr_tag(preposition(A,_),Tag) :-
    !,
    (   member(A,[van,in,op,met,voor,aan,bij,uit,over,naar,door,na,
		  tegen,tot,volgens,tussen,om,onder,tijdens,achter,
		  met,per,binnen,aldus,vanwege,sinds,zonder,via,
		  vanaf])
    ->  Tag = preposition(A)
    ;   Tag = preposition
    ).

tr_tag(preposition(A,_,Sc),Tag) :-
    !,
    (   member(A,[van,in,op,met,voor,aan,bij,uit,over,naar,door,na,
		  tegen,tot,volgens,tussen,om,onder,tijdens,achter,
		  met,per,binnen,aldus,vanwege,sinds,zonder,via,
		  vanaf])
    ->  Tag = preposition(A,Sc)
    ;   Tag = preposition(Sc)
    ).

tr_tag(er_adverb([_|_]),Tag) :-
    !,
    Tag = er_adverb(complex).

tr_tag(waar_adverb([_|_]),Tag) :-
    !,
    Tag = waar_adverb(complex).

tr_tag(adjective(Infl0),adjective(Infl)) :-
    !,
    adj_infl(Infl0,Infl).

tr_tag(adjective(Infl0,Sc0),adjective(Infl,Sc)) :-
    !,
    adj_infl(Infl0,Infl),
    tr_sc(Sc0,Sc).

tr_tag(fixed_part(_),Tag) :-
    !,
    Tag=fixed_part.

%% TMP
tr_tag(determiner(onze),Tag) :-
    !,
    Tag=determiner(de).

tr_tag(verb(A,B,C), verb(A,NB,NC)) :-
    !,
    tr_infl(B,NB),
    tr_sc(C,NC).

tr_tag(noun(DeHet,_,Num), Tag) :-
    !,
    Tag = noun(DeHet,Num).
tr_tag(noun(DeHet,_,Num,Sc), Tag) :-
    !,
    Tag = noun(DeHet,Num,Sc).
tr_tag(meas_mod_noun(DeHet,_,Num), Tag) :-
    !,
    Tag = meas_mod_noun(DeHet,Num).
tr_tag(meas_mod_noun(DeHet,_,Num,Sc), Tag) :-
    !,
    Tag = meas_mod_noun(DeHet,Num,Sc).
tr_tag(amoun_meas_mod_noun(DeHet,_,Num), Tag) :-
    !,
    Tag = amoun_meas_mod_noun(DeHet,Num).
tr_tag(amoun_meas_mod_noun(DeHet,_,Num,Sc), Tag) :-
    !,
    Tag = amoun_meas_mod_noun(DeHet,Num,Sc).
tr_tag(mod_noun(DeHet,_,Num), Tag) :-
    !,
    Tag = mod_noun(DeHet,Num).
tr_tag(mod_noun(DeHet,_,Num,Sc), Tag) :-
    !,
    Tag = mod_noun(DeHet,Num,Sc).
tr_tag(tmp_noun(DeHet,_,Num), Tag) :-
    !,
    Tag = tmp_noun(DeHet,Num).
tr_tag(tmp_noun(DeHet,_,Num,Sc), Tag) :-
    !,
    Tag = tmp_noun(DeHet,Num,Sc).
tr_tag(ge_nominalized_adjective,Tag) :-
    !,
    Tag = nominalized_adjective.
tr_tag(end_nominalized_adjective,Tag) :-
    !,
    Tag = nominalized_adjective.
tr_tag(ge_nominalized_adjective(SC),Tag) :-
    !,
    Tag = nominalized_adjective(SC).
tr_tag(end_nominalized_adjective(SC),Tag) :-
    !,
    Tag = nominalized_adjective(SC).
tr_tag(conj(_),Tag) :-
    !,
    Tag = conj.
tr_tag(left_conj(_),Tag) :-
    !,
    Tag = left_conj.
tr_tag(right_conj(_),Tag) :-
    !,
    Tag = right_conj.
tr_tag(adv_tag,Tag) :-
    !,
    Tag = tag.
tr_tag(Tag,Tag).

adj_infl(aller_st(X),St) :-
    !,
    St = st(X).
adj_infl(X,X).

tr_sc(ninv(A,_B),ninv(NA)) :-
    !,
    tr_sc(A,NA).
tr_sc(ninv(A),ninv(NA)) :-
    !,
    tr_sc(A,NA).
% tr_sc(fixed(A,B),fixed(NA,B)) :-
%     !,
%     tr_fixed(A,NA).
tr_sc(part_fixed(Part,_,_),R) :-
    !,
    R = fixed(Part).
tr_sc(fixed(_,_),R) :-
    !,
    R = fixed.
tr_sc(fixed(_),R) :-
    !,
    R = fixed.
tr_sc(pp_sbar_subj(uit),pp_sbar_subj_opt_het(uit)) :-  % TMP
    !.
tr_sc(ap_copula(_),R) :-
    !,
    R = ap_copula.
tr_sc(pp_copula(_,_),R) :-
    !,
    R = pp_copula.
tr_sc(pp_pred_np(_,_),R) :-
    !,
    R = pp_pred_np.
tr_sc(pp_pred_np_vp(_,_),R) :-
    !,
    R = pp_pred_np_vp.
tr_sc(pp_pred_np_sbar(_,_),R) :-
    !,
    R = pp_pred_np_sbar.
tr_sc(S,S).

/*
tr_fixed([],[]).
tr_fixed([H|T],[NH|NT]) :-
    tr_fixed_el(H,NH),
    tr_fixed(T,NT).

tr_fixed_el({X},{NX}) :-
    !,
    tr_fixed(X,NX).
tr_fixed_el(acc(_),acc) :-
    !.
tr_fixed_el(vc(_,Inf,_),vc(Inf)) :-
    !.
tr_fixed_el([_|_],[]) :-
    !.
tr_fixed_el(pp_pred(_,_),pp_pred) :-
    !.
tr_fixed_el(ap_pred(_),ap_pred) :-
    !.
tr_fixed_el(np_pred(_),np_pred) :-
    !.
tr_fixed_el(inv_pron_acc(_),inv_pron_acc) :-
    !.
tr_fixed_el(svp_dat(_),svp_dat) :-
    !.
tr_fixed_el(svp_acc(_),svp_acc) :-
    !.
tr_fixed_el(svp_pp(_,_),svp_pp) :-
    !.
tr_fixed_el(i(Cat0,Cat),Cat) :-
    !,
    tr_fixed_el(Cat0,Cat).
tr_fixed_el(yt(Cat0),yt(Cat)) :-
    !,
    tr_fixed_el(Cat0,Cat).
tr_fixed_el(nt(Cat0),nt(Cat)) :-
    !,
    tr_fixed_el(Cat0,Cat).
tr_fixed_el(inv(Cat0),inv(Cat)) :-
    !,
    tr_fixed_el(Cat0,Cat).
tr_fixed_el(subj(_),subj) :-
    !.
tr_fixed_el(X,X).
*/

tr_infl(past(X),X) :-
    !.
tr_infl(both(X),X) :-
    !.
tr_infl(X,X).
