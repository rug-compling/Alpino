:- module(alpino_fluency_maxent, [ sentence_fluency_maxent/4 ]).

sentence_fluency_maxent(Cat,Tree,P,Features) :-
    count_maxent_features(Cat,Tree,Features),
    feature_weights(Features,0.0,P).

%% lm       -> (n-gram) language model score.
%%
%% TO TRY:
%%
%% - Punctuation domination.
%%

count_maxent_features(Cat,Tree,Features) :-
    alpino_data:cat_to_result(Cat,Result),
    alpino_dt:result_to_dt_simple(Result,DT),
%    alpino_data:result_frames(Result,Frames),
    alpino_cg:frames_of_deriv(Tree,Frames),
    alpino_penalties:construction_features(Tree,DT,Frames,InitFeatures,Features1),
    output_features(Tree,Features1,[]),
%   for now
    InitFeatures=Features.
%    hdrug_util:hdrug_flag(application_type,Domain),
%    alpino_penalties:domain_features(InitFeatures,Features,Domain).

% sentence(Tree,Tokens0,Tokens,Tags0,Tags)

sentence(tree(_,_,Ds),W0,W,T0,T) :-
    sentence_ds(Ds,W0,W,T0,T).
sentence(tree(_,_,Ds,_),W0,W,T0,T) :-
    sentence_ds(Ds,W0,W,T0,T).

sentence_ds([],W,W,T,T).
sentence_ds(lex(ref(Tag0,_,_,Surf0,_,_,_,_,_,_,_)),[Surf|W],W,[Tag|T],T) :-
    !,
    hdrug_util:term_atom(Tag0,Tag),
    hdrug_util:term_atom(Surf0,Surf).
sentence_ds(lex(Skip),[Skip|W],W,[skip|T],T) :-  % only for parser
    atom(Skip).
sentence_ds([H|L],W0,W,T0,T) :-
    sentence(H,W0,W1,T0,T1),
    sentence_ds(L,W1,W,T1,T).

output_features(Tree,[ngram_lm-PW,ngram_tag-PT|Fs0],Fs) :-
    sentence(Tree,Tokens,['<END>'],Tags,['<END>']),
    alpino_cg:phrase_fluency(['<START>','<START>'|Tokens],PW),
    alpino_cg:phrase_tag_prob(['<START>','<START>'|Tags],PT),
    paraphrase_features(Tokens,Fs0,Fs).

paraphrase_features(Tokens0,[para_gtm-GTM|Fs],Fs) :-
    hdrug_util:hdrug_flag(copy_input_if_paraphrase_failed,on(_,Words0)),
    !,
    lists:append(Tokens0,['<END>'],Tokens),
    lists:append(Words0,['<END>'],Words),
    alpino_geneval:gtm(['<START>','<START>'|Tokens],['<START>','<START>'|Words],GTM).
paraphrase_features(_,Fs,Fs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Feature weight application %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

feature_weights([],S,S).
feature_weights([P|Ps],S0,S) :-
    gen_feature(P,Feature,Count),
    try_feature_weight(Feature,S1),
    S2 is S0+(Count*S1),
    feature_weights(Ps,S2,S).

gen_feature(Feature-Count,F,C) :-
    !,
    Feature=F,
    Count=C.
gen_feature(Feature,Feature,1).

try_feature_weight(P,S) :-
    (   alpino_fluency_weights:feature_weight(P,S0)
    ->  true
    ;   S0 = 0.0
    ),
    (   additional_weight(P,S1)
    ->  true
    ;   S1 = 0.0
    ),
    S is S0 + S1.

%% can be trained, after re-training to be removed
%% for reasons I can't understand, this feature gets a
%% *negative* weight after training, and most of our results
%% get an added question mark, unless they are explicitly
%% declarative
%% additional_weight(extra_question_mark, 2.0).

%% additional_weight(para,-1).

%% prefer similarity to input!
%% idea by Gosse
additional_weight(para_gtm,-3).

additional_weight(q(q),        -2.0).
additional_weight(q(yesno),    -2.0).
additional_weight(coord(en,sg), 1.0).

/*
additional_weight(r1(imp_mod_imp), 1.0).

additional_weight(mf(mcat_sbar,  _),    5.0).
additional_weight(mf(mcat_redrel,_),    5.0).
additional_weight(mf(mcat_vp,    _),    5.0).

additional_weight(r2(dp_dp_root,2,'.'),1).
additional_weight(r2(dp_dp_root,2,':'),0.5).
*/

additional_weight(r1(start_start_dubb_punt),-3.0) :-
    hdrug_util:hdrug_flag(treex_corrections,on).
additional_weight(r1(sat_nucl(sbar)),-3.0) :-
    hdrug_util:hdrug_flag(treex_corrections,on).


additional_weight(lexical_choice({[heb,ben]},Surf),0.2):-
    lists:member(Surf,[ben,was,is,zijn,waren,bent]).

