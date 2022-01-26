:- module(alpino_ngram_lm, [ evaluate_fluency/0,
			     fluency_model_initialize/0,
			     phrase_fluency/2,
			     unigram_fluency/2,
			     sentence_fluency/2,
			     phrase_tag_prob/2,
			     sentence_tag_prob/2 ]).

foreign(pro_init_lm, c, init_lm0(+term,+term,+term,+term,+term,+term)).
foreign(phrase_fluency, c, phrase_fluency0(+term,[-float])).
foreign(unigram_fluency, c, unigram_fluency0(+term,[-float])).
foreign(sentence_fluency, c, sentence_fluency0(+term,[-float])).
foreign(phrase_tag_prob, c, phrase_tag_prob0(+term,[-float])).
foreign(sentence_tag_prob, c, sentence_tag_prob0(+term,[-float])).

foreign_resource(pro_lm, [ pro_init_lm,
			   phrase_fluency,
			   unigram_fluency,
			   sentence_fluency,
			   phrase_tag_prob,
			   sentence_tag_prob,
			   init(lm_init_hook),
			   deinit(lm_deinit_hook)
			 ]).

:- load_foreign_resource(pro_lm).

:- if(current_predicate(with_mutex/2)).

:- else.

:- meta_predicate with_mutex(?,:).
with_mutex(_,Call) :-
    once(Call).

:- endif.

fluency_model_initialize :-
    with_mutex(fadd,fluency_model_initialize_).

:- dynamic fluency_model_initialized/0.

fluency_model_initialize_ :-
    (   fluency_model_initialized
    ->  true
    ;   hdrug_util:hdrug_flag(ngram_model_dir,Dir0),
	absolute_file_name(Dir0,Dir),
	atom_concat(Dir,'/words.fsa',Words),
	atom_concat(Dir,'/unigrams.tpl',Unigrams),
	atom_concat(Dir,'/bigrams.tpl',Bigrams),
	atom_concat(Dir,'/trigrams.tpl',Trigrams),
	atom_concat(Dir,'/tags.fsa',Tags),
	atom_concat(Dir,'/tag-unigrams.tpl',TagUnigrams),
	atom_concat(Dir,'/tag-bigrams.tpl',TagBigrams),
	atom_concat(Dir,'/tag-trigrams.tpl',TagTrigrams),
	hdrug_util:debug_message(1,"Initializing fluency dictionaries...~n",[]),
	init_lm([Unigrams,Words],[Bigrams,Words,Words],
		[Trigrams,Words,Words,Words],
		[TagUnigrams,Tags],[TagBigrams,Tags,Tags],
		[TagTrigrams,Tags,Tags,Tags]),
	assertz(fluency_model_initialized),
	hdrug_util:debug_message(1,"Initialized fluency dictionaries~n",[])
    ).

init_lm(WordUnigramList0,WordBigramList0,WordTrigramList0,
	TagUnigramList0,TagBigramList0,TagTrigramList0) :-
    absolute_file_name_list(WordUnigramList0,WordUnigramList),
    absolute_file_name_list(WordBigramList0,WordBigramList),
    absolute_file_name_list(WordTrigramList0,WordTrigramList),
    absolute_file_name_list(TagUnigramList0,TagUnigramList),
    absolute_file_name_list(TagBigramList0,TagBigramList),
    absolute_file_name_list(TagTrigramList0,TagTrigramList),
    init_lm0(WordUnigramList,WordBigramList,WordTrigramList,
	     TagUnigramList,TagBigramList,TagTrigramList).

absolute_file_name_list([],[]).
absolute_file_name_list([H0|T0],[H|T]) :-
    absolute_file_name(H0,H),
    absolute_file_name_list(T0,T).

sentence_fluency(Sent0,P) :-
    fluency_model_initialize,
    atoms_to_words(Sent0,Sent),
    sentence_fluency0(Sent,P).

sentence_tag_prob(Tags,P) :-
    fluency_model_initialize,
    sentence_tag_prob0(Tags,P).

phrase_fluency([W0,W1|Sent0],P) :-
    fluency_model_initialize,
    atoms_to_words([W0,W1|Sent0],Sent),
    phrase_fluency0(Sent,P).

unigram_fluency([S|Sent0],P) :-
    fluency_model_initialize,
    atoms_to_words([S|Sent0],Sent),
    unigram_fluency0(Sent,P).

phrase_tag_prob([T0,T1|Tags],P) :-
    fluency_model_initialize,
    phrase_tag_prob0([T0,T1|Tags],P).

atoms_to_words([],[]).
atoms_to_words([H|T],Words) :-
    atom_codes(H,Codes),
    alpino_util:codes_to_words(Codes,Words,Words1),
    atoms_to_words(T,Words1).

evaluate_fluency :-
    findall(String,user:a_sentence(_,String),Strings),
    length(Strings,Len),
    sentence_fluency_list(Strings,0,P),
    format(user_error,"total score ~w after ~w sentences~n",[P,Len]).

sentence_fluency_list([],P,P).
sentence_fluency_list([H|T],P0,P) :-
    sentence_fluency(H,P1),
    P2 is P0 + P1,
    sentence_fluency_list(T,P2,P).

