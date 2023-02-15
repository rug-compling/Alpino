:- module(alpino_unknowns, [ guess_unknown_words/2,
			     guess_names/1,
			     guess_slash_pairs/0,
			     guess_eng_compounds/1,
			     add_quoted_names/5,
			     normal_capitalized_word/5,
			     special_capitalized_word/6
			   ]).

:- expects_dialect(sicstus).

:- use_module(latin1).
:- use_module(hdrug(hdrug_util)).
:- use_module(utils).
:- use_module('../Names/classify_named_entity').
:- use_module(library(lists)).

:- ensure_loaded('../Names/name_accents').

:- discontiguous
    unknown_word_heuristic/8,
    guess_compound/4.

%% treat capitalized word as normal, if
%% - at beginning of sentence, remove capital
%% Vandaag --> vandaag
%% some very common words are always also treated as normal
%% - specified as such below
%% De --> de
normal_capitalized_word(P,Word,Rest,Rest,DecapWord) :-
    (   is_start_sentence(P)
    ->  true
    ;   is_decap_only(Word)
    ;   decap_only_one(Word)
    ),
    decap_first(Word,DecapWord).
%    ;   Rest = [],                       % krantenkopjes zoals "TIPS"
%	only_capitals(Word,DecapWord)
%    ).

normal_capitalized_word(P,Word,[Word1|Rest],[Word2|Rest],Word) :-
    is_start_sentence(P),
    starts_with_quote(Word),
    decap_first(Word1,Word2).

starts_with_quote('\'n').
starts_with_quote('\'s').
starts_with_quote('\'t').
starts_with_quote('\'k').

%% if it is written in full capitals, find all analyses
%% with lower case
%% HET --> het
special_capitalized_word(P,Word,Rest0,Rest,DecapWord,Length) :-
    only_capitals(Word,DecapWord),
    atom_length(Word,Len), Len > 1,  % not for "U"
    only_capitals_prefix(Rest0,Rest,P,Q,0,4),
    Length is 1+Q-P.

%% if it is written in full capitals, and the word is unknown
%% remove all but the first capital
%% AMSTERDAM --> Amsterdam
%% SAO PAULO --> Sao Paulo
%% but not VARA --> Vara (is another name)
special_capitalized_word(P0,Word,Rest0,Rest,DecapWord,Length) :-
    only_capitals_but_one(Word,DecapWord),
    P1 is P0 + 1,
    only_capitals_but_one_prefix(Rest0,Rest,P1,P,0,4),
    \+ tag(P0,P,_,_,_,_,normal(_),_),
    Length is P-P0.

tag(A,B,C,D,E,F,G,H) :-
    alpino_lexical_analysis:tag(A,B,C,D,E,F,G,H).

search_tag_tag(A,B) :-
    alpino_lexical_analysis:search_tag_tag(A,B).

%search_tag_r0(A,B) :-
%    alpino_lexical_analysis:search_tag_r0(A,B).

search_tag_stem(A,B) :-
    alpino_lexical_analysis:search_tag_stem(A,B).

assert_tag(P0,P,R0,R,Label,History,Tag) :-
    \+ alpino_lexical_analysis:tag(P0,P,R0,R,Label,_,_,Tag),
    alpino_lexical_analysis:assert_tag(P0,P,R0,R,Label,History,Tag).

assert_tag(tag(P0,P,R0,R,Label,History,Tag)) :-
    \+ alpino_lexical_analysis:tag(P0,P,R0,R,Label,_,_,Tag),
    alpino_lexical_analysis:assert_tag(P0,P,R0,R,Label,History,Tag).

guess_unknown_words(Words,Ps) :-
    alpino_lexical_analysis:remove_brackets(Words,Words1),
    retractall(guessed_compound(_,_)),
    retractall(guessed_similar(_,_)),
    guess_unused(Words,Words1,0,0,Ps).

%% guess_slash_pairs/0
guess_slash_pairs :-
    findall(t(R0,R,Slash),a_slash_tag(R0,R,Slash),Rs0),
    sort(Rs0,Rs),
    (   member(t(R1,R2,Slash),Rs),
	tag(P0,_P1,R0,R1,Label1,Used1,His1,TagL),
        tag(_P2,P, R2,R, Label2,Used2,His2,TagR),
        similar_tags(Slash,TagL,TagR,Tag),
	P-P0 < 8,
	\+ tag(P0,P,R0,R,_,_,normal(_),_),
	alpino_lex:concat_stems([Label1,Slash,Label2],Label,' '),
	concat_all([Used1,Slash,Used2],Used,' '),
	debug_message(1,"guessing|slash|~p|~p~n",[Used,Tag]),
	assert_tag(P0,P,R0,R,Label,slash(His1,His2),Tag),
	fail
    ;   true
    ).

similar_tags(_,punct(_),_,_) :- !, fail.
similar_tags(_,Tag,Tag,Tag) :- !.
similar_tags(_,noun(_,_,Nm), noun(_,_,Nm),noun(both,both,Nm)).
                                % groente & fruit
                                % marketing & communicatie
similar_tags(_,proper_name(_,ORG),proper_name(_,ORG),proper_name(both,ORG)).
similar_tags(_,proper_name(_),proper_name(_),proper_name(both)).

%% inw. / km²
similar_tags('/',noun(A,B,C),meas_mod_noun(_,_,_),noun(A,B,C)).

a_slash_tag(R0,R,Surf) :-
    slash_tag(R0,R,Surf),
    \+ wikipedia_list(Surf).

slash_tag(R0,R,'/') :-
    search_tag_stem('/',tag(_,_,R0,R,_,'/',_,_)).
slash_tag(R0,R,'&') :-
    search_tag_stem('&',tag(_,_,R0,R,_,'&',_,_)).
slash_tag(R0,R,'+') :-
    search_tag_stem('+',tag(_,_,R0,R,_,'+',_,_)).
slash_tag(R0,R,'x') :-
    search_tag_stem('x',tag(_,_,R0,R,_,'x',_,_)).
slash_tag(R0,R,'-') :-
    search_tag_stem('-',tag(_,_,R0,R,_,'-',_,_)).

wikipedia_list :-
    wikipedia_list_sep(Sep,N),
    wikipedia_list(Sep,N).

wikipedia_list(Sep) :-
    wikipedia_list_sep(Sep,N),
    wikipedia_list(Sep,N).

wikipedia_list(Sep,N) :-
    findall(_,search_tag_stem(Sep,tag(_,_,_,_,_,Sep,_,_)),List),
    length(List,Len),
    Len >= N.

wikipedia_list_sep('-',4).
wikipedia_list_sep('|',4).
wikipedia_list_sep('--',4).
wikipedia_list_sep('__',4).
wikipedia_list_sep('*',4).
wikipedia_list_sep(';',10).
wikipedia_list_sep(',',15).
wikipedia_list_sep('/',20).

retract_wikipedia_list :-
    (   wikipedia_list_sep(Sep,N),
        wikipedia_list(Sep,N),
        retractall(alpino_lexical_analysis:tag(_,_,_,_,_,Sep,_,_)),
        fail
    ;   true
    ).

guess_eng_compounds(Words) :-
    guess_eng_compounds(Words,0,0).

guess_eng_compounds([],_,_).
guess_eng_compounds([H|T],P0,R0) :-
    skip_bracket(H,T,T1,P0,P1,R0,R1),
    guess_eng_compounds_next(T1,P1,R1).

guess_eng_compounds_next([],_,_).
guess_eng_compounds_next([Word|T1],P0,R0):-
    P1 is P0 + 1, P2 is P1 + 1,
    R1 is R0 + 1, R2 is R1 + 1,
    %% no brackets between P1 and P2!
    guess_eng_compounds_position(T1,Word,P0,P2,R0,R2),
    guess_eng_compounds(T1,P1,R1).

guess_eng_compounds_position([],_,_,_,_,_).
guess_eng_compounds_position([Word2|_],Word,P0,P1,R0,R1) :-
    findall(Tag,a_eng_compound(Word,Word2,P0,P1,R0,R1,Tag),Tags0),
    sort(Tags0,Tags),
    (   member(Tag1,Tags),
	assert_tag(Tag1),
	fail
    ;   true
    ).

a_eng_compound(Wouw,Effect,P0,P,R0,R,tag(P0,P,R0,R,Label,wouw_compound(normal),Tag)) :-
    \+ tag(P0,P,R0,R,_,_,_,_),
    atom(Wouw),atom(Effect),
    wouw(Wouw),
    alpino_lex:lexicon(Tag,EffectLabel,[Effect],[],normal),
    noun_tag_plus(Tag),
    atom(EffectLabel),
    hdrug_util:concat_all([Wouw,EffectLabel],Label,' '),
    debug_message(1,"wouw compound|~w ~w|~w~n",[Wouw,Effect,Tag]).

a_eng_compound(Word,Word2,P0,P,R0,R,tag(P0,P,R0,R,Label,english_compound(normal),Tag)) :-
    \+ tag(P0,P,R0,R,_,_,_,_),
    atom(Word),atom(Word2),
    atom_concat(Word,Word2,WordWord2),
    alpino_lex:lexicon(Tag,Label,[WordWord2],[],normal),
    atomic(Label), % only nouns anyway
    atom_concat(Word,UWord2,Label),
    atom_concat('_',NOTDIM,UWord2),
    NOTDIM \= 'DIM',
    noun_tag_plus(Tag),
    debug_message(1,"mis-spelled compound|~w ~w|~w~n",[Word,Word2,Tag]).

wouw(wauw).
wouw(wow).

%% guess_names/1
guess_names(Words) :-
    prepare_normal_analysis,
    guess_names(Words,Words,0,0).

guess_names([],_,_,_).
guess_names([H|T],Words,P0,R0) :-
    skip_bracket(H,T,T1,P0,P1,R0,R1),
    guess_names_next(T1,Words,P1,R1).

guess_names_next([],_,_,_).
guess_names_next([Word|T1],Words,P1,R1) :-
    guess_names_position(Word,T1,P1,R1,Words),
    P2 is P1+1,
    R2 is R1+1,
    guess_names(T1,Words,P2,R2).

guess_names_position(W,Ws,P1,R1,Words) :-
    findall(Len-f(Msg,MsgArgs,Tag),name_heuristic(P1,R1,W,Ws,Words,Len,Msg,MsgArgs,Tag),Tags0),
    longest_match(Tags0,Tags),
%    Tags0=Tags,
    (   lists:member(_-f(Msg,MsgArgs,Tag),Tags),
	debug_message(1,"guessing|",[]),
	debug_message(1,Msg,MsgArgs),
	assert_tag(Tag),
	fail
    ;	true
    ).


%% if there is a name of more than 10,
%% remove all with length >4 <longest
longest_match(Tags0,Tags) :-
    keysort(Tags0,Tags1),
    reverse(Tags1,Tags2),
    longest_match_(Tags2,Tags).

longest_match_([],[]).
longest_match_([Len-El|Els0],[Len-El|Els]) :-
    (    Len > 10
    ->   longest_match_(Els0,Len,Els)
    ;    Els0 = Els
    ).

longest_match_([],_,[]).
longest_match_([Len0-El|T],Len,Result) :-
    (   Len0 < 6
    ->  Result = [Len0-El|T]
    ;   longest_match_(T,Len,Result)
    ).

name_heuristic(P1,R1,W,Ws,Words,Len,Msg,MsgArgs,Tag) :-
    (   is_decap_only(W)
    ->  tag(P1,_,R1,_,W,_,normal(names_dictionary),_)
    ;   true
    ),
    lexical_analysis_name([W|Ws],P1,R1,Words,Len,Msg,MsgArgs,Tag).

lexical_analysis_name([W|Words],P0,R0,All,Len,Msg,MsgArgs,Tag) :-
    lexical_analysis_name_([W|Words],P0,R0,All,Len,Msg,MsgArgs,Tag,_Least).

lexical_analysis_name_(Words,P0,R0,All,Len,String,Args,Tag,Least) :-
    potential_name(Words,_,Names,P0,P,History),
    lexical_analysis_name__(Names,P0,P,History,R0,All,Len,String,Args,Tag,Least).

lexical_analysis_name__(Names,P0,P,History,R0,All,Len,"name|~p|~p~n",
			[Stem,NewTag],tag(P0,P,R0,R,Stem,name(Start),NewTag),Least) :-
    \+ subsumed_by_dict(P0,P,Names,proper_name(both)),
    (	nonvar(Least)
    ->	P > Least
    ;	true
    ),
    concat_all(Names,Stem,' '),
    add_features(History,Names,proper_name,NewTag,P0,P,All,_,Stem),
    start_of_sentence(P0,Start),
    Len is P - P0,
    R is R0 + Len.
    

lexical_analysis_name__(Names0,P0,P,History,R0,All,Len,"genitive_name|~p|~p~n",
		      [Stem,NewTag],tag(P0,P,R0,R,Stem,name_gen(Start),NewTag),Least) :-
    ends_with_genitive_marker(Names0,Names),
    \+ subsumed_by_dict(P0,P,Names0,name_determiner(pron)),
    (	nonvar(Least)
    ->	P > Least
    ;	true
    ),
    Len is P-P0,
    Len < 4,
    dif(TYPE,'MISC'),
    concat_all(Names,Stem,' '),
    add_features(History,Names,name_determiner(pron),NewTag,P0,P,All,TYPE,Stem),
    start_of_sentence(P0,Start),
    R is R0 + Len.

lexical_analysis_name__(Names,P0,P,_History,R0,_All,1,"adjective_name|~p|~p~n",
		      [Stem,adjective(E)],tag(P0,P,R0,R,Stem,name_adj(Start),adjective(E)),Least) :-
    P is P0 + 1,   % !
    ends_with_adjective_marker(Names,E),
    \+ subsumed_by_dict(P0,P,Names,adjective(E)),
    (	nonvar(Least)
    ->	P > Least
    ;	true
    ),
    concat_all(Names,Stem,' '),
    start_of_sentence(P0,Start),
    R is R0 + 1.

guess_unused([],_,_,_,_).
guess_unused([H|T],Words,P0,R0,Ps) :-
    skip_bracket(H,T,T1,P0,P1,R0,R1),
    guess_unused_next(T1,Words,P1,R1,Ps).

guess_unused_next([],_,_,_,_).
guess_unused_next([Word|T1],Words,P1,R1,Ps) :-
    (   Ps=[P1|Ps1]
    ->  time(1,guess_unused_position(Word,T1,P1,R1,Words))
    ;   Ps=Ps1
    ),
    P2 is P1+1,
    R2 is R1+1,
    guess_unused(T1,Words,P2,R2,Ps1).

guess_unused_position(W,Ws,P1,R1,Words) :-
    (	\+ alpino_lexical_analysis:hesitation(W),
	\+  (   tag(P1,_,_,_,_,_,name(_),_),
	        (  name_title(W)
		;  name_vanhet(W)
		)
	    ),
	(   name_unknown(W,P1),
	    \+ decap_foreign_word(W)
        ->   debug_message(1,"unknown|~p|~p~n",[W,P1])
        ;    true
        ),
	unknown_word_heuristic(P1,R1,W,Ws,Msg,MsgArgs,Words,_),
	debug_message(1,"guessing|",[]),
	debug_message(1,Msg,MsgArgs),
	fail
    ;	true
    ).

%% op- en terugbellen
unknown_word_heuristic(P0,R0,W,Ws,
		       "part_verb_conjunct|~p|~p~n",[W,Tags],_,len(1)) :-
    debug_message(3,"trying heuristic part_verb_conjunct~n",[]),
    atom(W),
    atom_concat(Part,'-',W),
    alpino_lex:lexicon(particle(_),_,[Part],[],_),
    Tags = [_|_],
    findall(Tag,
	    lexical_analysis_prefixed_verb(Ws,P0,R0,Part,
					   part_verb_conjunct,Tag),
	    Tags
	   ).

unknown_word_heuristic(P1,R1,W,_,"lonely-foreign|~p|~p~n",[W,Stem],_,none) :-
    debug_message(3,"trying heuristic lonely-foreign~n",[]),
    decap_foreign_word(W,Stem),
    !,
    start_of_sentence(P1,Start),
    P is P1 + 1, R is R1 + 1,
    \+ (   tag(Q0,Q,_,_,_,_,name(_),_),
           Q0 =< P1,
           P =< Q
       ),
    assert_tag(P1,P,R1,R,Stem,name(Start),proper_name(both)).

unknown_word_heuristic(P1,R1,W,_,"lonely-part-of-name|~p~n",[W],_,len(1)) :-
    debug_message(3,"trying heuristic lonely-part-of-name~n",[]),
    name_title0(W),
    !,
    P is P1 + 1, R is R1 + 1,
    \+ (   tag(Q0,Q,_,_,_,_,name(_),_),
           Q0 =< P1,
           P =< Q
       ),
    assert_tag(P1,P,R1,R,W,default,noun(both,both,both)).

unknown_word_heuristic(P1,R1,W,_,"cap-lonely-part-of-name|~p~n",[W],_,len(1)) :-
    debug_message(3,"trying heuristic cap-lonely-part-of-name~n",[]),
    name_title(W),
    !,
    start_of_sentence(P1,Start),
    P is P1 + 1, R is R1 + 1,
    \+ (   tag(Q0,Q,_,_,_,_,name(_),_),
           Q0 =< P1,
           P =< Q
       ),
    assert_tag(P1,P,R1,R,W,name(Start),proper_name(both)).

unknown_word_heuristic(P0,R0,W0,[W1|Ts],"additional_space|~p|~p|~p|~p~n",
		       [W0,W1,W,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic additional-space~n",[]),
    atom(W0),atom(W1),
    atom_concat(W0,W1,W),
    findall(Tag,lexical_analysis_add_space([W|Ts],P0,R0,add_space,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P0,R0,W0,[W1|Ts],"dashed_additional_space|~p|~p|~p|~p~n",
		       [W0,W1,W,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic dashed_additional_space~n",[]),
    atom(W0),atom(W1),
    \+ W1=en,    % op- en aanmerkingen
    atom_concat(Left,'-',W0),
    atom_concat(Left,W1,W),
    findall(Tag,lexical_analysis_add_space([W|Ts],P0,R0,add_space,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P0,R0,W0,[W1|Ts],"bracketed_additional_space|~p|~p|~p|~p~n",
		       [W0,W1,W,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic bracketed_additional_space~n",[]),
    atom(W0),atom(W1),
    atom_concat(Left,'-)',W0),
    atom_concat('(',Center,Left),
    atom_concat(Center,W1,W),
    findall(Tag,lexical_analysis_add_space([W|Ts],P0,R0,add_space,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P0,R0,W0,[W1|Ts],"bracketed_additional_space|~p|~p|~p|~p~n",
		       [W0,W1,W,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic bracketed_additional_space~n",[]),
    atom(W0),atom(W1),
    atom_concat(Left,')',W0),
    atom_concat('(',Center,Left),
    atom_concat(Center,W1,W),
    findall(Tag,lexical_analysis_add_space([W|Ts],P0,R0,add_space,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P0,R0,W,Ws,"within_word_conjunct|~p~n",
                       [W],_,len(1)) :-
    debug_message(3,"trying heuristic within_word_conjunct~n",[]),
    atom(W),
    atom_concat(_,'-',W),
    P is P0 + 1,
    R is R0 + 1,

    (   search_tag_tag(conj(_),tag(Q0,_,_,_,_,_,_,_)),
	Q0 > P0
    ;   search_tag_tag(right_conj(_),tag(Q0,_,_,_,_,_,_,_)),
	Q0 > P0
    ),
    
    \+ end_sentence(Ws),
    !,
    assert_tag(P0,P,R0,R,W,within_word_conjunct,within_word_conjunct).

unknown_word_heuristic(P1,R1,W,Ws,"cap|~p|~p|~p~n",[W,Wmin,[Th|Tt]],_,none) :-
    debug_message(3,"trying heuristic cap~n",[]),
    cap_first(W,Wmin),
    findall(Tag,
	alternative([Wmin|Ws],P1,_,R1,_,cap,Tag,_),
	[Th|Tt]).

unknown_word_heuristic(P1,R1,W,Ws,"cap|~p|~p|~p~n",[W,Wmin,[Th|Tt]],_,none) :-
    debug_message(3,"trying heuristic cap_all~n",[]),
    cap_all(W,Wmin),
    findall(Tag,
	alternative([Wmin|Ws],P1,_,R1,_,cap_all,Tag,_),
	[Th|Tt]).

%% DONE? spurious analyses for different capitalized prefixes where
%% such prefixes are not used at all
%% DONE? this is extremely slow for long capitalized sequences too
unknown_word_heuristic(P1,R1,W,Ws0,Mess,Args,Rest,None) :-
    only_capitals(W,W1),
    !,
    (   \+ tag(P1,_,_,_,_,_,special(decap(_)),_),
	only_capitals_prefix(Ws0,Ws,P1,P,0,4),
	Len is 1+P-P1,
	None = none,
	unknown_word_heuristic(P1,R1,W1,Ws,Mess,Args,Rest,len(Len))
    ;		   % this is a copy of the heuristic prefix_name below
	           % for cases such as ESA-ESTEC
	Mess = "prefix_name|~p|~p|~p~n",
	Args = [W,Wmin,[Th|Tt]],
	None = len(1),
	    debug_message(3,"trying heuristic prefix_name~n",[]),
	\+ tag(P1,_,_,_,_,_,decap(_),_),
	\+ tag(P1,_,_,_,_,_,special(decap(_)),_),
	findall(Wfirst-Wmin,guess_prefix_compound(W,Wfirst,Wmin),Wmins0),
	sort(Wmins0,Wmins),
	member(Wfirst-Wmin,Wmins),
	findall(Tag,
		alternative_proper_name(Wfirst,Wmin,P1,R1,prefix_name,Tag),
		[Th|Tt])
    ).

unknown_word_heuristic(P1,R1,W,Ws,"strip_diacritics|~p|~p|~p~n",
		       [W,Wmin,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic strip_diacritics~n",[]),
    strip_accents(W,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,w_dia,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P1,R1,W,Ws,"strip_repeated_letters|~p|~p|~p~n",
		       [W,Wmin,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic strip_repeated_letters~n",[]),
    strip_repeated_letters(W,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,strip_repeated_letters,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P1,R1,W,Ws,"y_ij|~p|~p|~p~n",
		       [W,Wmin,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic y_ij~n",[]),
    y_ij_letters(W,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,y_ij_letters,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P1,R1,W,Ws,"ij_ei|~p|~p|~p~n",
		       [W,Wmin,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic ij_ei~n",[]),
    ij_ei_letters(W,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,ij_ei_letters,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P1,R1,W,Ws,"ei_ij|~p|~p|~p~n",
		       [W,Wmin,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic ei_ij~n",[]),
    ei_ij_letters(W,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,ei_ij_letters,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P1,R1,W,Ws,"g_ch|~p|~p|~p~n",
		       [W,Wmin,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic g_ch~n",[]),
    g_ch_letters(W,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,g_ch_letters,Tag,HIS),
	    [Th|Tt]).



unknown_word_heuristic(P1,R1,W,Ws,"skip_bracket|~p|~p|~p~n",
		       [W,Wmin,[Th|Tt]],_,HIS ):-
    debug_message(3,"trying heuristic skip_bracket~n",[]),
    sub_atom(W,_,1,_,')'),
    atom_concat(Prefix,Rest,W),
    atom_concat(')',Suffix,Rest),
    atom_concat(Prefix,Suffix,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,skip_bracket,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P1,R1,W,Ws,"skip_bracket|~p|~p|~p~n",
		       [W,Wmin,[Th|Tt]],_,HIS ):-
    debug_message(3,"trying heuristic skip_bracket~n",[]),
    sub_atom(W,_,1,_,'('),
    atom_concat(Prefix,Rest,W),
    atom_concat('(',Suffix,Rest),
    atom_concat(Prefix,Suffix,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,skip_bracket,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P1,R1,W,Ws,"skip_upto_bracket|~p|~p|~p~n",
		       [W,Suffix,[Th|Tt]],_,HIS ):-
    debug_message(3,"trying heuristic skip_upto_bracket~n",[]),
    sub_atom(W,_,1,_,')'),
    atom_concat(_Prefix,Rest,W),
    atom_concat(')',Suffix,Rest),
    findall(Tag,
	    alternative([Suffix|Ws],P1,_,R1,_,skip_upto_bracket,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P1,R1,W,Ws,"skip_upto_bracket|~p|~p|~p~n",
		       [W,Prefix,[Th|Tt]],_,HIS ):-
    debug_message(3,"trying heuristic skip_upto_bracket~n",[]),
    sub_atom(W,_,1,_,'('),
    atom_concat(Prefix,Rest,W),
    atom_concat('(',_Suffix,Rest),
    findall(Tag,
	    alternative([Prefix|Ws],P1,_,R1,_,skip_upto_bracket,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P1,R1,W,Ws,"iseer|~p|~p|~p~n",
		       [W,Wmin,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic iseer~n",[]),
    iseer_letters(W,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,iseer_letters,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P1,R1,W,Ws,"add_diacritics|~p|~p|~p~n",
		       [W,Wmin,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic add_diacritics~n",[]),
    add_accents(W,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,wo_dia,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P1,R1,W,Ws,"wrong_diacritics|~p|~p|~p~n",
		       [W,Wmin,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic wrong_diacritics~n",[]),
    strip_accents(W,Wtussen),
    add_accents(Wtussen,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,wr_dia,Tag,HIS),
	    [Th|Tt]).

unknown_word_heuristic(P1,R1,W,_,"diminutive|~p|~p|~p~n",
		       [W,Stam,Tag],_,len(1)) :-
    debug_message(3,"trying heuristic diminutive~n",[]),
    diminutive(W,Stam0,Tag),
    atom_concat(Stam0,'_DIM',Stam),
    lexical_analysis_tag(Tag,P1,R1,Stam,diminutive).

unknown_word_heuristic(P1,R1,W,_,"verb-ster|~p|~p|~p~n",
		       [W,Stam,Tag],_,len(1)) :-
    debug_message(3,"trying heuristic diminutive~n",[]),
    verb_ster(W,Stam,Tag),
    lexical_analysis_tag(Tag,P1,R1,Stam,verb_ster).

unknown_word_heuristic(P1,R1,W,Ws,Msg,Args,Rest,none) :-
    decap_some(W,Wmin),
    decap_some_word_heuristic(Wmin,P1,R1,W,Ws,Msg,Args,Rest).

decap_some_word_heuristic(Wmin,P1,R1,W,Ws,
                          "europarl|~p|~p|~p~n",
                          [W,Wmin,[Th|Tt]],_) :-
    europarl(W),  % only the noun reading of the words are often capitalized
    debug_message(3,"trying heuristic europarl~n",[]),
    findall(Tag,
	    (   noun_tag(Tag),
		alternative([Wmin|Ws],P1,_,R1,_,europarl,Tag,_)
	    ),
	[Th|Tt]).

decap_some_word_heuristic(Wmin,P1,R1,W,Ws,
                          "decap_not_begin|~p|~p|~p~n",
                          [W,Wmin,[Th|Tt]],_) :-
    debug_message(3,"trying heuristic decap_not_begin~n",[]),
    \+ tag(P1,_,_,_,_,_,special(decap(_)),_),
    \+ tag(P1,_,_,_,_,_,europarl,_),
    findall(Tag,
	alternative([Wmin|Ws],P1,_,R1,_,decap(not_begin),Tag,_),
	[Th|Tt]).

decap_some_word_heuristic(W,P1,R1,W0,Ws,
                          "decap_and_strip_diacritics|~p|~p|~p~n",
                          [W0,Wmin,[Th|Tt]],_) :-
    debug_message(3,"trying heuristic decap_and_strip_diacritics~n",[]),
    \+ tag(P1,_,_,_,_,_,special(decap(_)),_),
    strip_accents(W,Wmin),
    \+ tag(P1,_P,R1,_R,Wmin,W0,w_dia,_),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,decap_w_dia,Tag,_),
	    [Th|Tt]).

decap_some_word_heuristic(W,P1,R1,W0,Ws,"decap_and_add_diacritics|~p|~p|~p~n",
		       [W0,Wmin,[Th|Tt]],_) :-
    debug_message(3,"trying heuristic decap_and_add_diacritics~n",[]),
    \+ tag(P1,_,_,_,_,_,special(decap(_)),_),
    add_accents(W,Wmin),
    \+ tag(P1,_P,R1,_R,Wmin,W0,wo_dia,_),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,decap_wo_dia,Tag,_),
	    [Th|Tt]).

decap_some_word_heuristic(W,P1,R1,CapW,_,"decap_and_diminutive|~p|~p|~p~n",
		       [CapW,Stam,Tag],_) :-
    debug_message(3,"trying heuristic decap_and_diminutive~n",[]),
    \+ tag(P1,_,_,_,_,_,special(decap(_)),_),
    \+ tag(P1,_,_,_,_,_,diminutive,_),
    diminutive(W,Stam0,Tag),
    atom_concat(Stam0,'_DIM',Stam),
    lexical_analysis_tag(Tag,P1,R1,Stam,decap_and_diminutive).

decap_some_word_heuristic(W,P1,R1,CapW,Ws,"decap(strip_repeated_letters)|~p|~p|~p~n",
		       [CapW,Wmin,[Th|Tt]],_) :-
    debug_message(3,"trying heuristic decap(strip_repeated_letters)~n",[]),
    strip_repeated_letters(W,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,decap(strip_repeated_letters),Tag,_),
	    [Th|Tt]).

decap_some_word_heuristic(W,P1,R1,CapW,Ws,"decap(y_ij)|~p|~p|~p~n",
		       [CapW,Wmin,[Th|Tt]],_) :-
    debug_message(3,"trying heuristic decap(y_ij)~n",[]),
    y_ij_letters(W,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,decap(y_ij_letters),Tag,_),
	    [Th|Tt]).

decap_some_word_heuristic(W,P1,R1,CapW,Ws,"decap(g_ch)|~p|~p|~p~n",
		       [CapW,Wmin,[Th|Tt]],_) :-
    debug_message(3,"trying heuristic decap(g_ch)~n",[]),
    g_ch_letters(W,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,decap(g_ch_letters),Tag,_),
	    [Th|Tt]).

decap_some_word_heuristic(W,P1,R1,CapW,Ws,"decap(iseer)|~p|~p|~p~n",
		       [CapW,Wmin,[Th|Tt]],_) :-
    debug_message(3,"trying heuristic decap(iseer)~n",[]),
    iseer_letters(W,Wmin),
    findall(Tag,
	    alternative([Wmin|Ws],P1,_,R1,_,decap(iseer_letters),Tag,_),
	    [Th|Tt]).

decap_some_word_heuristic(W,P1,R1,CapW,_Ws,"decap_and_compound(~p)|~p|~p|~p~n",
		       [Len,CapW,Wmin,[Th|Tt]],_) :-
    debug_message(3,"trying heuristic decap_and_compound()~n",[]),
    \+ tag(P1,_,_,_,_,_,special(decap(_)),_),
    is_start_sentence(P1),
    findall(Len-Wmin-Parts,guess_proper_compound(W,Wmin,Parts,Len),Wmins0),
    sort(Wmins0,Wmins1),
    remove_longer_compounds_with_same_suffix(Wmins1,Wmins),
    member(Len-Wmin-Parts,Wmins),
    findall(Tag,
	    alternative_open_class_decap_compound(Wmin,Parts,P1,R1,
		decap_and_compound(Len),Tag),
	    [Th|Tt]).

unknown_word_heuristic(P0,R0,W,_Ws,"verbal compound|~p|~p|~p",
		       [W,Wmin,Stems],_,len(1)) :-
    debug_message(3,"trying heuristic verbal compound~n",[]),
    verbal_compound_prefix(Prefix),
    atom(W),
    atom_concat(Prefix,Wmin,W),
    P is P0+1, R is R0+1,
    findall(Wmin,(alpino_lex:lexicon(verb(Hz,Infl,Sc),Stem0,[Wmin],[],His),
		  alpino_lex:concat_stems([Prefix,Stem0],Stem),
		  assert_tag(P0,P,R0,R,Stem,verbal_compound(His),verb(Hz,Infl,Sc))
		 ),[H0|T0]),
    sort([H0|T0],Stems).

unknown_word_heuristic(P0,R0,W,_Ws,"verbal compound|~p|~p|~p",
		       [W,Wmin,Stems],_,len(1)) :-
    debug_message(3,"trying heuristic verbal compound~n",[]),
    atom(W),
    verbal_compound_prefix(Prefix,_,_,_),
    atom_concat(Prefix,Wmin,W),
    P is P0+1, R is R0+1,
    findall(Wmin,(alpino_lex:lexicon(verb(hebben,Infl,Sc0),Stem0,[Wmin],[],His),
		  verbal_compound_prefix(Prefix,Prefix1,Sc0,Sc),
		  alpino_lex:concat_stems([Prefix1,Stem0],Stem),
		  assert_tag(P0,P,R0,R,Stem,verbal_compound(His),verb(hebben,Infl,Sc))
		 ),[H0|T0]),
    sort([H0|T0],Stems).

unknown_word_heuristic(P1,R1,W,_Ws,"dashed_mwu|~p|~p~n",
		       [W,Tag],_,len(1)) :-
    debug_message(3,"trying heuristic dashed_mwu~n",[]),
    \+ tag(P1,_,_,_,_,_,decap(_),_),
    \+ tag(P1,_,_,_,_,_,special(decap(_)),_),
    atom_codes(W,Codes),
    split_string(Codes,"-",[C1,C2|CTail]),
    codes_words_to_words([C1,C2|CTail],[W1,W2|Tail]),
    alpino_lex:xl(W1,Tag0,_,[W2|Tail],[]),
    (   Tag0 = with_dt(Tag1,_)
    ->  (   Tag1 == complex_etc
	->  Tag = etc
	;   Tag = Tag1
	)
    ;   Tag0 = Tag
    ),
    P is P1 + 1,
    R is R1 + 1,
    alpino_lex:concat_stems([W1,W2|Tail],Stem,' '),
    assert_tag(P1,P,R1,R,Stem,dashed_mwu,Tag).

codes_words_to_words([],[]).
codes_words_to_words([H|T],[W0|W]) :-
    atom_codes(W0,H),
    codes_words_to_words(T,W).

unknown_word_heuristic(P1,R1,W,_Ws,"spelling_variant_compound|~p|~p~n",
		       [W,[Th|Tt]],_,_) :-
    debug_message(3,"trying heuristic spelling_variant_compound~n",[]),
    findall(Orig/Tag,
	    spelling_variant_compound(W,Orig,P1,R1,Tag),
	    [Th|Tt]).

unknown_word_heuristic(P1,R1,W,_Ws,"compound(~p)|~p|~p|~p~n",
		       [Len,W,Wmin,[Th|Tt]],_,len(1)) :-
    debug_message(3,"trying heuristic compound()~n",[]),
    \+ tag(P1,_,_,_,_,_,decap(_),_),
    \+ tag(P1,_,_,_,_,_,special(decap(_)),_),
    \+ tag(P1,_,_,_,_,_,verb_ster,_),
    guess_compound(W,Wmins),
    member(Len-Wmin-Parts,Wmins),
    findall(Tag,
	    alternative_open_class(Len,Wmin,Parts,P1,R1,compound(Len),Tag),
	    [Th|Tt]).

%% aanmeld
%% is filtered out as a verb if no "ik" in the sentence
%% assume it is misspelled "aanmeldt"

unknown_word_heuristic(P1,R1,W,_Ws,"dt|~p|~p~n",[W,Tags],_,_) :-
    debug_message(3,"trying heuristic dt~n",[]),
    atom(W),
    typical_spelling_mistake_suffixes(T,D),
    atom_concat(Pref,T,W),
    atom_concat(Pref,D,Alt),
    \+ \+ alpino_lex:lexicon___(Alt,_Tag,_,[],[],_),
    P is P1 +1,
    R is R1 +1,
    !,
    findall(Tag1,
	    (  alpino_lex:lexicon___(Alt,Tag1,Stem1,[],[],_),
	       assert_tag(P1,P,R1,R,Stem1,dt,Tag1)
	    ),
	    Tags
	   ).


unknown_word_heuristic(P1,R1,W,_Ws,"dt_psp|~p|~p~n",[W,[TagsH|TagsT]],_,_) :-
    P is P1 + 1,
    R is R1 + 1,
    findall(Tag,(  psp_variant(W,Tag,Stem),
		   assert_tag(P1,P,R1,R,Stem,dt_psp,Tag)
		), [TagsH|TagsT]
	   ).

unknown_word_heuristic(P1,R1,W,_Ws,"ninv_dt|~p|~p~n",[W,Tags],_,_) :-
    debug_message(3,"trying heuristic ninv_dt~n",[]),
    alpino_lex:lexicon___(W,verb(_,sg1,ninv(_,_)),_,[],[],'part-V'),
    P is P1 +1,
    R is R1 +1,
    !,
    findall(verb(H1,sg3,ninv(Sc1,Sc2)), 
	    (  alpino_lex:lexicon___(W,verb(H1,sg1,ninv(Sc1,Sc2)),Stem1,[],[],'part-V'),
	       assert_tag(P1,P,R1,R,Stem1,ninv_dt,verb(H1,sg3,ninv(Sc1,Sc2)))
	    ),
	    Tags
	   ).

unknown_word_heuristic(P1,R1,W,_,"prefix_name|~p|~p|~p~n",
		       [W,Wmin,[Th|Tt]],_,len(1)) :-
    debug_message(3,"trying heuristic prefix_name~n",[]),
    \+ tag(P1,_,_,_,_,_,decap(_),_),
    \+ tag(P1,_,_,_,_,_,special(decap(_)),_),
    findall(Wfirst-Wmin,guess_prefix_compound(W,Wfirst,Wmin),Wmins0),
    sort(Wmins0,Wmins),
    member(Wfirst-Wmin,Wmins),
    findall(Tag,
	    alternative_proper_name(Wfirst,Wmin,P1,R1,prefix_name,Tag),
	    [Th|Tt]).

unknown_word_heuristic(P1,R1,W,_Ws,"suffix_name|~p|~p|~p~n",
		       [W,Wmin,[proper_name(both,'LOC')]],_,len(1)) :-
    debug_message(3,"trying heuristic suffix_name~n",[]),
    \+ tag(P1,_,_,_,_,_,decap(_),_),
    \+ tag(P1,_,_,_,_,_,special(decap(_)),_),
    findall(Wfirst-Wmin,guess_suffix_compound(W,Wfirst,Wmin),Wmins0),
    sort(Wmins0,Wmins),
    member(Wfirst-Wmin,Wmins),
    P is P1 +1,
    R is R1 +1,
    alpino_lex:concat_stems([Wfirst,Wmin],Stem),
    assert_tag(P1,P,R1,R,Stem,suffix_name,proper_name(both,'LOC')).

%% motie-Goudzwaard
unknown_word_heuristic(P1,R1,W,Ws,"left_headed_compound|~p|~p|~p~n",
		       [Surf,Wmin,[Th|Tt]],_,len(SurfLength)) :-
    debug_message(3,"trying heuristic left_headed_compound~n",[]),
    guess_left_headed_compound(W,Ws,Wmin,Stem,Surf,SurfLength),
    findall(Tag,
	lexical_analysis_noun(Wmin,Stem,P1,R1,SurfLength,
			      left_headed_compound,Tag),
	    [Th|Tt]).

%% science fiction-schrijver
unknown_word_heuristic(P1,R1,W,Ws,"double_compound|~p|~p|~p~n",
		       [Surf0,W,[Th|Tt]],Words,none) :-
    debug_message(3,"trying heuristic double_compound~n",[]),
    atom(W),
    once(atom_split(W,'-',Fiction,Schrijver)),
    
    append(Prefix,[W|Ws],Words),
    append(_,[X0|Xn],Prefix),
    length([X0|Xn],XL), XL < 5,
%    \+ decap_only(X0),   % Ben & Jerry's-ijs
    append([X0|Xn],[Fiction],SF),
    append([X0|Xn],[W],Surf0),
    length(SF,L),
    P0 is P1-L+1,
    R0 is R1-L+1,
    findall(Tag,lexical_analysis_double(Schrijver,SF,Surf0,_Surf,P0,R0,Tag),
	    [Th|Tt]).

%% science fiction-schrijver
lexical_analysis_double(Input,SF,Surf0,Surf,P0,R0,Tag):-
    length(SF,LEN),
    alpino_lex:lexicon(WITH,Stem0,SF,[],_),
    \+ WITH = with_dt(_,_), % otherwise order of stems gets mixed up
    concat_all(Surf0,Surf,' '),
    alpino_lex:lexicon(Tag0,Stem1,[Input],[],_),
    open_class_tag(Tag0,Tag),
    P is P0+LEN,
    R is R0+LEN,
    alpino_lex:concat_stems([Stem0,Stem1],Stem),
    assert_tag(P0,P,R0,R,Stem,double_compound,Tag).

%% Harry Janos-suite
lexical_analysis_double(Input,SF,Surf0,Surf,P0,R0,Tag):-
    length(SF,LEN),
    potential_name(SF,[],Stem00,P0,P,_),
    P is P0+LEN,
    R is R0+LEN,
    concat_all(Surf0,Surf,' '),
    alpino_lex:lexicon(Tag0,Stem1,[Input],[],_),
    open_class_tag(Tag0,Tag),
    concat_all(Stem00,Stem0,' '),
    alpino_lex:concat_stems([Stem0,Stem1],Stem),
    assert_tag(P0,P,R0,R,Stem,double_compound,Tag).

%% nummer 1-hit; artikel 12-status
lexical_analysis_double(Input,[Nummer,Een],Surf0,Surf,P0,R0,Tag) :-
    alpino_lex:lexicon(noun(_,_,_),Stem0a,[Nummer],[],_),
    alpino_lex:lexicon(number(hoofd(_)),Stem0b,[Een],[],_),
    concat_all(Surf0,Surf,' '),
    alpino_lex:lexicon(Tag0,Stem1,[Input],[],_),
    open_class_tag(Tag0,Tag),
    P is P0+2,
    R is R0+2,
    alpino_lex:concat_stems([Stem0a,Stem0b,Stem1],Stem),
    assert_tag(P0,P,R0,R,Stem,double_compound,Tag).

%% oer-rock & roll
unknown_word_heuristic(P1,R1,W,[WsH|WsT],"compound_double|~p|~p ...|~p~n",
		       [W,Rock,[Th|Tt]],_Words,HIS) :-
    debug_message(3,"trying heuristic compound_double~n",[]),
    atom(W),
    once(atom_split(W,'-',Oer,Rock)),
    atom_length(Oer,Length), Length < 15,
    atom_length(Rock,Length2), Length2 < 30,
    findall(Tag,
	    alternative_prefix([Rock,WsH|WsT],Oer,P1,R1,compound_double,Tag,HIS),
	    [Th|Tt]).

alternative_prefix(Input,Stem0,P0,R0,Name,Tag,len(Length)):-
    alpino_lex:lexicon(Tag,Stem1,Input,Input1,_),
    (   (  Tag = proper_name(_) ; Tag = proper_name(_,_) )
    ->  starts_with_capital(Stem0)
    ),
    append([_Stem0|UsedInput],Input1,Input),
    length([_|UsedInput],Length),
    Length > 1,  % otherwise normal compound
    P is P0+Length,
    R is R0+Length,
    alpino_lex:concat_stems([Stem0,Stem1],Stem),
    assert_tag(P0,P,R0,R,Stem,Name,Tag).

unknown_word_heuristic(P1,R1,W,_,"plural_nominalization|~p|~p|~p~n",
		       [W,A,Tag],_,len(1)) :-
    debug_message(3,"trying heuristic nominalized_adjective~n",[]),
    atom(W),
    atom_concat(A,n,W),
    alpino_lex:lexicon(adjective(E),Astem,[A],[],_),
    (   E = e,    Tag = nominalized_adjective
    ;   E = ere,  Tag = nominalized_compar_adjective
    ;   E = ste,  Tag = nominalized_super_adjective
    ),
    lexical_analysis_tag(Tag,P1,R1,Astem,nom_adj).
 
unknown_word_heuristic(P1,R1,W,_,"comparative|~p|~p|~p~n",
		       [W,A,adjective(er(ADV))],_,len(1)) :-
    debug_message(3,"trying heuristic comparative~n",[]),
    atom(W),
    atom_concat(A,er,W), 
    (  alpino_lex:lexicon(adjective(no_e(ADV)),Astem,[A],[],_)
    ;  alpino_lex:lexicon(adjective(end(ADV)),Astem,[A],[],_)
    ;  alpino_lex:lexicon(adjective(ge_no_e(ADV)),Astem,[A],[],_)
    ),
    lexical_analysis_tag(adjective(er(ADV)),P1,R1,Astem,compar_adj).
 
unknown_word_heuristic(P1,R1,W,_,"comparative|~p|~p|~p~n",
		       [W,A,adjective(ere)],_,len(1)) :-
    debug_message(3,"trying heuristic comparative+e~n",[]),
    atom(W),
    atom_concat(A,ere,W), 
    (  alpino_lex:lexicon(adjective(no_e(_)),Astem,[A],[],_)
    ;  alpino_lex:lexicon(adjective(end(_)),Astem,[A],[],_)
    ;  alpino_lex:lexicon(adjective(ge_no_e(_)),Astem,[A],[],_)
    ),
    lexical_analysis_tag(adjective(ere),P1,R1,Astem,compar_adj).
 
%%% memo'ed for long repeated sequences
:- dynamic guessed_similar/2.
:- thread_local guessed_similar/2.
similar_endings(P1,R1,W,Wmin,Tags) :-
    (   guessed_similar(W,List)
    ->  true
    ;   findall(f(Wmin,Tags),similar_endingsXX(W,Wmin,Tags),List),
	noclp_assertz(guessed_similar(W,List))
    ),
    lists:member(f(Wmin,Tags),List),
    assert_tags(Tags,P1,R1,W,Wmin).

similar_endingsXX(W,Wmin,[TagH|TagT]) :-
    guess_using_suffix0(W,Prefix,Wmin,3,5),
    findall(Stem/Tag,alternative_open_class_suffix(Wmin,Prefix,Stem,Tag),[TagH|TagT]),
    !.  % don't try shorter suffixes
    
unknown_word_heuristic(P1,R1,W,_,"anti-W|~p|adjective(both(nonadv))~n",
		       [W],_,len(1)) :-
    debug_message(3,"trying heuristic anti-W~n",[]),
    atom(W),
    atom_concat('anti-',_,W),
    lexical_analysis_tag(adjective(both(nonadv)),P1,R1,W,anti).

unknown_word_heuristic(P1,R1,W,_,"num_let|~p|number(hoofd(both))~n",
		       [W],_,len(1)) :-
    debug_message(3,"trying heuristic num_let~n",[]),
    atom(W),
    member(Let,[a,b,c,d,e,f,g]),
    atom_concat(Num,Let,W),
    atom_codes(Num,NumCodes),
    alpino_lex:number_codes_silent(_,NumCodes),
    lexical_analysis_tag(number(hoofd(both)),P1,R1,W,num_let).

unknown_word_heuristic(P1,R1,W,_Ws,"suffix|~p|~p|~p~n",[W,Suffix,Tag],_,len(1)) :-
    debug_message(3,"trying heuristic suffix~n",[]),
    \+ tag(P1,_,_,_,_,_,decap(_),_),
    \+ tag(P1,_,_,_,_,_,special(decap(_)),_),
    \+ tag(P1,_,_,_,_,_,compound(hyphen),_),
    \+ tag(P1,_,_,_,_,_,prefix_name,_),
    \+ tag(P1,_,_,_,_,_,compar_adj,_),
    guess_form_of_suffix(W,Root,Suffix,Tag,CompTag),
    \+ tag(P1,_,_,_,_,_,_,CompTag),
    findall(_,lexical_analysis_tag(Tag,P1,R1,Root,form_of_suffix(Suffix)),[_|_]).

unknown_word_heuristic(P1,R1,W,_Ws,"similar_ending|~p|~p|~p~n",
		       [W,Wmin,Tags],_,len(1)) :-
    debug_message(3,"trying heuristic similar_ending~n",[]),
    \+ tag(P1,_,_,_,_,_,decap(_),_),
    \+ tag(P1,_,_,_,_,_,compound(_),_),
    \+ tag(P1,_,_,_,_,_,diminutive,_),
    \+ tag(P1,_,_,_,_,_,spelling_variant_compound(_),_),
    \+ tag(P1,_,_,_,_,_,prefix_name,_),
    \+ tag(P1,_,_,_,_,_,decap_and_compound(_),_),
    \+ tag(P1,_,_,_,_,_,form_of_suffix(_),_),
    \+ tag(P1,_,_,_,_,_,compar_adj,_),
    similar_endings(P1,R1,W,Wmin,Tags).

unknown_word_heuristic(P0,R0,W0,_,"subjunctive|~p|~p~n",[W,Stems],_,len(1)) :-
    debug_message(3,"trying heuristic subjunctive~n",[]),
    decap(W0,W),
    atom(W),
    \+ tag(P0,_,_,_,_,W,decap(not_begin),_),
    atom_concat(_,e,W),
    atom_concat(W,n,Inf),
    P is P0+1, R is R0+1,
    findall(Stem,(alpino_lex:lexicon(verb(Hz,inf,Sc),Stem,[Inf],[],_),
		  \+ alpino_lex:impossible_subcat_infl_combination(verb(Hz,subjunctive,Sc)),
		  assert_tag(P0,P,R0,R,Stem,subjunctive,
			     verb(Hz,subjunctive,Sc))
		 ),[H0|T0]
	   ),
    sort([H0|T0],Stems).

unknown_word_heuristic(P0,R0,W0,_,"drop_n|~p|~p~n",[W,Stems],_,len(1)) :-
    debug_message(3,"trying heuristic drop_n~n",[]),
    decap(W0,W),
    atom(W),
    \+ tag(P0,_,_,_,_,W,decap(not_begin),_),
    atom_concat(_,e,W),
    atom_concat(W,n,Inf),
    P is P0+1, R is R0+1,
    findall(Stem,(alpino_lex:lexicon(verb(Hz,inf,Sc),Stem,[Inf],[],_),
		  assert_tag(P0,P,R0,R,Stem,drop_n,
			     verb(Hz,inf,Sc))
		 ),[H0|T0]
	   ),
    sort([H0|T0],Stems).

unknown_word_heuristic(P0,R0,W0,_,"end_d_t|~p|~p~n",[W,Stems],_,len(1)) :-
    debug_message(3,"trying heuristic end_d_t~n",[]),
    decap(W0,W),
    atom(W),
    \+ tag(P0,_,_,_,_,W,decap(not_begin),_),
    atom_concat(Prefix,d,W),
    atom_concat(Prefix,t,Alt),
    P is P0+1, R is R0+1,
    findall(Stem,(alpino_lex:lexicon(verb(Hz,sg3,Sc),Stem,[Alt],[],_),
		  assert_tag(P0,P,R0,R,Stem,drop_n,
			     verb(Hz,sg3,Sc))
		 ),[H0|T0]
	   ),
    sort([H0|T0],Stems).

measure_tag(meas_mod_noun(A,B,M0),
            meas_mod_noun(A,B,M)) :-
    measure_meas(M0,M).
measure_tag(meas_mod_noun(A,B,M0,Sc),
            meas_mod_noun(A,B,M,Sc)) :-
    measure_meas(M0,M).

measure_meas(meas,bare_meas).
measure_meas(bare_meas,bare_meas).
measure_meas(sg,sg).
measure_meas(pl,pl).
measure_meas(both,both).

unknown_word_heuristic(P0,R0,W0,_,"measure|~p|~p|~p~n",[W0,Stam,Measure],_,len(1)):-
    debug_message(3,"trying heuristic measure~n",[]),
    P is P0+1, R is R0+1,
    atom(W0),
    between(1,5,TwintigL),
    sub_atom(W0,0,TwintigL,_,Twintig),
    alpino_lex:lexicon(number(hoofd(pl_num)),TwintigStam,[Twintig],[],number_expression),
    atom_concat(Twintig,Kg,W0), 
    alpino_lex:lexicon(Measure0,KgStam,[Kg],[],_),
    measure_tag(Measure0,Measure),
    atom_concat(TwintigStam,'_',Stam1),
    atom_concat(Stam1,KgStam,Stam),
    assert_tag(P0,P,R0,R,Stam,measure,Measure).

unknown_word_heuristic(P1,R1,W,_,"te-Verb|~p~n",[W],_,len(1)) :-
    debug_message(3,"trying heuristic te-Verb~n",[]),
    atom(W),
    \+ W = vreden,
    \+ W = allen,  % ?? volgens R.Baars, maar niet kunnen reproduceren
    \+ W = gen,
    \+ W = zijnen,
    \+ W = een,
    \+ starts_with_capital(W),
    (  atom_concat(_,aan,W),	% te veslaan
       atom_concat(Stem,an,W)
    ;  atom_concat(Stem,en,W)
    ),
    te_context(P1,R1),
    P is P1 + 1, R is R1 + 1,
    (   member(Tag,[ verb('hebben/zijn',inf,intransitive),
		     verb('hebben/zijn',inf,transitive)
		   ]),
	assert_tag(P1,P,R1,R,Stem,te_verb,Tag),
	fail
    ;   true
    ).

unknown_word_heuristic(P1,R1,W,_,"te-Adj|~p~n",[W],_,len(1)) :-
    debug_message(3,"trying heuristic te-Adj~n",[]),
    \+ starts_with_capital(W),
    te_int_context(P1,R1),
    P is P1 + 1, R is R1 + 1,
    atom(W),
    \+ atom_concat(_,en,W),
    assert_tag(P1,P,R1,R,W,te_adj,adjective(no_e(adv))).

unknown_word_heuristic(P1,R1,W,_,"avonds|~p ~p|~p~n",[Quote,W,Tag],_,len(1)) :-
    debug_message(3,"trying heuristic avonds~n",[]),
    atom(W),
    decap_first(W,W1),
    quote_context(P1,R1,P0,R0,Quote),
    P is P1 + 1, R is R1 + 1,
    alpino_lex:lexicon(Tag,Stem,[Quote,W1],[],His),
    assert_tag(P0,P,R0,R,Stem,avonds(His),Tag).

quote_context(P1,R1,P0,R0,Quote) :-
    tag(P0,P1,R0,R1,Quote,_,_,_),
    starts_with_quote(Quote).

te_context(P1,R1) :-
    tag(_P0,P1,_R0,R1,te,te,normal(normal),complementizer(te)).

te_context(P1,R1) :-
    tag(P2,P1,R2,R1,_,Surf,_,Tag),
    (  alpino_lexical_analysis:skip_word(Surf)
    ;  alpino_lexical_analysis:skip_tag(Tag)
    ),
    tag(_P0,P2,_R0,R2,te,te,normal(normal),complementizer(te)).

te_int_context(P1,R1) :-
    tag(_P0,P1,_R0,R1,te,te,normal(normal),intensifier).

te_int_context(P1,R1) :-
    tag(P2,P1,R2,R1,_,Surf,_,Tag),
    (  alpino_lexical_analysis:skip_word(Surf)
    ;  alpino_lexical_analysis:skip_tag(Tag)
    ),
    tag(_P0,P2,_R0,R2,te,te,normal(normal),intensifier).

unknown_word_heuristic(P1,R1,W,_,"lonely-name|~p~n",[W],_,len(1)) :-
    debug_message(3,"trying heuristic lonely-name~n",[]),
    name_capital(W,P1),
    unlikely_name_lonely(W),!,
    start_of_sentence(P1,Start),
    P is P1 + 1, R is R1 + 1,
    \+ (   tag(Q0,Q,_,_,_,_,name(_),_),
           Q0 =< P1,
           P =< Q
       ),
    assert_tag(P1,P,R1,R,W,name(Start),proper_name(both)).

unknown_word_heuristic(P1,R1,W,Ws,"dash|~p|~p~n",[W,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic dash~n",[]),
    atom(W),
    atom_concat('-',Word,W),
    findall(Tag,alternative([Word|Ws],P1,_,R1,_,dash,Tag,HIS),[Th|Tt]).

unknown_word_heuristic(P1,R1,W,Ws,"spaced|~p|~p~n",[W,[Th|Tt]],_,HIS) :-
    debug_message(3,"trying heuristic spaced~n",[]),
    alpino_lex:long_single_letter_sequence(Sequence,Ws,_),
    findall(Tag,spaced_alternative([W|Sequence],P1,_,R1,_,spaced,Tag,HIS),[Th|Tt]).

unknown_word_heuristic(P1,R1,W,Ws,"mistok|~p|~p|~p~n",[W,W1,[H|T]],_,HIS) :-
    debug_message(3,"trying heuristic mistok~n",[]),
    \+ tag(P1,_,R1,_,_,_,decap(_),_),
    atom(W),
    mis_tokenized(W,W1),
    \+ W1 = '',
    findall(Tag,alternative([W1|Ws],P1,_,R1,_,mistok,Tag,HIS),[H|T]).

% -De
mis_tokenized(W,W1) :-
    atom_concat('',W1,W).
mis_tokenized(W,W1) :-
    atom_concat('',W1,W).
mis_tokenized(W,W1) :-
    atom_concat(W1,'',W).
mis_tokenized(W,W1) :-
    atom_concat(W1,'',W).
mis_tokenized(W,W1) :-
    atom_concat('-',W2,W),
    decap_first(W2,W1).
mis_tokenized(W,W1) :-
    atom_concat('«',W2,W),
    decap_first(W2,W1).
mis_tokenized(W,W1) :-
    atom_concat('-',W1,W).
mis_tokenized(W,W1) :-
    atom_concat('«',W1,W).
mis_tokenized(W,W1) :-
    atom_concat('”',W1,W).
mis_tokenized(W,W1) :-
    atom_concat(W1,'.',W).
mis_tokenized(W,W1) :-
    atom_concat(W1,'!',W).
mis_tokenized(W,W1) :-
    atom_concat(W1,'?',W).
mis_tokenized(W,W1) :-
    atom_concat(W1,';',W).
mis_tokenized(W,W1) :-
    atom_concat(W1,',',W).
mis_tokenized(W,W1) :-
    atom_concat(W1,':',W).
mis_tokenized(W,W1) :-
    atom_concat(W1,'\'',W).
mis_tokenized(W,W1) :-
    atom_concat(W1,'"',W).  % "
mis_tokenized(W,W1) :-
    atom_concat(W1,'”',W).  % "
mis_tokenized(W,W1) :-
    atom_concat(W1,'*',W).
mis_tokenized(W,W1) :-
    atom_concat(W1,'-',W).
mis_tokenized(W,W1) :-
    atom_concat(W1,'»',W).
mis_tokenized(W,W1) :-
    atom_concat(W1,'.»',W).
mis_tokenized(W,W1) :-
    atom_concat(W1,'!»',W).
mis_tokenized(W,W1) :-
    atom_concat(W1,'?»',W).
mis_tokenized(W,W1) :-
    atom_concat('(',R,W),
    atom_concat(W1,')',R).

unknown_word_heuristic(P1,R1,W,_Ws,"onomatopoeia|~p~n",[W],_,len(1)) :-
    debug_message(3,"trying heuristic onomatopoeia~n",[]),
    atom(W),
    (  atom_concat(_,'\\o',W)
    ;  atom_concat(_,'\\O',W)
    ), !,
    P is P1 + 1, R is R1 + 1,
    assert_tag(P1,P,R1,R,W,onomatopoeia,tag).

unknown_word_heuristic(P1,R1,W,_Ws,"default|~p|~p~n",
		       [W,noun(both,both,both)],_Words,len(1)) :-
    debug_message(3,"trying heuristic default~n",[]),
				%    R2 is R1 + 1,

    \+ (   tag(P1,_,_,_,Surf,_,name(_),_),
	   only_capitals(Surf,_)
       ),
    
    \+ name_and(W),
    \+ (   tag(P1,_,_,_,_,_,name(_),_),
	   (   name_title(W)
	   ;   name_vanhet(W)
	   )
       ),
    \+ tag(P1,_,_,_,_,_,decap(_),_),
    \+ tag(P1,_,_,_,_,_,prefix_name,_),
    \+ (   name_capital(W,P1),
	   tag(P1,_,_,_,_,_,_,_)
       ),
    \+ tag(P1,_,_,_,_,_,diminutive,_),
    \+ tag(P1,_,_,_,_,_,compound(_),noun(_,_,_)),
    \+ tag(P1,_,_,_,_,_,decap_and_compound(_),noun(_,_,_)),
    \+ tag(P1,_,_,_,_,_,compound(_),tmp_noun(_,_,_)),
    \+ tag(P1,_,_,_,_,_,decap_and_compound(_),tmp_noun(_,_,_)),
    \+ tag(P1,_,_,_,_,_,compound(_),mod_noun(_,_,_)),
    \+ tag(P1,_,_,_,_,_,decap_and_compound(_),mod_noun(_,_,_)),
    \+ tag(P1,_,_,_,_,_,compound(_),meas_mod_noun(_,_,_)),
    \+ tag(P1,_,_,_,_,_,decap_and_compound(_),meas_mod_noun(_,_,_)),
    \+ tag(P1,_,_,_,_,_,left_headed_compound,noun(_,_,_)),
    \+ tag(P1,_,_,_,_,_,measure,_),
    \+ tag(P1,_,_,_,_,_,spelling_variant_compound(_),_),
    \+ tag(P1,_,_,_,_,_,w_dia,_),
    \+ tag(P1,_,_,_,_,_,wo_dia,_),
    \+ tag(P1,_,_,_,_,_,mistok,_),
    \+ tag(P1,_,_,_,_,_,decap_w_dia,_),
    \+ tag(P1,_,_,_,_,_,decap_wo_dia,_),
    \+ tag(P1,_,_,_,_,_,nom_adj,nominalized_adjective),
    \+ tag(P1,_,_,_,_,_,form_of_suffix(achtigen),nominalized_adjective),
    \+ tag(P1,_,_,_,_,_,form_of_suffix(ische),_),
    \+ tag(P1,_,_,_,_,_,dashed_mwu,_),
    \+ tag(P1,_,_,_,_,_,verb_ster,_),
    findall(_,lexical_analysis_tag(noun(both,both,both),P1,R1,W,noun),[_|_]).

unknown_word_heuristic(P,_,_,_,_,_,_,_) :-
    debug_message(2,"heuristics at ~p DONE ~n",[P]),
    fail.

%% 1. don't hallucinate Dutch for English (French, German, ...)
%% also, these are often non-capitalized parts of titles etc.
%% 2. list common English (...) words that are also in Dutch lexicon,
%% so these are allowed to be part of long foreign quotes/terms.

foreign_word(bracket(_)) :-
    !,
    fail.

foreign_word(a).
foreign_word(abnormale).  % in scientific names
foreign_word(about).
foreign_word(account).
foreign_word(after).
foreign_word(again).
foreign_word(ainsi).
foreign_word(air).
foreign_word(airline).
foreign_word(all).
foreign_word(alles).
foreign_word(also).
foreign_word(am).
foreign_word(amour).
foreign_word(an).
foreign_word(anale).  % in scientific names
foreign_word(and).
foreign_word(angel).
foreign_word(angels).
foreign_word(are).
foreign_word(arms).
foreign_word(art).
foreign_word(arts).
foreign_word(as).
foreign_word(assist).
foreign_word(at).
foreign_word(au).
foreign_word(auch).
foreign_word(auf).
foreign_word(audit).
foreign_word('aujourd\'hui').
foreign_word(aus).
foreign_word(aussi).
foreign_word(autrement).
foreign_word(aux).
foreign_word(avec).
foreign_word(award).
foreign_word(away).
foreign_word(baby).
foreign_word(back).
foreign_word(bad).
foreign_word(ballet).
foreign_word(bangers).
foreign_word(basale). % scientific names
foreign_word(baseline).
foreign_word(baselines).
foreign_word(basic).
foreign_word(be).
foreign_word(beauty).
foreign_word(been).
foreign_word(bei).
foreign_word(belief).
foreign_word(best).
foreign_word(bête).
foreign_word(bêtes).
foreign_word(big).
foreign_word(bis).
foreign_word(black).
foreign_word(blanc).
foreign_word(bleu).
foreign_word(blow).
foreign_word(board).
foreign_word(boards).
foreign_word(body).
foreign_word(bonbon).
foreign_word(bonbons).
foreign_word(booming).
foreign_word(bottle).
foreign_word(boy).
foreign_word(boys).
foreign_word(box).
foreign_word(brand).
foreign_word(break).
foreign_word(breeds).
foreign_word(bug).
foreign_word(business).
foreign_word(but).
foreign_word(by).
foreign_word(call).
foreign_word(calls).
foreign_word(camp).
foreign_word(campus).
foreign_word(can).
foreign_word('can\'t').
foreign_word(care).
foreign_word(cast).
foreign_word(casual).
foreign_word(casuals).
foreign_word(ce).
foreign_word(center).
foreign_word(certains).
foreign_word(ces).
foreign_word('c\'est').
foreign_word(cette).
foreign_word(change).
foreign_word(check).
foreign_word('check-in').
foreign_word(chef).
foreign_word(chip).
foreign_word(cinema).
foreign_word(city).
foreign_word(class).
foreign_word(clock).
foreign_word(close).
foreign_word(club).
foreign_word(come).
foreign_word(comme).
foreign_word(commercial).
foreign_word(commercials).
foreign_word(consent).
foreign_word(consultant).
foreign_word(consultants).
foreign_word(cool).
foreign_word(corner).
foreign_word(could).
foreign_word(country).
foreign_word(credit).
foreign_word(crew).
foreign_word(crime).
foreign_word(culture).
foreign_word('d\'un').
foreign_word('d\'une').
foreign_word(dance).
foreign_word(dansant).
foreign_word(dansants).
foreign_word(das).
foreign_word(dass).
foreign_word(date).
foreign_word(day).
foreign_word(de).
foreign_word(dead).
foreign_word(death).
foreign_word(dein).
foreign_word(del).
foreign_word(della).
foreign_word(dem).
foreign_word(den).
foreign_word(der).
foreign_word(des).
foreign_word(design).
foreign_word(deux).
foreign_word(dich).
foreign_word(did).
foreign_word(die).
foreign_word(dixit).
foreign_word(do).
foreign_word(does).
foreign_word('doesn\'t').
foreign_word(dollar).
foreign_word(dollars).
foreign_word(donc).
foreign_word('don\'t').
foreign_word(dont).
foreign_word(dorsale).   % scientific names
foreign_word(dossier).
foreign_word(dossiers).
foreign_word(down).
foreign_word(drink).
foreign_word(du).
foreign_word('d\'une').
foreign_word(e).
foreign_word(economy).
foreign_word(ein).
foreign_word(eine).
foreign_word(einer).
foreign_word(einem).
foreign_word(einen).
foreign_word(el).
foreign_word(elle).
foreign_word(encore).
foreign_word(end).
foreign_word(enterprise).
foreign_word(entertainment).
foreign_word(entre).
foreign_word(ergo).
foreign_word(error).
foreign_word(est).
foreign_word(et).
foreign_word(even).
foreign_word(evens).
foreign_word(ever).
foreign_word(été).
foreign_word(être).
foreign_word(factor).
foreign_word(fair).
foreign_word(fait).
foreign_word(family).
foreign_word(father).
foreign_word(featuring).
foreign_word(fee).
foreign_word(feel).
foreign_word(field).
foreign_word(file).
foreign_word(first).
foreign_word(fit).
foreign_word(fits).
foreign_word(fix).
foreign_word(focus).
foreign_word(for).
foreign_word(format).
foreign_word(formats).
foreign_word(frame).
foreign_word(free).
foreign_word(from).
foreign_word(full).
foreign_word(fun).
foreign_word(funding).
foreign_word(fuzzy).
foreign_word(für).
foreign_word(game).
foreign_word(games).
foreign_word(gap).
foreign_word(gegen).
foreign_word(gendarme).
foreign_word(générale).
foreign_word(gens).
foreign_word(get).
foreign_word(girl).
foreign_word(glamour).
foreign_word(global).
foreign_word(go).
foreign_word(goes).
foreign_word(gold).
foreign_word(golden).
foreign_word(good).
foreign_word(got).
foreign_word(gum).
foreign_word(gun).
foreign_word(gut).
foreign_word(grand).
foreign_word(grande).
foreign_word(grandes).
foreign_word(great).
foreign_word(green).
foreign_word(haben).
foreign_word(had).
foreign_word(happy).
foreign_word(hard).
foreign_word(has).
foreign_word(hat).
foreign_word(hate).
foreign_word(have).
foreign_word(he).
foreign_word(heart).
foreign_word(her).
foreign_word(here).
foreign_word(high).
foreign_word(highs).
foreign_word(him).
foreign_word(his).
foreign_word(home).
foreign_word(homo).
foreign_word(hot).
foreign_word(hots).
foreign_word(house).
foreign_word(how).
foreign_word(i).
foreign_word('I\'m').
foreign_word(ich).
foreign_word(if).
foreign_word(ihn).
foreign_word(il).
foreign_word(im).
foreign_word(in).
foreign_word(indeed).
foreign_word(input).
foreign_word(instant).
foreign_word(into).
foreign_word(is).
foreign_word(issue).
foreign_word(issues).
foreign_word(ist).
foreign_word(it).
foreign_word(its).
foreign_word('it\'s').
foreign_word(jazz).
foreign_word(je).
foreign_word(joint).
foreign_word(jus).
foreign_word(just).
foreign_word(kick).
foreign_word(know).
foreign_word(la).
foreign_word(label).
foreign_word(le).
foreign_word(les).
foreign_word(let).
foreign_word(leur).
foreign_word(leurs).
foreign_word(libre).
foreign_word(life).
foreign_word(light).
foreign_word(lights).
foreign_word(like).
foreign_word(little).
foreign_word(live).
foreign_word(lives).
foreign_word(long).
foreign_word(look).
foreign_word(loop).
foreign_word(lors).
foreign_word(los).
foreign_word(lost).
foreign_word(lounge).
foreign_word(love).
foreign_word(lover).
foreign_word(loves).
foreign_word(low).
foreign_word(loyelty).
foreign_word(ma).
foreign_word(maai).
foreign_word(made).
foreign_word(mais).
foreign_word(major).
foreign_word(make).
foreign_word(man).
foreign_word(management).
foreign_word(marginale).  % in scientific names
foreign_word(market).
foreign_word(marketing).
foreign_word(match).
foreign_word(matter).
foreign_word(matters).
foreign_word(may).
foreign_word(me).
foreign_word(media).
foreign_word(meet).
foreign_word(meeting).
foreign_word(meetings).
foreign_word(mehr).
foreign_word(men).
foreign_word(même).
foreign_word(mich).
foreign_word(military).
foreign_word(mind).
foreign_word(mit).
foreign_word(mode).
foreign_word(model).
foreign_word(module).
foreign_word(mon).
foreign_word(monsieur).
foreign_word(more).
foreign_word(mort).
foreign_word(much).
foreign_word(music).
foreign_word(my).
foreign_word(naõ).
foreign_word(name).
foreign_word('n\'est').
foreign_word(never).
foreign_word(new).
foreign_word(nicht).
foreign_word(night).
foreign_word(no).
foreign_word(node).
foreign_word(non).
foreign_word(not).
foreign_word(nothing).
foreign_word(nous).
foreign_word(now).
foreign_word(o).
foreign_word(of).
foreign_word(off).
foreign_word(official).
foreign_word(old).
foreign_word(on).
foreign_word(one).
foreign_word(online).
foreign_word(only).
foreign_word(on).
foreign_word(ont).
foreign_word(operator).
foreign_word(operators).
foreign_word(or).
foreign_word(os).  % beginwoord voor rare botjes
foreign_word(other).
foreign_word(ou).
foreign_word(où).
foreign_word(our).
foreign_word(out).
foreign_word(output).
foreign_word(ovale).
foreign_word(over).
foreign_word(own).
foreign_word(paper).
foreign_word(para).
foreign_word(part).
foreign_word(partner).
foreign_word(party).
foreign_word(pas).
foreign_word(pass).
foreign_word(passer).
foreign_word(people).
foreign_word(perfect).
foreign_word(peur).
foreign_word(peut).
foreign_word('pick-up').
foreign_word(pink).
foreign_word(planning).
foreign_word(play).
foreign_word(point).
foreign_word(points).
foreign_word(por).
foreign_word(pour).
foreign_word(power).
foreign_word(producer).
foreign_word(product).
foreign_word(professional).
foreign_word(professionals).
foreign_word(protocol).
foreign_word(public).
foreign_word(que).
foreign_word(qui).
foreign_word('qu\'il').
foreign_word(quite).
foreign_word(rating).
foreign_word(ratio).
foreign_word(real).
foreign_word(really).
foreign_word(record).
foreign_word(recorder).
foreign_word(recorders).
foreign_word(recycling).
foreign_word(red).
foreign_word(relaxed).
foreign_word(release).
foreign_word(releases).
foreign_word(research).
foreign_word(return).
foreign_word(right).
foreign_word(rock).
foreign_word(road).
foreign_word(roei).   % scientific
foreign_word(room).
foreign_word(rooms).
foreign_word(roses).
foreign_word(rund).
foreign_word(running).
foreign_word(sa).
foreign_word(safe).
foreign_word(said).
foreign_word(sans).
foreign_word(say).
foreign_word(screen).
foreign_word(scroll).
foreign_word(se).
foreign_word(see).
foreign_word(sein).
foreign_word(service).
foreign_word(services).
foreign_word(ses).
foreign_word(set).
foreign_word(sie).
foreign_word(simple).
foreign_word(single).
foreign_word(singles).
foreign_word(she).
foreign_word(short).
foreign_word(shot).
foreign_word(shots).
foreign_word(should).
foreign_word(show).
foreign_word(shows).
foreign_word(showroom).
foreign_word(si).
foreign_word(sleazy).
foreign_word(sleep).
foreign_word(slip).
foreign_word(small).
foreign_word(smart).
foreign_word(so).
foreign_word(soft).
foreign_word(software).
foreign_word(solitaire).
foreign_word(sollen).
foreign_word(some).
foreign_word(son).
foreign_word(song).
foreign_word(songs).
foreign_word(sont).
foreign_word(sorry).
foreign_word(sound).
foreign_word(space).
foreign_word(special).
foreign_word(spot).
foreign_word(spots).
foreign_word(stand).
foreign_word(stands).
foreign_word(star).
foreign_word(stars).
foreign_word(state).
foreign_word(statement).
foreign_word(status).
foreign_word(still).
foreign_word(stop).
foreign_word(story).
foreign_word(stream).
foreign_word(subtitles).
foreign_word(such).
foreign_word(suis).
foreign_word(sur).
foreign_word(team).
foreign_word(tempo).
foreign_word(take).
foreign_word(takes).
foreign_word(tango).
foreign_word(tér).  % Hongaarse straat?
foreign_word(test).
foreign_word(than).
foreign_word(that).
foreign_word('that\'s').
foreign_word(the).
foreign_word(their).
foreign_word(them).
foreign_word(then).
foreign_word(there).
foreign_word(they).
foreign_word(thing).
foreign_word(things).
foreign_word(think).
foreign_word(this).
foreign_word(ticket).
foreign_word(til). 
foreign_word(time).
foreign_word(to).
foreign_word(too).
foreign_word(top).
foreign_word(tout).
foreign_word(track).
foreign_word(tracks).
foreign_word(train).
foreign_word(trains).
foreign_word(très).
foreign_word(trial).
foreign_word(triviale). % scientific names
foreign_word(trials).
foreign_word(tour).
foreign_word(um).
foreign_word(un).
foreign_word(und).
foreign_word(une).
foreign_word(uniforme).  %  scientific names
foreign_word(unit).
foreign_word(uns).
foreign_word(up).
foreign_word(upgrade).
foreign_word(urban).
foreign_word(us).
foreign_word(vers).
foreign_word(very).
foreign_word(vie).
foreign_word(vieux).
foreign_word(virus).
foreign_word(vou).
foreign_word(voucher).
foreign_word(vouchers).
foreign_word(vous).
foreign_word(wake).
foreign_word(want).
foreign_word(was).
foreign_word(war).
foreign_word(wax).
foreign_word(way).
foreign_word(we).
foreign_word(well).
foreign_word(were).
foreign_word(what).
foreign_word(whatever).
foreign_word(when).
foreign_word(where).
foreign_word(white).
foreign_word(who).
foreign_word(why).
foreign_word(will).
foreign_word(win).
foreign_word(with).
foreign_word(without).
foreign_word(woman).
foreign_word(women).
foreign_word(work).
foreign_word(world).
foreign_word(would).
foreign_word(y).
foreign_word(ya).
foreign_word(year).
foreign_word(years).
foreign_word(you).
foreign_word('you\'re').
foreign_word(young).
foreign_word(your).
foreign_word(zu).

foreign_word('*').
foreign_word('&').

foreign_word('PIC').
foreign_word('FIG').
foreign_word('FILE').

decap_foreign_word(X) :- foreign_word(X).
decap_foreign_word(X) :- decap_first(X,Xx), foreign_word(Xx).

decap_foreign_word(X,X) :- foreign_word(X).
decap_foreign_word(X,Xx) :- decap_first(X,Xx), foreign_word(Xx).


decap_first(Capped,Small) :-
    atom(Capped),
    atom_codes(Capped,[Upper|Codes]),
    isupper(Upper),
    tolower(Upper,Lower),    
    atom_codes(Small,[Lower|Codes]).

%% these words can be first words of names, but never single name..
decap_only_one('Alle').
decap_only_one('De').
decap_only_one('Dat').
decap_only_one('Deze').
decap_only_one('Dit').
decap_only_one('Drie').
decap_only_one('Goedenavond').
decap_only_one('Het').
decap_only_one('Heel').
decap_only_one('Ik').
decap_only_one('Twee').
decap_only_one('Vier').
decap_only_one('Rechtstreeks').

%% hack to rule out very frequent beginning of sentence words,
%% which are very unlikely *first parts* of names

is_decap_only(Word) :-
    atom(Word),
    atom_length(Word,Len),
    Len < 50,
    decap_only(Word).

%% part one, function words etc.
decap_only('Aan').
decap_only('Aandacht').
decap_only('Aangezien').
decap_only('Aanvang').
decap_only('Actualiteiten').
decap_only('Actueel').
decap_only('Afbeelding').
decap_only('Al').
decap_only('Albumtrack').
decap_only('Aldus').
decap_only('Alleen').
decap_only('Allereerst').
decap_only('Allez').
decap_only('Als').
decap_only('Alsof').
decap_only('Alweer').
decap_only('Behalve').
decap_only('Ben').
decap_only('Beste'). 
decap_only('Bij').
decap_only('Bijvoorbeeld').
decap_only('Binnen').
decap_only('Breng').
decap_only('Daar').
decap_only('Daarom').
decap_only('Dáárom').
decap_only('Dankzij').
decap_only('Denk').
decap_only('Edoch').
decap_only('Een').
decap_only('Eind').
decap_only('En').
decap_only('Evenals').
decap_only('Fijn').
decap_only('Foto').
decap_only('Geen').
decap_only('Gelet').
decap_only('Gezien').
decap_only('Gij').
decap_only('Had').
decap_only('Hallo').
decap_only('Heeft').
decap_only('Hem').
decap_only('Hier').
decap_only('Hij').
decap_only('Hoe').
decap_only('Hoeveel').
decap_only('Hoofdstuk').
decap_only('Hun').
decap_only('Illustratie').
decap_only('In').
decap_only('Is').
decap_only('Ja').
decap_only('Jazeker').
decap_only('Je').
decap_only('Kan').
decap_only('Maar').
decap_only('Mama').
decap_only('Me').
decap_only('Met').
decap_only('Na').
decap_only('Nadat').
decap_only('Naschrift').
decap_only('Nee').
decap_only('Niet').
decap_only('Niets').
decap_only('Nogmaals').
decap_only('Of').
decap_only('Om').
decap_only('Omdat').
decap_only('Ondanks').
decap_only('Ondertiteling').
decap_only('Ook').
decap_only('Op').
decap_only('Over').
decap_only('Overwegende').
decap_only('Pagina'). decap_only('PAGINA').
decap_only('Postbus').
decap_only('Procent').		% often capitalized !?!?
decap_only('Publicatiedatum').
decap_only('Regie').
decap_only('Rond').
decap_only('Sinds').
decap_only('Slechts').
decap_only('Soms').
decap_only('Te').
decap_only('Tegen').
decap_only('Tegenover').
decap_only('Terwijl').
decap_only('Tijdens').
decap_only('Toch').
decap_only('Toen').
decap_only('Ton').
decap_only('Tot').
decap_only('Tussen').
decap_only('Typisch').
decap_only('TV').
decap_only('U').
decap_only('Uit').
decap_only('Uitgerekend').
decap_only('Uw').
decap_only('Vanwege').
decap_only('Vanuit').
decap_only('Veel').
decap_only('Vert.').
decap_only('Vind').
decap_only('Volgens').
decap_only('Volk').
decap_only('Voor').
decap_only('Vooral').
decap_only('Waar').
decap_only('Waarom').
decap_only('Waarop').
decap_only('Wanneer').
decap_only('Want').
decap_only('Wat').
decap_only('Welk').
decap_only('Welke').
decap_only('Wie').
decap_only('Wil').
decap_only('Wiens').
decap_only('Winst').
decap_only('Zaken').
decap_only('Zal').
decap_only('Ze').
decap_only('Zelfs').
decap_only('Zie').
decap_only('Zijn').
decap_only('Zo').
decap_only('Zo\'n').
decap_only('Zoals').
decap_only('Zondag').
decap_only('Zonder').
decap_only('Zou').
decap_only('Zowel').


%% part two, words typically taking an apposition name
%% todo: suffixes  'PvdA-woordvoerder X'
%% -kenner
%% -deskundige
%% agent
%% ajacied
%% -doelman
%% -fan(s)
%% -adept
%% -trainer

decap_only(X) :-
    function(X).

decap_only(Atom) :-
    between(3,24,SuffixLen),
    sub_atom(Atom,_,SuffixLen,0,Suffix),
    atom_concat(Prefix,Suffix,Atom),
    (   \+ never_compound_part(Prefix),
	decap_only_suffix(Suffix)
    ;   SuffixLen > 4,
	cap_first(Suffix,Function),
	function(Function),
	alpino_lex:lexicon(Tag,_,[Prefix],[],_),
	proper_name_tag(Tag)
    ).

decap_only(Atom) :-
    strip_accents(Atom,Atom1),
    decap_only(Atom1).

function('Aandeelhouder').
function('Aanklager').
function('Aannemer').
function('Aanvaller').
function('Aanvoerder').
function('Aartsbisschop').
function('Aartsengel').
function('Aartsrivaal').
function('Accountant').
function('Accountantskantoor').
function('Achtervolger').
function('Achtervolgers').
function('Acteur').
function('Acteurs').
function('Actrice').
function('Actrices').
function('Activist').
function('Admiraal').
function('Ajacied').
function('Algerijn').
function('Amerikaan').
function('Amerikanen').
function('Ambassadeur').
function('Ambassadrice').
function('Analist').
function('Analiste').
function('Architect').
function('Argentijn').
function('Armeniër').
function('Arts').
function('Assistent').
function('Assistente').
function('Astronaut').
function('Atleet').
function('Atlete').
function('Australiër').
function('Autocoureur').
function('Automatiseerder').
function('Ayatollah').
function('Beklaagde').
function('Belg').
function('Bestuurder').
function('Bestuurslid').
function('Bewoner').
function('Bewoonster').
function('Bestuurslid').
function('Bestuursvoorzitter').
function('Bierbrouwer').
function('Bisschop').
function('Bondsarts').
function('Bondscoach').
function('Bondskanselier').
function('Branchegenoot').
function('Brit').
function('Broeder').
function('Broer').
function('Buitenlandse').
function('Buurland').
function('Canadees').
function('CEO').
function('Chileen').
function('Coach').
function('Collega').
function('Collumnist').
function('Commandant').
function('Commissaris').
function('Concurrent').
function('Correspondent').
function('Correspondente').
function('Darter').
function('Dichter').
function('Dichteres').
function('Directeur').
function('Dirigent').
function('Doelman').
function('Docent').
function('Docente').
function('Dochter').
function('Dokter').
function('Dominee').
function('Duitser').
function('Econoom').
function('Echtgenoot').
function('Echtgenote').
function('Eigenaar').
function('Electronicaconcern').
function('Eurocommissaris').
function('Europarlementariër').
function('Familie').
function('Feyenoorder').
function('Filosoof').
function('Fotograaf').
function('Fotografe').
function('Fotomodel').
function('Fractievoorzitter').
function('Fractievoorzitster').
function('Fransman').
function('Freule').
function('Gedeputeerde').
function('Generaal').
function('Gigant').
function('Gitarist').
function('Gouverneur').
function('Grensrechter').
function('Griek').
function('Grootaandeelhouder').
function('Hoofdcommissaris').
function('Hoofdredacteur').
function('Hoofdredactrice').
function('Hoogleraar').
function('Initiatiefnemer').
function('Inspecteur').
function('Inspectrice').
function('Invaller').
function('Invalster').
function('Italiaan').
function('Japanner').
function('Judoka').
function('Jonkvrouw').
function('Juffrouw').
function('Kamerlid').
function('Kamervoorzitter').
function('Kamervoorzitster').
function('Kampioen').
function('Kanselier').
function('Keniaan').
function('Koploper').
function('Kopman').
function('Korpschef').
function('Kroonprins').
function('Kroaat').
function('Landskampioen').
function('Lijsttrekker').
function('Madam').
function('Mademoiselle').
function('Makelaar').
function('Manager').
function('Marktleider').
function('Marokkaan').
function('Mejuffrouw').
function('Meneer').
function('Mevrouw').
function('Middenvelder').
function('Middenveldster').
function('Mijnheer').
function('Minister').
function('Nederlander').
function('Nigeriaan').
function('Nobelprijswinnaar').
function('Nobelprijswinnares').
function('Officier').
function('Onderzoeker').
function('Onderzoeksrechter').
function('Ontwerper').
function('Oostenrijker').
function('Oppositieleider').
function('Oppositieleidster').
function('Oprichter').
function('Oprichtster').
function('Organisatie').
function('Organisator').
function('Organisatrice').
function('Outsider').
function('Overste').
function('Partijleider').
function('Partijleidster').
function('Pianist').
function('Pianiste').
function('Ploeggenoot').
function('Ploeggenote').
function('Ploegleider').
function('Ploegleidster').
function('Politiechef').
function('Politieman').
function('Premier').
function('Preses').
function('Presentator').
function('Presentatrice').
function('President').
function('Producent').
function('Producente').
function('Raadsman').
function('Rapper').
function('Rapporteur').  % europarl
function('Rechter').
function('Regisseur').
function('Rotterdammer').
function('Russin').
function('Schaatser').
function('Schaatsster').
function('Scheidsrechter').
function('Senator').
function('Serviër').
function('Spanjaard').
function('Spits').
function('Sponsor').
function('Sprinter').
function('Sprintster').
function('Staatssecretaris').
function('Stadgenoot').
function('Tanzaniaan').
function('Tegenstander').
function('Tegenstandster').
function('Tennisser').
function('Titelhouder').
function('Titelverdediger').
function('Titelverdedigster').
function('Toetsenist').
function('Toneelmeester').
function('Topfotograaf').
function('Topfotografe').
function('Topman').
function('Topscorer').
function('Trainer').
function('Trainster').
function('Tsjech').
function('Tweede-Kamerlid').
function('Uitblinker').
function('Uitblinkster').
function('Uitgever').
function('Verdachte').
function('Verdediger').
function('Verdedigster').
function('Verslaggever').
function('Verzekeraar').
function('Verzorger').
function('Verzorgster').
function('Voorlichter').
function('Voorzitter').
function('Voorzitster').
function('Vrouw').
function('Wereldkampioen').
function('Wethouder').
function('Wielrenner').
function('Winnaar').
function('Woordvoerder').
function('Woordvoerdster').
function('Zanger').
function('Zangeres').
function('Zuid-Afrikaan').
function('Zuster').
function('Zweed').
function('Zwitser').

function(InterimPremier) :-
    atom(InterimPremier),
    atom_concat(interim,_,InterimPremier).
function(ExFun) :-
    atom(ExFun),
    atom_concat('Ex-',Fun,ExFun),
    function(Fun).
function(ExFun) :-
    atom(ExFun),
    atom_concat('Oud-',Fun,ExFun),
    function(Fun).
function(ExFun) :-
    atom(ExFun),
    atom_concat('Vice-',Fun,ExFun),
    function(Fun).

decap_only_suffix('\'er').  % de PvdA'er Jansen
decap_only_suffix('\'ers').  % de PvdA'ers Jansen en Jansen
decap_only_suffix(DashNoun) :-  % Feyenoord-verdediger
    atom(DashNoun),
    atom_concat('-',Noun,DashNoun),
    \+ never_final_compound_part(Noun),
    alpino_lex:lexicon(noun(_,_,_),_,[Noun],[],_).

name_koning(A,[]) :-
    name_koning(A).
name_koning('B',['&','W']).
name_koning('B',['&','W.']).
name_koning('B',[en,'W']).
name_koning('B',[en,'W.']).
name_koning('B.',['&','W']).
name_koning('B.',['&','W.']).
name_koning('B.',[en,'W']).
name_koning('B.',[en,'W.']).

name_koning('Abt').
name_koning('Bisschop').
name_koning('B&W').
name_koning('Burgemeester').
name_koning('Dominee').
name_koning('Frater').
name_koning('Generaal').
name_koning('Graaf').
name_koning('Gravin').
name_koning('Hertog').
name_koning('Hertogin').
name_koning('Kolonel').
name_koning('Koning').
name_koning('Koningin').
name_koning('Kapitein').
name_koning('Kardinaal').
name_koning('Majoor').
name_koning('Pastoor').
name_koning('Pater').
name_koning('Paus').
name_koning('President').
name_koning('Prins').
name_koning('Prinses').
name_koning('Professor').
name_koning('Schout').

guess_number(W) :-
    \+ wikipedia_list(W),
    \+ alpino_lexical_analysis:hesitation(W),
    atom(W),
    atom_codes(W,Codes),
    guess_number_(Codes).

%% starts with digit, or solely non-alphabetic and longer than 1 char
guess_number_([]).
guess_number_([H|T]) :-
    (   isdigit(H)
    ->  true
    ;   \+ isalpha(H),
        T = [N|T1],
        guess_number__([N|T1])
    ).

guess_number__([]).
guess_number__([H|T]) :-
    \+ isalpha(H),
    guess_number__(T).

guess_left_headed_compound(W,Ws,Wmin,Stem,Surf,SurfLength) :-
    atom(W),
    atom_codes(W,Codes),
    char_code('-',Hyphen),
    append([FirstH1,FirstH2|First],[Hyphen,RestH1,RestH2|RestT],Codes),
%    isupper(RestH1),		% verslag-van Noord
    islower(FirstH1),
    atom_codes(Wmin,[FirstH1,FirstH2|First]),
    \+ compound_part(Wmin,_),
    atom_codes(W1,[RestH1,RestH2|RestT]),
    hdrug_util:debug_message(5,"try lhc: ~w~n",[[W1|Ws]]),
    alpino_lex:lexicon(_,Stem,[W1|Ws],Remains,names_dictionary),
    append([_|Surf0],Remains,[W1|Ws]),
    length([W|Surf0],SurfLength),
    concat_all([W|Surf0],Surf,' ').

guess_prefix_compound(W,Wfirst,WLast) :-
    atom(W),
    once(atom_split(W,'-',Wfirst,WLast)),
    \+ Wfirst = 'Sint',  % Sint-X is often not a PER but a LOC or ORG
    (   %% oost-Frankrijk; ex-Ajax
        compound_part(Wfirst,_)
    ;   %% Ajax-Feyenoord
        alpino_lex:lexicon(_,_,[Wfirst],[],names_dictionary),
	alpino_lex:lexicon(_,_,[WLast],[],names_dictionary)
    ).

%% Groningen-centrum
guess_suffix_compound(W,Wfirst,WLast) :-
    atom(W),
    once(atom_split(W,'-',Wfirst,WLast)),
    loc_suffix(WLast),
    alpino_lex:lexicon(_,_,[Wfirst],[],names_dictionary).

loc_suffix(centrum).
loc_suffix(midden).
loc_suffix(noord).
loc_suffix(oost).
loc_suffix(west).
loc_suffix(zuid).

loc_suffix('Centrum').
loc_suffix('Midden').
loc_suffix('Noord').
loc_suffix('Oost').
loc_suffix('West').
loc_suffix('Zuid').

%%% memo'ed for long repeated sequences
:- dynamic guessed_compound/2.
:- thread_local guessed_compound/2.
guess_compound(W,Results0) :-
    (   guessed_compound(W,Results)
    ->  true
    ;   findall(Name-Last-Parts,guess_compoundXX(W,Last,Parts,Name),Wmins0),
	sort(Wmins0,Wmins1),
	remove_longer_compounds_with_same_suffix(Wmins1,Results),
	noclp_assertz(guessed_compound(W,Results))
    ),
    Results0=Results.

%% man/vrouw compounds
guess_compoundXX(W,WLast,[WfirstStem],slash) :-
    atom(W),
    once(atom_split(W,'/',Wfirst,WLast)),
    \+ never_compound_part(Wfirst),
    \+ never_compound_part(WLast),
    (   compound_part(first,Wfirst,WfirstStem)
    ->  true
    ;   Wfirst = WfirstStem
    ).

%% Word-Word compounds
guess_compoundXX(W,WLast,[WfirstStem],hyphen) :-
    atom(W),
    once(atom_split(W,'-',Wfirst,WLast)),
    \+ never_compound_part(Wfirst),
    \+ never_compound_part(WLast),
    (   compound_part(first,Wfirst,WfirstStem)
    ->  true
    ;   Wfirst = WfirstStem
    ).

%% WordWordWordWord compounds
guess_compoundXX(W,Wmin,Parts,Len) :-
    guess_proper_compound(W,Wmin,Parts,Len),
    \+ illegal_compound(Wmin,Parts).

illegal_compound(Tail,List) :-
    illegal_compound_pattern(Tail,Prefix),
    append(_,Prefix,List).


illegal_compound_pattern(ster,[ven]).
illegal_compound_pattern(meter,[kilo]).
illegal_compound_pattern(meters,[kilo]).
illegal_compound_pattern(positie,[ex]).
illegal_compound_pattern(posities,[ex]).
illegal_compound_pattern(strijd,[wed]).
illegal_compound_pattern(strijden,[wed]).
illegal_compound_pattern(tent,[as,sis]).

guess_proper_compound(W,Wmin,Parts,Len) :-
    atom(W),
    \+ contains_never_compound_part(W),
    guess_compound_(W,Wmin,1,Len,Parts0,[]),
    append(Parts,[_],Parts0),
    debug_message(2,"compound: ~p~n",[Parts0]).

guess_compound_(W,FinalPart,Len0,Len,[PrefixStem|Parts],Previous):-
    Len1 is Len0+1,
    atom(W),
    atom_pair_lengths(W,Prefix,Rest,20),
    compound_part(first,Prefix,PrefixStem),
    guess_compound__(Rest,FinalPart,Len1,Len,Parts,[PrefixStem|Previous],Prefix).

guess_compound__(W,W,L,L,[Wstem],_,_) :-
    compound_part(final,W,Wstem).
guess_compound__(SW,FinalPart,L0,L,Parts,Prev,PrevPrefix) :-
    atom(SW),
    atom_concat(s,W,SW),
    W \= schap, % no -s- with schap
    allow_verbindings_s(Prev),
    guess_compound__nos(W,FinalPart,L0,L,Parts,Prev,PrevPrefix).  % not 2x s "ademhalingsspieren =/= ademhaling_pier
guess_compound__(W,FinalPart,L0,L,Parts,Prev,PrevPrefix):-
    \+ do_not_split(W),
    guess_compound___(W,FinalPart,L0,L,Parts,Prev,PrevPrefix).

guess_compound__nos(W,W,L,L,[Wstem],_,_) :-
    compound_part(final,W,Wstem).
guess_compound__nos(W,FinalPart,L0,L,Parts,Prev,PrevPrefix):-
    \+ do_not_split(W),
    guess_compound___(W,FinalPart,L0,L,Parts,Prev,PrevPrefix).


allow_verbindings_s([her|_]):-
    !,
    fail.
allow_verbindings_s([nieuw|_]):-
    !,
    fail.
allow_verbindings_s([H0|_]) :-
    (   H0 = v_root(H1,_)
    ->  H1 = H
    ;   H0 = H
    ),
    \+ (  sub_atom(H,_,1,0,S),
	  forbid_verbindings_s(S)
       ),
    \+ only_verbal(H).

only_verbal(H) :-
    once(open_class_stem_tag_pair(non_final,H,_,Cat)),  % verb is last possibility
    Cat=verb(_,_,_).
%%    ;  Cat=noun(_,mass,_)  ????

%% don't allow verbindings-s after s
forbid_verbindings_s(s).
forbid_verbindings_s(a).
forbid_verbindings_s(e).
forbid_verbindings_s(i).
forbid_verbindings_s(o).
forbid_verbindings_s(u).


do_not_split(engezinnen). % migrantengezinnen ..
do_not_split(smiddelen).  % bevestiging-s-middelen ..
do_not_split(stermijn).   % inschrijvingstermijn

do_not_split(Word) :-
    atom(Word),
    alpino_lex:xl(Word,_,_,[],[]).

do_not_split(Word) :-
    atom(Word),
    alpino_lex:in_names_dictionary(_,Word,_,[],[],_).

guess_compound___(W,FinalPart,Len0,Len,[PrefixStem|Parts],Previous,PrevPrefix):-
    Len1 is Len0+1, Len1 < 10,    
    atom(W),
    atom_pair_lengths(W,Prefix,Rest,20),
    \+ Prefix = PrevPrefix,
    compound_part(middle,Prefix,PrefixStem),
    guess_compound__(Rest,FinalPart,Len1,Len,Parts,[PrefixStem|Previous],Prefix).

compound_part(first,W,Stem) :-
    \+ never_compound_part(W),
    once(open_class_stem_tag_pair(non_final,W,Stem,_)).

compound_part(first,W0,W) :-
    compound_part(W0,W).
compound_part(first,W0,W) :-
    decap_first(W0,W1),
    compound_part(W1,W).
compound_part(first,WD,W) :-
    atom(WD),
    atom_concat(W0,'-',WD),
    compound_part(W0,W).

compound_part(middle,W,Stem) :-
    \+ never_compound_part(W),
    \+ never_middle_compound_part(W),
    once(open_class_stem_tag_pair(non_final,W,Stem,_)).

compound_part(final,W,Stem) :-
    \+ never_compound_part(W),
    \+ never_final_compound_part(W),
    unique_open_class_stem_tag_pair(final,W,Stem).

compound_part(W,W) :-
    compound_part(W).
compound_part(kinder,kind).
compound_part(hersen,hersen).
compound_part(hoender,hoen).
compound_part(huishoud,huishoud).

compound_part(achteraf).
compound_part(anti).
compound_part(ex).
compound_part(half).
compound_part(her).
compound_part(hyper).
compound_part(inter).
compound_part(intra).
compound_part(kei).
compound_part(macro).
compound_part(mega).
compound_part(micro).
compound_part(multi).
compound_part(neo).
compound_part(niet).
compound_part(noord).
compound_part(oer).
compound_part(on).
compound_part(oost).
compound_part(oud).
compound_part(post).
compound_part(pre).
compound_part(pro).
compound_part(semi).
compound_part(sub).
compound_part(super).
compound_part(ultra).
compound_part(wan).
compound_part(welkomst).
compound_part(west).
compound_part(zuid).


unique_open_class_stem_tag_pair(Final,W,Stem) :-
    findall(Stem,open_class_stem_tag_pair(Final,W,Stem,_),List0),
    sort(List0,List),
    member(Stem,List).

%% since we try this for all possible suffixes, this ought to be fast
%% and therefore we immediately call alpino_lex:xl/5...
%%
%% try non-verbs/adjs before adjs before verbs, because the resulting lemma is more
%% often correct...
open_class_stem_tag_pair(Final,W,Stem,Cat) :-
    (   alpino_lex:xl(W,Cat0,Stem0,[],[]), 
	\+ Cat0 = verb(_,_,_),
	\+ Cat0 = adjective(_),
	\+ Cat0 = adjective(_,_),
	exc_stem(W,Stem0,Stem)
    ;   alpino_lex:xl(W,adjective(ADV),Stem,[],[]),
        Cat0 = adjective(ADV)
    ;   alpino_lex:in_names_dictionary(Cat0,W,Stem,[],[],_),
        \+ Cat0 = proper_name(_,'PER')
    ;   alpino_lex:simple_convert_number(W,_),
        Cat0 = number(hoofd(pl_num)),
        Stem=W
    ;   alpino_lex:xl(W,Cat0,Stem0,[],[]),
	Cat0 = verb(_,_,_),
	(   Final == non_final,
	    Stem0 = v_root(Stem,_)
	;   Stem0 = Stem
	)
    ),
    open_class_tag_or_name(Final,Cat0,Cat).

exc_stem(_,StemDim,Stem):-
    atom(StemDim),
    atom_concat(Stem,'_DIM',StemDim).
exc_stem(vol,volume,vol).
exc_stem(media,medium,media).
exc_stem(data,datum,data).
exc_stem(_,B,B).

open_class_tag_or_name(Final,Cat,Cat) :-
    open_class_tag_or_name(Final,Cat).
open_class_tag_or_name(final,Tag0,Tag) :-
    open_class_tag(Tag0,Tag).

open_class_tag_or_name(non_final,proper_name(_)).
open_class_tag_or_name(non_final,proper_name(_,_)).
open_class_tag_or_name(non_final,Tag) :-
    non_final_open_class_tag(Tag).

non_final_open_class_tag(verb(_,sg1,_)).
non_final_open_class_tag(number(hoofd(pl_num))).
non_final_open_class_tag(noun(_,_,_)).
non_final_open_class_tag(mod_noun(_,_,_)).
non_final_open_class_tag(tmp_noun(_,_,_)).
non_final_open_class_tag(meas_mod_noun(_,_,_)).
non_final_open_class_tag(adverb).
non_final_open_class_tag(dir_adverb).
non_final_open_class_tag(tmp_adverb).
non_final_open_class_tag(loc_adverb).
non_final_open_class_tag(adjective(X)) :- \+ X = prefix, \+ X = meer.

contains_never_compound_part(Atom) :-
    sub_atom(Atom,_,1,_,Sym),
    never_compound_part_symbol(Sym).

% never_compound_part_symbol('-').
never_compound_part_symbol('.').
never_compound_part_symbol(':').
never_compound_part_symbol('*').
% never_compound_part_symbol('/').
never_compound_part_symbol('0').
never_compound_part_symbol('1').
never_compound_part_symbol('2').
never_compound_part_symbol('3').
never_compound_part_symbol('4').
never_compound_part_symbol('5').
never_compound_part_symbol('6').
never_compound_part_symbol('7').
never_compound_part_symbol('8').
never_compound_part_symbol('9').

never_final_compound_part(aal).
never_final_compound_part(alen).
never_final_compound_part(aan).
never_final_compound_part(amen).
never_final_compound_part(end).
never_final_compound_part(enden).
never_final_compound_part(ere).
never_final_compound_part(ex).
never_final_compound_part(in).
never_final_compound_part(ion).
never_final_compound_part(ijk).
never_final_compound_part(ken).
never_final_compound_part(lig).
never_final_compound_part(na).
never_final_compound_part(naar).
never_final_compound_part(om).
never_final_compound_part(on).
never_final_compound_part(op).
never_final_compound_part(pel).
never_final_compound_part(ping).
never_final_compound_part(pro).
never_final_compound_part(ren).
never_final_compound_part(sis).
never_final_compound_part(tal).
never_final_compound_part(tel).
never_final_compound_part(tij).
never_final_compound_part(ven).
never_final_compound_part(loos). % is a derivation
never_final_compound_part(loze). % is a derivation

never_middle_compound_part(aan).
never_middle_compound_part(af).
never_middle_compound_part(in).
never_middle_compound_part(na).
never_middle_compound_part(om).
never_middle_compound_part(on).
never_middle_compound_part(op).
never_middle_compound_part(pi).

never_compound_part(L) :-
    atom(L),
    atom_length(L,Len), Len < 2.

never_compound_part(X) :-
    never_compound_part_sc(X).

never_compound_part(X) :-
    decap_first(X,DeCapX),
    never_compound_part_sc(DeCapX).

never_compound_part(Atom) :-
    never_compound_part_sub(Sym),
    sub_atom(Atom,_,_,_,Sym).

never_compound_part('Kinder').  
never_compound_part('Pro').  % but pro is ok

never_compound_part_sc(aar).
never_compound_part_sc(aars).
never_compound_part_sc(acc).
never_compound_part_sc(ace).
never_compound_part_sc(ach).
never_compound_part_sc(air).
never_compound_part_sc(al).
never_compound_part_sc(ama).
never_compound_part_sc(an).
never_compound_part_sc(anw).
never_compound_part_sc(app).
never_compound_part_sc(are).
never_compound_part_sc(aren).
never_compound_part_sc('a\'s').
never_compound_part_sc(au).
never_compound_part_sc(ava).
never_compound_part_sc(ba).
never_compound_part_sc(ben).
never_compound_part_sc(bet).
never_compound_part_sc(bo).
never_compound_part_sc(bok).  % wel bokken- en boks-
never_compound_part_sc(bra).
never_compound_part_sc(ca).
never_compound_part_sc(cao).
never_compound_part_sc(cl).
never_compound_part_sc('d\'s').
never_compound_part_sc(da).
never_compound_part_sc(dan).
never_compound_part_sc(de).
never_compound_part_sc(den).
never_compound_part_sc(der).
never_compound_part_sc(des).
never_compound_part_sc(di).
never_compound_part_sc(dia).
never_compound_part_sc(die).
never_compound_part_sc(do).
never_compound_part_sc(dom).
never_compound_part_sc(don).
never_compound_part_sc(dos).
never_compound_part_sc(dra).
never_compound_part_sc(eb).
never_compound_part_sc(eer).
never_compound_part_sc(ei).
never_compound_part_sc(ek).
never_compound_part_sc(el).
never_compound_part_sc(en).
never_compound_part_sc(end).
never_compound_part_sc(ene).
never_compound_part_sc(enen).
never_compound_part_sc(ende).
never_compound_part_sc(ent).
never_compound_part_sc(er).
never_compound_part_sc(es).
never_compound_part_sc(eu).
never_compound_part_sc(eur).
never_compound_part_sc('e\'s').
never_compound_part_sc(fa).
never_compound_part_sc(ga).
never_compound_part_sc(ge).
never_compound_part_sc(gen).
never_compound_part_sc(gr).
never_compound_part_sc(ha).
never_compound_part_sc(heden).
never_compound_part_sc(ho).
never_compound_part_sc(hu).
never_compound_part_sc('h\'s').
never_compound_part_sc(ie).
never_compound_part_sc(iel).
never_compound_part_sc(iet).
never_compound_part_sc(ij).
never_compound_part_sc(ijst).
never_compound_part_sc(int).
never_compound_part_sc(is).
never_compound_part_sc('i\'s').
never_compound_part_sc(ja).
never_compound_part_sc('j\'s').
never_compound_part_sc('k\'s').
never_compound_part_sc(kb).
never_compound_part_sc(ken).
never_compound_part_sc(kilom).
never_compound_part_sc(kon).
never_compound_part_sc(la).
never_compound_part_sc(lag).
never_compound_part_sc(lei).
never_compound_part_sc(let).
never_compound_part_sc(leu).
never_compound_part_sc(li).
never_compound_part_sc(lp).
never_compound_part_sc(ma).
never_compound_part_sc('ma\'s').
never_compound_part_sc(me).
never_compound_part_sc(men).
never_compound_part_sc(ment).
never_compound_part_sc(mi).
never_compound_part_sc(min).
never_compound_part_sc(ne).
never_compound_part_sc(nee).
never_compound_part_sc(nen).
never_compound_part_sc(nk).
never_compound_part_sc(nm).
never_compound_part_sc(ngo).
never_compound_part_sc(nob).
never_compound_part_sc(ns).
never_compound_part_sc(nv).
never_compound_part_sc('n\'s').
never_compound_part_sc(och).
never_compound_part_sc(oe).
never_compound_part_sc(of).
never_compound_part_sc(ok).
never_compound_part_sc(oo).
never_compound_part_sc(or).
never_compound_part_sc(ov).
never_compound_part_sc('o\'s').
never_compound_part_sc('p\'s').
never_compound_part_sc(par).
never_compound_part_sc(pa).
never_compound_part_sc(pg).
never_compound_part_sc(pi).
never_compound_part_sc(po).
never_compound_part_sc(pk).
never_compound_part_sc(pr).
never_compound_part_sc('r\'s').
never_compound_part_sc(ra).
never_compound_part_sc(re).
never_compound_part_sc(rk).
never_compound_part_sc(ro).
never_compound_part_sc(sch).
never_compound_part_sc(sec).
never_compound_part_sc(sen).
never_compound_part_sc(sens).
never_compound_part_sc(si).
never_compound_part_sc(sic).
never_compound_part_sc(slo).
never_compound_part_sc(so).
never_compound_part_sc(sol).
never_compound_part_sc(sp).
never_compound_part_sc(st).
never_compound_part_sc(sten).
never_compound_part_sc('s\'s').
never_compound_part_sc(te).
never_compound_part_sc(teek).
never_compound_part_sc(té).
never_compound_part_sc(ter).
never_compound_part_sc(teu).
never_compound_part_sc('t\'s').
never_compound_part_sc(ti).
never_compound_part_sc(tuk).
never_compound_part_sc(tv).
never_compound_part_sc(ui).
never_compound_part_sc('u\'s').
never_compound_part_sc(va).
never_compound_part_sc(ver).
never_compound_part_sc(vi).
never_compound_part_sc(vo).
never_compound_part_sc(vr).
never_compound_part_sc(wij).
never_compound_part_sc(wo).
never_compound_part_sc('y\'s').
never_compound_part_sc(za).
never_compound_part_sc(ze).
never_compound_part_sc(zo).
never_compound_part_sc('\'s').

never_compound_part_sub('--').
never_compound_part_sub('==').
never_compound_part_sub('-=').
never_compound_part_sub('://').

remove_longer_compounds_with_same_suffix([],[]).
remove_longer_compounds_with_same_suffix([I-Stem-Parts|R0],[I-Stem-Parts|R]) :-
    remove_longer_compounds_with_same_suffix_(R0,Stem,R1),
    remove_longer_compounds_with_same_suffix(R1,R).

remove_longer_compounds_with_same_suffix_([],_,[]).
remove_longer_compounds_with_same_suffix_([I-R-Parts|T],R0,List) :-
    (	R0 == R
    ->	List = T0
    ;	List = [I-R-Parts|T0]
    ),
    remove_longer_compounds_with_same_suffix_(T,R0,T0).

%% TODO: add proper stems!!
guess_form_of_suffix(W,Root,Suffix,Tag,CompTag) :-
    guess_using_suffix0(W,_,Suffix,2,1),  % generate in decreasing length
    findall(Suffix-RootSuffix-Tag-CompTag,apply_suffix_rule(Suffix,RootSuffix,Tag,CompTag,W),[H|T]),
    !,
    member(Suffix-RootSuffix-Tag-CompTag,[H|T]),
    construct_root(Tag,Suffix,RootSuffix,W,Root).
%    atom_concat(Pref0,Suffix,W),
%    remove_ge_if_psp(Tag,Pref0,Pref),
%    atom_concat(Pref,RootSuffix,Root0),
%    decap(Root0,Root).

guess_form_of_suffix(W,Stem,ge_dt,verb('hebben/zijn',psp,intransitive),verb(_,psp,intransitive)) :-
    atom_concat(ge,Rest,W),
    psp_suffix(Suffix,StemSuffix),
    atom_concat(_,Suffix,Rest),
    atom_concat(Stem,StemSuffix,Rest),
    \+ member(W,[gerard]).

guess_form_of_suffix(W,Stem,ge_dt,verb('hebben/zijn',psp,transitive),verb(_,psp,transitive)) :-
    atom_concat(ge,Rest,W),
    psp_suffix(Suffix,StemSuffix),
    atom_concat(_,Suffix,Rest),
    atom_concat(Stem,StemSuffix,Rest),
    \+ member(W,[gerard]).

guess_form_of_suffix(W,Stem,ge_dt,verb('hebben/zijn',psp,intransitive),verb(_,psp,ninv(intransitive,part_intransitive(Af)))) :-
    atom_concat(Afge,Rest,W),
    atom_concat(Af,ge,Afge),
    alpino_lex:xl(Af,particle(_),_,[],[]),
    psp_suffix(Suffix,StemSuffix),
    atom_concat(_,Suffix,Rest),
    atom_concat(Stem0,StemSuffix,Rest),
    \+ member(W,[gerard]),
    atom_concat(Stem0,'_',Stem1),
    atom_concat(Stem1,Af,Stem).

guess_form_of_suffix(W,Stem,ge_dt,verb('hebben/zijn',psp,transitive),verb(_,psp,ninv(transitive,part_transitive(Af)))) :-
    atom_concat(Afge,Rest,W),
    atom_concat(Af,ge,Afge),
    alpino_lex:xl(Af,particle(_),_,[],[]),
    psp_suffix(Suffix,StemSuffix),
    atom_concat(_,Suffix,Rest),
    atom_concat(Stem0,StemSuffix,Rest),
    \+ member(W,[gerard]),
    atom_concat(Stem0,'_',Stem1),
    atom_concat(Stem1,Af,Stem).

psp_suffix(cht,t).
psp_suffix(ft,t).
psp_suffix(gd,d).
psp_suffix(kt,t).
psp_suffix(ld,d).
psp_suffix(md,d).
psp_suffix(nd,d).
psp_suffix(pt,t).
psp_suffix(rd,d).
psp_suffix(st,t).

no_exceptional_suffix_rule(List,Root) :-
    member(Suffix,List),
    atom_concat(_,Suffix,Root),
    !,
    fail.
no_exceptional_suffix_rule(capital,Root):-
    starts_with_capital(Root),
    !,
    fail.
no_exceptional_suffix_rule(_,_).

apply_suffix_rule(Suffix,RootSuffix,Tag,CompTag,Word) :-
    form_of_suffix_rule(Suffix,RootSuffix,Tag,CompTag,Exceptions),
    no_exceptional_suffix_rule(Exceptions,Word).

form_of_suffix_rule(A,B,C,C,D) :-
    form_of_suffix_rule(A,B,C,D).
form_of_suffix_rule('\'s','',noun(both,count,pl),noun(_,count,pl),[]).

% here, Tag and CompTag (competing tag that must not exist) are same
form_of_suffix_rule(erwijs,erwijs,  adverb,[onderwijs]).
form_of_suffix_rule(erwijze,erwijze, adverb,[]).
form_of_suffix_rule(shalve,shalve,  adverb,[]).

form_of_suffix_rule(aals,aals,adjective(no_e(adv)),[]).
form_of_suffix_rule(aans,aans,adjective(no_e(adv)),[]).
form_of_suffix_rule(aal,aal,adjective(no_e(adv)),[éénmaal,
						  gemaal,
						  journaal,
						  kapitaal,
						  kanaal,
						  materiaal,
						  schandaal,
						  staal,
						  tribunaal,
						  verhaal]).
form_of_suffix_rule(air,air,adjective(no_e(adv)),[]).
form_of_suffix_rule(eerd,eerd,adjective(ge_no_e(adv)),[]).
form_of_suffix_rule(ees,ees,adjective(no_e(adv)),[abonnees,
						  trainees,
						  vlees,
						  vrees]).
form_of_suffix_rule(ees,ees,post_adjective(no_e),[abonnes,
						  trainees,
						  vlees,
						  vrees]).
form_of_suffix_rule(end,end,adjective(end(both)),[]).
form_of_suffix_rule(esk,esk,adjective(no_e(adv)),[desk]).
form_of_suffix_rule(eus,eus,adjective(no_e(adv)),[keus,
						  reus]).
form_of_suffix_rule(ieel,ieel,adjective(no_e(adv)),[]).
form_of_suffix_rule(iële,ieel,adjective(e),[]).
form_of_suffix_rule(ief,ief,adjective(no_e(adv)),[brief,
						  chief, % archief
						  dief,
						  gerief,
						  motief,
						  perspectief,
						  tarief]).
form_of_suffix_rule(iek,iek,adjective(no_e(adv)),[atletiek,
						  fabriek,
						  kliniek,
						  linguïstiek,
						  mozaiek,
						  muziek,
						  piek,
						  republiek,
						  rubriek,
						  tactiek,
						  techniek]).
form_of_suffix_rule(ig,ig,adjective(no_e(adv)),['Dantzig',
						dértig,
						tuig]).
form_of_suffix_rule(isch,isch,adjective(no_e(adv)),[]).
form_of_suffix_rule(ïsch,ïsch,adjective(no_e(adv)),[]).
form_of_suffix_rule(ischt,ischt,adjective(no_e(adv)),[]).
form_of_suffix_rule(ïscht,ïscht,adjective(no_e(adv)),[]).
form_of_suffix_rule(ïscht,ïscht,adjective(no_e(adv)),[]).
form_of_suffix_rule(lijk,lijk,adjective(no_e(adv)),[babylijk]).
form_of_suffix_rule(loos,loos,adjective(no_e(adv)),[]).
form_of_suffix_rule(loos,loos,adjective(no_e(adv)),[]).
form_of_suffix_rule(baar,baar,adjective(no_e(adv)),[]).
form_of_suffix_rule(oir,oir,adjective(no_e(adv)),[reservoir]).
form_of_suffix_rule(gewijs,gewijs,adjective(no_e(adv)),[]).
form_of_suffix_rule(loos,loos,adjective(no_e(padv)),[]).
form_of_suffix_rule(zaam,zaam,adjective(no_e(adv)),[]).


form_of_suffix_rule(tigste,  tig,     number(rang),[vergeetachtigste]).

form_of_suffix_rule(aalse,   aals,    adjective(e),[]).
form_of_suffix_rule(aanse,   aans,    adjective(e),[]).
form_of_suffix_rule(aire,    air,     adjective(e),[]).
form_of_suffix_rule(ale,     aal,     adjective(e),[centrale,
						    finale,
						    whale]).
form_of_suffix_rule(eerde,   eerd,    adjective(e),[]).
form_of_suffix_rule(ende,    end,     adjective(ende(padv)),['Balkenende',
							     bende,
							     oostende]).
form_of_suffix_rule(eske,    esk,     adjective(e),[burleske]).
form_of_suffix_rule(euze,    eus,     adjective(e),[]).
form_of_suffix_rule(ieke,    iek,     adjective(e),[]).
form_of_suffix_rule(ieve,    ief,     adjective(e),[]).
form_of_suffix_rule(ige,     ig,      adjective(e),[]).
form_of_suffix_rule(ische,   isch,    adjective(e),[]).
form_of_suffix_rule(ïsche,   ïsch,    adjective(e),[]).
form_of_suffix_rule(ischte,  isch,    adjective(e),[]).
form_of_suffix_rule(ïschte,  ïsch,    adjective(e),[]).
form_of_suffix_rule(ke,      k,       adjective(e),[]).
form_of_suffix_rule(le,      l,       adjective(e),['Cercle',
						    cercle,
						    djingle,
						    shuttle,
						    jingle]).
form_of_suffix_rule(ste,     st,      adjective(e),[extremiste]).
form_of_suffix_rule(bare,    'baar',  adjective(e),[]).
form_of_suffix_rule(ere,     er,      adjective(e),[àndere,
						    ándere]).
form_of_suffix_rule(ese,     ees,     adjective(e),[]).
form_of_suffix_rule(ïde,     ïde,     adjective(both(adv)),[]).
form_of_suffix_rule(oire,    oir,     adjective(e),[]).
form_of_suffix_rule(gewijze, gewijs,  adjective(e),[]).
form_of_suffix_rule(loze,    loos,    adjective(e),[]).
form_of_suffix_rule(zame,    zaam,    adjective(e),[]).

form_of_suffix_rule(iger,    ig,      adjective(er(adv)),[verdediger,
							  reiziger,
							  vertegenwoordiger
							 ]).
form_of_suffix_rule(ischer,  isch,    adjective(er(adv)),[]).
form_of_suffix_rule(ïscher,  ïsch,    adjective(er(adv)),[]).
form_of_suffix_rule(lijker,  lijk,    adjective(er(adv)),[]).
form_of_suffix_rule(lozer,   loos,    adjective(er(adv)),[]).
form_of_suffix_rule(ender,   end,     adjective(er(adv)),[]).
form_of_suffix_rule(baarder, baar,    adjective(er(adv)),[]).


form_of_suffix_rule(aals,   aal,     post_adjective(no_e),[]).
form_of_suffix_rule(aans,   aan,     post_adjective(no_e),[]).
form_of_suffix_rule(airs,   air,     post_adjective(no_e),[]).
form_of_suffix_rule(eerds,  eerd,    post_adjective(no_e),[]).
form_of_suffix_rule(ends,   end,     post_adjective(no_e),[]).
form_of_suffix_rule(esks,   esk,     post_adjective(no_e),[]).
form_of_suffix_rule(ieels,  ieel,    post_adjective(no_e),[]).
form_of_suffix_rule(iefs,   ief,     post_adjective(no_e),[]).
form_of_suffix_rule(ieks,   iek,     post_adjective(no_e),[]).
form_of_suffix_rule(igs,    ig,      post_adjective(no_e),[]).
form_of_suffix_rule(igers,  ig,      post_adjective(er),[]).
form_of_suffix_rule(isch,   isch,    post_adjective(no_e),[]).
form_of_suffix_rule(ïsch,   ïsch,    post_adjective(no_e),[]).
form_of_suffix_rule(ischers,isch,    post_adjective(er),[]).
form_of_suffix_rule(ïschers,ïsch,    post_adjective(er),[]).
form_of_suffix_rule(lijks,  lijk,    post_adjective(no_e),[]).
form_of_suffix_rule(lijkers,lijk,    post_adjective(er),[]).
form_of_suffix_rule(loos,   loos,    post_adjective(no_e),[]).
form_of_suffix_rule(lozers, loos,    post_adjective(er),[]).
form_of_suffix_rule(baars,  baar,    post_adjective(no_e),[]).
form_of_suffix_rule(ders,   der,     post_adjective(no_e),[]).
form_of_suffix_rule(oirs,   oir,     post_adjective(no_e),[]).

form_of_suffix_rule('-er','-er',     noun(de,count,sg),[]).
form_of_suffix_rule('-ers','-er',   noun(de,count,pl),[]).
form_of_suffix_rule('\'er','\'er',   noun(de,count,sg),[]).
form_of_suffix_rule('\'ers','\'er',  noun(de,count,pl),[]).
form_of_suffix_rule('iër','iër',    noun(de,count,sg),[]).
form_of_suffix_rule('iërs','iër',   noun(de,count,pl),[]).
form_of_suffix_rule('lander','lander',    noun(de,count,sg),[]).
form_of_suffix_rule('landers','lander',   noun(de,count,pl),[]).
form_of_suffix_rule('ees','ees',    noun(de,count,sg),[]).
form_of_suffix_rule('ezen','ees',   noun(de,count,pl),[]).
%%% form_of_suffix_rule('aan','aan',    noun(de,count,sg),[]).
form_of_suffix_rule('anen','aan',   noun(de,count,pl),[]).
form_of_suffix_rule('aar','aar',    noun(de,count,sg),[]).
form_of_suffix_rule('aars','aar',   noun(de,count,pl),[]).

form_of_suffix_rule('iteit','iteit',  noun(de,count,sg),[]).
form_of_suffix_rule('iteiten','iteit',noun(de,count,pl),[]).

form_of_suffix_rule('ingen','ing',noun(de,count,pl),[]).

form_of_suffix_rule(eert,eer/eren,verb(hebben,sg3,intransitive),[]).
form_of_suffix_rule(eert,eer/eren,verb(hebben,sg3,transitive),[]).
form_of_suffix_rule(eren,eer/eren,verb(hebben,pl,intransitive),[jongeren,
							   kinderen]).
form_of_suffix_rule(eren,eer/eren,verb(hebben,pl,transitive),[jongeren,
							   kinderen]).
form_of_suffix_rule(eren,eer/eren,verb(hebben,inf,intransitive),[jongeren,
							   kinderen]).
form_of_suffix_rule(eren,eer/eren,verb(hebben,inf,transitive),[jongeren,
							   kinderen]).
form_of_suffix_rule(ëren,eer/ëren,verb(hebben,pl,intransitive),[]).
form_of_suffix_rule(ëren,eer/ëren,verb(hebben,pl,transitive),[]).
form_of_suffix_rule(ëren,eer/ëren,verb(hebben,inf,intransitive),[]).
form_of_suffix_rule(ëren,eer/ëren,verb(hebben,inf,transitive),[]).
form_of_suffix_rule(eerd,eer/eren,verb(hebben,psp,intransitive),[]).  % wrong root
form_of_suffix_rule(eerd,eer/eren,verb(hebben,psp,transitive),[]).    % wrong root
form_of_suffix_rule(eerde,eer/eren,verb(hebben,past(sg),intransitive),[]).
form_of_suffix_rule(eerde,eer/eren,verb(hebben,past(sg),transitive),[]).
form_of_suffix_rule(eerden,eer/eren,verb(hebben,past(pl),intransitive),[]).
form_of_suffix_rule(eerden,eer/eren,verb(hebben,past(pl),transitive),[]).

form_of_suffix_rule(achtigen,achtig,nominalized_adjective,[]).

form_of_suffix_rule('\'s','',determiner(pron),capital).


guess_using_suffix0(Word,Prefix,Suffix,MinLengthPrefix,MinLengthSuffix) :-
    \+ guess_using_suffix_exception(Word),
    atom(Word),
    between(MinLengthSuffix,8,SuffixLength,'-'),
    sub_atom(Word,_,SuffixLength,0,Suffix),
    atom_concat(Prefix,Suffix,Word),
    atom_length(Prefix,A), A>MinLengthPrefix.

guess_using_suffix_exception(Word) :-
    atom(Word),
    atom_concat(_,mogelijkheden,Word).

strip_accents(Word0,Word) :-
    atom(Word0),
    atom_codes(Word0,C0),
    deaccent_chars(C0,C,0,1),
    atom_codes(Word,C).

strip_repeated_letters(Word0,Word) :-
    atom(Word0),
    atom_codes(Word0,C0),
    length(C0,L), L>2,
    strip_repeated_letters(C0,C,0,1),
    atom_codes(Word,C).

y_ij_letters(Word0,Word) :-
    atom(Word0),
    atom_codes(Word0,C0),
    y_ij_letters(C0,C,0,1),
    atom_codes(Word,C).

ij_ei_letters(Word0,Word) :-
    atom(Word0),
    atom_codes(Word0,C0),
    ij_ei_letters(C0,C,0,1),
    atom_codes(Word,C).

ei_ij_letters(Word0,Word) :-
    atom(Word0),
    atom_codes(Word0,C0),
    ei_ij_letters(C0,C,0,1),
    atom_codes(Word,C).

g_ch_letters(Word0,Word) :-
    atom(Word0),
    atom_codes(Word0,C0),
    g_ch_letters(C0,C,0,1),
    atom_codes(Word,C).

iseer_letters(Word0,Word) :-
    atom(Word0),
    iseer(End0,End),
    atom_concat(Pre,End0,Word0),
    atom_concat(Pre,End,Word).

iseer(izeer,iseer).
iseer(izeert,iseert).
iseer(izeerd,iseerd).
iseer(izeren,iseren).

%% replace any isupper with islower variant
decap(Word0,Word) :-
    atom(Word0),
    atom_codes(Word0,Chars0),
    decap_chars(Chars0,Chars),
    atom_codes(Word,Chars).

%% replace first letter islower with isupper variant
%% must start with islower
cap_first(Word0,Word) :-
    atom(Word0),
    atom_codes(Word0,[C0|Codes]),
    islower(C0),
    toupper(C0,C),
    atom_codes(Word,[C|Codes]).

%% gpv => GPV
cap_all(Word0,Word) :-
    atom(Word0),
    atom_codes(Word0,[C0,C1,C2|Codes0]),
    cap_chars([C0,C1,C2|Codes0],Codes),
    atom_codes(Word,Codes).

decap_chars([],[]).
decap_chars([C0|Cs0],[C|Cs]):-
    tolower(C0,C),
    decap_chars(Cs0,Cs).

cap_chars([],[]).
cap_chars([C0|Cs0],[C|Cs]):-
    toupper(C0,C),
    cap_chars(Cs0,Cs).

%% replace all isupper with islower variant
%% must at least contain 1 isupper
%% first char need not be replaced
%% (FRANSE -> Franse)
decap_some(Word0,Word) :-
    atom(Word0),
    atom_codes(Word0,Chars0),
    decap_most_chars(Chars0,Chars),
    atom_codes(Word,Chars).

decap_some(Word0,Word) :-
    atom(Word0),
    atom_codes(Word0,[48|T]),
    atom_codes(Word1,[79|T]),
    decap_some(Word1,Word).

only_capitals_prefix([H0|T0],[H|T],P0,P,N0,N) :-
    only_capitals(H0,H),
    P1 is P0 + 1,
    N0 < N,
    N1 is N0+1,
    only_capitals_prefix(T0,T,P1,P,N1,N).
only_capitals_prefix(Ws,Ws,P,P,_,_).

only_capitals(Word0,Word) :-
    atom(Word0),
    atom_codes(Word0,Chars0),
    decap_all_chars(Chars0,Chars),
    atom_codes(Word,Chars).

%% leave first capital for names
%% SAO PAULO
only_capitals_but_one(Word0,Word) :-
    atom(Word0),
    atom_codes(Word0,[C|Chars0]),
    isupper(C),
    decap_all_chars(Chars0,Chars),
    atom_codes(Word,[C|Chars]).

only_capitals_but_one_prefix([H0|T0],[H|T],P0,P,N0,N) :-
    only_capitals_but_one(H0,H),
    P1 is P0 + 1,
    N0 < N,
    N1 is N0 + 1,
    only_capitals_prefix(T0,T,P1,P,N1,N).
only_capitals_but_one_prefix(Ws,Ws,P,P,_,_).

decap_all_chars([],[]).
decap_all_chars([C0|Cs0],[C|Cs]) :-
    (   isupper(C0)
    ->  tolower(C0,C)
    ;   C0=45  % "-"
    ->  C =45
    ),
    decap_all_chars(Cs0,Cs).

decap_most_chars([],[]).
decap_most_chars([C0|Cs0],[C|Cs]) :-
    (   isupper(C0),
        tolower(C0,C),
        decap_most_chars(Cs0,Cs,1,1)
    ;   C0 = C,  % don't check if isupper(C0) for FRANSE -> Franse
        decap_most_chars(Cs0,Cs,0,1)
    ).

decap_most_chars([],[],L,L).
decap_most_chars([C0|Cs0],[C|Cs],L0,L) :-
    (   isupper(C0)
    ->  tolower(C0,C),
        L1=1
    ;   C0 = C,
        L0=L1
    ),
    decap_most_chars(Cs0,Cs,L1,L).        

start_of_sentence(P,Start) :-
    (	is_start_sentence(P)
    ->	Start=begin
    ;	Start=not_begin
    ).

is_end_sentence(P) :-
    nonvar(P),
    is_end_sentence_(P),
    !.

is_end_sentence_(P) :-
    \+ tag(P,_,_,_,_,_,_,_).
is_end_sentence_(P0) :-
    tag(P0,P,_,_,_,_,_,punct(_)),
    is_end_sentence_(P).

is_start_sentence(P) :-
    nonvar(P),
    is_start_sentence_(P),
    !.

start_sentence_punct(dubb_punt).
start_sentence_punct(hellip).
start_sentence_punct(vraag).
start_sentence_punct(ligg_streep).
start_sentence_punct(aanhaal_both).
start_sentence_punct(aanhaal_links).

is_start_sentence_(0).		% begin of sentence: at position 0
is_start_sentence_(P) :-
    \+ (  tag(P1,_,_,_,_,_,_,_),	% for things like * ·
	   P1 < P
       ).
is_start_sentence_(P):-
    tag(_,P,_,_,_,_,_,punct(Dubb)),
    start_sentence_punct(Dubb).

is_start_sentence_(P):-
    tag(_,P,_,_,_,_,normal(enumeration),tag).
is_start_sentence_(P):-
    tag(P0,P,_,_,_,_,_,punct(Punct)), % or only preceded by punctuation
    is_start_sentence__(P0,Punct).
is_start_sentence_(P):-
    alpino_lexical_analysis:open_bracket(P0,P,_),
    is_start_sentence_(P0).
is_start_sentence_(P):-
    tag(P0,P,_,_,_,Quoted,_,_),
    starts_with_quote(Quoted),
    is_start_sentence_(P0).

is_start_sentence__(0,_).
is_start_sentence__(P,Punct) :-
    tag(_,P,_,_,_,_,_,punct(Punct)).

%% only punctuation remaining
end_sentence([]).
end_sentence([H|T]) :-
    alpino_lex:xl(H,punct(_),_,[],[]),
    end_sentence(T).

noun_tag(noun(_,_,_)).
noun_tag(tmp_noun(_,_,_)).
noun_tag(mod_noun(_,_,_)).
noun_tag(meas_mod_noun(_,_,_)).

noun_tag_plus(noun(_,_,_)).
noun_tag_plus(tmp_noun(_,_,_)).
noun_tag_plus(mod_noun(_,_,_)).
noun_tag_plus(meas_mod_noun(_,_,_)).
noun_tag_plus(noun(_,_,_,_)).
noun_tag_plus(tmp_noun(_,_,_,_)).
noun_tag_plus(mod_noun(_,_,_,_)).
noun_tag_plus(meas_mod_noun(_,_,_,_)).

open_class_tag(noun(A,B,meas),noun(A,B,sg)).
open_class_tag(tmp_noun(A,B,meas),tmp_noun(A,B,sg)).
open_class_tag(mod_noun(A,B,meas),mod_noun(A,B,sg)).
open_class_tag(meas_mod_noun(A,B,meas),meas_mod_noun(A,B,sg)).
open_class_tag(noun(A,B,meas,measure),noun(A,B,sg,measure)).
open_class_tag(noun(A,B,meas,app_measure),noun(A,B,sg,app_measure)).
open_class_tag(verb(A,B,Sc0),verb(A,B,Sc)) :-
    Sc0 = [_|_],!,
    non_part_sc(Sc0,Sc).
open_class_tag(verb(A,B,Sc0),verb(A,B,Sc)) :-
    non_part_sc_el(Sc0,[Sc],[]).
open_class_tag(v_noun(Sc0),v_noun(Sc)) :-
    non_part_sc_el(Sc0,[Sc],[]).

open_class_tag(Tag,Tag) :-
    open_class_tag(Tag).

open_class_tag(noun(_,_,_)).
open_class_tag(mod_noun(_,_,_)).
open_class_tag(meas_mod_noun(_,_,_)).
open_class_tag(tmp_noun(_,_,_)).
open_class_tag(noun(_,_,_,measure)).
open_class_tag(noun(_,_,_,app_measure)).

open_class_tag(number(hoofd(pl_num))).
open_class_tag(adverb).
open_class_tag(dir_adverb).
open_class_tag(tmp_adverb).
open_class_tag(loc_adverb).
open_class_tag(adjective(X)) :- \+ X = prefix, \+ X = meer.
open_class_tag(adjective(_,_)).
open_class_tag(nominalized_adjective).

non_part_sc([],[]).
non_part_sc([H0|T0],T1) :-
    non_part_sc_el(H0,T1,T),
    non_part_sc(T0,T).

non_part_sc_el(Term,T0,T) :-
    functor(Term,Functor,_),
    (   atom_concat(part_,_,Functor)
    ->  T0 = T
    ;   illegal_compound_part_sc(Term)
    ->  T0 = T
    ;   T0 = [Term|T]
    ).    

illegal_compound_part_sc(fixed(_,_)).
illegal_compound_part_sc(subj_control(_)).
illegal_compound_part_sc(ap_copula).
illegal_compound_part_sc(ap_pred_np).
illegal_compound_part_sc(copula).
illegal_compound_part_sc(El) :-
    alpino_lex:impossible_nominalization_frame(El).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% unknown word heuristics %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexical_analysis_prefixed_verb(Ws,P0,R0,Part,Name,Tag):-
    alpino_lex:lexicon(ConjTag,_,Ws,Ws0,_),
    (   ConjTag = conj(en)
    ;   ConjTag = right_conj(_)
    ),
    Tag0 = verb(C,D,ninv(TagA,TagB0)),
    alpino_lex:lexicon(Tag0,VStem,Ws0,_,_),
    Tag  = verb(C,D,ninv(TagA,TagB)),
    TagB0 =.. [Fun,Part0|Tail],
    TagB =.. [Fun,Part|Tail],
    P is P0+1,
    R is R0+1,
    replace_part(VStem,Stem,Part0,Part),
    assert_tag(P0,P,R0,R,Stem,Name,Tag).

alternative(Input,P0,P,R0,R,Name,Tag,len(Length)):-
    alpino_lex:lexicon(Tag,Stem,Input,Input1,_),
    append([_Stem0|UsedInput],Input1,Input),
    length([_|UsedInput],Length),
    P is P0+Length,
    R is R0+Length,
    assert_tag(P0,P,R0,R,Stem,Name,Tag).

spaced_alternative(Input,P0,P,R0,R,Name,Tag,len(Length)):-
    concat_all(Input,Word,''),
    alpino_lex:lexicon(Tag,Stem,[Word],[],_),
    length(Input,Length),
    P is P0+Length,
    R is R0+Length,
    assert_tag(P0,P,R0,R,Stem,Name,Tag).

lexical_analysis_add_space(Input,P0,R0,Name,Tag,len(Consumed)):-
    alpino_lex:lexicon(Tag0,Stem,Input,Input1,normal),
    append([_Stem0|UsedInput],Input1,Input),
    length([_|UsedInput],Length),
    Consumed is Length+1,
    P is P0+Consumed,
    R is R0+Consumed,
    (   Tag0 = with_dt(Fr,Ds0)
    ->  adapt_with_dt(Ds0,Ds),
	Tag = with_dt(Fr,Ds)
    ;   Tag0 = Tag
    ),
    \+ tag(P0,P,R0,R,_,_,english_compound(normal),_),
    assert_tag(P0,P,R0,R,Stem,Name,Tag).

assert_tags([],_P0,_R0,_W,_Wmin).
assert_tags([Stem/Tag|Tags],P0,R0,W,Wmin) :-
    P is P0 + 1,
    R is R0 + 1,
    assert_tag(P0,P,R0,R,Stem,suffix,Tag),
    assert_tags(Tags,P0,R0,W,Wmin).

alternative_open_class_suffix(Word,Prefix,NewStem,Tag):-
    alpino_lex:lexicon(Tag0,Stem,[Word],[],_),
    open_class_tag(Tag0,Tag),
    \+ Tag = noun(_,_,_), % this one is generated anyway using "noun" heuristic
    build_stem(Stem,Prefix,NewStem).

build_stem(STEM0,Prefix,STEM) :-
    (   STEM0 = v_root(Stem0,Lemma0)
    ->  STEM = v_root(Stem,Lemma),
	atom_concat(Prefix,Stem0,Stem),
	atom_concat(Prefix,Lemma0,Lemma)
    ;   atom_concat(Prefix,STEM0,STEM)
    ).

%% for compounds, are asserted in increasing length. Don't
%% assert longer compounds with same tag.

alternative_open_class(Integer,Word,Parts,P0,R0,Name,Tag):-
    integer(Integer),
    alpino_lex:lexicon(Tag0,Stem,[Word],[],_),
    open_class_tag(Tag0,Tag1),
    allow_verb_only_if_particle(Tag1,Tag,Parts,Stem,NewStem),
    P is P0+1,
    R is R0+1,
    \+ shorter_tag(P0,P,R0,R,Tag,Name),
    check_de_het(P0,Tag),
    assert_tag(P0,P,R0,R,NewStem,Name,Tag).

%% vooroverboog = voorover + boog
%%                dir_adverb + ld_adv
alternative_open_class(2,Boog,[Voorover],P0,R0,Name,Tag) :-
    alpino_lex:lexicon(verb(HZ,Fin,Sc0),v_root(Root,Lemma),[Boog],[],_),
    alpino_lex:lexicon(dir_adverb,VooroverL,[Voorover],[],_),
    ld_dir(Sc0,Sc1,Sc2,Voorover),
    P is P0 + 1,
    R is R0 + 1,
    concat_all([Root,VooroverL],RootL,'_'),
    concat_all([Voorover,Lemma],LemmaL,'_'),
    Tag = verb(HZ,Fin,ninv(Sc1,Sc2)),
    assert_tag(P0,P,R0,R,v_root(RootL,LemmaL),dir_v(Name),Tag).

alternative_open_class(slash,Word,Parts,P0,R0,Name,Tag):-
    alpino_lex:lexicon(Tag,Stem,[Word],[],_),
    lists:append(Parts,[Stem],PartsStem),
    alpino_lex:concat_stems(PartsStem,NewStem),
    P is P0+1,
    R is R0+1,
    assert_tag(P0,P,R0,R,NewStem,Name,Tag).

alternative_open_class(hyphen,Word,Parts,P0,R0,Name,Tag):-
    alpino_lex:lexicon(Tag0,Stem,[Word],[],_),
    open_class_tag(Tag0,Tag),
    lists:append(Parts,[Stem],PartsStem),
    alpino_lex:concat_stems(PartsStem,NewStem),
    check_de_het(P0,Tag),
    P is P0+1,
    R is R0+1,
    assert_tag(P0,P,R0,R,NewStem,Name,Tag).

shorter_tag(P0,P,R0,R,Tag,compound(Int)) :-
    tag(P0,P,R0,R,_,_,compound(Name),Tag),
    integer(Name),
    Name < Int.

check_de_het(P0,Tag) :-
    Tag=noun(het,_,sg),
    !,
    \+ search_tag_stem(de,tag(_,P0,_,_,de,_,_,determiner(de))).
check_de_het(P0,Tag) :-
    Tag=noun(het,_,sg,_),
    !,
    \+ search_tag_stem(de,tag(_,P0,_,_,de,_,_,determiner(de))).
check_de_het(P0,Tag) :-
    Tag=tmp_noun(het,_,sg),
    !,
    \+ search_tag_stem(de,tag(_,P0,_,_,de,_,_,determiner(de))).
check_de_het(P0,Tag) :-
    Tag=mod_noun(het,_,sg),
    !,
    \+ search_tag_stem(de,tag(_,P0,_,_,de,_,_,determiner(de))).
check_de_het(P0,Tag) :-
    Tag=meas_mod_noun(het,_,sg),
    !,
    \+ search_tag_stem(de,tag(_,P0,_,_,de,_,_,determiner(de))).
check_de_het(P0,Tag) :-
    Tag=v_noun(_),
    !,
    \+ search_tag_stem(de,tag(_,P0,_,_,de,_,_,determiner(de))).
check_de_het(_,_).

%% as previous, but no compound should already be asserted
alternative_open_class_decap_compound(Word,Parts,P0,R0,Name,Tag):-
    alpino_lex:lexicon(Tag0,Stem,[Word],[],_),
    open_class_tag(Tag0,Tag1),
    allow_verb_only_if_particle(Tag1,Tag,Parts,Stem,NewStem),
    P is P0+1,
    R is R0+1,
    \+ tag(P0,P,R0,R,_,_,_,Tag),
    check_de_het(P0,Tag),
    assert_tag(P0,P,R0,R,NewStem,Name,Tag).

alternative_proper_name(Stem0,Word,P0,R0,Name,Tag):-
    \+ lists:member(Word,['Noord','Zuid','West','Oost']),
    alpino_lex:lexicon(Tag,Stem1,[Word],[],_),
    proper_name_tag(Tag),
    P is P0+1,
    R is R0+1,
    alpino_lex:concat_stems([Stem0,Stem1],Stem),
    assert_tag(P0,P,R0,R,Stem,Name,Tag).

%% as suggested by RJ Baars r.j.baars@xs4all.nl
allow_verb_only_if_particle(adjective(no_e(adv)),_,_,net,_) :-
    !,
    fail.
allow_verb_only_if_particle(verb(HZ,VF,SC),
			    verb(HZ,VF,ninv(SC,SC2)),
			    Parts,Stem,NewStem) :-
    !,
    Parts = [Part],
    atomic(Part),
    alpino_lex:lexicon(particle(_),_,[Part],[],_),
    alpino_lex:concat_part_to_root(Stem,Part,NewStem),
    SC =.. [Fun0|Args],
    Fun0 \= ninv,   % otherwise we get ninv(ninv(...))
    atom_concat('part_',Fun0,Fun),
    SC2 =.. [Fun,Part|Args].

% campagnevoeren; skispringen ...
% but disallow longer compounds
allow_verb_only_if_particle(v_noun(X),v_noun(X),Parts,Stem,NewStem) :-
    !,
    Parts = [_],
    lists:append(Parts,[Stem],PartsStem),
    alpino_lex:concat_stems(PartsStem,NewStem).
allow_verb_only_if_particle(Tag,Tag,[Noord],Stem0,Stem) :-
    noord(Noord),
    !,
    atom_concat(Noord,Stem0,Stem).
allow_verb_only_if_particle(Tag,Tag,Parts,Stem,NewStem) :-
    lists:append(Parts,[Stem],PartsStem),
    alpino_lex:concat_stems(PartsStem,NewStem).

noord('Noord-').
noord('Zuid-').
noord('Oost-').
noord('West-').

%adjective_tag(adjective(no_e(_))).
%adjective_tag(adjective(e)).

proper_name_tag(proper_name(_)).
proper_name_tag(proper_name(_,_)).

lexical_analysis_noun(Word,Stem0,P0,R0,SurfLength,Name,Tag):-
    alpino_lex:lexicon(Tag,Stem,[Word],[],_),
    noun_tag(Tag),
    P is P0+SurfLength,
    R is R0+SurfLength,
    alpino_lex:concat_stems([Stem,Stem0],NewStem),
    assert_tag(P0,P,R0,R,NewStem,Name,Tag).

lexical_analysis_tag(Tag,P0,R0,Stem,Name):-
    P is P0 + 1,
    R is R0 + 1,
    assert_tag(P0,P,R0,R,Stem,Name,Tag).

add_features(proper_name(Agr), proper_name(Agr,TYPE),TYPE).
add_features(name_determiner(Agr), name_determiner(Agr,TYPE),TYPE).

add_number(proper_name,proper_name(Agr),TYPE,Stem) :-
    add_number(TYPE,Agr,Stem).
add_number(name_determiner(Pron), name_determiner(Pron),_,_).

%% or should this use guess_number from lex.pl
add_number('PER',Agr,Stem) :-
    Agr = sg,
    atom(Stem),
    \+ atom_concat(_,en,Stem),
    \+ atom_concat(_,s,Stem),
    !.
add_number(_,both,_).

add_features(History,Names,Cat0,Cat,P0,P,All,TYPE,Stem) :-
    guess_name_type(P0,P,All,TYPE),
    His = classifier,
    add_number(Cat0,Cat1,TYPE,Stem),
    (   TYPE == 'MISC'
    ->  Cat1=Cat
    ;   add_features(Cat1,Cat,TYPE)
    ),
    debug_message(3,"NE-type|~p|~p|~p|~p~n",[Names,History,TYPE,His]).

guess_name_type(P0,P,All,TYPE) :-
    NameLength is P-P0,
    (   NameLength > 8  % rubbish
    ->  TYPE='MISC'
    ;   length(LeftContextList0,P0),
	append(LeftContextList0,Rest,All),
	alpino_lexical_analysis:remove_brackets(LeftContextList0,LeftContextList),
	last_or_eps(LeftContextList,LC2,LC1),
	length(NameList,NameLength),
	append(NameList,RightContextList0,Rest),
	alpino_lexical_analysis:remove_brackets(RightContextList0,RightContextList),
	first_or_eps(RightContextList,RC1,RC2),
	concat_all(NameList,Name,' '),
	atom_length(Name,Atom),
	(   Atom > 60
	->  TYPE = 'MISC'
	;   header_name(Name)
	->  TYPE = 'MISC'
	;   debug_message(3,
			  "using maxent classifier for \"~p|~p|~p|~p|~p\"~n",
			  [LC2,LC1,Name,RC1,RC2]),
	    classify_named_entity(Name,LC2,LC1,RC1,RC2,TYPE)
	)
    ).

header_name('Jong Feyenoord').
header_name('Milieubeheer').
header_name('Ned/RAB').

last_or_eps([],'','').
last_or_eps([H|T],LC2,LC1) :-
    last_or_eps(T,H,LC2,LC1).

last_or_eps([],H,'',H).
last_or_eps([H|T],P,LC2,LC1) :-
    last_or_eps(T,P,H,LC2,LC1).

last_or_eps([],LC2,LC1,LC2,LC1).
last_or_eps([H|T],_P0,P,LC2,LC1) :-
    last_or_eps(T,P,H,LC2,LC1).

first_or_eps([],'','').
first_or_eps([H|T],H,RC) :-
    first_or_eps(T,RC).

first_or_eps([],'').
first_or_eps([H|_],H).

%% if only in lexicon as a non-name because of full-decapitalization, then
%% we don't regarded it as subsumed.
%% SAR should be both proper name and verb, since only sar is in the dict
%% RSI should be only noun, since RSI is in dict as well
%% CPN should be only name through names_dictionary

subsumed_by_dict(P0,P,_Surf,_Tag) :-
    tag(P0,P,_,_,_,_C,special(decap(normal)),noun(_,_,_)).
subsumed_by_dict(P0,P,_Surf,Tag) :-
    tag(P0,P,_,_,_,C,normal(His),_),
    \+ exceptional_not_subsumed_by_dict(His,C,Tag).

subsumed_by_dict(P0,P,_,Tag) :-
    between(0,P0,Q0),
    tag(Q0,Q,_,_,_,C,normal(His),_),
    P =< Q,
    Q0-Q \= P0-P,
    \+ exceptional_not_subsumed_by_dict(His,C,Tag).

subsumed_by_dict(_,_,[Surf],_) :-
    accent(Surf,Name),
    alpino_lex:lexicon(_,_,[Name],[],_).

%% John Major van Groot-Brittanië
%% forbid: "Name1 van Name2"
%% where "Name1" and "Name2" are known names,
%% whereas "van Name2" is not
%% So we do allow combinations of known "Name1" + known "van Name2"
subsumed_by_dict(P0,P,_,_) :-
    tag(P0,P1,_,_,_,_,normal(names_dictionary),_), P1 < P,
    tag(P1,P2,_,_,van,van,_,_),                    P2 < P,
    optional_de_het(P2,P3),                        P3 < P,
    tag(P3,P,_,_,_,_,normal(names_dictionary),_),
    \+ tag(P1,P,_,_,_,_,normal(names_dictionary),_).

%% forbid: Name1 ( Name2 ) where Name1 is known person, and Name2 is known org
subsumed_by_dict(P0,P,_,_) :-
    tag(P0,P1,_,_,_,_,normal(names_dictionary),proper_name(sg,'PER')),  P1 < P,
    tag(P1,P2,_,_,'(','(',_,_),                    P2 < P,
    tag(P2,P3,_,_,_,_,normal(names_dictionary),proper_name(_,'ORG')),   P3 < P,
    tag(P3,P,_,_,')',')',_,_).

%% forbid: Bosnische Serviërs
subsumed_by_dict(P0,P,_,_) :-
    tag(P0,P1,_,_,_,_,normal(normal),adjective(_)), P1 < P,
    tag(P1,P ,_,_,_,_,normal(normal),noun(_,_,_)).

%% forbid: Duitse Siemens
%%         Rotterdamse Kuip
%% ok:     Frans Habets
subsumed_by_dict(P0,P,_,_) :-
    tag(P0,P1,_,_,_,_,normal(normal),adjective(_)), P1 < P,
    \+ tag(P0,P1,_,_,_,_,normal(names_dictionary),_),
    tag(P1,P ,_,_,W,_,normal(names_dictionary),_),
    W \= 'Kanaal',  % het Eindhovens Kanaal
    W \= 'Baan'.  % cf "het Barneveldse Baan" vs. "de Keulse Baan"

%% forbid: Beide Nederlandse
%% allow:  Jo Hollands ("jo" is a tag, too)
subsumed_by_dict(P0,P,_,_) :-
    tag(P0,P1,_,_,_,_,normal(decap(normal)),_), P1 < P,
    \+ tag(P0,P1,_,_,_,_,normal(names_dictionary),_),
    tag(P1,P ,_,_,_,_,normal(normal),adjective(_)).

%% forbid: Amsterdam en Brussel
%%         Londen en New [York]
%%         [New] York en London
%%         [..] Zaken en Justitie
subsumed_by_dict(P0,P,_,_) :-
    between(0,P0,Pa),
    tag(Pa,P1,_,_,_,_,normal(names_dictionary),proper_name(_,L)),
    Pa =< P0,
    (  tag(P1,P2,_,_,_,_,normal(normal),conj(en))
    ;  tag(P1,P2,_,_,_,_,normal(normal),conj(of))
    ),
    optional_de_het(P2,P3),
    tag(P3, P4,_,_,_,_,normal(names_dictionary),proper_name(_,L)),
    P4 >= P.

%% [Hier en] Nu en Tweekamp
subsumed_by_dict(P0,P,_,_) :-
    between(0,P0,Pa),
    tag(Pa,P1,_,_,_,_,normal(names_dictionary),proper_name(_)),
    Pa =< P0,
    (  tag(P1,P2,_,_,_,_,normal(normal),conj(en))
    ;  tag(P1,P2,_,_,_,_,normal(normal),conj(of))
    ),
    optional_de_het(P2,P3),
    tag(P3, P4,_,_,_,_,normal(names_dictionary),proper_name(_)),
    P4 >= P.

%% forbid: Turkse en [de] Marokkaanse
subsumed_by_dict(P0,P,_,_) :-
    tag(P0,P1,_,_,_,_,normal(normal),T),
    (  tag(P1,P2,_,_,_,_,normal(normal),conj(en))
    ;  tag(P1,P2,_,_,_,_,normal(normal),conj(of))
    ),
    optional_de_het(P2,P3),
    tag(P3, P,_,_,_,_,normal(normal),T).

%% forbid: Brits-Nederlanse
subsumed_by_dict(_,_,[Surf],_) :-
    atom(Surf),
    once(atom_split(Surf,'-',Brits,Nederlands)),
    alpino_lex:xl(Brits,adjective(no_e(_)),_,[],[]),
    alpino_lex:xl(Nederlands,adjective(_),_,[],[]).

subsumed_by_dict(_,_,[Surf],_) :-
    guess_prefix_compound(Surf,_,Wmin),
    alpino_lex:lexicon(Tag,_,[Wmin],[],_),
    proper_name_tag(Tag).

%% forbid: ASVA as name, because already special(decap(names_dict))
subsumed_by_dict(P0,P,_,proper_name(_)) :-
    tag(P0,P,_,_,_,_,special(decap(_)),Tag),
    proper_name_tag(Tag).

%% Heer !
subsumed_by_dict(P0,P,_,_) :-
    tag(P0,P1,_,_,_,_,normal(_),_),
    tag(P1,P,_,_,_,_,_,punct(uitroep)).

subsumed_by_dict(_P0,_P,[Surf],_) :-
    name_koning(Surf).

optional_de_het(P,P).
optional_de_het(P0,P) :-
    tag(P0,P,_,_,de,de,_,_).
optional_de_het(P0,P) :-
    tag(P0,P,_,_,het,het,_,_).

%% the verb SAR should not block the name SAR, since only sar is in dict
%% C is made up of capitals only
%% C is not in the dictionary
exceptional_not_subsumed_by_dict(_,Initial,_) :-
    name_initial(Initial).
exceptional_not_subsumed_by_dict(His,C,_) :-
    His \= names_dictionary,   % dictionary name CPN *should* block CPN 
    His \= decap(names_dictionary),   
    only_capitals(C,_SmallC),
    \+ alpino_lex:xl(C,_,_,[],[]).
exceptional_not_subsumed_by_dict(gen(names_dictionary),_,C) :-
    C \= name_determiner(pron).
exceptional_not_subsumed_by_dict(variant(_,gen(names_dictionary)),_,C) :-
    C \= name_determiner(pron).
exceptional_not_subsumed_by_dict(enumeration,_,C) :-
    C \= proper_name(_).
exceptional_not_subsumed_by_dict(decap('Adj-s'),_,_).

ends_with_adjective_marker(List,E) :-
    last(List,Gen),
    atom(Gen),
    \+ foreign_word(Gen),
    atom_concat(_,Marker,Gen),
    adjective_marker(Marker,E).

adjective_marker('se',e).	% Utrechtse
adjective_marker(XS,no_e(nonadv)):-	% Utrechts
    char_code(s,S),
    atom(XS),
    atom_codes(XS,[NotQuote,S]),
    char_code('\'',Quote),
    NotQuote \== Quote.

ends_with_genitive_marker(List0,List) :-
    append(Prefix,[Gen0],List0),
    append(Prefix,[Gen],List),
    atom(Gen0),
    \+ foreign_word(Gen0),
    atom_concat(_Pre,Marker,Gen0),
    genitive_marker(Marker,Subtract),!,  % because 's also matches s
    atom_concat(Gen,Subtract,Gen0).

genitive_marker('\'s','\'s').
genitive_marker('’s','’s').  % thanks gosse
genitive_marker('s\'','\'').
genitive_marker('sch\'','\'').
genitive_marker('x\'','\'').
genitive_marker(s,'').   
genitive_marker(sh,'').   
genitive_marker(x,'').   
genitive_marker(ic,'').   
genitive_marker(sch,''). 

%% potential names are given by a finite automaton. Start node is 0
potential_name(Input,Rest,Prefix,P0,P,History) :-
    start_of_sentence(P0,START),
    \+ unlikely_name_start(Input),
    \+ part_of_longest_match(P0),
    potential_name_fsa(START,P0,Input,Rest,Prefix,History),
    length(Prefix,Len),
    P is P0+Len,
    \+ unlikely_name(Prefix,P0,P,Rest).

part_of_longest_match(Q) :-
    Q0 is Q-4,
    between(0,Q0,P0),
    tag(P0,P,_,_,_,_,name(_),_),
    P > Q + 4,
    \+ normal_analysis(P0,Q),
    \+ normal_analysis(Q,P),
    \+ komma(P0,Q),
    \+ komma(Q,P).

komma(P0,P) :-
    P1 is P-1,
    between(P0,P1,Q0),
    search_tag_tag(punct(komma),tag(Q0,Q,_,_,_,_,_,punct(komma))),
    P0 =< Q0, Q =< P.

:- dynamic normal_tag/2.
:- thread_local normal_tag/2.

prepare_normal_analysis :-
    retractall(normal_tag(_,_)),
    (   tag(P0,P,_,_,_,_Surf,normal(_),_),
	(   normal_tag(P0,P)
	->  true
	;   noclp_assertz(normal_tag(P0,P))
	),
	fail
    ;   true
    ).

normal_analysis(S,S).
normal_analysis(S0,S) :-
    normal_tag(S0,S1),
    S1 =< S,
    normal_analysis(S1,S).

unlikely_name_start([H|T]) :-
    unlikely_name_prefix(H,T).

%% this is for sequences of words that can be *part* of names but are
%% never names on their own.

%% for long sequences of repeated words, don't assume all subsequences
%% are names
unlikely_name(List,_,_,[Name,Name|_Rest]) :-
    last(List,Name).

unlikely_name(List,P0,P,_Rest) :-
    unlikely_name(List,P0,P).

unlikely_name(List,_,_) :-
    last(List,W),
    unlikely_last_word_name(W).

unlikely_name(List,_,_) :-
    bridge_wind(List).

%unlikely_name([H|T],_,_):-
%    unlikely_name_prefix(H,T).

unlikely_name([H],_,_):-
    europarl(H).

unlikely_name([H],_,_):-
    unlikely_name_lonely(H).

unlikely_name([A,B],_,_) :-
    unlikely_name_pair(A,B).

unlikely_name([A,B,C|_],_,_) :-
    unlikely_name_triple(A,B,C).

unlikely_name(Input,_,_) :-
    append([Burgemeester|B],Rest,Input),
    name_koning(Burgemeester,B),
    append(_,[van|Geo],Rest),
    alpino_lex:lexicon(proper_name(both,'LOC'),_,Geo,[],names_dictionary).

%% name with comma: there should not be a larger name
%% seperated by comma
unlikely_name(List,P1,P) :-
    member(',',List),
    between(0,P1,P0),
    tag(P0,P2,_,_,_,_,name(_),_), P =< P2.

%% if all capitals then not proceeded or followed by another
%% all capitals
unlikely_name(List,P1,P2) :-
    \+ capitalized_name(P1,P2),
    no_lower_case(List),
    (   next_one_special_decap(P2)
    ;   previous_one_special_decap(P1)
    ;   is_start_sentence(P1),
	all_special_decap(P1,P2),
	P2-P1 > 2
    ;   is_end_sentence(P2),
	all_special_decap(P1,P2),
	P2-P1 > 2
    ).

%%% looks like a list
%%% case 1: list already started before this name:
unlikely_name([_,_|_],P1,_P2) :-
    tag(_,P1,_,_,_,_,normal(names_dictionary),proper_name(both,LAB)),
    per_or_loc_or_org(LAB),
    loc_list(LAB,P1).

%%% case 2: 
unlikely_name([_,_|_],P1,P2) :-
    between(P1,P2,Q0),
    tag(Q0,Q1,_,_,_,_,normal(names_dictionary),proper_name(both,LAB)),
    Q1 < P2,
    per_or_loc_or_org(LAB),
    loc_list(LAB,Q1).

next_one_special_decap(P0) :-
    tag(P0,_,_,_,_,_,special(decap(_)),_).
next_one_special_decap(P0) :-
    tag(P0,P1,_,_,_,_,_,punct(_)),
    next_one_special_decap(P1).

previous_one_special_decap(P) :-
    tag(_,P,_,_,Foto,_,special(decap(_)),_),!,
    \+ foto(Foto).
previous_one_special_decap(P) :-
    tag(P0,P,_,_,_,_,_,punct(_)),
    previous_one_special_decap(P0).

foto(door).
foto(foto).

per_or_loc_or_org('LOC').
per_or_loc_or_org('PER').
per_or_loc_or_org('ORG').

%% at least three more, not necc part of the current name, for org and
%% loc;
%% for PER we require five, since personal names are more often
%% combined to form personal names
loc_list('ORG',P0) :-
    tag(P0,P1,_,_,_,_,normal(names_dictionary),proper_name(both,'ORG')),
    tag(P1,P2,_,_,_,_,normal(names_dictionary),proper_name(both,'ORG')),
    tag(P2,_P3,_,_,_,_,normal(names_dictionary),proper_name(both,'ORG')).

loc_list('LOC',P0) :-
    tag(P0,P1,_,_,_,_,normal(names_dictionary),proper_name(both,'LOC')),
    tag(P1,P2,_,_,_,_,normal(names_dictionary),proper_name(both,'LOC')),
    tag(P2,_P3,_,_,_,_,normal(names_dictionary),proper_name(both,'LOC')).

loc_list('PER',P0) :-
    tag(P0,P1,_,_,_,_,normal(names_dictionary),proper_name(both,'PER')),
    tag(P1,P2,_,_,_,_,normal(names_dictionary),proper_name(both,'PER')),
    tag(P2,P3,_,_,_,_,normal(names_dictionary),proper_name(both,'PER')),
    tag(P3,P4,_,_,_,_,normal(names_dictionary),proper_name(both,'PER')),
    tag(P4,__,_,_,_,_,normal(names_dictionary),proper_name(both,'PER')).

all_special_decap(P,P).
all_special_decap(P0,P) :-
    special_decap_step(P0,P1),
    all_special_decap(P1,P).

special_decap_step(P0,P) :-
    findall(P,tag(P0,P,_,_,_,_,special(decap(_)),_),Ps0),
    sort(Ps0,Ps),
    lists:member(P,Ps).

no_lower_case([]).
no_lower_case([H|T]) :-
    atom(H),
    atom_codes(H,HCodes),
    no_lower_case_codes(HCodes),
    no_lower_case(T).

no_lower_case_codes([]).
no_lower_case_codes([H|T]) :-
    \+ islower(H),
    no_lower_case_codes(T).

capitalized_name(P,P).
capitalized_name(P0,P) :-
    tag(P0,P1,_,_,_,_,special(decap(names_dictionary)),_),
    !,
    capitalized_name(P1,P).
capitalized_name(P0,P) :-
    tag(P0,P1,_,_,_,_,normal(names_dictionary),_),
    !,
    capitalized_name(P1,P).
capitalized_name(P0,P) :-
    tag(P0,P1,_,_,_,VAN,_,_),
    only_capitals(VAN,V),
    (  name_vanhet_maybe(V)
    ;  name_vanhet(V)
    ),
    name_vanhet_start(VAN),
    capitalized_name(P1,P).


unlikely_name_pair(alles,best).
unlikely_name_pair(alles,even).
unlikely_name_pair(alles,had).
unlikely_name_pair(alles,over).
unlikely_name_pair(alles,perfect).
unlikely_name_pair(arts,had).
unlikely_name_pair(best,even).
unlikely_name_pair(brand,had).
unlikely_name_pair(club,had).
unlikely_name_pair(den,loop).
unlikely_name_pair(den,man).
unlikely_name_pair(dollar,had).
unlikely_name_pair(dollar,over).
unlikely_name_pair(even,had).
unlikely_name_pair(even,hard).
unlikely_name_pair(is,alles).
unlikely_name_pair(man,had).
unlikely_name_pair(man,over).
unlikely_name_pair(media,over).
unlikely_name_pair(name,over).
unlikely_name_pair(posted,by).
unlikely_name_pair(set,had).
unlikely_name_pair(team,had).
unlikely_name_pair(van,den).
unlikely_name_pair('De','EU').
unlikely_name_pair('Dit',parlement).
unlikely_name_pair('Dit','Parlement').
unlikely_name_pair('I',am).
unlikely_name_pair('If',you).
unlikely_name_pair('I',love).
unlikely_name_pair('Originally',posted).

unlikely_name_triple(brand,in,'Volendam').
unlikely_name_triple('Herfkens',voor,'Ontwikkelingssamenwerking').
unlikely_name_triple('Dat',is,alles).
unlikely_name_triple('Dat',is,best).
unlikely_name_triple('Dat',is,even).
unlikely_name_triple('Dat',is,hard).
unlikely_name_triple('Dat',was,alles).
unlikely_name_triple('Dat',was,best).
unlikely_name_triple('Dat',was,even).
unlikely_name_triple('Dat',was,hard).
unlikely_name_triple('Dit',is,alles).
unlikely_name_triple('Dit',is,best).
unlikely_name_triple('Dit',is,even).
unlikely_name_triple('Dit',is,hard).
unlikely_name_triple('Dit',was,alles).
unlikely_name_triple('Dit',was,best).
unlikely_name_triple('Dit',was,even).
unlikely_name_triple('Dit',was,hard).
unlikely_name_triple('Die',is,alles).
unlikely_name_triple('Die',is,best).
unlikely_name_triple('Die',is,even).
unlikely_name_triple('Die',is,hard).
unlikely_name_triple('Die',was,alles).
unlikely_name_triple('Die',was,best).
unlikely_name_triple('Die',was,even).
unlikely_name_triple('Die',was,hard).
unlikely_name_triple('Die',man,had).
unlikely_name_triple('Ik',had,die).
unlikely_name_triple('Nu',is,alles).
unlikely_name_triple('Nu',was,alles).
unlikely_name_triple('Hij',is,even).
unlikely_name_triple('Hij',was,even).
unlikely_name_triple('Originally',posted,by).
unlikely_name_triple('Verenigde','Staten',over).

unlikely_name_prefix('Afbeelding',_).
unlikely_name_prefix('Betrekkingen',_).
unlikely_name_prefix('Discografie',_).
unlikely_name_prefix('Geschiedenis',_).

unlikely_name_prefix('PAG.',_).
unlikely_name_prefix('ART.',_).

%% twitter
unlikely_name_prefix('Ah',_).
unlikely_name_prefix('Aah',_).
unlikely_name_prefix('Aaah',_).
unlikely_name_prefix('Aaaah',_).
unlikely_name_prefix('Euh',_).
unlikely_name_prefix('Euhh',_).
unlikely_name_prefix('Euhhh',_).
unlikely_name_prefix('Haha',_).
unlikely_name_prefix('HAHA',_).
unlikely_name_prefix('Hahaha',_).
unlikely_name_prefix('HAHAHA',_).
unlikely_name_prefix('Hahahaha',_).
unlikely_name_prefix('Hahahahaha',_).
unlikely_name_prefix('Hahahahahaha',_).
unlikely_name_prefix('Hm',_).
unlikely_name_prefix('Hmm',_).
unlikely_name_prefix('Hmmm',_).
unlikely_name_prefix('Hmmmm',_).
unlikely_name_prefix('Pff',_).
unlikely_name_prefix('Pfff',_).
unlikely_name_prefix('Pffff',_).
unlikely_name_prefix('Neenee',_).
unlikely_name_prefix('Sorry',_).

%% krantenkopjes
unlikely_name_prefix('Acceleratie',_).
unlikely_name_prefix('Achterbankzitting',_).
unlikely_name_prefix('BEELDENDE',_).
unlikely_name_prefix('Buitentemperatuurmeter',_).
unlikely_name_prefix('Copyright',_).
unlikely_name_prefix('Diagnose',_).
unlikely_name_prefix('EXTRA\'S',_).
unlikely_name_prefix('FOTO',_).
unlikely_name_prefix('FOTO\'S',_).
unlikely_name_prefix('Foto',_).
unlikely_name_prefix('Foto\'s',_).
unlikely_name_prefix('Fotos',_).
unlikely_name_prefix('Goedenavond',_).
unlikely_name_prefix('Heren',['Goedenavond'|_]). % WSUE
unlikely_name_prefix('ILLUSTRATIE',_).
unlikely_name_prefix('Inbraak/diefstalalarm',_).
unlikely_name_prefix('INFORMATIEF',_).
unlikely_name_prefix('ISBN',_).
unlikely_name_prefix('ISSN',_).
unlikely_name_prefix('Landen',_).
unlikely_name_prefix('MARKETING',_).
unlikely_name_prefix('Mistacherlicht',_).
unlikely_name_prefix('NASCHRIFT',_).
unlikely_name_prefix('PRAATPROGRAMMA\'S',_).
unlikely_name_prefix('PRAKTISCH',_).
unlikely_name_prefix('Radio',['Voorkeur']).
unlikely_name_prefix('Reizen',_).
unlikely_name_prefix('Reisadviezen',_).
unlikely_name_prefix('Reisadvies',_).
unlikely_name_prefix('Resultatenrekening',_).
unlikely_name_prefix('Samenwerking',_).
unlikely_name_prefix('Skiluik',_).
unlikely_name_prefix('SkiluikNee',_).
unlikely_name_prefix('SOCIAAL-ECONOMISCHE',_).
unlikely_name_prefix('Tewerkstelling',_).
unlikely_name_prefix('VANAVOND',_).
unlikely_name_prefix('VANDAAG',_).
unlikely_name_prefix('VERVOLG',['OP'|_]).
unlikely_name_prefix('VERVOLG',['VAN'|_]).
unlikely_name_prefix('Vert',_).
unlikely_name_prefix('VERT',_).
unlikely_name_prefix('Waarschuwingszoemer',_).
unlikely_name_prefix('Wisser/sproeier',_).

unlikely_name_prefix('MAANDAG',_).
unlikely_name_prefix('DINSDAG',_).
unlikely_name_prefix('WOENSDAG',_).
unlikely_name_prefix('DONDERDAG',_).
unlikely_name_prefix('VRIJDAG',_).
unlikely_name_prefix('ZATERDAG',_).
unlikely_name_prefix('ZONDAG',_).


%% misc
unlikely_name_prefix(man,[die,alles|_]).
unlikely_name_prefix(top,[in|_]).  % Nice, Brussel, etc.
unlikely_name_prefix('Advocaat',[_|_]). %% Advocaat is a name too...
unlikely_name_prefix('Antillianen',[en,'Arubanen'|_]).
unlikely_name_prefix('Centraal',['Station',in|_]).
unlikely_name_prefix('Doe',[alles,over|_]).
unlikely_name_prefix('Door',[de,brand|_]).
unlikely_name_prefix('Geel',[voor|_]).
unlikely_name_prefix('Hij',[is,de|_]).
unlikely_name_prefix('Leers',[van,'Maastricht'|_]).
unlikely_name_prefix('Letterkundig',['Museum',in|_]).
unlikely_name_prefix('Nieuwe',['Kerk',in|_]).
unlikely_name_prefix('Noord',[en,'Zuid'|_]).
unlikely_name_prefix('Olympische',['Spelen',van|_]).
unlikely_name_prefix('Oost',[en,'West'|_]).
unlikely_name_prefix('Opstelten',[van,'Rotterdam'|_]).
unlikely_name_prefix('Pas',[in|_]).
unlikely_name_prefix('Peper',[van,'Rotterdam'|_]).
unlikely_name_prefix('Spelen',[van|_]).
unlikely_name_prefix('Staten',[en|_]).
unlikely_name_prefix('Tel',_).
unlikely_name_prefix('Tel.',_).
unlikely_name_prefix('Tél',_).
unlikely_name_prefix('Tél.',_).
unlikely_name_prefix('Telefax',_).
unlikely_name_prefix(u,_).
unlikely_name_prefix('Van',['de','Nederlanders'|_]).
unlikely_name_prefix('Verdonk',[voor,'Vreemdelingenzaken'|_]).
unlikely_name_prefix('Wallage',[van,'Groningen'|_]).
unlikely_name_prefix('Winterspelen',[van|_]).
unlikely_name_prefix('Zaken',[in|_]).
unlikely_name_prefix('Zaken',[en|_]).

unlikely_name_prefix('&',_).
unlikely_name_prefix('*',_).
unlikely_name_prefix('(',_).
unlikely_name_prefix(',',_).
unlikely_name_prefix('...',_).
unlikely_name_prefix('^',_).

%% o.a. AD2001
unlikely_name_prefix('Afwezigen',_).

europarl('Act').
europarl('Agenda').
europarl('Agentschap').
europarl('Akkoord').
europarl('Artikel').
europarl('Autoriteit').
europarl('Balkanoorlog').
europarl('Begrotingscommissie').
europarl('Beschikking').
europarl('Besluit').
europarl('Commissietekst').
europarl('Commissievoorstel').
europarl('Commissievoorstellen').
europarl('Conferentie').
europarl('Consensus').
europarl('Conventie').
europarl('Fractie').
europarl('Gemeenschappen').
europarl('Grondrecht').
europarl('Grondrechten').
europarl('Handvest').
europarl('Landbouwraad').
europarl('Leden').
europarl('Lid-Staten').
europarl('Lid-Staat').
europarl('Octrooiverdrag').
europarl('Ontwerpverdrag').
europarl('Overeenkomst').
europarl('Overeenkomsten').
europarl('Parlement').
europarl('Parlementen').
europarl('Parlementariërs').
europarl('Parlementsgebouw').
europarl('Parlementsleden').
europarl('Parlementslid').
europarl('Parlementsverkiezing').
europarl('Parlementsverkiezingen').
europarl('Protocol').
europarl('Publicatiedatum').
europarl('Raad').
europarl('Raden').
europarl('Raadsvoorzitter').
europarl('Raadsvoorzitterschap').
europarl('Richtlijn').
europarl('Samenwerkingsovereenkomst').
europarl('Secretariaat').
europarl('Statuut').
europarl('Stichting').
europarl('Structuurfonds').
europarl('Structuurfondsen').
europarl('Uitvoeringsmaatregelen').
europarl('Unieverdrag').
europarl('Verdrag').
europarl('Verdragen').
europarl('Vergadering').
europarl('Verordering').
europarl('Vertegenwoordiger').
europarl('Vervoer').
europarl('Voorzitterschap').
europarl('Witboek').
europarl('Zwartboek').

unlikely_name_lonely('Air').
unlikely_name_lonely('Algemeen').
unlikely_name_lonely('Algemene').
unlikely_name_lonely('Alles').
unlikely_name_lonely('And').
unlikely_name_lonely('Awards').
unlikely_name_lonely('Baldakijn').
unlikely_name_lonely('Bank').
unlikely_name_lonely('Beach').
unlikely_name_lonely('Bin').
unlikely_name_lonely('Business').
unlikely_name_lonely('Center').
unlikely_name_lonely('Company').
unlikely_name_lonely('Conventie').
unlikely_name_lonely('Co.').
unlikely_name_lonely('C\'est').
unlikely_name_lonely('Demografie').
unlikely_name_lonely('Den').
unlikely_name_lonely('Derde').
unlikely_name_lonely('Die').
unlikely_name_lonely('Economie').
unlikely_name_lonely('Een').
unlikely_name_lonely('Eerste').
unlikely_name_lonely('Ensemble').
unlikely_name_lonely('Er').
unlikely_name_lonely('European').
unlikely_name_lonely('Express').
unlikely_name_lonely('Financieel').
unlikely_name_lonely('Galerie').
unlikely_name_lonely('Grote').
unlikely_name_lonely('Group').
unlikely_name_lonely('Formule').
unlikely_name_lonely('Global').
unlikely_name_lonely('Great').
unlikely_name_lonely('Gouden').
unlikely_name_lonely('Hij').
unlikely_name_lonely('Holding').
unlikely_name_lonely('Hotel').
unlikely_name_lonely('II').
unlikely_name_lonely('III').
unlikely_name_lonely('IV').
unlikely_name_lonely('Ik').
unlikely_name_lonely('Il').
unlikely_name_lonely('Internationaal').
unlikely_name_lonely('International').
unlikely_name_lonely('Jaar').
unlikely_name_lonely('Je').
unlikely_name_lonely('Jij').
unlikely_name_lonely('Jou').
unlikely_name_lonely('Juridische').
unlikely_name_lonely('Kwartet').
unlikely_name_lonely('La').
unlikely_name_lonely('Laatste').
unlikely_name_lonely('Le').
unlikely_name_lonely('Magazine').
unlikely_name_lonely('Management').
unlikely_name_lonely('Maritieme').
unlikely_name_lonely('Meer').
unlikely_name_lonely('Mij').
unlikely_name_lonely('Mijn').
unlikely_name_lonely('Miss').
unlikely_name_lonely('Mooi').
unlikely_name_lonely('Naar').
unlikely_name_lonely('Nationaal').
unlikely_name_lonely('National').
unlikely_name_lonely('Nationale').
unlikely_name_lonely('News').
unlikely_name_lonely('Nieuwe').
unlikely_name_lonely('Oké').
unlikely_name_lonely('On').
unlikely_name_lonely('Ons').
unlikely_name_lonely('Ontwikkeling').
unlikely_name_lonely('Onze').
unlikely_name_lonely('Open').
unlikely_name_lonely('Oude').
unlikely_name_lonely('Partnerschap').
unlikely_name_lonely('Prijs').
unlikely_name_lonely('Orchestra').
unlikely_name_lonely('Quartet').
unlikely_name_lonely('Quintet').
unlikely_name_lonely('Radio').
unlikely_name_lonely('Records').
unlikely_name_lonely('Recordings').
unlikely_name_lonely('Releases').
unlikely_name_lonely('Rue').
unlikely_name_lonely('Sao').
unlikely_name_lonely('Sankt').
unlikely_name_lonely('School').
unlikely_name_lonely('Services').
unlikely_name_lonely('St').
unlikely_name_lonely('St.').
unlikely_name_lonely('Saint').
unlikely_name_lonely('Sociaal').
unlikely_name_lonely('Sport').
unlikely_name_lonely('Studio').
unlikely_name_lonely('Street').
unlikely_name_lonely('Systems').
unlikely_name_lonely('Te').
unlikely_name_lonely('Team').
unlikely_name_lonely('Technologies').
unlikely_name_lonely('Ten').
unlikely_name_lonely('Theatre').
unlikely_name_lonely('Theatergroep').
unlikely_name_lonely('This').
unlikely_name_lonely('To').
unlikely_name_lonely('Trends').
unlikely_name_lonely('Trio').
unlikely_name_lonely('Tweede').
unlikely_name_lonely('Universele').
unlikely_name_lonely('University').
unlikely_name_lonely('Vader').
unlikely_name_lonely('Vandaag').
unlikely_name_lonely('Vanavond').
unlikely_name_lonely('Verschenen').
unlikely_name_lonely('Via').
unlikely_name_lonely('We').
unlikely_name_lonely('Wij').
unlikely_name_lonely('World').
unlikely_name_lonely('Zee').
unlikely_name_lonely('Zelf').
unlikely_name_lonely('Zij').

unlikely_name_lonely('Dr.').
unlikely_name_lonely('Mr.').
unlikely_name_lonely('Mrs.').

unlikely_name_lonely('Atletiek').
unlikely_name_lonely('Beachvolleybal').
unlikely_name_lonely('Reis').
unlikely_name_lonely('IJshockey').
unlikely_name_lonely('Wielrennen').

unlikely_name_lonely('Grootvader').
unlikely_name_lonely('Grootmoeder').
unlikely_name_lonely('Opa').
unlikely_name_lonely('Oma').
unlikely_name_lonely('Vader').
unlikely_name_lonely('Moeder').
unlikely_name_lonely('Oom').
unlikely_name_lonely('Tante').
unlikely_name_lonely('Pa').
unlikely_name_lonely('Moe').
unlikely_name_lonely('Zus').
unlikely_name_lonely('Zoon').

unlikely_name_lonely('Excellentie').
unlikely_name_lonely('Hoogheid').
unlikely_name_lonely('Majesteit').

unlikely_name_lonely(W) :-
    decap_foreign_word(W).

bridge_wind([]).
bridge_wind([H|T]) :-
    bridge_wind1(H),
    bridge_wind(T).

bridge_wind1('Noord').
bridge_wind1('Oost').
bridge_wind1('Zuid').
bridge_wind1('West').

unlikely_last_word_name('De').
unlikely_last_word_name('Den').  % Deetman van Den
unlikely_last_word_name('DE').
unlikely_last_word_name('Het').
unlikely_last_word_name('HET').
unlikely_last_word_name('Een').
unlikely_last_word_name('EEN').
%unlikely_last_word_name('Van').  is a person name and location name 
unlikely_last_word_name('VAN').
unlikely_last_word_name('THE').
unlikely_last_word_name('The').
unlikely_last_word_name('Nationale').
unlikely_last_word_name(a).
unlikely_last_word_name(al).
unlikely_last_word_name('ALS').
unlikely_last_word_name(and).
unlikely_last_word_name(at).
unlikely_last_word_name(au).
unlikely_last_word_name(bin).
unlikely_last_word_name(but).
unlikely_last_word_name(de).
unlikely_last_word_name(del).
unlikely_last_word_name(della).
unlikely_last_word_name(dello).
%unlikely_last_word_name(den).  parasolden
unlikely_last_word_name(der).
unlikely_last_word_name(die).
unlikely_last_word_name('doesn\'t').
unlikely_last_word_name('don\'t').
unlikely_last_word_name(du).
unlikely_last_word_name(e).
unlikely_last_word_name(el).
unlikely_last_word_name(en).
unlikely_last_word_name('En').
unlikely_last_word_name('EN').
unlikely_last_word_name(et).
unlikely_last_word_name('Externe').
unlikely_last_word_name(for).
unlikely_last_word_name(für).
unlikely_last_word_name(he).
unlikely_last_word_name(het).
unlikely_last_word_name('I\'m').
unlikely_last_word_name(in).
unlikely_last_word_name('IN').
unlikely_last_word_name('it\'s').
unlikely_last_word_name(is).
unlikely_last_word_name('IS').
unlikely_last_word_name(je).
unlikely_last_word_name(la).
unlikely_last_word_name(las).
unlikely_last_word_name(le).
unlikely_last_word_name(les).
unlikely_last_word_name(los).
unlikely_last_word_name(ma).
unlikely_last_word_name('Monetaire').
unlikely_last_word_name(my).
unlikely_last_word_name(nam).
unlikely_last_word_name(of).
unlikely_last_word_name('OP').
unlikely_last_word_name(or).
unlikely_last_word_name(over).
unlikely_last_word_name(por).
unlikely_last_word_name(si).
unlikely_last_word_name(sur).
unlikely_last_word_name('\'t').
unlikely_last_word_name(ten).
unlikely_last_word_name(ter).
unlikely_last_word_name('that\'s').
unlikely_last_word_name(the).
unlikely_last_word_name('THE').
unlikely_last_word_name(their).
unlikely_last_word_name(to).
unlikely_last_word_name('TO').
unlikely_last_word_name('Universele').
unlikely_last_word_name(van).
unlikely_last_word_name('VAN').
unlikely_last_word_name('VOOR').
unlikely_last_word_name(voor).
unlikely_last_word_name(vande).
unlikely_last_word_name(vanden).
unlikely_last_word_name(vander).
unlikely_last_word_name('Vande').
unlikely_last_word_name('Vanden').
unlikely_last_word_name('Vander').
unlikely_last_word_name(wanna).
unlikely_last_word_name(was).
unlikely_last_word_name('WAS').
unlikely_last_word_name(with).

unlikely_last_word_name('&').
unlikely_last_word_name('*').

unlikely_last_word_name(Init) :-
    name_small_initial(Init).
unlikely_last_word_name(Word) :-
    atom(Word),
    atom_concat(_,'-',Word).

%% there is an obvious generalization missed here:
unlikely_last_word_name('Arctisch').
unlikely_last_word_name('Arctische').
unlikely_last_word_name('Confederaal').
unlikely_last_word_name('Confederale').
unlikely_last_word_name('Constitutioneel').
unlikely_last_word_name('Constitutionele').
unlikely_last_word_name('Economisch').
unlikely_last_word_name('Economische').
unlikely_last_word_name('Grondwettelijk').
unlikely_last_word_name('Hoge').
unlikely_last_word_name('Oostelijk').
unlikely_last_word_name('Oostelijke').
unlikely_last_word_name('Paritaire').
unlikely_last_word_name('Paritair').
unlikely_last_word_name('Parlementaire').
unlikely_last_word_name('Parlementair').
unlikely_last_word_name('Progressieve').
unlikely_last_word_name('Progressief').
unlikely_last_word_name('Regionale').
unlikely_last_word_name('Regionaal').
unlikely_last_word_name('Tijdelijke').
unlikely_last_word_name('Tijdelijk').
unlikely_last_word_name('Verenigde').
unlikely_last_word_name('Verenigd').
unlikely_last_word_name('Voormalige').
unlikely_last_word_name('Voormalig').
unlikely_last_word_name('Westelijk').
unlikely_last_word_name('Westelijke').

%% state 0: start
potential_name_fsa_not_begin(unknown,P0,[Word|Words],Ws,[Word|Prefix],
		   [title|His]) :-
    only_capitals(Word,_),
    name_title(Word),
    P1 is P0 + 1,
    potential_name_fsa(2,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(_,P0,[Word|Words],Ws,[Word|Prefix],
		   [title|His]) :-
    name_title(Word),!,
    P1 is P0 + 1,
    potential_name_fsa(1,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(_,P0,[Word|Words],Ws,[Word|Prefix],
		   [firma|His]) :-
    name_firma(Word),!,
    P1 is P0 + 1,
    potential_name_fsa(11,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(_,P0,[Word1,Word|Words],Ws,[Word1,Word|Prefix],
		   [vanhet2|His]) :-
    name_vanhet_maybe_start(Word1,Word),!,
    P1 is P0 + 2,
    potential_name_fsa(3,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(_,P0,[Word1,Word|Words],Ws,[Word1,Word|Prefix],
		   [vanhet2|His]) :-
    name_vanhet_start(Word1,Word),!,
    P1 is P0 + 2,
    potential_name_fsa(33,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(_,P0,[Word|Words],Ws,[Word|Prefix],
		   [vanhet1|His]) :-
    Word='De',!,
    P1 is P0 + 1,    
    potential_name_fsa(33,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(_,P0,[Word|Words],Ws,[Word|Prefix],
		   [vanhet1|His]) :-
    name_vanhet_maybe_start(Word),!,
    P1 is P0 + 1,    
    potential_name_fsa(3,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(_,P0,[Word|Words],Ws,[Word|Prefix],
		   [vanhet1|His]) :-
    name_vanhet_start(Word),
    \+ decap_foreign_word(Word),
    !,
    P1 is P0 + 1,
    potential_name_fsa(33,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(_,P0,[Word|Words],Ws,[Word|Prefix],
		   [vanhet1|His]) :-
    name_vanhet_start(Word),
    decap_foreign_word(Word),
    P1 is P0 + 1,
    potential_name_fsa(33,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(_,P0,[Rue,de,la|Words],Ws,[Rue,de,la|Prefix],
		   [rue_de|His]) :-
    rue(Rue),!,
    P1 is P0 + 2,
    potential_name_fsa(6,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(_,P0,[Rue,de|Words],Ws,[Rue,de|Prefix],
		   [rue_de|His]) :-
    rue(Rue),!,
    P1 is P0 + 2,
    potential_name_fsa(6,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(_,P0,[Rue,du|Words],Ws,[Rue,du|Prefix],
		   [rue_de|His]) :-
    rue(Rue),!,
    P1 is P0 + 2,
    potential_name_fsa(6,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(_,P0,[Rue,des|Words],Ws,[Rue,des|Prefix],
		   [rue_de|His]) :-
    rue(Rue),!,
    P1 is P0 + 2,
    potential_name_fsa(6,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(_,P0,[The,Word|Words],Ws,
                   [The,Word|Prefix],[foreign,foreign|His]) :-
    ( The == the ; The == 'The' ),
    !,
    P1 is P0 + 2,
    potential_name_fsa(222,P1,Words,Ws,Prefix,His).

potential_name_fsa_not_begin(_,P0,[Word|Words],Ws,[Word|Prefix],
		   [foreign|His]) :-
    unknown_foreign_word(Word),
    !,
    P1 is P0 + 1,
    potential_name_fsa(22,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(_,P0,[Word|Words],Ws,[Word|Prefix],
		   [initial|His]) :-
    name_initial(Word),!,
    P1 is P0 + 1,
    potential_name_fsa(2,P1,Words,Ws,Prefix,His).

potential_name_fsa_not_begin(_,P0,[Word,Word2|Words],Ws,[Word,Word2|Prefix],
		   [initial|His]) :-
    name_initial(Word,Word2),!,
    P1 is P0 + 1,
    potential_name_fsa(2,P1,Words,Ws,Prefix,His).

%% not begin - unknown
potential_name_fsa_not_begin(_,P0,[Word|Words],Ws,[Word|Prefix],
		   [foreign|His]) :-
    decap_first(Word,Wordx),
    unknown_foreign_word(Wordx),
    P1 is P0 + 1,    
    potential_name_fsa(23,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(unknown,P0,[Word|Words],Ws,[Word|Prefix],
		   [capital|His]) :-
    name_capital(Word,P0),!,
    P1 is P0 + 1,
    (   name_unknown(Word,P0),
        potential_name_fsa(23,P1,Words,Ws,Prefix,His)
    ;   \+ alpino_lexical_analysis:hesitation(Word),
	potential_name_fsa(2,P1,Words,Ws,Prefix,His)
    ).
potential_name_fsa_not_begin(unknown,P0,['\'s',Word|Words],Ws,
                             ['\'s',Word|Prefix],
		   [capital|His]) :-
    name_capital(Word,P0),!,
    P1 is P0 + 2,
    potential_name_fsa(2,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(unknown,P0,[Word|Words],Ws,[Word1|Prefix],
		   [missing_capital|His]) :-
    name_unknown(Word,P0),
    cap_first(Word,Word1),
    alpino_lex:in_names_dictionary(Tag,Word1,_,[],[],_),
    (  Tag = proper_name(_)
    ;  Tag = proper_name(_,_)
    ),
    P1 is P0 + 1,    
    potential_name_fsa(8,P1,Words,Ws,Prefix,His).
potential_name_fsa_not_begin(unknown,P0,[Word|Words],Ws,[Word|Prefix],
                             [unknown|His]) :-
    name_unknown(Word,P0),
    P1 is P0 + 1,
    potential_name_fsa(23,P1,Words,Ws,Prefix,His).

potential_name_fsa_not_begin(unknown,_,['>',Word,Hellip|Ws],Ws,['>',Word,Hellip],['>']):-
    hellip(Hellip),
    !.  % for names of menu items in QtLeap corpus

potential_name_fsa_not_begin(unknown,P0,['>',Word|Words],Ws,['>',Word|Prefix],['>'|His]):-
    \+ Word='>',
    \+ Word='>>',
    !,  % for names of menu items in QtLeap corpus
    P1 is P0 + 2,
    potential_name_fsa(23,P1,Words,Ws,Prefix,His).

potential_name_fsa_not_begin(_,P0,[Open|Words],Ws,[Open|Prefix],[open|His]) :-
    start_quote_or_bracket(Open,Close),
    P is P0 + 1,
    potential_name_fsa(haakje3(Close),P,Words,Ws,Prefix,His).



%% not begin - decap
potential_name_fsa_not_begin(normal_if_decap,P0,[Word|Words],Ws,[Word|Prefix],
                             [capital|His]) :-
    name_capital(Word,P0),!,
    P1 is P0 + 1,
    potential_name_fsa(8,P1,Words,Ws,Prefix,His).

rue(bois).
rue(col).
rue(cote).
rue(côte).
rue(rue).
rue(via).

%%  only for begin: "de Prinses Margriet vaart uit"

%% President Bill Clinton
%% should not be single name
%% whereas Koningin Wilhelminacollege
%% is ok
a_person_name(Words) :-
    alpino_lex:lexicon(proper_name(_,'PER'),_,Words,_,names_dictionary).

potential_name_fsa(2,_,['>',Word,Helip|Ws],Ws,['>',Word,Helip],['>']) :-
    hellip(Helip),
    !.
potential_name_fsa(222,_,['>',Word,Helip|Ws],Ws,['>',Word,Helip],['>']) :-
    hellip(Helip),
    !.
 
potential_name_fsa(2,P0,['>',Word|Words],Ws,['>',Word|Prefix],['>'|His]) :-
    !,
    P is P0 + 2,
    potential_name_fsa(222,P,Words,Ws,Prefix,His).

potential_name_fsa(222,P0,['>',Word|Words],Ws,['>',Word|Prefix],['>'|His]) :-
    !,
    P is P0 + 2,
    potential_name_fsa(222,P,Words,Ws,Prefix,His).

potential_name_fsa(2,_,Ws,Ws,[],[]).
potential_name_fsa(4,_,Ws,Ws,[],[]).
potential_name_fsa(5,_,Ws,Ws,[],[]).
potential_name_fsa(55,_,Ws,Ws,[],[]).
potential_name_fsa(6,_,Ws,Ws,[],[]).
%% potential_name_fsa(8,_,Ws,Ws,[],[]).

potential_name_fsa(not_begin,Pos0,Ws0,Ws,Ls0,Hs) :-
    potential_name_fsa_not_begin(unknown,Pos0,Ws0,Ws,Ls0,Hs).

potential_name_fsa(not_begin(Flag),Pos0,Ws0,Ws,Ls0,Hs) :-
    potential_name_fsa_not_begin(Flag,Pos0,Ws0,Ws,Ls0,Hs).

potential_name_fsa(begin,P0,[Word1|Words0],Words,[Word1|Used],[begin|His]) :-
    \+ ( Word1 = 'De', Words0 = [Ini|_], \+ name_initial(Ini) ), % De K. had samen met een vriend een man lastig gevallen die ...
    Word1 \= 'Het',
    Word1 \= '\'t',
    %%
    %% Kommer 't Mannetje
    %% Ton Sondergaard
    (   \+ tag(P0,_,_,_,_,Word1,normal(names_dictionary),_),
        tag(P0,_,_,_,_,Word1,normal(decap(ADJ)),_),
        ADJ \= 'Adj-s'
    ->  Flag=normal_if_decap  % be hesitant this is start of a name
    ;   Flag=unknown          % be confident this is start of a name
    ),
    potential_name_fsa(not_begin(Flag),P0,[Word1|Words0],Words,[Word1|Used],His),
    (   name_koning(Word1)
    ->  \+ a_person_name(Used)
    ;   true
    ).

%% state 1: just seen a title
potential_name_fsa(1,P0,[Word|Words],Ws,[Word|Prefix],[title|His]) :-
    name_title(Word),!,
    P1 is P0 + 1,
    potential_name_fsa(1,P1,Words,Ws,Prefix,His).
potential_name_fsa(1,P0,[Word|Words],Ws,[Word|Prefix],[vanhet1|His]) :-
    name_vanhet_maybe(Word),
    P1 is P0 + 1,
    potential_name_fsa(3,P1,Words,Ws,Prefix,His).
potential_name_fsa(1,P0,[Word|Words],Ws,[Word|Prefix],[vanhet1|His]) :-
    name_vanhet(Word),
    P1 is P0 + 1,
    potential_name_fsa(33,P1,Words,Ws,Prefix,His).
potential_name_fsa(1,P0,[Word1,Word|Words],Ws,[Word1,Word|Prefix],
		   [vanhet2|His]) :-
    name_vanhet_maybe(Word1,Word),
    P1 is P0 + 2,
    potential_name_fsa(3,P1,Words,Ws,Prefix,His).
potential_name_fsa(1,P0,[Word1,Word|Words],Ws,[Word1,Word|Prefix],
		   [vanhet2|His]) :-
    name_vanhet(Word1,Word),
    P1 is P0 + 2,
    potential_name_fsa(33,P1,Words,Ws,Prefix,His).
potential_name_fsa(1,P0,[Word|Words],Ws,[Word|Prefix],[capital|His]) :-
    name_capital(Word,P0),
    P1 is P0 + 1,
    potential_name_fsa(2,P1,Words,Ws,Prefix,His).
potential_name_fsa(1,P0,[Word|Words],Ws,[Word|Prefix],[baron|His]) :-
    name_baron(Word),
    P1 is P0 + 1,
    potential_name_fsa(2,P1,Words,Ws,Prefix,His).

%% state 11: just seen a firm
potential_name_fsa(11,P0,[Word|Words],Ws,[Word|Prefix],[title|His]) :-
    name_title(Word),!,
    P1 is P0 + 1,
    potential_name_fsa(11,P1,Words,Ws,Prefix,His).
potential_name_fsa(11,P0,[Word|Words],Ws,[Word|Prefix],[firma|His]) :-
    name_firma(Word),!,
    P1 is P0 + 1,
    potential_name_fsa(11,P1,Words,Ws,Prefix,His).
potential_name_fsa(11,P0,[Word|Words],Ws,[Word|Prefix],[capital|His]) :-
    name_capital(Word,P0),
    P1 is P0 + 1,
    potential_name_fsa(2,P1,Words,Ws,Prefix,His).

%% state 2: just seen capitalized token
potential_name_fsa(2,_P0,[et,'al.'|Words],Words,[et,'al.'],[etal]) :-
    !.
potential_name_fsa(2,_P0,[Junior|Words],Words,[Junior],[junior]) :-
    junior(Junior),
    !.
potential_name_fsa(2,_P0,[de,Rang|Words],Words,[de,Rang],[devijfde]) :-
    word_rang(Rang),
    !.
potential_name_fsa(2,P0,[Word|Words],Ws,[Word|Prefix],His) :-
    name_and(Word),
    foreign_word(Word),
    P1 is P0 + 1,
    His=[and|His1],
    potential_name_fsa(7,P1,Words,Ws,Prefix,His1).
potential_name_fsa(2,P0,[Word|Words],Ws,[Word|Prefix],[foreign|His]) :-
    unknown_foreign_word(Word),
    P is P0 + 1,
    potential_name_fsa(22,P,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[Word|Words],Ws,[Word|Prefix],[capital|His]) :-
    name_capital(Word,P0),!,
    P1 is P0 + 1,
    potential_name_fsa(2,P1,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[Word|Words],Ws,[Word|Prefix],[initial|His]) :-
    name_initial(Word),!,
    P1 is P0 + 1,
    potential_name_fsa(2,P1,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[Word|Words],Ws,[Word|Prefix],[initial|His]) :-
    name_small_initial(Word),!,
    P1 is P0 + 1,
    potential_name_fsa(2,P1,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[','|Words],Ws,[','|Prefix],[comma|His]) :-
    !,
    P is P0 + 1,
    potential_name_fsa(22,P,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[Word|Words],Ws,[Word|Prefix],[baron|His]) :-
    name_baron(Word),!,
    P1 is P0 + 1,
    potential_name_fsa(2,P1,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[Word|Words],Ws,[Word|Prefix],[vanhet1|His]) :-
    name_vanhet_maybe(Word),
    P1 is P0 + 1,
    potential_name_fsa(3,P1,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[Word|Words],Ws,[Word|Prefix],[vanhet1|His]) :-
    name_vanhet(Word),
    P1 is P0 + 1,
    potential_name_fsa(33,P1,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[de,'\''|Words],Ws,[de,'\''|Prefix],[de_quote|His]) :-
    %% Italiaanse namen Bastida de' Dossi
    %% mistokenized as  Bastida de ' Dossi
    P1 is P0 + 1,
    potential_name_fsa(33,P1,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[Word1,Word|Words],Ws,[Word1,Word|Prefix],
		   [vanhet2|His]) :-
    name_vanhet_maybe(Word1,Word),
    P1 is P0 + 2,
    potential_name_fsa(3,P1,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[Word1,Word|Words],Ws,[Word1,Word|Prefix],
		   [vanhet2|His]) :-
    name_vanhet(Word1,Word),
    P1 is P0 + 2,
    potential_name_fsa(33,P1,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[Word|Words],Ws,[Word|Prefix],[and|His]) :-
    name_and(Word),!,
    P is P0 + 1,
    potential_name_fsa(7,P,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[Word|Words],Ws,[Word|Prefix],[and|His]) :-
    Word=voor,  % de Commissie voor X en Y
    !,
    P is P0 + 1,
    potential_name_fsa(2,P,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[Word|Words],Ws,[Word|Prefix],[end_loc|His]) :-
    name_end_loc(Word),!,
    P is P0 + 1,
    potential_name_fsa(5,P,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[Word|Words],Ws,[Word|Prefix],[end_firma|His]) :-
    name_end_firma(Word),!,
    P is P0 + 1,
    potential_name_fsa(5,P,Words,Ws,Prefix,His).
%% Harewood-Leser produktie N.V.
potential_name_fsa(2,P0,[Word,Word1|Words],Ws,[Word,Word1|Prefix],[end_firma|His]) :-
    name_end_firma(Word1),!,
    P is P0 + 1,
    potential_name_fsa(5,P,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[',',Word|Words],Ws,[',',Word|Prefix],[end_firma|His]) :-
    name_end_firma(Word),!,
    P is P0 + 2,
    potential_name_fsa(5,P,Words,Ws,Prefix,His).
potential_name_fsa(2,P0,[Word|Words],Ws,[Word|Prefix],[number|His]) :-
    name_number(Word),!,
    P is P0 + 1,
    (  potential_name_fsa(5,P,Words,Ws,Prefix,His)
    ;  potential_name_fsa(8,P,Words,Ws,Prefix,His)
    ).

potential_name_fsa(2,P0,[Word|Words],Ws,[Word|Prefix],[unknown|His]) :-
    name_unknown(Word,P0),
    P is P0 + 1,
    potential_name_fsa(6,P,Words,Ws,Prefix,His).

potential_name_fsa(2,P0,[Open|Words],Ws,[Open|Prefix],[open|His]) :-
    start_quote_or_bracket(Open,Close),
    P is P0 + 1,
    potential_name_fsa(haakje(Close),P,Words,Ws,Prefix,His).

potential_name_fsa(haakje(Close),P0,[Word|Words],Ws,[Word|Prefix],
                   [capital|His]) :-
    name_capital(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(haakje2(Close),P,Words,Ws,Prefix,His).

potential_name_fsa(haakje(Close),P0,[Word|Words],Ws,[Word|Prefix],
                   [capital|His]) :-
    name_unknown(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(haakje2(Close),P,Words,Ws,Prefix,His).

potential_name_fsa(haakje2(Close),P0,[Word|Words],Ws,[Word|Prefix],
                   [capital|His]) :-
    name_capital(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(haakje2(Close),P,Words,Ws,Prefix,His).

potential_name_fsa(haakje2(Close),P0,[Word|Words],Ws,[Word|Prefix],
                   [capital|His]) :-
    name_unknown(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(haakje2(Close),P,Words,Ws,Prefix,His).

potential_name_fsa(haakje2(Close),P0,[Close|Words],Ws,[Close|Prefix],
                   [close|His]) :-
    P is P0 + 1,
    potential_name_fsa(2,P,Words,Ws,Prefix,His).

potential_name_fsa(haakje3(Close),P0,[Word|Words],Ws,[Word|Prefix],
                   [capital|His]) :-
    name_capital(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(haakje3(Close),P,Words,Ws,Prefix,His).

potential_name_fsa(haakje3(Close),P0,[Word|Words],Ws,[Word|Prefix],
                   [capital|His]) :-
    name_unknown(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(haakje3(Close),P,Words,Ws,Prefix,His).

potential_name_fsa(haakje3(Close),P0,[Close|Words],Ws,[Close|Prefix],
                   [close|His]) :-
    Prefix = [_|_],
    P is P0 + 1,
    potential_name_fsa(2,P,Words,Ws,Prefix,His).

%% Volumia!, Warning!, Yahoo!, GroenFront!, Oppassen!
potential_name_fsa(2,_,['!'|Words],Words,['!'],[uitroepteken]).

%% just seen vanhet
potential_name_fsa(3,P0,[Word|Words],Ws,[Word|Prefix],[capital|His]) :-
    name_capital(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(4,P,Words,Ws,Prefix,His),
    \+ not_a_second_name([Word|Prefix]).
potential_name_fsa(3,_,Ws,Ws,[],[]).  % ??

potential_name_fsa(33,P0,[Word|Words],Ws,[Word|Prefix],[capital|His]) :-
    name_capital(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(4,P,Words,Ws,Prefix,His).
potential_name_fsa(33,_,Ws,Ws,[],[]).

%% just seen capitalized normal word; we want another capital first to be convinced this
%% could be a name...
potential_name_fsa(8,P0,[Word|Words],Ws,[Word|Prefix],[capital|His]) :-
    name_unknown(Word,P0),
    name_capital(Word,P0),
    !,
    P is P0 + 1,
    potential_name_fsa(2,P,Words,Ws,Prefix,His).

potential_name_fsa(8,P0,[Word|Words],Ws,[Word|Prefix],[capital|His]) :-
    alpino_lex:in_names_dictionary(Tag,Word,_,[],[],_),
    Tag \= proper_name(both,'PER'),
    !,
    P is P0 + 1,    
    potential_name_fsa(2,P,Words,Ws,Prefix,His).

potential_name_fsa(8,P0,[Word|Words],Ws,[Word|Prefix],[capital|His]) :-
    name_capital(Word,P0),
    !,
    P is P0 + 1,
    potential_name_fsa(8,P,Words,Ws,Prefix,His).
potential_name_fsa(8,P0,[The,Word|Words],Ws,[The,Word|Prefix],[foreign,foreign|His]) :-
    (   The == the ; The == 'The'  ),
    !,
    P is P0 + 2,
    potential_name_fsa(222,P,Words,Ws,Prefix,His).
potential_name_fsa(8,P0,[Word|Words],Ws,[Word|Prefix],[foreign|His]) :-
    unknown_foreign_word(Word),
    !,
    P is P0 + 1,
    potential_name_fsa(22,P,Words,Ws,Prefix,His).

potential_name_fsa(8,P0,[Word|Words],Ws,[Word|Prefix],[foreign|His]) :-
    foreign_word(Word),
    !,
    P is P0 + 1,
    potential_name_fsa(23,P,Words,Ws,Prefix,His).

%% second part of name...
potential_name_fsa(4,_P0,[et,'al.'|Words],Words,[et,'al.'],[etal]) :-
    !.
potential_name_fsa(4,P0,[Word|Words],Ws,[Word|Prefix],[capital|His]) :-
    name_capital(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(4,P,Words,Ws,Prefix,His).
potential_name_fsa(4,P0,[Word|Words],Ws,[Word|Prefix],[number|His]) :-
    name_number(Word),!,
    P is P0 + 1,
    potential_name_fsa(5,P,Words,Ws,Prefix,His).
potential_name_fsa(4,P0,[Word|Words],Ws,[Word|Prefix],[end_firma|His]) :-
    name_end_firma(Word),!,
    P is P0 + 1,
    potential_name_fsa(5,P,Words,Ws,Prefix,His).
potential_name_fsa(4,P0,[Word|Words],Ws,[Word|Prefix],[end_loc|His]) :-
    name_end_loc(Word),!,
    P is P0 + 1,
    potential_name_fsa(5,P,Words,Ws,Prefix,His).
potential_name_fsa(4,P0,[',',Word|Words],Ws,[',',Word|Prefix],[end_firma|His]) :-
    name_end_firma(Word),!,
    P is P0 + 2,
    potential_name_fsa(5,P,Words,Ws,Prefix,His).
potential_name_fsa(4,P0,[Word|Words],Ws,[Word|Prefix],[end_person|His]) :-
    name_end_person(Word),!,
    P is P0 + 1,
    potential_name_fsa(8,P,Words,Ws,Prefix,His).
potential_name_fsa(4,P0,[Word|Words],Ws,[Word|Prefix],[vanhet1|His]) :-
    name_vanhet_maybe(Word),
    P is P0 + 1,
    potential_name_fsa(3,P,Words,Ws,Prefix,His).
potential_name_fsa(4,P0,[Word|Words],Ws,[Word|Prefix],[vanhet1|His]) :-
    name_vanhet(Word),
    P is P0 + 1,
    potential_name_fsa(33,P,Words,Ws,Prefix,His).
potential_name_fsa(4,P0,[Word1,Word|Words],Ws,[Word1,Word|Prefix],[vanhet2|His]) :-
    name_vanhet_maybe(Word1,Word),
    P is P0 + 2,
    potential_name_fsa(3,P,Words,Ws,Prefix,His).
potential_name_fsa(4,P0,[Word1,Word|Words],Ws,[Word1,Word|Prefix],[vanhet2|His]) :-
    name_vanhet(Word1,Word),
    P is P0 + 2,
    potential_name_fsa(33,P,Words,Ws,Prefix,His).
potential_name_fsa(4,P0,[Word|Words],Ws,[Word|Prefix],[unknown|His]) :-
    name_unknown(Word,P0),
    P is P0 + 1,
    potential_name_fsa(6,P,Words,Ws,Prefix,His).

potential_name_fsa(5,P0,[',',Word|Words],Ws,[',',Word|Prefix],[end_firma|His]) :-
    name_end_firma(Word),!,
    P is P0 + 2,
    potential_name_fsa(5,P,Words,Ws,Prefix,His).
potential_name_fsa(5,P0,[Word|Words],Ws,[Word|Prefix],[end_firma|His]) :-
    name_end_firma(Word),!,
    P is P0 + 1,
    potential_name_fsa(5,P,Words,Ws,Prefix,His).
potential_name_fsa(5,P0,[',',Word|Words],Ws,[','|Prefix],[postcode]) :-
    P1 is P0 + 1,
    postcode([Word|Words],P1,Ws,Prefix).
potential_name_fsa(5,P0,[Word|Words],Ws,Prefix,[postcode]) :-
    postcode([Word|Words],P0,Ws,Prefix).
potential_name_fsa(5,P0,[Word|Words],Ws,[Word|Prefix],[end_loc|His]) :-
    name_end_loc(Word),!,
    P is P0 + 1,
    potential_name_fsa(5,P,Words,Ws,Prefix,His).
potential_name_fsa(5,P0,[Word|Words],Ws,[Word|Prefix],[number|His]) :-
    name_number(Word),
    P is P0 + 1,
    potential_name_fsa(55,P,Words,Ws,Prefix,His).

potential_name_fsa(55,P0,[',',Word|Words],Ws,[',',Word|Prefix],[end_firma|His]) :-
    name_end_firma(Word),!,
    P is P0 + 2,
    potential_name_fsa(5,P,Words,Ws,Prefix,His).
potential_name_fsa(55,P0,[Word|Words],Ws,[Word|Prefix],[end_firma|His]) :-
    name_end_firma(Word),!,
    P is P0 + 1,
    potential_name_fsa(5,P,Words,Ws,Prefix,His).
potential_name_fsa(55,P0,[',',Word|Words],Ws,[','|Prefix],[postcode]) :-
    P1 is P0 + 1,
    postcode([Word|Words],P1,Ws,Prefix).
potential_name_fsa(55,P0,[Word|Words],Ws,Prefix,[postcode]) :-
    postcode([Word|Words],P0,Ws,Prefix).
potential_name_fsa(55,P0,[Word|Words],Ws,[Word|Prefix],[end_loc|His]) :-
    name_end_loc(Word),!,
    P is P0 + 1,
    potential_name_fsa(5,P,Words,Ws,Prefix,His).

potential_name_fsa(6,P0,[Word|Words],Ws,[Word|Prefix],[capital|His]) :-
    name_capital(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(6,P,Words,Ws,Prefix,His).
potential_name_fsa(6,P0,[Word|Words],Ws,[Word|Prefix],[unknown|His]) :-
    name_unknown(Word,P0),
    P is P0 + 1,
    potential_name_fsa(6,P,Words,Ws,Prefix,His).
potential_name_fsa(6,P0,[Open|Words],Ws,[Open|Prefix],[open|His]) :-
    start_quote_or_bracket(Open,Close),
    P is P0 + 1,
    potential_name_fsa(haakje2(Close),P,Words,Ws,Prefix,His).


%% just seen and
potential_name_fsa(7,P0,[Word|Words],Ws,[Word|Prefix],[capital|His]) :-
    name_capital(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(4,P,Words,Ws,Prefix,His).
potential_name_fsa(7,P0,[Word|Words],Ws,[Word|Prefix],[unknown|His]) :-
    name_unknown(Word,P0),  % co; the
    P is P0 + 1,
    potential_name_fsa(6,P,Words,Ws,Prefix,His).

%% just seen unknown foreign word
potential_name_fsa(22,P0,[The,Word|Words],Ws,[The,Word|Prefix],[foreign,foreign|His]) :-
    (   The == the ; The == 'The'  ),
    !,
    P is P0 + 2,
    potential_name_fsa(222,P,Words,Ws,Prefix,His).
potential_name_fsa(22,P0,[Word|Words],Ws,[Word|Prefix],[foreign|His]) :-
    decap_foreign_word(Word),
    !,
    P is P0 + 1,
    potential_name_fsa(222,P,Words,Ws,Prefix,His).
potential_name_fsa(22,P0,[','|Words],Ws,[','|Prefix],[comma|His]) :-
    !,
    P is P0 + 1,
    potential_name_fsa(23,P,Words,Ws,Prefix,His).
potential_name_fsa(22,P0,[Word|Words],Ws,[Word|Prefix],[unknown|His]) :-
    name_unknown(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(222,P,Words,Ws,Prefix,His).
potential_name_fsa(22,P0,[Word|Words],Ws,[Word|Prefix],[capital|His]) :-
    name_capital(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(222,P,Words,Ws,Prefix,His).
potential_name_fsa(22,P0,[Word|Words],Ws,[Word|Prefix],[and|His]) :-
    name_and(Word),!,
    P is P0 + 1,
    potential_name_fsa(7,P,Words,Ws,Prefix,His).
%potential_name_fsa(22,P0,[Word|Words],Ws,[Word|Prefix],[normal|His]) :-
%    \+ \+ alpino_lex:lexicon(_,_,[Word],[],_),
%    \+ wikipedia_list(Word),
%    % allow normal word 'in, of, ...'
%    P is P0 + 1,
%    potential_name_fsa(23,P,Words,Ws,Prefix,His).

%% just seen 'The'
potential_name_fsa(222,_,Ws,Ws,[],[]).
potential_name_fsa(222,_,['?'|Ws],Ws,['?'],[vraagteken]) :- !.
potential_name_fsa(222,_,['!'|Ws],Ws,['!'],[uitroepteken]) :- !.
potential_name_fsa(222,P0,Ws0,Ws,W0,His) :-
    potential_name_fsa(22,P0,Ws0,Ws,W0,His).

potential_name_fsa(23,_P0,[et,'al.'|Words],Words,[et,'al.'],[etal]) :-
    !.
potential_name_fsa(23,P0,[The,Word|Words],Ws,[The,Word|Prefix],[foreign,foreign|His]) :-
    (   The == the ; The == 'The'  ),
    !,
    P is P0 + 2,
    potential_name_fsa(222,P,Words,Ws,Prefix,His).
potential_name_fsa(23,P0,[Word|Words],Ws,[Word|Prefix],[foreign|His]) :-
    unknown_foreign_word(Word),!,
    P is P0 + 1,
    potential_name_fsa(222,P,Words,Ws,Prefix,His).
potential_name_fsa(23,P0,[Word|Words],Ws,[Word|Prefix],[foreign|His]) :-
    foreign_word(Word),!,
    P is P0 + 1,
    potential_name_fsa(23,P,Words,Ws,Prefix,His).
potential_name_fsa(23,P0,[','|Words],Ws,[','|Prefix],[comma|His]) :-
    !,
    P is P0 + 1,
    potential_name_fsa(23,P,Words,Ws,Prefix,His).
potential_name_fsa(23,P0,[Word|Words],Ws,[Word|Prefix],[unknown|His]) :-
    name_unknown(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(222,P,Words,Ws,Prefix,His).
potential_name_fsa(23,P0,[Word|Words],Ws,[Word|Prefix],[capital|His]) :-
    name_capital(Word,P0),!,
    P is P0 + 1,
    potential_name_fsa(23,P,Words,Ws,Prefix,His).
potential_name_fsa(23,P0,[Word|Words],Ws,[Word|Prefix],[capital_foreign|His]) :-
    decap_foreign_word(Word),!,
    P is P0 + 1,
    potential_name_fsa(23,P,Words,Ws,Prefix,His).
potential_name_fsa(23,_,['>',Word,Hellip|Ws],Ws,['>',Word,Hellip],['>']) :-
    hellip(Hellip),
    !.
potential_name_fsa(23,P0,['>',Word|Words],Ws,['>',Word|Prefix],['>'|His]) :-
    \+ Word='>',
    \+ Word='>>',
    !,
    P is P0 + 2,
    potential_name_fsa(222,P,Words,Ws,Prefix,His).
potential_name_fsa(23,P0,[Open|Words],Ws,[Open|Prefix],[open|His]) :-
    start_quote_or_bracket(Open,Close),
    P is P0 + 1,
    Prefix = [_,_,_|_],
    potential_name_fsa(haakje2(Close),P,Words,Ws,Prefix,His).

unknown_foreign_word(Word) :-
    foreign_word(Word),
    \+ common_dutch_word(Word).

common_dutch_word(de).
common_dutch_word(der).
common_dutch_word(die).
common_dutch_word(had).
common_dutch_word(in).
common_dutch_word(is).
common_dutch_word(je).
common_dutch_word(me).
common_dutch_word(men).
common_dutch_word(of).
common_dutch_word(over).
common_dutch_word(want).
common_dutch_word(was).
common_dutch_word(we).

junior(jr).
junior(sr).
junior('jr.').
junior('sr.').
junior('Jr').
junior('Sr').
junior('Jr.').
junior('Sr.').


word_rang(eerste).
word_rang(tweede).
word_rang(derde).
word_rang(vierde).
word_rang(vijfde).
word_rang(zesde).
word_rang(zevende).
word_rang(achtste).
word_rang(negende).
word_rang(tiende).
word_rang(elfde).
word_rang(twaalfde).
word_rang(dertiende).
word_rang(veertiende).
word_rang(vijftiende).
word_rang(zestiende).
word_rang(zeventiende).
word_rang(achttiende).
word_rang(negentiende).
word_rang(twintigste).
word_rang(eenentwintigste).
word_rang(tweeëntwintigste).
word_rang(drieëntwintigste).
word_rang(vierentwintigste).
word_rang(vijfentwintigste).
word_rang(zesentwintigste).
word_rang(zevenentwintigste).
word_rang(achtentwintigste).
word_rang(negenentwintigste).
word_rang(dertigste).

name_initial(Atom) :-
    atom(Atom),
    atom_codes(Atom,Codes),
    alpino_lex:is_initial_codes(Codes).

%% many tokenization mistakes in e.g. SONAR
name_initial(Atom,'.') :-
    atom(Atom),
    atom_codes(Atom,[Code]),
    isalpha(Code).

name_small_initial(Atom) :-
    atom(Atom),
    atom_codes(Atom,[A|"."]),
    islower(A).

name_unknown(Word,P0) :-
    atom(Word),
    P0 > -1,
    P is P0 + 1,
    \+ ( tag(P0,P,_,_,_,Word,His,_),
         His \= normal(gen(names_dictionary)),
         His \= normal('Adj-s')
       ),
    \+ wikipedia_list(Word),
    \+ alpino_lexical_analysis:hesitation(Word),
    \+ name_vanhet(Word),
    \+ name_and(Word),
    \+ name_title(Word),
    \+ name_firma(Word),
    \+ is_decap_only(Word),
    \+ longpunct(Word),
    \+ ( add_accents(Word,Stripped),
         alpino_lex:lexicon(_,_,[Stripped],[],_)
       ),
    \+ ( strip_accents(Word,Stripped),
         alpino_lex:lexicon(_,_,[Stripped],[],_)
       ).

longpunct(W) :-
    alpino_lex:punct(W,_).

name_capital(W,P0) :-
    atom(W),
    (   is_decap_only(W)
    ->  (   tag(P0,_,_,_,W,_,normal(names_dictionary),_)
	;   W == 'TV'
	)
    ;   true
    ),
    atom_codes(W,[F|T]),
    (	isupper(F)
    ->	true
    ;	(   F =:= 100		% d  l
	;   F =:= 108
	),
	T = [39,F2|_],		% 'CAP
	isupper(F2)             % d'Hondt d'Estaing d'Ancona l'Auto
    ->	true
    ;   islower(F),
	T = [N,_|_],
	isupper(N)
    ->  true                    % eBay, iPhone,  xBox
    ;   atom_concat('al-',_,W)
    ->  true			% al-Qaida
    ).				

starts_with_capital(W) :-
    atom(W),
    atom_codes(W,[F|_]),
    isupper(F).

name_baron(baron).	   % C.O.A. baron Schimmelpenninck van der Oye
name_baron(barones).
name_baron(graaf).
name_baron(gravin).
name_baron(ridder). % mr L.R.J. ridder van Rappard

name_baron(kardinaal).

name_vanhet(de,la).
name_vanhet(de,las).
name_vanhet(de,los).
name_vanhet(van,'\'t').
name_vanhet(van,den).
name_vanhet(van,der).
name_vanhet('v.','d.').
name_vanhet('v.','\'t').

name_vanhet_maybe(van,het).
name_vanhet_maybe(van,de).

name_vanhet_maybe_start(W) :-
    W \= van,
    W \= de,
    W \= het,
    name_vanhet_maybe(W).

name_vanhet_maybe_start(W0) :-
    decap_some(W0,W),
    name_vanhet_maybe(W).

name_vanhet_maybe_start(W1,W2) :-
    W1-W2 \= van-het,
    W1-W2 \= van-de,
    name_vanhet_maybe(W1,W2).

name_vanhet_maybe_start(X1,W2) :-
    decap_some(X1,W1),
    name_vanhet_maybe(W1,W2).

name_vanhet_start(W) :-
    W \= van,
    W \= de,
    W \= het,
    W \= der,
    name_vanhet(W).

name_vanhet_start(W0) :-
    decap_some(W0,W),
    name_vanhet(W).

name_vanhet_start(W1,W2) :-
    W1-W2 \= van-het,
    W1-W2 \= van-de,
    name_vanhet(W1,W2).

name_vanhet_start(X1,W2) :-
    decap_some(X1,W1),
    name_vanhet(W1,W2).

name_vanhet_maybe(van).
name_vanhet_maybe(de).

name_vanhet(von).
name_vanhet('v.').
name_vanhet('v.d.').
name_vanhet(den).
name_vanhet('d.').
name_vanhet(der).
name_vanhet(di).
name_vanhet(al).
name_vanhet(el).
name_vanhet(het).
name_vanhet(ten).
name_vanhet(ter).
name_vanhet('\'t').
name_vanhet(dos).
name_vanhet(du).
name_vanhet(da).
name_vanhet(von).
name_vanhet(del).
name_vanhet(la).
name_vanhet(le).
name_vanhet(las).
name_vanhet(los).
name_vanhet(vande). % Vlaams
name_vanhet(vanden). % Vlaams
name_vanhet(vander). % Vlaams
name_vanhet(vd).

name_and('&').
name_and(and).
name_and(et).
name_and(und).
name_and(en).
name_and(of).

name_end_firma('co').
name_end_firma('co.').
name_end_firma('bv').
name_end_firma('bv.').
name_end_firma('b.v.').
name_end_firma('inc').
name_end_firma('inc.').
name_end_firma('nv').
name_end_firma('nv.').
name_end_firma('n.v.').
name_end_firma(gmbh).
name_end_firma('GmbH').


name_end_firma('Co').
name_end_firma('Co.').
name_end_firma('BV').
name_end_firma('BV.').
name_end_firma('B.V.').
name_end_firma('Inc').
name_end_firma('Inc.').
name_end_firma('NV').
name_end_firma('NV.').
name_end_firma('N.V.').

name_end_loc('län').
name_end_loc('Län').

name_end_person('RA').  % register accountant
name_end_person('BA').
name_end_person('MA').
name_end_person('Ph.D.').

name_number(N) :-
    atom(N),
    N \== '--',
    N \== '...',
    guess_number(N).

name_title(Atom0) :-
    decap(Atom0,Atom),
    name_title0(Atom).

%% todo: mevr de wed. J. Jansen

name_title0(dr).
name_title0(dhr).
name_title0(prof).
name_title0(drs).
name_title0(ds).
name_title0(ing).
name_title0(ir).
name_title0(jhr).
name_title0(mej).
name_title0(mw).
name_title0(mevr).
name_title0(mgr).
name_title0(mr).
name_title0(mrs).
name_title0(wed).   % weduwe

name_title0('dr.').
name_title0('dhr.').
name_title0('prof.').
name_title0('prof.dr').
name_title0('drs.').
name_title0('ds.').
name_title0('ing.').
name_title0('ir.').
name_title0('jhr.').
name_title0('mej.').
name_title0('mw.').
name_title0('mevr.').
name_title0('mgr.').
name_title0('mr.').
name_title0('mrs.').
name_title0('wed.').   % weduwe

name_title0(sir).

name_firma(Atom0) :-
    decap(Atom0,Atom),
    name_firma0(Atom).

name_firma0('bv').
name_firma0('bv.').
name_firma0('b.v.').
name_firma0('nv').
name_firma0('n.v.').
name_firma0('fa').    % de fa. N. Jansen (firma)
name_firma0('fa.').   % de fa. N. Jansen (firma)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% QUOTED NAMES %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_quoted_names(Input,P0,P,R0,R) :-
    (	lexical_analysis_quoted_name(Input,P0,P,R0,R),
	fail
    ;	true
    ).

lexical_analysis_quoted_name(List0,P0,P,R0,R) :-
    skip_bracket_next(List0,List,P0,P1,R0,R1),
    lexical_analysis_quoted_name__(List,P1,P,R1,R).

lexical_analysis_quoted_name__([H1,H2|T],P0,P,R0,_R) :-
    start_quote(H1,H2,E1,E2),
    P1 is P0 + 2,
    list_of_words_ending_in_quote2(T,P1,P2,Ws,E1,E2),
    R1 is R0 + 2,
    R is R1 + (P2-P1),
    concat_all(Ws,Label,' '),
    \+ (   punct_only(0,P1),	% otherwise too many false hits
	   punct_only(P2,P)
       ),			% so " kjals , lkjasdlfj " . is not a name
    \+ tag(P1,P2,R1,R,_,_,_,proper_name(_,_)),
    \+ tag(P1,P2,R1,R,_,_,_,proper_name(_)),
    assert_tag(P1,P2,R1,R,Label,quoted_name(H1,H2,E1,E2),proper_name(both)).

lexical_analysis_quoted_name__([H|T],P0,P,R0,_R) :-
    start_quote(H,E),
    P1 is P0+1,
    list_of_words_ending_in_quote(T,P1,P2,Ws,E),
    R1 is R0 + 1,
    R is R1 + (P2-P1),
    concat_all(Ws,Label,' '),
    \+ tag(P1,P2,R1,R,_,_,_,proper_name(_)),
    \+ tag(P1,P2,R1,R,_,_,_,proper_name(_,_)),
    (   H == '«'
    ->  true
    ;   \+ (  punct_only(0,P1),	% otherwise too many false hits
	      punct_only(P2,P)) % so " kjals , lkjasdlfj " .
				% is not a name
    ),
    assert_tag(P1,P2,R1,R,Label,quoted_name(H,E),proper_name(both)).

lexical_analysis_quoted_name__([_H|T],P0,P,R0,R) :-
    R1 is R0+1,
    P1 is P0+1,
    lexical_analysis_quoted_name(T,P1,P,R1,R).

punct_only(P,P).
punct_only(P0,P) :-
    tag(P0,P1,_,_,_,_,_,punct(_)),
    punct_only(P1,P).

start_quote_or_bracket('(',')').
start_quote_or_bracket(Open,Close) :-
    start_quote(Open,Close).

start_quote('"','"'). 
start_quote('`','\''). 
start_quote('`','`'). 
start_quote('\'','\''). 
start_quote('``','\'\''). 
start_quote('\'\'','\'\'').
start_quote(',,','\'\'').
start_quote('‘','’').
start_quote('“','”').
start_quote('<','>').
start_quote('«','»').
start_quote('“','"').

start_quote('`','`','\'','\'').
start_quote(',',',','\'','\'').
start_quote(‘,’,’,’).
start_quote(’,’,’,’).

allow_final_punctuation_in_quote('>').
allow_final_punctuation_in_quote('»').

illegal_word_in_quoted_name(bracket(_)).
illegal_word_in_quoted_name('(').
illegal_word_in_quoted_name(')').
illegal_word_in_quoted_name('`').
illegal_word_in_quoted_name('\'').
illegal_word_in_quoted_name('"').  %"
illegal_word_in_quoted_name(’).
illegal_word_in_quoted_name(‘).

illegal_final_word_in_quoted_name(',').
illegal_final_word_in_quoted_name('.').
illegal_final_word_in_quoted_name('?').
illegal_final_word_in_quoted_name('!').
illegal_final_word_in_quoted_name(';').
illegal_final_word_in_quoted_name('...').

list_of_words_ending_in_quote([W|Ws],P0,P,Rs,E) :-
    (   E == '»'
    ->  true
    ;   \+ illegal_word_in_quoted_name(W)
    ),
    Rs = [W|Rs1],
    P1 is P0+1,
    list_of_words_ending_in_quote_(Ws,P1,P,Rs1,E,W).

list_of_words_ending_in_quote_([W|Ws],P0,P,Rs,E,Prev) :-
    (	W \== E
    ->	(   E == '»'
	->  true
	;   \+ illegal_word_in_quoted_name(W)
	),
        Rs = [W|Rs1],
	P1 is P0+1,
	list_of_words_ending_in_quote_(Ws,P1,P,Rs1,E,W)
    ;	(   allow_final_punctuation_in_quote(E)
        ->  true
        ;   \+ illegal_final_word_in_quoted_name(Prev)
        ),
        P0 = P,
	Rs = []
    ).

list_of_words_ending_in_quote2([W|Ws],P0,P,Rs,E0,E) :-
    (   E == '»'
    ->  true
    ;   \+ illegal_word_in_quoted_name(W)
    ),
    Rs = [W|Rs1],
    P1 is P0+1,
    list_of_words_ending_in_quote_2(Ws,P1,P,Rs1,E0,E,W).

list_of_words_ending_in_quote_2([W,W1|Ws],P0,P,Rs,E0,E,Prev) :-
    (	W \== E0
    ->	(   E0 == '»'
	->  true
	;   \+ illegal_word_in_quoted_name(W)
	),
        Rs = [W|Rs1],
	P1 is P0+1,
	list_of_words_ending_in_quote_2([W1|Ws],P1,P,Rs1,E0,E,W)
    ;	W  == E0,
	W1 == E,
        \+ illegal_final_word_in_quoted_name(Prev),
        P = P0,
	Rs = []
    ).

%% skip_bracket(W0,Tail,Tail1,+P0,P,+R0,R)
%% in [W0|Tail],  Tail1 is the list starting with first non-bracket (if any)
%% current string positions: P0 (real), R0 (virtual)
%% P, R are string positions of W

skip_bracket(bracket(open),T,T1,P0,P,R0,R) :-
    !,
    P1 is P0 + 1,
    skip_bracket_type(T,T1,P1,P,R0,R).
skip_bracket(bracket(close),T,T1,P0,P,R0,R) :-
    !,
    P1 is P0 + 1,
    skip_bracket_next(T,T1,P1,P,R0,R).
skip_bracket(W,T,[W|T],P,P,R,R).

skip_bracket_type([],[],P,P,R,R).
skip_bracket_type([H|T],T1,P0,P,R0,R) :-
    (   atom(H),
        atom_codes(H,[C|_String]),
        C =:= 64   % @
    ->  P1 is P0 + 1,
	skip_bracket_next(T,T1,P1,P,R0,R)
    ;   skip_bracket_next([H|T],T1,P0,P,R0,R)
    ).

skip_bracket_next([],[],P,P,R,R).
skip_bracket_next([H|T],T2,P0,P,R0,R) :-
    skip_bracket(H,T,T2,P0,P,R0,R).

%% typically mis-parsed as part of person-names
%% "Els Borst van Volskgezondheid"

not_a_second_name(Words) :-
    alpino_lex:lexicon(proper_name(_,'ORG'),_,Words,[],names_dictionary),
    \+ alpino_lex:lexicon(proper_name(_,'LOC'),_,Words,[],names_dictionary).

%% Pieters van de Nederlandse ...
not_a_second_name([Word]) :-
    alpino_lex:lexicon(adjective(_),_,[Word],[],normal).

%% J.Pieters van de Belgische Kamer van Koophandel
not_a_second_name([Word|Words]) :-
    alpino_lex:lexicon(adjective(_),_,[Word],[],normal),
    alpino_lex:lexicon(proper_name(_,'ORG'),_,Words,[],names_dictionary).

not_a_second_name([De0|Words]) :-
    decap_some(De0,De),
    ( De=de ; De = het ),
    alpino_lex:lexicon(proper_name(_,'ORG'),_,Words,[],names_dictionary).

not_a_second_name([Word|_]) :-
    not_a_second_name_word(Word).

not_a_second_name_word(Word) :-
    atom(Word),
    atom_codes(Word,[A,B|_]),
    isupper(A),
    isupper(B).

not_a_second_name_word('Barcelona').
not_a_second_name_word('Binnenlandse').
not_a_second_name_word('Champions').
not_a_second_name_word('Economische').
not_a_second_name_word('Grotesteden-').
not_a_second_name_word('Noord-Holland').
not_a_second_name_word('Sociale').
not_a_second_name_word('Sydney').
not_a_second_name_word('The').
not_a_second_name_word('Volksgezondheid').
not_a_second_name_word('Zuid-Holland').

%decap_normal(Word) :-
%    decap_first(Word,Decap),
%    alpino_lex:lexicon(_,_,[Decap],[],normal).

verb_ster(W,W,noun(de,count,sg)):-
    atom(W),
    atom_concat(Verb,ster,W),
    \+ \+ (  alpino_lex:lexicon(verb(_,Sg,_),_,[Verb],[],_),
	      lists:member(Sg,[sg1,sg])
	  ).

verb_ster(W,W,noun(de,count,pl)):-
    atom(W),
    atom_concat(Verb,sters,W),
    \+ \+ (   alpino_lex:lexicon(verb(_,Sg,_),_,[Verb],[],_),
	      lists:member(Sg,[sg1,sg])
	  ).

diminutive(W,Stam,Tag) :-
    atom(W),
    parts_diminutive(W,Stam0,Agr),
    alpino_lex:lexicon(Tag0,Stam,[Stam0],[],_),
    dif(Agr0,pl),
    add_dim_tag(Tag0,Agr0,Tag,Agr).

parts_diminutive(W,Stam,sg) :-
    atom(W),
    atom_concat(_,je,W),
    parts_diminutive(W,Stam).

parts_diminutive(W,Stam,pl) :-
    atom_concat(W1,s,W),
    atom_concat(_,je,W1),
    parts_diminutive(W1,Stam).

parts_diminutive(W,Stam) :-
    atom_concat(W0,inkje,W),
    atom_concat(W0,ing,  Stam).
parts_diminutive(W,Stam) :-
    atom_concat(W0,mpje,W),
    atom_concat(W0,m,  Stam).
parts_diminutive(W,Stam) :-
    atom_concat(W0,ggetje,W),
    atom_concat(W0,g,  Stam).
parts_diminutive(W,Stam) :-
    atom_concat(W0,lletje,W),
    atom_concat(W0,l,  Stam).
parts_diminutive(W,Stam) :-
    atom_concat(W0,nnetje,W),
    atom_concat(W0,n,  Stam).
parts_diminutive(W,Stam) :-
    atom_concat(W0,rretje,W),
    atom_concat(W0,r,  Stam).
parts_diminutive(W,Stam) :-
    atom_concat(W0,aatje,W),
    atom_concat(W0,a,  Stam).
parts_diminutive(W,Stam) :-
    atom_concat(W0,eetje,W),
    atom_concat(W0,e,  Stam).
parts_diminutive(W,Stam) :-
    atom_concat(W0,ietje,W),
    atom_concat(W0,i,  Stam).
parts_diminutive(W,Stam) :-
    atom_concat(W0,ietje,W),
    atom_concat(W0,ie, Stam).
parts_diminutive(W,Stam) :-
    atom_concat(W0,ootje,W),
    atom_concat(W0,o,  Stam).
parts_diminutive(W,Stam) :-
    atom_concat(W0,uutje,W),
    atom_concat(W0,u,  Stam).
parts_diminutive(W,Stam) :-
    atom_concat(Stam,tje,W),
    (   atom_concat(_,w,  Stam)
    ;   atom_concat(_,au, Stam)
    ;   atom_concat(_,ee, Stam)
    ;   atom_concat(_,eu, Stam)
    ;   atom_concat(_,oe, Stam)
    ;   atom_concat(_,ou, Stam)
    ;   atom_concat(_,ei, Stam)
    ;   atom_concat(_,ij, Stam)
    ;   atom_concat(_,aun, Stam)
    ;   atom_concat(_,een, Stam)
    ;   atom_concat(_,eun, Stam)
    ;   atom_concat(_,oen, Stam)
    ;   atom_concat(_,oun, Stam)
    ;   atom_concat(_,ein, Stam)
    ;   atom_concat(_,ijn, Stam)
    ).
parts_diminutive(W,Stam) :-
    atom_concat(Stam,je,W),
    (   atom_concat(_,b,Stam)
    ;   atom_concat(_,d,Stam)
    ;   atom_concat(_,f,Stam)
    ;   atom_concat(_,g,Stam)
    ;   atom_concat(_,k,Stam)
    ;   atom_concat(_,p,Stam)
    ;   atom_concat(_,s,Stam)
    ;   atom_concat(_,t,Stam)
    ).

add_dim_tag(noun(_,_,O),O,              noun(het,count,Num),    Num).
add_dim_tag(noun(_,_,O,Sc),O,           noun(het,count,Num,Sc), Num) :- check_pred_pp(Sc,Num).
add_dim_tag(tmp_noun(_,_,O),O,          tmp_noun(het,count,Num),    Num).
add_dim_tag(tmp_noun(_,_,O,Sc),O,       tmp_noun(het,count,Num,Sc), Num) :- check_pred_pp(Sc,Num).
add_dim_tag(mod_noun(_,_,O),O,          mod_noun(het,count,Num),    Num).
add_dim_tag(mod_noun(_,_,O,Sc),O,       mod_noun(het,count,Num,Sc), Num) :- check_pred_pp(Sc,Num).
add_dim_tag(meas_mod_noun(_,_,O),O,     meas_mod_noun(het,count,Num),    Num).
add_dim_tag(meas_mod_noun(_,_,O,Sc),O,  meas_mod_noun(het,count,Num,Sc), Num) :- check_pred_pp(Sc,Num).

check_pred_pp(SC,Agr) :-
    (  (   SC = pred_pp(_)
       ;   SC = pred_pp(_,_)
       )
    -> Agr \= pl
    ;  (   SC = pred_pp_pl(_)
       ;   SC = pred_pp_pl(_,_)
       )
    -> Agr = pl
    ;  true
    ).    

postcode(Ws0,P0,Ws,Prefix) :-
    pc(P0,P1,Ws0,Ws1),
    plaats(P1,Ws1,Ws),
    append(Prefix,Ws,Ws0).

pc(P0,P) -->
    opt_nl(P0,P1),
    [Code],
    { pc_string(Code),
      P is P1 + 1 }.

pc(P0,P) -->
    opt_nl(P0,P1),
    [Code1],
    [Code2],
    { pc_string(Code1,Code2),
      P is P1 + 2 }.

pc(P,P) --> [].

opt_nl(P0,P) -->
    ['NL'],
    { P is P0 + 1 }.
opt_nl(P,P) --> [].

%% NL
pc_string(Atom) :-
    atom(Atom),
    atom_codes(Atom,[A,B,C,D,E,F]),
    isdigit(A),
    isdigit(B),
    isdigit(C),
    isdigit(D),
    isupper(E),
    isupper(F).

%% B
pc_string(Atom) :-
    atom(Atom),
    atom_codes(Atom,[A,B,C,D]),
    isdigit(A),
    isdigit(B),
    isdigit(C),
    isdigit(D).    
pc_string(Atom) :-
    atom(Atom),
    atom_codes(Atom,[66,45,A,B,C,D]),   % B-1234
    isdigit(A),
    isdigit(B),
    isdigit(C),
    isdigit(D).
%% D/F
pc_string(Atom) :-
    atom(Atom),
    atom_codes(Atom,[A,B,C,D,E]),   % 12345
    isdigit(A),
    isdigit(B),
    isdigit(C),
    isdigit(D),
    isdigit(E).    
pc_string(Atom) :-
    atom(Atom),
    atom_codes(Atom,[X,45,A,B,C,D,E]),   % D-12345
    ( X = 68 ; X = 70 ),
    isdigit(A),
    isdigit(B),
    isdigit(C),
    isdigit(D),
    isdigit(E).    

%% NL
pc_string(Atom1,Atom2) :-
    atom(Atom1),
    atom(Atom2),
    atom_codes(Atom1,[A,B,C,D]),
    atom_codes(Atom2,[E,F]),
    isdigit(A),
    isdigit(B),
    isdigit(C),
    isdigit(D),
    isupper(E),
    isupper(F).

pc_string(Atom1,Atom2) :-
    atom(Atom1),
    atom(Atom2),
    atom_concat('NL-',Atom1a,Atom1),
    atom_codes(Atom1a,[A,B,C,D]),
    atom_codes(Atom2,[E,F]),
    isdigit(A),
    isdigit(B),
    isdigit(C),
    isdigit(D),
    isupper(E),
    isupper(F).

%% B
pc_string('B-',Atom2) :-
    atom(Atom2),
    atom_codes(Atom2,[A,B,C,D]),
    isdigit(A),
    isdigit(B),
    isdigit(C),
    isdigit(D).
pc_string('BE-',Atom2) :-
    atom(Atom2),
    atom_codes(Atom2,[A,B,C,D]),
    isdigit(A),
    isdigit(B),
    isdigit(C),
    isdigit(D).

%% D
pc_string('D-',Atom2) :-
    atom(Atom2),
    atom_codes(Atom2,[A,B,C,D,E]),
    isdigit(A),
    isdigit(B),
    isdigit(C),
    isdigit(D),
    isdigit(E).

plaats(P0,Ws0,Ws) :-
    plaats0(P0,P),
    L is P-P0,
    length(Prefix,L),
    append(Prefix,Ws,Ws0).

plaats0(P0,P) :-
    tag(P0,P1,_,_,_,_,_,proper_name(_,'LOC')),
    opt_land(P1,P).

opt_land(P,P).
opt_land(P0,P) :-
    tag(P0,P1,_,_,_,_,_,punct(komma)),
    plaats0(P1,P).
opt_land(P0,P) :-
    plaats0(P0,P).

verbal_compound_prefix(her).

verbal_compound_prefix('over-',over,intransitive,refl).
verbal_compound_prefix(over,over,intransitive,refl).

strip_repeated_letters([],[],I,I).
strip_repeated_letters([H,H|Hs],[H],_,1) :-
    hs(Hs,H),
    !.
strip_repeated_letters([H,H,H|T0],[H,H|T],_,1) :-
    alpino_latin1:isalpha(H),
    !,
    repeated(T0,H,T1),
    strip_repeated_letters(T1,T,_,1).
strip_repeated_letters([H|T0],[H|T],I0,I) :-
    strip_repeated_letters(T0,T,I0,I).

hs([],_).
hs([H|T],H) :-
    hs(T,H).

repeated([],_,[]).
repeated([H0|T],H,T1) :-
    (   H0 == H
    ->  repeated(T,H,T1)
    ;   T1 = [H0|T]
    ).

y_ij_letters([],[],I,I).
y_ij_letters([121|T0],[105,106|T],_,1) :-
    !,
    y_ij_letters(T0,T,_,1).
y_ij_letters([H|T0],[H|T],I0,I) :-
    y_ij_letters(T0,T,I0,I).

ij_ei_letters([],[],I,I).
ij_ei_letters([105,106|T0],[101,105|T],_,1) :-
    !,
    ij_ei_letters(T0,T,_,1).
ij_ei_letters([H|T0],[H|T],I0,I) :-
    ij_ei_letters(T0,T,I0,I).

ei_ij_letters([],[],I,I).
ei_ij_letters([101,105|T0],[105,106|T],_,1) :-
    !,
    ei_ij_letters(T0,T,_,1).
ei_ij_letters([H|T0],[H|T],I0,I) :-
    ei_ij_letters(T0,T,I0,I).

g_ch_letters([],[],I,I).
g_ch_letters([103|T0],[99,104|T],_,1) :-
    !,
    g_ch_letters(T0,T,_,1).
g_ch_letters([H|T0],[H|T],I0,I) :-
    g_ch_letters(T0,T,I0,I).

%% +TweeHonderdTachtig +Honderd ?Twee ?Tachtig).
%% more efficient than 2x atom_concat
%% and we generate candidates from right to left,
%% rather than vice versa, because that is more
%% often what we want "Kon-Tiki-expeditie"
atom_split(Tweehonderdtachtig,Honderd,Twee,Tachtig) :-
    findall(Before-After,sub_atom(Tweehonderdtachtig,Before,_,After,Honderd),List),
    reverse(List,List0),
    member(Before-After,List0),
    sub_atom(Tweehonderdtachtig,0,Before,_,Twee),
    sub_atom(Tweehonderdtachtig,_,After,0,Tachtig).


%% breaks down atom W into Prefix and Suffix, such that
%% atom Prefix has length at least 1, at most MaxPrefix
%% atom Suffix has length at least 2
atom_pair_lengths(W,Prefix,Suffix,MaxPrefix) :-
%    atom_length(W,Wlen),
%    (   Wlen < MaxPrefix
%    ->  L = Wlen
%    ;   L = MaxPrefix
%    ),
    between(1,MaxPrefix,Len),
    sub_atom(W,0,Len,_,Prefix),
    atom_concat(Prefix,Suffix,W),
    atom_length(Suffix,SuffixLen),
    SuffixLen>2.

replace_part(v_root(A,B),Root,Part0,Part) :-
    !,
    Root = v_root(A1,B1),
    replace_root(A,A1,Part0,Part),
    replace_lemma(B,B1,Part0,Part).

replace_part(Root0,Root,Part0,Part) :-
    hdrug_util:hdrug_flag(root_of_verb_uses_inf,on),
    !,
    replace_lemma(Root0,Root,Part0,Part).

replace_part(Root0,Root,Part0,Part) :-
    replace_root(Root0,Root,Part0,Part).

replace_root(Root0,Root,Part0,Part) :-
    atom(Root0),
    atom(Part0),
    atom_concat(Prefix,Part0,Root0),
    atom_concat(Prefix,Part,Root).

replace_lemma(Lemma0,Lemma,Part0,Part):-
    atom(Lemma0),
    atom(Part0),
    atom_concat(Part0,Suffix,Lemma0),
    atom_concat(Part,Suffix,Lemma).

hellip('...').
hellip('..').

psp_variant(Geprobeert,verb(HZ,psp,Sc),Stem) :-
    atom(Geprobeert),
    atom_concat(Geprobeer,t,Geprobeert),
    atom_concat(Geprobeer,d,Geprobeerd),
    alpino_lex:lexicon___(Geprobeerd,verb(HZ,psp,Sc),Stem,[],[],_).

psp_variant(Geprobeerdt,verb(HZ,psp,Sc),Stem) :-
    atom(Geprobeerdt),
    atom_concat(Geprobeer,dt,Geprobeerdt),
    atom_concat(Geprobeer,d,Geprobeerd),
    alpino_lex:lexicon___(Geprobeerd,verb(HZ,psp,Sc),Stem,[],[],_).

psp_variant(Gefeesd,verb(HZ,psp,Sc),Stem) :-
    atom(Gefeesd),
    atom_concat(Gefees,d,Gefeesd),
    atom_concat(Gefees,t,Gefeest),
    alpino_lex:lexicon___(Gefeest,verb(HZ,psp,Sc),Stem,[],[],_).

psp_variant(Gefeesdt,verb(HZ,psp,Sc),Stem) :-
    atom(Gefeesdt),
    atom_concat(Gefees,dt,Gefeesdt),
    atom_concat(Gefees,t,Gefeest),
    alpino_lex:lexicon___(Gefeest,verb(HZ,psp,Sc),Stem,[],[],_).

%% change 0-1 into 0-2, and increase other numbers by 1
adapt_with_dt(dt(Cat,L0), dt(Cat,L)) :-
    adapt_with_dt_l(L0,L).
adapt_with_dt(l(Lemma,Tag,P0,P),l(Lemma,Tag,Q0,Q)):-
    adapt_with_dt_pos(P0,P,Q0,Q).
adapt_with_dt(l(Lemma,Tag,Cat,P0,P),l(Lemma,Tag,Cat,Q0,Q)):-
    adapt_with_dt_pos(P0,P,Q0,Q).
adapt_with_dt(ix(Ix),ix(Ix)).
adapt_with_dt(ix(Ix,DT0),ix(Ix,DT)):-
    adapt_with_dt(DT0,DT).

adapt_with_dt_pos(P0,P,Q0,Q):-
    (    P0 == 0, P == 1
    ->   Q0 = 0, Q = 2
    ;    Q0 is P0 + 1,
	Q is P + 1
    ).

adapt_with_dt_l([],[]).
adapt_with_dt_l([Rel=DT0|L0],[Rel=DT|L]):-
    adapt_with_dt(DT0,DT),
    adapt_with_dt_l(L0,L).

construct_root(verb(_,Inf,_),Suffix,Eer/Eren,W,v_root(Prefeer,Preferen)) :-
    !,
    atom_concat(Pref0,Suffix,W),
    remove_ge_if_psp(Inf,Pref0,Pref),
    atom_concat(Pref,Eer,Prefeer0),
    atom_concat(Pref,Eren,Preferen0),
    decap(Prefeer0,Prefeer),
    decap(Preferen0,Preferen).
construct_root(_,Suffix,RootSuffix,W,Root) :-
    atom_concat(Pref,Suffix,W),
    atom_concat(Pref,RootSuffix,Root0),
    (   only_capitals(Pref,_)
    ->  Root0 = Root
    ;   alpino_lex:in_names_dictionary(_,Pref,_,[],[],_)
    ->  Root0 = Root
    ;   decap_first(Root0,Root)
    ->  true
    ;   Root0 = Root
    ).

remove_ge_if_psp(psp,Stem0,Stem) :-
    atom(Stem0),
    atom_concat(ge,Stem,Stem0),
    !.
remove_ge_if_psp(_,Stem,Stem).

%% typical_spelling_mistake_suffixes(T,D)
%% Word-T does not exist,
%% Word-D does exist
typical_spelling_mistake_suffixes(t,d).      % ondervont -> ondervond
typical_spelling_mistake_suffixes(dt,t).     % bemindt   -> bemint
typical_spelling_mistake_suffixes(tte,te).   % wenstte   -> wenste
typical_spelling_mistake_suffixes(tten,ten). % wenstten  -> wensten
typical_spelling_mistake_suffixes(dde,de).   % bloosdde  -> bloosde
typical_spelling_mistake_suffixes(dden,den). % bloosdden -> bloosden

spelling_variant_compound(HandelsAccoord,HandelsAkkoord,P0,R0,Frame) :-
    atom(HandelsAccoord),
    P is P0 + 1,
    R is R0 + 1,
    atom_concat(Handels,Accoord,HandelsAccoord),
    alpino_lex:spelling_variant(Accoord,Akkoord),
    atom_concat(Handels,Akkoord,HandelsAkkoord),
    alpino_lex:lexicon(Frame,Stem,[HandelsAkkoord],[],_),
    guess_proper_compound(HandelsAkkoord,Akkoord,_,2),
    assert_tag(P0,P,R0,R,Stem,spelling_variant_compound(Akkoord),Frame).

ld_dir(ld_dir,intransitive,part_intransitive(Voorover),Voorover).
ld_dir(refl_ld_dir,refl,part_refl(Voorover),Voorover).
ld_dir(np_ld_dir,transitive,part_transitive(Voorover),Voorover).

add_accents(W,Wmin) :-
    alpino_lex:accent(W,Wmin).
add_accents(W,Wmin) :-
    accent(W,Wmin).  % from names

