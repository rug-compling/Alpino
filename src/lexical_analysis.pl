:- module(alpino_lexical_analysis, [ lexical_analysis/1,
				     lex_all/0, lex_all_keys/0,
				     tag_all/0, tag_all_keys/0,
				     do_one/2,
				     do_one/3,
				     default_frames/2,
				     remove_brackets/2
				   ]).

:- expects_dialect(sicstus).

:- use_module(hdrug(hdrug_util)).
:- use_module(hdrug(hdrug_gui)).
:- use_module(utils).
:- use_module(latin1).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(charsio), [ format_to_chars/4 ]).

:- meta_predicate tagtime(:,?), tagtime(:,?,?).

:- use_module(filter_tag).
:- use_module(unknowns).

:- dynamic
    add_lex_table/5,
    add_tag_table/5,
    add_skip_word/2,
    word_form/1,
    tag/8,
    copy_tag/8,
    rpos/2,
    open_bracket/3, close_bracket/3,
    syn_lex_analysis/4, syn_lex_analysis/6, syn_sem_lex_analysis/10,
    tag_nr/2,
    user_skips/1,
    skips/3,
    reachable/1,
    co_reachable/1,
    index_tag_r0/2,
    index_tag_tag/2,
    index_tag_stem/2,
    alpino_score_ref:score_ref/2.

:- thread_local
    add_lex_table/5,
    add_tag_table/5,
    add_skip_word/2,
    word_form/1,
    tag/8,
    reachable/1,
    co_reachable/1,
    open_bracket/3, close_bracket/3,
    syn_lex_analysis/4, syn_lex_analysis/6, syn_sem_lex_analysis/10,
    tag_nr/2,
    user_skips/1,
    skips/3,
    index_tag_r0/2,
    index_tag_tag/2,
    index_tag_stem/2,
    alpino_score_ref:score_ref/2.

%% Alpino lexical analysis; including unknown word guessing.

lexical_analysis_cleanup :-
    %% cleanup stuff from previous parse:
    retractall(alpino_score_ref:score_ref(_,_)),
    retractall(alpino_lex:m_next_word(_,_,_,_,_)),
    retractall(alpino_lex:found_number_expression(_,_,_)),
    retractall(alpino_lex:found_number_expression_word(_,_,_)),
    retractall(word_form(_)),
    retractall(rpos(_,_)),
    retractall(tag(_,_,_,_,_,_,_,_)),
    retractall(copy_tag(_,_,_,_,_,_,_,_)),
%%    retractall(cgn_tag(_,_,_)),
    retractall(index_tag_tag(_,_)),
    retractall(index_tag_stem(_,_)),
    retractall(index_tag_r0(_,_)),
    retractall(skips(_,_,_)),
    retractall(open_bracket(_,_,_)),
    retractall(close_bracket(_,_,_)),
    retractall(reachable(_)),
    retractall(co_reachable(_)),
    retractall(syn_lex_analysis(_,_,_,_)),
    retractall(syn_lex_analysis(_,_,_,_,_,_)),
    retractall(syn_sem_lex_analysis(_,_,_,_,_,_,_,_,_,_)).

:- initialize_flag(unknowns,on).

:- initialize_thread_flag(current_input_sentence,[a]).

:- public only_unknowns/1.
only_unknowns(Input) :-
    set_thread_flag(current_input_sentence,[Input]),
    lexical_analysis_cleanup,
    add_word_forms([Input]),
    guess_names([Input]),
    guess_unknowns([Input],0),
    (   tag(_,_,_,_,Root,Surf,History,Tag),
        format("~w|~w|~w|~w~n", [Root,Surf,History,Tag]),
        fail
    ;   true
    ).

:- initialize_thread_flag(current_lexical_analysis_values,none).

%% lexical analysis asserts all possible analyses of the terminals
%% each transition can be the start of a number of lexical covers
lexical_analysis(Input) :-
    statistics(runtime,[T0,_]),
    hdrug_flag(current_ref,Ref),
    call_cleanup(
	lexical_analysisXX(Input),
	debug_call(1,
		   (   statistics(runtime,[T1,_]),
		       Time is T1-T0,
		       (   thread_flag(current_lexical_analysis_values,f(R,Edges0,Edges,VEdges,VVEdges,VVVVEdges))
		       ->  count_edges(( syn_lex_analysis(_,_,_,_,_,_)
				       ; syn_lex_analysis(_,_,_,_)
				       ),E3),
			   format(user_error,
				  "Lexical analysis: ~w words, ~w -> ~w -> ~w -> ~w -> ~w tags, ~w signs, ~w msec (item ~w)~n",
				  [R,Edges0,Edges,VEdges,VVEdges,VVVVEdges,E3,Time,Ref])
		       ;   format(user_error,
				  "Lexical analysis: ? words, ? -> ? -> ? -> ? -> ? tags, 0 signs, ~w msec (item ~w) ABORTED~n",
				  [Time,Ref])
		       
		       )
		   )
		  )
		).

ignore(['[','MISSING','PARA',']']).
ignore(['BETA','COMPLEET']).
ignore(['BETA',instart]).
ignore(['CA','2']).
ignore(['CA','3']).
ignore(['(','*',')']).
ignore(['Uitzonderingen',':','G']).
ignore(['<','!','TABLE','!','>']).
ignore(['<','!','INDEX','!','>']).
ignore(['bèta','compleet']).
ignore(['*','*','*']).

replace_alt([],_,[]).
replace_alt([W|Ws],P0,[N|Ns]) :-
    replace_w(W,P0,N),
    P is P0 + 1,
    replace_alt(Ws,P,Ns).

replace_w(W,P0,N) :-
    user_skips(List),
    lists:member(alt(P0,N,W),List),
    !.
replace_w(W,_,W).


lexical_analysisXX(Input0) :-
    replace_alt(Input0,0,Input),
    lexical_analysisXXX(Input).

lexical_analysisXXX(Input) :-
    set_thread_flag(current_input_sentence,Input),
    set_thread_flag(current_lexical_analysis_values,none),
    hdrug_flag(debug,Debug),
    hdrug_flag(unknowns,Unk),
    time(Debug,lexical_analysis_cleanup),

    (   ignore(Input)
    ->  true
    ;   

	add_word_forms(Input),
	length(Input,Length),
	MaxPos is Length-1,
	
    %% add tags for words that are in the lexicon:
	
	time(Debug,
	     lexical_analysis(Input,0,P,0,R,UnbracketedInput,0,[])),

	%%    time(Debug,assert_cgn_tags(R)),

	(   Unk == on
	->  time(Debug,guess_names(Input)), % unknowns.pl
	    time(Debug,guess_slash_pairs),  % unknowns.pl
	    time(Debug,guess_eng_compounds(Input))
	;   true
	),
	time(Debug,enforce_unique_match),
	time(Debug,enforce_longest_match(UnbracketedInput,0,P)),
	%% add tags for unknown words:
	(   Unk == on
	->  time(Debug,guess_unknowns(Input,MaxPos)),
	    time(Debug,guess_english_compounds),
	    time(Debug,add_quoted_names(Input,0,P,0,R))
	;   true
	),
	count_edges(tag(_,_,_,_,_,_,_,_),Edges0),
	hdrug_flag(filter_lexical_analysis,Filter),
	hdrug_flag(interactive_lexical_analysis,Interactive),
	
	add_user_skips,
	

	time(2,filter_tags(Filter)),

	(   Unk == on
	->  time(Debug,ensure_connected(Input,MaxPos)), % adds pseudo tags, useful for skipper
	    time(Debug,skips),
	    time(Debug,filter_te_tags),
	    time(Debug,replace_per_tags),
	    time(Debug,replace_dehet_tags),
	    time(Debug,filter_enumeration_tags),
	    time(Debug,filter_bracketed_tags),
	    time(Debug,ensure_connected(Input,MaxPos)), % adds pseudo tags, useful for tagger
	    count_edges(tag(_,_,_,_,_,_,_,_),Edges)
	),
	
	time(Debug,alpino_postagger:pos_filter(UnbracketedInput,EraseTags0,RefTags)),
	sort(EraseTags0,EraseTags), % remove duplicates tagger/cgn
	filter_interactively(Interactive,Input,UnbracketedInput,
			     EraseTags,ErasedTags),
	restore_alternatives_to_unknown(ErasedTags,MaxPos,UnbracketedInput),
	count_edges(tag(_,_,_,_,_,_,_,_),VEdges),
	(	VEdges < Edges
	->	time(2,filter_tags(Filter))
	;	true
	),
	count_edges(tag(_,_,_,_,_,_,_,_),VVEdges),
	time(Debug,check_connected(Input,P,ErasedTags,Filter)),
	
	alpino_unknowns:retract_wikipedia_list,
	
	%% some tags are removed, so we re-do filter_tags
	%% example: Een storm van beschuldigingen aan het adres van Weinreb
	%%                brak los (pos_tager=on)
	count_edges(tag(_,_,_,_,_,_,_,_),VVVEdges),
	(	VVVEdges < VVEdges
	->	time(2,filter_tags(Filter))
	;	true
	),
	count_edges(tag(_,_,_,_,_,_,_,_),VVVVEdges),
	
	add_score_refs(RefTags),
	
	%% create full signs for each of the remaining tags:
	time(Debug,create_syn_lex_analysis),
	
	%% communicate debug values to call_cleanup above,
	%% sometimes goes wrong otherwise, presumably due to
	%% bug in SICStus?
	set_thread_flag(current_lexical_analysis_values,f(R,Edges0,Edges,VEdges,VVEdges,VVVVEdges)),
	%% same for the cut, I don't get it...
	!
    ).

%% do lexicon lookup for each of the words in the input
lexical_analysis([],P,P,R,R,[],BracketCounter,_LC) :-
    hdrug_flag(require_brackets_balanced,Balanced),
    (	BracketCounter =:= 0
    ->	true
    ;   Balanced == on
    ->	format(user_error,"error: brackets not balanced~n",[])
    ;   true
    ).
lexical_analysis([Word|Rest],P0,P,R0,R,Input,BC0,LC0):-
    lexical_analysis_or_bracket(Word,Rest,Rest1,P0,P1,R0,R1,
				Input,Input0,BC0,BC,LC0,LC),
    lexical_analysis(Rest1,P1,P,R1,R,Input0,BC,LC).

lexical_analysis_or_bracket(bracket(open),Rest,Rest1,P0,P,R,R,I,I,BC0,BC,LC,LC) :-
    !,
    open_bracket_type(Rest,Rest1,P0,P),
    BC is BC0+1.
lexical_analysis_or_bracket(bracket(close),Rest,Rest1,P0,P,R,R,I,I,BC0,BC,LC,LC) :-
    !,
    hdrug_flag(require_brackets_balanced,Balanced),
    (	(   BC0 > 0
	;   Balanced == off
	)
    ->	BC is BC0-1,
	close_bracket_type(Rest,Rest1,P0,P)
    ;	format(user_error,"error: brackets not balanced~n",[]),
	raise_exception(alpino_error(unbalanced_brackets))
    ).
lexical_analysis_or_bracket(W,Rest,Rest,P0,P,R0,R,[W|I],I,BC,BC,LC,[W|LC]) :-
    P is P0 + 1,
    R is R0 + 1,
    assertz(rpos(P0,R0)),
    lexical_analysis_([W|Rest],P0,R0,LC).

open_bracket_type([],[],P0,P) :-
    P is P0 + 1,
    noclp_assertz(open_bracket(P0,P,_)).
open_bracket_type([H|T],T,P0,P) :-
    atom(H),
    atom_codes(H,[64|String]),
    !,
    atom_codes(Cat,String),
    P is P0 + 2,
    noclp_assertz(open_bracket(P0,P,Cat)).
open_bracket_type([H|T],[H|T],P0,P) :-
    P is P0 + 1,
    noclp_assertz(open_bracket(P0,P,_)).

close_bracket_type([],[],P0,P) :-
    P is P0 + 1,
    noclp_assertz(close_bracket(P0,P,_)).
close_bracket_type([H|T],T,P0,P) :-
    atom(H),
    atom_codes(H,[64|String]),
    !,
    atom_codes(Cat,String),
    P is P0 + 2,
    noclp_assertz(close_bracket(P0,P,Cat)).
close_bracket_type([H|T],[H|T],P0,P) :-
    P is P0 + 1,
    noclp_assertz(close_bracket(P0,P,_)).

lexical_analysis_(Input,P0,R0,LC):-
    (	lexical_analysis__(Input,P0,R0,LC),
	fail
    ;   true
    ).


lexical_analysis__(Input,P0,R0,LC):-
    Input = [InputStart|_],
    debug_message(3,"lexical analysis on position ~w ~p~n",[P0,InputStart]),
    lex_lexicon(Tag,Label,Input,Input1,P0,History,LC),
    append(UsedInput,Input1,Input),
    length(UsedInput,Length),
    P is P0+Length,
    R is R0+Length,
    concat_all(UsedInput,UsedInput1,' '),
    assert_tag(P0,P,R0,R,Label,UsedInput1,History,Tag).

lex_lexicon(Tag,Label,Input0,Input,P0,His0,LC) :-
    if( added_lex(Tag,Label,Input0,Input,His,LC),
	His0 = normal(His),
        normal_lex(Tag,Label,Input0,Input,P0,His0,LC)
      ).

%% ADD LEX
%% for the current session, make sure Word is analyzed just like Like
%% this is a quick hack to increase treebank construction speed
added_lex(Tag,Label,[Word|Input0],Input,His,LC) :-
    add_lex_table(Word,Label,Input0,Input1,Alternative),
    in_lexicon(Tag,_,[Alternative|Input1],Input,His,LC).

added_lex(Tag,Label,[Word|Input0],Input,user,_) :-
    add_tag_table(Word,Label,Input0,Input,Tag).

%% added_lex(Tag,Label,[Word|Input0],Input,universal(Weight),_) :-
%%    add_universal_table(Word,Label,Input0,Input,Tag,Weight).

normal_lex(Tag,Label,[Word|Input0],Input,P,His,LC) :-
    atom(Word),
    alpino_util:split_atom(Word,"~",['',A|C]),
    !,
    lists:member(NW,[A|C]),
    normal_lex_(Tag,Label,[NW|Input0],Input,P,His,LC).
normal_lex(Tag,Label,[W|Input0],Input,P,His,LC) :-
    normal_lex_(Tag,Label,[W|Input0],Input,P,His,LC).

normal_lex_(Tag,Label,Input0,Input,P,His,LC) :-
    findall(f(Tag1,Label1,Input1,His1),in_lexicon(Tag1,Label1,Input0,Input1,His1,LC),List),
    normal_lex2(Tag,Label,Input0,Input,P,His,LC,List).

normal_lex2(Tag,Label,_Input0,Input,_P,normal(His),_,List) :-
    lists:member(f(Tag,Label,Input,His),List).

normal_lex2(Tag,Label,[Word|Input0],Input,P,normal(decap(His)),LC,Tags) :-
    alpino_unknowns:normal_capitalized_word(P,Word,Input0,Input1,DecapWord),
    in_lexicon(Tag,Label,[DecapWord|Input1],Input,His,LC),
    \+ member(f(Tag,_,Input,_),Tags),
    \+ forbid_odd_normal_word(Word,Tag).

normal_lex2(Tag,Label,[Word|Input0],Input,P,special(decap(His)),LC,Tags) :-
    alpino_unknowns:special_capitalized_word(P,Word,Input0,Input1,DecapWord,Len),
    length(Consumed,Len),
    lists:append(Consumed,Input,[DecapWord|Input1]),
    in_lexicon(Tag,Label,[DecapWord|Input1],Input,His,LC),
    \+ His = chess,
    \+ His = variant(variant(_,_),normal),
    \+ member(f(Tag,_,Input,_),Tags).

normal_lex2(Tag,Label,Input0,Input,_,special(His),LC,_) :-
    alpino_lex:special_lexicon(Tag,Label,Input0,Input,His,LC).

forbid_odd_normal_word('A',conj(_)).
forbid_odd_normal_word('A',preposition(_,_)).

in_lexicon(Tag,Label,[Word|Input0],Input,His,LC) :-
    alpino_lex:lexicon(Tag,Label,[Word|Input0],Input,His,LC).

:- public add_lex/1, add_tag/1, add_sc/1, cmd_skip_word/1, % add_universal/1,
          tags/0, tr_tags/0.

scored_tag(A,B,C,D,E,F,G,H,Score) :-
    clause(tag(A,B,C,D,E,F,G,H),_,Ref),
    (   alpino_score_ref:score_ref(Ref,Score)
    ->  true
    ;   Score = 0
    ).

%% list all tags, ordered
tags :-
    findall(tag(A,B,C,D,E,F,G,H)-Ref,clause(tag(A,B,C,D,E,F,G,H),_,Ref),Tags0),
    sort(Tags0,Tags),
    (   member(El-Ref,Tags),
        (   alpino_score_ref:score_ref(Ref,Score)
	->  Prob is exp(-Score),
	    format("~w ~6f~n",[El,Prob])
	;   format("~w~n",[El])
	),
	fail
    ;   true
    ).

tr_tags :-
    findall(tag(A,B,C,D,E,F,G,Tag),
	    (   tag(A,B,C,D,E,F,G,H),
		alpino_tr_tag:tr_tag(H,Tag)
	    ),
	    Tags0),
    sort(Tags0,Tags),
    (   member(El,Tags),
        format("~w~n",[El]),
	fail
    ;   true
    ).

%%add_universal([Word|Tail]) :-
%%    (   concat_all([Word|Tail],Label,' '),
%%	append(Tail,Var,TailVar),
%%	universal_tag(Tag,Weight),
%%	assertz(add_universal_table(Word,Label,TailVar,Var,Tag,Weight)),
%%	fail
%%    ;   true
%%    ).


add_lex(Words) :-
    append(Words0,[Like],Words),
    add_lex(Words0,Like).

add_lex([Word|Tail],Like) :-
    concat_all([Word|Tail],Label,' '),
    append(Tail,Var,TailVar),
    noclp_assertz(add_lex_table(Word,Label,TailVar,Var,Like)).

add_tag(Words) :-
    append(Words0,[Tag],Words),
    add_tag(Words0,Tag).

add_tag([Word|Tail],Tag) :-
    concat_all([Word|Tail],Label,' '),
    append(Tail,Var,TailVar),
    noclp_assertz(add_tag_table(Word,Label,TailVar,Var,Tag)).

add_sc(Words) :-
    append(Words0,[Frame],Words),
    add_sc(Words0,Frame).

add_sc([Word|Tail],Frame) :-
    append(Tail,Var,TailVar),
    findall(p(HZ,Infl,Label),
	    non_part_lexicon_verb([Word|Tail],HZ,Infl,Label),
	    Triples0),
    sort(Triples0,Triples),
    (	member(p(HZ,Infl,Label),Triples),
	noclp_assertz(add_tag_table(Word,Label,TailVar,Var,verb(HZ,Infl,Frame))),
	fail
    ;	true
    ).

non_part_lexicon_verb([Word|Tail],HZ,Infl,Label) :-
    in_lexicon(verb(HZ,Infl,Subcat),Label,[Word|Tail],[],_,[]),
    functor(Subcat,Fun,_A),
    \+ atom_concat(part_,_,Fun).

cmd_skip_word([Word|Tail]) :-
    noclp_assertz(add_skip_word(Word,Tail)).

current_word(P0,P,UsedInput):-
    thread_flag(current_input_sentence,Input),
    length(Input,Len),
    between(0,Len,P0),
    P is P0 + 1,
    surface_form(Input,P0,P,UsedInput).

assert_tag(P0,P,R0,R,Label,History,Tag) :-
    interval_not_bracket(P0,P),
    thread_flag(current_input_sentence,Input),
    surface_form(Input,P0,P,UsedInput),
    assert_tag(P0,P,R0,R,Label,UsedInput,History,Tag).

:- if(current_prolog_flag(dialect, swi)).

% %% has no effect for SWI version 5.11.29 and beyond
% :- index(tag(1, % P0
% 	     0, % P
% 	     1, % R0
% 	     0, % R
% 	     1, % Label
% 	     0, % UsedInput
% 	     0, % History
% 	     1)). % Tag

assert_tag(P0,P,R0,R,Label,UsedInput,History,Tag) :-
    (   tag(P0,P,R0,R,Label,UsedInput,History,Tag)
    ->  true
    ;   History = skip(N,_,_,His),
        tag(P0,P,R0,R,Label,UsedInput,skip(M,_,_,His),Tag),
	M < N
    ->  true
    ;   noclp_assert(tag(P0,P,R0,R,Label,UsedInput,History,Tag)),
	noclp_assert(copy_tag(P0,P,R0,R,Label,UsedInput,History,Tag))
    ).	      

search_tag_tag(Tag,tag(P0,P,R0,R,Label,UsedInput,History,Tag)) :-
    tag(P0,P,R0,R,Label,UsedInput,History,Tag).

search_tag_r0(R0,tag(P0,P,R0,R,Label,UsedInput,History,Tag)) :-
    tag(P0,P,R0,R,Label,UsedInput,History,Tag).

search_tag_stem(Label,tag(P0,P,R0,R,Label,UsedInput,History,Tag)) :-
    tag(P0,P,R0,R,Label,UsedInput,History,Tag).

erase_tag(Ref) :-
    erase(Ref).

:- else.

erase_tag(Ref) :-
    erase(Ref),
    retractall(index_tag_tag(_,Ref)),
    retractall(index_tag_stem(_,Ref)),
    retractall(index_tag_r0(_,Ref)).

assert_tag(P0,P,R0,R,Label,UsedInput,History,Tag) :-
    (   tag(P0,P,R0,R,Label,UsedInput,History,Tag)
    ->  true
    ;   History = skip(N,_,_,His),
        tag(P0,P,R0,R,Label,UsedInput,skip(M,_,_,His),Tag),
	M < N
    ->  true
    ;   assertz(tag(P0,P,R0,R,Label,UsedInput,History,Tag),Ref),
	assertz(copy_tag(P0,P,R0,R,Label,UsedInput,History,Tag)),
	assertz(index_tag_tag(Tag,Ref)),
	assertz(index_tag_stem(Label,Ref)),
	assertz(index_tag_r0(R0,Ref))
    ).

search_tag_tag(Pos,Tag) :-
    index_tag_tag(Pos,Ref),
    clause(Tag,_,Ref).

search_tag_r0(R0,Tag) :-
    index_tag_r0(R0,Ref),
    clause(Tag,_,Ref).

search_tag_stem(Label,Tag) :-
    index_tag_stem(Label,Ref),
    clause(Tag,_,Ref).

:- endif.

%% surface_form(+Input,+P0,+P,-UsedInput)
surface_form(Input,P0,P,Used) :-
    length(Prefix,P0),
    append(Prefix,Rest,Input),
    Len is P - P0,
    length(Mid,Len),
    append(Mid,_,Rest),
    concat_all(Mid,Used,' ').

%% create full signs (feature structures) for each of the tags;
%% but only for those tags that are valid
create_syn_lex_analysis :-
    (	bagof(Syn-m(UsedInput,Ref),
	      lookup_tag(P0,P,UsedInput,Syn,Ref),
	      Cands0
	     ),
        alpino_lc:freeze_const(Cands0,Cands1),
	alpino_lc:check_equivalence(Cands1,Cands),
	assert_lexical_analysis(Cands,P0,P),
	fail
    ;	true
    ).

lookup_tag(P0,P,Used,Syn,Ref) :-
    tag(P0,P,R0,R,Label,Used,His,Tag),
    copy_term(Tag,Tag1),  % in case there are variables in Tag;
                          % this is needed otherwise numbering scheme fails
    %% add integer n indicating that this is the n-th analysis for Tag
    bb_put(count_fs_per_tag,0),
    (   Label=v_root(Stem,Lemma)
    ->  true
    ;   Label=Stem,
	Label=Lemma
    ),
    adapt_for_skips(His,R0,R,RR0,RR),
    alpino_data:lexical(Hwrd,Stem,Lemma,Used,RR0,RR,His,_Bc),
    tagtime(unique_lex(Tag,Hwrd,List),Tag),
    length(List,Length),
    (   Length < 1
    ->  debug_call(1,warn_unexpected_missing_fs(Tag1,Used))
    ;   debug_message(3,
            "found ~w feature structures for ~w (~w)~n",
		      [Length,Tag1,Used])
    ),
    nth(N,List,Cat-Constraints),
    call_constraints(Constraints),
    alpino_data:separate(Cat,Syn),
    Ref=ref(Class,Tag1,Label,Used,P0,P,R0,R,His,N,GuidesTag),
    alpino_tr_tag:tr_tag(Tag1,Class),
    alpino_guides:tr_tag(Tag1-N,GuidesTag),
    assertz(syn_sem_lex_analysis(Tag1,Label,Used,P0,P,R0,R,His,N,Cat)),
    debug_message(4,"found feature structure ~w for ~w (~w)~n",[N,Tag1,Used]).

adapt_for_skips(skip(_,List1,List2,His),R0,R,X0,X) :-
    !,
    adapt_l(List1,R0,R1),
    adapt_r(List2,R,R2),
    adapt_for_skips(His,R1,R2,X0,X).
adapt_for_skips(_,R0,R,R0,R).

adapt_l([],R,R).
adapt_l([_|T],R0,R) :-
    R1 is R0 + 1,
    adapt_l(T,R1,R).

adapt_r([],R,R).
adapt_r([_|T],R0,R) :-
    R1 is R0 - 1,
    adapt_r(T,R1,R).

unique_lex(Tag,His,List) :-
    findall(Cat-Constraints, alpino_lex_types:lex(Cat,Tag,His,Constraints), List0),
    remove_variants(List0,List,Bool),
    (   Bool > 0
    ->  debug_message(2,
         "alpino_lexical_analysis: removing variants for ~w~n",[Tag])
    ;   true
    ).


%% called from lc.pl; second phase
get_lref(ref(_Class,Tag0,Label,Used,P0,P,Q0,Q,His,N,_),P0,P,Cat,
	 frame(P0,P,Q0,Q,Label,Tag,Used,His)) :-
    copy_term(Tag0,Tag),
    syn_sem_lex_analysis(Tag,Label,Used,P0,P,Q0,Q,His,N,Cat).

get_lref_list(List,W,LexRef,P0,P,Small,Frame) :-
    member(_-m(W,LexRef),List),
    get_lref(LexRef,P0,P,Small,Frame).

:- if(current_prolog_flag(dialect,swi)).

assert_lexical_analysis(List,P0,P) :-
    (	member(f(Syn,Cons)-History0,List),
        remove_skips_his(History0,History),
%	call(Cons),
	hdrug_flag(use_guides,Guides),
	alpino_lc_in:create_lex(P0,P,Syn,History,Guides,Call),
	noclp_assertz((Call :- Cons)),
	fail
    ;	true
    ).

:- else.

assert_lexical_analysis(List,P0,P) :-
    (	member(f(Syn,Cons)-History0,List),
        remove_skips_his(History0,History),
	call(Cons),
	assertz(syn_lex_analysis(P0,P,Syn,History)),
	fail
    ;	true
    ).

:- endif.

remove_skips_his(List0,List) :-
    count_skips_his(List0,List1),
    keysort(List1,List2),
    delete_skips_his(List2,List).

count_skips_his([],[]).
count_skips_his([Item|Items0],[Count-Item|Items]) :-
    count_skip_his(Item,0,Count),
    count_skips_his(Items0,Items).

count_skip_his(m(_,ref(_,_,_,_,_,_,_,_,His,_,_)),C0,C) :-
    count_skip_his_his(His,C0,C).

count_skip_his_his(skip(Count,_L,_R,_),C0,C) :-
    !,
%    length(L,Lc),
%    length(R,Rc),
    C is C0 + Count.
count_skip_his_his(_,C,C).

delete_skips_his([],[]).
delete_skips_his([Count-Item|Items0],[Count-Item|Items]) :-
    delete_skips_his(Items0,Count,Items).

delete_skips_his([],_,[]).
delete_skips_his([Count1-Item|Items0],Count,Items) :-
    (   Count1 > Count
    ->  Items = []
    ;   Items = [Count1-Item|Items1],
        delete_skips_his(Items0,Count1,Items1)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% FILTER TAGS %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialize_flag(filter_lexical_analysis,on).
:- initialize_flag(interactive_lexical_analysis,off).

filter_tags(on) :-
    filter_tags.

filter_tags(off).

filter_tags :-
    findall([],tag(_,_,_,_,_,_,_,_),List),
    length(List,L),
    filter_tags_until_stable(L).

filter_tags_until_stable(L) :-
    filter_tags__,
    findall([],tag(_,_,_,_,_,_,_,_),List),
    length(List,NewL),
    (   NewL < L
    ->  debug_message(2,"filtered ~w tags to ~w tags~n",[L,NewL]),
	filter_tags_until_stable(NewL)
    ;   true
    ).

filter_tags__ :-
    alpino_filter_tag:initialize_filter_tags,
    (	clause(tag(_,_,Q0,Q,Root,_,_,H),true,Ref),
	statistics(runtime,[T0,_]),
	(    filter_tag(H,Root,Q0,Q)
	->   true
	;    erase_tag(Ref)
	),
	statistics(runtime,[T,_]),
	Time is T-T0,
	(    Time > 20
	->   debug_message(2,"filter_tag ~w ~w ~w ~w (~w msec)~n",[H,Root,Q0,Q,Time])
	;    true
	),
	fail
    ;	true
    ),
    time(2,filter_unique_fixed_tags).

filter_te_tags :-
    (	clause(tag(_,_,_,Q1,te,_,_,preposition(te,_,nodet)),true,Ref),
	\+ followed_by_proper_name_or_tmp_np(Q1),
	erase_tag(Ref),
	fail
    ;	clause(tag(_,_,_,Q1,te,_,_,Intensifier),true,Ref),
	intensifier_tag(Intensifier),
	\+ followed_by_adjective(Q1),
	erase_tag(Ref),
	fail
    ;   clause(tag(_,_,_,Q1,te,_,_,complementizer(te)),true,Ref),
        \+ followed_by_infinitive(Q1),
        erase_tag(Ref),
        fail
    ;	tag(_,Q1,_,_,te,_,_,_),
        clause(tag(Q1,_,_,_,_,_,_,Tag),true,Ref),
        cannot_follow_te(Tag),
        erase_tag(Ref),
        fail
    ;   true
    ).

replace_per_tags :-
    (   clause(tag(P1,P,Q1,Q,W,L,His,proper_name(Num,'PER')),true,Ref),
	unique(P1,P,Ref),
	(   tag(_,P1,_,Q1,het,_,_,_)
	->  erase_tag(Ref),
	    assert_tag(P1,P,Q1,Q,W,L,replace_per(His),proper_name(Num)),
	    debug_message(1,"ignore 'PER' after 'het' for ~w~n",[W])
	),
	fail
    ;   true
    ).

dehet(de,het).
dehet(het,de).

replace_dehet_tags :-
    (   clause(tag(P1,P,Q1,Q,W,L,His,noun(De,Count,sg)),true,Ref),
	dehet(De,Het),
	\+ non_noun_tag(P1,P),
	(   tag(_,P1,_,Q1,Het,_,_,_)
	->  erase_tag(Ref),
	    \+ tag(P1,P,Q1,Q,W,L,_,noun(Het,_,sg)),
	    assert_tag(P1,P,Q1,Q,W,L,replace_dehet(His),noun(both,Count,sg)),
	    debug_message(1,"ignore ~w after ~w for ~w~n",[Het,De,W])
	),
	fail
    ;   clause(tag(P1,P,Q1,Q,W,L,His,noun(De,Count,sg,SC)),true,Ref),
	dehet(De,Het),
	\+ non_noun_tag(P1,P),
	(   tag(_,P1,_,Q1,Het,_,_,_)
	->  erase_tag(Ref),
	    \+ tag(P1,P,Q1,Q,W,L,_,noun(Het,_,sg)),
	    assert_tag(P1,P,Q1,Q,W,L,replace_dehet(His),noun(both,Count,sg,SC)),
	    debug_message(1,"ignore ~w after ~w for ~w~n",[Het,De,W])
	),
	fail
    ;   true
    ).

non_noun_tag(P1,P) :-
    tag(Z0,Z,_,_,_,_,_,Tag),
    overlap(P1,P,Z0,Z),
    \+ Tag = noun(_,_,_,_),
    \+ Tag = noun(_,_,_).    

unique(P0,P,Ref) :-
    findall(Ref2,( clause(tag(Z0,Z,_,_,_,_,_,_),_,Ref2),
		   overlap(P0,P,Z0,Z)
		 ), [Ref]).
    

filter_enumeration_tags :-
    findall(Ref,enumeration_tag_with_enumeration_neighbor(Ref),Refs),
    (   member(R,Refs),
	erase_tag(R),
	fail
    ;   true
    ).

filter_bracketed_tags :-
    %% forbid "3 )" as a tag if preceded by "("
    %% forbid "( 3" as a tag if followed by ")"
    (   current_word(P0,P1,'('),
	current_word(P2,P,')'),
	(   clause(tag(P1,P,_,_,_,_,_,_),true,R)
	;   clause(tag(P0,P2,_,_,_,_,_,_),true,R)
	),
	erase_tag(R),
	fail
    ;   true
    ).
    

enumeration_tag_with_enumeration_neighbor(Ref) :-
    clause(tag(_,_,_,Q1,_,_,normal(enumeration),_),true,Ref1),
    clause(tag(_,_,Q1,_,_,_,normal(enumeration),_),true,Ref2),
    (   Ref = Ref1
    ;   Ref = Ref2
    ).	

%% only bother about the systematic ones
cannot_follow_te(v_noun(_)).
cannot_follow_te(verb(_,pl,_)).

intensifier_tag(intensifier).
intensifier_tag(me_intensifier).
intensifier_tag(vp_om_intensifier).
intensifier_tag(vp_om_me_intensifier).


%% if a fixed-part is the unique tag for a certain position, and
%% there is a unique position which requires that fixed-part, then
%% all alternatives for the latter position can be removed.


filter_unique_fixed_tags :-
    (   fixed_tag(Tag),
	search_tag_tag(Tag,tag(P0,P,_,_,_,_,_,Tag)),
	\+ not_unique(Tag,P0,P),
	findall(Q0/Q,requires_fixed(Q0,Q,Tag),Ps0),
	sort(Ps0,[Q0/Q]),
	clause(tag(R0,R,_,_,_,_,_,Alternative),true,Ref),
	overlap(Q0,Q,R0,R),
	\+ requires_fixed_tag(Tag,Alternative),
	erase_tag(Ref),
	fail
    ;   true
    ).

not_unique(Tag,P0,P) :-
    tag(Z0,Z,_,_,_,_,_,AltTag),
    Tag \== AltTag,
    overlap(P0,P,Z0,Z).

fixed_tag(fixed_part(_Part)).
fixed_tag(particle(_Op)).

requires_fixed(Q0,Q,Fixed) :-
    tag(Q0,Q,_,_,_,_,_,Tag),
    requires_fixed_tag(Fixed,Tag).

requires_fixed_tag(fixed_part(Part),Tag) :-
    alpino_filter_tag:check_fixed_part(Part,Tag,0,0).
requires_fixed_tag(particle(Part),Tag) :-
    alpino_filter_tag:check_part_required(Part,Tag,0,0).

followed_by_adjective(Q1) :-
    search_tag_r0(Q1,tag(_,_,Q1,_,_,_,_,ADJ)),
    adjective_tag(ADJ).
followed_by_adjective(Q1) :-
    search_tag_r0(Q1,tag(_,_,Q1,Q2,_,_,_,punct(_))),
    followed_by_adjective(Q2).

followed_by_infinitive(Q1) :-
    search_tag_r0(Q1,tag(_,_,Q1,_,_,_,_,INF)),
    infinitive_tag(INF).
followed_by_infinitive(Q1) :-
    search_tag_r0(Q1,tag(_,_,Q1,Q2,_,_,_,punct(_))),
    followed_by_infinitive(Q2).

infinitive_tag(verb(_,inf,_)).
infinitive_tag(verb(_,inf(_),_)).

followed_by_proper_name_or_tmp_np(Q1) :-
    search_tag_r0(Q1,tag(_,_,_,_,_,W,_,_)),
    lists:member(W,[lucht,wapen,huis,drommel]).
followed_by_proper_name_or_tmp_np(Q1) :-
    search_tag_r0(Q1,tag(_,_,Q1,_,_,_,_,NAME)),
    proper_name_or_tmp_np_tag(NAME).
followed_by_proper_name_or_tmp_np(Q1) :-
    search_tag_r0(Q1,tag(_,_,Q1,_,_,_,_,determiner(der))).

proper_name_or_tmp_np_tag(proper_name(_)).
proper_name_or_tmp_np_tag(proper_name(_,_)).
proper_name_or_tmp_np_tag(tmp_np).
proper_name_or_tmp_np_tag(np(year)).

adjective_tag(adjective(_)).
adjective_tag(adjective(_,_)).
adjective_tag(np_adjective).
adjective_tag(np_adjective(_)).
adjective_tag(het_np_adjective(_)).
adjective_tag(np_me_adjective(_)).
adjective_tag(nominalized_adjective).
adjective_tag(post_adjective_anders(_)).
adjective_tag(post_adjective(_)).
adjective_tag(post_adjective(_,_)).


%% adding UNKNOWN tags between each position,to ensure connectedness
ensure_connected(Words,MaxPos) :-
    (	between(0,MaxPos,P),
        find_word(Words,P,W,Wp0,Wp,Wr0,Wr),
	\+ tag(Wp0,Wp,_,_,_,_,_,_),
	assert_tag(Wp0,Wp,Wr0,Wr,W,W,'UNKNOWN','UNKNOWN'),
	fail
    ;	true
    ).

find_word(List,P,Word,Wp0,Wp,Wr0,Wr) :-
    alpino_unknowns:skip_bracket_next(List,Rest,0,P1,0,R1),
    find_word(Rest,P,Word,P1,R1,Wp0,Wp,Wr0,Wr).

find_word([Word|_],P0,Word,P0,R0,P0,P,R0,R) :-
    !,
    P is P0+1, R is R0+1.
find_word([_|List],Ptarget,Word,Pc0,Rc0,P0,P,R0,R) :-
    Ptarget > Pc0,
    Pc1 is Pc0+1, Rc1 is Rc0+1,
    alpino_unknowns:skip_bracket_next(List,Rest,Pc1,Pc,Rc1,Rc),
    find_word(Rest,Ptarget,Word,Pc,Rc,P0,P,R0,R).

add_skips_for_unknowns(Words) :-
    length(Words,Length),
    MaxPos is Length-1,
    (	unused_pos(P,MaxPos),
	find_word(Words,P,W,Wp0,Wp,Wr0,Wr),
	assert_tag(Wp0,Wp,Wr0,Wr,W,W,skip,skip),
	fail
    ;	true
    ).

%% this can happen when the pos-tagger has removed the "licensing" tag
%% (todo: restore that licensing tag?)
%% two cases:
%% 1. this was actually correct, because this last tag was wrong, but we
%%    should restore the correct alternative 
%% 2. this was undesirable, restore licensing tag
check_connected(Words,MaxPos,_Erased,Filter) :-
    retractall(tag(_,_,_,_,_,_,'UNKNOWN','UNKNOWN')),
    (   unused_pos(P,MaxPos),
        P1 is P+1,
        nth(P1,Words,W),
        debug_message(1,
                                 "filter_tags removed last tag|~w|~w~n",
                                 [W,Words]),

	find_symbolic_begin(P,R0),
	R is R0 + 1,

	%% treat it as unknown
	call_with_decreased_debug(guess_unknown_words(Words,[P])),

	%% and known
	add_lexical_types(W,P,P1,R0,R),
	
	%% and as skippable
	%assert_tag(P,P1,R0,R,W,W,skip,skip),
	%expand_skip_tags,
	%keep_track_of_skips,
	fail
    ;   remove_orphans(MaxPos),
	filter_tags(Filter)
    ).

remove_orphans(MaxPos) :-
    (   clause(tag(X0,X1,_,_,_,_,_,_),_,Ref),
	(  X1 < MaxPos,
	   \+  starting_position(X1)
	;  X0 > 0,
           \+ ending_position(X0)
	),
	erase_tag(Ref),
	fail
    ;   true
    ).

ending_position(X0) :-
    tag(_,X0,_,_,_,_,_,_).
ending_position(X0) :-
    open_bracket(_,X0,_).
ending_position(X0) :-
    close_bracket(_,X0,_).

starting_position(X1):-
    tag(X1,_,_,_,_,_,_,_).
starting_position(X1):-
    open_bracket(X1,_,_).
starting_position(X1):-
    close_bracket(X1,_,_).



restore_tags(Pos,List) :-
    (   member(tag(P0,P,R0,R,L,U,H,T),List),
	Pos >= P0,
	Pos <  P,
	assert_tag(P0,P,R0,R,L,U,H,T),
	fail
    ;   true
    ).

%% enforce longest match for certain types of tags
%% sometimes long chess diagrams are tokenized as a single
%% sentence. Without this technique, we run out of memory
%% during **tagging**. Sigh.
requires_longest_match(normal(bridge)).
requires_longest_match(normal(chess)).
requires_longest_match(normal(opening_hours)).
requires_longest_match(normal(names_dictionary)).
requires_longest_match(normal(spaced_letters)).
requires_longest_match(name(not_begin)).
requires_longest_match(name(begin)).

requires_unique_match(name(not_begin),proper_name(sg,_),normal(enumeration),proper_name(both),_,_).
requires_unique_match(normal(names_dictionary),proper_name(_),normal(enumeration),proper_name(both),_,_).
requires_unique_match(normal(names_dictionary),proper_name(_,_),normal(enumeration),proper_name(both),_,_).
requires_unique_match(normal(decap(normal)),meas_mod_noun(_,_,_),normal(enumeration),proper_name(both),_,_).
requires_unique_match(normal(decap(normal)),noun(_,_,_),normal(enumeration),proper_name(both),_,_).
requires_unique_match(normal(number_expression),number(hoofd(_)),normal(enumeration),proper_name(both),_,_).
requires_unique_match(normal(variant(wrong_quote_s,normal)),_,normal(variant(variant21('\'s','\'',s),normal)),_,0,0).
requires_unique_match(normal(variant(wrong_quote_s,normal)),_,normal(variant(variant21('\'s','\'',s),variant)),_,0,0).
requires_unique_match(normal(spaced_letters),_,normal(_),_,5,0).
requires_unique_match(normal(bridge),_,normal(bridge),_,5,0).
requires_unique_match(normal(number_sequence),_,normal(number_sequence),_,5,0).
requires_unique_match(name(_),_,name(_),_,10,5).
requires_unique_match(name(_),_,name_gen(_),_,10,4).
requires_unique_match(name_gen(_),_,name_gen(_),_,6,3).

enforce_longest_match(Words,0,P) :-
    count_edges(tag(_,_,_,_,_,_,_,_),Edges0),
    (   requires_longest_match(His),
	enforce_longest_match(His,Words,0,P),
	fail
    ;   true
    ),
    count_edges(tag(_,_,_,_,_,_,_,_),Edges),
    (   Edges < Edges0
    ->  hdrug_util:debug_message(1,"enforce longest match: ~w to ~w edges~n",[Edges0,Edges])
    ;   true
    ).


enforce_unique_match :-    
    count_edges(tag(_,_,_,_,_,_,_,_),Edges0),
    (   requires_unique_match(His,Tag,His1,Tag1,MaxLarge,MaxSmall),
	enforce_unique_match(His,Tag,His1,Tag1,MaxLarge,MaxSmall),
	fail
    ;   true
    ),
    remove_identical_tags_but_lemma,
    count_edges(tag(_,_,_,_,_,_,_,_),Edges),
    (   Edges < Edges0
    ->  hdrug_util:debug_message(1,"enforce unique match: ~w to ~w edges~n",[Edges0,Edges])
    ;   true
    ).

remove_identical_tags_but_lemma :-
    (   tag(P0,P,Q0,Q,Lemma,Word,History,Tag),
	dif(Lemma,Lemma2),
	clause(tag(P0,P,Q0,Q,Lemma2,Word,History,Tag),true,Ref),
	prefer_lemma(Lemma,Lemma2,Tag),
	erase_tag(Ref),
	fail
    ;   true
    ).

prefer_lemma(v_root(_,B),v_root(_,D),Tag) :-
    prefer_lemma(B,D,Tag).
prefer_lemma(Lemma1,Lemma2,_) :-
    alpino_unknowns:cap_first(Lemma2,Lemma1).   % prefer lemma Breskens over breskens
prefer_lemma(leiden,geleiden,_).
prefer_lemma(raken,geraken,_).

% proper_longer(normal(_),R0,R,S0,S) :-
%     !,
%     R0 =< S0,
%      S =<  R.
proper_longer(_,R0,R,S0,S) :-
    R0 =< S0,
     S =<  R,
    RL is R0-1,
    RR is R+1,
     \+ (  (  normal_analysis(R0,S0)
	   ;  normal_analysis(RL,S0)
	   ),
	   (  normal_analysis( S, R)
	   ;  normal_analysis( S,RR)
	   )
	).

%proper_longer(R0,R,S0,S,H) :-
%    retractall(normal_tag(_,_)),
%    normal_base_cases(H),
%    proper_longer(R0,R,S0,S).


/*

this was way too slow for things like:
TOEN TOM HOORDE DAT HIJ DOOD WAS Tragikomedie van Jacco Groen .

so now we do it bottom-up, and cached, below

normal_analysis(S0,S,_) :-
    S0 >= S,
    !.
normal_analysis(R0,R,H) :-
    R0 < R,
    tag(_,_,Q0,R1,_,_Surf,Normal,_),
    isa_normal_tag(H,Normal),
    Q0 =< R0,
    R1 > R0,
    normal_analysis(R1,R,H).
*/

:- dynamic normal_tag/2.
:- thread_local normal_tag/2.

single_letter(Surf) :-
    atom_codes(Surf,[Code]),
    alpino_latin1:isalpha(Code).

normal_base_cases(H) :-
    (   tag(_,_,P0,P,_,Surf,Normal,_),
	\+ single_letter(Surf),
	isa_normal_tag(H,Normal),
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

isa_normal_tag(H,normal(Sub)) :-
    H \= normal(Sub),
    Sub \= enumeration.
isa_normal_tag(name(not_begin),name_adj(not_begin)).

enforce_longest_match(H,Words,0,_Final) :-
    retractall(normal_tag(_,_)),
    \+ \+ tag(_,_,_,_,_,_,H,_),
    normal_base_cases(H),
    (	tag(_Sf0,_Sf,R0,R,_Surf0,_,H,Tag1),
	R-R0 > 1,
	longest_match_candidate(H,R0,R,Words,Type),

	clause(tag(_,_,S0,S,_,Surf,H1,Tag2),true,Ref2),
	R-R0 > S-S0,
        R0 =< S0,
        S =< R,

        \+ longest_match_survivor(H,S0,S,Surf,Words,Ref2),

        (   Type == normal
        ->  (   once(competing_tag(H,H1,Tag1,Tag2,Surf)),
                proper_longer(H,R0,R,S0,S)
            ->  %format(user_error,"remove ~w because of ~w (normal)~n",[Surf,Surf0]),
		erase_tag(Ref2)
            ;   true
            )
        ;   Type = commas,
            (  H1 = name(_) ; H1 = name_gen(_) ; H1 = name_adj(_) ),
            search_tag_tag(punct(komma),tag(_,_,Q0,Q,_,_,_,punct(komma))),
            S0 < Q0, Q < S,
            (   proper_longer(H,R0,R,S0,S)
            ->  %format(user_error,"remove ~w because of ~w (commas)~n",[Surf,Surf0]),
		erase_tag(Ref2)
            ;   true
            )
        ),
        fail
    ;   true
    ).

%% unknown word within all-capitalized normal words
%% should remain unknown word, otherwise only very long
%% name reading survives.
longest_match_survivor(normal(_),_,_,_,_,_) :-
    !,
    fail.
longest_match_survivor(_,S0,S,Surf,List,Ref) :-
    S is S0+1,
    S1 is S0-1,
    S2 is S+1,
    (  fully_capitalized(S1,S,List)
    ;  fully_capitalized(S0,S2,List)
    ),
    \+ ( clause(tag(_,_,S0,S,_,Surf,_,_),true,Ref2),
         Ref2\==Ref
       ).

competing_tag(normal(spaced_letters),normal(spaced_letters),_,_,_).
competing_tag(normal(spaced_letters),normal(normal),_,tag,_).
competing_tag(normal(names_dictionary),normal(normal),_,tag,_).
competing_tag(normal(spaced_letters),normal(_),_,enumeration,_).
competing_tag(normal(names_dictionary),normal(_),_,enumeration,_).
competing_tag(normal(spaced_letters),normal(normal),_,proper_name(both),Surf) :-
    atom_length(Surf,1).
competing_tag(normal(names_dictionary),normal(normal),_,proper_name(both),Surf) :-
    atom_length(Surf,1).
competing_tag(normal(spaced_letters),normal(normal),_,noun(de,count,sg),Surf) :-
    atom_length(Surf,1).
competing_tag(normal(names_dictionary),normal(normal),_,noun(de,count,sg),Surf) :-
    atom_length(Surf,1).
competing_tag(_,_,number(hoofd(_)),number(hoofd(_)),_).
competing_tag(H,H,Tag,Tag,_).
competing_tag(name(Begin),name(Begin),Tag0,Tag,_) :-
    functor(Tag0,proper_name,_),
    functor(Tag,proper_name,_).
competing_tag(name(begin),normal(enumeration),Tag0,Tag,_) :-
    functor(Tag0,proper_name,_),
    functor(Tag,proper_name,_).
competing_tag(name(not_begin),normal(enumeration),_,_,_).
competing_tag(normal(names_dictionary),normal(enumeration),_,_,_).
%% Berlijnse Muur, Olympische Spelen, ...,
%% prevent capitalized adjectives that modify names if the thing itself is a name too
competing_tag(normal(names_dictionary),normal(normal),_,adjective(_),Surf) :-
    atom(Surf),
    atom_codes(Surf,[H|_]),
    isupper(H).
%% prevent determiner reading of first part of (known) name
%% prevent "Andries Cats"
competing_tag(normal(names_dictionary),normal(gen(names_dictionary)),_,_,_).

longest_match_candidate(name(_Begin),R0,R,List,Type) :-
    !,
    (  all_capitalized(R0,R,List),
       Type=normal
    ;  contains_commas(List),
       Type=commas
    ).

longest_match_candidate(_,_,_,_,normal).

contains_commas(List) :-
    findall([],member(',',List),L),
    length(L,Len),
    Len > 2.


all_capitalized(R0,R,List) :-
    (   R =< R0
    ->  true
    ;   nth0(R0,List,W),
	capitalized(W),
	R1 is R0 + 1,
	all_capitalized(R1,R,List)
    ).

fully_capitalized(R0,R,List) :-
    (   R =< R0
    ->  true
    ;   nth0(R0,List,W),
	alpino_unknowns:only_capitals(W,_),
	R1 is R0 + 1,
	all_capitalized(R1,R,List)
    ).

capitalized(W) :-
    atom(W),
    atom_codes(W,[F|_]),
    isupper(F).


enforce_unique_match(H,Tag,H1,Tag1,MaxLarge,MaxSmall) :-
    (   number(MaxLarge),
	number(MaxSmall),
	search_tag_tag(Tag,tag(_,_,R0,R,_,_,H,Tag)),
	R-R0 > MaxLarge,
	clause(tag(_,_,S0,S,_,_,H1,Tag1),true,Ref2),
	S-S0 > MaxSmall,
	part_of_interval(R0,R,S0,S),
	erase_tag(Ref2),
	fail
    ;   var(MaxLarge),
	var(MaxSmall),
	search_tag_tag(Tag,tag(_,_,S0,S,_,_,H,Tag)),
	clause(tag(_,_,S0,S,_,_,H1,Tag1),true,Ref2),
	erase_tag(Ref2),
	fail
    ;   true
    ).

part_of_interval(R0,R,S0,S) :-
    (   R0 < S0
    ->  R >= S
    ;   R0 = S0,
	R > S
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% INTERACTIVE FILTER %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filter_interactively(off,_,_,EraseTagRefs,ErasedTags) :-
    erase_tags(EraseTagRefs,ErasedTags).
filter_interactively(current,Input,_UnbInput,EraseTagRefs,ErasedTags):-
    init_filter_tags_interactively(Widget,C,Tags,EraseTagRefs),
    display_filter_tags(Widget,C,Tags,Input),
    restore_used_tags,
    remove_removed_tags(ErasedTags).
filter_interactively(on,Input,UnbInput,EraseTagRefs,ErasedTags) :-
    if_gui(
	filter_interactively_on(Input,UnbInput,
						 EraseTagRefs,ErasedTags),
	filter_interactively(off,Input,UnbInput,
					      EraseTagRefs,ErasedTags)
	             ).

filter_interactively_on(Input,UnbInput,EraseTagRefs,ErasedTags) :-
    filter_interactively_with_return(Input,UnbInput,EraseTagRefs),
    remove_removed_tags(ErasedTags).

filter_interactively_with_return(Input,UnbInput,EraseTagRefs) :-
    init_filter_tags_interactively(Widget,C,Tags,EraseTagRefs),
    display_filter_tags(Widget,C,Tags,Input),
    tcl('wm deiconify ~a',[Widget]),
    tcl('~a.f.t.c yview moveto 0',[Widget]),
    tcl('grab ~a',[Widget]),
    tcl('tkwait variable pos_filter_continue',[]),
    tcl('set dummy $pos_filter_continue',[],Return),
    (   Return == destroyed
    ->  fail
    ;   tcl('grab release ~a',[Widget]),
	tcl('wm withdraw ~a ; update',[Widget]),
	(   Return == go
	->  true
	;   Return == go_nonint
	->  set_flag(interactive_lexical_analysis,off)
	;   Return == tagger
	->  call_cleanup((hdrug_flag(pos_tagger,Old,on),
			  alpino_postagger:pos_filter(UnbInput,NewEraseTags,_)
			 ),
			 hdrug_flag(pos_tagger,_,Old)
			),
	    filter_interactively_with_return(Input,UnbInput,NewEraseTags)
	;   Return == cancel
	->  fail
	;   %% anything else such as invisible
	    fail
	)
    ).

:- public restore_used_tags/0.   % from tcltk
restore_used_tags :-
    bb_get(used_tags,UsedTags),
    restore_used_tags(UsedTags).

restore_used_tags([]).
restore_used_tags([H-Val|T]) :-
    restore_used_tag(H,Val),
    restore_used_tags(T).

restore_used_tag(tag(_,_,P0,P,Stem,Used,_,Label),Val) :-
    clause(tag(_,_,P0,P,Stem,Used,_,Label),true,Ref),
    (	tag_nr(Ref,N)
    ->	tcl('set radio~w ~w',[N,Val])
    ;	true
    ).

remove_removed_tag(Tag-Val) :-
    tag_nr(Ref,N),
    instance(Ref,(Tag:-true)),
    tcl('set dummy $radio~w',[N],Val),
    (	Val == on
    ->	true
    ;	erase_tag(Ref)
    ).

remove_removed_tags(Erased) :-
    findall(Tag,remove_removed_tag(Tag),Used),
    bb_put(used_tags,Used),
    findall(T,member(T-off,Used),Erased).

erase_tags([],[]).
erase_tags([Ref|Refs],[H|T]) :-
    instance(Ref,(H:-true)),
    erase_tag(Ref),
    erase_tags(Refs,T).

%% TODO: * display potential skips
%%       * allow user to add skips
%%       * only allow single tag per position?
init_filter_tags_interactively(Top,Widget,Tags,ErasedTags) :-
    all_tags(ErasedTags,Tags),
    hdrug_flag(filter_tags_widget,Widget0),
    (	Widget0 == undefined
    ->	create_top_canvas(C,Top),
	canvas_frame(C,Widget),
	tcl('frame ~a ; pack ~a',[Widget,Widget]),
	set_flag(filter_tags_widget,Top/Widget)
    ;	Widget0 = Top1/Widget1,
	tcl('winfo exists ~a',[Widget1],Return),
	(   Return == '1'
	->  Widget=Widget1,
	    Top=Top1,
%	    tcl('wm withdraw ~a',[Top]),
	    tcl('catch { destroy ~a }; frame ~a ; pack ~a',
		      [Widget,Widget,Widget])
	;   create_top_canvas(C,Top),
	    canvas_frame(C,Widget),
	    tcl('frame ~a ; pack ~a',[Widget,Widget]),
	    set_flag(filter_tags_widget,Top/Widget)
	)
    ).

create_top_canvas(Canvas,W) :-
    gen_sym(W,'.posfilter'),
    tcl("create_top_canvas_pos_filter ~a",[W],Canvas).


all_tags(EraseTagRefs,Tags):-
    findall(Tag,a_tag(Tag,EraseTagRefs),Tags0),
    sort(Tags0,Tags).

a_tag(tag(P0,P,R0,R,OnOff,Stam,Used,Label,Ref),ErasedTagRefs) :-
    clause(tag(P0,P,R0,R,Stam,Used,_,Label),true,Ref),
    (	member(Ref,ErasedTagRefs)
    ->	OnOff=off
    ;	OnOff=on
    ).

all_atoms([],[]).
all_atoms([H0|T0],[H|T]) :-
    term_atom(H0,H),
    all_atoms(T0,T).

display_filter_tags(Top,C,Tags,Input0) :-
    tcl('set dotcursor [. cget -cursor]',[]),
    tcl('set topcursor [~a cget -cursor]',[Top]),
    tcl('. configure -cursor watch',[]),
    tcl('~a configure -cursor watch',[Top]),
    tcl('update',[]),
    call_cleanup(display_filter_tags_c(C,Tags,Input0),
                 (  tcl('~a configure -cursor $topcursor',[Top]),
                    tcl('. configure -cursor $dotcursor',[])
                 )).

display_filter_tags_c(C,Tags,Input0) :-
    all_atoms(Input0,Input1),
    concat_all(Input1,Input,' '),
    tcl('label ~a.sent -text {~w}',[C,Input]),
    tcl('frame ~a.top',[C]),
    tcl('frame ~a.top.l',[C],CL),
    tcl('frame ~a.top.m',[C],CM),
    tcl('frame ~a.top.r',[C],CR),
    tcl('frame ~a.top.w',[C],CW),
    tcl('frame ~a.top.s',[C],CS),
    tcl('frame ~a.top.short',[C],CSH),
%    tcl('frame ~a.top.cgn',[C],CCGN),
    tcl('pack ~a ~a ~a ~a ~a ~a -side left',[CL,CM,CR,CW,CS,CSH]),
    titels_tags_to_tk(CL,CM,CW,CR,CS,CSH),
    retractall(tag_nr(_,_)),
    tags_to_tk(Tags,CL,CM,CW,CR,CS,CSH,0,Tags),
    tcl('pack ~a.sent ~a.top -side top -anchor w',[C,C]),
    tcl('update',[]).

tags_to_tk([],_,_,_,_,_,_,_,_).
tags_to_tk([tag(P0,P,_R0,_R,OnOff,Stem,Used,Label,Ref)|Tags],
           CL,CM,CW,CR,CS,CSH,N0,Ts) :-
    (	Stem == Used
    ->	Extra = Used
    ;	Extra = Stem
    ),
%%    (   cgn_tag(R0,R,CGN)
%%    ->  true
%%    ;   CGN = ''
%%    ),
    alpino_tr_tag:tr_tag(Label,Short),
    noclp_assertz(tag_nr(Ref,N0)),
    tcl('set radio~w ~w',[N0,OnOff]),
    tcl('radiobutton ~a.on~w -variable radio~w -value on -selectcolor green -height 1 -bd 1',[CL,N0,N0]),
    unique_tag_code(Ts,N0,Code),
    tcl('bind ~a.on~w <2> { ~s }',[CL,N0,Code]),
    tcl('bind ~a.on~w <3> { ~s }',[CL,N0,Code]),
    tcl('radiobutton ~a.off~w -variable radio~w -value off -selectcolor red -height 1 -bd 1',[CM,N0,N0]),
    tcl('label ~a.used~w -text {~w ~w ~w} -height 1 -bd 2',[CW,N0,P0,P,Used]),
    tcl('label ~a.label~w -text {~w} -height 1 -bd 2',[CR,N0,Label]),
    tcl('label ~a.stem~w -text {~w} -height 1 -bd 2',[CS,N0,Extra]),
    tcl('label ~a.stem~w -text {~w} -height 1 -bd 2',[CSH,N0,Short]),
%    tcl('label ~a.stem~w -text {~w} -height 1 -bd 2',[CCGN,N0,CGN]),
    tcl('pack ~a.on~w ~a.off~w ~a.label~w ~a.used~w ~a.stem~w ~a.stem~w -anchor w ',
	[CL,N0,CM,N0,CR,N0,CW,N0,CS,N0,CSH,N0]),
    N is N0+1,
    tags_to_tk(Tags,CL,CM,CW,CR,CS,CSH,N,Ts).

titels_tags_to_tk(CL,CM,CW,CR,CS,CSH) :-
    tcl('label ~a.top -text {ON}',[CL]),
    tcl('label ~a.top -text {OFF}',[CM]),
    tcl('label ~a.top -text {WORD}',[CW]),
    tcl('label ~a.top -text {TAG}',[CR]),
    tcl('label ~a.top -text {STEM}',[CS]),
    tcl('label ~a.top -text {SHORT TAG}',[CSH]),
    tcl('pack ~a.top ~a.top ~a.top ~a.top ~a.top ~a.top -anchor w',
	[CL,CM,CR,CW,CS,CSH]).

unique_tag_code(A,B,Code) :-
    nth0(B,A,tag(P0,P,_,_,_,_,_,_,_)),
    findall(N, (   nth0(N,A,tag(PP0,PP,_,_,_,_,_,_,_)),
		   overlap(P0,P,PP0,PP)
	       ), Ns),
    radio_codes(Ns,Code,Codes1),
    format_to_chars("set radio~w on~n",[B],Codes1,[]).

radio_codes([],C,C).
radio_codes([H|T],C0,C) :-
    format_to_chars("set radio~w off~n",[H],C0,C1),
    radio_codes(T,C1,C).

%% P0-P and PP0-PP are intervals
%% succeeds if there is an overlap
overlap(P0,P,PP0,PP) :-
    (	integer(PP0),
	integer(P),
	PP0 >= P
    ->	fail
    ;	integer(PP),
	integer(P0),
	PP =< P0
    ->	fail
    ;	true
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% SKIPS %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

skips :-
    add_skip_tags,
    expand_skip_tags,
    keep_track_of_skips.

keep_track_of_skips :-
    (   retract(tag(P0,P,_,_,_,Word,_,skip)),
	\+ skips(P0,P,Word),
	noclp_assertz(skips(P0,P,Word)),
	fail
    ;   open_bracket(P0,P1,Cat),
	skips(P1,P,_),
	\+ open_bracket(P0,P,Cat),
	noclp_assertz(open_bracket(P0,P,Cat)),
	fail
    ;   close_bracket(P0,P1,Cat),
	skips(P1,P,_),
	\+ close_bracket(P0,P,Cat),
	noclp_assertz(close_bracket(P0,P,Cat)),
	fail
    ;   true
    ).

%% these are obligatory!
add_user_skips :-
    (   user_skips(UserSkips)
    ->  add_user_skips(UserSkips)
    ;   true
    ).

add_user_skips([]).
add_user_skips([H|Tail]) :-
    add_user_skip(H),
    add_user_skips(Tail).

remove_alternatives_to_skip(P0,P) :-
    (   clause(tag(Q0,Q,_,_,_,_,_,_),true,Ref),
	overlap(P0,P,Q0,Q),
	erase_tag(Ref),
	fail
    ;   true
    ).

remove_alternatives_to_posflt(P0,P,TagFilter) :-
    (   clause(tag(Q0,Q,_,_,_,_,_,Tag),true,Ref),
	overlap(P0,P,Q0,Q),
	\+ functor(Tag,TagFilter,_),
	erase_tag(Ref),
	fail
    ;   true
    ).

remove_alternatives_to_mwu(P0,P) :-
    (   clause(tag(Q0,Q,_,_,_,_,_,Tag),true,Ref), % remove alternatives
        overlap(P0,P,Q0,Q),
	(   Q0/Q = P0/P
	->  Tag=with_dt(_,_)
	;   true
	),
        erase_tag(Ref),
        fail
    ;   true
    ),
    (   tag(P0,P,_,_,_,_,_,_)
    ->  true
    ;   find_symbolic_begin(P0,R0),
        R is R0 + (P-P0),
        thread_flag(current_input_sentence,Input),
        surface_form(Input,P0,P,Used),
        assert_tag(P0,P,R0,R,Used,Used,mwu,proper_name(both))
    ).

remove_alternatives_to_mwu_alt(P0,P,Alt) :-
    (   clause(tag(Q0,Q,_,_,_,_,_,Tag),true,Ref), % remove alternatives
        overlap(P0,P,Q0,Q),
        erase_tag(Ref),
        fail
    ;   true
    ),
    find_symbolic_begin(P0,R0),
    R is R0 + (P-P0),
    thread_flag(current_input_sentence,Input),
    surface_form(Input,P0,P,Used),
    (   in_lexicon(Tag,Label,[Alt],[],His,[]),
	assert_tag(P0,P,R0,R,Label,Used,mwu_alt(His),Tag),
	fail
    ;   true
    ).

add_user_skip(skip(P0,P)) :-
    !,
    find_symbolic_begin(P0,R0),
    remove_alternatives_to_skip(P0,P),
    R is R0 + (P-P0),
    thread_flag(current_input_sentence,Input),
    surface_form(Input,P0,P,Used),
    assert_tag(P0,P,R0,R,Used,Used,skip,skip).

add_user_skip(phantom_skip(_)) :-
    !.

add_user_skip(postag(P0,P,Tag,Surf)) :-
    !,
    find_symbolic_begin(P0,R0),
    remove_alternatives_to_skip(P0,P),
    R is R0 + (P-P0),
    concat_all(Surf,SurfAtom,' '),
    assert_tag(P0,P,R0,R,SurfAtom,SurfAtom,skip,Tag).

/*
add_user_skip(universal(P0,P,Surf)) :-
    find_symbolic_begin(P0,R0),
    remove_alternatives_to_skip(P0,P),
    R is R0 + (P-P0),
    concat_all(Surf,SurfAtom,' '),
    (   universal_tag(Tag,Score),
        assert_tag(P0,P,R0,R,SurfAtom,SurfAtom,
                   normal(universal(Score)),Tag),
        fail
    ;   true
    ).

add_user_skip(subset_universal(P0,P,Tag,Surf)) :-
    find_symbolic_begin(P0,R0),
    remove_alternatives_to_skip(P0,P),
    R is R0 + (P-P0),
    concat_all(Surf,SurfAtom,' '),
    (   universal_tag(Tag,Score),
        assert_tag(P0,P,R0,R,SurfAtom,SurfAtom,
                   normal(universal(Score)),Tag),
        fail
    ;   true
    ).
*/
%    assert_tag(P0,P,R0,R,SurfAtom,SurfAtom,skip,Tag).

add_user_skip(posflt(P0,P,Tag)) :-
    !,
    remove_alternatives_to_posflt(P0,P,Tag).

add_user_skip(mwu(P0,P)) :-
    !,
    remove_alternatives_to_mwu(P0,P).

add_user_skip(mwu_alt(P0,P,Alt)) :-
    !,
    remove_alternatives_to_mwu_alt(P0,P,Alt).

add_user_skip(_).

%% compare_alternatives_for_cgn(P0,P,CgnTag,Ref) :-
%%    findall(Tag-Ref,clause(tag(_,_,P0,P,_,Surf,_,Tag),true,Ref),TagList0),
%%    sort(TagList0,TagList1),
%%    select_alternatives(TagList1,TagList,EraseList,CgnTag,Surf),
%%    (    TagList == []
%%    ->   fail
%%    ;    member(Ref,EraseList)
%%    ).
%%
%% select_alternatives([],[],[],_,_).
%% select_alternatives([Tag-Ref|Ts0],Ts,Es,Cgn,Word) :-
%%    (   alternative_for_cgn(Cgn,Tag,Word)
%%    ->  Ts = [Tag-Ref|Ts1],
%%	Es=Es1
%%    ;   Ts = Ts1,
%%	Es=[Ref|Es1]
%%    ),
%%    select_alternatives(Ts0,Ts1,Es1,Cgn,Word).

find_symbolic_begin(P0,R0) :-
    tag(_,P0,_,R0,_,_,_,_),
    !.
find_symbolic_begin(P0,R0) :-
    tag(P0,_,R0,_,_,_,_,_),
    !.
find_symbolic_begin(0,0).

add_skip_tags :-
    findall(Tag,skippable(Tag),Tags0),
    sort(Tags0,Tags),
    assert_all_tags(Tags).

assert_all_tags([]).
assert_all_tags([tag(A,B,C,D,E,F,G,H)|T]) :-
    assert_tag(A,B,C,D,E,F,G,H),
    assert_all_tags(T).

skippable(tag(P0,P,R0,R,Surf0,Surf0,skip,skip)):-
    word_form(Surf),
    skip_word(Surf),
    tag(P0,P,R0,R,_,Surf0,_His,_),
    (   Surf0 = Surf
    ;   alpino_unknowns:decap_first(Surf0,Surf)
    ),	
    \+ alpino_unknowns:wikipedia_list(Surf).
skippable(tag(P0,P,R0,R,Surf,Surf,skip,skip)):-
    findall(Q0/Q,tag(Q0,Q,_,_,_,_,normal(longpunct),_),Pairs),
    tag(P0,P,R0,R,_,Surf,_His,Tag),
    skip_tag(Tag),
    \+ part_of_longpunct(Pairs,P0,P),
    \+ alpino_unknowns:wikipedia_list(Surf).
skippable(tag(P0,P,R0,R,'.','.',skip,skip)) :-
    tag(P0,P,R0,R,'.','.',normal(normal),punct(punt)),
    tag(P,_,R,_,Comma,Comma,normal(normal),punct(_)),
    punct_after_abbr_dot(Comma).
skippable(tag(P0,P,R0,R,Words,Words,skip,skip)) :-
    skip_words(Words),
    skip_words_tags(Words,P0,P,R0,R).
skippable(tag(P0,P,R0,R,[W0|Words],[W0|Words],skip,skip)) :-
    tag(P0,P1,R0,R1,_,W0,_,_),
    skip_larger(W0),
    skip_larger_tags(Words,P1,P,R1,R).
skippable(tag(P0,P1,R0,R1,Stem,Surf,skip,skip)):-
    tag(P0,P1,R0,R1,Stem,Surf0,_,Tag),
    skip_pair(Surf0,Surf1,Surf),
    tag(P1,_P,R1,_R,Stem,Surf1,_,Tag),
    \+ (   tag(Q0,Q,_,_,_,_,_,number(hoofd(_))),
	   overlap(Q0,Q,P0,P1)
       ),
%    \+ (   tag(_,P0,_,R0,_,Surfmin1,_,Tag),
%	   skip_pair(Surfmin1,Surf0,_)
%       ),
    debug_message(1,"~w ~w ---> ~w ~w ~w ~n",[Surf0,Surf1,Surf,P0,P1]).
skippable(tag(P0,P1,R0,R1,Stem,Surf,skip,skip)):-
    word_form(He),
    hesitation(He),
    tag(P1,P2,R1,R2,_,He,_,_),
    tag(P0,P1,R0,R1,Stem,Surf0,_,Tag),
    skip_pair(Surf0,Surf1,Surf),
    tag(P2,_,R2,_,Stem,Surf1,_,Tag),
    debug_message(1,"~w ~w ~w ---> ~w~n",[Surf,He,Surf,Surf]).
skippable(tag(P0,P1,R0,R1,de,de,skip,skip)):-
    tag(P0,P1,R0,R1,de,de,normal(normal),determiner(de)),
    tag(P1,_,R1,_,zijn,zijn,normal(normal),determiner(pron)).
skippable(tag(P0,P1,R0,R1,de,de,skip,skip)):-
    tag(P0,P1,R0,R1,de,de,normal(normal),determiner(de)),
    tag(P1,_,R1,_,alle,alle,normal(normal),determiner(alle,nwh,mod,pro,nparg)).
skippable(tag(P0,P1,R0,R1,de,de,skip,skip)):-
    tag(P0,P1,R0,R1,de,de,normal(normal),determiner(de)),
    tag(P1,_,R1,_,deze,deze,normal(normal),determiner(de,nwh,nmod,pro,nparg)).
skippable(tag(P0,P1,R0,R1,de,de,skip,skip)):-
    tag(P0,P1,R0,R1,de,de,normal(normal),determiner(de)),
    tag(P1,_,R1,_,u,uw,normal(normal),determiner(pron)).
skippable(tag(P0,P1,R0,R1,de,de,skip,skip)):-
    tag(P0,P1,R0,R1,de,de,normal(normal),determiner(de)),
    tag(P1,_,R1,_,degeen,degene,normal(normal),pronoun(nwh,thi,sg,de,both,def,strpro)).


part_of_longpunct([H|T],P0,P) :-
    lists:member(Q0/Q,[H|T]),
    Q0 =< P0,     P =< Q.

punct_after_abbr_dot(',').
punct_after_abbr_dot('-').
punct_after_abbr_dot(';').
punct_after_abbr_dot(';').
punct_after_abbr_dot('&').
punct_after_abbr_dot(':').

skip_pair(Surf,Surf,Surf) :-
    \+ alpino_unknowns:wikipedia_list(Surf),
    \+ alpino_unknowns:longpunct(Surf),
    \+ alpino_unknowns:name_initial(Surf).
skip_pair('\'k',ik,ik).
skip_pair(ik,'\'k',ik).
skip_pair('De',de,de).
skip_pair(dat,da,dat).

skip_words_tags([],P,P,R,R).
skip_words_tags([W|Ws],P0,P,R0,R) :-
    tag(P0,P1,R0,R1,_,W,_,_),
    skip_words_tags(Ws,P1,P,R1,R).

expand_skip_tags :-
    findall(Tag,expand_skip(Tag),Tags0),
    sort(Tags0,Tags),
    assert_all_tags(Tags).

expand_skip(tag(P0,P,R0,R,Stem,Surf,skip(Count,[],Rights,His),Tag)) :-
    tag(P0,P1,R0,R1,Stem,Surf,His,Tag),
    Tag \= skip,
    Tag \= longpunct,
    Tag \= 'UNKNOWN',
    skip_labels(P1,P,R1,R,Rights,Count),
    \+ tag(P0,P,R0,R,_,_,_,Tag).

expand_skip(tag(0,P,R0,R,Stem,Surf,skip(Count,Lefts,Rights,His),Tag)) :-
    skip_labels(0,P1,R0,R1,Lefts,Count1),
    tag(P1,P2,R1,R2,Stem,Surf,His,Tag),
    Tag \= skip,
    Tag \= longpunct,
    Tag \= 'UNKNOWN',
    skip_labels(P2,P,R2,R,Rights,Count1,Count),
    \+ tag(0,P,R0,R,_,_,_,Tag).

%% at least 1
skip_labels(P0,P,R0,R,Names,C) :-
    skip_closure(P0,P,R0,R,Names,0,C).

%% 0 or more
skip_labels(P,P,R,R,[],C,C).
skip_labels(P0,P,R0,R,Names,C0,C) :-
    skip_closure(P0,P,R0,R,Names,C0,C).
    
skip_closure(P0,P,R0,R,Names,C0,C) :-
    C is C0 + 1,
    skip_label(P0,P,R0,R,Names).

skip_closure(P0,P,R0,R,Names,C0,C) :-
    skip_label(P0,P1,R0,R1,Names0),
    skip_label(P1,P2,R1,R2,Names1),
    lists:append(Names0,Names1,Names2),
    C1 is C0 + 2,
    skip_closure_lm(P2,P,R2,R,Names2,Names,C1,C).

skip_closure_lm(P0,P,R0,R,Names0,Names,C0,C) :-
    skip_label(P0,P2,R0,R2,Names1),
    !,
    lists:append(Names0,Names1,Names2),
    C2 is C0 + 1,
    skip_closure_lm(P2,P,R2,R,Names2,Names,C2,C).
skip_closure_lm(P,P,R,R,Names,Names,C,C).


skip_label(P0,P,R0,R,Ws) :-
    tag(P0,P,R0,R,_,W,_,skip),
    (   P =:= P0 + 1
    ->  Ws = [W]
    ;   atom(W)
    ->  atom_codes(W,WCodes),
        split_string(WCodes," ",ListCodes),
        length(ListCodes,Len),
        (   P =:= P0 + Len
        ->  list_codes(ListCodes,W,[])
        ;   format(user_error,"error in skip-label: ~w ~w ~w~n",[W,P0,P]),
            fail
        )
    ;   W = [_|_],
	W = Ws
    ).

list_codes([],R,R).
list_codes([H0|T0],[H|R0],R) :-
    atom_codes(H,H0),
    list_codes(T0,R0,R).

skip_tag(punct(aanhaal_both)).
skip_tag(punct(aanhaal_links)).
skip_tag(punct(aanhaal_rechts)).
skip_tag(punct(hellip)).
skip_tag(punct(ligg_streep)).
skip_tag(punct(schuin_streep)).
skip_tag(punct(is_gelijk)).
skip_tag(punct(haak_open)).
skip_tag(punct(haak_sluit)).
skip_tag(punct(komma)).
skip_tag(punct(punt_komma)).
skip_tag(punct(dubb_punt)).

%% all tokens that can be ignored
skip_word(X) :-
    hesitation(X).
skip_word(mm).
skip_word(xxx).
skip_word('<p>').
skip_word('&virtpunt;').
skip_word('**').
skip_word('***').
skip_word('****').
skip_word(Atom) :-   %html codes
    atom(Atom),
    atom_concat('<',Rest,Atom),
    atom_concat(_,'>',Rest).
skip_word(Atom) :-      % ignore mis-tokenized stuff in brackets
    atom(Atom),
    atom_concat('(',_,Atom),
    atom_concat(_,')',Atom).
skip_word(Atom) :-
    atom(Atom),
    atom_concat(_,'\\o',Atom).  % onomatopoeia in ernestus corpus
skip_word(bracket(_)).

skip_larger_tags([H|T],P0,P,R0,R):-
    tag(P0,P1,R0,R1,_,H,_,_),
    skip_larger(H),
    skip_larger_tags1(T,P1,P,R1,R).

skip_larger_tags1([H|T],P0,P,R0,R):-
    tag(P0,P1,R0,R1,_,H,_,_),
    skip_larger(H),!,
    skip_larger_tags1(T,P1,P,R1,R).
skip_larger_tags1([],P,P,R,R).

skip_larger('>').
skip_larger('>>').
skip_larger('>>>').
skip_larger('>>>').
skip_larger('>>>>').
skip_larger('>>>>>').
skip_larger('>>>>>>').
skip_larger('>>>>>>>').
skip_larger('>>>>>>>>').
skip_larger('>>>>>>>>>').
skip_larger('>>>>>>>>>>').
skip_larger('>>>>>>>>>>>').
skip_larger('>>>>>>>>>>>>').
skip_larger('>>>>>>>>>>>>>').
skip_larger('>>>>>>>>>>>>>>').


%% all sequences of tokens that can be ignored
skip_words([H|List]) :-
    add_skip_word(H,List).
skip_words([',',Hes,',']) :-
    hesitation(Hes).
skip_words(['(',')']).
skip_words(['(','!',')']).
skip_words(['(','!!',')']).
skip_words(['(','!!!',')']).
skip_words(['(','!!!!',')']).
skip_words(['(','-',')']).
skip_words(['(','--',')']).
skip_words(['(','---',')']).
skip_words(['(','----',')']).
skip_words(['(','.',')']).
skip_words(['(','..',')']).
skip_words(['(','...',')']).
skip_words(['(','....',')']).
skip_words(['(','?',')']).
skip_words(['(','??',')']).
skip_words(['(','???',')']).
skip_words(['(','????',')']).
skip_words(['(','*',')']).
skip_words(['(','**',')']).
skip_words(['(','***',')']).
skip_words(['(','****',')']).
skip_words(['(','*','*',')']).
skip_words(['(','*','*','*',')']).
skip_words(['(','*','*','*','*',')']).
skip_words(['(','*','*','*','*','*',')']).
skip_words(['(','*','*','*','*','*','*',')']).
skip_words(['(','*','*','*','*','*','*','*',')']).
skip_words(['(','*','*','*','*','*','*','*','*',')']).
skip_words(['[','!',']']).
skip_words(['[','!!',']']).
skip_words(['[','!!!',']']).
skip_words(['[','!!!!',']']).
skip_words(['[','-',']']).
skip_words(['[','--',']']).
skip_words(['[','---',']']).
skip_words(['[','----',']']).
skip_words(['[','.',']']).
skip_words(['[','..',']']).
skip_words(['[','...',']']).
skip_words(['[','....',']']).
skip_words(['[','?',']']).
skip_words(['[','??',']']).
skip_words(['[','???',']']).
skip_words(['[','????',']']).
skip_words(['[','*',']']).
skip_words(['[','**',']']).
skip_words(['[','***',']']).
skip_words(['[','****',']']).

%% ?

hesitation(Atom) :-
    alpino_unknowns:decap_first(Atom,Atom2),!,
    hesitation1(Atom2).
hesitation(Atom) :-
    hesitation1(Atom).

hesitation1('*').
hesitation1('#').
hesitation1('·').
hesitation1('^').

hesitation1(eeh).
hesitation1(eh).
hesitation1(ehm).
hesitation1(ehh).
hesitation1(euh).
hesitation1(euhm).
hesitation1(ggg).
hesitation1(hm).
hesitation1(hmm).
hesitation1(hmmm).
hesitation1(Atom) :-
    atom(Atom),
    atom_concat(hmmm,_,Atom).
hesitation1(Atom) :-
    atom(Atom),
    atom_concat(mmmm,_,Atom).
hesitation1(huh).
hesitation1(hu).
hesitation1(hum).
hesitation1('mm-hm').
hesitation1('mm-hu').
hesitation1('mm-uh').
hesitation1(mmm).
hesitation1(mmmm).
hesitation1(mwah).
hesitation1(uh).
hesitation1(uhm).
hesitation1(uhmm).
hesitation1(uhu).
hesitation1(uhum).
hesitation1(zuh).
hesitation1('<@>').
hesitation1('<@m>').

hesitation1(Atom) :-
    atom(Atom),
    atom_concat(_,'*a',Atom).  % CGN mark-up

hesitation1(Atom) :-
    atom(Atom),
    atom_concat(_,'\\*',Atom).  % broken word in ernestus

hesitation1(Atom) :-
    atom(Atom),
    atom_concat(_,'\\v',Atom).  % wrongly produced in ernestus

hesitation1(G) :-
    garbage(G).

%% sometimes garbage ends up here, we only want to skip it!
garbage(Atom) :-
    atom(Atom),
    garbage_atom(Atom).

garbage_atom(Atom) :-
    atom_concat('align=',_,Atom).
garbage_atom(Atom) :-
    atom_concat('bgcolor=',_,Atom).
garbage_atom(Atom) :-
    atom_concat('colspan=',_,Atom).
garbage_atom(Atom) :-
    atom_concat('quote=',_,Atom).
garbage_atom(Atom) :-
    atom_concat(_,px,Atom).
garbage_atom('PIC').
garbage_atom('FIG').
garbage_atom('FILE').
garbage_atom('__').
garbage_atom('__').
garbage_atom(Atom) :-
    atom(Atom),
    sub_atom(Atom,_,_,_,'_____').
garbage_atom(Atom) :-
    atom(Atom),
    sub_atom(Atom,_,_,_,'-----').
garbage_atom(Atom) :-
    atom(Atom),
    sub_atom(Atom,_,_,_,'=====').
garbage_atom('NOTOC').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% ONLY DO LEX ANALYSIS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_one(Flag,Key) :-
    hdrug:a_sentence(Key,_,Sent0),
    hdrug:extern_phon(Sent0,Sent),
    do_one(Flag,Key,Sent).

do_one(lex,Key,Sent) :-
    format("KEY#~w|",[Key]),
    write_list(Sent),nl,
    (   lexical_analysis(Sent),
	add_skips_for_unknowns(Sent), % have UNKNOWN tags!
	fail
    ;   true
    ),
    display_lexical_analysis(Sent).

do_one(tag,Key,Sent) :-
    hdrug_util:set_flag(inform_postags,on),
    format("# sent_id = ~w~n",[Key]),
    format("# text = ",[]),
    write_list(Sent),nl,
    lexical_analysis(Sent).

lex_all :-
    do_all(lex).

tag_all :-
    do_all(tag).

lex_all_keys :-
    do_all_keys(lex).

tag_all_keys :-
    do_all_keys(tag).

do_all_keys(Flag) :-
    findall(K,user:a_sentence_with_map(_,K,_),Ks),
    do_all_keys(Ks,Flag).

do_all_keys([],_).
do_all_keys([Key|Keys],Flag):-
    do_one(Flag,Key),
    do_all_keys(Keys,Flag).


do_all(Flag) :-
    initialize_flag(current_ref,0),
    repeat,
    hdrug_flag(current_ref,Ref0),
    read_line(Line),
    (	Line == []
    ->	fail
    ;   Line = [37|_]
    ->  fail           % lines starting with % are treated as comments
    ;	Line == end_of_file
    ->	!
    ;   append(Prefix,[124|ParseLine],Line)
    ->  atom_codes(Ref,Prefix),
	set_flag(current_ref,Ref),
        debug_message(1,"**** lexical analysis of ~w~n",[Ref]),
        codes_to_words(ParseLine,Words0),
        hdrug:check_length(Words0),
        hdrug:extern_phon(Words0,Words),
        do_one(Flag,Ref,Words),
	fail
    ;   hdrug_flag(current_ref,Ref0),
	(   integer(Ref0)
        ->  Ref is Ref0 + 1,
            set_flag(current_ref,Ref)
        ;   Ref = s(Ref0),
	    set_flag(current_ref,Ref)
        ),
        debug_message(1,"**** lexical analysis of line number ~w~n",[Ref]),
        codes_to_words(Line,Words0),
        hdrug:check_length(Words0),
        hdrug:extern_phon(Words0,Words),
        do_one(Flag,Ref,Words),
        fail
    ).

lexical_analysis_one(Words,Tag,Root) :-
    length(Words,P),
    lexical_analysis(Words),
    tag(P0,P,P0,P,Root,_,_,Tag).

%:- initialize_flag(display_lexical_analysis,tagger_test).
:- initialize_flag(display_lexical_analysis,normal).

display_lexical_analysis(Sent) :-
    hdrug_flag(display_lexical_analysis,Flag),
    display_lexical_analysis(Flag,Sent).

display_lexical_analysis(normal,Sent) :-
    findall(t(P0,P,Tag,History,Stem,Surf,Ref),clause(tag(P0,P,_,_,Stem,Surf,History,Tag),_,Ref),Tags0),
    sort(Tags0,Tags),
    display_tags(Tags,Sent).

display_lexical_analysis(unknowns,[Sent|_]) :-
    findall(t(P0,P,Tag,History,Stem,Surf,Ref),clause(tag(P0,P,_,_,Stem,Surf,History,Tag),_,Ref),Tags0),
    sort(Tags0,Tags),
    display_unknowns(Tags,Sent).

display_lexical_analysis(tagger_test,Sent) :-
    alpino_postagger:all_tags(Tags,_),
    display_tags(Tags,Sent).

display_unknowns([],Sent) :-
    format(user_error,"unknown: ~w~n",[Sent]).
display_unknowns([_|_],_).

display_tags([],_Sent).
display_tags([H|T],Sent) :-
    display_tag(H,Sent),
    display_tags(T,Sent).

display_tag(t(Q0,Q,Tag,History,Stem,Surf),_Sent) :-
    prettyvars(Tag),
    format("TAG#~w|~w|~w|~w|~w|~w~n",[Q0,Q,Surf,Tag,History,Stem]).

display_tag(t(Q0,Q,Tag,History,Stem,Surf,Ref),_Sent) :-
    prettyvars(Tag),
    (   alpino_score_ref:score_ref(Ref,Score)
    ->  format("TAG#~w|~w|~w|~w|~w|~w|~w~n",[Q0,Q,Surf,Tag,History,Stem,Score])
    ;   format("TAG#~w|~w|~w|~w|~w|~w~n",[Q0,Q,Surf,Tag,History,Stem])
    ).

display_tag(t(Q0,Q,Tag),Sent) :-
    get_surface(Q0,Q,Sent,Surf),
    prettyvars(Tag),
    format("TAG#~w|~w|",[Q0,Q]),
    write_list(Surf),
    format("|~s~n",[Tag]).

get_surface(Q0,Q,Sent,Part) :-
    length(Prefix,Q0),
    L is Q-Q0,
    length(Part,L),
    append(Prefix,Rest,Sent),
    append(Part,_,Rest).

write_list([]).
write_list([H|T]) :-
    write(H),
    write_list_(T).

write_list_([]).
write_list_([H|T]) :-
    tab(1), write(H),
    write_list_(T).

unused_pos(Pos,MaxPos) :-
    between(0,MaxPos,Pos),
    \+ is_used(Pos).

unused_dict_pos(Pos,MaxPos) :-
    between(0,MaxPos,Pos),
    \+ is_dict_used(Pos).

%% this can be defined shorter, but this way we have first argument indexing
%% for the very common case.
is_used(P1) :-
    tag(P1,_,_,_,_,_,I,_),
    I \== 'UNKNOWN'.
is_used(P1) :-
    tag(P0,P,_,_,_,_,I,_),
    I \== 'UNKNOWN',
    P0 < P1,
    P1 < P.
is_used(P) :-
    close_bracket(P,_,_).
is_used(P1) :-
    close_bracket(P0,P,_),
    P0 < P1, P1 < P.
is_used(P) :-
    open_bracket(P,_,_).
is_used(P1) :-
    open_bracket(P0,P,_),
    P0 < P1, P1 < P.

is_dict_used(P1) :-
    tag(P1,P2,_,_,_,_,normal(Type),_),
    \+ Type = url,
    \+ probably_wrong_tag(P1,P2).
is_dict_used(P1) :-
    tag(P0,P,_,_,_,_,normal(Type),_),
    P0 < P1,
    P1 < P,
    \+ Type = url,
    \+ probably_wrong_tag(P0,P).
is_dict_used(P) :-
    close_bracket(P,_,_).
is_dict_used(P1) :-
    close_bracket(P0,P,_),
    P0 < P1, P1 < P.
is_dict_used(P) :-
    open_bracket(P,_,_).
is_dict_used(P1) :-
    open_bracket(P0,P,_),
    P0 < P1, P1 < P.
is_dict_used(P1) :-
    alpino_unknowns:wikipedia_list,
    (   tag(P1,_,_,_,_,_,name(_),_)
    ;   tag(P0,P,_,_,_,_,name(_),_),
        P0 < P1,
        P1 < P
    ).

% alternative_to_het_sg_noun(P1,P) :-
%     tag(P1,P,_,_,_,_,_,Tag),
%     Tag \= noun(het,_,sg),
%     Tag \= noun(het,_,sg,_).
% alternative_to_het_sg_noun(P1,P) :-
%     tag(P1,P,_,_,_,_,_,Tag),
%     (  Tag = noun(het,_,sg)
%     ;  Tag = noun(het,_,sg,_)
%     ),
%     tag(P,_,_,_,_,_,_,adjective(_,pred)).

% alternative_to_de_sg_noun(P1,P) :-
%     tag(P1,P,_,_,_,_,_,Tag),
%     Tag \= noun(de,_,sg),
%     Tag \= noun(de,_,sg,_).

%%% now treated differently 
%probably_wrong_tag(P1,P) :-
%    search_tag_stem(de,tag(_,P1,_,_,de,_,_,determiner(de))),
%    \+ alternative_to_het_sg_noun(P1,P).
%
%probably_wrong_tag(P1,P) :-
%    search_tag_stem(het,tag(_,P1,_,_,het,'Het',_,determiner(het,nwh,nmod,pro,nparg,wkpro))),
%    \+ alternative_to_de_sg_noun(P1,P).

probably_wrong_tag(P1,P) :-
    search_tag_stem(te,tag(_,P1,_,_,_,_,preposition(te,[],nodet),_)),
    \+ te_following_tag(P1,P).

te_following_tag(P1,P) :-
    tag(P1,P,_,_,_,_,_,Tag),
    te_following_tag(Tag).

te_following_tag(Tag) :-
    adjective_tag(Tag).
te_following_tag(Tag) :-
    infinitive_tag(Tag).
te_following_tag(Tag) :-
    proper_name_or_tmp_np_tag(Tag).
te_following_tag(determiner(der)).
te_following_tag(Tag) :-
   skip_tag(Tag).

tagtime(Module:Goal,Msg) :-
    hdrug_flag(debug,Debug),
    (    Debug > 0
    ->   statistics(runtime,[Tstart,_]),
	 bb_put(tagtime,0),
	 tagtime(Module:Goal,Tstart,Msg)
    ;    call(Module:Goal)
    ).

tagtime(Module:Goal,_,_):-
    call(Module:Goal),
    bb_get(tagtime,N0),
    N is N0+1,
    bb_put(tagtime,N).
tagtime(_,Tstart,Msg):-
    bb_get(tagtime,N),
    statistics(runtime,[Tend,_]),
    Time is Tend-Tstart,
    (   N < 1
    ->  debug_message(1,"tag ~w has no solutions; hope this is ok...~n",[Msg])
    ;   N > 1, Time > 50
    ->  debug_message(1,"tag ~w ~w msec ~w solutions~n",[Msg,Time,N])
    ;   true
    ),
    fail.

guess_unknowns(Input,MaxPos) :-
    findall(Pos,unused_dict_pos(Pos,MaxPos),Ps),
    guess_unknown_words(Input,Ps).      % unknowns.pl

remove_brackets([],[]).
remove_brackets([H|T],Rest) :-
    remove_bracket(H,T,T2,Rest,Rest0),
    remove_brackets(T2,Rest0).

remove_bracket(bracket(open),T0,T,Rest,Rest) :-
    !,
    remove_bracket_type(T0,T).
remove_bracket(bracket(close),T0,T,Rest,Rest) :-
    !,
    remove_bracket_type(T0,T).
remove_bracket(W,R,R,[W|T],T).

remove_bracket_type([],[]).
remove_bracket_type([H|T],R) :-
    (   atom(H),
	atom_codes(H,[64|_])
    ->  R=T
    ;   R=[H|T]
    ).

extract_skip_positions([],Rest,Rest,_,[]).
extract_skip_positions([H|T],Rest0,Rest,P0,[skip(P0,P)|SkipIntervals]) :-
    start_skip([H|T],Input0),
    !,
    find_end_skip(Input0,Input,Rest0,Rest1,P0,P),
    extract_skip_positions(Input,Rest1,Rest,P,SkipIntervals).
extract_skip_positions([H|T],Rest0,Rest,P0,[phantom_skip(P0)|SkipIntervals]) :-
    phantom_skip([H|T],Input0),
    !,
    find_end_skip(Input0,Input,Rest0,Rest1,P0,P),
    (   P is P0 + 1
    ->  true
    ;   format(user_error,"ERROR phantom_skips only supported for single tokens!~n",[]),
        fail
    ),
    extract_skip_positions(Input,Rest1,Rest,P,SkipIntervals).
extract_skip_positions([H|T],Rest0,Rest,P0,[postag(P0,P,Tag,Surf)|SkipIntervals]) :-
    start_postag_skip([H|T],Input0,Tag),
    !,
    find_end_skip(Input0,Input,Rest0,Rest1,P0,P),
    (   P0 =:= P
    ->  format(user_error,"error: postag skip without token: ~w!~n",
               [Tag]),
        raise_exception(alpino_error(unbalanced_brackets))
    ;   true
    ),
    append(Surf0,Input,Input0),  % Surf0 includes closing bracket
    append(Surf,[_],Surf0),
    extract_skip_positions(Input,Rest1,Rest,P,SkipIntervals).
extract_skip_positions([H|T],Rest0,Rest,P0,[alt(P0,Lex,Surf)|SkipIntervals]) :-
    start_alt_skip([H|T],Input0,Lex),
    !,
    find_end_skip(Input0,Input,Rest0,Rest1,P0,P),
    (   P0 =:= P
    ->  format(user_error,"error: alt skip without word: ~w!~n",
               [Lex]),
        raise_exception(alpino_error(unbalanced_brackets))
    ;   P0 + 1 < P
    ->  format(user_error,"error: alt skip with more than a single word: ~w!~n",
	       [Lex])
    ;   true
    ),
    append([Surf,_],Input,Input0), 
    extract_skip_positions(Input,Rest1,Rest,P,SkipIntervals).
extract_skip_positions([H|T],[Word|Rest0],Rest,P0,[folia(P0,P,Lemma,Tag)|SkipIntervals]) :-
    start_folia_skip([H|T]),
    !,
    (   folia_skip([H|T],Input,Lemma,Tag,Word)
    ->  true
    ;   format(user_error,"error in folia metatag!~n",[]),
        raise_exception(alpino_error(unbalanced_brackets))
    ),
    P is P0 + 1,
    extract_skip_positions(Input,Rest0,Rest,P,SkipIntervals).
/*
extract_skip_positions([H|T],Rest0,Rest,P0,[universal(P0,P,Surf)|SkipIntervals]) :-
    start_universal_skip([H|T],Input0),!,
    find_end_skip(Input0,Input,Rest0,Rest1,P0,P),
    (   P0 =:= P
    ->  format(user_error,"error: universal skip without token!~n",
               []),
        raise_exception(alpino_error(unbalanced_brackets))
    ;   true
    ),
    append(Surf0,Input,Input0),  % Surf0 includes closing bracket
    append(Surf,[_],Surf0),
    extract_skip_positions(Input,Rest1,Rest,P,SkipIntervals).
extract_skip_positions([H|T],Rest0,Rest,P0,
                       [subset_universal(P0,P,Tag,Surf)|SkipIntervals]) :-
    start_subset_universal_skip([H|T],Input0,Tag),!,
    find_end_skip(Input0,Input,Rest0,Rest1,P0,P),
    (   P0 =:= P
    ->  format(user_error,"error: subset_universal skip without token!~n",
               []),
        raise_exception(alpino_error(unbalanced_brackets))
    ;   true
    ),
    append(Surf0,Input,Input0),  % Surf0 includes closing bracket
    append(Surf,[_],Surf0),
    extract_skip_positions(Input,Rest1,Rest,P,SkipIntervals).
*/
extract_skip_positions([H|T],Rest0,Rest,P0,[posflt(P0,P,Tag)|SkipIntervals]) :-
    start_posflt_skip([H|T],Input0,Tag),
    !,
    find_end_skip(Input0,Input,Rest0,Rest1,P0,P),
    (   P0 =:= P
    ->  format(user_error,"error: posflt skip without token: ~w!~n",
               [Tag]),
        raise_exception(alpino_error(unbalanced_brackets))
    ;   true
    ),
    extract_skip_positions(Input,Rest1,Rest,P,SkipIntervals).
extract_skip_positions([H|T],Rest0,Rest,P0,[mwu(P0,P)|SkipIntervals]) :-
    start_mwu_skip([H|T],Input0),
    !,
    find_end_skip(Input0,Input,Rest0,Rest1,P0,P),
    (   P0 =:= P
    ->  format(user_error,"error: mwu skip without token!~n",
               []),
        raise_exception(alpino_error(unbalanced_brackets))
    ;   true
    ),
    extract_skip_positions(Input,Rest1,Rest,P,SkipIntervals).
extract_skip_positions([H|T],Rest0,Rest,P0,[mwu_alt(P0,P,Alt)|SkipIntervals]) :-
    start_mwu_alt_skip([H|T],Input0,Alt),
    !,
    find_end_skip(Input0,Input,Rest0,Rest1,P0,P),
    (   P0 =:= P
    ->  format(user_error,"error: mwu skip without token!~n",
               []),
        raise_exception(alpino_error(unbalanced_brackets))
    ;   true
    ),
    extract_skip_positions(Input,Rest1,Rest,P,SkipIntervals).
extract_skip_positions([H|T],[H1|Rest0],Rest,P0,SkipIntervals) :-
    escaped_input(H,H1),
    !,
    P1 is P0 + 1,
    extract_skip_positions(T,Rest0,Rest,P1,SkipIntervals).
extract_skip_positions([H|T],[H1|Rest0],Rest,P0,SkipIntervals) :-
    bracket(H,H1),!,
    P1 is P0 + 1,
    extract_skip_positions(T,Rest0,Rest,P1,SkipIntervals).
extract_skip_positions([H|T],[H|Rest0],Rest,P0,SkipIntervals) :-
    P1 is P0 + 1,
    extract_skip_positions(T,Rest0,Rest,P1,SkipIntervals).

escaped_input('\\[','[').
escaped_input('\\]',']').
escaped_input('\\\\[','\\[').
escaped_input('\\\\]','\\]').

bracket('[',bracket(open)).
bracket(']',bracket(close)).

start_skip(['[','@skip'|L],L).

phantom_skip(['[','@phantom'|L],L).

% id_skip(['[','@id',Id,']'|L],Id,L).

start_postag_skip(['[','@postag',Tag0|L],L,Tag) :-
    atom_term(Tag0,Tag).

start_alt_skip(['[','@alt',Tag|L],L,Tag).

% for historical reasons:
start_alt_skip(['[','@add_lex',Tag|L],L,Tag).

% start_universal_skip(['[','@universal'|L],L).

% start_subset_universal_skip(['[','@subset_universal',Tag0|L],L,Tag) :-
%     atom_term(Tag0,Tag).

start_posflt_skip(['[','@posflt',Tag0|L],L,Tag) :-
    atom_term(Tag0,Tag).

start_mwu_alt_skip(['[','@mwu_alt',Word|L],L,Word).

start_mwu_skip(['[','@mwu'|L],L).

start_folia_skip(['[','@folia'|_]).

folia_skip(['[','@folia',Lemma,Postag,Word,']'|Tail],Tail,Lemma,Postag,Word).

find_end_skip([],_,_,_,_,_) :-
    !,
    format(user_error,"error: no matching end-bracket for skip!!~n",[]),
    fail.
find_end_skip(['['],_,_,_,_,_) :-
    !,
    format(user_error,"error: opening bracketing within skip!!~n",[]),
    fail.
find_end_skip([']'|T],T,Rest,Rest,P,P) :-
    !.
find_end_skip([H|T],Input,[H|Rest0],Rest,P0,P):-
    P1 is P0 + 1,
    find_end_skip(T,Input,Rest0,Rest,P1,P).

%% "universal" tag:
%% :- use_module(universal_tag).

remove_variants([],[],0).
remove_variants([H|T],L,Bool) :-
    remove_variants(T,H,L,Bool).

remove_variants([],H,[H],0).
remove_variants([B|L0],A,L,Bool) :-
    add_frozen_copy([A,B|L0],L1),
    keysort(L1,L2),
    check_variant_frozen(L2,L,0,Bool).

add_frozen_copy([],[]).
add_frozen_copy([ITEM|T0],[(FROZEN/Cons)-ITEM|T]) :-
    copy_term(ITEM,FROZEN,Cons),
    numbervars(FROZEN/Cons,0,_),
    add_frozen_copy(T0,T).

check_variant_frozen([],[],Bool,Bool).
check_variant_frozen([FROZEN-ITEM|T0],[ITEM|T],Bool0,Bool) :-
    check_variant_frozen(T0,FROZEN,T,Bool0,Bool).

check_variant_frozen([],_,[],Bool,Bool).
check_variant_frozen([PREV-_ITEM|T0],PREV,T,_Bool0,Bool) :-
    !,
    Bool=1,
    check_variant_frozen(T0,PREV,T,_,_).
check_variant_frozen([NEXT-ITEM|T0],_PREV,[ITEM|T],Bool0,Bool) :-
    check_variant_frozen(T0,NEXT,T,Bool0,Bool).

add_word_forms([]).
add_word_forms([W|Ws]) :-
    add_word_form(W),
    add_word_forms(Ws).

%% TODO: skip @ meta words
add_word_form(bracket(_)).
add_word_form(Word) :-
    atom(Word),
    alpino_util:split_atom(Word,"~",['',A|C]),
    !,
    add_word_forms([A|C]).
add_word_form(W) :-
    atomic(W),
    atom_codes(W,Wstr0),
    alpino_unknowns:decap_most_chars(Wstr0,Wstr1,_,_),
    deaccent_chars(Wstr1,Wstr),
    atom_codes(F,Wstr),
    (   word_form(F)
    ->  true
    ;   noclp_assertz(word_form(F))
    ).
		     

%% restore tags that the pos-tagger wanted to remove,
%% if the only alternative is UNKNOWN tag
restore_alternatives_to_unknown(ErasedTags,MaxPos,Input) :-
    (   tag(P0,_,_,_,Word,_,'UNKNOWN','UNKNOWN'),
        unused_pos(P0,MaxPos),
        debug_message(1,"warning: UNKNOWN|~w|",[Word]),
        debug_call(1,write_list(Input,user_error)),
        debug_call(1,nl(user_error)),
        restore_tags(P0,ErasedTags),
        fail
    ;   true
    ).


/*
:- use_module(universal_tag).
*/

/*
assert_reachables(P0,P) :-
    retractall(reachable(_)),
    retractall(co_reachable(_)),
    check_reachable([P0],[P0],ListR),
    (   lists:member(R,ListR),
	assertz(reachable(R)),
	fail
    ;   true
    ),
    check_co_reachable([P],[P],ListS),
    (   lists:member(S,ListS),
	assertz(co_reachable(S)),
	fail
    ;   true
    ).

%%%check_reachable(Surf0,Surf,Start,Final) :-
%%%    check_reachable(Start,Surf0),
%%%    check_co_reachable(Surf,Final).
%%%
%%%check_reachable(P0,P) :-
%%%    check_reachable([P0],[P0],List),
%    memberchk(P,List).

check_reachable([],R,R).
check_reachable([H|T],R0,R) :-
    findall(P,check_reachable_new_pos(H,P,R0),Ps0),
    sort(Ps0,Ps),
    append(Ps,R0,R1),
    append(T,Ps,Agenda),
    check_reachable(Agenda,R1,R).

check_reachable_new_pos(H,P,R0) :-
    tag(H,P,_,_,_,_,_,_),
    \+ member(P,R0).

check_reachable_new_pos(H,P,R0) :-
    open_bracket(H,P,_),
    \+ member(P,R0).

check_reachable_new_pos(H,P,R0) :-
    close_bracket(H,P,_),
    \+ member(P,R0).

%%%check_co_reachable(P0,P) :-
%%%    check_co_reachable([P],[P],List),
%%%    memberchk(P0,List).

%%% check_co_reachable(Ag,Co_reachables0,Co_reachables)
check_co_reachable([],R,R).
check_co_reachable([H|T],R0,R) :-
    findall(P,check_co_reachable_new_pos(H,P,R0),Ps0),
    sort(Ps0,Ps),
    append(Ps,R0,R1),
    append(T,Ps,Agenda),
    check_co_reachable(Agenda,R1,R).

check_co_reachable_new_pos(H,P,R0) :-
    tag(P,H,_,_,_,_,_,_),
    \+ member(P,R0).

check_co_reachable_new_pos(H,P,R0) :-
    open_bracket(P,H,_),
    \+ member(P,R0).

check_co_reachable_new_pos(H,P,R0) :-
    close_bracket(P,H,_),
    \+ member(P,R0).
*/

first_compound_root(v_root(klaverjas,klaverjassen),klaverjas).    % avond competitie
first_compound_root(v_root(zon_door,door_zonnen),doorzon).        % woning kamer
first_compound_root(v_root(help,helpen),help).                    % index bladzijde knop
first_compound_root(v_root(schrijf_in,inschrijven),inschrijf).    % formulier procedure
first_compound_root(v_root(drijf_aan,aandrijven),aandrijf).       % as varianten
first_compound_root(v_root(parkeer,parkeren),parkeer).            % terrein plaats
first_compound_root(v_root(verzamel,verzamelen),verzamel).        % DVD CD cd cd-box
first_compound_root(v_root(kick_af,af_kicken),afkick).
first_compound_root(v_root(loop_hard,hard_lopen),hardloop).       % schoenen wedstrijd training
first_compound_root(v_root(fris_op,op_frissen),opfris).           % curus beurt 
first_compound_root(alle,aller).
first_compound_root(mede,'mede-').
first_compound_root(top,top).
first_compound_root(v_root(houd_huis,huis_houden),huishoud).      % folie afdeling dingen winkels
first_compound_root(v_root(zoek,zoeken),zoek).
first_compound_root(v_root(rijd_achteruit,achteruit_rijden),achteruitrij).
first_compound_root(v_root(berg_op,op_bergen),opberg).
first_compound_root(v_root(haal_af,af_halen),afhaal).
first_compound_root(v_root(slank_af,af_slanken),afslank).
first_compound_root(v_root(schrijf_in,in_schrijven),inschrijf).
first_compound_root(v_root(vraag_aan,aan_vragen),aanvraag).
first_compound_root(taal,taal).

guess_english_compounds :-
    findall(Tag, guess_english_compound(Tag), Tags0),
    sort(Tags0,Tags),
    (   member(tag(P0,P,R0,R,Label,Used,His,Cat), Tags),
	assert_tag(P0,P,R0,R,Label,Used,His,Cat),
	fail
    ;   true
    ).

guess_english_compound(tag(P0,P,R0,R,Label,Used,normal(english_compound),Tag2)) :-
    (   noun_tag(Tag1),
	search_tag_tag(Tag1,tag(P0,P1,R0,R1,Label1,Used1,His1,Tag1)),
	His1 \= add_space,
	first_part_english_compound(R0,R1)
    ;   first_compound_root(Label1,Used1),
	search_tag_stem(Label1,tag(P0,P1,R0,R1,Label1,Used1,_His1,_Tag1))
    ),
    search_tag_r0(R1,tag(P1,P, R1,R, Label2,Used2,_His2,Tag2)),
    second_part_english_compound(tag(P1,P, R1,R, Label2,Used2,_His2,Tag2)),
    (   Label1 = v_root(Label1A,_)
    ->  true
    ;   Label1 = Label1A
    ),
    alpino_lex:concat_stems([Label1A,Label2],Label,' '),
    concat_all([Used1,Used2],Used,' '),
    \+ search_tag_r0(R0,tag(P0,P,R0,R,_,_,_,_)),
    \+ (  tag(Z0,Z,_,_,_,_,_,_),
	   Z0 =< P0,
	   P =< Z
       ),
    debug_message(1,"english compound|~w|~w|~w~n",[Used1,Used2,Tag2]).

first_part_english_compound(R0,R1) :-
    \+ alternative_to_compound(R0,R1,l),
    \+ first_alternative_to_compound(R0,R1).

second_part_english_compound(tag(_,_,R1,R,_,_,_,Tag2)) :-
    noun_tag(Tag2),
    \+ alternative_to_compound(R1,R,r).
second_part_english_compound(tag(_,_,_,_,L,_,_,Tag)):-
    second_part_lemma(L,Tag).

second_part_lemma(elite,noun(_,_,_)).
second_part_lemma(vormig,adjective(_)).

first_alternative_to_compound(R0,R1) :-
    search_tag_r0(R0,tag(_,_,R0,R1,_,_,_,Tag)),
    a_first_noun_tag(Tag).

a_first_noun_tag(noun(_,_,_,M)) :-
    a_first_noun_tag_sc(M).
a_first_noun_tag(tmp_noun(_,_,_,M)) :-
    a_first_noun_tag_sc(M).
a_first_noun_tag(mod_noun(_,_,_,M)) :-
    a_first_noun_tag_sc(M).
a_first_noun_tag(meas_mod_noun(_,_,_,M)) :-
    a_first_noun_tag_sc(M).

a_first_noun_tag_sc(measure).
a_first_noun_tag_sc(app_measure).

alternative_to_compound(R0,R1,LR) :-
    search_tag_r0(R0,tag(_,_,R0,R1,_,Used,His,Tag)),
    (   alternative_to_compound_root(LR,Used)
    ;   His=normal(_), \+ allowed_alt_tag(Tag)
    ).

alternative_to_compound_root(_,alles).
alternative_to_compound_root(_,heb).
alternative_to_compound_root(_,hoef).
alternative_to_compound_root(_,kom).
alternative_to_compound_root(_,krijg).
alternative_to_compound_root(_,tast).
alternative_to_compound_root(_,wacht).
alternative_to_compound_root(_,zit).
alternative_to_compound_root(_,zet).
alternative_to_compound_root(_,zondag).
alternative_to_compound_root(_,maandag).
alternative_to_compound_root(_,dinsdag).
alternative_to_compound_root(_,woensdag).
alternative_to_compound_root(_,donderdag).
alternative_to_compound_root(_,vrijdag).
alternative_to_compound_root(_,zaterdag).
alternative_to_compound_root(l,keer).
alternative_to_compound_root(l,tijd).


allowed_alt_tag(noun(_,_,_)).
allowed_alt_tag(tmp_noun(_,_,_)).
allowed_alt_tag(mod_noun(_,_,_)).
allowed_alt_tag(meas_mod_noun(_,_,_)).
allowed_alt_tag(noun(_,_,_,_)).
allowed_alt_tag(tmp_noun(_,_,_,_)).
allowed_alt_tag(mod_noun(_,_,_,_)).
allowed_alt_tag(meas_mod_noun(_,_,_,_)).
allowed_alt_tag(verb(_,sg,_)).
allowed_alt_tag(verb(_,sg1,_)).
allowed_alt_tag(verb(_,imp(sg),_)).
allowed_alt_tag(verb(_,imp(sg1),_)).
allowed_alt_tag(particle(_)).
allowed_alt_tag(post_n_n).
allowed_alt_tag(fixed_part(_)).
allowed_alt_tag(post_adjective(no_e)).

noun_tag(noun(_,_,_)).
noun_tag(tmp_noun(_,_,_)).
noun_tag(mod_noun(_,_,_)).
noun_tag(meas_mod_noun(_,_,_)).

call_constraints([]).
call_constraints([H|T]) :-
    call_constraint(H),!, % one is enough...
    call_constraints(T).

call_constraint(als_word) :-
    tag(_,_,_,_,_,_,_,complementizer(als)).

call_constraint(er_word) :-
    er_tag(Tag),
    tag(_,_,_,_,_,_,_,Tag).

call_constraint(tag(Tag)) :-
    tag(_,_,_,_,_,_,_,Tag).

er_tag(er_vp_adverb).     % er
er_tag(er_loc_adverb).    % daar hier ergens nergens overal
er_tag(er_wh_loc_adverb). % waar
er_tag(iets_adverb).      % ergens nergens (anders)
er_tag(wh_iets_adverb).   % waar (anders)


%% re-enter tags for a known word - where
%% all tags have been removed by tagger and/or
%% filter_tag
add_lexical_types(Word,P0,P,R0,R) :-
    normal_lex(Cat,Root,[Word],[],P0,His,[]),
    assert_tag(P0,P,R0,R,Root,Word,His,Cat).

add_score_refs(RefTags) :-
    findall(Ref,clause(tag(_,_,_,_,_,_,_,_),_,Ref),Refs),
    add_score_refs(Refs,RefTags).

%% find relevant RefTags, keeping track of each minitag
add_score_refs(Refs,RefTags) :-
    get_latest_reftags(Refs,LatestRefTags,RefTags,TagsUsed,[]),
    count_used(TagsUsed,TagsCount),
    normalize_score_ref(LatestRefTags,TagsCount).

get_latest_reftags([],[],_,Used,Used).
get_latest_reftags([Ref|Refs],Latest,RefTags,Used0,Used) :-
    (   lists:member(score_ref(Ref,His),RefTags)
    ->  add_his_tags(His,Used0,Used1),
	Latest = [score_ref(Ref,His)|Latest1]
    ;   Used0=Used1,
	Latest = [score_ref(Ref,[])|Latest1]
    ),
    get_latest_reftags(Refs,Latest1,RefTags,Used1,Used).

add_his_tags([],Used,Used).
add_his_tags([t(P0,P,Tag,_)|T],[t(P0,P,Tag)-_|Used0],Used) :-
    add_his_tags(T,Used0,Used).

count_used(List0,Counts) :-
    keysort(List0,List1),
    count_used_(List1,Counts).

count_used_([],[]).
count_used_([Tag-_|Tags],[Tag-Count|Counts]) :-
    count_used__(Tags,Tag,1,Count,Counts).

count_used__([],_,Count,Count,[]).
count_used__([Tag0-_|Tags],Tag1,Count0,Count,Counts) :-
    (   Tag0 == Tag1
    ->  Count1 is Count0 + 1,
	count_used__(Tags,Tag1,Count1,Count,Counts)
    ;   Count0=Count,
	Counts = [Tag0-Cnt|Counts1],
	count_used__(Tags,Tag0,1,Cnt,Counts1)
    ).

normalize_score_ref([],_).
normalize_score_ref([score_ref(Ref,HisTags)|T],Counts) :-
    normalize_score_ref_(HisTags,Ref,Counts),
    normalize_score_ref(T,Counts).

normalize_score_ref_([],Ref,_) :-
    assertz(alpino_score_ref:score_ref(Ref,15.0)). % very small prob for erased tags that might survive after all
normalize_score_ref_([H|T],Ref,Counts) :-
    compute_score([H|T],0,Score,Counts),
    assertz(alpino_score_ref:score_ref(Ref,Score)).

compute_score([],Score,Score,_).
compute_score([t(P0,P,Tag,Score1)|T],Score0,Score,Counts) :-
    lists:member(t(P0,P,Tag)-Count, Counts),
    Score2 is Score0 + Score1 + log(Count),
    compute_score(T,Score2,Score,Counts).

warn_unexpected_missing_fs(Tag,Used) :-
    prettyvars(Tag),
    format(user_error,"warning: no feature structure for ~w (~w)~n",[Tag,Used]).


interval_not_bracket(P0,P) :-
    \+ (   between(P0,P,P1),
	   P1 > P0,
	   (  alpino_lexical_analysis:open_bracket(_,P1,_)
	   ;  alpino_lexical_analysis:close_bracket(_,P1,_)
	   )
       ).

%%% do a simple hill-climbing search over tags
default_frames(String,Frames) :-
    length(String,P),
    search_default_frames(P,Frames),!.
default_frames(_,[]).  % catch all in case anything weird has happendd

%% path(Score,P,R,Frames).
search_default_frames(FinalP,Frames) :-
    findall(path(Score,P,R,[Frame]),scored_frame(0,P,0,R,Frame,Score),Frames0),
    add_frames_agenda(Frames0,[],Agenda),
    process_agenda(Agenda,FinalP,Frames).

process_agenda([path(_,P0,_,Frames0)|_],P,Frames) :-
    P0 == P, !,
    lists:reverse(Frames0,Frames).

process_agenda([path(Score0,P0,R0,Path)|Ag0],P,Frames) :-
    extend_frames(Score0,P0,R0,Path,Paths),
    add_frames_agenda(Paths,Ag0,Ag1),
    process_agenda(Ag1,P,Frames).

extend_frames(Score0,P0,R0,Frames0,Paths) :-
    findall(Path,extend_frame(Score0,P0,R0,Frames0,Path),Paths).

extend_frame(Score0,P0,R0,Frames0,path(Score,P,R,[Frame|Frames0])):-
    scored_frame(P0,P,R0,R,Frame,Score1),
    Score is Score0 + Score1.

scored_frame(P0,P,R0,R,frame(P0,P,R0,R,Stem,Tag,Surf,His),Score) :-
    scored_tag(P0,P,R0,R,Stem,Surf,His,Tag,Score).
		   
add_frames_agenda([],Agenda,Agenda).
add_frames_agenda([Fr|Frs],Agenda0,Agenda) :-
    add_frame_agenda(Agenda0,Fr,Agenda1),
    add_frames_agenda(Frs,Agenda1,Agenda).

add_frame_agenda([],Fr,[Fr]).
add_frame_agenda([path(Score0,P,R,Path)|Ag0],path(Score1,P1,R1,Path1),Ag) :-
    (   Score0 =< Score1
    ->  (   P==P1, R==R1
	->  Ag = [path(Score0,P,R,Path)|Ag0]
	;   Ag = [path(Score0,P,R,Path)|Ag2],
	    add_frame_agenda(Ag0,path(Score1,P1,R1,Path1),Ag2)
	)
    ; %% Score 1 is lower (better)
	Ag = [path(Score1,P1,R1,Path1)|Ag2],
	remove_worse([path(Score0,P,R,Path)|Ag0],P1,R1,Ag2)
    ).

remove_worse([],_,_,[]).
remove_worse([path(Score,P,R,Path)|Ag0],P1,R1,Ag):-
    (   P == P1,
	R == R1
    ->  remove_worse(Ag0,P1,R1,Ag)
    ;   Ag = [path(Score,P,R,Path)|Ag1],
	remove_worse(Ag0,P1,R1,Ag1)
    ).

