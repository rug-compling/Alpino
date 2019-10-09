:- module(alpino_postagger, [ pos_filter/3
			    ]).

:- expects_dialect(sicstus).

:- use_module(tr_tag).

:- use_module(survive_tagger).
:- use_module(hdrug(hdrug_util)).
:- use_module('../src/utils').

:- if(current_predicate(with_mutex/2)).

:- else.

:- meta_predicate with_mutex(?,:).
with_mutex(_,Call) :-
    once(Call).

:- endif.

foreign(alpino_pos_init,
	c,
	alpino_pos_init(+string)).

foreign(alpino_pos_filter,
	c,
        alpino_pos_filter(+term,+term,+integer,+integer,+float,+integer,+integer,
			  +integer,[-term])).

foreign_resource(alpino_pos,[alpino_pos_filter,alpino_pos_init]).

:- load_foreign_resource(alpino_pos).

:- initialize_flag(pos_tagger,off).
:- initialize_flag(pos_tagger_n,-1).
:- initialize_flag(pos_tagger_p,-1).
:- initialize_flag(pos_tagger_m,3).
:- initialize_flag(pos_tagger_state_size,3).
:- initialize_flag(alpino_pos_initialized,no).

pos_filter_initialize :-
    with_mutex(fadd,pos_filter_initialize_).

:- dynamic pos_filter_initialized/0.

pos_filter_initialize_ :-
    (   pos_filter_initialized
    ->  true
    ;	hdrug_flag(pos_tagger_dir,Dir0),
        absolute_file_name(Dir0,Dir1),
	atom_concat(Dir1,'/',Dir),
	debug_message(1,"Initializing tagger (~w)...~n",[Dir]),
	alpino_pos_init(Dir),
	assertz(pos_filter_initialized)
    ).

pos_filter(Sent,EraseTagRefs,RefTagList):-
    statistics(runtime,[T0,_]),
    hdrug_flag(pos_tagger,Tagger),
    (	Tagger == on
    ->	pos_filter_initialize,   % ensure it has been initialized
	all_tags(Tags,RefsLists),
	length(Tags,TagsLength),
	hdrug_flag(pos_tagger_n,P_t),
	hdrug_flag(pos_tagger_p,P_p),
	hdrug_flag(pos_tagger_m,P_m),
	hdrug_flag(pos_tagger_state_size,Size),
	hdrug_flag(debug,Debug0),
	Debug is Debug0-1,
	
	debug_message(1,"Starting fw/bw POS-tagger (~w tags)~n",[TagsLength]),

	with_mutex(fadd,alpino_pos_filter(Sent,Tags,P_t,P_p,P_m,TagsLength,Debug,Size,Solution)),

	debug_message(1,"DONE     fw/bw POS-tagger~n",[]),

        alpino_pos_filter_continue(Solution,TagsLength,T0,
                                   RefsLists,EraseTagRefs,Tags,RefTagList,Sent)
    ;	EraseTagRefs=[],
	RefTagList=[]
    ).

alpino_pos_filter_continue(Solution,TagsLength,T0,RefsList,EraseTagRefs,Tags,RefTagList,Sent) :-
    keep_tags(Solution,KeepTagRefs,0,RemainingTagLength,Tags),
    debug_message(2,"Tags after pos-tagger: ~p~n",[KeepTagRefs]),
    inform_postags(KeepTagRefs,Sent),
    collect_erased_tags(RefsList,KeepTagRefs,EraseTagRefs0,RefTagList,[]),
    survive_tagger(EraseTagRefs0,EraseTagRefs),
    statistics(runtime,[T1,_]),
    T is T1-T0,
    length(RefsList,TagsRefsLength),
    length(EraseTagRefs,EraseTagRefsLength),
    RemainingTagRefsLength is TagsRefsLength-EraseTagRefsLength,
    debug_message(2,
         "PosTagger (classes): ~w --> ~w tags~n",
         [TagsLength,RemainingTagLength]),
    debug_message(2,
         "PosTagger (categories): ~w --> ~w tags (~w msec)~n",
         [TagsRefsLength,RemainingTagRefsLength,T]).

%% get a list of unique tags
%% get a list of key-vals where key is a reference to orig tags, and vals the
%%     tags as used here
all_tags(Tags,RefsTags) :-
    findall(Ref-Ts,a_tag(Ref,Ts),RefsTags),
    select_all_tags(RefsTags,Tags0),
    sort(Tags0,Tags),
    debug_message(2,"Tags before pos-tagger: ~p~n",[RefsTags]).

select_all_tags([],[]).
select_all_tags([_-Ts|T0],Tags) :-
    lists:append(Ts,Tags1,Tags),
    select_all_tags(T0,Tags1).

a_tag(Ref,Tags):-
    clause(alpino_lexical_analysis:tag(_,_,R0,R,W,L,His,Tag0),true,Ref),
    His \= skip(_,_,_,_),
    findall(Tag,map_tag(R0,R,Tag0,Tag),Tags),
    debug_message(2,"~w --> ~p~n",[tag(_,_,R0,R,W,L,His,Tag0),Tags]).

a_tag(Ref,Tags):-
    His = skip(_,_,_,_),
    clause(alpino_lexical_analysis:tag(R0,R,R0,R,W,L,His,Tag0),true,Ref),
    findall(Tag,map_expand_tag(R0,R,His,Tag0,Tag),Tags),
    debug_message(2,"~w --> ~p~n",[tag(R0,R,R0,R,W,L,His,Tag0),Tags]).

map_tag(R0,R,Tag0,t(Q0,Q,TagChars)) :-    
    tr_tag(Tag0,Tag1),
    in_pieces(R0,R,Q0,Q,Tag1,Tag2),
    prettyvars(Tag2),
    charsio:format_to_chars('~q',[Tag2],TagChars).

map_expand_tag(R0,R,His,Tag0,t(Q0,Q,TagChars)) :-    
    tr_tag(Tag0,Tag1),
    expand_skips(His,Tag1,R0,R,S0,S,Tag2),
    in_pieces(S0,S,Q0,Q,Tag2,Tag3),
    prettyvars(Tag3),
    charsio:format_to_chars('~q',[Tag3],TagChars).

expand_skips(skip(_,Left,Right,_),Tag0,R0,R,S0,S,Tag) :-
    expand_skip_left(Left,Right,Tag0,R0,R,S0,S,Tag).

% expand_skip_left([],Tag,R0,R,R0,R,Tag).
expand_skip_left([_|_],_Right,_Tag0,R0,_,R0,R1,skip) :-
    R1 is R0 + 1.
expand_skip_left([_|T],Right,Tag0,R0,R,S0,S,Tag) :-
    R1 is R0 + 1,
    expand_skip_left(T,Right,Tag0,R1,R,S0,S,Tag).
expand_skip_left([],Right,Tag0,R0,R,S0,S,Tag) :-
    expand_skip_right(Right,Tag0,R0,R,S0,S,Tag).

expand_skip_right([],Tag,R0,R,R0,R,Tag).
expand_skip_right([_|_],_,_,R,R1,R,skip) :-
    R1 is R - 1.
expand_skip_right([_|T],Tag0,R0,R,S0,S,Tag) :-
    R1 is R - 1,
    expand_skip_right(T,Tag0,R0,R1,S0,S,Tag).

in_pieces(P0,P,Q0,Q,Tag1,Tag) :-
    Length is P-P0,
    (	Length =:= 1
    ->	Tag1=Tag,
	P0=Q0, P=Q
    ;	in_pieces0(P0,P,Q0,Q,1,Tag1,Tag,Length)
    ).

in_pieces0(P0,P,P0,P1,N,Tag,N/Q-Tag,Q) :-
    P0 < P,
    P1 is P0 + 1.

in_pieces0(P0,P,R0,R,N,Tag0,Tag,Q) :-
    P1 is P0 + 1,
    P1 < P,
    N1 is N + 1,
    in_pieces0(P1,P,R0,R,N1,Tag0,Tag,Q).

keep_tags([],[],S,S,_).
keep_tags([I,Score|Is],[t(P0,P,Tag,Score)|Keeps],S0,S,Tags) :-
    lists:nth0(I,Tags,t(P0,P,Tag)),
    S1 is S0 + 1,
    keep_tags(Is,Keeps,S1,S,Tags).

collect_erased_tags([],_,[],Refs,Refs).
collect_erased_tags([H|T],Keep,Erased0,Refs0,Refs) :-
    collect_erased_tag(H,Keep,Erased0,Erased,Refs0,Refs1),
    collect_erased_tags(T,Keep,Erased,Refs1,Refs).

collect_erased_tag(Ref-Tags,Keep,Erased0,Erased,[score_ref(Ref,HisTags)|Refs],Refs) :-
    (   trace_tags(Tags,HisTags,Keep)
    ->  Erased=Erased0
    ;   HisTags = [],
	Erased0=[Ref|Erased]
    ).

trace_tags([],[],_).
trace_tags([t(P0,P,Tag)|T],[t(P0,P,Tag,Score1)|X],Keep) :-
    lists:member(t(P0,P,Tag,Score1),Keep),
    trace_tags(T,X,Keep).

:- hdrug_util:initialize_flag(inform_postags,off).
inform_postags(List,Sent) :-
    hdrug_util:hdrug_flag(inform_postags,OnOff),
    inform_postags(OnOff,List,Sent).

inform_postags(off,_,_).
inform_postags(on,List,Sent) :-
    inform_postags_(List,Sent).

inform_postags_([],_).
inform_postags_([t(P0,P,String,_LogProb)|Tail],Sent) :-
    lists:nth0(P0,Sent,Word),
    format("~w\t~w\t~w\t~s\t~s\t~n",[P,Word,Word,String,String]),
    inform_postags_(Tail,Sent).
