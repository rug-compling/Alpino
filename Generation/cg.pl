:- module(alpino_cg, [active_edges/0,
		      inactive_edges/0,
		      update_inactive_edges/0
		     ]).

:- expects_dialect(sicstus).

:- use_module(library(timeout)).

:- use_module(library(lists)).
:- use_module(hdrug(hdrug_util)).
:- use_module(alpino('src/utils')).
:- use_module(treex).

:- dynamic active_edge/7, inactive_edge/3, inactive_edge_frozen/5,
           inactive_edge_ref/2, his/2.
:- dynamic lex/6.          % also used in genlex.pl src/filter_tags.pl
:- dynamic unpack_memo/3.

:- thread_local active_edge/7, inactive_edge/3, inactive_edge_frozen/5,
           inactive_edge_ref/2, his/2.
:- thread_local lex/6.          % also used in genlex.pl src/filter_tags.pl
:- thread_local unpack_memo/3.

:- use_module( [ genlex,
		 bitcode,
		 'fluency/pro_lm',
		 'fluency/maxent'
%%		 optrel   % currently not used!
		 ]).


%%% TODO: robustness should not do dp-dp (punctuation is inserted)
%%%       but something special so that no additional punctuation is inserted
%%% [ ik verslijt hem voor ] [ zwakzinnig ]
%%% => ik verslijt hem voor ; zwakzinnig
%%% 

%%%
%%% FLUENCY
%%%

%% is now called from start_hook in src/start.pl; otherwise cputime of
%% first sentence is too high
%%
%% ...and now it has even moved to the hdrug_initialization hook to get
%% everything loaded correctly for static compilation
:- public load_fluency/0.

load_fluency :-
    hdrug_flag(use_fluency_model,UseFluency),
    hdrug_flag(end_hook,Th),
    load_fluency(UseFluency,Th).

load_fluency(UseFluency,Th) :-
    (   ( UseFluency == on ; Th = train_fluency )
    ->  load_maxent_fluency_weights
    ;   true
    ).

load_maxent_fluency_weights :-
    hdrug_flag(fluency_feature_weights,File),
    (   File==undefined
    ->  alpino_util:inform_undefined_module(fluency_feature_weights,alpino_fluency_weights)
    ;   overrule_module(File)
    ).

extract_and_score_sentences(Fs,BitcodeAll,Obj,Str,UseFluency,SortedResults,Robust) :-
    findall(P-o(Obj,Str),
            extract_and_score_sentence(Fs,BitcodeAll,o(Obj,Str),P,UseFluency,Robust),
            KeyedResults),
    (	UseFluency == on
    ->	keysort(KeyedResults,SortedResults)
    ;	SortedResults = KeyedResults
    ).
        
extract_and_score_sentence(Fs,BitcodeAll,o(Obj,Str),P,UseFluency,Robust) :-
    generated_sentence(P,Features,DerivTree,Fs,BitcodeAll,UseFluency,Str,Robust),
    alpino_data:robust_list_to_cat([Fs],Cat),
    alpino_data:result_term(p(P,Features),Str,Cat,DerivTree,_Frames,Obj).

%%%
%%% GENERATE
%%% 
%%% Hdrug hook

:- public generate/1.

generate(o(Obj,Str,Sem0)) :-
    hdrug_flag(generate_failsafe,Val),
    alpino_data:result_term(Sc,Str0,Cat,DerivTree,Frames,Obj0),
    alpino_data:result_term(Sc,Str, Cat,DerivTree,Frames,Obj),
    
    (   Val == off
    ->  combine_mwu(Sem0,Sem1),
	transform_adt(Sem1,Sem),
	if(generate_one(Obj0,Sem,top),
	   true,
	   generate_parts(Obj0,Sem)
	  ),
	number_terminals(DerivTree,0,_,Str0,[]),
	frames_of_deriv(DerivTree,Frames)
    ;   Val == on
    ->  combine_mwu(Sem0,Sem1),
	transform_adt(Sem1,Sem),
	generate_last_resort(Obj0,Sem),
	number_terminals(DerivTree,0,_,Str0,[]),
	frames_of_deriv(DerivTree,Frames)
    ;   Val == last,
        generate_last_last_resort(_,Sem0,Str0)
    ),
    capitalize_first(Str0,Str1),
    add_punt_if_not(Str1,Str).

generate_one(Obj,Sem0,TopRobust) :-
    debug_message(2,"generating for ~w~n",[Sem0]),
    ensure_indices_ok(Sem0,Sem),
    hdrug_flag(use_fluency_model,UseFluency),
    prepare_adt_and_lex(Sem,Fs,BitcodeAll,TopRobust),
    count_edges(lex(_,_,_,_,_,_),Edges),
    (   Edges > 0
    ->  debug_message(1,"~w lexical frames for generation~n",[Edges])
    ;   debug_message(1,"no lexical frames for generation, generating from empty DT?~n",[]),
	fail
    ),
    clean_edges,
    bb_put(edge_count,0),
    bb_put(his_count,0),
    chart_initialization(Agenda),
    length(Agenda,Len),
    debug_message(1,"~w lexical attribute value structures for generation~n",[Len]),
    all_bitcodes(Agenda,0,All),
    (   All == BitcodeAll
    ->  true
    ;   format(user_error,"Lexical lookup incomplete (bitcodes: found ~w for ~w)~n",[All,BitcodeAll]),
	%% attempt to give informative message, but this fails if multiple roots missing
	Missing is BitcodeAll - All,
	findall(Root,lex(_,Root,Missing,_,_,_),Roots0),
	sort(Roots0,[R|_Roots]),
	format(user_error,"No feature-structures for root '~w'~n",[R]),
	fail
    ),
    process_agenda(Agenda,Sem),
    extract_and_score_sentences(Fs,BitcodeAll,Obj,Str,UseFluency,SortedResults,TopRobust),
    member(_-o(Obj,Str),SortedResults),
    debug_message(1,"generated: ~w~n",[Str]).

%% this is called after time-out
generate_last_resort(Obj,Sem) :-
    \+ hdrug_flag(robustness,off),
    findall(Part,get_lexical_part(Sem,Part),[Part|Parts]),
    debug_message(2,"No full generation; attempting to generate parts...~n",[]),
    generate_list([Part|Parts],Results,[]),
    combine_results(Results,_Sc,Obj,_Str).

generate_last_last_resort(Obj,Sem,Results) :-
    \+ hdrug_flag(robustness,off),
    get_yield(Sem,Results,[]),
    alpino_data:result_term(_,Results,_,_,_,Obj).

get_yield(tree(Node,Ds),Results0,Results):-
    get_yield(Ds,Node,Results0,Results).

get_yield([],Node,Results0,Results) :-
    get_lexical_yield(Node,Results0,Results).
get_yield([H|T],_,Res0,Res):-
    get_yield(H,Res0,Res1),
    get_yield_ds(T,Res1,Res).

get_yield_ds([],Res,Res).
get_yield_ds([H|T],Res0,Res):-
    get_yield(H,Res0,Res1),
    get_yield_ds(T,Res1,Res).

get_lexical_yield(r(_,L),Res0,Res):-
    get_lexical_yield(L,Res0,Res).
get_lexical_yield(i(_),Res,Res).
get_lexical_yield(i(_,L),Res0,Res):-
    get_lexical_yield(L,Res0,Res).
get_lexical_yield(adt_lex(_,Root,_,_,_), [Root|Rs],Rs).

%% ensure renumbering of positions in the parts!
generate_parts(Obj,Sem) :-
    \+ hdrug_flag(robustness,off),
    findall(Part,get_part(Sem,Part),[Part|Parts]),
    debug_message(2,"No full generation; attempting to generate parts...~n",[]),
    generate_list([Part|Parts],Results,[]),
    combine_results(Results,_Sc,Obj,_Str).

get_lexical_part(tree(r(top,p(top)),Ds),tree(r(top,p(top)),[tree(r('--',Cat),[])])) :-
    lexical_sub_tree_ds(Ds,Cat).

lexical_sub_tree(tree(r(_,Cat),[]),Cat) :-
    Cat = adt_lex(_,_,_,_,_).
lexical_sub_tree(tree(_,Ds),Cat) :-
    lexical_sub_tree_ds(Ds,Cat).

lexical_sub_tree_ds(List,Cat) :-
    member(El,List),
    lexical_sub_tree(El,Cat).

get_part(tree(r(top,p(top)),Ds),tree(r(top,p(top)),[tree(r('--',Cat),XDs)])) :-
    sub_tree(Ds,Cat,XDs).

sub_tree([tree(_,Ds1)],Cat,XDs) :-
    !,
    sub_tree(Ds1,Cat,XDs).
sub_tree([H|T],Cat,XDs) :-
    member(tree(r(_,Cat),XDs),[H|T]).

lexical_adt(tree(r(top,p(top)),[tree(r(--,adt_lex(_,Root,_,_,_)),[])]),Root).
lexical_adt(tree(r(top,p(top)),[tree(r(--,i(_,adt_lex(_,Root,_,_,_))),[])]),Root).

generate_list([],[],_).
generate_list([Sem|Sems],[Obj|Results],Context) :-
    \+ lexical_adt(Sem,_),
    generate_one(Obj,Sem,robust),
    !,
    alpino_data:result_term(_,Str,_,_,_,Obj),
    debug_message(2,"generating for ~w : ~w~n",[Sem,Str]),
    lists:append(Context,Str,Context1),
    generate_list(Sems,Results,Context1).

generate_list([Sem|Sems],Results,Context) :-
    debug_message(2,"generating for ~w nope~n",[Sem]),
    findall(Part,get_part(Sem,Part),Parts),
    generate_list_continue(Parts,Sem,Sems,Results,Context).

generate_list_continue([],Sem,Sems,[Result|Results],Context) :-
    lexical_adt(Sem,_),
    prepare_adt_and_lex(Sem,_,BcAll,robust),
    findall(Score-l(Str,Cat,Ref,Context2),generate_word_fallback(Score,Str,Cat,Ref,Context,Context2,BcAll),KeyList0),
    sort(KeyList0,[Score-l(Str,Cat,Ref,Context2)|_]),
    !,				% only one, the first available one
    alpino_data:result_term(p(0.0,[]),Str,robust([Cat]),tree(Cat,robust,lex(Ref),_),[],Result),
    debug_message(1,"generated: ~w~n",[Str]),
    generate_list(Sems,Results,Context2).

generate_list_continue([],Sem,Sems,[Result|Results],Context) :-
    lexical_adt(Sem,Root),
    alpino_genlex:surf_to_list(Root,Str,[]),
    lists:append(Context,Str,Context2),
    alpino_data:result_term(p(0.0,[]),Str,robust([MAX]),tree(MAX,robust,lex(ref(none,none,Root,Root,P0,P0,R0,R0,_,_,_)),_),[],Result),
    alpino_data:max_dt(MAX),
    unify_lex(MAX,none,Root,Root,P0,R0),
    alpino_data:dt(MAX,DT),
    alpino_data:dt(DT,Hwrd,_,_,_),
    alpino_data:label(Hwrd,Root,Root,_,_,_),
    !,
    debug_message(1,"generated: ~w~n",[Str]),
    generate_list(Sems,Results,Context2).


generate_list_continue([],_Sem,Sems,Results,Cx) :-
    generate_list(Sems,Results,Cx).

generate_list_continue([H|T],_,Sems,Results,Cx) :-
    lists:append([H|T],Sems,NewSems),
    generate_list(NewSems,Results,Cx).

%% todo: don't allow particle for a verb input here
generate_word_fallback(Score,Str,Cat,Ref,Context,ContextStr,BcAll) :-
    Ref = ref(_,Frame,Root,Surf,P0,P0,R0,R0,_His,_N,_GuidesTag),
    %% we should accept combinations of lex s.t. their bc add to BcAll...
    if(   lex(Frame,Root,BcAll,_,_,Surfs),
        (   member(Surf,Surfs),
	    alpino_genlex:surf_to_list(Surf,Str,[])
	),
        (   lex(Frame,Root,Bc1,_,_,Surfs0),
	    lex(particle(_),_Root1,Bc2,_,_,Surfs1),
	    BcAll is Bc1 + Bc2,
	    member(Surf0,Surfs0),
	    member(Surf1,Surfs1),
	    concat_all([Surf0,Surf1],Surf,' '),
	    Str = [Surf0,Surf1]
	)
    ),
  
    lists:append(Context,Str,ContextStr),
    lookup_tag(Frame,Root,Cat,Ref,_Dt,Surf),
    unify_lex(Cat,Frame,Root,Surf,P0,R0),
    alpino_ngram_lm:sentence_fluency(ContextStr,Score).

combine_results(Results,Sc,Obj,Str) :-
    categories(Results,Scs,Strs,Cats,Trees),
    alpino_data:robust_list_to_cat(Cats,Cat),
    append_all(Strs,Str),
    sum_all(Scs,Sc),
    alpino_data:result_term(Sc,Str,Cat,tree(robust,robust,Trees,_),_,Obj).

sum_all([p(Sc0,His0)|Tail],p(Sc,[His0|His])) :-
    sum_all(Tail,Sc0,Sc,His).

sum_all([],Sc,Sc,[]).
sum_all([p(Sc0,His0)|Tail],Sc1,Sc,[His0|His]) :-
    Sc2 is Sc0 + Sc1,
    sum_all(Tail,Sc2,Sc,His).

append_all([],[]).
append_all([H|T],S) :-
    append_all(T,H,S).

append_all([],S,S).
append_all([H|T],S0,S) :-
    append(S0,H,S1),
    append_all(T,S1,S).

categories([],[],[],[],[]).
%categories([Objs|Objss],Scs,Strs,Cats,Trees) :-
%    Objs == [], !,
%    categories(Objss,Scs,Strs,Cats,Trees).
categories([Obj|Objss],[Sc|Scs],[Str|Strs],[Cat|Cats],[Tree|Trees]) :-
%    member(Obj,Objs),
    alpino_data:result_term(Sc,Str,Cat0,Tree,_,Obj),
    Cat0 = robust([Cat]),
    categories(Objss,Scs,Strs,Cats,Trees).

%% Hdrug hook
:- public count/0, count/1, clean/0.
count :-
    debug_call(1,print_edge_counts).

%% Hdrug hook
count(N) :-
    bb_get(edge_count,N).

active_edge_count(Count) :-
    findall(_,active_edge(_,_,_,_,_,_,_),Ids),
    length(Ids,Count).

inactive_edge_count(Count) :-
    findall(_,inactive_edge(_,_,_),Ids),
    length(Ids,Count).

history_count(Count) :-
    findall(_,his(_,_),Ids),
    length(Ids,Count).

print_edge_counts :-
    history_count(HisCount),
    format(user_error,"     Histories: ~w~n",[HisCount]),
    active_edge_count(ActiveCount),
    format(user_error,"  Active edges: ~w~n",[ActiveCount]),
    inactive_edge_count(InActiveCount),
    format(user_error,"Inactive edges: ~w~n",[InActiveCount]).

%% hdrug hook
clean :-
    clean_edges,
    alpino_adt:clean,
    retractall(lex(_,_,_,_,_,_)).

clean_edges :-
    bb_put(hdrug_gen_sym:generator_values,0),
    retractall(inactive_edge_ref(_,_)),
    retractall(inactive_edge(_,_,_)),
    retractall(inactive_edge_frozen(_,_,_,_,_)),
    retractall(active_edge(_,_,_,_,_,_,_)),
    retractall(his(_,_)).

prepare_adt_and_lex(CombinedAdt,Fs,BcAll,Robust) :-
%    alpino_adt:combine_mwu(Adt,CombinedAdt0),
%    transform_adt(CombinedAdt0,CombinedAdt),
    alpino_adt:bitcode_lookup_frames_adt(CombinedAdt,BcAdt,0,NBits),
    give_bits(NBits,BcAll),
    alpino_adt:adt_to_fs(BcAdt,FsAdt,Fs,Robust),
    alpino_genlex:lex_lookup(FsAdt).

%%%%%%%%%%%%%%%%%%
% Main generator %
%%%%%%%%%%%%%%%%%%

process_agenda([],_).
process_agenda([Edge|Edges],Sem) :-
    (   terms:cyclic_term(Edge)
    ->  debug_message(1,"cyclic term detected (ignored)~n",[]),
	Edges = NewEdges
    ;   next_step(Edge,Edges,NewEdges,Sem)
    ),
    process_agenda(NewEdges,Sem).

%% Process the next edge:
%%
%% - Pack the edge if possible, otherwise
%% - find interactions with the chart and grammar rules.
next_step(Edge0,Edges,NewEdges,Sem) :-
    freeze_edge(Edge0,EdgeFrozen),
    (   pack_edge(Edge0,EdgeFrozen)
    ->  Edges=NewEdges
    ;   number_inactive_edge(Edge0,Edge1),
        inactive_edge_history(Edge1,Edge),
        findall(NewEdge,gen_edge(Edge,NewEdge,Sem),NewEdges,Edges),
        put_on_chart(EdgeFrozen,Edge)
    ).

number_inactive_edge(inactive_edge(SynSem,Bitcode,Ds),
		     inactive_edge(SynSem,Bitcode,Ds,Count)) :-
    !,
    bb_get(edge_count,Count),
    NewCount is Count + 1,
    bb_put(edge_count,NewCount).
number_inactive_edge(E,E).

inactive_edge_history(inactive_edge(SynSem,Bitcode,His,Count),
		      inactive_edge(SynSem,Bitcode,Count)) :-
    !,
    assert_history(Count,His).
inactive_edge_history(E,E).

gen_edge(inactive_edge(Cat,Bc,N),NewEdge,Sem) :-
    rule_invocation(inactive_edge(Cat,Bc,N),NewEdge,Sem).
gen_edge(Edge,NewEdge,_) :-
    dot_movement(Edge,NewEdge).

%% For all the signs that appear in the input bag, create inactive edges.
chart_initialization(Agenda) :-
    findall(Item,initial_item(Item),Agenda).

initial_item(inactive_edge(SynSem,Bitcode,l(ref(Class,Tag1,Label,Used,P0,P0,R0,R0,His,N,GuidesTag)))) :-
    syn_sem_lex(SynSem,lex(ref(Class,Tag1,Label,Used,P0,P0,R0,R0,His,N,GuidesTag)),Bitcode).

initial_item(inactive_edge(SynSem,0,gap(ID))) :-
    alpino_genrules:gap(ID,SynSem).

%% Initialize active edges from inactive edges.
rule_invocation(inactive_edge(OldMother,Bitcode,N),EDGE,Sem) :-
    %% The mother of an inactive edge is the head daugther of the
    %% rules that we are interested in.
    alpino_genrules:headed_grammar_rule(OldMother,Id,Mother,Rest),
    rule_condition(Id,Sem),
    %% Since we already know the head daughter, we can make a tree node for
    %% the daughter representing the head.
    substitute(OldMother,Rest,N,UDaughters),
    construct_edge(Mother,Bitcode,Id,[],UDaughters,EDGE).

%% Dot movement for imp rules, which handle verb-second word order. imp rules have
%% a second daughter that stores an identifier and vslash, to be retrieved by a
%% vgap lateron. Once we encounter such predicate daughter, we known that an inactive
%% vgap edge should exist as well.
dot_movement(active_edge(Mother,Bitcode,Id,PDtrs,
                         [call(put_val(Identifier,VSlash))|UDtrs]),EDGE) :-
    !,
    gen_sym(Identifier,generator_values),  % GvN: ensure Identifier is grounded,
                                                      % so we cannot later combine wrong
                                                      % get_val / put_val

    % copy_term(VSlash,VSlash1),  perhaps we need this once vpx coordinations work?
    
    alpino_genrules:headed_grammar_rule(call(get_val(Identifier,VSlash)),vgap,VGapMother,_),    
    (   EDGE = inactive_edge(VGapMother,0,vgap)
    ;   EDGE = active_edge(Mother,Bitcode,Id,PDtrs,UDtrs)
    ).

%% Normal dot movement over active edges.
dot_movement(active_edge(AMother,ABitcode,AId,APDtrs,[IMother|AUDtrs]),EDGE) :-
    inactive_edge(IMother,IBitcode,INum),
    dot_movement_aux(IMother,IBitcode,INum,AMother,ABitcode,
		     AId,APDtrs,AUDtrs,EDGE).

%% Normal dot movement over inactive edges.
dot_movement(inactive_edge(IMother,IBitcode,INum),EDGE) :-
    active_edge(IMother,AMother,ABitcode,AId,APDtrs,AUDtrs,_),
    dot_movement_aux(IMother,IBitcode,INum,AMother,ABitcode,
		     AId,APDtrs,AUDtrs,EDGE).

dot_movement_aux(_IMother,IBitcode,INum,AMother,ABitcode,AId,
		 PDtrs,UDtrs,EDGE) :-
    %% The active and inactive edge should not cover the same lexical items.
    combine_bitcodes(ABitcode,IBitcode,NewBitcode),
    construct_edge(AMother,NewBitcode,AId,[INum|PDtrs],UDtrs,EDGE).

construct_edge(Mother,Bitcode,Id,PDtrs0,UDtrs0,EDGE) :-
    \+ \+ check_part_of_dt(Id,Mother),
    skip_head(UDtrs0,PDtrs0,UDtrs,PDtrs),
    active_inactive(UDtrs,Bitcode,Id,Mother,PDtrs,EDGE).

active_inactive(UDtrs,Bitcode,Id,Mother,PDtrs0,EDGE) :-    
    (	UDtrs == []
    ->	reverse(PDtrs0,PDtrs),
        EDGE = inactive_edge(Mother,Bitcode,r(Id,PDtrs)),
	filter_edge(Id,PDtrs)
    ;	EDGE = active_edge(Mother,Bitcode,Id,PDtrs0,UDtrs)
    ).

assert_history(InactiveId,His) :-
    assertz(his(InactiveId,His)),
    debug_message(3,"~w ~w~n",[InactiveId,His]).

%% Head skipping: heads were already processed during rule invocation,
%% and have an inactive edge number. We can just move them to the list
%% of processed daughters.
skip_head([],PDtrs,[],PDtrs).
skip_head([Next|UDtrs0],PDtrs,UDtrs,NewPDtrs) :-
    (   number(Next)
    ->  NewPDtrs = [Next|PDtrs],
	UDtrs = UDtrs0
    ;   NewPDtrs = PDtrs,
	UDtrs = [Next|UDtrs0]
    ).

%%%%%%%%%%%%%%%%%
% Edge creation %
%%%%%%%%%%%%%%%%%

%%
%% These predicates make chart edges. An edge can be in two states:
%%
%% 1. the edge is on the agenda
%% 2. the edge is on the chart
%%
%% Agenda edges are processed one by one, attempting rule invocation and dot
%% movement. When we are done with an agenda edge, it is removed from the agenda
%% and put on the chart. An edge that is on the chart is only passively used by
%% the generator: it can be used in dot movement invoked by edges on the agenda
%% as they are processed.
%%

%% Create an edge on the chart.
put_on_chart(fs(Hash,Copy,Bitcode,Cons),inactive_edge(Mother,Bitcode,Count)) :-
    assertz(inactive_edge(Mother,Bitcode,Count),Ref),
    assertz(inactive_edge_ref(Count,Ref)),
    assertz(inactive_edge_frozen(Hash,Copy,Bitcode,Cons,Count)).
put_on_chart(none,active_edge(Mother,Bitcode,Id,PDtrs,[UDtr|UDtrs])) :-
    (   UDtr = call(put_val(_,_))
    ->  true
    ;   bb_get(edge_count,Count),
	assertz(active_edge(UDtr,Mother,Bitcode,Id,PDtrs,UDtrs,Count)),
	NewCount is Count + 1,
	bb_put(edge_count,NewCount)
    ).
    

%%%%%%%%%%%%%%%%%%%%%%%
% Generated sentences %
%%%%%%%%%%%%%%%%%%%%%%%

:- initialize_flag(fluency_candidates_beam,0).

generated_sentence(P,Features,Tree,DtFsAll,BitcodeAll,UseFluency,Sentence,Robust) :-
    hdrug_flag(number_analyses,Number0),
    (	integer(Number0),
	Number0 > 0
    ->	Number0 = Number
    ;	Number = 100000000
    ),
    hdrug_flag(fluency_candidates_beam,CandidatesBeam),
    start_it_deepening(generator,1,10,MaxPunct),

    %% iterative deepening..
    findall_atmost(CandidatesBeam,Sentence-DtFsAll-Tree,
            unpack_it(BitcodeAll,DtFsAll,MaxPunct,Tree,Sentence,Robust),
            Trees),
    length(Trees,NTrees),
    debug_message(2,"~d realization candidates~n",
			     [NTrees]),
    score_trees(Trees,ScoredTrees,UseFluency),
    keysort(ScoredTrees,SortedTrees),
    list_first_n(SortedTrees,BestTrees,Number),
    member(P-Features-Sentence-DtFsAll-Tree,BestTrees),
    alpino_data:deriv_tree_struct(_,DtFsAll,_,Tree),
    end_it_deepening(generator).


%% Iterative deepening - increase the amount of allowed punctuation until
%% we are able to successfully unpack a derivation.

start_it_deepening(Key,Depth0,Depth,CurDepth) :-
    bb_put(Key,failed),
    between(Depth0,Depth,CurDepth),
    bb_get(Key,failed).

end_it_deepening(Key) :-
    bb_put(Key,succeeded).

unpack_it(BitcodeAll,DtFsAll,MaxPunct,Tree,Sentence,Robust) :-
    unpack_top(BitcodeAll,DtFsAll,MaxPunct,Tree,Robust),
    terminals(Tree,Sentence,[]).

capitalize_first([],[]).
capitalize_first([H|T],[H|T]) :-
    alpino_lex:qtleap_hide_it_prefix(H), !.
capitalize_first([H0|T],[H|T]) :-
    alpino_unknowns:cap_first(H0,H), !.
capitalize_first([H0|T0],[H0|T]) :-
    punct(H0), !,
    capitalize_first(T0,T).
capitalize_first([H|T],[H|T]).

add_punt_if_not([],[]).
add_punt_if_not([H|T],Result):-
    add_punt_if_not(T,H,Result).

%% hack: vp_vp_colon rule has been applied, so we get "aslkfjlskdj : ."
add_punt_if_not(['.'],':',Rest) :-
    !,
    Rest = [':'].
add_punt_if_not(['?'],':',Rest) :-
    !,
    Rest = [':'].
add_punt_if_not([],Last,[Last|Rest]):-
    (   (   Last == '.'
	;   Last == '?'
	;   Last == ':'
	)
    ->  Rest = []
    ;   Rest = ['.']
    ).
add_punt_if_not([H|T],P,[P|Rest]):-
    add_punt_if_not(T,H,Rest).


punct('"').    %"
punct('\'').  
punct('\'s').

score_trees([],[],_).
score_trees([Sentence-Fs-Tree|T],[P-Features-Sentence-Fs-Tree|NewT],UseFluency) :-
    (   UseFluency == on
    ->  sentence_fluency_maxent(Fs,Tree,P,Features)
    ;   Features = [],
	P = 0.0
    ),
    score_trees(T,NewT,UseFluency).

%%%%%%%%%%%%%
% Unpacking %
%%%%%%%%%%%%%

%% XXX - new and experimental

%% Unpack all edges with a top category.
unpack_top(BitcodeAll,DtFsAll,MaxPunct,Tree,Robust) :-
    hdrug_flag(generate_bc_check,BcCheck),
    hdrug_flag(fluency_beam,Beam0),
    (   integer(Beam0),
	Beam0 > 0
    ->  Beam0 = Beam
    ;   Beam = 0
    ),	
    retractall(unpack_memo(_,_,_)),
    top_edge(Robust,EdgeId),
    unpack_all(BitcodeAll,DtFsAll,EdgeId,MaxPunct,Tree,BcCheck,Beam).

is_inactive_edge(Fs,Bc,Id) :-
    (   nonvar(Id)
    ->  inactive_edge_ref(Id,Ref),
	clause(inactive_edge(Fs,Bc,Id),Body,Ref),
	call(Body)
    ;   inactive_edge(Fs,Bc,Id)
    ).

unpack_all(BitcodeAll,DtFsAll,EdgeId,MaxPunct,Tree,BcCheck,Beam) :-
    (   BcCheck == on
    ->  is_inactive_edge(DtFsAll,BitcodeAll,EdgeId)
    ;   is_inactive_edge(DtFsAll,_,EdgeId)
    ),
    unpack(Beam,EdgeId,DtFsAll,+,Tree,0,_,MaxPunct).

unpack(0,Id,Fs,_,Tree,Pu0,Pu,Pux) :-
    !,
    his(Id,His),
    unpack_(His,Id,Fs,Tree,Pu0,Pu,Pux,0),
    Pu =< Pux.
unpack(Beam,Id,Fs,Proj,Tree,Pu0,Pu,Pux) :-
    unpack_beam(Proj,Id,Fs,Tree,Pu0,Pu,Pux,Beam).

%% Unpacking with a beam:
%%
%% - Unpack all derivations for non-maximal projections (-).
%% - Unpack the (locally) most probable derivation trees if the history
%%   represents a maximal projection (+).
unpack_beam(-,Id,Fs,Tree,Pu0,Pu,Pux,Beam) :-
    his(Id,His),
    unpack_(His,Id,Fs,Tree,Pu0,Pu,Pux,Beam),
    Pu =< Pux.
unpack_beam(+,Id,Fs,Tree,Pu0,Pu,Pux,Beam) :-
    Punct is Pux-Pu0,  % max aantal punctuations
    (   has_unpack_memo(Id,Punct,List)
    ->  true
    ;   unpack_best(Beam,Id,List,Punct),
	memoize_unpack(Id,Punct,List)
    ),
    memo_members(List,Fs,Tree,Pu0,Pu,Pux).

has_unpack_memo(Id,Punct,List) :-
    unpack_memo(Id,Punct,List),
    !.
has_unpack_memo(Id,Pu,[H|T]) :-
    unpack_memo(Id,Pu0,[H|T]),
    Pu0 < Pu.		      %  at least one solution found with smaller number of max punct

unpack_best(Beam,Id,List,Pux) :-
    n_best(Beam,Score,
	   (   his(Id,His),
	       unpack_(His,Id,Fs,Tree,0,Pu,Pux,Beam),
	       sentence_fluency_maxent(Fs,Tree,Score,_)
	   ),
	   g(Fs,Tree,Pu),List
	  ).

%% Calculate the amount of punctuation after using a memoized history.
memo_members(List,Fs,Tree,Pu0,Pu,Pux) :-
    member(g(Fs,Tree,PuDiff),List),
    Pu is Pu0 + PuDiff,
    Pu =< Pux.    

memoize_unpack(Id,Punct,List) :-
    assertz(unpack_memo(Id,Punct,List)).

unpack_(vgap,EdgeId,Fs,Tree,Pu,Pu,_Pux,_Beam) :-
    inactive_edge_ref(EdgeId,Ref),
    clause(inactive_edge(Fs,_,EdgeId),Body,Ref),
    call(Body),
    alpino_data:deriv_tree_struct(vgap,Fs,[],Tree).
unpack_(gap(RuleId),EdgeId,Fs,Tree,Pu,Pu,_Pux,_Beam) :-
    inactive_edge_ref(EdgeId,Ref),
    clause(inactive_edge(Fs,_,EdgeId),Body,Ref),
    call(Body),
    alpino_data:deriv_tree_struct(RuleId,Fs,[],Tree).
unpack_(l(ref(Class,Tag,Label,Used,P0,P0,R0,R0,His,N,GuidesTag)),EdgeId,Fs,Tree,
       Pu0,Pu,_Pux,_Beam) :-
    (   Tag = punct(_)
    ->  Pu is Pu0 + 1
    ;   Pu = Pu0
    ),
    inactive_edge_ref(EdgeId,Ref),
    clause(inactive_edge(Fs,_,EdgeId),Body,Ref),
    call(Body),
    alpino_data:deriv_tree_struct(Used,Fs,
			   lex(ref(Class,Tag,Label,Used,P0,P0,R0,R0,His,N,GuidesTag)),Tree),
    unify_lex(Fs,Tag,_,Used,P0,R0).
unpack_(r(RuleId,His),_,Mother,Tree,Pu0,Pu,Pux,Beam) :-
    alpino_genrules:grammar_rule_unpack(RuleId,Mother,Fses,Projs),
    unpack_ds(His,Fses0,Projs,Trees,Pu0,Pu,Pux,Beam),
    Fses0 = Fses,
    alpino_data:deriv_tree_struct(RuleId,Mother,Trees,Tree).

unpack_ds([],[],[],[],Pu,Pu,_Pux,_Beam).
unpack_ds([H|T],[FsH|FsT],[ProjH|ProjT],[HDtr|NewT],Pu0,Pu,Pux,Beam) :-
    unpack(Beam,H,FsH,ProjH,HDtr,Pu0,Pu1,Pux),
    unpack_ds(T,FsT,ProjT,NewT,Pu1,Pu,Pux,Beam).

top_edge(top,EdgeId) :-
    alpino_lc_in:top_category_(Mother),
    inactive_edge(Mother,_BitcodeAll,EdgeId).

top_edge(robust,EdgeId) :-
    alpino_lc_in:robust_top_category_(Mother),
    inactive_edge(Mother,_BitcodeAll,EdgeId).

%% Unify some lexical information in the dt structure.
unify_lex(Fs,Tag,Stem,Used,P,R) :-
    (   alpino_data:punct(Fs)
    ->  true
    ;   % this fails for punctuation, which has no dt att!
	(   nonvar(Tag), Tag = with_dt(_,_)
	->  alpino_data:dt_fwrd_positions(Fs,P,R)
	;   alpino_data:dt_hwrd_positions(Fs,P,R),
	    alpino_data:lexical_node(Fs,Stem,Used,_,_)
	)
    ).

list_first_n(L,NewL,N) :-
    list_first_n(L,NewL,0,N).

%% steadfast, zonder cuts:
list_first_n([],[],_,_).
list_first_n([H|T],Result,Count,N) :-
    (   Count =:= N
    ->  Result = []
    ;   NewCount is Count + 1,
	Result=[H|NewT],
	list_first_n(T,NewT,NewCount,N)
    ).

%%%%%%%%%%%%%%%%%%%%%
% Terminal handling %
%%%%%%%%%%%%%%%%%%%%%

frames_of_deriv(Tree,Frames) :-
    findall(Frame,frame_of_deriv(Tree,Frame),Frames).

frame_of_deriv(Tree,frame(P0,P,Q0,Q,Label,Tag,Used,His)) :-
    subtree(Tree,tree(_,_,lex(ref(_Class,Tag,Label,Used,P0,Q0,P,Q,His,_N,_GuidesTag)),_)).

subtree(Tree,Tree).
subtree(tree(_,_,Ds,_),Tree) :-
    member(Tree1,Ds),
    subtree(Tree1,Tree).

number_terminals(tree(_,_,Ds,_),N,NextN,Str0,Str) :-
    number_terminals_tree_ds(Ds,N,NextN,Str0,Str).

number_terminals_tree_ds(lex(ref(_,_,_,Terminal,N,N,NextN,NextN,_,_,_)),N,NextN,
                         Str0,Str) :-
    alpino_genlex:surf_to_list(Terminal,Words,[]),
    length(Words,Len),
    NextN is N + Len,
    append(Words,Str,Str0).
number_terminals_tree_ds([],N,N,Str,Str).
number_terminals_tree_ds([H|T],N,NextN,Str0,Str) :-
    number_terminals_daughters([H|T],N,NextN,Str0,Str).

number_terminals_daughters([],N,N,Str,Str).
number_terminals_daughters([Daughter|Rest],N,NextN,Str0,Str) :-
    number_terminals(Daughter,N,NextN0,Str0,Str1),
    number_terminals_daughters(Rest,NextN0,NextN,Str1,Str).

terminals(tree(_,_,Ds,_),Str0,Str) :-
    terminals_tree_ds(Ds,Str0,Str).

terminals_tree_ds(lex(ref(_,_,_,Terminal,_,_,_,_,_,_,_)),Str0,Str) :-
    alpino_genlex:surf_to_list(Terminal,Words,[]),
    append(Words,Str,Str0).
terminals_tree_ds([],Str,Str).
terminals_tree_ds([H|T],Str0,Str) :-
    terminals_daughters([H|T],Str0,Str).

terminals_daughters([],Str,Str).
terminals_daughters([Daughter|Rest],Str0,Str) :-
    terminals(Daughter,Str0,Str1),
    terminals_daughters(Rest,Str1,Str).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Syntax/semantics lookup %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The syn_sem_lex predicate adds
%% subcategorization information and the word inflection, based on
%% the tag of a lexical item. 
lookup_tag(Tag,Label,Cat,Ref,Dt,Word) :-
    findall(Cat-Constraints1,alpino_lex_types:lex(Cat,Tag,Hwrd,Constraints1),List),
    nth(N,List,Cat-Constraints),
    alpino_genlex:call_constraints(Constraints),
    (   Tag = with_dt(complex_etc,_)
    ;   alpino_data:dt_if_defined(Cat,Dt)
    ),
    alpino_data:lexical(Hwrd,Label,_,Word,_,_,gen,_),
    alpino_tr_tag:tr_tag(Tag,Class),
    Ref=ref(Class,Tag,Label,Word,_,_,_,_,gen,N,_).

%% Add syntax/semantics to the lexical entries.
syn_sem_lex(Cand,His,Bitcode) :-
    lex_or_punct(Tag,Label,Bitcode,Dt,_,Surfs),
    member(Surf,Surfs),
    findall(SynSem-lex(Ref),
            lookup_tag(Tag,Label,SynSem,Ref,Dt,Surf),
            Cands),
    member(Cand-His,Cands),
    debug_message(3,"found att-val structure for lex ~w ~w~n",[Label,Tag]).

lex_or_punct(punct(Type),Comma,0,_,_,[Comma]) :-
    alpino_genlex:punct(Comma,Type).
lex_or_punct(Tag,Label,Bitcode,Dt,Dt2,Surfs) :-
    lex(Tag,Label,Bitcode,Dt,Dt2,Surfs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% check_part_of_dt %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% a. dt_part/1
%% check that the DT of the node is unifiable with
%% some part of the input semantics
%%
%% b. check_c_list/1
%% for nodes of type clist, take the values of
%% cats and conj, and check that there is a DT
%% such that our cats is a sublist of CNJ and
%% our conj is a sublist of CRD
%%
%% c. dt_check_percolation_features
%%
%% check that all elements on the list-valued
%% features capps, cdets, cmods, cpredms
%% actually occur as app, det, mod, predm
%% in the current DT, in an accessible place.
%%
%% d. check mexs feature
%%
%% e. check tags feature
%%
%% these checks are crucial to keep the number
%% of edges small

check_part_of_dt(_,Mother) :-
    (   alpino_data:dt(Mother,Dt)
    ->  (   var(Dt)  % e.g. rule imp, imp_imp
        ->  true
        ;   alpino_adt:dt_part(Dt),
            dt_check_percolation_features(Mother,Dt)
        )
    ;   check_clist(Mother)
    ),
    check_mexs(Mother),
    check_tags(Mother).

check_tags(Mother) :-
    alpino_data:tags(Mother,List),
    check_tags_list(List).

check_tags_list(Var):-
    var(Var), !.
check_tags_list([]).
check_tags_list([H|T]) :-
    check_tags_val(H),
    check_tags_list(T).

check_tags_val(Tag) :-
    alpino_data:dt_out(Tag,DT),
    alpino_adt:dt_part(DT).

check_mexs(Mother) :-
    alpino_data:mexs(Mother,List),
    check_mexs_list(List).

check_mexs_list(Var):-
    var(Var), !.
check_mexs_list([]).
check_mexs_list([H|T]) :-
    check_mexs_val(H),
    check_mexs_list(T).

check_mexs_val(H) :-
    alpino_data:mexs_cat_mods(H,Out),
    (  Out = []
    ;  Out = [DT],
       alpino_data:dt(DT,_,_,Cat,_),
       \+ Cat = ap,  %  only modifiers that *can* extrapose, such as pp, rel, cp
       \+ Cat = advp,
       alpino_adt:dt_part(DT)
    ).


dt_check_percolation_features(Mother,Dt) :-
    alpino_data:percolation_features(Mother,Apps,Capps,Dets,Cdets,
                              Mods,Cmods,Predms,Cpredms),
    alpino_wappend:wsubseq(Cmods,Mods),  % in all orderings
    alpino_wappend:wsubseq(Capps,Apps),   % in given ordering
    alpino_wappend:wsubseq(Cdets,Dets),
    alpino_wappend:wsubseq(Cpredms,Predms),
    dt_check_percolation_f(Cmods,mod,Dt),
    dt_check_percolation_f(Capps,app,Dt),
    dt_check_percolation_f(Cdets,det,Dt),
    dt_check_percolation_f(Cpredms,predm,Dt).

dt_check_percolation_f(List,_Att,_Dt) :-
    var(List), !.
dt_check_percolation_f([],_,_).
dt_check_percolation_f([H|T],Att,Dt):-
    dt_check_percolation(H,Att,Dt),
    dt_check_percolation_f(T,Att,Dt).

%% Val must unify with a member of Dt:(vc|body)*:Att  also tag and nucl
dt_check_percolation(Val,Att,Dt) :-
    hdrug_feature:e(Att,Dt,List),
    member(Val,List).

dt_check_percolation(Val,Att,Dt) :-
    hdrug_feature:e(vc,Dt,VC),
    dt_check_percolation(Val,Att,VC).

dt_check_percolation(Val,Att,Dt) :-
    hdrug_feature:e(cnj,Dt,List),
    member(BODY,List), % typically in each cnj..
    dt_check_percolation(Val,Att,BODY).

dt_check_percolation(Val,Att,Dt) :-
    hdrug_feature:e(tag,Dt,Tag),
    dt_check_percolation(Val,Att,Tag).

dt_check_percolation(Val,Att,Dt) :-
    hdrug_feature:e(nucl,Dt,Tag),
    dt_check_percolation(Val,Att,Tag).

dt_check_percolation(Val,Att,Dt) :-
    hdrug_feature:e(cmp,Dt,COMP),
    bridge_comp(COMP),
    hdrug_feature:e(body,Dt,BODY),
    dt_check_percolation(Val,Att,BODY).

bridge_comp(COMP) :-
    alpino_data:dt(COMP,Hwrd,_,_,_),
    alpino_data:label(Hwrd,Bridge,_,_,_,_),
    bridge(Bridge).

bridge('aan het').
bridge(dat).
bridge(om).
bridge(te).

check_clist(Mother) :-
    (   nonvar(Mother),
        alpino_data:clist(Mother,CatsList0,ConjList0)
    ->  alpino_adt:dt_part(DT),
	alpino_data:dt_cnj_crd(DT,CatsList,ConjList),
	lists:append(_,CatsList0,CatsList),
	lists:append(_,ConjList0,ConjList)
    ;   true
    ).


%%%
%%% Printing and other debugging tools
%%%

:- public active_edges/0, inactive_edges/0,
          print_active_edge_ids/0,
          print_inactive_edge_ids/0.

print_active_edge_ids :-
	findall(Id,active_edge(_,_,_,Id,_,_,_),Ids),
	print_edge_ids_aux(Ids).

print_inactive_edge_ids :-
	findall(Id,inactive_edge(_,_,Id),Ids),
	print_edge_ids_aux(Ids).


print_edge_ids_aux([]).
print_edge_ids_aux([Head|Tail]) :-
	write(Head), nl,
	print_edge_ids_aux(Tail).

active_edges :-
    (   active_edge(_,_,Bc,Id,Dtrs1,Dtrs2,N),
        start_it_deepening(debug_edges,0,10,Cur),
	unpack_ds(Dtrs1,_,_,Trees,0,Cur,Cur,0),
	get_terminals_ds(Trees,Terms,Terms1),
	get_terminals_uds(Dtrs2,Terms1,[]),
        end_it_deepening(debug_edges),
        write(active_edge(N,Bc,Id,Terms)),nl,
        fail
    ;   true
    ).  

get_terminals_uds(Dtrs,Terms0,Terms) :-
    member(Dtr,Dtrs),
    number(Dtr),
    !,
    start_it_deepening(uds,0,10,Cur),
    unpack(0,Dtr,_,_,Tree,0,Cur,Cur),
    end_it_deepening(uds),
    get_terminals(Tree,Terms0,Terms).
get_terminals_uds(_,T,T).


get_terminals(Tree,Terms) :-
    get_terminals(Tree,Terms,[]).

get_terminals(tree(_,_,Ds,_),Terms0,Terms) :-
    get_terminals_ds(Ds,Terms0,Terms).

get_terminals_ds(lex(ref(_,_,_,Word,_,_,_,_,_,_,_)),[Word|Terms],Terms).
get_terminals_ds([],Terms,Terms).
get_terminals_ds([H|T],Terms0,Terms) :-
    get_terminals_daughters([H|T],Terms0,Terms).

get_terminals_daughters([],Terms,Terms).
get_terminals_daughters([Daughter|Rest],Terms0,Terms) :-
    get_terminals(Daughter,Terms0,Terms1),
    get_terminals_daughters(Rest,Terms1,Terms).


inactive_edges :-
    inactive_edge_words(_).

inactive_edge_words(Words) :-
    (   inactive_edge(_,Bc,N),
	format("inactive edge: ~w ~w~n",[N,Bc]),
        start_it_deepening(debug_edges,0,10,Cur),
	unpack_all(Bc,_,N,Cur,Tree,on,0),
        end_it_deepening(debug_edges),
	get_terminals(Tree,Words),
        get_id(Tree,Id),
	format("    ~w ~w~n",[Id,Words]),
        fail
    ;	true
    ).

get_id(Tree,Tag/N) :-
    alpino_data:deriv_tree_struct(_,_,lex(ref(_,Tag,_,_,_,_,_,_,_,N,_)),Tree),
    !.
get_id(Tree,Id) :-
    alpino_data:deriv_tree_struct(Id,_,_,Tree).

simple_inactive_edge(inactive_edge(N,Bc,Id,Words)) :-
        start_it_deepening(debug_edges,0,10,Cur),
	unpack_all(Bc,_,N,Cur,Tree,on,0),
        end_it_deepening(debug_edges),
	get_terminals(Tree,Words),
	get_id(Tree,Id).

update_inactive_edges :-
	findall(Edge,simple_inactive_edge(Edge),Edges),
	hdrug_gui:update_array(Edges,inactive_edges).

:- public show_edge/3, show_gen_lex/3, gen_init/1.

gen_init(Ref):-
    clean,
    hdrug:a_lf(Ref,Sem),
    prepare_adt_and_lex(Sem,_Fs,_Bitcodeall,top),
    count_edges(lex(_,_,_,_,_,_),Edges),
    debug_message(2,"~w lexical frames for generation~n",[Edges]).

show_gen_lex(Word,Type0,Output) :-
    (   Type0 == default -> Type=fs ; Type0=Type ),
    lex(A0,Word,B0,C0,D0,E0),
    copy_term(lex(A0,Word,B0,C0,D0,E0),lex(A,Word,B,C,D,E),Cons),
    hdrug_show:show(Type,Output,[clause(lex(A,Word,B,C,D,E),Cons)]).

/*
show_gen_fs(Type0,Output) :-
    (   Type0 == default -> Type=fs ; Type0=Type ),
    alpino_adt:simple_fs(Result0),
    copy_term(Result0,Result,Cons),
    hdrug_show:show(Type,Output,[clause(Result,Cons)]).
*/

show_edge(N,Type,Output) :-
    number(N),
    start_it_deepening(debug_edges,0,10,Cur),
    unpack_all(_,Fs,N,Cur,Tree,off,0),
    end_it_deepening(debug_edges),
    alpino_data:result_term(_,_,Fs,Tree,_,Result),
    hdrug_show:show(Type,Output,[value(Result)]).

%% todo
show_edge(N,Type,Output) :-
     number(N),
     start_it_deepening(debug_edges,0,10,Cur),
     active_edge(Next,Cat,_Bc,Id,Dtrs1,Dtrs2,N),
     unpack_ds(Dtrs1,_,_,Trees1,0,Pu,Cur,0),
     unpack_show([Next|Dtrs2],Trees2,Pu,Cur),
     lists:append(Trees1,Trees2,Trees),
     end_it_deepening(debug_edges),
     alpino_data:result_term(_,_,Cat,tree(Cat,Id,Trees,_),_,Result),
     hdrug_show:show(Type,Output,[value(Result)]).

unpack_show([],[],_,_).
unpack_show([H|T],[Tree|Trees],Cur0,Cur) :-
    unpack_show1(H,Tree,Cur0,Cur1,Cur),
    unpack_show(T,Trees,Cur1,Cur).

unpack_show1(Num,Tree,Cur0,Cur1,Cur) :-
    number(Num),!,
    unpack_ds([Num],_,_,[Tree],Cur0,Cur1,Cur,0).
unpack_show1(Term,tree('   ******'(Fun),'   ******'(Fun),[],_),Cur,Cur,_) :-
    functor(Term,Fun,_).

:- public show_top_edge/2, show_cat_edge/3.

show_top_edge(Type,Output) :-
    alpino_data:top_cat(Cat),
    alpino_cg:inactive_edge(Cat,_,N),
    show_edge(N,Type,Output).

show_cat_edge(Cat,Type,Output) :-
    atomic(Cat),
    alpino_cg:inactive_edge(Term,_,N),
    functor(Term,Cat,_),
    show_edge(N,Type,Output).

:- public combine_rule_edges/2.
combine_rule_edges(Rule,Id1) :-
    is_inactive_edge(OldMother,BitCode,Id1),
    %his(His,_Id,IDerivTree),
    alpino_genrules:headed_grammar_rule(OldMother,Rule,Mother,Rest),
    substitute(OldMother,Rest,Id1,UDaughters),
    construct_edge(Mother,BitCode,Rule,[],UDaughters,EDGE),
    dot_movement(EDGE,_NEDGE).

glex(N) :-
    set_flag(parse_or_generate,generate),
    hdrug:a_sentence(N,Sent),
    hdrug:display_extern_phon(Sent),
    set_flag(current_ref,N),
    alpino_treebank:treebank_directory(Dir),
    charsio:format_to_chars('~w/~w.xml',[Dir,N],Chars),
    atom_codes(FileXml,Chars),
    alpino_treebank:xml_file_to_dt(FileXml,DT),
    clean,
    alpino_adt:dt_to_adt(DT,Sem),    
    prepare_adt_and_lex(Sem,_Fs,_Bc,top),
    count_edges(lex(_,_,_,_,_,_),Edges),
    debug_message(1,"~w lexical frames for generation~n",[Edges]),
    gtags.

:- public glex/1, gtags/0.
gtags :-
    (   lex(Frame,Root,BitCode,_,_,Surfs),
        format(user_error,"~w ~w ~w ~w~n",[Root,Frame,BitCode,Surfs]),
        fail
    ;   true
    ).

%%%%%%%%%%%%
%% Packing %
%%%%%%%%%%%%

%% This predicate produces a 'frozen' representation of an edge
%% where blocked goals are separated and variables are instantiated
%% with numbered terms.
freeze_edge(inactive_edge(SynSem,Bitcode,_),FS) :-
    copy_term(SynSem,Copy,Const),
    numbervars(Copy/Const,0,_),
    terms:term_hash(Copy/Const/Bitcode,Hash),
    FS = fs(Hash,Copy,Bitcode,Const).
freeze_edge(active_edge(_,_,_,_,_),none).

%% Pack an inactive edge if there is already an inactive edge on the chart
%% with the same (frozen) attribute-value structure. Packing adds a
%% construction history to the existing edge.
pack_edge(inactive_edge(__SynSem,Bitcode,Ds),fs(Hash,Copy,Bitcode,Cons)) :-
    inactive_edge_frozen(Hash,Copy,Bitcode,Cons,N),
    %debug_message(2,"history ~q can be forward-packed as edge ~d~n",
    %			     [Ds,N]),
    assert_history(N,Ds).

%%%%%%%
%%% survive if at least one of your derivs is not ruled out
filter_edge(Id,DtrPs) :-
    (   rule_out(Id,_)        % only try if there is a filter for this rule anyway
    ->  unpack_rules(Id,DtrPs,tree(Rule,Ds)),
	\+ rule_out(Rule,Ds),
	!
    ;   true
    ).


%%
%%
%%

rule_out(n_n_mod(komma),[_,_,tree(modifier_p(1),_),_]).
rule_out(n_n_modroot(min),
	 [_,_,tree(top_start_xp,[tree(max_xp(post_pp),_)]),_]
	).
rule_out(n_n_modroot(min),
	 [_,_,tree(top_start_xp,[tree(max_xp(pp),_)]),_]
	).
rule_out(n_n_modroot(min),
	 [_,_,tree(top_start_xp,[tree(max_xp(pred),_)]),_]
	).
rule_out(n_n_modroot(haak),
	 [_,_,tree(top_start_xp,[tree(max_xp(post_pp),_)]),_,_]
	).
rule_out(n_n_modroot(haak),
	 [_,_,tree(top_start_xp,[tree(max_xp(pp),_)]),_,_]
	).
rule_out(n_n_modroot(haak),
	 [_,_,tree(top_start_xp,[tree(max_xp(pred),_)]),_,_]
	).
rule_out(n_n_modroot(haak),
	 [_,_,tree(top_start_xp,[tree(max_xp(om_rel),_)]),_,_]
	).
rule_out(n_n_mod_a,[_,l(ref(Tag,_,_,_,_,_,_,_,_,_,_))]) :-
    lists:member(Tag,[adjective(ge_no_e(_)),
		      adjective(end(_)),
		      adjective(ende(_)),
		      adjective(ge_both(_))
		     ]).
rule_out(n_n_mod_a,[_,tree(a_part_a,_)]).
rule_out(n_n_mod_a,[_,tree(a_pred_a,_)]).
rule_out(n_n_mod_a,[_,tree(a_a_np_comp,_)]).
rule_out(n_n_mod_a,[_,tree(a_np_comp_a,_)]).

rule_out(vp_v_mod,[_,tree(mod1a,[l(_)])]).

rule_out(a_a_bracketed_mod,[_,_,tree(top_start_xp,[tree(max_xp(pp),_)]),_,_]).

rule_out(modifier_p(1),[_,tree(np_det_n,[_,tree(n_n_modroot(haak),_)]),_,_]).

unpack_rules(Id,Ptrs,tree(Id,Ids)) :-
    unpack_rules_ds(Ptrs,Ids).

unpack_rules_ds([],[]).
unpack_rules_ds([Ptr|Ptrs],[D|Ds]) :-
    his(Ptr,His),
    unpack_rule(His,D),
    unpack_rules_ds(Ptrs,Ds).

unpack_rule(l(L),l(L)).
unpack_rule(gap(G),gap(G)).
unpack_rule(vgap,vgap).
unpack_rule(r(Id,Ptrs),Pat) :-
    unpack_rules(Id,Ptrs,Pat).

/*
survive_rule_out([tree(Id,Tree)|T]) :-
    (   rule_out(Id,Tree)
    ->  survive_rule_out(T)
    ;   true
    ).
*/

:- initialize_flag(treex_corrections,off).

transform_adt(Tree0,Tree) :-
    apply_generic_transformations(Tree0,Tree1),
    hdrug_flag(treex_corrections,Bool),
    (   Bool == on
    ->  alpino_treex:apply_transformations(Tree1,Tree),
	instantiate_indexes(Tree),
	debug_message(1,"treex corrections done~n",[])
    ;   Tree1 = Tree
    ).

apply_generic_transformations(Tree0,Tree) :-
    apply_generic_transformations0(Tree0,Tree1),
    (   Tree0 == Tree1
    ->  Tree1 = Tree
    ;   apply_generic_transformations(Tree1,Tree)
    ).

apply_generic_transformations0(Tree0,Tree) :-
    generic_transform_rule(Tree0,Tree1),
    !,
    apply_generic_transformations0(Tree1,Tree).
apply_generic_transformations0(tree(Node,Ds0),tree(Node,Ds)) :-
    generic_transform_ds(Ds0,Ds).

generic_transform_ds([],[]).
generic_transform_ds([H0|T0],[H|T]):-
    apply_generic_transformations(H0,H),
    generic_transform_ds(T0,T).

%% for any input
%% topic_drop is currently not allowed for generation (explicitly
%% ruled out in the grammar), so we'll try to generate a ynquestion
%% or imparative instead (which should be pretty close).
% generic_transform_rule(tree(r(Rel,adt_lex(A,B,C,D,E0)),[]),
% 		       tree(r(Rel,adt_lex(A,B,C,D,[stype=imparative|E])),[])) :-
%     select(stype=topic_drop,E0,E).

% generic_transform_rule(tree(r(Rel,i(I,adt_lex(A,B,C,D,E0))),[]),
% 		       tree(r(Rel,i(I,adt_lex(A,B,C,D,[stype=imparative|E]))),[])) :-
%     select(stype=topic_drop,E0,E).

%% ik moet [ld er] wel doorvaren
%% = ik moet wel [ld er door] varen
generic_transform_rule(tree(RelCat,Ds0),tree(RelCat,[Hd,LD|Ds])) :-
    LD0=tree(r(ld,adt_lex(advp,ER,_,_,_)),[]),
    lists:select(LD0,Ds0,Ds1),
    lists:member(ER,[er,daar,hier,waar]),
    Hd0=tree(r(hd,adt_lex(HdCat,VOER_DOOR,_,verb,Atts)),[]),
    lists:select(Hd0,Ds1,Ds),
    atom_concat(VOER,DOOR,VOER_DOOR),
    atom_concat('_',PREP,DOOR),
    lists:member(PREP,[af,door,tussen,heen,uit,vandoor,in,vandaan,vanaf]),
    Hd=tree(r(hd,adt_lex(HdCat,VOER,VOER,verb,Atts)),[]),
    LD = tree(r(ld,p(pp)),[tree(r(hd,adt_lex(pp,PREP,PREP,prep,[])),[]),
			   tree(r(obj1,adt_lex(advp,ER,ER,adv,[])),[])]).
	    
	    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Combine MWU nodes to a single node %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

combine_mwu(tree(r(REL,i(Ix,CAT0)),Ds0),
            tree(r(REL,i(Ix,CAT)),Ds)) :-
    !,
    combine_mwu(tree(r(REL,CAT0),Ds0),tree(r(REL,CAT),Ds)).
combine_mwu(tree(r(REL,p(mwu)),Ds),tree(r(REL,adt_lex(_,New,New,Cat,Attrs)),[])) :-
    mwu_words(Ds,Words,Cat,Attrs),
    hdrug_util:concat_all(Words,New,' '),
    !.
combine_mwu(tree(r(REL,p(mwu(Root0))),Ds),tree(r(REL,adt_lex(_,Root,Root,Cat,Attrs)),[])) :-
    mwu_words(Ds,_Words,Cat,Attrs),
    (   atom(Root0)
    ->  Root = Root0
    ;   atom_chars(Root,Root0)
    ), !.
combine_mwu(tree(r(REL,p(mwu(ROOT,SENSE))),Ds),tree(r(REL,adt_lex(_,ROOT,SENSE,Cat,Attrs)),[])) :-
    mwu_words(Ds,_Words,Cat,Attrs),
    !.
combine_mwu(tree(r(Rel,Cat),Ds),tree(r(Rel,Cat),NewDs)) :-
    combine_mwu_daughters(Ds,NewDs).
combine_mwu_daughters([],[]).
combine_mwu_daughters([Head|Tail],[NewHead|NewTail]) :-
    combine_mwu(Head,NewHead),
    combine_mwu_daughters(Tail,NewTail).

mwu_words([],[],_,_).
mwu_words([tree(r(mwp,adt_lex(_,Hw,_Sense,C,A)),[])|T],[Hw|Ht],
          C,A) :-
    !,   % mwu's agree about their attributes
    mwu_words(T,Ht,C,A).
mwu_words([tree(r(mwp,adt_lex(_,Hw,_Sense,_C,_A)),[])|T],[Hw|Ht],
          C,A) :-
    !,   % mwu's agree about their attributes, but we don't rely on it
    mwu_words(T,Ht,C,A).


all_bitcodes([],All,All).
all_bitcodes([inactive_edge(_,Code,_)|T],All0,All) :-
    All1 is All0 \/ Code,
    all_bitcodes(T,All1,All).

instantiate_indexes(Tree) :-
    collect_indexes(Tree,Indexes,[],Variables,[]),
    next_n(Indexes,I),
    instantiate_indexes0(Variables,I).

instantiate_indexes0([],_).
instantiate_indexes0([H|T],H) :-
    H2 is H + 1,
    instantiate_indexes0(T,H2).

collect_indexes(tree(Node,Ds),Is0,Is,Vs0,Vs) :-
    collect_indexes(Node,Is0,Is1,Vs0,Vs1),
    collect_indexes_ds(Ds,Is1,Is,Vs1,Vs).
collect_indexes(r(_,Node),Is0,Is,Vs0,Vs) :-
    collect_indexes(Node,Is0,Is,Vs0,Vs).
collect_indexes(p(_),Is,Is,Vs,Vs).
collect_indexes(adt_lex(_,_,_,_,_),Is,Is,Vs,Vs).
collect_indexes(i(_),Is,Is,Vs,Vs).
collect_indexes(i(Index,_),Is0,Is,Vs0,Vs) :-
    (   var(Index)
    ->  Vs0 = [Index|Vs],
	Is0 = Is
    ;   Is0 = [Index|Is],
	Vs0 = Vs
    ).

collect_indexes_ds([],Is,Is,Vs,Vs).
collect_indexes_ds([H|T],Is0,Is,Vs0,Vs):-
    collect_indexes(H,Is0,Is1,Vs0,Vs1),
    collect_indexes_ds(T,Is1,Is,Vs1,Vs).

next_n(Indexes0,I) :-
    sort(Indexes0,Indexes),
    add1_last(Indexes,I).

add1_last([],0).
add1_last([H|T],Index) :-
    add1_last(T,H,Index).

add1_last([],I0,I) :-
    I is I0 + 1.
add1_last([H|T],_,I) :-
    add1_last(T,H,I).


%%% ensure there are no indexed nodes i(I,N)
%%% for which there is no dependent node i(I)
%%% also, ensure there are no indexed nodes i(I)
%%% for which there are no i(I,N)
%%% and make sure, the i(I,N) precedes I(I) 
ensure_indices_ok(Sem0,Sem) :-
    collapse_all(Sem0,Sem1,Ps0),
    find_missing_ps(Ps0,Missing,[]),
    all_ana_indices(Sem1,Ana,[]),
    reconsider_indices(Sem1,Sem,Ana,Missing).

all_ana_indices(tree(Node,Ds)) -->
    all_ana_indices_node(Node),
    all_ana_indices_ds(Ds).

all_ana_indices_ds([]) --> [].
all_ana_indices_ds([H|T]) -->
    all_ana_indices(H),
    all_ana_indices_ds(T).

all_ana_indices_node(r(_,N)) -->
    all_ana_indices_node(N).

all_ana_indices_node(p(_)) --> [].
all_ana_indices_node(adt_lex(_,_,_,_,_)) --> [].
all_ana_indices_node(i(_,_)) --> [].
all_ana_indices_node(i(I)) --> [I].

reconsider_indices(tree(Node0,Ds0),tree(Node,Ds),Ind,Missing) :-
    reconsider_indices_node(Node0,Node,Ind),
    reconsider_indices_ds(Ds0,Ds,Ind,Missing).

reconsider_indices_ds([],[],_,_).
reconsider_indices_ds([H0|T0],T,Ind,Missing) :-
    missing(H0,Missing),
    !,
    reconsider_indices_ds(T0,T,Ind,Missing).

reconsider_indices_ds([H0|T0],[H|T],Ind,Missing) :-
    reconsider_indices(H0,H,Ind,Missing),
    reconsider_indices_ds(T0,T,Ind,Missing).

reconsider_indices_node(r(Rel,Node0), r(Rel,Node), Ind) :-
    reconsider_indices_node(Node0,Node, Ind).
reconsider_indices_node(i(I),i(I),_).
reconsider_indices_node(p(Cat),p(Cat),_).
reconsider_indices_node(adt_lex(A,B,C,D,E),adt_lex(A,B,C,D,E),_).
reconsider_indices_node(i(Ix,Node),Result,Ind) :-
    (   lists:member(Ix,Ind)
    ->  Result = i(Ix,Node)
    ;   Result = Node
    ).

missing(tree(r(_,i(Ix)),[]),Missing):-
    lists:member(Ix,Missing).
missing(tree(r(_,i(Ix,_)),[]),Missing):-
    lists:member(Ix,Missing).
    

collapse_all(tree(r(Rel,Cat0),Ds0), tree(r(Rel,Cat),Ds), Params) :-
    collapse_one(Cat0,Ds0,Cat,Ds1,Params),
    collapse_ds(Ds1,Ds,Params).

collapse_ds(Var,Var2,_) :-
    var(Var),
    !,
    Var = Var2.
collapse_ds([],[],_).
collapse_ds([H0|T0],[H|T],Ps) :-
    collapse_all(H0,H,Ps),
    collapse_ds(T0,T,Ps).

collapse_one(i(Ix,Cat),Ds0,Out,Ds,Ps) :-
    lists:member(Ix=Val,Ps), !,
    (   var(Val)          % we see the index for the first time, put info on list
    ->  Val = s(Cat,Ds),
	Out = i(Ix,Cat),
	Ds0 = Ds
    ;		    % we saw the index before, now we know its content
	collapse_ds(Ds0,Ds1,Ps),
	Val = s(Cat,Ds1),
	Out = i(Ix),
	Ds = []
    ).
    
collapse_one(i(Ix),[],Out,Ds,Ps) :-
    lists:member(Ix=Val,Ps), !,
    (   var(Val)          % we see the index for the first time
    ->  Val = s(Cat,Ds),  % therefore its content should show up here
	Out = i(Ix,Cat)
    ;   Val = s(_,_),   % seen before, nothing changes
	Out = i(Ix),
	Ds = []
    ).

collapse_one(p(Cat),Ds,p(Cat),Ds,_).
collapse_one(adt_lex(A,B,C,D,E),[],adt_lex(A,B,C,D,E),[],_).


find_missing_ps(Var,Ps0,Ps) :-
    var(Var), !,
    Ps0 = Ps.
find_missing_ps([],Ps,Ps).
find_missing_ps([H|T],Ps0,Ps) :-
    find_missing_p(H,Ps0,Ps1),
    find_missing_ps(T,Ps1,Ps).

find_missing_p(Ind=s(Var,Ds),Ps0,Ps) :-
    (   var(Var)
    ->  Ps0 = [Ind|Ps],
	Var = none, Ds = []
    ;   Ps0 = Ps
    ).

%% rule_condition(Id,ADT)
rule_condition(vp_c_rootbar_c_v,ADT) :-
    !,
    has_rel(ADT,dlink),
    !.
rule_condition(vp_c_whq_c_v,ADT) :-
    !,
    mod_has_root(ADT),
    !.
rule_condition(vp_v_komma_arg(np_heavy),ADT) :-
    !,
    has_cat(ADT,whrel),
    !.
rule_condition(vp_arg_comma_v(np),ADT) :-
    !,
    has_cat(ADT,whrel),
    !.
rule_condition(vp_v_komma_arg(pred_heavy),ADT) :-
    !,
    rel_has_cat(ADT,predc,whrel),
    !.
rule_condition(vp_v_extra,ADT) :-
    !,
    has_rel(ADT,obcomp),
    !.
rule_condition(vp_v_m_extra,ADT) :-
    !,
    has_cat(ADT,rel),
    !.
rule_condition(vp_v_m_extra_vp,ADT) :-
    !,
    rel_has_cat(ADT,mod,oti),
    !.
rule_condition(vp_v_komma_mod,ADT) :-
    !,
    has_cat(ADT,cp),
    !.
      
rule_condition(_,_).

has_rel(tree(r(Rel,_),_),Rel).
has_rel(tree(_,Ds),R) :-
    lists:member(D,Ds),
    has_rel(D,R).

rel_has_cat(tree(r(Rel,Val),_),Rel,Cat):-
    has_cat0(Val,Cat).
rel_has_cat(tree(_,Ds),Rel,Cat) :-
    lists:member(D,Ds),
    rel_has_cat(D,Rel,Cat).

has_cat(tree(r(_,Val),_),Cat):-
    has_cat0(Val,Cat).
has_cat(tree(_,Ds),R) :-
    lists:member(D,Ds),
    has_cat(D,R).

has_cat0(p(Cat),Cat).
has_cat0(adt_lex(Cat,_,_,_,_),Cat).
has_cat0(i(_,Val),Cat) :-
    has_cat0(Val,Cat).

mod_has_root(tree(r(mod,Cat),_)) :-
    root(Cat).
mod_has_root(tree(_,Ds)) :-
    lists:member(D,Ds),
    mod_has_root(D).

root(p(Cat)):-
    root0(Cat).
root(i(_,X)):-
    root(X).

root0(smain).
root0(sv1).
root0(whq).

:- public analyse_edges/0.

analyse_edges :-
    findall(Rule,his(_,r(Rule,_)),Rules0),
    sort(Rules0,Rules),
    count_usage(Rules,CountedIds0),
    keysort(CountedIds0,CountedIds),
    format(user_error,"~w~n",[CountedIds]).

count_usage([],[]).
count_usage([Rule|Ids0],[C-Rule|Ids]) :-
    findall([],used_id(Rule),L),
    length(L,C),
    count_usage(Ids0,Ids).

used_id(Rule) :-
    his(Id,r(Rule,_)),
    (   his(_,r(_,List)),
	lists:member(Id,List)
    ;   top_edge(top,Id)
    ).

