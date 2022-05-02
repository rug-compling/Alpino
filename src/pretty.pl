:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- multifile
    hdrug_command/3,
    hdrug_command_help/3.
:- discontiguous
    hdrug_command/3,
    hdrug_command_help/3.

:- use_module(hdrug(hdrug_util)).
:- use_module(library(lists)).
:- use_module(utils).

%%%%%%%%%%%%%%%%%
%%%%% GUI %%%%%%%
%%%%%%%%%%%%%%%%%

:- public show_object_default2/1, show_object_default3/1.   % tcltk

%% click with mouse button <2> on number of object:
show_object_default2(No) :-
    show_object_no(No,tree(syn),clig),
    show_object_no(No,fs([[score]]),clig).

%% click with mouse button <3> on number of object:
show_object_default3(No) :-
    show_object_no(No,tree(user(dt)),clig),
    show_object_no(No,fs([[score]]),clig).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SHOW RULES AND LEX %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialize_flag(pretty_print_atmost,20).

hdrug_command(lex,hdrug_show:show(Type,Output,Things),L0) :-
    hdrug_flag(pretty_print_atmost,N),
    hdrug_cmdint:show_command(Type0,Output,L0,L1),
    (   Type0 == default -> Type = fs ; Type0 = Type ),
    findall_atmost(N,Thing,plex(Thing,L1,[]),Things).

hdrug_command(lex_analysis,hdrug_show:show(Type,Output,Things),L0) :-
    hdrug_flag(pretty_print_atmost,N),
    hdrug_cmdint:show_command(Type0,Output,L0,L1),
    (   Type0 == default -> Type = fs ; Type0 = Type ),
    findall_atmost(N,Thing,slex(Thing,L1,[]),Things).

hdrug_command(taglex,hdrug_show:show(Type,Output,Things),L0) :-
    hdrug_flag(pretty_print_atmost,N),
    hdrug_cmdint:show_command(Type0,Output,L0,L1),
    (   Type0 == default -> Type = fs ; Type0 = Type ),
    findall_atmost(N,Thing,taglex(Thing,L1,[]),Things).

hdrug_command(tl,hdrug_show:show(term(print),user,Things),L) :-
    findall(Thing,tl(Thing,L,[]),Things).

hdrug_command(mlex,mlex(L),L).

hdrug_command_help(lex, "lex <Type> <Output> Word",
		   "to view lexical entry").

:- public mlex/1.  % cmdint

mlex(L) :-
    (   lex_lexicon(Tag,Label,L,[],His),
	format("~w|~w|~w|~w~n", [Label,L,His,Tag]),
	fail
    ;	true
    ).

hdrug_command(rule,hdrug_show:show(Type,Output,Things),L0) :-
    hdrug_flag(pretty_print_atmost,N),
    hdrug_cmdint:show_command(Type0,Output,L0,L1),
    (   Type0 == default -> Type = fs ; Type0 = Type ),
    findall_atmost(N,Thing,prule(Thing,L1,[]),Things).

hdrug_command(grule,hdrug_show:show(Type,Output,Things),L0) :-
    hdrug_flag(pretty_print_atmost,N),
    hdrug_cmdint:show_command(Type0,Output,L0,L1),
    (   Type0 == default -> Type = fs ; Type0 = Type ),
    findall_atmost(N,Thing,grule(Thing,L1,[]),Things).

:- public prule/3, grule/3.

prule(clause(grammar_rule(Label,M,Ds),[]),[Label|L],L):-
    alpino_lc_in:grammar_rule(Label,M,Ds).

grule(clause(grammar_rule(Label,M,Ds),[]),[Label|L],L):-
    alpino_lc_in:grammar_rule_g(Label,M,Ds).

plex(clause(lex(Cat,Tag,Label,His),Constraints),L0,L):-
    lex_lexicon(Tag,Label,L0,L,His),
    alpino_lex_types:lex(Cat0,Tag,_,_),
    copy_term(Cat0,Cat,Constraints).

:- public slex/3, taglex/3.
slex(clause(syn_lex_analysis(P0,P,Cat,Rest),Constraints),[P0|L],L) :-
    alpino_lexical_analysis:syn_lex_analysis(P0,P,Cat0,Rest),
    copy_term(Cat0,Cat,Constraints).

taglex(clause(syn_sem_lex_analysis(Tag,Label,Used,P0,P,R0,R,His,N,Cat),
              Constraints),
       [Tag|L],L) :-
    alpino_lexical_analysis:tag(P0,P,R0,R,Label,Used,His,Tag),
    alpino_lexical_analysis:syn_sem_lex_analysis(Tag,Label,Used,P0,P,R0,R,His,N,Cat0),
    copy_term(Cat0,Cat,Constraints).


tl(clause(syn_sem_lex_analysis(Tag,Label,Used,P0,P,R0,R,His,N,SimpleCat),
              []),[],[]) :-
    alpino_lexical_analysis:tag(P0,P,R0,R,Label,Used,His,Tag),
    alpino_lexical_analysis:syn_sem_lex_analysis(Tag,Label,Used,P0,P,R0,R,His,N,Cat0),
    copy_term(Cat0,Cat,_),
    simplify_cat(Cat,SimpleCat).

hdrug_command(lexcount,lexcount(Ws),Ws).

:- public lexcount/1. % cmdint
lexcount(Ws) :-
    findall(_,plex(_,Ws,[]),List),
    length(List,N),
    format("~w entries for ~w~n",[N,Ws]).

send_rules :-
    tcl("set rules(max) 0",[]),
    format(user_error,"Updating rules...",[]),
    findall(Name,hook(alpino_lc_in:grammar_rule(Name,_,_)),Names0),
    sort(Names0,Names),
    update_array(Names,rules,0),
    format(user_error,"Done.~n",[]).

send_frames :-
    tcl("set frames(max) 0",[]),
    format(user_error,"Updating frames...",[]),
    findall(F/A,(hook(alpino_lex_types:user_clause(Term,_)),
		 functor(Term,F,A)),Names0),
    sort(Names0,Names),
    update_array(Names,frames,0),
    format(user_error,"Done.~n",[]).

send_tops :-
    tcl("set tops(max) 0",[]),
    (	top(grammar,_)
    ->	tcl("set tops(max) 1",[])
    ;	true
    ).

:- public a_lex/2, a_lex_concat/2.
a_lex(Words0,clause(lex(Cat,Tag,Label,His),[])) :-
    %%atom_term(Words0,Words1),
    (	atomic(Words0)
    ->	Words=[Words0]
    ;	Words=Words0
    ),
    lex_lexicon(Tag,Label,Words,[],His),
    alpino_lex_types:lex(Cat,Tag,_,_).

a_lex_concat(W0,clause(lex(Cat,Tag,Label,His),[])) :-
    split_atom(W0,Words),
    lex_lexicon(Tag,Label,Words,[],His),
    alpino_lex_types:lex(Cat,Tag,_,_).

split_atom(W0,Words) :-
    atom(W0),
    atom_codes(W0,Chars),
    split_chars(Chars,Words).

split_chars([],[]).
split_chars([C|Cs],[W|Words]) :-
    split_chars(Cs,[C|Rest],Rest,W,Words).

split_chars([],Chars,[],W,[]) :-
    atom_codes(W,Chars).
split_chars([C|Cs],Chars,Rest,W,Words) :-
    (	C==32
    ->	Rest=[],
	atom_codes(W,Chars),
	Words=[W1|Words1],
	split_chars(Cs,Rest1,Rest1,W1,Words1)
    ;	Rest=[C|Rest1],
	split_chars(Cs,Chars,Rest1,W,Words)
    ).

show_lex_id(Id,Type,Output) :-
    (	Id = '$ref'(_Refa,_Refb)
    ->	findall(
	 clause(lex_analysis(P0,P,Cat,Tag),[]),
	 clause(alpino_lexical_analysis:syn_lex_analysis(P0,P,Cat,Tag),true,Id),
		Cats),
	show(fs,clig,Cats)
    ;   show_lex(Id,Type,Output)
    ).

:- public show_lex/3.
show_lex(Lex,Type,Output) :-
    hdrug_flag(pretty_print_atmost,N),
    (	findall_atmost(N,Clause,a_lex(Lex,Clause),[Cl|Clauses])
    ->	show(Type,Output,[Cl|Clauses])
    ;	findall_atmost(N,Clause,a_lex_concat(Lex,Clause),Clauses),
	show(Type,Output,Clauses)
    ).

:- public show_rule/3.
show_rule(Rule,Type,Output) :-
    hdrug_flag(pretty_print_atmost,N),
    findall_atmost(N,Clause,a_rule(Rule,Clause),Clauses),
    show(Type,Output,Clauses).

:- public show_top/2.
show_top(Type,Output) :-
    hdrug_flag(pretty_print_atmost,N),
    findall_atmost(N,Clause,a_top(Clause),Clauses),
    show(Type,Output,Clauses).

:- public a_top/1.
a_top(clause(top(Cat),[])) :-
    alpino_lc_in:top_category_(Cat).

a_rule(Name,Clause) :-
    if(a_rule0(Name,Clause),
       true,
       a_rule1(Name,Clause)
      ).

a_rule0(Name,clause(grammar_rule(Name,Cat,Ds),[])) :-
    alpino_lc_in:grammar_rule(Name,Cat,Ds).

a_rule1(Name0,clause(grammar_rule(Name,Cat,Ds),[])) :-
    atomic(Name0),
    catch(atom_term(Name0,Name),
	  syntax_error(_,_,_,_,_),
	  fail
	 ),
    alpino_lc_in:grammar_rule(Name,Cat,Ds).

a_grule(Name,Clause) :-
    if(a_grule0(Name,Clause),
       true,
       a_grule1(Name,Clause)
      ).

a_grule0(Name,clause(grammar_rule(Name,Cat,Ds),[])) :-
    alpino_lc_in:grammar_rule_g(Name,Cat,Ds).

a_grule1(Name0,clause(grammar_rule(Name,Cat,Ds),[])) :-
    atomic(Name0),
    catch(atom_term(Name0,Name),
	  syntax_error(_,_,_,_,_),
	  fail
	 ),
    alpino_lc_in:grammar_rule_g(Name,Cat,Ds).

:- public show_relation/2.
show_relation(F/A,Medium) :-
    show_predicate(F/A,fs,Medium).

hdrug_command(frame,hdrug_show:show(Type,Output,Things),L0) :-
    hdrug_flag(pretty_print_atmost,N),
    hdrug_cmdint:show_command(Type0,Output,L0,L1),
    (   Type0 == default -> Type = fs ; Type0 = Type ),
    findall_atmost(N,Thing,frame(Thing,L1),Things).

hdrug_command(tag,hdrug_show:show(Type,Output,Things),L0) :-
    hdrug_flag(pretty_print_atmost,N),
    hdrug_cmdint:show_command(Type0,Output,L0,L1),
    (   Type0 == default -> Type = fs ; Type0 = Type ),
    findall_atmost(N,Thing,tag(Thing,L1),Things).

:- public show_frame/1, show_frame/3,
    show_tag/1, show_tag/3.

show_frame(In) :-
    show_frame(In,fs([]),clig).
show_frame(In,Type,Output) :-
    hdrug_flag(pretty_print_atmost,N),
    findall_atmost(N,Thing,frame(Thing,[In]),Things),
    hdrug_show:show(Type,Output,Things).

show_tag(In) :-
    show_tag(In,fs([]),clig).
show_tag(In,Type,Output) :-
    hdrug_flag(pretty_print_atmost,N),
    findall_atmost(N,Thing,tag(Thing,[In]),Things),
    hdrug_show:show(Type,Output,Things).

frame_to_goal(In,Frame) :-
    (	atom(In),
	atom_term(In,F/A)
    ->	functor(Frame,F,A)
    ;	In = F/A
    ->	functor(Frame,F,A)
    ;	atom(In)
    ->	alpino_lex_types:user_clause(Frame,_),
	functor(Frame,In,_)
    ;   In =.. [F|Args],
        append(Args,[_],NewArgs),
        Frame =.. [F|NewArgs]
    ;   In =.. [F|Args],
        append(Args,[_,_Cs0,_Cs],NewArgs),
        Frame =.. [F|NewArgs]
    ).

:- public frame/2.
frame(clause(Goal,Constraints),Ls) :-
    member(Frame,Ls),
    frame_to_goal(Frame,Goal),
    copy_term(Goal,Goal1),
    hook(alpino_lex_types:Goal1),
    copy_term(Goal1,Goal,Constraints).

:- public tag/2.
tag(clause(tag(Tag,Sign),Constraints),Ls) :-
    member(Tag,Ls),
    alpino_lex_types:lex(Sign0,Tag,_,_),
    copy_term(Sign0,Sign,Constraints).

hdrug_command(values,hdrug_show:show(Type,Output,Things),L0) :-
    hdrug_flag(pretty_print_atmost,N),
    hdrug_cmdint:show_command(Type0,Output,L0,L1),
    (   Type0 == default -> Type = fs ; Type0 = Type ),
    findall_atmost(N,Thing,value(Thing,L1),Things).

:- public value/2.

value(clause(val(Key,Val),BodyL),Ls) :-
    (   Ls = []
    ->  true
    ;   member(Key,Ls)
    ),
    alpino_parse_values:val(Key,Val,Body),
    prolog_conjunction(Body,BodyL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% OBJECT COMPARISON %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public
    compare_dt/2,           % cmdint
    compare_score/2,        % cmdint
    all_compare_score/2,    % cmdint
    analyse_differences/2,  % cmdint
    analyse_differences/0.  % tcltk, cmint, start.pl

hdrug_command(compare,analyse_differences(Obj1,Obj2),[Obj1,Obj2]).
hdrug_command(compare_long,analyse_differences_long(Obj1,Obj2),[Obj1,Obj2]).

hdrug_command(compare,analyse_differences,[]).

hdrug_command(compare_score,     compare_score(Obj1,Obj2),[Obj1,Obj2]).
hdrug_command(compare_penalties, compare_score(Obj1,Obj2),[Obj1,Obj2]).
hdrug_command(compare_features,  compare_score(Obj1,Obj2),[Obj1,Obj2]).

hdrug_command(all_compare_score,     all_compare_score(Obj1,Obj2),[Obj1,Obj2]).
hdrug_command(all_compare_penalties, all_compare_score(Obj1,Obj2),[Obj1,Obj2]).
hdrug_command(all_compare_features,  all_compare_score(Obj1,Obj2),[Obj1,Obj2]).

hdrug_command(compare_dt,        compare_dt(Obj1,Obj2),[Obj1,Obj2]).
hdrug_command(compare_dt,        compare_dt(1,2),[]).

analyse_differences(Obj1,Obj2) :-
    compare_dt(Obj1,Obj2,Result),
    (	Result == identical
    ->	compare_score(long,Obj1,Obj2,Result2),
	(   Result2 == identical
	->  compare_deriv(long,Obj1,Obj2,Result3),
	    (   Result3 == identical
	    ->  analyse_differences_long(Obj1,Obj2)
	    ;   true
	    )
	;   true
	)
    ;	true
    ).

analyse_differences_long(Obj1,Obj2) :-
    object(Obj1,Term10), copy_term(Term10,Term1,Cons1),
    object(Obj2,Term20), copy_term(Term20,Term2,Cons2),
    prettyvars(Term1-Cons1),
    prettyvars(Term2-Cons2),
    display_differences(Term1-Cons1,Term2-Cons2).

analyse_differences_short(Obj1,Obj2) :-
    compare_dt(Obj1,Obj2,identical),
    compare_score(short,Obj1,Obj2,_),
    compare_deriv(long,Obj1,Obj2,_).

analyse_differences :-
    format(user_error,
          "comparing objects until first identical dt found...~n",[]),
    object(Obj1,_),
    format(user_error,"~w ",[Obj1]),
    object(Obj2,_),
    Obj2 > Obj1,
    analyse_differences_short(Obj1,Obj2),
    !.
analyse_differences :-
    format(user_error,"~ncompared all objects.~n",[]).

compare_dt(Obj1,Obj2,Result) :-
    object(Obj1,o(Term1,_,_)),
    object(Obj2,o(Term2,_,_)),
    result_to_dt(Term1,Dt1),
    result_to_dt(Term2,Dt2),
    (	Dt1 == Dt2
    ->	format(user_error,
	       "~n*** ~w and ~w have identical dt ***~n",
	       [Obj1,Obj2]),
	Result=identical
    ;   simplify_dt(Dt1,DtSimple),
        simplify_dt(Dt2,DtSimple)
    ->  Result=identical,
        format(user_error,
	       "~n*** ~w and ~w have similar dt ***~n",
	       [Obj1,Obj2])

    ;	Result=different
    ).

%% ignore POS and ROOT
simplify_dt(tree(Label0,Extra,Ds0),tree(Label,Extra,Ds)) :-
    simplify_dt_label(Label0,Label),
    simplify_dt_ds(Ds0,Ds).

simplify_dt_ds([],[]).
simplify_dt_ds([H0|T0],[H|T]) :-
    simplify_dt(H0,H),
    simplify_dt_ds(T0,T).

simplify_dt_label(r(Rel,Node0),r(Rel,Node)) :-
    simplify_dt_label(Node0,Node).
simplify_dt_label(i(Ix),i(Ix)).
simplify_dt_label(p(Cat),p(Cat)).
simplify_dt_label(i(Ix,Node0),i(Ix,Node)) :-
    simplify_dt_label(Node0,Node).
simplify_dt_label(l(_,_,_/Positions),Positions).

compare_dt(Obj1,Obj2) :-
    object(Obj1,o(Term1,_,_)),
    object(Obj2,o(Term2,_,_)),
    result_to_dt(Term1,Dt1),
    result_to_dt(Term2,Dt2),
    (	Dt1 == Dt2
    ->	format(user_error,
	       "~n*** ~w and ~w have identical dt ***~n",
	       [Obj1,Obj2])
    ;	display_differences(Dt1,Dt2)
    ).

compare_deriv(Long,Obj1,Obj2,Result) :-
    object(Obj1,o(Term1,_,_)),
    object(Obj2,o(Term2,_,_)),
    hdrug_show:change_thing_tree(Term1,deriv0,Tree1),
    hdrug_show:change_thing_tree(Term2,deriv0,Tree2),
    prettyvars(Tree1),
    prettyvars(Tree2),
    (	Tree1 == Tree2
    ->	format(user_error,
	       "*** ~w and ~w have identical derivation-tree ***~n",
	       [Obj1,Obj2]),
	Result=identical
    ;	format(user_error,
	       "    ~w and ~w have different derivation-tree:~n",
	       [Obj1,Obj2]),
	(   Long == long
        ->  display_differences(Tree1,Tree2)
        ;   true
        ),
	Result=different
    ).

display_differences(Dt1,Dt2):-
    (	terms:variant(Dt1,Dt2)
    ->	true
    ;	atomic(Dt1)
    ->	format(user_error,"~w \\= ~w~n",[Dt1,Dt2])
    ;   var(Dt1), nonvar(Dt2)
    ->  format(user_error,"~w \\= ~w~n",[Dt1,Dt2])
    ;   nonvar(Dt1), var(Dt2)
    ->  format(user_error,"~w \\= ~w~n",[Dt1,Dt2])
    ;   var(Dt1), var(Dt2)
    ->  true
    ;	functor(Dt1,F,A),
	functor(Dt2,F2,A2),
	(   F/A == F2/A2
	->  display_differences(A,Dt1,Dt2)
	;   format(user_error,"~w \\= ~w~n",[F/A,F2/A2])
	)
    ).

display_differences(Arity,Dt1,Dt2):-
    (	Arity < 1
    ->	true
    ;	arg(Arity,Dt1,Arg1),
	arg(Arity,Dt2,Arg2),
	display_differences(Arg1,Arg2),
	Arity1 is Arity-1,
	display_differences(Arity1,Dt1,Dt2)
    ).

%% compare_score(Obj1,Obj2)
%% compare the differences in penalties assigned to Obj1 and Obj2, and
%% display result to user_error
compare_score(Obj1,Obj2) :-
    compare_score(long,Obj1,Obj2,_).

all_compare_score(Obj1,Obj2) :-
    hdrug_util:hdrug_flag(display_zero_weight_penalties,Old,on),
    call_cleanup(compare_score(Obj1,Obj2),
                 hdrug_util:set_flag(display_zero_weight_penalties,Old)
		).


compare_score(Long,Obj1,Obj2,R) :-
    object(Obj1,o(Cat1,_,_)),
    object(Obj2,o(Cat2,_,_)),
    alpino_data:result_term(p(Tot1,His1),_,_,_,_,Cat1),
    alpino_data:result_term(p(Tot2,His2),_,_,_,_,Cat2),
    (	His1 == His2
    ->	format(user_error,"*** ~w and ~w have identical scores ***~n",
	       [Obj1,Obj2]),
	R=identical
    ;	(   Long==long
        ->  select_all(His1,His1Left0,His2,His2Left0),
            sort_not_uniq(His1Left0,His1Left),
            sort_not_uniq(His2Left0,His2Left),
            format(user_error,"~npenalties peculiar to ~w (~w):~n",
                   [Obj1,Tot1]),
            display_penalties(His1Left),
            format(user_error,"~npenalties peculiar to ~w (~w):~n",
                   [Obj2,Tot2]),
            display_penalties(His2Left)
        ;   true
        ),
	R=different
    ).

display_penalties(Pens0) :-
    add_weights(Pens0,Pens1),
    keysort(Pens1,Pens),
    display_weighted_penalties(Pens).

add_weights([],[]).
add_weights([H|T0],[W1-W2-Count-Feature|T]) :-
    parse_feature(H,Feature,Count),
    hdrug_flag(parse_or_generate,ParseOrGen),
    (  (  Feature=weight(W1)
       -> true
       ;  ParseOrGen = generate
       -> alpino_fluency_weights:feature_weight(Feature,W1)
       ;  alpino_disambiguation_weights:feature_weight(Feature,W1)
       )
    -> true
    ;  W1 = 0
    ),
    (  ParseOrGen = parse
    -> W2=0
    ;  (  alpino_fluency_maxent:additional_weight(Feature,W2)
       -> true
       ;  W2=0
       )
    ),
    add_weights(T0,T).

:- initialize_flag(display_zero_weight_penalties,off).

display_weighted_penalties([]).
display_weighted_penalties([W1-W2-Count-Feature|T]) :-
    hdrug_flag(display_zero_weight_penalties,OnOff),
    (   W1 == 0, OnOff == off
    ->  true
    ;   W1 == 0, OnOff == on
    ->  format(user_error,"~p * ~p~n",[Count,Feature])
    ;   format(user_error,"~p * ~p ~p~n",[Count,Feature,W1])
    ),
    (   W2 == 0
    ->  true
    ;   format(user_error,"~p * ~p ~p ***ADDITIONAL***~n",[Count,Feature,W2])
    ),
    display_weighted_penalties(T).


/*
display_penalties([]).
display_penalties([H|T]) :-
    parse_feature(H,Feature,Count),
    hdrug_flag(parse_or_generate,ParseOrGen),
    (  (  Feature=weight(W1)
       -> true
       ;  ParseOrGen = generate
       -> alpino_fluency_weights:feature_weight(Feature,W1)
       ;  alpino_disambiguation_weights:feature_weight(Feature,W1)
       )
    -> format(user_error,"~p * ~p ~p~n",[Count,Feature,W1])
    ;  format(user_error,"~p * ~p~n",[Count,Feature])
    ),
    (  ParseOrGen = parse
    -> (  alpino_penalties:additional_weight(Feature,W2)
       -> format(user_error,"~p * ~p ~p ***ADDITIONAL***~n",[Count,Feature,W2])
       ;  true
       )
    ;  true
    ),
    display_penalties(T).
*/

parse_feature(Feature-Count,F,C) :-
    !,
    Feature=F,
    Count=C.
parse_feature(weight(W),F,C) :-
    !,
    F=weight(W),
    C=1.
parse_feature(Feature,Feature,1).

select_all([],[],His2,His2).
select_all([H|T],Left1,His2,His2Left) :-
    (	select(H,His2,His2Left0)
    ->	Left1T=Left1
    ;	Left1=[H|Left1T],
	His2Left0=His2
    ),
    select_all(T,Left1T,His2Left0,His2Left).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% FEATURES %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public
    display_penalties_of_obj/1,
    all_display_penalties_of_obj/1,
    format_features_of_obj/1.

hdrug_command(features,display_penalties_of_obj(Obj),[Obj]).
hdrug_command(features,display_penalties_of_obj(1),[]).
hdrug_command(penalties,display_penalties_of_obj(Obj),[Obj]).
hdrug_command(penalties,display_penalties_of_obj(1),[]).

hdrug_command(all_features,all_display_penalties_of_obj(Obj),[Obj]).
hdrug_command(all_features,all_display_penalties_of_obj(1),[]).
hdrug_command(all_penalties,all_display_penalties_of_obj(Obj),[Obj]).
hdrug_command(all_penalties,all_display_penalties_of_obj(1),[]).

all_display_penalties_of_obj(N) :-
    hdrug_util:hdrug_flag(display_zero_weight_penalties,Old,on),
    call_cleanup(display_penalties_of_obj(N),
                 hdrug_util:set_flag(display_zero_weight_penalties,Old)
		).

display_penalties_of_obj(N) :-
    hdrug_flag(current_ref,Key),
    object(N,o(Cat,_,_)),
    display_penalties_of_result(Cat,Key).

display_penalties_of_result(Result,_) :-
    alpino_data:result_term(p(_,His0),_,_,_,_,Result),
    sort_not_uniq(His0,His),
    display_penalties(His).

format_features_of_obj(N) :-
    hdrug_flag(current_ref,Key),
    object(N,o(Cat,_,_)),
    format_features_of_result(Cat,Key).

format_features_of_result(Result,_) :-
    alpino_data:result_term(p(_,His),_,_,_,_,Result),
    format_counted_features(His).

:- public check_lexical_analysis/0.

check_lexical_analysis :-
    repeat,
    read_line(Codes),
    codes2pl(Codes),
    !.

codes2pl(end_of_file).
codes2pl([C0|Cs]) :-
    split_string([C0|Cs]," ",Fields0),
    atom_codes_list(Fields0,Fields),
    check_lexical_analysis(Fields,[C0|Cs]),
    fail.

atom_codes_list([],[]).
atom_codes_list([H0|T0],[H|T]) :-
    atom_codes(H,H0),
    atom_codes_list(T0,T).

check_lexical_analysis(Words,Codes) :-
    (  lex_lexicon(_Tag,_Stem,Words,[],_His)
    -> format("+++ ~s~n",[Codes])
    ;  format("--- ~s~n",[Codes])
    ).


lex_lexicon(Tag,Stem,Words0,Words,His):-
    set_flag(expand_subcat,off),
    set_flag(check_word_form,off),
    call_cleanup(alpino_lex:lexicon(Tag,Stem,Words0,Words,His),
                 ( set_flag(expand_subcat,on),
                   set_flag(check_word_form,on))).

shorten_label(T0,T) :-
    (   var(T0)
    ->  T0 = T
    ;   DT => dt,
        T0 = DT
    ->  T => dt,
        shorten_dt(T0,T)
    ;   functor(T0,F,A),
        functor(T,F,A),
        shorten_label(T0,T,A)
    ).

shorten_label(T0,T,Ar) :-
    (   Ar < 1
    ->  true
    ;   arg(Ar,T0,ArgA),
        arg(Ar,T ,ArgB),
        shorten_label(ArgA,ArgB),
        Ar1 is Ar-1,
        shorten_label(T0,T,Ar1)
    ).

shorten_dt(Dt0,Dt) :-
    (   var(Dt0)
    ->  Dt = 'VAR!'(Dt0)
        %Dt0 = Dt
    ;   Dt0 == []
    ->  Dt = '$VAR'('_')   % will not be printed
    ;   Dt0 => dt,
        Dt => dt,
        alpino_data:dt_features(L0,L),
        shorten_dt(L0,Dt0,Dt1),
        shorten_dt_list(L,Dt1,Dt)
    ).

shorten_dt_list([],Dt,Dt).
shorten_dt_list([Att|Atts],Dt0,Dt) :-
    unify_except(Dt0,Dt1,Att),
    Dt0:Att <=> X0,
    Dt1:Att <=> X,
    shorten_dt_list_el(X0,X),
    shorten_dt_list(Atts,Dt1,Dt).

shorten_dt_list_el(Dt0,Dt) :-
    (   var(Dt0)
    ->  % Dt0 = Dt
        Dt = 'VAR!'(Dt0)
    ;   Dt0 == []
    ->  Dt = '$VAR'('_')   % will not be printed
    ;   Dt0 = [H0|T0],
        Dt = [H|T],
        shorten_dt(H0,H),
        shorten_dt_list_el1(T0,T)
    ).

shorten_dt_list_el1(Dt0,Dt) :-
    (   var(Dt0)
    ->  Dt0 = Dt
        %% Dt = 'VAR!'(Dt0)
    ;   Dt0 == []
    ->  Dt = []  % once we have a list, tails must be []
                 % to display [a,b,c] rather than [a,b,c|_]
    ;   Dt0 = [H0|T0],
        Dt = [H|T],
        shorten_dt(H0,H),
        shorten_dt_list_el1(T0,T)
    ).

shorten_dt([],Dt,Dt).
shorten_dt([Att|Atts],Dt0,Dt) :-
    unify_except(Dt0,Dt1,Att),
    Dt0:Att <=> X0,
    Dt1:Att <=> X,
    shorten_dt(X0,X),
    shorten_dt(Atts,Dt1,Dt).


hdrug_command(obj,show_obj(Type,Output,Things),L0) :-
    hdrug_cmdint:show_command(Type0,Output,L0,L1),
    (   Type0 == default -> Type = fs ; Type0 = Type ),
    hdrug_cmdint:obj_spec(Things,L1,[]).

:- public show_obj/3.
show_obj(Type,Output,Things) :-
    findall(Clause,show_an_obj(Things,Clause),Clauses),
    hdrug_show:show(Type,Output,Clauses).

show_an_obj(List,clause(Cat,Conditions)) :-
    member(object(N,o(Goal,_,_)),List),
    copy_term(Goal,Goal1),
    object(N,o(Goal1,_,_)),
    copy_term(Goal1,Goal,Conditions),
    alpino_data:result_term(_,_,Cat,_,_,Goal).

hdrug_command(lf,show_lf(Ns,Type,Output),Ns0) :-
    hdrug_cmdint:show_command(Type0,Output,Ns0,Ns),
    (   Type0 == default -> Type = tree(adt) ; Type0 = Type ).

:- public show_lf/3.
show_lf([],Type,Output) :-
    hdrug_flag(current_ref,Ref),
    show_lf([Ref],Type,Output).
show_lf([Ns0|Ns],Type,Output) :-
    set_flag(current_ref,Ns0),
    findall(value(Adt),(member(N,[Ns0|Ns]),
                 lf(N,Adt)
                ), Values),
    hdrug_show:show(Type,Output,Values).


%% for Kostadin

:- public paradigm/1.
hdrug_command(paradigm,paradigm(Form),[Form]).

paradigm(Form,Sg1,Sg3,Pl,PastSg,PastPl,Psp) :-
    lex_lexicon(verb(_,_,Sc),Root,[Form],[],_),
    functor(Sc,Fun,_),\+ atom_concat(part_,_,Fun),
    (   alpino_genlex:dict_entry(Root,verb(_,sg1,Sc),Sg1)
    ;   alpino_genlex:dict_entry(Root,verb(_,sg,Sc),Sg1)
    ),
    (   alpino_genlex:dict_entry(Root,verb(_,sg3,Sc),Sg3)
    ;   alpino_genlex:dict_entry(Root,verb(_,sg,Sc),Sg3)
    ),
    alpino_genlex:dict_entry(Root,verb(_,pl,Sc),Pl),
    alpino_genlex:dict_entry(Root,verb(_,past(sg),Sc),PastSg),
    alpino_genlex:dict_entry(Root,verb(_,past(pl),Sc),PastPl),
    alpino_genlex:dict_entry(Root,verb(_,psp,Sc),Psp),
    !.

paradigm(Form) :-
    paradigm(Form,Sg1,Sg3,Pl,PastSg,PastPl,Psp),
    format("paradigm|~p|~p|~p|~p|~p|~p|~p~n",
           [Form,Sg1,Sg3,Pl,PastSg,PastPl,Psp]).

simplify_cat(Cat,Term) :-
    nonvar(Cat),
    functor(Cat,F,_),
    if_defined(Cat:sc,Sc0,[]),
    if_defined(Cat:slash,Sl0,[]),
    if_defined(Cat:e_deps,Ed0,[]),
    if_defined(Cat:nform,Norm,NormNform), NormNform => norm,
    if_defined(Cat:case,Case,NormCase),   NormCase => acc,
    if_defined(Cat:ctype,Ctype,NormCtype),NormCtype => c_dat,
    simplify_l(Sc0,Sc1),
    simplify_n(Sc1,Sc,sc),
    simplify_l(Sl0,Sl1),
    simplify_n(Sl1,Sl,sl),
    simplify_l(Ed0,Ed1),
    simplify_n(Ed1,Ed,ed),
    append(Sc,Sl,Args0),
    append(Args0,Ed,Args1),
    add_nform(Norm,Args1,Args2),
    add_case(Case,Args2,Args3),
    add_ctype(Ctype,Args3,Args),
    !, % only single solution!
    Term =.. [F|Args].
simplify_cat(Cat,Cat).

simplify_l(Var,L) :-
    var(Var),!,
    Var=L.
simplify_l([],[]).
simplify_l([H|T],[NH|NT]) :-
    simplify_cat(H,NH),
    simplify_l(T,NT).

simplify_n(Var,L,Att) :-
    var(Var),
    !,
    L=[Att=Var].
simplify_n([],[],_).
simplify_n([H|T],L,Att) :-
    L=[Att=[H|T]].

add_nform(Nform,Args,Args) :-
    Nform => ~er & ~refl & ~het_nform & ~none & ~cleft_het.
add_nform(Nform,Args,[er|Args]) :-
    Nform => er.
add_nform(Nform,Args,[refl|Args]) :-
    Nform => refl.
add_nform(Nform,Args,[het|Args]) :-
    Nform => het_nform.
add_nform(Nform,Args,[het|Args]) :-
    Nform => cleft_het.
add_nform(Nform,Args,[none|Args]) :-
    Nform => none.

add_case(Case,Args,Args) :-
    Case => ~nom & ~dat.
add_case(Case,Args,[dat|Args]) :-
    Case => dat.
add_case(Case,Args,[nom|Args]) :-
    Case => nom.

add_ctype(Ctype,Args,Args) :-
    Ctype => ~c_naar & ~c_dip & ~c_none.
add_ctype(Ctype,Args,[dip|Args]) :-
    Ctype => c_dip.
add_ctype(Ctype,Args,[naar|Args]) :-
    Ctype => c_naar.
add_ctype(Ctype,Args,[none|Args]) :-
    Ctype => c_none.

%%%
:- public check_suites/1, check_suite/1.
check_suites([]).
check_suites([H|T]) :-
    check_suite(H),
    check_suites(T).

check_suite(H) :-
    set_flag(suite,H),
    set_flag(treebank,undefined),
    load_suite,
    alpino_treebank:check_suite_and_treebank_are_consistent.

:- multifile user:portray/1.

user:portray(X/mod) :-
    format("~w/~w",[X,mod]).
