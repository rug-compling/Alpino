%           -*-Mode: prolog;-*-

:- multifile unknown_predicate_handler/3.

table_item:unknown_predicate_handler(_,fail).
table_goal:unknown_predicate_handler(_,fail).
table_goal_l:unknown_predicate_handler(_,fail).
table_goal_r:unknown_predicate_handler(_,fail).

% load generic libraries
%% GvN: if module is already known, only import predicates, but don't
%% check if a newer version exists. This is crucial for runtime systems
%% in which the library is not even around anymore!
:- use_module(charsio,library(charsio),all). % not used?
:- use_module(lists,library(lists),all).
:- use_module(terms,library(terms),all).

:- use_module(hc_compile).      % grammar compiler for hc hc_mixtus
:- use_module(lc_compile).      % grammar compiler for lc lc_mixtus
:- use_module(hc).
:- use_module(lc).

:- use_module('anlt2.suite').

:- initialize_flag(parser,lc).
:- set_flag(parser(hc),on).
:- set_flag(parser(lc),on).
:- initialize_flag(head,left).  % left-most daughter == head daughter.
:- initialize_flag(top_features,grammar).

top(grammar,Obj) :-
	result_cat(Obj,Cat),
	grammar:top_category(Cat).

:- set_flag(max_objects,10000).
:- set_flag(max,100000000).
:- set_flag(object_exists_check,off).
:- set_flag(object_saving,semi).  % for batch work, use `off' value.
:- set_flag(application_name,'Alvey').
:- set_flag(useful_try_check,off).

dump_grammar_g(File) :-
	tell(File),
	(  grammar:grammar_rule(A,B,C),
	   format("~q.~n",[grammar_rule(A,B,C)]),
	   fail
	;  grammar:lex(A,B,C),
	   format("~q.~n",[lex(A,B,C)]),
	   fail
	;  grammar:sem_filter(A,B),
	   format("~q.~n",[sem_filter(A,B)]),
	   fail
	;  grammar:top_category(A),
	   format("~q.~n",[top_category(A)]),
	   fail
	;  grammar:head(A,B),
	   format("~q.~n",[head(A,B)]),
	   fail
	;  true
	),
	told.

grammar:top_category(_,Y) :-
	grammar:top_category(Y).


:- multifile graphic_path/3, graphic_label/3, graphic_daughter/4.

graphic_path(d,X,Tree):-
	result_tree(X,Tree).

graphic_label(d,tree(L0,_,_),L) :- 
	(  functor(L0,_,2),
           arg(1,L0,A1),
	   arg(2,L0,A2),
	   L = A2/A1
	-> true
	;  L0=L
	).
graphic_label(d,L,L).

graphic_daughter(d,Nth,tree(_,_,Ds),D) :-
	lists:nth(Nth,Ds,D).

/*
graphic_path(syn,X,Tree):-
        user:result_cat(X,Sign),
        (  var(Sign)
        -> true
        ;  arg(1,Sign,Tree)
        ).

graphic_label(syn,Tree,R) :-
        arg(1,Tree,rule(R)),!.
graphic_label(syn,Tree,R) :-
        arg(1,Tree,R).

graphic_daughter(syn,1,Tree,D) :-
        arg(2,Tree,D),nonvar(D).
graphic_daughter(syn,2,Tree,D) :-
        arg(3,Tree,D),nonvar(D).
graphic_daughter(syn,3,Tree,D) :-
        arg(4,Tree,D),nonvar(D).
graphic_daughter(syn,4,Tree,D) :-
        arg(5,Tree,D),nonvar(D).         
*/

grammar:top_category(_,Y) :-
        grammar:top_category(Y).

% lex_syn(P0,P,SynCat)  Ref = '$ref'(Refa,Refb)
% lex_total(Refa,Refb,P0,P,TotalCat,Id)
lexical_analysis(String) :-
	statistics(runtime,[Time0,_]),
	retractall(lex:lex(_,_,_,_)),

	lexical_analysis(String,1),

	hdrug_flag(debug,Val),
	(  Val > 0
	-> report_count_edges(lex:lex(_,_,_,_))
	;  true
	),
	statistics(runtime,[Time1,_]),
	Time is Time1-Time0,
	format(user_error,"lexical lookup: ~w msec~n",[Time]).


lexical_analysis([],_).
lexical_analysis([Word|Tail],P0) :-
	P1 is P0+1,
	(  %% functor(Id,Word,2),
	   %% lex_in:lex(Id,Cat,SemCat,Word,RestWords),
	   lex_in:syn_lex(Word,RestWords,Cat,Ids),
	   cover_rest(RestWords,Tail,P1,P),
	   assertz(lex:lex(P0,P,Cat,Ids)),
	   fail
	;  true
	),
	lexical_analysis(Tail,P1).

cover_rest([],_,P,P).
cover_rest([W|T0],[W|T],P0,P) :-
	P1 is P0+1,
	cover_rest(T0,T,P1,P).


compile_lexicon :-
	format(user_error,"Indexing lexical entries..~n",[]),
	(  hdrug_flag(lexid(W)),
	   hdrug_flag(lexid(W),_,0),
	   fail
	;  true
	),
	(  lex_in:current_predicate(_,X),
	   functor(X,F,A),
	   lex_in:abolish(F/A),
	   fail
	;  true
	),
	(  grammar:sem_filter(Vars,Cat),
	   replace_each(Vars,Cat,Syn,SemCons,SemCat),
	   assertz(lex_in:separate(Cat,Syn,SemCons,SemCat)),
	   fail
	;  true
	),
	(  grammar:lex(Id0,M,Ws),
	   check_id(Id0,Ws,Id,W,WsRest),
	   (  try_separate(M,Syn,_Cons,Sem)
	   -> assert_lexicon(Id,Syn,Sem,W,WsRest)
	   ;  format(user_error,"Error in lexical entry ~w ~w~n",[Id,W])
	   ),
	   fail
	;  true
	),
	report_count_edges_pred(lex_in:syn_lex/4),
	report_count_edges_pred(lex_in:total_lex/4),
	format(user_error,"Indexing lexical entries done~n",[]).

% syn_lex(W,WsRest,Syn,Ids)
% total_lex(Id,Cat,W,WsRest)
assert_lexicon(Id,Syn,Sem,W,WsRest) :-
	assert_syn_lex(W,WsRest,Syn,Id),
	Syn=Sem,
	assertz(lex_in:total_lex(Id,Syn,W,WsRest)).

assert_syn_lex(W,WsRest,Syn,Id) :-
	(  copy_term(Syn,Copy),
	   lex_in:clause(syn_lex(W,WsRest,Copy,Ids),_,Ref),
	   lex_in:clause(syn_lex(W,WsRest,General,Ids),_,Ref),
	   terms:subsumes_chk(General,Syn)
	-> erase(Ref),
	   assertz(lex_in:syn_lex(W,WsRest,General,[Id|Ids]))
	;  findall(Id0s,erase_more_specific_lex(W,WsRest,Syn,Id0s),Id0ss),
	   append_all(Id0ss,Ids),
	   assertz(lex_in:syn_lex(W,WsRest,Syn,[Id|Ids]))
	).

erase_more_specific_lex(W,WsRest,Syn,Ids):-
	copy_term(Syn,Copy),
	lex_in:clause(syn_lex(W,WsRest,Copy,Ids),_,Ref),
	lex_in:clause(syn_lex(W,WsRest,Specific,Ids),_,Ref),
	terms:subsumes_chk(Syn,Specific),
	erase(Ref).

append_all([],[]).
append_all([H|T],L) :-
	append_all(T,H,L).

append_all([],H,H).
append_all([H|T],H0,L) :-
	lists:append(H0,H,H1),
	append_all(T,H1,L).

   
% id must always be W(X,Y) where W is the first word of Ws
check_id(Id0,Ws,Id,W,WsRest) :-
	(  Ws = [W|WsRest]
	-> true
	;  atomic(Ws),
	   W = Ws,
	   WsRest = []
	),
	(  functor(Id0,W,2)
	-> Id = Id0
	;  functor(Id,W,2),
	   hdrug_flag(lexid(W),No0),
	   (  No0 == undefined
	   -> Reading = 1
	   ;  Reading is No0 + 1
	   ),
	   hdrug_flag(lexid(W),_,Reading),
	   arg(1,Id,Reading),
	   arg(2,Id,Id0)
	).

try_separate(Cat,Syn,SemCons,Sem) :-
	lex_in:separate(Cat,Syn,SemCons,Sem).
try_separate(Cat,Syn,SemCons,Sem) :-
	\+ lex_in:separate(Cat,Syn,SemCons,Sem),
	Cat=Syn,
	SemCons=[],
	Cat=Sem.

replace_each([],C,C,[],_).
replace_each([H|T],C0,C,[PlaceHolder=H|Cons],SemCat) :-
	replace(H,PlaceHolder,C0,C1,no,YesNo,SemCat),
	(  YesNo = no
	-> format("Error in sem_filter/2: couldn't replace ~q in ~q~n",[H,C0]),
	   fail
	;  replace_each(T,C1,C,Cons,SemCat)
	).

%% replace(TermOld,TermNew,In,Out)
replace(T0,T,I0,I,_B,yes,Sem) :-
	T0 == I0,
	!,
	Sem=T0,
	T = I.
replace(_T0,_T,I0,I,B,B,_) :-
	var(I0),
	!,
	I0=I.
replace(T0,T,I0,I,B0,B,SC) :-
	functor(I0,F,A),
	functor(I,F,A),
	functor(SC,F,A),
	replace(A,T0,T,I0,I,B0,B,SC).

replace(0,_,_,_,_,B,B,_) :-
	!.
replace(Arg,T0,T,I0,I,B0,B,SC) :-
	arg(Arg,I0,A0),
	arg(Arg,I ,A ),
	arg(Arg,SC,Asc),
	replace(T0,T,A0,A,B0,B1,Asc),
	Arg2 is Arg-1,
	replace(Arg2,T0,T,I0,I,B1,B,SC).


:- use_module(system,library(system),all).

load_grammar :-
    hdrug_flag(load_grammar_mode,Flag),
    (	Flag=interpret
    ->  compile_grammar
    ;	(   hdrug_flag(parser(Parser),on),
	    load_grammar_dump(Parser),
	    fail
	;   true
	)
    ).

compile_grammar :-
	hdrug_flag(grammar,File),
	(  File=undefined
	-> format(user_error,
           "note: hdrug_flag(grammar,undefined) hence not loading a grammar~n",[])
	;  load_grammar_file(File)
	).

compile_grammar_file(File) :-
	load_grammar_file(File).

reconsult_grammar :-
	hdrug_flag(grammar,File),
	load_grammar_file(File).

reconsult_grammar_file(File) :-
	load_grammar_file(File).

load_grammar_file(File) :-
	statistics(runtime,[T0,_]),
	grammar:reconsult(File),
	compile_lexicon,
	notify_parsers(File),
	notify_hdrug,
	statistics(runtime,[T,_]),
	Time is T-T0,
	format(user_error,"Grammar compilation done (~w msec)~n",[Time]).

notify_hdrug :-
    if_gui(( send_lexs, send_rules )).  % pretty.pl

%% each parser optionally defines the predicate Module:compile_grammar to
%% enable parser-specific compilations (e.g. the left-corner parser 
%% compiles a left-corner table)
notify_parsers(File) :-
	reset_compiled_flags,
	prolog_flag(unknown,Old,fail),
	(  hdrug_flag(parser(P),Val),
	    notify_parser(Val,P,File),
	   fail
	;  true
	),
	prolog_flag(unknown,_,Old).

notify_parser(on,P,File) :-
	ensure_grammar_compiled_for_parser(P,File).

notify_parser(off,_,_). 

reset_compiled_flags :-
	% a new grammar is loaded, so no parser_compiler is up to date
	% yet..
	(  hdrug_flag(parser(P)),
	   hdrug_flag(parser_compiled(P),_,no),
	   fail
	;  true
	).

load_grammar_dump(lc) :-
    prolog_flag(redefine_warnings,Old,off),
    call_cleanup(use_module(anlt2_lc),
		 prolog_flag(redefine_warnings,_,Old)),
    set_flag(parser_compiled(lc),yes).
load_grammar_dump(hc) :-
    hdrug_flag(head,Head),
    prolog_flag(redefine_warnings,Old,off),
    call_cleanup(load_grammar_dump_hc(Head),
		 prolog_flag(redefine_warnings,_,Old)),    
    set_flag(parser_compiled(hc),yes).

load_grammar_dump_hc(Head):-
    (	Head==left
    ->	use_module(anlt2_hc_lc)
    ;   use_module(anlt2_hc)
    ).

ensure_grammar_compiled_for_parser(Mode,File) :-
	hdrug_flag(parser_compiled(Mode),Val),
	ensure_grammar_compiled_for_parser(Val,Mode,File).

ensure_grammar_compiled_for_parser(yes,_Mode,_File).
ensure_grammar_compiled_for_parser(no,Mode,File) :-
	format(user_error,"compiling grammar for parser ~w~n",[Mode]),
	Mode:compile_grammar(File),
	format(user_error,"compiling grammar for parser ~w done~n",[Mode]),
	hdrug_flag(parser_compiled(Mode),_,yes).   % it is up to date now

ensure_grammar_compiled_for_parser(undefined,Mode,File) :-
        format(user_error,"compiling grammar for parser ~w~n",[Mode]),
        Mode:compile_grammar(File),
        format(user_error,"compiling grammar for parser ~w done~n",[Mode]),
        hdrug_flag(parser_compiled(Mode),_,yes).   % it is up to date now
    

dump_grammar(OutFile):-
	tell(OutFile),
	hdrug_flag(parser,Parser),
	Parser:dump_grammar,
	dump_predicate(grammar:top_category(_)),
	dump_predicate(lex_in:syn_lex(_,_,_,_)),
	dump_predicate(lex_in:total_lex(_,_,_,_)),
	dump_predicate(grammar:sem_filter(_,_)),
	told.

:- meta_predicate dump_predicate(:,?,?), dump_predicate(:).

% k is for canonical, so we are not dependent on the operator declarations
% that might be in effect. I've learned this the hard way :-(
dump_predicate(Call) :-
	dump_predicate(Call,"~k.~n",[Call]).

dump_predicate(Call,Fmt,Args) :-
	(  call(Call),
	   format(Fmt,Args),
	   fail
	;  nl
	).

%% this is called whenever we start parsing. Time needed is not
%% included in parse times..
start_hook(parse,Module,_,_) :-
	hdrug_flag(grammar,Gram),
	ensure_grammar_compiled_for_parser(Module,Gram).

%% this is called whenever we start parsing. Time needed will be
%% included in total parse times..
start_hook0(parse,_,o(_,String,_),_) :-
	lexical_analysis(String).


user_max(_Length,Msec):-
	hdrug_flag(max,Msec).
	   
:- initialize_flag(enable_result_hook,off).
result_hook(parse,_,o(Obj,_,_),_) :-
    hdrug_flag(enable_result_hook,OnOff),
    (	OnOff == off
    ->	true
    ;	show(tree(d),clig,[value(Obj)])
    ).

end_hook(parse,_,_,_) :-
    show_object_no(1,tree(d),clig).

show_object_default2(No) :-
    show_object_no(No,tree(d),clig).

a_more_specific(Module:Item) :-
	a_more_specific(Module:Item,_).

a_more_specific(Module:Item,Ref) :-
	copy_term(Item,Copy),
	Module:clause(Copy,_,Ref),     % quick check for unification
	Module:clause(Specific,_,Ref), % new copy for subsumption check
	terms:subsumes_chk(Item,Specific).

a_more_general(Module:Item) :-
	a_more_general(Module:Item,_).

a_more_general(Module:Item,Ref):-
	copy_term(Item,Copy),
	Module:clause(Copy,_,Ref),
	Module:clause(General,_,Ref),
	terms:subsumes_chk(General,Item).


%% this version doesn't keep track of more specific items at all...
%% can give rise to spurious ambiguities..
add_item(Module:Item,Ref) :-
	(  a_more_general(Module:Item,Ref)
	-> true
	;  Module:assertz(Item,Ref)
	).


up_to_date(_Module,A,B,A,B).

result_cat(p(A,_B),A).
result_tree(p(_A,B),B).

treat_words([]).
treat_words([H|T]) :-
	hdrug_flag(enable_result_hook,_,on),
	parse([H|T]).

:- multifile option/3.

option(suite) -->
    [File],
    { use_module(File) }.

option(load) -->
    [File],
    { use_module(File) }.


option(parse,Words,[]) :-
    hdrug_initialization,
    treat_words(Words),
    halt.

hdrug_initialization :-
    load_grammar.
