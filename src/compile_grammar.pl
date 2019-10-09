:- module(alpino_compile_grammar,
	  [ load_grammar/0,
	    load_lexicon/0,
	    ensure_grammar_compiled/0,
	    dump_grammar/0
	  ]).

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- use_module(hdrug(hdrug_util)).
:- use_module(utils).

% every parser P should have a predicate P:compile_grammar(File).
% compiled predicates are asserted in P_in.
% This does the neccessary compilation stuff. The actual loading of the
% grammar, and the compilation of the lexicons and update files, is
% defined here.

:- initialize_flag(load_grammar_mode,compiled).

:- multifile user:help_flag/2.

user:help_flag(load_grammar_mode,
"You can use the existing grammar and lexicon in two ways:
compiled or interpret. In the first case, the
system will start up much quicker, and will parse faster. In
the interpreted mode, the system is able to visualize grammar
rules and lexical entries in various interesting ways. The
idea is that if you are working on the grammar and/or lexicon,
you want to use the interpreted version; whereas if you are
working on other aspects, or if you simply want to test the
system, you need the compiled mode. ").

%% :- use_module(hdrug_util,hdrug_util,all).

:- use_module(library(terms)).

load_lexicon :-
    hdrug_flag(lexicon,LFile),
    hdrug_flag(lex,LBaseFile),
    hdrug_flag(lex_conv,LexConv),
    (	LFile==undefined
    ->	inform_undefined_module(lexicon)
    ;	LFile==n
    ->	inform_undefined_module(lexicon)
    ;	LBaseFile==undefined
    ->	inform_undefined_module(lex)
    ;	LBaseFile==n
    ->	inform_undefined_module(lex)
    ;	compile(LBaseFile),
	interpret_lexicon_file(LexConv)
    ).

load_grammar :-
    hdrug_flag(grammar,GFile),
    (	GFile==undefined
    ->	inform_undefined_module(grammar)
    ;	GFile==n
    ->	inform_undefined_module(grammar)
    ;	hdrug_flag(load_grammar_mode,Mode),
        hdrug_flag(genrules,GenGrammar),
	(   Mode==interpret
	->  interpret_grammar_file(GFile),
            compile(GenGrammar)
	;   load_grammar_dump,
            ensure_loaded(GenGrammar)
	)
    ).

%% load File
%% load_lexicon_dump(File0) :-
%%    atom_concat(File0,'_conv',File),
%%    use_module(File).

%% load Compiler_lex where Compiler is the grammar compiler of the
%% current parser (hdrug_flag(parser))
load_grammar_dump :-
    use_module(alpino('Grammar/grammar_lc')),
    set_flag(grammar_compiled,yes).

interpret_lexicon_file(File) :-
    statistics(runtime,[T0,_]),
    set_flag(lexicon,File),
    (	File==undefined
    ->	inform_undefined_module(lexicon)
    ;	File==n
    ->	inform_undefined_module(lexicon)
    ;	alpino_lex_types:load_files(File,[compilation_mode(assert_all)])
    ),
    compile_user_clause(alpino_lex_types),
    statistics(runtime,[T,_]),
    Time is T-T0,
    debug_message(1,"Lexicon compilation done (~w msec)~n",[Time]).

%% interpret_grammar_file/1
%% * reconsults File
%% * create user_clauses
%% * lexicon
%% * warns parser-compiler to take action
%% * warns gui to take action
interpret_grammar_file(File):-
    statistics(runtime,[T0,_]),
    set_flag(grammar,File),
    alpino_grammar:load_files(File,[compilation_mode(assert_all)]),  %% for clause/2
    compile_user_clause(alpino_grammar),
    notify_compiler,
    notify_hdrug_grammar,  % cf start.pl
    statistics(runtime,[T,_]),
    Time is T-T0,
    debug_message(1,"Grammar compilation done (~w msec)~n",[Time]).

notify_compiler :-
    set_flag(grammar_compiled,no),
    hdrug_flag(load_grammar_mode,Mode),
    (	Mode == interpret             % normal case,
    ->	alpino_lc:compile_grammar     % parser-compiler compilation
    ;   load_grammar_dump       % assume it's in dump...
    ),
    set_flag(grammar_compiled,yes).  % we're up-to-date again

%% if parsing or generation is attempted, we first check that
%% compilation is up-to-date
ensure_grammar_compiled :-
    hdrug_flag(grammar_compiled,Val),
    ensure_grammar_compiled(Val).

ensure_grammar_compiled(yes).
ensure_grammar_compiled(no):-
    notify_compiler.
ensure_grammar_compiled(undefined) :-
    notify_compiler.

%% DUMP LEXICON / GRAMMAR
dump_grammar :-
    hdrug_flag(grammar,Grammar),
    set_flag(load_grammar_mode,interpret),
    interpret_grammar_file(Grammar),
    module_listing(alpino_lc_in,'grammar_lc.pl').

%% writes all *dynamic* predicates in Module to File0.pl
%% so you can make the system dump a predicate by declaring it
%% as dynamic
module_listing(Module,File) :-
    tell(File),
    call_cleanup((
      format(":- module(~k,[]).~n~n",[Module]),
      (   (   clause_predicate(Module,A0)
	  ;   A0 = alpino_grammar:user_clause(_,_)
	  ),
	  catch(Module:clause(A0,B0),_,fail),
	  copy_term(A0/B0,A/B,Res),
	  (   terms:cyclic_term((A,B))
	  ->  format(user_error,"error: cyclic term: ~@~n",
		     [write_term((A:-B),[max_depth(10)])]),
	      fail
	  ;   true
	  ),
	  add_constraints(Res,Body,B),
	  prettyvars((A:-Body)),
	  flatten_body(Body,FlatBody),
	  (   FlatBody == true
	  ->  format("~q .~n",[A])
	  ;   format("~q .~n",[(A:-FlatBody)])
	  ),
	  fail
      ;	  true
      )
		 ), told).

add_constraints([],B,B).
add_constraints([H|T],B0,B):-
    add_constraints(T,H,B0,B).

add_constraints([],H,B0,B):-
    (	B == true
    ->	B0 = H
    ;	B0 = (H,B)
    ).
add_constraints([H|T],F,(F,B0),B):-
    add_constraints(T,H,B0,B).

flatten_body(Body0,Body):-
    flatten_body(Body0,List,[]),
    body(List,Body).

flatten_body(Var,Body0,Body) :-
    var(Var),
    !,
    Body0=[Var|Body].
flatten_body(true,Body0,Body) :-
    !,
    Body0=Body.
flatten_body((H,T),Body0,Body) :-
    !,
    flatten_body(H,Body0,Body1),
    flatten_body(T,Body1,Body).
flatten_body(Goal,[Goal|Body],Body).

body([],true).
body([H|T],Goal):-
    body(T,H,Goal).

body([],Goal,Goal).
body([H|T],Goal,(Goal,Rest)):-
    body(T,H,Rest).


