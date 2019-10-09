:- set_flag(application_name,'Ale').

:- set_flag(grammar,ale('Hpsg/hpsg')).     %% default grammar, can be overridden by
                                    %% -grammar Grammar option (after -a).
:- set_flag(suite,ale('Hpsg/hpsg_suite')). %% default file with examples, can be
                                    %% overridden with -suite Suite option.
:- set_flag(decl,ale('Hpsg/hpsg_decl')).   %% default declarations file, can be
                                    %% overridden with -decl option


:- multifile help_flag/2.
help_flag(grammar,"File name which is the default grammar").
help_flag(suite,"File name which is the default file with example sentences").
help_flag(decl,
	  "File name which is the default file with grammar declarations").
help_flag(ale_portray,"For tracing, you can set this flag on such that typical Ale terms will be portrayed in feature structure notation. I don't really like it myself...").

help_flag(ale_interpreter,"Indicates verbosity of Ale interpreter.").
help_flag(ale_subsumption,"Optional edge subsumption checking. For completeness of parsing, one only needs to ensure that, for every pair of nodes in the chart, the most general feature structure spanning those nodes is stored in the chart. This can reduce the number of edges in many domains.").
help_flag(ale_adderrs,"Errors from adding descriptions can be suppressed or ignored").

help_flag(parser(parser),"If this flag is set then compiler produces code for parsing").
help_flag(generator(generator),"If this flag is set then compiler produces code for generation").


:- multifile option/3.
option(grammar) -->
    [GrammarFile],
    {  set_flag(grammar,GrammarFile)  }.

option(suite) -->
    [SuiteFile],
    {  set_flag(suite,SuiteFile)  }.

option(decl) -->
    [DeclFile],
    {  set_flag(decl,DeclFile)  }.

option(parse) -->
    { set_flag(parser,parser),
      set_flag(parser(parser),on) }.

option(generate) -->
    { set_flag(generator,generator),
      set_flag(generator(generator),on) }.

option(noparse) -->
    { set_flag(parser,undefined),
      set_flag(parser(parser),off) }.

option(nogenerate) -->
    { set_flag(generator,undefined),
      set_flag(generator(generator),off)
    }.

:- multifile usage_option/3.

usage_option(suite,"-suite Suite","use Suite as file with example sentences").
usage_option(grammar,"-grammar Grammar","use Grammar as grammar file").
usage_option(decl,"-decl Decl","use Decl as declarations file").
usage_option(parse,"-parse","allow compilation for parsing").
usage_option(noparse,"-noparse","disallow compilation for parsing").
usage_option(generate,"-generate","allow compilation for generation").
usage_option(nogenerate,"-nogenerate","disallow compilation for generation").


% parsers and generators
% default: parsing, and not generating.
:- initialize_flag(parser(parser),on).
:- initialize_flag(parser,parser).

:- initialize_flag(generator(generator),off).
%% :- initialize_flag(generator,generator).


:- version('ALE Version 3.2 beta; May, 1999').

:- initialize_flag(ale_portray,off).

:- multifile portray/1.
portray(X-Y):-
    hdrug_flag(ale_portray,on),
    pp_fs(X-Y,[]).


% Extending GUI
gram_startup_hook_end :-
    if_gui(( initialize_ale_flags,
	     update_ale_flags,
	     ale_tcl,
	     menu_flag(ale_portray,[on,off]),
	     menu_flag(ale_interpreter,[verbose,quiet,no]),
	     menu_flag(ale_subsumption,[on,off]),
	     menu_flag(ale_adderrs,[on,off]),
	     menu_flag(parser(parser),[on,off]),
	     menu_flag(generator(generator),[on,off])
	   )).

start_hook(parse,_,o(_,W,_),_) :-
    if_gui((
	    length(W,L),
	    tcl('global chartlength ; set chartlength ~w',L)
	   )).

end_hook(parse,_,_,_) :-
    object(1,o(Obj,_,_)),
    if_gui((   hdrug_flag(demo,on)
	   ->  semantics(Obj,Sem),
	       clig_ale(fs(Sem-bot),[]),
	       arg(4,Obj,N),
	       show(tree(derivation),clig,[value(N)]),
	       notify_active_obj(1)
	   ;   fail
	   )).

initialize_ale_flags :-
    initialize_ale_flag(lexs),
    initialize_ale_flag(macros),
    initialize_ale_flag(lex_rules),
    initialize_ale_flag(types),
    initialize_ale_flag(clauses),
    initialize_ale_flag(rules),
    tcl('set chartlength 0').

initialize_ale_flag(Key) :-
    tcl('set ale(~w) {}',[Key]).

update_ale_flags :-
    if_gui((
	    update_ale_lexs,
	    update_ale_macros,
	    update_ale_lex_rules,
	    update_ale_types,
	    update_ale_clauses,
	    update_ale_rules,
	    update_ale_empty,
	    update_chartlength
	   )).

update_chartlength :-
    (	hook(parsing(W))
    ->	length(W,L),
	tcl('set chartlength ~w',L)
    ;	true
    ).

update_ale_empty :-
    (	hook(empty(_))
    ->	tcl('set ale(empty) 1')
    ;	tcl('set ale(empty) 0')
    ).
	
update_ale_types :-
    format(user_error,"Updating types...",[]),
    findall(Name,hook(type(Name)),Names0),
    sort(Names0,Names),
    tcl("set types(max) 0"),
    update_array(Names,types),
    format(user_error,"Done.~n",[]).

update_ale_rules :-
    format(user_error,"Updating rules...",[]),
    findall(Name,hook(rule(Name,_)),Names0),
    sort(Names0,Names),
    tcl("set rules(max) 0"),
    update_array(Names,rules),
    format(user_error,"Done.~n",[]).

update_ale_clauses :-
    format(user_error,"Updating clauses...",[]),
    findall(Name,a_clause(Name),Names0),
    sort(Names0,Names),
    tcl("set clauses(max) 0"),
    update_array(Names,clauses),
    format(user_error,"Done.~n",[]).

a_clause(F/A) :-
    hook(if(Head,_Body)),
    Head =.. [F|Tail],
    length(Tail,A).

update_ale_lexs :-
    format(user_error,"Updating lexical entries...",[]),
    findall(Name,hook(lex(Name,_A,_B,_C)),Names0),
    sort(Names0,Names),
    tcl("set lexs(max) 0"),
    update_array(Names,lexs),
    format(user_error,"Done.~n",[]).

update_ale_lex_rules :-
    format(user_error,"Updating lexical rules...",[]),
    findall(Name,hook(lex_rule(Name,_)),Names0),
    sort(Names0,Names),
    tcl("set lex_rules(max) 0"),
    update_array(Names,lex_rules),
    format(user_error,"Done.~n",[]).

update_ale_macros :-
    format(user_error,"Updating macro names...",[]),
    findall(Name,hook(macro(Name,_)),Names0),
    sort(Names0,Names),
    tcl("set macros(max) 0"),
    update_array(Names,macros),
    format(user_error,"Done.~n",[]).


%%%%%%%%%%%%%%%%% compile_grammar %%%%%%%%%%%%%%%%%%

% no distinction between reconsult/compile.
% compile_grammar/0 is compile_grammar/1 where flag(grammar) determines
% the file
% flag(load_grammar_mode,[load,compile]
compile_grammar :-
    hdrug_flag(grammar,GrammarFile),
    (	GrammarFile == undefined
    ->	format(user_error,
	   "Warning: flag(grammar) not defined, no grammar loaded~n",[])
    ;	compile_grammar_file(GrammarFile)
    ).

reconsult_grammar :-
    compile_grammar.

reconsult_grammar_file(File) :-
    compile_grammar_file(File).

compile_grammar_file(File) :-
    compile_gram(File),
    update_ale_flags.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile_declarations :-
    hdrug_flag(decl,File),
    (	File == undefined
    ->	format(user_error,
	    "Warning: flag(decl) not defined, no declarations loaded~n",[])
    ;	ensure_loaded(File)
    ).

compile_suite :-
    hdrug_flag(suite,File),
    (	File == undefined
    ->	format(user_error,
	    "Warning: flag(suite) not defined, no examples loaded~n",[])
    ;	ensure_loaded(File)
    ).

hdrug_initialization :-
    abolish(term_expansion/2),
    ale_source_dir(Dir),
    assertz(file_search_path(ale,Dir)),
    ensure_loaded(ale(ale)),
    initialize_parsing_and_generation, % after ale.pl, before grammar
    compile_grammar,
    compile_declarations,
    compile_suite,
    ensure_loaded(ale(show)),
    ensure_loaded(ale(pp_chart)),
    ensure_loaded(ale(parser)),
    ensure_loaded(ale(generator)).

initialize_parsing_and_generation :-
    hdrug_flag(parser(parser),P),
    hdrug_flag(generator(generator),G),
    initialize_parsing_and_generation(P,G).

initialize_parsing_and_generation(on,on) :-
    parse_and_gen.
initialize_parsing_and_generation(off,on) :-
    generate.
initialize_parsing_and_generation(on,off) :-
    parse.
initialize_parsing_and_generation(off,off) :-
    format(user_error,"I am surprised that you want the compiler to produce no code at all....~n",[]).

ale_source_dir(Dir) :-
    source_file(ale_source_dir(_),File),    %% this file!
    concat(Dir,'/start.pl',File).

ale_edge(A,B,C,D,E,F,G,H) :-
    %% ale 3.2 back to 3.0:
    edge(A,B,C,D,E,F,G,H).



ale_tcl :-
    tcl('

       set ale_msg "ALE\\
             Copyright (C) 1995, Bob Carpenter and Gerald Penn\\
             All rights reserved.\\
             The Hdrug/Ale interface is written by Gertjan van Noord"
    '), 
    tcl('
button .t.version -text {Ale} -command {
 tk_dialog .d "About Ale" $ale_msg "" 0 ok 
}
pack .t.version -side right

.menu.help.m add command -label "Ale On-line Manual" -command {
    send_url_to_netscape http://www.let.rug.nl/~~vannoord/doc/nlp/ale/guide/
}
.menu.help.m add command -label "Ale Home Page" -command {
    send_url_to_netscape http://macduff.andrew.cmu.edu/ale/
}
.menu.help.m add command -label "About Ale" -underline 0\\
               -command {
    tk_dialog .d "About Ale" $ale_msg "" 0 ok
}

    '), 
    tcl('


# redefines Hdrug show menu completely
# showing objects available through obj buttons anyway..
catch {destroy .menu.show.m}    
menu .menu.show.m -postcommand {
    catch {.menu.show.m delete 0 last}

    .menu.show.m add command -label "Show Chart (graph)"\\
              -command {prolog pp_chart}
    .menu.show.m add command -label "Show Chart (tree)"\\
              -command {prolog {show(tree(chart),tk,[value(0)])}}


    if { $lexs(max) > 0 } then {
	.menu.show.m add cascade -label "View Lexical Entry" \\
                            -menu .menu.show.m.lex
    }
    if { $macros(max) > 0 } then {
	.menu.show.m add cascade -label "View Macro" \\
                            -menu .menu.show.m.macro
    }
    if { $rules(max) > 0 } then {
	.menu.show.m add cascade -label "View Rules" \\
                            -menu .menu.show.m.rule
    }
    if { $lex_rules(max) > 0 } then {
	.menu.show.m add cascade -label "View Lexical Rule" \\
                            -menu .menu.show.m.lex_rule
    }
    if { $types(max) > 0 } then {
	.menu.show.m add cascade -label "View Type" \\
                            -menu .menu.show.m.type
    }
    if $ale(empty) then {
	.menu.show.m add cascade -label "View Empty Categories" \\
                            -menu .menu.show.m.empty
    }
    if { $clauses(max) > 0} then {
	.menu.show.m add cascade -label "View Clause" \\
                            -menu .menu.show.m.clause
    }
    .menu.show.m add cascade -label "View Signature"\\
	-menu .menu.show.m.signature
}

    '),
    tcl('
menu .menu.show.m.lex
.menu.show.m.lex add command -label "Tk" \\
	-command "show_it tk lexs"
.menu.show.m.lex add command -label "Ale" \\
	-command "show_it ale lexs"
.menu.show.m.lex add command -label "Latex" \\
	-command "show_it latex lexs"
.menu.show.m.lex add command -label "Clig" \\
	-command "show_it clig lexs"

# not used
menu .menu.show.m.object
.menu.show.m.object add command -label "Tk" \\
	-command {
        set obj [hdrug_select_obj]
        if {$obj == 0} {return 0}
	prolog "show_it(tk,object,$obj)"
}
.menu.show.m.object add command -label "Ale" \\
	-command {
        set obj [hdrug_select_obj]
        if {$obj == 0} {return 0}
	prolog "show_it(ale,object,$obj)"
}

.menu.show.m.object add command -label "Latex" \\
	-command {
        set obj [hdrug_select_obj]
        if {$obj == 0} {return 0}
	prolog "show_it(latex,object,$obj)"
}

.menu.show.m.object add command -label "Clig" \\
	-command {
        set obj [hdrug_select_obj]
        if {$obj == 0} {return 0}
	prolog "show_it(clig,object,$obj)"
}

    '), 
    tcl('
menu .menu.show.m.macro
.menu.show.m.macro add command -label "Tk" \\
	-command "show_it tk macros"
.menu.show.m.macro add command -label "Ale" \\
	-command "show_it ale macros"
.menu.show.m.macro add command -label "Latex" \\
	-command "show_it latex macros"
.menu.show.m.macro add command -label "Clig" \\
	-command "show_it clig macros"

menu .menu.show.m.rule
.menu.show.m.rule add command -label "Tk" \\
	-command "show_it tk rules"
.menu.show.m.rule add command -label "Ale" \\
	-command "show_it ale rules"
.menu.show.m.rule add command -label "Latex" \\
	-command "show_it latex rules"
.menu.show.m.rule add command -label "Clig" \\
	-command "show_it clig rules"

menu .menu.show.m.lex_rule
.menu.show.m.lex_rule add command -label "Tk" \\
        -command "show_it tk lex_rules"
.menu.show.m.lex_rule add command -label "Ale" \\
        -command "show_it ale lex_rules"
.menu.show.m.lex_rule add command -label "Latex" \\
        -command "show_it latex lex_rules"
.menu.show.m.lex_rule add command -label "Clig" \\
        -command "show_it clig lex_rules"

menu .menu.show.m.type
.menu.show.m.type add command -label "Tk" \\
	-command "show_it tk types"
.menu.show.m.type add command -label "Ale" \\
	-command "show_it ale types"
.menu.show.m.type add command -label "Latex" \\
	-command "show_it latex types"
.menu.show.m.type add command -label "Clig" \\
	-command "show_it clig types"

    '), 
    tcl('
menu .menu.show.m.empty
.menu.show.m.empty add command -label "Tk" \\
	-command "prolog show_tk(empty,empty)"

.menu.show.m.empty add command -label "Ale" \\
	-command "prolog empty"

.menu.show.m.empty add command -label "Latex" \\
	-command "prolog show_latex(empty,empty)"

.menu.show.m.empty add command -label "Clig" \\
	-command "prolog show_clig(empty,empty)"

menu .menu.show.m.clause
.menu.show.m.clause add command -label "Tk" \\
        -command "show_it tk clauses"

.menu.show.m.clause add command -label "Ale" \\
        -command "show_it ale clauses"

.menu.show.m.clause add command -label "Latex" \\
        -command "show_it latex clauses"

.menu.show.m.clause add command -label "Clig" \\
        -command "show_it clig clauses"


menu .menu.show.m.signature
.menu.show.m.signature add command -label "Tk"\\
    -command "prolog show_it(tk,signature,bot)"
.menu.show.m.signature add command -label "Ale"\\
    -command "prolog show_it(ale,signature,bot)"
.menu.show.m.signature add command -label "Latex"\\
    -command "prolog show_it(latex,signature,bot)"
.menu.show.m.signature add command -label "Clig"\\
    -command "prolog show_it(clig,signature,bot)"

    '), 
    tcl('

proc show_it {output type} {
    global $type
    set thing [hdrug_qbox_array .w $type $type "Show $type" ]
    if {$thing != 0} {prolog "show_it($output,$type,$thing)"}
}

# overwrite this from hdrug.tcl...
proc hdrug_object {no} {
    # should not be necc:
    catch "destroy .bb.o.obj$no"

    menubutton .bb.o.obj$no -menu .bb.o.obj$no.a -text "$no"

    bind .bb.o.obj$no <2> "prolog try_hook(user:show_object_default2($no))"
    bind .bb.o.obj$no <3> "prolog try_hook(user:show_object_default3($no))"
		       
    pack .bb.o.obj$no -side left
    menu .bb.o.obj$no.a
    .bb.o.obj$no.a add cascade -label "View" \\
	            -menu .bb.o.obj$no.a.show
    menu .bb.o.obj$no.a.show

    .bb.o.obj$no.a.show add cascade -label "Tk" \\
	-menu .bb.o.obj$no.a.show.tk
    menu .bb.o.obj$no.a.show.tk
    catch {.bb.o.obj$no.a.show.tk delete 0 last}
    .bb.o.obj$no.a.show.tk add command -label Matrix \\
	-command "prolog show_it(tk,object,$no)"      
    .bb.o.obj$no.a.show.tk add command -label Tree \\
	-command "prolog show_it(tk,tree,$no)"      
    .bb.o.obj$no.a.show.tk add command -label MatrixTree \\
	-command "prolog show_it(tk,matrix_tree,$no)"      
    
    .bb.o.obj$no.a.show add cascade -label "Ale" \\
        -menu .bb.o.obj$no.a.show.prolog    
    menu .bb.o.obj$no.a.show.prolog
    catch {.bb.o.obj$no.a.show.prolog delete 0 last}
    .bb.o.obj$no.a.show.prolog add command -label Semantics \\
	-command "prolog show_object_no($no,sem,user)"
    .bb.o.obj$no.a.show.prolog add command -label Matrix \\
	-command "prolog show_ale(object,$no)"      
    .bb.o.obj$no.a.show.prolog add command -label Tree \\
	-command "prolog show_ale(tree,$no)"      

    .bb.o.obj$no.a.show add cascade -label "Latex" \\
	-menu .bb.o.obj$no.a.show.latex
    menu .bb.o.obj$no.a.show.latex
    catch {.bb.o.obj$no.a.show.latex delete 0 last}
    .bb.o.obj$no.a.show.latex add command -label Matrix \\
	-command "prolog show_it(latex,object,$no)"      
    .bb.o.obj$no.a.show.latex add command -label Tree \\
	-command "prolog show_it(latex,tree,$no)"      
    .bb.o.obj$no.a.show.latex add command -label MatrixTree \\
	-command "prolog show_it(latex,matrix_tree,$no)"      

    .bb.o.obj$no.a.show add cascade -label "Clig" \\
	-menu .bb.o.obj$no.a.show.clig
    menu .bb.o.obj$no.a.show.clig
    catch {.bb.o.obj$no.a.show.clig delete 0 last}
    .bb.o.obj$no.a.show.clig add command -label Matrix \\
	-command "prolog show_it(clig,object,$no)"      
    .bb.o.obj$no.a.show.clig add command -label Tree \\
	-command "prolog show_it(clig,tree,$no)"      
    .bb.o.obj$no.a.show.clig add command -label MatrixTree \\
	-command "prolog show_it(clig,matrix_tree,$no)"      

    prolog try_hook(create_object_hook(\'.bb.o.obj$no.a\',$no))
    update
}

wm iconname . "Ale"

').