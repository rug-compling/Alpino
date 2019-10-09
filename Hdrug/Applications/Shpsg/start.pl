
:- set_flag(grammar,'grammar').

:- multifile option/3.

option(grammar) -->
    [GrammarFile],
    {  set_flag(grammar,GrammarFile)  }.

:- multifile usage_option/3.

usage_option(grammar,"-grammar Grammar","use Grammar as grammar file").


%% parsers and generators
:- initialize_flag(parser(parser),on).
%:- initialize_flag(generator(generator),off).

:- initialize_flag(parser,parser).
%:- initialize_flag(generator,generator).

:- initialize_flag(model,maxent).
:- initialize_flag(iis_iterations,100).

:- multifile user:help_flag/2.

user:help_flag(model, "Probability model.").
user:help_flag(iis_iterations, "Number of iterations of IIS.").

%% top_features
top(s,parse(Cat,_)) :-
    Cat => phrase,
    Cat:syn:head => v,
    Cat:syn:spr => [].

top(any,parse(_,_)).

:- initialize_flag(top_features,s).

% grammar compilation
compile_grammar :-
    hdrug_flag(grammar,GrammarFile),
    (   GrammarFile == undefined
    ->  format(user_error,
           "Warning: hdrug_flag(grammar) not defined, no grammar loaded~n",[])
    ;   compile_grammar_file(GrammarFile)
    ).

reconsult_grammar :-
    compile_grammar.

reconsult_grammar_file(File) :-
    compile_grammar_file(File).

compile_grammar_file(File) :-
  retractall(template(_,_)),
  compile(File).

:- version('Stochastic HPSGs (R. Malouf, 28-Jul-2001)').

% Extending GUI
gram_startup_hook_end :-
%    menu_flag(max_depth,[2,5,10,100]),
%    menu_flag(model,[scfg,maxent]),
    menu_flag(iis_iterations,[1,10,100,250,1000,2500]),
    tcl('
button .t.version -text {SHPSG} -command {
 tk_dialog .d "About the Grammar" "Stochastic HPSGs (R. Malouf, 28-Jul-2001) demonstration system for ESSLLI 2001 course" "" 0 ok
}
button .t.train -text {Train} -command {prolog train}

button .t.test -text {Test} -command {prolog test}
pack .t.version .t.test .t.train -side right

.menu.show.m add command -label "Model" -command {
prolog show_model
}

.menu.help.m add command -label "About the grammar" -underline 0\
               -command {
    tk_dialog .d "About the Grammar" "Stochastic HPSG (R. Malouf, 28-Jul-2001) demonstration system for ESSLLI 2001 course" "" 0 ok
}

wm iconname . "SHPSG"

       ').

result_hook(parse,_,o(Obj,_,_),_) :-
    show(tree(syn),clig,[value(Obj)]).

:- compile_grammar.

:- ensure_loaded(probs).
:- ensure_loaded(parser).
:- ensure_loaded(pretty).

