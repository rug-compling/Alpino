
:- set_flag(grammar,'gram1').

:- multifile option/3.

option(grammar) -->
    [GrammarFile],
    {  set_flag(grammar,GrammarFile)  }.

:- multifile usage_option/3.

usage_option(grammar,"-grammar Grammar","use Grammar as grammar file").


%% parsers and generators
:- initialize_flag(parser(parser),on).
:- initialize_flag(generator(generator),on).

:- initialize_flag(parser,parser).
:- initialize_flag(generator,generator).

:- initialize_flag(max_depth,5).
:- initialize_flag(model,scfg).

:- multifile user:help_flag/2.

user:help_flag(max_depth,
	       "Maximum depth for DCG parser (to avoid problems with left recursion.").

user:help_flag(model,
	       "Probability model.").

%% top_features
top(s,parse(r(_,s,_),_)).

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
    compile(File).

:- version('Stochastic DCGs (R. Malouf, 29-Jul-2001)').

% Trees
:- multifile graphic_path/3,
             graphic_label/3,
	     graphic_daughter/4.

graphic_path(syn,parse(S,_),S).

graphic_label(syn,r(_,L,_),L).
graphic_label(syn,w(W),W).

graphic_daughter(syn,No,r(_,_,Ds),D):-
    lists:nth(No,Ds,D).

graphic_path(prob,parse(_,P),P).

graphic_label(prob,Term,Term).

% Extending GUI
gram_startup_hook_end :-
    menu_flag(max_depth,[2,5,10,100]),
    menu_flag(model,[scfg,maxent]),
    tcl('
button .t.version -text {SDCGs} -command {
 tk_dialog .d "About the Grammar" "Stochastic DCGs (R. Malouf, 29-Jul-2001)" "" 0 ok
}
pack .t.version -side right

.menu.show.m add command -label "Grammar" -command {
    prolog show_grammar
}


.menu.test.m add separator
.menu.test.m add command -label "Train from treebank" -command {
    prolog parser:train
}

.menu.help.m add command -label "About the grammar" -underline 0\
               -command {
    tk_dialog .d "About the Grammar" "Stochastic DCGs (R. Malouf, 29-Jul-2001)" "" 0 ok
}

wm iconname . "SDCG"

       ').

result_hook(parse,_,o(Obj,_,_),_) :-
    show(tree(syn),clig,[value(Obj)]).


:- compile_grammar.
:- compile(parser).
:- compile(generator).

%% Display stuff

show_lexicon :-
    ( lexicon(Word,r(Id,Cat,_)),
	hdrug_flag(model,Model),
	prob(Id,Model,P),
	format("~q: ~q --> ~q (~q)~n",[Id,Cat,Word,P]),
	fail
    ;
	true
    ).

print_dtr([],[]).
print_dtr([r(_,Cat,_)|Xs],[Cat|Ys]) :-
    print_dtr(Xs,Ys).

show_rules :-
    ( rule(r(Id,Mother,_), Dtrs),
	hdrug_flag(model,Model),
	prob(Id,Model,P),
	print_dtr(Dtrs,Cats),
	format("~q: ~q --> ~q (~q)~n",[Id,Mother,Cats,P]),
	fail
    ;
	true
    ).

show_grammar :-
    show_rules,
    show_lexicon.

    
	   
