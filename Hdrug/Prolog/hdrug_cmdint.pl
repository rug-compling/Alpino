%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
% written by Gertjan van Noord %
% (C) 1998                     %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% hookable (use multifile declaration):
%% hdrug_command(CmdName,Goal,Args)
%% hdrug_command_help(CmdName,UsageString,ExplanationString)
%% e.g.
%% hdrug_command(plus,(X is A + B, format('~w~n',[X])),[A,B]).
%%
%% goals are called in user module by default!
%% 
%%
%% |: plus 10 14
%% 24
%% |:
%% hdrug_command_help(plus,
%%                    "plus Inta Intb","prints Inta+Intb").
%%
%%
%%
%% history mechanism
%% alias mechanism
%% escape to unix shell
%% escape to Prolog
%% extendible
%% on-line help
%%
%% A command is typed in by the user as one line of text; it's
%% tokenized as a sequence of `words', where spaces and tabs are
%% treated as separators. Each word is treated as an atomic (Prolog
%% atom or Prolog integer), unless it is written within { and }. In
%% the latter case the `word' is parsed as a Prolog term (in the latter
%% case spaces and tabs are not interpreted as separators).
%% |: flag jan jan(a,b,c)
%% is equivalent to the Prolog goal ?- set_flag(jan,'jan(a,b,c)')
%% wherease
%% |: flag jan {jan(a,b,c)}
%% is equivalent to the Prolog goal ?- set_flag(jan,jan(a,b,c))
%%
%% variables occuring in such terms have scope _over the full 
%% command-line_! Thus, in order to parse any 3 word sentence in which the
%% first and third work are identical you can give the command;
%% |: parse {A} {B} {A}
%%
%% 
%%
%% the following meta-devices apply:
%% all occurences of $word are replaced by the definition of
%% the alias word. The alias command itself can be used to
%% define aliases:
%%
%% 19 |: alias hallo ! cat hallo
%% 20 |: $hallo
%%
%% so command number 20 will have the same effect as typing
%%
%% 33 |: ! cat hallo
%%
%% and if this command had indeed been typed as command number 33 then
%% typing
%%
%% 35 |: $33
%%
%% gives also the same result
%% The special meaning of $ can be turned off by prefixing it with
%% another $, e.g.:
%% |: cd $$HOME
%%
%%
%% Moreover, if no alias has been defined, then it will apply the last
%% command that started with the name of the alias:
%%
%% 66 |: parse john kisses mary
%% 67 |: $parse
%%
%% will have the same meaning (in this order) if the macro parse 
%% is not defined.
%%
%% It is also possible to issue Prolog commands; however some
%% restrictions apply. 
%%
%% 39 |: p {member(X,[X|T])}
%%
%% Note that this may succeed, but 'yes' or 'no' and variable bindings 
%% will NOT be printed.
%%
%% To extend this package, simply define some dcg clauses for hdrug_command/3.
%% also add hdrug_command_help/3 predicates for the commands you add.
%% You might need to prefix the command with appropriate module name.
%% You have to prefix hdrug_command/3 definitions with a multifile declaration.
%%
%% For an overview of built-in commands, issue the command `help command'

:- module(hdrug_cmdint, [ start_cmdint/0,
			  r/0
			]).

:- expects_dialect(sicstus).

:- multifile
    user:help_pred/3.

:- dynamic
    hdrug_command_alias:alias/2.

:- public
    user:help_pred/3.

:- discontiguous
    hdrug_command/3,
    hdrug_command_help/3.

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(hdrug_util).

:- meta_predicate dev_run(:,:).

%% :- meta_predicate hdrug_command(?,:,?).
%% goals are called in user module!

start_cmdint :-
    hdrug_flag(cmdint,OnOff),
    start_cmdint(OnOff).

start_cmdint(on) :-
    r.
start_cmdint(_Off).

user:help_pred(r,"r","Starts the command interpreter.").

r :-
    format(user_error,
	"*** Welcome to the Hdrug Command Interpreter (type ? for help) ***~n",
	   []),
    catch(restart,
	  Exception,
	  handle_exception(Exception)
	 ).

handle_exception(restart) :-
    nl, write('*** execution restarted ***'),nl,nl,
    r.

handle_exception(Exc) :-
    print_message(error,Exc),
    r.

restart :-
    repeat,                     % repeat loop
    create_prompt(N,Prompt),	% incorporate number in prompt
    prompt(_,Prompt),           % and redefine prompt
    hdrug_read_line(Input_line,End_input),
    (	End_input == end_of_file
    ->	true
    ;	replace_aliases(Input_line,[H|T]),
	usr_cmd(N,[H|T],End_input)
    ),
    !.                          % repeat cut
        
usr_cmd(N,[H|T],end_of_line) :-          % fail if Input line is empty
    (	parse_cmd(Cmd,[H|T])
    ->	increase_command_count,
	alias([N,H|T]),	        % define alias for command counter
	exec_cmd(Cmd)		% execute it, and fail (unless Cmd=prolog)
    ;	format(user_output,"*** error: command cannot be parsed ***~n",[]),
	fail
    ).

exec_cmd(Cmd) :-
    (	Cmd == prolog
    ->  format(user_error,"*** execution interrupted ***~n",[])
    ;	if(user:Cmd,
	   true,
	   format(user_output,
		  "~n*** error: command failed: ***~n~w~n",[Cmd])
	  ),
	fail
    ).

command_count(N) :-
    (   bb_get(command_count,N)
    ->  true
    ;   bb_put(command_count,1),
	N is 1
    ).

increase_command_count :-
    bb_get(command_count,N0),
    N is N0+1,
    bb_put(command_count,N).

create_prompt(N,Prompt):-
    dev_run(prolog:'$breaklevel'(BreakLevel,0),BreakLevel=0),	
    command_count(N),
    (   BreakLevel =:= 0
    ->  charsio:format_to_chars('~w |: ',[N],Chars)
    ;   charsio:format_to_chars('{~w} ~w |: ',[BreakLevel,N],Chars)
    ),
    name(Prompt,Chars).

%% replace_aliases(+ListOfWords,-ListOfWords)
%% all words in the first list that start with
%% a dollar sign are replaced by appropriate
%% lists

%% 1: finished:
replace_aliases([],[]).

%% 2: escaped alias: note in that case that $ is concatenated to
%%    the following atom... (this is solely useful for the Tcl
%%    commands I believe).
%%
replace_aliases([Var|T0],T):-
    var(Var),
    !,
    T=[Var|T1],
    replace_aliases(T0,T1).
    
replace_aliases([$,$,W0|T0],[W|T]) :-
    !,
    concat($,W0,W),
    replace_aliases(T0,T).

%% 3: a proper alias
replace_aliases([$,Word|T],Res ):-
    command_alias(Word,Y),
    !,
    replace_aliases(T,T2),
    append(Y,T2,Res).

%% 4: find command that started with that word
replace_aliases([$,Word|T],Res ):-
    command_alias(_,[Word|Y]),
    !,
    replace_aliases(T,T2),
    append([Word|Y],T2,Res).

%% single dollar: repeat last command
replace_aliases([$],Res) :-
    command_alias(_,Res0),
    !,
    Res=Res0.

%% 5: not a dollar sign, continue
replace_aliases([H|T],[H|T2]):-
    replace_aliases(T,T2).

%%%%%%%%%%%%%%%%%%
%%% parse_cmd %%%%
%%%%%%%%%%%%%%%%%%

parse_cmd((set_flag(Flag,Val),wr_flag(Flag)),L) :-
    flag_option(Flag,Val,L).

parse_cmd(X,[Cmd|Args]) :-
    hdrug_command(Cmd,X,Args).

flag_option(Flag,Val,[FlagIsVal]) :-
    nonvar(FlagIsVal),
    parse_flag(FlagIsVal,Flag,Val). %% from hdrug_main, treating options

flag_option(Flag,Val,[Flag,Is,Val]) :-
    nonvar(Is),
    Is = = .

flag_option(Flag,Val,[flag,Flag,Val]).

%% hdrug_command(+Key,-Goal,+WordsIn)
%% Key is the first word of the command
%% Goal is the Goal to be called
%% WordsIn are the list of words (possible arguments to the Key command)

hdrug_command(A,B,C) :-
    hdrug:hdrug_user_command(A,B,C).
hdrug_command_help(A,B,C) :-
    hdrug:hdrug_user_command_help(A,B,C).

hdrug_command(flag,wr_flag(Flag),[Flag]).
hdrug_command_help(flag,"flag Flag [Val]","\
without Val displays value of Flag; with Val sets Flag to Val").
hdrug_command_help(flag,"Flag=Val","sets Flag to Val.").

% for comments:
hdrug_command('%',true,_).
hdrug_command_help('%',"% Words","ignores Words (comment). Note that there needs to be a space after %.").

hdrug_command(fc,fcompile(List),List).
hdrug_command_help(fc,"fc Files","fcompiles(Files).").

hdrug_command(um,use_module(List),List).
hdrug_command_help(um,"um Files","use_module(Files).").

hdrug_command(el,ensure_loaded(List),List).
hdrug_command_help(el,"el Files","ensure_loaded(Files).").

hdrug_command(c,compile(List),List).
hdrug_command_help(c,"c Files","compile(Files).").

hdrug_command(rc,reconsult(List),List).
hdrug_command_help(rc,"rc Files","reconsult(Files).").

hdrug_command(ld,load(List),List).
hdrug_command_help(ld,"ld Files","load(Files).").

hdrug_command(libum,use_module(List),List0):-
    add_l(List0,List).
hdrug_command_help(libum,"libum Files",
		   "for each File, use_module(library(File)).").

hdrug_command(librc,reconsult(List),List0):-
    add_l(List0,List).
hdrug_command_help(librc,"librc Files",
		   "for each File, reconsult(library(File)).").

hdrug_command(libc,compile(List),List0):-
    add_l(List0,List).
hdrug_command_help(libc,"libc Files",
		   "for each File, compile(library(File)).").

hdrug_command(libel,ensure_loaded(List),List0):-
    add_l(List0,List).
hdrug_command_help(libel,"libel Files",
		   "for each File, ensure_loaded(library(File)).").

hdrug_command(libld,load(List),List0):-
    add_l(List0,List).
hdrug_command_help(libld,"libld Files",
		   "for each File, load(library(File)).").

add_l([],[]).
add_l([H|T0],[library(H)|T]) :-
    add_l(T0,T).

hdrug_command(v,version_command,[]).
hdrug_command(version,version_command,[]).
hdrug_command_help(version,"version","displays version information.").

hdrug_command(quit,halt,[]).
hdrug_command(q,halt,[]).
hdrug_command(exit,halt,[]).
hdrug_command(stop,halt,[]).
hdrug_command(halt,halt,[]).
hdrug_command_help(quit,"quit|exit|halt|q|stop","quits Hdrug.").

%hdrug_command(b,break,[]).
%hdrug_command_help(b,"b","break; enters Prolog prompt at next break level.").

hdrug_command(d,debug,[]).
hdrug_command_help(d,"d","debug/0.").

hdrug_command(nd,nodebug,[]).
hdrug_command_help(nd,"nd","nodebug/0.").

hdrug_command(p,prolog,[]).
hdrug_command(p,X,[X]).
hdrug_command_help(p,"p [Goal]",
"without Goal: quits command interpreter -- falls back to Prolog
prompt with Goal: calls Goal. Normally you will need {} around the
Goal. For example: 

        p { member(X,[a,b,c]), write(X), nl }

		  ").

hdrug_command(!, (   concat_all([H|T],Cmd,' '),
		     hdrug_cmdint:hdrug_shell(Cmd)
		 ),
	      [H|T]).
hdrug_command_help(!,"! Command","Command is executed by the shell. Note that the space between ! and Command is required.").

hdrug_command(alias,hdrug_cmdint:alias(List),List).
hdrug_command_help(alias,"alias [Name [Val]]","\
No args: lists all aliases; one arg: displays alias Name; two args:
defines an alias Name with meaning Val."). 


hdrug_command(? ,Cmd, Args ):-
    hdrug_command(help, Cmd, Args).
hdrug_command_help(?,Str0,Str) :-
    hdrug_command_help(help,Str0,Str).


hdrug_command(help,help_key(help,command,user),[]).
hdrug_command(help,help(O),[O]) :-
    hdrug_help:a_module(O).
hdrug_command(help,help(O,P),[O,P]) :-
    hdrug_help:a_module(O),
    hdrug_help:a_class(O,P).
hdrug_command(help,help_key(K,P,O),[O,P,K]) :-
    hdrug_help:a_module(O),
    hdrug_help:a_class(O,P),
    hdrug_help:a_key(O,P,K).
hdrug_command(help,help_key(K,P,O),[K]) :-
    hdrug_help:a_module(O),
    hdrug_help:a_class(O,P),
    hdrug_help:a_key(O,P,K).
hdrug_command(help,help_key(K,P,O),[O,K]) :-
    hdrug_help:a_module(O),
    hdrug_help:a_class(O,P),
    hdrug_help:a_key(O,P,K).
hdrug_command(help,help_key(K,P,O),[P,K]) :-
    hdrug_help:a_module(O),
    hdrug_help:a_class(O,P),
    hdrug_help:a_key(O,P,K).
hdrug_command_help(help,"help [Module] [Class] [Key]",
			String) :-
    findall(M,hdrug_help:a_module(M),Modules),
    charsio:format_to_chars("Displays help on the Module; Module Class; Module Class Key.
Help is available for the following modules:~n~w",[Modules],String).


hdrug_command(spy, spy([ListH|ListT]),L0) :-
    findall(Module:Pred,
	    (   is_module(Module,L0,L1),
		pred(Pred,F,L1,[]),
		Module:current_predicate(F,_)
	    ),
	    [ListH|ListT]
	   ).
hdrug_command_help(spy,"spy [Module] Pred",
	"set spypoint on Module:Pred; Pred can either be Fun or Fun/Ar.").

is_module(Module) -->
    [Module],
    { current_module(Module) }.
is_module(Module) -->
    { current_module(Module) }.

%% for spy
pred(X/Y,X) -->
        [X],{atom(X)},
        ['/'],
        [Y],{integer(Y)}.
pred(X,X) -->
        [X],{atom(X)}.

hdrug_command(cd,system:working_directory(_,Dir),[Dir]).
hdrug_command(cd,system:working_directory(_,'$HOME'),[]).
hdrug_command_help(cd,"cd [Dir]",
"change working directory to Dir; without argument cd to home
directory."). 

hdrug_command(pwd,hdrug_cmdint:hdrug_shell(pwd),[]).
hdrug_command_help(pwd,"pwd",
		   "print working directory.").

hdrug_command(ls,hdrug_cmdint:hdrug_shell(ls),[]).
hdrug_command_help(ls,"ls",
		   "listing of directory contents").

hdrug_command(lt,call_tree_bu_tk(F),[tk,F]).
hdrug_command(lt,call_tree_bu_clig(F),[clig,F]).
hdrug_command(lt,call_tree_bu_latex(F),[latex,F]).
hdrug_command(lt,call_tree_bu_tk,[tk]).
hdrug_command(lt,call_tree_bu_clig,[clig]).
hdrug_command(lt,call_tree_bu_latex,[latex]).
hdrug_command(lt,call_tree_bu(F),[F]).
hdrug_command(lt,call_tree_bu,[]).
hdrug_command_help(lt,"lt [tk/clig/latex] [Type]","\
prints lexical hierarchy for Type; without Type, prints
lexical hierarchy for top"). 

hdrug_command(x,really_start_x,[]).
hdrug_command_help(x,"x",
		   "(re)starts graphical user interface").
hdrug_command(nox,halt_x,[]).
hdrug_command_help(nox,"nox","halts graphical user interface").

hdrug_command(tcl,
	      (	  hdrug_cmdint:tcl_list([H|T],Return),
		  format(user_error,"=> ~s~n",[Return])
	      ), [H|T]).
hdrug_command_help(tcl,"tcl Cmd",
"calls tcl command Cmd; what is returned by the tcl command will be
printed on the screen after the => arrow.  

        75 |: tcl expr 3 * [ expr 5 + 4 ]
        => 27    

Remember that { and } need to be prefixed with backlash since
otherwise the Hdrug shell treats them. For instance 

        63 |: tcl expr 3+4
        => 7
").

:- public tcl_list/2.	
tcl_list(List,Return) :-
    vars_to_codes(List,Codes,[]),
    hdrug_gui:tcl(Codes,[],Return).

vars_to_codes([]) -->
    [].
vars_to_codes([H|T]) -->
    charsio:format_to_chars("~w ",[H]),
    vars_to_codes(T).

hdrug_command(source,tcl("source ~w",[H]),[H]).
hdrug_command_help(source,"source File",
	      "sources Tcl source File").

:- initialize_flag(output_default,user).
:- initialize_flag(type_default(clig),fs([])).
:- initialize_flag(type_default(tk),fs([])).

hdrug_command(s,show(Format,Output,Thing),L0) :-
    show_command(Format0,Output,L0,L1),
    item_spec(Thing,L1,[]),
    resolve_default(Format0,Output,Format).

resolve_default(default,Format,Output) :-
    !,
    resolve_default(Format,Output).
resolve_default(Output,_,Output).

resolve_default(Format,Output) :-
    hdrug_flag(type_default(Format),Output),
    Output \== undefined,
    !.
resolve_default(_,term(print)).  %default default

hdrug_command_help(s,"s [Format] [Output] Values","\
displays Objects with specified Format and Output; cf help on
s_format, s_output and s_value respectively.").
hdrug_command_help(s_format,"i/j/s/w/f [Path]/T","\
Specifies the Format of the s command.

i                write/1;

j                print/1 (default);

s                semantics (third argument of o/3 object terms)

w                words (second argument of o/3 object terms)

f Path           display as a feature structure; the optional path is
a sequence of attributes separated by colons (it selects the value at
that path). The prefix of the path can be a sequence of integers
seperated by / in order to select a specific node in the tree: this is
only possible of the category is a tree datastructure with functor
tree/3 where tree labels are specified in the first argument and lists
of daughters are specified in the third argument. 

T                T is a tree-format, display as a tree with that
format. Tree-formats are specified with the hook predicates
graphic_path, graphic_label and graphic_daughter."). 
hdrug_command_help(s_output,"user/latex/tk/clig/dot","\
Specifies the Output of the s command. 


user             as text to standard output (default);

latex            LaTeX; ghostview is used to display result;

tk               in the canvas of the graphical user interface;

dot              used DOT

clig             uses Clig").

hdrug_command_help(s_value,"ObjSpec/DefSpec/ValSpec",
"Specifies the Objects to be shown form the s command.

ObjSpec will select a number objects (parser/generator results):

s 2 5 8    specifies the objects numbered 2, 5 and 8

s 4 +      specifies the objects number 4 and above

s 3 -      specifies all objects up to number 3

s 5 to 12  specifies all objects between 5 and 12

DefSpec will select a user_clause definition:

s l Fun/Ar     specifies a listing of the Fun/Ar predicate.

ValSpec will specify a goal, and select an argument of that goal:

s [Module:]Fun/Ar [Pos]

The Module prefix is optional (user module is assumed if not
specified); the optional Pos argument selects a specific argument to
be printed. If no Pos argument is specified then the full goal is
printed. For example, if you have the following predicate defined: 

        x23(f(16),g(17),h).
        
then the following commands are possible:

        |: s x23/3
        
        x23(f(16),g(17),h)
        
        |: s x23/3 1
        
        h
        
        |: s user:x23/3 2
        
        g17
").
show_command(Format,Output) -->
    show_type(Format),
    show_output(Output).

show_command(Format,Output) -->
    show_output(Output),
    show_type(Format).

hdrug_command_help(type,"type [t/x/tk/clig/dot] [Type]","\
displays type t=tree x=latex tree, tk=tk tree, clig=clig tree,
dot=dot tree, none=textual information. No Type implies that top is used."). 
hdrug_command(type,pretty_graphic(type,Type),[t,Type]) :-
    hdrug_feature:define_type(Type,_,_,_,_).
hdrug_command(type,pretty_graphic(type,top),[t]).
hdrug_command(type,latex_tree(type,Type),[x,Type]) :-
    hdrug_feature:define_type(Type,_,_,_,_).
hdrug_command(type,latex_tree(type,top),[x]).
hdrug_command(type,tk_tree(type,Type),[tk,Type]):-
    hdrug_feature:define_type(Type,_,_,_,_).
hdrug_command(type,tk_tree(type,top),[tk]).
hdrug_command(type,clig_tree(type,Type),[clig,Type]) :-
    hdrug_feature:define_type(Type,_,_,_,_).
hdrug_command(type,clig_tree(type,top),[clig]).
hdrug_command(type,dot_tree(type,Type),[dot,Type]) :-
    hdrug_feature:define_type(Type,_,_,_,_).
hdrug_command(type,dot_tree(type,top),[dot]).
hdrug_command(type,pretty_type(Type),[Type]):-
    hdrug_feature:define_type(Type,_,_,_,_).
hdrug_command(type,pretty_type(top),[]).

hdrug_command(go,parser_comparisons([H|T]),[H|T]).
hdrug_command(go,parser_comparisons,[]).
hdrug_command(ps,parser_comparisons([H|T]),[H|T]).
hdrug_command(ps,parser_comparisons,[]).
hdrug_command_help(ps,"ps [Keys]","\
compares parsers on each sentence with key in Keys; without Keys,
compares parsers on all availables sentences;"). 

hdrug_command(psint,parser_comparisons_int(I,J),[I,J]):-
    integer(I),
    integer(J).
hdrug_command(goint,parser_comparisons_int(I,J),[I,J]):-
    integer(I),
    integer(J).
hdrug_command_help(psint,"psint I J","\
compares parsers on each sentence with key between I and J").

hdrug_command(gs,generator_comparisons([H|T]),[H|T]).
hdrug_command(gs,generator_comparisons,[]).
hdrug_command_help(gs,"gs [Keys]","\
compares generators on each lf with key in Keys; without Keys,
compares generators on all available lfs;"). 

hdrug_command(gsint,generator_comparisons_int(I,J),[I,J]):-
    integer(I),
    integer(J).
hdrug_command_help(gsint,"gsint I J","\
compares generators on each lf with key between I and J"). 

hdrug_command(rt,reset_table(P),[P]):- hdrug_flag(parser(P)).
hdrug_command(rt,reset_table(P),[P]):- hdrug_flag(generator(P)).
hdrug_command(rt,reset_table,[]).
hdrug_command_help(rt,"rt [Parser/Generator]","\
reset tables for parser/generator comparison for parser Parser or
generator Generator; without argument reset tables for all parsers and
generators"). 

hdrug_command(sentences,sentences,[]).
hdrug_command_help(sentences,
		   "sentences","lists all sentences").

hdrug_command(lfs,lfs,[]).
hdrug_command_help(lfs,
		   "lfs","lists all logical forms").

hdrug_command(pt,print_table,[]).
hdrug_command_help(pt,
	      "pt","print parser comparison overview").

hdrug_command(ptt,print_table_total,[]).
hdrug_command_help(ptt,
	      "ptt","print parser comparison tables in detail").

hdrug_command(pc,parse_compare(L),L).
hdrug_command_help(pc,
		   "pc Sentence",
		   "compares parsers on Sentences").

hdrug_command(gc,generate_compare(Term),[Term]).
hdrug_command_help(gc,
		   "gc LF","compares generators on LF").

hdrug_command(gco,generate_compare_object(ObjNo),[ObjNo]):- integer(ObjNo).

hdrug_command(list_sentences,list_sentences([H|List]),[H|List]).
hdrug_command(l,list_sentences([H|List]),[H|List]).

hdrug_command_help(gco,
		   "gco ObjNo","compares generators on LF of object ObjNo").

hdrug_command('*',parse(L),L).
hdrug_command('@',parse(L),L).
hdrug_command_help('*',"* Sentence","parses Sentence").
hdrug_command_help('@',"@ Sentence","parses Sentence").
hdrug_command(parse,parse(L),L).
hdrug_command_help(parse,"parse Sentence",
		   "parses Sentence").

hdrug_command('-',generate_obj(Obj),[ObjNo]):-
    integer(ObjNo),
    hdrug:object(ObjNo,Obj).
hdrug_command('-',generate(Sem),[Sem]).
hdrug_command_help('-',"- Term","\
if Term is an integer ObjNo, then generate from LF of object ObjNo;
otherwise Term is a semantic representation that is generated from"). 

hdrug_command(generate,generate_obj(Obj),[ObjNo]):-
    integer(ObjNo),
    hdrug:object(ObjNo,Obj).
hdrug_command(generate,generate(Sem),[Sem]).
hdrug_command_help(generate,"generate Term","\
if Term is an integer ObjNo, then generate from LF of object ObjNo;
otherwise Term is a semantic representation that is generated from"). 

hdrug_command(lg,compile_grammar,[]).
hdrug_command(lg,compile_grammar_file(File),[File]).
hdrug_command_help(lg,"lg [File]","\
with File, compile_grammar_file(File); without File,
compile_grammar."). 

hdrug_command(rcg,reconsult_grammar,[]).
hdrug_command(rcg,reconsult_grammar_file(File),[File]).
hdrug_command_help(rcg,"rcg [File]","\
with File, reconsult_grammar_file(File); without File,
reconsult_grammar."). 

hdrug_command(tkconsol,(set_flag(tkconsol,on),restart_x),[]).
hdrug_command_help(tkconsol,"tkconsol","\
(re)starts graphical user interface with TkConsol feature"). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% FLAG commands %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hdrug_command(av,hdrug:available,[]). 
hdrug_command_help(av,"av",
		   "shows activity status of parsers and generator").

%% "
hdrug_command(no,hdrug_cmdint:dels_flag(generator(G),G,[H|T]),[gm,H|T]).
hdrug_command(yes,hdrug_cmdint:adds_flag(generator(G),G,[H|T]),[gm,H|T]).
hdrug_command(only,hdrug_cmdint:only_flag(generator(G),G,[H|T]),[gm,H|T]).
hdrug_command(no,hdrug_cmdint:dels_flag(parser(G),G,[H|T]),[H|T]).
hdrug_command(yes,hdrug_cmdint:adds_flag(parser(G),G,[H|T]),[H|T]).
hdrug_command(only,hdrug_cmdint:only_flag(parser(G),G,[H|T]),[H|T]).
hdrug_command_help(no,"no [gm] List","\
with gm, List is a list of generators which are set to inactive
status; without gm, List is a list of parsers which are set to
inactive status"). 
hdrug_command_help(yes,"yes [gm] List","\
with gm, List is a list of generators which are set to inactive
status; without gm, List is a list of parsers which are set to
inactive status"). 
hdrug_command_help(only,"only [gm] List","\
with gm, List is a list of the only remaining active generators;
without gm, List is a list of the only remaining active parsers;"). 

hdrug_command(profile,hdrug_cmdint:profiling,[]).
hdrug_command(pr,hdrug_cmdint:profiling,[]).
hdrug_command(view,user:view(_:_),[]).
hdrug_command(v,user:view(_:_),[]).
hdrug_command_help(view,"v[iew]","\
calls the profiler on all predicates, i.e., it calls view(_:_);").


:- if(current_prolog_flag(language,sicstus)).


hdrug_command_help(profile,"pr[ofile]","\
loads the gauge profiler, and sets the prolog flag compilint to
profiledcode;").

:- public profiling/0.			
profiling:-
    user:use_module(library(gauge)),
    prolog_flag(compiling,_,profiledcode).

:- endif.

:- public only_flag/3, 	dels_flag/3, adds_flag/3.	   
only_flag(M,P,L):-
	( hdrug_flag(M),
	  (  member(P,L)
	  -> hdrug_flag(M,_,on)
	  ;  hdrug_flag(M,_,off)
	  ),
	  fail
	; true
	),
	hdrug:available. % hdrug_main.pl

dels_flag(M,P,L):-
	( member(P,L),
          hdrug_flag(M,_,off),
	  fail
	; true
	),
	hdrug:available.

adds_flag(M,P,L):-
	( member(P,L),
          hdrug_flag(M,_,on),
	  fail
	; true
	),
	hdrug:available.

hdrug_command(sts,sts(P),L):-
    parser_modes(P,+,L,[]).
hdrug_command(sts,sts,[]).
hdrug_command_help(sts,"sts [Parsers]","\
graphically displays statistics for Parsers; without Parsers displays
statistics for all parsers"). 

parser_modes([],*) --> 
	[].
parser_modes([H|T],_) -->
	[H],
	{hdrug_flag(parser(H))},
	parser_modes(T,_).

%% auxiliary DCG preds

add_subs([],L,L).
add_subs([H|T],L0,L):-
    subst_member(L0,H,L1),
    add_subs(T,L1,L).

subst_member([],Pair,[Pair]).
subst_member([H|T],El,List):-
    (	H=El
    ->  T=List
    ;   List=[H|List1],
	subst_member(T,El,List1)
    ).

read_from_chars(Chars,Term,Options) :-
    charsio:read_term_from_chars(Chars,Term,Options).

hdrug_read_line(Word_list,End_input):-
    read_line_to_codes(CodeList),
    (   CodeList == end_of_file
    ->  End_input=end_of_file,
	Word_list = []
    ;   hdrug_read_line(Word_list,End_input,CodeList)
    ).

:- if(current_predicate(rl_add_history/1)).

read_line_to_codes(List) :-
    read_line(List),
    (   List == end_of_file
    ->  true
    ;   atom_codes(Atom,List),
	rl_add_history(Atom)
    ).

:- else.

read_line_to_codes(List) :-
    read_line(List).

:- endif.

get_code(C,[C|T],T) :-
    !.
get_code(10,[],[]).

char_type_backslash(C_mid,C_out,Type,Cs0,Cs) :-
    (	C_mid =:= 92
    ->	get_code(C_out,Cs0,Cs),
	Type=normal
    ;   C_mid = C_out,
	hdrug_char_type(C_out,Type),
	Cs0=Cs
    ).

hdrug_read_line(WordList0,end_of_line,Codes0) :-
    get_code(C_in,Codes0,Codes1),
    skip_space(C_in,C_out,Codes1,Codes2),
    read_words(C_out,WordList,[],_Subs,Codes2,[]),
    WordList0=WordList.

read_words(C_in0,Ws,Subs0,Subs) -->
    char_type_backslash(C_in0,C_in,Type),
    read_words_cont(Type,C_in,Ws,Subs0,Subs).

read_words_cont(end_of_line,_,[],Subs,Subs) -->
    !.
read_words_cont(start_escape,_,[Term|Words],Subs0,Subs) -->
    !,
    read_word_esc(Str,C_mid),
    {  read_from_chars(Str,Term,[variable_names(Pairs)]),
       add_subs(Pairs,Subs0,Subs1)
    },
    skip_space(C_mid,C_out),
    read_words(C_out,Words,Subs1,Subs).

read_words_cont(_,C_in,[Word|Words],Subs0,Subs) -->
    read_word(C_in,Str,C_mid),
    { name(Word,Str) },
    skip_space(C_mid,C_out),
    read_words(C_out,Words,Subs0,Subs).

%% which syntax_char do we really need???
hdrug_char_type(Char,Type) :-
    (	Char =:= 32
    ->	Type = space
    ;   Char =:= 9
    ->  Type = space
    ;   Char =:= 26
    ->  Type = end_of_file
    ;   Char =:= -1
    ->  Type = end_of_file
    ;   Char =:= 10
    ->  Type = end_of_line
    ;   Char =:= 36
    ->  Type = syntax_char
    ;   Char =:= 123
    ->  Type=start_escape
    ;   Char =:= 125
    ->  Type=end_escape
    ;   Type = normal
    ).

%% 32=<space>, 9=<tab>, 26=<^Z>, -1=<EOF>, 10=<CR>
%% 33=! 36=$ 40=( 41=) 42=* 43=+ 44=,
%% 58=: 59=; 60=< 61== 62=> 63=? 
%% 91=[ 92=\  93=] 123={ 124=| 125=}
read_word(Next,Word,Last) -->
    {  hdrug_char_type(Next,Type) },
    read_word(Type,Next,Word,Last).

read_word(space,Next,[],Next) --> [].
read_word(end_of_file,Next,[],Next) --> [].
read_word(end_of_line,Next,[],Next) --> [].
read_word(syntax_char,Next,[Next],Last) -->
    get_code(Last).
read_word(normal,Next,[Next|Chars],Last) --> 
    get_code(C_mid0),
    char_type_backslash(C_mid0,C_mid,MidType),
    read_word_normal(MidType,C_mid,Chars,Last).

read_word(start_escape,Next,[Next|Chars],Last) -->
    get_code(C_mid0),
    char_type_backslash(C_mid0,C_mid,MidType),
    read_word_normal(MidType,C_mid,Chars,Last).

read_word(end_escape,Next,[Next|Chars],Last) -->
    get_code(C_mid0),
    char_type_backslash(C_mid0,C_mid,MidType),
    read_word_normal(MidType,C_mid,Chars,Last).

read_word_normal(syntax_char,C_mid,[],C_mid) -->
    !.
read_word_normal(MidType,C_mid,Chars,Last) -->
    read_word(MidType,C_mid,Chars,Last).


%% read_word reads anything up to the next space as an atom
%% read_esc_word reads anything up to the next } as a term, it
%% removes } and adds space . 32,46

read_word_esc(Word,Last) -->
    get_code(Next0),
    char_type_backslash(Next0,Next,Type),
    read_word_esc(Type,Next,Word,Last).

read_word_esc(end_of_file,_,_,_)-->
    { raise_exception(
	hdrug_error('cmdint: Unexpected end_of_file; expected }~n',[]))
    }.
read_word_esc(end_of_line,_,_,_)-->
    { raise_exception(
	hdrug_error('cmdint: Unexpected end_of_line; expected }~n',[]))
    }.
read_word_esc(end_escape,_,_,_)-->
    { raise_exception(hdrug_error('cmdint: Unexpected }~n',[])) }.
read_word_esc(normal,Next,[Next|Chars],Last) -->
    get_code(C_mid0),
    char_type_backslash(C_mid0,C_mid,MidType),
    read_word_esc_continue(MidType,C_mid,Chars,Last).

read_word_esc(start_escape,Next,[Next|Chars],Last) -->
    get_code(C_mid0),
    char_type_backslash(C_mid0,C_mid,MidType),
    read_word_esc_continue(MidType,C_mid,Chars,Last).

read_word_esc(space,Next,[Next|Chars],Last) -->
    get_code(C_mid0),
    char_type_backslash(C_mid0,C_mid,MidType),
    read_word_esc_continue(MidType,C_mid,Chars,Last).

read_word_esc(syntax_char,Next,[Next|Chars],Last)-->
    get_code(C_mid0),
    char_type_backslash(C_mid0,C_mid,MidType),
    read_word_esc_continue(MidType,C_mid,Chars,Last).

read_word_esc_continue(end_escape,_,[32,46],Last) -->
    !,
    get_code(Last).
read_word_esc_continue(MidType,C_mid,Chars,Last) -->
    read_word_esc(MidType,C_mid,Chars,Last).

skip_space(C_in,C_out) -->
    {  hdrug_char_type(C_in,Type) },
    skip_space(Type,C_in,C_out).

skip_space(space,_,C_out) -->
    !,
    get_code(C_mid),
    skip_space(C_mid,C_out).
skip_space(_,C,C) --> [].

command_alias(Alias,Command) :-
    hdrug_command_alias:alias(Alias,Command).

add_command_alias(Alias,Command) :-
    retractall(hdrug_command_alias:alias(Alias,_)),
    asserta(hdrug_command_alias:alias(Alias,Command)).

:- public hdrug_shell/1.
hdrug_shell(Cmd):-
    exec(Cmd,[null,pipe(Out),pipe(Err)],PID),
    copy_stream(Err),
    copy_stream(Out),
    close(Err),
    close(Out),
    wait(PID,_).

copy_stream(Stream):-
    get_byte(Stream,Char),
    copy_stream(Char,Stream).

copy_stream(Char0,Stream):-
    (	Char0 < 0
    ->  true
    ;   put_byte(Char0),
	get_byte(Stream,Char),
	copy_stream(Char,Stream)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% SHOW %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the things to show are
%
% - term
% - clause
% - listing (i.e. a sequence of clauses)
%
% the options are
%
% - terms are feature structures
% - terms are trees (different possibilities!)
% - terms are terms (internal)
%
% output goes to
%
% - standard output (ascii)
% - latex & xdvi
% - tk
% - clig
% - dot

show_type(words) -->
	[w].

show_type(words) -->
	[words].

show_type(sem) -->
	[s].

show_type(sem) -->
	[sem].

show_type(fs(Paths)) -->
	[f],
	paths(Paths).

show_type(term(write)) -->
	[i].

show_type(tree(Kind)) -->
	[Kind],
        { hdrug:graphic_path(Kind,_,_) }.

show_type(term(print)) -->
	[j].

show_type(default) -->  % default
	[].

show_output(tk) -->
	[tk].

show_output(latex) -->
	[x].

show_output(latex) -->
	[latex].

show_output(user) -->
	[user].

show_output(clig) -->
	[clig].

show_output(dot) -->
	[dot].

show_output(Default) -->
    { hdrug_flag(output_default,Default) },
    [].

%%%%%%%%%%%%%%%%%
%               %
% FIND OBJECTS  %
%               %
%%%%%%%%%%%%%%%%%

item_spec(Obj_spec) -->
        obj_spec(Obj_spec).

item_spec(Pred_spec) -->
	definition_spec(Pred_spec).

item_spec(Pred_spec) -->
	value_spec(Pred_spec).

%% only for user_clause/2
definition_spec(Clauses) -->
	[l],                     % for `listing'
	fa_spec(user,F,A),
	{ all_defs(F,A,Clauses)}.

fa_spec(Module,F,A) -->
	[Exp],
	{ atom_codes(Exp,String0),
	  append(Mstr,[58|String],String0),
	  atom_codes(Module,Mstr),
	  append(Fstr,[47|Astr],String),
	  atom_codes(F,Fstr),
	  number_codes(A,Astr),
	  functor(Term,F,A),
	  Module:current_predicate(F,Term)
        }.

fa_spec(user,F,A) -->
	[Exp],
	{ atom_codes(Exp,String),
	  append(Fstr,[47|Astr],String),
	  atom_codes(F,Fstr),
	  number_codes(A,Astr),
	  functor(Term,F,A),
	  user:current_predicate(F,Term)
        }.

value_spec(Value) -->
	fa_spec(Module,F,A),
	pos_spec(Arg),
	{ all_val(Module,F,A,Arg,Value) }.

all_val(Module,F,A,Arg,Values) :-
	findall(V,a_val(Module,F,A,Arg,V),Values).

all_defs(F,A,Clauses) :-
	findall(Clause,a_def(F,A,Clause),Clauses).

a_def(F,A,clause(Term,Body)) :-
    a_user_clause(F/A,Term,Body).

a_val(Module,F,A,whole,value(Term)) :-
    functor(Term,F,A),
    Module:Term.

a_val(Module,F,A,Arg,value(Term)) :-
    integer(Arg),
    functor(T0,F,A),
    arg(Arg,T0,Term),
    Module:T0.

pos_spec(Arg) -->
	[Arg],
	{ integer(Arg) }.

pos_spec(whole) -->
	[].

obj_spec(Objects) -->
        num_specs(Nums),
	{ findall(Obj,find_obj(Nums,Obj),Objects) }.

find_obj([No,to,No2],object(N,Obj)):-
    !,
    between(No,No2,N),
    hdrug:object(N,Obj).

find_obj([No,plus],object(N,Obj)):-
    !,
    hdrug:find_current_no(Max),	% hdrug_main
    between(No,Max,N),
    hdrug:object(N,Obj).

find_obj([No,minus],object(No2,Obj)):-
    !,
    between(1,No,No2,'-'),
    hdrug:object(No2,Obj).

find_obj([],object(N,O)) :-
    !,
    hdrug:object(N,O).

find_obj(List,object(No,O)) :-
    lists:member(No,List),
    hdrug:object(No,O).

num_specs1([plus]) -->
        ['+'],
        !.

num_specs1([minus]) -->
        ['-'],
        !.

num_specs1([to,Num]) -->
        [to],
        num_spec(Num),
        !.

num_specs1([Num|Nums]) -->
        num_spec(Num),
        num_specs1(Nums).

num_specs1([]) -->
        [].

num_specs([Num|Nums]) -->
        num_spec(Num),
        num_specs1(Nums).
num_specs([1]) --> [].

num_spec(Num) -->
        [Num],
        {integer(Num)}.

%%%%%%%%%%%%%%%%%
% FEATURE-path  %
%%%%%%%%%%%%%%%%%

paths([]) -->
        [].

paths([Path|Paths])-->
        path(Path),
        paths(Paths).

path(Path) --> [P],
        { is_path(P,Path) }.

is_path(Atom,Path):-
	parse_specials(Atom,List),
        path2(Path,List,[]).

parse_specials(Atom,AtomList) :-
    atomic(Atom),
    name(Atom,CharList),
    parse_specials2(CharList,AtomList).

parse_specials2([],[]).
parse_specials2(CharList,[Atom,SpecialAtom|Tail]):-
	append(Prefix,[Special|Rest],CharList),
	special(Special,SpecialAtom),!,
	name(Atom,Prefix),
	parse_specials2(Rest,Tail).
parse_specials2([H|T],[Atom]):-
	name(Atom,[H|T]),
	is_attribute(Atom).

is_attribute(A) :-
	hdrug_feature:e(A,_,_).
is_attribute(A) :-
	integer(A).

special(58,':').
special(47,'/').

%% een pad bestaat uit een eerste deel dat dochter aanwijst
%% gescheiden met /
%% en een tweede deel dat ingebedde featurestructuur aanwijst
%% gescheiden met :

path2(path(Feature)) -->
	feature_path(Feature).

feature_path([Att|Tail]) -->
	[Att],
	{is_attribute(Att)},
	feature_path2(Tail).

feature_path2([]) -->
	[].
feature_path2([Att|Tail]) -->
	[':'],
	[Att],
	{is_attribute(Att)},
	feature_path2(Tail).

%% alias
%% no arguments: list of all aliases
alias([]) :-
    findall(X-Z,command_alias(X,Z),List0),
    keysort(List0,List),
    (	member(X-Z,List),
	format("~w |:",[X]),
	(   member(W,Z),
	    format(" ~w",[W]),
	    fail
	;   true
	),
        nl,
	fail
    ;	true
    ).

alias([H|T]) :-
    alias0(T,H).

%% one argument: show value of that particular alias
alias0([],H) :-
    command_alias(H,Z),
    print(H=Z),
    nl.

%% several arguments: set value of head of list to tail
alias0(T,H):-
    add_command_alias(H,T). 


dev_run(Dev,Run) :-
    hdrug_util:system_type(DR),
    dev_run(DR,Dev,Run).

dev_run(development,Dev,_):-
    call(Dev).
dev_run(runtime,_,Run):-
    call(Run).


