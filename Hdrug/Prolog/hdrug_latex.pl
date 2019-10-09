%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                         %
% written by Gertjan van Noord                            %
% (C) 1993  all rights reserved                           %
%                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(hdrug_latex,
	  [ latex_term/1,
	    latex_term_list/1,
	    latex_tree/2,
	    latex_tree_list/2,
	    latex_fs/1,
	    latex_fs_list/1
%           escape_l/2                 %% used by latex help predicates
%	    write_it_fs_latex/1        %% used by Ale
	  ]).

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- use_module(hdrug_util      ).
:- use_module(library(system) ).
:- use_module(library(charsio)).
:- use_module(library(lists)  ).
:- use_module(library(system) ).

:- multifile user:help_pred/3.
:- public user:help_pred/3.

user:help_pred(latex_tree,"hdrug_latex:latex_tree(+TreeFormat,+Term)","Displays Term as a tree according to the TreeFormat specifications, in Ghostview. This predicate produces LaTeX code (with PsTricks extensions); it runs LaTeX and dvips on the result. The TreeFormat should be specified by means of clauses for the hook predicates graphic_path, graphic_daughter and graphic_label.").

user:help_pred(latex_tree_list,"hdrug_latex:latex_tree(+TreeFormat,+ListOfTerms)","Displays each Term in ListOfTerms as a tree according to the TreeFormat specifications, in Ghostview. This predicate produces LaTeX code (with PsTricks extensions); it runs LaTeX and dvips on the result. The TreeFormat should be specified by means of clauses for the hook predicates graphic_path, graphic_daughter and graphic_label.").

user:help_pred(latex_fs,"hdrug_latex:latex_fs(+Term)","Displays Term as a feature structure in Xdvi. The predicate produces LaTeX code (using Chris Manning's avm macro's); it runs LaTeX and xdvi on the result.").

user:help_pred(latex_fs_list,"hdrug_latex:latex_fs_list(+List)","Displays each Term in List as a feature structure in Xdvi. The predicate produces LaTeX code (using Chris Manning's avm macro's); it runs LaTeX and xdvi on the result.").

user:help_pred(latex_term,"hdrug_latex:latex_term(+Term)","Displays Term in Xdvi. The predicate produces LaTeX code; it runs LaTeX and xdvi on the result.").

user:help_pred(latex_term_list,"hdrug_latex:latex_term_list(+List)","Displays each Term in List in Xdvi. The predicate produces LaTeX code; it runs LaTeX and xdvi on the result.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% latex output for ordinary prolog terms %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

latex_term_list(List) :-
    latex_term(list(List)).

latex_term(Thing) :-
    hdrug_flag(latex(vspace),_,on),
    files_term(Tex,Dvi),
    telling(Old), tell(Tex),
    start_docu_simple,
    print_it(Thing),
    end_docu,
    told, tell(Old),
    latex_and_xdvi(Tex,Dvi).

print_it_list([]).
print_it_list([H|T]) :-
    print_it(H),
    print_it_list(T).

print_it(list(List)):-
    print_it_list(List).

print_it(Thing0) :-
    hdrug_show:change_thing_term(Thing0,Thing),
    print_it0(Thing).

%% to do..
print_it0(value(H,T)):-
    print_it0(clause(v(H),T)).

print_it0(value(T)) :-
    write('\\begin{flushleft}'),nl,
    prettyvars(T),
    tex_begin_line,
    write_goal(T, 1199, 0, Co),
    write_fullstop(Co),
    tex_end_line,
    write('\\end{flushleft}').

print_it0(clause(H)) :-
    write('\\begin{flushleft}'),nl,
    prettyvars(H),
    tex_begin_line,
    portray_clause1(H, Co),
    write_fullstop(Co),
    tex_end_line,
    write('\\end{flushleft}').

print_it0(clause(H,B)) :-
    write('\\begin{flushleft}'),nl,
    prettyvars((H:-B)),
    tex_begin_line,
    portray_clause1((H:-B), Co),
    write_fullstop(Co),
    tex_end_line,
    write('\\end{flushleft}').

%%% changed the following so as to generate latex code...
%%% buggy? definitely hacky.
%%% 

%% ADAPTED FROM:
%   File   : WRITE.PL
%   Author : Richard A. O'Keefe
%   Updated: 22 October 1984
%   Purpose: Portable definition of write/1 and friends.

% Priority 999 is o.k. if printed e.g. as elements of a list. /MC

%   maybe_paren(P, Prio, Char, Ci, Co)
%   writes a parenthesis if the context demands it.
%   Context = 2'000 for alpha
%   Context = 2'001 for quote
%   Context = 2'010 for other
%   Context = 2'100 for punct

maybe_paren(P, Prio, Lpar, '(', _, 2'100) :-
	P > Prio, !,
	write(Lpar).
maybe_paren(_, _, Lpar, Lpar, C, C).

maybe_paren(P, Prio, _, 2'100) :-
	P > Prio, !,
	write(')').
maybe_paren(_, _, C, C).

%   maybe_space(LeftContext, TypeOfToken)
%   generates spaces as needed to ensure that two successive
%   tokens won't run into each other.

maybe_space(Ci, Co) :-
	(   Ci\/Co<2'100, Ci#Co<2'010 -> tex_tab(1)  %%put(0' )
	;   true
	).

/*
sticky_contexts(alpha, alpha).
sticky_contexts(quote, quote).
sticky_contexts(other, other).
sticky_contexts(alpha, quote).
sticky_contexts(quote, alpha).
*/

%   write_out(Term, SynStyle, LexStyle, Prio, PrePrio, Depth, Lpar, Ci, Co)
%   writes out a Term in given SynStyle, LexStyle
%   at nesting depth Depth
%   in a context of priority Priority (that is, expressions with
%   greater priority must be parenthesized), 
%   and prefix operators =< PrePrio must be parenthesized,
%   where the last token to be
%   written was of type Ci, and reports that the last token it wrote
%   was of type Co.

write_out(Term, _, _, _, _, _, _, Ci, 2'000) :-
	var(Term), !,
	maybe_space(Ci, 2'000),
	tex_variable(Term).
write_out('$VAR'(N), SynStyle, LexStyle, _, _, Depth, _, Ci, Co) :- !,
	Depth1 is Depth+1,
	write_VAR(N, SynStyle, LexStyle, Depth1, Ci, Co).
write_out(_, print(Limit), _, _, _, Depth, _, Ci, 2'010) :-
	Depth >= Limit, !,
	maybe_space(Ci, 2'010),
	tex_dots.
/*
write_out(Term, print(_), _, _, _, _, _, _, 2'000) :-
	(   \+call_user_def(portray(Term), user) ->
	    fail		 % portray might bind variables
	;   true
	), !.
*/
write_out(Atom, _, LexStyle, _, PrePrio, _, Lpar, _, 2'100) :-
	atom(Atom),
	hdrug_util:current_prefixop(Atom, P, _),
	P =< PrePrio, !,
	write(Lpar),
	write_atom(LexStyle, Atom, 2'100, _),
	write(')').
write_out(Atom, _, LexStyle, _, _, _, _, Ci, Co) :-
	atom(Atom), !,
	write_atom(LexStyle, Atom, Ci, Co).
write_out(N, _, _, _, _, _, _, Ci, 2'000) :-
	number(N), !,
	(   N < 0 -> maybe_space(Ci, 2'010)
	;   maybe_space(Ci, 2'000)
	),
	write(N).
write_out(Term, noop, LexStyle, _, _, Depth, _, Ci, 2'100) :-
	functor(Term, Atom, Arity), !,
	write_atom(LexStyle, Atom, Ci, _),
	Depth1 is Depth+1,
	write_args(0, Arity, Term, noop, LexStyle, Depth1).
write_out({Term}, SynStyle, LexStyle, _, _, Depth, _, _, 2'100) :- !,
%%	put_code(0'{),
        write('\\{ '),
	Depth1 is Depth+1,
	write_out(Term, SynStyle, LexStyle, 1200, 0, Depth1, '(', 2'100, _),
	write('\\} ').
%%	put_code(0'}).
write_out([Head|Tail], SynStyle, LexStyle, _, _, Depth, _, _, 2'100) :- !,
	write('['),
	Depth1 is Depth+1,
	write_out(Head, SynStyle, LexStyle, 999, 0, Depth1, '(', 2'100, _),
	write_tail(Tail, SynStyle, LexStyle, Depth1).
write_out((A,B), SynStyle, LexStyle, Prio, _, Depth, Lpar, Ci, Co) :- !,
	%  This clause stops writeq quoting commas.
	Depth1 is Depth+1,
	maybe_paren(1000, Prio, Lpar, Lpar1, Ci, C1),
	write_out(A, SynStyle, LexStyle, 999, 0, Depth1, Lpar1, C1, _),
	write(','),
	write_out(B, SynStyle, LexStyle, 1000, 1000, Depth1, '(', 2'100, C2),
	maybe_paren(1000, Prio, C2, Co).
write_out(Term, SynStyle, LexStyle, Prio, PrePrio, Depth, Lpar, Ci, Co) :-
	functor(Term, F, N),
	Depth1 is Depth+1,
	write_out(N, F, Term, SynStyle, LexStyle, Prio, PrePrio, Depth1, Lpar, Ci, Co).

write_out(1, F, Term, SynStyle, LexStyle, Prio, _, Depth, Lpar, Ci, Co) :-
	hdrug_util:current_postfixop(F, P, O), !,
	(hdrug_util:current_infixop(F, _, _, _) -> O1=1200; O1=O),
	maybe_paren(O1, Prio, Lpar, Lpar1, Ci, C1),
	arg(1, Term, A),
	write_out(A, SynStyle, LexStyle, P, 1200, Depth, Lpar1, C1, C2),
	write_atom(LexStyle, F, C2, C3),
	maybe_paren(O1, Prio, C3, Co).
write_out(1, F, Term, SynStyle, LexStyle, Prio, PrePrio, Depth, Lpar, Ci, Co) :-
	F \== -,
        hdrug_util:current_prefixop(F, O, P), !,
	(PrePrio=1200 -> O1 is P+1; O1=O),	% for "fy X yf" etc. cases
	maybe_paren(O1, Prio, Lpar, _, Ci, C1),
	write_atom(LexStyle, F, C1, C2),
	arg(1, Term, A),
	write_out(A, SynStyle, LexStyle, P, P, Depth, ' (', C2, C3),
	maybe_paren(O1, Prio, C3, Co).
write_out(2, F, Term, SynStyle, LexStyle, Prio, PrePrio, Depth, Lpar, Ci, Co) :-
        hdrug_util:current_infixop(F, P, O, Q), !,
	(PrePrio=1200 -> O1 is Q+1; O1=O),	% for "U xfy X yf" etc. cases
	maybe_paren(O1, Prio, Lpar, Lpar1, Ci, C1),
	arg(1, Term, A),
	write_out(A, SynStyle, LexStyle, P, 1200, Depth, Lpar1, C1, C2),
	write_atom(LexStyle, F, C2, C3),
	arg(2, Term, B),
	write_out(B, SynStyle, LexStyle, Q, Q, Depth, '(', C3, C4),
	maybe_paren(O1, Prio, C4, Co).
write_out(N, F, Term, SynStyle, LexStyle, _, _, Depth, _, Ci, 2'100) :-
	write_atom(LexStyle, F, Ci, _),
	write_args(0, N, Term, SynStyle, LexStyle, Depth).

write_VAR(N, SynStyle, _, _, Ci, 2'000) :-
	integer(N), N >= 0,
	SynStyle \== noop, !,
	maybe_space(Ci, 2'000),
	tex_var(N).

write_VAR(String, SynStyle, _, _, Ci, Co) :-
	nonvar(String),
	(   catch(atom_codes(Atom, String),_,fail)
	->  true
	;   Atom = String
	),
	atom(Atom),
	SynStyle \== noop, !,
	write_atom(noquote,Atom,Ci,Co).
write_VAR(X, SynStyle, LexStyle, Depth, Ci, 2'100) :-
	write_atom(LexStyle, '$VAR', Ci, _),
	write_args(0, 1, '$VAR'(X), SynStyle, LexStyle, Depth).

write_atom(noquote, Atom, Ci, Co) :-
	% prolog:'$atom_mode'(Atom, Co),
        Co = 2'000,
	maybe_space(Ci, Co),
	tex_atom(Atom).
write_atom(quote, Atom, Ci, Co) :-
        Co = 2'000,
	% prolog:'$atom_mode'(Atom, Co),
	maybe_space(Ci, Co),
	tex_atom(Atom).

%   write_args(DoneSoFar, Arity, Term, SynStyle, LexStyle, Depth)
%   writes the remaining arguments of a Term with Arity arguments
%   all told in SynStyle, LexStyle, given that DoneSoFar have already been written.

write_args(N, N, _, _, _, _) :- !,
	write(')').
write_args(I, _, _, print(Limit), _, Depth) :-
	Depth >= Limit, !,
	write_args(I, Depth),
	tex_dots,
	write(')').
write_args(I, N, Term, SynStyle, LexStyle, Depth) :-
	write_args(I, Depth),
	J is I+1,
	arg(J, Term, A),
	write_out(A, SynStyle, LexStyle, 999, 0, Depth, '(', 2'100, _),
	write_args(J, N, Term, SynStyle, LexStyle, Depth).

write_args(0, _) :- !, write('(').
write_args(_, 0) :- !, write(',').
write_args(_, _) :- write(',').



%   write_tail(Tail, SynStyle, LexStyle, Depth)
%   writes the tail of a list of a given SynStyle, LexStyle, Depth.

write_tail(Var, _, _, _) :-			%  |var]
	var(Var), !,
	write('|'),
	write(Var),
	write(']').
write_tail([], _, _, _) :- !,			%  ]
	write(']').
write_tail(_, print(Limit), _, Depth) :-
	Depth >= Limit, !,
	write('|'),
	tex_dots,
	write(']').
write_tail([Head|Tail], SynStyle, LexStyle, Depth) :- !, %  ,Head tail
	write(','),
	write_out(Head, SynStyle, LexStyle, 999, 0, Depth, '(', 2'100, _),
	Depth1 is Depth+1,
	write_tail(Tail, SynStyle, LexStyle, Depth1).
write_tail(Other, SynStyle, LexStyle, Depth) :-	%  |junk]
	write('|'),
	write_out(Other, SynStyle, LexStyle, 999, 0, Depth, '(', 2'100, _),
	write(']').

portray_clause1(:-(Command), Co) :-
	functor(Command, Key, 1),
	current_op(_, fx, Key), !,
	arg(1, Command, Body),
	'list clauses'(Body, :-(Key), 8, Co).
portray_clause1((Pred:-true), Co) :- !,	
	 write_goal(Pred, 1199, 0, Co).
portray_clause1((Pred:-Body), Co) :- !,	
	write_goal(Pred, 1199, 1200, _),
	'list clauses'(Body, 0, 8, Co).
portray_clause1((Pred-->Body), Co) :- !,
	write_goal(Pred, 1199, 1200, _),
	'list clauses'(Body, 2, 8, Co).
portray_clause1(Pred, Co) :-
	write_goal(Pred, 1199, 0, Co).


write_goal(M:Goal, Prio, PrePrio, C) :- !,
	write_out(M:Goal, op, quote, Prio, PrePrio, -2, '(', 2'100, C).
write_goal(Goal, Prio, PrePrio, C) :-
	write_out(Goal, op, quote, Prio, PrePrio, -1, '(', 2'100, C).

write_fullstop(Ci) :-
	maybe_space(Ci, 2'010),
	write('.'),
	tex_nl.


'list clauses'((A,B), L, D, Co) :- !,
	'list clauses'(A, L, D, _),
	'list clauses'(B, 1, D, Co).
'list clauses'((A;B), L, D, 2'100) :- !,
	'list magic'(L, D),
	'list disj'(A, 3, D),
	'list disj'(B, D).
'list clauses'((A->B), L, D, 2'100) :- !,
	'list magic'(L, D),
	E is D+4,
	'list clauses'(A, 3, E, _),
	'list clauses'(B, 5, E, _),
	tex_nl, tex_tab(D),
	write(')').
'list clauses'(!, 0, _, 2'100) :- !,
        write(' :- !').
'list clauses'(!, 1, _, 2'100) :- !,
	write(', !').
'list clauses'(!, 2, _, 2'100) :- !,
	write(' --> !').
'list clauses'(Goal, L, D, Co) :- !,
	'list magic'(L, D),
	write_goal(Goal, 999, 0, Co).


'list magic'(0, D) :-
	tex_if,
	tex_nl, tex_tab(D).
'list magic'(1, D) :-
	write(','),
	tex_nl, tex_tab(D).
'list magic'(2, D) :-
	write(' -->'),
	tex_nl, tex_tab(D).
'list magic'(3, _) :-
	write('(   ').
'list magic'(4, _) :-
	write(';   ').
'list magic'(5, D) :-
	write(' ->'),
	tex_nl, tex_tab(D).
'list magic'(:-(Key), D) :-
	tex_if,
	write(Key),
	tex_nl, tex_tab(D).

'list disj'((A;B), D) :- !,
	'list disj'(A, 4, D),
	'list disj'(B, D).
'list disj'(Conj, D) :-
	'list disj'(Conj, 4, D),
	write(')').

'list disj'((A->B), L, D) :- !,
	E is D+4,
	'list clauses'(A, L, E, _),
	'list clauses'(B, 5, E, _),
	tex_nl, tex_tab(D).
'list disj'(A, L, D) :-
	E is D+4,
	'list clauses'(A, L, E, _),
	tex_nl, tex_tab(D).


tex_term(Val) :-
	write_goal(Val,1199,0,_Co).    % cf below!

dir(TmpDir) :-
	environ('TMPDIR',TmpDir),!.
dir('/tmp').

%% different files, since aux might contain packages specific info
%% that the other files do not know about
files_term(Tex,Dvi) :-
	dir(Tmp),
	concat(Tmp,'/hdrug_latex_termXXXXXX',Base0),
	mktemp(Base0,Base),
	concat(Base,'.tex',Tex),
	concat(Base,'.dvi',Dvi).

files_fs(Tex,Dvi) :-
	dir(Tmp),
	concat(Tmp,'/hdrug_latex_avmXXXXXX',Base0),
	mktemp(Base0,Base),
	concat(Base,'.tex',Tex),
	concat(Base,'.dvi',Dvi).

files_tree(Tex,Dvi,Ps) :-
	dir(Tmp),
	concat(Tmp,'/hdrug_latex_treeXXXXXX',Base0),
	mktemp(Base0,Base),
	concat(Base,'.tex',Tex),
	concat(Base,'.dvi',Dvi),
	concat(Base,'.ps',Ps).


latex_and_xdvi(Tex,Dvi) :-
	dir(Dir),
	format_to_chars('sh -c "(cd ~a ; latex ~a ; xdvi -geometry 800x480+100+800 -paper a1 ~a)" &',[Dir,Tex,Dvi],Chars),
	name(Cmd,Chars),
	shell(Cmd).

% the structures are embedded in a document, hence we set up the
% document:
% LaTeX2E
start_docu_simple :-
	format("
\\documentclass{article}
\\setlength{\\parindent}{-100pt}
\\begin{document}
\\thispagestyle{empty}
\\topmargin -100pt
",[]).

%%% these are all supposed to be in mathematical mode !
tex_tab(0) :-
	!.
tex_tab(N0) :-
	write('~'),
	N is N0-1,
	tex_tab(N).


tex_nl :-
	tex_end_line,
	tex_begin_line.


tex_end_line :-
	(  hdrug_flag(latex(vspace),on) 
        -> write('$\\\\')
        ;  write('$')
        ),nl.

tex_begin_line :-
	write('$'),nl.

tex_variable(Term) :-
	write(Term), 
	nl.  % added for long lines


tex_dots :-
	write('\\dots ').


tex_atom([]) :-
	!,
	write('[~]').
tex_atom(A) :-
	write(' \\mbox{'),  % removed \\it
	tx_atom(A),
	write('} '),
	nl.           % added to make lines not too long..

tx_atom(A) :-
	escape_chars(A,A2),
	write(A2).

escape_chars(A,A2):-
	term_atom(A,A1),
	atom_codes(A1,Chars),
	hdrug_stats:escape_l(Chars,Chars2),
	atom_codes(A2,Chars2).


tex_var(N) :-
	Letter is mod(N,26) + 0'A,
	write('\\mbox{'),put_code(Letter),write('}'),
	(   N>=26 ->
	    Rest is N//26, write('_{'),write(Rest),write('}')
	;   true
	).

tex_if :-
	write('{\\mbox{\\tt :-}}').


%%%%%%%%%%%%%%%
%%%% TREES %%%%
%%%%%%%%%%%%%%%

:- initialize_flag(nodeskip,1).

latex_tree(Kind,Term):-
    latex_tree_list(Kind,[Term]).

%% use with_output_to_chars ?
latex_tree_list(Kind,List):-
    files_tree(TexFile,DviFile,PsFile),
    tell(TexFile),
    start_pstricks,
    pstricks_tree_list(List,Kind),
    end_pstricks,
    told,
    pstricks_command(TexFile,DviFile,PsFile).

pstricks_tree_list([],_).
pstricks_tree_list([Tree0|T],Kind):-
    hdrug_show:change_thing_tree(Tree0,Kind,Tree),
    pstricks_tree(Tree,Kind),
    pstricks_tree_list(T,Kind).

pstricks_command(Tex,Dvi,Ps) :-
    dir(Dir),
    format_to_chars("sh -c '( cd ~a ; latex ~a ; latex ~a; latex ~a ; dvips -f ~a > ~a ; ghostview ~a )' &",[Dir,Tex,Tex,Tex,Dvi,Ps,Ps],Chars),
    name(Cmd,Chars),
    shell(Cmd).

start_pstricks :-
	hdrug_flag(tex_library,Texdir),
        format("
\\documentclass{article}
\\usepackage{pst-tree}
\\usepackage{~a/avm}
\\avmfont{\\sc} 
\\avmvalfont{\\em} 
\\avmsortfont{\\scriptsize\\it}
\\begin{document}
\\avmvskip{.1ex}
\\avmhskip{.5em}  

",[Texdir]).

end_pstricks :-
	format("\

\\end{document}
",[]).
        

pstricks_tree(tree(L,_,Ds),Kind) :-
    hdrug_flag(nodeskip,CM),
    format("\\centerline{\\pstree[levelsep=*~wcm,nodesep=3pt]{",[CM]),
    pstricks_label(Kind,L),
    format("}{",[]),
    pstricks_ds(Ds,Kind),
    format("}}",[]).

pstricks_tree0(tree(L,_,Ds),Kind) :-
    format("\\pstree{",[]),
    pstricks_label(Kind,L),
    format("}{",[]),
    pstricks_ds(Ds,Kind),
    format("}",[]).

pstricks_label(matrix(_),L) :-
    !,
    format("\\Tr[ref=c]{",[]),
    tree_label_fs(L),
    format("}",[]).

pstricks_label(user(_),L) :-
    !,
    format("\\Tr[ref=c]{",[]),
    hdrug:latex_tree_user_node(L),
    format("}",[]).

pstricks_label(_,L) :-
    format("\\Tr[ref=c]{",[]),
    tree_label(L),
    format("}",[]).

pstricks_ds([],_).
pstricks_ds([H|T],Kind) :-
    pstricks_tree0(H,Kind),
    pstricks_ds(T,Kind).

tree_label(A) :-
    write('$ '),
    tex_term(A),
    write(' $'),
    nl.

tree_label_fs(A) :-
    write_it_fs_latex(value(A)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% prolog terms as feature structures %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

latex_fs_list(Things) :-
    latex_fs(list(Things)).

latex_fs(Thing) :-
    hdrug_flag(latex(vspace),_,on),
    files_fs(Tex,Dvi),
    telling(Old), tell(Tex),
    start_docu,
    print_it_fs(Thing),
    end_docu,
    told, telling(Old),
    latex_and_xdvi(Tex,Dvi).

print_it_fs_list([]).
print_it_fs_list([H|T]) :-
    print_it_fs(H),
    print_it_fs_list(T).

print_it_fs(list(List)) :-
    print_it_fs_list(List).

print_it_fs(Thing0) :-
    hdrug_show:change_thing_fs(Thing0,Thing),
    write('\\begin{flushleft}'),nl,
    write_it_fs_latex(Thing),
    write('\\end{flushleft}').

write_it_fs_latex(value(FS,C)) :-
    tex_begin_line_avm,
    pp(FS),tex_if,
    tex_end_line_avm,
    write_pretty_constraint_latexs(C).

write_it_fs_latex(value(FS)) :-
    tex_begin_line_avm,
    pp(FS),
    tex_end_line_avm.

write_it_fs_latex(clause(H)) :- 
    tex_begin_line_avm,
    write_pretty_constraint_latex(H),
    write('.'), tab(1), tex_end_line_avm.

write_it_fs_latex(clause(H,B)) :- 
    tex_begin_line_avm, 
    write_pretty_constraint_latex(H), 
    tex_if,
    tex_end_line_avm, 
    write_pretty_constraint_latexs(B).

write_pretty_constraint_latexs([H|T]) :-
    tex_begin_line_avm, 
    write('~~~~'),
    write_pretty_constraint_latex(H),
    write_pretty_constraint_latexs0(T).

write_pretty_constraint_latexs0([]) :-
	write('.'), tab(1), tex_end_line_avm.
write_pretty_constraint_latexs0([H|T]) :-
	write(','),tab(1),
	tex_end_line_avm,
	tex_begin_line_avm, 
	write('~~~~'),
	write_pretty_constraint_latex(H),nl,
	write_pretty_constraint_latexs0(T).

write_pretty_constraint_latex(true).   % ??
write_pretty_constraint_latex(H) :-
	H =.. [F|Args],
	write_relation(F),
	write_begin_functor_latex(Args),
	write_pretty_argument_latexs0(Args),
	write_end_functor_latex(Args).

write_begin_functor_latex([]).
write_begin_functor_latex([_|_]):-
	write('(').

write_end_functor_latex([]).
write_end_functor_latex([_|_]) :-
	write(')').

write_pretty_argument_latexs0([]).
write_pretty_argument_latexs0([H|T]):-
	pp(H),
	write_pretty_argument_latexs(T).

write_pretty_argument_latexs([]).
write_pretty_argument_latexs([H|T]):-
	write(','),
	pp(H),
	write_pretty_argument_latexs(T).

pp(_Var/n=FS):-
	!,
        ppl_latex(FS,no).

pp(Var/_='R'):-
        !,
	tex_var_avm(Var).

pp(Var/y=[]):-
        !,
	tex_var_avm(Var).

pp(Var/y='$VAR'(_)):-
	!,
	tex_var_avm(Var).

pp(Var/y=FS):-
	!,
%	tex_var(Var),
        ppl_latex(FS,yes(Var)).

pp(lex(W)) :-
	!,
	write(W).

pp('$VAR'(X)) :-   %% shoud this happen?
    !,
    write('$VAR'(X)).

ppl_latex([a(_Att,Thing)|Rest],Tab):-
	do_not_print(Thing),!,
	ppl_latex(Rest,Tab).

ppl_latex([a(type,['.']),a(_,Head),a(_,Tail)],no) :-
	!,
	ppl_list(Head,Tail).

ppl_latex([a(type,['.']),a(_,Head),a(_,Tail)],yes(Var)) :-
	!,
	tex_var_avm(Var),write('~'),
	ppl_list(Head,Tail).

ppl_latex([a(type,[[]])],_) :-
	!,
	write('$\\langle$$\\rangle$').

ppl_latex([a(type,Types)|T],no):-
	all_empty(T),
	!,
	hdrug_feature:write_as_conj(Types,PTypes),
	write_type_latex(PTypes).

ppl_latex([a(type,Types)|T],yes(Var)):-
	all_empty(T),
	!,
	tex_var_avm(Var),write('~'),
	hdrug_feature:write_as_conj(Types,PTypes),
	write_type_latex(PTypes).

ppl_latex([a(type,Types)|T],no):-
	!,
	hdrug_feature:write_as_conj(Types,PTypes),
	write('\\[{'), write_type_latex(PTypes), write('}'),
	ppl_latex(T,_Tab),
	write('\\]'),nl.

ppl_latex([a(type,Types)|T],yes(Var)):-
	!,
	tex_var_avm(Var),
	hdrug_feature:write_as_conj(Types,PTypes),
	write('\\[{'), write_type_latex(PTypes), write('}'),
	ppl_latex(T,_Tab),
	write('\\]'),nl.

ppl_latex([a('BOOLEAN',_Type,Val)|T],no):-
	all_empty(T),
	!,
	hdrug_feature:give_boolean_type(Val,Exp),
	write_type_latex(Exp).

ppl_latex([a('BOOLEAN',_Type,Val)|T],yes(Var)):-
	all_empty(T),
	!,
	tex_var_avm(Var),write('~'),
	hdrug_feature:give_boolean_type(Val,Exp),
	write_type_latex(Exp).


ppl_latex([a('BOOLEAN',_Type,Val)|T],no):-
	hdrug_feature:give_boolean_type(Val,Exp),
	write('\\[{'),	write_type_latex(Exp), write('}'),
	ppl_latex(T,_Tab),
	write('\\]'),nl.


ppl_latex([a('BOOLEAN',_Type,Val)|T],yes(Var)):-
	hdrug_feature:give_boolean_type(Val,Exp),
	tex_var_avm(Var),
	write('\\[{'),	write_type_latex(Exp), write('}'),
	ppl_latex(T,_Tab),
	write('\\]'),nl.

ppl_latex([a('UNTYPED',_Att,Val)|T],no):-
	tex_term0(Val),
	ppl2_latex(T,_Tab).

ppl_latex([a('UNTYPED',_Att,Val)|T],yes(Var)):-
	tex_var_avm(Var),write('~'),
        tex_term0(Val),
	ppl2_latex(T,_Tab).

/*
ppl_latex([a('TREE',Mark0,Cat,Ds)|T],_) :-
	mrk_t(Mark0,Mark),
	write(Mark),tab(1),
	pp_ds([Cat|Ds],0),
	ppl2_latex(T,_).
*/

ppl_latex([a(Att,FS)|T],no):-
        !,
	write_attribute(Att),
	write('&'),
        pp(FS),
        ppl2_latex(T,_Tab).


ppl_latex([a(Att,FS)|T],yes(Var)):-
        !,
	tex_var_avm(Var),
	write_attribute(Att),
	write('&'),
        pp(FS),
        ppl2_latex(T,_Tab).

ppl_latex([],_) :- !.

ppl_latex('$VAR'(_),_) :-
	!,
	write('\\_').
%%	write('$VAR'(X)).


%% so we can have arbitrary terms here. I don't think the change
%% predicates support this, though.
ppl_latex(Thing,_) :-
	write_pretty_constraint_latex(Thing).

tex_term0(Term) :-
	write('\\mbox{$'),
	tex_term(Term),
	write('$}').

/* not used anymore?
pplist_latex(atom(X),I,_Tab):-
	write('\\\\'),
	nl,
%	write_list(Tab),
	write(I),
	write(X).
pplist_latex([],_,_).
pplist_latex([H|T],I,Tab):-
	write('\\\\'),
	nl,
%%	fs,
	pp(H),
%%	fsfs,
	I2 is I + 1,
	pplist_latex(T,I2,Tab).
pplist_latex('$VAR'(_),_,_).
*/

ppl2_latex([a(_Att,Thing)|Rest],Tab):-
	do_not_print(Thing),!,
	ppl2_latex(Rest,Tab).

ppl2_latex([a(Att,FS)|T],Tab):-
        !,
	write('\\\\'),
	nl,
        write_attribute(Att),
	write('&'),
        pp(FS),
        ppl2_latex(T,Tab).

ppl2_latex([],_).
ppl2_latex('$VAR'(_),_).

ppl_list(Head,Tail) :-
	write('$\\langle$'),
	ppx(Head),
	ppl_list(Tail),
	write('$\\rangle$').

ppl_list(V/y='R') :-
	!,
	write('$|$'),
	ppx(V/y='R').

ppl_list(V/YN='$VAR'(_)) :-
	!,
	write('$|$'),
	ppx(V/YN='$VAR'(_)).
	
ppl_list(_Var/_YN=[a(type,[[]])]) :-
	!.

ppl_list(_Var/_YN=[a(type,['.']),a(_,Head),a(_,Tail)]) :-
	write(','),
	ppx(Head),
	ppl_list(Tail).

ppx(ListEl) :-
	pp(ListEl),!.

ppx(_) :-
	write('\\_').

write_attribute(A) :-
%	write(' \\mbox{\\it '),
	tx_atom(A).
%	write('} ').

write_relation(A) :-
	write('\\mbox{\\tt '),
	tx_atom(A),
	write('}').

write_type_latex(A) :-
%	write(' \\mbox{\\sc '),
	tx_atom(A).
%	write('} ').


all_empty([]).
all_empty([a(_,H)|T]):-
        do_not_print(H),
        all_empty(T).
                         

do_not_print(_Var/n='$VAR'(_)). 



tex_end_line_avm :-
	write('\\end{avm}\\\\'),nl.

tex_begin_line_avm :-
	write('\\avmoptions{sorted}\\begin{avm}'),nl.


tex_var_avm(N) :-
	Letter is mod(N,26) + 0'A,
%%	write('\\@{'),
	write('\\mbox{\\rm\\small '),
	put_code(Letter),
	(   N>=26 ->
	    Rest is N//26, write('$_{'),write(Rest),write('}$')
	;   true
	),
	write('}'). 

start_docu :-
    hdrug_flag(tex_library,Texdir),
	format("
\\documentclass{article}
\\usepackage{~a/avm}
\\setlength{\\parindent}{-100pt}
\\avmfont{\\sc} 
\\avmvalfont{\\it} 
\\avmsortfont{\\scriptsize\\it}
\\begin{document}
\\thispagestyle{empty}
\\topmargin -100pt
",[Texdir]).

% and finish the document:
end_docu :-
        nl,write('\\end{document}'),nl.

