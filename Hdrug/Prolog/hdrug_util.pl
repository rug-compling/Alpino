:- module( hdrug_util,
	   [ %%atom_length/2,
	     concat/3,  concat_all/2,  concat_all/3,
	     between/3, between/4,
	     atom_term/2, term_atom/2, term_atomq/2,
	     gen_sym/1, gen_sym/2,
	     count_edges/2, report_count_edges/1, report_count_edges_pred/1,
	     debug_call/2, debug_message/3, call_with_decreased_debug/1,
	     initialize_flag/2, hdrug_flag/1,
	     hdrug_flag/2, hdrug_flag/3, set_flag/2,
	     wr_flag/1, wr_flag/2, parse_flag/3,
	     flag_list/1, write_flag_list/0, write_flag_list/1,
	     prettyvars/1, prettyvars/3, un_prettyvars/2,
	     ensure_prolog_conjunction/2, prolog_conjunction/2,
	     prolog_disjunction/2,
	     portray_chars/1,
	     try_hook/1, try_hook/2, hook/1, hook/2,
	     if_gui/1, if_gui/2,
	     has_atmost_solutions/2,
	     gensym/1, freesym/1, resetsym/0,
	     user_confirmation/2,
	     system_type/1,
	     current_prefixop/3,current_postfixop/3,current_infixop/4,
	     compile_user_clause/0, compile_user_clause/1,
	     clause_predicate/2, clause_predicate_spec/2,
	     report_user_clause/0, a_user_clause/3,
	     copy_term/3
	   ]).

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- use_module(library(charsio)).
:- use_module(library(lists)).

:- multifile
    user:help_pred/3.
:- public
    user:help_pred/3.
:- discontiguous
    user:help_pred/3.

:- meta_predicate has_atmost_solutions(:,?), few_solutions(?,:,?).
:- meta_predicate if_gui(:), if_gui(:,:).
:- meta_predicate debug_call(?,:).
:- meta_predicate call_with_decreased_debug(:).
:- meta_predicate count_edges(:,?).
:- meta_predicate report_count_edges(:).
:- meta_predicate try_hook(:,:).
:- meta_predicate try_hook(:).
:- meta_predicate hook(:).
:- meta_predicate hook(:,:).
:- meta_predicate is_current_predicate(:).

user:help_pred(concat,"concat(Atom,Atom,Atom)",
"Two of the three arguments must be Prolog atoms. The print-name of
the third atom is the concatenation of the print names of the first
two atoms. Examples:

        | ?- concat(foo,bar,X).

        X = foobar ?

        yes
        | ?- concat(X,bar,foobar).

        X = foo ?

        yes
        | ?- concat(foo,X,foobar).

        X = bar ?
").

concat(A,B,C) :-
    atom_concat(A,B,C).  %% built-in as of 3.8

concat_all([],'').
concat_all([H|T],Atom):-
    concat_all2(T,H,Atom).

concat_all2([],H,H).
concat_all2([H|T],F,Atom) :-
    atom_concat(F,H,N),
    concat_all2(T,N,Atom).

concat_all([],'',_).
concat_all([H|T],Atom,Sep):-
    concat_all2(T,H,Atom,Sep).

concat_all2([],H,H,_).
concat_all2([H|T],F,Atom,Sep) :-
    atom_concat(F,Sep,F2),
    atom_concat(F2,H,N),
    concat_all2(T,N,Atom,Sep).

user:help_pred(concat_all,"concat_all(+ListOfAtoms,?Atom[,+Atom])",
"concetenates the print names of all the atoms in ListOfAtoms
together; possibly using the optional third argument as a
seperator. Example:

        ?- concat_all([foo,bar,foo,bar],L,'+').

        L = 'foo+bar+foo+bar' ?
").
user:help_pred(between,"between(+Lower, +Upper, ?Number[, +/-])",
"Is true when Lower, Upper, and Number are integers, and Lower =<
Number =< Upper.  If Lower and Upper are given,  Number can be tested
or enumerated.  If either Lower or Upper is absent, there is not
enough information to find it, hence failure.  Numbers are generated
in ascending order. If you want descending order, use between/4. The
optional fourth argument is the atom + to indicate ascending order, or
- to indicate descending order. Example:

        ?- findall(X,between(1,10,X), Xs).

        Xs = [1,2,3,4,5,6,7,8,9,10] ?

        ?- findall(X,between(1,10,X,-), Xs).

        Xs = [10,9,8,7,6,5,4,3,2,1] ?").

:- if(\+current_predicate(between/3)).
between(Edge1,Edge2,I) :-
    integer(Edge1),
    integer(Edge2),
    Edge1 =< Edge2,
    between(Edge1,Edge2,I,+).
:- endif.

%% between/4 as between/3, except that the fourth argument ('-' or
%% '+') indicates whether integers are generated in descending or
%% ascending order
between(Lower,Upper,I,Dir) :-
    (	integer(I)
    ->	Lower =< I,
	I =< Upper
    ;	between1(Dir,Lower,Upper,I)
    ).

%%  between1(Dir,Lower, Upper, Point)
%%   enumerates values of Point satisfying Lower =< Point =< Upper,
%%   where it is already known that Lower =< Upper and Point was a
%%   variable.  Dir is an atom ('-' or '+') indicating whether the
%%   integers are generated in descending or ascending order.

between1(+,L, U, L) :-
	L =< U.
between1(+,L, U, N) :-
	L < U,
	M is L+1,
	between1(+,M, U, N).
between1(-,L, U, U) :-
	L =< U.
between1(-,L, U, N) :-
	U > L,
	M is U-1,
	between1(-,L, M, N).

user:help_pred(atom_term,"atom_term(+Atom,?Term).",
"Atom is read-in as if it where a Prolog term. Example:

        | ?- atom_term('f(A,B,A)',L).

        L = f(_A,_B,_A) ?").

user:help_pred(term_atom,"term_atom(+Term,?Atom).",
"The Prolog term Term is turned into an atom, as if quotes were placed
around it. Example:

        | ?- term_atom(f(f(f(f))),L).

        L = 'f(f(f(f)))' ?

As is clear from the following example, the result is arbitrary in
case Term contains variables:

        ?- term_atom(f(_A,_B,_A),L).

        L = 'f(_83,_105,_83)' ?").

:- if(current_prolog_flag(dialect, swi)).

atom_term(Atom,Term) :-
    term_to_atom(Term, Atom).

term_atom(Term,Atom) :-
    format(atom(Atom), '~w', [Term]).

term_atomq(Term,Atom) :-
    term_to_atom(Term, Atom).

:- else.

atom_term(Atom,Term) :-
    concat(Atom,' .',Atom1),	% space, because Atom may end with
				% funny character
    name(Atom1,Chars),
    charsio:read_from_chars(Chars,Term).

term_atom(Term,Atom) :-
    charsio:format_to_chars('~w',[Term],Chars),
    atom_codes(Atom,Chars).

term_atomq(Term,Atom) :-
    charsio:format_to_chars('~q',[Term],Chars),
    atom_codes(Atom,Chars).

:- endif.

user:help_pred(gen_sym,"gen_sym(-Atom[,+Prefix])",
"A new atom Atom is generated. If Prefix is specified, then the print
name of Atom will start with Prefix.").

gen_sym(Atom) :-
    gen_sym(Atom,'').

:- if(current_prolog_flag(dialect, swi)).

gen_sym(Symbol, Prefix) :-
    gensym(Prefix, Symbol).

:- else.

%% 33554432
gen_sym(Symbol,Prefix):-
    (	bb_get(hdrug_gen_sym:Prefix,Integer)
    ->  true
    ;   Integer=0
    ),
    Next is Integer+1,
    bb_put(hdrug_gen_sym:Prefix,Next),
    format_to_chars('~w~w',[Prefix,Integer],SymbolChars),
    atom_codes(Symbol,SymbolChars).

:- endif.

report_count_edges_pred(Module:F/A) :-
    functor(Pred,F,A),
    count_edges(Module:Pred,Int),
    debug_message(1,"~w: ~w~n",[F/A,Int]).

user:help_pred(report_count_edges_pred,"report_count_edges_pred(:Spec)",
"Writes to standard output the number of times :Spec succeeds. Example:

        | ?- report_count_edges_pred(library_directory/1).

        library_directory/1: 2
").

user:help_pred(report_count_edges,"report_count_edges(:Goal)",
"Writes to standard output the number of times :Goal succeeds. Example:

        | ?- report_count_edges(lists:member(_,[a,b,c,d])).

        lists:member(_95,[a,b,c,d]) : 4").

report_count_edges(Pred) :-
    count_edges(Pred,I),
    format(user_error,"~w : ~w~n",[Pred,I]).

user:help_pred(count_edges,"count_edges(:Goal,?Int)",
"Int is an integer indicating the number of times Goal succeeds.").
count_edges(Pred,Int):-
    findall(_A,Pred,As),
    length(As,Int).

user:help_pred(debug_call,"debug_call(+Int,:Goal)",
"If Int is smaller or equal to the current value of hdrug_flag(debug), then
Goal is called. Used to wrap around debugging and continuation
calls. Larger values for Int indicate that the goal is executed less
often.").
debug_call(Level,Call) :-
    hdrug_flag(debug,Level0),
    (   Level0 == undefined    -> true
    ;   Level =< Level0        -> call(Call)
    ;   true
    ).

user:help_pred(call_with_decreased_debug,"call_with_decreased_debug(:Goal)",
"Call Goal with a lower value of the global variable Debug. The value is
decreased by one.").
call_with_decreased_debug(Call) :-
    hdrug_flag(debug,Level0),
    (   Level0 == undefined    -> call(Call)
    ;   Level0 =:= 0           -> call(Call)
    ;   Level0 > 0,
	Level is Level0 - 1,
	set_flag(debug,Level),
	call_cleanup(Call,set_flag(debug,Level0))
    ).

user:help_pred(debug_message,"debug_message(+Int,+FormatStr,+FormatArgs)",
"If Int is smaller or equal to the current value of hdrug_flag(debug), then
the goal format(user_error,FormatStr,FormatArgs) is executed.").

debug_message(Level,Format,Args) :-
    debug_call(Level,format(user_error,Format,Args)).

%% setting/reading global variables that have a single value
%% with hdrug_flag/2,3
user:help_pred(initialize_flag,"initialize_flag(+Flag,?Val)",
"Hdrug manages a number of global variables, called flags. This
predicate sets flag Flag to Val only if Flag is currently
undefined.").

user:help_pred(set_flag,"set_flag(+Flag,?Val)",
"Hdrug manages a number of global variables, called flags. This predicate sets flag Flag to Val.").

user:help_pred(hdrug_flag,"hdrug_flag(+Flag[,?OldVal[,?NewVal]])",
"Hdrug manages a number of global variables, called flags. This
predicate sets flag Flag to NewVal, unifying the old value with
OldVal. If only two arguments are given, then the flag is
unchanged. If only a single argument is given, then Flag is allowed to
be uninstantiated. It will be bound to all existing flags upon
backtracking.").

:- dynamic
    flag_value/2,    
    local_flag_value/2.
:- thread_local
    local_flag_value/2.
	       
%% variables are not touched
initialize_flag(Flag,Val) :-
    hdrug_flag(Flag,Old),
    (	Old == undefined	% if undefined
    ->	hdrug_flag(Flag,_,Val)	% change it
    ;	true			% else don't
    ).

set_flag(Flag,Val):-
    hdrug_flag(Flag,_Val,Val).

hdrug_flag(Flag) :-
    hdrug_flag(Flag, _).

hdrug_flag(Flag,Val) :-		% to read Val of variable Flag
    ground(Flag), !,
    (   local_flag_value(Flag,Val0)
    ->  Val = Val0
    ;   flag_value(Flag,Val)
    ->  true
    ;   Val = undefined
    ).

hdrug_flag(Flag,Val) :-
    (   local_flag_value(Flag,Val)
    ;   flag_value(Flag,Val),
	\+ local_flag_value(Flag,_)
    ).

%% note: only the first flag that matches is changed...
hdrug_flag(Flag,Old,New):-		% to set Flag from Old to New value
    (	ground(Flag)
    ->	true
    ;	hdrug_flag(Flag)	        % instantiate to existing flag...
    ),
    					% get old value and defining clause
    (	(   thread_self(main)
	;   thread_self(hdrug_gui)
	)
    ->	(   clause(flag_value(Flag,Old), true, Ref)
	->  true
	;   Old = undefined
	)
    ;	(   clause(local_flag_value(Flag,Old), true, Ref)
	->  true
	;   flag_value(Flag,Old)
	->  true
	;   Old = undefined
	)
    ),
    (	Old == New
    ->	true                            % no change
    ;	(   var(Ref)
	->  true
	;   erase(Ref)
	),
	(   New == undefined
	->  true
	;   (   (   thread_self(main)
		;   thread_self(hdrug_gui)
		)
	    ->	asserta(flag_value(Flag,New))
	    ;	asserta(local_flag_value(Flag,New))
	    ),
	    if_gui(hdrug_util:hook(hdrug_gui:tcl(
		"set flag(~q) {~q}",[Flag,New]))) % then send it to tk
	)
    ).

wr_flag(A):-
    hdrug_flag(A,V),
    format(user_error,"~w=~w~n",[A,V]).

wr_flag(Str,A):-
    hdrug_flag(A,V),
    format(Str,"~w=~w~n",[A,V]).

% un_prettyvars(+Term0,Term) melts a term with $VAR back into ordinary ones
user:help_pred(un_prettyvars,"un_prettyvars(+Term0,?Term)",
"Reverses the effect of prettyvars; i.e. all '$VAR'/1 terms are
replaced by corresponding variables.").
un_prettyvars(Term0,Term) :-
	un_prettyvars(Term0,Term,_).

un_prettyvars(Var0,Var,_) :-
	var(Var0),!,
	Var0=Var.
un_prettyvars('$VAR'(No),Var,Done) :-
	!,
	un_prettyvar(No,Var,Done).

un_prettyvars(Term0,Term,Done) :-
	functor(Term0,F,A),
	functor(Term, F,A),
	un_prettyvars(A,Term0,Term,Done).

un_prettyvars(0,_,_,_) :-
	!.
un_prettyvars(I,T0,T,D) :-
	arg(I,T0,A0),
	arg(I,T, A ),
	un_prettyvars(A0,A,D),
	I2 is I-1,
	un_prettyvars(I2,T0,T,D).

un_prettyvar('_',_,_) :-!.
un_prettyvar(N,V,D) :-
	member(N/V,D),!.

user:help_pred(prettyvars,"prettyvars(?Term)",
"Similar to the built-in numbervars, except that all variables which
only occur once in Term are replaced by '$VAR'('_').").
prettyvars(Term) :-
	prettyvars(Term,0,_).

prettyvars(Term,I0,I) :-
	prettyvars0(Term, Vars0, []),    % collect vars
	keysort(Vars0, Vars),            % then you only need to compare neighbors
	set_singleton_vars(Vars, I0, I). % to find out about singletion-vars

prettyvars0(Var) -->
	{var(Var)}, !, [Var-[]].
prettyvars0([X|Xs]) --> !,
	prettyvars0(X),
	prettyvars0(Xs).
prettyvars0(X) -->
	{functor(X, _, A)},
	prettyvars0(0, A, X).

prettyvars0(A, A, _) --> !.
prettyvars0(A0, A, X) -->
	{A1 is A0+1},
	{arg(A1, X, X1)},
	prettyvars0(X1),
	prettyvars0(A1, A, X).

set_singleton_vars([], No, No).
set_singleton_vars([X,Y|Xs], N0,N) :-
	X==Y, !,
	X='$VAR'(N0)-[],
	N1 is N0+1,
	set_singleton_vars(Xs, X, N1, N).
set_singleton_vars(['$VAR'('_')-[]|Xs], N0,N) :-
	set_singleton_vars(Xs, N0,N).

set_singleton_vars([X|Xs], Y, N0,N) :-
	X==Y, !,
	set_singleton_vars(Xs, Y, N0,N).
set_singleton_vars(Xs, _, N0,N) :-
	set_singleton_vars(Xs, N0,N).


% convert only if not already converted..
ensure_prolog_conjunction(A,B) :-
	prolog_conjunction(A,B),
	!.
ensure_prolog_conjunction(A,A).

user:help_pred(prolog_conjunction,
"prolog_conjunction(Conjunction, ListOfConjuncts)",
"handles the syntax of conjuncts.  This code wraps call(_) around
variables, flattens conjunctions to (A;(B;(C;(D;E)))) form, and drops
'true' conjuncts.").

prolog_conjunction(Conjunction, ListOfConjuncts) :-
	nonvar(Conjunction),
	functor(Conjunction, ',', 2),!,
	pl_explode(Conjunction, ',', 'true', L, []),
	ListOfConjuncts = L.
prolog_conjunction(Conjunction, ListOfConjuncts) :-
	pl_explode(ListOfConjuncts, ',', 'true', L0),
	pl_implode(L0, ',', 'true', Conjunction),
	!.

%% this last clause (and the previous cut)
%% was added by GvN, to allow
%% for conjunctions consisting of one conjunct
%%    the cut is necc. because atom can be `true' in which
%%    case I only want the prev. solution ([])
%% it's quite ugly, yes.
prolog_conjunction(Atom,List) :-
	nonvar(Atom),
	List = [Atom].

user:help_pred(prolog_disjunction,
"prolog_disjunction(Disjunction,ListOfDisjuncts)",
"handles the syntax of disjuncts.  This code wraps call(_) around
variables, flattens disjunctions to (A,(B,(C,(D,E)))) form, and drops
'false' disjuncts.").
prolog_disjunction(Disjunction, ListOfDisjuncts) :-
	nonvar(Disjunction),
	!,
	functor(Disjunction, ';', 2),
	pl_explode(Disjunction, ';', 'fail', L, []),
	ListOfDisjuncts = L.
prolog_disjunction(Disjunction, ListOfDisjuncts) :-
	pl_explode(ListOfDisjuncts, ';', 'fail', L0),
	pl_implode(L0, ';', 'fail', Disjunction).

%   pl_explode(Form, Op, Zero, L0, L)
%   flattens a binary tree built using Op into a list between L0 and L,
%   eliminating Zero nodes, and wrapping call(_) around variable nodes.

pl_explode(V, _, _, [call(V)|L], L) :-
	var(V),
	!.
pl_explode(Z, _, Z, L, L) :- !.
pl_explode(F, O, Z, L0, L) :-
	functor(F, O, 2),
	arg(1, F, A),
	arg(2, F, B),
	!,
	pl_explode(A, O, Z, L0, L1),
	pl_explode(B, O, Z, L1, L).
pl_explode(F, _, _, [F|L], L).

%   pl_explode(List, Op, Zero, L)
%   flattens each of the elements of List using pl_explode/5
%   and forms the result into a big list L.

pl_explode([], _, _, []) :- !.
pl_explode([H|T], O, Z, L0) :-
	pl_explode(H, O, Z, L0, L),
	pl_explode(T, O, Z, L).

%   pl_implode(List, Op, Zero, Tree)
%   forms the list [F1,...,Fn] into the tree Op(F1,Op(...Op(_,Fn))).

pl_implode([], _, Z, Z).
pl_implode([H|T], O, _, Form) :-
	pl_implode2(T, O, H, Form).

pl_implode2([], _, F, F).
pl_implode2([H|T], O, X, F) :-
	functor(F, O, 2),
	arg(1, F, X),
	arg(2, F, Y),
	pl_implode2(T, O, H, Y).

user:help_pred(try_hook,"try_hook(:Goal[,:Goal])",
"Tries to call Goal, but only if the predicate is known to exist. If
the first Goal fails, or if it does not exist, then the second goal is
called. If no second goal is given then the predicate succeeds.").


try_hook(Call) :-
    try_hook(Call,true).

try_hook(Call,Alt) :-
%    format("try_hook: ~w ~w~n",[Call,Alt]),
    (	is_current_predicate(Call)
    ->	if(Call,true,Alt)
    ;   call(Alt)
    ).

user:help_pred(hook,"hook(:Goal).",
"hook/1 calls its argument, but only if it is defined; if it is not
defined the precate fails. Useful to call optional hook predicates for
which no undefined predicate warnings should be produced.").

user:help_pred(hook,"hook(:Goal,:Goal2).",
"hook/1 calls its argument, but only if it is defined; if it is not
defined, Goal2 is called. ").

hook(Call) :-
    is_current_predicate(Call),
    Call.

hook(Call,Call2) :-
%    format("hook: ~w ~w~n",[Call,Call2]),
    (   is_current_predicate(Call)
    ->  Call
    ;   Call2
    ).

:- if(current_prolog_flag(dialect,swi)).
is_current_predicate(Module:Call) :-
    functor(Call, Name, Arity),
    current_predicate(Module:Name/Arity).
:- else.
is_current_predicate(Module:Call) :-
    (	current_predicate(_,Module:Call)
    ->  true
    ;	predicate_property(Module:Call,imported_from(NewModule)),
	is_current_predicate(NewModule:Call)
    ).
:- endif.

:- multifile user:portray/1.

%user:portray(Atom) :-
%    atom(Atom),
%    atom_length(Atom,Len), Len > 40,
%    !,
%    sub_atom(Atom,0,10,_,Prefix),
%    format("~a...{~w}",[Prefix,Len]).

user:portray(Term) :-
    hdrug_util:portray_chars(Term).

%   by R. O'Keefe
%   portray_chars(Chars)
%   checks whether Chars is a non-empty plausible list of character codes.
%   If it is, it prints the characters out between double quotes.
%   THIS IS A DEBUGGING AID.  Control characters are written out in ^X
%   form, rather than using \x.  If the list ends with a variable or
%   $VAR(_), that is written out as |_X, which will not, of course, be
%   read back.  That's ok, it's just for looking at.

portray_chars(Chars) :-
    Chars = [_|_],		% a non-empty list
    plausible_chars(Chars),	% of plausible characters
    put_code(0'"),   %"
    portray_chars_(Chars),
    put_code(0'").   %"

portray_chars_(Var) :-
    var(Var),
    !,
    put_code(0'|),
    write(Var).
portray_chars_([]).
portray_chars_('$VAR'(N)) :-
    put_code(0'|),
    write('$VAR'(N)).
portray_chars_([Char|Chars]) :-
    (	Char =:= 0'"               % "
    ->	put_code(0'\\), put_code(0'")        % " is written \"
    ;	put_code(Char)
    ),
    portray_chars_(Chars).

plausible_chars(Var) :-
	var(Var), !.
plausible_chars('$VAR'(_)).
plausible_chars([]).
plausible_chars([Char|Chars]) :-
    integer(Char),
    %% removed control char's; they are typically not part of a string
    Char >= 32,
    Char =< 255,
    plausible_chars(Chars).

user:help_pred(if_gui,"if_gui(:Goal[,:AltGoal])",
"calls Goal only if graphical user interface is currently running; if
not the predicate calls AltGoal, if it is specified, or succeeds").
if_gui(Call) :-
    if_gui(Call,true).

%% solves Call if gui is running, solves Else otherwise
if_gui(Call,Else) :-
    (   hdrug_flag(gui,started)
    ->  call(Call)
    ;   call(Else)
    ).


%%% this must be below auxiliary definitions.

flag_list(KeyVals) :-
    findall(Flag-Val,hdrug_flag(Flag,Val),KeyVals).

write_flag_list :-
    (	wr_flag(_),
	fail
    ;   true
    ).

write_flag_list(Str):-
    (	wr_flag(Str,_),
	fail
    ;   true
    ).

%% succeeds if Goal has at most N solutions (Goal will not be bounded)

:- if(current_prolog_flag(dialect, swi)).

has_atmost_solutions(Goal,N) :-
    State = count(0),
    call(Goal),
    arg(1, State, C0),
    (	C0 == N
    ->	!, fail
    ;	C1 is C0+1,
	nb_setarg(1, State, C1),
	fail
    ).
has_atmost_solutions(_, _).

:- else.

has_atmost_solutions(Goal,N) :-
    gensym(L),
    bb_put(few_solutions:L,0),
    few_solutions(L,Goal,N).

few_solutions(L,Goal,N):-
    call(Goal),
    bb_get(few_solutions:L,Old),
    New is Old+1,
    bb_put(few_solutions:L,New),
    Old=:=N,   % more than N solutions
    !,
    bb_delete(few_solutions:L,_),
    freesym(L),
    fail.      % hence fail
few_solutions(L,_,_) :-
    freesym(L),
    bb_delete(few_solutions:L,_).

:- endif.

gensym(Integer) :-
    (   bb_get(gensym,Integer0)
    ->  Integer is Integer0+1
    ;   Integer = 0
    ),
    bb_put(gensym,Integer).
freesym(_).

resetsym :-
    bb_put(gensym,0).

user_confirmation(Msg,Format) :-
    if_gui(hdrug_gui:gui_user_confirmation(Msg,Format),
	   txt_user_confirmation(Msg,Format)
	  ).

txt_user_confirmation(Msg,Format) :-
    format(user_error,Msg,Format),
    get_code(C), get_code(_),  % swallow return too.
    confirmation_char(C).

confirmation_char(121).
confirmation_char(106).
confirmation_char(74).
confirmation_char(89).

%% system_type(?Val) where Val is development or runtime

:- if(current_prolog_flag(language, sicstus)).
system_type(Val) :-
    prolog_flag(system_type,Val).
:- else.
system_type(development).
:- endif.

current_prefixop(C,D,E) :-
    current_op(D,fx,C),
    E is D+1.
current_prefixop(C,D,D) :-
    current_op(D,fy,C).

current_postfixop(C,D,E) :-
    current_op(D,xf,C),
    E is D+1.
current_postfixop(C,D,D) :-
    current_op(D,yf,C).

current_infixop(C,D,E,F) :-
    current_op(E,xfx,C),
    D is E-1,
    F is E-1.
current_infixop(C,D,E,E) :-
    current_op(E,xfy,C),
    D is E-1.
current_infixop(C,E,E,F) :-
    current_op(E,yfx,C),
    F is E-1.
current_infixop(C,E,E,E) :-
    current_op(E,yfy,C).

:- if(current_prolog_flag(language, sicstus)).
flag(X) :- hdrug_flag(X).
flag(X,Y) :- hdrug_flag(X,Y).
flag(X,Y,Z) :- hdrug_flag(X,Y,Z).

thread_self(main).

:- endif.

%% USER_CLAUSE/2
user:help_pred(compile_user_clause,
	  "compile_user_clause[(Module)]",
"This predicate will construct Module:user_clause/2 definitions based
on the available Module:clause/2 clauses (if no Module is specified,
user is assumed). In the body of these clauses feature constraints are
expanded out. The user_clause predicate is used for graphical display
of predicates defined in the grammar. Each module is inspected for
potential user_clause/2 definitions.").

%:- meta_predicate call_constraints(:,?,?).
%
%:- meta_predicate call_constraints0(:,?,?).
%
%:- meta_predicate call_constraint(:,?,?,?).

clause_predicate(Module,Head) :-
    Module:current_predicate(_,Head),
    (   Module:predicate_property(Head,dynamic)
    ->  true
    ;   Module:predicate_property(Head,interpreted)
    ->  true
    ),
    \+ Module:predicate_property(Head,imported_from(_)),
    \+ Head = user_clause(_,_).

clause_predicate_spec(Module,F/A) :-
    clause_predicate(Module,Head),
    functor(Head,F,A).

compile_user_clause :-
    compile_user_clause(user).

compile_user_clause(Module) :-
    debug_message(2,"Creating user_clauses..~n",[]),
    abolish(Module:user_clause/2),
    % retractall(Module:user_clause(_,_)),
    (	clause_predicate(Module,Head),
	catch(Module:clause(Head,Body0),_,fail),
	debug_message(3,"expanding:~q~n",[(Head:-Body0)]),
	(   allows_expansion(Body0)
	->  call_constraints(Body0,BodyList,Module)
	;   prolog_conjunction(Body0,BodyList)
	),
	debug_message(3,"expanded:~q~n",[(Head:-BodyList)]),
	assertz(Module:user_clause(Head,BodyList)),
	fail
    ;   true
    ),
    report_count_edges_pred(Module:user_clause/2),
    debug_message(2,"Creating user_clauses done~n",[]).

allows_expansion(Goal) :-
    (	var(Goal)
    ->	fail
    ;   Goal=(G1,G2),
	allows_expansion(G1),
	allows_expansion(G2)
    ;   Goal=when(_,_)
    ;   Goal=dif(_,_)
    ;   \+ predicate_property(Goal,built_in)
    ).


expansion_constraint('==>'(Path,_)) :- proper_path(Path).
expansion_constraint('=>'(Path,V)) :-
    proper_path(Path),
    nonvar(V).
expansion_constraint('<=>'(P1,P2)) :-
    proper_path(P1),
    proper_path(P2).
expansion_constraint('<?=?>'(P1,P2)) :-
    proper_path(P1),
    proper_path(P2).
expansion_constraint(if_defined(P,_)) :-
    proper_path(P).
expansion_constraint(if_defined(P,_,_)) :-
    proper_path(P).
expansion_constraint('/=>'(Path,T)) :-
    proper_path(Path),
    nonvar(T).
expansion_constraint(unify_except(P1,P2,Path)) :-
    proper_path(P1),
    proper_path(P2),
    ground(Path).
expansion_constraint(unify_except_l(P1,P2,Path)) :-
    proper_path(P1),
    proper_path(P2),
    ground(Path).
expansion_constraint(overwrite(P1,P2,Path1,Path2)) :-
    proper_path(P1),
    proper_path(P2),
    ground(Path1),
    ground(Path2).

proper_path(V) :-
    var(V),!.
proper_path(_:Atts) :-
    ground(Atts).

%% use solutions of constraints only if not more than four, otherwise
%% leave the clause alone.
call_constraints(Goal,List,Module) :-
    (	has_atmost_solutions(call_constraints0(Goal,_,Module),4)
    ->	call_constraints0(Goal,List,Module)
    ;   prolog_conjunction(Goal,List)
    ).

call_constraints0((G1,G2),List,Module) :-
    !,
    call_constraint(G1,List0,List,Module),
    call_constraints0(G2,List0,Module).
call_constraints0(G1,List,Module) :-
    call_constraint(G1,[],List,Module).

call_constraint(H,Rest0,Rest,Module) :-
    (	expansion_constraint(H)
    ->  Rest0=Rest,
	Module:call(H)  %% might succeed multiple times
    ;   Rest=[H|Rest0]
    ).

:- public report_user_clause/0.
report_user_clause :-
    (	bagof(Head/Body,a_user_clause(F/A,Head,Body),List),
	length(List,Len),
	format("~w: ~w~n",[F/A,Len]),
	fail
    ;   findall(_,a_user_clause(_,_,_),List),
	length(List,Len),
	format("total: ~w~n",[Len])
    ).

a_user_clause(F/A,Head,Body) :-
    current_module(Module),
    hook(Module:user_clause(Head,Body)),
    functor(Head,F,A),
    F/A \== user_clause/2.

:- if(current_prolog_flag(language,sicstus)).

copy_term(Term,Copy,Cons) :-
    call_residue(copy_term(Term,Copy),KeyCons),
    vals(KeyCons,Cons).

vals([],[]).
vals([_-H|T0],[H|T]) :-
    vals(T0,T).

:- endif.

:- if(current_prolog_flag(language,sicstus)).

parse_flag(Thing,Flag,Val) :-
    catch(atom_term(Thing,Term),
	  syntax_error(_,_,_,_,_),
	  fail
	 ),
    nonvar(Term),
    Term=(Flag=Val).

:- else.

parse_flag(Thing,Flag,Val) :-
    catch(atom_term(Thing,Term),
	  error(syntax_error(_),_),
	  fail
	 ),
    nonvar(Term),
    Term=(Flag=Val).

:- endif.
