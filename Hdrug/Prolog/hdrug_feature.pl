%%%% file: feature.pl
%%%%
%%%% TYPES: a la HPSG, but implemented a la P-PATR and C. Mellish

%%%% Formally different from HPSG type system, but seems powerful enough
%%%% to implement most of the HPSG linguistics --- except for the
%%%% truly recursive uses (append)

:- module(hdrug_feature,
	  ['=>'/2, '<=>'/2, '==>'/2,          % basic feature constraints
	   '===>'/2, '<==>'/2,                % aliases for ==> and <=>
	   '=*>'/2,                           % ? not used?
	   '/=>'/2,                           % type inequality
	   '<?=?>'/2, '=?>'/2, '==?>'/2,      % if_defined version of basic constraints
	   is_defined/2,
	   if_defined/2, if_defined/3,        %
	   unify_except/3, unify_except_l/3,  % 'defaults'
	   unify_except_type/3, overwrite/4,  % 'defaults'
	   find_type/2, find_type/3,          % meta
	   subtype/3,                         % ? not used?
	   type_compiler/0, type_compiler/1,  % compilation
           type_compiler/2,
	   pretty_type/1,                     % pretty printing
	   change/6,                          % util for pretty printing
	   eval_b_type/2, give_boolean_type/2 % util pretty printing
	  ]).

%% TODO
%% . clearly distinguish compiler and predicates that it provides for usage
%% . usercall only in compiler, other calls all from some pre-defined module
%% . type_compiler asserts into that module _or_:
%% . term_expansion variant which loads predicates directly in that module (so they
%%   can be compiled)

:- expects_dialect(sicstus).

:- use_module( hdrug_feature_ops ).

:- use_module( library(lists),
	       [ substitute/4,select/3,member/2,append/3,nth/3 ] ).

:- use_module( hdrug_util ).

:- public
    help_info/4.
:- discontiguous
    help_info/4.

help_info(pred,pretty_type,"hdrug_feature:pretty_type(Type)",
"pretty prints information on Type. Types should have been compiled
with hdrug_feature:type_compiler.").  


help_info(pred,find_type,"hdrug_feature:find_type(?Term,-Types[,-Atts])",
"Types will be bound to the list of most informatives sub-types of
Term; Atts will be bound to the list of all attributes of
Term. Meta-logical. Types should have been compiled with
hdrug_feature:type_compiler."). 

help_info(pred,unify_except,
"hdrug_feature:unify_except(T1,T2,Path)",
"T1 and T2 are Prolog terms. Path is a sequence of attributes
separated by colons. The predicate evaluates T1:Path and T2:Path (in
order to ensure that Path is consistent with both
objects. Furthermore, T1 and T2 are unified except for the values at
T1:Path and T2:Path. Types should have been compiled with
hdrug_feature:type_compiler."). 

help_info(pred,unify_except_l,
"hdrug_feature:unify_except_l(T1,T2,ListOfPaths)",
"Similar to unify_except, except that the third argument now is a list
of paths. T1 and T2 are Prolog terms. Each path in ListOfPaths is a
sequence of attributes separated by colons. The predicate evaluates
for each Path, T1:Path and T2:Path (in order to ensure that Path is
consistent with both objects. Furthermore, T1 and T2 are unified
except for all values at T1:Path and T2:Path for Path in
ListOfPaths. Types should have been compiled with
hdrug_feature:type_compiler."). 

help_info(pred,overwrite,
"hdrug_feature:overwrite(T1,T2,Path,Type)", 
"Abbreviation for unify_except(T1,T2,Path), T2:Path => Type; i.e. T1
and T2 are identical, except that T2:Path is of type Type. Types
should have been compiled with hdrug_feature:type_compiler."). 

help_info(pred,'=>',
"hdrug_feature:(ObjPath => Type)",
"This predicate evaluates ObjPath, and assigns Type to the result
(i.e. the result is unified with the Prolog term representation of
Type). ObjPath is a Prolog term followed by a (possibly empty) list of
attributes separated by the colon :. A path such as X:syn:head:cat
refers to the cat attribute of the head attribute of the syn attribute
of X. Type must be a type (Prolog atom) or a boolean expression of
boolean types. Types should have been compiled with
hdrug_feature:type_compiler."). 


help_info(pred,'/=>',
"hdrug_feature:(ObjPath /=> Type)",
"This predicate evaluates ObjPath, and ensures that it is not of type
Type (i.e. the result is not allowed to subsume the Prolog term
representation of Type). ObjPath is a Prolog term followed by a
(possibly empty) list of attributes separated by the colon :. A path
such as X:syn:head:cat refers to the cat attribute of the head
attribute of the syn attribute of X. Type must be a type (Prolog atom)
or a boolean expression of boolean types. Types should have been
compiled with hdrug_feature:type_compiler. The implementation of this
construct uses delayed evaluation."). 


help_info(pred,'==>',
"hdrug_feature:(ObjPath ==> Term)",
"This predicate evaluates ObjPath, and unifies Term with the
result. ObjPath is a Prolog term followed by a (possibly empty) list
of attributes separated by the colon :. A path such as X:syn:head:cat
refers to the cat attribute of the head attribute of the syn attribute
of X. Term is an arbitrary Prolog term. This predicate is often used
to include arbitrary Prolog terms inside feature structures. You can
define a hook predicate catch_print_error/3 in order to define pretty
printing for such terms. Types should have been compiled with
hdrug_feature:type_compiler."). 

help_info(pred,'<=>',
"hdrug_feature:(ObjPathA <=> ObjPathB)",
"This predicate evaluates PathA and PathB, and unifies the
results. ObjPathA and ObjPathB each is a Prolog term followed by a
(possibly empty) list of attributes separated by the colon :. A path
such as X:syn:head:cat refers to the cat attribute of the head
attribute of the syn attribute of X. Types should have been compiled
with hdrug_feature:type_compiler."). 

help_info(pred,'<?=?>',
"hdrug_feature:(PathA <?=?> PathB)",
"This predicate uses the if_defined/2 construct in order to unify two
paths, provided each of the two paths is defined. It is defined by: 

        A <?=?> B :-
              if_defined(A,Val), 
              if_defined(B,Val).").

help_info(pred,is_defined,
"hdrug_feature:is_defined(Path,Bool)",
"This predicate evaluates Path. If this is possible (i.e. the attributes
are all appropriate) then Bool=yes. Otherwise Bool=no.
	 ").
	      
help_info(pred,if_defined,
"hdrug_feature:if_defined(Path,Val[,Default])",
"This predicate evaluates Path, and unifies the result with Val. If
the path cannot be evaluated (for instance because a feature is used
which is not appropriate for the given type) then the predicate
succeeds (in the binary case) or unifies Val with Default (in the
ternary case). For example: 

        if_defined(X:head:subcat,List,[]),

could be used as part of the definition of a valence principle, in
order to obtain the list value of the subcat attribute. However, for
categories which have no subcat attribute, List is instantiated to
[]. 
	       ").

help_info(pred,type_compiler,
"hdrug_feature:type_compiler[(Module)]","Compiles type declarations
(loaded in Module or user) into definitions for the predicates =>/2,
<=>/2, ==>/2, unify_except/3, overwrite/4. The type declarations
consist of definitions for the hook predicates top/1, at/1, type/3,
list_type/2, extensional/1, boolean_type/2, intensional/1. The top/1
declaration is required. 

top(Subtypes) is an abbreviation for type(top,[Subtypes],[]).

at(Type) is an abbreviation for type(Type,[],[]).

type(Type,[T0,..,Tn],Atts), where each Ti is atomic, is an abbrevation for 
type(Type,[[T0,..,Tn]],Atts).

Each type is specified by a list (conjunction) of lists (exclusive
disjunctions) of subtypes and a list of attributes. 

Objects of type
type(Type,[[A1..An],[B1..Bn],...,[Z1..Zn]],[Att1..Attn]) will be
represented by the Prolog term Type(Ai',Bi',..,Zi',Att1',..,Attn',_) 

For example, the declaration

        type(sign,[[basic,complex],[nominal,verbal]],[mor,sem])

implies that everything of type sign is represented with a term sign(BorC,NorV,Mor,Sem,_) where the first argument represents the first sub-type (basic or complex and any associated information with these subtypes), the second argument represents the second subtype (nominal or verbal), the third argument represents the value of the 'mor' attribute, and the fourth argument represents the value of the 'sem' attribute. The fifth argument is introduced in order that such objects are 'intensional': objects are identical only if they have been unified.

Assumptions:

'top' has no appropriate features, will always be denoted with Variable bottom has no appropriate features, will not be denoted -> failure hence top is only specified along one 'dimension' (use top/1).

Other types can be further specified along several dimensions, hence can have more than one subtype, at the same time. Subtypes of a type are mutually exclusive (in the example above, you cannot be both nominal and verbal). 

All types describe intensional objects (as in PATR II). For this purpose, during compilation an extra argument position is added to which you cannot refer. You can use extensional/1 for a specific type in order that this extra position is not added.

Boolean types.

The technique discussed in Chris Mellish' paper in Computational Linguistics is available to be able to express boolean combinations of simple types. First, boolean types are declared using the hook predicate boolean_type(Type,ListOfLists). For example, the declaration

        boolean_type(agr,[[1,2,3],[sg,pl],[mas,fem,neut]])

declares that objects of type 'agr' are elements of the cross-product of {1,2,3} x {sg,pl} x {mas,fem,neut}. Instead of simple types, boolean combinations are allowed, using the operators & for conjunction, ~ for negation and ; for disjunction.

        ?- X => (sg & ~fem ; pl).
        
        X = agr(0,_A,_B,_C,_C,_D,_E,_F,_G,_H,_H,_I,_J,_K,_L,_M,_M,_N,1) ? 

").


usercall(Call) :-
    hdrug_flag(hdrug_feature_module,Module),
    hook(Module:Call).

% some abbreviations...
define_type(top,[Subs],[],_,true) :-
	usercall(top(Subs)).

define_type(Type,Subs,Atts,_,true) :-
	usercall(type(Type,List,Atts)),
	list_to_subs(List,Subs).

define_type(Type,[],[],_,true) :-
	usercall(at(Type)).

define_type(Type,Subs,Atts,Var,ConstrOnVar) :-
    %% undocumented feature
    usercall(define_type(Type,Subs,Atts,Var,ConstrOnVar)).

x_define_type(A,B,C,D,E) :-
	define_type(A,B,C,D,E).
x_define_type('.',[],[H,T],_,true) :-
	usercall(list_type(H,T)).
x_define_type([],[],[],_,true) :-
	usercall(list_type(_,_)).

list_to_subs([H|T],[[H|T]]) :-
    atomic(H).
list_to_subs([[Hh|Ht]|T],[[Hh|Ht]|T]).
list_to_subs([],[]).

reset_type_compiler_start :-
    (   dynamic_pred(Pred),
	abolish_if_exists_and_make_dynamic(Pred),
	fail
    ;   true
    ).

dynamic_pred(btype/4).
dynamic_pred(has_type/3).
dynamic_pred(e/3).
dynamic_pred(not_e/2).
dynamic_pred(attribute/1).
dynamic_pred(xtype/4).
dynamic_pred(eval_atom/2).

real_dynamic_pred(negated_value/2).

:- dynamic negated_value/2.

reset_type_compiler :-
    (   dynamic_pred(Pred),
	abolish_if_exists(Pred),
	fail
    ;   true
    ),
    (   real_dynamic_pred(Pred),
	abolish_if_exists_and_make_dynamic(Pred),
	fail
    ;   true
    ).

abolish_if_exists_and_make_dynamic(F/A) :-
    functor(Term,F,A),
    (   current_predicate(F,Term)
    ->  abolish(F/A)
    ;   true
    ),
    %% hack to make the predicate known as dynamic!
    assertz(Term,Ref),
    erase(Ref).

abolish_if_exists(F/A) :-
    functor(Term,F,A),
    (   current_predicate(F,Term)
    ->  abolish(F/A)
    ;   true
    ).

type_compiler :-
    type_compiler(user).

type_compiler(Module) :-
    set_flag(hdrug_feature_module,Module),
    reset_type_compiler_start,
    check_supertype,
    first_phase,		% assert xtype/4, e/3
    add_list_type,
    second_phase,		% assert has_type/3
    third_phase,		% retracts e/3 and asserts e/3
				% ( with constraints instantiated)
    check_unique_types,
    compile_ifdef,              % new!
    compile_boolean_types.

type_compiler(Module,List) :-
    type_compiler(Module),
    type_compiler_listing(List).

type_compiler_listing(List) :-
    findall(Clause,type_compiler_clause(Clause),List),
    reset_type_compiler.

type_compiler_clause(hdrug_feature:(Head:-Body)) :-
    current_predicate(_,Head),
    predicate_property(Head,dynamic),
    clause(Head,Body,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% first phase %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the first phase takes the type definitions and compiles
% an xtype definition for each type. This xtype def contains
% a term that will be used as the internal rep of the type.
% However, not all associated constraints of the type are
% evaluated yet - these remain in the third argument pos. of
% xtype. In the second phase the constraints will be evaluated.
%
% Furthermore definition for e/3 is built which picks out the 
% value of an attribute for a given internal rep. Similarly, 
% associated constraints are not yet evaluated. In the third
% phase they are... 

% Probably something is wrong here, since
% the second phase uses e/3 definitions without constraints..
% Also note that modules & constraints is not taken care of.
% As long as constraints are simple type assignments this all
% seems to work.

% Note that if there weren't any constraints, the first phase
% would be all there is to it.

first_phase :-
	define_type(top,[List],[],_,true),
	assertz( xtype(top,Var,true,Var) ),
	first_phase(List,Term,Term,true,first).

check_duplicates(List,Type,Sub) :-
    length(List,L),
    sort(List,List2),
    length(List2,L2),
    (   L2 < L
    ->  hdrug_util:debug_message(0,
             "~n****~nerror: duplicate ~w in type ~w~n****~n",
                                    [Sub,Type])
    ;   true
    ).

first_phase([],_,_,_,_).
first_phase([H0|T],Term,Top,Cin,ForNf):-
	(  (   define_type(H0,HList,AList,Top,Cons)
	   ->  check_duplicates(HList,H0,subtypes),
               check_duplicates(AList,H0,attributes),
               length(HList,HL),
               length(SubTypes,HL),
               length(AList,AL),
               length(Attributes,AL),
               H0 =.. [H|Type],
               append(Type,SubTypes,For),
               append(For,Attributes,NewList0),
               f_or_nf(ForNf,H,NewList0,NewList),
               Term =.. [H|NewList],
               assertz( xtype(H0,Top,(Cin,Cons),SubTypes) ),
               P is HL + 1,
               assert_all_e(AList,P,Term,Top),
               subtypes(HList,1,Term,Top,(Cin,Cons))
	   ;  %%msg(['Type not defined: ',H0,nl])
	       format(user_error,"Type not defined: ~w~n",[H0])
           ),
	   fail
        ;  first_phase(T,Term,Top,Cin,ForNf)
        ).

% NOTE: extra argument position to represent
% 'reentrancy'. This position will never get
% instantiated, nor can constraints refer to it.
% the pretty printer detects such reentrancies.
f_or_nf(nfirst,_Type,L,L).
f_or_nf(first,H,L0,L) :-
	f_or_nf2(H,L0,L).

f_or_nf2(Type,L,L) :-
	usercall(extensional(Type)),!.
f_or_nf2(_Type,L0,L) :-
	append(L0,[_ReentVar],L).

assert_all_e([],_,_,_).
assert_all_e([H|T],P,Term,Top):-
	( arg(P,Term,El),
	  (   attribute(H)
	  ->  true
	  ;   assertz(attribute(H))
	  ),
	  assertz( e(H,Top,El) ),
	  fail
        ; P2 is P + 1,
	  assert_all_e(T,P2,Term,Top)
        ).

subtypes([],_,_,_,_).
subtypes([H|T],P,Term,Top,C):-
	( arg(P,Term,HArg),
	  first_phase(H,HArg,Top,C,nfirst),
	  fail
        ; P2 is P + 1,
	  subtypes(T,P2,Term,Top,C)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% second phase %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the second phase takes all xtype definitions and
% tries to evaluate the constraints of each one. The
% evaluated xtypes are asserted as has_type/3.
second_phase :-
	findall(xtype(Type,Term,Cons,Sub),xtype(Type,Term,Cons,Sub),Xtypes),
	eval_constraints(Xtypes).

% Non-deterministically picks an xtype-def whose constraints
% can be evaluated already. This gives rise to has_type def.
% that may be useful for other constraints! 
% If not all xtypes can have their constraints evaluated, this
% results in an error. This can happen e.g. if type definitions
% are associated with constraints that use that very same type:
% type(x,[],[],X,X:v => x). 
eval_constraints([]).
eval_constraints([Hlist|Tlist]):-
	(  ( select(xtype(A,B,Cons,Sub),[Hlist|Tlist],List2),
	     call(Cons))
	-> assertz(has_type(A,B,Sub)),
	   eval_constraints(List2)
        ;  %%msg(['Could not compile the following types:'|List]),
	   %%nl,
	   format(user_error,"Could not compile the following types: ~w~n",
                  [[Hlist|Tlist]]),
	  fail
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% third phase %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% after the second_phase the 'has_type' is correct (?)
%% however, the 'e' is not, because eval_path may
%% introduce types, hence it should do inheritance of
%% the extra constraints...

third_phase :-
	findall(e(A,B,C),retract(e(A,B,C)),List),
	assert_es(List).

assert_es([]).
assert_es([e(A,B,C)|T]):-
	functor(B,Type,_),
	has_type(Type,B,_),
	assertz(e(A,B,C)),
	assert_es(T).


compile_ifdef :-
    (	setof(B,C^e(A,B,C),Set),
	list_to_diff_conj(Set,B,Conj),
	assertz((not_e(A,B):-Conj)),
	fail
    ;   true
    ).

list_to_diff_conj([H|T],B,Conj):-
    list_to_diff_conj(T,H,B,Conj).

list_to_diff_conj([],H,B,not_type(H,B)).
list_to_diff_conj([N|T],H,B,(not_type(H,B),Conj)):-
    list_to_diff_conj(T,N,B,Conj).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% add_list_type %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% for ordinary prolog lists!


add_list_type :-
        % defines the attributes to point to head and tail
	usercall(list_type(Head,Tail)),  
	!,
	assertz(xtype([],[],true,[])),
	assertz(xtype('.',[_|_],true,[])),
	assertz(xtype('[|]',[_|_],true,[])),
	assertz(attribute(Head)),
	assertz(attribute(Tail)),
	assertz(e(Head,[H|_],H)),
	assertz(e(Tail,[_|T],T)).
add_list_type.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% error detection %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_supertype :-
	define_type(T,_,_,_,_),
	(  super_type(T)
	-> fail
	;  %% msg(['error: type ',T,' has no super type',nl]),
           format(user_error,"error: type ~w has no super type~n",[T]),
	   fail
	).
check_supertype.

super_type(top).
super_type(T):-
	define_type(_,L,_,_,_),
	member(S,L),
	member(T,S).

% to detect some errors:
check_unique_types :-
	define_type(T,_,_,_,_),
	(  unique_type(T)
	-> fail
	;  %%msg(['error: type ',T,' is not unique',nl]),
	   format(user_error,"error: type ~w is not unique~n",[T]),
	   fail
	).
check_unique_types.

unique_type(T):-
	bagof(X,htype(T,X),List),
	List=[_].

htype(T,X):-
	has_type(T,X,_).
htype(T,_X):-
	usercall(boolean_type(T,_)).
htype(T,_X):-
	usercall(boolean_type(_F,M)),
	member(S,M),
	member(T,S).


% find_type(?InternalRep,-Types)
% gives back the most informative types of InternalRep
% THIS IS CLEARLY META-LOGICAL
%
% unify_except(?Rep1,?Rep2,Path)
% unifies the internal reps Rep1 and Rep2, except for the value of
% the path Path.
% IS THIS LOGICAL?
%
% overwrite(?Rep1,?Rep2,Path,Val)
% unifies the internal reps Rep1 and Rep2, except for the value of
% the path path. Moreover, in Rep2 the value of Path is assigned Val.
% IS THIS LOGICAL?
%
% subtype(?Rep,+Type,?SubType(s))
% is true if Rep is of type Type and all info embedded under
% this type is ?SubType(s).
%
%
% To pretty print internal representations, use the package
% p_feature.pl tex_feature.pl

Path => Type :-
    (	var(Type)
    ->	format(user_error,"error: variable in type assignment: ~w~n",
	       [Path => Type]),
	fail
    ;	eval_path(Path,Val),
	eval_type(Type,Val)
    ).

Path ==> Val :-
	eval_path(Path,Val).

Path ===> Val :-
	eval_path(Path,Val).

Path1 <=> Path2 :-
	eval_path(Path1,Val),
	eval_path(Path2,Val).

Path1 <==> Path2 :-
	eval_path(Path1,Val),
	eval_path(Path2,Val).

Path1 =*> Call0 :-
	eval_path(Path1,Val),
	eval_macro(Call0,Val).

% eval_macro(+Macro,?Val)
eval_macro(M0,Val) :-
	M0 =.. [F|Tail],
	M =.. [F,Val|Tail],
	user:M.

eval_type(Type,Val):-
	has_type(Type,Val0,_),
	!,
	Val = Val0.

eval_type(Type,Val):-
	btype(Type,_,Val0,_),
	!,
	Val0=Val.

eval_type(Exp,Val):-
	eval_b_type(Exp,Val0),
	!,
	Val = Val0.

eval_type(NoType,_) :-
%	msg(['error: ',NoType,' is not a proper type',nl]),
	format(user_error,"error: ~w is not a proper type~n",[NoType]),
	fail.

% eval_path(?Path,?Val)

eval_path(Var,X):-  % path consisists of variable only
	var(Var),!,
	Var = X.

eval_path(@Macro,Val) :-
	!,
	eval_macro(Macro,Val).

eval_path('\`'Type,Val) :-
	!,
	eval_type(Type,Val).

eval_path(=Term,Term0) :-
	!,
	Term=Term0.

eval_path(Obj:Path,Val):-  % path starts with (instantiated) variable
	!,
	eval_path(Path,Obj,Val).

eval_path(X <=> Y, Val):-  % to allow A <=> B <=> C.
	!,
	eval_path(X,Val),
	eval_path(Y,Val).

%
%eval_path(X,Y):-
%	usercall(user_defined_eval(X)),!,
%	usercall(user_eval(X,Y)).

eval_path(X & Y,Val) :-
	!,
	eval_path(X,Val),
	eval_path(Y,Val).

eval_path(X && Y,Val) :-
	!,
	eval_path(X,Val),
	eval_path(Y,Val).

% catch_all
eval_path(X,X).   % path consists of instantiated variable only

% eval_path/3
eval_path(Var,_,_) :-
    var(Var),
    !,
    format(user_error,"error: variable path~n",[]),
    fail.
eval_path(PH:PT,Obj,Val):-
    !,
    is_attribute(PH),
    e(PH,Obj,V),
    eval_path(PT,V,Val).

eval_path(PH,Obj,Val):-
    is_attribute(PH),
    e(PH,Obj,Val).

is_attribute(PH) :-
    (	nonvar(PH),
	attribute(PH)
    ->	true
    ;	raise_exception(
          hdrug_error("attribute expected: ~w~n",[PH]))
    ).

%% attempt to get rid of efficiency problem of unify_except_list

unify_except(N1,N2,Path) :-
    unify_except_l(N1,N2,[Path]).

unify_except_l(N1,N2,Paths) :-
    construct_skel(Paths,Skel),
    unify_except2(Skel,N1,N2).

construct_skel([],_).
construct_skel([Path|T],Skel) :-
    Skel:Path <=> Val,		% Path should make sense,
    Val = '***',		% *** remembers which part not to unify
    construct_skel(T,Skel).

unify_except2(Val,N1,N2) :-
    (	var(Val)
    ->	N1=N2
    ;	Val == '***'
    ->	true
    ;	functor(Val,F,A),
	functor(N1,F,A),
	functor(N2,F,A),
	unify_except3(A,Val,N1,N2)
    ).

unify_except3(A,Val,N1,N2):-
    (	A < 1
    ->	true
    ;	arg(A,Val,ValArg),
	arg(A,N1,N1Arg),
	arg(A,N2,N2Arg),
	unify_except2(ValArg,N1Arg,N2Arg),
	A1 is A-1,
	unify_except3(A1,Val,N1,N2)
    ).


/*
%unify_except_l(?Node,?Node,ListofPaths).
unify_except_l(Node,Node,[]).
unify_except_l(N1,N2,[H|T]):-
	unify_except(N1,X,H),
	unify_except_l(X,N2,T).

unify_except(N1,N2,Path) :-
	N1:Path <=> _,      % make Path defined
        N2:Path <=> _,      % make Path defined
	Var:Path <=> Val,   % Path should make sense,
        Val = '***',        % *** remembers which part should not be unified
        unify_except2(N1,N2,Var).

unify_except2(N1,N2,N3):-
	var(N3),!,
	N1 = N2.
unify_except2(_,_,'***') :-
	!.
unify_except2(N1,N2,_):-
	var(N1),
	var(N2),!,
	N1 = N2.
unify_except2(N1,N2,N3):-
	var(N1),!,
	unify_except2(N2,N1,N3).
unify_except2(N1,N2,N3):-
	functor(N1,F,Ar),
	functor(N2,F,Ar),
	functor(N3,F,Ar),
	unify_except3(N1,N2,N3,Ar,0).

unify_except3(_,_,_,I,I):-
	!.
unify_except3(N1,N2,N3,I,J):-
	arg(I,N1,A1),
	arg(I,N2,A2),
	arg(I,N3,A3),
	unify_except2(A1,A2,A3),
	I2 is I-1,
	unify_except3(N1,N2,N3,I2,J).
*/

% overwrite(?InFS,?OutFS,+Path,+Type)
overwrite(FS,FS2,Path,Type) :-
	unify_except(FS2,FS,Path),
	FS2:Path => Type.

% find_type(Term,MinimalTypes)
% find_type(Term,MinimalTypes,Attributes)

find_type(Var,Top):-
	var(Var),!,
	Top = [top].
find_type(Term,T):-
	find_type(Term,T,_Atts).

% catches some cases of untyped that look typed:

find_type([],[[]],[]):-
	usercall(list_type(_,_)).
find_type([_|_],['.'],[H,T]):-
	usercall(list_type(H,T)).
find_type([_|_],['[|]'],[H,T]):-
	usercall(list_type(H,T)).
find_type(Term,_Ts,_):-
	functor(Term,F,_),
	x_define_type(F,_,_,_,_),
	x_define_type(top,S,_,_,_),
        \+ mem_mem(F,S),!,fail.
% otherwise:
find_type(Term,Ts,Atts):-
	find_type(Term,[top],Ts,[],Atts).  

mem_mem(F,S):-
	member(L,S),
	member(F,L),!.

find_type(Var,T,T,A,A):-
	var(Var),!.
find_type('$VAR'(_),T,T,A,A):-!.
find_type(Term,T1,T,A1,A):-
	Term =.. [Fun|Ar],
	x_define_type(F,Subs,Atts,_,_),
	F =.. [Fun|Begin],
	append(Begin,_Rest,Ar),
	replace_type(F,T1,T2),
	append(A1,Atts,A2),
	find_types(Subs,Term,T2,T,A2,A,1).

find_types([],_,T,T,A,A,_):-
	!.
find_types([_H|Ta],Term,T1,T,A1,A,P):-
	arg(P,Term,V),
	find_type(V,T1,T2,A1,A2),
	P2 is P + 1,
	find_types(Ta,Term,T2,T,A2,A,P2).

replace_type(New,Types,[New|Res]):-
	select(T,Types,Res),
	x_define_type(T,ConjSubs,_,_,_),
	member(Subs,ConjSubs),
	member(New,Subs),!.
replace_type(New,T,[New|T]).   % in case top type was already removed..

% subtype(?Rep,+Type,?SubType(s))
% is true if Rep is of type Type and all info embedded under
% this type is ?SubType(s).

subtype(Rep,Type,Sub) :-
	has_type(Type,Rep,Sub).

% unify_except_type(?Rep0,Rep,+Type).
% is true if Rep0 is of type Type and all info embedded under
% this type is forgotten in Rep.
unify_except_type(Rep0,Rep,Type) :-
	Rep0 => Type,     % 
	Rep => Type,      % 
	has_type(Type,_,Lt),   % in order to know how many types there are
	Rep0 =.. [_|L0],
	Rep  =.. [_|L],
	forget(Lt,L0,L).

forget([],Rep,Rep).
forget([_|T],[_|R0],[_|R]):-
	forget(T,R0,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% MELLISH' DISJUNCTION AND NEGATION OF ATOMIC TYPES %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
% package to compile expressions with negation and disjunction into
% 'Mellish-terms' (see Computational Linguistics 1988), and vice
% versa.
%
% note that this is NOT a seperate module
% declarations of the form boolean_type(Name,SetofSets)
% eg. boolean_type(agr,[[1,2,3],[sg,pl],[mas,fem,neut]])
%            
%
% after compilation of such boolean types 'compile_boolean_types'
% the following two predicates are useful:
%
% eval_b_type(+Exp,-MellishTerm)
%
% give_boolean_type(+MellishTerm,-Exp)
%
% 
% 
% syntax of Exp:  atom
%                 (Exp & Exp)
%                 ~Exp
%                 (Exp ; Exp)
%
% where all atoms occuring in an expression are taken from the same 
% boolean_type definition

compile_boolean_types:-
	(   usercall(boolean_type(Type,ListOfSets)),
	    hdrug_util:debug_message(1,"compiling ~w~n",[Type]),
	    ff1(ListOfSets,Ar),
	    Ar20 is Ar + 1,
	    ( usercall(intensional(Type)) -> Ar2 is Ar20 + 1 ; Ar2 is Ar20 ),
	    functor(Term,Type,Ar2),
	    arg(1,Term,0),
	    arg(Ar20,Term,1),
	    setof(X,poss(X,ListOfSets),List),
	    assertz(btype(Type,List,Term,ListOfSets)),
	    assert_eval_atom(ListOfSets,List,Term),
	    hdrug_util:debug_message(1,"compiled ~w~n",[Type]),
	    fail
        ;   check_duplicate_boolean_types
	).

assert_eval_atom(ListOfSets,List,Term) :-
    (   member(L,ListOfSets),
        member(Atom,L),
	atom(Atom),
	compute_eval_atom(List,1,2,Atom,Term),
	assertz(eval_atom(Atom,Term)),
	fail
    ;   true
    ).


check_duplicate_boolean_types :-
    (	btype(TypeA,Sub1,_,_),
	lists:member(S1,Sub1),
	lists:member(Type1,S1),
	dif(TypeA,TypeB),
	btype(TypeB,Sub2,_,_),
	lists:member(S2,Sub2),
	lists:member(Type1,S2),
	format(user_error,"error: ~w is a sub-type of both ~w and ~w~n",
	       [Type1,TypeA,TypeB]),
	fail
    ;	true
    ).

%%%%%%%%%%%%%%%%%%%
%%% eval_b_type %%%
%%%%%%%%%%%%%%%%%%%

%% new: it is now illegal to specify
%% a type which is always false or which is always
%% true
%% X => (sg;~sg) will raise an error
%%
%% this simplifies the code considerably
%% and in practice, such expressions are
%% typical mistakes

eval_b_type(Var,Var2):-
    var(Var),!,
    Var = Var2.

eval_b_type(Exp,Sign):-
    eval(Exp,Sign0),
    !,
    Sign0 = Sign.

eval((A&B),Sign):-
    eval(A,Sign),
    eval(B,Sign).

eval((A&&B),Sign):-
    eval(A,Sign),
    eval(B,Sign).

eval((A;B),Sign):-
    eval(~A,Sign1),
    eval(~B,Sign1),
    negate(Sign1,Sign).

eval(~A,Sign):-
    !,
    mem_eval(A,Sign).

eval(Atom,Sign):-
    eval_atom(Atom,Sign).

mem_eval(A,Term):-
    negated_value(A,Value),
    !,
    Value = Term.

mem_eval(A,Term):-
    eval(A,Sign1),
    negate(Sign1,Value),
    assertz_negated_value(A,Value),
    Term=Value.

%%% depending how the library is used, negated_value/2 may
%%% no longer be dynamic..., sigh, sigh
assertz_negated_value(A,Value) :-
    (   predicate_property(negated_value(_,_),compiled)
    ->  abolish_if_exists_and_make_dynamic(negated_value/2)
    ;   true
    ),
    assertz(negated_value(A,Value)).

compute_eval_atom([],_,_,_,_).
compute_eval_atom([H|T],I,J,Atom,Sign):-
    (   member(Atom,H)
    ->  true
    ;   arg(I,Sign,X),
	arg(J,Sign,X)
    ),
    K is J + 1,
    compute_eval_atom(T,J,K,Atom,Sign).

negate(Term,Term2):-
    functor(Term,F,A0),
    functor(Term2,F,A0),
    arg(1,Term2,0),
    arg(A0,Term2,1),
    negate1(A0,Term,Term2).

negate1(1,_,_):-
    !.
negate1(I,A,B):-
    J is I-1,
    arg(J,A,A1),
    arg(I,A,A2),
    arg(J,B,B1),
    arg(I,B,B2),
    negate2(A1,A2,B1,B2),
    negate1(J,A,B).

negate2(A1,A2,B1,B2) :-
    (   A1 == A2
    ->  true
    ;   B1 = B2
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% tools for compiler %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

poss([],[]).
poss([H|T],[L|L2]):-
	member(H,L),
	poss(T,L2).

ff1([],0).
ff1([H|T],Out):-
	ff([H|T],1,Out).

ff([],In,In).
ff([H|T],In,Out):-
	length(H,I),
	In2 is In * I,
	ff(T,In2,Out).

give_boolean_type(Obj,Exp2):-
	btype(Type,List,Obj,_),
	compall(List,Obj,[],Exp,1,2),
	rewrite_disj(Type,Exp,Exp2).

compall([],_Obj,E,E,_,_).
compall([H|T],Obj,In,Out,I1,I2):-
        I3 is I2 + 1,
	arg(I1,Obj,X),
	arg(I2,Obj,Y),
	(   X == Y,
            X \== '$VAR'('_')   % for prettyvar-ed avm's
        ->  Next = In
        ;   Next = [H|In]
        ),
        compall(T,Obj,Next,Out,I2,I3).

/* examples
t(A):-
	eval_b_type(A,X),
	give_boolean_type(X,Y),
	write(X),nl,
	write(Y),nl,fail.
	
t1(X,Y) :-
	eval_b_type((~2;masc),X),
	give_boolean_type(X,Y).


t2(X,Y) :-
	eval_b_type((2;(masc&sg)),X),
	give_boolean_type(X,Y).


t3(X,Y) :-
	eval_b_type(sg& ~1,X),
	give_boolean_type(X,Y).


t4(X,Y) :-
	eval_b_type(~ ((sg;masc)&(pl;fem)),X),
	give_boolean_type(X,Y).

tf(X,Y) :-
	eval_b_type(~ ((sg;masc)&(pl;fem)),X),
	give_boolean_type(X,Y).


t5(X,Y) :-
	eval_b_type(masc,X),
	give_boolean_type(X,Y).

t6 :- write((3;sg;(pl&1))),nl,t((3;sg;(pl&1))).

boolean_type(agr,[[1,2,3],[sg,pl],[mas,fem,neut]]).
*/

rewrite_disj(Type,P0,P):-
	btype(Type,_,_,Set),
	remove_unspecified_values(Set,P0,P1),
	(   P1 == [[]]
	->  P = Type
	;   write_as_disj(P1,P)
	).

%remove_doubles(X,Y):-
%	select(V,X,X2),
%	member(V,X2),!,
%	remove_doubles(X2,Y).
%remove_doubles(X,X).

%% 
%% if for some value each possibility is multiplied because
%% it is simply not known
%%
%% [[a,b,c],[x,y]]
%% instead a&x ;b&x ;c&x yields x
remove_unspecified_values(Tuples,I,Out):-
	select(F,I,In),
	find_candidate(Tuples,F,In,In2),!,
	remove_unspecified_values(Tuples,In2,Out).
remove_unspecified_values(_Tuples,I,I).

find_candidate(Tuples,F,In,[F2|Out]):-
	select(L,F,F2),
	find_tuple(L,Tuples,Others),
	remove_others(F,L,Others,In,Out).

find_tuple(L,Tuples,Others):-
	member(Tuple,Tuples),
	select(L,Tuple,Others).

remove_others(_F,_L,[],In,In).
remove_others(F,L,[H|T],In,Out):-
	%% replace(L,H,F,L2),!,
	substitute(L,F,H,L2),!,
	select(L2,In,In2),
	remove_others(F,L,T,In2,Out).

write_as_disj([H|T],Disj) :-
    write_as_disj(T,H,Disj).

write_as_disj([],H,H2) :-
    write_as_conj(H,H2).
write_as_disj([N|T],H,(H2;T2)) :-
    write_as_conj(H,H2),
    write_as_disj(T,N,T2).

write_as_conj([H|T],Conj) :-
    write_as_conj(T,H,Conj).

write_as_conj([],H,H).
write_as_conj([N|T],H,H&T2) :-
    write_as_conj(T,N,T2).

change_it_fs(clause(H0),clause(H),No) :-
    change_pretty_constraints([H0],[H],[],No).

change_it_fs(clause(H0,B0),clause(H,B),No) :-
    change_pretty_constraints([H0|B0],[H|B],[],No).

change_it_fs(value(FS0,C0),value(FS,C),No) :-
	change(FS0,FS,[],Out,No,OutNo),
	change_pretty_constraints(C0,C,Out,OutNo).
%
%change_it_fs(fs(FS0),fs(FS),No) :-
%	change(FS0,FS,[],_Out,No,_OutNo).

change_it_fs(value(FS0),value(FS),No) :-
    change(FS0,FS,[],_Out,No,_OutNo).

change_pretty_constraints([],[],_,_).
change_pretty_constraints([H|T],[NH|NT],In,InNo) :-
	change_pretty_constraint(H,NH,In,In2,InNo,InNo2),
	change_pretty_constraints(T,NT,In2,InNo2).

change_pretty_constraint(_Module:H,NH,In0,In,InNo0,InNo) :-
	!,
	change_pretty_constraint(H,NH,In0,In,InNo0,InNo).

change_pretty_constraint(when(_,H),NH,In0,In,InNo0,InNo) :-
	!,
	change_pretty_constraint(H,NH,In0,In,InNo0,InNo).

change_pretty_constraint(H,NH,In0,In,InNo0,InNo) :-
	H =.. [F|Args],
	change_pretty_arguments(Args,NArgs,In0,In,InNo0,InNo),
	NH =.. [F|NArgs].

change_pretty_arguments([],[],C,C,I,I).
change_pretty_arguments([H|T],[NH|NT],C0,C,I0,I):-
	change(H,NH,C0,C1,I0,I1),
	change_pretty_arguments(T,NT,C1,C,I1,I).

%%%%%%%%%%%%%%%%
% change does some things in parallel:
%  - find out about reentrancies: only first occurrence is printed
%    ( variables are only printed if there are being referred to)
%  - change datastructure back into something with explicit attributes etc.

change(Term1,Term2,InHis,OutHis,InNo,OutNo):-
	change(Term1,Term2,InHis,OutHis,InNo,OutNo,root).

% Atomic
% Integer/YesNo = Term
% Integer is some index
% YesNo if yes then it is an index which some other substructure refers to
% Term is a term, or special symbol 'R' when it should be skipped because
% already printed...

% intensional(Term)
% if intensional, then ordinary reentrancy labels if the same
% if extensional, then this is superfluous of course, so no reentrancy
%                 labels.
% Note that the in/extensionality of a type is defined by its highest
% supertype below top.
% Anything that contains variables is intensional in this sense.
intensional(Term) :-       % default
	\+ m_ground(Term).

m_ground(tree(_,_,_)).

m_ground(Term) :-
	atomic(Term),
	!.
m_ground('$VAR'(_)) :-
	!,
	fail.
m_ground(Term) :-
	functor(Term,_F,A),
	m_ground(A,Term).

m_ground(0,_Term) :- !.
m_ground(I,Term) :-
	arg(I,Term,Arg),
	m_ground(Arg),
	J is I-1,
	m_ground(J,Term).



% case 1: anonymous var
change('$VAR'('_'),_/n='$VAR'('_'),H,H,I,I,_) :-
	!.

%case 2: term is already there somewhere in history
change(Term1,P/y='R',HisList,HisList,I,I,_):-
	intensional(Term1),
%%	\+ Term1 = [],   % empty lists are not intensional
	member(P/y=Term1,HisList),!.

%case 3: term is not there but of type '$VAR'
% in that case there is some substructure referring to it, otherwise
% catched by case 1! However, this substructure could be dominated by something
% that already takes care of coindexing...
change('$VAR'(No),No/Y='$VAR'(No),Out,[No/Y='$VAR'(No)|Out],J,J,_) :-
	!.

%case 4: term is not there yet

change(T1,I/Y=[a(type,Types)|T2],H0,H,I,J,_):-
	find_type(T1,Types0,Atts),
	change_type(Types0,Types,[I/Y=T1|H0],H1),
	I2 is I + 1,
	change_ts(Atts,T1,T2,H1,H,I2,J),!.

% case 5: boolean type
change(Term,I/Y=[a('BOOLEAN',Type,Term)],H,[I/Y=Term|H],I,J,_):-
    J is I+1,
	functor(Term,Type,_),
	btype(_,_,Term,_),
	%% usercall(boolean_type(Type,_)),
	usercall(intensional(Type)),
	!.

% case 5: boolean type
change(Term,I/_Y=[a('BOOLEAN',Type,Term)],H,H,I,I,_):-
	functor(Term,Type,_),
	btype(_,_,Term,_),
	%% usercall(boolean_type(Type,_)),
	!.

change(tree(Cat,Mark0,Ds),I/_Y=[a(type,Mark),
                               a(0,Cat2) | Ds2], HI,HO,I,J,_Att) :-
        !,
	change_mark(Mark0,Mark),
        I2 is I + 1,
	change(Cat,Cat2,HI,H2,I2,I3,root),
        new_change_l(Ds,Ds2,H2,HO,I3,J,root,1).


change(lex(X),_/_=[a(type,[X])],H,H,I,I,_) :-
	!.

% case 7: UNTYPED!!:
change(Term0,I/_Y=[a('UNTYPED',Att,Term)],H0,H,I,I,Att) :-
	change_term(Term0,Term,H0,H).


change_type([Type0],[Type],H0,H) :-
	!,
	functor(Type0,F,Ar),
	functor(Type,F,Ar),
	change_term(Ar,Type0,Type,H0,H).
change_type(Types0,Types,H0,H) :-
	change_term(Types0,Types,H0,H).


change_mark(foot,[tree,foot]) :- !.
change_mark(subs,[tree,subs]) :- !.
change_mark(_,[tree]).


% case 1: anonymous var
change_term('$VAR'('_'),'$VAR'('_'),H,H) :-
	!.
% case 2: thing has been printed already
change_term(Term,'$VAR'(P),H,H) :-
%%	\+ atomic(Term),
        intensional(Term),
	member(P/y=Term,H),
	!.
% case 3 term not there yet but non-anonymous var
change_term('$VAR'(No),'$VAR'(No),Out,[No/_Yes = '$VAR'(No)|Out]) :-
	!.
% case 4 term not there yet: go down recursively
change_term(Term0,Term,H0,H) :-
	functor(Term0,F,Ar),
	functor(Term,F,Ar),
	change_term(Ar,Term0,Term,H0,H).

change_term(0,_,_,H,H) :-
	!.
change_term(I0,T0,T,H0,H) :-
	arg(I0,T0,A0),
	arg(I0,T,A),
	change_term(A0,A,H0,H1),
	I is I0 - 1,
	change_term(I,T0,T,H1,H).


new_change_l([],[],H,H,I,I,_,_Dno).
new_change_l([H|T],[a(Dno,H2)|T2],HI,HO,I,J,A,Dno):-
	change(H,H2,HI,Ho,I,I2,A),
	Dno2 is Dno + 1,
	new_change_l(T,T2,Ho,HO,I2,J,A,Dno2).

new_change_l('$VAR'(X),'$VAR'(X),H,H,I,I,_,_).

% change/7:
change_ts([],_T1,[],H,H,I,I).
change_ts([H|T],T1,[a(H,V2)|R],H1,HO,I,O):-
        e(H,T1,V1),
	change(V1,V2,H1,H2,I,I2,H),
        change_ts(T,T1,R,H2,HO,I2,O).






pretty_type(T) :-
	pretty_type_one(T,0),
	nl.

pretty_type_conj([],_).

pretty_type_conj([H],Tab) :-
	!,
	pretty_type_disj(H,Tab).

pretty_type_conj([H|T],Tab) :-
	nl,
	tab(Tab),
	write('{'),
	pretty_type_disj(H,Tab),
	pretty_type_conj2(T,Tab),
	nl,
	tab(Tab),
	write('}').

pretty_type_conj2([],_).
pretty_type_conj2([H|T],Tab):-
	nl,
	tab(Tab),
	write(and),
	pretty_type_disj(H,Tab),
	pretty_type_conj2(T,Tab).

pretty_type_disj([],_).

pretty_type_disj([H],Tab) :-
	!,
	pretty_type_one(H,Tab).

pretty_type_disj([H|T],Tab) :-
	nl,
	tab(Tab),
	write('{'),
	pretty_type_one(H,Tab),
	pretty_type_disj2(T,Tab),
	nl,
	tab(Tab),
	write('}').

pretty_type_disj2([],_).
pretty_type_disj2([H|T],Tab):-
	write(' OR '),
	pretty_type_one(H,Tab),
	pretty_type_disj2(T,Tab).

pretty_type_one(H,Tab):-
	define_type(H,List,Atts,_,_),
	nl,
	tab(Tab),
	write_type(H),
	write_atts(Atts),
	Tab2 is Tab + 5,
	pretty_type_conj(List,Tab2).

write_type(Type):-
	write(Type).

write_atts([]):-!.
write_atts(Atts):-
	concat_all(Atts,A,' '),
	tab(1),
	write('<'),
	write(A),
	write('>').

if_defined(Var,_) :-
    var(Var),!,
    format(user_error,"error: variable path~n",[]),
    fail.
if_defined(Obj:Path,Val):-
    !,
    if_defined(Path,Obj,Val,_Default).
if_defined(Obj,Obj).

if_defined(Var,_,_) :-
    var(Var),!,
    format(user_error,"error: variable path~n",[]),
    fail.
if_defined(Obj:Path,Val,Default):-
    !,
    if_defined(Path,Obj,Val,Default).
if_defined(Obj,Obj,_).

PathA =?> Type :-
    if_defined(PathA,Val),
    Val => Type.

PathA ==?> Term :-
    if_defined(PathA,Term).

PathA <?=?> PathB :-
    (	var(PathA)
    ->	PathA=Val
    ;   if_defined(PathA,Val)
    ),
    (	var(PathB)
    ->	PathB=Val
    ;   if_defined(PathB,Val)
    ).

if_defined(Var,Obj,Val,Default) :-
    (	var(Var)
    ->	format(user_error,"error: variable path~n",[]),
	fail
    ;   atomic(Var)
    ->  (   \+ e(Var,_,_)
	->  format(user_error,"error: ~w is not an attribute!~n",[Var]),
	    fail
	;   (   nonvar(Obj),
		e(Var,Obj,Val),
		!
	    ;   e(Var,Obj,Val)
	    ;	not_e(Var,Obj),
		Val=Default
	    )
	)
    ;   Var=(Att:Path),
	(   \+ e(Att,_,_)
	->  format(user_error,"error: ~w is not an attribute!~n",[Att]),
	    fail
	;   (   nonvar(Obj),
	        e(Att,Obj,Obj1),
		!,
		if_defined(Path,Obj1,Val,Default)
	    ;   e(Att,Obj,Obj1),
		if_defined(Path,Obj1,Val,Default)
	    ;	not_e(Att,Obj),
		Val=Default
	    )
	)
    ).

%% is_defined(Path,YesNo)

is_defined(Obj:Var,Bool) :-
    is_defined(Var,Obj,Bool).

is_defined(Var,Obj,Val) :-
    (	var(Var)
    ->	format(user_error,"error: variable path~n",[]),
	fail
    ;   Var=(Att:Path),
	(   nonvar(Obj),
	    e(Att,Obj,Obj1),
	    !,
	    is_defined(Path,Obj1,Val)
	;   e(Att,Obj,Obj1),
	    is_defined(Path,Obj1,Val)
	;   not_e(Att,Obj),
	    Val=no
	)
    ;   atomic(Var),
	(   nonvar(Obj),
	    e(Var,Obj,_),
	    !,
	    Val = yes
	;   e(Var,Obj,_),
	    Val = yes
	;   not_e(Var,Obj),
	    Val = no
	)
    ).


%% not_type(Type,Structure)
%% Structure should not be of type Type, i.e., Type should not
%% subsume Structure.
Path /=> Type :-
    TypeTerm => Type,
    eval_path(Path,Struct),
    not_type(TypeTerm,Struct).

:-block not_type(?,-).

not_type0(TypeTerm,Struct) :-
    (	var(TypeTerm)
    ->	fail
    ;	not_type(TypeTerm,Struct)
    ).
not_type(TypeTerm,Struct):-
    functor(TypeTerm,F,A),
    functor(Struct,F2,A2),
    (	F/A \== F2/A2
    ->	true
    ;	has_type(F,TypeTerm,SubTypeTerms),
	has_type(F,Struct,SubStructs),
	not_types(SubTypeTerms,SubStructs)
    ).

not_types([H|T],[H2|T2]) :-
    not_types(T,H,T2,H2).

% auxiliary to avoid choicepoint for last element
not_types(_,H,_,H2):-
    not_type0(H,H2).
not_types([H|T],_,[H2|T2],_):-
    not_types(T,H,T2,H2).

:- multifile user:help_info/4.

user:help_info(module,hdrug_feature,"The Hdrug Feature Library",

" The feature library provides extensive possibilities to compile
feature equations into Prolog terms, and to view such compiled Prolog
terms as feature-structures.  The motivation for such an approach
might be that you want feature structures for readability on the one
hand, but Prolog terms and Prolog unification of such terms for
effiency reasons internally. The package is heavily influenced by the
work of Chris Mellish.

Types

Before feature structures can be compiled into terms, a number of type
declarations need to be specified. The declarations that need to be
defined are top/1, type/3 and at/1. These three definitions define a
type hierarchy. This hierarchy has the shape of a tree. The top/1
definition defines the daughter nodes of the root of the tree. This
root is always called 'top'.

Attributes can be attached to a single type in the type hierarchy. If
a type is associated with an attribute then this attribute is
inherited by all of its subtypes. The top node of the type hierarchy
can be seen as a variable. You can not specify any attributes for this
type. The type/3 predicate defines for a given type (first argument) a
list of subtypes (second argument) and a list of attributes (third
argument).

The at/1 definitions define terminals of the tree that do not
introduce attributes. It is an abbreviation of a type/3 definition in
which the second and third argument are both the empty list.

As an example, consider the following type tree definition:

        top([boolean,sign,cat]).
         type(boolean,[+,-],[]).
          at(+).
          at(-).
         type(sign,[],[cat,phon,sem]).
         type(cat,[noun,verb],[agr]).
         type(noun,[],[pro]).
         type(verb,[],[aux,inv,subj]).

If this type definition is consulted by Hdrug, and if the directive:

        :- type_compiler.

is called, then it is possible to view the type definition by choosing
the 'view type tk' menu. This gives rise to a tree on the
canvas as http://www.let.rug.nl/~vannoord/Hdrug/Manual/type.png

The meaning of such a type tree can be understood as follows. The class
of objects is divided in three mutually exclusive subclasses, called
boolean, sign and cat. Objects of type boolean can be further
subdivided into classes + or -. Objects of type sign can be further
specified for a cat, phon or sem attribute. 

The meaning of this type tree can also be understood by looking at the
way in which objects of a certain type are represented as Prolog
terms. This is illustrated as
http://www.let.rug.nl/~vannoord/Hdrug/Manual/tree.png


Equational constraints

If the type definition is compiled, then the following predicates can
be used: <=>/2, =>/2, ==>/2.  The first predicate equates two *paths*,
the second predicate assigns a type to a path, and the third predicate
assigns an arbitrary Prolog term to a path.

A path is a Prolog term followed by a sequence of attributes,
seperated by a colon (:). Therefore, given the previous example of a
type tree, we can have the following equational constraint:

        X:cat => noun.
            X = sign(_H,cat(noun(_G,_F),_E,_D),_C,_B,_A)
        Y:cat:agr <=> Y:cat:subj:cat:agr.
            Y = sign(_O,cat(verb(_N,_M,_L,sign(_K,cat(_J,_E,_I),
                                       _H,_G,_F)),_E,_D),_C,_B,_A)
        Z:phon ==> [jan,kust,marie].
           sign(_D,_C,[jan,kust,marie],_B,_A)

Lists

You can add (ordinary Prolog) lists to your type tree by the simple
definition: 

        list_type(HeadAtt,TailAtt).

This will allow the use of attributes HeadAtt and TailAtt for
referring to parts of lists. Furthermore, lists of typed objects will
be shown appropriately. For example:

        [-user].
        | list_type(h,t).
        | {user consulted, 20 msec 48 bytes}
        ^D
        yes
        | ?- X:t:h:cat => verb.
        
        X = [_A,sign(_K,cat(verb(_J,_I,_H,_G),_F,_E),_D,_C,_B)|_L] ?
        
        yes
        | ?- X:t:h:cat => verb, show(fs,latex,[value(X)]).
        ....
        
        X = [_A,sign(_K,cat(verb(_J,_I,_H,_G),_F,_E),_D,_C,_B)|_L] ?




Extensionality

Direct subtypes of type 'top' are represented using an extra variable
position. This is to make sure that objects are only identical if they
have been unified. For some types this does not make much sense. Types
that you want to consider as 'extensional' in this way are to be
declared with the predicate extensional/1.  Boolean types (cf. below) are
extensional by default. Providing an intentional/1 definition makes a
boolean type intensional.

The following example illustrates the difference. Without the
extensional predicate we have:

        X:inv => -, X:aux => -, tty_fs(X).
         {verb}
         |aux <B> {-}
         |inv <B>.

After declaring that boolean and '-' be extensional types (and recompiling
the type tree), we get:

        X:inv => -, X:aux => -, tty_fs(X).
         {verb}
         |aux {-}
         |inv {-}.

The difference is that Hdrug does not show explicitly that the values
of aux and inv are the same in the second example. This is redundant
information because objects of extensional types always are the same
if they have the same information content.

Unify_except

The library provides the predicates unify_except/3, unify_except_l/3
and overwrite/4. The first argument takes two feature terms and a
path. The first and second argument are unified *except* for the value
at the path}.

As an example (assuming the simple type system given above), we
might have:

        | ?- unify_except(X,Y,cat:agr).
        
        X = sign(_G,cat(_F,_E,_D),_C,_B,_A),
        Y = sign(_G,cat(_F,_H,_D),_C,_B,_A) ?

The predicate unify_except_l is similar, except it takes a list of
paths rather than a single path as its third argument.  Finally, the
predicate overwrite/4 can be understood by looking at its definition:

        overwrite(FS,FS2,Path,Type) :-
                unify_except(FS2,FS,Path),
                FS2:Path => Type.

Find_type

The meta-logical predicates find_type/2 and find_type/3 can be used to
get the most specific type of a feature term. The first argument is
the feature term, the second argument is a list of most specific types
(for simple usage just consider the first element of this list). The
optional third argument is a list of attributes that are appropriate
for this type.  For example:

        | ?- X:agr <=> X:subj:agr, find_type(X,[Y|_]).
        
        X = cat(verb(_G,_F,_E,cat(_D,_B,_C)),_B,_A),
        Y = verb ?

It is clear that find_type/2,3 are meta-logical predicates by looking
at the following example, where the conjuncts are swapped:

        | ?- find_type(X,[Y|_]), X:agr <=> X:subj:agr.
        
        X = cat(verb(_G,_F,_E,cat(_D,_B,_C)),_B,_A),
        Y = top ? ;

Disjunction and Negation over Atomic Values

A special mechanism is provided for atomic values to allow for
disjunction and negation over such atomic values. These atomic values
are not declared in the type-system as shown above, but rather they
are introduced by the predicate boolean_type/2. The first argument of
this predicate is an identifier, the second argument of this predicate
is a list of lists that is understood as a set product.  For example,
agreement features could be defined as:

        boolean_type(agr,[[1,2,3],[sg,pl],[mas,fem,neut]])

So valid and fully specified values for agreement consist of an
element from each of the three lists. The syntax for type-assignment
is extended to include disjunction (';'), conjunction ('&') and
negation ('~') of types. For example, to express that X has either
singular masculine or not-second person agreement, we simply state:

        X => ( sg & mas ; ~2 ).

The following example illustrates the use of this package:

        | ?- [-user].
        | boolean_type(agr,[[1,2,3],[sg,pl],[mas,fem,neut]]).
        | {user consulted, 10 msec 368 bytes}
        
        yes
        | ?- type_compiler.
        
        yes
        | ?- X => ( sg & mas ; ~2 ).
        
        X = agr(0,_L,_K,_J,_I,_H,_G,_G,_G,_G,_G,_F,_F,_E,_D,_C,_B,_A,1) ?

The example shows how complex terms are created for such boolean
types. This is useful because disjunction and negation can be handled
by ordinary unification in this way. Luckily the pretty printing
routines will turn such complex turns back into something more
readible:

        | ?- X: agr => (sg & mas ; ~2 & neut), show(fs,latex,[value(X)]).

").


help_info(hook,top,"top(Subtypes)",

"Defines all sub-types of top as a list of atoms.").
 
help_info(hook,type,"type(Type,Subtypes,Attributes)",

"Defines a Type with Subtypes and Attributes. In general, Subtypes is
a list of list of types. If a list of types [T0..Tn] is given, then
this is automatically converted to [[T0..Tn]]. ").

help_info(hook,at,"at(Type)",

"Type is an atomic type, i.e. without any sub-types and without any
attributes.").

help_info(hook,list_type,"list_type(Head,Tail)",

"Declares Head and Tail to be the attributes to refer to the head and
the tail of objects of type 'list'.").

help_info(hook,extensional,"extensional(Type)",

"Declares Type to be an extensional type, i.e. no extra variable is
added to objects of this type; extensional objects are identical if
they have the same value for each of their attributes. Intensional
objects are identical only if they have been unified.").

help_info(hook,boolean_type,"boolean_type(Type,Model)",

"Declares Type to be a boolean type with Model as its model (list of
list of atoms). For instance, boolean_type(agr, [[1,2,3], [sg,pl],
[mas,fem,neut]]) defines that agr is such a boolean type.").

help_info(hook,intensional,"intensional(Type)",

"Type must be a boolean type. Boolean types are extensional by
default, unless this predicate is defined for them.").

help_info(class,pred,"Predicates",
"This section lists the predicates exported by the hdrug_feature
library.").  

help_info(class,hook,"Hook Predicates",
"This section lists the hook predicates used by the hdrug_feature
library.").  


