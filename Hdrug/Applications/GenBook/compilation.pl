%% GvN: if module is already known, only import predicates, but don't
%% check if a newer version exists. This is crucial for runtime systems
%% in which the library is not even around anymore!
:- use_module(terms,library(terms),all).

:- use_module([ transitive,
		restrict,
		database,
		packing
	      ]).

:- use_module( lists, library(lists), [ select/3,
					reverse/2,
					member/2,
					append/3 ]).

:- multifile term_expansion/2.

term_expansion( (( Mother ---> Ds) :- Calls),Rules) :-
    findall(Rule,xx(Mother,Ds,Calls,Rule),Rules).

term_expansion( (( Mother ----> Ds) :- Calls),Rules) :-
    findall(Rule,xxx(Mother,Ds,Calls,Rule),Rules).

term_expansion( ( Mother ---> Ds),Rules) :-
    findall(Rule,xx(Mother,Ds,true,Rule),Rules).

term_expansion( ( Mother ----> Ds),Rules) :-
    findall(Rule,xxx(Mother,Ds,true,Rule),Rules).


xx(Mother,[Word],Calls,Rule) :-
    !,
    call(Calls),
    gen_sym(Name,Word),
    packing:divide_synsem(Mother,[],Mother1,[]),
    both(lexicon([Word],Mother,Name),
	 ign_lexicon([Word],Mother1,Name),
	 Rule).

xx(Mother,Daughters,Calls,Rule) :-
    call(Calls),
    functor(Mother,F,_),
    gen_sym(Name,F),
    c_ds(Daughters,Ds,[]),
    packing:divide_synsem(Mother,Ds,Mother1,Ds1),
    both(rule(Mother,Ds,Name),
	 ign_rule(Mother1,Ds1,Name),
	 Rule).

xxx(Mother,[Word],Calls,Rule) :-
    !,
    call(Calls),
    gen_sym(Name,Word),
    packing:divide_synsem(Mother,[],Mother1,[]),
    both(lexicon([Word],Mother,Name),
	 ign_lexicon([Word],Mother1,Name),
	 Rule).

xxx(Mother:Pointer,Daughters,Calls,Rule) :-
    call(Calls),
    functor(Mother,F,_),
    gen_sym(Name,F),
    c_ds(Daughters,Ds,[]),
    packing:divide_synsem(Mother,Ds,Mother1,Ds1),
    both(rule(Mother,Ds,Name/Pointer),
	 ign_rule(Mother1,Ds1,Name/Pointer),
	 Rule).


both(A,_,A).
both(_,A,A).


c_ds(Var,[Var|D],D) :-
	var(Var),!.

c_ds((A,B),D0,D):-
	!,
	c_d(A,D0,D1),
	c_ds(B,D1,D).

c_ds(A,D0,D):-
	c_d(A,D0,D).

c_d(Var,[Var|D0],D0) :-
	var(Var),!.

c_d([],D,D):-
	!.
c_d(Node,[Node|D],D).


%%:- add_expansion(c_r).

% GIVEN:
%%%% lexicon(Word,Cat,Name)       
%%%% ign_lexicon(Word,Cat,Name)
%%%% rule(M,Ds,Name)
%%%% ign_rule(M,Ds,Name)

% PRODUCES:
%%%%     h_rule
%%%%     h_gap
%%%%     h_link
%%%% ign_h_rule
%%%% ign_h_gap
%%%% ign_h_link

%%%% gap
%%%% ign_gap

%%%%     l_rule
%%%%     l_link
%%%% ign_l_link
%%%% ign_l_rule

%%%%     r_rule
%%%% ign_r_rule

% lexicon, ign_lexicon, rule, ign_rule already exist.
% everything is in module user, so no recompilation if parser
% is reconsulted.
compile_grammar :-
        compile(d),
        load_grammar.

reconsult_grammar :-
        reconsult(d),
        load_grammar.

compile_grammar_file(File):-
        compile(File),
        load_grammar.

reconsult_grammar_file(File):-
        reconsult(File),
        load_grammar.

load_grammar(File_list) :-
	( member(File,File_list),
	  compile(File),
	  fail
        ; true ),
	load_grammar.

load_grammar :-
	compile_rules.

compile_rules :-
	hdrug_util:report_count_edges(user:rule(_,_,_)),
	hdrug_util:report_count_edges(user:ign_rule(_,_,_)),
	hdrug_util:report_count_edges(user:lexicon(_,_,_)),
	hdrug_util:report_count_edges(user:ign_lexicon(_,_,_)),
	compile_links,
	compile_ign_links,
	compile_h_rules,        % h_rule, h_gap
	compile_h_links,
	compile_ign_h_rules,    % ign_h_rule, ign_h_link, ign_h_gap
	compile_ign_h_links,
	compile_gaps,
	compile_ign_gaps,
	compile_h2_rules,
	compile_ign_h2_rules,
	compile_r_rules,
	compile_ign_r_rules,
	compile_l_rules,
	compile_l_links,
	compile_ign_l_rules,
	compile_ign_l_links,
	compile_bug.

:- dynamic h_rule/5, h_gap/2, h_link/2, gap/2, pre_ncr/1, cr/4, ncr/3,
           esem/2, ign_h_rule/4, ign_h_gap/2, h2_rule/4, ign_h2_rule/4,
	   l_rule/4, ign_l_rule/4, r_rule/4, ign_r_rule/4, ign_gap/2,
	   ign_link/2, link/2, ign_h_link/2.

compile_h_rules :-
%	abolish(h_rule/5),
%	abolish(h_gap/2),
	retractall(h_rule(_,_,_,_,_)),
	retractall(h_gap(_,_)),
	( rule(M,Ds,Name),
	  compile_hcr(Ds,M,Name,Rule),
	  assertz(Rule),
	  fail
        ; true
        ),
	hdrug_util:report_count_edges(user:h_rule(_,_,_,_,_)),
	hdrug_util:report_count_edges(user:h_gap(_,_)).

compile_h_links :-
				%abolish(h_link/2),
    retractall(h_link(_,_)),
    transitive(b_h_link,h_link,restrict_pair_h_link),
    hdrug_util:report_count_edges(user:h_link(_,_)).

compile_links :-
				%abolish(link/2),
    retractall(link(_,_)),
    transitive(b_link,link,restrict_pair_link),
    hdrug_util:report_count_edges(user:link(_,_)).

compile_ign_links :-
	%abolish(ign_link/2),
	retractall(ign_link(_,_)),
	transitive(b_ign_link,ign_link,restrict_pair_ign_link),
	hdrug_util:report_count_edges(user:ign_link(_,_)).

b_ign_link(Left,Mother):-
	ign_rule(Mother,[Left|_],_).
b_ign_link(X,X).

b_link(Left,Mother):-
	rule(Mother,[Left|_],_).
b_link(X,X).

restrict_pair_link(link(A1,B1), link(A2,B2)):-
	restriction(A1,A2),
	restriction(B1,B2).

restrict_pair_ign_link(ign_link(A1,B1), ign_link(A2,B2)):-
	restriction(A1,A2),
	restriction(B1,B2).

restrict_pair_h_link(h_link(x(A1,A2,A3),
                            x(B1,B2,B3)), 
                     h_link(x(Ar,A2,A3),
                            x(Br,B2,B3))) :-
	restriction(A1,Ar),
	restriction(B1,Br).

posi([],[],L,R,L,R).
posi([_|_],[],_,R,_,R).
posi([],[_|_],L,_,L,_).
posi([_|_],[_|_],_,_,_,_).

compile_hcr([],M,Name,h_gap(M,Name)).
compile_hcr([Hd|Td],M,Name,h_rule(Head,M,RevLs,Rs,Name)) :-
	find_syn_head([Hd|Td],M,Head,Ls,Rs,Name),
	reverse(Ls,RevLs).

find_syn_head(Ds,_M,Head,Ls,Rs,_/Pointer):-
	!,
	P is Pointer - 1,
	length(Ls,P),
	append(Ls,[Head|Rs],Ds).

find_syn_head(Ds,M,Head,Ls,Rs,_) :-
	append(Ls,[Head|Rs],Ds),
	syn_head(M,Head).

b_h_link(x(Head,L,R), x(Mother,LM,RM)):-
	h_rule(Head,Mother,Lefts,Rights,_),
	posi(Lefts,Rights,L,R,LM,RM).

b_h_link(x(MC,L,R),x(MC,L,R)).

compile_ign_h_rules :-
	%abolish(ign_h_rule/5),
	%abolish(ign_h_gap/2),
	retractall(ign_h_rule(_,_,_,_,_)),
	retractall(ign_h_gap(_,_)),
	( ign_rule(M,Ds,Name),
	  compile_ign_hcr(Ds,M,Name,Rule),
	  assertz(Rule),
	  fail
        ; true
        ),
	hdrug_util:report_count_edges(user:ign_h_rule(_,_,_,_,_)),
	hdrug_util:report_count_edges(user:h_gap(_,_)).

compile_ign_h_links :-
	%abolish(ign_h_link/2),
	retractall(ign_h_link(_,_)),
	transitive(ign_b_h_link,ign_h_link,restrict_pair_ign_h_link),
	hdrug_util:report_count_edges(user:ign_h_link(_,_)).

ign_b_h_link(x(Head,L,R), x(Mother,LM,RM)):-
	ign_h_rule(Head,Mother,Lefts,Rights,_),
	posi(Lefts,Rights,L,R,LM,RM).

ign_b_h_link(x(MC,L,R),x(MC,L,R)).

restrict_pair_ign_h_link(ign_h_link(x(A1,A2,A3),
                                    x(B1,B2,B3)), 
                         ign_h_link(x(Ar,A2,A3),
                                    x(Br,B2,B3))) :-
	restriction(A1,Ar),
	restriction(B1,Br).

compile_ign_hcr([],M,Name,ign_h_gap(M,Name)).
compile_ign_hcr([Hd|Td],M,Name,ign_h_rule(Head,M,RevLs,Rs,Name)) :-
	find_syn_head([Hd|Td],M,Head,Ls,Rs,Name),
	reverse(Ls,RevLs).

compile_l_links :-
	%abolish(l_link/2),
	retractall(l_link(_H,_G)),
	transitive(b_l_link,l_link,restrict_pair_l_link),
	hdrug_util:report_count_edges(user:l_link(_,_)).

compile_h2_rules :-
	%abolish(h2_rule/4),
	retractall(h2_rule(_,_,_,_)),
	( gap(H,_),
	  h_rule(H,M,Left,Right,_Name),
	  reverse(Left,Rev),
	  append(Rev,Right,Ds),
	  reverse(Ds,Ds2),
	  left_gap(Ds2,[A|B]),
	  gen_sym(Name,h2),
	  assertz(h2_rule(A,M,B,Name)),
	  fail
        ; true
        ),
	hdrug_util:report_count_edges(user:h2_rule(_,_,_,_)).

compile_ign_h2_rules :-
	%abolish(ign_h2_rule/4),
	retractall(ign_h2_rule(_,_,_,_)),
	( h2_rule(A,B,C,Name),
          packing:divide_synsem(B,[A|C],B1,[A1|C1]),
	  assertz(ign_h2_rule(A1,B1,C1,Name)),
	  fail
        ; true
        ),
	hdrug_util:report_count_edges(user:ign_h2_rule(_,_,_,_)).


compile_l_rules :-
	%abolish(l_rule/4),
	retractall(l_rule(_,_,_,_)),
	( rule(M,Ds,_Name),
	  left_gap(Ds,[H|T]),
	  gen_sym(Name,rl),
	  assertz(l_rule(H,M,T,Name)),
	  fail
        ; true
        ),
	hdrug_util:report_count_edges(user:l_rule(_,_,_,_)).

compile_r_rules :-
	%abolish(r_rule/4),
	retractall(r_rule(_,_,_,_)),
	( rule(M,Ds,_Name),
	  reverse(Ds,RevDs),
	  left_gap(RevDs,[H|T]),
	  gen_sym(Name,lr),
	  assertz(r_rule(H,M,T,Name)),
	  fail
        ; true
        ),
	hdrug_util:report_count_edges(user:r_rule(_,_,_,_)).

b_l_link(X,X).
b_l_link(H,M) :-
	l_rule(H,M,_,_).

restrict_pair_l_link(l_link(A,B), l_link(Ar,Br)) :-
	restriction(A,Ar),
	restriction(B,Br).

compile_ign_gaps :-
	%abolish(ign_gap/2),
	retractall(ign_gap(_,_)),
	( gap(A,Name),
	  packing:divide_synsem(A,[],A1,[]),
	  assertz(ign_gap(A1,Name)),
	  fail
        ; true ),
	hdrug_util:report_count_edges(user:ign_gap(_,_)).

compile_ign_l_links :-
	transitive(b_ign_l_link,ign_l_link,restrict_pair_ign_l_link),
	hdrug_util:report_count_edges(user:ign_l_link(_,_)).

compile_ign_l_rules :-
	%abolish(ign_l_rule/4),
	retractall(ign_l_rule(_,_,_,_)),
	( l_rule(A,B,C,Name),
          packing:divide_synsem(B,[A|C],B1,[A1|C1]),
	  assertz(ign_l_rule(A1,B1,C1,Name)),
	  fail
        ; true
        ),
	hdrug_util:report_count_edges(user:ign_l_rule(_,_,_,_)).

compile_ign_r_rules :-
	%abolish(ign_r_rule/4),
	retractall(ign_r_rule(_,_,_,_)),
	( r_rule(A,B,C,Name),
          packing:divide_synsem(B,[A|C],B1,[A1|C1]),
	  assertz(ign_r_rule(A1,B1,C1,Name)),
	  fail
        ; true
        ),
	hdrug_util:report_count_edges(user:ign_r_rule(_,_,_,_)).

b_ign_l_link(X,X).
b_ign_l_link(H,M) :-
	ign_l_rule(H,M,_,_).

restrict_pair_ign_l_link(ign_l_link(A,B), ign_l_link(Ar,Br)) :-
	restriction(A,Ar),
	restriction(B,Br).

compile_gaps :-
	clean_up_database(gap(_,_)),
	make_basic_gaps(Gaps),
	make_complex_gaps(Gaps),
	hdrug_util:report_count_edges(user:gap(_,_)).

% left_gap(+Ds,-Ds2) non-deterministically rewrites left-most daughter(s)
% as gap or not.
% note: do not produce empty rhs, because gap already exists:
% left_gap([],_) :- fail.

left_gap([H|T],[H|T]).
left_gap([H|T],T2) :-
	gap(H,_),
	left_gap(T,T2).

% make_basic_gaps
% simply asserts rules with nil string as gaps
make_basic_gaps(Gaps):-
	findall(Gap,make_basic_gap(Gap),Gaps).

make_basic_gap(M):-
	rule(M,[],Name), 
	assertz_most_general(gap(M,Name),no).

make_complex_gaps([]).
make_complex_gaps([H|T]):-
	make_c_gaps(H,Gaps),
	append(T,Gaps,T2),
	make_complex_gaps(T2).

make_c_gaps(H,Gaps):-
	findall(Gap,make_c_gap(H,Gap),Gaps).

make_c_gap(H,M):-
	rule(M,Ds,_),
	select(H,Ds,Ds2),
	algap(Ds2),
	gen_sym(Name,gp),
	assertz_most_general(gap(M,Name),no).

algap([]).
algap([H|T]):-
	gap(H,_),
	algap(T).

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% for generation %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile_bug :-
	retractall(pre_cr(_H,_M,_Ds,_N)),
	retractall(pre_ncr(_M,_Ds,_N)),
	%abolish(g_link/2),
	retractall(g_link(_H,_G)),
	( rule(M,Ds,Name),
	  bug_compile(M,Ds,Name,Rule),
	  assertz(Rule),
	  fail
        ; true
        ),
	( lexicon([Word],Cat,Name),
	  assertz(pre_ncr(node(Cat,[Word|Q],Q),[],Name)),
	  fail
	; true
        ),
	compile_empty_sem_away,
	transitive(b_g_link,g_link,restrict_pair).

restrict_pair(g_link(A,B), g_link(Ar,Br)) :-
	restriction(A,Ar),
	restriction(B,Br).

b_g_link(X,X).
b_g_link(H,M) :-
	cr(node(H,_,_),node(M,_,_),_,_).
b_g_link(H,M) :-
	head_gap(H,M).


bug_compile(M,_Ds,_Name,_Result) :-
	head_gap(_,M2),   % hence use head_gap instead of this gap
                          % to generate with.
	variant(M,M2),
	!,
	fail.
bug_compile(M,Ds,Name,Result) :-
        find_head(Hd,Ds,M,Head,OtherDs,P0,P),
	cr_or_ncr(Hd,Head,node(M,P0,P),OtherDs,Name,Result).


cr_or_ncr('not headed',_,M,Ds,Name,pre_ncr(M,Ds,Name)).

cr_or_ncr(headed,H,M,Ds,Name,pre_cr(H,M,Ds,Name)).

find_head('not headed',[],_,_,[],P,P).
find_head(headed,[Head|Others],M,node(Head,P0,P1),Others2,P0,P):-
	semantics(M,MSem),
	semantics(Head,HSem),
	MSem == HSem,
	!,
	cont(Others,Others2,P1,P).
find_head(X,[NH|Others],M,Head,[node(NH,P0,P1)|O],P0,P):-
	find_head(X,Others,M,Head,O,P1,P).



cont([],[],P,P).
cont([NH|Others],[node(NH,P0,P1)|O],P0,P):-
	cont(Others,O,P1,P).

% COMPILE_EMPTY_SEM_AWAY
% method is similar to compile_gaps_away
% a `gap' is a non-chain-rule of which the mother
% node has variable semantics. This predicate thus
% operates on the basis of chain/non-chain rules, and
% produces rules of these two kinds. A chain-rule is
% a rule of which one daughter shares its semantics with
% mother node.
compile_empty_sem_away :-
	clean_up_database(esem(_,_)),
	make_basic_esems(Esem),
	make_complex_esems(Esem),
	rule_compilation_wrt_esem.

rule_compilation_wrt_esem :-
	%abolish(cr/4),
	%abolish(ncr/3),
	retractall(cr(_H,_M,_Ds,_N)),
	retractall(ncr(_M,_Ds,_N)),
	( pre_cr(H,M,Ds,N),         % input
	  sel_esem(Ds,Ds2),
          assertz(cr(H,M,Ds2,N)), % output
	  fail
        ; true 
        ),
	( pre_ncr(M,Ds,N),          % input
          sel_esem(Ds,Ds2),
	  no_esem(M,Ds2),
	  assertz(ncr(M,Ds2,N)),     % output
	  fail
        ; true
        ).

no_esem(M,[]):-
	semantics(M,Var),
	var(Var),
	!,
	fail.
no_esem(_,_).

sel_esem([],[]).
sel_esem([node(H,P0,P)|T],T2):-
	sel_esem1(node(H,P0,P),T2,T3),
	sel_esem(T,T3).

sel_esem1(H,[H|T],T).
sel_esem1(H,T,T):-
	esem(H,_name).

n_semantics(node(M,_,_),Sem):-
	semantics(M,Sem).

make_basic_esems(Esems):-
	findall(Esem,make_basic_esem(Esem),Esems).

% note: only if rhs is empty. What about if it is not: these
% are found in n_semantics! Is this OK?
make_basic_esem(esem(Node,Name)):-
	ncr(Node,[],Name),
	n_semantics(Node,Sem),
	var(Sem),
	assertz_most_general(esem(Node,Name),no).

make_complex_esems([]).
make_complex_esems([H|T]):-
	make_c_esems(H,Esems),
	append(T,Esems,T2),
	make_complex_esems(T2).

make_c_esems(Esem,Esems):-
	findall(NewEsem,make_c_esem(Esem,NewEsem),Esems).

make_c_esem(Esem,esem(M,Name)):-
	n_semantics(M,Sem),
	ncr(M,Ds,Name),
	var(Sem),
	select(Esem,Ds,Ds2),
	all_esem(Ds2),
	assertz_most_general(esem(M,Name),no).

all_esem([]).
all_esem([H|T]):-
	esem(H,_),
	all_esem(T).

listing_compiled :-
	( a_compiled(Spec),
	  listing(Spec),  % Note: only makes sense, and only works
                          % in case definition wasn't already compiled!
          fail
        ; true ).

a_compiled(lexicon/3).
a_compiled(ign_lexicon/3).
a_compiled(rule/3).
a_compiled(ign_rule/3).
a_compiled(h_link/2).
a_compiled(ign_h_link/2).
a_compiled(link/2).
a_compiled(ign_link/2).
a_compiled(h_gap/2).
a_compiled(h_rule/5).
a_compiled(ign_h_rule/5).
a_compiled(l_link/2).
a_compiled(ign_l_link/2).
a_compiled(h2_rule/4).
a_compiled(ign_h2_rule/4).
a_compiled(l_rule/4).
a_compiled(ign_l_rule/4).
a_compiled(gap/2).
a_compiled(r_rule/4).
a_compiled(ign_gap/2).
a_compiled(ign_r_rule/4).
a_compiled(g_link/2).
a_compiled(cr/4).
a_compiled(ncr/3).
a_compiled(head_gap/2).

