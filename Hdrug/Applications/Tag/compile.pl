%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% COMPILATION %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module( lists, library(lists), [ member/2, append/3 ]).


%% user provides definitions of elementary trees as
%% definitions for the predicate 
%% init(Name,Tree)
%% aux(Name,Tree)
%%
%% where Tree is Node:ListOfExpr
%%            or Node:TerminalSymbol
%% 
%% ListOfExpr is []
%%            or [Expr]
%%            or [Expr_1 .. + Expr_i .. Expr_n]
%%
%% Expr is * Node
%%      or = Node
%%      or   Tree
%%
%% Nodes are prolog-terms of which the exact form is determined by the user
%% as long as the following predicates are defined for such nodes:
%% address(Node,Address)   % that is, an argument position should be reserved for addresses
%% user_syn(Node,Syn)      % the information that is shared by bottom and top features
%% user_top(Node,Top)      % the top features of a node
%% user_bottom(Node,Top)   % the bottom features of a node
%% user_sem(Node,Sem)      % the semantics of a node
%%
%% some constriants:
%% no foot nodes in initial trees, one foot node in auxiliary trees
%% head-corner of root node of initial tree must be terminal symbol
%% head-corner of foot node of auxiliary tree is its foot node
%% 

compile_unit_tag_tree(init(Name,Tree),
	       [ init_rule( Top,Lex, Chain,Name),
             ign_init_rule(ITop,Lex,IChain,Name)]) :-
	compile_init(Tree,Top,Lex,Chain),
	ignore_sem(Top,ITop),
	ignore_sem_ch(Chain,IChain).

compile_unit_tag_tree(aux(Name,Tree),
               [ aux_rule(Foot,Top,Chain,Lexs,Name),
             ign_aux_rule(IFoot,ITop,IChain,Lexs,Name)]):-
	compile_aux(Tree,Foot,Top,Chain,Lexs),
	ignore_sem(Foot,IFoot),
	ignore_sem(Top,ITop),
	ignore_sem_ch(Chain,IChain).

:- multifile term_expansion/2.

term_expansion((init(A,B):-Body),Results):-
    call(Body),
    compile_unit_tag_tree(init(A,B),Results).
term_expansion((aux(A,B):-Body),Results):-
    call(Body),
    compile_unit_tag_tree(aux(A,B),Results).
term_expansion(init(A,B),Results):-
    compile_unit_tag_tree(init(A,B),Results).
term_expansion(aux(A,B),Results):-
    compile_unit_tag_tree(aux(A,B),Results).

compile_aux(Top:Ds,Foot,Top,Chain,Lexs) :-
	compile_ds(Ds,Top,foot(Foot),[],Chain,Lexs,[],0,1).

compile_init(Top:Ds,Top,Lex,Chain) :-
	compile_ds(Ds,Top,lex(Lex/_),[],Chain,_L,[],0,1).

compile_ds([],Cat,no_ds(Cat),I,I,L,L,A,_) :-
	address(Cat,A).
compile_ds(Ds,Cat,HCat,In,Out,L0,L,Aroot,A):-
	user_syn(Cat,_),
	address(Cat,Aroot),
	split(Ds,Left,Head,Right),
	compile_ds2(Left,LeftC,L0,L2,A,A2),
	A3 is A2 + 1,
	compile_ds2(Right,RightC,L2,L3,A3,_),
	compile_h(Head,HCat,[t(Cat,LeftC,RightC)|In],Out,L3,L,A2).

compile_ds2([],[],L,L,A,A).
compile_ds2([H|T],[H2|T2],L0,L,A0,A):-
	compile_n(H,H2,L0,L1,A0),
	A1 is A0 + 1,
	compile_ds2(T,T2,L1,L,A1,A).

compile_n(*C,subs_head(C,[]),L,L,A) :-
	address(C,A),
	user_syn(C,_). 
compile_n(=C,foot(C),L,L,A) :-
	address(C,A),
	user_syn(C,_).
compile_n(Cat:Ds,Node,L0,L,A):-
	A1 is (A*10) + 1,
	compile_ds(Ds,Cat,HCat,[],Chain,L0,L,A,A1),
	build_node(HCat,Chain,Node).

build_node(lex(Word),Chain,lex_head(Word,Chain)).
build_node(subs(Cat),Chain,subs_head(Cat,Chain)).
build_node(no_ds(Cat),Chain,e_head(Cat,Chain)).

compile_h(lex(W),lex(W),In,In,[W|L],L,_).
compile_h(*Head,subs(Head),In,In,L,L,A) :-
	address(Head,A),
	user_syn(Head,_).
compile_h(=Head,foot(Head),In,In,L,L,A) :-
	address(Head,A),
	user_syn(Head,_).
compile_h(Cat:Ds,HCat,In,Out,L0,L,A):-
	A1 is (A*10) + 1,
	compile_ds(Ds,Cat,HCat,In,Out,L0,L,A,A1).

split(Ds,Left,Head,Right):-
	append(Left,[+Head|Right],Ds),!.
split([D],[],D,[]).
split(W,[],lex(W/_),[]) :-
	\+ W=[],
	atomic(W).

%% ignore_sem(Node1,Node2)
%% unifies all information in Node1 and Node2 except the semantic information
%% uses the user-defined predicate ignore_semantics(Node1,Node2)
ignore_sem(Node,Node2) :-
	ignore_semantics(Node,Node2).
ignore_sem(lex_head(A,B),lex_head(A,BI)):-
	ignore_sem_ch(B,BI).
ignore_sem(subs_head(A,B),subs_head(AI,BI)):-
	ignore_sem(A,AI),
	ignore_sem_ch(B,BI).
ignore_sem(e_head(A,B),e_head(AI,BI)):-
	ignore_sem(A,AI),
	ignore_sem_ch(B,BI).

ignore_sem_ch([],[]).
ignore_sem_ch([t(Node,L,R)|T],[t(INode,IL,IR)|T2]):-
	ignore_sem(Node,INode),
	ignore_sem_l(L,IL),
	ignore_sem_l(R,IR),
	ignore_sem_ch(T,T2).

ignore_sem_l([],[]).
ignore_sem_l([H|T],[IH|IT]):-
	ignore_sem(H,IH),
	ignore_sem_l(T,IT).


compile_grammar :-
	compile(gram).

reconsult_grammar :-
	reconsult(gram).

compile_grammar_file(File) :-
	compile(File).

reconsult_grammar_file(File) :-
	reconsult(File).

