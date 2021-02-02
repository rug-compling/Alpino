%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tree format definitions %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- use_module(library(lists)).

:- multifile
    graphic_path/3,
    graphic_label/3,
    graphic_daughter/4,
    show_node/3,
    show_node2/3,
    show_node3/3,
    tk_tree_show_node_help/2.

:- discontiguous
    graphic_path/3,
    graphic_label/3,
    graphic_label_rest/3,
    graphic_daughter/4,
    show_node/3,
    show_node2/3,
    show_node3/3,
    tk_tree_show_node_help/2.

%% FORMAT SYN

graphic_path(syn,Result,Tree) :-
    alpino_data:result_term(_,_,_,Tree,_,Result).

graphic_daughter(syn,No,tree(_,_,[Dh|Dt],_),D) :-
    nth(No,[Dh|Dt],D).
graphic_daughter(syn,1,tree(_,_,lex(Ref),_),lex(Ref)).

graphic_label(syn,lex(Ref),L) :-
    lex_surf(Ref,L).

graphic_label(syn,tree(Sign,_,_,_),SynLabel) :-
    (	hdrug_feature:find_type(Sign,[Label0|_]) % use type as label in syntax trees
    ->  shorter(Label0,Label),
	(   alpino_data:slash(Sign,[Slash])
	->  hdrug_feature:find_type(Slash,[SlLabel|_]),
	    SynLabel = Label/SlLabel
	;   SynLabel = Label
	)
    ;	functor(Sign,SynLabel,_)
    ).

shorter(optpunct,L) :-
    !,
    L='O'.
shorter(punct,L) :-
    !,
    L='P'.
shorter(optpunct(E),L) :-
    !,
    L=o(E).
shorter(L,L).

%% what happens if you click on a node of a tree
show_node(syn,tree(Sign0,_Id,_,_),Medium) :-
    copy_term(Sign0,Sign,Cons),
    show(fs,Medium,[clause(value(Sign),Cons)]).

%%% if you press button 2 on a node label:  RULE
show_node2(syn,tree(_,Rule,_,_),Medium) :-
    nonvar(Rule),
    (	alpino_lc_in:grammar_rule(Rule,_,_)
    ->	findall(Clause,a_rule(Rule,Clause),Clauses),
	show(fs,Medium,Clauses)
    ;	show_lex(Rule,fs,Medium) % pretty.pl
    ).

show_node3(Type,Tree,Medium) :-
    show_node2(Type,Tree,Medium).

%% display what would happen if you'd click on a node
tk_tree_show_node_help(syn,'<1> clig <2/3> rule clig').

%% FORMAT MATRIX(SYN)

graphic_path(matrix(syn),Result,Tree) :-
    alpino_data:result_term(_,_,_,Tree,_,Result).

graphic_daughter(matrix(syn),No,tree(_,_,Ds,_),D) :-
    nth(No,Ds,D).

graphic_label(matrix(syn),lex(Ref),L) :-
    lex_surf(Ref,L).
graphic_label(matrix(syn),tree(L,_,_,_),L).

%% FORMAT DERIV

graphic_path(deriv,Result,Tree) :-
    alpino_data:result_term(_,_,_,Tree,_,Result).

graphic_daughter(deriv,No,tree(_,_,[Dh|Dt],_),D) :-
    nth(No,[Dh|Dt],D).
% graphic_daughter(deriv,1,tree(_,_,lex(Ref)),lex(Ref)).

graphic_label(deriv,lex(Ref),L) :-
    lex_surf(Ref,L).
graphic_label(deriv,tree(_,Name0,_,_),Name) :-
    shorter(Name0,Name).

%% what happens if you click on a node of a tree
show_node(deriv,tree(Sign,_,_,_),Medium) :-
    show(fs,Medium,[value(Sign)]).

%%% if you press button 2 on a node label:  RULE
show_node2(deriv,tree(_,Rule,_,_),Medium) :-
    nonvar(Rule),
    (	alpino_lc_in:grammar_rule(Rule,_,_)
    ->	findall(Clause,a_rule(Rule,Clause),Clauses),
	show(fs,Medium,Clauses)
    ;	show_lex(Rule,fs,Medium) % pretty.pl
    ).

%% display what would happen if you'd click on a node
tk_tree_show_node_help(deriv,'<1> clig <2> rule clig').

%% FORMAT DERIV0 (DERIV WITH FULL LEXICAL DETAILS)

graphic_path(deriv0,Result,Tree) :-
    alpino_data:result_term(_,_,_,Tree,_,Result).

graphic_daughter(deriv0,No,tree(_,_,[Dh|Dt],_),D) :-
    nth(No,[Dh|Dt],D).
graphic_daughter(deriv0,1,tree(_,_,lex(Ref),_),Ref).

lex_surf(ref(_,_,_,Surf,_,_,_,_,_,_,_),Surf) :- !.
lex_surf(ref(_,_,_,Surf,_,_,_,_,_,_),Surf) :- !.
lex_surf(L,skip(L)). % skipped nodes in robust.pl

graphic_daughter(deriv0,1,ref(Class,Tag,W0,W1,P0,P1,Q0,Q1,His,N,_),
		 ref2(Class,Tag,W0,W1,P0,P1,Q0,Q1,His,N)).

graphic_label(deriv0,ref(_,Tag,_,_,_,_,_,_,_,Nth1,_),Tag/Nth1).
graphic_label(deriv0,tree(_,Name,_,_),Name) :-
    nonvar(Name).
graphic_label(deriv0,ref2(_,_,A,B,C,D,E,F,G,_),ref(A,B,C,D,E,F,G)).
graphic_label(deriv0,tree(Label,_,_,_),Label).
graphic_label(deriv0,X,X).

%% what happens if you click on a node of a tree
show_node(deriv0,tree(Sign,_,_,_),Medium) :-
    show(fs,Medium,[value(Sign)]).
show_node(deriv0,ref2(A,B,C,D,E,F,G,H,I,J),Medium) :-
    findall(clause(value(Cat),Constraints),
      (  alpino_lexical_analysis:get_lref(ref(_,A,B,C,D,E,F,G,H,I,J),_P0,_P,Cat0,_Frame),
	 copy_term(Cat0,Cat,Constraints)
      ), Values),
    show(fs,Medium,Values).
show_node(deriv0,ref(A,B,C,D,E,F,G,H,I,J),Medium) :-
    findall(clause(value(Cat),Constraints),
      (  alpino_lexical_analysis:get_lref(ref(_,A,B,C,D,E,F,G,H,I,J),_P0,_P,Cat0,_Frame),
         copy_term(Cat0,Cat,Constraints)
      ), Values),
    show(fs,Medium,Values).

%%% if you press button 2 on a node label:  RULE
show_node2(deriv0,tree(_,Rule,_,_),Medium) :-
    nonvar(Rule),
    (	alpino_lc_in:grammar_rule(Rule,_,_)
    ->	findall(Clause,a_rule(Rule,Clause),Clauses),
	show(fs,Medium,Clauses)
    ;	show_lex_id(Rule,fs,Medium)  % pretty.pl
    ).

%% display what would happen if you'd click on a node
tk_tree_show_node_help(deriv0,'<1> clig <2> rule clig').


%% FORMAT DERIV1 (DERIV WITH SOME LEXICAL DETAILS)

graphic_path(deriv1,Result,Tree) :-
    alpino_data:result_term(_,_,_,Tree,_,Result).

graphic_daughter(deriv1,No,tree(_,_,[Dh|Dt],_),D) :-
    nth(No,[Dh|Dt],D).
graphic_daughter(deriv1,1,tree(_,Name,lex(_),_),xxx(Name)).

graphic_label(deriv1,xxx(Name),Name).
graphic_label(deriv1,tree(_,_,lex(ref(_,Tag,_,_,_,_,_,_,_,_,_)),_),Tag).
graphic_label(deriv1,tree(_,Name,_,_),Name).

%% FORMAT DTS
graphic_path(dts,Result,Tree) :-
    result_to_dt(Result,Tree).  % dt.pl

graphic_label(dts,tree(r(Rel,Label),_,_),Rest) :-
    graphic_label_rest(Label,Rel,Rest).

graphic_label(dts,word(W0/_),W) :-
    root_of_pair(W0,W).

graphic_label_rest(i(X),Rel,Label) :-
    short_pair(Rel,X,Label).
graphic_label_rest(p(X),Rel,L) :- extract_cat(X,Y), short_pair(Rel,Y,L).
graphic_label_rest(l(_,_,_),Rel,Rel).
graphic_label_rest(i(I,p(X)),Rel,L) :- extract_cat(X,Y), short_pair(Rel,I:Y,L).
graphic_label_rest(i(I,l(_,_,_)),Rel,L) :- short_pair(Rel,I,L).

extract_cat(Term,F) :-
    (   var(Term)
    ->  true
    ;   functor(Term,F,_)
    ).

graphic_daughter(dts,1,tree(r(_,l(_,_,Word)),_,[]),word(Word)).
graphic_daughter(dts,1,tree(r(_,i(_,l(_,_,Word))),_,[]),word(Word)).
graphic_daughter(dts,N,tree(_,_,Ds),D) :-
    nth(N,Ds,D).

%% FORMAT DT dts with sense
graphic_path(dt,Result,Tree) :-
    result_to_dt(Result,Tree).  % dt.pl

graphic_label(dt,tree(r(Rel,Label),_,_),Rest) :-
    graphic_label_rest(Label,Rel,Rest).

graphic_label(dt,sense(W),W).

graphic_daughter(dt,1,tree(r(_,l(Frame,_,Word0/_)),_,[]),sense(Sense)) :-
    root_of_pair(Word0,Word),
    alpino_treebank:frame2sense(Word,Frame,Sense).
graphic_daughter(dt,1,tree(r(_,i(_,l(Frame,_,Word0/_))),_,[]),sense(Sense)) :-
    root_of_pair(Word0,Word),
    alpino_treebank:frame2sense(Word,Frame,Sense).
graphic_daughter(dt,N,tree(_,_,Ds),D) :-
    nth(N,Ds,D).


%% FORMAT DTP, DT with positions
graphic_path(dtp,Result,Tree) :-
    result_to_dt(Result,Tree).  % dt.pl

graphic_label(dtp,tree(r(Rel,Label),_,_),Rest) :-
    graphic_label_rest(Label,Rel,Rest).

graphic_label(dtp,word(A-B-W0/_L),A-B-W) :-
    root_of_pair(W0,W).
graphic_label(dtp,position([P0,_]),P0).

graphic_daughter(dtp,1,tree(r(_,l(A,B,Word)),_,[]),word(A-B-Word)).
graphic_daughter(dtp,1,tree(r(_,i(_,l(A,B,Word))),_,[]),word(A-B-Word)).
graphic_daughter(dtp,N,tree(_,_,Ds),D) :-
    nth(N,Ds,D).
graphic_daughter(dtp,1,word(_-_-_/L),position(L)).

%% FORMAT USER(DT)  DT WITH NICE BOXES
graphic_path(user(dt),Result,Tree) :-
    result_to_dt(Result,Tree).  % dt.pl

graphic_daughter(user(dt),N,tree(_,_,Ds),D) :-
    nth(N,Ds,D).

graphic_label(user(dt),tree(Label,_,_),Label).

%% for user(dt) pretty formats, definition of required hooks:
%% NB: formats supported: only CLiG and DOT!

%% OUTPUT_tree_user_node
%% CLIG
clig_tree_user_node(r(Rel,Label)) -->
    format_to_chars(" { drs { plain-text { ~w } } ",[Rel]),
    clig_dt_node(Label,Rel),
    format_to_chars(" }  ",[]).

clig_dt_node(i(I),_) -->
    format_to_chars(" { bold-text { ~w } } ",[I]).
clig_dt_node(i(I,Node),Rel) -->
    format_to_chars(" { stack { bold-text { ~w } } ",[I]),
    clig_dt_node(Node,Rel),
    format_to_chars(" } ",[]).
clig_dt_node(p(Label0),_) -->
    { extract_cat(Label0,Label) },
    format_to_chars(" { plain-text { ~w } } ",[Label]).
clig_dt_node(l(Frame,_Cat,Word0/_),_Rel) -->
    { root_of_pair(Word0,Word) },
    { alpino_postags:postag_of_frame(Frame,Pos,_) },
    { alpino_treebank:frame2sense(Word,Frame,Sense) },
    format_to_chars(" { stack { plain-text { ~w } } { plain-text { ~w }} }",
		    [Pos,Sense]).

%% OUTPUT_tree_user_node
%% DOT
dot_tree_user_node(r(Rel,Label)) -->
    format_to_chars("{ ~w | {",[Rel]),
    dot_dt_node(Label),
    format_to_chars(" } }  ",[]).

dot_dt_node(i(I)) -->
    format_to_chars(" ~w ",[I]).
dot_dt_node(i(I,Node)) -->
    format_to_chars(" ~w  | {",[I]),
    dot_dt_node(Node),
    format_to_chars(" } ",[]).
dot_dt_node(p(Label0)) -->
    { extract_cat(Label0,Label) },
    format_to_chars("  ~w  ",[Label]).
dot_dt_node(l(Frame,_,Word/_)) -->
    {  alpino_postags:postag_of_frame(Frame,Pos,_) },
    format_to_chars(" { ~w \\n ~w }", [Pos,Word]).


%% generate short version of (A) = (B), 'A=B'
short_pair(Rel,Cat,Atom) :-
    (   var(Cat)
    ->  format_to_chars("~w",[Rel],Chars)
    ;   Cat = mwu(_,_)
    ->  format_to_chars("~w=~w",[Rel,mwu],Chars)
    ;   format_to_chars("~w=~w",[Rel,Cat],Chars)
    ),
    atom_codes(Atom,Chars).


%% adt
graphic_label_rest_adt(i(X),Rel,Atom) :-
    short_pair(Rel,X,Atom).
    
graphic_label_rest_adt(p(X),Rel,Atom) :-
    short_pair(Rel,X,Atom).

graphic_label_rest_adt(adt_lex(Cat,_,_,_,_),Rel,Atom) :-
    short_pair(Rel,Cat,Atom).
graphic_label_rest_adt(i(I,p(X)),Rel,Atom) :-
    (   nonvar(X)
    ->  short_pair(Rel,I:X,Atom)
    ;   short_pair(Rel,I,Atom)
    ).
    
graphic_label_rest_adt(i(I,adt_lex(Cat,_,_,_,_)),Rel,Atom) :-
    (   nonvar(Cat)
    ->  short_pair(Rel,I:Cat,Atom)
    ;   short_pair(Rel,I,Atom)
    ).

graphic_path(adt,Result,Tree) :-
    (   var(Result)
    ->  true
    ;   Result=tree(_,_)
    ->  Result=Tree
    ;   alpino_adt:result_to_adt(Result,Tree)
    ).

graphic_label(adt,tree(r(Rel,Label),_),Rest) :-
    graphic_label_rest_adt(Label,Rel,Rest).

graphic_label(adt,sense(W),W).
graphic_label(adt,sense(W,[]),W).
graphic_label(adt,sense(_,[Att|_T]),Att) :-
    nonvar(Att).
graphic_label(adt,sense(_,[Att|_T]),'_') :-
    var(Att).

graphic_daughter(adt,1,tree(r(_,adt_lex(_,_Root,Sense,Pos,Atts)),[]),
                 sense(Sense,[Pos|Atts])) :-
    nonvar(Pos), nonvar(Sense).
graphic_daughter(adt,1,tree(r(_,adt_lex(_,Root,Sense,Pos,Atts)),[]),
                 sense(Root,[Pos|Atts])) :-
    nonvar(Pos), var(Sense).
graphic_daughter(adt,1,tree(r(_,i(_,adt_lex(_,_Root,Sense,Pos,Atts))),[]),
                 sense(Sense,[Pos|Atts])) :-
    nonvar(Pos), nonvar(Sense).
graphic_daughter(adt,1,tree(r(_,i(_,adt_lex(_,Root,Sense,Pos,Atts))),[]),
                 sense(Root,[Pos|Atts])) :-
    nonvar(Pos), var(Sense).
graphic_daughter(adt,1,tree(r(_,adt_lex(_,_Root,Sense,Pos,Atts)),[]),
                 sense(Sense,Atts)) :-
    var(Pos), nonvar(Sense).
graphic_daughter(adt,1,tree(r(_,adt_lex(_,Root,Sense,Pos,Atts)),[]),
                 sense(Root,Atts)) :-
    var(Pos), var(Sense).
graphic_daughter(adt,1,tree(r(_,i(_,adt_lex(_,_Root,Sense,Pos,Atts))),[]),
                 sense(Sense,Atts)) :-
    var(Pos), nonvar(Sense).
graphic_daughter(adt,1,tree(r(_,i(_,adt_lex(_,Root,Sense,Pos,Atts))),[]),
                 sense(Root,Atts)) :-
    var(Pos), var(Sense).
graphic_daughter(adt,N,tree(_,Ds),D) :-
    nth(N,Ds,D).

graphic_daughter(adt,1,sense(A,[_,H|T]),sense(A,[H|T])).
graphic_daughter(adt,1,sense(A,[_]),sense(A)).


%% cfg
graphic_path(cfg,T,T).

graphic_label(cfg,tree(Id,_,_,_),Id).
graphic_label(cfg,lex(Id),Id).

graphic_daughter(cfg,No,tree(_,_,Ds,_),D) :-
    nth(No,Ds,D).

graphic_daughter(cfg,1,tree(_,_,lex(W),_),lex(W)).


root_of_pair(v_root(_Root0,Lemma),Root) :-
    !,
    Lemma=Root.
root_of_pair(Root,Root).

show_node(adt,Tree,Medium) :-
    show(fs,Medium,[value(Tree)]).

