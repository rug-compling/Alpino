%% Stochastic Context-Free Grammar parser

:- module(parser,[]).

:- dynamic user:prob/3.

%% GvN: if module is already known, only import predicates, but don't
%% check if a newer version exists. This is crucial for runtime systems
%% in which the library is not even around anymore!
:- use_module(lists,   library(lists),   all).
:- use_module(ordsets, library(ordsets), all).
:- use_module(system,  library(system),  all).
:- use_module(charsio, library(charsio), all).

%% Depth-bounded DCG parser

parse(o(parse(Obj,P),Str,Fs)) :-
    parse0(Obj,0,Str,[]),
    hdrug_util:hdrug_flag(model,Model),
    tree_prob(Obj,Fs,Model,P).

parse0(Obj,_) -->
    [W],
    { user:lexicon(W,Obj) }.
parse0(r(Id,Cat,Ds),D0) -->
    { D0 < 10, user:rule(r(Id,Cat,Ds),Ds), D is D0 + 1 },
    parse_ds(Ds,D).

parse_ds([],_) --> [].
parse_ds([H|T],D) -->
    parse0(H,D),
    parse_ds(T,D).

%% Grammar stuff

:- multifile user:user_clause/2.

user:user_clause(H,B) :-
    user:rule(r(_,H,_),B).

user:user_clause(H,[B]) :-
    user:lexicon(B,r(_,H,_)).

%user:user_clause(feature(M,H),[B]) :-
%    user:feature(M,H,B).

lhs(Id,Lhs) :-
    user:rule(r(Id,Cat,_), _),
    functor(Cat,Lhs,_).
lhs(Id,Lhs) :-
    user:lexicon(_,r(Id,Cat,_)),
    functor(Cat,Lhs,_).

%% Compute the probability of a tree

tree_prob(Tree,Ids,Model,P) :-
    feats(Tree,Model,Ids),
    ( probs(Ids,Model,Ps)
    ->
	product(Model,Ps,P)
    ;
	P is 1
    ).
probs([],_,[]).
probs([Id|Ids],Model,[P|Ps]) :-
    user:prob(Id,Model,P),
    probs(Ids,Model,Ps).

product(scfg,List,Result) :-
    product(scfg,List,1,Result).
product(scfg,[],P,P).
product(scfg,[L|Ls],P0,P) :-
    P1 is P0 * L,
    product(Ls,P1,P).

product(maxent,List,Result) :-
    sum_list(List,Result).

%% Collect the IDs of all the features used in a tree

feats(w(W),Model,Fs) :-
    findall(F,user:feature(Model,F,w(W)),Fs).
feats(r(Id,Cat,Ds),Model,F1) :-
    findall(F,user:feature(Model,F,r(Id,Cat,Ds)),Fs),
    feats0(Ds,Model,DFs),
    append(Fs,DFs,F1).

feats0([],_,[]).
feats0([R|Rs],Model,Ids) :-
    feats(R,Model,Ids0),
    feats0(Rs,Model,Ids1),
    append(Ids0,Ids1,Ids).

%% Train SCFG from treebank

train :-
    retractall(user:prob),
    hdrug_util:hdrug_flag(model,Model),
    train(Model),
    format("Trained ~q.~n",[Model]).

accum(X,V,[(X,V1)|Xs],[(X,V2)|Xs]) :-
    V2 is V1 + V.
accum(X,V,[],[(X,V)]).
accum(X,V,[(X1,V1)|Xs],[(X1,V1)|Ys]) :-
    X \= X1,
    accum(X,V,Xs,Ys).

count_rules(Rules,Cats) :-
    findall(treebank(F,T),user:treebank(F,T),Ts),
    count_tree_rules(Ts,[],Rules,[],Cats).

count_tree_rules([],Rules,Rules,Cats,Cats).
count_tree_rules([treebank(Freq,T)|Ts],R0,R,C0,C) :-
    feats(T,scfg,Ids),
    count_feats(Ids,Freq,R0,R1),
    count_lhs(Ids,Freq,C0,C1),
    count_tree_rules(Ts,R1,R,C1,C).

count_feats([],_,R,R).
count_feats([Id|Ids],Freq,R0,R) :-
    accum(Id,Freq,R0,R1),
    count_feats(Ids,Freq,R1,R).

count_lhs([],_,C,C).
count_lhs([Id|Ids],Freq,C0,C) :-
    lhs(Id,Lhs),
    accum(Lhs,Freq,C0,C1),
    count_lhs(Ids,Freq,C1,C).

train(scfg) :-
    count_rules(Rules,Cats),
    train_scfg(Rules,Cats).
train_scfg([],_).
train_scfg([(Id,F)|Rs],Cats) :-
    lhs(Id,Lhs),
    member((Lhs,Z),Cats),
    P is F / Z,
    assert(user:prob(Id,scfg,P)),
    train_scfg(Rs,Cats).

%% Train random field from treebank

find_features(Feats,NTrees,NFeats) :-
    findall(S,(user:treebank(_,T),feats(T,maxent,F),list_to_ord_set(F,S)),Ss),
    length(Ss,NTrees),
    ord_union(Ss,Set),
    number_feats(Set,0,Feats),
    length(Feats,NFeats).

number_feats([],_,[]).
number_feats([F|Fs],N0,[(F,N0)|Ps]) :-
    N is N0 + 1,
    number_feats(Fs,N,Ps).

tmp(File) :-
    hdrug_latex:dir(Tmp),
    hdrug_util:concat(Tmp,'/iisXXXXXX',Base0),
    mktemp(Base0,File).

run_iis(Events,Params) :-
    tmp(Temp),
    format_to_chars('./do_iis ~w ~w ~w',[Events,Params,Temp],Chars),
    name(Cmd,Chars),
    shell(Cmd).

iis(Feats,NTrees,NFeats,Params) :-
    tmp(Events),
    tmp(Params),
    open(Events,write,EStream),
    format(EStream,"&header~nnfeats=~d~n/~n~d~n",[NFeats,NTrees]),
    events(EStream,Feats),
    close(EStream),
    run_iis(Events,Params).

events(EStream,Feats) :-
    ( user:treebank(F,T),
	feats(T,maxent,Ids),
	count_feats(Ids,1,[],Fs),
	length(Fs,N),
	format(EStream,"~d ~d ",[F,N]),
	write_feats(EStream,Feats,Fs),
	format(EStream,"~n",[]),
	fail
    ;
	true
    ).

write_feats(_,_,[]).
write_feats(EStream,Feats,[(F,C)|Fs]) :-
    member((F,N),Feats),
    format(EStream,"~d ~d ",[N,C]),
    write_feats(EStream,Feats,Fs).

get_params(Params,Feats) :-
    open(Params,read,PStream),
    read_params(PStream,Feats),
    close(PStream),
    delete_file(Params,[ignore]).

read_params(PStream,_) :-
    at_end_of_stream(PStream),
    !,
    true.
read_params(PStream,Feats) :-
    read(PStream,weight(C,W)),
    member((F,C),Feats),
    assert(user:prob(F,maxent,W)),
    read_params(PStream,Feats).

train(maxent) :-
    find_features(Feats,NTrees,NFeats),
    iis(Feats,NTrees,NFeats,Params),
    get_params(Params,Feats).
