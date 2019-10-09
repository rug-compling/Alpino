/*
A simple DCG for Dutch

Each node is a term x(Cat/LF, Topic, Verb2, Deriv) where Topic is used
for a gap threading analysis of topicalization; Verb2 percolates
information on verb second; and Deriv is a simple derivation tree.
Cat is a term representing syntactic information and LF represents the
argument structure, input for generation.

*/

%% GvN: if module is already known, only import predicates, but don't
%% check if a newer version exists. This is crucial for runtime systems
%% in which the library is not even around anymore!
:- use_module( restrict, [ restrict/3 ]).
:- use_module( lists, library(lists), [ member/2 ]).

:- ensure_loaded(defaults).

top(main,x(main/_,L-L,nil,_)).
top(sbar,x(sbar(_)/_,L-L,nil,_)).
top(no_gap,x(_,L-L,nil,_)).
top(nov,x(_,_,nil,_)).
top(np,x(np(_)/_,_,_,_)).

syn_head(x(Syn/_,_,_,_),x(Head/_,_,_,_)) :-
	sh(Syn,Head).

sh(main,v0(_,_,_,_)).
sh(sbar(_),comp(_,_)).
sh(sbar(_,_),comp(_,_)).
sh(s(_),vp(_,_,_)).
sh(vp(_,_,_),vp(_,_,_)).
sh(vp(_,_,_),v(_,_,_)).
sh(v(_,_,_),v0(_,_,_,_)).
sh(np(_),n(_)).
sh(pp,p(_)).
sh(n(_),n(_)).

% these at the top, because must be defined for term_expansion to work
topicalizable(np(_)/_).   
topicalizable(pp/_).
topicalizable(sbar(_)/_).    
topicalizable(sbar(_,_)/_).  
topicalizable(adv/_).

extra(sbar(_)/_,yes).   % must be extraposed
extra(sbar(_,_)/_,yes). % must be extraposed
extra(np(_)/_,no).      % may not 
extra(part/_,no).       % may not 
extra(adv/_,no).        % may not 
extra(pp/_,_).          % can be extraposed

vp_mod(pp/_).
vp_mod(adv/_).
vp_mod(sbar(_)/_).
vp_mod(sbar(_,_)/_).

syntax(   x(Syn/_,_,_,_),Syn).

semantics(x(_/Sem,_,_,_),Sem).

ignore_semantics(x(A/_,B,C,_), x(A/_,B,C,_)).

restriction(x(S/_,_,V,_),x(SR/_,_,VR,_)) :-
	restrict(S,3,SR),
	restrict(V,2,VR).

hfc(x(_,_,V,_),x(_,_,V,_),_,_,_,_).

% main -> topic v0 s
x(main/LF,X-X,nil,main(T,V,S)) ---> 
  x(Topic, Y-Y, nil, T),  
  x(v0(VT,fin,B,C)/VS,Z-Z,nil,V),
  x(s(fin)/LF,Topic-nil,v0(VT,fin,B,C)/VS,S) :-
 	topicalizable(Topic).

% sbar -> comp s
x(sbar(fin)/LF,I-I,nil,sbar(C,S)) --->        
  x(comp(fin,SS)/LF,O-O,nil,C),
  x(s(fin)/SS,P-P,nil,S).

x(sbar(infin,Sj)/LF,I-I,nil,sbar(C,S)) --->
  x(comp(infin,SS)/LF,O-O,nil,C),
  x(vp(te,Sj,[])/SS,P-P,nil,S).

% s -> np vp
x(s(VF)/LF,I-O,Verb,s(SS,VV)) ---> 
  x(np(Agr)/Sj,I-O2,nil,SS), 
  x(vp(VF,np(Agr)/Sj,[])/LF,O2-O,Verb,VV).

% vp -> compl vp
x(vp(VF,S,T)/LF,I-O,V,vp(C,VP)) --->
  x(H,I-O2,nil,C),  
  x(vp(VF,S,[H|T])/LF,O2-O,V,VP) :-
	extra(H,no).

% vp -> vp compl (extraposition)
x(vp(VF,S,T)/LF,I-O,V,vp(VP,C)) --->
  x(vp(VF,S,[H|T])/LF,O2-O,V,VP),
  x(H,I-O2,nil,C) :-
	extra(H,yes).
%
% vp -> mod vp
x(vp(VF,S,Subcat)/[Moder,Modee],G0-G,V,vp(C,VP)) --->
  x(H/Moder,G0-G1,nil,C),
  x(vp(VF,S,Subcat)/Modee,G1-G,V,VP) :-
	vp_mod(H/Moder),
	extra(H/Moder,no).

% vp -> vp mod
x(vp(VF,S,Subcat)/[Moder,Modee],G0-G,V,vp(VP,C)) --->
  x(vp(VF,S,Subcat)/Modee,G0-G1,V,VP),
  x(H/Moder,G1-G,nil,C) :-
	vp_mod(H/Moder),
	extra(H/Moder,yes).

% vp -> v1
x(vp(VF,Sj,Sc)/LF,I-O,V,vp(VV)) --->
  x(v(VF,Sj,Sc-[])/LF,I-O,V,VV).


% v1 -> v0
x(v(VF,Sj,Sc)/LF,I-O,V,v(VV)) --->
  x(v0(main,VF,Sj,Sc)/LF,I-O,V,VV).

% v1 -> aux v1
x(v(F,Sj,Sc)/LF,I-O,V,v(A,B)) --->
  x(v0(aux,F,Sj,v(_A,_B,_C)/_S+Sc)/LF,I-I2,V,A),
  x(v(_A,_B,_C)/_S,I2-O,nil,B).

% gap -> v0

head_gap(x(v0(VT,fin,Sj,Sc)/LF,_,nil,_Tree),
  x(v0(VT,fin,Sj,Sc)/LF,X-X,v0(VT,fin,Sj,Sc)/LF,vgap)).


x(v0(VT,fin,Sj,Sc)/LF,X-X,v0(VT,fin,Sj,Sc)/LF,vgap) --->
  [].


% np -> det n
x(np(Nm)/LF,I-I,nil,np(Det,N)) --->
  x(det(Ns,Nm)/LF,nil-nil,nil,Det),
  x(n(Nm)/Ns,nil-nil,nil,N).

% n -> n pp
x(n(Nm)/[Moder,Modee],G0-G,nil,n(N,PP)) --->
  x(n(Nm)/Modee,G0-G1,nil,N),
  x(pp/Moder,G1-G,nil,PP).

% pp -> p np
x(pp/LF,I-I,nil,pp(P,NP)) ---> 
  x(p(Np)/LF,J-J,nil,P), 
  x(np(_)/Np,O-O,nil,NP).

% topic -> 
x(Topic,Topic-nil,nil,topic_gap) ---> 
  [] :-
	topicalizable( Topic ).

x(np(sg)/john,I-I,nil,np(john)) ---> [jan].
x(np(sg)/mary,I-I,nil,np(mary)) ---> [marie].
x(np(sg)/jane,I-I,nil,np(jane)) ---> [jane].
x(np(sg)/tarzan,I-I,nil,np(tarzan)) ---> [tarzan].
x(n(sg)/sg(man),I-I,nil,n(man)) ---> [man].
x(n(pl)/pl(man),I-I,nil,n(mannen)) ---> [mannen].
x(n(sg)/sg(woman),I-I,nil,n(vrouw)) ---> [vrouw].
x(n(pl)/pl(woman),I-I,nil,n(vrouwen)) ---> [vrouwen].
x(n(sg)/sg(mashed_potatoes),I-I,nil,n(puree)) ---> [puree].
x(det(Ns,_)/Ns,I-I,nil,det(de)) ---> [de].
x(det(Ns,pl)/Ns,I-I,nil,det(e)) ---> [].
x(p(Np)/naar(Np),I-I,nil,p(naar)) ---> [naar].
x(p(Np)/met(Np),I-I,nil,p(met)) ---> [met].
x(p(Np)/in(Np),I-I,nil,p(in)) ---> [in].
x(p(Np)/van(Np),I-I,nil,p(van)) ---> [van].
x(p(Np)/aan(Np),I-I,nil,p(aan)) ---> [aan].
x(part/op,I-I,nil,part(op)) ---> [op].
x(comp(fin,S)/dat(S),O-O,nil,comp(dat)) ---> [dat].
x(comp(fin,S)/because(S),O-O,nil,comp(omdat)) ---> [omdat].
x(comp(infin,S)/S,O-O,nil,comp(om)) ---> [om].
x(comp(infin,S)/S,O-O,nil,comp) ---> [].

x(adv/vandaag,I-I,nil,adv(vandaag)) ---> [vandaag].

verb(sleep(A1),_/A1,X-X, 
  [fin/sg/slaapt,fin/pl/slapen,infin/_/slapen,part/_/geslapen]).

verb(catch(A1,A2),_/A1,[part/op,np(_)/A2|X]-X,   %particle verb
  [fin/sg/vangt,fin/pl/vangen,infin/_/vangen,part/_/gevangen]).

verb(in_trouble(A1),_/A1,[pp/in(sg(mashed_potatoes))|X]-X, %idiom
  [fin/sg/zit,fin/pl/zitten,infin/_/zitten,part/_/gezeten]).

verb(kiss(A1,A2),_/A1,[np(_)/A2|X]-X,
  [fin/sg/kust,fin/pl/kussen,infin/_/kussen,part/_/gekust]).

verb(say(A1,A2),_/A1,[sbar(fin)/dat(A2)|X]-X,
  [fin/sg/zegt,fin/pl/zeggen,infin/_/zeggen,part/_/gezegd]).

% object control
verb(force(A1,A2,A3),Sj/A1,[sbar(infin,Sj/A2)/A3,np(_)/A2|X]-X,
  [fin/sg/dwingt,fin/pl/dwingen,infin/_/dwingen,part/_/gedwongen]).

verb(try(A1,A2),Sj/A1,[sbar(infin,Sj/A1)/A2|X]-X,  % subject control
  [fin/sg/probeert,fin/pl/proberen,infin/_/proberen,part/_/geprobeerd]).

verb(give(A1,A2,A3),_/A1,[np(_)/A2,np(_)/A3|X]-X,
  [fin/sg/geeft,fin/pl/geven,infin/_/geven,part/_/gegeven]).

verb(give(A1,A2,A3),_/A1,[pp/aan(A3),np(_)/A2|X]-X,
  [fin/sg/geeft,fin/pl/geven,infin/_/geven,part/_/gegeven]).

aux(perf(X),v(part,np(Agr)/A1,Sc)/X,Sc,np(Agr)/A1,
  [ fin/sg/heeft,fin/pl/hebben,infin/_/hebben, part/_/gehad]).

aux(fut(X),v(infin,np(Agr)/A1,Sc)/X,Sc,np(Agr)/A1,
  [ fin/sg/zal,fin/pl/zullen,infin/_/zullen ]).

aux(X,v(infin,Sj,Sc)/X,Sc,Sj,[ te/_/te ] ).

aux(try(A1,A2),v(te,_/A1,Sc)/A2,Sc,np(_Agr)/A1, %raising
  [ fin/sg/probeert,fin/pl/proberen,infin/_/proberen,part/_/proberen ] ).

aux(see(A1,A2),v(infin,Sj,A-[Sj|E])/A2,A-E,np(_Agr)/A1, % aci raising
  [ fin/sg/ziet, fin/pl/zien, infin/_/zien,part/_/zien ] ).

aux(help(A1,A2),v(infin,Sj,A-[Sj|E])/A2,A-E,np(_Agr)/A1, % aci raising
  [ fin/sg/helpt, fin/pl/helpen,infin/_/helpen,part/_/helpen ] ).


% verbs and auxes pick out an inflectional variant:
% these at the end, because otherwise not yet defined in
% term_expansion.
x(v0(main,Fin,np(Agr)/A1,Sc)/LF,I-I,nil,v0(Form)) --->
  [ Form ] :-
	verb(LF,np(Agr)/A1,Sc,List),
	member(Fin/Agr/Form,List).

x(v0(aux,Fin,np(Agr)/A1,Verb+Sc)/LF,I-I,nil,aux(Form)) --->
  [ Form ] :-
	aux(LF,Verb,Sc,np(Agr)/A1,List),
	member(Fin/Agr/Form,List).

%%% pretty printing

:- multifile graphic_path/3,
             graphic_label/3,
	     graphic_daughter/4.

graphic_path(tree,x(_,_,_,P),P).
graphic_daughter(tree,1,Term,D1):-
    arg(1,Term,D1).
graphic_daughter(tree,2,Term,D2):-
    arg(2,Term,D2).
graphic_daughter(tree,3,Term,D3):-
    arg(3,Term,D3).
graphic_daughter(tree,4,Term,D4):-
    arg(4,Term,D4).
graphic_label(tree,L,F):-
    functor(L,F,_).

:- use_module(lex_string).

start_hook(parse,_,o(_Obj,Str,_),_) :-
	lex_string(Str).


show_object_default2(No) :-
    show_object_no(No,tree(tree),clig).

end_hook(parse,_,_,_) :-
    show_object_no(1,tree(tree),clig).

