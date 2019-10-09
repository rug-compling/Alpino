%%           -*-Mode: prolog;-*-

:- multifile macro/2.

:- discontiguous macro/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code from: Transducers from Rewrite Rules with Backreferences %%%
%%%             by Dale Gerdemann and Gertjan van Noord           %%%
%%%                    Proceedings of ACL 1999                    %%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% For the sake of comparison and experimentation, the code is
%% mostly identical to that in the published paper. The code can be
%% optimized by use of the regex_cache=on flag.
%% 
%% The code can be optimized by implementing some of the
%% operators in terms of low-level manipulation of states and
%% transitions (van Noord & Gerdemann, "An extendible regular
%% expression compiler for finite state approaches in natural
%% language processing." in WIA 99). In order to understand the logic
%% of the approach, however, it is better to see it implemented in
%% this high-level finite-state-calculus approach.
%%
%% For the alternative, refer to replace.pl
%%
%% Unfortunately, the published version contained a couple of small
%% bugs, which we have corrected and commented in this code. 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Leftmost/Longest Contexted Replacement %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


macro(replace(T0,Left0,Right), pragma([T-T0,Left-Left0], cleanup(
      non_markers
          o
       r(Right)
          o
      f(domain(T))
          o
 left_to_right(domain(T))
          o
 longest_match(domain(T))
          o
    aux_replace(T)
          o
       l1(Left)
          o
       l2(Left)
          o
   inverse(non_markers)))
     ).       


macro(replace(T0), pragma([T-T0], cleanup(
      non_markers
          o
        r([])
          o
      f(domain(T))
          o
 left_to_right(domain(T))
          o
 longest_match(domain(T))
          o
    aux_replace(T)
          o
       l1([])
          o
       l2([])
          o
   inverse(non_markers)
			))
     ).       


%%%%%%%%%%%%%%%%%%%%%%%
%%% Special Symbols %%%
%%%%%%%%%%%%%%%%%%%%%%%

macro(lb1,['<1',1]).
macro(lb2,['<2',1]).
macro(rb2,['2>',1]).
macro(rb1,['1>',1]).
macro(lb,{lb1,lb2}).
macro(rb,{rb1,rb2}).
macro(b1,{lb1,rb1}).
macro(b2,{lb2,rb2}).
macro(brack,{lb,rb}).



macro(sig,[?,0]).
macro(xsig,[?,{0,1}]).

macro(non_markers,[?,[]:0]*).
macro(non_markers(E),
      range(E o non_markers)).

%%%%%%%%%%%%%
%%% Utils %%%
%%%%%%%%%%%%%

macro(not(X),xsig* - X).
macro($$(X),[xsig*,X,xsig*]).


macro(intro(S0),   pragma([S-S0], {xsig-S,[] x S}*)).
macro(xintro(S0),  pragma([S-S0], {[],[xsig-S,intro(S)]})).
macro(intro_x(E2),                {[],[intro_(E2),ext_sigma]}).
macro(introx(S0),  pragma([S-S0], {[],[intro(S),xsig-S]})).
macro(xintrox(S0), pragma([S-S0], {[],[xsig-S], [xsig-S,intro(S),xsig-S]})).


macro( ign( E1,S),range(E1 o  intro( S))).
macro(xign( E1,S),range(E1 o xintro( S))).
macro( ignx(E1,S),range(E1 o  introx(S))).
macro(xignx(E1,S),range(E1 o xintrox(S))).


macro(if_p_then_s(L1,L2), 
      not([L1,not(L2)])).
macro(if_s_then_p(L1,L2), 
      not([not(L1),L2])).
macro(p_iff_s(V1,V2), pragma([L1-V1,L2-V2],
      if_p_then_s(L1,L2) 
            & 
      if_s_then_p(L1,L2))).

macro(l_iff_r(L,R), 
   p_iff_s([xsig*,L],[R,xsig*])).


macro(true,? *).
macro(false,{}).

macro(coerce_to_boolean(E), 
      range(E o (true x true))).


macro(if(Cond,Then,Else), pragma([C-Cond],
   {  coerce_to_boolean(C) o Then,
     ~coerce_to_boolean(C) o Else
   })).



%%%%%%%%%%%%%%%%%%%%%%%
%%% Main Procedures %%%
%%%%%%%%%%%%%%%%%%%%%%%

macro(r(R0), pragma([R-R0],
  if([] & R,       % If: [] is in R:
     [[[] x rb2,sig]*,[] x rb2], 
      intro(rb2)   % Else:
            o
  l_iff_r(rb2,xign(non_markers(R),rb2))))).



macro(f(Phi), intro(lb2)
                 o
  l_iff_r(lb2,[[xignx(non_markers(Phi),b2),lb2^]-lb2, % Modified from
             rb2])).                                  % published version


macro(left_to_right(Phi),
  [[xsig*,
    [lb2 x lb1,
     (ign(non_markers(Phi),b2)
              o
      inverse(intro(lb2))
     ),
     rb2 x rb1]
   ]*, xsig*]).

macro(longest_match(Phi),
  not($$([lb1,
          (ignx(non_markers(Phi),brack)
                     &
                  $$(rb1)
          ),     % longer match must be
          rb     % followed by an rb 
         ]))     % so context is ok
          o
   % done with rb2, throw away:
   inverse(intro(rb2))).  


/**** The following optimization is suggested in fn. 4:
 * macro(longest_match(Phi),
 *  not($$([lb1,
 *          (ignx(non_markers(Phi),brack)
 *                     &
 *                  $$(rb1)
 *          ),
 *          [ign(non_markers(Phi),brack),rb1,xsig*]
 *         ]))
 *          o
 *   inverse(intro(rb2))).  
 */



macro(aux_replace(T),
  {{sig,lb2},
   [lb1,
    inverse(non_markers) 
         o T o 
       non_markers,
    rb1 x []
   ]
  }*).



macro(l1(L),
  ign(if_s_then_p(
       ign([xsig*,non_markers(L)],lb1), % The published version had ignx here
       [lb1,xsig*]),                    % this fails on replace(a x [],b,[])
      lb2)                              % when the string is: baaa
        o
 inverse(intro(lb1))). 



macro(l2(L),
  if_s_then_p(
    ign(not([xsig*,non_markers(L)]),lb2), % Published version has ignx here
    [lb2,xsig*])
          o
 inverse(intro(lb2))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Longest Match Concatenation %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
macro(lm_concat(Ts),mark_boundaries(Domains) o ConcatTs):-
   domains(Ts,Domains), concatT(Ts,ConcatTs).

domains([],[]).
domains([F|R0],[domain(F)|R]):- domains(R0,R).

concatT([],[]).
concatT([T|Ts], [inverse(non_markers) o T,lb1 x []|Rest]):- concatT(Ts,Rest).

%% macro(mark_boundaries(L),Exp): This is the central component of lm_concat. For our
%% "topological" example we will have:
%% mark_boundaries([domain([{[t,o],[t,o,p]},[]: #]),
%%                  domain([{o,[p,o,l,o]},[]: #]),
%%                  domain({[g,i,c,a,l],[o^,l,o,g,i,c,a,l]})])
%% which simplifies to:
%% mark_boundaries([{[t,o],[t,o,p]}, {o,[p,o,l,o]}, {[g,i,c,a,l],[o^,l,o,g,i,c,a,l]}]).
%% Then by macro expansion, we get:
%% [{[t,o],[t,o,p]} o non_markers,[]x lb1,
%%  {o,[p,o,l,o]} o non_markers,[]x lb1,
%%  {[g,i,c,a,l],[o^,l,o,g,i,c,a,l]} o non_markers,[]x lb1]
%%              o
%% % Filter 1: {[t,o],[t,o,p]} gets longest match
%% ~ [ignx_1(non_markers({[t,o],[t,o,p]}),lb1),
%%    ign(non_markers({o,[p,o,l,o]}),lb1),
%%    ign(non_markers({[g,i,c,a,l],[o^,l,o,g,i,c,a,l]}),lb1)]
%%                   o
%% % Filter 2: {o,[p,o,l,o]} gets longest match
%% ~ [non_markers({[t,o],[t,o,p]}),lb1,
%%    ignx_1(non_markers({o,[p,o,l,o]}),lb1),
%%    ign(non_markers({[g,i,c,a,l],[o^,l,o,g,i,c,a,l]}),lb1)] 

macro(mark_boundaries(L),Exp):-
   boundaries(L,Exp0), % guess boundary positions 
   greed(L,Exp0,Exp).  % filter non-longest matches

boundaries([],[]).
boundaries([F|R0],[F o non_markers, [] x lb1 |R]):- boundaries(R0,R).

greed(L,Composed0,Composed) :-
   aux_greed(L,[],Filters), compose_list(Filters,Composed0,Composed).

aux_greed([H|T],Front,Filters):- aux_greed(T,H,Front,Filters,_CurrentFilter).

aux_greed([],F,_,[],[ign(non_markers(F),lb1)]).
aux_greed([H|R0],F,Front,[~L1|R],[ign(non_markers(F),lb1)|R1]) :-
   append(Front,[ignx_1(non_markers(F),lb1)|R1],L1),
   append(Front,[non_markers(F),lb1],NewFront),
   aux_greed(R0,H,NewFront,R,R1).

%% ignore at least one instance of E2 except at end
macro(ignx_1(E1,E2), range(E1 o [[? *,[] x E2]+,? +])).

compose_list([],SoFar,SoFar).
compose_list([F|R],SoFar,Composed):- compose_list(R,(SoFar o F),Composed).




%%%%%%%%%%%%%
%%% Tests %%%
%%%%%%%%%%%%%

%% Use this test with the string abababa
macro(test1,replace(([a,b] x x),[a,b],a)).

%% Use this test with the string <1 <2 <1 <2 <1 <2 <1 
macro(test2,replace((['<1','<2'] x '1>'),['<1','<2'],'<1')).

%% Use test string: abbaa
macro(test3,replace(({[a,b],b,[b,a],[a,b],a} x x),{[a,b],b},a)).

