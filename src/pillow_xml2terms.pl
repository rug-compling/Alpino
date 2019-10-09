% Copyright (C) 1996/1997/1998/1999/2000 CLIP.

% This package is free software; you can redistribute it and/or
% modify it under the terms of the GNU Library General Public
% License as published by the Free Software Foundation; either
% version 2 of the License, or (at your option) any later version.
%
% This package is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Library General Public License for more details.
%
% You should have received a copy of the GNU Library General Public
% License along with this package; if not, write to the Free
% Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
%
% M. Hermenegildo (herme@fi.upm.es) & D. Cabeza (dcabeza@fi.upm.es)

% adapted by Gertjan van Noord, so that it only uses the
% xml2terms/2 predicate where the first argument is instantiated,
% i.e., only for parse_xml/5
 

:- module(pillow_xml2terms, [xml2terms/2]).

:- expects_dialect(sicstus).		% This is the SICStus port of the pillow!

:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(sockets)).

xml2terms(Chars, Terms) :-
        parse_xml([], Terms, [], Chars, []).

%% XML -> Terms translation %%

parse_xml(Stack,NStack,Dict) -->
        "<",
        {tidy_string(Stack, Stack2)},
        xml_unit(Stack2,Stack3,Dict), !,
        parse_xml(Stack3,NStack,Dict).
% build on an open string
parse_xml([Elem|Stack],NStack,Dict) -->
        {nonvar(Elem), Elem = string(S,T)},
        [C], !,
        {T = [C|TT]},
        parse_xml([string(S,TT)|Stack],NStack,Dict).
% open a new string
parse_xml(Stack,NStack,Dict) -->
        [C], !,
        parse_xml([string([C|T],T)|Stack],NStack,Dict).
% base case - close open strings
parse_xml(Stack,NStack,_Dict) --> "",
        {tidy_string(Stack,Stack2),
         reverse(Stack2,NStack)}.

% env terminators
xml_unit(S,NS,Dict) -->
        "/",
        xml_tag(N),
        whitespace0,
        ">",
        { poptokenstack(N,S,NS,Dict) },
        !.
% comment
xml_unit(S,[comment(Text)|S],_Dict) -->
        "!--",
        string(Text),
        "-->",
        !.
% declaration
xml_unit(S,[declare(Text)|S],_Dict) -->
        "!",
        string(Text),
        ">",
        !.
% xml declarations
xml_unit(S,[xmldecl(Ats)|S],Dict) -->
        "?xml",
        xml_tag_atts(Ats,Dict),
        whitespace0,
        "?>",
        !.
% elements or env beginnings
xml_unit(S,[El|S],Dict) -->
        xml_tag(N),
        xml_tag_atts(A,Dict),
        whitespace0,
        elem_envbeg(El, N, A),
        ">",
        !.

elem_envbeg(elem(N,A), N, A) -->
        "/", !.
elem_envbeg($(N,A),N, A) --> "".

xml_tag(N) -->
        xml_tag_start(C),
        xml_tag_rest(Cs),
        { atom_codes(N,[C|Cs]) }.

xml_tag_atts([],_Dict) --> "".
xml_tag_atts([A|As],Dict) -->
        whitespace,
        xml_tag_att(A,Dict),
        xml_tag_atts(As,Dict).

xml_tag_att(N=V,Dict) -->
        xml_tag(N),
        whitespace0,
        "=",
        whitespace0,
        xml_value(V, Dict).

xml_value(V, Dict) -->
        "_", !,
        xml_tag(N),
        {list_lookup(Dict, (=), N, V)}.
xml_value(V,_Dict) -->
        """", !,
        xml_quoted_string(0'",V).
xml_value(V,_Dict) -->
        "'", !,
        xml_quoted_string(0'',V).
xml_value(V,_Dict) --> % This is not correct syntax
        xml_bad_value(V).

xml_quoted_string(Q, []) --> [Q], !.
xml_quoted_string(Q, [0'&,0'q,0'u,0'o,0't,0';|Cs]) -->
        """", !,
        xml_quoted_string(Q, Cs).
xml_quoted_string(Q, [C|Cs]) -->
        [C],
        xml_quoted_string(Q, Cs).

xml_bad_value([]) --> "".
xml_bad_value([C|Cs]) -->
        [C],
        xml_bad_value(Cs).

xml_tag_start(C) --> loalpha(C), !.
xml_tag_start(C) --> upalpha(C), !.
xml_tag_start(0'_) --> "_".
xml_tag_start(0':) --> ":".

xml_tag_rest([C|Cs]) -->
        xml_tag_char(C), !,
        xml_tag_rest(Cs).
xml_tag_rest([]) --> "".

xml_tag_char(C) --> loalpha(C), !.
xml_tag_char(C) --> upalpha(C), !.
xml_tag_char(C) --> digit(C), !.
xml_tag_char(0'_) --> "_".
xml_tag_char(0':) --> ":".
xml_tag_char(0'.) --> ".".
xml_tag_char(0'-) --> "-".


elem_or_template_var('v',_,[NameS],Var,Dict) :-
        catch(atom_codes(Name,NameS), _, fail),
        list_lookup(Dict, (=), Name, Var),  !.
elem_or_template_var(EnvTag,Atts,Insides,env(EnvTag,Atts,Insides),_).

poptokenstack(EnvTag,Stack,NStack,Dict) :-
        pop_ts(EnvTag,Stack,[],NStack,Dict),
        !.
poptokenstack(EnvTag,Stack,[$(SlashEnvTag,[])|Stack],_) :-
        atom_concat('/',EnvTag,SlashEnvTag).

pop_ts(EnvTag,[Elem|S],Insides,NS,Dict) :-
        ( nonvar(Elem), Elem = $(EnvTag,Atts) ->
            elem_or_template_var(EnvTag,Atts,Insides,E,Dict),
            NS = [E|S]
        ; pop_ts(EnvTag,S,[Elem|Insides],NS,Dict)
        ).

tidy_string([Elem|Stack],[L|Stack]) :-
        nonvar(Elem), Elem = string(L,T), !, T = [].
tidy_string(Stack,Stack).


whitespace --> whitespace_char, whitespace0.

whitespace0 --> whitespace_char, whitespace0, !.
whitespace0 --> [].

whitespace_char --> [10], !. % newline
whitespace_char --> [13], !. % return
whitespace_char --> [32], !. % space
whitespace_char --> [9], !.  % tab

string([]) --> "".
string([C|Cs]) -->
        [C],
        string(Cs).

loalpha(C) --> [C], {C >= 0'a, C =< 0'z}.

upalpha(C) --> [C], {C >= 0'A, C =< 0'Z}.

digit(C) --> [C], {C >= 0'0, C =< 0'9}.

list_lookup(List, Functor, Key, Value) :-
	var(List), !,
        functor(Pair, Functor, 2),
        arg(1, Pair, Key),
        arg(2, Pair, Value),
	List=[Pair|_].
list_lookup([Pair|_], Functor, Key, Value) :-
        functor(Pair, Functor, 2),
        arg(1, Pair, Key0),
	Key0==Key, !,
        arg(2, Pair, Value).
list_lookup([_|List], Functor, Key, Value) :-
	list_lookup(List, Functor, Key, Value).

/*
%%% reads lines from input
%%% for every complete XML string, calls callback for that parsed string
%%% calls
read_xml_call_back(Env,Arg^Call) :-
    read_line(X),
    read_xml_call_back(Env,Arg^Call,X).

read_xml_call_back(Env,Arg^Call,X) :-
    xml2terms(X,Terms),
    lists:member(env(Env,_Atts,_Conts),Terms),
    !,
    copy_term(Arg^Call,Terms^RealCall),
    call(RealCall),
    read_xml_call_back(Env,Arg^Call).
read_xml_call_back(Env,Call,X) :-
    read_line(X2),
    lists:append(X,X2,X3),
    read_xml_call_back(Env,Call,X3).


test :-
    read_xml_call_back(alpino_ds,Terms^format("~w~n",[Terms])).
*/
