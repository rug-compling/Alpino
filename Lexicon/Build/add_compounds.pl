:- module(add_compounds, [ add_compounds/5 ]).

:- use_module(library(lists)).
:- use_module(hdrug(hdrug_util)).

add_compounds(_,Stem,Surf,Stem,Surf).
add_compounds([H|T],Stem0,Surf0,Stem,Surf) :-
    lists:member(Prefix,[H|T]),
    add_compound(Prefix,Stem0,Surf0,Stem1,Surf1),
    %% stead-fast against atom_concat etc if Stem or Surf is already known:
    Stem1=Stem,
    Surf1=Surf.

%% boekhouding => boek_houding
add_compound(Prefix,Stem0,Surf0,Stem,Surf) :-    
    atom(Prefix),!,
    atom_concat(Prefix,Surf0,Surf),
    atom_concat(Prefix,'_',PrefixStem1),
    atom_concat(PrefixStem1,Stem0,Stem).
%% on+deskundig => ondeskundig
add_compound(x(Prefix),Stem0,Surf0,Stem,Surf) :-    
    atom(Prefix),!,
    atom_concat(Prefix,Surf0,Surf),
    atom_concat(Prefix,Stem0,Stem).
%% mond- en klauwzeercrisis => mond- en klauwzeer_crisis
add_compound([H|T],Stem2,Surf1,Stem,Surf) :-     % mond- en klauwzeercrisis
    concat_all([H|T],Stem0,' '),
    atom_concat(Stem0,'_',Stem1),
    atom_concat(Stem1,Stem2,Stem),
    lists:append(Prefix,[Last],[H|T]),
    atom_concat(Last,Surf1,Surf2),
    lists:append(Prefix,[Surf2],Surf).
%% pruimeboom => pruim_boom
add_compound(i(StemPrefix,SurfPrefix),Stem0,Surf0,Stem,Surf) :- % i
    atom_concat(StemPrefix,'_',StemPrefix1),
    atom_concat(StemPrefix1,Stem0,Stem),
    atom_concat(SurfPrefix,Surf0,Surf).
%% EU-vergadering => EU_vergadering
add_compound(h(Prefix),Stem0,Surf0,Stem,Surf) :-
    atom_concat(Prefix,'_',StemPrefix1),
    atom_concat(StemPrefix1,Stem0,Stem),
    atom_concat(Prefix,'-',SurfPrefix1),
    atom_concat(SurfPrefix1,Surf0,Surf).
%% van der Valk-concern => van der Valk_concern
add_compound(wh(List),Stem0,Surf0,StemAtom,SurfList) :-
    lists:append(List0,[Prefix],List),
    lists:append(List0,[Surf],SurfList),
    lists:append(List0,[Stem],StemList),
    atom_concat(Prefix,'_',StemPrefix1),
    atom_concat(StemPrefix1,Stem0,Stem),
    atom_concat(Prefix,'-',SurfPrefix1),
    atom_concat(SurfPrefix1,Surf0,Surf),
    concat_all(StemList,StemAtom,' ').
%% beleidsconflict => beleid_conflict
add_compound(s(Prefix),Stem0,Surf0,Stem,Surf) :-
    atom_concat(Prefix,'s',PrefixSurf1),
    atom_concat(PrefixSurf1,Surf0,Surf),
    atom_concat(Prefix,'_',PrefixStem1),
    atom_concat(PrefixStem1,Stem0,Stem).
%% "English compounds" software engineer => software engineer
add_compound(f(Prefix),Stem0,Surf0,Stem,Surf) :-
    lists:append(Prefix,[Surf0],Surf),
    concat_all(Prefix,StemPrefix,' '),
    atom_concat(StemPrefix,' ',StemPrefix1),
    atom_concat(StemPrefix1,Stem0,Stem).
%% left-headed compouns "kabinet-Kok" => kabinet_Kok
add_compound(post_h(Suffix),Stem0,Surf0,Stem,Surf) :-
    atom_concat(Stem0,'_',Stem1),
    atom_concat(Stem1,Suffix,Stem),
    atom_concat(Surf0,'-',Surf1),
    atom_concat(Surf1,Suffix,Surf).
%% commissie-van Traa => commissie_van Traa
add_compound(post_wh([First|Rest]),Stem0,Surf0,Stem,[Surf2|Rest]) :-
    atom_concat(Surf0,'-',Surf1),
    atom_concat(Surf1,First,Surf2),
    atom_concat(Stem0,'_',Stem1),
    atom_concat(Stem1,First,Stem2),
    concat_all([Stem2|Rest],Stem,' ').
