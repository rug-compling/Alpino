
%% zinnen en bijzinnen

s --> np, aux(fin), vp(no_aux).
s --> np, mod(fin), vp(no_mod).
s --> np, v(fin),   vp(no_v).

cp --> compl, np, vp(fin).

compl --> [Word], {compl(Word)}.

%% vp's en vc's

vp(V) --> vc(V).
vp(V) --> np, vp(V).
%vp(V) --> pp, vp(V).
%vp(V) --> adv, vp(V).

%vp(V) --> vc(V), pp.
vp(V) --> vc(V), cp.

vc(V) --> v(V).
vc(V) --> aux(V), v(prt).
vc(V) --> mod(V), v(inf).
vc(V) --> v(prt), aux(V).
vc(V) --> v(inf), mod(V).

%% werkwoorden

aux(fin) --> [Word], {auxiliary(Word)}.
aux(no_aux) --> [].

mod(fin) --> [Word], {modal(Word)}.
mod(no_mod) --> [].

v(fin) --> [Word], {verb(Word,_,_,_)}.
v(fin) --> [Word], {verb(_,Word,_,_)}.
v(prt) --> [Word], {verb(_,_,Word,_)}.
v(inf) --> [Word], {verb(_,_,_,Word)}.
v(no_v) --> []. 

%% np's

np --> det(Lidw), n(Lidw). 
%np --> det(Lidw), n(Lidw), pp.

det(Det) --> [Word], {determiner(Det,Word)}. 

n(Lidw)  --> a,   n(Lidw).
n(Lidw) -->   [Word], {noun(Lidw,Word)}.

a -->  [Word], {adjective(Word)}.

%% woordenboek

adjective(bruine).
adjective(gevaarlijke).
adjective(zeldzame).
adjective(exotische).
adjective(roze).

noun(de,beer).
noun(de,hond).
noun(de,flamingo).
noun(de,olifant).
noun(de,directeur).
noun(het,beest).
noun(het,dier).
noun(het,paard).

determiner(de,de).
determiner(het,het).

auxiliary(heeft).

modal(wil).

verb(denkt,dacht,gedacht,denken).
verb(voert,voerde,gevoerd,voeren).
verb(verhuurt,verhuurde,verhuurd,verhuren).
verb(nadert,naderde,genaderd,naderen).
verb(ontsnapt,ontsnapte,ontsnapt,ontsnappen).
verb(schrikt,schrok,geschrokken,schrikken).
verb(snurkt,snurkte,gesnurkt,snurken).

compl(dat).

%%%%%%%%%%%%%%%% testing example sentences %%%%%%%%%%%%%%%%%%%%%%%%

sentence(1,[de,beer,ontsnapt]).
sentence(2,[de,directeur,voert,de,olifant]).

ungrammatical(1,[het,beer,ontsnapt]).
ungrammatical(2,[de,directeur,de,beer,voert]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  from here you should not change anything  %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test :-
    test_sentence,
    test_ungrammatical.

test_sentence :-
    (  sentence(X,Sentence),
       format(user_error,"parsing sentence ~w...~n",[X]),
       parse(Sentence),
       format(user_error,"parsing sentence ~w ok !!!~n",[X]),
       fail
    ;  true
    ).

test_ungrammatical :-
    (  ungrammatical(X,Sentence),
       format(user_error,"parsing ungrammatical sentence ~w...~n",[X]),
       parse(Sentence),
       format(user_error,"parsing ungrammatical sentence ~w ok ??? ~n",[X]),
       fail
    ;  true
    ).

%% pprint a tree
pprint_tree(Tree) :-
    (   ground(Tree)
    ->  (   pprint_this_line(Tree,0,0)
        ->  nl, nl
        ;   format(user_error,"ERROR: Parse boom bevat fouten: ~w~n",[Tree]),
            fail
        )
    ;   format(user_error,"ERROR: Parse boom bevat variabelen: ~w~n",[Tree]),
        fail
    ).

%% parse a string, and pprint the result
parse(String) :-
    current_predicate(s/2),  % prevent potential error message
    s(String,[]).
parse(String) :-
    current_predicate(s/3),  % prevent potential error message
    s(Tree,String,[]),
    pprint_tree(Tree).

%%% pretty printer voor (horizontaal) printen van bomen van de vorm

%%% Tree is van de vorm: b(Knoop,Dochters). Knoop moet een atom zijn (geen prolog term).  
%%% Dochters is een lijst.

%%% vb: b(np,[b(det,[w(de)]),b(n,[w(man)]),b(pp,[b(p,[w(uit)]),b(np,[w(groningen)])])])

%%% Heb je labels/knopen die langer dan 6 posities zijn, dan moet je 8 en 6 in decode
%%% hieronder door hogere getallen vervangen (bv 12 en 10). De code maakt gebruik van
%%% de sicstus prolog built-in format/2, zie de sicstus manual voor tekst en uitleg. 

pprint_this_line(b(Mother,Daughters),Indent,Dashes) :-
    format(' ~*c ~a',[Dashes,45,Mother]),
    dashes(Mother,NewDashes),
    NewIndent is Indent + 8, 
    pprint_daughters(Daughters,NewIndent,NewDashes).
pprint_this_line(w(Word),_,Dashes) :-
    format(' ~*c ~a',[Dashes,45,Word]).

pprint_next_line(b(Mother,Daughters),Indent,Dashes) :-
    Spaces is Indent - Dashes, 
    format('~n~*c ~*c ~a',[Spaces,32, Dashes,45,Mother]),
    dashes(Mother,NewDashes),
    NewIndent is Indent + 8, 
    pprint_daughters(Daughters,NewIndent,NewDashes).

pprint_daughters([D|Ds],Indent,Dashes) :-
    pprint_this_line(D,Indent,Dashes),
    pprint_next_daughters(Ds,Indent,Dashes). 
    
pprint_next_daughters([],_,_).
pprint_next_daughters([D|Ds],Indent,Dashes) :-
    pprint_next_line(D,Indent,Dashes),
    pprint_next_daughters(Ds,Indent,Dashes).

dashes(Atom,N) :-
    name(Atom,Chars),
    length(Chars,C),
    N is 6 - C.

    

