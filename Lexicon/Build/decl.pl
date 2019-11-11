:- expects_dialect(sicstus).

:- use_module(library(lists)).
:- use_module(hdrug(hdrug_util)).
:- use_module(alpino('src/latin1')).
:- use_module(alpino('src/utils')).
:- use_module(add_compounds).

:- use_module(adjectives:adjectives).
:- use_module(verbs:verbs).
:- use_module(nouns:nouns).
:- use_module(adverbs:adverbs).
:- use_module(numbers:numbers).
:- use_module(misc:misc).

mem_eq(W0,W) :-
    (	var(W0)
    ->	fail
    ;   W0 = [_|_]
    ->	lists:member(W,W0)
    ;   W0=W
    ).

create_entries :-
    (   m(Stam0,Cat1,WordOrWords),
	hdrug_flag(expand_subcat,OnOff),
	expand_sc_list(OnOff,Cat1,Cat),
	(   atomic(WordOrWords)
	->  WordOrWords=Word,
	    Rest=[]
	;   WordOrWords=[Word|Rest]
	),
	(   Cat = with_dt(_,_)
	->  generate_with_dt_stem(Cat,Stam)
	;   Stam0 = Stam
	),
	assertz(entry(Word,Cat,Stam,Rest)),
	fail
    ;   true
    ).
    
expand_sc_list(off,L,L).
expand_sc_list(on,verb(HZ0,Infl,List0),verb(HZ,Infl,List)) :-
    !,
    (   HZ0=HZ,
        expand_sc_list_(List0,List,List0)
    ;   %% "we zijn uitgeluld"
        (HZ0=hebben;HZ0='hebben/zijn'),
        lists:member(intransitive,List0),
        Infl=psp,
        HZ=unacc,
        List=[part_intransitive(uit)]
    ).
expand_sc_list(on,V,V).

expand_sc_list_([],[],_).
expand_sc_list_([H0|T0],List,Orig) :-
    expand_sc_list__(H0,Orig,List,List1),
    expand_sc_list_(T0,List1,Orig).

expand_sc_list__(H0,Orig,List0,List) :-
    findall(Sc,add_new_subcat_frame(H0,Sc,Orig),List0,List).

add_new_subcat_frame(X,X,_).

%% tegen NP aan ==> omdat hij tegen me aanbotst
%%                  omdat hij aan botst tegen ..
%%                  omdat om deze ontwikkelingen niemand heen kan
add_new_subcat_frame(ld_pp,part_ld_pp(Part,Prep),Orig) :-
    postposition(Part,Prep),
    \+ lists:member(part_ld_pp(Part),Orig).
add_new_subcat_frame(np_ld_pp,part_np_ld_pp(Part,Prep),Orig) :-
    postposition(Part,Prep),
    \+ lists:member(part_np_ld_pp(Part),Orig).

%% NP in ==> omdat hij het bos infietst
%%           omdat hij het bos net in fietste
add_new_subcat_frame(ld_dir,part_ld_transitive(Part),Orig) :-
    postposition(Part),
    \+ lists:member(part_transitive(Part),Orig).
add_new_subcat_frame(np_ld_dir,part_np_ld_transitive(Part),Orig) :-
    postposition(Part),
    \+ lists:member(part_np_np(Part),Orig).
add_new_subcat_frame(refl_ld_dir,part_refl_ld_transitive(Part),Orig) :-
    postposition(Part),
    \+ lists:member(part_refl_np(Part),Orig).

%% tussen NP ==> omdat er iets tussenkomt
%% also for pc "omdat hij daar wel snel achterkwam"?
add_new_subcat_frame(ld_pp,part_ld_er_transitive(Part),Orig) :-
    (   Part=af
    ;   Part=door
    ;   Part=tussen             % TODO: others???
    ;   Part=heen
    ;   Part=uit
    ;   Part=vandoor
    ;   Part=in
    ;   Part=vandaan
    ;   Part=vanaf
    ),
    \+ lists:member(part_transitive(Part),Orig).

add_new_subcat_frame(np_ld_pp,part_np_ld_er_transitive(Part),Orig) :-
    (   Part=af
    ;   Part=door
    ;   Part=tussen             % TODO: others???
    ;   Part=heen
    ;   Part=uit
    ;   Part=vandoor
    ;   Part=in
    ;   Part=vandaan
    ;   Part=vanaf
    ),
    \+ lists:member(part_np_np(Part),Orig).

add_new_subcat_frame(er_pp_sbar(Prep),part_er_sbar(Prep),Orig) :-
    (   Prep = achter
    ;   Prep = onderuit
    ;   Prep = in
    ;   Prep = uit
    ),
    \+ lists:member(part_sbar(Prep),Orig),
    \+ lists:member(part_np_sbar(Prep),Orig).

add_new_subcat_frame(er_pp_vp(Prep),part_er_vp(Prep),Orig) :-
    (   Prep = achter
    ;   Prep = onderuit
    ;   Prep = in
    ;   Prep = uit
    ),
    \+ lists:member(part_vp(Prep),Orig),
    \+ lists:member(part_np_vp(Prep),Orig).

add_new_subcat_frame(intransitive,part_intransitive(Voort),_Orig) :-
    productive_particle(Voort).

%% eropuit = er op uit
add_new_subcat_frame(part_pc_pp(uit,op),pc_pp(uit_op),_).
add_new_subcat_frame(part_er_pp_vp(uit,op),er_pp_vp(uit_op),_).
add_new_subcat_frame(part_er_pp_sbar(uit,op),er_pp_sbar(uit_op),_).



%% only those that are not also adverb..., reduce boring ambiguities
productive_particle(door).
productive_particle(raak).  % hij kletst maar wat raak
productive_particle(voort).

% productive_particle(mee).
% productive_particle(verder).

particle_form(PartVerb,Verb,Part) :-
    entry(Verb,verb(_,_,ScList),_,[]),
    lists:member(Sc,ScList),
    part_sc(Sc,Part),
    atom(Part),
    (   atom_concat(Part,Verb,PartVerb)
    ;   ends_in_vowel(Part),
	starts_with_vowel(Verb),
	concat_all([Part,'-',Verb],PartVerb)
    ),
    \+ illegal_particle_form(PartVerb).

illegal_particle_form(rondis).
illegal_particle_form(toeis).
illegal_particle_form(afis).
illegal_particle_form(vanafis).
illegal_particle_form(wegis).
illegal_particle_form(omis).
illegal_particle_form(inis).
illegal_particle_form(opis).
illegal_particle_form(overis).
illegal_particle_form(dooris).
illegal_particle_form(langsis).
illegal_particle_form(uitis).

ends_in_vowel(Part) :-
    atom_concat(_,a,Part).
ends_in_vowel(Part) :-
    atom_concat(_,e,Part).
ends_in_vowel(Part) :-
    atom_concat(_,i,Part).
ends_in_vowel(Part) :-
    atom_concat(_,o,Part).
ends_in_vowel(Part) :-
    atom_concat(_,u,Part).

starts_with_vowel(V) :-
    atom_concat(a,_,V).
starts_with_vowel(V) :-
    atom_concat(e,_,V).
starts_with_vowel(V) :-
    atom_concat(i,_,V).
starts_with_vowel(V) :-
    atom_concat(o,_,V).
starts_with_vowel(V) :-
    atom_concat(u,_,V).

part_sc(Term,Part) :-
    functor(Term,Fun,_),
    atom_concat(part_,_,Fun),
    arg(1,Term,Part).
part_sc(Frame,Part) :-
    lists:member(Frame,[ld_dir,np_ld_dir,refl_ld_dir]),
    postposition(Part).

%% Build index of words with diacritics

contains_accent([]) :-
    false.
contains_accent([In0|_]) :-
    isaccented(In0), !.
contains_accent([_|Ins]) :-
    contains_accent(Ins).

check_wrong_arity :-
    (   wrong_arity_goal(G),
        try_hook(G,fail),
	format(user_error,"*** error: not a lexical entry: ~w.~n",[G]),
	fail
    ;   true
    ).

wrong_arity_goal(adjectives:a(_)).
wrong_arity_goal(adjectives:a(_,_)).
wrong_arity_goal(adjectives:a(_,_,_,_,_)).

wrong_arity_goal(adverbs:adverb(_,_)).
wrong_arity_goal(adverbs:adverb(_,_,_)).
wrong_arity_goal(adverbs:adverb(_,_,_,_)).

wrong_arity_goal(numbers:number(_)).
wrong_arity_goal(numbers:number(_,_,_)).
wrong_arity_goal(numbers:number(_,_,_,_)).
wrong_arity_goal(numbers:number(_,_,_,_,_)).

wrong_arity_goal(nouns:n(_)).
wrong_arity_goal(nouns:n(_,_)).
wrong_arity_goal(nouns:n(_,_,_,_,_)).

wrong_arity_goal(verbs:v(_)).
wrong_arity_goal(verbs:v(_,_)).
wrong_arity_goal(verbs:v(_,_,_)).
wrong_arity_goal(verbs:v(_,_,_,_)).
wrong_arity_goal(verbs:v(_,_,_,_,_)).
wrong_arity_goal(verbs:v(_,_,_,_,_,_)).
%wrong_arity_goal(verbs:v(_,_,_,_,_,_,_,_)).
wrong_arity_goal(verbs:v(_,_,_,_,_,_,_,_,_)).

wrong_arity_goal(misc:m(_)).
wrong_arity_goal(misc:m(_,_)).
wrong_arity_goal(misc:m(_,_,_,_)).
wrong_arity_goal(misc:m(_,_,_,_,_)).

wrong_arity_goal(misc:with_dt(_)).
wrong_arity_goal(misc:with_dt(_,_)).
wrong_arity_goal(misc:with_dt(_,_,_,_)).
wrong_arity_goal(misc:with_dt(_,_,_,_,_)).

wrong_arity_goal(misc:punct(_)).
wrong_arity_goal(misc:punct(_,_,_)).
wrong_arity_goal(misc:punct(_,_,_,_)).

wrong_arity_goal(misc:tag(_,_)).
wrong_arity_goal(misc:tag(_,_,_)).
wrong_arity_goal(misc:tag(_,_,_,_)).

wrong_arity_goal(misc:particle(_,_)).
wrong_arity_goal(misc:particle(_,_,_)).
wrong_arity_goal(misc:particle(_,_,_,_)).

wrong_arity_goal(misc:preposition(_,_,_)).
wrong_arity_goal(misc:preposition(_,_,_,_)).
wrong_arity_goal(misc:preposition(_,_,_,_,_)).

wrong_arity_goal(misc:sentence_adverb(_,_)).
wrong_arity_goal(misc:sentence_adverb(_,_,_)).
wrong_arity_goal(misc:sentence_adverb(_,_,_,_)).

wrong_arity_goal(misc:loc_adverb(_,_)).
wrong_arity_goal(misc:loc_adverb(_,_,_)).
wrong_arity_goal(misc:loc_adverb(_,_,_,_)).

wrong_arity_goal(misc:dir_adverb(_,_)).
wrong_arity_goal(misc:dir_adverb(_,_,_)).
wrong_arity_goal(misc:dir_adverb(_,_,_,_)).

wrong_arity_goal(misc:tmp_adverb(_,_,_)).
wrong_arity_goal(misc:tmp_adverb(_,_,_,_)).

wrong_arity_goal(misc:nominalized_adjective(_)).
wrong_arity_goal(misc:nominalized_adjective(_,_,_,_)).
wrong_arity_goal(misc:nominalized_adjective(_,_,_,_,_)).

wrong_arity_goal(misc:measure_noun(_)).
wrong_arity_goal(misc:measure_noun(_,_)).
wrong_arity_goal(misc:measure_noun(_,_,_)).
wrong_arity_goal(misc:measure_noun(_,_,_,_,_)).
wrong_arity_goal(misc:measure_noun(_,_,_,_,_,_)).

wrong_arity_goal(misc:tmp_noun(_)).
wrong_arity_goal(misc:tmp_noun(_,_)).
wrong_arity_goal(misc:tmp_noun(_,_,_,_,_)).
wrong_arity_goal(misc:tmp_noun(_,_,_,_,_,_)).


m(Stam,Cat,Words) :-
    adjectives:m(Stam,Cat,Words).
m(Stam,Cat,Words) :-
    adverbs:m(Stam,Cat,Words).
m(Stam,Cat,Words) :-
    numbers:m(Stam,Cat,Words).
m(Stam,Cat,Words) :-
    nouns:m(Stam,Cat,Words).
m(Stem,Cat,Words) :-
    verbs:m(Stem,Cat,Words).
m(Stem,Cat,Words) :-
    misc:m(Stem,Cat,Words).

m(Stem,fixed_part(F),F) :-
    hdrug_flag(expand_subcat,OnOff),
    verbs:m(_,B0,_),
    expand_sc_list(OnOff,B0,B),
    extract_fixed_verb(B,F),
    concat_all(F,Stem,' ').

m(Stem,fixed_part(F),F) :-
    hdrug_flag(expand_subcat,OnOff),
    adjectives:m(_,B0,_),
    expand_sc_list(OnOff,B0,B),
    extract_fixed_adj(B,F),
    concat_all(F,Stem,' ').

:- initialize_flag(expand_subcat,on).

write_if_accented(Word0,ACCENT) :-
    (   atom(Word0),
        atom_codes(Word0,Chars0),
        contains_accent(Chars0),
        deaccent_chars(Chars0,Chars),
        atom_codes(Word,Chars),
        format(ACCENT,"~q.~n",[accent(Word,Word0)]),
        fail
    ;   true
    ).

particle_form_to_code(PartVerb,Verb,Part) :-
    particle_form(PartVerb,Verb,Part).

%:- bb_put(trace_dict,none).
%
%trace_dict(N) :-
%    functor(N,F,_),
%    bb_get(trace_dict,M),
%    (   F == M
%    ->  true
%    ;   bb_put(trace_dict,F),
%	debug_message(2,"constructing ~w~n",[F])
%    ).

%% entry point
%% FORMAT: SURF+STEM+TAG where + is the tab
%%
list_entries :-
    create_entries,
    open('lex.t',           write, DICT),
    open('lex_inv.t',       write, INVLEX),
    open('lex_accent0.pl',write, ACCENT),
    open('lex_with_dt0.pl',   write, WITHDT),
    open('lex_prefix.t',    write, PREFIX),
    open('lex_lemma0.pl',   write, LEMMA),
    (   entry(A,B,C,Rest),
	prettyvars(B),
%	trace_dict(B),
	(   Rest==[]
	->  format(DICT,"~w\t~q\t~q~n",[A,C,B]),
            Surf=A
	;   length(Rest,Length),
	    concat_all([A|Rest],Surf,' '),
	    format(DICT,"~w\t~q\t~q~n",[Surf,C,B]),
	    format(DICT,"~w\t~q\t~q~n",[A,A,'#'(Length)])
	),
        write_with_dt(B,C,WITHDT),
        write_inv_lex(C,Surf,B,INVLEX),
        write_exc_inv_lex(B,C,Surf,INVLEX),
	write_if_accented(A,ACCENT),
        create_part_verb(Rest,A,B,C,INVLEX,LEMMA),
	fail
    ;   % trace_dict(prefix),
	particle_form(A,Verb,Part),
	format(PREFIX,"~w\t~w\t~w~n",[A,Verb,Part]),
	fail
    ;   true
    ),
    close(DICT),
    close(INVLEX),
    close(ACCENT),
    close(PREFIX),
    close(WITHDT),
    close(LEMMA),
    check_wrong_arity.

write_with_dt(Tag,Stem,Stream) :-
    Tag = with_dt(_,_),
    !,
    atom_codes(Stem,Codes),
    alpino_util:split_string(Codes," ",Roots),
    format_with_dt(Roots,Atoms,Stream),
    format_with_dt2(Atoms,Stream).
write_with_dt(_,_,_).

format_with_dt2([H|T],Stream) :-
    format(Stream,"with_dt_all(~q,~q).~n",[H,T]).

format_with_dt([],[],_).
format_with_dt([H|T],[Atom|Atoms],Stream) :-
    atom_codes(Atom,H),  % create atom first, because we need ~q 
    format(Stream,"with_dt_root(~q).~n",[Atom]),
    format_with_dt(T,Atoms,Stream).

write_exc_inv_lex(Tag,Root,Surf,INVLEX) :-
    (   write_exc_pattern(Tag,Surf,NewSurf,Root,NewRoot),
        write_inv_lex(NewRoot,NewSurf,Tag,INVLEX),
        fail
    ;   true
    ).

write_inv_lex(v_root(Root,_),Surf,Tag,INVLEX) :-
    !,
    write_inv_lex(Root,Surf,Tag,INVLEX).
write_inv_lex(Root,Surf,Tag,INVLEX) :-
    (   Tag = with_dt(_,_)
    ->  format(INVLEX,"~w\t~w~n",[Root,Surf])
    ;   Root == Surf
    ->  true
    ;   format(INVLEX,"~w\t~w~n",[Root,Surf])
    ).

rest_goals([H|T],Ws0,Ws,Goals) :-
    rest_goals(T,H,Ws0,Ws,Goals).

rest_goals([],H,Ws0,Ws,next_word(H,Ws0,Ws)).
rest_goals([N|T],H,Ws0,Ws,(next_word(H,Ws0,Ws1),Goals)) :-
    rest_goals(T,N,Ws1,Ws,Goals).

extract_fixed_verb(verb(_,_,List),Fixed) :-
    extract_fixed_v(List,Fixed).

extract_fixed_adj(adjective(_,fixed(FixedEls)),Fixed) :-
    lists:member(El,FixedEls),
    fixed(El,Fixed).

extract_fixed_v(List,Fixed) :-
    (  lists:member(fixed(FixedEls,_),List)
    ;  lists:member(part_fixed(_,FixedEls,_),List)
    ),
    lists:member(El,FixedEls),
    fixed(El,Fixed).

fixed([Hw|Tw],[Hw|Tw]).
fixed({List},Fixed) :-
    lists:member(El,List),
    fixed(El,Fixed).

%% na?
postposition(achterna).
postposition(af).
postposition(binnen).
postposition(door).
postposition(in).
postposition(langs).
postposition(om).
postposition(op).
postposition(over).
postposition(rond).
postposition(tegemoet).
postposition(uit).
postposition(vanaf).

postposition(aan,achter).
postposition(aan,tegen).
postposition(af,op).
postposition(af,van).
postposition(door,achter).
postposition(door,onder).
postposition(door,tussen).
postposition(heen,door).
postposition(heen,langs).
postposition(heen,om).
postposition(heen,over).
postposition(in,tegen).
postposition(in,tussen).
postposition(langs,achter).
postposition(toe,naar).
postposition(uit,achter).
postposition(uit,van).
postposition(vanaf,van).
postposition(vandaan,van).
postposition(vandaan,achter).
postposition(vandaan,onder).
postposition(vandaan,elders).


    
create_part_verb([],A,B,Root0,INVLEX,LEMMA) :-
    (   (   Root0 = v_root(Root1,Inf)
	->  format(LEMMA,"~q .~n",[lemma_root(Inf,Root1)])
	;   true
	),
        one_frame(B,Frame),
	adapt_part_label(Frame,Part,Root0,Root2),
	(   Root2 = v_root(Root3,Inf2)
	->  format(LEMMA,"~q .~n",[lemma_root(Inf2,Root3)])
	;   Root2 = Root3
	),
        add_inflection_particle(Part,A,Surf),
        write_inv_lex(Root3,Surf,Frame,INVLEX),
        write_exc_inv_lex(Frame,Root3,Surf,INVLEX),
        fail
    ;   true
    ).    

one_frame(verb(HZ,Infl,Scs),verb(HZ,Infl,Sc)) :-
    lists:member(Sc,Scs).

adapt_part_label(verb(_A,_B,Frame),Part,Label0,Label) :-
    functor(Frame,Fun,_),
    atom_concat(part_,_,Fun),
    !,
    arg(1,Frame,Part),
    concat_part_to_root(Label0,Part,Label).
adapt_part_label(verb(_A,_B,ninv(_,Frame)),Part,Label0,Label) :-
    functor(Frame,Fun,_),
    atom_concat(part_,_,Fun),
    !,
    arg(1,Frame,Part),
    concat_part_to_root(Label0,Part,Label).

concat_part_to_root(v_root(Stam,Inf),Part,Result) :-
    !,
    hdrug_util:concat_all([Stam,'_',Part],Stam2,''),
    hdrug_util:concat_all([Part,'_',Inf],Inf2,''),
    Result = v_root(Stam2,Inf2).
concat_part_to_root(Label0,Part,Label) :-
    hdrug_util:hdrug_flag(root_of_verb_uses_inf,Flag),
    concat_part_to_root(Flag,Label0,Part,Label).

concat_part_to_root(On,Label0,Part,Label) :-
    (   On == on
    ->  hdrug_util:concat_all([Part,'_',Label0],Label,'')
    ;   hdrug_util:concat_all([Label0,'_',Part],Label,'')
    ).

add_inflection_particle(Part,Inflection0,Inflection) :-
    atom_concat(Part,Inflection0,Inflection).
add_inflection_particle(_,Inflection,Inflection).

simplify_root(Root0,Root) :-
    (   var(Root0)
    ->  fail
    ;   Root0 = v_root(Stem,_Inf)
    ->  Root = Stem
    ;   Root0 = Root
    ).

adj_e(e).
adj_e(ende).
adj_e(ge_e).
adj_e(ere).
adj_e(ste).

no_e(postn_no_e(_),no_e).
no_e(no_e(_),no_e).
no_e(both(_),no_e).
no_e(er(_),  er).

write_exc_pattern(verb(_,inf,_),Surf,NewSurf,Stem,Stem) :-
    atom_concat(Surf,den,NewSurf).
write_exc_pattern(verb(_,inf(no_e),_),Surf,NewSurf,Stem,Stem) :-
    atom_concat(Surf,den,NewSurf).
write_exc_pattern(verb(_,inf,_),Surf,NewSurf,Stem,Stem) :-
    atom_concat(Surf,de,NewSurf).
write_exc_pattern(verb(_,inf(no_e),_),Surf,NewSurf,Stem,Stem) :-
    atom_concat(Surf,de,NewSurf).
write_exc_pattern(verb(_,inf,_),Surf,NewSurf,Stem,Stem) :-
    atom_concat(Surf,d,NewSurf).
write_exc_pattern(verb(_,inf(no_e),_),Surf,NewSurf,Stem,Stem) :-
    atom_concat(Surf,d,NewSurf).
write_exc_pattern(adjective(E),Surf,NewSurf,Stem,Stem) :-
    adj_e(E),
    atom_concat(Surf,n,NewSurf).
write_exc_pattern(verb(_,Sg,_),Surf,NewSurf,Stem,Stem) :-
    (   Sg = sg ; Sg = sg1 ),
    atom_concat(ge,Surf,NewSurf).
write_exc_pattern(adjective(NoE),Surf,NewSurf,Stem,NewStem) :-
    no_e(NoE,_),
    atom_concat(Stem,'_heid',NewStem),
    atom_concat(Surf,heid,NewSurf).
write_exc_pattern(adjective(NoE),Surf,NewSurf,Stem,NewStem) :-
    no_e(NoE,_),
    atom_concat(Stem,'_heid',NewStem),
    atom_concat(Surf,heden,NewSurf).
write_exc_pattern(adjective(NoE),Surf,NewSurf,Stem,Stem) :-
    no_e(NoE,_),
    atom_concat(Surf,s,NewSurf).
write_exc_pattern(adjective(NoE,_),Surf,NewSurf,Stem,Stem) :-
    no_e(NoE,_),
    atom_concat(Surf,s,NewSurf).
write_exc_pattern(adjective(st(_)),Surf,NewSurf,Stem,Stem) :- % NB
    atom_concat('het ',Surf,NewSurf).
write_exc_pattern(adjective(ste),Surf,NewSurf,Stem,Stem) :- % NB
    atom_concat('het ',Surf,NewSurf).


generate_with_dt_stem(with_dt(_,Dt),Stem) :-
    roots_from_dt(Dt,Roots0,[]),
    sort_not_unique(Roots0,Roots1),
    %% Daniel: Any chance of having duplicates? 
    %% GJ: Would it matter?
    %% GJ: YES. If a sentence contains "was" twice, then lookup
    %% is attempted of "was" once, which is slow (and perhaps could
    %% even succeed...
    hdrug_util:concat_all(Roots1,Stem,' ').

sort_not_unique(List,Sorted) :-
    add_vals(List,KeyList),
    keysort(KeyList,SortedKeyList),
    del_vals(SortedKeyList,Sorted).

add_vals([],[]).
add_vals([H|T0],[H-_|T]) :-
    add_vals(T0,T).

%% keys with args swappen. Have first arg indexing.
del_vals([],[]).
del_vals([H-_|T0],[H|T]) :-
    del_vals(T0,T).

%% Extract roots from a with_dt frame.
%% GJ: added cases for ix(..)
%%     changed order of 2 and 3 argument, to be standard DCG ordering
roots_from_dt(dt(_,Ds),Roots0,Roots) :-
    roots_from_dt_ds(Ds,Roots0,Roots).
roots_from_dt(_=R,Roots0,Roots) :-
    roots_from_dt(R,Roots0,Roots).
roots_from_dt(l(Root,_,_,_,_),Roots0,Roots):-
    roots_from_l(Root,Roots0,Roots).
roots_from_dt(l(Root,_,_,_),Roots0,Roots):-
    roots_from_l(Root,Roots0,Roots).
roots_from_dt(orig(_),Roots,Roots).
roots_from_dt(ix(_),Roots,Roots).
roots_from_dt(ix(_,B),Roots0,Roots) :-
    roots_from_dt(B,Roots0,Roots).

%roots_from_dt(l(Root,_,_,_),[Root|Roots],Roots).
%roots_from_dt(l(Root,_,_,_,_),[Root|Roots],Roots).

roots_from_l(v_root(Root,_),L0,L) :-
    !,
    L0 = [Root|L].
roots_from_l(Root,[Root|Roots],Roots).

roots_from_dt_ds([],Roots,Roots).
roots_from_dt_ds([H|T],Roots0,Roots) :-
    roots_from_dt(H,Roots0,Roots1),
    roots_from_dt_ds(T,Roots1,Roots).


