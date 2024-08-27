:- module(alpino_lex,[]).

:- expects_dialect(sicstus).

:- use_module(library(terms)).

:- discontiguous
    abbreviation/2,
    abbreviation/4,
    parse_only_lex/3,
    parse_only_lex/5,
    inv_abbreviation/2,
    spelling_variant/2,
    inv_spelling_variant/2,
    spelling_variant21/3,
    inv_spelling_variant21/3,
    spelling_variant31/4,
    inv_spelling_variant31/4,
    context_spelling_variant_context/6,
    lexicon_fallback_/7,
    lexicon_/6,
    lexicon__/7.


:- use_module(alpino('src/latin1')).

:- ensure_loaded(lex_more).
:- ensure_loaded(lex_accent).
:- ensure_loaded(lex_with_dt).
:- ensure_loaded(lex_lemma).

:- multifile user:term_expansion/2.
user:term_expansion(spelling_variant(A,B),
                    [spelling_variant(A,B),
                     inv_spelling_variant(B,A)]).
user:term_expansion(abbreviation(A,B),
                    [abbreviation(A,B),
                     inv_abbreviation(B,A)]).
user:term_expansion(spelling_variant21(A,B,C),
                    [spelling_variant21(A,B,C),
                     inv_spelling_variant21(C,A,B)]).
user:term_expansion(spelling_variant31(A,B,C,D),
                    [spelling_variant31(A,B,C,D),
                     inv_spelling_variant31(D,A,B,C)]).

lexicon(Cat,Label,In,Out,His) :-
    lexicon(Cat,Label,In,Out,His,[]).

special_lexicon(Cat,Label,L0,L,exception,_) :-
    exception(Cat,Label,L0,L).

%% lookup results are ordered and unique:
lexicon(Cat,Label,In,Out,His,LC):-
    lex_initialize,
    lexicon_continue(Cat,Label,In,Out,His,LC).

lexicon_continue(Cat,Label,In,Out,His,LC) :-
    findall(l(Cat,Label,In,Out,His),
	    lexicon_with_check(Cat,Label,In,Out,His,LC),
	    [LsH|Ls0]),
    !,
    sort([LsH|Ls0],Ls),
    member_with_history_check(Ls,Cat,Label,In,Out,His).

lexicon_continue(Cat,Label,In,Out,His,LC):-
    findall(l(Cat,Label,In,Out,His),
	    lexicon_fallback_with_check(Cat,Label,In,Out,His,LC),
	    [LsH|Ls0]),
    sort([LsH|Ls0],Ls),
    member_with_history_check(Ls,Cat,Label,In,Out,His).

%% sorted, so if equal except for history, they should be consecutive
member_with_history_check([l(C0,L0,I0,O0,H0)|Tail],C,L,I,O,H) :-
    member_with_history_check(Tail,C0,L0,I0,O0,H0,C,L,I,O,H).

member_with_history_check([l(C0,L0,I0,O0,H1)|Tail],C0,L0,I0,O0,H0,C,L,I,O,H) :-
    !,  % ignore this one
    lists:append(Words,O0,I0),
    hdrug_util:debug_message(2,"ignore duplicated word: ~w ~w ~w ~w ~w~n",
			     [C0,L0,Words,H0,H1]),
    (   H1 == normal
    ->  H2 = H1
    ;   H2 = H0
    ),
    member_with_history_check(Tail,C0,L0,I0,O0,H2,C,L,I,O,H).
member_with_history_check(_,C,L,I,O,H,C,L,I,O,H).
member_with_history_check([l(C0,L0,I0,O0,H0)|Tail],_,_,_,_,_,C,L,I,O,H) :-
    member_with_history_check(Tail,C0,L0,I0,O0,H0,C,L,I,O,H).

%% only if no "normal" category exists for a word

lexicon_fallback(Cat,Label,Ws0,Ws,His,LC) :-
    next_word(Word,Ws0,Ws1,Variant,LC),
    combine_his(Variant,His0,His),
    (   long_punct_start(Word,Ws1,_,_,_,_,_)
    ->  fail
    ;   lexicon_fallback_(Word,Cat,Label,Ws1,Ws,His0,LC)
    ).

%% zwemmenden
%% W inf       --> W+den nom_adjective // \+ W
lexicon_fallback_(Word,end_nominalized_adjective,Label,Ws,Ws,'V-den',_) :-
    atom(Word),
    atom_concat(Inf,den,Word),
    xl(Inf,verb(_,InfVal,_Frame),Label,[],[]),
    inf(InfVal).

lexicon_fallback_(Word,end_nominalized_adjective,Label,Ws,Ws,'part-V-den',_) :-
    atom(Word),
    atom_concat(PartInf,den,Word),
    particle_form(PartInf,Part,Inf),
    xl(Inf,verb(_,InfVal,Frame),Label0,[],[]),
    inf(InfVal),
    lists:member(Sc,Frame),
    functor(Sc,Fun,_),
    atom_concat(part_,_,Fun),
    arg(1,Sc,Part),
    concat_part_to_root(Label0,Part,Label).

lexicon_fallback_(Word,ge_nominalized_adjective,Label,Ws,Ws,'A-n',_) :-
    atom(Word),
    atom_concat(Adj,n,Word),
    xl(Adj,adjective(ge_e),Label,[],[]).

lexicon_fallback_(Word,nominalized_adjective,Label,Ws,Ws,'A-n',_) :-
    atom(Word),
    atom_concat(Adj,n,Word),
    xl(Adj,adjective(e),Label,[],[]).

lexicon_fallback_(Word,end_nominalized_adjective,Label,Ws,Ws,'A-n',_) :-
    atom(Word),
    atom_concat(Adj,n,Word),
    xl(Adj,adjective(ende(_)),Label,[],[]).

lexicon_fallback_(Word,nominalized_compar_adjective,Label,Ws,Ws,'A-n',_) :-
    atom(Word),
    atom_concat(Adj,n,Word),
    xl(Adj,adjective(ere),Label,[],[]).

lexicon_fallback_(Word,nominalized_super_adjective,Label,Ws,Ws,'A-n',_) :-
    atom(Word),
    atom_concat(Adj,n,Word),
    xl(Adj,adjective(ste),Label,[],[]).

%adj_e(e).
%adj_e(ende).
%adj_e(ge_e).

%% gezeur/gebel/geloop/gezucht...
%% W sg1       --> ge+W noun
%% v-noun, because adverbs can be used as pre-modifiers:
%% "na enig heen en weer gepraat"
%% is this really true???
lexicon_fallback_(Word,ge_v_noun(intransitive),Word,Ws,Ws,'ge-',_) :-
    atom(Word),
    atom_concat(ge,Stem,Word),
    sg1(Sg),
    xl(Stem,verb(_,Sg,_Frame),_Label,[],[]).

%% ADJ-heid de-noun
lexicon_fallback_(Word,noun(de,count,sg),LiefHeidStem,Ws,Ws,'-heid',_) :-
    atom(Word),
    atom_concat(Lief,heid,Word),
    xl(Lief,adjective(NOE),LiefStem,[],[]),
    no_e(NOE,no_e),
    concat_stems([LiefStem,heid],LiefHeidStem,'').

%% ADJ-heden de-noun
lexicon_fallback_(Word,noun(de,count,pl),LiefHeidStem,Ws,Ws,'-heden',_) :-
    atom(Word),
    atom_concat(Lief,heden,Word),
    xl(Lief,adjective(NOE),LiefStem,[],[]),
    no_e(NOE,no_e),
    concat_stems([LiefStem,heid],LiefHeidStem,'').

next_words([],Ws,Ws,0,_,[]).
next_words([H|T],Ws0,Ws,N,Prev,[V|Vs]) :-
    N > 0,
    next_word(H,Ws0,Ws1,V,[Prev]),
    N1 is N-1,
    next_words(T,Ws1,Ws,N1,H,Vs).

%% next_word/3 and next_word/4
%% should not do anything special in generation mode...

:- dynamic
    m_next_word/5.

:- thread_local
    m_next_word/5.

next_word(Word,In,Out,His) :-
    next_word(Word,In,Out,His,[]).

next_word(Word,In,Out,His,Context) :-
    hdrug_util:hdrug_flag(parse_or_generate,PG),
    next_word_(PG,Word,In,Out,His,Context).

%% treat ~w1~w2~w3 as either w1, w2 or w3
%% so starting ~ is required
next_word_(parse,Next,[Word|Ws],Ws,normal,_) :-
    atom(Word),
    alpino_util:split_atom(Word,"~",['',A|C]),
    !,
    lists:member(Next,[A|C]).

next_word_(_,Word,[Word|Out],Out,normal,_) :-
    atom(Word).

next_word_(parse,Word,[InH|InT],Out,His,Context) :-
    (   var(InT) -> format(user_error,"lex.pl: variable input list???~n",[]) ; true ),
    term_hash(m(InH,InT,Context),Ix),
    (    m_next_word(Ix,InH,InT,Context,List)
    ->   true
    ;    findall(WordX/Out/HisX,next_word__(WordX,[InH|InT],Out,HisX,Context),List),
	 noclp_assertz(m_next_word(Ix,InH,InT,Context,List))
    ),
    lists:member(Word/Out/His,List).

next_word__(Word,[Word1|Out],Out,lc_variant(Word,Word1,Word2),[Word2|LC]) :-
    context_spelling_variant(Word1,Word2,LC,Word).
next_word__(Word,[Word1,Word3|Out],[Word3|Out],lrc_variant(Word,Word1,Word2,Word3),[Word2|LC]) :-
    context_spelling_variant_context(Word1,Word2,LC,Word3,Out,Word).

next_word__(Word,[Word1|Out],Out,variant(Word,Word1),_) :-
    spelling_variant(Word1,Word).
next_word__(Word,[Word1,Word2|Out],[Word2|Out],rc_variant(Word,Word1,Word2),_) :-
    spelling_variant_context(Word1,Word2,Out,Word).
next_word__(Word,[Word1,Word2|Out],Out,variant21(Word,Word1,Word2),_) :-
    spelling_variant21(Word1,Word2,Word).
next_word__(Word,[Word1,Word2,Word3|Out],Out,variant31(Word,Word1,Word2,Word3),_) :-
    spelling_variant31(Word1,Word2,Word3,Word).
next_word__(Word,[Word1,Word2,Word3,Word4|Out],Out,variant41(Word,Word1,Word2,Word3,Word4),_) :-
    spelling_variant41(Word1,Word2,Word3,Word4,Word).
next_word__(Word,[Word1,Word2,Word3,Word4,Word5|Out],Out,variant51(Word,Word1,Word2,Word3,Word4,Word5),_) :-
    spelling_variant51(Word1,Word2,Word3,Word4,Word5,Word).

next_word__(NW,[Word|Ws],Ws,ignore_internal_brackets,_) :-
    atom(Word),
    atom_length(Word,Len),
    %% only if there is a single ( .. ) pair
    %% be robust against blog input such as
    %% (y)(y)(y)(y)(y)(y)(y)(y)(y)(y)(y)(y)(y)(y)(y)(y)(y)(y)(y)(y)(y)(y)(y)(
    findall(Before1,sub_atom(Word,Before1,1,_After1,'('),[Before1]),
    findall(Before2/After2,sub_atom(Word,Before2,1,After2,')'),[Before2/After2]),
    Before2 > Before1,
    sub_atom(Word,0,Before1,_,First),

    (	% medewerk(st)er -> mederwerker
	% speler(s) -> speler
	Before3 is Before2 + 1,
    
	sub_atom(Word,Before3,_,0,Second),
	
	atom_concat(First,Second,NW),
        atom_length(NW,NWLength),
	NWLength > 1
    ;   % medewerk(st)er -> medewerkster
	% speler(s) -> spelers

	\+ ( Before1 =:= 0, After2 =:= 0 ),
	
	Before1a is Before1 + 1,
	Before3 is Before2 + 1,
	After2L is Len - After2 - 1 - Before1a,
	
	sub_atom(Word,Before1a,After2L,_,Second),
	sub_atom(Word,Before3,_,0,Third),
	
	atom_concat(First,Second,NW0),
	atom_concat(NW0,Third,NW)
    ).

%% zo\'n --> zo'n
next_word__(Word,[Word0|Ws],Ws,wrong_quote,_) :-
    atom(Word0),
    sub_atom(Word0,Before1,2,_,'\\\''),
    atom_length(Word0,Len),
    SuffixLength is Len - 1 - Before1,
    sub_atom(Word0,0,Before1,_,Prefix),
    sub_atom(Word0,_,SuffixLength,0,Suffix),
    atom_concat(Prefix,Suffix,Word).

%% i¨  -> ï
next_word__(Word,[Word0|Ws],Ws,wrong_trema,_) :-
    atom(Word0),
    sub_atom(Word0,Before1,2,_,'i¨'),
    sub_atom(Word0,0,Before1,_,Prefix),
    Before3 is Before1 + 2,
    sub_atom(Word0,Before3,_,0,Suffix),
    atom_concat(Prefix,ï,Prefix1),
    atom_concat(Prefix1,Suffix,Word).

%% i´  -> í
next_word__(Word,[Word0|Ws],Ws,wrong_accent,_) :-
    atom(Word0),
    sub_atom(Word0,Before1,2,_,'i´'),
    sub_atom(Word0,0,Before1,_,Prefix),
    Before3 is Before1 + 2,
    sub_atom(Word0,Before3,_,0,Suffix),
    atom_concat(Prefix,í,Prefix1),
    atom_concat(Prefix1,Suffix,Word).

%% zo&apos;n --> zo'n
next_word__(Word,[Word0|Ws],Ws,xml_entity,_) :-
    atom(Word0),
    sub_atom(Word0,_,6,_,'&apos;'),
    atom_concat(Prefix,Rest,Word0),
    atom_concat('&apos;',Suffix,Rest),
    atom_concat(Prefix,'\'',Mid),
    atom_concat(Mid,Suffix,Word).

%% ĳ --> ij
next_word__(Word,[Word0|Ws],Ws,lange_ij,_) :-
    atom(Word0),
    sub_atom(Word0,_,1,_,'ĳ'),
    atom_concat(Prefix,Rest,Word0),
    atom_concat('ĳ',Suffix,Rest),
    atom_concat(Prefix,ij,Mid),
    atom_concat(Mid,Suffix,Word).

%% treat ( ex- ) medewerker as ex-medewerker
next_word__(Medewerker,['(',Ex,')',Medewerker|Ws],Ws,repair_tokenize_brackets,_) :-
    atom(Ex),
    atom_concat(Ex0,'-',Ex),
    atom_codes(Ex0,Codes),
    lists:append(_,[Final],Codes),
    islower(Final),
    \+ xl(Medewerker,punct(_),_,Ws,_).

%% treat ( ex-)minister as ex-minister
next_word__(GrandSlamtoernooi,['(',GS|Ws],Ws,repair_tokenize_brackets,_) :-
    atom(GS),
    sub_atom(GS,_,1,_,')'),
    atom_concat(Prefix,Rest,GS),
    atom_concat(')',Suffix,Rest),
    atom_concat(Prefix,Suffix,GrandSlamtoernooi).

%% treat ( turf-)schip as turfschip
next_word__(GrandSlamtoernooi,['(',GS|Ws],Ws,repair_tokenize_brackets,_) :-
    atom(GS),
    sub_atom(GS,_,2,_,'-)'),
    atom_concat(Prefix,Rest,GS),
    atom_concat('-)',Suffix,Rest),
    atom_concat(Prefix,Suffix,GrandSlamtoernooi).

%% treat ( turf-)schip as schip
next_word__(Suffix,['(',GS|Ws],Ws,repair_tokenize_brackets_robust,_) :-
    atom(GS),
    sub_atom(GS,_,2,_,'-)'),
    atom_concat(_,Rest,GS),
    atom_concat('-)',Suffix,Rest).

%% treat ( Grand Slam-)toernooi as Grand Slam-toernooi
next_word__(Word,['('|Ws0],Ws,skip_l_brack(His),LC) :-
    Ws0 = [W1,GS|Tail],
    atom(GS),
    sub_atom(GS,_,1,_,')'),
    atom_concat(Prefix,Rest,GS),
    atom_concat(')',Suffix,Rest),
    atom_concat(Prefix,Suffix,GrandSlamtoernooi),
    next_word(Word,[W1,GrandSlamtoernooi|Tail],Ws,His,['('|LC]).

next_word__(Autos,[Auto,'\'',s|Ws],Ws,wrong_quote_s,_) :-
    atom(Auto),
    atom_concat(Auto,'\'s',Autos).

next_word__(Autos,[Auto,'\'s'|Ws],Ws,wrong_quote_s,_) :-
    atom(Auto),
    atom_concat(Auto,'\'s',Autos).

next_word__(Autos,[Auto,'&apos;',s|Ws],Ws,xml_entity_s,_) :-
    atom(Auto),
    atom_concat(Auto,'\'s',Autos).

next_word__(Autos,[Auto,'\\u2019s'|Ws],Ws,u2019s,_) :-
    atom(Auto),
    atom_concat(Auto,'\'s',Autos).

next_word__(Autos,[Auto,'\'',n|Ws],Ws,wrong_quote_n,_) :-
    atom(Auto),
    atom_concat(Auto,'\'n',Autos).

next_word__(Word,[QWord|Ws],Ws,_,strange_char222) :-
    atom(QWord),
    sub_atom(QWord,_,_,_,''),
    atom_codes(QWord,QCodes),
    lists:substitute(146,QCodes,39,Codes),
    atom_codes(Word,Codes).

lexicon_with_check(Cat,Label,Ws0,Ws,His,LC) :-
    lexicon_(Cat,Label,Ws0,Ws,His,LC),
    with_dt_check(Cat,Ws0,Ws).

lexicon_fallback_with_check(Cat,Label,Ws0,Ws,His,LC) :-
    lexicon_fallback(Cat,Label,Ws0,Ws,His,LC),
    with_dt_check(Cat,Ws0,Ws).

with_dt_check(with_dt(_,Deriv),Ws0,Ws) :-
    !,
    lists:append(Words,Ws,Ws0),
    length(Words,Span),
    find_span(Deriv,Span).
with_dt_check(_,_,_).

find_span(Deriv,Span) :-
    find_deriv_pos(Deriv,Pos,[]),
    sort(Pos,Pos1),
    lists:last(Pos1,Span).

find_deriv_pos(dt(_,List),Pos0,Pos) :-
    find_deriv_pos_list(List,Pos0,Pos).
find_deriv_pos(orig(_),Pos,Pos).
find_deriv_pos(ix(_),Pos,Pos).
find_deriv_pos(ix(_,Node),Pos0,Pos) :-
    find_deriv_pos(Node,Pos0,Pos).
find_deriv_pos(l(_,_,_,P0,P),Pos0,Pos) :-
    add_deriv_pos(P0,Pos0,Pos1),
    add_deriv_pos(P, Pos1,Pos).
find_deriv_pos(l(_,_,P0,P),Pos0,Pos) :-
    add_deriv_pos(P0,Pos0,Pos1),
    add_deriv_pos(P, Pos1,Pos).

add_deriv_pos(P,Pos0,Pos) :-
    (   integer(P)
    ->  Pos0=[P|Pos]
    ;   lists:append(P,Pos,Pos0)
    ).

find_deriv_pos_list([],Pos,Pos).
find_deriv_pos_list([_=H|T],Pos0,Pos) :-
    find_deriv_pos(H,Pos0,Pos1),
    find_deriv_pos_list(T,Pos1,Pos).

long_punct_start(W,Ws0,Ws,[W|Stems],Stems,punct(X),W) :-
    next_word(W,Ws0,Ws,_),
    punct(W,X).

punct('"',aanhaal_both).  % "
punct('\'',aanhaal_both).
punct('`',aanhaal_links).
punct('``',aanhaal_links).
punct('‘',aanhaal_links).  % enkel rechts
punct('&#8220;',aanhaal_links).  % dubbele aanhalingstekens links
punct('“',aanhaal_links).   % dubbele aanhalingstekens links
punct(',,',aanhaal_links).
punct('\'\'',aanhaal_rechts).
punct('\'\'\'',aanhaal_rechts).
punct('&#8221;',aanhaal_rechts). % idem rechts
punct('”',aanhaal_rechts). % idem rechts
punct('’',aanhaal_rechts).
punct(:,dubb_punt).
punct('(',haak_open).
punct(')',haak_sluit).
punct('{',haak_open).
punct('}',haak_sluit).
punct('\\[',haak_open).
punct('\\]',haak_sluit).
punct('[',haak_open).
punct(']',haak_sluit).
%punct('«',haak_open).
punct('«',aanhaal_links).
%punct('»',haak_sluit).
punct('»',aanhaal_rechts).
punct('<',haak_open).
punct('>',haak_sluit).
punct(..,hellip).
punct(...,hellip).
punct(....,hellip).
punct(.....,hellip).
punct(......,hellip).
punct('…',hellip).
punct('...!',hellip).
punct('...?',hellip).
punct('...!!',hellip).
punct('...??',hellip).
punct(=,is_gelijk).
punct('=>',is_gelijk).
punct('<=',is_gelijk).
punct('<=>',is_gelijk).
punct('==>',is_gelijk).
punct('<==',is_gelijk).
punct('<==>',is_gelijk).
punct('===>',is_gelijk).
punct('<===',is_gelijk).
punct('<===>',is_gelijk).
punct('->',is_gelijk).
punct('<-',is_gelijk).
punct('<->',is_gelijk).
punct('-->',is_gelijk).
punct('<--',is_gelijk).
punct('<-->',is_gelijk).
punct('--->',is_gelijk).
punct('<---',is_gelijk).
punct('<--->',is_gelijk).

punct(',',komma).

punct(-,ligg_streep).
punct('',ligg_streep).
punct('&#8211;',ligg_streep).   % dash
punct('–',ligg_streep).   % dash
punct('&#8212;',ligg_streep).   % mdash
punct('—',ligg_streep).   % mdash
punct('&#8211;',ligg_streep).   % ndash
punct('‰',ligg_streep).   % ndash
punct('­',ligg_streep).  % 173
punct('--',ligg_streep).
punct(['-','-'],ligg_streep).  % trouw2004
punct('---',ligg_streep).
punct('----',ligg_streep).
punct('-----',ligg_streep).
punct('- -',ligg_streep).
punct('_',ligg_streep).  % ad2000...
punct('__',ligg_streep).  % ad2001...
punct('~',ligg_streep).   % twitter
punct('.',punt).
punct(;,punt_komma).
punct('/',schuin_streep).
punct('//',schuin_streep).
punct('///',schuin_streep).
punct('////',schuin_streep).
punct('/////',schuin_streep).
punct('//////',schuin_streep).
punct('\\',schuin_streep).
punct('\\\\',schuin_streep).
punct('\\\\\\',schuin_streep).
punct('\\\\\\\\',schuin_streep).
punct('\\\\\\\\\\',schuin_streep).
punct('!',uitroep).
punct('!!',uitroep).
punct('!!!',uitroep).
punct('!!!!',uitroep).
punct('!!!!!',uitroep).
punct('!!!!!!',uitroep).
punct('!!!!!!!',uitroep).
punct('!!!!!!!!',uitroep).
punct('?',vraag).
punct('??',vraag).
punct('???',vraag).
punct('????',vraag).
punct('?????',vraag).
punct('??????',vraag).
punct('???????',vraag).
punct('????????',vraag).
punct('?!',vraag).
punct('?!?',vraag).
punct('!!?',vraag).
punct('?!?!',vraag).
punct('!?',vraag).
punct('!?!?',vraag).
punct('?!',uitroep).
punct('?!?',uitroep).
punct('!!?',uitroep).
punct('?!?!',uitroep).
punct('!?',uitroep).
punct('!?!?',uitroep).
punct(x,maal).
punct('×',maal).
punct(+,plus).
punct(&,ampersand).
punct('|',staand_streep).
punct('||',staand_streep).
punct('|||',staand_streep).
punct('||||',staand_streep).
punct('|||||',staand_streep).
punct('',aanhaal_rechts).
punct('',aanhaal_links).

punct(Atom,Type) :-
    atom(Atom),
    atom_length(Atom,Len),
    Len > 1,
    atom_codes(Atom,[C|Codes]),
    atom_codes(Catom,[C]),
    punct(Catom,Type0),
    all(Codes,C),
    strange_punct_type(Type0,Type).

strange_punct_type(Punct,Type) :-
    (   Punct == punt
    ->  Type = hellip
    ;   Type =  Punct
    ).

all([],_).
all([H|T],H) :-
    all(T,H).

long_punct(Word,Ws0,Ws,StemL0,StemWord) :-
    (   next_word(Word,Ws0,Ws1,_)
    ->  StemL0=[StemWord|StemL1],
	long_punct(Word,Ws1,Ws,StemL1,StemWord)
    ;   StemL0 = [StemWord],
	Ws0 = Ws
    ).

lexicon_(Cat,Label,Ws0,Ws,His,LC) :-
    next_word(Word,Ws0,Ws1,NextWordHis,LC),
    combine_his(NextWordHis,His0,His),
    lexicon_(Word,Cat,Label,Ws1,Ws,His0,LC).

lexicon_(Cat,Label,Ws0,Ws,His,_) :-
    initials(Label0,Ws0,Ws1),
    next_word(Word,Ws1,Ws2,Variant),
    combine_his(Variant,His0,His),
    (   Cat = proper_name(sg,'PER'),
	in_names_dictionary(proper_name(sg,'PER'),Word,Label1,Ws2,Ws,His0),
	lists:append(Label0,[Label1],Labels),
	hdrug_util:concat_all(Labels,Label,' ')
    ;   atom_concat(Name,'\'s',Word),
	in_names_dictionary(proper_name(sg,'PER'),Name,Label1,Ws2,Ws,His0),
	lists:append(Label0,[Label1],Labels),
	hdrug_util:concat_all(Labels,Label,' '),
	determiner_from_name(proper_name(sg,'PER'),Cat)
    ).

lexicon_(Cat,Label,In,Out,His,_) :-
    phrasal_entry(Cat,Label,His,In,Out).  % in lex_more.pl; currently ignores history from next_word

lexicon_(Word,Cat,Label,Ws1,Ws,His,LC) :-
    long_punct_start(Word,Ws1,Ws2,StemL0,StemL,PunctCat,StemWord),
    !,
    (   % only current word,
	Ws1=Ws, His=normal,
	xl(Word,Cat,Label,[],[])
	
    ;   % or longest unique match for all
	PunctCat=Cat,
	\+ LC = [Word|_],
	His=longpunct,
	long_punct(Word,Ws2,Ws,StemL,StemWord),
	concat_stems(StemL0,Label,' ')
    ).

lexicon_(Word,Cat,Label,Ws1,Ws,His,LC) :-
    lexicon__(Word,Cat,Label,Ws1,Ws,His,LC).

lexicon_(Word,Cat,Name,Ws1,Ws,His,_LC) :-
    in_names_dictionary(Cat,Word,Name,Ws1,Ws,His).

lexicon_(Word,proper_name(X,'LOC'),Name,Ws1,Ws,His,_LC) :-
    in_names_dictionary(proper_name(X,'LOC'),Word,Name,Ws1,Ws2,His),
    loc_suffix(Ws2,Ws).

lexicon_(NoordWord,proper_name(X,'LOC'),Name,Ws1,Ws,His,_LC) :-
    atom(NoordWord),
    loc_prefix(Noord,NoordStem),
    atom_concat(Noord,Word,NoordWord),
    in_names_dictionary(proper_name(X,'LOC'),Word,RestStem,Ws1,Ws,His),
    hdrug_util:concat_all([NoordStem,RestStem],Name,'').

lexicon_(Noord,proper_name(X,'LOC'),Name,[Angola|Ws1],Ws,His,_LC) :-
    atom(Noord),
    ind_loc_prefix(Noord),
    in_names_dictionary(proper_name(X,'LOC'),Angola,_,Ws1,Ws,His),
    hdrug_util:concat_all([Noord,Angola],Name,' ').

lexicon_(Word,proper_name(X,'PER'),Name,Ws1,Ws,His,_LC) :-
    in_names_dictionary(proper_name(X,'PER'),Word,Name0,Ws1,Ws2,His),
    n_word(Next,Ws2,Ws),
    per_suffix(Next),
    hdrug_util:concat_all([Name0,Next],Name,' ').

%% one word genetive names
lexicon_(Word,Cat,Stem,Ws,Ws,gen(His),_LC) :-
    \+ not_a_genitive_name(Word),
    genitive_s(Word,Name),
    in_names_dictionary(Cat0,Name,Stem,[],[],His),
    determiner_from_name(Cat0,Cat).

%% two word genetive names
%% Van Gelders progressieve gedachten over het leiden van een club contrasteerden met ...
lexicon_(Word0,Cat,Stem,[Word|Ws],Ws,gen(His),_LC) :-
    genitive_s(Word,Name),
    in_names_dictionary(Cat0,Word0,Stem,[Name],[],His),
    determiner_from_name(Cat0,Cat).

lexicon_(Word,Cat,Stem,Ws,Ws,plural(His),_LC) :-
    atom(Word),
    atom_concat(Name,'\'s',Word),
    in_names_dictionary(Cat0,Name,Stem,[],[],His),
    plural_from_name(Cat0,Cat).

lexicon_(Word,proper_name(pl,Type),Stem,Ws,Ws,plural(name),_LC) :-
    atom(Word),
    plural_name(Word,Stem,Type).

prefer_his_list([normal|_],[variant(_,_)|_]).
prefer_his_list([_|T0],[_|T]) :-
    prefer_his_list(T0,T).

%% this complication is motivated by the fact that
%% - both
%%   Karel van 't Reve
%%   Karel van het Reve
%% are in the dictionary
%% because 't is a variant of het, we would otherwise get two readings
%% for the occurrence of "Karel van 't Reve" in the input
multi_name(Tag,Word,Name,Ws1,Ws,His,TYPE,DictNo,Number) :-
    findall(Tag/Name/Ws1/Ws/His/TYPE/VariantList,
	    base_multi_name(Tag,Word,Name,Ws1,Ws,His,TYPE,DictNo,Number,VariantList),
	    List
	   ),
    lists:select(Tag/Name/Ws1/Ws/His/TYPE/VariantList,List,List2),
    \+ (   lists:member(Tag/_/Ws1/Ws/His/TYPE/VariantList2,List2),
	   prefer_his_list(VariantList2,VariantList)
       ).

base_multi_name(Tag,Word,Name,Ws1,Ws,His,TYPE,DictNo,Number,VariantList) :-
    next_words(Words,Ws1,Ws,Number,Word,VariantList),
    (   hdrug_util:concat_all([Word|Words],Name,' '),
	pro_fadd:morph_word(Name,DictNo,_,TYPE),
	Tag = proper_name(Both),
	guess_number([Word|Words],Both),
	His = names_dictionary
    ;   lists:append(Prefix,[Last],[Word|Words]),
	atom_concat(LastName,'\'s',Last),
	lists:append(Prefix,[LastName],NameList),
	hdrug_util:concat_all(NameList,Name,' '),
	pro_fadd:morph_word(Name,DictNo,_,TYPE),
	(   Tag = name_determiner(pron),
	    His = gen(names_dictionary)
	;   Tag = proper_name(pl),
	    His = plural(names_dictionary)
	    )
	).

in_names_dictionary(Cat,Word,Name,Ws1,Ws,His) :-
    atom(Word),
    initialize_names_dict(DictNo),
    pro_fadd:morph_word(Word,DictNo,_,Result),
    atom_codes(Result,[ResultH|ResultCodes]),
    (   [ResultH] == "#"
    ->  number_codes(Number,ResultCodes),
	multi_name(Tag,Word,Name,Ws1,Ws,His,TYPE,DictNo,Number)
    ;   TYPE = Result,
	Name = Word,
	Ws1  = Ws,
	Tag  = proper_name(Both),
	guess_number([Word],Both0),
	Both0=Both,
	His  = names_dictionary
    ),
    add_type(TYPE,Tag,Cat0),
    Cat0=Cat.

genitive_s('Ditvoorst','Ditvoorst').  % hack for frequent case in Wikipedia
genitive_s(Word,Name) :-     % Frankrijks opstelling
    remove_s(Word,Name).
genitive_s(Name,Name) :-     % Ajax opstelling, really not correct, but for robustness
    hdrug_util:hdrug_flag(parse_or_generate,parse),
    remove_s(Name).
genitive_s(Word,Name) :-	% Jan's opstelling; Ajax's opstelling
    atom(Word),
    atom_concat(Name,'\'s',Word).
genitive_s(Word,Name) :-	% Ajax' opstelling
    atom(Word),
    atom_concat(Name,'\'',Word),
    remove_s(Name).

determiner_from_name(proper_name(_),name_determiner(pron)).
determiner_from_name(proper_name(_,TYPE),name_determiner(pron,TYPE)).

plural_from_name(proper_name(_),proper_name(pl)).
plural_from_name(proper_name(_,'PER'),proper_name(pl,'PER')).


plural_name('Alma\'s','Alma','ORG').
plural_name('Boeings','Boeing','ORG').
plural_name('Ferrari\'s','Ferrari','ORG').
plural_name('Fokkers','Fokker','ORG').
plural_name('Fords','Ford','ORG').
plural_name('Kuifjes','Kuifje','PER').
plural_name('Harley-Davidsons','Harley-Davidson','ORG').
plural_name('Heinekens','Heineken','PER').
plural_name('Hurricanes','Hurricane','ORG').
plural_name('Japen','Jaap','PER').
plural_name('Lee\'s','Lee','PER').
plural_name('Limburgen','Limburg','GEO').
plural_name('Mercedessen','Mercedes','ORG').
plural_name('Obama\'s','Obama','PER').
plural_name('Orions','Orion','ORG').
plural_name('Oscars','Oscar','MISC').
plural_name('Pfaffs','Pfaff','PER').
plural_name('Picasso','Picasso\'s','PER').
plural_name('Porsches','Porsche','ORG').
plural_name('Rabo\'s','Rabo','ORG').
plural_name('Santossen','Santos','PER').
plural_name('Spitfires','Spitfire','ORG').
plural_name('UWV\'s','UWV','ORG').
plural_name('Vespa\'s','Vespa','ORG').
plural_name('VVV\'s','VVV','ORG').
plural_name('Willy\'s','Willy','PER').

add_type('MISC',Tag,Tag) :-
    !.
add_type(Label,Tag0,Tag) :-
    Tag0 =.. [Fun|Args0],
    lists:append(Args0,[Label],Args),
    Tag =.. [Fun|Args].

guess_number(Name,Both) :-
    (   number_both(Name)
    ->  Both = both
    ;   Both = sg
    ).

number_both(['Admirals']).
number_both(['Alpen']).
number_both(['Ardennen']).
number_both(['Artsen'|_]).
number_both(['Bahama\'s']).
number_both(['Balearen']).
number_both(['B&W']).
number_both(['B&W.']).
number_both(['B.&W']).
number_both(['B.&W.']).
number_both(['BW']).
number_both(['Borg']).
number_both(['Boston','Bruins']).
number_both(['Boys'|_]).
number_both(['Caraïben']).
number_both(['Clintons']).
number_both(['De','Borg']).
number_both(['De','Heideroosjes']).
number_both(['Deathray','Davies']).
number_both(['De','Nederlanden']).
number_both(['Domobranci']).
number_both(['Drumbassadors']).
number_both(['Dukes'|_]).
number_both(['Filipijnen']).
number_both(['Filippijnen']).
number_both(['FNV-Bondgenoten']).
number_both(['G.S.']).
number_both(['GS']).
number_both(['Hoogovens']).
number_both(['Indiana','Pacers']).
number_both(['Jackson','5']).
number_both(['Jackson','Five']).
number_both(['Kamers'|_]).
number_both(['Kempen']).
number_both(['Lakers']).
number_both(['Les','Bleus']).
number_both(['Molukken']).
number_both(['Mujahedeen']).
number_both(['NS']).
number_both(['Nederlanden']).
number_both(['Pinkstergemeenten']).
number_both(['PS']).  % provinciale staten?
number_both(['Pyreneeen']).
number_both(['Pyreneeën']).
number_both(['Rode','Khmers']).
number_both(['Seychellen']).
number_both(['Staten','Generaal']).
number_both(['Staten-Generaal']).
number_both(['Talibaan']).
number_both(['Taliban']).
number_both(['Tilburg','Trappers']).
number_both(['Tories']).
number_both(['Trappers']).
number_both(['Twin','Towers']).
number_both(['VN']).
number_both(['Vogezen']).
number_both(['VS']).
number_both(['Verenigde','Staten','van','Amerika']).

number_both(List) :-
    lists:member('&',List).
number_both(List) :-
    lists:member('en',List).
number_both(List) :-
    lists:member('and',List).

number_both(List) :-
    lists:last(List,Word),
    alpino_unknowns:decap(Word,Word1),
    number_both_last(Word1).

number_both_last(Word) :-
    xl(Word,noun(_,_,pl),_,[],[]).

number_both_last(Atom) :-
    atom(Atom),
    atom_concat(_,Suffix,Atom),
    plural_suffix(Suffix).

%% Biologische soorten:
plural_suffix(ae).

plural_suffix(admirals).
plural_suffix(alpen).
plural_suffix(angels).
plural_suffix(antillen).
plural_suffix(ardennen).
plural_suffix(auteurs).
plural_suffix(avalanches).
plural_suffix(awards).
plural_suffix(azoren).
plural_suffix(balearen).
plural_suffix('bardi\'s').
plural_suffix(beatles).
plural_suffix(bees).
plural_suffix(blowfish).
plural_suffix(boys).
plural_suffix(breeders).
plural_suffix(brigades).
plural_suffix(brothers).
plural_suffix(bulls).
plural_suffix(busters).
plural_suffix(byrds).
plural_suffix(cardigans).
plural_suffix(carpenters).
plural_suffix(cars).
plural_suffix(cats).
plural_suffix(charlatans).
plural_suffix(chieftains).
plural_suffix(comoren).
plural_suffix(corrs).
plural_suffix(cranberries).
plural_suffix(creatures).
plural_suffix(crowes).
plural_suffix(crows).
plural_suffix(dagen).
plural_suffix(damnations).
plural_suffix(datsuns).
plural_suffix(deftones).
plural_suffix(degrees).
plural_suffix(delgados).
plural_suffix(diamonds).
plural_suffix(diamonds).
plural_suffix(dirtbombs).
plural_suffix(doctors).
plural_suffix(donnas).
plural_suffix(doors).
plural_suffix(drifters).
plural_suffix(drumbassadors).
plural_suffix(dubliners).
plural_suffix(duivels).
plural_suffix('du-tels').
plural_suffix(dutch).
plural_suffix(dutchmen).
plural_suffix(dynamites).
plural_suffix(eagles).
plural_suffix(eilanden).
plural_suffix(emiraten).
plural_suffix(eurythmics).
plural_suffix(falklands).
plural_suffix(feesten).
plural_suffix(fighters).
plural_suffix(files).
plural_suffix(flamingos).
plural_suffix(flatlanders).
plural_suffix(fortunes).
plural_suffix(fugees).
plural_suffix(games).
plural_suffix(gees).
plural_suffix(generals).
plural_suffix(girls).
plural_suffix(globetrotters).
plural_suffix('go-betweens').
plural_suffix('go.go\'s').
plural_suffix(groenen).
plural_suffix(gourds).
plural_suffix(grasshoppers).
plural_suffix(hammers).
plural_suffix(hives).
plural_suffix('houdini\'s').
plural_suffix(hunters).
plural_suffix(islands).
plural_suffix(jazzmeteors).
plural_suffix(jicks).
plural_suffix(jumpers).
plural_suffix(kids).
plural_suffix(kinks).
plural_suffix(koloniën).
plural_suffix(lemonheads).
plural_suffix(lions).
plural_suffix(lips).
plural_suffix(llamas).
plural_suffix(lovers).
plural_suffix(lunatics).
plural_suffix(magyaren).
plural_suffix(makers).
plural_suffix(marileens).
plural_suffix(microphones).
plural_suffix(midlands).
plural_suffix(modes).
plural_suffix(molukken).
plural_suffix(monkees).
plural_suffix(motions).
plural_suffix(movies).
plural_suffix(murphys).
plural_suffix(nationals).
plural_suffix(navigators).
plural_suffix(necks).
plural_suffix(nits).
plural_suffix(nomads).
plural_suffix(olympics).
plural_suffix(oranjes).
plural_suffix(osbournes).
plural_suffix(outsiders).
plural_suffix(pacers).
plural_suffix(paladins).
plural_suffix(papas).
plural_suffix(paralympics).
plural_suffix(peppers).
plural_suffix(persons).
plural_suffix(persuasions).
plural_suffix(pigs).
plural_suffix(pistols).
plural_suffix(pistons).
plural_suffix(plassen).
plural_suffix(players).
plural_suffix(poets).
plural_suffix(posies).
plural_suffix(postmen).
plural_suffix(pretenders).
plural_suffix(pumpkins).
plural_suffix(puritans).
plural_suffix(raden).
plural_suffix(ramblers).
plural_suffix(ramones).
plural_suffix(rangers).
plural_suffix(reds).
plural_suffix(residents).
plural_suffix(rijders).
plural_suffix(rockers).
plural_suffix(roots).
plural_suffix(roses).
plural_suffix(rousers).
plural_suffix(scotsman).
plural_suffix(screws).
plural_suffix(serenes).
plural_suffix(sharks).
plural_suffix(shepherds).
plural_suffix(shoes).
plural_suffix(simpsons).
plural_suffix(singers).
plural_suffix(sisters).
plural_suffix(skidmarks).
plural_suffix(smiths).
plural_suffix(soloists).
plural_suffix(sparks).
plural_suffix(specials).
plural_suffix(spelen).
plural_suffix(spurs).
plural_suffix(states).
plural_suffix(stones).
plural_suffix(stooges).
plural_suffix(streets).
plural_suffix(stripes).
plural_suffix(strokes).
plural_suffix(supremes).
plural_suffix(swingers).
plural_suffix(tartaren).
plural_suffix(thunderbugs).
plural_suffix(tijgers).
plural_suffix(trammps).
plural_suffix(tramps).
plural_suffix(underwolves).
plural_suffix(viewers).
plural_suffix(walkabouts).
plural_suffix(warhols).
plural_suffix(warriers).
plural_suffix(wizards).

lexicon__(Word,Cat,Label,Ws0,Ws,His,_) :-
    lexicon___(Word,Cat,Label,Ws0,Ws,His).

%% W adjective --->  W+s post_adjective
lexicon__(Word0,Tag,Label,Ws,Ws,'Adj-s',_) :-
    remove_s(Word0,Word),
    adj_s_lexicon(Word,Tag,Label).

lexicon__(op,fixed_part(op_een_v),Label,Ws0,Ws,op_een_v,_) :-
    next_word(een,Ws0,Ws1,_),
    next_word(Inf,Ws1,Ws,_),
    lexicon___(Inf,verb(_,InfVal,intransitive),LabelV,Ws,Ws,_),
    inf(InfVal),
    concat_stems([op,een,LabelV],Label,' ').

lexicon__(Imp,verb(HZ,imp(InfVal),fixed([[ze]],no_passive)),Label,Ws0,Ws,v_ze,_) :-
    word_form(ze),
    lexicon___(Imp,verb(HZ,InfVal,intransitive),Label,Ws0,Ws,_),
    sg1(InfVal).

%% zwemmend
%% W inf       ---> W+d adjective // \+ W adj
lexicon__(Word,Cat,Label,Ws,Ws,'V-d'(His),_) :-
    atom(Word),
    atom_concat(Inf,d,Word),
    lexicon___(Inf,verb(_,InfVal,Frame),Label,Ws,Ws,His),
    inf(InfVal),
    gerund_frame(Frame,adjective(end(both)),Cat),
    \+ xl(Word,adjective(_),_,[],[]).

%% zwemmende
%% W inf       --> W+de adjective // \+ W adj
lexicon__(Word,Cat,Label,Ws,Ws,'V-de'(His),_) :-
    atom(Word),
    atom_concat(Inf,de,Word),
    lexicon___(Inf,verb(_,InfVal,Frame),Label,Ws,Ws,His),
    inf(InfVal),
    gerund_frame(Frame,adjective(ende(padv)),Cat),
    \+ xl(Word,adjective(_),_,[],[]).

%% W inf       --> tot W+s toe adj
%% tot vervelens toe; tot bloedens toe
lexicon__(tot,sentence_adverb,Label,Ws0,Ws,'tot V-s toe',_) :-
    next_word(VerbInfS,Ws0,Ws1,_),
    atom(VerbInfS),
    next_word(toe,Ws1,Ws,_),
    atom(VerbInfS),
    atom_concat(VerbInf,s,VerbInfS),
    xl(VerbInf,verb(_,InfVal,_),_,[],[]),
    inf(InfVal),
    hdrug_util:concat_all([tot,VerbInfS,toe],Label,' ').

%% W inf       --> tot W+s aan toe adj
%% tot bloedens aan toe
lexicon__(tot,adverb,Label,Ws0,Ws,'tot V-s aan toe',_) :-
    next_word(VerbInfS,Ws0,Ws1,_),
    atom(VerbInfS),
    next_word(aan,Ws1,Ws2,_),
    next_word(toe,Ws2,Ws,_),
    atom_concat(VerbInf,s,VerbInfS),
    xl(VerbInf,verb(_,InfVal,_),_,[],[]),
    inf(InfVal),
    hdrug_util:concat_all([tot,VerbInfS,aan,toe],Label,' ').

%% TODO: allersterksten
lexicon__(AllerXst,Adj,AllerLabel,Ws,Ws,'aller-Asuper'(His),LC) :-
    atom(AllerXst),
    atom_concat(aller,Xst,AllerXst),
    lexicon__(Xst,Adj0,Label,Ws,Ws,His,LC),
    aller(Adj0,Adj),
    atom_concat(aller_,Label,AllerLabel).

lexicon__(aller,Adj,AllerLabel,Ws0,Ws,'aller-Asuper'(His),LC) :-
    aller_path(Ws0,Ws1,Consumed,[Label]),
    next_word(Xst,Ws1,Ws2,_),
    lexicon__(Xst,Adj0,Label,Ws2,Ws,His,LC),
    aller(Adj0,Adj),
    hdrug_util:concat_all([aller|Consumed],AllerLabel,' ').

lexicon__('aller-',Adj,AllerLabel,Ws0,Ws,'aller-Asuper'(His),LC) :-
    aller_path(Ws0,Ws1,Consumed,[Label]),
    next_word(Xst,Ws1,Ws2,_),
    lexicon__(Xst,Adj0,Label,Ws2,Ws,His,LC),
    aller(Adj0,Adj),
    hdrug_util:concat_all(['aller-'|Consumed],AllerLabel,' ').

lexicon__(op,pp,Label1,Ws0,Ws,'op zijn Belgisch'(His),LC) :-
    lists:member(W/L,[zijn/zijn,
		      'z\'n'/zijn
		      ]),
    next_word(W,Ws0,Ws1,_),
    next_word(Belgisch,Ws1,Ws2,_),
    alpino_unknowns:starts_with_capital(Belgisch),
    lexicon__(Belgisch,adjective(no_e(_)),Label,Ws2,Ws,His,[op,W|LC]),
    hdrug_util:concat_all([op,L,Label],Label1,' ').
    
%% op zijn hoogst / op zijn zachtst (gezegd) / ..
%% TODO: zij speelde op haar best
%%       jij speelde op je best
%% TODO: op zijn Argentijns
%% TODO: make this reversible (by moving into lexicon files)
lexicon__(op,adjective(het_st(adv)),Label1,Ws0,Ws,'op zijn Asuper'(His),LC) :-
    lists:member(W/L,[zijn/  zijn,
		      'z\'n'/zijn,
		      haar/  haar,
		      'd\'r'/haar,
		      mijn/  mijn,
		      'm\'n'/mijn,
		      ons/   ons,
		      jullie/jullie,
		      hun/   hun,
		      je/    je]),
    next_word(W,Ws0,Ws1,_),
    next_word(Word,Ws1,Ws2,_),
    (   lexicon__(Word,adjective(st(_A)),Label,Ws2,Ws,His,[op,W|LC])
    ;   lexicon__(Word,adjective(aller_st(_A)),Label,Ws2,Ws,His,[op,W|LC])
    ;   Word = elfendertigst,
	Label = elfendertigst,
	Ws2 = Ws,
	His=elfendertigst
    ),
    hdrug_util:concat_all([op,L,Label],Label1,' ').

%% VLAAMS: om ter snelst; om ter hardst
lexicon__(om,adjective(het_st(adv)),Label1,Ws0,Ws,'om ter Asuper'(His),LC) :-
    next_word(ter,Ws0,Ws1,_),
    next_word(Word,Ws1,Ws2,_),
    (  lexicon__(Word,adjective(st(_A)),Label,Ws2,Ws,His,[om,ter|LC])
    ;  lexicon__(Word,adjective(aller_st(_A)),Label,Ws2,Ws,His,[om,ter|LC])
    ),
    hdrug_util:concat_all([om,ter,Label],Label1,' ').    

%% zij is het aardigst(e) ---> ambiguous between complex adj and [@np det adj]
%% zij zwemt het hardst(e)
%% zij is het leukste, also analysed as NP
%% since indeed you can add relative clause
lexicon__(het,Adj,Label,[STE|Ws],Ws,'het Asuper'(His),_) :-
    het_ste_tag(Adj,Label0,STE,His),
    hdrug_util:concat_all([het,Label0],Label,' ').
lexicon__('\'t',Adj,Label,[STE|Ws],Ws,'het Asuper'(His),_) :-
    het_ste_tag(Adj,Label0,STE,His),
    hdrug_util:concat_all([het,Label0],Label,' ').
lexicon__('`t',Adj,Label,[STE|Ws],Ws,'het Asuper'(His),_) :-
    het_ste_tag(Adj,Label0,STE,His),
    hdrug_util:concat_all([het,Label0],Label,' ').

lexicon__(om,adverb,Label,[het,STE|Ws],Ws,'om het Asuper'(His),_) :-
    het_ste_tag(_Adj,Label0,STE,His),
    atom_concat('om het ',Label0,Label).
lexicon__(om,adverb,Label,['\'t',STE|Ws],Ws,'om het Asuper'(His),_) :-
    het_ste_tag(_Adj,Label0,STE,His),
    atom_concat('om het ',Label0,Label).
lexicon__(om,adverb,Label,['`t',STE|Ws],Ws,'om het Asuper'(His),_) :-
    het_ste_tag(_Adj,Label0,STE,His),
    atom_concat('om het ',Label0,Label).

lexicon__(Word0,Tag,Label,Ws1,Ws,abbreviation(His),LC) :-
    abbreviation(Word0,Abb,Ws1,Ws),
    atom(Abb),
    lexicon__(Abb,Tag,Label,[],[],His,LC).

lexicon__(Word0,Tag,Label,Ws1,Ws,abbreviation(His),LC) :-
    abbreviation(Word0,[Word|Words],Ws1,Ws),
    lexicon__(Word,Tag0,Label,Words,[],His,LC),
    (   Tag0 = with_dt(Tag,_)
    ->  true
    ;   Tag0 = Tag
    ).
    % otherwise positions in with_dt don't make sense

lexicon__(Word,Cat,Label,Ws0,Ws,variant,LC) :-
    hdrug_util:hdrug_flag(parse_or_generate,parse),
    parse_only_lex(Word,Label,Cat,Ws0,Ws,LC).

lexicon__(Word,Cat,Label,Ws,Ws,variant(His),LC) :-
    hdrug_util:hdrug_flag(parse_or_generate,parse),
    parse_only_variant(Word,Word1,Cat),
    lexicon__(Word1,Cat,Label,Ws,Ws,His,LC).

%% op m'n eentje
lexicon__(op,TAG,Label1,Ws0,Ws,'op mijn eentje',_) :-
    lists:member(W/L,[zijn/  zijn,
		      'z\'n'/zijn,
		      haar/  haar,
		      'd\'r'/haar,
		      mijn/  mijn,
		      'm\'n'/mijn,
		      ons/   ons,
		      jullie/jullie,
		      uw/u,
		      hun/   hun,
		      je/    je]),
    next_word(W,Ws0,Ws1,_),
    next_word(eentje,Ws1,Ws,_),
    sort_not_unique([in,L,één],Roots0),
    hdrug_util:concat_all(Roots0,Label1,' '),
    TAG = with_dt(pp,dt(pp,[hd=l(op,preposition(op,[]),pp,0,1),
			    obj1=dt(np,[det=l(L,determiner(pron),np,1,2),
					hd=l(één,pronoun(nwh,thi,sg,de,both,indef,strpro),2,3)
				       ])
			   ])).

%% in je eentje
lexicon__(in,TAG,Label1,Ws0,Ws,'in mijn eentje',_) :-
    lists:member(W/L,[zijn/  zijn,
		      'z\'n'/zijn,
		      haar/  haar,
		      'd\'r'/haar,
		      mijn/  mijn,
		      'm\'n'/mijn,
		      ons/   ons,
		      jullie/jullie,
		      uw/u,
		      hun/   hun,
		      je/    je]),
    next_word(W,Ws0,Ws1,_),
    next_word(eentje,Ws1,Ws,_),
    sort_not_unique([in,L,één],Roots0),
    hdrug_util:concat_all(Roots0,Label1,' '),
    TAG = with_dt(pp,dt(pp,[hd=l(in,preposition(in,[]),pp,0,1),
			    obj1=dt(np,[det=l(L,determiner(pron),np,1,2),
					hd=l(één,pronoun(nwh,thi,sg,de,both,indef,strpro),2,3)
				       ])
			   ])).


%% special stuff for verbs
%% dat hij daar tegen inbracht dat ...
%% hij bracht daar [tegenin] dat ...
lexicon___(Word,Cat,Label,Ws0,Ws,normal) :-
    xl(Word,Cat1,Label0,Ws0,Ws),
    identical_verbal_infl(Cat1,Cat2,Word,Label0),
    select_subcat_frame(Cat2,Cat3),
    \+ impossible_non_particle_form_cat(Cat3),
    \+ impossible_subcat_infl_combination(Cat3),
    adapt_part_label(Cat3,Label0,Label),
    melt_part_prep(Cat3,Cat4),
    nominalization(Cat4,Cat5,Word),
    iets_adjective(Cat5,Cat,Word).

%% ten ondergaat
lexicon___(ten,Cat,Label,[Word0|Ws0],Ws,'part-V'):-
    hdrug_util:concat_all([ten,Word0],Word,' '),
    particle_form(Word,Part,Verb),
    xl(Verb,verb(X,Y,PartScList),Label0,Ws0,Ws),
    part_sc_list(PartScList,Part,[ScH|ScT]),
    identical_verbal_infl(verb(X,Y,[ScH|ScT]),Cat0,Word,Label0),
    select_subcat_frame(Cat0,Cat1),
    \+ impossible_subcat_infl_combination(Cat1),
    adapt_part_label(Cat1,Label0,Label),
    nominalization(Cat1,Cat,Word).



%% special stuff for verbs
%% partV
lexicon___(Word,Cat,Label,Ws0,Ws,'part-V'):-
    particle_form(Word,Part,Verb),
    xl(Verb,verb(X,Y,PartScList),Label0,Ws0,Ws),
    part_sc_list(PartScList,Part,[ScH|ScT]),
    identical_verbal_infl(verb(X,Y,[ScH|ScT]),Cat0,Word,Label0),
    select_subcat_frame(Cat0,Cat1),
    \+ impossible_subcat_infl_combination(Cat1),
    adapt_part_label(Cat1,Label0,Label),
    nominalization(Cat1,Cat,Word).

aller_path(L,L,M,M).
aller_path(L0,L,[','|M],M) :-
    next_word(',',L0,L,_).
aller_path(L0,L,[',',Aller|M0],M) :-
    next_word(',',L0,L1,_),
    next_word(Aller,L1,L2,_),
    aller_word(Aller),
    aller_path(L2,L,M0,M).

aller_word(aller).
aller_word('aller-').               

adj_s_lexicon(Word,post_adjective(NoE),Label) :-
    xl(Word,adjective(NoE0),Label,[],[]),
    no_e(NoE0,NoE).

adj_s_lexicon(Word,post_adjective(NoE,subject_vp),Label) :-
    xl(Word,adjective(NoE0,subject_vp),Label,[],[]),
    no_e(NoE0,NoE).

adj_s_lexicon(Word,post_adjective(NoE,subject_sbar),Label) :-
    xl(Word,adjective(NoE0,subject_sbar),Label,[],[]),
    no_e(NoE0,NoE).

het_ste_tag(Adj,Label,STE,His) :-
    atom(STE),
    atom_concat(ST,e,STE),
    lexicon_(Adj0,Label,[ST],[],His,[]),
    het_st(Adj0,Adj).
het_ste_tag(Adj,Label,STE,His) :-
    lexicon_(Adj0,Label,[STE],[],His,[]),
    het_st(Adj0,Adj).

het_st(adjective(st(Adv)),         adjective(het_st(Adv))).
het_st(adjective(st(Adv),Sc),      adjective(het_st(Adv),Sc)).
het_st(adjective(aller_st(Adv)),         adjective(het_st(Adv))).
het_st(adjective(aller_st(Adv),Sc),      adjective(het_st(Adv),Sc)).


aller(adjective(st(A)),   adjective(aller_st(A))).
aller(adjective(aller_st(A)),   adjective(aller_st(A))).
aller(adjective(ste),     adjective(ste)).
aller(adjective(st(A),B), adjective(aller_st(A),B)).
aller(adjective(aller_st(A),B), adjective(aller_st(A),B)).
aller(adjective(ste,B),   adjective(ste,B)).

no_e(postn_no_e(_),no_e).
no_e(no_e(_),no_e).
no_e(both(_),no_e).
no_e(er(_),  er).

remove_s(Word0,Word) :-
    atom(Word0),
    atom_concat(Word,s,Word0),
    \+ remove_s(Word).

remove_s(Word) :-
    atom(Word),
    s_atom(Suf),
    atom_concat(_,Suf,Word).

s_atom(s).
s_atom(sch).
s_atom(che).
s_atom(x).
s_atom(vic).

identical_verbal_infl(Cat,Cat,_,_).
identical_verbal_infl(verb(HZ,inf,Sc),verb(HZ,pl,Sc),Word,Label) :-
    \+ ( Word = wezen, Label = v_root(ben,zijn) ),
    \+ xl(Word,verb(HZ,both(pl),Sc),Label,[],[]).
identical_verbal_infl(verb(HZ,inf(no_e),Sc),verb(HZ,pl,Sc), _, _).

gerund_frame(ninv(Sc,_),  A0,A) :-
    gerund_frame(Sc,A0,A).
gerund_frame(pc_pp(_),     adjective(A),adjective(A)       ).
gerund_frame(ld_pp,        adjective(A),adjective(A)       ).
gerund_frame(ld_transitive,        adjective(A),adjective(A)       ).
gerund_frame(transitive,   adjective(A),adjective(A)       ).
gerund_frame(transitive_ndev,   adjective(A),adjective(A)       ).
gerund_frame(transitive_ndev_ndev,   adjective(A),adjective(A)       ).
gerund_frame(intransitive, adjective(A),adjective(A)       ).
gerund_frame(sbar_subj_so_np,adjective(A),adjective(A,subject_sbar)).
gerund_frame(sbar_subj,adjective(A),adjective(A,subject_sbar)).
gerund_frame(vp_subj_so_np,adjective(A),adjective(A,subject_vp)).
gerund_frame(vp_subj,adjective(A),adjective(A,subject_vp)).
gerund_frame(Frame0,       adjective(A),adjective(A,Frame) ) :-
    gerund_frame_map(Frame0,Frame).

gerund_frame_map(ld_transitive,        ld_transitive  ).
gerund_frame_map(np_ld_transitive,     np_ld       ).
gerund_frame_map(refl_ld_transitive,   refl_ld     ).
gerund_frame_map(transitive,           transitive  ).
gerund_frame_map(transitive_ndev,      transitive  ).
gerund_frame_map(transitive_ndev_ndev,      transitive  ).
gerund_frame_map(meas,                 transitive  ).
gerund_frame_map(refl,                 refl        ).
gerund_frame_map(part_intransitive(P), part(P)     ).
gerund_frame_map(ap_copula,            ap_pred     ).
gerund_frame_map(ap_copula(T),         ap_pred(T)  ).
gerund_frame_map(nonp_copula,          pred        ).
gerund_frame_map(copula,               pred        ).
gerund_frame_map(so_copula,            pred_so_np  ).
gerund_frame_map(so_nonp_copula,       pred_so_np  ).
gerund_frame_map(so_ap_copula,         pred_so_np  ).
gerund_frame_map(pc_pp(Prep),	       pp(Prep)    ).
gerund_frame_map(np_np,                np_np       ).
gerund_frame_map(so_meas,              np_np       ).
gerund_frame_map(refl_np,              refl_np     ).
gerund_frame_map(sbar,                 object_sbar ).
gerund_frame_map(tr_sbar,	       object_sbar ).
gerund_frame_map(vp,                   object_vp   ).
gerund_frame_map(ap_pred_refl,	       pred_refl   ).
gerund_frame_map(pred_refl,	       pred_refl   ).
gerund_frame_map(pred_np,	       pred_np     ).
gerund_frame_map(fixed(List,_),	       fixed(List) ) :-
    \+ non_adj_fixed_list(List).
gerund_frame_map(so_np,                so_np       ).
gerund_frame_map(aux_psp_hebben,       aux_psp_hebben).
gerund_frame_map(ld_pp,                ld_pp       ).
gerund_frame_map(np_ld_pp,             np_ld_pp       ).
gerund_frame_map(refl_ld_pp,           refl_ld_pp ).
gerund_frame_map(refl_pc_pp(Prep),     refl_pp(Prep) ).
gerund_frame_map(refl_sbar,            refl_sbar).
gerund_frame_map(er_pp_sbar(Prep),     er_pp_sbar(Prep)).
gerund_frame_map(er_pp_sbar_dip(Prep), er_pp_sbar_dip(Prep)).
gerund_frame_map(er_pp_vp(Prep),       er_pp_vp(Prep)).

gerund_frame_map(Frame0, part(Part,Frame)) :-
    Frame0 =.. [Functor0,Part|Args],
    atom_concat(part_,Functor,Functor0),
    Frame1 =.. [Functor|Args],
    gerund_frame_map(Frame1,Frame).

non_adj_fixed_list(List) :-
    lists:member(El,List),
    non_adj_fixed(El).

non_adj_fixed(El) :-
    impossible_nominalization_fixed_el(El).
non_adj_fixed(vp).
non_adj_fixed(extra_sbar(_)).
non_adj_fixed(extra_vp(_)).
non_adj_fixed(vc(_,_,_)).
non_adj_fixed(het_pobj1(vp)).
non_adj_fixed({L}) :-
    lists:member(El,L),
    non_adj_fixed(El).

iets_adjective(X,X,_).
iets_adjective(adjective(NoE0),post_adjective(NoE),Word) :-
    remove_s(Word),
    no_e(NoE0,NoE).
iets_adjective(adjective(NoE0,subject_sbar),
	       post_adjective(NoE,subject_sbar),Word) :-
    remove_s(Word),
    no_e(NoE0,NoE).
iets_adjective(adjective(NoE0,subject_vp),
	       post_adjective(NoE,subject_vp),Word) :-
    remove_s(Word),
    no_e(NoE0,NoE).

nominalization(V,V,_).
nominalization(verb(_,InfVal,Frame0), v_noun(Frame),Word) :-
    inf(InfVal),
    drop_inv(Frame0,Frame1),
    possible_nominalization(Frame1,Frame2),
    optionally_drop_np_complement(Frame2,Frame),
    \+ check_exception(Word,Frame),  
    check_already_het_noun(Frame,Word).

check_exception(maken,ap_copula). % subsumed by copula
check_exception(leren,transitive_ndev_ndev).  % subsumed

check_already_het_noun(Intransitive,Word) :-
    (   Intransitive == intransitive
    ->  check_already_het_noun(Word)
    ;   true
    ).

check_already_het_noun(Word) :-
    (   exception_het_noun(Word)
    ->  true
    ;   \+ (   xl(Word,TAG,_,[],[]),
	       sg_het_noun(TAG)
	   )
    ).

exception_het_noun(eten).
exception_het_noun(wezen).
exception_het_noun(optreden).
exception_het_noun(schrijven).
exception_het_noun(voorkomen).
%% allow genuine ambiguity
%% "een schrijven"
%% "zelf schrijven/optreden deed hij niet"
%% allow genuine ambiguity
%% het vaak voorkomen van ...
%% hij heeft een ruig voorkomen


sg(sg).
sg(both).
sg(meas).

sg_het_noun(noun(het,_,SG)) :-
    sg(SG).
sg_het_noun(tmp_noun(het,_,SG)) :-
    sg(SG).
sg_het_noun(meas_mod_noun(het,_,SG)) :-
    sg(SG).
sg_het_noun(mod_noun(het,_,SG)) :-
    sg(SG).

drop_inv(Frame0,Frame) :-
    functor(Frame0,F,A),
    (   F/A == ninv/2
    ->  arg(1,Frame0,Frame)
    ;   Frame0=Frame
    ).

%% "dat is de moeite van het bekijken niet waard"
%% in nominalization, object np can be left out. If we'd treat this
%% in syntax, then many spurious ambiguities. Therefore, we do it here
%% [.. wrong too, since we can't treat 'de moeite van het laten bekijken']
optionally_drop_np_complement(Sc,Sc).

optionally_drop_np_complement(transitive,intransitive).
optionally_drop_np_complement(transitive_ndev,intransitive).
optionally_drop_np_complement(transitive_ndev_ndev,intransitive).
optionally_drop_np_complement(np_np,transitive).
optionally_drop_np_complement(np_np,intransitive).
optionally_drop_np_complement(part_np_np(X),part_transitive(X)).
optionally_drop_np_complement(part_np_np(X),part_intransitive(X)).
optionally_drop_np_complement(het_subj,intransitive).
optionally_drop_np_complement(refl,intransitive).
optionally_drop_np_complement(ld_pp,intransitive).
optionally_drop_np_complement(np_ld_pp,ld_pp).
optionally_drop_np_complement(pc_pp(_),intransitive).
optionally_drop_np_complement(refl_ld_pp,intransitive).
optionally_drop_np_complement(refl_pc_pp,intransitive).
optionally_drop_np_complement(np_ld_dir,ld_dir).
optionally_drop_np_complement(ld_transitive,intransitive).
optionally_drop_np_complement(pred_np,copula).
optionally_drop_np_complement(nonp_pred_np,nonp_copula).
optionally_drop_np_complement(nonp_pred_np_ndev,nonp_copula).
optionally_drop_np_complement(ap_pred_np,ap_copula).
optionally_drop_np_complement(pp_pred_np,pp_copula).
optionally_drop_np_complement(part_transitive(X),part_intransitive(X)).
optionally_drop_np_complement(part_pred_np(X),part_copula(X)).
optionally_drop_np_complement(np_vp_obj,vp).
optionally_drop_np_complement(np_vp_subj,vp).
optionally_drop_np_complement(np_vp_obj1,vp).
optionally_drop_np_complement(fixed(List0,Passive), fixed(List,Passive)) :-
    lists:select(acc,List0,List).

possible_nominalization(het_subj,intransitive).
possible_nominalization(Fr0,Fr0) :-
    strip_part(Fr0,Fr1),
    \+ impossible_nominalization_frame(Fr1).

strip_part(Fr0,Fr) :-
    functor(Fr0,Fun,_),
    atom_concat(part_,Fun2,Fun),
    !,
    Fr0 =.. [Fun,_Part|Rest],
    Fr =.. [Fun2|Rest].
strip_part(F,F).

%% done: passive appears possible if the vc argument goes left
%% het gekust worden is fijn
%% het *worden gekust is fijn
%% *het te vinden zijn is fijn
%% *het zijn te vinden is fijn
%% *het geslapen hebben is fijn
%% *het hebben geslapen is fijn
%% *het gekomen zijn is fijn
%% *he zijn gekomen is fijn

impossible_nominalization_frame(het_subj).
impossible_nominalization_frame(so_nonp_copula).
impossible_nominalization_frame(so_nonp_copula_vp).
impossible_nominalization_frame(so_nonp_copula_sbar).
impossible_nominalization_frame(so_copula).
impossible_nominalization_frame(so_copula_vp).
impossible_nominalization_frame(so_copula_sbar).
impossible_nominalization_frame(so_copula_np).
impossible_nominalization_frame(te_passive).   
impossible_nominalization_frame(dat_te_passive).   
impossible_nominalization_frame(dat_passive).   
impossible_nominalization_frame(aux_psp_hebben).
impossible_nominalization_frame(aux_psp_zijn).
impossible_nominalization_frame(simple_cleft).
impossible_nominalization_frame(cleft).
impossible_nominalization_frame(cleft_np).
impossible_nominalization_frame(aan_het).
impossible_nominalization_frame(fixed_dep(_)).
impossible_nominalization_frame(np_aan_het).
impossible_nominalization_frame(no_subj).
impossible_nominalization_frame(alsof_sbar_subj).
impossible_nominalization_frame(alsof_sbar_subj_so_np).
impossible_nominalization_frame(sbar_subj_subj).
impossible_nominalization_frame(sbar_subj_het).
impossible_nominalization_frame(sbar_subj_opt_het).
impossible_nominalization_frame(sbar_subj_np).
impossible_nominalization_frame(sbar_subj_meas).
impossible_nominalization_frame(sbar_subj_dat_meas).
impossible_nominalization_frame(sbar_subj_so_np).
impossible_nominalization_frame(sbar_subj_np_np).
impossible_nominalization_frame(sbar_subj_so_np_opt_het).
impossible_nominalization_frame(sbar_subj_so_np_no_het).
impossible_nominalization_frame(sbar_subj_np_no_het).
impossible_nominalization_frame(sbar_subj_refl_no_het).
impossible_nominalization_frame(sbar_subj_refl_opt_het).
impossible_nominalization_frame(dip_sbar_subj_so_np).
impossible_nominalization_frame(dip_sbar_subj_so_np_no_het).
impossible_nominalization_frame(dip_sbar_subj_so_np_opt_het).
impossible_nominalization_frame(dip_sbar_subj).
impossible_nominalization_frame(dip_sbar_subj_no_het).
impossible_nominalization_frame(dip_sbar_subj_opt_het).
impossible_nominalization_frame(van_sbar_subj_no_het).
impossible_nominalization_frame(van_sbar_subj_so_np_no_het).
impossible_nominalization_frame(sbar_subj).
impossible_nominalization_frame(sbar_subj_no_het).
impossible_nominalization_frame(sbar_subj_no_het_tpart).
impossible_nominalization_frame(pp_sbar_subj).
impossible_nominalization_frame(pp_sbar_subj_opt_het).
impossible_nominalization_frame(pp_sbar_subj_no_het).
impossible_nominalization_frame(er_sbar_subj_no_het).
impossible_nominalization_frame(ld_pp_sbar_subj_no_het).
impossible_nominalization_frame(ld_adv_sbar_subj_no_het).
impossible_nominalization_frame(vp_subj).
impossible_nominalization_frame(vp_subj_no_het).
impossible_nominalization_frame(vp_subj_np).
impossible_nominalization_frame(vp_subj_so_np).
impossible_nominalization_frame(vp_subj_meas).
impossible_nominalization_frame(vp_subj_adv_meas).
impossible_nominalization_frame(vp_subj_dat_meas).
impossible_nominalization_frame(vp_subj_np_np).
impossible_nominalization_frame(vp_subj_np_no_het).
impossible_nominalization_frame(pp_vp_subj).
impossible_nominalization_frame(pp_vp_subj_no_het).
impossible_nominalization_frame(vp_subj_np).
impossible_nominalization_frame(vp_subj_so_np).
impossible_nominalization_frame(vp_subj_so_np_no_het).
impossible_nominalization_frame(vp_subj_np_np).
impossible_nominalization_frame(sbar_subj_te_passive).
impossible_nominalization_frame(inverted_aux(_)).
impossible_nominalization_frame(copula_sbar).
impossible_nominalization_frame(copula_vp). 
impossible_nominalization_frame(so_copula_sbar).
impossible_nominalization_frame(so_copula_vp). 
impossible_nominalization_frame(pred_np_sbar).
impossible_nominalization_frame(pred_np_vp).
impossible_nominalization_frame(van_sbar).
impossible_nominalization_frame(subj_control(pass_te)).
impossible_nominalization_frame(so_control(pass_te)).
impossible_nominalization_frame(fixed(List,_)) :-
    lists:member(El,List),
    impossible_nominalization_fixed_el(El).
impossible_nominalization_frame(refl_vp).
impossible_nominalization_frame(dip_sbar).
impossible_nominalization_frame(pp_dip_sbar).
impossible_nominalization_frame(acc_np_dip_sbar).
impossible_nominalization_frame(er_er).
impossible_nominalization_frame(obj_er_er).

impossible_nominalization_fixed_el({L}) :-
    lists:member(El,L),
    impossible_nominalization_fixed_el(El).

impossible_nominalization_fixed_el(het_subj).
impossible_nominalization_fixed_el(vp_subj).
impossible_nominalization_fixed_el(vp_subj_no_het).
impossible_nominalization_fixed_el(vp_subj_opt_het).
impossible_nominalization_fixed_el(sbar_subj).
impossible_nominalization_fixed_el(sbar_subj_no_het).
impossible_nominalization_fixed_el(sbar_subj_opt_het).
impossible_nominalization_fixed_el(short_sbar_subj).
impossible_nominalization_fixed_el(naar_sbar_subj).
impossible_nominalization_fixed_el(dip_sbar).
impossible_nominalization_fixed_el(no_subj).
impossible_nominalization_fixed_el(subj(_)).
impossible_nominalization_fixed_el(yt(_)).


melt_part_prep(verb(A,B,Sc0),verb(A,B,Sc)) :-
    melt_part_prep_sc(Sc0,Sc).
melt_part_prep(V,V).

melt_part_prep_pair(aan,tegen,tegenaan).
melt_part_prep_pair(in, tegen,tegenin).
melt_part_prep_pair(op, tegen,tegenop).
melt_part_prep_pair(toe,naar, naartoe).
melt_part_prep_pair(af, van,  vanaf).
melt_part_prep_pair(uit,van,  vanuit).

add_er(W,W).
add_er(W1,W) :-
    atom(W1),
    er(Er),
    atom_concat(Er,W1,W).

er(er).
er(daar).
er(hier).
er(waar).

%% as a consequence, 'tegenaan' is both
%  preposition(tegen,[],extracted_np)
%  preposition(tegenaan,[],extracted_np)
%
% this is not necc of course if the form is an ordinary preposition (vanaf, vanuit)
% because these are always potentially +extracted_np anyway


melt_part_prep_sc(part_pc_pp(In,Tegen),er_pc_pp(Tegenin)) :-
    melt_part_prep_pair(In,Tegen,Tegenin).
melt_part_prep_sc(part_np_pc_pp(In,Tegen),np_er_pc_pp(Tegenin)) :-
    melt_part_prep_pair(In,Tegen,Tegenin).
melt_part_prep_sc(part_er_pp_sbar(In,Tegen),er_pp_sbar(Tegenin)) :-
    melt_part_prep_pair(In,Tegen,Tegenin).
melt_part_prep_sc(part_np_er_pp_sbar(In,Tegen),np_er_pp_sbar(Tegenin)) :-
    melt_part_prep_pair(In,Tegen,Tegenin).
melt_part_prep_sc(part_er_pp_vp(In,Tegen),er_pp_vp(Tegenin)) :-
    melt_part_prep_pair(In,Tegen,Tegenin).
melt_part_prep_sc(part_np_er_pp_vp(In,Tegen),np_er_pp_vp(Tegenin)) :-
    melt_part_prep_pair(In,Tegen,Tegenin).

:- hdrug_util:initialize_flag(expand_subcat,on).

select_subcat_frame(verb(A,B,[ScH|ScT]),verb(A,B,Sc)) :-
    !,
    hdrug_util:hdrug_flag(parse_or_generate,PG),
    hdrug_util:hdrug_flag(expand_subcat,OnOff),
    lists:member(Sc0,[ScH|ScT]),
    add_new_subcat_frame(OnOff,Sc0,Sc,A),
    filter_subcat_frame(PG,Sc).
select_subcat_frame(V,V).

filter_subcat_frame(generate,_).
filter_subcat_frame(parse,Sc) :-
    functor(Sc,Fun,_),
    atom_concat(part_,Fun1,Fun),
    !,
    Sc =.. [Fun,Part|Args],
    word_form(Part),
    Sc1 =.. [Fun1|Args],
    filter_subcat(Sc1).
filter_subcat_frame(parse,Sc) :-
    filter_subcat(Sc).

filter_subcat(van_sbar) :-
    !,
    word_form(van).
filter_subcat(alsof_sbar_subj) :-
    !,
    word_form(alsof).
filter_subcat(alsof_sbar_subj_so_np) :-
    !,
    word_form(alsof).
filter_subcat(alsof_sbar) :-
    !,
    word_form(alsof).
filter_subcat(np_alsof_sbar) :-
    !,
    word_form(alsof).
filter_subcat(fixed(Fixed,_)) :-
    !,
    filter_fixed(Fixed).
filter_subcat(te_passive) :-
    !,
    word_form(te).
filter_subcat(subj_control(te)) :-
    !,
    word_form(te).
filter_subcat(aux(te)) :-
    !,
    word_form(te).
filter_subcat(so_aux(te)) :-
    !,
    word_form(te).
filter_subcat(copula_vp) :-
    !,
    word_form(te).
filter_subcat(so_copula_vp) :-
    !,
    word_form(te).
filter_subcat(er_er) :-
    !,
    word_form(er).
filter_subcat(aan_het) :-
    !,
    (   word_form(aan),
	word_form(het)
    ;   word_form('aan\'t')
    ;   word_form('aan`t')
    ;   word_form(aant)
    ).
filter_subcat(_).

filter_fixed([]).
filter_fixed([El|Els]) :-
    filter_fixed_el(El),
    filter_fixed(Els).

filter_fixed_el([H|T]) :-
    !,
    word_forms([H|T]).
filter_fixed_el({List}) :-
    !,
    filter_fixed(List).
filter_fixed_el(van_sbar) :-
    !,
    word_form(van).
filter_fixed_el(svp_er) :-
    !,
    word_form(er).
%filter_fixed_el(acc(Word)) :-
%    !,
%    word_form(Word).
%filter_fixed_el(svp_dat(Word)) :-
%    !,
%    word_form(Word).
%filter_fixed_el(svp_acc(Word)) :-
%    !,
%    word_form(Word).
%filter_fixed_el(np_pred(Word)) :-
%    !,
%    word_form(Word).
%filter_fixed_el(ap_pred(Word)) :-
%    !,
%    word_form(Word).
%filter_fixed_el(pp_pred(Word1,Word2)) :-
%    !,
%    word_form(Word2),
%    word_form(Word1).
%filter_fixed_el(subj(Word)) :-
%    !,
%    word_form(Word).
%filter_fixed_el(svp_pp(_,Word)) :-
%    !,
%    word_form(Word).
filter_fixed_el(_).

adapt_part_label(verb(_A,_B,Frame),Label0,Label) :-
    functor(Frame,Fun,_),
    atom_concat(part_,_,Fun),
    !,
    arg(1,Frame,Part),
    concat_part_to_root(Label0,Part,Label).
    % hdrug_util:concat_all([Label0,'_',Part],Label,'').
adapt_part_label(verb(_A,_B,ninv(_,Frame)),Label0,Label) :-
    functor(Frame,Fun,_), 
    atom_concat(part_,_,Fun),
    !,
    arg(1,Frame,Part),
    concat_part_to_root(Label0,Part,Label).
    % hdrug_util:concat_all([Label0,'_',Part],Label,'').
adapt_part_label(_V,L,L).

concat_part_to_root(v_root(Stam,Inf),Part0,Result) :-
    !,
    replace_space(Part0,Part),
    hdrug_util:concat_all([Stam,'_',Part],Stam2,''),
    hdrug_util:concat_all([Part,'_',Inf],Inf2,''),
    Result = v_root(Stam2,Inf2).
concat_part_to_root(Label0,Part0,Label) :-
    replace_space(Part0,Part),
    hdrug_util:hdrug_flag(root_of_verb_uses_inf,Flag),
    concat_part_to_root(Flag,Label0,Part,Label).

concat_part_to_root(On,Label0,Part,Label) :-
    (   On == on
    ->  hdrug_util:concat_all([Part,'_',Label0],Label,'')
    ;   hdrug_util:concat_all([Label0,'_',Part],Label,'')
    ).

part_sc_list([],_,[]).
part_sc_list([H0|T0],Part,List) :-
    part_sc(H0,Part,List,List0),
    part_sc_list(T0,Part,List0).

part_sc(PartSc,Part,ScL0,ScL) :-
    PartSc=..[Fun,Part|Args],
    atom_concat(part_,NewFun,Fun),
    !,
    Sc=..[NewFun|Args],
    ScL0=[ninv(Sc,PartSc)|ScL].
part_sc(_,_,L,L).

%%%%%%%%%%%%%%%%%%%% variants for parsing only %%%%%%%%%%%%%%%%%%%%%%%%

%% 3 --> 1 word
spelling_variant31(Soc,'-',Dem, SocDem) :-
    atom(Soc), \+ parse_number_simple(Soc),
    atom(Dem), \+ parse_number_simple(Dem),
    hdrug_util:concat_all([Soc,Dem],SocDem,'-').

spelling_variant31(Soc,'-',Dem, SocDem) :-
    atom(Soc), \+ parse_number_simple(Soc),
    atom(Dem), \+ parse_number_simple(Dem),
    hdrug_util:concat_all([Soc,Dem],SocDem,'').

spelling_variant31(van,te,voren,   vantevoren).
spelling_variant31(of,te,wel,      oftewel).
spelling_variant31(doe,het,zelf,   'doe-het-zelf').
spelling_variant31(door,de,weeks,  doordeweeks).
spelling_variant31(door,de,weekse, doordeweekse).
spelling_variant31(uit,dienst,name,uitdienstname).
spelling_variant31(in,dienst,name, indienstname).
spelling_variant31(so,wie,so,      sowieso).
spelling_variant31(tot,stand,koming,totstandkoming).
spelling_variant31('?','?',n,      één).  % part of SONAR
spelling_variant31(te,zelfder,tijd,tezelfdertijd).
spelling_variant31(ter,zelfder,tijd,tezelfdertijd).  % oud
spelling_variant31(wel,is,waar,weliswaar).
spelling_variant31(in,en,intriest,'in-en-in-triest').
spelling_variant31('in-',en,intriest,'in-en-in-triest').
spelling_variant31(als,je,blieft,alsjeblieft).

%% ad2003/ad2004
spelling_variant31('sji-','\'',ietisch,'sji\'itisch').
spelling_variant31('sji-','\'',ietische,'sji\'itische').
spelling_variant31('sji-','\'',itisch,'sji\'itisch').
spelling_variant31('sji-','\'',itische,'sji\'itische').
spelling_variant31('sji-','\'',iet,'sji\'iet').
spelling_variant31('sji-','\'',ieten,'sji\'ieten').

%% 2 --> 1 word
%% repair tokenization errors:
spelling_variant21('`',s,         '\'s').
spelling_variant21('`',ns,         '\'ns').
spelling_variant21('\'',s,        '\'s').
spelling_variant21('\'',ns,       '\'ns').

%% old tokenization...
spelling_variant21('da','\'s',      'da\'s').
spelling_variant21('Da','\'s',      'Da\'s').

%% repair author's errors:
spelling_variant21(a,propos,      apropos).
spelling_variant21(à,propos,      apropos).
spelling_variant21(achter,kant,   achterkant).
spelling_variant21(afkick,verschijnselen,afkickverschijnselen).
spelling_variant21(aller,eerste,  allereerste).
spelling_variant21(aller,laatste, allerlaatste).
spelling_variant21(alles,behalve, allesbehalve).
spelling_variant21(als,maar,      alsmaar).
spelling_variant21(als,mede,      alsmede).
spelling_variant21(als,of,        alsof).
spelling_variant21(ambulance,personeel,ambulancepersoneel).
spelling_variant21(beneden,verdieping,benedenverdieping).
spelling_variant21(binnen,kant,   binnenkant).
spelling_variant21(boven,kant,    bovenkant).
spelling_variant21(boven,verdieping,bovenverdieping).
spelling_variant21(bij,voorbeeld, bijvoorbeeld).
%spelling_variant21(buiten,spel,   buitenspel).
spelling_variant21(bus,station,   busstation).
spelling_variant21(daar,straks,   daarstraks).
spelling_variant21(dank,zij,      dankzij).
spelling_variant21(dat,gene,      datgene).
spelling_variant21(de,ze,         deze).
spelling_variant21(functie,eis,   functieeis).
spelling_variant21(functie,eisen, functieeisen).
spelling_variant21(half,stok,     halfstok).
spelling_variant21('\'t',geen,    hetgeen).
spelling_variant21(het,geen,      hetgeen).
spelling_variant21(het,welk,      hetwelk).
spelling_variant21('\'t',welk,    hetwelk).
spelling_variant21('`t',welk,     hetwelk).
spelling_variant21(t,zelfde,      hetzelfde).
spelling_variant21(heen,weg,      heenweg).
spelling_variant21(hoger,op,      hogerop).
spelling_variant21(hoogst,persoonlijk,hoogstpersoonlijk).
spelling_variant21(hoofd,rol,     hoofdrol).
spelling_variant21(in,zover,      inzover).
spelling_variant21(in,zake,       inzake).
spelling_variant21(kijk,plezier,  kijkplezier).
spelling_variant21(langs,daar,    daarlangs).  % VL
spelling_variant21(langzaam,aan,  langzaamaan).
spelling_variant21(lees,plezier,  leesplezier).
spelling_variant21(menig,een,     menigeen).
spelling_variant21(middag,uur,    middaguur).
spelling_variant21(na,dat,        nadat).
spelling_variant21(naar,mate,     naarmate).
spelling_variant21(ofte,wel,      oftewel).
spelling_variant21(om,heen,       omheen).
spelling_variant21(op,eens,       opeens).
spelling_variant21(non,actief,    'non-actief').
spelling_variant21(plus,minus,    plusminus).
spelling_variant21(score,verloop, scoreverloop).
spelling_variant21(sinds,dien,    sindsdien).
spelling_variant21(te,kort,       tekort).
spelling_variant21(te,weeg,       teweeg).
spelling_variant21(tegen,op,      tegenop).
spelling_variant21(tegen,over,    tegenover).
spelling_variant21(terug,reis,    terugreis).
spelling_variant21(terug,weg,     terugweg).
spelling_variant21(tot,dat,       totdat).
spelling_variant21(tot,standkoming,totstandkoming).
spelling_variant21(tussen,door,   tussendoor).
spelling_variant21(tussen,tijd,   tussentijd).
spelling_variant21(twee,derde,    tweederde).
spelling_variant21(verhaal,lijn,  verhaallijn).
spelling_variant21(verhaal,lijnen,verhaallijnen).
spelling_variant21(verzend,kosten,verzendkosten).
spelling_variant21(van,af,        vanaf).
spelling_variant21(van,daag,      vandaag).
spelling_variant21(van,daan,      vandaan).
spelling_variant21(van,tevoren,   vantevoren).
spelling_variant21(van,zelf,      vanzelf).
spelling_variant21(vol,strekt,    volstrekt).
spelling_variant21(voor,dat,      voordat).
spelling_variant21(vóór,dat,      voordat).
spelling_variant21(voor,zover,    voorzover).
spelling_variant21(vuurwerk,ramp, vuurwerkramp).
spelling_variant21(waar,toe,      waartoe).
spelling_variant21(tegen,partij,  tegenpartij).
spelling_variant21(te,gelijkertijd,     tegelijkertijd).
spelling_variant21(te,gemoed,     tegemoet).
spelling_variant21(te,gemoet,     tegemoet).
spelling_variant21(te,zamen,      samen).
spelling_variant21(te,samen,      samen).
spelling_variant21(tezelfder,tijd,tezelfdertijd).
spelling_variant21(terzelfder,tijd,tezelfdertijd).  % oud
spelling_variant21(te,zijnertijd, tezijnertijd).
spelling_variant21(ten,gevolge,   tengevolge).
spelling_variant21(ter,sluiks,    tersluiks).
spelling_variant21(ter,zake,      terzake).
spelling_variant21(tussen,uur,    tussenuur).
spelling_variant21(voor,avond,    vooravond).
spelling_variant21(waar,toe,      waartoe).
spelling_variant21(wereld,wijd,   wereldwijd).
spelling_variant21(wereld,wijde,  wereldwijde).
spelling_variant21(zich,zelf,     zichzelf).
%% spelling_variant21(zo,iets,       zoiets).  is in with_dt

%% frequent spelling mistake
spelling_variant21(boven,dien,    bovendien).
spelling_variant21(de,degenen,    degenen).
spelling_variant21(de,dezelfde,   dezelfde).
spelling_variant21(de,het,        het).
spelling_variant21(gis,teren,     gisteren).
spelling_variant21(goeden,avond,  goedenavond).
spelling_variant21(hebben,hebben, hebben).
spelling_variant21(hoofd,rol,     hoofdrol).
spelling_variant21(van,ouds,      vanouds).
spelling_variant21(zich,zich,     zich).

spelling_variant21('o.','a.',     'o.a.').

spelling_variant21('?',cht,       echt).
spelling_variant21('?',berhaupt,  überhaupt).

spelling_variant21(dmv,'.',       dmv).

spelling_variant21('in-en-in',triest,'in-en-in-triest').

spelling_variant21(o,zoveel,      zoveel).
spelling_variant21(oh,zoveel,     zoveel).

/* too many spur amb, since in most contexts, the two words
are analysed correctly if written separately
spelling_variant21(Daar,Achter,   Daarachter) :-
    lists:member(Daar,[daar,er,hier]),
    atom(Achter),
    atom_concat(Daar,Achter,Daarachter),
    xl(Achter,preposition(Prep,_),_,[],[]),
    xl(Daarachter,er_adverb(Prep),_,[],[]).

spelling_variant21(waar,Achter,   Waarachter) :-
    atom(Achter),
    atom_concat(waar,Achter,Waarachter),
    xl(Achter,preposition(Prep,_),_,[],[]),
    xl(Waarachter,waar_adverb(Prep),_,[],[]).
*/

%%% this is about the word directly *after* the verb 
r_requires3(zich).
r_requires3(men).
r_requires3(hij).
r_requires3(ie).
r_requires3('-ie').
r_requires3('\'ie').
r_requires3(u) :-
    \+ is_word_form(ik).
r_requires3(het) :-
    \+ is_word_form(ik).
r_requires3(dat) :-
    \+ is_word_form(ik).

r_requires1(ik).
r_requires1(je) :-
    \+ is_word_form(ik).
r_requires1(jij) :-
    \+ is_word_form(ik).

r_requires_pl(we).
r_requires_pl(wij).
r_requires_pl(jullie).

r_requires_psp(wordt).
r_requires_psp(werd).
r_requires_psp(werden).
r_requires_psp(worden).

r_requires_psp(is).
r_requires_psp(was).
r_requires_psp(waren).
r_requires_psp(zijn).

r_requires_psp(hebt).
r_requires_psp(heeft).
r_requires_psp(hebben).

r_requires_ge_adj(raakt).
r_requires_ge_adj(raakte).
r_requires_ge_adj(raak).
r_requires_ge_adj(raakten).
r_requires_ge_adj(geraakt).

r_requires_ge_adj(is).
r_requires_ge_adj(was).
r_requires_ge_adj(waren).
r_requires_ge_adj(zijn).
r_requires_ge_adj(geweest).


%%% this is about the word directly *before* the verb 
l_requires1(ik).
l_requires1('\'k').
l_requires1('Ik').

l_requires3(hij).
l_requires3(men).
l_requires3(jij).
l_requires3(je).
l_requires3(ze).
l_requires3(er).
l_requires3(het).
l_requires3(dat).
l_requires3(dit).
l_requires3(die).
l_requires3(u).
l_requires3(iedereen).
l_requires3('Hij').
l_requires3('Men').
l_requires3('Jij').
l_requires3('Je').
l_requires3('Ze').
l_requires3('Er').
l_requires3('Het').
l_requires3('Dat').
l_requires3('Dit').
l_requires3('U').
l_requires3('Wat').
l_requires3(zich).
l_requires3('Iedereen').

%% left neighbour
l_requires_psp(Word,_) :-
    l_requires_psp(Word).
%% or one word further to the left
l_requires_psp(_,[Word|_]) :-
    l_requires_psp(Word).

l_requires_psp(anders).
l_requires_psp('Wordt').
l_requires_psp(wordt).
l_requires_psp(worden).
l_requires_psp(werden).
l_requires_psp(werd).
l_requires_psp(is).
l_requires_psp(was).
l_requires_psp(waren).
l_requires_psp(zijn).
l_requires_psp(ben).
l_requires_psp(heeft).
l_requires_psp(hebben).
l_requires_psp(had).
l_requires_psp(hadden).
l_requires_psp(raakt).
l_requires_psp(raakte).
l_requires_psp(raakten).
l_requires_psp(raak).
l_requires_psp(raken).

l_requires_pl(we).
l_requires_pl(wij).
l_requires_pl(jullie).
l_requires_pl('We').
l_requires_pl('Wij').
l_requires_pl('Jullie').

spelling_variant_context(as,ge,_,als).
spelling_variant_context(zijde,gij,_,bent).
spelling_variant_context(bende,gij,_,bent).
spelling_variant_context(kunde,gij,_,kunt).
spelling_variant_context(hebde,gij,_,hebt).
spelling_variant_context(krijg,hij,_,krijgt).

spelling_variant_context(wilt,hij,_,wil).

%% verrichte hij --> verrichtte hij
spelling_variant_context(StemTe,Requires3,_,StemTTe) :-
    atom(StemTe),
    (   r_requires3(Requires3)
    ;   r_requires1(Requires3)
    ),
    atom_concat(Stem,te,StemTe),
    atom_concat(Stem,tte,StemTTe),
    lexicon___(StemTTe,verb(_,past(sg),_),_,[],[],_).

spelling_variant_context(StemD,Requires3,_,StemDt) :-
    atom(StemD),
    r_requires3(Requires3),
    atom_concat(Stem,d,StemD),
    (   atom_concat(StemD,t,StemDt),
        lexicon___(StemDt,verb(_,sg3,_),_,[],[],_)
    ;   atom_concat(Stem,t,StemDt),
	lexicon___(StemDt,verb(_,sg3,_),_,[],[],_)
    ).

spelling_variant_context(StemDe,Requires3,_,StemDde) :-
    atom(StemDe),
    r_requires3(Requires3),
    atom_concat(Stem,e,StemDe),
    (   atom_concat(Stem,de,StemDde),
	lexicon___(StemDde,verb(_,past(sg),_),_,[],[],_)
    ;   atom_concat(Stem,te,StemDde),
	lexicon___(StemDde,verb(_,past(sg),_),_,[],[],_)
    ).

spelling_variant_context(StemDt,Requires1,_,StemD) :-
    atom(StemDt),
    r_requires1(Requires1),
    atom_concat(StemD,t,StemDt),
    atom_concat(_,d,StemD),
    lexicon___(StemDt,verb(_,sg3,_),_,[],[],_).

spelling_variant_context(StemT,RequiresPsp,_,StemD) :-
    atom(StemT),
    r_requires_psp(RequiresPsp),
    atom_concat(Stem,t,StemT),
    (   atom_concat(Stem,d,StemD)
    ;   Stem=StemD, atom_concat(_,d,Stem)
    ),
    lexicon___(StemD,verb(_,psp,_),_,[],[],_).

spelling_variant_context(bereidt,bent,_,bereid).
spelling_variant_context(bereidt,ben,_,bereid).

spelling_variant_context(StemDDen,RequiresPsp,_,StemDen) :-
    atom(StemDDen),
    r_requires_psp(RequiresPsp),
    (   atom_concat(Stem,dden,StemDDen),
	atom_concat(Stem,den,StemDen)
    ;   atom_concat(Stem,tten,StemDDen),
	atom_concat(Stem,ten,StemDen)
    ),
    lexicon___(StemDen,verb(_,psp,_),_,[],[],_).

spelling_variant_context(StemT,RequiresPsp,_,StemD) :-
    atom(StemT),
    r_requires_ge_adj(RequiresPsp),
    atom_concat(Stem,t,StemT),
    (   atom_concat(Stem,d,StemD)
    ;   Stem=StemD, atom_concat(_,d,Stem)
    ),
    lexicon___(StemD,adjective(ge_no_e(_)),_,[],[],_).

spelling_variant_context(StemDe,RequiresPl,_,StemDen) :-
    atom(StemDe),
    r_requires_pl(RequiresPl),
    atom_concat(Stem,de,StemDe),
    lexicon___(StemDe,verb(_,past(sg),_),Root,[],[],_),
    atom_concat(Stem,den,StemDen),
    lexicon___(StemDen,verb(_,past(pl),_),Root,[],[],_).
spelling_variant_context(StemDe,RequiresPl,_,StemDen) :-
    atom(StemDe),
    r_requires_pl(RequiresPl),
    atom_concat(Stem,te,StemDe),
    lexicon___(StemDe,verb(_,past(sg),_),Root,[],[],_),
    atom_concat(Stem,ten,StemDen),
    lexicon___(StemDen,verb(_,past(pl),_),Root,[],[],_).   

spelling_variant_context(deugd,niet,_,deugt).
spelling_variant_context(vin,ik,_,vind).
spelling_variant_context(heef,ik,_,heb).  % childes
spelling_variant_context(sommigen,mensen,_,sommige).
spelling_variant_context(de,brengen,_,te).
spelling_variant_context(de,gaan,_,te).
spelling_variant_context(de,kunnen,_,te).
spelling_variant_context(de,laten,_,te).
spelling_variant_context(de,maken,_,te).
spelling_variant_context(de,staan,_,te).
spelling_variant_context('Hou',oud,_,hoe).
spelling_variant_context(te,hoogte,_,de).
spelling_variant_context(te,kop,_,de).
spelling_variant_context(te,tafel,_,de).
spelling_variant_context(te,tijd,_,de).
spelling_variant_context(bied,plaats,_,biedt).
spelling_variant_context(vind,plaats,_,vindt).
spelling_variant_context('na-',dan,[voordelen|_],nadelen).
spelling_variant_context('voor-',dan,[nadelen|_],voordelen).
spelling_variant_context(een,down,_,één).
spelling_variant_context(een,en,[twee|_],één).
spelling_variant_context(een,of,[twee|_],één).
spelling_variant_context(een,à,[twee|_],één).
spelling_variant_context(een,tot,[twee|_],één).
spelling_variant_context(ieder,persoon,_,iedere).

spelling_variant_context(bekent,gemaakt,_,bekend).
spelling_variant_context(elke,jaar,_,elk).
spelling_variant_context(onderscheidt,gemaakt,_,onderscheid).
spelling_variant_context(puur,sang,_,pur).
spelling_variant_context(went,of,[keert|_],wendt).
spelling_variant_context(wend,of,[keert|_],wendt).
spelling_variant_context(lukken,het,_,lukt).   % sommige mensen lukken het niet -> lukt
spelling_variant_context(lukten,het,_,lukte).

spelling_variant_context('Das',een,_,'da\'s').
spelling_variant_context('tis',een,_,'da\'s').
spelling_variant_context('Tis',een,_,'da\'s').

spelling_variant_context(Voor,En,[Tegen|_],'voor-') :-
    (  Voor == voor
    ;  Voor == 'Voor'
    ),
    (  En == en
    ;  En == of
    ;  En == dan
    ),
    atom(Tegen),
    (  atom_concat(tegen,_,Tegen)
    ;  atom_concat(na,_,Tegen)
    ;  atom_concat(achter,_,Tegen)
    ).

spelling_variant_context(Heen,En,[Terug|_],'heen-') :-
    (  Heen == heen
    ;  Heen == 'Heen'
    ),
    (  En == en
    ;  En == of
    ;  En == dan
    ),
    atom(Terug),
    (  atom_concat(terug,_,Terug)
    ).

%% zuidelijk "de" pronominal
context_spelling_variant_context(de,_,_,al,_,je_POSTV).
context_spelling_variant_context(de,_,_,daar,_,je_POSTV).
context_spelling_variant_context(de,_,_,dan,_,je_POSTV).
context_spelling_variant_context(de,_,_,dat,_,je_POSTV).
context_spelling_variant_context(de,_,_,die,_,je_POSTV).
context_spelling_variant_context(de,_,_,geen,_,je_POSTV).
context_spelling_variant_context(de,_,_,doen,_,je_POSTV).
context_spelling_variant_context(de,_,_,'d\'r',_,je_POSTV).
context_spelling_variant_context(de,_,_,er,_,je_POSTV).
context_spelling_variant_context(de,_,_,hier,_,je_POSTV).
context_spelling_variant_context(de,_,_,nog,_,je_POSTV).
context_spelling_variant_context(de,_,_,niet,_,je_POSTV).
context_spelling_variant_context(de,_,_,ook,_,je_POSTV).
context_spelling_variant_context(de,_,_,toch,_,je_POSTV).
context_spelling_variant_context(de,_,_,wel,_,je_POSTV).
context_spelling_variant_context(de,_,_,wat,_,je_POSTV).
context_spelling_variant_context(de,_,_,wie,_,je_POSTV).


context_spelling_variant_context('Den','(',_,')',_,'Denemarken').   % Den is a location only if within ()

context_spelling_variant_context(behoeven,ten,_,van,_,behoeve).
context_spelling_variant_context(plaatst,in,_,van,_,plaats).
context_spelling_variant_context(nog,kant,_,wal,_,noch).
context_spelling_variant_context(raden,te,_,gaan,_,rade).
context_spelling_variant_context(te,uit,_,doeken,_,de).

context_spelling_variant_context(zij,hij,_,zij,_,zei).

context_spelling_variant(de,zijt,_,je_POSTV).

%% hij verrichte hij --> hij verrichtte
context_spelling_variant(StemTe,Requires3,_,StemTTe) :-
    atom(StemTe),
    (  l_requires3(Requires3)
    ;  l_requires1(Requires3)
    ),
    atom_concat(Stem,te,StemTe),
    atom_concat(Stem,tte,StemTTe),
    lexicon___(StemTTe,verb(_,past(sg),_),_,[],[],_).

%% hij vermoorde --> hij vermoordde
context_spelling_variant(StemTe,Requires3,_,StemTTe) :-
    atom(StemTe),
    (  l_requires3(Requires3)
    ;  l_requires1(Requires3)
    ),
    atom_concat(Stem,de,StemTe),
    atom_concat(Stem,dde,StemTTe),
    lexicon___(StemTTe,verb(_,past(sg),_),_,[],[],_).

context_spelling_variant(Wordt,Ik,_,Word) :-
    atom(Wordt),
    l_requires1(Ik),
    atom_concat(Word,t,Wordt),
    atom_concat(_,d,Word),
    lexicon___(Wordt,verb(_,sg3,_),_,[],[],_).

context_spelling_variant(Word,Hij,_,Wordt) :-
    atom(Word),
    l_requires3(Hij),
    atom_concat(Word0,d,Word),
    (   atom_concat(Word,t,Wordt),
	lexicon___(Wordt,verb(_,sg3,_),_,[],[],_)
    ;   atom_concat(Word0,t,Wordt),
	lexicon___(Wordt, verb(_,sg3,_),_,[],[],_)
    ).

%% Je denk toch niet ..
context_spelling_variant(denk,Ik,_,denkt) :-
    l_requires3(Ik).
context_spelling_variant(plaatst,de,_,plaats).

context_spelling_variant(HerinnerDDen,Werd,More,HerinnerDen) :-
    atom(HerinnerDDen),
    l_requires_psp(Werd,More),
    (   atom_concat(Herinner,dden,HerinnerDDen),
        atom_concat(Herinner,den,HerinnerDen)
    ;   atom_concat(Herinner,tten,HerinnerDDen),
	atom_concat(Herinner,ten,HerinnerDen)
    ),
    lexicon___(HerinnerDen,verb(_,psp,_),_,[],[],_).

context_spelling_variant(Herinnert,Werd,More,Herinnerd) :-
    atom(Herinnert),
    l_requires_psp(Werd,More),
    atom_concat(Herinner,t,Herinnert),
    (   atom_concat(Herinner,d,Herinnerd)
    ;   Herinner = Herinnerd
    ),
    lexicon___(Herinnerd,verb(_,psp,_),_,[],[],_).

context_spelling_variant(Wilde,We,_,Wilden):-
    atom(Wilde),
    lexicon___(Wilde,verb(_,past(sg),_),Root,[],[],_),
    l_requires_pl(We),
    atom_concat(Wilde,n,Wilden),
    lexicon___(Wilden,verb(_,past(pl),_),Root,[],[],_).

%% te plaatsten --> te plaatsen
context_spelling_variant(aankondigde,de,_,aangekondigde).
context_spelling_variant(anderen,een,_,andere).
context_spelling_variant(bevind,zich,_,bevindt).
context_spelling_variant(bijeenkomt,de,_,bijeenkomst).
context_spelling_variant(vin,Ik,_,vind) :-
    l_requires1(Ik).
context_spelling_variant(ben,jij,_,bent).
context_spelling_variant(ben,'Jij',_,bent).
context_spelling_variant(ben,jij,_,bent).
context_spelling_variant(ben,'Je',_,bent).
context_spelling_variant(heb,je,_,hebt).
context_spelling_variant(heb,'Jij',_,hebt).
context_spelling_variant(heb,je,_,hebt).
context_spelling_variant(heb,'Je',_,hebt).
context_spelling_variant(heb,jij,_,hebt).
context_spelling_variant(hebt,'Ik',_,heb). % opensubtitles
context_spelling_variant(hebt,ik,_,heb).  % opensubtitles
context_spelling_variant(bent,'Ik',_,ben). % opensubtitles
context_spelling_variant(bent,ik,_,ben).  % opensubtitles
context_spelling_variant(heef,ik,_,heb). % childes
context_spelling_variant(heb,'Jij',_,hebt).
context_spelling_variant(wilt,hij,_,wil).
context_spelling_variant(wilt,'Hij',_,wil).
context_spelling_variant(wilt,ze,_,wil).
context_spelling_variant(wilt,'Ze',_,wil).
context_spelling_variant(wilt,wie,_,wil).
context_spelling_variant(wilt,'Wie',_,wil).
context_spelling_variant(gaat,Ik,_,ga) :-
    l_requires1(Ik).
context_spelling_variant(denkt,Ik,_,denk) :-
    l_requires1(Ik).
context_spelling_variant(lus,Ik,_,lust) :-
    l_requires1(Ik).
context_spelling_variant(kun,je,_,kunt).
context_spelling_variant(kostte,ten,_,koste).
context_spelling_variant(l,'PAGINA',_,'1').  % de letter l ipv cijfer 1 
context_spelling_variant(opbrengt,de,_,opbrengst).
context_spelling_variant(opbrengt,'De',_,opbrengst).
context_spelling_variant(plaatsten,te,_,plaatsen).
context_spelling_variant(richtten,te,_,richten).
context_spelling_variant(steven,te,_,stevenen).
context_spelling_variant(stichtten,te,_,stichten).
context_spelling_variant(strand,schip,_,strandt).
context_spelling_variant(spraken,ter,_,sprake).
context_spelling_variant(overtuigd,van,_,overtuigd).
context_spelling_variant(verplaatsten,te,_,verplaatsen).
context_spelling_variant(vluchtten,te,_,vluchten).
context_spelling_variant(weining,te,_,weinig).
context_spelling_variant(bezuinigingen,te,_,bezuinigen).
context_spelling_variant(teruggeven,worden,_,teruggegeven).
context_spelling_variant(bezuinigen,de,_,bezuinigingen).
context_spelling_variant(plaatst,vond,_,plaats).
context_spelling_variant(uitkomt,de,_,uitkomst).
context_spelling_variant(uitsprak,de,_,uitspraak).
context_spelling_variant(verwachtte,de,_,verwachte).
context_spelling_variant(plaatsten,sommige,[op|_],plaatsen).
context_spelling_variant(plaatsten,het,_,plaatsen).
context_spelling_variant(waarschijnlijk,alle,[naar|_],waarschijnlijkheid).
context_spelling_variant(waarschijnlijk,alle,['Naar'|_],waarschijnlijkheid).
context_spelling_variant(zijde,beide,_,zijden).
context_spelling_variant(een,lijst,_,één).
context_spelling_variant(een,'Lijst',_,één).
context_spelling_variant(een,dan,[meer|_],één).
context_spelling_variant(een,dan,['Meer'|_],één).
context_spelling_variant(een,nummer,_,één).
context_spelling_variant(een,'Nummer',_,één).
context_spelling_variant(een,nummers,_,één).
context_spelling_variant(een,'Nummers',_,één).
context_spelling_variant(een,welgeteld,_,één).
context_spelling_variant(een,'Welgeteld',_,één).
context_spelling_variant(bied,plaats,_,biedt).
context_spelling_variant(gebied,eerlijkheid,_,gebiedt).
context_spelling_variant(leidden,zou,_,leiden).
context_spelling_variant(moet,de,_,moed).
context_spelling_variant(dok,de,_,dokter).
context_spelling_variant(vind,plaats,_,vindt).
context_spelling_variant(vraagt,de,_,vraag).
context_spelling_variant(vraagt,'De',_,vraag).

context_spelling_variant_context(eerst,de,_,keer,_,eerste).
context_spelling_variant_context(eerst,de,_,plaats,_,eerste).
context_spelling_variant_context(een,dan,_,op,_,één).
context_spelling_variant_context(gebeurd,er,_,'.',_,gebeurt).
context_spelling_variant_context(gebeurd,iets,[er|_],'.',_,gebeurt).
context_spelling_variant_context(geven,een,_,moment,_,gegeven).
context_spelling_variant_context(nog,kant,_,wal,_,noch).
context_spelling_variant_context(ons,naar,_,zin,_,onze).  % we hebben het naar ons zin
context_spelling_variant_context(te,uit,_,weg,_,de). % uit te weg ruimen
context_spelling_variant_context(te,rond,_,tafel,_,de). % rond te tafel
context_spelling_variant_context(te,'Rond',_,tafel,_,de). % rond te tafel
context_spelling_variant_context(toekomstig,het,_,'Europa',_,toekomstige).
context_spelling_variant_context(uitgebreid,het,_,'Europa',_,uitgebreide).

%% allow ` as variant of ' 
spelling_variant(Word1,    Word) :-
    quote_variant(Variant,VarVar),
    atom(Word1),
    Word1 \= Variant,
    Word1 \= VarVar,
    sub_atom(Word1,Before1,1,After,Variant),
    !,
    sub_atom(Word1,0,Before1,_,Prefix),
    Before2 is Before1+1,
    sub_atom(Word1,Before2,After,_,Suffix),
    hdrug_util:concat_all([Prefix,'\'',Suffix],Word).

spelling_variant(Word1, Word) :-
    atom(Word1),
    atom_concat('ge-e',Rest,Word1),
    atom_concat('geë',Rest,Word).

spelling_variant('.?',     '.').
spelling_variant('Eén',    één).
spelling_variant('eén',    één).
spelling_variant('éen',    één).
spelling_variant('eén',    één).

spelling_variant('vóór',  voor). % because vóór is also a PP, n

spelling_variant('`a',       à).

spelling_variant('\'t',    het).
spelling_variant('`t',     het).
spelling_variant(t,        het).


spelling_variant('TV',     tv).

spelling_variant(jouw,    jou).

spelling_variant(ais,     als).

spelling_variant(peleton, peloton).
spelling_variant(peletons,pelotons).

%% ck
spelling_variant(akademie,academie).
spelling_variant(akademiën,academiën).
spelling_variant(akademies,academies).
spelling_variant(akademisch,academisch).
spelling_variant(akademische,academische).
spelling_variant(akademischer,academischer).
spelling_variant(akademischere,academischere).
spelling_variant(akademischst,academischst).
spelling_variant(akademischste,academischste).
spelling_variant(abstrakt,abstract).
spelling_variant(abstrakte,abstracte).
spelling_variant(abstrakter,abstracter).
spelling_variant(abstraktere,abstractere).
spelling_variant(abstrakts,abstracts).
spelling_variant(abstraktst,abstractst).
spelling_variant(abstraktste,abstractste).
spelling_variant(accoord,akkoord).
spelling_variant(accoorden,akkoorden).
spelling_variant(kabaret,cabaret).
spelling_variant(kabarets,cabarets).
spelling_variant(kabine,cabine).
spelling_variant(kabines,cabines).

spelling_variant(aanbeeld, aambeeld).
spelling_variant(aanbeelden, aambeelden).
spelling_variant(beidde,beide).
spelling_variant(burgelijk, burgerlijk).
spelling_variant(burgelijke,burgerlijke).
spelling_variant(honderste,honderdste).
spelling_variant(hondertal,honderdtal).
spelling_variant(laaste,   laatste).
spelling_variant(meerde,   meerdere).
spelling_variant(moelijk,  moeilijk).
spelling_variant(moelijke, moeilijke).
spelling_variant(moskeën,  moskeeën).
spelling_variant(opeenvolgde,opeenvolgende).
spelling_variant(ouwe,     oude).
spelling_variant(ouwer,    ouder).
spelling_variant(ouwere,   oudere).
spelling_variant(stief,    stierf).
spelling_variant(stiekum,  stiekem).
spelling_variant(geraced,  geracet).
spelling_variant('ge-uit', geuit).
spelling_variant(goeie,    goede).
spelling_variant(goeien,   goede).
spelling_variant(groffe,   grove).
spelling_variant(hektare,  hectare).
spelling_variant(hektaren, hectaren).
spelling_variant(hektares, hectares).
spelling_variant(kwaaie,   kwade).

spelling_variant(gristen,  christen).  

spelling_variant(nonactief, 'non-actief').
spelling_variant('zuid-oosten',zuidoosten).
spelling_variant('zuid-westen',zuidwesten).
spelling_variant('noord-oosten',noordoosten).
spelling_variant('noord-westen',noordwesten).

spelling_variant('Celcius','Celsius').

%% eerder in misc.pl
spelling_variant(dees, deze).
spelling_variant(eener,ener).

spelling_variant(wa,wat).

spelling_variant(zijt,ben).
spelling_variant(zijt,bent).

spelling_variant(kinders,kinderen).

spelling_variant(zegde,zei).
spelling_variant(zeide,zei).
spelling_variant(zegden,zeiden).

spelling_variant(aanzegde,aanzei).
spelling_variant(aanzeide,aanzei).
spelling_variant(aanzegden,aanzeiden).

spelling_variant(afzegde,afzei).
spelling_variant(afzeide,afzei).
spelling_variant(afzegden,afzeiden).

spelling_variant(opzegde,opzei).
spelling_variant(opzeide,opzei).
spelling_variant(opzegden,opzeiden).

spelling_variant(toezegde,toezei).
spelling_variant(toezeide,toezei).
spelling_variant(toezegden,toezeiden).

spelling_variant(hadt,had).

spelling_variant(vondt,vond).

spelling_variant(zoudt,zou).
spelling_variant(zoude,zou).
spelling_variant(zouen,zouden).

spelling_variant(dorst,durfde).
spelling_variant(dorsten,durfden).
spelling_variant(vollop,volop).
spelling_variant(vijwel,vrijwel).
spelling_variant(vrijwiliger,vrijwilliger).

%% (gesproken) Vlaamse (?) variant
spelling_variant(beetjen,beetje).
spelling_variant(boekske,boekje).
spelling_variant(bomma,oma).
spelling_variant(buitenhuis,buitenshuis).
spelling_variant(briefke,briefje).
spelling_variant(da,dat).
spelling_variant(dewelke,welke).
spelling_variant(dezen,deze).
spelling_variant(dingske,dingetje).
spelling_variant(dingskes,dingetjes).
spelling_variant(eike,eitje).
spelling_variant(eikes,eitjes).
spelling_variant(elken,elke).
spelling_variant(iet,iets).
spelling_variant(gemakske,gemak).
spelling_variant(gère,graag).
spelling_variant(goei,goed).
spelling_variant(goei,goede).
spelling_variant(keinder,kinderen).
spelling_variant(machien,machine).
spelling_variant(maman,mama).
spelling_variant(oe,uw).
spelling_variant(oe,hoe).
spelling_variant(oew,uw).
spelling_variant(stukske,stukje).
spelling_variant(stukskes,stukjes).
spelling_variant(tornooi,toernooi).
spelling_variant(uni,universiteit).
spelling_variant(unief,universiteit).
spelling_variant(uurke,uurtje).
spelling_variant(vakske,vakje).
spelling_variant('z\'ne',zijn).
spelling_variant(zoiet,zoiets).
spelling_variant(zoietske,zoiets).

%% gesproken taal
spelling_variant(an,aan).
spelling_variant(astublieft,alstublieft).
spelling_variant(bakkie,bakje).
spelling_variant(bieb,bibliotheek).
spelling_variant(blijve,blijven).
spelling_variant(daaro,daar).
spelling_variant(datte,dat).
spelling_variant(denke,denken).
spelling_variant(eergister,eergisteren).
spelling_variant(fietsie,fietsje).
spelling_variant(gister,gisteren).
spelling_variant('goede-avond',goedenavond).
spelling_variant(goeiemiddag,goedemiddag).
spelling_variant(goeienavond,goedenavond).
spelling_variant(goeiedag,goedendag).
spelling_variant(goeiendag,goedendag).
spelling_variant(goeiemorgen,goedemorgen).
spelling_variant(hiero,hier).
spelling_variant(ikke,ik).
spelling_variant(klere,kleren).
spelling_variant(mammie,mama).
spelling_variant(ouwerwets,ouderwets).
spelling_variant(ouwerwetse,ouderwetse).
spelling_variant(pappie,papa).
spelling_variant(peerd,paard). % van Ome Loeks
spelling_variant(peerden,paarden). % van Ome Loeks
spelling_variant(stukkie,stukje).
spelling_variant(tuurlijk, natuurlijk).
spelling_variant('\'tzelfde',hetzelfde).
spelling_variant('\'tgeen',hetgeen).
spelling_variant('`tzelfde',hetzelfde).
spelling_variant('`tgeen',hetgeen).
spelling_variant(veels,veel).
spelling_variant(verders,verder).
spelling_varaint(verlieze,verliezen).
spelling_variant(wadde,wat).
spelling_variant(watte,wat).
spelling_variant(wete,weten).
spelling_variatn(worde,worden).
spelling_variant(zekers,zeker).

%% OpenSubTitles
spelling_variant('AIs',als).
spelling_variant('ÁIs',als).

%% CHILDES
spelling_variant('d\'erin', daarin).
spelling_variant(autos,     'auto\'s').
spelling_variant(eve,       even).
spelling_variant(hardstikke,hartstikke).
spelling_variant(negentientachtig, '1980').
spelling_variant(negentieneenentachtig, '1981').
spelling_variant(mamaas,    'mama\'s').
spelling_variant(mamas,     'mama\'s').
spelling_variant(papagaai,  papegaai).
spelling_variant(papas,     'papa\'s').
spelling_variant(portemonnaie,portemonnee).
spelling_variant(pijama,    pyjama).
spelling_variant(prullebak, prullenbak).
spelling_variant(prullemand, prullenmand).
spelling_variant(weecee,    wc).
spelling_variant(weecees,   'wc\'s').

%% very frequent spelling mistakes
spelling_variant(aanwijzigingen, aanwijzingen).
spelling_variant(accuut,      acuut).
spelling_variant(accute,      acute).
spelling_variant(acher,       achter).
spelling_variant(afgeproken,  afgesproken).
spelling_variant(aleen,       alleen).
spelling_variant(anderszijds, anderzijds).
spelling_variant(barste,      barstte).
spelling_variant(barsten,     barstten).
spelling_variant(basseer,     baseer).
spelling_variant(basseert,    baseert).
spelling_variant(basseren,    baseren).
spelling_variant(gebasseerd,  gebaseerd).
spelling_variant(basseerde,   baseerde).
spelling_variant(basseerden,  baseerden).
spelling_variant(belangrijkse,belangrijkste).
spelling_variant(benieuw,     benieuwd).
spelling_variant(betaamd,     betaamt).
spelling_variant(beteken,     betekenen).
spelling_variant(bezighoud,   bezighoudt).
spelling_variant(biezonder,   bijzonder).
spelling_variant(biezondere,   bijzondere).
spelling_variant(biezonderder,   bijzonderder).
spelling_variant(biezonderdere,   bijzonderdere).
spelling_variant(biezonderst,   bijzonderst).
spelling_variant(biezonderste,   bijzonderste).
spelling_variant(bijft,       blijft).
spelling_variant(binnenkamers,binnenskamers).
spelling_variant(brachtten,   brachten).
spelling_variant(buitenlanse, buitenlandse).
spelling_variant(centimer,    centimeter).
spelling_variant(conlusie,    conclusie).
spelling_variant(creeër,      creëer).
spelling_variant(creeërt,     creëert).
spelling_variant(creëeren,    creëren).
spelling_variant(creeëren,    creëren).
spelling_variant(creeërde,    creëerde).
spelling_variant(creeërden,   creëerden).
spelling_variant(gecreeërd,   gecreëerd).
spelling_variant(creër,       creëer).
spelling_variant(creërt,      creëert).
spelling_variant(creërde,     creëerde).
spelling_variant(creërden,    creëerden).
spelling_variant(gecreërd,    gecreëerd).
spelling_variant(creéer,      creëer).
spelling_variant(creéert,     creëert).
spelling_variant(creéerde,    creëerde).
spelling_variant(creéerden,   creëerden).
spelling_variant(creéren,     creëren).
spelling_variant(douch,       douche).
spelling_variant(gecreéerd,   gecreëerd).
spelling_variant(daarintegen, daarentegen).
spelling_variant(desalnietemin,desalniettemin).
spelling_variant(destijd,     destijds).
spelling_variant(dezelfe,     dezelfde).
spelling_variant(dichts,      dichtst).
spelling_variant(dichtsbijzijnde,      dichtstbijzijnde).
spelling_variant(dindsdag,    dinsdag).
spelling_variant(direkt,      direct).
spelling_variant(doogeschoten,doodgeschoten).
spelling_variant(doorbereken, doorberekenen).
spelling_variant(duizende,    duizenden).
spelling_variant(dult,        duldt).
spelling_variant(éé,          één).
spelling_variant(eistte,      eiste).
spelling_variant(eistten,     eisten).
spelling_variant('ge-e-maild','ge-emaild').
spelling_variant(failliette,  failliete).
spelling_variant(fronzen,     fronsen).
spelling_variant(gefronsd,    gefronst).
spelling_variant(gene,        gêne).
spelling_variant(formateer,   formatteer).
spelling_variant(formateert,  formatteert).
spelling_variant(formateren,  formatteren).
spelling_variant(geformateer, geformatteerd).
spelling_variant(formateerde, formatteerde).
spelling_variant(formateerden,formatteerden).
spelling_variant(frekwent,    frequent).
spelling_variant(frekwente,   frequente).
spelling_variant(frekwenter,  frequenter).
spelling_variant(frekwentere, frequentere).
spelling_variant(frekwentst,  frequentst).
spelling_variant(frekwentste, frequentste).
spelling_variant(fronsde,     fronste).
spelling_variant(fronsden,    fronsten).
spelling_variant(frusterend,  frustrerend).
spelling_variant(frusterende, frustrerende).
spelling_variant(gecrashd,    gecrasht).
spelling_variant(gecrashed,   gecrasht).
spelling_variant('ge-crashed',gecrasht).
spelling_variant('ge-crashd', gecrasht).
spelling_variant(gecoached,   gecoacht).
spelling_variant(gebeid,      gebied).
spelling_variant(geenzins,    geenszins).
spelling_variant(gegegeven,   gegeven).
spelling_variant(geleidde,    geleide).
spelling_variant(geplaats,    geplaatst).
spelling_variant(gepushed,    gepusht).
spelling_variant(geruimte,    geruime).
spelling_variant(geschikst,   geschiktst).
spelling_variant(geschikste,  geschiktste).
spelling_variant(gestichtte,  gestichte).
spelling_variant(geworde,     geworden).
spelling_variant(gezamelijk,  gezamenlijk).
spelling_variant(haden,       hadden).
spelling_variant(haddden,     hadden).
spelling_variant(hebbben,     hebben).
spelling_variant(heben,       hebben).
spelling_variant(heef,        heeft).
spelling_variant(heeeft,      heeft).
spelling_variant(herrinner,   herinner).
spelling_variant(herrinnert,  herinnert).
spelling_variant(herrinnerde, herinnerde).
spelling_variant(herrinnerden,herinnerden).
spelling_variant(herrinneren, herinneren).
spelling_variant(hielde,      hield).
spelling_variant(honderen,    honderden).
spelling_variant(honderde,    honderden).
spelling_variant(ht,          het).
spelling_variant(huidge,      huidige).
spelling_variant(indentiek,   identiek).
spelling_variant(indentificeer,   identificeer).
spelling_variant(indentificeert,  identificeert).
spelling_variant(indentificeren,  identificeren).
spelling_variant(geïndentificeer, geïdentificeerd).
spelling_variant(indentificeerde, identificeerde).
spelling_variant(indentificeerden,identificeerden).
spelling_variant(insziens,    inziens).
spelling_variant(interressant,interessant).
spelling_variant(interressante,interessante).
spelling_variant(jute,        juten).
spelling_variant(keeg,        kreeg).
spelling_variant(kijgen,      krijgen).
spelling_variant(kijgt,       krijgt).
spelling_variant(knieen,      knieën).
spelling_variant(kunsstof,    kunststof).
spelling_variant(kunsstoffen, kunststoffen).
spelling_variant(premiere,    première).
spelling_variant(koninging,   koningin).
spelling_variant(kunen,       kunnen).
spelling_variant(kunnnen,     kunnen).
spelling_variant(kunne,       kunnen).
spelling_variant(kwa,         qua).
spelling_variant(laatse,      laatste).
spelling_variant(lk,          ik).
spelling_variant(lostte,      loste).
spelling_variant(lostten,     losten).
spelling_variant(manoevreer,    manoeuvreer).
spelling_variant(manoevreert,   manoeuvreert).
spelling_variant(manoevreren,   manoeuvreren).
spelling_variant(gemanoevreerd, gemanoeuvreerd).
spelling_variant(manoevreerde,  manoeuvreerde).
spelling_variant(manoevreerden, manoeuvreerden).
spelling_variant(meerendeel,    merendeel).
spelling_variant(meot,          moet).
spelling_variant(milibar,       millibar).
spelling_variant(miligram,      milligram).
spelling_variant(mililiter,     milliliter).
spelling_variant(milimeter,     millimeter).
spelling_variant(miloen,        miljoen).
spelling_variant(mijoen,        miljoen).
spelling_variant(miloenen,        miljoenen).
spelling_variant(mijoenen,        miljoenen).
spelling_variant(ministeriele,ministeriële).
spelling_variant(mistte,      miste).
spelling_variant(mistten,     misten).
spelling_variant(moeit,       moeite).
spelling_variant('Nederlandsers','Nederlanders').
spelling_variant(nietemin,    niettemin).
spelling_variant(niewe,       nieuwe).
spelling_variant(officeel,    officieel).
spelling_variant(omgeveer,    ongeveer).
spelling_variant(ondek,       ontdek).
spelling_variant(ondekt,      ontdekt).
spelling_variant(ondekken,    ontdekken).
spelling_variant(ondekte,     ontdekte).
spelling_variant(ondekten,    ontdekten).

spelling_variant(onbreekt,    ontbreekt).
spelling_variant(onbreken,    ontbreken).
spelling_variant(onbrak,      ontbrak).
spelling_variant(onbraken,    ontbraken).
spelling_variant(ondere,      onder).
spelling_variant(ongever,     ongeveer).
spelling_variant(onmiddelijk, onmiddellijk).
spelling_variant(onstaan,     ontstaan).
spelling_variant(onstaat,     ontstaat).
spelling_variant(onstane,     ontstane).
spelling_variant(onstond,     ontstond).
spelling_variant(onstonden,   ontstonden).
spelling_variant(ontvreemdden,ontvreemden).
spelling_variant(opniew,      opnieuw).
spelling_variant(oogste,      oogstte).
spelling_variant(oogsten,     oogstten).
spelling_variant(ornitoloog,  ornitholoog).
spelling_variant(ornitologen, ornithologen).
spelling_variant(overeenkomt, overeenkomst).
spelling_variant(overgens,    overigens).
spelling_variant(penitentiare,penitentiaire).
spelling_variant(penitentiar, penitentiair).
spelling_variant(permiteer,   permitteer).
spelling_variant(permiteerde, permitteerde).
spelling_variant(permiteerden,permitteerden).
spelling_variant(permiteert,  permitteert).
spelling_variant(permiteren,  permitteren).
spelling_variant(gepermiteerd,gepermitteerd).
spelling_variant(pitbul,      pitbull).
spelling_variant(pitbuls,     pitbulls).
spelling_variant(plaatsvind,  plaatsvindt).
spelling_variant(progamma,    programma).
spelling_variant(profesioneel,professioneel).
spelling_variant(profesionele,professionele).
spelling_variant(quite,       quitte).
spelling_variant(rechtsstreeks,rechtstreeks).
spelling_variant(rechtsstreekse,rechtstreekse).
spelling_variant(registeren,  registreren).
spelling_variant(rigoreus,    rigoureus).
spelling_variant(rigoreuze,   rigoureuze).
spelling_variant(sateliet,    satelliet).
spelling_variant(schijft,     schrijft).
spelling_variant(sierraad,    sieraad).
spelling_variant(sind,        sinds).
spelling_variant(sinsdien,    sindsdien).
spelling_variant(suggeren,    suggereren).
spelling_variant(symphatiek,  sympathiek).
spelling_variant(symphatieke, sympathieke).
spelling_variant(onsymphatiek,onsympathiek).
spelling_variant(onsymphatieke,onsympathieke).
spelling_variant(onsymphathiek,onsympathiek).
spelling_variant(onsymphathieke,onsympathieke).
spelling_variant(tegehouden,tegenhouden).
spelling_variant(tegenoverstelde,
                              tegenovergestelde).
spelling_variant(tjdens,      tijdens).
spelling_variant(todat,       totdat).
spelling_variant(toekomt,     toekomst).
spelling_variant(tusen,       tussen).
spelling_variant(tusssen,     tussen).
spelling_variant(uitaard,     uiteraard).
spelling_variant(uitbreid,    uitgebreid).
spelling_variant(uitbreid,    uitbreidt).
spelling_variant(uitmond,     uitmondt).
spelling_variant(vam,         van).
spelling_variant(verrastte,   verraste).
spelling_variant(verassing,   verrassing).
spelling_variant(verloorloven,veroorloven).
spelling_variant(verontwaardigt,verontwaardigd).
spelling_variant(verslaggvers,verslaggevers).
spelling_variant(verwezelijk, verwezenlijk).
spelling_variant(verwezelijkt,verwezenlijkt).
spelling_variant(verwezelijken,verwezenlijken).
spelling_variant(verwezelijkte,verwezenlijkte).
spelling_variant(verwezelijkten,verwezenlijkten).
spelling_variant(vollop,      volop).
spelling_variant(vooor,       voor).
spelling_variant(wannneer,    wanneer).
spelling_variant(wanner,      wanneer).
spelling_variant(waneer,      wanneer).
spelling_variant(weing,       weinig).
spelling_variant(weining,     weinig).
spelling_variant(welliswaar,  weliswaar).
spelling_variant(wende,       wendde).
spelling_variant(wilen,       willen).
spelling_variant(woren,       worden).
spelling_variant(zachts,      zachtst). % op zijn zachts gezegd
spelling_variant(ziten,       zitten).
spelling_variant(zjn,         zijn).
spelling_variant(zulen,       zullen).
spelling_variant('Duiste',    'Duitse').
spelling_variant('Duitste',   'Duitse').
spelling_variant('Zwitsere',  'Zwitserse').  % 89 maal!

spelling_variant('vlgs',      volgens).

spelling_variant(bewierrook,     bewierook).
spelling_variant(bewierrookt,    bewierookt).
spelling_variant(bewierroken,    bewieroken).
spelling_variant(bewierrookte,   bewierookte).
spelling_variant(bewierrookten,  bewierookten).

%% really toe-eigenen is correct, but Alpino doesn't know...
spelling_variant('toe-eigen',    toeeigen).
spelling_variant('toe-eigent',   toeeigent).
spelling_variant('toe-eigenen',  toeeigenen).
spelling_variant('toe-eigende',  toeeigende).
spelling_variant('toe-eigenden', toeeigenden).
spelling_variant('toeëigen',    toeeigen).
spelling_variant('toeëigent',   toeeigent).
spelling_variant('toeëigenen',  toeeigenen).
spelling_variant('toeëigende',  toeeigende).
spelling_variant('toeëigenden', toeeigenden).

spelling_variant('de-installeer',deïnstalleer).
spelling_variant('de-installeert',deïnstalleert).
spelling_variant('de-installeren',deïnstalleren).
spelling_variant('de-installeerde',deïnstalleerde).
spelling_variant('de-installeerden',deïnstalleerden).
spelling_variant('gede-installeerd',gedeïnstalleerd).

%% twee-en-een-half
spelling_variant(tweeëneenhalf,tweeënhalf).
spelling_variant(drieëneenhalf,drieënhalf).
spelling_variant(viereneenhalf,vierenhalf).
spelling_variant(vijfeneenhalf,vijfenhalf).
spelling_variant(zeseneenhalf,zesenhalf).
spelling_variant(zeveneneenhalf,zevenenhalf).
spelling_variant(achteneenhalf,achtenhalf).
spelling_variant(negeneneenhalf,negenenhalf).

spelling_variant('d\'ruitziet',eruitziet).

spelling_variant('d\'raan',eraan).
spelling_variant('d\'rachter',erachter).
spelling_variant('d\'rachteraan',erachteraan).
spelling_variant('d\'raf',eraf).
spelling_variant('d\'rbij',erbij).
spelling_variant('d\'rboven',erboven).
spelling_variant('d\'rbovenop',erbovenop).
spelling_variant('d\'rbuiten',erbuiten).
spelling_variant('d\'rdoor',erdoor).
spelling_variant('d\'rdoorheen',erdoorheen).
spelling_variant('d\'rheen',ernaar).
spelling_variant('d\'rin',erin).
spelling_variant('d\'rlangs',erlangs).
spelling_variant('d\'rmee',ermee).
spelling_variant('d\'rna',erna).
spelling_variant('d\'rnaar',ernaar).
spelling_variant('d\'rnaartoe',ernaartoe).
spelling_variant('d\'rnaast',ernaast).
spelling_variant('d\'rom',erom).
spelling_variant('d\'romheen',eromheen).
spelling_variant('d\'ronder',eronder).
spelling_variant('d\'rop',erop).
spelling_variant('d\'rover',erover).
spelling_variant('d\'roverheen',eroverheen).
spelling_variant('d\'rtegen',ertegen).
spelling_variant('d\'rtegenaan',ertegenaan).
spelling_variant('d\'rtegenover',ertegenover).
spelling_variant('d\'rtoe',ertoe).
spelling_variant('d\'rtussen',ertussen).
spelling_variant('d\'rtussenuit',ertussenuit).
spelling_variant('d\'ruit',eruit).
spelling_variant('d\'rvan',ervan).
spelling_variant('d\'rvanaf',ervanaf).
spelling_variant('d\'rvandaan',ervandaan).
spelling_variant('d\'rvandoor',ervandoor).
spelling_variant('d\'rvoor',ervoor).

spelling_variant('Gaza-strook','Gazastrook').
spelling_variant('Groot-Britannië','Groot-Brittannië').
spelling_variant('Groot-Brittanië','Groot-Brittannië').
spelling_variant('Groot-Britanië','Groot-Brittannië').
spelling_variant('Groot-Britannie','Groot-Brittannië').
spelling_variant('Groot-Brittanie','Groot-Brittannië').
spelling_variant('Groot-Britanie','Groot-Brittannië').
spelling_variant('Groot-britannië','Groot-Brittannië').
spelling_variant('Groot-brittanië','Groot-Brittannië').
spelling_variant('Groot-britanië','Groot-Brittannië').
spelling_variant('Groot-britannie','Groot-Brittannië').
spelling_variant('Groot-brittanie','Groot-Brittannië').
spelling_variant('Groot-britanie','Groot-Brittannië').

spelling_variant41(doe,het,zelf,zaak,'doe-het-zelf-zaak').
spelling_variant41(doe,het,zelf,zaken,'doe-het-zelf-zaken').
spelling_variant41(doe,het,zelf,zaken,'doe-het-zelf-zaken').
spelling_variant41(twee,en,een,half, tweeënhalf).
spelling_variant41(drie,en,een,half, drieënhalf).
spelling_variant41(vier,en,een,half, vierenhalf).
spelling_variant41(vijf,en,een,half, vijfenhalf).
spelling_variant41(zes,en,een,half,  zesenhalf).
spelling_variant41(zeven,en,een,half,zevenenhalf).
spelling_variant41(acht,en,een,half, achtenhalf).
spelling_variant41(negen,en,een,half,negenenhalf).
spelling_variant41(in,en,in,triest,'in-en-in-triest').
spelling_variant41('in-',en,in,triest,'in-en-in-triest').

spelling_variant41('&','#',x20ac,';', euro).  % frequent in Sonar

spelling_variant51(blijf,van,mijn,lijf,huis,'blijf-van-mijn-lijf-huis').
spelling_variant51(blijf,van,me,lijf,huis,'blijf-van-mijn-lijf-huis').

%% diacritics missing (alternative is in lexicon too)
spelling_variant(a,           à).

spelling_variant('C=', euro).
spelling_variant('¤',  euro).
		 
%% special for IMIX
spelling_variant('anti_RSI_oefening','anti-RSI-oefening').
spelling_variant('anti_RSI_oefeningen','anti-RSI-oefeningen').
spelling_variant('RSI_bestrijding','RSI-bestrijding').
spelling_variant('RSI_klacht',     'RSI-klacht').
spelling_variant('RSI_klachten',   'RSI-klachten').
spelling_variant('RSI_patient',    'RSI-patiënt').
spelling_variant('RSI_patienten',  'RSI-patiënten').
spelling_variant('RSI_percentage', 'RSI-percentage').
spelling_variant('RSI_percentages','RSI-percentages').
spelling_variant(programmas,       'programma\'s').
spelling_variant(categorien,        categorieën).
spelling_variant(nederland,        'Nederland').
spelling_variant(nederlands,       'Nederlands').
spelling_variant(nederlandse,      'Nederlandse').
spelling_variant(nederlander,      'Nederlander').
spelling_variant(nederlanders,     'Nederlanders').
spelling_variant(europa,           'Europa').
spelling_variant(europese,         'Europese').
spelling_variant(workpace,         workspace).

%% result of tokenization errors
spelling_variant('al.',    al).
spelling_variant('u.',      u).
spelling_variant('i.',      i).

spelling_variant('.!',     '.').

%% twitter
spelling_variant('1tje',  eentje).
spelling_variant(abbo,    abonnement).
spelling_variant(ah,      'AH').  % ik werk bij de ah
spelling_variant(ak,      aardrijkskunde).
spelling_variant(altyd,   altijd).
spelling_variant(bn,      ben).
spelling_variant(buite,   buiten).
spelling_variant(binne,   binnen).
spelling_variant(binnekort,   binnenkort).
spelling_variant(btg,     beltegoed).
spelling_variant(chagerijnig, chagrijnig).
spelling_variant(chagerijnige, chagrijnige).
spelling_variant(cker,    zeker).
spelling_variant(dalijk,  dadelijk).
spelling_variant(d8,      dacht).
spelling_variant(dagt,    dacht).
spelling_variant(dese,    deze).
spelling_variant(dn,      dan).
spelling_variant(drm,     daarom).
spelling_variant(dt,      dat).
spelling_variant(duss,    dus).
spelling_variant(egt,     echt).
spelling_variant(eigelijk,eigenlijk).
spelling_variant(eigenlijks,eigenlijk).
spelling_variant(eigelijks,eigenlijk).
spelling_variant(enne,    en).
spelling_variant(enso,    enzo).
spelling_variant(et,      '\'t').
spelling_variant(facking, fucking).
spelling_variant(fakking, fucking).
spelling_variant(focking, fucking).
spelling_variant(fokking, fucking).
spelling_variant(followd, followt).
spelling_variant(followed,followt).
spelling_variant(fotos,   'foto\'s').
spelling_variant(gaa,     ga).
spelling_variant(gedouched,gedoucht).
spelling_variant('ge-eist',geëist).
spelling_variant(gestrest,gestresst).
spelling_variant(gezegt,  gezegd).
spelling_variant(gha,     ga).
spelling_variant(gs,      geschiedenis).
spelling_variant(gwoon,   gewoon).
spelling_variant(hb,      heb).
spelling_variant(hw,      huiswerk).
spelling_variant(hy,      hij).
spelling_variant(indd,    inderdaad).
spelling_variant(iriteer, irriteer).
spelling_variant(jy,      jij).
spelling_variant(k,       ik).
spelling_variant(lkkr,    lekker).
spelling_variant(maare,   maar).
spelling_variant(men,     'm\'n').
spelling_variant(mnd,     maand).
spelling_variant(morge,   morgen).
spelling_variant(mt,      met).
spelling_variant(myn,     mijn).
spelling_variant(neee,    nee).
spelling_variant(neit,    niet).
spelling_variant(ng,      nog).
spelling_variant(nii,     niet).
spelling_variant(nix,     niets).
spelling_variant(ofso,    ofzo).
spelling_variant(ofz,     ofzo).
spelling_variant(ofzoo,   ofzo).
spelling_variant(pw,      proefwerk).
spelling_variant(rep,     repetitie).
spelling_variant(same,    samen).
spelling_variant(sgool,   school).
spelling_variant(slape,   slapen).
spelling_variant(spaced,  spacet).
spelling_variant(strakkies,strakjes).
spelling_variant(strax,   straks).
spelling_variant(suc6,    succes).
spelling_variant(sws,     sowieso).
spelling_variant(t,      '\'t').
spelling_variant(tege,    tegen).
spelling_variant(tel,     telefoon).
spelling_variant(teminste,tenminste).
spelling_variant(tie,     hij).
spelling_variant(tog,     toch).
spelling_variant(togh,    toch).
spelling_variant(um,      '\'m').
spelling_variant(ut,      '\'t').
spelling_variant(vanaaf,  vanavond).
spelling_variant(vnv,     vanavond).
spelling_variant(vanacht, vannacht).
spelling_variant(veul,    veel).
spelling_variant(vn,      van).
spelling_variant(vn,      'VN').
spelling_variant(vo,      voor).
spelling_variant(volges,  volgens).
spelling_variant(w8,      wacht).
spelling_variant(waars,   waarschijnlijk).
spelling_variant('we\'ll',wel).
spelling_variant(wnr,     wanneer).
spelling_variant(wnt,     want).
spelling_variant(wrm,     waarom).
spelling_variant(wss,     waarschijnlijk).
spelling_variant(wt,      wat).
spelling_variant(zn,      'z\'n').
spelling_variant(zoo,     zo).
spelling_variant(zooo,    zo).
spelling_variant(zoiezo,  sowieso).
spelling_variant(zowiezo, sowieso).
spelling_variant(zyn,     zijn).

spelling_variant(lk,      ik).  % small l is capital I in some fonts
spelling_variant(ln,      in).
spelling_variant(ls,      is).

spelling_variant(word,    wordt) :-
    \+ word_form(ik).
spelling_variant(vind,    vindt) :-
    \+ word_form(ik).

spelling_variant(kondt, kunt).

spelling_variant('Belgie','België').
spelling_variant('Brazilie','Brazilië').
spelling_variant('Israel','Israël').
spelling_variant('Israels','Israëls').
spelling_variant('Italie','Italië').
spelling_variant('Munchen','München').

spelling_variant('Nos','NOS').
spelling_variant('Psv','PSV').
spelling_variant('V.S.','VS').

parse_only_variant(notabene,'nota bene', modal_adverb).
parse_only_variant(opzoek, 'op zoek', adjective(pred(padv))).
parse_only_variant(opzoek, 'op zoek', adjective(pred(padv),pp(naar))).

%% twitter, opensubtitles
parse_only_variant('aan\'t', 'aan het',complementizer(aan_het)).
parse_only_variant('aan`t', 'aan het',complementizer(aan_het)).
parse_only_variant(aant, 'aan het',complementizer(aan_het)).

parse_only_variant(zegmaar,'zeg maar',adverb).
parse_only_variant(zegmaar,'zeg maar',adv_tag).
parse_only_variant(zegmaar,'zeg maar',modal_adverb(adv_noun_prep)).

parse_only_variant('\'r',er,    er_vp_adverb).
parse_only_variant('\'r',haar,  determiner(pron)).
%% 'r is not a variant of haar as pronoun, because its tag changes to wkpro

parse_only_variant(der, haar,  determiner(pron)).
parse_only_variant(der, er,    er_vp_adverb).
parse_only_variant(der, haar,  gen_determiner(sg)).

parse_only_variant(dr,  haar,   determiner(pron)).
parse_only_variant(dr,  er,     er_vp_adverb).
parse_only_variant(dr,  haar,   gen_determiner(sg)).

parse_only_variant('d\'r',er,   er_vp_adverb).
parse_only_variant('d\'r',haar, determiner(pron)).
parse_only_variant('d\'r',haar, gen_determiner(sg)).

parse_only_variant('d\'r', '\'r',  pronoun(nwh,thi,sg,de,dat_acc,def,wkpro)).
parse_only_variant(dr,     '\'r',  pronoun(nwh,thi,sg,de,dat_acc,def,wkpro)).
parse_only_variant(der,    '\'r',  pronoun(nwh,thi,sg,de,dat_acc,def,wkpro)).

parse_only_variant(heul, heel,    intensifier).

parse_only_variant(aant,'aan het', preposition(aan,[],nodet)).
parse_only_variant(id,'in de', preposition(in,[],nodet)).

parse_only_variant(jaa,    ja,     tag).
parse_only_variant(jaaa,   ja,     tag).
parse_only_variant(jaaaa,  ja,     tag).
parse_only_variant(jaaaaa, ja,     tag).

parse_only_variant(mn,     mijn,   determiner(pron)).
parse_only_variant(me,     mijn,   determiner(pron)).
parse_only_variant('m\'ne',mijn,   determiner(pron)).
parse_only_variant('m\'n', mijn,   determiner(pron)).

parse_only_variant(kk,kanker,adjective(prefix)).
parse_only_variant(kkr,kanker,adjective(prefix)).

parse_only_variant(kk,kanker,intensifier).
parse_only_variant(kkr,kanker,intensifier).

%% others
parse_only_variant(rede, reden,noun(de,count,sg)).
parse_only_variant(rede, reden,noun(de,count,sg,_)).

parse_only_variant(vraagde, vroeg,verb(_,_,_)).
parse_only_variant(vraagden,vroegen,verb(_,_,_)).

parse_only_variant(ware,    was,    verb(_,_,_)).
parse_only_variant(waart,   was,    verb(_,_,_)).

parse_only_variant(zoja,    'zo ja',sbar).

parse_only_lex(ie,      hij,    pronoun(nwh,thi,sg,de,nom,def,wkpro)).
parse_only_lex('-ie',   hij,    pronoun(nwh,thi,sg,de,nom,def,wkpro)).
parse_only_lex('\'ie',  hij,    pronoun(nwh,thi,sg,de,nom,def,wkpro)).

% qtleap: login op de website
% ipv     log in op de website
parse_only_lex(login,  v_root(log_in,inloggen), verb(hebben,imp(sg1),intransitive)).

parse_only_lex(deste,'des te',hoe_adv).

parse_only_lex(ookal,'ook al',complementizer(al)).
parse_only_lex(ookal,'ook al',adverb).

parse_only_lex(iig,       'in ieder geval',  sentence_adverb).
parse_only_lex(nieteens,  'niet eens',       sentence_adverb).
parse_only_lex(nogsteeds, 'nog steeds',      sentence_adverb).
parse_only_lex(ofniet,    'of niet',         etc).
parse_only_lex(omw,       'on my way',       adjective(pred(dir_locadv))).
parse_only_lex(optijd,    'op tijd',         adjective(pred(adv))).
parse_only_lex(telaat,    'te laat',         adjective(pred(adv))).
parse_only_lex(volgensmij,'volgens mij',     sentence_adverb).

parse_only_lex(es,      eens,  wk_tmp_adverb).
parse_only_lex('\'s',   eens,  wk_tmp_adverb).
parse_only_lex('\'ns',  eens,  wk_tmp_adverb).

parse_only_lex(tov,'ten opzichte van',preposition([ten,opzichte,van],[]),L0,L) :-
    next_word(van,L0,L,_).
parse_only_lex(ivm,'in verband met',preposition([in,verband,met],[]),L0,L) :-
    next_word(met,L0,L,_).

%% 6

parse_only_lex(Word,Word,noun(het,count,sg),Ws,Ws,['Het'|_]) :-
    de_word_used_as_het(Word).

parse_only_lex(Word,Word,noun(de,count,sg),Ws,Ws,[de|_]) :-
    het_word_used_as_de(Word).

parse_only_lex(Word,Word,noun(de,count,sg),Ws,Ws,['De'|_]) :-
    het_word_used_as_de(Word).

parse_only_lex(Word,Word,noun(de,count,sg),Ws,Ws,[deze|_]) :-
    het_word_used_as_de(Word).

parse_only_lex(Word,Word,noun(de,count,sg),Ws,Ws,['Deze'|_]) :-
    het_word_used_as_de(Word).

parse_only_lex(Abbr,Label,Tag,['.'|Ws],Ws,LC):-
    abbreviation(Abbr,Word),
    atom(Word),
    lexicon__(Word,Tag,Label,[],[],_His,LC).

parse_only_lex(Abbr,Label,Tag,['.'|Ws],Ws,LC):-
    abbreviation(Abbr,Word),
    Word = [H|T],
    lexicon__(H,Tag0,Label,T,[],_His,LC),
    (   Tag0 = with_dt(Tag,_)
    ->  true
    ;   Tag0 = Tag
    ).
    % otherwise positions in with_dt don't make sense

parse_only_lex(Word,Root,Tag,Ws,Ws,_LC) :-
    parse_only_lex(Word,Root,Tag).

parse_only_lex(Word,Root,Tag,Ws0,Ws,_LC) :-
    parse_only_lex(Word,Root,Tag,Ws0,Ws).

%% 5

parse_only_lex(de,     hoeveel,wh_adjective(odet_adv),                  [hoeveel|X],X).

parse_only_lex(de,     gij,    pronoun(nwh,inv,sg,both,both,def),       [gij|X],   X).
parse_only_lex(de,     jullie, pronoun(nwh,inv,both,de,nom,def),        [gijle|X], X).
parse_only_lex(de,     jullie, pronoun(nwh,inv,both,de,nom,def),        [gulder|X],X).
parse_only_lex(de,     één,    pronoun(nwh,thi,sg,both,both,def,strpro),[enen|X],  X).

parse_only_lex(en,     'en/of',conj('en/of'),                           [of|X],    X).
parse_only_lex(of,     'of dat',     complementizer(of),                      [dat|X],   X).

parse_only_lex(hetgeen,waar,   er_wh_loc_adverb,                        [waar|X],  X).

parse_only_lex('\'s',   wereld, determiner(pron),               [wereld|X],    X).
parse_only_lex('\'',    wereld, determiner(pron),               [swerelds|X],  X).
parse_only_lex('\'',    wereld, determiner(pron),               [sWerelds|X],  X).
parse_only_lex('`',     wereld, determiner(pron),               [swerelds|X],  X).
parse_only_lex('`',     wereld, determiner(pron),               [sWerelds|X],  X).

parse_only_lex(over,    'over de schreef',    fixed_part([over,de,schreef]),   [de,scheef|X],X).

parse_only_lex(s,      '\'s avonds',    tmp_adverb, [avonds|X],    X).
parse_only_lex(s,      '\'s middags',   tmp_adverb, [middags|X],   X).
parse_only_lex(s,      '\'s morgens',   tmp_adverb, [morgens|X],   X).
parse_only_lex(s,      '\'s nachts',    tmp_adverb, [nachts|X],    X).
parse_only_lex(s,      '\'s ochtends',  tmp_adverb, [ochtends|X],  X).

parse_only_lex(s,      '\'s avonds',    tmp_adverb, ['Avonds'|X],    X).
parse_only_lex(s,      '\'s middags',   tmp_adverb, ['Middags'|X],   X).
parse_only_lex(s,      '\'s morgens',   tmp_adverb, ['Morgens'|X],   X).
parse_only_lex(s,      '\'s nachts',    tmp_adverb, ['Nachts'|X],    X).
parse_only_lex(s,      '\'s ochtends',  tmp_adverb, ['Ochtends'|X],  X).

parse_only_lex('\'',  '\'s avonds',    tmp_adverb, [savonds|X],    X).
parse_only_lex('\'',  '\'s middags',   tmp_adverb, [smiddags|X],   X).
parse_only_lex('\'',  '\'s morgens',   tmp_adverb, [smorgens|X],   X).
parse_only_lex('\'',  '\'s nachts',    tmp_adverb, [snachts|X],    X).
parse_only_lex('\'',  '\'s ochtends',  tmp_adverb, [sochtends|X],  X).

parse_only_lex('\'',  '\'s avonds',    tmp_adverb, [sAvonds|X],    X).
parse_only_lex('\'',  '\'s middags',   tmp_adverb, [sMiddags|X],   X).
parse_only_lex('\'',  '\'s morgens',   tmp_adverb, [sMorgens|X],   X).
parse_only_lex('\'',  '\'s nachts',    tmp_adverb, [sNachts|X],    X).
parse_only_lex('\'',  '\'s ochtends',  tmp_adverb, [sOchtends|X],  X).

parse_only_lex('DEN','Den Haag',proper_name(sg,'LOC'),['HAAG'|X],  X).

%% meta-commentaar in Mediargus
parse_only_lex(kranteregel,krant_regel,noun(de,count,sg)).

parse_only_lex('\'m',  hem,    fixed_part([hem])).
parse_only_lex(m,      hem,    fixed_part([hem])).
parse_only_lex(heur,   haar,   determiner(pron)).
parse_only_lex(nen,    een,    determiner(een)).
parse_only_lex(ne,     een,    determiner(een)).
parse_only_lex(ene,    een,    determiner(een)).
parse_only_lex(enen,   een,    determiner(een)).

parse_only_lex(enen,   één,    pronoun(nwh,thi,sg,both,both,indef,strpro)).

parse_only_lex(dieje,  die,    determiner(de,nwh,nmod,pro,nparg)).
parse_only_lex(diejen, die,    determiner(de,nwh,nmod,pro,nparg)).
parse_only_lex(diene,  die,    determiner(de,nwh,nmod,pro,nparg)).
parse_only_lex(dien,   die,    determiner(de,nwh,nmod,pro,nparg)).

parse_only_lex(ulder,  hun,    determiner(pron)).
parse_only_lex(ulder,  hun,    pronoun(nwh,thi,pl,de,dat_acc,def)).
parse_only_lex(zullie, hun,    determiner(pron)).
parse_only_lex(zullie, hun,    pronoun(nwh,thi,pl,de,dat_acc,def)).
parse_only_lex(ulle,   jullie, pronoun(nwh,je,pl,de,dat_acc,def)).
parse_only_lex(gijle,  jullie, pronoun(nwh,je,both,de,nom,def)).
parse_only_lex(gulder, jullie, pronoun(nwh,inv,both,de,nom,def)).

parse_only_lex(mijne,  mijn,   determiner(pron)).
parse_only_lex(mijner, mijn,   determiner(pron)).

parse_only_lex(onzer,  ons,    determiner(pron)).

parse_only_lex(uwe,    uw,     determiner(pron)).
parse_only_lex(uwen,   uw,     determiner(pron)).
% parse_only_lex(uwer,   uw,     determiner(pron)).

% parse_only_lex(zijner, zijn,   determiner(pron)).

parse_only_lex(zijne,  zijn,   determiner(pron)).

parse_only_lex(moogt,  v_root(mag,mogen),    verb(hebben,sg3,intransitive)).
parse_only_lex(moogt,  v_root(mag,mogen),    verb(hebben,sg3,ld_pp)).
parse_only_lex(moogt,  v_root(mag,mogen),    verb(hebben,sg3,ld_adv)).
parse_only_lex(moogt,  v_root(mag,mogen),    verb(hebben,sg3,ld_dir)).
parse_only_lex(moogt,  v_root(mag,mogen),    verb(hebben,sg3,uit)).
parse_only_lex(moogt,  v_root(mag,mogen),    verb(hebben,sg3,transitive_ndev_ndev)).
parse_only_lex(moogt,  v_root(mag,mogen),    verb(hebben,sg3,passive)).
parse_only_lex(moogt,  v_root(mag,mogen),    verb(hebben,sg3,nonp_pred_np_ndev)).
parse_only_lex(moogt,  v_root(mag,mogen),    verb(hebben,sg3,modifier(aux(inf)))).

parse_only_lex(geweest, v_root(ben,zijn), verb(hebben,psp,copula)).

parse_only_lex(té,     te,     intensifier).
parse_only_lex(té,     te,     vp_om_intensifier).
parse_only_lex(té,     te,     me_intensifier).
parse_only_lex(té,     te,     vp_om_me_intensifier).
parse_only_lex(tè,     te,     intensifier).
parse_only_lex(tè,     te,     vp_om_intensifier).
parse_only_lex(tè,     te,     me_intensifier).
parse_only_lex(tè,     te,     vp_om_me_intensifier).

parse_only_lex(den,    de,     determiner(den)).

parse_only_lex(nikske, niks,   meas_mod_noun(het,mass,sg)).
parse_only_lex(nikske, niks,   meas_mod_noun(het,mass,sg,pred_pp(voor))).

%% (Vlaamse) blog etc.
parse_only_lex(em,     hem,    pronoun(nwh,thi,sg,de,dat_acc,def) ).
parse_only_lex(goe,    goed,   adjective(no_e(adv))    ).
parse_only_lex(greetz, groet,  noun(de,count,pl)       ).
parse_only_lex(grtz,   groet,  noun(de,count,pl)       ).
parse_only_lex(gwn,    gewoon, adjective(no_e(adv))    ).
parse_only_lex(gewn,   gewoon, adjective(no_e(adv))    ).
parse_only_lex(ier,    hier,   er_loc_adverb           ).
parse_only_lex(ni,     niet,   adverb                  ).
parse_only_lex(nie,    niet,   adverb                  ).
parse_only_lex(niej,   niet,   adverb                  ).
parse_only_lex(oek,    ook,    sentence_adverb         ).
parse_only_lex(omda,   omdat,  complementizer          ).
parse_only_lex(tis,    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(copula),incorporated_subj_topic(copula)))).
parse_only_lex(tis,    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(intransitive),incorporated_subj_topic(intransitive)))).
parse_only_lex(tis,    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(passive),incorporated_subj_topic(passive)))).
parse_only_lex(tis,    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(te_passive),incorporated_subj_topic(te_passive)))).
parse_only_lex(tis,    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(pc_pp(van)),incorporated_subj_topic(pc_pp(van))))).
parse_only_lex('da\'s',    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(copula),incorporated_subj_topic(copula)))).
parse_only_lex('da\'s',    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(intransitive),incorporated_subj_topic(intransitive)))).
parse_only_lex('da\'s',    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(passive),incorporated_subj_topic(passive)))).
parse_only_lex('da\'s',    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(te_passive),incorporated_subj_topic(te_passive)))).
parse_only_lex('da\'s',    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(pc_pp(van)),incorporated_subj_topic(pc_pp(van))))).
parse_only_lex('t\'is',    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(copula),incorporated_subj_topic(copula)))).
parse_only_lex('t\'is',    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(intransitive),incorporated_subj_topic(intransitive)))).
parse_only_lex('t\'is',    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(passive),incorporated_subj_topic(passive)))).
parse_only_lex('t\'is',    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(te_passive),incorporated_subj_topic(te_passive)))).
parse_only_lex('t\'is',    v_root(ben,zijn),    verb(zijn,sg3,ninv(incorporated_subj_topic(pc_pp(van)),incorporated_subj_topic(pc_pp(van))))).
parse_only_lex(kben,    v_root(ben,zijn),    verb(zijn,sg1,ninv(incorporated_subj_topic(copula),incorporated_subj_topic(copula)))).
parse_only_lex(kben,    v_root(ben,zijn),    verb(zijn,sg1,ninv(incorporated_subj_topic(intransitive),incorporated_subj_topic(intransitive)))).
parse_only_lex(kben,    v_root(ben,zijn),    verb(zijn,sg1,ninv(incorporated_subj_topic(passive),incorporated_subj_topic(passive)))).
parse_only_lex(kben,    v_root(ben,zijn),    verb(zijn,sg1,ninv(incorporated_subj_topic(te_passive),incorporated_subj_topic(te_passive)))).
parse_only_lex(kben,    v_root(ben,zijn),    verb(zijn,sg1,ninv(incorporated_subj_topic(pc_pp(van)),incorporated_subj_topic(pc_pp(van))))).

parse_only_lex(kzou,    v_root(zal,zullen),  verb(hebben,past(sg),ninv(incorporated_subj_topic(aux(inf)),incorporated_subj_topic(aux(inf))))).

parse_only_lex('zei-die',v_root(zeg,zeggen),   verb(hebben,past(sg),ninv(incorporated_subj_topic(transitive),incorporated_subj_topic(transitive)))).
parse_only_lex('zei-die',v_root(zeg,zeggen),   verb(hebben,past(sg),ninv(incorporated_subj_topic(sbar),incorporated_subj_topic(sbar)))).

parse_only_lex(kga,v_root(ga,gaan),verb(zijn,sg1,ninv(incorporated_subj_topic(aux(inf)),incorporated_subj_topic(aux(inf))))).
parse_only_lex(kga,v_root(ga,gaan),verb(zijn,sg1,ninv(incorporated_subj_topic(ld_pp),incorporated_subj_topic(ld_pp)))).
parse_only_lex(kga,v_root(ga,gaan),verb(zijn,sg1,ninv(incorporated_subj_topic(ap_copula),incorporated_subj_topic(ap_copula)))).
parse_only_lex(kga,v_root(ga,gaan),verb(zijn,sg1,ninv(incorporated_subj_topic(intransitive),incorporated_subj_topic(intransitive)))).
parse_only_lex(kga,v_root(ga,gaan),verb(zijn,sg1,ninv(part_incorporated_subj_topic(weg,intransitive),incorporated_subj_topic(intransitive)))).

parse_only_lex(kheb,v_root(heb,hebben),verb(hebben,sg1,ninv(incorporated_subj_topic(aux_psp_hebben),incorporated_subj_topic(aux_psp_hebben)))).
parse_only_lex(kheb,v_root(heb,hebben),verb(hebben,sg1,ninv(incorporated_subj_topic(transitive_ndev),incorporated_subj_topic(transitive_ndev)))).
parse_only_lex(kheb,v_root(heb,hebben),verb(hebben,sg1,ninv(incorporated_subj_topic(nonp_pred_np_ndev),incorporated_subj_topic(nonp_pred_np_ndev)))).

parse_only_lex(kvin,v_root(vind,vinden),verb(hebben,sg1,ninv(incorporated_subj_topic(X),incorporated_subj_topic(X)))) :-
    lists:member(X,[pred_np,tr_sbar,van_sbar,np_pc_pp(aan),transitive_ndev_ndev]).

parse_only_lex(zen,    zijn,   determiner(pron)        ).

parse_only_lex(ofdat,  of,     complementizer(of)      ).

parse_only_lex(ff,      even,      adjective(both(tmpadv)) ).
parse_only_lex(fff,     even,      adjective(both(tmpadv)) ).
parse_only_lex(fftjes,  eventjes,  adjective(both(tmpadv)) ).
parse_only_lex(eff,     even,      adjective(both(tmpadv)) ).
parse_only_lex(effe,    even,      adjective(both(tmpadv)) ).
parse_only_lex(effetjes,eventjes,  adjective(both(tmpadv)) ).
parse_only_lex(efkes,   eventjes,  adjective(both(tmpadv)) ).
parse_only_lex(effekes, eventjes,  adjective(both(tmpadv)) ).

parse_only_lex(instaat,'in staat', Tag) :-
    xl(in,Tag,'in staat',[staat],[]).

%%% many variants of 's nachts etc

% s-nachts s-Nachts
parse_only_lex(Atom, Root, Tag ) :-
    atom_concat('s-',Avonds,Atom),
    avonds(Avonds,Tag,Root).

% s'nachts s'Nachts
parse_only_lex(Atom, Root, Tag ) :-
    atom_concat('s\'',Avonds,Atom),
    avonds(Avonds,Tag,Root).

% snachts sNachts
parse_only_lex(Atom, Root, Tag ) :-
    atom_concat('s',Avonds,Atom),
    avonds(Avonds,Tag,Root).

parse_only_lex(welks,welk,determiner(pron,rwh)).
parse_only_lex(welker,welk,determiner(pron,rwh)).

% des nachts des Nachts
parse_only_lex(des, Root, Tag,[Avonds],[]) :-
    avonds(Avonds,Tag,Root).

parse_only_lex(de, welk, determiner(welke,rwh,nmod,pro,yparg),[welke],[]).

avonds(Avonds,Tag,Root) :-
    lexicon_('\'s',Tag,Root,[Avonds],[],_,[]).

avonds(Avonds0,Tag,Root) :-
    alpino_unknowns:decap_first(Avonds0,Avonds),
    lexicon_('\'s',Tag,Root,[Avonds],[],_,[]).

de_word_used_as_het(cover).
de_word_used_as_het(epiloog).
de_word_used_as_het(proloog).

het_word_used_as_de(boek).
het_word_used_as_de(vervolg).



%%%%%%%%%%%%%%%%% abbreviations: both for parsing and generation 

abbreviation_b('a.s',    aanstaande).
abbreviation_b(beh,      behalve).
abbreviation_b(bijv,     bijvoorbeeld).
abbreviation_b(bvb,      bijvoorbeeld).
abbreviation_b(bv,       bijvoorbeeld).
abbreviation_b(vb,       bijvoorbeeld).
abbreviation_b(ca,       circa).
abbreviation_b(el,       eetlepel).
abbreviation_b(eetl,     eetlepel).
abbreviation_b(el,       eetlepels).
abbreviation_b(eetl,     eetlepels).
abbreviation_b(enz,      enzovoorts).
abbreviation_b(etc,      etcetera).
abbreviation_b(evt,      eventueel).
abbreviation_b(evt,      eventuele).
abbreviation_b(gekwalif, gekwalificeerd).
abbreviation_b(gespr,    gesproken).
abbreviation_b(gespr,    gesproken).
abbreviation_b(gespr,    gesproken).
abbreviation_b(herv,     hervormd).
abbreviation_b(herv,     hervormde).
abbreviation_b(incl,     inclusief).
abbreviation_b(exc,      exclusief).
abbreviation_b(excl,     exclusief).
abbreviation_b(jl,       jongstleden).
abbreviation_b(jr,       junior).
abbreviation_b(kath,     katholiek).
abbreviation_b(mld,      miljard).
abbreviation_b(mln,      miljoen).
abbreviation_b('mln.',   miljoen).
abbreviation_b(mnd,      maand).
abbreviation_b('Pag',    pagina).
abbreviation_b(pag,      pagina).
abbreviation_b(pg,       pagina).
abbreviation_b(pag,      'pagina\'s').
abbreviation_b(sr,       senior).
abbreviation_b(verz,     verzorger).
abbreviation_b(verz,     verzorging).
abbreviation_b(gr,       gram).
abbreviation_b(ha,       hectare).
abbreviation_b(cm,       centimeter).
abbreviation_b(dm,       decimeter).
abbreviation_b(dl,       deciliter).
abbreviation_b(ft,       foot).
abbreviation_b(kg,       kilogram).
abbreviation_b(km,       kilometer).
abbreviation_b(m,        meter).
abbreviation_b(mm,       millimeter).
abbreviation_b(mg,       milligram).
abbreviation_b(ml,       milliliter).
abbreviation_b(nr,       nummer).
abbreviation_b(nrs,      nummers).
abbreviation_b(pct,      procent).
abbreviation_b(res,      reserveringen).
abbreviation_b(theel,    theelepel).
abbreviation_b(tl,       theelepel).
abbreviation_b(theel,    theelepels).
abbreviation_b(tl,       theelepels).
abbreviation_b(bl,       bladzij).
abbreviation_b(blz,      bladzij).
abbreviation_b(bladz,    bladzij).
abbreviation_b(sec,      seconden).
abbreviation_b(pnt,      punten).
abbreviation_b('Inl',    inlichtingen).
abbreviation_b('inl',    inlichtingen).
abbreviation_b(ma,       maandag).
abbreviation_b(di,       dinsdag).
abbreviation_b(wo,       woensdag).
abbreviation_b(do,       donderdag).
abbreviation_b(vr,       vrijdag).
abbreviation_b(za,       zaterdag).

abbreviation(Word,Alternative,Ws,Ws) :-
    abbreviation(Word,Alternative).

abbreviation(WordPunt, FullWord) :-
    atom(WordPunt),
    atom_concat(Word,'.',WordPunt),
    abbreviation_b(Word,FullWord).

abbreviation(Word, FullWord) :-
    abbreviation_b(Word,FullWord).

abbreviation('WB',     wereldbeker).
abbreviation('aant.',  aantekeningen).
abbreviation('adr.',   adres).
abbreviation('adr.',   adressen).
abbreviation('afb.',   afbeelding).
abbreviation('afgel.', afgelast).
abbreviation('alc.',   alcohol).
abbreviation('alg.',   algemeen).
abbreviation('alg.',   algemene).
abbreviation('afk.',   afkorting).
abbreviation('afl.',   aflevering).
abbreviation('Afl.',   aflevering).
abbreviation('art.',   artikel).
abbreviation('a.u.b.', alstublieft).
abbreviation('aub',    alstublieft).
abbreviation('s.v.p.', alstublieft).
abbreviation('svp',    alstublieft).
abbreviation('ber.',   beroepen).
abbreviation('b.v.',   bijvoorbeeld).
abbreviation('bv.',    bv).
abbreviation('bijv.',  bijvoeglijke).
abbreviation('bz.',    bijzin).
abbreviation(cb,       consultatieburo).
abbreviation('Chr.',   christelijk).
abbreviation('Chr.',   christelijke).
abbreviation('chr.',   christelijk).
abbreviation('chr.',   christelijke).
abbreviation('cons.',  conservatief).
abbreviation('cons.',  conservatieve).
abbreviation('dag.',   dagelijks).
abbreviation('dep.',   departement).
abbreviation(dgn,      dagen).
abbreviation('dir.',   directeur).
abbreviation('div.',   divers).
abbreviation('div.',   diverse).
abbreviation('div.',   divisie).
abbreviation(dln,      delen).
abbreviation('Duitsl.','Duitsland').
abbreviation('eig.',   eigenlijk).
abbreviation('electr.', electrisch).
abbreviation('electr.', electrische).
abbreviation('elektr.', electrisch).
abbreviation('elektr.', electrische).
%abbreviation('\'m',    hem).  % wkpro
abbreviation('fam.',   familie).
abbreviation('geïll.', geïllustreerd).
abbreviation('gelijkn.',gelijknamig).
abbreviation('gelijkn.',gelijknamige).
abbreviation('geref.', gereformeerd).
abbreviation('geref.', gereformeerde).
abbreviation('gest.',  gestorven).
abbreviation('hoogl.', hoogleraar).
abbreviation('\'k',    ik).
abbreviation(idd,      inderdaad).
abbreviation('inf.',   infanterie).
abbreviation('inl.',   inlichtingen).
abbreviation('inl.',   inleiding).
abbreviation('geb.',   geboren).
abbreviation('imp.',   importeur).
abbreviation('inw.',   inwoners).
abbreviation('Jap.',   'Japans').
abbreviation('Jap.',   'Japanse').
abbreviation('Jap.',   'Japan').
abbreviation('j.l.',   jongstleden).
abbreviation('kand.',  kandidaat).
abbreviation('kon.',   koninklijk).
abbreviation('kon.',   koninklijke).
abbreviation('lett.',  letterlijk).
abbreviation('lib.',   liberaal).
abbreviation(mil,      miljoen).
abbreviation(mischien, misschien).
abbreviation(mss,      misschien).
abbreviation('Ned.',   'Nederland').
abbreviation('Ned.',   'Nederlands').
abbreviation('Ned.',   'Nederlandse').
abbreviation(nl,       'Nederland').
abbreviation('nl.',    namelijk).
abbreviation('n.l.',    namelijk).
abbreviation('overl.', overleden).
abbreviation('plm.',   plusminus).
abbreviation('plus-minus',   plusminus).
abbreviation('plv.',   plaatsvervangend).
abbreviation('plv.',   plaatsvervangende).
abbreviation('pred.',  predikant).
abbreviation('prot.',  protestant).
abbreviation('prot.',  protestants).
abbreviation('red.',   redactie).
abbreviation(redir,    redirect).
abbreviation('r.k.',   'rooms-katholiek').
abbreviation('r.k.',   'rooms-katholieke').
abbreviation('r.-k.',   'rooms-katholiek').
abbreviation('r.-k.',   'rooms-katholieke').
abbreviation('rk',     'rooms-katholiek').
abbreviation('rk',     'rooms-katholieke').
abbreviation('samenst.', samenstelling).
abbreviation('schr.',   schrijver).
abbreviation('staatssecr.', staatssecretaris).
abbreviation('wijkgem.',wijkgemeente).
abbreviation('zg.',    zogenaamd).
abbreviation('zg.',    zogenaamde).
abbreviation('zgn.',   zogenaamd).
abbreviation('zgn.',   zogenaamde).
abbreviation('z.g.',   zogenaamd).
abbreviation('z.g.',   zogenaamde).
abbreviation(g,        gram).
abbreviation(kb,       kilobyte).
abbreviation(mb,       megabyte).
abbreviation(gb,       gigabyte).
abbreviation('resp.',  respectievelijk).
abbreviation('dB',     decibel).
abbreviation(hz,       hertz).
abbreviation('Hz',     hertz).
abbreviation(kb,       kilobyte).
abbreviation(khz,      kilohertz).
abbreviation(kHz,      kilohertz).
abbreviation(kV,       kilovolt).
abbreviation(kW,       kilowatt).
abbreviation(kWh,      kilowattuur).
abbreviation(mhz,      megahertz).
abbreviation('Mhz',    megahertz).
abbreviation('Mw',     megawatt).
abbreviation('MW',     megawatt).
abbreviation('n.a.g.', genoemd).  % niet afzonderlijk genoemd
abbreviation(nm,       nanometer).
abbreviation('Nm',     nanometer).
abbreviation('no.',    nummer).
abbreviation('ong.',   ongeveer).
abbreviation('prov.',  provincie).
abbreviation(pta,      peseta).
abbreviation(ptn,      punten).
abbreviation('reg.',   regering).
abbreviation('m.',     meter).
abbreviation('min.',   minuut).
abbreviation('min.',   minuten).
abbreviation('min.',   minimum).
abbreviation('tel.',   telefoon).
abbreviation('Tel.',   telefoon).
abbreviation('tel.nr.',   telefoonnummer).
abbreviation('Tel.nr.',   telefoonnummer).
abbreviation('uitg.',  uitgever).   % or uitgeverij
abbreviation('vert.',  vertaling).  % or vertaler or verticaal
abbreviation('vnl.',   voornamelijk).
abbreviation('w.o.',   waaronder).
abbreviation('pseud.', pseudoniem).
abbreviation('oorspr.',oorspronkelijk).
abbreviation('gem.',   gemiddeld).
abbreviation('gem.',   gemiddelde).
abbreviation('gem.',   gemeente).
abbreviation(idd,inderdaad).
abbreviation('vm.',    voormalig).
abbreviation('vm.',    voormalige).
abbreviation('muz.',   muziek).
abbreviation('vg.',    vergelijk).
abbreviation('op.',    opus).
abbreviation('opp.',   oppervlakte).
abbreviation('werkz.', werkzaam).
abbreviation('mv.',    meervoud).
abbreviation('aggl.',  agglomeratie).
abbreviation('temp.',  temperatuur).
abbreviation('max.',   maximaal).
abbreviation('max.',   maximale).
abbreviation('max.',   maximum).
abbreviation('p.',     'pagina\'s').
abbreviation(tzt,      tezijnertijd).
abbreviation('t.z.t.', tezijnertijd).
abbreviation(v,        versus).
abbreviation('vs.',    vers).
abbreviation('vs.',    versus).
abbreviation('verm.',  vermoedelijk).
abbreviation('verm.',  vermoedelijke).
abbreviation('vriendsch.', vriendschappelijk).
abbreviation('vrijg.',  vrijgemaakt).
abbreviation('vrijg.',  vrijgemaakte).
abbreviation('wnd.',   waarnemend).
abbreviation('wnd.',   waarnemende).
abbreviation('zw.',    'zwart-wit').

abbreviation('LA', linksaf).
abbreviation('RA', rechtsaf).


abbreviation('zo.', zondag).

abbreviation('a.h.v.',        [aan,de,hand,van]).
abbreviation('a.h.w.',        [als,het,ware]).
abbreviation('a.i',           [ad,interim]).
abbreviation('a.i.',          [ad,interim]).
abbreviation('c.s.',          [cum,suis]).
abbreviation('d.d.',          [de,dato]).
abbreviation('d.d',           [de,dato]).
abbreviation('dd.',           [de,dato]).
abbreviation('dd',            [de,dato]).
abbreviation('dmv.',          [door,middel,van]).
abbreviation(dmv,             [door,middel,van]).
abbreviation('d.m.v.',        [door,middel,van]).
abbreviation(dwz,             [dat,wil,zeggen]).
abbreviation('dwz.',          [dat,wil,zeggen]).
abbreviation('d.w.z.',        [dat,wil,zeggen]).
abbreviation('i.c.',          [in,casu]).
abbreviation(ihb,             [in,het,bijzonder]).
abbreviation('i.h.b.',        [in,het,bijzonder]).
abbreviation('ihkv',          [in,het,kader,van]).
abbreviation('i.h.k.v.',      [in,het,kader,van]).
abbreviation(ipv,             [in,plaats,van]).
abbreviation('ipv.',          [in,plaats,van]).
abbreviation('i.p.v.',        [in,plaats,van]).
abbreviation(ism,             [in,samenwerking,met]).
abbreviation('i.s.m.',        [in,samenwerking,met]).
abbreviation(itt,             [in,tegenstelling,tot]).
abbreviation('i.t.t.',        [in,tegenstelling,tot]).
abbreviation(ivm,             [in,verband,met]).
abbreviation('i.v.m',         [in,verband,met]).
abbreviation('i.v.m.',        [in,verband,met]).
abbreviation(maw,             [met,andere,woorden]).
abbreviation('m.a.w.',        [met,andere,woorden]).
abbreviation(mbt,             [met,betrekking,tot]).
abbreviation('m.b.t.',        [met,betrekking,tot]).
abbreviation(mbv,             [met,behulp,van]).
abbreviation('m.b.v.',        [met,behulp,van]).
abbreviation('m.i.',          [mijns,inziens]).
abbreviation('miv',           [met,ingang,van]).
abbreviation('m.i.v.',        [met,ingang,van]).
abbreviation(mmv,             [met,medewerking,van]).
abbreviation('m.m.v.',        [met,medewerking,van]).
abbreviation(mn,              [met,name]).
abbreviation('m.n.',          [met,name]).
abbreviation('mn.',           [met,name]).
abbreviation(muv,             [met,uitzondering,van]).
abbreviation('m.u.v.',        [met,uitzondering,van]).
abbreviation(nav,             [naar,aanleiding,van]).
abbreviation('n.a.v.',        [naar,aanleiding,van]).
abbreviation(oa,              [onder,andere]).
abbreviation('oa.',           [onder,andere]).
abbreviation('o.a.',          [onder,andere]).
abbreviation('o.a',           [onder,andere]).
abbreviation(oa,              [onder,andere]).
abbreviation('o.i.',          [ons,inziens]).
abbreviation(obv,             [op,basis,van]).
abbreviation('o.b.v.',        [op,basis,van]).
abbreviation(oiv,             [onder,invloed,van]).
abbreviation('o.i.v.',        [onder,invloed,van]).
abbreviation('o.m.',          [onder,meer]).
abbreviation(olv,             [onder,leiding,van]).
abbreviation('o.l.v.',        [onder,leiding,van]).
abbreviation(tav,             [ten,aanzien,van]).
abbreviation('t.a.v.',        [ten,aanzien,van]).
abbreviation(tbv,             [ten,behoeve,van]).
abbreviation('t.b.v.',        [ten,behoeve,van]).
abbreviation(tgv,             [ten,gevolge,van]).
abbreviation('t.g.v.',        [ten,gevolge,van]).
abbreviation(tgv,             [ten,gunste,van]).
abbreviation('t.g.v.',        [ten,gunste,van]).
abbreviation('t.h.v.',        [ter,hoogte,van]).
abbreviation(thv,             [ter,hoogte,van]).
abbreviation(tlv,             [ten,laste,van]).
abbreviation('t.l.v.',        [ten,laste,van]).
abbreviation('t.l.v',         [ten,laste,van]).
abbreviation(tm,              [tot,en,met]).
abbreviation('t.m.',          [tot,en,met]).
abbreviation('t/m',           [tot,en,met]).
abbreviation('t.e.m.',        [tot,en,met]).
abbreviation(tnv,             [ten,name,van]).
abbreviation('t.n.v.',        [ten,name,van]).
abbreviation(tov,             [ten,opzichte,van]).
abbreviation('t.o.v.',        [ten,opzichte,van]).
abbreviation(tw,              [te,weten]).
abbreviation('v.a.',          [vanaf]).
abbreviation('v.a.',          [vanaf]).
abbreviation('z.i.',          [zijns,inziens]).

abbreviation('d\'rdoor', erdoorheen,              [heen|Ws],Ws).
abbreviation('d\'r',     erdoorheen,              [doorheen|Ws],Ws).
abbreviation('d\'rdoor', erdoorheen,              [heen|Ws],Ws).
abbreviation('d\'r',     ernaartoe,               [naartoe|Ws],Ws).
abbreviation('d\'r',     eromheen,                [omheen|Ws],Ws).
abbreviation('d\'r',     eromheen,                [om,heen|Ws],Ws).
abbreviation('d\'r',     eroverheen,              [overheen|Ws],Ws).
abbreviation('d\'r',     ertussenin,              [tussenin|Ws],Ws).
abbreviation('d\'r',     ertussendoor,            [tussendoor|Ws],Ws).
abbreviation('d\'r',     [er,vlakbij],            [vlakbij|Ws],Ws).
abbreviation('d\'rvoor', [ervoor,in,de,plaats],   [in,de,plaats|Ws],Ws).

abbreviation(ter,[ter,plaatse],[plaatste|Ws],Ws).


productive_fixed_part([W|Ws]) :-
    refl_np(List),
    lists:member([W|Ws],List).

productive_fixed_part(Ws) :-
    geen_snars(Ws).

productive_fixed_part([af,en,aan]).


%% ??
productive_fixed_part([wat,aan]).
productive_fixed_part([wat,af]).
productive_fixed_part([heel,wat,af]).
productive_fixed_part([ze]).

add_new_subcat_frame(_,Frame,Frame,_).
add_new_subcat_frame(on,Frame0,Frame,X) :-
    add_new_subcat_frame(Frame0,Frame,X).

add_new_subcat_frame(intransitive,
		     fixed(Frame,no_passive),
		     _) :-
    refl_np(Frame).
add_new_subcat_frame(part_intransitive(Part),
		     part_fixed(Part,Frame,no_passive),
		     _) :-
    refl_np(Frame).
add_new_subcat_frame(ninv(intransitive,part_intransitive(Part)),
		     ninv(fixed(Frame,no_passive),
			  part_fixed(Part,Frame,no_passive)),
		     _) :-
    refl_np(Frame).
add_new_subcat_frame(intransitive,
		     fixed([er_pp([op,los])],imp_passive),
		     H) :-
    h(H),
    word_forms([op,los]).
add_new_subcat_frame(intransitive,
		     fixed([[bij,elkaar],acc],norm_passive),
		     _) :-
    word_forms([bij,elkaar]).
add_new_subcat_frame(transitive,
		     fixed([er_pp([op,los])],imp_passive),
		     H) :-
    h(H),
    word_forms([op,los]).
add_new_subcat_frame(sbar_transitive,
		     fixed([er_pp([op,los])],imp_passive),
		     H) :-
    h(H),
    word_forms([op,los]).
add_new_subcat_frame(intransitive,
                     fixed([er_pp(tegenop)],imp_passive),
                     H) :-
    h(H),
    word_form(tegenop).

add_new_subcat_frame(transitive,
		     fixed([er_pp(van),GeenSnars],no_passive),
		     H) :-
    h(H),
    geen_snars(GeenSnars).

add_new_subcat_frame(ld_pp,fixed([[af,en,aan]],imp_passive),_) :-
    word_forms([af,en,aan]).

add_new_subcat_frame(intransitive,fixed([[wat,aan]],imp_passive),_) :-
    word_forms([wat,aan]).

add_new_subcat_frame(intransitive,fixed([[wat,af]],imp_passive),_) :-
    word_forms([wat,af]).

add_new_subcat_frame(intransitive,fixed([[heel,wat,af]],imp_passive),_) :-
    word_forms([heel,wat,af]).

%% ze glinsteren je tegemoet
add_new_subcat_frame(intransitive,fixed([[tegemoet],dat],no_passive),_) :-
    word_forms([tegemoet]).

geen_snars([geen,Snars]) :-
    word_form(geen),
    snars(Snars),
    word_form(Snars).
geen_snars([geen,ene,Snars]) :-
    word_form(geen),
    word_form(ene),
    snars(Snars),
    word_form(Snars).

snars(bal).
snars(barst).
snars(biet).
snars(bliksem).
snars(donder).
snars(drol).
snars(flikker).
snars(fluit).
snars(fock).
snars(fuck).
snars(hol).
snars(hout).
snars(klap).
snars(kloot).
snars(klote).
snars(kloten).
snars(lor).
snars(moer).
snars(reet).
snars(ruk).
snars(sikkepit).
snars(snars).
snars(spat).
snars(zak).
snars(zier).

h(hebben).
h('hebben/zijn').

refl_np([Phrase,refl]) :-
    refl_np_phrase(Phrase),
    word_forms(Phrase).

refl_np([acc(weg),refl]) :-
    word_form(weg).

refl_np([svp_pp(uit,lijf),[de,ziel],refl]) :-
    word_forms([de,ziel,uit,lijf]).

refl_np([svp_pp(uit,lijf),[de,benen],refl]) :-
    word_forms([de,benen,uit,lijf]).

refl_np([svp_pp(uit,lijf),[de,longen],refl]) :-
    word_forms([de,longen,uit,lijf]).

refl_np([svp_pp(uit,lijf),[de,benen]]) :-
    word_forms([de,benen,uit,lijf]).

refl_np([svp_pp(uit,lijf),[de,longen]]) :-
    word_forms([de,longen,uit,lijf]).

refl_np_phrase([een,ongeluk]).
refl_np_phrase([de,pestpokken]).
refl_np_phrase([de,pleuris]).
refl_np_phrase([het,pleuris]).
refl_np_phrase([de,tering]).
refl_np_phrase([een,slag,in,de,rondte]).
refl_np_phrase([het,apezuur]).
refl_np_phrase([het,schompes]).
refl_np_phrase([het,leplazarus]).
refl_np_phrase([het,leplazerus]).
refl_np_phrase([het,lazarus]).
refl_np_phrase([het,lazerus]).
refl_np_phrase([het,snot,voor,de,ogen]).
refl_np_phrase([drie,slagen,in,de,rondte]).
refl_np_phrase([rot]).
refl_np_phrase([suf]).
refl_np_phrase([te,barsten]).
refl_np_phrase([te,blubber]).

%% list all forms containing + here too, since + is special in s_fsa :-(
%% xl('+',punct(plus),'+').

xl(Non,_,_,_,_):-
    \+ atom(Non),
    format(user_error,"~w xl not atomic~n",[Non]),
    fail.

%xl('+',preposition('+',[]),'+',L,L).
%xl('Canal',proper_name(sg,'ORG'),'Canal +',['+'|L],L).
%xl('Canal+',proper_name(sg,'ORG'),'Canal+',L,L).

xl(W,fixed_part([W|Rest]),Stem,Ws0,Ws) :-
    hdrug_util:hdrug_flag(expand_subcat,on),
    productive_fixed_part([W|Rest]),
    lists:append(Rest,Ws,Ws0),
    hdrug_util:concat_all([W|Rest],Stem,' ').

xl(Word,Cat,Label,Ws0,Ws) :-
    atom(Word),
    hdrug_util:hdrug_flag(lex_dict,Dict),
    Dict \== undefined,  % otherwise no lex_dict available
    pro_fadd:morph_word(Word,Dict,Label0,AtomCat),
    hdrug_util:atom_term(Label0,Label1),
    hdrug_util:atom_term(AtomCat,Cat1),
    (   Cat1 = '#'(Number)
    ->  next_words(Words,Ws0,Ws,Number,Word,_VariantList),
	hdrug_util:concat_all([Word|Words],Name,' '),
	pro_fadd:morph_word(Name,Dict,Label2,AtomCat2),
	hdrug_util:atom_term(Label2,Label3),
	hdrug_util:atom_term(AtomCat2,Cat)
    ;   Label1=Label3,
	Cat1 = Cat,
	Ws0=Ws
    ),
    simplify_root(Label3,Label).

xl(Word,Cat,Label,Ws0,Ws) :-
    atom(Word),
    hdrug_util:hdrug_flag(lex_dict2,Dict),
    Dict \== undefined,  % otherwise no lex_dict2 available
    pro_fadd:morph_word(Word,Dict,Label0,AtomCat),
    hdrug_util:atom_term(Label0,Label1),
    hdrug_util:atom_term(AtomCat,Cat1),
    (   Cat1 = '#'(Number)
    ->  next_words(Words,Ws0,Ws,Number,Word,_VariantList),
	hdrug_util:concat_all([Word|Words],Name,' '),
	pro_fadd:morph_word(Name,Dict,Label2,AtomCat2),
	hdrug_util:atom_term(Label2,Label3),
	hdrug_util:atom_term(AtomCat2,Cat)
    ;   Label1=Label3,
	Cat1 = Cat,
	Ws0=Ws
    ),
    simplify_root(Label3,Label).

xl(Word,proper_name(sg),Word,Ws,Ws) :-
    atomic(Word),
    qtleap_hide_it_prefix(Prefix),
    atom_concat(Prefix,_,Word).

qtleap_hide_it_prefix('XXXCMDXXX').
qtleap_hide_it_prefix('xxxURLxxx').
qtleap_hide_it_prefix('xxxMAILxxx').
qtleap_hide_it_prefix('xxxURLxxx').
qtleap_hide_it_prefix('xxxUPATH').
qtleap_hide_it_prefix('xxxWPATH').
qtleap_hide_it_prefix('xxxPFILENAME').
qtleap_hide_it_prefix('xxxNExxx').

simplify_root(Root0,Root) :-
    (   var(Root0)
    ->  Root = none
    ;   /* Root0 = v_root(Stem,Inf)
    ->  hdrug_util:hdrug_flag(root_of_verb_uses_inf,Flag),
	(   Flag == on
	->  Root = Inf
	;   Root = Stem
	)
    ;   */ Root0 = Root
    ).

particle_form(Word,Part,Rest) :-
    atom(Word),
    hdrug_util:hdrug_flag(lex_prefix,PrefixDict),
    pro_fadd:morph_word(Word,PrefixDict,Rest,Part).

lex_initialize :-
    hdrug_util:hdrug_flag(lex,Lexicon),
    hdrug_util:hdrug_flag(initialize_lexicon,Initialize),
    (	Lexicon == undefined
    ->	hdrug_util:debug_message(1,
		"lexicon=undefined; not initializing lex.fsa~n",[])
    ;	Lexicon == n
    ->	hdrug_util:debug_message(1,
		"lexicon=undefined; not initializing lex.fsa~n",[])
    ;   Initialize == off
    ->  true
    ;   absolute_file_name(Lexicon,LexiconPath),
	atom_concat(LexiconPath,'.fsa',DictFile),
	pro_fadd:init_morph(DictFile,0,0,0,0,Dict),
	hdrug_util:set_flag(lex_dict,Dict),
	hdrug_util:debug_message(1,"Initialized ~w (~w)~n",
				 [DictFile,Dict]),

	atom_concat(LexiconPath,'_prefix.fsa',PrefixFile),
	pro_fadd:init_morph(PrefixFile,0,0,0,0,PrefixDict),
	hdrug_util:set_flag(lex_prefix,PrefixDict),
	hdrug_util:debug_message(1,"Initialized ~w (~w)~n",
				 [PrefixFile,PrefixDict]),

        atom_concat(LexiconPath,'_inv.fsa',InvFile),
	pro_fadd:init_morph(InvFile,0,0,0,0,InvDict),
	hdrug_util:set_flag(lex_inv,InvDict),
	hdrug_util:debug_message(1,"Initialized ~w (~w)~n",
				 [InvFile,InvDict])


    ),
    hdrug_util:set_flag(initialize_lexicon,off).

lex_initialize2(DictFile2) :-
    pro_fadd:init_morph(DictFile2,0,0,0,0,Dict2),
    hdrug_util:set_flag(lex_dict2,Dict2),
    hdrug_util:debug_message(1,"Initialized ~w (~w)~n",
				 [DictFile2,Dict2]).

initialize_names_dict(No) :-
    hdrug_util:hdrug_flag(initialized_names_dict,Init),
    initialize_names_dict(Init,No).

initialize_names_dict(undefined,No) :-
    !,
    hdrug_util:hdrug_flag(names_dict,File),
    pro_fadd:init_morph(File,0,0,0,0,No),
    hdrug_util:debug_message(1,"initialized names_dict ~w (~w)~n",[File,No]),
    hdrug_util:set_flag(initialized_names_dict,initialized(No)).
initialize_names_dict(initialized(No),No).


initials([H|T]) -->
    initial(H),
    more_initials(T).

initials([H|T]) -->
    title(H),
    more_titles_initials(T).

more_titles_initials([]) --> [].
more_titles_initials([H|T]) -->
    title(H),
    more_titles_initials(T).
more_titles_initials([H|T]) -->
    initial(H),
    more_initials(T).

title('Drs').
title('Drs.').
title('drs').
title('drs.').
title('Dr').
title('Dr.').
title('dr').
title('dr.').
title('mr').
title('mr.').
title('Mr').
title('Mr.').
title('ir').
title('ir.').
title('Ir').
title('Ir.').
title('ing.').
title(ing).
title(prof).
title(jhr).
title('jhr.').
title(mgr).
title('mgr.').
title('Prof').
title('Prof.').
title('prof.').
title('prof.dr').
title('prof.dr.').
title('Prof.dr.').
title(sir).
title('Sir').

n_word(W,L0,L) :-
    next_word(W,L0,L,_).

initial(Atom) -->
    n_word(Atom),
    { initial(Atom) }.

title(Atom) -->
    n_word(Atom),
    { title(Atom) }.

more_initials([]) --> [].
more_initials([H|T]) -->
    initial(H),
    more_initials(T).

initial(Atom) :-
    atom_codes(Atom,Codes),
    is_initial_codes(Codes).

is_initial_codes([Upper,46|Tail]) :-
    isupper(Upper),
    is_initial_codes_more(Tail).

is_initial_codes_more([]).
is_initial_codes_more([Upper,46|Tail]) :-
    isupper(Upper),
    is_initial_codes_more(Tail).
is_initial_codes_more([Upper,Lower,46|Tail]) :-	% Th. Tj. Wm.
    isupper(Upper),
    islower(Lower),
    is_initial_codes_more(Tail).

impossible_non_particle_form_cat(verb(_,_,SC)) :-
    impossible_non_particle_form_cat_sc(SC).

impossible_non_particle_form_cat_sc(part_ld_er_transitive(_)).
impossible_non_particle_form_cat_sc(part_np_ld_er_transitive(_)).

adjective_e(e).
adjective_e(ere).
adjective_e(ste).

non_e_sc(subject_vp_sbar).
non_e_sc(subject_vp_sbar_no_het).
non_e_sc(refl_vp).
non_e_sc(refl_sbar).
non_e_sc(refl_er_pp_vp(van)).

impossible_subcat_infl_combination(adjective(E,Sc)) :-
    adjective_e(E),
    non_e_sc(Sc).

impossible_subcat_infl_combination(verb(_,INFL,SC)) :-
    impossible_subcat_infl_combination(SC,INFL).

impossible_subcat_infl_combination(aci,psp).
impossible_subcat_infl_combination(aci_no_obj,psp).
impossible_subcat_infl_combination(aci_no_obj1,psp).
impossible_subcat_infl_combination(aci_refl_sbar,psp).
impossible_subcat_infl_combination(aci_simple,psp).
impossible_subcat_infl_combination(aux(_),psp).
impossible_subcat_infl_combination(so_aux(_),psp).
impossible_subcat_infl_combination(aux_modifier(inf),psp).
impossible_subcat_infl_combination(aux_psp_hebben,psp).
impossible_subcat_infl_combination(aux_psp_zijn,psp).
impossible_subcat_infl_combination(simple_aux_psp_zijn,psp).
impossible_subcat_infl_combination(obj_control(te),psp).
impossible_subcat_infl_combination(obj_control(te_inf),psp).
impossible_subcat_infl_combination(so_control(te_inf),psp).
impossible_subcat_infl_combination(passive,psp).
impossible_subcat_infl_combination(dat_passive,psp).
%impossible_subcat_infl_combination(norm_passive,psp).  % omdat het woonhuis behouden is gebleven
impossible_subcat_infl_combination(sbar_passive,psp).
impossible_subcat_infl_combination(sbar_subj_te_passive,psp).
impossible_subcat_infl_combination(subj_control(wk_te),psp).
impossible_subcat_infl_combination(subj_control(te),psp).
impossible_subcat_infl_combination(subj_control(te_inf),psp).
impossible_subcat_infl_combination(modifier(aux(inf)),psp).
impossible_subcat_infl_combination(fixed(L,_),psp) :-
    lists:member(vc(_,_,passive),L).
impossible_subcat_infl_combination(fixed(L,_),psp) :-
    lists:member(vc(_,inf,_),L).
impossible_subcat_infl_combination(fixed(L,_),psp) :-
    lists:member(vc(_,te,_),L).
impossible_subcat_infl_combination(fixed(L,_),psp) :-
    lists:member(vc(_,psp,_),L).

%impossible_subcat_infl_combination(ninv(_,_),sg1) :-
%    \+ is_word_form(ik).

impossible_subcat_infl_combination(inverted_aux(inf),inf).
impossible_subcat_infl_combination(inverted_aux(inf),psp).
impossible_subcat_infl_combination(inverted_aux(inf),past(_)).

impossible_subcat_infl_combination(HetSubj,Infl) :-
    check_third_person_sg_sc(HetSubj),
    non_3sg(Infl).

impossible_subcat_infl_combination(HetSubj,Infl) :-
    check_third_person_sc(HetSubj),
    non_3(Infl).

impossible_subcat_infl_combination(fixed_dep(_),Fin) :-
    finite(Fin).
impossible_subcat_infl_combination(part_fixed_dep(_,_),Fin) :-
    finite(Fin).
impossible_subcat_infl_combination(ninv(_,part_fixed_dep(_,_)),Fin) :-
    finite(Fin).

impossible_subcat_infl_combination(er_er,Fin) :-
    singular(Fin).

impossible_subcat_infl_combination(obj_er_er,Fin) :-
    \+ sg2(Fin).


sg2(sg1).
sg2(sg_hebt).
sg2(modal_not_u).
sg2(modal_inv).
sg2(sg_bent).

singular(sg3).
singular(sg1).
singular(past(sg)).
singular(sg_hebt).
singular(sg_heeft).
singular(sg_bent).
singular(sg_is).

non_3sg(sg1).
non_3sg(sg_hebt).
non_3sg(sg_bent).
non_3sg(pl).
non_3sg(past(pl)).
non_3sg(both(pl)).
non_3sg(modal_inv).

non_3(sg1).
non_3(modal_inv).
non_3(sg_hebt).
non_3(sg_bent).

finite(past(_)).
finite(both(_)).
finite(sg).
finite(sg1).
finite(sg3).
finite(sg_heeft).
finite(sg_hebt).
finite(sg_bent).
finite(sg_is).
finite(modal_u).
finite(modal_not_u).
finite(modal_inv).
finite(pl).
finite(subjunctive).

check_third_person_sg_sc(Frame0) :-
    strip_part(Frame0,Frame),
    third_person_sg_sc(Frame).

third_person_sg_sc(ninv(Sc,_)):-
    third_person_sg_sc(Sc).

third_person_sg_sc(het_subj).
third_person_sg_sc(het_subj_sbar_obcomp).
third_person_sg_sc(no_subj).
third_person_sg_sc(sbar_sbar_subj).
third_person_sg_sc(sbar_subj).
third_person_sg_sc(sbar_subj_np).
third_person_sg_sc(sbar_subj_np_np).
third_person_sg_sc(sbar_subj_no_het).
third_person_sg_sc(sbar_subj_so_np).
third_person_sg_sc(sbar_subj_so_pp).
third_person_sg_sc(sbar_subj_so_np_no_het).
third_person_sg_sc(sbar_subj_so_np_opt_het).
third_person_sg_sc(sbar_subj_refl).
third_person_sg_sc(sbar_subj_refl_no_het).
third_person_sg_sc(sbar_subj_refl_opt_het).
third_person_sg_sc(sbar_subj_opt_het).
third_person_sg_sc(sbar_subj_adv_meas).
third_person_sg_sc(sbar_subj_opt_het_meas).
third_person_sg_sc(dip_sbar_subj).
third_person_sg_sc(dip_sbar_subj_no_het).
third_person_sg_sc(dip_sbar_subj_opt_het).
third_person_sg_sc(dip_sbar_subj_so_np_no_het).
third_person_sg_sc(dip_sbar_subj_so_np_opt_het).
third_person_sg_sc(dip_sbar_subj_so_np).
third_person_sg_sc(van_sbar_subj_no_het).
third_person_sg_sc(van_sbar_subj_so_np_no_het).
third_person_sg_sc(ld_adv_sbar_subj_no_het).
third_person_sg_sc(ld_pp_sbar_subj_no_het).
third_person_sg_sc(pp_sbar_subj_no_het(_)).
third_person_sg_sc(pp_sbar_subj_opt_het(_)).
third_person_sg_sc(pp_sbar_subj(_)).
third_person_sg_sc(er_sbar_subj_no_het).
third_person_sg_sc(sbar_subj_te_passive).
third_person_sg_sc(sbar_subj_dat_meas).
third_person_sg_sc(sbar_subj_meas).
third_person_sg_sc(alsof_sbar_subj).
third_person_sg_sc(alsof_sbar_subj_so_np).
third_person_sg_sc(su_ap_pred_sbar).
third_person_sg_sc(su_ap_pred_vp).
third_person_sg_sc(sbar_subj_no_het_tpart).
			    
third_person_sg_sc(pp_vp_subj(_)).
third_person_sg_sc(vp_subj).
third_person_sg_sc(vp_subj_meas).
third_person_sg_sc(vp_subj_dat_meas).
third_person_sg_sc(np_vp_subj).
third_person_sg_sc(vp_subj_so_np).
third_person_sg_sc(vp_subj_so_pp).
third_person_sg_sc(vp_subj_np).
third_person_sg_sc(vp_subj_np_np).
third_person_sg_sc(vp_subj_adv_meas).

third_person_sg_sc(fixed(L,_)) :-
    lists:member(El,L),
    third_person_sg_sc_el(El).

third_person_sg_sc(sbar_passive).

third_person_sg_sc(copula_sbar).
third_person_sg_sc(ap_copula_sbar).
third_person_sg_sc(pp_copula_sbar).
third_person_sg_sc(nonp_copula_sbar).
third_person_sg_sc(so_copula_sbar).
third_person_sg_sc(so_nonp_copula_sbar).

third_person_sg_sc(copula_vp).
third_person_sg_sc(so_copula_vp).
third_person_sg_sc(nonp_copula_vp).
third_person_sg_sc(so_nonp_copula_vp).

third_person_sg_sc_el(sbar_subj_no_het).
third_person_sg_sc_el(sbar_subj_opt_het).
third_person_sg_sc_el(no_subj).
third_person_sg_sc_el(het_subj).
third_person_sg_sc_el(sbar_subj).
third_person_sg_sc_el(naar_sbar_subj).
third_person_sg_sc_el(naar_sbar_subj_no_het).
third_person_sg_sc_el(short_sbar_subj).
third_person_sg_sc_el(vp_subj).
third_person_sg_sc_el(vp_subj_no_het).
third_person_sg_sc_el(subj(sprake)).

third_person_sg_sc_el({List}) :-
    lists:member(El,List),
    third_person_sg_sc_el(El).

check_third_person_sc(Frame0) :-
    strip_part(Frame0,Frame),
    third_person_sc(Frame).

third_person_sc(ninv(Sc,_)):-
    third_person_sc(Sc).

third_person_sc(fixed(L,_)) :-
    lists:member(El,L),
    third_person_sc_el(El).

third_person_sc_el(subj(_)).
third_person_sc_el({List}) :-
    lists:member(El,List),
    third_person_sc_el(El).

:- hdrug_util:initialize_flag(check_word_form,on).


word_form(Word) :-
    hdrug_util:hdrug_flag(parse_or_generate,PG),
    hdrug_util:hdrug_flag(check_word_form,Val),
    word_form(PG,Val,Word).

word_form(parse,Val,Word) :-
    word_form(Val,Word).
word_form(generate,_,_).

word_form(off,_).

word_form(on,Word) :-
    is_word_form(Word),
    !.

word_form(on,W) :-
    melt_part_prep_pair(W,_,W1),
    add_er(W1,W2),
    is_word_form(W2),
    !.

word_form(on,W) :-
    inv_spelling_variant(W,W1),
    is_word_form(W1),
    !.

word_form(on,W) :-
    inv_abbreviation(W,W1),
    is_word_form(W1),
    !.

word_form(on,W) :-
    inv_spelling_variant21(W,W1,W2),
    is_word_form(W1),
    is_word_form(W2),
    !.

word_form(on,W) :-
    inv_spelling_variant31(W,W1,W2,W3),
    is_word_form(W1),
    is_word_form(W2),
    is_word_form(W3),
    !.

word_forms([]).
word_forms([H|T]) :-
    word_form(H),
    word_forms(T).

word_forms([over,de,schreef]) :-
    alpino_lexical_analysis:word_form(over),
    alpino_lexical_analysis:word_form(de),
    alpino_lexical_analysis:word_form(scheef).

% now: list accented/capitalized here explicitly!
is_word_form(één) :-
    alpino_lexical_analysis:word_form(een).
is_word_form(W) :-
    alpino_lexical_analysis:word_form(W).
is_word_form(hem) :-
    alpino_lexical_analysis:word_form('\'m').
is_word_form(hem) :-
    alpino_lexical_analysis:word_form(m).
is_word_form(ik) :-
    alpino_lexical_analysis:word_form(ik).
is_word_form(ik) :-
    alpino_lexical_analysis:word_form('\'k').
is_word_form(koste) :-
    alpino_lexical_analysis:word_form(kostte).

is_word_form('ten onder') :-
    alpino_lexical_analysis:word_form(ten),
    alpino_lexical_analysis:word_form(onder).

is_word_form(alsof) :-
    alpino_lexical_analysis:word_form(of).

inv_lex(Root,Surf) :-
    atom(Root),
    lex_initialize,
    hdrug_util:hdrug_flag(lex_inv,InvDict),
    pro_fadd:morph_word(Root,InvDict,Surf,_).

%% for Bob Wielinga, B.J.Wielinga@uva.nl

:- public infinitive_of_root/2.
infinitive_of_root(Root,Infinitive) :-
    hdrug_util:set_flag(parse_or_generate,generate),
    is_infinitive_of_root(Root,Infinitive).

%% prefer part-V
%% only one solution
is_infinitive_of_root(Root,Infinitive) :- 
    inv_lex(Root,Infinitive),
    lexicon__(Infinitive,verb(_,Inf,_),Root,[],[],'part-V',[]),
    inf(Inf),
    !.
is_infinitive_of_root(Root,Infinitive) :-
    inv_lex(Root,Infinitive),
    lexicon__(Infinitive,verb(_,Inf,_),Root,[],[],normal,[]),
    inf(Inf),
    !.

inf(inf).
inf(inf(no_e)).

sg1(sg1).
sg1(sg).

per_suffix(jr).
per_suffix(sr).
per_suffix('jr.').
per_suffix('sr.').
per_suffix('Jr').
per_suffix('Sr').
per_suffix('Jr.').
per_suffix('Sr.').

loc_suffix(Ws0,Ws) :-
    n_word(centraal,Ws0,Ws).
loc_suffix(Ws0,Ws) :-
    n_word(cs,Ws0,Ws).
loc_suffix(Ws0,Ws) :-
    n_word('CS',Ws0,Ws).
loc_suffix(Ws0,Ws) :-
    n_word(centraal,Ws0,Ws1),
    n_word(station,Ws1,Ws).

concat_stems(List,Stem) :-
    concat_stems(List,Stem,'_').

concat_stems([H|T],Stem,Sep) :-
    concat_stems(T,H,Stem,Sep).

concat_stems([],H,H,_).
concat_stems([H|T],First,Stem,Sep) :-
    concat_stems2(First,H,Next,Sep),
    concat_stems(T,Next,Stem,Sep).

concat_stems2(v_root(Stem0,Lemma0),v_root(Stem1,Lemma1),v_root(Stem,Lemma),Sep) :-
    !,
    hdrug_util:concat_all([Stem0,Stem1],Stem,Sep),
    hdrug_util:concat_all([Lemma0,Lemma1],Lemma,Sep).
concat_stems2(Stem0,v_root(Stem1,Lemma1),v_root(Stem,Lemma),Sep) :-
    !,
    hdrug_util:concat_all([Stem0,Stem1],Stem,Sep),
    hdrug_util:concat_all([Stem0,Lemma1],Lemma,Sep).
concat_stems2(v_root(Stem0,Lemma0),Stem1,v_root(Stem,Lemma),Sep) :-
    !,
    hdrug_util:concat_all([Stem0,Stem1],Stem,Sep),
    hdrug_util:concat_all([Lemma0,Stem1],Lemma,Sep).
concat_stems2(Stem0,Stem1,Stem,Sep) :-
    hdrug_util:concat_all([Stem0,Stem1],Stem,Sep).


un_is_verb_lemma(Lemma,Root) :-
    findall(Root,is_verb_lemma(Lemma,Root),Roots0),
    sort(Roots0,Roots),
    lists:member(Root,Roots),
    !.

un_is_verb_lemma(Lemma,Root) :-
    findall(Root,is_verb_lemma2(Lemma,Root),Roots0),
    sort(Roots0,Roots),
    lists:member(Root,Roots),
    !.

un_is_verb_lemma(L,L).

is_verb_lemma(Lemma,Root):-
    lemma_root(Lemma,Root).

is_verb_lemma2(Lemma,Root) :-
    lexicon(verb(_,Inf,_),v_root(Root,Lemma),[Lemma],[],normal),
    (  Inf = inf  ; Inf = inf(no_e) ).

is_verb_lemma2(Lemma,Root) :-
    lexicon(verb(_,Inf,_),v_root(Root,_),[Lemma],[],'part-V'),
    (  Inf = inf  ; Inf = inf(no_e) ).

quote_variant('`','``').
quote_variant('’','’’').
quote_variant('´','´').

%% exceptions: not for generation
%%             will *not* block longest match

%% europarl: de Commissie juridische zaken
%% treat here, not in dict, otherwise "Europese Commissie" is ambiguous
exception(noun(de,count,sg,app_measure),commissie) --> ['Commissie'].

combine_his(normal,His0,His) :-
    !,
    His0 = His.
combine_his(Variant,His0,variant(Variant,His0)).

replace_space(Part0,Part) :-
    atom(Part0),
    sub_atom(Part0,_,_,_,' '),
    !,
    atom_codes(Part0,Codes0),
    replace_spaces(Codes0,Codes),
    atom_codes(Part,Codes).
replace_space(Part,Part).

replace_spaces([],[]).
replace_spaces([H0|T0],[H|T]) :-
    (   H0 == 32
    ->  H   = 95
    ;   H0  = H
    ),
    replace_spaces(T0,T).

%% as in Build/decl.pl
generate_with_dt_stem(with_dt(_,Dt),Stem) :-
    roots_from_dt(Dt,Roots0,[]),
    sort_not_unique(Roots0,Roots1),
    %% Daniel: Any chance of having duplicates? 
    %% GJ: Would it matter?
    %% GJ: YES. If a sentence contains "was" twice, then lookup
    %% is attempted of "was" once, which is slow (and perhaps could
    %% even succeed...
    hdrug_util:concat_all(Roots1,Stem,' ').

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

loc_prefix('Zuid-','Zuid-').
loc_prefix('Noord-','Noord-').
loc_prefix('Oost-','Oost-').
loc_prefix('West-','West-').
loc_prefix('zuid-','Zuid-').
loc_prefix('noord-','Noord-').
loc_prefix('oost-','Oost-').
loc_prefix('west-','West-').

ind_loc_prefix(zuid).
ind_loc_prefix(noord).
ind_loc_prefix(oost).
ind_loc_prefix(west).

ind_loc_prefix('Zuid').
ind_loc_prefix('Noord').
ind_loc_prefix('Oost').
ind_loc_prefix('West').

ind_loc_prefix(zuidwest).
ind_loc_prefix(noordwest).
ind_loc_prefix(zuidoost).
ind_loc_prefix(noordoost).

ind_loc_prefix('Zuidwest').
ind_loc_prefix('Noordwest').
ind_loc_prefix('Zuidoost').
ind_loc_prefix('Noordoost').

ind_loc_prefix('Zuid-west').
ind_loc_prefix('Noord-west').
ind_loc_prefix('Zuid-oost').
ind_loc_prefix('Noord-oost').

ind_loc_prefix('Zuid-West').
ind_loc_prefix('Noord-West').
ind_loc_prefix('Zuid-Oost').
ind_loc_prefix('Noord-Oost').

not_a_genitive_name('Alex').
not_a_genitive_name('Charles').
not_a_genitive_name('Chris').
not_a_genitive_name('Els').
not_a_genitive_name('Frans').
not_a_genitive_name('Hans').
not_a_genitive_name('Jacques').
not_a_genitive_name('Joris').
not_a_genitive_name('Jos').
not_a_genitive_name('Kees').
not_a_genitive_name('Klaas').
not_a_genitive_name('Kris').
not_a_genitive_name('Louis').
not_a_genitive_name('Lukas').
not_a_genitive_name('Ons').
not_a_genitive_name('Thomas').
not_a_genitive_name('Wiens').
not_a_genitive_name('Yves').

