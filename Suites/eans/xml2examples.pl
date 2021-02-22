:- use_module(library(lists)).
:- use_module(library(pillow)).


main :-
    prolog_flag(argv,[File]),
    xml2examples(File),
    halt.

xml2examples(File) :-
    file2terms(File,Terms),
    transform_xml(_,Terms,Examples),
    !,
    format_examples(Examples).

xml2examples(File,Out) :-
    file2terms(File,Terms),
    transform_xml(_,Terms,Examples),
    !,
    tell(Out),
    format_examples(Examples),
    told.

transform_xml(Class,[Env|Rest],[Env1|Rest1]) :-
    trans_xml_term(Class,Env,Env1),
    transform_xml(Class,Rest,Rest1).
transform_xml(_,[],[]).

trans_xml_term(Class,Env,Env1) :-
    xml_trans_rule(Class,Env,Env1),
    !.
trans_xml_term(Class,env(Tag,Att,Content),env(Tag,Att,Content1)) :-
    transform_xml(Class,Content,Content1).
trans_xml_term(_,declare(String),declare(String)).
trans_xml_term(_,[Char|Chars],[Char|Chars]).
trans_xml_term(_,[],[]).

xml_trans_rule(_,env(xlist,_,Xs),xlist(Exs)) :-
    !,
    transform_xml(_,Xs,Exs).

xml_trans_rule(_,env(x,_,[env(xlist,_,Xs)]),Exs) :-
    !,
    transform_xml(_,Xs,Exs).

xml_trans_rule(_,env(x,[url=URL,_,ori=ORI,judgement=_],Example),ignore_ex(ID,Ex)) :-
    append(URL,[47|ORI],ID), % '/' 
    transform_sent(Example,Ex,[]).
xml_trans_rule(_,env(x,[url=URL,_,ori=ORI,object=_],Example),ignore_ex(ID,Ex)) :-
    append(URL,[47|ORI],ID), % '/' 
    transform_sent(Example,Ex,[]).

xml_trans_rule(_,env(x,[url=URL,_,ori=ORI],Example),ex(ID,Ex)) :-
    append(URL,[47|ORI],ID), % '/' 
    transform_sent(Example,Ex,[]).


transform_sent([Word|Rest]) -->
    transform_word(Word),
    transform_sent(Rest).
transform_sent([]) --> [].

transform_word(Chars) -->
    [Chars],
    {plausible_chars(Chars)}.
transform_word(env(ref,_,String)) -->
    transform_sent(String).
transform_word(env(em,_,String)) -->
    String.
transform_word(env(sup,_,String)) -->
    String.
/*
Ik heb <ref>m'n eigen</ref> daar nooit mee bemoeid. ( 
    <label>
    </label>
    in de standaardtaal)
*/
transform_word(env(label,_,_)) -->
    [].

/* apparently script mistakes
ie 05/02/03/02 is "Zoals wij in hoofdstuk 3 zagen..." in the ANS
xlist05.xml:    Zoals <ref>wij</ref> in <a id="rt3" url="03"></a> zagen... 
xlist14.xml:    paragraaf <a id="rt2" url="14/07"></a> 
xlist24.xml-    <ref>Het substantief of zelfstandig naamwoord wordt</ref> behandeld in 
xlist24.xml:    <a id="rt3" url="03"></a>. 
xlist05.xml:      in dezelfde betekenis als <a ref="xt30">11a</a>) 
*/
transform_word(env(a,[_,url=ID],[])) --> [ID].
transform_word(env(a,[_],[ID])) --> [ID].

%% should perhaps be split in 2 examples...
transform_word(env(question,_,Words)) -->
    transform_sent(Words).
transform_word(env(answer,_,Words)) -->
    transform_sent(Words).


%% term is usually put around abbreviations. Convert to upper case?
transform_word(env(term,_,[String])) -->
    {to_upper(String,Upper)},
    [Upper].
transform_word(env(ia,_,Words)) -->
    transform_sent(Words).



format_examples([]).
format_examples(xlist(Exs)) :-
    !,
    format_examples(Exs).
format_examples(ignore_ex(_,_)).
format_examples(ex(ID,Sent)) :-
    !,
    atom_codes(Atom,ID),
    writeq(Atom), write('|'),
    write_sent(Sent),nl.
format_examples([Exs|Exs1]) :-
    !,
    format_examples(Exs),
    !,
    format_examples(Exs1).
format_examples(Other) :-
    !,
    nl, write(error), portray_term(Other), nl.
    
write_sent([]).
write_sent([Word|Words]) :-
    remove_nl(Word,Word1),
    format(" ~s ",[Word1]),
    write_sent(Words).

remove_nl([10|Cs0],Cs) :-
    !,
    remove_nl(Cs0,Cs).
remove_nl([13|Cs0],Cs) :-
    !,
    remove_nl(Cs0,Cs).
remove_nl([124|Cs0],Cs) :-  % also removes |
    !,
    remove_nl(Cs0,Cs).
remove_nl([C|Cs0],[C|Cs]) :-
    remove_nl(Cs0,Cs).
remove_nl([],[]).

to_upper([L|Ls],[U|Us]) :-
    (	97 =< L, L =< 122
    ->	U is L - 32
    ;	U = L
    ),
    to_upper(Ls,Us).
to_upper([],[]).



portray(X) :- portray_chars(X).

%%   by R. O'Keefe
%%   portray_chars(Chars)
%%   checks whether Chars is a non-empty plausible list of character codes.
%%   If it is, it prints the characters out between double quotes.
%%   THIS IS A DEBUGGING AID.  Control characters are written out in ^X
%%   form, rather than using \x.  If the list ends with a variable or
%%   $VAR(_), that is written out as |_X, which will not, of course, be
%%   read back.  That's ok, it's just for looking at.

portray_chars(Chars) :-
    Chars = [_|_],              % a non-empty list
    plausible_chars(Chars),     % of plausible characters
    put_code(0'"),   %"
    portray_chars_(Chars),
    put_code(0'").   %"

portray_chars_(Var) :-
    var(Var),
    !,   
    put_code(0'|),
    write(Var).
portray_chars_([]).
portray_chars_('$VAR'(N)) :-
    put_code(0'|),
    write('$VAR'(N)).
portray_chars_([Char|Chars]) :-
    (   Char =:= 0'"               % "
    ->  put_code(0'\\), put_code(0'")        % " is written \" 
    ;   put_code(Char)
    ),
    portray_chars_(Chars).

plausible_chars(Var) :-
        var(Var), !.
plausible_chars('$VAR'(_)).
plausible_chars([]).
plausible_chars([Char|Chars]) :-
    integer(Char),
    %% removed control char's; they are typically not part of a string
    %% Char >= 32,
    Char =< 255,
    plausible_chars(Chars).

file2terms(FN,Terms) :-
    open(FN,read,Stream),
    getchars(Stream,String,[]),
    close(Stream),
    pillow:xml2terms(String,Terms0),
    remove_white_space(Terms0,Terms).

getchars(Stream,In,Out) :-
    get_code(Stream,Char),
    (   Char = -1
    ->	In = Out
    ;	In = [Char|Tail],
	getchars(Stream,Tail,Out)
    ).

%% in thistle and xml files, all only-whitespace elements can be ignored
remove_white_space([],[]).
remove_white_space([Chars|Terms],Terms1) :-
    white_space(Chars),
    !,
    remove_white_space(Terms,Terms1).
remove_white_space([env(Type,Atts,Content)|Terms],
		   [env(Type,Atts,Content1)|Terms1]) :-
    remove_white_space(Content,Content1),
    !,
    remove_white_space(Terms,Terms1).
remove_white_space([Term|Terms],[Term|Terms1]) :-
    remove_white_space(Terms,Terms1).

white_space([]).
white_space([32|Chars]) :-
    white_space(Chars).
white_space([10|Chars]) :-
    white_space(Chars).
white_space([13|Chars]) :-
    white_space(Chars).



:- main.
