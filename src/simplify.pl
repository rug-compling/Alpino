:- module(alpino_user_transformation, [ apply_adt_transformations/2 ]).

:- use_module([simplify_words,
	       simplify_passive,
	       simplify_modifier,
	       simplify_split
	      ]).

apply_adt_transformations(Tree0,Tree) :-
    hdrug_util:hdrug_flag(simplify_split,Split),
    hdrug_util:hdrug_flag(simplify_passive,Pass),
    apply_split_transformations(Split,Tree0,Tree1),
    apply_passive_transformations(Pass,Tree1,Tree2),
    apply_further_adt_transformations(Tree2,Tree,Split).

apply_split_transformations(on,Tree0,Tree) :-
    apply_split_transformations(Tree0,Tree).
apply_split_transformations(off,Tree,Tree).
apply_split_transformations(undefined,Tree,Tree).

apply_passive_transformations(on,Tree0,Tree) :-
    apply_passive_transformations(Tree0,Tree).
apply_passive_transformations(off,Tree,Tree).
apply_passive_transformations(undefined,Tree,Tree).

apply_further_adt_transformations(Tree2,Tree,Split) :-
    hdrug_util:hdrug_flag(simplify_modifier,Mod),
    hdrug_util:hdrug_flag(simplify_words,Words),
    apply_further_adt_transformations(Tree2,Tree,Mod,Words,Split).

apply_further_adt_transformations(Tree0,Tree,Mod,Words,Split) :-
    apply_a_transformation(Tree0,Tree1,Mod,Words,Split),
    !,
    apply_further_adt_transformations(Tree1,Tree,Mod,Words,Split).
apply_further_adt_transformations(Tree,Tree,_,_,_).

apply_further_adt_transformations(Tree0,Tree,Mod,Words,Split,Up) :-
    apply_a_transformation(Tree0,Tree1,Up,Mod,Words,Split),
    !,
    apply_further_adt_transformations(Tree1,Tree,Mod,Words,Split,Up).
apply_further_adt_transformations(Tree,Tree,_,_,_,_).

apply_a_transformation(tree(Cat0,Ds0),tree(Cat,Ds),Mod,Words,Split) :-
    adt_transformation_and_flatten(Cat0,Ds0,Cat,Ds,[Cat0/Ds0],Mod,Words,Split).
apply_a_transformation(tree(Cat,Ds0),tree(Cat,Ds),Mod,Words,Split) :-
    apply_a_transformation_list(Ds0,Ds,[Cat/Ds0],Mod,Words,Split).

apply_a_transformation(tree(Cat0,Ds0),tree(Cat,Ds),Up,Mod,Words,Split) :-
    adt_transformation_and_flatten(Cat0,Ds0,Cat,Ds,[Cat0/Ds0|Up],Mod,Words,Split).
apply_a_transformation(tree(Cat,Ds0),tree(Cat,Ds),Up,Mod,Words,Split) :-
    apply_a_transformation_list(Ds0,Ds,[Cat/Ds0|Up],Mod,Words,Split).

apply_a_transformation_list([H0|T],[H|T],Up,Mod,Words,Split) :-
    apply_a_transformation(H0,H,Up,Mod,Words,Split).
apply_a_transformation_list([H|T0],[H|T],Up,Mod,Words,Split) :-
    apply_a_transformation_list(T0,T,Up,Mod,Words,Split).

adt_transformation_and_flatten(r(Rel,Cat0),Ds0,r(Rel,Cat),Ds,Up,Mod,Words,Split) :-
    adt_transformation(r(Rel,Cat0),Ds0,r(Rel,Cat1),Ds1,Up,Mod,Words,Split),
    flatten(Cat1,Ds1,Cat,Ds).

adt_transformation(r(Rel,VAR),A,r(Rel2,i(X,Cat2)),B,Up,Mod,Words,Split) :-
    nonvar(VAR),
    VAR = i(X,Cat),
    adt_transformation(r(Rel,Cat),A,r(Rel2,Cat2),B,Up,Mod,Words,Split).

adt_transformation(Node0,Ds0,Node,Ds,_,_,_,on) :-
    alpino_simplify_split:simple_split_transformation(Node0,Ds0,Node,Ds).

adt_transformation(Node0,Ds0,Node,Ds,Up,Mod,_Words,_) :-
    adt_transformation_mod(Mod,Node0,Ds0,Node,Ds,Up).

adt_transformation(Node0,Ds0,Node,Ds,Up,_Mod,Words,_) :-
    adt_transformation_words(Words,Node0,Ds0,Node,Ds,Up).
    
adt_transformation_mod(on,Node0,Ds0,Node,Ds,Up) :-
    alpino_simplify_modifier:modifier_transformation(Node0,Ds0,Node,Ds,Up).

adt_transformation_words(on,Node0,Ds0,Node,Ds,Up) :-
    alpino_simplify_words:words_transformation(Node0,Ds0,Node,Ds,Up).


flatten(i(Id,Cat0),Ds0,i(Id,Cat),Ds) :-
    !,
    flatten(Cat0,Ds0,Cat,Ds).
flatten(adt_lex(A,B,C,D,E),[],adt_lex(A,B,C,D,E),[]).
flatten(p(_),[tree(r(hd,HdCat),Ds)],HdCat,Ds).
flatten(p(du),[tree(r(dp,Cat),Ds)],Cat,Ds).
flatten(p(VAR),Ds,p(VAR),Ds) :-
    Ds = [_,_|_].

/*

mysteries

ellipsis? "dat" verwijst naar verwijderd werkwoord:
Hoewel de gesprekskosten in veel landen zijn verlaagd , zijn niet alle prijzen dat .
=> Alle prijzen zijn niet dat .

pmi doet het niet:
Het is een sprong in het diepe.
=> Het is een sprong 

      original: Het belooft een mooi 2009 te worden
=> paraphrased: Het belooft een 2009 te worden .

motivatie voor pmi:
boter bij de vis => boter
op eigen benen staan => op benen staan

DONE negatie bij splitsen is fout:
DONE Men had geen enkele richtlijn voorgesteld die voorzag in de bescherming van de consument  =>
DONE Men had geen enkele richtlijn voorgesteld . Ze voorzag in de bescherming van de consument .

weghalen mod bij de .. PROPN:
?daar heeft de bekende X gepredikt -> daar heeft de X gepredikt



DONE ik lust geen brood met kaas =/= ik lust geen brood


DONE split gek:
DONE Het duurt niet lang . Of de politie verhoort hem .
DONE Er gaat hier in het Europees Parlement geen zitting voorbij . Of we zeggen iets .



STYPE van conjunctie van smain/sv1
      original: De koning is dood , troost je de koningin ?
=> paraphrased: De koning is dood , troost je de koningin .



ONDUIDELIJKE/FOUTE PARSES

Volgens de ramingen van de ECB zou zo ongeveer vier vijfde van de werkloosheid *van aard* zijn .


OPEN PROBLEMEN

DONE - generation of topic drop, "Mijnheer de Voorzitter , neem mij niet kwalijk , maar ik heb een opmerking over de Notulen ."
DONE 
DONE - generation of number expressions "De behandeling van een malariapatiënt kost tussen de 10 en 25 dollar"

CONJ MODIFIERS?

Desalniettemin spelen deze ziekten in menselijk en economisch opzicht wel degelijk een rol van betekenis . =>
Desalniettemin spelen deze ziekten in opzicht wel degelijk een rol van betekenis .
?DONE?

SPLIT:

DONE we hebben het hoofd bedekt en de voeten daarbij onbedekt moeten laten

tot slot , de verschillende systemen waarin alternerend geleerd wordt , hebben een ondoorzichtige structuur en zijn niet afgestemd :



TODO GENERATION:


* imperatives with subject are parsed as ynquestion...

* Denkt u zich eens in ...

DONE * Tegelijkertijd dient er een aantal hervormingen te worden doorgevoerd
ld=er doorgevoerd =/= men voert ld=erdoor

* EMBEDDED DIP???

Ik heb geprobeerd tegen hem te zeggen : ik kom niet
yet ok:
Ik heb geprobeerd te zeggen : ik kom niet

* UNKNOWN LEMMA's

- heuristics do not check pos/attributes
"dat wordt gelagerd" -> men X dat

DONE tussen X en Y  : dynamic with_dt, available in alpino_paraphrase:add_lex/3

?

- Er is de Unie veel aan gelegen om efficiënt gebruik te maken van de fondsen voor hulpverlening .


GRAMMAR:

DONE Het is geen geheim dat Van Miert en mezelf een grotere samenhang hadden gewild .

?

het wil er bij mij niet in dat ...
frame: mod_pp(bij)
maar mod is hier verplicht. Echter, er is al een pc (er in)...


%% TODO:
%% de vraag die we ons moeten stellen is wie er komt
%% => wie komt er. Deze moeten we stellen.
%% requires frame for "stellen" requires "vraag", but we only have a pronoun
%% 


DONE ADT:
DONE ten aanzien ervan/daarvan/hiervan get same lemma...



impersonal passive -> men subject.
Dat gaat heel vaak fout, natuurlijk:

Als waarschuwing wordt eerst flink gesist -->
Men sist


Modifiers

als men die uitsluitend vanuit een *continentaal* standpunt bekijkt ?

Pronouns

Net als in 1975 werd gevreesd voor het leven van Sheene , maar ( evenals Middelburg ) kwam hij er met veel wilskracht weer bovenop .
Men vreesde voor het leven . Maar hij kwam er met veel wilskracht bovenop .

Polarity

hij hoefde enkel een schop in de grond te stoppen
Hij hoefde een schop in de grond te stoppen .

Zo Graag

Geen enkel ander boek van Goethe werd zo graag door zijn tijdgenoten gelezen
Graag las men geen enkel ander boek van Goethe .

Contrast verdwijnt

Voor dat doeleinde is de Proton echter nooit ingezet , maar wel voor het lanceren van kunstmanen en ruimtestations .
Men heeft de Proton nooit ingezet . Maar wel voor het lanceren van kunstmanen en ruimtestations .

Split

Wij mogen nu echter niet op onze lauweren rusten en het verdere beleid overlaten aan de Europese Centrale Bank . =>
We mogen niet op onze lauweren rusten . We mogen het beleid overlaten aan de Europese Centrale Bank .



*/