% Sample Generation Grammar
% ==============================================================================
% an implementation in ALE of the grammar in Shieber & al, "Semantic-Head-Driven
% Generation", CL 16-1, 1990.
% written for ALE by Octav Popescu

% Signature
% ------------------------------------------------------------------------------

bot sub [pred, list, sem, form, agr, sign].
  pred sub [decl, imp, love, call_up, leave, see, john, mary, mark, friends,
            often, friend, up, you, i].
    decl sub []. imp sub [].
    leave sub []. love sub []. call_up sub []. see sub [].
    john sub []. mary sub []. mark sub [].
    friends sub []. friend sub [].
    often sub []. up sub [].
    you sub []. i sub [].
  list sub [e_list, ne_list, arg_list, subcat_list].
    e_list sub [].
    ne_list sub [arg_ne_list, subcat_ne_list]
            intro [hd:bot, tl:list].
    arg_list sub [e_list, arg_ne_list].
      arg_ne_list sub [] intro [hd:sem, tl:arg_list].
    subcat_list sub [e_list, subcat_ne_list].
      subcat_ne_list sub [] intro [hd:sign, tl:subcat_list].
  sem sub [] intro [pred:pred, args:arg_list].
  form sub [finite, nonfinite].
    finite sub [].
    nonfinite sub [].
  agr sub [sg1, sg2, sg3, pl1, pl2, pl3].
    sg1 sub []. sg2 sub []. sg3 sub []. pl1 sub []. pl2 sub []. pl3 sub [].
  sign sub [sentence, verbal, np, adv, p]
       intro [sem:sem].
    sentence sub [].
    verbal sub [s, vp] intro [form:form].
      s sub [].
      vp sub [] intro [subcat:subcat_list].
    np sub [det, n]
       intro [agr:agr, arg:sem].
      det sub [] intro [np_sem:sem].
      n sub [].
    adv sub [] intro [varg:sem].
    p sub [].

ext([sg1,sg2,sg3,pl1,pl2,pl3]).

% Lexicon
% ------------------------------------------------------------------------------

love --->
  vp, form:nonfinite,
  subcat:[(np,sem:Obj),(np,sem:Subj)],
  sem:(pred:love,args:[Subj,Obj]).

call --->
  vp, form:nonfinite,
  subcat:[(np,sem:Obj),(p,sem:pred:up),(np,sem:Subj)],
  sem:(pred:call_up,args:[Subj,Obj]).

call --->
  vp, form:nonfinite,
  subcat:[(p,sem:pred:up),(np,sem:Obj),(np,sem:Subj)],
  sem:(pred:call_up,args:[Subj,Obj]).

leave --->
  vp, form:nonfinite,
  subcat:[(np,sem:Subj)],
  sem:(pred:leave,args:[Subj]).

see --->
  vp, form:nonfinite,
  subcat:[(np,sem:Obj),(np,sem:Subj)],
  sem:(pred:see,args:[Subj,Obj]).

see --->
  vp, form:nonfinite,
  subcat:[(s,form:finite,sem:Obj),(np,sem:Subj)],
  sem:(pred:see,args:[Subj,Obj]).

john --->
  np, agr:sg3, sem:(pred:john,args:[]).

mary --->
  np, agr:sg3, sem:(pred:mary,args:[]).

mark --->
  np, agr:sg3, sem:(pred:mark,args:[]).

friends --->
  np, agr:pl3, sem:(pred:friends,args:[]).

friend --->
  n, agr:sg3, arg:X, sem:(pred:friend,args:[X]).
 
i --->
  np, agr:sg1, sem:(pred:i,args:[]).

you --->
  np, agr:sg2, sem:(pred:you,args:[]).

often --->
  adv, varg:VP, sem:(pred:often,args:[VP]).

up --->
  p, sem:(pred:up,args:[]).

% Lexical Rules
% ------------------------------------------------------------------------------

sg3 lex_rule (vp, form:nonfinite, subcat:Subcat, sem:Sem) **>
             (vp, form:finite, subcat:NewSubcat, sem:Sem)
  if add_sg3(Subcat,NewSubcat)
  morphs (X,y) becomes (X,i,e,s),
         X becomes (X,s).

non_sg3 lex_rule (vp, form:nonfinite, subcat:Subcat, sem:Sem) **>
                 (vp, form:finite, subcat:NewSubcat, sem:Sem)
  if add_nonsg3(Subcat,NewSubcat)
  morphs X becomes X.

% Grammar Rules
% ------------------------------------------------------------------------------

sentence1 rule
  (sentence,sem:(pred:decl,args:[S])) ===>
  cat> (s,form:finite,sem:S).

sentence2 rule
  (sentence,sem:(pred:imp,args:[S])) ===>
  cat> (vp,form:nonfinite,subcat:[(np,sem:(pred:you,args:[]))],sem:S).

s rule
  (s,form:Form,sem:S) ===>
  cat> Subj,
  sem_head> (vp,form:Form,subcat:[Subj],sem:S).

vp1 rule
  (vp,form:Form,subcat:Subcat,sem:S) ===>
  sem_head> (vp,form:Form,subcat:[Compl|Subcat],sem:S),
  cat> Compl.

vp2 rule
  (vp,form:Form,subcat:[Subj],sem:S) ===>
  cat> (vp,form:Form,subcat:[Subj],sem:VP),
  sem_head> (adv,varg:VP,sem:S).

% Semantics Definition
% ------------------------------------------------------------------------------

semantics sem1.

% Definite Clauses
% ------------------------------------------------------------------------------

sem1(sem:S,sem:S) if true.

add_sg3([(np,sem:Sem)],[(np,agr:sg3,sem:Sem)]) if !, true.
add_sg3([Cat|Cats],[Cat|NewCats]) if add_sg3(Cats,NewCats).

add_nonsg3([(np,sem:Sem)],[(np,agr:(=\=sg3),sem:Sem)]) if !, true.
add_nonsg3([Cat|Cats],[Cat|NewCats]) if add_nonsg3(Cats,NewCats).


