
check_path(e_deps).
check_path(exs).
check_path(iexs).
check_path(slash).
check_path(vslash).
check_path(sc).
check_path(parts).
check_path(cmods).
check_path(mods).
check_path(cpredms).
check_path(predms).
check_path(ccat).
check_path(cdets).
check_path(dets).
check_path(capps).
check_path(dt).
check_path(apps).
check_path(tags).
check_path(vslash).
check_path(imexs).

check_path(mexs).

grammar_checks :-
    (   alpino_lc_in:grammar_rule(A,B,C),
        prettyvars(grammar_rule(A,B,C)),
        (   lists:nth(DNo,C,Daughter)
        ;   DNo=0, Daughter=B
        ),
        check_path(P),
        Daughter:P <=> Term,
        subterm(Term,Subterm),
        Subterm == '$VAR'('_'),
        format(user_error,"daughter ~w of rule ~w has unbound ~w~n",
               [DNo,A,P]),
        fail
    ;   true
    ).


%% simple check
subterm(Term,Term).
%% complex check, many hits that are not "wrong"
%subterm(Term,Sub) :-
%    nonvar(Term),
%    Term =.. [_|List],
%    lists:member(El,List),
%    subterm(El,Sub).
