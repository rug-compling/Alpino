%%
%% Feature templates for maxent models

:- dynamic user:template/2.

template(F,id(Id)) :-
    F:id <=> Id.
