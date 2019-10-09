:- module(generator,[]).

generate(o(ale(Tag,SVs,Iqs,_),Str,_)) :-
    user:gen(Tag,SVs,Iqs,Str).

