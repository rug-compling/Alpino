top([sign,mor,rel]).

list_type(h,t).

%% extensional().

%% type(Type,SubTypes,Atts).

type(sign,[],[cat,val,dir,arg,sem,mor,phrase,rphrase,rel,gap]).

type(mor,[],[case,form,agr,det,def]).
	
type(rel,[],[rmor,index]).


:- type_compiler.

catch_print_error(_,V,_):-
   write(V).

