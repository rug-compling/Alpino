:- use_module('../tr_tag').
:- use_module(library(charsio)).

:- repeat, 
   read_line(Chars),
    (   Chars == end_of_file
    ->  true
    ;	catch(charsio:read_from_chars(Chars,X),
	      syntax_error(_,_,_,_,_),
	      (	 format(user_error,"syntax error in ~s~n",[Chars]),
		  format("~s~n",[Chars]), % keep in sync
		  fail
	      )
	     ),
	X=tag(B),
	tr_tag(B,NewB),
	hdrug_util:prettyvars(NewB),
	format("~q~n",[NewB]),
	fail
    ),
    halt.

