:- multifile macro/2.
:- multifile rx/2.

%%% tokenizer
%%% input: every paragraph is on a single line
%%% output: every sentence is on a single line; word-tokens are
%%%         separated by a single space


:- ensure_loaded(abbrs).

macro(long_breaks,
      replace(sep:break,[sep,{'.','!','?'}],[lower,lower])
     ).


