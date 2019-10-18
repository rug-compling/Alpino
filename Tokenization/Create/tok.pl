:- multifile macro/2.
:- multifile rx/2.

%%% tokenizer
%%% input: every paragraph is on a single line
%%% output: every sentence is on a single line; word-tokens are
%%%         separated by a single space


:- ensure_loaded(abbrs).


macro(breaks,

      replace(sep:control_p,[openhaak,sep,{word,num}+],[])

                    o
     
%%% don't break if followed by tlq tldq punctuation followed by .!? or ,
     
      replace(sep:break,
	      [sep,{eos,'...'},[sep,{sq,dq,rq,trq,trdq,tdq,sluithaak}]^],
	      {`{sq,dq,rq,tdq,trq,trdq,komma,eos,';',':',
                 sluithaak,openhaak,lower,'&','-','+',tlq,tldq},
	       [openhaak,sep,`num],
	       '...',
	       [lower,`lower],
               [{tlq,tldq},sep,`{eos,komma}],
               [{tlq,tldq},`sep],
               [{'-'},sep,upper]}
	     )
                    o
      replace(sep:break,
	      [eos,eos,[sep,{sq,dq,rq,trq,trdq,tdq,sluithaak}]^],
	      {`{sq,dq,rq,tdq,trq,trdq,komma,eos,';',':',
                 sluithaak,openhaak,lower,'&','-','+',tlq,tldq,num},
	       [openhaak,sep,`num],
	       [lower,`lower],
               [{tlq,tldq},sep,`{eos,komma}],
               [{tlq,tldq},`sep],
               [{'-'},sep,upper]}
	     )
                    o
       replace(control_p:sep)


     %  ( dat is een feit ) Ik ...  ==>
     %  ( dat is een feit )
     %  Ik

% machine gets too big
%       [sep*, '(',(? - ')')*,')',sep:break,upper,?*]
     
     ).


macro(tok,
      file('tokenize_before_breaks.m')
         o
      breaks
         o
      file('tokenize_after_breaks.m')
     ).

macro(tok_no_breaks,
      file('tokenize_before_breaks.m')
         o
      file('tokenize_after_breaks.m')
      ).

macro(before,file('tokenize_before_breaks.m')).
macro(after,file('tokenize_after_breaks.m')).
