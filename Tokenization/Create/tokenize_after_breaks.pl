:- multifile macro/2.
:- multifile rx/2.

:- ensure_loaded(abbrs).

macro(tokenize_after_breaks,
      {[sep:break, ?*, sep:break ],[sep x [break,break]]}
         o
      initial_dash
         o
      replace(sep:[],[num,u],'.')
         o
      replace(sep:[],[{break,sep},afkorting_after],['.',{break,sep}])
         o
      replace(sep:[],[{break,sep},ambiguous_afkorting_after],['.',sep,{word,num}])
         o
      replace([sep,'.']:['.',sep],[{break,sep},{afkorting_after,ambiguous_afkorting_after}],['.',{break,sep}])
         o
      replace({tlq:sq,
	       trq:sq,
	       l2_1:[komma,komma],
	       l2_2:['‘','‘'],
	       l2_3:['`','`'],
	       l2_4:['«','«'],
	       l2_5:['„','„'],
	       l2_6:['“','“'],
	       l2_7:dq,
	       l2_8: ”,
	       l2_9:[sq,sq],
	       q2_1: ”,
	       q2_2:[sq,sq],
	       r2_1: ”,
	       r2_2:[sq,sq],
	       r2_3:dq
	      })
         o
       hellip
         o
     replace('...':['.','.','.'])
         o
     replace([sep+]:sep)
         o
     [break:[], ?*, break:[]]
     ).

%% -Waarom kom je ? ==> - Waarom kom je ?
macro(initial_dash,
      replace([]:sep,[break,'-'], upper)).

%% . . . ==> ...
macro(hellip,replace(sep:[],[{sep,break,eos},eos],[eos,{eos,sep,break}])).

macro(ambiguous_afkorting_after,
      { file('words_ambiguous_afkorting_after.m'),
      [word+,s,z],   % Pieter Claesz. de Ruiter
      [num]
      }).

macro(afkorting_after,file('words_afkorting_after.m')).

