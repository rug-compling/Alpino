:- multifile macro/2.
:- multifile rx/2.

:- ensure_loaded(eacl99).

%%% escape [ ] \[ \] tokens with backslash
%%% for Alpino

macro(alp_escape,
              [ []:' ', ? *, []:' ' ]
                       o
            replace({ '[':['\\','['],
		      ']':['\\',']'],
		      ['\\','[']:['\\','\\','['],
		      ['\\',']']:['\\','\\',']']
		    },' ', ' ')
                       o
              [ ' ':[], ? *, ' ':[] ]

		    ).
