:- multifile macro/2.
:- multifile rx/2.

:- ensure_loaded('~/z/fsa6/Examples/GerdemannVannoord99/eacl99').

%% repair obvious mistakes in TwNC. Meant to be used XML -> XML
%%
%% in addition, we replace control characters with a meaningful alternative
%% 
macro(entities,
     replace({ word('&amp;#11;'):[],                % ?? trouw20021009
               word('&amp;#26;'):[],                % end of line or something?
               word('&amp;#30;'):[],                % ?? trouw20020823
               word('&amp;#128;'):word('&#8364;'),  % euro
               word('&amp;#129;'):'ü',
               word('&amp;#130;'):'é',
               word('&amp;#131;'):word('&#8364;'),  % euro
               word('&euro;'):word('&#8364;'),  % euro
               word('&amp;#137;'):'ë',
	       word('&amp;#139;'):'ï',              % parool 1999
               word('&amp;#141;'):'í',
	       word('&amp;#142;'):'ä',
	       word('&amp;#143;'):'è',
	       word('&amp;#144;'):'é',
	       word('&amp;#151;'):'-',              % NH1999 
	       word('&amp;#152;'):[],	            % ?? volkskrant20020918
	       word('&amp;#154;'):'Ü',     
	       word('&amp;#155;'):[],	% ?? nrc20010505
	       word('&amp;#157;'):'ù',

               word('&amp;;'):word('&amp;'),  % trouw1999
               
	       word('&amp;#158;'):'û',
	       word('&amp;hearts'):word('&#9829;'),  %% NB: final ; is missing!!
	       word('&amp;#8364;'):word('&#8364;'),

	       %% weird "i ´ " sequence in VK2003*

	       [i,' ','´',' ']:[i],

               '¡':'í',

	       word('&amp;bullet;'):'*',
               
	       %% control char:
	       %% some of these are from CP850 DOS LATIN1
	       %% but not all, and not systematically
	       %% also, in a.o. ac8ujournaal* and hpt* different conventions appear to be in use

               atom(128):word('&#8364;'),  % euro
	       atom(129) : ü,
	       atom(130) : é,
	       atom(131) : word('&#402;'),              %% florin sign
	       atom(132) : ä,   % ? some are different
	       atom(133) : [],  % ?
	       atom(134) : word('&#8225;'),  % dagger
	       atom(135) : ç,
	       atom(136) : ê,
	       atom(137) : ë,
	       atom(138) : è,
	       atom(139) : ï,
	       atom(140) : î,
	       atom(141) : ì,
	       atom(143) : 'Å',
	       atom(144) : 'É',
	       atom(145) : [],	    
	       atom(146) : [],
	       atom(148) : ö,
	       atom(150) : '-',
	       atom(151) : [],
	       atom(152) : '-',
	       atom(153) : 'Ö',
	       atom(154) : word('&#353;'),
	       atom(155) : [],
	       atom(158) : [],
	       atom(159) : word('&#402;') %% florin sign

	     })).

macro(atom(Code),Atom) :-
    atom_codes(Atom,[Code]).
