:- multifile macro/2.
:- multifile rx/2.

:- ensure_loaded(abbrs).

%% maps some frequent entities to ordinary characters
macro(entities, replace({ word('&lsquor;'):'é',
			  word('&ldquor;'):'ä',
			  word('&Aacute;'):'Á',
			  word('&Eacute;'):'É',
			  word('&Iacute;'):'Í',
			  word('&Oacute;'):'Ó',
			  word('&Uacute;'):'Ú',
			  word('&aacute;'):'á',
			  word('&eacute;'):'é',
			  word('&iacute;'):'í',
			  word('&oacute;'):'ó',
			  word('&uacute;'):'ú',
			  word('&Agrave;'):'À',
			  word('&Egrave;'):'È',
			  word('&Igrave;'):'Ì',
			  word('&Ograve;'):'Ò',
			  word('&Ugrave;'):'Ù',
			  word('&agrave;'):'à',
			  word('&egrave;'):'è',
			  word('&igrave;'):'ì',
			  word('&ograve;'):'ò',
			  word('&ugrave;'):'ù',
			  word('&Auml;'):'Ä',
			  word('&Euml;'):'Ë',
			  word('&Iuml;'):'Ï',
			  word('&Ouml;'):'Ö',
			  word('&Uuml;'):'Ü',
			  word('&auml;'):'ä',
			  word('&euml;'):'ë',
			  word('&iuml;'):'ï',
			  word('&ouml;'):'ö',
			  word('&uuml;'):'ü',
			  word('&circ;'):'ê',
			  word('&Icirc;'):'Î',
			  word('&caron;'):[],	%  ??
			  word('&gcirc;'):word(ge), %  ??
			  word('&zcaron;'):word('ž')
%			  word('–'):word('--'),
%			  word('—'):word('--'),
%			  word('ʬ'):'°',
%		  weird_space:' ',   
%			  word('•'):'*',
%			  word('…'):'...',
%			  word('⊘'):'ø'
			})

%     o   replace( {[i,¨]:ï,     % mostly for volkskrant2004
 %                  [i,´]:í      % idem
 %                 },{a..z,'A'..'Z'},{a..z,'A'..'Z'})
     ).

%macro(weird_space,Atom) :-
%    atom_codes(Atom,[160]).

