:- multifile macro/2.
:- multifile rx/2.

:- ensure_loaded(eacl99).

macro(eos,    {'!',escape(?),'.',''}).
macro(komma,  ',').
macro(sepchar,{',',openhaak,sluithaak,':',';','*',quotes,'&',eos,
	       '°','/','♥','♡','☹','☺',word('•'),'...',word('…'),word('–'),word('—'),word('ʬ') }).
macro(lower,  {a..z,á,ä,à,é,ë,è,í,ì,ï,ü,ù,ú,ó,ò,ö}).
macro(upper,  {'A'..'Z','Á','À','Ä','É','È','Ë','Í','Ì','Ï','Ó','Ò','Ö',
	      'Ú','Ù','Ü'}).
macro(word,   {upper,lower}).
macro(num,    {'0','1','2','3','4','5','6','7','8','9'}).
macro(quotes, {dq,sq,lq,rq,tlq,tldq,trq,trdq,tdq}).
macro(dq,     '"'). % "	
macro(sq,     '\'').
macro(backquote,'`').
macro(lq,     {'‘',backquote,'«','„','“','‚','',''}).
macro(rq,     {'»','’','”','“','’','',''}).

% quotes are temporarily "bracketed" once we know
% the types. But we keep the original characters
% because the tokenizer is not supposed to alter the input
% beyond white space
macro(tldq,   {l2_1,l2_2,l2_3,l2_4,l2_5,l2_6,l2_7,l2_8,l2_9}).
macro(trdq,   {r2_1,r2_2,r2_3}).
macro(tdq,    {q2_1,q2_2}).

macro(sep,    ' ').
macro(openhaak,{'(','[','{','\\['}).
macro(sluithaak,{')',']','}','\\]'}).
%macro(break,  control(j)).

/*
macro(l2_1,   control(c)).
macro(l2_2,   control(d)).
macro(l2_3,   control(e)).
macro(l2_4,   control(f)).
macro(l2_5,   control(g)).
macro(l2_6,   control(h)).
macro(l2_7,   control(k)).
macro(l2_8,   control(w)).
macro(l2_9,   control(n)).
macro(r2_1,   control(q)).
macro(r2_2,   control(r)).
macro(r2_3,   control(s)).
macro(q2_1,   control(t)).
macro(q2_2,   control(u)).
*/

macro(break, control(j)).

macro(space,{' ',control(i),' ','﻿','​'}).
/*  The third space character is:


        character:   (160, #o240, #xa0)
preferred charset: iso-8859-1 (Latin-1 (ISO/IEC 8859-1))
       code point: 0xA0
           syntax: . 	which means: punctuation
         category: .:Base, b:Arabic, j:Japanese, l:Latin
      buffer code: #xC2 #xA0
        file code: #xC2 #xA0 (encoded by coding system utf-8-unix)
          display: by this font (glyph code)
    xft:-unknown-DejaVu Sans Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1 (#x62)
   hardcoded face: nobreak-space

Character code properties: customize what to show
  name: NO-BREAK SPACE
  old-name: NON-BREAKING SPACE
  general-category: Zs (Separator, Space)
  decomposition: (noBreak 32) (noBreak ' ')

There are text properties here:
  face                 (font-lock-string-face)
  fontified            t
  rear-nonsticky       t
*/

/* the fourth space character is:
        character: ﻿ (65279, #o177377, #xfeff)
preferred charset: unicode (Unicode (ISO10646))
       code point: 0xFEFF
           syntax: w 	which means: word
      buffer code: #xEF #xBB #xBF
        file code: #xEF #xBB #xBF (encoded by coding system utf-8-with-signature-unix)
          display: by this font (glyph code)
    xft:-unknown-DejaVu Sans Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1 (#xBEC)

Character code properties: customize what to show
  name: ZERO WIDTH NO-BREAK SPACE
  old-name: BYTE ORDER MARK
  general-category: Cf (Other, Format)
*/

/* the fifth character

             position: 1624 of 3188 (51%), column: 36
            character: ​ (displayed as ​) (codepoint 8203, #o20013, #x200b)
    preferred charset: unicode (Unicode (ISO10646))
code point in charset: 0x200B
               script: symbol
               syntax:   	which means: whitespace
             to input: type "C-x 8 RET HEX-CODEPOINT" or "C-x 8 RET NAME"
          buffer code: #xE2 #x80 #x8B
            file code: #xE2 #x80 #x8B (encoded by coding system utf-8-unix)
              display: terminal code #xE2 #x80 #x8B

Character code properties: customize what to show
  name: ZERO WIDTH SPACE
  general-category: Cf (Other, Format)
  decomposition: (8203) ('​')
*/


macro(control(Letter), Atom ) :-
    atom_codes(Letter,[C|_]),
    D is C-96,
    atom_codes(Atom,[D]).

