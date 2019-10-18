:- multifile macro/2.
:- multifile rx/2.

:- ensure_loaded(abbrs).

macro(tokenize_before_breaks,
   [ []:sep,? *, []:sep ]
      o
   replace(control(m): [])
      o
   replace(space : sep)
      o
   replace(” : q2_1)
      o
   replace([sq,sq]:q2_2)
      o
   replace({ [komma,komma]:l2_1,
	     ['‘','‘']:l2_2,
             ['`','`']:l2_3,
	     ['«','«']:l2_4,
	     ['„','„']:l2_5,
	     ['“','“']:l2_6
	   })
      o
   replace(['.',sq]:escape_quote,word,word)
      o
   replace({sq:tlq,
	    dq:l2_7,
	    q2_1:l2_8,
	    q2_2:l2_9
	   },sep,`sep)		
      o
   replace({sq:trq,
	    q2_1:r2_1,
	    q2_2:r2_2,
	    dq:r2_3
	   },`sep,sep)
      o
   replace([]:sep,[sep,'-'],'"')           % AD200[345]:  Tbilisi -"Ik ben er trots op dat g...
      o
   replace([]:sep,[word,komma],word)
     o
   replace(word('...'):'...')
     o
   replace([[]:escape_left,sepchar - ';',[]:escape_right],{word,num,'-','+'},{word,num,'-','+'})
      o
   replace([[]:escape_left,'.',[]:escape_right],sep,{word,num})
      o
   replace([[]:escape_left,'/',[]:escape_right],[],'~')
      o
   replace([]:escape_here,'\\',{'[',']'})
      o
   replace([[]:sep,sepchar,[]:sep])
      o  
   replace([escape_here:[],sep:[]],'\\',{'[',']'})   %%%% ensure \[ and \] are not separated!
      o
   replace([escape_left:[],sep:[],sepchar,sep:[],escape_right:[]])
      o
   replace(escape_quote:['.',sq])
      o
   replace(sep+ x sep)
      o
   hellip1
      o
   replace([[]:sep,sluithaak,[]:sep,'-',[]:sep],{[upper,word*],num},[])
      o
   replace([]:sep,word,[openhaak,num])
      o
   replace(sep:[],[openhaak,word+],[sluithaak,sep])
      o
   replace([]:sep,openhaak,num)
      o
   quoted_single_letter
      o
   dot_single_letter
      o
   http
      o
   titels
      o
   afkortingen
      o
   additional_sep
      o
   quote_year
      o
   pagina
      o
   fondsen
     ).

macro(pagina,
      replace([]:sep,[sep,'P',a,g,'.'],num+)).

macro(fondsen,
      replace([:]:[sep,:,sep],
	      [sep,{word(gelijk),word(hoger),word(lager)}],
	      num)).

macro(dot_single_letter,
              replace(sep:[],[sep,word-{m,u,'U'}],['.',sep])
                               o
              replace(sep:[],[sep,['I','J']],['.',sep])
				         % Dimitri van IJ. heeft ..
		               o
              replace([]:sep,[sep,{h,'H'},e,t,sep,'I','J'],['.',sep])
                               o
              replace([]:sep,ampersand_names,['.',sep])
     ).

macro(ampersand_names,{
		       ['B',sep,'&',sep,'W'],
		       ['E',sep,'&',sep,'O'],
		       ['K',sep,'&',sep,'W'],
		       ['S',sep,'&',sep,'D'],
		       ['V',sep,'&',sep,'A'],
		       ['V',sep,'&',sep,'D'],
		       ['A','T',sep,'&',sep,'T'],
		       ['O','C',sep,'&',sep,'W']
		       }).

macro(single_quote,{sq,tlq,backquote}).

macro(quoted_single_letter,
              replace(sep:[],[sep,single_quote],[lower-u,sep])
                                o
              replace(sep:[],[sep,single_quote],[s,{-},upper])  % ' s-G ==> 's-G
                                o
              replace(sep:[],[{sep,single_quote},word-u],[{trq,sq},sep])
                                o
              replace(sep:[],[sep,single_quote],[n,s,sep])      % ' ns ==> 'ns
%                                o
%              replace(sep:[],[sep,{sq,tlq}],[s,l,a,n,d,s,sep]) % ' s lands => 's lands
                                 o    % repair "de letter `h' "
              replace([]:sep,[sep,tlq,word],[sq])
                                 o    % een album als 'Sgt Pepper's' onn..
              replace([]:sep,[word,sq,word],[trq,sep])
      ).

macro(http,
      replace([sep:[],:,sep:[],'/',sep:[],'/',sep:[]],{word(http),word(https)},[?])
                     o
      replace(sep:[],[{word(http),word(https)},(`sep)*],'/')
     ).

macro(titel,file('words_titel.m')).

macro(titels,
      replace([]:sep,[sep,titel,'.'],[])
        o
      replace(sep:[],[sep,titel],'.')
     ).

macro(afkortingen,
      replace([sep:[],u,'.',sep:[]],[sep,a,'.'],[b,'.',sep])
         o
      replace(sep:[],{naam_afkorting,['-',upper]},'.')
         o
      replace(sep:[],[sep,afkorting],'.')
         o %% no. 67 (no is abbr only if following by number
      replace(sep:[],[sep,afkorting_with_number],['.',sep,{num,['I',sep]}])
         o %% 'd.w.z'
      replace(sep:[],['.',word],['.',sep])
         o %% minus exceptions which are listed also in words_afkorting_after
      replace([]:sep,[sep,[e,'.',{a,d,v}]],['.',sep])
         o %% minus exceptions which are listed also in words_afkorting_after
      replace([]:sep,[sep,[j,'.',l]],['.'])
         o %% Hij speelt in de Serie A. Ik niet.
      replace([]:sep,[poule,sep,'A'..'Z'],['.',sep])
         o %% Verwarm voor op 180 ° C. Laat de pastei
      replace([]:sep,['°',sep,'C'],['.',sep])
         o %% '65+'er'
      replace([sep:[],{sq,tlq},sep:[]],'+',word(er))
         o
      replace([[]:sep,-,[]:sep],[sep,chr],{num,[sep,escape(?)]})
         o
      replace([]:sep,[sep,afkorting,'.'],['.'])
     ).

%                      [e,'.',v,'.',a],
%                     [j,'.',l]

%% not det?
macro(chr,[{v,n},'.'^,sep^,{c,'C'},[h,r]^,'.'^]).

macro(poule,file('words_poule.m')).

macro(afkorting_with_number,file('words_afkorting_with_number.m')).

%% no preceding sep required
macro(naam_afkorting,
      words(['Ch',
	     'Ed',
	     'Hub',
             'Im',
	     'Jac','Jacq','Joh',
	     'Nic',
	     'Ph',
	     'Sj',
	     'Th','Tj','Br'
	     ])).

macro(afkorting,
      { [e,t,sep,a,l],
	[?*,{'V',v},o,o,r,z],
	[?*,{'S',s},e,c,r],
	[?*,num,sep,m,i,n],
	[f,e,d],  % only small, otherwise American organisation "Fed"
	file('words_afkortingen.m')
}).

macro(quote_year,
      %% dat was in '78
      replace(sep:[],[sep,{sq,tlq}],[num,num,`{num,word}])
        o
      %% dat was in seizoen '67-'68
      replace(sep:[],[num,num,{'-','/'}],[{sq,tlq},num])
     ).

macro(additional_sep,
      replace([]:sep,word,{['-',i,e]/*,
			   ['/',sep]*/})
     ).

%% . . . ==> ...
macro(hellip1,replace(sep:[],[{sep,'.'},'.'],['.',{'.',sep}])).

