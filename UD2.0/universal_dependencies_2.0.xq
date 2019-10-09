declare namespace saxon="http://saxon.sf.net/";
declare namespace ud="https://universaldependencies.github.io/docs/" ;

declare option saxon:output "omit-xml-declaration=yes";
declare option saxon:output "indent=yes";

declare  variable $xml as xs:string external ;


declare  variable $MODE as xs:string external ;

declare function local:add_pos_tags($node as node()) as node() {
  if ($node[@pt])
  then element {name($node)} {($node/@*, local:universal_pos_tag($node))}
  else element {name($node)} {($node/@*, for $child in $node/node return local:add_pos_tags($child))}
};

declare function local:universal_pos_tag($node as element(node)) as attribute() {
  let $PT := $node/@pt
  let $REL := $node/@rel
  return 
  attribute ud:pos {
         if ($PT eq 'let') then 'PUNCT' 
    else if ($PT eq 'adj') then 'ADJ'
    else if ($PT eq 'bw')  then 'ADV'
    else if ($PT eq 'lid') then 'DET'
    else if ($PT eq 'n')   then
      if ($node/@ntype eq 'eigen') then  'PROPN' else 'NOUN'

    else if ($PT eq 'spec') then
      if ( $node/@spectype eq 'deeleigen') then 'PROPN'
      else if ($node/@spectype eq 'symb')  then  'SYM'  else 'X'  (: afk vreemd afgebr enof meta :)
    else if ($PT eq 'tsw') then 'INTJ'
    else if ($PT eq 'tw')  then 'NUM'
    else if ($PT eq 'vz')  then 'ADP'  (: v2: do not use PART for SVPs and complementizers :)
    else if ($PT eq 'vnw') then
      if ($REL eq 'det') then 'DET'
      else if ($node/@pdtype eq 'adv-pron' ) then 'ADV' else 'PRON'
    else if ($PT eq 'vg') then
      if ($node/@conjtype eq 'neven')  then  'CCONJ'  else 'SCONJ' (: V2: CONJ ==> CCONJ :)
    else if ($PT eq 'ww') then 
      if (local:auxiliary($node) eq 'verb') then  'VERB' else  'AUX' (: v2: cop and auxpass --> AUX  (already in place in v1?) :)
    else  'ERROR_NO_POS_FOUND'
    }
};

declare function local:auxiliary($node as element(node)) as xs:string
{ if ($node[@pt="ww" and @rel="hd" and not(../node[@rel="obj1" or @rel="se"]) and
                 ( contains(@sc,'copula') or
                   contains(@sc,'pred')   or   
                   contains(@sc,'cleft')  or 
                   ../node[@rel="predc"] 
                 ) ] )                                
  then "cop"    
  else if ( $node[@pt="ww" and @rel="hd" and (@lemma="zijn" or @lemma="worden") and 
                  ( @sc="passive"  or 
                    ../node[@rel="su"]/@index = ../node[@rel="vc"]/node[@rel="obj1"]/@index or 
                    not(../node[@rel="su"])  
                  ) ] )           
  then "aux:pass"    
  else if ($node[@pt="ww" and @rel="hd" and
                 ( starts-with(@sc,'aux') or
                   ( ../node[@rel="vc"  and   
                             ( @cat="ppart" or @cat="inf" or @cat="ti" or 
                               ( @cat="conj" and node[@rel="cnj" and (@cat="ppart" or @cat="ti" or @cat="inf")] )
                             )  ]   and  
                    (@lemma="zijn" or @lemma="hebben" or @lemma="zullen" or @lemma="mogen" or 
                        @lemma="hoeven" or @lemma="kunnen" or @lemma="gaan" or @lemma="laten" or @lemma="willen" or @lemma="blijken") 
                   ) ) ]  )                           
  then "aux"    
  else if ($node[@pt="ww"] )
  then "verb"
  else "ERROR_NO_VERB"
};

declare function local:add_features($node as node()) as node() {
  if      ($node[@ud:pos="NOUN" or @ud:pos="PROPN"])
  then    element {name($node)} {($node/@*, local:nominal_features($node))}
  
  else if ($node[@ud:pos="ADJ"]) 
  then    element {name($node)} {($node/@*, local:adjective_features($node))}
  
  else if ($node[@ud:pos="PRON"] )
  then    element {name($node)} {($node/@*, local:pronoun_features($node))}
       
  else if ($node[@ud:pos="VERB" or @ud:pos="AUX"] )
  then    element {name($node)} {($node/@*, local:verbal_features($node))}
       
  else if ($node[@ud:pos="DET"] )
  then    element {name($node)} {($node/@*, local:determiner_features($node))}  
  

  else if ($node[@ud:pos="X"] )
  then    element {name($node)} {($node/@*, local:special_features($node))}  
  
       
  else    element {name($node)} {($node/@*, for $child in $node/node return local:add_features($child))}
};

declare function local:special_features($node as element(node)) as attribute()+ { 
	( attribute ud:Foreign {
		if ($node/@spectype="vreemd")
		then "Yes"
		else "null"
	}
	,
	  attribute ud:Abbr {
	  	if ($node/@spectype="afk")
	  	then "Yes"
	  	else "null"
	  }
    )
};

declare function local:nominal_features($node as element(node)) as attribute()+ {
  let $GENUS := $node/@genus
  let $GETAL := $node/@getal
  let $DEGREE := $node/@graad
  return 
  ( attribute ud:Gender {
    if      ($GENUS eq 'zijd')  then 'Com' 
    else if ($GENUS eq 'onz')   then 'Neut'
    else if ($GENUS eq 'genus') then 'Com,Neut'
    else if ($GENUS)            then 'ERROR_IRREGULAR_GENDER'
    else                             'null'
    }
  ,
  attribute ud:Number {
    if      ($GETAL eq 'ev') then 'Sing' 
    else if ($GETAL eq 'mv') then 'Plur'
    else if ($GETAL)         then 'ERROR_IRREGULAR_NUMBER'
    else                          'null'
    }
  ,
  attribute ud:Degree {
    if      ($DEGREE eq 'dim' )  then 'null'
    else if ($DEGREE eq 'basis') then 'null'
    else if ($DEGREE)            then 'ERROR_IRREGULAR_DEGREE'
    else                              'null'
    }
  )
};

declare function local:adjective_features($node as element(node)) as attribute()+ {
  let $GRAAD := $node/@graad
  return 
  attribute ud:Degree
    {
    if      ($GRAAD eq 'basis') then 'Pos' 
    else if ($GRAAD eq 'comp')  then 'Cmp'
    else if ($GRAAD eq 'sup')   then 'Sup'
    else if ($GRAAD)            then 'ERROR_IRREGULAR_DEGREE'
    else                             'null'
    }
};

declare function local:pronoun_features($node as element(node)) as attribute()+ {
  let $VWTYPE  := $node/@vwtype
  let $PERSOON := $node/@persoon
  let $NAAMVAL := $node/@naamval
  return 
  ( attribute ud:PronType {
    if      ($VWTYPE eq 'pers')  then 'Prs' 
    else if ($VWTYPE eq 'refl')  then 'Prs'
    else if ($VWTYPE eq	'pr')    then 'Prs'
    else if ($VWTYPE eq	'bez')   then 'Prs'
    else if ($VWTYPE eq 'recip') then 'Rcp'
    else if ($VWTYPE eq 'vb')    then 'Int'
    else if ($VWTYPE eq 'aanw')  then 'Dem'
    else if ($VWTYPE eq 'onbep') then 'Ind'
    else if ($VWTYPE eq 'betr')  then 'Rel'
    else if ($VWTYPE)            then 'ERROR_IRREGULAR_PRONTYPE'
    else                              'null'
    }
    ,
    attribute ud:Reflex {
    if ($VWTYPE eq 'refl') 	then 'Yes'
    else 			     'null'
    }
    ,
    attribute ud:Poss {
    if ($VWTYPE eq 'bez') 	then 'Yes'
    else 			     'null'
    }
    ,
    attribute ud:Person {
    if      ($PERSOON eq '1')       then '1'
    else if ($PERSOON eq '2')       then '2'
    else if ($PERSOON eq '2b')      then '2'
    else if ($PERSOON eq '2v')      then '2'
    else if ($PERSOON eq '3')       then '3'
    else if ($PERSOON eq '3o')      then '3'
    else if ($PERSOON eq '3v')      then '3'
    else if ($PERSOON eq '3p')      then '3'
    else if ($PERSOON eq '3m')      then '3'
    else if ($PERSOON eq 'persoon') then 'null'
    else if ($PERSOON)        then 'IERROR_RREGULAR_PERSON'
    else                           'null'
    }
    ,
    attribute ud:Case {
    if    ($NAAMVAL eq 'nomin')  then 'Nom'
    else if ($NAAMVAL eq 'obl')  then 'Acc'
    else if ($NAAMVAL eq 'stan') then 'null'
    else if ($NAAMVAL)           then 'ERROR_IRREGULAR_CASE'
    else                              'null'
    }
  )
};

declare function local:verbal_features($node as element(node)) as attribute()+ {
  let $WVORM  := $node/@wvorm
  let $PVTIJD := $node/@pvtijd
  let $PVAGR  := $node/@pvagr
  return 
  ( attribute ud:VerbForm   {
    if      ($WVORM eq 'pv')  then 'Fin' 
    else if ($WVORM eq 'inf') then 'Inf'
    else if ($WVORM eq 'vd')  then 'Part'
    else if ($WVORM eq 'od')  then 'Part'
    else if ($WVORM)          then 'ERROR_IRREGULAR_VERBFORM'
    else                           'null'
    }
    ,
    attribute ud:Tense {
    if      ($PVTIJD eq 'verl') then 'Past'
    else if ($PVTIJD eq 'tgw')  then 'Pres'
    else if ($PVTIJD eq 'conj') then 'Pres'
    else if ($PVTIJD)           then 'ERROR_IRREGULAR_TENSE'
    else                             'null'
    }
    ,
    attribute ud:Number {
    if      ($PVAGR eq 'ev' )   then 'Sing'
    else if ($PVAGR eq 'met-t') then 'Sing'
    else if ($PVAGR eq 'mv')    then 'Plur'
    else if ($PVAGR)            then 'ERROR_IRREGULAR_NUMBER'
    else                             'null'
    }
  )
};

declare function local:determiner_features($node as element(node)) as attribute()+ {
  let $LWTYPE := $node/@lwtype
  return 
  attribute ud:Definite
    {
    if      ($LWTYPE eq 'bep')   then 'Def' 
    else if ($LWTYPE eq 'onbep') then 'Ind'
    else if ($LWTYPE)            then 'ERROR_IRREGULAR_DEFINITE'
    else                              'null'
    }
};


declare function local:add_dependency_relations($node as node()) as node() {
  if ($node[@pt])
  then element {name($node)} {($node/@*, local:dependency_relation($node))}
  else element {name($node)} {($node/@*, for $child in $node/node return local:add_dependency_relations($child))}
};


declare function local:dependency_relation($node as element(node)) as attribute()+ {
  ( attribute ud:Relation {
    local:dependency_label($node)
    }
    ,
    attribute ud:HeadPosition {
    if ($node[@rel="hd" and @ud:pos="ADP"] ) 
    then  if ($node/../node[@rel= ("obj1","vc","se","me")] )
          then     local:internal_head_position($node/../node[@rel= ("obj1","vc","se","me")])
          else if ($node/../node[@rel="pobj1"] )
               then local:internal_head_position($node/../node[@rel="pobj1"] )
               else if ($node[../@cat="ap" or ../@cat="ppart" or ../@cat="np"])    (: in de eerste rond --> typo in LassySmall/Wiki :)
                    then local:external_head_position($node/..)
                    else "ERROR_NO_HEAD_FOUND"
    
   

    else if ($node[@rel="hd" and  local:auxiliary($node) eq 'aux:pass' and ../node[@rel="vc"]])
    then     local:internal_head_position($node/../node[@rel="vc"])
    
    else if ($node[@rel="hd"] and local:auxiliary($node) eq 'aux' and $node/../node[@rel="vc"])
    then     local:internal_head_position($node/../node[@rel="vc"])
    
    else if ($node[@rel="hd" and local:auxiliary($node) eq 'cop'  and $node/../node[@rel="predc"]])
    then     local:internal_head_position($node/../node[@rel="predc"])
    
    else if ($node[@rel=("obj1","pobj1","se","me") and (../@cat="pp" or ../node[@rel="hd" and @ud:pos="ADP"])])   (: op zich nemen :)
    then     local:external_head_position($node/..)
    
    else if ($node[@rel=("mwp","cnj","dp")])
    then if ( deep-equal($node,$node/../node[@rel=$node/@rel][1]) ) 
         then    local:external_head_position($node/..)
         else    local:internal_head_position($node/..)   
         
    else if ($node[@rel="cmp" and ../node[@rel="body"]])
    then    local:internal_head_position($node/../node[@rel="body"][1])
         
    else if ( $node[@rel="--"] )
         then if ($node[@ud:pos = ("PUNCT","SYM","X","CONJ","NOUN","PROPN","NUM","ADP","ADV","DET","PRON") 
                         and ../node[@rel="--" and 
                                     not(@ud:pos=("PUNCT","SYM","X","CONJ","NOUN","PROPN","NUM","ADP","ADV","DET","PRON")) ] ] ) 
              then    local:internal_head_position($node/../node[@rel="--" and not(@ud:pos=("PUNCT","SYM","X","CONJ","NOUN","ADP","ADV","DET","PROPN","NUM","PRON"))][1])
              else if ( $node/../node[@cat]  ) 
              then    local:internal_head_position($node/../node[@cat][1])
              else if ($node[@ud:pos="PUNCT" and count(../node) > 1]) 
              then if ($node/../node[not(@ud:pos="PUNCT")] )
                   then local:internal_head_position($node/../node[not(@ud:pos="PUNCT")][1])
                   else if ( $node[@begin=../@begin] ) 
                        then local:external_head_position($node/..)
                        else "1" (: ie end of first punct token :)
              else local:external_head_position($node/..)
         
    else if ($node[@rel=("hd","nucl","body") ] ) 
    then    local:external_head_position($node/..)
       
    else if ( $node[@rel="predc"] ) 
         then if   ($node/../node[@rel=("obj1","se")] and $node/../node[@rel="hd"] )
              then local:internal_head_position($node/../node[@rel="hd"])
              else if  ( $node/..[@cat="np" and node[@rel="hd" and (@cat or @word) and not(@ud:pos=("COP","AUX") ) ]  ]  )  (: reduced relatives , make sure head is not empty (ellipsis) :)
                   then local:internal_head_position($node/../node[@rel="hd"])
                   else local:external_head_position($node/..)
             
    else if ($node[@rel="whd" or @rel="rhd"])
    then if ($node[@index]) 
         then local:external_head_position( ($node/../node[@rel="body"]//node[@index = $node/@index ])[1] ) 
         else local:internal_head_position($node/../node[@rel="body"])
    
    else if ( $node/..  ) 
    then    local:internal_head_position($node/..)
         
    else "ERROR_NO_HEAD_FOUND"
    }
  )
} ;

declare function local:internal_head_position($node as element(node)) as xs:string
{ if      ($node[@cat="pp"])
  then    if ($node/node[@rel="hd" and @pt=("bw","n")] )  (: n --> TEMPORARY HACK to fix error where NP is erroneously tagged as PP :)
          then $node/node[@rel="hd"]/@end
          else if ($node/node[@rel="obj1" or @rel="pobj1" or @rel="se"]) 
               then local:internal_head_position($node/node[@rel="obj1" or @rel="pobj1" or @rel="se"][1])
               else local:internal_head_position( $node/node[1] )    
  
  else if ($node[@cat="mwu"] ) 
  then    $node/node[@rel="mwp" and @begin=../@begin]/@end
  
  else if ($node[@cat="conj"])
  then    local:internal_head_position($node/node[@rel="cnj"][1])
  
  else if ( $node/node[@rel="predc"] ) 
        then if   ($node[node[@rel=("obj1","se")] and node[@rel="hd"]])
             then local:internal_head_position($node/node[@rel="hd"])
             else if  ( $node[@cat="np" and node[@rel="hd" and not(@ud:pos=("COP","AUX") ) ]  ]  )  (: reduced relatives :)
             then local:internal_head_position($node/node[@rel="hd"])
             else local:internal_head_position($node/node[@rel="predc"])
  
  else if ($node/node[@rel="vc"] and $node/node[@rel="hd" and @ud:pos="AUX"] )
  then    local:internal_head_position($node/node[@rel="vc"])
   
  else if ( $node/node[@rel="hd"]) 
  then    local:internal_head_position($node/node[@rel="hd"][1])
  
  else if ( $node/node[@rel="body"]) 
  then    local:internal_head_position($node/node[@rel="body"][1])
  
  else if ( $node/node[@cat="du"]) 
  then    local:internal_head_position($node/node[@cat="du"][1])
  
  else if ( $node/node[@rel="dp"]) 
  then    local:internal_head_position($node/node[@rel="dp"][1])
  
  else if ( $node/node[@rel="nucl"]) 
  then    local:internal_head_position($node/node[@rel="nucl"][1])
  
  else if ( $node[@word] )
  then    $node/@end
  
  else if ($node[@index and not(@word or @cat)] )
  then    local:internal_head_position($node/ancestor::*//node[@index = $node/@index and (@word or @cat)] )
  
  
  else    'ERROR_NO_INTERNAL_HEAD'
};

declare function local:external_head_position($node as element(node)) as xs:string
{ if      ( $node[@rel=("hd","body","nucl")] ) 
  then    local:external_head_position($node/..)
  
  else if ($node[@rel="predc"] )
        then if   ($node/../node[@rel=("obj1","se")] and $node/../node[@rel="hd"])
             then local:internal_head_position($node/../node[@rel="hd"])
             else if  ( $node/..[@cat="np" and node[@rel="hd" and (@cat or @word) and not(@ud:pos=("COP","AUX") ) ]  ]  )  (: reduced relatives , check head is not empty :)
             then local:internal_head_position($node/../node[@rel="hd"])
             else local:external_head_position($node/..)


  else if ( $node[@rel=("obj1","pobj1","me") and (../@cat="pp" or ../node[@ud:pos="ADP" and @rel="hd"])] )
  then    local:external_head_position($node/..)
  
  else if ($node[@rel=("cnj","dp")])
       then if ( deep-equal($node, $node/../node[@rel="cnj" or @rel="dp"][1]) )
            then local:external_head_position($node/..)
            else local:internal_head_position($node/../node[@rel=("cnj","dp")][1])
            
  else if ($node[@rel="--" and not(@ud:pos="PUNCT")])
       then   if      ( $node[@cat="mwu"]/../node[@cat and not(@cat="mwu")]  )    (: fix for multiword punctuation in Alpino output :)
              then    local:internal_head_position($node/../node[@cat and not(@cat="mwu")][1])
              else    local:external_head_position($node/..)

  else if ($node[@rel=("dlink","sat")])
       then if ($node/../node[@rel="nucl"])
             then local:internal_head_position($node/../node[@rel="nucl"])
             else "NO_EXTERNAL_HEAD"
   
  else if ($node[@rel="vc"]) 
       then if ($node/../node[@rel="hd" and @ud:pos="AUX"] and not($node/../node[@rel="predc"]) )
            then local:external_head_position($node/..)
            else if ($node/../node[@rel="hd" and @ud:pos="ADP"])
                 then local:external_head_position($node/..)
            else local:internal_head_position($node/..)
            
  else if ($node[@rel="whd" or @rel="rhd"]) 
       then local:external_head_position( $node/../node[@rel="body"]//node[@index = $node/@index] )

  else if ($node[@rel="top"]) then "0"
  
  else if ( $node[not(@rel="hd")] )
  then    local:internal_head_position($node/..)
  
  else    'ERROR_NO_EXTERNAL_HEAD'
} ;


declare function local:dependency_label($node as element(node)) as xs:string
{   if      ($node/..[@cat="top" and @end="1"])     then "root" 
    else if ($node[@rel="app"])                     then "appos"
    else if ($node[@rel="cmp"])                     then "mark"
    else if ($node[@rel="crd"])                     then "cc"
    else if ($node[@rel="me" and not(../node[@ud:pos="ADP"]) ])   then local:determine_nominal_mod_label($node)
    else if ($node[@rel="obcomp"])                  then "advcl"
    else if ($node[@rel="obj2"])                    then "iobj"
    else if ($node[@rel="pobj1"])                   then "expl"
    else if ($node[@rel="predc"]) 
         then if ( not ($node/../node[@rel="obj1" or @rel="se"] or $node/../node[@rel="hd" and not(@ud:pos="AUX")]) ) then local:dependency_label($node/..) else "xcomp"   
         (: hack for now: de keuze is gauw gemaakt :)
         (: was amod, is this more accurate?? :)
         (: examples of secondary predicates under xcomp suggests so :)
    else if ($node[@rel="se"])                      then "obj"
    else if ($node[@rel="su"])                      
        then if (local:auxiliary($node/../node[@rel="hd"]) eq 'aux:pass')
             then if ($node[@cat="whsub" or @cat="ssub"] ) then "csubj:pass" else "nsubj:pass"
        else if ($node[@cat="whsub" or @cat="ssub" or @cat="ti"] )then "csubj" else "nsubj"
    else if ($node[@rel="sup"])                     then "expl"
    else if ($node[@rel="svp"])                     then "compound:prt"  (: v2: added prt extension:)
    else if (local:auxiliary($node) eq 'aux:pass')  then "aux:pass"                                                
    else if (local:auxiliary($node) eq 'aux')       then "aux"    
    else if (local:auxiliary($node) eq 'cop')       then "cop"    
  
    
    else if ( $node[@rel="det"] ) 
         then if ( $node/@ud:pos = ("DET","PROPN","NOUN","ADJ","PRON","ADV")  ) then "det"   (: meer :)(: genoeg :)
         else if ( $node/@cat = ("mwu","np","pp","ap","detp") )                 then "det"
         (: tussen 5 en 6 .., needs more principled solution :)
         (: nog meer mensen dan anders  :)
         (: hetzelfde tijdstip als anders , nogal wat, :)
         else if ( $node/@ud:pos = ("NUM","SYM") )           then "nummod"
         else if ( $node[@cat="conj"]) 
              then if ($node/node[@rel="cnj"][1]/@ud:pos="NUM" )
                   then "nummod"
                   else "det"
      
                                                    else "ERROR_NO_LABEL_DET"
              
    else if ($node[@rel="obj1" or @rel="me"] )
         then if ( $node/../node[@rel="hd" and @ud:pos="ADP"] )
              then local:dependency_label($node/..)  
	      else                                      "obj"
						    
    else if ($node[@rel="mwp"])
         then if ($node[@begin = ../@begin])
              then local:dependency_label($node/..)
(:              else  if ( $node[@spectype="deeleigen"] )   
                                                    then "name"	         
						    else "mwe"
						    :)
			    else if ( $node/../node[@ud:pos="PROPN"]) 
			         then "flat:name"  (: v2: name --> flat:name :)
			         else "fixed"   (: v2 mwe-> fixed :)
						    
    else if ($node[@rel="cnj"])    
         then if   (deep-equal($node,$node/../node[@rel="cnj"][1]))
              then local:dependency_label($node/..)
              else                                       "conj"       
              
     else if ($node[@rel="dp"])    
         then if   (deep-equal($node,$node/../node[@rel="dp"][1]))
              then local:dependency_label($node/..)
              else                                      "parataxis"      
              
    else if ($node[@rel="tag"] )                   then "parataxis"
    
    else if ($node[@rel="sat"] )                   then "parataxis"
    
    else if ($node[@rel="dlink"])                  then "mark"
    
    else if ($node[@rel="nucl"]) 
         then  local:dependency_label($node/..)
    					    					    						   					    
    else if ($node[@rel="vc"] ) 
         then if ($node/../node[@rel="hd" and @ud:pos="AUX"] and not($node/../node[@rel="predc"]) )
              then local:dependency_label($node/..)
              else if ($node/../node[@rel="hd" and @ud:pos="ADP"])
              	   then local:dependency_label($node/..)
              	   else if ($node/node[@rel="su" and @index and not(@word or @cat)] )
                        then "xcomp"                                    
                       	else if ($node/../@cat="np") 
                       	     then "acl"               (: v2: clausal dependents of nouns always acl :)
                       	     else "ccomp"     
    
    else if ($node[@rel=("mod","pc","ld") and ../@cat="np"]) 
         (: modification of nomimal heads :)
         (: pc and ld occur in nominalizations :)
         then if ($node[@cat=("pp","np","conj","mwu") or @ud:pos=("NOUN","PRON","PROPN","X","PUNCT","SYM") ]) then "nmod"
         else if ($node[@ud:pos="ADJ" or @cat="ap"])   then "amod"
         else if ($node[@ud:pos="NUM"])                then "nummod"
         else if ($node[@cat=("rel","whrel")])         then "acl:relcl"  
                (: v2 added relcl -- whrel= met name waar ... :)
         else if ($node[@cat="cp"]/node[@rel="body" and (@ud:pos = ("NOUN","PROPN") or @cat=("np","conj"))] ) then "nmod"   
                (: zijn loopbaan [CP als schrijver] :) 
         else if ($node[@cat=("cp","sv1","smain","ppres","ppart","ti","oti","du") or @ud:pos="SCONJ"])  then "acl" 
                (: oa zinnen tussen haakjes :)
         else if ($node[@ud:pos= ("ADV","ADP","VERB","CCONJ") or @cat="advp"])  then "amod"
               (: VERB= aanstormend etc -> amod, ADV = nagenoeg alle prijzen, slechts 4 euro --> amod :)
               (: CCONJ = opdrachten zoals:   --> amod :)
         else if ($node[@index])     then "ERROR_INDEX_NMOD"
         else                        "ERROR_NO_LABEL_NMOD"
         
    else if ($node[@rel=("mod","pc","ld") and ../@cat=("sv1","smain","ssub","inf","ppres","ppart","oti","ap")]) 
         (: modification of verbal, adjectival heads :)
         (: nb some oti's directly dominate (preceding) modifiers :)
         then if ($node[@cat=("pp","np","conj","mwu") or @ud:pos=("NOUN","PRON","PROPN","X","PUNCT","SYM") ]) then "obl"
         else if ($node[@cat=("cp","sv1","smain","ppres","ppart","ti","oti","du","whq","whrel","rel")])  then "advcl"
         else if ($node[@ud:pos= ("ADJ","ADV","ADP","VERB","SCONJ") or @cat=("advp","ap")])  then "advmod"
         else if ($node[@ud:pos="NUM"])    then "nummod"
         else if ($node[@index])           then "ERROR_INDEX_VMOD"
         else                              "ERROR_NO_LABEL_VMOD"

    else if ($node[@rel="mod" and ../@cat=("pp","detp","advp")])
         then "amod"

    else if ($node[@rel="mod" and ../@cat=("cp")])
         (: net [cp als de Belgische regering]... :)
         then if ($node/../node[@rel="body"]/node[@rel="hd" and @ud:pos="VERB"])
              then "advmod"
              else "amod"
         
    else if ($node[@rel="hdf"])   then "case"
         
    else if ($node[@rel="predm"])
         then if ($node[@ud:pos])                      then "advmod"
         else                                               "advcl"
         
         
    else if ($node[@rel="rhd" and ../node[@rel="body"]//node/@index = @index] ) 
         then local:non_local_dependency_label($node,($node/../node[@rel="body"]//node[@index = $node/@index])[1])
         
    else if ($node[@rel="whd" and ../node[@rel="body"]//node[@index = ancestor-or-self::*//node[@rel="whd"]/@index ] ] ) 
         then local:non_local_dependency_label($node,($node/../node[@rel="body"]//node[@index = ancestor-or-self::*//node[@rel="whd"]/@index])[1])                                                                                        
       
    else if ($node[@rel="body"])
         then local:dependency_label($node/..)
         
    else if ($node[@rel="--"])
         then if ($node[@ud:pos="PUNCT"] )             
              then if ($node[not(../node[not(@ud:pos="PUNCT")]) and @begin=../@begin]) then "root" (:just punctuation :)
              else "punct"   
         else if ($node[@ud:pos="SYM" or @ud:pos="X"] )               
              then if ($node/../node[@cat]) then "appos"  (: 1. Jantje is ziek  1-->appos?? :)
                   else "root"       
         else if ($node[@lemma="\\"] )                 then "punct"  (: hack for tagging errors in lassy small 250 :)
  (:       else if ($node[@spectype="deeleigen"] )       then "punct" :) (: hack for tagging errors in lassy small 250 :)   
         else if ($node[@ud:pos="NUM" and ../node[@cat] ] )  then "parataxis" (: dangling number 1. :)
         else if ($node[@ud:pos="CCONJ" and ../node[@cat="smain" or @cat="conj"]] ) then "cc" 
         (: sentence initial or final 'en' :)
         else if ($node[(@ud:pos="NOUN" or @ud:pos="PROPN" or @ud:pos="VERB") and ../node[@cat="du" or @cat="smain"]] ) then "parataxis" (: dangling words :)
         else if (count($node/../node[not(@ud:pos="PUNCT" or @ud:pos="SYM" or @ud:pos="X")]) < 2 ) then "root" (: only one non-punct/sym/foreign element in the string :)
         else if ($node[@cat="mwu"])
              then if ($node[@begin = ../@begin and @end = ../@end]) 
                   then "root"
                   else if ($node/node[@ud:pos="PUNCT"]) (: fix for mwu punctuation in Alpino output :)
                        then  "punct"
                        else "NO_LABEL_--"
         else if ($node[not(@ud:pos)]/../@rel="top")   then "root"
         else if ($node[@ud:pos="PROPN" and not(../node[@cat]) ] ) then "root"   (: Arthur . :)
         else if ($node[@ud:pos="ADP"] )               then "parataxis"
         else if ($node[@ud:pos="ADV"] )               then "parataxis"
         else if ($node[@ud:pos="DET"] )               then "parataxis"
         else if ($node[@ud:pos="PRON"] )              then "parataxis"

         else "ERROR_NO_LABEL_--"
    
    else if ($node[@rel="hd"])
         then if ($node[@ud:pos="ADP" and not(../node[@rel="pc"]) ] )                then "case"   (: er blijft weinig over van het lijk : over heads a predc and has pc as sister  :)
              else local:dependency_label($node/..)
              
    else    					        "ERROR_NO_LABEL"
};

declare function local:non_local_dependency_label($head as element(node), $gap as element(node)) as xs:string 
{ if      ($gap[@rel="su"])    then "nsubj"
  else if ($gap[@rel="obj1"])  then "obj"
  else if ($gap[@rel="predc"]) then local:dependency_label($gap/..)
  else if ($gap[@rel="pc" or @rel="ld"] )
       then if ($head/node[@rel="obj1"])                       then "nmod"
            else if ($head[@ud:pos="ADV" or @ud:pos="ADP"])    then "advmod"
            else "NO_LABEL_INDEX"
  else if ($gap[@rel="mod"]) 
       then if ($head[@cat="pp"])                    then "nmod"
       else if ($head[@cat="np" or @ud:pos="NOUN"])  then "nmod"
       else if ($head[@ud:pos="PRON"])               then "nmod"
       else if ($head[@ud:pos="ADV" or @cat="advp"]) then "advmod"
       else if ($head[@cat="mwu"])                   then "advmod" (: daar waar :)
            else "ERROR_NO_LABEL_INDEX"
  else if ($gap[@rel="hd"] and $head[@ud:pos="ADP" or @ud:pos="ADV"]) (: waaronder A, B, en C :)
       then "case"
  else if ($gap[@rel="du" or @rel="dp"]) then "parataxis"
  else "ERROR_NO_LABEL_INDEX"
};

declare function local:determine_nominal_mod_label($node as element(node)) as xs:string
{ if ($node/../node[@rel="hd" and (@ud:pos="VERB" or @ud:pos="ADJ")]) 
  then "obl"
  else "nmod"
};

declare function local:determine_adjectival_mod_label($node as element(node)) as xs:string
{ if ($node/../node[@rel="hd" and (@ud:pos="VERB" or @ud:pos="ADJ")]) 
  then "obl"
  else "amod"
};



declare function local:conll-attribute($value as xs:string, $attribute as xs:string) as xs:string {
  if ($value eq 'null') 
  then "" 
  else string-join(($attribute,$value),"=")
};

declare function local:conll($node as element(node)) as xs:string*
{
for $word in $node//node[@word]

let $degree := if ($word/@ud:Degree)
               then local:conll-attribute($word/@ud:Degree,"Degree")
               else ""       
let $case   := if ($word/@ud:Case)
               then local:conll-attribute($word/@ud:Case,"Case")
               else ""
let $gender := if ($word/@ud:Gender)
               then local:conll-attribute($word/@ud:Gender,"Gender")
               else ""
let $person := if ($word/@ud:Person)
               then local:conll-attribute($word/@ud:Person,"Person")
               else ""
let $prontype := if ($word/@ud:PronType)
               then local:conll-attribute($word/@ud:PronType,"PronType")
               else ""
let $number := if ($word/@ud:Number)
               then local:conll-attribute($word/@ud:Number,"Number")
               else ""
let $reflex := if ($word/@ud:Reflex)
               then local:conll-attribute($word/@ud:Reflex,"Reflex")
               else ""
let $poss   := if ($word/@ud:Poss)
               then local:conll-attribute($word/@ud:Poss,"Poss")
               else ""
let $verbform := if ($word/@ud:VerbForm)
               then local:conll-attribute($word/@ud:VerbForm,"VerbForm")
               else ""
let $tense  := if ($word/@ud:Tense)
               then local:conll-attribute($word/@ud:Tense,"Tense")
               else ""
let $definite := if ($word/@ud:Definite)
               then local:conll-attribute($word/@ud:Definite,"Definite")
               else ""
let $foreign := if ($word/@ud:Foreign)
               then local:conll-attribute($word/@ud:Foreign,"Foreign")
               else ""
let $abbr := if ($word/@ud:Abbr)
               then local:conll-attribute($word/@ud:Abbr,"Abbr")
               else ""

              
               
let $features := replace(replace(replace(replace(
                               string-join(($abbr,$case,$definite,$degree,$foreign,$gender,$number,$person,$prontype,$reflex,$tense,$verbform),"|"),
                               "\|+","|"),
                               "^\|$","_"),
                               "^\|",""),
                               "\|$","")

let $quotes := $word/ancestor::node/descendant::node[@word=("'", '"')]/@begin

let $space_after := 
   if ($word/@end = 
   	     $word/ancestor::node/descendant::node[@word= (";",".",":","?","!",",",")","»")]/@begin)
               then "SpaceAfter=No"
               else if ($word/@word = ( "(" , "«" ) ) 
                    then "SpaceAfter=No"
                    else  if ($word/@word = ("'", '"')  and index-of($quotes,$word/@begin) mod 2 = 1 ) 
                          then "SpaceAfter=No"
                          else if ( $word/@end = 
   	                               $word/ancestor::node/descendant::node[@word= ("'",'"') and 
   	                                          index-of($quotes,@begin) mod 2 = 0]/@begin )
                                then "SpaceAfter=No"
                          else "_"


order by number($word/@end)
return 
('&#10;',
string-join(($word/@end, $word/@word , $word/@lemma , $word/@ud:pos, "_", $features, $word/@ud:HeadPosition, $word/@ud:Relation,"_","_"), "	" )
)
}; 


declare function local:sanity_check($node as element(node)) as element(node) {
let $count := count($node//node[@ud:Relation="root"])
let $zeroheadpos := count($node//node[@ud:HeadPosition="0"])
let $headpositionisself := count($node//node[@ud:HeadPosition=@end])
return
 element {name($node)} { ( $node/@*, 
                             attribute {"ud:roots"} {$count}, 
                             attribute {"ud:zeroheadpos"} {$zeroheadpos},
                             attribute {"ud:headpositionisself"} {$headpositionisself},
                             for $child in $node/node return $child ) }
};

for $node in doc($xml)//alpino_ds/node 
  let $uri := tokenize(base-uri($node),'/')[last()]
  return
  if ($MODE eq 'conll') 
  then  <pre>
          <code class="conllu" sentence-id="{$uri}">

            {$node/../sentence,
              local:conll(local:add_dependency_relations(local:add_features(local:add_pos_tags($node))))}
          !
          </code>
        </pre>
  else  <alpino_ds sentence-id="{$uri}">
          { $node/../sentence,
            local:sanity_check(local:add_dependency_relations(local:add_features(local:add_pos_tags($node))))
          }
        </alpino_ds>
  
