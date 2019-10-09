
module namespace alpino="alpino.xq" ;

(::::::::::::::::::::: General Functions ::::::::::::::::::::)


(: resolve-index 
 : for index-only nodes return co-indexed non-empty node
 : else return node itself 
 :)

declare function alpino:resolve-index($constituent as element(node)) as element(node)
  {   if ($constituent[@index and not(@pos or @cat)]) then
          $constituent/ancestor::alpino_ds/descendant::node[@index = $constituent/@index and (@pos or @cat)] 
      else $constituent
  };

(: resolve relative pronouns to the head of the NP in which they are contained 
   covers coordinated relative clauses as well 
:)

declare function alpino:resolve-relative-pro($constituent as element(node)) as element(node)
  {   let $head := $constituent/../../node[@rel="hd"] 
      let $conjhead := $constituent/..[@rel="cnj"]/..[@cat="conj"]/node[@rel="hd"]
      return
      if ( $constituent[@rel="rhd"] and $head ) then
         $head
      else if ($conjhead) then
         $conjhead 
      else $constituent
  };
          
(: head-of 
 : identify the lexical head node of a constituent: 
   resolve index nodes, return the head (or similar) daughter, 
   take 1st element of multiple crd daughters (of...of...) 
   if no suitable hd daughter is found, return node itself

   N.B. The head itself is not resolved, so it can be an index node. 
        Could be fixed by resolving, but then original @rel attribute is gone.
        What is preferable?
:)

declare function alpino:head-of($constituent as element(node)) as element(node)
  {  let $resolved := alpino:resolve-index($constituent)
     let $head := $resolved/node[@rel="hd" or @rel="crd" or @rel="cmp" or @rel="dlink" or @rel="rhd" or @rel="whd"]
     return
       if ($head) then
          $head[1]
       else $resolved
  };


(: return yield of a node 
 : get (non-index) leaf nodes, order by begin, concatenate words in the sequence 
 : nb interpunction is not included, as all interpunction is attached at the top-node
 :)

declare function alpino:yield($constituent as element(node)) as xs:string
  { let $words :=  
          for $leaf in $constituent/descendant-or-self::node[@word]
          order by number($leaf/@begin)
          return $leaf/@word
    return string-join($words," ")
  };


(: root-string(node) : 
   for heads: return the head prefixed by root-string of rel=svp & pos=fixed sisters, if any
   for mwu categories: return the concatenation of the roots of the daughters
   otherwise : return the root if defined
   otherwise : return nil
   root-string(node,separator) : as above, but use separator to concatenate roots (instead of ' ')

   NB fix for cases with two svp's
:) 

declare function alpino:root-string ($constituent as element(node)) as xs:string
  { alpino:root-string($constituent,' ')
  };

declare function alpino:root-string ($constituent as element(node), $separator as xs:string) as xs:string
  { if ($constituent[@rel="hd"]/../node[@rel="svp" and (@pos="fixed" or node[@pos="fixed"]) ]) then  
      concat(alpino:root-string($constituent/../node[@rel="svp"][1],$separator),$separator,string($constituent/@root))
    else if ($constituent/@root) then
      string($constituent/@root)
    else if ($constituent[@cat="mwu"] ) then
      string-join($constituent/node/@root,$separator)
    else 'nil'
  };

declare function alpino:word-string ($constituent as element(node)) as xs:string
  { alpino:word-string($constituent,' ')
  };

declare function alpino:word-string ($constituent as element(node), $separator as xs:string) as xs:string
  { if ($constituent[@rel="hd"]/../node[@rel="svp" and (@pos="fixed" or node[@pos="fixed"]) ]) then  
      concat(alpino:word-string($constituent/../node[@rel="svp"][1],$separator),$separator,string($constituent/@word))
    else if ($constituent/@word) then
      string($constituent/@word)
    else if ($constituent[@cat="mwu"] ) then
      string-join($constituent/node/@word,$separator)
    else 'nil'
  };

declare function alpino:head-root-string($constituent as element(node)) as xs:string 
  { alpino:root-string(alpino:head-of($constituent))
  };

(: print a string in a form acceptable to Prolog :)
(: escape (formule)  1_coureur :)

declare function alpino:prolog-atom($string as xs:string) as xs:string
  { let $escaped := replace(replace($string,"\\","\\\\"),"'","\\'")
    return 
    if (matches($escaped,"[ ':-]") or 
        not(matches($escaped,"^[a-z0-9]")) or
        ( matches($escaped, "^[0-9]") and not(matches($escaped, "^[0-9]+(\.)?[0-9]+$")) )
       )
    then concat("'",$escaped,"'")
    else $escaped
  } ;

(: category or terminal node that is nominal
   ie cat=np or pos=noun, pron, or name
      or cat=mwu and a daughter node has pos=noun or name
      or conjunction whose first cnj daughter is nominal 

   NB see Corea/MMAX makemarkables.xq script for a somewhat different definition...
:)


declare function alpino:nominal-node ($constituent as element(node)) as xs:boolean 
  { if ( $constituent[@cat="np" or @pos="noun" or @pos="pron" or @pos="name" 
                 or (@cat="mwu" and node[@pos="noun" or @pos="name"])
                 or (@cat="conj" and alpino:nominal-node(node[@rel="cnj"][1]))
                ]
       ) then 
      fn:true()
    else fn:false()
  };

declare function alpino:nominal-constituent ($constituent as element(node)) as xs:boolean
   { alpino:nominal-node($constituent) and not($constituent[@rel="hd" or @rel="mwp"]) 
   };

(::::::::::::: Information Extraction functions :::::::::::::::)

(: find the lexical head that selects for this constituent or hd
 :  -- either the hd sister 
 :  -- or the hd of the mother (if hd, cnj or crd, or sister of a sc=measure hd)
 :
 :  NB What about appositions? Idem to cnj
 :     Robust if applied to (hd of) top node?
:)

declare function alpino:selector-of ($constituent as element(node)) as element(node)
  { if ( $constituent[@rel="hd" or @rel="cnj" or @rel="crd" or @rel="app"] or
         $constituent[@rel="mod"]/../node[@rel="hd" and @sc="measure"] ) then 
      alpino:selector-of($constituent/..)
    else 
      alpino:head-of($constituent/..)
  };

(: semantic-role: the rel assigned to this constituent or its mother, if hd or app or cnj :)

declare function alpino:semantic-role ($constituent as element(node)) as xs:string
  { if ( $constituent[@rel="hd" or @rel="app" or @rel="cnj" or 
                      (@rel="mod" and ../node[@rel="hd" and @sc="measure"])
                     ] ) then
      string($constituent/../@rel)
    else string($constituent/@rel)
  };

(: is this constituent the name of a PERSON ? :)

declare function alpino:person-node ($constituent as element(node)) as xs:boolean
  { if ($constituent[@neclass="PER" or (@cat = "mwu" and node[@neclass="PER"])] ) then
       fn:true()
    else fn:false()
  };

(: return neclass of an NP containing a name:)
declare function alpino:neclass ($constituent as element(node)) as xs:string
  { if ($constituent[@neclass] ) 
    then string($constituent/@neclass) 
    else if ( $constituent[@cat = "mwu" and node[@neclass]]) 
    then string($constituent/node[1]/@neclass)
    else if ( $constituent/node[@rel="app"] )
    then  alpino:neclass($constituent/node[@rel="app"][1]) 
    else "nil"
  };


declare function alpino:date-node($constituent as element(node)) as xs:boolean
 { if ( $constituent[@special="tmp"] 
        or $constituent[@cat = "mwu" and node[@special="tmp"]]
      ) then 
       true()
    else  if($constituent[@cat = "pp" and node[@rel="obj1"]] ) then
              alpino:date-node($constituent/node[@rel="obj1"])
          else false()
 };

declare function alpino:date-dependents($head as element(node)) as element(node)*
 { for $dep in $head/../node
   where alpino:date-node($dep)
   return $dep
 };


declare function alpino:location-node($constituent as element(node)) as xs:boolean
 { if ( $constituent[@neclass="LOC"] 
        or $constituent[@cat = "mwu" and node[@neclass="LOC"]]
      ) then
           true()
    else if ( $constituent[@cat = "pp" and  node[@rel="obj1"] ] ) then
             alpino:location-node($constituent/node[@rel="obj1"])
         else false()
 };

declare function alpino:location-dependents($head as element(node)) as element(node)*
 { for $dep in $head/../node
   where alpino:location-node($dep)
   return $dep
 };


declare function alpino:die-verb($verb as element(node)) as xs:boolean
  { if ( $verb[@root="ga_dood" or @root="bezwijk" or @root="kom_om" or @root="overlijd" or 
               @root="sterf" or @root="verongeluk" or @root="verdrink" or @root="stik"] 
         or ( $verb[@root="pleeg" and ../node[@rel="obj1"]]
            and alpino:head-of($verb/../node[@rel="obj1"])/@root="zelfmoord"
            )
         or ( $verb[@root="kom" and ../node[@rel="svp"]/node[@root="leven"]] ) 
       ) then
     true()
    else false()
  };

declare function alpino:kill-verb($verb as element(node)) as xs:boolean
  { if ( $verb[@root="vermoord" or @root="schiet_neer" or @root="schiet_dood" or @root="dood" or 
               @root="steek_dood" or @root="sla_dood" or @root="martel_dood"] ) then
       fn:true()
    else fn:false()
  };


(::::::::::::: utils ::::::::::::::::::::)

(: file-id:  given a node from uri file:/users1/.../trees/17.xml return 17.xml :)

declare function alpino:file-id($constituent as element(node)) as xs:string 
  { tokenize(base-uri($constituent),"/")[last()]
  };
