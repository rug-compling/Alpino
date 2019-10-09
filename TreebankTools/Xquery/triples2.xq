
(: different from triples.xq:
   1. only triples with HD relation
   2. and also triples between a HD and each of the conjuncts of
      a dependent
:)

import module namespace alpino = "alpino.xq" at "alpino.xq" ;

declare namespace saxon="http://saxon.sf.net/";
declare option saxon:output "omit-xml-declaration=yes";

(: file-id:  given a node from uri file:/users1/.../trees/17.xml
    return trees/17.xml 
:)

declare function local:file-id($constituent as element(node)) as xs:string 
  { let $tok := tokenize(base-uri($constituent),"/")
    let $len := count($tok)
    return string-join(subsequence($tok,$len - 1,2),"/")
  };

declare function local:head-of($constituent as element(node)) as element(node)
  {  let $resolved := alpino:resolve-index($constituent)
     let $hd1 := $resolved[@rel="hd"]
     let $hd2 := $resolved/node[@rel="hd"]

     return
        if ($hd1) then $hd1[1] else 
        if ($hd2) then $hd2[1] else
        <node root="NIL" pos="NIL"></node>
  };      

declare function local:head-root-string($constituent as element(node)) as xs:string 
  {     if ($constituent[@root or @cat="mwu"]) then 
                alpino:root-string($constituent)
        else
            local:head-root-string(local:head-of($constituent))
  };

declare function local:deprel($constituent as element(node)) as xs:string
{       if ($constituent[@rel="cnj"]) then
                  string($constituent/../@rel)
        else
          string($constituent/@rel)
};

declare function local:head-pos-string($constituent as element(node)) as xs:string 
  {     if ($constituent[@pos]) then 
                string($constituent/@pos)
        else if ($constituent[@cat="mwu"]) then "mwu" 
        else
            local:head-pos-string(local:head-of($constituent))
  };


let $triples := for $node in /alpino_ds//node[not(@cat="mwu") and 
              node[@rel="hd"]]
    let $hd := local:head-of($node)
    let $hroot := local:head-root-string($hd)
    let $hpos := local:head-pos-string($hd)
    for $nd in ($node/node[not(node) or node[@rel="hd" or @rel="mwp"]],  $node/node[@cat="conj"]/node[@rel="cnj" and ( not(node) or node[@rel="hd" or @rel="mwp"])])
        let $droot := local:head-root-string($nd)
        let $dpos  := local:head-pos-string($nd) 
        let $hdrel := string($hd/@rel)
        let $deprel := local:deprel($nd)

        where ($nd/@id != $hd/@id and not($droot="NIL" or $dpos="NIL"))
        return concat(string($node/@cat),"|",
                      $dpos,"-",
                      $droot,"|",
                      $hdrel,"/",
                      $deprel,"|",
                      $hpos,"-",
                      $hroot,"|",
                      local:file-id($node))

where (exists($triples))
return concat(string-join(($triples),"&#10;"),"&#10;")






