
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
     let $hd1 := $resolved/node[@rel="hd"]
     let $hd2 := $resolved/node[@rel="cmp"]
     let $hd3 := $resolved/node[@rel="crd"]
     let $hd4 := $resolved/node[@rel="dlink"]
     let $hd5 := $resolved/node[@rel="rhd"]
     let $hd6 := $resolved/node[@rel="whd"]
     let $hd7 := $resolved/node[@rel]

     return
        if ($hd1) then $hd1[1] else 
        if ($hd2) then $hd2[1] else
        if ($hd3) then $hd3[1] else
        if ($hd4) then $hd4[1] else
        if ($hd5) then $hd5[1] else
        if ($hd6) then $hd6[1] else
        if ($hd7) then $hd7[1] else
        $resolved
  };      

declare function local:head-root-string($constituent as element(node)) as xs:string 
  {     if ($constituent[@root or @cat="mwu"]) then 
                alpino:root-string($constituent)
        else
            local:head-root-string(local:head-of($constituent))
  };


declare function local:head-pos-string($constituent as element(node)) as xs:string 
  {     if ($constituent[@pos]) then 
                string($constituent/@pos)
        else if ($constituent[@cat="mwu"]) then "mwu" 
        else
            local:head-pos-string(local:head-of($constituent))
  };


let $triples := for $node in collection('corpus')/alpino_ds//node[not(@cat="mwu") and node]
    let $hd := local:head-of($node)
    let $hroot := local:head-root-string($hd)
    let $hpos := local:head-pos-string($hd)
    for $nd in $node/node 
        let $droot := local:head-root-string($nd)
        let $dpos  := local:head-pos-string($nd) 
        where ($nd/@id != $hd/@id)
        return concat(string($node/@cat),"|",
                      $dpos,"-",
                      $droot,"|",
                      string($hd/@rel),"/",
                      string($nd/@rel),"|",
                      $hpos,"-",
                      $hroot,"|",
                      local:file-id($node))

where (exists($triples))
return concat(string-join(($triples),"&#10;"),"&#10;")






