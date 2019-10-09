###############################################################################
#
# trees -- drawing Trees in CLIG
# last update 22-Sep-95
# author : karsten konrad, konrad@coli-gate.uni-sb.de
#
###############################################################################
#
# 19-Oct-95: done major rewriting of tree drawing code. Tree mothers now get
#            centered above their immediate doughters.
#
# 27-Oct-95: redone horizontal trees
#
# 10-Oct-95: removed "large mother" bug. Doughters get moved when the 
#            mother object is too large to fit in the tree area.

#
# Trees have to be drawn first to get their size - then
# they can be positioned as usual.
#

#
# new tree implementation
#

# gravit moves positions slightly to adjust links between mother and child
# (sometimes a mother is directly over a child - the line should be straight)

proc gravit {x y area} { ;# relative position of x to y
        set diff [expr $y-$x]
        if {($diff>-$area)&&($diff<$area)} {return [expr $x+$diff]} \
                 else {return $x}}

#
# new tree implementation (v 3.0)
#

proc tree {x y where mtags motherchild} { ;# draws a tree
        global clig_globals main_color
        
        set mother [lindex $motherchild 0]
        set kids [lreplace $motherchild 0 0]
                
        set xsize 0; 
        set ysize 0; 
        set minchild -1
        
        set sizes {}  ;# sizes
        set relative {} ;# relative positions of children
        
        set xexpand $clig_globals(tree_hspace)  
        ;#  from menu: space between doughters
        set topoffset $clig_globals(tree_vspace)
        ;# space between mother and child

        set mothersize [calcsize $mother]
        
        # precalculating sizes

        foreach item $kids {
                set size [calcsize $item]
                lappend relative $xsize ;# xposition of child
                set ysize [max $ysize [lindex $size 1]]
                set xsize [expr $xsize+[lindex $size 0]+$xexpand]
                lappend sizes $size} ;# storing dims of child
                          
        ;# xsize is size of doughters
        set nkids [llength $kids] ;# number of kids
        set xsize [expr $xsize-$xexpand]

        ;# center of tree is dependent on children

        ;# center kid 1
        if {[llength [lindex $sizes 0]]==4} {
                        set ckid1 [lindex [lindex $sizes 0] 2]} \
        else {set ckid1 [expr [lindex [lindex $sizes 0] 0]/2]}
     
        ;# center last kid (= center+relative position)
        if {[llength [lindex $sizes [expr $nkids-1]]]==4} {
                        set ckid2 [expr [lindex [lindex $sizes \
                                      [expr $nkids-1]] 2]+ \
                                      [lindex $relative [expr $nkids-1]]]} \
        else {set ckid2 [expr [lindex \
                                [lindex $sizes [expr $nkids-1]] 0]/2+ \
                              [lindex $relative [expr $nkids-1]]]}

        ;# center position of mother relative to doughters
        set cmother [get-center $ckid1   \
                        [expr $ckid2-$ckid1] [lindex $mothersize 0]]

        ;# large mother correction (shifting tree)
        set large_mother [max [expr 0-$cmother] 0]
        
        set linkx [expr $x+$large_mother+$cmother+[lindex $mothersize 0]/2]

        if {$where!=".invisible"} {        

           execobj $mother [expr $x+$cmother+$large_mother] $y $where $mtags

           set linky [expr $y+[lindex $mothersize 1]+1]

           set index 0
       
           ;# this is the x/y position of the first child
                 
                set xcur [expr $x+$large_mother]
                set ycur [expr $y+[lindex $mothersize 1]+$topoffset]

                foreach item $kids { ;# catch the child and draw
                  set width [lindex [lindex $sizes $index] 0]
                  set height [lindex [lindex $sizes $index] 1]
                  set childsize [execobj $item $xcur $ycur $where $mtags]
                  if {[llength $childsize]==4} {
                        set centerkid [lindex $childsize 2]} \
                  else {set centerkid [expr $xcur+[lindex $childsize 0]/2]}

                  ;# correction of gravitation area (3 Points)
                  set centerkid [gravit $centerkid $linkx 3]
        
                  $where create line $centerkid $ycur \
                        $linkx $linky -tags $mtags -fill $main_color
                  ;# draw line to mother
                   
                  set xcur [expr $xcur+$width+$xexpand] ;# inc x position
                  set index [expr $index+1]}}

        return [list [expr [max [expr $cmother+[lindex $mothersize 0]] \
                     $xsize]+$large_mother] \
               [expr $ysize+[lindex $mothersize 1]+$topoffset] \
               $linkx [expr [lindex $mothersize 1]/2]]}



proc htree {x y where mtags motherchild} { ;# draws a horizontal tree
        global clig_globals main_color
        
        set mother [lindex $motherchild 0]
        set kids [lreplace $motherchild 0 0]
                
        set xsize 0; 
        set ysize 0; 
        set minchild -1
        
        set sizes {}  ;# sizes
        set relative {} ;# relative positions of children
        
        set yexpand $clig_globals(tree_hspace)  
        ;#  from menu: space between doughters
        set topoffset $clig_globals(tree_vspace)
        ;# space between mother and child

        set mothersize [calcsize $mother]
        
        # precalculating sizes

        foreach item $kids {
                set size [calcsize $item]
                lappend relative $ysize ;# xposition of child
                set xsize [max $xsize [lindex $size 0]]
                set ysize [expr $ysize+[lindex $size 1]+$yexpand]
                lappend sizes $size} ;# storing dims of child
                          
        ;# xsize is size of doughters
        set nkids [llength $kids] ;# number of kids
        set ysize [expr $ysize-$yexpand]

        ;# center of tree is dependent on children
       
        ;# center kid 1
        if {[llength [lindex $sizes 0]]==4} {
                        set ckid1 [lindex [lindex $sizes 0] 3]} \
        else {set ckid1 [expr [lindex [lindex $sizes 0] 1]/2]}
     
        ;# center last kid (= center+relative position)
        if {[llength [lindex $sizes [expr $nkids-1]]]==4} {
                        set ckid2 [expr [lindex [lindex $sizes \
                                      [expr $nkids-1]] 3]+ \
                                      [lindex $relative [expr $nkids-1]]]} \
        else {set ckid2 [expr [lindex \
                                [lindex $sizes [expr $nkids-1]] 1]/2+ \
                              [lindex $relative [expr $nkids-1]]]}

        ;# center position of mother relative to doughters
        set cmother [get-center $ckid1   \
                        [expr $ckid2-$ckid1] [lindex $mothersize 1]]

        ;# large mother correction (shifting tree)
        set large_mother [max [expr 0-$cmother] 0]
        
        set linky [expr $y+$large_mother+$cmother+[lindex $mothersize 1]/2]

        if {$where!=".invisible"} {        

           execobj $mother $x [expr $y+$cmother+$large_mother] $where $mtags

           set linkx [expr $x+[lindex $mothersize 0]+1]

           set index 0
       
           ;# this is the x/y position of the first child
                 
                set ycur [expr $y+$large_mother]
                set xcur [expr $x+[lindex $mothersize 0]+$topoffset]

                foreach item $kids { ;# catch the child and draw
                  set width [lindex [lindex $sizes $index] 0]
                  set height [lindex [lindex $sizes $index] 1]
                  set childsize [execobj $item $xcur $ycur $where $mtags]
                  if {[llength $childsize]==4} {
                        set centerkid [lindex $childsize 3]} \
                  else {set centerkid [expr $ycur+[lindex $childsize 1]/2]}

                  ;# correction of gravitation area (3 Points)
                  set centerkid [gravit $centerkid $linky 3]
        
                  $where create line $xcur $centerkid \
                        $linkx $linky -tags $mtags -fill $main_color
                  ;# draw line to mother
                   
                  set ycur [expr $ycur+$height+$yexpand] ;# inc y position
                  set index [expr $index+1]}}

        return  [list [expr $xsize+[lindex $mothersize 0]+$topoffset] \
                  [expr [max [expr $cmother+[lindex $mothersize 1]] \
                      $ysize]+$large_mother] \
                  [expr [lindex $mothersize 0]/2] $linky]}


#
# suppressed trees (object with triangle above it)  
#

proc suptree {x y where mtags supobj} { ;# draws a supressed tree
        global clig_globals main_color
        set supobj [lindex $supobj 0] ;# bracket parameter convention
        set size [calcsize $supobj]
        set width [max [lindex $size 0] 20]
        set height [lindex $size 1]
        set trioffset 2
        set triheight $clig_globals(tree_vspace)
        if {$where!=".invisible"} {  
          $where create line $x [expr $y+$triheight] \
                             [expr $x+$width/2] $y \
                             [expr $x+$width] [expr $y+$triheight] \
                             $x [expr $y+$triheight] \
                             -tags $mtags -fill $main_color
          execobj $supobj [get-center $x $width [lindex $size 0]] \
                 [expr $y+$trioffset+$triheight] $where $mtags}
        list $width [expr $height+$trioffset+$triheight]}


#
# Termnodes (for Holger)
# nodes with one child and half the usual space
#

proc termnode {x y where mtags termnode} { ;# draws a terminal node
        global clig_globals
        set triheight [expr $clig_globals(tree_vspace)/3]
        stack $x $y $where $mtags [list [lindex $termnode 0] \
                                        {vspace 1} \
                                        [list vline $triheight] \
                                        {vspace 1} \
                                        [lindex $termnode 1]]}

