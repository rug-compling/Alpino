###############################################################################
#
# drs -- drawing DRS's and EKN structures (head boxes)
# last update 8-Dec-95
# author : karsten konrad, konrad@coli-gate.uni-sb.de
#
###############################################################################

#
# Draw 
# All commands here are to be used within the DRS-description lists
# (DDL). Like other description lists, they are executable
# Tcl/Tk-code. 
#

proc draw-drs-border {x y ygrid w h where mtags} { ;# headbox outline
        global main_color
        draw-rect $x $y $w $h $where $mtags
        $where create line $x [expr $y+$ygrid] \
                [expr $x+$w] [expr $y+$ygrid] -tags $mtags -fill $main_color}

proc draw-corner-border {x y xgrid ygrid w h where mtags} { ;# cbox outline
         draw-rect $x $y $w $h $where $mtags
         draw-rect $x $y $xgrid $ygrid $where $mtags}

#
# drs
#

proc drs {x y where mtags drs} { ;# draw basic drs
        resize-drs $x $y 0 0 $where $mtags $drs}

proc resize-drs {x y xrs yrs where mtags drs} { ;# draw resizable basic drs
        set referents [lindex $drs 0]
        set list [lreplace $drs 0 0] ;# geting the body
        set refsize [calcsize $referents] ;# referents might be object
        set xoffset 12
        set yoffset 8
        set yrefoffset 3
        set heights {} ;# height and width of children
        set xsize 0
        set ysize 0 ;# stores maximum dimensions of body frame

        foreach item $list { ;#calculating global size
                set size [calcsize $item]
                lappend heights $size ;# stores dimensions of childs
                set xsize [max $xsize [lindex $size 0]]
                set ysize [expr $ysize+[lindex $size 1]+$yoffset]}

        set xsize [max $xsize [lindex $refsize 0]] ;# if referents need space
        set xsize [expr $xsize+$xoffset+$xrs] ;# reserve some outline space
        set ysize [expr $ysize+2*$yrefoffset+$yoffset+$yrs]

        ;# actual drawing (only when visible)
        if {$where != ".invisible"} {

         ;# now drawing outer frame
         draw-drs-border $x $y [expr [lindex $refsize 1]+2*$yrefoffset] \
                           $xsize [expr $ysize+[lindex $refsize 1]] \
                           $where $mtags

         ;# placing the head
         execobj $referents [get-center $x $xsize [lindex $refsize 0]] \
              [expr $y+$yrefoffset] $where $mtags 

         ;# drawing the children
         set index 0 ;# index points to current
                                               ;# dimensions
         set ycur [expr $y+2*$yrefoffset+$yoffset+[lindex $refsize 1]+$yrs/2]
                 ;# this is the y position of the first child 
         ;# 
         foreach item $list { ;# catch the child and draw
                 set width [lindex [lindex $heights $index] 0]
                 set height [lindex [lindex $heights $index] 1]
                 execobj $item [get-center $x $xsize $width] $ycur \
                         $where $mtags
                 set ycur [expr $ycur+$height+$yoffset] ;# inc y position
                 set index [expr $index+1]}}

        list $xsize [expr $ysize+[lindex $refsize 1]] ;# complete size
        }

#
# cornerbox
# 

#
# resizable: rsx and rsy reserve more space
#

proc resize-cornerbox {x y rsx rsy where mtags drs} { ;# draw refes. in corner
        set referents [lindex $drs 0]
        set list [lreplace $drs 0 0] ;# geting the body and refs
        set refsize [calcsize $referents] ;# referents might be object
        set xoffset 12
        set yoffset 8
        set yrefoffset 3
        set xrefoffset 4 
        set heights {} ;# height and width of children
        set xsize 0
        set ysize 0 ;# stores maximum dimensions of body frame

        foreach item $list { ;#calculating global size
                set size [calcsize $item]
                lappend heights $size ;# stores dimensions of childs
                set xsize [max $xsize [lindex $size 0]]
                set ysize [expr $ysize+[lindex $size 1]+$yoffset]}

        set xsize [max $xsize [lindex $refsize 0]] ;# if referents need space
        set xsize [expr $xsize+$xoffset+$rsx] ;# reserve some outline space
        set ysize [expr $ysize+2*$yrefoffset+$yoffset+$rsy]

        ;# actual drawing (only when visible)
        if {$where != ".invisible"} {

         ;# now drawing outer frame
         draw-corner-border $x $y \
                         [expr [lindex $refsize 0]+2*$xrefoffset] \
                         [expr [lindex $refsize 1]+2*$yrefoffset] \
                        $xsize [expr $ysize+[lindex $refsize 1]] \
                        $where $mtags

         ;# placing the head
         execobj $referents [expr $x+$xrefoffset] [expr $y+$yrefoffset] \
              $where $mtags

         ;# drawing the children
         set index 0 ;# index points to current
                                               ;# dimensions
         set ycur [expr $y+2*$yrefoffset+$yoffset+[lindex $refsize 1]+\
                        $rsy/2]
                 ;# this is the y position of the first child 
         ;# 
         foreach item $list { ;# catch the child and draw
                 set width [lindex [lindex $heights $index] 0]
                 set height [lindex [lindex $heights $index] 1]
                 execobj $item [get-center $x $xsize $width] $ycur \
                         $where $mtags
                 set ycur [expr $ycur+$height+$yoffset] ;# inc y position
                 set index [expr $index+1]}}

        list $xsize [expr $ysize+[lindex $refsize 1]] ;# complete size
        }

#
# the standard command withou resizing option
#

proc cornerbox {x y where mtags drs} { ;# draw refes. in corner
        resize-cornerbox $x $y 0 0 $where $mtags $drs}         

#
# followboxes: boxes that follow another object
# resizable.
#

proc followboxes {x y where mtags objs} { ;# draws obj1 followed by the rest
        resize-followboxes $x $y 0 0 $where $mtags $objs}

proc resize-followboxes {x y xrs yrs where mtags objs} { ;# resizable fboxes
        global main_color
        set xoffset 6
        set yoffset 4
        set boxoffset 6
        set yrefoffset 3 

        set first [lindex $objs 0]
        set follows [lreplace $objs 0 0]

        set firstsize [calcsize $first]

        set heights {} ;# height and width of children
        set xsize 0
        set ysize 0 ;# stores maximum dimensions of body frame

        foreach item $follows { ;#calculating global size
                set size [calcsize $item]
                lappend heights $size ;# stores dimensions of childs
                set xsize [expr $xsize+[lindex $size 0]+$xoffset+2*$boxoffset]
                set ysize [max $ysize [lindex $size 1]]}

        set ysize [expr $ysize+2*$boxoffset]
        set newheight [expr [max [lindex $firstsize 1] $ysize]+$yrs]
        # newheight ist the height the first object must have
        
        if {$where!=".invisible"} {
          set newfirst [resizeobj $first $x $y $xrs \
                        [expr $newheight-[lindex $firstsize 1]] \
                          $where $mtags]

          ;# drawing the children
           set index 0 ;# index points to current
                       ;# dimensions
           set xcur [expr $x+[lindex $newfirst 0]+$xoffset]
                   ;# this is the x position of the first child 
           set maxypos [expr $y+$newheight]
                   ;# this is the maximum y position
           ;# 
           foreach item $follows { ;# catch the child and draw
                   set width [lindex [lindex $heights $index] 0]
                   set height [lindex [lindex $heights $index] 1]
                   ;# draw objects with box around them
                   execobj $item \
                           [expr $xcur+$boxoffset] \
                           [get-center $y $newheight $height] \
                           $where $mtags
                   $where create rectangle $xcur $y \
                          [expr $xcur+$width+2*$boxoffset] \
                          $maxypos -tags $mtags -outline $main_color
                   $where create line $xcur $y [expr $xcur-$xoffset] $y \
                          -tags $mtags -fill $main_color
                   $where create line $xcur $maxypos \
                                      [expr $xcur-$xoffset] $maxypos \
                                      -tags $mtags -fill $main_color
                   set xcur [expr $xcur+$width+$xoffset+2*$boxoffset] 
                          ;# inc x position
                   set index [expr $index+1]}}

        list [expr $xsize+[lindex $firstsize 0]+$xrs] $newheight}
        

#
# stackboxes: boxes on top of each other
#

proc stackboxes {x y where mtags objs} { ;# draws boxes on top of an object
        resize-stackboxes $x $y 0 0 $where $mtags $objs}

proc resize-stackboxes {x y xrs yrs where mtags objs} { ;# resizable stackboxes
        global main_color
        set boxoffset 6

        set least [expr [llength $objs]-1] ;# position of last object
        set last [lindex $objs $least]     ;# last (main) object
        set stacked [lreplace $objs $least $least] ;# 

        set lastsize [calcsize $last]

        set heights {} ;# height and width of children
        set xsize 0
        set ysize 0 ;# stores maximum dimensions of body frame

        foreach item $stacked { ;#calculating global size
                set size [calcsize $item]
                lappend heights $size ;# stores dimensions of childs
                set ysize [expr $ysize+[lindex $size 1]+2*$boxoffset]
                set xsize [max $xsize [lindex $size 0]]}

        set xsize [expr $xsize+2*$boxoffset]
        set newwidth [expr [max [lindex $lastsize 0] $xsize]+$xrs]
        
        # newwidth ist the width the last object must have
        
        if {$where!=".invisible"} {

          ;# drawing the children
           set index 0 ;# index points to current
                       ;# dimensions
           set ycur $y
                   ;# this is the x position of the first child
           set maxxpos [expr $x+$newwidth]
                   ;# this is the maximum x position 
           ;# 
           foreach item $stacked { ;# catch the child and draw
                   set width [lindex [lindex $heights $index] 0]
                   set height [lindex [lindex $heights $index] 1]
                   ;# draw objects with box around them
                   execobj $item \
                           [get-center $x $newwidth $width] \
                           [expr $ycur+$boxoffset] \
                           $where $mtags
                   $where create rectangle $x $ycur $maxxpos \
                                [expr $ycur+$height+2*$boxoffset] \
                                -tags $mtags -outline $main_color 
                   set ycur [expr $ycur+$height+2*$boxoffset] 
                          ;# inc y position
                   set index [expr $index+1]}

           resizeobj $last $x $ycur [expr $newwidth-[lindex $lastsize 0]] \
                  $yrs $where $mtags}

        list $newwidth [expr $ysize+[lindex $lastsize 1]+$yrs]}
        
#
# stackboxes: boxes on top of each other
#

proc  stairboxes {x y where mtags objs} { ;# draws boxes with stairindx
        resize-stairboxes $x $y 0 0 $where $mtags $objs}

proc resize-stairboxes {x y xrs yrs where mtags objs} { ;# resizable stairboxes
        global main_color
        set xoffset 6
        set boxoffset 3
        set stairoffset 8
   
        set least [expr [llength $objs]-1] ;# position of last object
        set last [lindex $objs $least]     ;# last (main) object
        set stacked [lreplace $objs $least $least] ;# 

        set lastsize [calcsize $last]

        set heights {} ;# height and width of children
        set xsize 0
        set ysize 0 ;# stores maximum dimensions of body frame

        foreach item $stacked { ;#calculating global size
                set size [calcsize $item]
                lappend heights $size ;# stores dimensions of childs
                set ysize [expr $ysize+[lindex $size 1]+2*$boxoffset]
                set xsize [max $xsize [lindex $size 0]]}

        set xsize [expr $xsize+2*$boxoffset+$stairoffset]
        set newwidth [expr [max [lindex $lastsize 0] $xsize]+$xrs]
        
        # newwidth ist the width the last object must have
        
        if {$where!=".invisible"} {

          ;# drawing the children
           set index 0 ;# index points to current
                       ;# dimensions
           set ycur $y
                   ;# this is the x position of the first child
           set maxxpos [expr $x+$newwidth]
                   ;# this is the maximum y position
           set stairx [expr $x+$stairoffset] 
           set stairwidth [expr $newwidth-$stairoffset]
           ;# 
           foreach item $stacked { ;# catch the child and draw
                   set width [lindex [lindex $heights $index] 0]
                   set height [lindex [lindex $heights $index] 1]
                   ;# draw objects with box around them
                   execobj $item \
                           [get-center $stairx $stairwidth $width] \
                           [expr $ycur+$boxoffset] \
                           $where $mtags
                   $where create rectangle $stairx $ycur $maxxpos \
                                [expr $ycur+$height+2*$boxoffset] \
                                -tags $mtags -outline $main_color
                   set ycur [expr $ycur+$height+2*$boxoffset] 
                          ;# inc y position
                   set index [expr $index+1]}

           resizeobj $last $x $ycur [expr $newwidth-[lindex $lastsize 0]] \
                  $yrs $where $mtags}

        list $newwidth [expr $ysize+[lindex $lastsize 1]+$yrs]}

#
# labeling graphical objects
#        

proc labeled {x y where mtags objs} { ;# labeled object
        set offset 1
        set obj1 [lindex $objs 0]
        set obj2 [lindex $objs 1]
        set size [execobj $obj1 $x $y \
                        $where $mtags]
        set width [expr [lindex $size 0]+$offset]
        set height [expr [lindex $size 1]+$offset]
        set size2 [execobj $obj2 [expr $x+$width] [expr $y+$height] \
                $where $mtags]
        list [expr [lindex $size2 0]+$width] [expr [lindex $size2 1]+$height]}
