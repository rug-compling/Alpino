###############################################################################
#
# graphical toolbox -- basic graphical operations for clig
# v 0.1
# last update 9-Jan-96
# updates: 9-Jan-96: new parameter convention
#          3-Sep-96: small error in angle-size corrected
# author : karsten konrad, konrad@coli-gate.uni-sb.de
#
###############################################################################

#
# size evaluation and graphical formating
# obj stands for and command string object of the form {<command> rest}
# which is executable tcl-code
#

proc get-center { xbase xbasewidth width } { ;#calculates center pos.
        expr ($xbasewidth-$width)/2+$xbase}                

proc execobj {obj x y where mtags} { ;# executes the object description
        [lindex $obj 0] $x $y $where $mtags [lreplace $obj 0 0]}

proc resizeobj {obj x y rsx rsy where mtags} { ;# resizes&draws the object
        "resize-[lindex $obj 0]" $x $y $rsx $rsy \
                $where $mtags [lreplace $obj 0 0]}

proc calcsize {obj} { ;# calculates the size by invisible drawing
        global _calcsize_memo
        if {[info exists _calcsize_memo($obj)]} \
           {return $_calcsize_memo($obj)} \
        else {set _calcsize_memo($obj) [execobj $obj 0 0 .invisible ""]}}

#
# basic graphical procedures (return no size and start with draw-)
#

#
# Commando area: a filed white rectangle rectangle which gets highlighted
# when one of its childs get clicked on. A doubleclick executes a command.
# 
#
proc draw-commando-area {x y w h where mtag com} { ;# draw selectable 
                                                    ;# (command) rectangle
        global background_color highlight_color
    set tag [$where create rectangle\
                [expr $x-3] [expr $y-3] [expr $x+$w+3] [expr $y+$h+3] \
                -fill $background_color -outline $background_color \
                -tags [concat $mtag clickable cmg]]
        ;# higlighting area with border thick GvN: changed width 3 into 1
        set command1 [list $where itemconfigure $tag -width 1 \
                           -fill $highlight_color -outline black]
        ;# reset area
        set command2 [list $where itemconfigure $tag -width 1 \
                           -fill $background_color \
                            -outline $background_color] 
        ;# bind commandos
         ### GvN: changed <1> into <Enter>
        $where bind $mtag <Enter> +$command1 ;# highlight when pressing
        $where bind $mtag <Leave> +$command2 ;# reset background color
        $where bind $mtag <Double-Button-1> +$com}

proc draw-active-area {x y w h where mtag com} { ;# draw specicic 
                                                 ;# (command) rectangle
    # some more width around box by GvN
        global background_color highlight_color
    set tag [$where create rectangle\
                [expr $x-3] [expr $y-3] [expr $x+$w+3] [expr $y+$h+3] \
                -fill $background_color -outline $background_color \
                -tags [concat $mtag clickable cmg]]
        ;# higlighting area with border thick  GvN: changed width 3 into 1
        set command1 [list $where itemconfigure $tag -width 1 \
                           -fill $highlight_color -outline black]
        ;# reset area
        set command2 [list $where itemconfigure $tag -width 1 \
                           -fill $background_color \
                            -outline $background_color] 
        ;# bind commandos
        $where bind $mtag <Leave> +$command2 ;# reset background color
        
        foreach item $com {
         ### GvN: changed <1> into <Enter>
        $where bind $mtag <Enter> +$command1 ;# highlight
        $where bind $mtag [lindex $item 0] +$command1 ;# highlight
        if {[lindex $item 0]=="popup"} { ;# popup menu definition
                set menu [define-popup [lindex $item 2]]
                $where bind $mtag [lindex $item 1] \
                        +[list show-popup %X %Y $menu]} \
        else {$where bind $mtag [lindex $item 0] +[lindex $item 1]}}}

#
# Clickable objects
#

set clig_counter_clickable 0

proc clickable {x y where mtags comobj} {;# clickable object
        global clig_counter_clickable
        set object [lindex $comobj 0]
        set command [lindex $comobj 1]
        set size [calcsize $object]
        if {$where!=".invisible"} {
                set clig_counter_clickable [expr $clig_counter_clickable+1] 
                set tag "ct$clig_counter_clickable"
                draw-commando-area $x $y \
                        [lindex $size 0] [lindex $size 1] \
                        $where $tag $command
                execobj $object $x $y $where [list $tag clickable cmg]}
        return $size}

proc active {x y where mtags comobj} {;# action area
        global  clig_counter_clickable
        set object [lindex $comobj 0]
        set commands [lreplace $comobj 0 0]
        set size [calcsize $object]
        if {$where!=".invisible"} {
                set clig_counter_clickable [expr $clig_counter_clickable+1] 
                set tag "ct$clig_counter_clickable"
                draw-active-area $x $y \
                        [lindex $size 0] [lindex $size 1] \
                        $where $tag $commands
            execobj $object $x $y $where [list $tag clickable cmg]}
    return $size}

##############################################################################
# using colors
##############################################################################

set main_color black

proc color {x y where mtags color_object} { ;# changes main color
        global main_color clig_globals

        set color [lindex $color_object 0]
        set object [lindex $color_object 1]

        if {$clig_globals(no_colors)==1} { ;# do nothing when no colors
           execobj $object $x $y $where $mtags} \
        else {
                set store $main_color ;# store for restoring
                set main_color $color
                set size [execobj $object $x $y $where $mtags]
                set main_color $store
                return $size}}

proc color-area {x y where mtags col_obj} { ;# resizable large box
        global main_color clig_globals background_color

        set color [lindex $col_obj 0]
        set object [lindex $col_obj 1]

        set size [calcsize $object]
        set width [lindex $size 0]
        set height [lindex $size 1]

        if {$where!=".invisible"} {
          if {$clig_globals(no_colors)==1} { ;# no colors allowed
                set color $background_color}
          $where create rectangle $x $y \
                [expr $x+$width] [expr $y+$height] \
                -tags $mtags -fill $color
          execobj $object $x $y $where $mtags} 
        list $width $height}

#
# draw a rectangle
#

proc draw-rect {x y w h where mtags} { ;# draw unfilled rectangle
        global main_color
        $where create rectangle $x $y [expr $x+$w] [expr $y+$h] \
        -outline $main_color -tags $mtags}        

###############################################################################
# texts
###############################################################################

#
### special characters for application

set special_sign_table(neg) "\330"
set special_sign_table(disj) "\332" 
set special_sign_table(conj) "\331"  
set special_sign_table(lambda) "\154" 
set special_sign_table(merge) "\304" 
set special_sign_table(forall) "\042" 
set special_sign_table(exists) "\044" 
set special_sign_table(subseteq) "\315"
set special_sign_table(subset) "\314"
set special_sign_table(notsubset) "\313"
set special_sign_table(emptyset) "\306"
set special_sign_table(imp)      "\336"
set special_sign_table(rightarrow) "\256" 
set special_sign_table(equal) "\75"
set special_sign_table(notequal) "\271" 
 
proc sign {x y where mtags sign} { ;# special signs
        global special_font clig_globals special_sign_table        
        if {[info exists special_sign_table($sign)]} {
                set cstring $special_sign_table($sign)} \
        else {set cstring $sign}
        draw-plain-font-text $x $y $where [linsert $mtags 0 special-font] \
         $special_font($clig_globals(fontsize)) $cstring}

#
### Intentional Logic Operators

proc il-up {x y where mtags rest} { ;# intentional logic upward arrow
        global special_font clig_globals
        set fsize $clig_globals(fontsize)
         draw-plain-font-text $x [expr $y-$fsize/3] $where \
         [linsert $mtags 0 special-font] \
         $special_font([expr $fsize-2]) "\331"} 

proc il-down {x y where mtags rest} { ;# intentional logic upward arrow
        global special_font clig_globals
        set fsize $clig_globals(fontsize)
         draw-plain-font-text $x [expr $y-$fsize/3] $where \
         [linsert $mtags 0 special-font] \
         $special_font([expr $fsize-2]) "\332"} 

proc charbox {x y where mtags rest} { ;#box around space
        execobj {boxed 0 {plain-text " "}} $x $y $where $mtags}

proc chardiamond {x y where mtags rest} { ;#diamond around space
        global clig_globals
        set  fsize [expr $clig_globals(fontsize)/4]
        execobj [list diamond space $fsize $fsize] $x $y $where $mtags}

#
# drawing text
#

proc draw-text {x y where mtags font text} {
        global main_color
        $where create text $x $y -text $text \
               -anchor nw -tags $mtags \
               -font $font -fill $main_color}

proc draw-plain-font-text {x y where mtags font text} { ;# no command
        set tag [draw-text $x $y $where $mtags $font $text]
        list [expr [lindex [$where bbox $tag] 2]- \
                [lindex [$where bbox $tag] 0]-3] \
             [expr [lindex [$where bbox $tag] 3] - \
                   [lindex [$where bbox $tag] 1]]}

proc plain-text {x y where mtags text} { ;# no command
        global standard_font clig_globals
        draw-plain-font-text $x $y $where [linsert $mtags 0 plain-text] \
                $standard_font($clig_globals(fontsize)) [lindex $text 0]}

proc big-plain-text {x y where mtags text} { ;# no command
        global standard_font clig_globals
        draw-plain-font-text $x $y $where [linsert $mtags 0 big-plain-text] \
                $standard_font([expr $clig_globals(fontsize)+2]) \
                [lindex $text 0]}

proc small-plain-text {x y where mtags text} { ;# no command
        global standard_font clig_globals
        draw-plain-font-text $x $y $where [linsert $mtags 0 small-plain-text] \
                $standard_font([expr $clig_globals(fontsize)-2]) \
                [lindex $text 0]}

proc bold-text {x y where mtags text} { ;# no command
        global bold_font clig_globals
        draw-plain-font-text $x $y $where [linsert $mtags 0 bold-text] \
                $bold_font($clig_globals(fontsize)) [lindex $text 0]}

proc big-bold-text {x y where mtags text} { ;# no command
        global bold_font clig_globals
        draw-plain-font-text $x $y $where [linsert $mtags 0 big-bold-text] \
                $bold_font([expr $clig_globals(fontsize)+2]) \
                [lindex $text 0]}

proc small-bold-text {x y where mtags text} { ;# no command
        global bold_font clig_globals
        draw-plain-font-text $x $y $where [linsert $mtags 0 small-bold-text] \
                $bold_font([expr $clig_globals(fontsize)-2]) \
                [lindex $text 0]}

proc slant-text {x y where mtags text} { ;# no command
        global slant_font clig_globals
        draw-plain-font-text $x $y $where [linsert $mtags 0 slant-text] \
                $slant_font($clig_globals(fontsize)) [lindex $text 0]}

proc big-slant-text {x y where mtags text} { ;# no command
        global slant_font clig_globals
        draw-plain-font-text $x $y $where [linsert $mtags 0 big-slant-text] \
                $slant_font([expr $clig_globals(fontsize)+2]) \
                [lindex $text 0]}

proc small-slant-text {x y where mtags text} { ;# no command
        global slant_font clig_globals
        draw-plain-font-text $x $y $where [linsert $mtags 0 small-slant-text] \
                $slant_font([expr $clig_globals(fontsize)+2]) \
                [lindex $text 0]}

##############################################################################
# other graphic modules
##############################################################################

#
# logic operations

#
#
# negated objects
#

proc neg {x y where mtags obj} { ;# draw negated drs
        seq $x $y $where $mtags [list {sign neg} {plain-text " "} \
                [lindex $obj 0]]}


#
# implication 
#
    
proc imp {x y where mtags commands} { ;#draw implication
        set obj1 [lindex $commands 0]
        set obj2 [lindex $commands 1]
        seq $x $y $where $mtags \
                [list $obj1 {plain-text " "} \
                        {sign imp} {plain-text " "} $obj2]}

#
# brackets
#

proc draw-open-bracket {x y curve height where mtags} { ;#
        global main_color 
        $where create line [expr $x+$curve] $y \
                           $x [expr $y+[min [expr $height/4] 16]] \
                           $x [expr $y+$height/2] \
                           $x [expr $y+[max [expr $height*.75] \
                                        [expr $height-16]]] \
                           [expr $x+$curve] [expr $y+$height] \
                           -smooth T -splines 6 -tags $mtags \
                           -fill $main_color
        list $curve $height}

proc draw-open-angle {x y curve height where mtags} { ;#
        global main_color 
        $where create line [expr $x+$curve] $y \
                           $x [expr $y+$height/2] \
                           [expr $x+$curve] [expr $y+$height] \
                           -tags $mtags -fill $main_color
        list $curve $height}

proc draw-open-square {x y curve height where mtags} { ;#
        global main_color
        $where create line [expr $x+$curve] $y \
                           $x $y \
                           $x [expr $y+$height] \
                           [expr $x+$curve] [expr $y+$height] \
                           -tags $mtags -fill $main_color
        list $curve $height}

proc draw-close-bracket {x y curve height where mtags} { ;#
        global main_color 
        $where create line $x $y \
                           [expr $x+$curve] [expr $y+ \
                                                [min [expr $height/4] 16]] \
                           [expr $x+$curve] [expr $y+$height/2] \
                           [expr $x+$curve] [expr $y+[max [expr $height*.75] \
                                                        [expr $height-16]]] \
                           $x [expr $y+$height] \
                           -smooth T -splines 6 -tags $mtags -fill $main_color
        list $curve $height}

proc draw-close-angle {x y curve height where mtags} { ;# 
        global main_color
        $where create line $x $y \
                           [expr $x+$curve] [expr $y+$height/2] \
                           $x [expr $y+$height] \
                           -tags $mtags -fill $main_color
        list $curve $height}

proc draw-close-square {x y curve height where mtags} { ;#
        global main_color 
        $where create line $x $y \
                           [expr $x+$curve] $y \
                           [expr $x+$curve] [expr $y+$height] \
                           $x [expr $y+$height] -tags $mtags \
                           -fill $main_color
        list $curve $height}

proc bracket {x y where mtags object} { ;# bracket object
        set object [lindex $object 0] ;# bracket parameter convention

        set bracket_curve  7 ;# how "curvy" the bracket is
        set bracket_offset 3 ;# space between bracket and object
       
        set size [calcsize $object]
        if {[lindex $size 1] <= 20} {set bracket_curve 3}
        if {$where!=".invisible"} {
          ;# (
          draw-open-bracket $x $y $bracket_curve [lindex $size 1] $where $mtags
          ;# main object
          execobj $object [expr $x+$bracket_offset+$bracket_curve] $y \
                  $where $mtags
          ;# )
          draw-close-bracket \
                  [expr $x+[lindex $size 0]+2*$bracket_offset+$bracket_curve] \
                  $y $bracket_curve [lindex $size 1] $where $mtags}
          ;# size of complete object
        list [expr $x+[lindex $size 0]+2*$bracket_offset+2*$bracket_curve+1] \
             [lindex $size 1]}

proc angle {x y where mtags object} { ;# angle bracket object
        set object [lindex $object 0] ;# bracket parameter convention

        set bracket_curve  7 ;# how "curvy" the bracket is
        set bracket_offset 3 ;# space between bracket and object

        set size [calcsize $object]
        if {[lindex $size 1] <= 20} {set bracket_curve 3}
        if {[lindex $size 1] >= 80} {set bracket_curve 14}
        if {$where!=".invisible"} {
          ;# (
          draw-open-angle $x $y $bracket_curve [lindex $size 1] $where $mtags
          ;# main object
          execobj $object [expr $x+$bracket_offset+$bracket_curve] $y \
                  $where $mtags
          ;# )
          draw-close-angle \
                  [expr $x+[lindex $size 0]+2*$bracket_offset+$bracket_curve] \
                  $y $bracket_curve [lindex $size 1] $where $mtags}
          ;# size of complete object
        list [expr [lindex $size 0]+2*$bracket_offset+2*$bracket_curve+1] \
             [lindex $size 1]}

proc squarebracket {x y where mtags object} { ;# square bracket object
        set object [lindex $object 0] ;# bracket parameter convention
        set bracket_curve  6 ;# how "curvy" the bracket is
        set bracket_offset 4 ;# space between bracket and object
        set size [calcsize $object]
        if {[lindex $size 1] <= 20} {set bracket_curve 3}
        if {$where!=".invisible"} {
          ;# [
          draw-open-square $x $y $bracket_curve \
                  [expr [lindex $size 1]+2*$bracket_offset] $where $mtags
          ;# main object
          execobj $object [expr $x+$bracket_offset] \
                          [expr $y+$bracket_offset] $where $mtags
          ;# )
          draw-close-square \
                  [expr $x+[lindex $size 0]+2*$bracket_offset-$bracket_curve] \
                  $y $bracket_curve [expr [lindex $size 1]+ \
                          2*$bracket_offset]\
                  $where $mtags}
        ;# size of complete object
        list [expr [lindex $size 0]+2*$bracket_offset] \
             [expr [lindex $size 1]+2*$bracket_offset]}

#
# Lists
#

proc anglelist {x y where mtags objects} { ;# draws a list with angle brackets
if {$objects!={}} {
        set first [lindex $objects 0]
        set rest [lreplace $objects 0 0]
        set draw_list [list Seq $first] 
        foreach item $rest {
                lappend draw_list [list plain-text ","]
                lappend draw_list $item} 
                angle $x $y $where $mtags [list $draw_list]} else {
                    angle $x $y $where $mtags {plain-text}}}

#
# boxing and spacing
#

proc space {x y where mtags dim} { ;# "invisible" space rectangle
        return $dim}
 
proc vspace {x y where mtags vspace} { ;# vertical space
        list 0 $vspace}

proc hspace {x y where mtags hspace} { ;# vertical space
        list $hspace 0}

# cut and copy programming

proc boxed {x y where mtags borderobj} { ;# box with boxframe
        set border [lindex $borderobj 0]
        set obj [lindex $borderobj 1]
        set size [execobj $obj [expr $x+$border] [expr $y+$border] \
                                $where $mtags]
        set width [expr [lindex $size 0]+2*$border]
        set height [expr [lindex $size 1]+2*$border]
        draw-rect $x $y  $width $height $where $mtags
        list $width $height}

proc resize-framed {x y  rsx rsy where mtags borderobj} { 
                                                ;# invisible resizable box 
        set border [lindex $borderobj 0]
        set obj [lindex $borderobj 1]
        set size [execobj $obj [expr $x+$border+$rsx/2]\
                 [expr $y+$border+$rsy/2] \
                        $where $mtags]
        set width [expr [lindex $size 0]+2*$border+$rsx]
        set height [expr [lindex $size 1]+2*$border+$rsy]
        list $width $height}

proc framed {x y where mtags borderobj} { ;# invisible box around with frame
        set border [lindex $borderobj 0]
        set obj [lindex $borderobj 1]
        set size [execobj $obj [expr $x+$border] [expr $y+$border] \
                        $where $mtags]
        set width [expr [lindex $size 0]+2*$border]
        set height [expr [lindex $size 1]+2*$border]
        list $width $height}
 
proc bigbox {x y where mtags obj} { ;# box with large frame
        set obj [lindex $obj 0] ;# bracket parameter convention
        set border 6
        set size [execobj $obj [expr $x+$border] [expr $y+$border] \
                        $where $mtags]
        set width [expr [lindex $size 0]+2*$border]
        set height [expr [lindex $size 1]+2*$border]
        draw-rect $x $y  $width $height $where $mtags
        list $width $height}

proc resize-bigbox {x y rsx rsy where mtags obj} { ;# resizable large box
        set obj [lindex $obj 0] ;# bracket parameter convention
        set border 6
        set size [calcsize $obj]
        set width [expr [lindex $size 0]+2*$border+$rsx]
        set height [expr [lindex $size 1]+2*$border+$rsy]
        if {$where!=".invisible"} {
          draw-rect $x $y $width $height $where $mtags
          execobj $obj [expr $x+$border+$rsx/2] \
                        [expr $y+$border+$rsy/2] $where $mtags} 
        list $width $height}
 
proc smallbox {x y where mtags obj} { ;# box with small frame
        set obj [lindex $obj 0] ;# bracket parameter convention
        set border 2
        set size [execobj $obj [expr $x+$border] [expr $y+$border] \
                        $where $mtags]
        set width [expr [lindex $size 0]+2*$border]
        set height [expr [lindex $size 1]+2*$border]
        draw-rect $x $y  $width $height $where $mtags 
        list $width $height}

proc resize-smallbox {x y rsx rsy where mtags obj} { ;# resizable large box
        set obj [lindex $obj 0] ;# bracket parameter convention
        set border 2
        set size [calcsize $obj]
        set width [expr [lindex $size 0]+2*$border+$rsx]
        set height [expr [lindex $size 1]+2*$border+$rsy]
        if {$where!=".invisible"} {
          draw-rect $x $y $width $height $where $mtags
          execobj $obj [expr $x+$border/2+$rsx/2] \
                        [expr $y+$border/2+$rsy/2] $where $mtags} 
        list $width $height}   

#
# horizontal ordered objects (sequences)
#

proc draw-seq {x y where mtags xoffset sequence} { ;# sequencing objects
        set heights {}
        set ysize 0; set xsize 0        

        foreach item $sequence {
                set size [calcsize $item]
                lappend heights $size ;# storing dims of child
                set ysize [max $ysize [lindex $size 1]]
                set xsize [expr $xsize+[lindex $size 0]+$xoffset]}

        ;# actual drawing (only when visible)
        if {$where != ".invisible"} {

         ;# drawing the children
         set index 0 ;# index points to current
                     ;# dimensions
         set xcur $x
                 ;# this is the x position of the first child 
         ;# 
         foreach item $sequence { ;# catch the child and draw
                 set width [lindex [lindex $heights $index] 0]
                 set height [lindex [lindex $heights $index] 1]
                 execobj $item $xcur [get-center $y $ysize $height] \
                                $where $mtags
                 set xcur [expr $xcur+$width+$xoffset] ;# inc x position
                 set index [expr $index+1]}}

        list [expr $xsize-$xoffset] $ysize ;# complete size
        }

proc seq {x y where mtags sequence} { ;# no diff.
        draw-seq $x $y $where $mtags 1 $sequence}     

proc Seq {x y where mtags sequence} { ;# no diff.
        draw-seq $x $y $where $mtags 4 $sequence} 

proc SEQ {x y where mtags sequence} { ;# no diff.
        draw-seq $x $y $where $mtags 8 $sequence}        


#
# vertically ordered objects (stacks)
#

#
# new try with draw


proc draw-stack {x y where mtags yoffset stack} { ;# stacking objects, old
        set heights {}
        set ysize 0; set xsize 0        

        foreach item $stack {
                set size [calcsize $item]
                lappend heights $size ;# storing dims of child
                set xsize [max $xsize [lindex $size 0]]
                set ysize [expr $ysize+[lindex $size 1]+$yoffset]}

        ;# actual drawing (only when visible)
        if {$where != ".invisible"} {

         ;# drawing the children
         set index 0 ;# index points to current
                     ;# dimensions
         set ycur $y
                 ;# this is the x position of the first child 
         ;# 
         foreach item $stack { ;# catch the child and draw
                 set width [lindex [lindex $heights $index] 0]
                 set height [lindex [lindex $heights $index] 1]
                 execobj $item [get-center $x $xsize $width] $ycur \
                        $where $mtags
                 set ycur [expr $ycur+$height+$yoffset] ;# inc y position
                 set index [expr $index+1]}}

        list $xsize [expr $ysize-$yoffset] ;# complete size
        }

proc Stack {x y where mtags stack} {
        draw-stack $x $y $where $mtags 4 $stack} 

proc stack {x y where mtags stack} {
        draw-stack $x $y $where $mtags 0 $stack} 




proc draw-leftstack {x y where mtags yoffset stack} { ;# stacking aligned left
        set ysize 0; set xsize 0        

        foreach item $stack {
                set size [execobj $item $x [expr $y+$ysize] \
                                $where $mtags]
                set xsize [max $xsize [lindex $size 0]]
                set ysize [expr $ysize+[lindex $size 1]+$yoffset]}

        list $xsize [expr $ysize-$yoffset]}
                
proc leftstack {x y where mtags stack} {
        draw-leftstack $x $y $where $mtags 0 $stack} 

proc leftStack {x y where mtags stack} {
        draw-leftstack $x $y $where $mtags 4 $stack} 


#
# diamonds
#

proc diamond {x y where mtags obj} { ;# diamond around an object
        set obj [lindex $obj 0] ;# bracket parameter convention
        global main_color
        set size [calcsize $obj]
        set width [expr [lindex $size 0]+2]
        set height [expr [lindex $size 1]+2]
        execobj $obj [expr $x+$height/2+1] [expr $y+$width/2+1] \
                $where $mtags
        ;# draw diamond
        $where create line $x [expr $y+$height/2+$width/2] \
                           [expr $x+$height/2+$width/2] $y \
                           [expr $x+$width+$height] \
                           [expr $y+$height/2+$width/2] \
                           [expr $x+$height/2+$width/2] \
                           [expr $y+$width+$height] \
                           $x [expr $y+$height/2+$width/2] -tags $mtags\
                           -fill $main_color  
         list [expr $width+$height] [expr $width+$height]}

#
# bitmaps
#

proc bitmap {x y where mtags obj} { ;# draws image from file
        set graphics [image create bitmap -file $obj] 
        set tag [$where create image $x $y -image $graphics \
                     -anchor nw -tags $mtags]
        list [expr [lindex [$where bbox $tag] 2]- \
                [lindex [$where bbox $tag] 0]-3] \
             [expr [lindex [$where bbox $tag] 3] - \
                   [lindex [$where bbox $tag] 1]]}

#
# underlining objects
#

proc underline {x y where mtag obj} { ;# line under object
        set obj [lindex $obj 0] ;# bracket parameter convention
        global main_color
        set size [execobj $obj $x $y $where $mtag]
        set yline [expr $y+[lindex $size 1]+1]
        $where create line $x $yline \
                           [expr $x+[lindex $size 0]] $yline -tags $mtag \
                           -fill $main_color
        list [lindex $size 0] [expr $yline-$y]}

#
# sub and superscript
#

proc sub {x y where mtags objs} { ;# subscript (labeling) o1 with o2
        set obj1 [lindex $objs 0]
        set obj2 [lindex $objs 1]
        set size1 [execobj $obj1 $x $y $where $mtags]
        set size2 [calcsize $obj2]
        if {$where!=".invisible"} {
                execobj $obj2 [expr $x+[lindex $size1 0]] \
                              [max [expr $y+[lindex $size1 1]/2] \
                                [expr $y+[lindex $size1 1]- \
                                [lindex $size2 1]/1.5]] \
                        $where $mtags}
        list [expr [lindex $size1 0]+[lindex $size2 0]] \
              [max [expr [lindex $size1 1]+[lindex $size2 1]/3] \
                   [expr [lindex $size1 1]/2+[lindex $size2 1]]]}

proc subscript {x y where mtags objs} { ;# subscript text o1 with o2
        set obj1 [lindex $objs 0]
        set obj2 [lindex $objs 1]
        set size1 [calcsize $obj1]
        set size2 [calcsize $obj2]
        set ymax  [max [expr [lindex $size1 1]+[lindex $size2 1]/3] \
                            [expr [lindex $size1 1]/2+[lindex $size2 1]]]
        set yoffset [expr $ymax-[lindex $size1 1]]
        if {$where!=".invisible"} {
                set topsub [max [expr $y+[lindex $size1 1]/2] \
                                [expr $y+[lindex $size1 1]- \
                                [lindex $size2 1]/1.5]]
                execobj $obj1 $x [expr $y+$yoffset] $where $mtags
                execobj $obj2 [expr $x+[lindex $size1 0]] \
                              [expr $topsub+$yoffset] \
                        $where $mtags}
        list [expr [lindex $size1 0]+[lindex $size2 0]] \
            [expr $ymax+$yoffset]}

proc superscript {x y where mtags objs} { ;# superscript text o1 with o2
        set obj1 [lindex $objs 0]
        set obj2 [lindex $objs 1]
        set size1 [calcsize $obj1]
        set size2 [calcsize $obj2]
        set ymax  [max [expr [lindex $size1 1]+[lindex $size2 1]/3] \
                            [expr [lindex $size1 1]/2+[lindex $size2 1]]]
        set yoffset [expr $ymax-[lindex $size1 1]]
        if {$where!=".invisible"} {
                set topsub [max [expr $y+[lindex $size1 1]/2] \
                                [expr $y+[lindex $size1 1]- \
                                [lindex $size2 1]/1.5]]
                execobj $obj1 $x [expr $y+$yoffset] $where $mtags
                execobj $obj2 [expr $x+[lindex $size1 0]] \
                              $y $where $mtags}
        list [expr [lindex $size1 0]+[lindex $size2 0]] \
            [expr $ymax+$yoffset]}

proc super {x y where mtags objs} { ;# superscript o1 with o2
        set obj1 [lindex $objs 0]
        set obj2 [lindex $objs 1]
        set size1 [calcsize $obj1]
        set size2 [calcsize $obj2]
        if {$where!=".invisible"} {
                execobj $obj2 [expr $x+[lindex $size1 0]] \
                              $y $where $mtags
                execobj $obj1 $x \
                        [max [expr $y+[lindex $size2 1]/2] \
                             [expr $y+[lindex $size2 1]-\
                                        [lindex $size1 1]/3]] \
                        $where $mtags}         
        list [expr [lindex $size1 0]+[lindex $size2 0]] \
              [max [expr [lindex $size1 1]+[lindex $size2 1]*2/3] \
                   [expr [lindex $size1 1]/2+[lindex $size2 1]]]}

#
# other line objects
#

proc vline {x y where mtags height} { ;# vertical line
        global main_color
        $where create line $x $y $x [expr $height+$y] \
                -fill $main_color -tags $mtags
        list 0 $height} 

proc hline {x y where mtags width} { ;# horizontal line
        global main_color
        $where create line $x $y [expr $x+$width] $y \
                -fill $main_color -tags $mtags
        list $width 0}

#
# A/B for theorem proving etc.
# 

proc frac {x y where mtags objs} { ;# A/B
        set yoffset 2
        set obj1 [lindex $objs 0]
        set obj2 [lindex $objs 1]
        set size1 [calcsize $obj1]
        set size2 [calcsize $obj2]
        set xmax [max [lindex $size1 0] [lindex $size2 0]]
        if {$where!=".invisible"} {
                execobj $obj1 $x $y $where $mtags
                execobj [list hline $xmax] $x \
                        [expr $y+[lindex $size1 1]+$yoffset] $where $mtags 
                execobj $obj2 $x [expr $y+[lindex $size1 1]+2*$yoffset-1] \
                        $where $mtags}
        list $xmax [expr [lindex $size1 1]+[lindex $size2 1]+2*$yoffset-1]}

proc lfrac {x y where mtags objs} { ;# (a/b) with label c
        set yoffset 2
        set obj1 [lindex $objs 0] ;# object A
        set obj2 [lindex $objs 1] ;# object B
        set obj3 [lindex $objs 2] ;# label object C
        set size1 [calcsize $obj1]
        set size2 [calcsize $obj2]
        set size3 [calcsize $obj3]
        set xmax [max [lindex $size1 0] [lindex $size2 0]]
        if {$where!=".invisible"} {
                execobj $obj1 $x $y $where $mtags
                ;# draw fraction line
                execobj [list hline $xmax] $x \
                        [expr $y+[lindex $size1 1]+$yoffset] $where $mtags 
                execobj $obj2 $x [expr $y+[lindex $size1 1]+2*$yoffset-1] \
                        $where $mtags
                ;# put label in center to line
                execobj $obj3 [expr $x+$xmax] \
                        [max $y [expr $y+[lindex $size1 1]+$yoffset-\
                                [lindex $size3 1]/2]] $where $mtags}
        list [expr $xmax+[lindex $size3 0]] \
             [max [expr [lindex $size1 1]+\
                                [lindex $size2 1]+2*$yoffset-1] \
                        [expr [lindex $size1 1]+$yoffset+\
                                [lindex $size3 1]/2]]}


