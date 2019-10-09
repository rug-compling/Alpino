##############################################################################
# CLIG -- Computational Linguistic's Interactive Grapher
# zooming (graph compression) routines
# v: 0.1
# author karsten konrad, konrad@coli.uni-sb.de
# last update: 21-Nov-95
##############################################################################

#
# scaling the main canvas
#

set graph_scale 10 ;# graph scaled to 100% at fontsize 12

proc adjust-fontsize {scale where} { ;# adjusting fontsize to scaling
        global clig_globals standard_font slant_font bold_font special_font
        $where itemconfigure plain-text \
          -font $standard_font($scale)
        $where itemconfigure big-plain-text \
          -font $standard_font([expr $scale+2])
        $where itemconfigure slant-text \
          -font $slant_font($scale)
        $where itemconfigure big-slant-text \
          -font $slant_font([expr $scale+2])
        $where itemconfigure bold-text \
          -font $bold_font($scale)
        $where itemconfigure big-bold-text \
          -font $bold_font([expr $scale+2])
        $where itemconfigure special-font \
          -font $special_font($scale)}

proc scale-graph {scale where} {
        global font_zoom clig_globals
        set scale_factor $font_zoom($scale,$clig_globals(fontsize)) 
        adjust-fontsize $scale $where
	$where scale cmg 0 0 $scale_factor $scale_factor
        return $scale_factor}

proc unscale {scale where} { ;# quickly to 100%
        global font_zoom clig_globals
        set scale_factor [expr 1/$font_zoom($scale,$clig_globals(fontsize))] 
        $where scale cmg 0 0 $scale_factor $scale_factor
        fit-to-scale $scale_factor $where} 

proc fit-to-scale {scaling where} { ;# fits canvas to scaling
        set width [expr [lindex [$where configure -width] 4]*$scaling]
        set height [expr [lindex [$where configure -height] 4]*$scaling]

        .graphics configure -width $width
        .graphics configure -height $height
        .graphics configure -scrollregion [list 0 0 $width $height]}
        

#
# manual zoomboxes in graphics
#

set zoom_depth 9

# default depth for uncompressed objects

set _zoom_stack 0

proc zoombox {x y where mtags rest} { ;# box with ... in it
        set border 2
        set size [plain-text [expr $x+$border] [expr $y+$border] \
                        $where $mtags {...}]
        set width [expr [lindex $size 0]+2*$border]
        set height [expr [lindex $size 1]+2*$border]
        draw-rect $x $y  $width $height $where $mtags 
        list $width $height}

proc zoom {x y where mtags obj} { ;# the zoom command
        global _zoom_memo 
        if {[info exists _zoom_memo($obj)]} { ;# object is expanded?
           execobj $obj $x $y $where $mtags} \
        else {
            set com  [list clickable [list zoombox] \
                        [linsert \$main_object 0 clig]]
            execobj $com $x $y $where $mtags}}
        
