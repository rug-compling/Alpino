
###############################################################################
#
# fs -- drawing (typed) feature structures
# last update 17-Oct-95
# author : karsten konrad, konrad@coli-gate.uni-sb.de
#
###############################################################################


;# label-value-pairs

proc feature {x y where mtags lvp} { ;# feature-value
        global clig_globals
        set xoff [expr $clig_globals(fontsize)/2]
        set label [lindex $lvp 0] 
        set value [lindex $lvp 1]
        set size1 [plain-text $x $y $where $mtags $label]
        set xcur [expr $x+[lindex $size1 0]+$xoff]
        set size2 [execobj $value $xcur $y $where $mtags]
        list [expr [lindex $size1 0]+$xoff+[lindex $size2 0]] \
             [max [lindex $size1 1] [lindex $size2 1]]}
             
proc fs {x y where mtags lvps} { ;# feature structure
        squarebracket $x $y $where $mtags [list "leftStack $lvps"]}
