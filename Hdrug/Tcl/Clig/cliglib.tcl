##############################################################################
# CLIG -- Computational Linguistic's Interactive Grapher
# cliglib.tcl : main.tcl zonder de GUI-elementen van de clig viewer
# 
# Deze file is geschikt om te sourcen vanuit andere applicaties.
# Alleen het noodzakelijke wordt geladen/gedefinieerd.
#
# GJK Wed Oct 12 13:22:51 2005
##############################################################################

###
# constants and global variables
###

# choosing the working directory
# can be set with environment variable CLIG_DIR

if [info exists env(CLIG_DIR)] {set cligdir $env(CLIG_DIR)} \
else {if [info exists argv0] \
        {set cligdir [file dirname $argv0]} \
        else {set cligdir .}}

                 
#
# general preferences
#

set background_color lightgrey ;# white in last version
set highlight_color cornsilk
set button_color grey
set main_background_color lightgrey


set clig_globals(tree_hspace) 16 ;# horizontal space in trees
set clig_globals(tree_vspace) 42 ;# vertical space in trees

# set clig_globals(active_color) blue ;# set to "" when no special color for
                                      ;# clickable objects



# GJK Wed Oct 12 14:02:24 2005: 
#      wanneer wissen we _calcsize_memo (de cache) ?
#      in de huidige staat wordt deze niet automatisch gewist.
#      Het wissen gebeurt/gebeurde alleen met "Clear Stack".
#
#      Moeten we dit oplossen?  Zo ja, hoe?
#
#      Merk op dat dit probleem dus ook al speelde voor de overgang
#      van main.tcl naar cliglib.tcl.  

#
# global varibales: _calcsize_memo (memoizing the sizes), clig_globals
#

set _calcsize_memo("") {0 0}

# main object (for redrawing)

set main_object {text " "}

###
# invisible canvas for size testing
###

canvas .invisible

###
# syntactical sugar and basics
###

proc max {x y} { if {$x < $y} {return $y} else {return $x} }
proc min {x y} { if {$x < $y} {return $x} else {return $y} }


###############################################################################
# (auto) loading rest if the code
###############################################################################

#auto_mkdindex . *.tcl ;# comment out when version is non-test

#set auto_path [linsert $auto_path 0 .]

source $cligdir/graphbox.tcl
source $cligdir/drs.tcl
source $cligdir/trees.tcl
source $cligdir/fs.tcl
# source $cligdir/misc.tcl
source $cligdir/fonts.tcl
# source $cligdir/zoom.tcl

# loading extensions

source $cligdir/extend.tcl


#
# Color on/off
#

set clig_globals(no_colors) 0


# EOF
