##############################################################################
# CLIG -- Computational Linguistic's Interactive Grapher
# main file: loading others and setting up main menu
# v: 1.2
# author karsten konrad, konrad@cs.uni-sb.de
# last update: Nov. 96
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
source $cligdir/misc.tcl
source $cligdir/fonts.tcl
source $cligdir/zoom.tcl

# loading extensions

source $cligdir/extend.tcl

##############################################################################
# graphic stack
##############################################################################

set stack_pointer 0
set max_stack 0

set object_flag("") 0

proc stack-obj {obj} { ;# puts object on stack
        global max_stack object_stack stack_pointer object_flag
        if {[info exists object_flag($obj)]} \
          {set stack_pointer $object_flag($obj)} \
        else \
          {set max_stack [expr $max_stack+1]
           set stack_pointer $max_stack
           set object_stack($max_stack) $obj
           set object_flag($obj) $max_stack}}

proc stack-forward {} { ;# displays next object
        global stack_pointer max_stack object_stack
        if {$stack_pointer<$max_stack} \
                {set stack_pointer [expr $stack_pointer+1]}
        if {$stack_pointer>0} {show $object_stack($stack_pointer)}}

proc stack-backward {} { ;# displays previous object
        global stack_pointer max_stack object_stack
        if {$stack_pointer>1} \
                {set stack_pointer [expr $stack_pointer-1]}
        if {$stack_pointer>0} {show $object_stack($stack_pointer)}}

proc stack-top {} { ;# displays top object
        global stack_pointer max_stack object_stack
        if {$max_stack>0} \
                {set stack_pointer 1
                 show $object_stack($stack_pointer)}}


###############################################################################
# main graphical routine
###############################################################################

#
# define graph canvas -- different in tk3.6 and tk4.0
#

proc define-gr-canvas {} { ;# canvas deifintion varies with version
        global tk_version background_color
        if {$tk_version>=4} {
         canvas .graphics -bg $background_color -borderwidth 0 \
                          -highlightthickness 0 \
                          -scrollregion {0 0 10000 10000} \
                          -yscrollcommand ".ygraph set" \
                          -xscrollcommand ".xgraph set"} \
        else {
          canvas .graphics -bg $background_color -borderwidth 0 \
                          -scrollregion {0 0 10000 10000} \
                          -yscrollcommand ".ygraph set" \
                          -xscrollcommand ".xgraph set"}}

#
# memoization table update
#


set clig_globals(old_tree_vspace) 0
set clig_globals(old_tree_hspace) 0
set clig_globals(old_fontsize) 0

proc size-changed {} { ;# returns !=0 iff user has changed global vars
        global clig_globals
        ;# changed old values?
        set change [expr ($clig_globals(tree_vspace)!=\
                                $clig_globals(old_tree_vspace))||\
                         ($clig_globals(tree_hspace)!=\
                                $clig_globals(old_tree_hspace))||\
                         ($clig_globals(fontsize)!=\
                                $clig_globals(old_fontsize))]
        set clig_globals(old_tree_hspace) $clig_globals(tree_hspace)
        set clig_globals(old_tree_vspace) $clig_globals(tree_vspace)
        set clig_globals(old_fontsize) $clig_globals(fontsize)
        return $change}

#
# show a graph -- no change in graph stack
#

proc show {obj} { ;# destroys graphical top and displays obj
        global background_color _calcsize_memo clig_globals main_object \
                graph_scale
        change-source-text $obj ;# new object editing
        # if ([size-changed]) {unset _calcsize_memo}
        # memoization table gets deleted when tree spaces changed
        
        # waiting clock display
        . configure -cursor watch
        update idletasks

        ## puts [time {set size [execobj $obj 0 0 .graphics {}]} 1]

        # delete all objects (all have tag cmg)
	.graphics delete cmg
	
        set size [execobj $obj 0 0 .graphics {"cmg"}]

        ## cmg is the tag all objects MUST have !!!

        # scaling
        set scaling [scale-graph $graph_scale .graphics]

        # fit canvas to size
        
        set width [expr [lindex $size 0]*$scaling+1]
        set height [expr [lindex $size 1]*$scaling+1]

        .graphics configure -width $width
        .graphics configure -height $height
        .graphics configure -scrollregion [list 0 0 $width $height]
        
        # switch on cursor change        
        . configure -cursor {}      
        update idletasks
        }



proc clig {obj} { ;# main routine
        stack-obj $obj
        show $obj}

###############################################################################
# menu buttons
###############################################################################

frame .leftframe -bg $main_background_color
frame .rightframe -bg $main_background_color

frame .tm -relief raised -bd 2

menubutton .tm.file -text "File" -underline 0 -menu .tm.file.menu
menubutton .tm.view -text "View" -underline 0 -menu .tm.view.menu

menubutton .tm.help -text "Help" -underline 0 -menu .tm.help.menu

menu .tm.file.menu
menu .tm.view.menu
menu .tm.help.menu

#
# stack buttons
#

button .tm.forward -text "\256" -font $sfont -command {stack-forward} \
        -borderwidth 1
button .tm.back -text "\254" -font $sfont -command {stack-backward} \
        -borderwidth 1
#button .tm.top -text "Top" -underline 0  -command {stack-top} 

# key binding

bind . <Left> {stack-backward}
bind . <Right> {stack-forward}



#
# File menu
#

.tm.file.menu add command -label "New Window" -underline 0 \
         -command {exec $cligdir/clig &}

.tm.file.menu add separator

.tm.file.menu add command -label "Load..." -underline 0 \
         -command {load-clig-file}

.tm.file.menu add command -label "Save..." -underline 0 \
         -command {save-clig-file $object_stack($stack_pointer)}

.tm.file.menu add separator

.tm.file.menu add command -label "Print..." -underline 0 \
        -command {open-print-menu}

.tm.file.menu add separator

.tm.file.menu add command -label "Editor..." -underline 0 \
        -command {open-source-window $main_object}

.tm.file.menu add separator
.tm.file.menu add command -label "Exit" -underline 1 -command {destroy .}

#
# View menu
#

.tm.view.menu add command -label "Redisplay" -underline 0 \
         -command {show $object_stack($stack_pointer)}

.tm.view.menu add command -label "Fit Window" -underline 4 \
         -command {wm geometry . {}}

.tm.view.menu add separator

#
# Text shown in Program
#

set copyright \
    [list color-area ivory \
     [list bigbox [list stack [list bold-text \
             "Computational Linguistics Interactive Grapher"] \
                       [list Seq [list bitmap $cligdir/title.pic] \
              {Stack {plain-text "(c) FraCaS 1995/96"} \
                     {plain-text "V1.4 (public release)"} \
                     {color DarkGreen {underline {plain-text \
                                 "Please do distribute"}}}}]]]]

set author \
[list color-area ivory \
        [list bigbox [list stack [list seq [list plain-text "Karsten Konrad"] \
                        [list bitmap $cligdir/konrad.pic]] \
              [list plain-text "Computerlinguistik, Bau 17.2"] \
              [list plain-text "Universitaet des Saarlandes"] \
              [list plain-text "Postfach 1150, 66041 Saarbruecken"] \
              [list plain-text "email: konrad@coli.uni-sb.de"] \
              [list plain-text "phone:  +49 (681) 302 4496"]]]]

set object_stack(0) $copyright

.tm.view.menu add command -label "Clear Stack" -underline 0 \
        -command {set stack_pointer 0; set max_stack 0; \
                  unset _calcsize_memo
                  unset object_flag; set object_flag("") 0 
                  show $copyright}

#
# preferences 
###############

.tm.view.menu add separator

#
# Zooming
#

.tm.view.menu add cascade -label "Zooming" -underline 0 \
        -menu .tm.view.menu.zoom

menu .tm.view.menu.zoom

.tm.view.menu.zoom add command  -label "Smaller" \
        -command {if ($graph_scale>2) {
                        unscale $graph_scale .graphics
                        set graph_scale [expr $graph_scale-2]
                        set scaling [scale-graph $graph_scale .graphics]
                        fit-to-scale $scaling .graphics}}
.tm.view.menu.zoom add command  -label "Bigger" \
        -command {if ($graph_scale<32) {
                        unscale $graph_scale .graphics
                        set graph_scale [expr $graph_scale+2]
                        set scaling [scale-graph $graph_scale .graphics]
                        fit-to-scale $scaling .graphics}}
.tm.view.menu.zoom add separator
.tm.view.menu.zoom add command  -label "Normal (100%)" \
        -command {set graph_scale [expr $clig_globals(fontsize)]
                   show $main_object}

# same commands for + and -
#
# Color on/off
#

.tm.view.menu add separator

set clig_globals(no_colors) 0

.tm.view.menu add checkbutton -label "No Colors" -underline 0 \
        -variable clig_globals(no_colors) -state active

#
# Help menu
#

.tm.help.menu add command -label "Some hints" -underline 0 \
        -command {source $cligdir/help.tcl}

.tm.help.menu add separator

.tm.help.menu add command -label "About Clig" -underline 1 \
        -command {show $copyright}
.tm.help.menu add command -label "Clig's Author" -underline 7 \
        -command {show $author}

#
# open top window
#

proc open-clig {} { ;# opens the clig window
        global background_color main_background_color clig_globals
        . configure -bg $main_background_color
        wm minsize . 1 1 ;# top-window is resizable

        scrollbar .ygraph -command ".graphics yview" -width 10
        scrollbar .xgraph -command ".graphics xview" -width 10 \
                -orient horizontal
        
        define-gr-canvas ;# defining the graphics canvas
        pack .tm -fill x
        pack .tm.file .tm.view -side left -fill x
        pack .tm.forward .tm.back .tm.help -side right
        pack .leftframe -side top -anchor sw

	pack .ygraph -side right -fill y
        pack .xgraph -fill x -side bottom
        pack .graphics -padx 3 -pady 1
        

        .graphics bind clickable <Enter> \
                ". configure  -cursor {hand2 black white}"
        .graphics bind clickable <Leave> \
                 ". configure  -cursor {}"
        zoom-initialize}





