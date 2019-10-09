#########################
##### auxiliaries #######
#########################

# all kind of hacks. 

set current_cmd_no 0
set last_cmd_no 0
set search {}
set i 0
set flag(number_of_objects) 0

proc hdrug_select_obj {} {
    set current [prolog_var hdrug_util:hdrug_flag(current_no,C) C]
    if {$current == 1} {return 0}
    if {$current == 2} {return 1}
    set i 1
    set j {tk_dialog .d Select Object {} 1 cancel}
    while {$i < $current} {
	lappend j $i
	incr i
    }
    return [eval $j]
}

proc show_object_tree {type output} {
    set obj [hdrug_select_obj]
    if {$obj != 0} {prolog hdrug_show:show_object_no($obj,tree($type),$output)}
}

proc hdrug_delete_objects {} {
    global flag
    foreach w [winfo children .bb.o] { catch "destroy $w" }
    catch { destroy .bb.text .bb.label .bb.omax .bb.omaxv }
    set flag(number_of_objects) 0
}

# this sets var to val only if var is undefined
proc setd {var val} {
    upvar $var v
    if ![info exists v] {set v $val}
}


# envVal envValName
#   Looks up the envValName environment variable and returns its
#   value, or {} if it does not exists
proc envVal {envValName} {
    global env
    if [info exists env($envValName)] {return $env($envValName)} {return {}}
}

# loadAppDefaults classNameList ?priority?
#   Searches for the app-default files corresponding to classNames in
#   the order specified by X Toolkit Intrinsics, and loads them with
#   the priority specified (default: startupFile).
proc loadAppDefaults {classNameList {priority startupFile}} {
    set filepath "[split [envVal XUSERFILESEARCHPATH] :] \
                [envVal XAPPLRESDIR] \
                [split [envVal XFILESEARCHPATH] :] \
                [envVal HOME]/lib/X11/app-defaults \
		/usr/local/lib/X11/app-defaults \
                /usr/lib/X11/app-defaults"
    foreach i $classNameList {
	foreach j $filepath {
	    if {[file exists $j/$i]} {
		option readfile $j/$i $priority
		puts "loaded $j/$i"
		break
	    }
	}
    }
}


# w is assumed to be a canvas (hopefully with scrollbars)
proc start_sub_fs {w {ext f}} {
    #  global hdrug_library
    catch {destroy $w.$ext}
    frame $w.$ext
    $w create window 10 10 -window $w.$ext -anchor nw
    return $w.$ext
}

# not used anymore?
# w is assumed to be a canvas (hopefully with scrollbars)
proc new_start_sub_fs {w {ext f}} {
    #  global hdrug_library
    catch {destroy $w.$ext}
    frame $w.$ext
    $w create window 10 10 -window $w.$ext -anchor nw
    return $w.$ext
}

# not used anymore?
# w is assumed to be a canvas (hopefully with scrollbars)
proc n_start_sub_fs {w {ext f}} {
    if ![winfo exists $w.$ext] then {
	empty_canvas $w 
	frame $w.$ext
	$w create window 10 10 -window $w.$ext -anchor nw
    }
    return $w.$ext
}

proc empty_canvas {w} {
    $w delete all
    foreach child [winfo children $w] {
	destroy $child
    }
    $w yview moveto 0
}
# creates a toplevel window w consisting of a scrollable canvas and
# a `OK' button which destroys that canvas
proc create_top_canvas {w} {
    global hdrug_library
    toplevel $w
    wm iconbitmap $w @$hdrug_library/bitmaps/hdrug.xbm
    wm title $w Hdrug-$w
    wm iconname $w Hdrug-$w
    wm minsize $w 600 600
    frame $w.f
    pack $w.f -fill both -expand 1
    frame $w.f.t  
    frame $w.f.b
    pack $w.f.t -fill both -expand 1
    pack $w.f.b -fill x
    button $w.f.b.b -text Ok -command "destroy $w"
    pack $w.f.b.b -fill x
    canvas $w.f.t.c -bd 2 -yscrollcommand "$w.f.t.y set" \
	-xscrollcommand "$w.f.t.x set" \
	-scrollregion "-30 -30 199970 199970"
    scrollbar $w.f.t.x -orient horiz -command "$w.f.t.c xview"
    scrollbar $w.f.t.y -command "$w.f.t.c yview"
    pack $w.f.t.x -side bottom -fill x
    pack $w.f.t.y -side left -fill y
    pack $w.f.t.c  -fill both -expand 1
    return $w.f.t.c
}

#################
### ObjectBar ###
#################

proc hdrug_object {no} {
    # should not be necc:
    catch "destroy .bb.o.obj$no"

    global generator_exists treedefs features module

    menubutton .bb.o.obj$no -menu .bb.o.obj$no.a -text "$no"  
    bind .bb.o.obj$no <2> "prolog hdrug:show_object_default2($no)"
    bind .bb.o.obj$no <3> "prolog hdrug:show_object_default3($no)"

    pack .bb.o.obj$no -side left
    menu .bb.o.obj$no.a
    .bb.o.obj$no.a add cascade -label "View" \
	-menu .bb.o.obj$no.a.show
    menu .bb.o.obj$no.a.show


    .bb.o.obj$no.a.show add cascade -label "Clig" \
	-menu .bb.o.obj$no.a.show.clig
    catch {.bb.o.obj$no.a.show.clig delete 0 last}
    menu .bb.o.obj$no.a.show.clig
    foreach i $treedefs {
	.bb.o.obj$no.a.show.clig add command -label Tree($i) \
	    -command "prolog hdrug_show:show_object_no($no,tree($i),clig)"
    }

    .bb.o.obj$no.a.show add cascade -label "DOT" \
	-menu .bb.o.obj$no.a.show.dot
    catch {.bb.o.obj$no.a.show.dot delete 0 last}
    menu .bb.o.obj$no.a.show.dot
    foreach i $treedefs {
	.bb.o.obj$no.a.show.dot add command -label Tree($i) \
	    -command "prolog hdrug_show:show_object_no($no,tree($i),dot)"
    }

    .bb.o.obj$no.a.show.clig add command -label Matrix \
	-command "prolog hdrug_show:show_object_no($no,fs,clig)"
    if !$features {
	.bb.o.obj$no.a.show.clig entryconfigure last -state disabled
    }


    .bb.o.obj$no.a.show add cascade -label "Tk" \
	-menu .bb.o.obj$no.a.show.canvas
    menu .bb.o.obj$no.a.show.canvas
    catch {.bb.o.obj$no.a.show.canvas delete 0 last}
    foreach i $treedefs {
	.bb.o.obj$no.a.show.canvas add command -label Tree($i) \
	    -command "prolog hdrug_show:show_object_no($no,tree($i),tk)"
    }
    .bb.o.obj$no.a.show.canvas add command -label Matrix \
	-command "prolog hdrug_show:show_object_no($no,fs,tk)"
    if !$features {
	.bb.o.obj$no.a.show.canvas entryconfigure last -state disabled
    }
    .bb.o.obj$no.a.show.canvas add command -label Text \
	-command "prolog hdrug_show:show_object_no($no,term(print),tk)"
    .bb.o.obj$no.a.show.canvas add command -label Semantics \
	-command "prolog hdrug_show:show_object_no($no,sem,tk)"
    .bb.o.obj$no.a.show.canvas add command -label Phonology \
	-command "prolog hdrug_show:show_object_no($no,words,tk)"

    .bb.o.obj$no.a.show add cascade -label "Prolog" \
        -menu .bb.o.obj$no.a.show.prolog
    menu .bb.o.obj$no.a.show.prolog
    catch {.bb.o.obj$no.a.show.prolog delete 0 last}
    foreach i $treedefs {
	.bb.o.obj$no.a.show.prolog add command -label Tree($i) \
	    -command "prolog hdrug_show:show_object_no($no,tree($i),user)"
    }
    .bb.o.obj$no.a.show.prolog add command -label Semantics \
	-command "prolog hdrug_show:show_object_no($no,sem,user)"
    .bb.o.obj$no.a.show.prolog add command -label Phonology \
	-command "prolog hdrug_show:show_object_no($no,words,user)"
    .bb.o.obj$no.a.show.prolog add command -label Matrix \
	-command "prolog hdrug_show:show_object_no($no,fs,user)"
    if !$features {
	.bb.o.obj$no.a.show.prolog entryconfigure last -state disabled
    }
    .bb.o.obj$no.a.show.prolog add command -label Text \
	-command "prolog hdrug_show:show_object_no($no,term(print),user)"
    
    .bb.o.obj$no.a.show add cascade -label "LaTeX" \
	-menu .bb.o.obj$no.a.show.latex
    catch {.bb.o.obj$no.a.show.latex delete 0 last}
    menu .bb.o.obj$no.a.show.latex
    foreach i $treedefs {
	.bb.o.obj$no.a.show.latex add command -label Tree($i) \
	    -command "prolog hdrug_show:show_object_no($no,tree($i),latex)"
    }
    .bb.o.obj$no.a.show.latex add command -label Matrix \
	-command "prolog hdrug_show:show_object_no($no,fs,latex)"
    if !$features {
	.bb.o.obj$no.a.show.latex entryconfigure last -state disabled
    }
    .bb.o.obj$no.a.show.latex add command -label Text \
	-command "prolog hdrug_show:show_object_no($no,term(print),latex)"
    .bb.o.obj$no.a.show.latex add command -label Semantics \
	-command "prolog hdrug_show:show_object_no($no,sem,latex)"
    .bb.o.obj$no.a.show.latex add command -label Phonology \
	-command "prolog hdrug_show:show_object_no($no,words,latex)"



    .bb.o.obj$no.a add cascade -label "Generate" -menu .bb.o.obj$no.a.generate
    if !$generator_exists {
	.bb.o.obj$no.a entryconfigure last -state disabled
    }
    menu .bb.o.obj$no.a.generate
    .bb.o.obj$no.a.generate add command -label Generate \
	-command "prolog $module:generate_obj_no($no)"
    .bb.o.obj$no.a.generate add command -label "Compare Generators" \
	-command "prolog $module:generate_compare_object($no)"


    prolog hdrug:create_object_hook('.bb.o.obj$no.a',$no)
    update
}

proc menu_flag {w flagname values} {
    $w add cascade -label $flagname -menu $w.$flagname
    menu $w.$flagname -postcommand "menu_flag_post $w $flagname {$values}"
}

proc menu_flag_post {w flagname values} {
    global flag
    catch {$w.$flagname delete 0 last}
    foreach value $values {
	$w.$flagname add radiobutton -label $value \
	    -value $value -variable flag($flagname)\
	    -command "prolog hdrug_util:hdrug_flag($flagname,_,$value)"
    }
    $w.$flagname add command -label Help -command "prolog hdrug_gui:tk_help_flag($flagname)"
}

proc help_line {w msg} {
    if [option get . helpline Helpline] {
	if [winfo exists $w] {
            setBalloonHelp $w $msg
        }
   }
}

proc showStatus {msg} {
    catch {destroy .status}
    toplevel .status
    wm geometry .status +[expr 30+[winfo pointerx .]]+[winfo pointery .]
    wm overrideredirect .status 1
    label .status.l -text $msg -relief sunken -bd 3 -padx 2 -pady 2
    pack .status.l
}

proc hideStatus {} {
    catch {destroy .status}
}

proc send_url_to_netscape {url} {
    exec [envVal BROWSER] $url & 
}

proc display_text {w title text {header {}}} {
    catch {destroy $w}
    toplevel $w
    wm title $w $title

    frame $w.bot -relief raised -bd 1
    pack $w.bot -side bottom -fill both


    text $w.text -wrap word -relief flat -bd 10\
       -yscrollcommand "$w.scroll set" -setgrid true -width 70 -height 20
    scrollbar $w.scroll  -command "$w.text yview"
    pack $w.scroll -side right -fill y
    pack $w.text -expand yes -fill both

    $w.text tag configure color2 -foreground red
    
    $w.text insert 0.0 $header color2
    $w.text insert end "\n\n"
    $w.text insert end $text color1
    $w.text configure -state disabled
    button $w.bot.button -command "destroy $w" -text {OK}
    pack $w.bot.button
}

# Asks for some user input. The user can give input in an entry
# widget, or he can click in a listbox on one of the pre-defined
# input values. The listbox is only present if lvar is an array
# and lvar(max) is defined, and larger than 0. The value is going to
# returned as a sequence, separated by commas, where each element is 
# properly escaped in order to allow reading each element of the sequence 
# as a Prolog atom. So it's a Prolog list of atoms without the [ and ]; or
# it's just a single atom. Example: if you type 
# john didn't love mary
# you will get:
# {{john},{didn\'t},{love},{mary}}
#
proc hdrug_qbox_array {w lvar title text} {
    global answer
    global hdrug_library
    upvar $lvar arrayvar

    # 1. Create the top-level window and divide it into top
    # and bottom parts.

    set answer {}
    catch {destroy $w}
    toplevel $w -class Dialog
    wm iconbitmap $w @$hdrug_library/bitmaps/hdrug.xbm
    wm minsize $w 0 0
    wm title $w $title
    wm iconname $w Question
    frame $w.top  -bd 1
    pack $w.top -side top -fill both -expand 1
    entry $w.mid -bd 1 -textvariable answer -bg white
    pack $w.mid -side top -padx 3m -fill both -expand yes
    if [info exists arrayvar(max)] {
	frame $w.frame
	pack $w.frame -side top -expand yes -fill y -fill x
	scrollbar $w.frame.scroll -command "$w.frame.list yview"
	scrollbar $w.frame.xscroll  -command "$w.frame.list xview" \
	    -orient horiz
	listbox $w.frame.list -yscroll "$w.frame.scroll set" -relief sunken \
	    -xscroll "$w.frame.xscroll set" -width 70 -height 10
	pack $w.frame.xscroll -side bottom -fill x
	pack $w.frame.scroll -side right -fill y
	pack $w.frame.list -side left -expand yes -fill both
	set i 0
	while {$i < $arrayvar(max)} {
	    $w.frame.list insert $i $arrayvar($i)
	    set i [expr $i+1]
	}
	bind $w.frame.list <Double-1> {
	    set answer [join [selection get] " "] 
	    set ready 1
	} 
    }
    frame $w.bot  -bd 1
    pack $w.bot -side bottom -fill both
    bind $w.mid <Return> {set ready 1}

    # 2. Fill the top part with message.

    message $w.msg -text $text -width 5i
    pack $w.msg -in $w.top -side right -fill both -padx 1m -pady 1m -expand 1

    # 3. Create a row of buttons at the bottom of the dialog.

    button $w.button -text "Cancel" -command "set answer 0 ; set ready 1"
    button $w.buttonok -text "Ok" -command "set ready 1"
    pack $w.button $w.buttonok -in $w.bot -side left -expand 1 \
	-padx 3m -pady 3m -ipadx 2m -ipady 1m

    # 4. Set focus. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.

    set oldFocus [focus]
    focus $w
    tkwait variable ready
    catch {if {$answer == ""} {set answer [join [selection get] " "] }}
    destroy $w
    focus $oldFocus
    if {$answer == 0} {return 0} 

    # 5. do some hacking to treat ' inside atoms....
    # so. Isn't this really great? I mean isn\'t.
    regsub -all "\\\\" $answer "\\\\\\\\" answer
    regsub -all "%"   $answer "%%"     answer
    regsub -all '   $answer "\\\\'"     answer
    regsub -all " " $answer "'\} \{'" answer
    set answer [format "\{'$answer'\}" ]
    return [join $answer ,]
}



proc hdrug_qbox_simple {w title text} {
    global answer
    global hdrug_library

    # 1. Create the top-level window and divide it into top
    # and bottom parts.

    set answer {}
    catch {destroy $w}
    toplevel $w -class Dialog
    wm iconbitmap $w @$hdrug_library/bitmaps/hdrug.xbm
    wm minsize $w 0 0
    wm title $w $title
    wm iconname $w Question
    frame $w.top  -bd 1
    pack $w.top -side top -fill both -expand 1
    entry $w.mid -bd 1 -textvariable answer -bg white
    pack $w.mid -side top -padx 3m -fill both -expand yes
    frame $w.bot  -bd 1
    pack $w.bot -side bottom -fill both
    bind $w.mid <Return> {set ready 1}

    # 2. Fill the top part with message.

    message $w.msg -text $text -width 5i
    pack $w.msg -in $w.top -side right -fill both -padx 1m -pady 1m -expand 1

    # 3. Create a row of buttons at the bottom of the dialog.

    button $w.button -text "Cancel" -command "set answer 0 ; set ready 1"
    button $w.buttonok -text "Ok" -command "set ready 1"
    pack $w.button $w.buttonok -in $w.bot -side left -expand 1 \
	-padx 3m -pady 3m -ipadx 2m -ipady 1m

    # 4. Set focus. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.

    set oldFocus [focus]
    focus $w
    tkwait variable ready
    destroy $w
    focus $oldFocus
    if {$answer == 0} {return 0} 

    return "$answer"
}

# taken from http://wiki.tcl.tk/16317
proc setBalloonHelp {w msg args} {
     array set opt [concat {
         -tag ""
     } $args]
     if {$msg ne ""} then {
         set toolTipScript\
             [list showBalloonHelp %W [string map {% %%} $msg]]
         set enterScript [list after 1000 $toolTipScript]
         set leaveScript [list after cancel $toolTipScript]
         append leaveScript \n [list after 200 [list destroy .balloonHelp]]
     } else {
         set enterScript {}
         set leaveScript {}
     }
     if {$opt(-tag) ne ""} then {
         switch -- [winfo class $w] {
             Text {
                 $w tag bind $opt(-tag) <Enter> $enterScript
                 $w tag bind $opt(-tag) <Leave> $leaveScript
             }
             Canvas {
                 $w bind $opt(-tag) <Enter> $enterScript
                 $w bind $opt(-tag) <Leave> $leaveScript
             }
             default {
                 bind $w <Enter> $enterScript
                 bind $w <Leave> $leaveScript
             }
         }
     } else {
         bind $w <Enter> $enterScript
         bind $w <Leave> $leaveScript
     }
 }

 proc showBalloonHelp {w msg} {
     set t .balloonHelp
     catch {destroy $t}
     toplevel $t -bg black
     wm overrideredirect $t yes
     if {$::tcl_platform(platform) == "macintosh"} {
         unsupported1 style $t floating sideTitlebar
     }
     pack [label $t.l -text [subst $msg] -bg yellow -font {lucidasans 9}]\
         -padx 1\
         -pady 1
     set width [expr {[winfo reqwidth $t.l] + 2}]
     set height [expr {[winfo reqheight $t.l] + 2}]
     set xMax [expr {[winfo screenwidth $w] - $width}]
     set yMax [expr {[winfo screenheight $w] - $height}]
     set x [winfo pointerx $w]
     set y [expr {[winfo pointery $w] + 20}]
     if {$x > $xMax} then {
         set x $xMax
     }
     if {$y > $yMax} then {
         set y $yMax
     }
     wm geometry $t +$x+$y
     set destroyScript [list destroy .balloonHelp]
     bind $t <Enter> [list after cancel $destroyScript]
     bind $t <Leave> $destroyScript
 }

 # demo
 # setBalloonHelp .b "Text that describes\nwhat the button does"

############################ START-UP #########################

#######################
### initializations ###
#######################

setd HDRUG Hdrug

set hdrugwidth [expr [winfo screenwidth .] - 160]
if [expr $hdrugwidth > 1600 ] {
    set hdrugwidth 1600
}

set hdrugheight [expr [winfo screenheight .] - 160]
if [expr $hdrugheight > 800 ] {
    set hdrugheight 800
}

append hdruggeometry $hdrugwidth "x" $hdrugheight

wm geometry . $hdruggeometry
wm iconbitmap . @$hdrug_library/bitmaps/hdrug.xbm
wm title . $HDRUG
wm iconname . $HDRUG

set module [prolog_var hdrug_util:hdrug_flag(gui_calling_module,Module) Module]

set parser_exists [prolog_q {hdrug_util:hdrug_flag(parser)}]
set generator_exists [prolog_q {hdrug_util:hdrug_flag(generator)}]
setd objs 0
set treedefs [prolog_all hdrug_gui:a_treedef(Var) Var]
set features [prolog_q hdrug_util:hook(hdrug_feature:define_type(_,_,_,_,_))]

setd use_canvas 0

# `defaults' for application-defaults
option readfile $hdrug_library/Hdrug.ad 80

# user/system specific application-defaults, 
# possibly overwriting default application defaults
loadAppDefaults Hdrug 100

# Hdrug-application specific application defaults...
loadAppDefaults $HDRUG 100

########################
### The main widgets ###
########################

# MenuBar
frame .menu 
pack .menu -side top -fill x 

# ObjectBar
frame .bb  -height 1c
frame .bb.o -height 1c
pack .bb -fill x

button .bb.l -bitmap @$hdrug_library/bitmaps/l.bm  -command {
    .cv.canvas configure -width \
	[expr [lindex [.cv.canvas configure -width] 4] + \
 	      [lindex [.cw.canvas configure -width] 4]] 
    .cw.canvas configure -width 0
    pack forget .cw .ccc
    pack .cv -in .cu -side left -fill both -expand 1
} 
button .bb.r -bitmap @$hdrug_library/bitmaps/r.bm -command {
    .cw.canvas configure -width \
	[expr [lindex [.cv.canvas configure -width] 4] + \
 	      [lindex [.cw.canvas configure -width] 4]] 
    .cv.canvas configure -width 0
    pack forget .cv .ccc
    pack .cw -in .cu -side left -fill both -expand 1
} 
button .bb.d -bitmap @$hdrug_library/bitmaps/both.bm -command {
    set total [expr [lindex [.cv.canvas configure -width] 4] + \
 	      [lindex [.cw.canvas configure -width] 4]]
    .cw.canvas configure -width [expr $total/2]
    .cv.canvas configure -width [expr $total/2]
    pack forget .cw .ccc .cv
    pack .cv .ccc .cw -in .cu -side left -fill both -expand 1
    pack configure .ccc -expand 0
} 
label .bb.w -width 1 -height 2
pack .bb.l .bb.r .bb.d .bb.w .bb.o -side left

# 2 Canvases
frame .cu 
frame .cv
frame .cw
frame .ccc -width .2c  
pack .cu -expand 1 -fill both
pack .cv .ccc .cw -in .cu -side left -fill both -expand 1 
pack configure .ccc -expand 0


##.ccc is used to interactively drag the two canvases into their desired size
  bind .ccc <Enter> ".ccc configure -background black"
  bind .ccc <Leave> ".ccc configure -background [lindex [.ccc configure -bg] 4]"
  bind .ccc <1> "set cccx %x"
  bind .ccc <ButtonRelease-1> {
      .cv.canvas configure -width \
  	[expr [lindex [.cv.canvas configure -width] 4] + [expr %x - $cccx]]
      .cw.canvas configure -width \
  	[expr [lindex [.cw.canvas configure -width] 4] - [expr %x - $cccx]]
      set cccx %x
  }

# ButtonBar at the bottom
frame .t 
pack .t -side bottom -expand 0 -fill x

############
## Canvas ##
############


canvas .cv.canvas -bd 2\
        -confine 1\
        -width [expr [winfo width .]/2 - 50]\
        -height [expr [winfo height .]/2-50]\
    -yscrollcommand ".cv.sy set" -xscrollcommand ".cv.sx set"\
    -scrollregion {-30 -30 10000 10000000}


canvas .cw.canvas -bd 2\
     -confine 1\
     -width [expr [winfo width .]/2 - 50]\
     -height [expr [winfo height .]/2-50]\
    -yscrollcommand ".cw.sy set" -xscrollcommand ".cw.sx set"\
    -scrollregion {-30 -30 10000 10000000}

prolog "hdrug_util:set_flag(canvas,'.cv.canvas')"
prolog "hdrug_util:set_flag(canvas2,'.cw.canvas')"

## the canvases have scrollbars
scrollbar .cv.sx -orient horiz -command ".cv.canvas xview"
scrollbar .cv.sy -command ".cv.canvas yview"
scrollbar .cw.sx -orient horiz -command ".cw.canvas xview"
scrollbar .cw.sy -command ".cw.canvas yview"
pack .cv.sx -side bottom -fill x
pack .cv.sy -side left -fill y
pack .cv.canvas -fill both -expand 1
pack .cw.sx -side bottom -fill x
pack .cw.sy -side right -fill y
pack .cw.canvas -fill both -expand 1

bind .cv.canvas <2> ".cv.canvas scan mark %x %y"
bind .cv.canvas <B2-Motion> ".cv.canvas scan dragto %x %y"
bind .cv.canvas <3> "empty_canvas .cv.canvas"
bind .cw.canvas <2> ".cw.canvas scan mark %x %y"
bind .cw.canvas <B2-Motion> ".cw.canvas scan dragto %x %y"
bind .cw.canvas <3> "empty_canvas .cw.canvas"

#############
## MenuBar ##
#############

# Menu File
menubutton .menu.file -text "File" -underline 0 -menu .menu.file.m 
menu .menu.file.m

.menu.file.m add command -label "Compile default grammar" \
    -command {
	prolog $module:compile_grammar
	prolog hdrug_gui:update_types
	prolog hdrug_gui:update_preds
    } -underline 0 -accelerator {F C}
.menu.file.m add command -label "Reconsult default grammar" \
    -command {
	prolog $module:reconsult_grammar
	prolog hdrug_gui:update_types
	prolog hdrug_gui:update_preds
    } -underline 0 -accelerator {F R}
.menu.file.m add command -label "Compile grammar file"\
    -command {
	set filename [tk_getOpenFile]
	if {"$filename" != ""} {
	    prolog $module:compile_grammar_file('$filename')
	    prolog hdrug_gui:update_types
	    prolog hdrug_gui:update_preds
	}
    }
.menu.file.m add command -label "Reconsult grammar file" \
    -command {
	set filename [tk_getOpenFile]
	if {"$filename" != ""} {
	    prolog $module:reconsult_grammar_file('$filename') 
	    prolog hdrug_gui:update_types
	    prolog hdrug_gui:update_preds
	}
    }

.menu.file.m add separator
.menu.file.m add command -label "Compile Prolog file" -underline 8 \
    -command {
	set filename [tk_getOpenFile -defaultextension .pl]
	if {"$filename" != ""} {
	    prolog compile('$filename')
	}
    } -accelerator {F P}

.menu.file.m add command -label "Reconsult Prolog file" \
    -command {
	set filename [tk_getOpenFile -defaultextension .pl]
	if {"$filename" != ""} {
	    prolog reconsult('$filename')
	}
    }

.menu.file.m add command -label "Source TK/Tcl file" -underline 0 \
    -command {
	set filename [tk_getOpenFile -defaultextension .pl]
	if {"$filename" != ""} {
	    source $filename
	}
    } -accelerator {F S}

.menu.file.m add separator
.menu.file.m add command -label "Enlarge Left Canvas" \
    -command {
	.cv.canvas configure -width \
            [expr [lindex [.cv.canvas configure -width] 4]+50]
	.cw.canvas configure -width \
            [expr [lindex [.cw.canvas configure -width] 4]-50]
    }

.menu.file.m add command -label "Enlarge Right Canvas" \
    -command {
	.cv.canvas configure -width \
            [expr [lindex [.cv.canvas configure -width] 4]-50]
	.cw.canvas configure -width \
            [expr [lindex [.cw.canvas configure -width] 4]+50]
    }

.menu.file.m add command -label "Clear Left Canvas" \
    -command { empty_canvas .cv.canvas }

.menu.file.m add command -label "Clear Right Canvas" \
    -command { empty_canvas .cw.canvas }

.menu.file.m add separator
if [prolog_q current_prolog_flag(language,sicstus)] {
    .menu.file.m add command -label "Restart X" -underline 8 \
	-command {prolog hdrug_gui:really_restart_x} -accelerator {F X}
}

if [prolog_q current_prolog_flag(language,sicstus)] {
    .menu.file.m add command -label "Restart X with TkConsol"  \
	-command {prolog hdrug_util:set_flag(tkconsol,on); prolog hdrug_gui:really_restart_x} 
}

.menu.file.m add command -label "Quit X (keep Prolog alive)" \
    -underline 0 -command {prolog hdrug_gui:halt_x} -accelerator {F Q}

if [prolog_q current_prolog_flag(language,sicstus)] {
    .menu.file.m add command -label "Halt (both X and Prolog)" -underline 0 \
	-command { exit } -accelerator {F H}
}

# Menu Debug
menubutton .menu.debug -text "Debug" -underline 0 -menu .menu.debug.m
menu .menu.debug.m
.menu.debug.m add command -label "Nodebug" -underline 0 \
    -command {prolog nodebug} -accelerator {D N}
.menu.debug.m add command -label "Debug" \
    -command {prolog debug} -underline 0 -accelerator {D D}
.menu.debug.m add command -label "Remove spypoints" \
    -command {prolog nospyall} -underline 0 -accelerator {D R}
.menu.debug.m add command -label "Spy predicate" -underline 0\
    -command {
	prolog "hdrug_gui:spy_atom([hdrug_qbox_array .g question {Predicate to spy:} {}])"
    } -accelerator {D S}
.menu.debug.m add command -label "Unspy predicate" -underline 0 \
    -command {
	prolog "hdrug_gui:nospy_atom([hdrug_qbox_array .g question {Predicate to unspy:} {}])"
    } -accelerator {D U}
.menu.debug.m add separator
.menu.debug.m add command -label "Statistix" \
    -command {prolog statistics} -underline 8 -accelerator {D X}

# Menu Hdrug
menubutton .menu.hdrug -text "Hdrug" -underline 0 -menu .menu.hdrug.m
menu .menu.hdrug.m

# Menu Options
menubutton .menu.options -text "Options" -underline 0 -menu .menu.options.m
menu .menu.options.m

# Menu Parse
menubutton .menu.parse -text "Parse" -underline 0 -menu .menu.parse.m
if !$parser_exists {.menu.parse configure -state disabled} 

menu .menu.parse.m
.menu.parse.m add command -label "Parse sentence" -command {
    set sent [hdrug_qbox_array .p sents Question\
              "         Select a Sentence         "]
    if {$sent != 0} {prolog $module:parse(\[$sent\]) }
}  -underline 0 -accelerator {P P}

.menu.parse.m add command -label "Compare Parsers" -command {
    set sent [hdrug_qbox_array .p sents Question\
              "         Select a Sentence         "]
    if {"$sent" != 0} {prolog $module:parse_compare(\[$sent\]) }
    unset sent
}  -underline 0 -accelerator {P C}
.menu.parse.m add separator
.menu.parse.m add cascade -label "Select Active Parsers" \
    -menu .menu.parse.m.parsers
.menu.parse.m add separator
.menu.parse.m add command -label "Add Parse Widget" -command add_parse_widget


.menu.parse.m add command -label "Remove Parse Widget" -command {
    catch "destroy .pp"
}

menu .menu.parse.m.parsers -postcommand {
    catch {.menu.parse.m.parsers delete 0 last}
    foreach i [prolog_all hdrug_util:hdrug_flag(parser(X)) X] {
	set parser($i) [prolog_var hdrug_util:hdrug_flag(parser($i),Y) Y]
        .menu.parse.m.parsers add check -label $i \
	    -variable parser($i) -onvalue on -offvalue off\
	    -command "prolog hdrug_util:hdrug_flag(parser($i),_,\$parser($i))"
    }
}

# Menu Generate
menubutton .menu.generate -text "Generate" -underline 0 -menu .menu.generate.m
if !$generator_exists \
    {.menu.generate configure -state disabled}

menu .menu.generate.m

if $generator_exists then {
    .menu.generate.m add command -label "Generate object" -command {
        set m [hdrug_select_obj]
        if {$m != 0} {prolog "$module:generate_obj_no($m)"}
        unset m
    } -underline 0 -accelerator {G G}
    .menu.generate.m add command -label "Generate Lf" -command {
        set lf [hdrug_qbox_array .l lfs "Select" "     Select a logical form:     "]
        if {$lf != 0} {prolog "$module:generate_atom(($lf))"}
        unset lf
    } -underline 9 -accelerator {G L}
    .menu.generate.m add separator
    .menu.generate.m add command -label "Compare Generators on object" \
        -command {
	    set m [hdrug_select_obj]
            if {$m != 0} {prolog "$module:generate_compare_object($m)"}
            unset m
	} -underline 0 -accelerator {G C}
    .menu.generate.m add command -label "Compare Generators on lf" \
        -command {
	    set lf [hdrug_qbox_array .l lfs \
			"Select" "          Select a logical form:          "]
	    if {$lf != 0} {prolog "$module:generate_compare_atom($lf)"}
	    unset lf
	} 
    .menu.generate.m add separator
    .menu.generate.m add cascade -label "Select Active Generators" \
	-menu .menu.generate.m.generators 
}

menu .menu.generate.m.generators -postcommand {
    catch {.menu.generate.m.generators delete 0 last}
    foreach i [prolog_all hdrug_util:hdrug_flag(generator(G)) G] {
	set generator($i) [prolog_var hdrug_util:hdrug_flag(generator($i),Y) Y]
        .menu.generate.m.generators add check -label $i \
	    -variable generator($i) -onvalue on -offvalue off\
	    -command "prolog hdrug_util:hdrug_flag(generator($i),_,\$generator($i))"
    }
}

# Menu Test
menubutton .menu.test -text "Test-Suite" -underline 0 -menu .menu.test.m
menu .menu.test.m
.menu.test.m add command -label "Run test suite (parsing)" \
    -command {prolog $module:parser_comparisons} -underline 0 -accelerator {T R}
.menu.test.m add command -label "       -- with view" -underline 23\
    -command {prolog hdrug_stats:go_and_table_tk} -accelerator {T V}
.menu.test.m add command -label "           -- (averages)" \
    -underline 29\
    -command {prolog hdrug_stats:go_and_table_tk_add} -accelerator {T A}
.menu.test.m add command -label "Run test suite (generation)" \
    -command {prolog $module:generator_comparisons}
.menu.test.m add command -label "Compile test suite" -underline 0\
    -command {
	set filename [tk_getOpenFile]
	if {"$filename" != ""} {
	    prolog "$module:try_compile_test_suite('$filename')"
	    prolog hdrug_gui:update_sents
	    prolog hdrug_gui:update_lfs
	}
    }  -accelerator {T C}
.menu.test.m add command -label "Reconsult test suite" -underline 0\
    -command {
	set filename [tk_getOpenFile]
	if {"$filename" != ""} {
	    prolog $module:try_reconsult_test_suite('$filename')
	    prolog hdrug_gui:update_sents
	    prolog hdrug_gui:update_lfs
	}
    }  -accelerator {T R}
.menu.test.m add cascade -label "View test results"\
    -menu .menu.test.m.view
.menu.test.m add separator
.menu.test.m add command -label "Destroy test results"\
    -command {prolog hdrug_stats:reset_table} -underline 0 -accelerator {T D}
.menu.test.m add command -label "Analyse corpus (number of words)"\
    -command {prolog hdrug_stats:analyse_nr_words}

.menu.test.m configure -postcommand {
    if {[prolog_q hdrug:a_sentence(_,_,_)] && $parser_exists} {
        .menu.test.m entryconfigure 1 -state normal
        .menu.test.m entryconfigure 2 -state normal
        .menu.test.m entryconfigure 3 -state normal
    } else {
        .menu.test.m entryconfigure 1 -state disabled
        .menu.test.m entryconfigure 2 -state disabled
        .menu.test.m entryconfigure 3 -state disabled
    }
    if ![prolog_q hdrug:table_entry(_,_,_,_,_,_)] {
	.menu.test.m entryconfigure 7 -state disabled
	.menu.test.m entryconfigure 9 -state disabled
    } else {
	.menu.test.m entryconfigure 7 -state normal
	.menu.test.m entryconfigure 9 -state normal
    }
    if $generator_exists {
	.menu.test.m entryconfigure 4 -state normal
    } else {
	.menu.test.m entryconfigure 4 -state disabled
    }
    if ![prolog_q hdrug:a_sentence(_,_,_)] {
	.menu.test.m entryconfigure 10 -state disabled 
    }
}

menu .menu.test.m.view
.menu.test.m.view add command -label "Individual Time (Tk)"\
    -command {prolog hdrug_stats:sts}
.menu.test.m.view add command -label "Individual Space (Tk)"\
    -command {prolog hdrug_stats:sts_space}
.menu.test.m.view add command -label "Average Time per #words (Tk)"\
    -command {prolog hdrug_stats:sts_add}
.menu.test.m.view add command -label "Average Space per #words (Tk)"\
    -command {prolog hdrug_stats:sts_space_add}
.menu.test.m.view add command -label "Average Time per #words (LaTeX)"\
    -command {prolog hdrug_stats:p_tt}
.menu.test.m.view add command -label "Average Time per #readings (LaTeX)"\
    -command {prolog hdrug_stats:p_tt_amb}
.menu.test.m.view add command -label "Individual (Prolog)"\
    -command {prolog hdrug_stats:print_table}
.menu.test.m.view add command -label "Total Time per #words (Prolog)"\
    -command {prolog hdrug_stats:print_table_add}
.menu.test.m.view add command -label "Totals (Prolog)"\
    -command {prolog hdrug_stats:print_table_total}
.menu.test.m.view add command -label "Percentage within bounds (Prolog)"\
    -command {prolog hdrug_stats:results_within_bound}
.menu.test.m.view add command -label "Percentage within bounds (Tk)"\
    -command {prolog hdrug_stats:blt_within_bound}
.menu.test.m.view add command -label "Analyse corpus (number of readings)"\
    -command {prolog hdrug_stats:analyse_nr_readings}



# Menu View  
# check wether they can be enabled/disabled.
menubutton .menu.show -text "View" -menu .menu.show.m -underline 0
menu .menu.show.m 

.menu.show.m add cascade -label "Object" -menu .menu.show.m.object
.menu.show.m add cascade -label "Predicate" -menu .menu.show.m.predicate
.menu.show.m add cascade -label "Type" -menu .menu.show.m.type

.menu.show.m configure -postcommand {
    if [prolog_q hdrug:object(_,_)] { 
	.menu.show.m entryconfigure Object -state normal } else {
	.menu.show.m entryconfigure Object -state disabled
    }
    if [prolog_q hdrug_util:hook(a_user_clause(_,_,_))] {
        .menu.show.m entryconfigure Predicate -state normal } else {
	.menu.show.m entryconfigure Predicate -state disabled
    }
    if $features {
	.menu.show.m entryconfigure Type -state normal } else {
	.menu.show.m entryconfigure Type -state disabled
    }
}

# Viewing objects
menu .menu.show.m.object
.menu.show.m.object add cascade -label "Clig" \
    -menu .menu.show.m.object.clig
.menu.show.m.object add cascade -label "DOT" \
    -menu .menu.show.m.object.dot
.menu.show.m.object add cascade -label "Tk" \
    -menu .menu.show.m.object.canvas
.menu.show.m.object add cascade -label "Prolog" \
    -menu .menu.show.m.object.prolog
.menu.show.m.object add cascade -label "LaTeX" \
    -menu .menu.show.m.object.latex

menu .menu.show.m.object.canvas -postcommand {
    catch {.menu.show.m.object.canvas delete 0 last}
    foreach i $treedefs {
	.menu.show.m.object.canvas add command -label Tree($i) \
	    -command "show_object_tree $i tk"
    }
    .menu.show.m.object.canvas add command -label Matrix \
	-command {
	    set obj [hdrug_select_obj]
	    if {$obj != 0} {prolog hdrug_show:show_object_no($obj,fs,tk)}
	    unset obj
	}
    if !$features {
	.menu.show.m.object.canvas entryconfigure last -state disabled
    }
    .menu.show.m.object.canvas add command -label Semantics \
	-command {
	    set obj [hdrug_select_obj]
	    if {$obj != 0} {prolog hdrug_show:show_object_no($obj,sem,tk)}
	    unset obj
	}
    .menu.show.m.object.canvas add command -label Phonology \
	-command {
	    set obj [hdrug_select_obj]
	    if {$obj != 0} {prolog hdrug_show:show_object_no($obj,words,tk)}
	    unset obj
	}
    .menu.show.m.object.canvas add command -label Text \
	-command {
	    set obj [hdrug_select_obj]
	    if {$obj != 0} {prolog hdrug_show:show_object_no($obj,term(print),tk)}
	    unset obj
	}
}

menu .menu.show.m.object.prolog -postcommand {
    catch {.menu.show.m.object.prolog delete 0 last}
    foreach i $treedefs {
	.menu.show.m.object.prolog add command -label Tree($i) \
	    -command "show_object_tree $i user"
    }
    .menu.show.m.object.prolog add command -label Semantics \
	-command {
	    set obj [hdrug_select_obj]
	    if {$obj != 0} {prolog hdrug_show:show_object_no($obj,sem,user)}
	    unset obj
	}
    .menu.show.m.object.prolog add command -label Phonology \
	-command {
	    set obj [hdrug_select_obj]
	    if {$obj != 0} {prolog hdrug_show:show_object_no($obj,words,user)}
	    unset obj
	}
    .menu.show.m.object.prolog add command -label Matrix \
	-command {
	    set obj [hdrug_select_obj]
	    if {$obj != 0} {prolog hdrug_show:show_object_no($obj,fs,user)}
	}
    if !$features {
	.menu.show.m.object.prolog entryconfigure last -state disabled
    }
    .menu.show.m.object.prolog add command -label Text \
	-command {
	    set obj [hdrug_select_obj]
	    if {$obj != 0} {prolog hdrug_show:show_object_no($obj,term(print),user)}
	}
}

menu .menu.show.m.object.latex -postcommand {
    catch {.menu.show.m.object.latex delete 0 last}
    foreach i $treedefs {
	.menu.show.m.object.latex add command -label Tree($i) \
	    -command "show_object_tree $i latex"
    }
    .menu.show.m.object.latex add command -label Matrix \
	-command {
	    set obj [hdrug_select_obj]
	    if {$obj != 0} {prolog hdrug_show:show_object_no($obj,fs,latex)}
	}
    if !$features {
	.menu.show.m.object.latex entryconfigure last -state disabled
    }
    .menu.show.m.object.latex add command -label Semantics \
	-command {
	    set obj [hdrug_select_obj]
	    if {$obj != 0} {prolog hdrug_show:show_object_no($obj,sem,latex)}
	}
    .menu.show.m.object.latex add command -label Phonology \
	-command {
	    set obj [hdrug_select_obj]
	    if {$obj != 0} {prolog hdrug_show:show_object_no($obj,words,latex)}
	}
    .menu.show.m.object.latex add command -label Text \
	-command {
	    set obj [hdrug_select_obj]
	    if {$obj != 0} {prolog hdrug_show:show_object_no($obj,term(print),latex)}
        }
}

menu .menu.show.m.object.clig -postcommand {
    catch {.menu.show.m.object.clig delete 0 last}
    foreach i $treedefs {
	.menu.show.m.object.clig add command -label Tree($i) \
	    -command "show_object_tree $i clig"
    }
    .menu.show.m.object.clig add command -label Matrix \
	-command {
	    set obj [hdrug_select_obj]
	    if {$obj != 0} {prolog hdrug_show:show_object_no($obj,fs,clig)}
	}
    if !$features {
	.menu.show.m.object.clig entryconfigure last -state disabled
    }
}

menu .menu.show.m.object.dot -postcommand {
    catch {.menu.show.m.object.dot delete 0 last}
    foreach i $treedefs {
	.menu.show.m.object.dot add command -label Tree($i) \
	    -command "show_object_tree $i dot"
    }
}

# Viewing Predicates
menu .menu.show.m.predicate

.menu.show.m.predicate add cascade -label "Tk" \
    -menu .menu.show.m.predicate.tk
.menu.show.m.predicate add cascade -label "Prolog" \
    -menu .menu.show.m.predicate.prolog
.menu.show.m.predicate add cascade -label "LaTeX" \
    -menu .menu.show.m.predicate.latex
.menu.show.m.predicate add cascade -label "Clig" \
    -menu .menu.show.m.predicate.clig

menu .menu.show.m.predicate.tk
.menu.show.m.predicate.tk add command -label "Text" \
    -command {
	set pred [hdrug_qbox_array .w preds Pred "Pred to show:" ]
	if {$pred != 0} {prolog hdrug_show:show_predicate($pred,term(print),tk)}
    }

.menu.show.m.predicate.tk add command -label "Matrix" \
    -command {
	set pred [hdrug_qbox_array .w preds Pred "Pred to show:" ]
	if {$pred != 0} {prolog hdrug_show:show_predicate($pred,fs,tk)}
    }
if !$features {
    .menu.show.m.predicate.tk entryconfigure last -state disabled
}

menu .menu.show.m.predicate.latex
.menu.show.m.predicate.latex add command -label "Text" \
    -command {
	set pred [hdrug_qbox_array .w preds Pred "Pred to show:" ]
	if {$pred != 0} {prolog hdrug_show:show_predicate($pred,term(print),latex)}
    }

.menu.show.m.predicate.latex add command -label "Matrix" \
    -command {
	set pred [hdrug_qbox_array .w preds Pred "Pred to show:" ]
	if {$pred != 0} {prolog hdrug_show:show_predicate($pred,fs,latex)}
    }
if !$features {
    .menu.show.m.predicate.latex entryconfigure last -state disabled
}

menu .menu.show.m.predicate.clig
.menu.show.m.predicate.clig add command -label "Matrix" \
    -command {
	set pred [hdrug_qbox_array .w preds Pred "Pred to show:" ]
	if {$pred != 0} {prolog hdrug_show:show_predicate($pred,fs,clig)}
    }
if !$features {
    .menu.show.m.predicate.clig entryconfigure last -state disabled
}

menu .menu.show.m.predicate.prolog
.menu.show.m.predicate.prolog add command -label "Text" \
    -command {
	set pred [hdrug_qbox_array .w preds Pred "Pred to show" ]
	if {$pred != 0} {prolog hdrug_show:show_predicate($pred,term(print),user)}
    }
.menu.show.m.predicate.prolog add command -label "Matrix" \
    -command {
	set pred [hdrug_qbox_array .w preds Pred "Pred to show:" ]
	if {$pred != 0} {prolog hdrug_show:show_predicate($pred,fs,user)}
    }
if !$features {
    .menu.show.m.predicate.prolog entryconfigure last -state disabled
}

# Viewing type definitions
menu .menu.show.m.type
.menu.show.m.type add command -label "Tk" -command {
    set type [hdrug_qbox_array .w types Type "Type to show" ]
    if {$type != 0} {prolog hdrug_gui:tk_tree(type,$type)}
}
.menu.show.m.type add cascade -label "Prolog" -menu .menu.show.m.type.prolog
.menu.show.m.type add command -label "LaTeX"     -command {
    set type [hdrug_qbox_array .w types Type "Type to show" ]
    if {$type != 0} {prolog hdrug_latex:latex_tree(type,$type)}
}
.menu.show.m.type add command -label "Clig"     -command {
    set type [hdrug_qbox_array .w types Type "Type to show" ]
    if {$type != 0} {prolog hdrug_clig:clig_tree(type,$type)}
}
menu .menu.show.m.type.prolog
.menu.show.m.type.prolog add command -label "Tree" -command {
    set type [hdrug_qbox_array .w types Type "Type to show"]
    if {$type != 0} {prolog hdrug_txt:pretty_graphic(type,$type)}
}
.menu.show.m.type.prolog add command -label "Text" -command {
    set type [hdrug_qbox_array .w types Type "Type to show"]
    if {$type != 0} {prolog hdrug_feature:pretty_type($type)}
}

# Menu Help
menubutton .menu.help -text "Help" -underline 0 -menu .menu.help.m
menu .menu.help.m
.menu.help.m add command -label "About" -underline 0 \
    -command {
	tk_dialog .dialog "About" \
	    "All flames to vannoord@let.rug.nl"\
	    @$hdrug_library/bitmaps/hdrug_vannoord.bm 0 {Seen enough?}
    } -accelerator {H A}

.menu.help.m add command -label "Version" -underline 0 \
    -command {prolog version} -accelerator {H V}

.menu.help.m add command -label "Hdrug On-line Manual" -command {
    send_url_to_netscape http://www.let.rug.nl/~vannoord/Hdrug/Manual/
}

pack .menu.file .menu.debug .menu.hdrug \
    .menu.options .menu.parse .menu.generate\
    .menu.test .menu.show -side left

pack .menu.help -side right

bind . <Any-FocusIn> {
    if {("%d" == "NotifyVirtual") && ("%m" == "NotifyNormal")} {
	focus .menu
    }
}

#################
### ButtonBar ###
#################

label .t.xbm -bitmap @$hdrug_library/bitmaps/hdrug.xbm -width 50
pack .t.xbm -side left

menubutton .t.topt -menu .t.topt.top_features -text Top 
menu .t.topt.top_features -postcommand \
    {menu_flag_post .t.topt top_features \
	"undefined [prolog_all hdrug:top(Top,_) Top]"}

label .t.top -textvariable flag(top_features) -width 10
pack .t.topt .t.top -side left

if $parser_exists then {
    label .t.parser -textvariable flag(parser) -width 10
    menubutton .t.parsert -menu .t.parsert.parser -text Parser 
    menu .t.parsert.parser -postcommand \
	{ menu_flag_post .t.parsert parser \
	      "[prolog_all hdrug_util:hdrug_flag(parser(X)) X]" }
    pack .t.parsert .t.parser -side left
}

if $generator_exists then {
    label .t.generator -textvariable flag(generator) -width 10
    menubutton .t.generatort -menu .t.generatort.generator -text Generator 
    menu .t.generatort.generator -postcommand \
	{ menu_flag_post .t.generatort generator \
	      "[prolog_all hdrug_util:hdrug_flag(generator(X)) X]" }
    pack .t.generatort .t.generator -side left
}

checkbutton .t.check -variable use_canvas -text "New Canvas"

pack .t.check -side left

if [option get . helpline Helpline] {
#    label .t.helpline -wraplength 200 -justify left
#    pack .t.helpline -side left
    help_line .t.top "Current top-category"
    help_line .t.topt "Current top-category"
    help_line .t.check "Next graphical output in separate top-level window" 
    help_line .bb.l    "Show only left window"
    help_line .bb.r    "Show only right window"
    help_line .bb.d    "Show left and right window"
    help_line .ccc "Drag to change canvas sizes"
    help_line .t.parser "Current parser"
    help_line .t.parsert "Current parser"
    help_line .t.generator "Current generator"
    help_line .t.generatort "Current generator"
    help_line .t.xbm "$HDRUG"
}

proc add_parse_widget {} {
    global pp_answer internal_pp_answer sents current_cmd_no last_cmd_no pp_answer_history
    catch "destroy .pp"
    frame .pp
    pack .pp -before .bb -fill x -expand no
    frame .pp.top
    pack .pp.top -side top -fill x -expand yes
    button .pp.top.left -text Parse -command {
        if {$pp_answer != {}} {
            set internal_pp_answer $pp_answer
            regsub -all "\\\\" $internal_pp_answer "\\\\\\\\" internal_pp_answer
            regsub -all "%" $internal_pp_answer "%%" internal_pp_answer
            regsub -all '   $internal_pp_answer "\\\\'"     internal_pp_answer
            regsub -all " " $internal_pp_answer "'\} \{'" internal_pp_answer
            set internal_pp_answer [format "\{'$internal_pp_answer'\}" ]
            set last_cmd_no [expr $last_cmd_no+1]
            set current_cmd_no $last_cmd_no
            set pp_answer_history($last_cmd_no) $pp_answer
            prolog $module:parse(\[[join $internal_pp_answer ,]\])
        }
        
    }
    entry .pp.top.mid -bd 1 -textvariable pp_answer 
    bind .pp.top.mid <Return> {
        if {$pp_answer != {}} {
            set internal_pp_answer $pp_answer
            regsub -all "\\\\" $internal_pp_answer "\\\\\\\\" internal_pp_answer
            regsub -all "%%" $internal_pp_answer "%%%%" internal_pp_answer
            regsub -all '   $internal_pp_answer "\\\\'"     internal_pp_answer
            regsub -all " " $internal_pp_answer "'\} \{'" internal_pp_answer
            set internal_pp_answer [format "\{'$internal_pp_answer'\}" ]
            set last_cmd_no [expr $last_cmd_no+1]
            set current_cmd_no $last_cmd_no
            set pp_answer_history($last_cmd_no) $pp_answer
            prolog $module:parse(\[[join $internal_pp_answer ,]\])
        }
        
    }
    
    # history ..
    
    bind .pp.top.mid <Up> {
        if [expr $current_cmd_no > 1] {
            set current_cmd_no [expr $current_cmd_no-1]
            set pp_answer $pp_answer_history($current_cmd_no)
        }
    }
    bind .pp.top.mid <Down> {
        if [expr $current_cmd_no < $last_cmd_no] {
            set current_cmd_no [expr $current_cmd_no+1]
            set pp_answer $pp_answer_history($current_cmd_no)
        }
    }
    bind .pp.top.mid <3> {set pp_answer {}}

    bind .pp.top.mid <F1> {
        set pp_answer [selection get]
    }

    pack .pp.top.left -side left
    pack .pp.top.mid -side left -fill x -expand yes
#    pack .pp.top.right -side left
    if {[prolog_q hdrug_util:hdrug_flag(complex_parse_widget,on)] && 
        [info exists sents(max)]} {
        frame .pp.frame
        pack .pp.frame -side top -expand yes -fill y -fill x
        scrollbar .pp.frame.scroll -command ".pp.frame.list yview"
        scrollbar .pp.frame.xscroll  -command ".pp.frame.list xview" \
            -orient horiz
        listbox .pp.frame.list -yscroll ".pp.frame.scroll set" -relief sunken \
            -xscroll ".pp.frame.xscroll set" -width 70 -height 5
        pack .pp.frame.xscroll -side bottom -fill x
        pack .pp.frame.scroll -side left -fill y
        pack .pp.frame.list -side left -expand yes -fill both
        set i 0
        while {$i < $sents(max)} {
            .pp.frame.list insert $i $sents($i)
            set i [expr $i+1]
        }
        bind .pp.frame.list <Double-1> {
            set pp_answer [selection get]
            if {$pp_answer != {}} {
                set internal_pp_answer $pp_answer
                regsub -all "\\\\" $internal_pp_answer "\\\\\\\\" internal_pp_answer
                regsub -all "%%" $internal_pp_answer "%%%%" internal_pp_answer
                regsub -all '   $internal_pp_answer "\\\\'"     internal_pp_answer
                regsub -all " " $internal_pp_answer "'\} \{'" internal_pp_answer
                set internal_pp_answer [format "\{'$internal_pp_answer'\}" ]
                set last_cmd_no [expr $last_cmd_no+1]
                set current_cmd_no $last_cmd_no
                set pp_answer_history($last_cmd_no) $pp_answer
                prolog $module:parse(\[[join $internal_pp_answer ,]\])
            }
            
        } 
        bind .pp.frame.list <ButtonRelease-1> {
            set pp_answer [selection get]
            set internal_pp_answer $pp_answer
            regsub -all "\\\\" $internal_pp_answer "\\\\\\\\" internal_pp_answer
            regsub -all "%%" $internal_pp_answer "%%%%" internal_pp_answer
            regsub -all '   $internal_pp_answer "\\\\'"     internal_pp_answer
            regsub -all " " $internal_pp_answer "'\} \{'" internal_pp_answer
            set internal_pp_answer [format "\{'$internal_pp_answer'\}" ]
            prolog $module:try_to_set_current_ref(\[[join $internal_pp_answer ,]\])
        }

        bind .pp.frame.list <2> {
            set search [hdrug_qbox_simple .g {Search: } {Pattern:}]
            set i 1
            set found 0
            while { $i < $sents(max) } {
                set str [.pp.frame.list get $i]
                if [regexp -nocase $search $str ] {
                    .pp.frame.list see $i
                    .pp.frame.list activate $i
                    selection clear
                    .pp.frame.list selection set $i
                    set pp_answer [selection get]
                    set internal_pp_answer $pp_answer
                    regsub -all "\\\\" $internal_pp_answer "\\\\\\\\" internal_pp_answer
                    regsub -all "%%" $internal_pp_answer "%%%%" internal_pp_answer
                    regsub -all '   $internal_pp_answer "\\\\'"     internal_pp_answer
                    regsub -all " " $internal_pp_answer "'\} \{'" internal_pp_answer
                    set internal_pp_answer [format "\{'$internal_pp_answer'\}" ]
                    prolog $module:try_to_set_current_ref(\[[join $internal_pp_answer ,]\])
                    set found 1
                    break
                }
                set i [expr $i+1]
            }
            if {$found == 0} {
                tk_dialog .d Search "No maches for $search" {} 0 ok
            }
        }
        bind .pp.frame.list <3> {
            set found 0
            set i [expr $i+1]
            while { $i < $sents(max) } {
                set str [.pp.frame.list get $i]
                if [regexp -nocase $search $str ] {
                    .pp.frame.list see $i
                    .pp.frame.list activate $i
                    selection clear
                    .pp.frame.list selection set $i
                    set pp_answer [selection get]
                    set internal_pp_answer $pp_answer
                    regsub -all "\\\\" $internal_pp_answer "\\\\\\\\" internal_pp_answer
                    regsub -all "%%" $internal_pp_answer "%%%%" internal_pp_answer
                    regsub -all '   $internal_pp_answer "\\\\'"     internal_pp_answer
                    regsub -all " " $internal_pp_answer "'\} \{'" internal_pp_answer
                    set internal_pp_answer [format "\{'$internal_pp_answer'\}" ]
                    prolog $module:try_to_set_current_ref(\[[join $internal_pp_answer ,]\])
                    set found 1
                    break
                }
                set i [expr $i+1]
            }
            if {$found == 0} {
                tk_dialog .d Search "No further matches for $search" {} 0 ok
            }
        }
    }

    frame .pp.bot  -bd 1
    pack .pp.bot -side bottom -fill both

    help_line .pp.frame "<1> select <2> search <3> search again"

}

proc many_photo { w n image } {
    set i 0
    set j 0
    frame $w
    while { $i < $n } {
        frame $w.r$i
        set j 0
        while { $j < $n } {
            label $w.r$i.f$j -image $image
            pack $w.r$i.f$j -side left
            incr j
        }
        pack $w.r$i
        incr i
    }
    pack $w
    after 2000 "catch \"destroy $w\""
}

