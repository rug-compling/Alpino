# some nonsense first
set about_text {tk_dialog .d  {About Alpino} \
  {The ALPINO ® grammar / parser / generator. This program is one of the results of 
the NWO PIONIER project Algorithms for Linguistic Processing. Project
members include(d) Leonoor van der Beek, Gosse Bouma, Jan Daciuk, 
Tanja Gaustad, Rob Malouf, Robbert Prins, Begona Villada, Mark-Jan
Nederhof, Tony Mullen and Gertjan van Noord. It has been further
developed by Gertjan van Noord and Daniël de Kok.}\
		    {} 0 ok }

.menu.help.m add command -label "About Alpino" -underline 10\
    -command $about_text -accelerator {H G}

.menu.parse.m delete 2 4

.menu.parse.m add separator

.menu.parse.m add cascade -label Mode -menu .menu.parse.m.r

menu .menu.parse.m.r 

.menu.parse.m.r add radiobutton -label Slow -value slow -variable flag(parse_mode) \
      -command "prolog $module:slow_options"
.menu.parse.m.r add radiobutton -label Fast -value fast -variable flag(parse_mode) \
      -command "prolog $module:fast_options"
.menu.parse.m.r add radiobutton -label "Very Fast" -value veryfast -variable flag(parse_mode) \
      -command "prolog $module:veryfast_options"


.menu.show.m add separator

if $features then {
    .menu.show.m add cascade -label "Lexical Entry" -menu .menu.show.m.lex
}
if {$rules(max) > 0} then {
        .menu.show.m add cascade -label "Grammar Rule" -menu .menu.show.m.rule
}
if {$tops(max) > 0} then {
	.menu.show.m add cascade -label "Top Categories" -menu .menu.show.m.top
}

if {$frames(max) > 0} then {
	.menu.show.m add cascade -label "Subcat Frames" -menu .menu.show.m.frame
}

menu .menu.show.m.lex
.menu.show.m.lex add cascade -label "Tk"  -menu .menu.show.m.lex.tk
.menu.show.m.lex add cascade -label "Prolog" -menu .menu.show.m.lex.prolog
.menu.show.m.lex add cascade -label "LaTeX"  -menu .menu.show.m.lex.latex
.menu.show.m.lex add cascade -label "Clig"  -menu .menu.show.m.lex.clig

proc show_lex {how where} {
    global lexs
    set lex [hdrug_qbox_array .w lexs Lex "Word or (Prolog-) list of words to show:" ]
    if {$lex == 0} {return 0}
    prolog "$module:show_lex($lex,$how,$where)"
}

menu .menu.show.m.lex.tk
.menu.show.m.lex.tk add command -label "Text" \
    -command {show_lex term(print) tk}
.menu.show.m.lex.tk add command -label "Matrix" \
    -command {show_lex fs tk}
if !$features {
	.menu.show.m.lex.tk entryconfigure last -state disabled
    }


menu .menu.show.m.lex.latex
.menu.show.m.lex.latex add command -label "Text" \
    -command {show_lex term(print) latex}
.menu.show.m.lex.latex add command -label "Matrix" \
    -command {show_lex fs latex}
if !$features {
	.menu.show.m.lex.latex entryconfigure last -state disabled
    }

menu .menu.show.m.lex.clig
.menu.show.m.lex.clig add command -label "Matrix" \
    -command {show_lex fs clig}
if !$features {
	.menu.show.m.lex.clig entryconfigure last -state disabled
    }

menu .menu.show.m.lex.prolog
.menu.show.m.lex.prolog add command -label "Text" \
    -command {show_lex term(print) user}
.menu.show.m.lex.prolog add command -label "Matrix" \
    -command {show_lex fs user}
if !$features {
	.menu.show.m.lex.prolog entryconfigure last -state disabled
    }

menu .menu.show.m.rule
.menu.show.m.rule add cascade -label "Tk"  -menu .menu.show.m.rule.tk
.menu.show.m.rule add cascade -label "Prolog" -menu .menu.show.m.rule.prolog
.menu.show.m.rule add cascade -label "LaTeX"  -menu .menu.show.m.rule.latex
.menu.show.m.rule add cascade -label "Clig"  -menu .menu.show.m.rule.clig

proc show_rule {how where} {
    global rules module
    set rule [hdrug_qbox_array .w rules Rule "Rule to show:" ]
    if {$rule == 0} {return 0}
    prolog "$module:show_rule($rule,$how,$where)"
}

menu .menu.show.m.rule.tk
.menu.show.m.rule.tk add command -label "Text" \
    -command {show_rule term(print) tk}
.menu.show.m.rule.tk add command -label "Matrix" \
    -command {show_rule fs tk}
if !$features {
	.menu.show.m.rule.tk entryconfigure last -state disabled
    }

menu .menu.show.m.rule.latex
.menu.show.m.rule.latex add command -label "Text" \
    -command {show_rule term(print) latex}
.menu.show.m.rule.latex add command -label "Matrix" \
    -command {show_rule fs latex}
if !$features {
	.menu.show.m.rule.latex entryconfigure last -state disabled
    }

menu .menu.show.m.rule.clig
.menu.show.m.rule.clig add command -label "Matrix" \
    -command {show_rule fs clig}
if !$features {
	.menu.show.m.rule.clig entryconfigure last -state disabled
    }

menu .menu.show.m.rule.prolog
.menu.show.m.rule.prolog add command -label "Text" \
    -command {show_rule term(print) user}
.menu.show.m.rule.prolog add command -label "Matrix" \
    -command {show_rule fs user}
if !$features {
	.menu.show.m.rule.prolog entryconfigure last -state disabled
    }

menu .menu.show.m.frame
.menu.show.m.frame add cascade -label "Tk"  -menu .menu.show.m.frame.tk
.menu.show.m.frame add cascade -label "Prolog" -menu .menu.show.m.frame.prolog
.menu.show.m.frame add cascade -label "LaTeX"  -menu .menu.show.m.frame.latex
.menu.show.m.frame add cascade -label "Clig"  -menu .menu.show.m.frame.clig

proc show_frame {how where} {
    global frames module
    set frame [hdrug_qbox_array .w frames Frame "Frame to show:" ]
    if {$frame == 0} {return 0}
    prolog "$module:show_frame($frame,$how,$where)"
}

menu .menu.show.m.frame.tk
.menu.show.m.frame.tk add command -label "Text" \
    -command {show_frame term(print) tk}
.menu.show.m.frame.tk add command -label "Matrix" \
    -command {show_frame fs tk}
if !$features {
	.menu.show.m.frame.tk entryconfigure last -state disabled
    }

menu .menu.show.m.frame.latex
.menu.show.m.frame.latex add command -label "Text" \
    -command {show_frame term(print) latex}
.menu.show.m.frame.latex add command -label "Matrix" \
    -command {show_frame fs latex}
if !$features {
	.menu.show.m.frame.latex entryconfigure last -state disabled
    }

menu .menu.show.m.frame.clig
.menu.show.m.frame.clig add command -label "Matrix" \
    -command {show_frame fs clig}
if !$features {
	.menu.show.m.frame.clig entryconfigure last -state disabled
    }

menu .menu.show.m.frame.prolog
.menu.show.m.frame.prolog add command -label "Text" \
    -command {show_frame term(print) user}
.menu.show.m.frame.prolog add command -label "Matrix" \
    -command {show_frame fs user}
if !$features {
	.menu.show.m.frame.prolog entryconfigure last -state disabled
    }

menu .menu.show.m.top
.menu.show.m.top add cascade -label "Tk"     -menu .menu.show.m.top.tk
.menu.show.m.top add cascade -label "Prolog" -menu .menu.show.m.top.prolog
.menu.show.m.top add cascade -label "LaTeX"  -menu .menu.show.m.top.latex
.menu.show.m.top add cascade -label "Clig"  -menu .menu.show.m.top.clig

menu .menu.show.m.top.tk
.menu.show.m.top.tk add command -label "Text" \
    -command {prolog $module:show_top(term(print),tk)}
.menu.show.m.top.tk add command -label "Matrix" \
    -command {prolog $module:show_top(fs,tk)}
if !$features {
	.menu.show.m.top.tk entryconfigure last -state disabled
    }

menu .menu.show.m.top.latex
.menu.show.m.top.latex add command -label "Text" \
    -command {prolog $module:show_top(term(print),latex)}
.menu.show.m.top.latex add command -label "Matrix" \
    -command {prolog $module:show_top(fs,latex)}
if !$features {
	.menu.show.m.top.latex entryconfigure last -state disabled
    }

menu .menu.show.m.top.clig
.menu.show.m.top.clig add command -label "Matrix" \
    -command {prolog $module:show_top(fs,clig)}
if !$features {
	.menu.show.m.top.clig entryconfigure last -state disabled
    }

menu .menu.show.m.top.prolog
.menu.show.m.top.prolog add command -label "Text" \
    -command {prolog $module:show_top(term(print),user)}
.menu.show.m.top.prolog add command -label "Matrix" \
    -command {prolog $module:show_top(fs,user)}
if !$features {
	.menu.show.m.top.prolog entryconfigure last -state disabled
    }

.menu.show.m add separator

.menu.show.m add cascade -label "Rule Hierarchy" -menu .menu.show.m.rh

menu .menu.show.m.rh
## too much memory required:
# .menu.show.m.rh add command -label "Clig" -command "prolog hdrug_call_tree:call_tree_bu_clig"
.menu.show.m.rh add command -label "Dot" -command "prolog grammar_hierarchy_dot"
.menu.show.m.rh add command -label "Ascii" -command "prolog grammar_hierarchy"

.menu.show.m add cascade -label "Lexical Hierarchy" -menu .menu.show.m.lh

menu .menu.show.m.lh
.menu.show.m.lh add command -label "Dot" -command "prolog lexicon_hierarchy_dot"
.menu.show.m.lh add command -label "Ascii" -command "prolog lexicon_hierarchy"


################### LOAD ######################

menubutton .menu.load -text "Load" -menu .menu.load.m -underline 0 
menu .menu.load.m
.menu.load.m add command -label "Suite " -command "prolog $module:load_suite"
.menu.load.m add command -label "Suite (Browse ..)" -command {
    set filename [tk_getOpenFile]
    if {"$filename" != ""} {
	prolog hdrug_util:set_flag(suite,'$filename')
	prolog $module:load_suite
    }
}
.menu.file.m delete 1 5


pack .menu.show .menu.load .menu.test -after .menu.parse  -side left

.menu.help.m add command -label "man Alpino" \
    -command "exec xterm -e man Alpino"

pack .menu.help -side right

.menu.test.m configure -postcommand {}
.menu.test.m delete 1 9

.menu.test.m configure -postcommand {
    if [prolog_q hdrug:a_sentence(_,_,_)] {
	.menu.test.m entryconfigure 1 -state normal
    } else {
	.menu.test.m entryconfigure 1 -state disabled
    }
}


#.menu.test.m entryconfigure {Compile test suite} -state disabled
#.menu.test.m entryconfigure {Reconsult test suite} -state disabled


proc create_top_canvas_pos_filter {w} {
    global hdrug_library
    global pos_filter_continue
    set pos_filter_continue wait
    toplevel $w
    wm iconbitmap $w @$hdrug_library/bitmaps/hdrug.xbm
    wm iconify $w
    wm title $w Hdrug-$w
    wm iconname $w Hdrug-$w
    wm minsize $w 800 500
    bind $w <Destroy> {set pos_filter_continue destroyed}
#    bind $w <Unmap> {set pos_filter_continue invisible}
    frame $w.f
    pack $w.f -fill both -expand 1
    frame $w.f.t  
    frame $w.f.b
    pack $w.f.t -fill both -expand 1
    pack $w.f.b -fill x
    button $w.f.b.b -text Parse -command "set pos_filter_continue go"
    button $w.f.b.t -text Tagger -command "set pos_filter_continue tagger"
    button $w.f.b.c -text Cancel -command "set pos_filter_continue stop"
    button $w.f.b.d -text NonInteractive -command "set pos_filter_continue go_nonint"
    button $w.f.b.r -text {Restore Previous} -command "prolog alpino_lexical_analysis:restore_used_tags"
    button $w.f.b.rp -text {Parse with Previous} -command {prolog alpino_lexical_analysis:restore_used_tags; set pos_filter_continue go}
    pack $w.f.b.b $w.f.b.t $w.f.b.d $w.f.b.c -side left -fill x
    pack $w.f.b.r $w.f.b.rp -side right -fill x
    canvas $w.f.t.c -bd 2 -yscrollcommand "$w.f.t.y set" \
	-xscrollcommand "$w.f.t.x set" \
	-scrollregion "-30 -30 19970 19970"
    scrollbar $w.f.t.x -orient horiz -command "$w.f.t.c xview"
    scrollbar $w.f.t.y -command "$w.f.t.c yview"
    pack $w.f.t.x -side bottom -fill x
    pack $w.f.t.y -side left -fill y
    pack $w.f.t.c  -fill both -expand 1
    return $w.f.t.c
}

proc extend_parse_widget { } {
    global pp_nr internal_pp_answer last_cmd_no module
    entry .pp.top.nr -bd 1 -width 10 -textvariable pp_nr
    bind .pp.top.nr <Return> { prolog $module:put_sentence_key($pp_nr) }
    bind .pp.top.nr <2> { prolog $module:put_sentence_key($pp_nr) }
    bind .pp.top.nr <3> {
			 prolog $module:put_sentence_key($pp_nr)
			 prolog $module:put_sentence_key_and_parse($pp_nr)
			}
    pack .pp.top.nr -side right
    bind .pp.top.left <2> {
    set internal_pp_answer $pp_answer
    regsub -all "\\\\" $internal_pp_answer "\\\\\\\\" internal_pp_answer
    regsub -all "%%"   $internal_pp_answer "%%%%"       internal_pp_answer
    regsub -all '   $internal_pp_answer "\\\\\'"      internal_pp_answer
    regsub -all " " $internal_pp_answer "'\} \{\'" internal_pp_answer
    set internal_pp_answer [format "\{'$internal_pp_answer'\}" ]

    set last_cmd_no [expr $last_cmd_no+1]
    set current_cmd_no $last_cmd_no
    set pp_answer_history($last_cmd_no) $internal_pp_answer

    prolog $module:parse_with_tags(\[[join $internal_pp_answer ,]\])
                          }

    bind .pp.top.left <3> {prolog $module:next_unannotated_sentence_key}
       help_line .pp.top.left "<1> Parse this sentence <2> Parse sentence again, with POS-tags assigned previously <3> Parse next unannotated sentence"
       

}

.menu.generate.m add command -label "Generate from Xml" -command {
    set filename [tk_getOpenFile]
    if {"$filename" != ""} {
	prolog $module:generate_from_treebank('$filename')
    }
}

.menu.generate.m add command -label "Generate from Xml ADT" -command {
    set filename [tk_getOpenFile]
    if {"$filename" != ""} {
	prolog $module:generate_from_xml_adt('$filename')
    }
}

.menu.generate.m add command -label "Inactive edges" -command {
		prolog $module:update_inactive_edges
		set edge [hdrug_qbox_array .l inactive_edges "Select" " Select an edge:"]
		if {$edge != 0} {
			if [regexp {^.inactive_edge\(([0-9]+)} $edge match number ] then {
				prolog alpino_cg:show_edge($number,tree(deriv),clig)
			}
		}
		unset edge
	} -underline 0 -accelerator {G I}
