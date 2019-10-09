##############################################################################
# Misc. Procedures for files etc. in the main menu
# v: 0.1
# author karsten konrad, konrad@coli.uni-sb.de
# last update: 20-Sep-95
##############################################################################


#
# Loading and saving files
#

#proc read-file {file} { ;# opens file and reads content
        #set f [open $file]
        #set cont ""
        #while {[gets $f line] >= 0} {
        #        set cont $line}
#        source $file}

proc load-clig-file {} { ;# selects and shows file
        global cligdir clig_globals
        update
        set file [exec $cligdir/FileSelect -name "Load Graph"]
        if {$file!=""} {source $file
            set clig_globals(print_file) "$file.ps"}}


proc save-object {file obj} { ;# opens file and reads content
        set f [open $file w]
        puts $f [list clig $obj]
        close $f}

proc save-clig-file {obj} { ;# selects and shows file
        global cligdir
        update
        set file [exec $cligdir/FileSelect -name "Save Graph"] 
        if {$file!=""} {save-object $file $obj}}

#
# stdin input
#

proc read-clig {} { ;# reads from stdin
}


###############################################################################
# Printer menu
###############################################################################

set clig_globals(print_file) "~/tmp/graph.ps" 
;# gets reseted when loading a file

proc print-graphics {fit} { ;# prints graphics to file
        global clig_globals font_translate
        if {$fit} {.graphics postscript -file $clig_globals(print_file) \
                        -fontmap font_translate -pageheight 27c \
                        -pagewidth 18c} \
        else {.graphics postscript -file $clig_globals(print_file) \
                        -fontmap font_translate}}

proc open-print-menu {} { ;# selection for printer
        global clig_globals standard_font
        toplevel .print
        # filename entry area
        frame .print.fname
        label .print.l1 -text "Filename:" -font $standard_font(10)
        entry .print.file -width 40 -relief sunken -bd 2 \
                -textvariable clig_globals(print_file)
        # options
        frame .print.foptions -borderwidth 5
        checkbutton .print.fit -text "fit to DIN A4" \
                -variable clig_globals(print_fit) 
        # print to file
        button .print.postscript -text "Create Postscript" \
          -command {print-graphics $clig_globals(print_fit); destroy .print}
        # leave menu
        button .print.cancel -text "Cancel" \
          -command {destroy .print}
        pack .print.l1 .print.file -side left -in .print.fname
        pack .print.fname
        pack .print.fit -side left
        pack .print.postscript .print.cancel -side right
        tkwait visibility .print ;# locking input when printer is open
        grab set -global .print
        tkwait window .print}


##############################################################################
# viewing the sources
##############################################################################

proc open-source-window {text} { ;# opens the source window
       global main_object
       if {[winfo exists .editor]} {destroy .editor}
       toplevel .editor
       frame .editor.tm
       button .editor.tm.clig -text "send back to Clig" -bd 1 \
        -command {eval "clig [.editor.text get 1.0 end]"}
       button .editor.tm.close -text "close" -bd 1 \
        -command {destroy .editor}
       text .editor.text -relief sunken -bd 1 -bg ivory \
        -yscrollcommand ".editor.scroll set"
       scrollbar .editor.scroll -command ".editor.text yview" -width 10
      pack .editor.tm.clig .editor.tm.close -side left -fill x
      pack .editor.tm -fill x
      pack .editor.text -in .editor -side left -fill y
      pack .editor.scroll -in .editor -side right -fill y   
      .editor.text insert end "{$text}"
      focus .editor.text}

proc change-source-text {text} { ;# change after loading
       global main_object
       set main_object $text
       if {[winfo exists .editor]} \
        {.editor.text delete 1.0 end 
         .editor.text insert end "{$text}"}}

