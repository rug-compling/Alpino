
proc help-general {} { ;# general help
     show {bigbox {leftstack 
                {bold-text "General help:"}
                {plain-text ""}
                {plain-text "1. Try loading some examples from the examples"}
                {plain-text "    directory with Load... in the File-menu."}
                {plain-text " "}
                {plain-text "2. Some things are mouse-sensitive. The cursor"}
                {plain-text "    will change from an arrow to a hand, when"}
                {plain-text "    you point on them. Try the CLICK-ME file to"}
                {plain-text "    see what is possible."}
                {plain-text ""}
                {plain-text "3. You can load all examples into your editor"}
                {plain-text "    and see how it is done."}}}}

proc help-file {} { ;# file menu help
      show {bigbox {leftstack
                {bold-text "The File-menu:"}
                {plain-text ""}
                {plain-text "New window -- opens a new Clig so that you"}
                {plain-text "   can have several windows open."}
                {plain-text ""}
                {plain-text "Load... -- loads a Clig-file. All files can be"}
                {plain-text 
                        "   loaded and viewed in a standard text editor."}
                {plain-text ""}
                {plain-text "Save... -- saves the current graph into a file."}
                {plain-text ""}
                {plain-text 
                        "Print... -- prints current graph on an Postscript"}
                {plain-text "   printer or saves it as a .ps-file"}
                {plain-text ""}
                {plain-text "Exit -- leaves the program"}
                        }}}

proc help-view {} { ;# file menu view
        show {bigbox {leftstack
                {bold-text "The View-menu:"}
                {plain-text ""}
                {plain-text "Redisplay -- redisplays curent graph."}
                {plain-text ""}
                {plain-text "Fit Window -- fit window to current graph."}
                {plain-text ""}
                {plain-text "View Top -- shows newest object in stack."}
                {plain-text ""}
                {plain-text "Clear Stack - removes all graphs from stack."}
                {plain-text ""}
                {plain-text "Tree HSpace -- selects the space between"}
                {plain-text "    daughters in trees."}
                {plain-text ""}
                {plain-text "Tree Vspace -- selects space between mothers"}
                {plain-text "    and their daughters in trees."}
                {plain-text ""}
                {plain-text "Zooming -- chooses display area size."}
                {plain-text ""}
                {plain-text "No Colors -- switches colors off and on."}}}}

proc help-arrows {} { ;# file menu view
        show {bigbox {leftstack
                {bold-text "The arrows:"}
                {plain-text ""}
                {plain-text "The arrows allow you to view all items in the"}
                {plain-text "object stack. Use them to display graphs"}
                {plain-text "that have been displayed earlier (left arrow)"}
                {plain-text "or later (right arrow) than the current one."}}}}

show {bigbox {Stack {leftstack 
                        {plain-text "I can give you some hints about the"}
                        {plain-text "following:"}
                        {plain-text ""}}
            {clickable {color red {underline {plain-text "General help"}}}
                 {help-general}}
            {clickable {color red {underline {plain-text "File menu"}}}
                 {help-file}}
            {clickable {color red {underline {plain-text "View menu"}}}
                 {help-view}}
            {clickable {color red {underline {plain-text "Arrows"}}}
                 {help-arrows}}
            {plain-text ""}}}




