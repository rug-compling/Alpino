#
# menues: standard menu commands for Clig
# last update: 26-Oct-95
#

set menu_counter 0 ;# for name definitions

proc define-popup {entries} { ;# menu definition from entry list
        global menu_counter
        set menu_counter [expr $menu_counter+1]
        set name ".pop$menu_counter"
        menu $name -tearoff 0
        foreach item $entries {
                eval "$name add $item"}
        return $name}

#proc show-popup {x y menu} { ;# popup the menu
#    $menu post $x $y           ;# waiting for the menu window
#    tkwait visibility $menu
#    update idletasks
#    grab set -global $menu
#    tkwait window $menu
#    update idletasks}         

proc show-popup {x y menu} { ;# popup the menu
    tk_popup $menu  $x $y           ;# waiting for the menu window
}         
