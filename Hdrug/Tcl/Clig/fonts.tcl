##############################################################################
# fonts.tcl -- screen and printer fonts
# v: 1.1
# author karsten konrad, konrad@coli.uni-sb.de
# last update: 13-Nov-95
##############################################################################

###
# fonts
### 

# standard_font is the standard proportional helvetica font
# special_font is the symbol font (greek chars) of the same size
# bold and italics_font are variants of standard of the same size

proc font-initialize {} { ;# set tab entries for fonts
    global standard_font special_font bold_font slant_font
    global font_translate
    foreach i {2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 \
                 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34} {
      set standard_font($i) "-adobe-helvetica-medium-r-*-*-$i-*-*-*-*-*-iso8859-*"
      set special_font($i)  "-adobe-symbol-*-*-*-*-$i-*-*-*-*-*-adobe-*"
      set bold_font($i) "-adobe-helvetica-bold-r-*-*-$i-*-*-*-*-*-iso8859-*"
      set slant_font($i) "-adobe-helvetica-medium-o-*-*-$i-*-*-*-*-*-iso8859-*"
      set font_translate($standard_font($i)) [list Helvetica $i]
      set font_translate($bold_font($i)) [list Helvetica-Bold $i]
      set font_translate($special_font($i)) [list Symbol $i]
      set font_translate($slant_font($i)) [list Helvetica-Italics $i]}}

font-initialize

set sfont $special_font(8)

set clig_globals(fontsize) 10


#
# zoom font table -- fontsize to zoom ratio
#

proc zoom-initialize {} { ;# sets up tab for zooming relations
      global standard_font font_zoom
      # reference string
      set test_string "XXXPlease do not distributeXXX"
      # size of reference string (x only)
      foreach t {10} {
        set ref_size [lindex [draw-plain-font-text \
                                0 0 .invisible "" $standard_font($t) \
                                $test_string] 0].0
        foreach i {2 4 6 8 10 12 14 16 18 \
                    20 22 24 26 28 30 32 34} {
           set size [lindex [draw-plain-font-text \
                                 0 0 .invisible "" $standard_font($i) \
                                 $test_string] 0]
           set font_zoom($i,$t) [expr $size/$ref_size]}}}


# zoom-initialize gets called with open-clig
