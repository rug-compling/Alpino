#!/usr/bin/env python3

import sys

def main():
    for line in sys.stdin:
        (w,t,k,i,j,h,p,r,c,s) = line.rstrip().split("|")
        if t == "pronoun(nwh,thi,sg,both,both,indef,strpro)" and r == "één":
            t = "pronoun(nwh,thi,sg,de,both,indef,strpro)"
        if t == "adjective(ge_no_e(both))" and r == "bedroeven":
            t = "adjective(no_e(both))"
            r = "bedroefd"
        if t == "adjective(ge_no_e(both))" and r == "bedroefd":
            t = "adjective(no_e(both))"
        if t == "modal_adverb(noun_prep)" and r == "ongeveer":
            t = "modal_adverb(adv_noun_prep)"
        print("{}|{}|{}|{}|{}|{}|{}|{}|{}|{}".format(w,t,k,i,j,h,p,r,c,s))


if __name__ == "__main__":
    main()
