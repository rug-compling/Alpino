#!/usr/bin/env python3

import sys

def main():
    for line in sys.stdin:
        (w,t,k,i,j,h,p,r,c,s) = line.rstrip().split("|")
        if t == "adjective(ge_no_e(padv))" and r == "geïrriteerd":
            t = "adjective(ge_no_e(both))"
        if t == "adjective(er(padv))" and r == "geïrriteerder":
            t = "adjective(er(both))"
        if t == "adjective(no_e(adv))" and r == "centraal":
            t = "adjective(postn_no_e(adv))"
        if t == "modal_adverb(noun_prep)" and r == "waarschijnlijk":
            t = "modal_adverb(adv_noun_prep)"
        if t == "modal_adverb(noun_prep)" and r == "wellicht":
            t = "modal_adverb(adv_noun_prep)"
        if t == "modal_adverb(noun_prep)" and r == "uiterlijk":
            t = "modal_adverb(adv_noun_prep)"
        if t == "modal_adverb(noun_prep)" and r == "misschien":
            t = "modal_adverb(adv_noun_prep)"
        if t == "adjective(ge_no_e(adv))"  and r == "gezien":
            t = "adjective(ge_no_e(both))"
        if t == "adjective(ge_no_e(adv),pp(bij))"  and r == "gezien":
            t = "adjective(ge_no_e(both),pp(bij))"
        if t == "adjective(ge_no_e(adv),fixed([[tegemoet]]))"  and r == "gezien":
            t = "adjective(ge_no_e(both),fixed([[tegemoet]]))"
        if t == "tag" and h == "normal(enumeration)":
            t = "enumeration"
        if t == "adjective(no_e(adv))" and r == "dankbaar":
            t = "adjective(no_e(both))"
        if t == "adjective(er(adv))" and r == "dankbaarder":
            t = "adjective(er(both))"
        if t == "adjective(ge_no_e(adv))" and r == "voor_bereiden":
            t = "adjective(ge_no_e(both))"
        if t == "adjective(ge_no_e(adv))" and r == "onvoorbereid":
            t = "adjective(ge_no_e(both))"
        if t == "noun(de,count,sg)" and r == "vlucht":
            t = "tmp_noun(de,count,sg)"
        if t == "intensifier" and r == "meer dan":
            t = "sentence_adverb"
        if t == "intensifier" and r == "meer als":
            t = "sentence_adverb"
        if t == "intensifier" and r == "minder dan":
            t = "sentence_adverb"
        if t == "intensifier" and r == "minder als":
            t = "sentence_adverb"
        if t == "intensifier" and r == "niet meer dan":
            t = "sentence_adverb"
        if t == "intensifier" and r == "niet meer als":
            t = "sentence_adverb"
        if t == "intensifier" and r == "niet minder dan":
            t = "sentence_adverb"
        if t == "intensifier" and r == "niet minder als":
            t = "sentence_adverb"
        print("{}|{}|{}|{}|{}|{}|{}|{}|{}|{}".format(w,t,k,i,j,h,p,r,c,s))


if __name__ == "__main__":
    main()
