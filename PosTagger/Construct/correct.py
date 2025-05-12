#!/usr/bin/env python3

import sys

def main():
    MAXL = 1000
    for line in sys.stdin:
        try:
            (w,t,k,i,j,h,p,r,c,s) = line.rstrip().split("|")
        except ValueError:
            print("warning: cannot parse line: {}".format(line),file=sys.stderr)
            continue
        ### the next few lines are for some sentences in wikipedia that consist of very long s
        ### sequences of underscores
        if len(w) > MAXL:
            w = w[:MAXL]
        if len(r) > MAXL:
            r = r[:MAXL]
        ### correct "old" files after changes in lexicon
        if r == "verspreiden"  and t == "adjective(er(adv))":
            t = "adjective(er(both))"
        if r == "verspreiden"  and t == "adjective(ge_no_e(adv))":
            t = "adjective(ge_no_e(both))"
        if t == "adverb" and r == "nagenoeg":
            t = "sentence_adverb"
        if t == "adjective(ge_no_e(padv))" and r == "irriteren":
            t = "adjective(ge_no_e(both))"
        if t == "adjective(er(padv))" and r == "irriteren":
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
        if t == "adjective(er(adv))" and r == "dankbaar":
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
        if r == "oudsher van" and t == "with_dt(pp(van),dt(pp,[hd=l(van,preposition(van,[]),0,1),obj1=l(oudsher,adverb,advp,1,2)]))":
            t = "with_dt(sentence_adverb,dt(pp,[hd=l(van,preposition(van,[]),0,1),obj1=l(oudsher,adverb,advp,1,2)]))"
        if t == "verb(hebben,sg_heeft,transitive_ndev)":
            t = "verb(hebben,sg_heeft,transitive_ndev_npas)"
        if r == "nog eens" and t == "modal_adverb(noun)":
            t = "with_dt(modal_adverb(noun),dt(advp,[hd=l(nog,adverb,advp,0,1),mod=l(eens,adverb,advp,1,2)]))"
        if r == "eens nog" and t == "with_dt(modal_adverb(noun),dt(advp,[mod=l(nog,adverb,advp,0,1),hd=l(eens,adverb,advp,1,2)]))":
            r = "nog eens"
            t = "with_dt(modal_adverb(noun),dt(advp,[hd=l(nog,adverb,advp,0,1),mod=l(eens,adverb,advp,1,2)]))"
        if r == "frustreren" and t == "adjective(ge_no_e(adv))":
            t = "adjective(ge_no_e(padv))"
        if r == "frustreren" and t == "adjective(er(adv))":
            t = "adjective(er(padv))"
        if r == "verwoesten" and t == "adjective(ge_no_e(adv))":
            t = "adjective(ge_no_e(padv))"
        print("{}|{}|{}|{}|{}|{}|{}|{}|{}|{}".format(w,t,k,i,j,h,p,r,c,s))


if __name__ == "__main__":
    main()
