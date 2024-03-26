#!/usr/bin/env python3

import sys

def main():
    for line in sys.stdin:
        (w,t,k,i,j,h,p,r,c,s) = line.rstrip().split("|")
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
        print("{}|{}|{}|{}|{}|{}|{}|{}|{}|{}".format(w,t,k,i,j,h,p,r,c,s))


if __name__ == "__main__":
    main()
