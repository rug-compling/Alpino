#!/usr/bin/env python3

import sys

def main():
    for line in sys.stdin:
        (w,t,k,i,j,h,p,r,c,s) = line.rstrip().split("|")
        if t == "pronoun(nwh,thi,sg,both,both,indef,strpro)" and r == "één":
            t = "pronoun(nwh,thi,sg,de,both,indef,strpro)"
        print("{}|{}|{}|{}|{}|{}|{}|{}|{}|{}".format(w,t,k,i,j,h,p,r,c,s))


if __name__ == "__main__":
    main()
