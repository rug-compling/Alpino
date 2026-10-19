#!/usr/bin/env python3

import sys


def report(line, counter):
    print("{}\t{}".format(line.rstrip(), counter))


def main():
    """
    can be used to count repeated lines and then prints these counts after each line
    in the format
    line[TAB]count
    input can also have that same shape, and then the corresponding counts of identical lines
    are summed

    a
    a
    a
    b
    b
    c
    ==>
    a	3
    b	2
    c	1

    and
    a	3
    a	4
    b	2
    b	4
    c	1
    ==>
    a	7
    b	6
    c	1

    the idea is that you count using the first option, and then merge counts files using
    sort -m and the second option.

    export LC_ALL=C
    sort a.txt | uniq-c.py > a.cnts
    sort b.txt | uniq-c.py > b.cnts
    merge a.cnts b.cnts | uniq-c.py > all.cnts
    
    
    """
    started = False
    prev = ""
    counter = 0
    for line in sys.stdin:
        line = line.rstrip()
        if "\t" in line:
            (cline,ccountstr) = line.split('\t')
            try:
                ccount = int(ccountstr)
            except ValueError:
                print("wrong line: {}".format(line),file=sys.stderr)
                continue
        else:
            cline = line
            ccount = 1
        if started and cline == prev:
            counter += ccount
        elif started:
            report(prev, counter)
            prev = cline
            counter = ccount
        else:
            prev = cline
            counter = ccount
        started = True
    report(prev, counter)
    
    
if __name__ == "__main__":
    main()
