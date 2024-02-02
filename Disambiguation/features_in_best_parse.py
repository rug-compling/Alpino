#!/usr/bin/env python3

import sys
import errno

def score(iscore):
    (overlap,correct,system,postaglemma,postaglemma_total) = iscore.split('|')
    n = max(int(correct),int(system))
    pen = n-int(overlap)
    if n:
        return 100*(1-pen/n)
    else:
        return 100


def printit(features):
    flist = features.split('|')
    for pair in flist:
        try:
            fields = pair.split('@')
            count=fields[0]
            feat='@'.join(fields[1:])
            print(feat)
        except Exception as e:
            if e.errno == errno.EPIPE:
                raise
            print(pair,file=sys.stderr)
            print(e,file=sys.stderr)


def main():
    prev_key=""
    best_score=0

    for line in sys.stdin:
        try:
            fields = line.rstrip().split('#')   #            (key,ilen,iscore,features)
            key = fields[0]
            ilen = fields[1]
            iscore = fields[2]
            features = '#'.join(fields[3:])
            if not key == prev_key:
                if prev_key:
                    printit(best_features)
                prev_key=key
                best_features=features
                best_score=score(iscore)
            else:
                next_score=score(iscore)
                if next_score > best_score:
                    best_features=features
                    best_score=next_score
        except Exception as e: 
            if e.errno == errno.EPIPE:
                raise
            print("ignoring line {}".format(line[:70]),file=sys.stderr)
            print(e,file=sys.stderr)

    if key:
        printit(best_features)


if __name__ == "__main__":
    main()

