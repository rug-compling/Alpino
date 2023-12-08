#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Barbara, October 2010
import os
import sys
from argparse import ArgumentParser

numparses=0
parses=[]
scores=[]

# set to true if you wanna keep only best and worst parses
scoreBestWorstOnly=False

# by default: osborne=True (keep constant marginal)
# turn off with --no-osborne option
osborne=True


parser = ArgumentParser(os.path.basename(sys.argv[0]))
parser.add_argument("--no-osborne", dest='noOsborne',
                                        action='store_true',
                                        default=False,
                                        help='does not keep marginal constant')

options = parser.parse_args()

if options.noOsborne:
    osborne=False

def updateScores(scores, parses):
    idx = 0
    scoreBest = max(scores)
    scoreWorst = min(scores)
    numBestParses = scores.count(scoreBest)
    newScore = 1/float(numBestParses)
    if not scoreBestWorstOnly:
        for s in scores:
            if s == scoreBest:
                if not osborne:
                    parses[idx][0] = '1'
                else:
                    parses[idx][0] = str(newScore)
            idx+=1
    else:
        # keep only best and worst (works worse)
        subsetParses = []
        for s in scores:
            if s == scoreBest:
                parses[idx][0] = '1'
                subsetParses.append(parses[idx])
            elif s == scoreWorst:
                parses[idx][0] = '0'
                subsetParses.append(parses[idx])
            idx+=1
        parses = subsetParses
    return parses


for line in sys.stdin:
    line = line.strip()
    c = line.split()
    if len(c) == 1:
        numparses= c[0]
        if len(parses) > 0: #thus we are not at first line
            # update parse scores: those parses with scoreBest get a 1
            parses = updateScores(scores,parses)
            print(len(parses))
            for i in parses:
                print(' '.join(i))
        parses=[]
        scores=[]
    else:
        scores.append(c[0])
        c[0] = '0'
        parses.append(c)

# don't forget last one
if len(parses)> 0:
    parses = updateScores(scores,parses)
    print(len(parses))
    for i in parses:
        print(' '.join(i))
