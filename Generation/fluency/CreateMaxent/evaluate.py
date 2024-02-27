#!/usr/bin/env python3

import argparse
import io
import random
import re
import sys
from functools import reduce

# List element shuffling. We have random.shuffle, but it is in-place
# and does not seem to have such good properties. This should do for
# our purposes.
def shuffle(x):
    n = len(x)
    shuffled = []
    indicesSet = set()

    while len(shuffled) != n:
        pick = int(random.random() * n)
        if not pick in indicesSet:
            shuffled.append(x[pick])
            indicesSet.add(pick)

    return shuffled


# Bad habits? ;)
def endCurry(func, *curried):
    return lambda *args: func(*(args + curried))

def readFeatureWeights(fh):
    featureWeights = {}

    for line in fh:
        feature, weight = line.strip().rsplit('|', 1)

        featureWeights[feature] = -float(weight)

    return featureWeights

def featureValues(featurePairs):
    vals = {}

    for pair in featurePairs:
        val, feature = pair.split('@', 1)

        vals[feature] = float(val)

    return vals


def scoreSent(featureWeights, featureValues):
    score = 0.0

    for key, value in featureValues.items():
        weight = featureWeights.get(key, 0.0)
        score += weight * value

    return score

def scoreSents(sents, featureWeights):
    scoredSents = []
    for sent in sents:
        sentParts = sent.split('#')
        featurePairs = sentParts[4].split(r'|')
        vals = featureValues(featurePairs)
        score = scoreSent(featureWeights, vals)
        scoredSents.append((score, vals['ngram_lm'], vals['ngram_tag'], 0.0, float(sentParts[3])))

    return scoredSents

def sentKey(x, scoreField):
    return x[scoreField]

def evalScoredSents(scoredSents, scoreField = 0, inverse = False):
    keyFun0 = sentKey
    keyFun0 = endCurry(keyFun0, scoreField)
    scoredSents = sorted(scoredSents, key=keyFun0, reverse=inverse)

    return scoredSents[0][4]

def processSents(fh, featureWeights, minRealizations):
    prevSent = 0
    sents = []
    scores = []
    lmScores = []
    tagScores = []
    randomScores = []
    bestScores = []
    worstScores = []

    for line in fh:
        line = line.strip()

        sentMatch = re.match(r'G#([^#]+)#', line)
        if sentMatch is None:
            continue

        sent = sentMatch.group(1)
        if sent != prevSent:
            if len(sents) >= minRealizations:
                # Let's shuffle the realizations, to prevent that for
                # some reason the first realization is picked, giving
                # interference between models.
                sents = shuffle(sents)
                # To avoid confusion, the actual scores returned are
                # not scores of the MaxEnt model or n-gram model, but the
                # GTM scores of the best sentence selected according
                # to these models.
                scoredSents = scoreSents(sents, featureWeights)
                scores.append(evalScoredSents(scoredSents))
                lmScores.append(evalScoredSents(scoredSents, 1))
                tagScores.append(evalScoredSents(scoredSents, 2))
                randomScores.append(evalScoredSents(scoredSents, 3))
                bestScores.append(evalScoredSents(scoredSents, 4, True))
                worstScores.append(evalScoredSents(scoredSents, 4, False))
            sents = []
            prevSent = sent

        sents.append(line)

    return (scores, lmScores, tagScores, randomScores, bestScores, worstScores)

def printResults(scores, bestScores, randomScores, worstScores):
    if not scores:
        return
    scoreSum = sum(scores)
    scoreAvg = scoreSum / len(scores)
    exactMatches = reduce(lambda x, y: x + 1 if y == 1.0 else x, scores, 0)
    exactMatchRatio = float(exactMatches) / len(scores)
    bestMatches = reduce(lambda x, y: x + 1 if y[0] == y[1] else x,
                         list(zip(scores, bestScores)), 0)
    bestMatchRatio = float(bestMatches) / len(scores)
    relScores = [(s_b_w[0] - s_b_w[2]) / (s_b_w[1] - s_b_w[2]) if s_b_w[1] != s_b_w[2] else 1.0 for s_b_w in zip(scores, bestScores, worstScores)]
    relScoreAvg = sum(relScores) / len(relScores)

    bestAvg = sum(bestScores) / len(bestScores)
    randomAvg = sum(randomScores) / len(randomScores)
    rndRelAvg = (scoreAvg - randomAvg) / (bestAvg - randomAvg)
    #rndRelScores = map(lambda (s, b, r): (s - r) / (b - r) if b != r else s / r, zip(scores, bestScores, randomScores))
    #rndRelScoreAvg = sum(rndRelScores) / len (rndRelScores)

    print("Average GTM score: %f" % scoreAvg)
    print("Avg relative GTM score (worst): %f" % relScoreAvg)
    print("Avg relative GTM score (random): %f" % rndRelAvg)
    print("Exact matches: %f" % exactMatchRatio)
    print("Best matches: %f" % bestMatchRatio)

def printModelScores(scores, bestScores):
    bestMatches = [1 if x[0] == x[1] else 0.0 for x in zip(scores, bestScores)]

    for score in zip(scores, bestMatches):
        print(score[0], score[1])

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--features", dest = "features",
                      default = "train.weights", help = "Feature weight file")
    parser.add_argument("-r", "--realizations", dest = "realizations",
              default = "1", help = "Minimum number of realizations")
    parser.add_argument("-s", "--scores", dest = "modelScores",
          action="store_true", default = False, help = "Print model scores")

    options = parser.parse_args()

    # Make results reproducable.
    random.seed(13)

    # sys.stdin.reconfigure(encoding='utf-8')
    # sys.stdout.reconfigure(encoding='utf-8')
    sys.stdin = io.TextIOWrapper(sys.stdin.detach(), encoding='utf-8', newline=None)
    sys.stdout = io.TextIOWrapper(sys.stdout.detach(), encoding='utf-8', newline=None, line_buffering=True)


    with open(options.features, encoding='utf-8') as featureFh:
        featureWeights = readFeatureWeights(featureFh)

    minRealizations = int(options.realizations)

    (scores, lmScores, tagScores, randomScores, bestScores, worstScores) = processSents(
        sys.stdin,
        featureWeights,
        minRealizations)

    if options.modelScores:
        printModelScores(scores, bestScores)
        sys.exit(0)

    print("--- Overview ---")
    print("Inputs: %d" % len(scores))

    # print "--- random model ---"
    # printResults(randomScores, bestScores, randomScores, worstScores)

    # print "--- word ngram model ---"
    # printResults(lmScores, bestScores, randomScores, worstScores)

    # print "-- tag ngram model ---"
    # printResults(tagScores, bestScores, randomScores, worstScores)

    print("--- maxent model ---")
    printResults(scores, bestScores, randomScores, worstScores)
