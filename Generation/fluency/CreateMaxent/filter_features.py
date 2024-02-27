#!/usr/bin/env python3
#
# WIP: feature counts seem off...
#

import argparse
import io
import sys

VELLDAL_FEATURES = ['lds', 'ldsb', 'lds_dl', 'lds_skew', 'tngram', 'tngramw']

PARSE_FEATURES = ['appos_person', 'dep23', 'dep34', 'dep35', 'depprep',
                  'depprep2', 'dist', 'f1' , 'f2', 'h1', 'in_year', 'mf',
                  'p1', 'r1', 'r2', 's1', 'sdep', 'z_appos_person',
                  'z_dep35', 'z_depprep', 'z_depprep2', 'z_f2']

DEP_FEATURES = ['lds_deps']

TAG_NGRAM_FEATURES = ['ngram_tag']

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", "--no-parse-features", action = "store_false",
                      dest = "parseFeatures", default = True,
                      help = "don't include parsing features in the evaluation")
    parser.add_argument("-d", "--no-dependency-features", action = "store_false",
                      dest = "depFeatures", default = True,
                      help = "don't include lds dependency features in the evaluation")
    parser.add_argument("-t", "--no-tag-ngram-features", action = "store_false",
                      dest = "tagNgramFeatures", default = True,
                      help = "don't include tag ngram features in the evaluation")
    parser.add_argument("-v", "--no-velldal-features", action = "store_false",
                      dest = "velldalFeatures", default= True,
                      help = "don't include Velldal features")

    options = parser.parse_args()
    parseFeatures = options.parseFeatures
    depFeatures = options.depFeatures
    tagNgramFeatures = options.tagNgramFeatures
    velldalFeatures = options.velldalFeatures

    # sys.stdin.reconfigure(encoding='utf-8')
    # sys.stdout.reconfigure(encoding='utf-8')
    sys.stdin = io.TextIOWrapper(sys.stdin.detach(), encoding='utf-8', newline=None)
    sys.stdout = io.TextIOWrapper(sys.stdout.detach(), encoding='utf-8', newline=None, line_buffering=True)

    for line in sys.stdin:
        line = line.strip()
        if line.find('(') == -1:
            featureFun = line.split('|')[0]
        else:
            featureFun = line.split('(')[0]

        if not parseFeatures:
            if featureFun in PARSE_FEATURES:
                continue

        if not depFeatures:
            if featureFun in DEP_FEATURES:
                continue

        if not tagNgramFeatures:
            if featureFun in TAG_NGRAM_FEATURES:
                continue

        if not velldalFeatures:
            if featureFun in VELLDAL_FEATURES:
                continue

        print(line)
