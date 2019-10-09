#!/usr/bin/python
#
# WIP: feature counts seem off...
#

import optparse
import sys

VELLDAL_FEATURES = ['lds', 'ldsb', 'lds_dl', 'lds_skew', 'tngram', 'tngramw']

PARSE_FEATURES = ['appos_person', 'dep23', 'dep34', 'dep35', 'depprep',
                  'depprep2', 'dist', 'f1' , 'f2', 'h1', 'in_year', 'mf',
                  'p1', 'r1', 'r2', 's1', 'sdep', 'z_appos_person',
                  'z_dep35', 'z_depprep', 'z_depprep2', 'z_f2']

DEP_FEATURES = ['lds_deps']

TAG_NGRAM_FEATURES = ['ngram_tag']

if __name__ == "__main__":
    parser = optparse.OptionParser()
    parser.add_option("-p", "--no-parse-features", action = "store_false",
                      dest = "parseFeatures", default = True,
                      help = "don't include parsing features in the evaluation")
    parser.add_option("-d", "--no-dependency-features", action = "store_false",
                      dest = "depFeatures", default = True,
                      help = "don't include lds dependency features in the evaluation")
    parser.add_option("-t", "--no-tag-ngram-features", action = "store_false",
                      dest = "tagNgramFeatures", default = True,
                      help = "don't include tag ngram features in the evaluation")
    parser.add_option("-v", "--no-velldal-features", action = "store_false",
                      dest = "velldalFeatures", default= True,
                      help = "don't include Velldal features")

    (options, args) = parser.parse_args()
    parseFeatures = options.parseFeatures
    depFeatures = options.depFeatures
    tagNgramFeatures = options.tagNgramFeatures
    velldalFeatures = options.velldalFeatures

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

        print line

