#!/usr/bin/env python3



"""Example of using estimate to build MaxEnt categorizers."""

__author__ = "Rob Malouf <rmalouf@mail.sdsu.edu>"
__date__ = "12 February 2004"
__version__ = "$Revision: 1.2 $"

import os
import sys
import re
import tempfile
from math import log
import argparse
import pickle

try:
    import psyco
    psyco.full()
    from psyco.classes import *
except:
    pass

if sys.version_info.major == 2:
    my_intern = intern
else:
    my_intern = sys.intern

class Source:
    pass

class C4_5Source(Source):
    """Data source class for C4.5 files"""

    def __init__(self,infile):
        """Create a data source from a C4.5 file"""
        self.stream = open(infile)

    def __iter__(self):
        return self

    def __next__(self):

        # If we're at the end, rewind stream and stop

        line = self.stream.readline().rstrip()
        if line == '':
            self.stream.seek(0)
            raise StopIteration

        # get features

        if line[-1] == '.':
            line = line[:-1]
        items = line.split(',')

        # get category

        cl = my_intern(items.pop())

        # collect feature/value pairs

        j = 0
        for i in range(0,len(items)):
            items[i] = (j,my_intern(items[i]))
            j += 1
        return (items,cl,line)

    next = __next__

class RainbowSource(Source):
    """Data source class for files generated using rainbow -B sin"""

    binary = False
    def __init__(self,infile,binary=False):
        """Create a data source from a rainbow file"""
        self.stream = open(infile)
        self.binary = binary

    def __iter__(self):
        return self

    def __next__(self):

        line = self.stream.readline().rstrip()
        items = re.split(r'\s+',line)

        # if we're at the end, rewind stream and stop

        if items == ['']:
            self.stream.seek(0)
            raise StopIteration

        # get category

        cl = my_intern(items[1])

        # collect feature/value pairs

        pairs = []
        for i in range(2,len(items),2):
            if self.binary:
                pairs.append((items[i],1))
            else:
                pairs.append((items[i],items[i+1]))

        return (pairs,cl,line)

    next = __next__

class SparseSource(Source):
    """Data source class for Sparse Binary files"""

    def __init__(self,infile,names):
        """Create a data source from a Sparse Binary file"""
        self.stream = open(infile)

    def __iter__(self):
        return self

    def __next__(self):

        # if we're at the end, rewind stream and stop

        line = self.stream.readline().rstrip()
        if line == '':
            self.stream.seek(0)
            raise StopIteration

        # get features

        if line[-1] == '.':
            line = line[:-1]
        items = line.split(',')

        # get category (minus trailing period)

        cl = my_intern(items.pop())
        if cl[-1] == '.':
            cl = my_intern(cl[:-1])

        # collect feature/value pairs

        for i in range(0,len(items)):
            items[i] = (items[i],1)
        return (items,cl,line)

    next = __next__

class Sink:
    pass

class AccuracySink(Sink):
    """Result sink that collects accuracy statistics"""

    def __init__(self):
        self.total = 0
        self.right = 0
        self.matrix = {}
        self.tags = {}

    def update(self,feats,cl,best,write):
        if write:
            sys.stdout.write('{}\n'.format(best))
        if best is not None:
            self.total += 1
            if cl == best:
                self.right += 1
            self.tags[cl] = 1
            self.tags[best] = 1
            self.matrix[(cl,best)] = self.matrix.get((cl,best),0) + 1

    def score(self):
        """Returns accuracy so far"""
        return self.right / self.total

    def confusion(self):
        """Print confusion matrix"""
        sys.stdout.write('Confusion matrix\n\n\tas\nis\t ')
        sys.stdout.write('\t'.join(list(self.tags.keys())) + '\n')
        for tag1 in list(self.tags.keys()):
            sys.stdout.write('{}\t '.format(tag1))
            for tag2 in list(self.tags.keys()):
                sys.stdout.write('{}\t '.format(self.matrix.get((tag1,tag2),0)))
            sys.stdout.write('\n')

class Categorizer:

    def __init__(self):
        self.categories = {}
        self.features = {}

    def categorize(self,event):
        best_score = -1000000.0
        cat = ''
        for cand in list(self.categories.keys()):
            score = self.features.get((cand),0.0)
            for f,v in event:
                score += self.features.get((f,v,cand),0.0)
            if score > best_score:
                best_score = score
                cat = cand
        return cat

    def apply(self,source,sink,write):
        for feats,cl,line in source:
            if cl is None:
                sink.update(line,cl,None,write)
            else:
                sink.update(line,cl,self.categorize(feats),write)

    def dump(self):
        for cand in list(self.categories.keys()):
            sys.stdout.write("CATEGORY|%s|%s\n" % ( cand , self.features.get((cand),0.0) ))

        for feat in self.features:
            if feat not in ('LOC','ORG','MISC','PER'):
                sys.stdout.write("FEATURE|%s|%s|%s|%s\n" % ( feat[0],
                                                feat[1],
                                                feat[2] ,
                                                self.features[feat] ))

class Inducer:
    pass

class MaxEntInducer(Inducer):

    def train(self,source,eps=0,frtol=1e-8,var=0,lasso=0,quiet=False,\
              use_baseline=False,cutoff=0):
        """Construct a MaxEnt categorizer from a data source"""

        cat = MaxEntCategorizer()

        # Collect features

        counts = {}
        baseline = None

        for feats,cl,line in source:
            if cl is None:
                continue
            cat.categories[cl] = cat.categories.get(cl,0) + 1
            if use_baseline and baseline is None:
                baseline = cl
            for f,v in feats:
                counts[(f,v)] = counts.get((f,v),0) + 1

        # collect category statistics

        n = sum(cat.categories.values())
        for cl in cat.categories:
            cat.categories[cl] /= n

        # number features

        feats = 0
        for cl in cat.categories:
            if baseline != cl:
                cat.features[(cl)] = feats
                feats += 1
                for (f,v) in counts:
                    if counts[(f,v)] > cutoff:
                        cat.features[(f,v,cl)] = feats
                        feats += 1

        # generate events file

        self.make_events(source,cat,eps)

        # run estimate

        varopt = ''
        if var != 0:
            varopt = '-l2 %d ' % (var)
        elif lasso != 0:
            varopt = '-l1 %d ' % (lasso)

        command = 'estimate -monitor -events_in %s -params_out %s %s' % (self.eventfile,self.paramsfile,varopt)

        if quiet:
            os.system(command+' >/dev/null')
        else:
            sys.stderr.write('=======\n{}\n'.format(command))
            os.system(command)
            sys.stderr.write('=======\n')

        # load parameter values

        weights = [ float(w.rstrip())
                    for w in open(self.paramsfile)]

        cat.count = 0
        for i in range(0,len(weights)):
            if weights[i] != 0:
                cat.count = cat.count + 1

        for f,v in list(cat.features.items()):
            if v < len(weights):
                cat.features[f] = weights[v]
            else:
                cat.features[f] = 0.0

        # clean up temporary files

        os.unlink(self.eventfile)
        os.unlink(self.paramsfile)

        return cat

    def make_events(self,source,cat,eps=0):

        events = {}

        # read events

        for feats,cl,line in source:
            if cl is None:
                continue
            feats = tuple(feats)
            try:
                entry = events[feats]
                entry[cl] = entry.get(cl,0) + 1
            except KeyError:
                events[feats] = { cl : 1 }

        # output events

        self.eventfile=tempfile.mktemp()
        self.paramsfile=tempfile.mktemp()
        outfile = open(self.eventfile,'w')
        cl_list  = list(cat.categories.keys())

        for feats, entry in list(events.items()):
            outfile.write('{}\n'.format(len(cl_list)))
            for c in cl_list:
                codes = []
                if c in cat.features:
                    codes.append(cat.features[(c)])
                for f,v in feats:
                    if (f,v,c) in cat.features:
                        codes.append(cat.features[(f,v,c)])
                outfile.write('{} {} '.format(entry.get(c,0)+eps, len(codes)))
                for f in codes:
                    outfile.write('{} 1 '.format(f))
                outfile.write('\n')

        outfile.close()


class MaxEntCategorizer(Categorizer):
    pass

class NaiveBayesInducer(Inducer):

    def train(self,source,eps=0,quiet=False):
        """Construct a Naive Bayes categorizer from a data source"""
        
        cat = NaiveBayesCategorizer()

        # count features

        freq = {}
        features = {}

        for feats,cl,line in source:
            if cl is None:
                continue
            cat.categories[cl] = cat.categories.get(cl,0) + 1
            for f,v in feats:
                features[(f,v)] = 1
                freq[(f,v,cl)] = freq.get((f,v,cl),0) + 1
            
        # compute parameter values


        overflow = False
        for (f,v) in features:
            for cl in cat.categories:
                try:
                    cat.features[(f,v,cl)] = log(eps+freq.get((f,v,cl),0)) - \
                                             log(2.0*eps+cat.categories[cl])
                except OverflowError:
                    overflow = True

        if overflow:
            sys.stderr.write('Zero probabilities found -- try smoothing counts (-s)!\n')

        # collect category priors

        n = sum(cat.categories.values())
        for cl in cat.categories:
            cat.features[(cl)] = log(cat.categories[cl]) - log(n)

        return cat

class NaiveBayesCategorizer(Categorizer):
    pass

def main():

    # process options

    parser = argparse.ArgumentParser()
    parser.add_argument("-p", "--prior", type=float, default=0,
                      help="use Gaussion prior with variance VAR",
                      metavar="VAR")
    parser.add_argument("-a", "--algorithm", choices=['MaxEnt','NaiveBayes'],
                      default='MaxEnt',
                      help="use learning algoritm ALG (MaxEnt (def), NaiveBayes)",
                      metavar="ALG")
    parser.add_argument("-d", "--data-format", choices=['C45','Sparse','Rainbow'],
                      default='C45',
                      help="use data format FORM (C45 (def), Sparse, Rainbow)",
                      metavar="FORM")
    parser.add_argument("-s", "--smooth", type=float, default=0,
                      help="add EPS to all counts",
                      metavar="EPS")
    parser.add_argument("-m", "--model", action="store", default='',
                      help="use model FILE",
                      metavar="FILE")
    parser.add_argument("-f", "--train", action="store", default='',
                      help="read training data from FILE",
                      metavar="FILE")
    parser.add_argument("-t", "--test", action="store", default='',
                      help="read test data from FILE",
                      metavar="FILE")
    parser.add_argument("-q", "--quiet", action="store_true", default=False,
                      help="be quiet")
    parser.add_argument("-b", "--no-baseline", action="store_true", default=False,
                      help="don't use a baseline category")
    parser.add_argument("-c", "--confusion", action="store_true", default=False,
                      help="print confusion matrix")
    parser.add_argument("-y", "--display-parameters", action="store_true", default=False,
                      help="display all parameters")


    if True:
        sys.stderr.write('\nBayesian classifier\n\n')

    options = parser.parse_args()

    # Construct model

    if options.train == '':
        # no training file, so check model file
        if options.model != '':
            if not options.quiet:
                sys.stderr.write('Reading model from {}\n'.format(options.model))
            file = open(options.model,'r')
            me = pickle.load(file)
            file.close()
        else:
            sys.stderr.write('No training file or model given!\n')
            raise Exception
    else:
        if options.data_format == 'Rainbow':
            if not options.quiet:
                sys.stderr.write('Reading Rainbow training data from {}\n'.format(options.train))
            source = RainbowSource(options.train,binary=True)
        elif options.data_format == 'C45':
            if not options.quiet:
                sys.stderr.write('Reading C4.5 training data from {}\n'.format(options.train))
            source = C4_5Source(options.train)
        elif options.data_format == 'Sparse':
            if not options.quiet:
                sys.stderr.write('Reading Sparse Binary training data from {}\n'.format(options.train))
            source = SparseSource(options.train)
        else:
            raise Exception

        if options.algorithm == 'MaxEnt':
            sys.stderr.write('Building MaxEnt model\n')
            cat = MaxEntInducer().train(source, \
                                        quiet=options.quiet, \
                                        eps=options.smooth, \
                                        var=options.prior, \
                                        use_baseline=not options.no_baseline)
        elif options.algorithm == 'NaiveBayes':
            sys.stderr.write('Building naive Bayes model\n')
            cat =  NaiveBayesInducer().train(source, \
                                             quiet=options.quiet, \
                                             eps=options.smooth)

    # write model (if desired)

    if (options.train != '') and (options.model != ''):
        if (not options.quiet):
            sys.stderr.write('Writing model to {}\n'.format(options.model))
        file = open(options.model,'w')
        pickle.dump(cat,file,True)
        file.close()

    # evaluate model

    if options.test != '':
        if options.data_format == 'Rainbow':
            if not options.quiet:
                sys.stderr.write('Reading Rainbow test data from {}\n'.format(options.test))
            source = RainbowSource(options.test,binary=True)
        elif options.data_format == 'C45':
            if not options.quiet:
                sys.stderr.write('Reading C4.5 test data from {}\n'.format(options.test))
            source = C4_5Source(options.test)
        elif options.data_format == 'Sparse':
            if not options.quiet:
                sys.stderr.write('Reading Sparse Binary test data from {}\n'.format(options.test))
            source = SparseSource(options.test)
        else:
            raise Exception

        acc = AccuracySink()
        cat.apply(source,acc,options.write)

        if not options.write:
            sys.stdout.write('Accuracy = {}\n'.format(acc.score() * 100.0))

        if options.confusion:
            acc.confusion()

    if options.display_parameters:
        cat.dump()

if __name__ == '__main__':
    main()
