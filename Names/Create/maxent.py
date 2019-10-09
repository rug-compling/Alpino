#!/usr/bin/env python

from __future__ import division

"""Example of using estimate to build MaxEnt categorizers."""

__author__ = "Rob Malouf <rmalouf@mail.sdsu.edu>"
__date__ = "12 February 2004"
__version__ = "$Revision: 1.2 $"

import os
import sys
import re
import string
import tempfile
from math import log
import optparse
import cPickle

try:
    import psyco
    psyco.full()
    from psyco.classes import *
except:
    pass

class Source:
    pass

class C4_5Source(Source):
    """Data source class for C4.5 files"""

    def __init__(self,infile):
        """Create a data source from a C4.5 file"""
        self.stream = open(infile)
        return

    def __iter__(self):
        return self 

    def next(self):

        # If we're at the end, rewind stream and stop

        line = self.stream.readline().rstrip()
        if (line == ''):
            self.stream.seek(0)
            raise StopIteration

        # get features

        if (line[-1] == '.'):
            line = line[:-1]
        items = line.split(',')

        # get category
        
        cl = intern(items.pop())

        # collect feature/value pairs

        j = 0        
        for i in xrange(0,len(items)):
            items[i] = (j,intern(items[i]))
            j += 1
        return (items,cl,line)

class RainbowSource(Source):
    """Data source class for files generated using rainbow -B sin"""

    binary = False
    def __init__(self,infile,binary=False):
        """Create a data source from a rainbow file"""
        self.stream = open(infile)
        self.binary = binary
        return

    def __iter__(self):
        return self 

    def next(self):

        line = self.stream.readline().rstrip()
        items = re.split('\s+',line)
        
        # if we're at the end, rewind stream and stop

        if (items == ['']):
            self.stream.seek(0)
            raise StopIteration

        # get category 

        cl = intern(items[1])

        # collect feature/value pairs

        pairs = []
        for i in xrange(2,len(items),2):
            if (self.binary):
                pairs.append((items[i],1))
            else:
                pairs.append((items[i],items[i+1]))

        return (pairs,cl,line)

class SparseSource(Source):
    """Data source class for Sparse Binary files"""

    def __init__(self,infile,names):
        """Create a data source from a Sparse Binary file"""
        self.stream = open(infile)
        return

    def __iter__(self):
        return self 

    def next(self):

        # if we're at the end, rewind stream and stop

        line = self.stream.readline().rstrip()
        if (line == ''):
            self.stream.seek(0)
            raise StopIteration

        # get features

        if (line[-1] == '.'):
            line = line[:-1]
        items = line.split(',')

        # get category (minus trailing period)

        cl = intern(items.pop())
        if (cl[-1] == '.'):
            cl = intern(cl[:-1])

        # collect feature/value pairs

        for i in xrange(0,len(items)):
            items[i] = (items[i],1)
        return (items,cl,line)
    
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
        if (write):
            print >> sys.stdout, best
        if (best != None):
            self.total += 1
            if (cl == best):
                self.right += 1
            self.tags[cl] = 1
            self.tags[best] = 1
            self.matrix[(cl,best)] = self.matrix.get((cl,best),0) + 1

    def score(self):
        """Returns accuracy so far"""
        return self.right / self.total

    def confusion(self):
        """Print confusion matrix"""
        print 'Confusion matrix'
        print 
        print '\tas'
        print 'is\t',string.join(self.tags.keys(),'\t')
        for tag1 in self.tags.keys():
            print tag1,'\t',
            for tag2 in self.tags.keys():
                print self.matrix.get((tag1,tag2),0),'\t',
            print

class Categorizer:

    def __init__(self):
        self.categories = {}
        self.features = {}
    
    def categorize(self,event):
        best_score = -1000000.0
        cat = ''
        for cand in self.categories.keys():
            score = self.features.get((cand),0.0)
            for f,v in event:
                score += self.features.get((f,v,cand),0.0)
            if (score > best_score):
                best_score = score
                cat = cand
        return cat

    def apply(self,source,sink,write):
        for feats,cl,line in source:
            if (cl == None):
                sink.update(line,cl,None,write)
            else:
                sink.update(line,cl,self.categorize(feats),write)

    def dump(self):
        for cand in self.categories.keys():
            print "CATEGORY|%s|%s" % ( cand , self.features.get((cand),0.0) )

        for feat in self.features:
            if (feat not in ('LOC','ORG','MISC','PER')):
                print "FEATURE|%s|%s|%s|%s" % ( feat[0],
                                                feat[1],
                                                feat[2] ,
                                                self.features[feat] )

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
            if cl == None:
                continue
            cat.categories[cl] = cat.categories.get(cl,0) + 1
            if use_baseline and baseline == None:
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
            if (baseline != cl):
                cat.features[(cl)] = feats
                feats += 1
                for (f,v) in counts:
                    if (counts[(f,v)] > cutoff):
                        cat.features[(f,v,cl)] = feats
                        feats += 1

        # generate events file

        self.make_events(source,cat,eps)
        
        # run estimate

        varopt = ''
        if (var != 0):
            varopt = '-l2 %d ' % (var)
        elif (lasso != 0):
            varopt = '-l1 %d ' % (lasso)
                            
        command = '/net/aps/32/bin/estimate -monitor -events_in %s -params_out %s %s' % (self.eventfile,self.paramsfile,varopt)

        if (quiet):
            os.system(command+' >/dev/null')
        else:
            print >> sys.stderr,'======='
            print >> sys.stderr,command
            os.system(command)
            print >> sys.stderr,'======='
        
        # load parameter values
                
        weights = [ float(w.rstrip())
                    for w in open(self.paramsfile).xreadlines()]

        cat.count = 0
        for i in xrange(0,len(weights)):
            if (weights[i] != 0):
                cat.count = cat.count + 1

        for f,v in cat.features.items():
            if (v < len(weights)):
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
            if cl == None:
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
        cl_list  = cat.categories.keys()

        for feats, entry in events.items():
            print >> outfile, len(cl_list)
            for c in cl_list:
                codes = []
                if (cat.features.has_key((c))):
                    codes.append(cat.features[(c)])
                for f,v in feats:
                    if (cat.features.has_key((f,v,c))):
                        codes.append(cat.features[(f,v,c)])
                print >> outfile, entry.get(c,0)+eps, len(codes),
                for f in codes:
                    print >> outfile, f, '1',
                print >> outfile

        outfile.close()
        
        return
 
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
            if cl == None:
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
            print >>sys.stderr, 'Zero probabilities found -- try smoothing counts (-s)!'                    
            
        # collect category priors
        
        n = sum(cat.categories.values())
        for cl in cat.categories:
            cat.features[(cl)] = log(cat.categories[cl]) - log(n)

        return cat

class NaiveBayesCategorizer(Categorizer):
    pass

def main():

    # process options

    parser = optparse.OptionParser()
    parser.add_option("-p", "--prior", type="float", default=0,
                      help="use Gaussion prior with variance VAR",
                      metavar="VAR")
    parser.add_option("-a", "--algorithm", choices=['MaxEnt','NaiveBayes'],
                      default='MaxEnt',
                      help="use learning algoritm ALG (MaxEnt (def), NaiveBayes)",
                      metavar="ALG")
    parser.add_option("-d", "--data-format", choices=['C45','Sparse','Rainbow'],
                      default='C45',
                      help="use data format FORM (C45 (def), Sparse, Rainbow)",
                      metavar="FORM")
    parser.add_option("-s", "--smooth", type="float", default=0,
                      help="add EPS to all counts",
                      metavar="EPS")
    parser.add_option("-m", "--model", action="store", default='',
                      help="use model FILE",
                      metavar="FILE")
    parser.add_option("-f", "--train", action="store", default='',
                      help="read training data from FILE",
                      metavar="FILE")
    parser.add_option("-t", "--test", action="store", default='',
                      help="read test data from FILE",
                      metavar="FILE")
    parser.add_option("-q", "--quiet", action="store_true", default=False,
                      help="be quiet")
    parser.add_option("-b", "--no-baseline", action="store_true", default=False,
                      help="don't use a baseline category")
    parser.add_option("-c", "--confusion", action="store_true", default=False,
                      help="print confusion matrix")
    parser.add_option("-y", "--display-parameters", action="store_true", default=False,
                      help="display all parameters")


    if (True):
        print >> sys.stderr
        print >> sys.stderr, 'Bayesian classifier'
        print >> sys.stderr

    (options, args) = parser.parse_args()

    # Construct model

    if (options.train == ''):
        # no training file, so check model file
        if (options.model != ''):
            if (not options.quiet):
                print >> sys.stderr, 'Reading model from',options.model
            file = open(options.model,'r')
            me = cPickle.load(file)
            file.close()
        else:
            print >> sys.stderr, 'No training file or model given!'
            raise Exception
    else:
        if (options.data_format == 'Rainbow'):
            if (not options.quiet):
                print >> sys.stderr, 'Reading Rainbow training data from', \
                      options.train
            source = RainbowSource(options.train,binary=True)
        elif (options.data_format == 'C45'):
            if (not options.quiet):
                print >> sys.stderr, 'Reading C4.5 training data from', \
                      options.train
            source = C4_5Source(options.train)
        elif (options.data_format == 'Sparse'):
            if (not options.quiet):
                print >> sys.stderr, 'Reading Sparse Binary training data from', \
                      options.train
            source = SparseSource(options.train)
        else:
            raise Exception

        if options.algorithm == 'MaxEnt':
            print >>sys.stderr, 'Building MaxEnt model'
            cat = MaxEntInducer().train(source, \
                                        quiet=options.quiet, \
                                        eps=options.smooth, \
                                        var=options.prior, \
                                        use_baseline=not options.no_baseline)
        elif options.algorithm == 'NaiveBayes':
            print >>sys.stderr, 'Building naive Bayes model'
            cat =  NaiveBayesInducer().train(source, \
                                             quiet=options.quiet, \
                                             eps=options.smooth)

    # write model (if desired)

    if (options.train != '') and (options.model != ''):
        if (not options.quiet):
            print >> sys.stderr, 'Writing model to',options.model
        file = open(options.model,'w')
        cPickle.dump(cat,file,True)
        file.close()

    # evaluate model

    if (options.test != ''):
        if (options.data_format == 'Rainbow'):
            if (not options.quiet):
                print >> sys.stderr, 'Reading Rainbow test data from', \
                      options.test
            source = RainbowSource(options.test,binary=True)
        elif (options.data_format == 'C45'):
            if (not options.quiet):
                print >> sys.stderr, 'Reading C4.5 test data from', \
                      options.test
            source = C4_5Source(options.test)
        elif (options.data_format == 'Sparse'):
            if (not options.quiet):
                print >> sys.stderr, 'Reading Sparse Binary test data from', \
                      options.test
            source = SparseSource(options.test)
        else:
            raise Exception

        acc = AccuracySink()
        cat.apply(source,acc,options.write)

        if (not options.write):
            print 'Accuracy =', acc.score() * 100.0

        if options.confusion:
            acc.confusion()

    if (options.display_parameters):
        cat.dump()

if __name__ == '__main__':
    main()
    
