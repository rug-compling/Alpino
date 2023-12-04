#!/usr/bin/env python3

'''
Script augmenting the tabular conll format with the alpino full pos tags (which are not in Alpino XML)

#Script replacing the column containing the original POS tags from the Alpino
#dependency trees with a a column containing POS tags predicted by the
#Memory-based tagger (Mbt) by POS tags from the Memory-based tagger (Mbt)

'''

__author__ = 'Erwin Marsi <e.c.marsi@uvt.nl> modified by Barbara <b.plank@rug.nl>'
__version__ = '$Id: retag.py,v 1.4 2006/01/12 19:28:34 erwin Exp $'

import string
import sys
import os.path
import optparse
import subprocess
import copy

import alpino as parse

debug = True

# global variable tags
tags = []

def retag(intabfn, outtabfn=None):
        '''
        replaces the column containing the original POS tags from the Alpino
        dependency trees with a a column containing POS tags predicted by the
        Alpino full pos tagger
        '''
        lines = []
        tokens = []

        for l in open(intabfn):
                fields = l.split('\t')
                lines.append(fields)
        print(fields)

        ##### surround words by [ @mwu WORD ] to give tokenization/bracketing to Alpino
        ##### prevents that Alpino interprets 'het ADJ' as a mwu
                if len(fields) > 2:
            word = fields[options.wordcol-1]
            if word.find('_'):
                word = word.replace('_',' ')
            token = "[ @mwu %s ]" % word
            tokens.append(token)

        if fields[0] == '\n':
            if len(tokens) == 1:
                t=0
                #don't parse just copy
                print("don't parse, just a single word: ", tokens)
                print(lines)
                new = []
                new.append(lines[t][3])
                print(new)
                newlines = lines[t][0:3+1] + new + new + lines[t][4:]
                print(newlines)
                lines[t] = newlines
                write_output_single(outtabfn, lines)
                break
            
            print("Parse string with Alpino")
            tmptags = parse.alpino(tokens, outtabfn+".parsed")
            print("tags:")
            print(tmptags)

            taglines = tmptags.split('\n')

            if len(taglines) == 1 and taglines[0] == '':
                print("NO triples returned")
                t=0
                for l in lines:
                    if len(l) > 1:
                        print(l)
                        new = []
                        new.append(lines[t][3])
                        print(new)
                        newlines = lines[t][0:3+1] + new + new + lines[t][4:]
                        print(newlines)
                        lines[t] = newlines
                        t+=1
                    
                write_output_single(outtabfn, lines)
                break
            
                

            #tags = []
            for i in taglines:
                tags.append(i.split('\t'))
                
            
            print(lines)
            print(tags)
            print(len(tags))

            if len(tags) == len(lines):
                #write to output
                write_output(outtabfn, tags, lines)
                print("done. exit. ", outtabfn, file=sys.stdout)

            else:
                print("LENGTH MISMATCH: split/join", file=sys.stdout)
                                #split or join
                t = 0
                for fields in lines:
                    # skip over empty lines
                    if len(fields) > 2 and len(tags[t]) > 2:

                        tag = tags[t]
                        tword = tags[t][1]
                        lword = lines[t][options.wordcol-1]
                        llabel = lines[t][3]  # = fields[3]
                                                
                        if debug:
                            print(tword, " ", lword, " llabe=",llabel)
                            print("t= ",t)

                        if lword == "t_/_m" and tword =="tot_en_met":
                            t += 1 #jump over it
                            print("jump over t_/_m")
                            continue
                            


                        ###
                        ###  special case: when tags contain _ AND label is mwu
                        ###  decider whether to join or split
                        ###
                        if llabel == 'mwu' and (len(tword.split('_')) > 1):
                            ## e.g Ruud_Witgen Ruud_Witgen_voor_Nijmegen
                            ##     dier_park Dierenpark_Wassenaar
                            linesNum = len(lword.split('_'))
                            tagsNum = len(tword.split('_'))
                            if linesNum < tagsNum:
                                t += splitFields(lword,tword,t,lines,outtabfn)
                            elif linesNum > tagsNum:
                                t += joinFields(lword,tword,t,lines,outtabfn)
                            else:
                                # Dierenpark_Wassenaar case (2979.tab)
                                t += joinFields(lword,tword,t,lines,outtabfn)
                        ###
                        ### join tags
                        ###
                        elif llabel == 'mwu':
                            t += joinFields(lword,tword,t,lines,outtabfn)
                            

                        ####
                        #### split tags
                        ####
                        elif (len(tword.split('_')) > 1):
                            #if options.split:
                            print("splitting")
                            t += splitFields(lword,tword,t,lines,outtabfn)
                            
                        ## else do nothing
                        else:
                            t += 1

                print("***** PROBLEM WITH FILE ", outtabfn)

                            


def joinFields(lword,tword,t,lines,outtabfn):
    '''
    joins fields together
    '''
    global tags

    print("TODO too long - deal with mwu")
    print("TODO join ", outtabfn)
    print("t=",t)
    print("lword=",lword, " tword=", tword)

    if lword != tword:

        j_max= getLengthMWU(lword,tags,t)

        print("length_mwu max_j=",j_max)
        j = getEffectiveLengthMWU(lword,tags,t,j_max)
        if j < j_max:
            print("**j < j_max: with j=", j)
        else:
            print("same j")


        # check if next word is the same, then don't join
        # to prevent case 1034.tab
        #print "##check ", tags[t+1][1], " ", lines[t+1][2]
        #if tags[t+1][1] == lines[t+1][2] and j >1:
        #    j = j - 1  #don't join with this

        print("** tags **")
        for x in tags:
            print(x)
        print("t=",t)
        fieldstojoin = tags[t:t+j]
        word = join(fieldstojoin, 1, '_')
        tagcol = join(fieldstojoin,2,'_')
        featcol = join(fieldstojoin,3, '|')
        newtag = []
        newtag.append(tags[t][0])
        newtag.append(word)
        newtag.append(tagcol)
        newtag.append(featcol)
        print("newtag:",newtag)
        # update tags
        tags = tags[0:t] + tags[t+j:]
        tags.insert(t, newtag)

        if len(tags) == len(lines):
            print("now they do match")
            write_output(outtabfn, tags, lines)
            print("done. exit. ", outtabfn, file=sys.stdout)
            sys.exit(0)
        else:
            print("TODOO continue over rest ")
            print_list(tags)
            print_list(lines)
    else:
        print("nothing")
    return 1
                            
def splitFields(lword,tword,t,lines,outtabfn):
    '''
    splits fields
    '''
    global tags
    
    print("TODOOO shorter ", os.path.basename(outtabfn))
    print(tword, lword)
    tag = tags[t]
                            
    if not equal(tword, lword) and nextWordsDiffer(tags,lines,t):

        print("they differ")
        positions = getSplitPositions(tword)
        print("###split positions: ", positions)

        positions = getEffectiveSplitPositions(tword,positions,lines,t)
        
        print("###effective: ", positions)

        if len(positions) ==0:
            print("no splitting found - try again")
            return joinFields(lword,tword,t,lines,outtabfn)
        
        newtag = split_pos(tag, positions)
        
        print("###newtag:", newtag)
        
        tags[t:t+1] = newtag
        
        
        #t += 1 # don't jump over split rest
        
        if debug:
            print("tags:** ", tags)

        if len(tags) == len(lines):
            print("now they do match")
            write_output(outtabfn, tags, lines)
            print("done. exit. ", outtabfn, file=sys.stdout)
            sys.exit(0)
        else:
            print("TODOO continue over rest ")
            print_list(tags)
            print_list(lines)

    else:
        print("no - don't differ")
        #t += 1

    return 1
                            

        
def join(list,index,symb):
    out = []
    for i in list:
        out.append(i[index])
    return string.join(out,symb)

def split_pos(tag, positions):
    word = tag[options.wordcol-1]
    newlist = split_word(word, positions)
    print("newlist: ",newlist)

    newtag = [] # empty list
    for part in newlist:
        new = copy.copy(tag) # copy rest
        new[options.wordcol-1] = part #override word
        newtag.append(new)

    return newtag
        

def nextWordsDiffer(tags,lines,t):
    print("check whether next Words differ")
    # if they don't differ, don't split
    
    # TODO: problem if second word in splitword equal to next word!
    # only split if next words are not equal
    # -- take stem from lines
    # (if we are not already at the end)
    if t < (len(tags)-2) and t < (len(lines)-2):
        next_lword = lines[t+1][options.wordcol]
        next_tword = tags[t+1][1]
        print(next_tword, " ==? ", next_lword)
        if next_tword == next_lword:
            ## check also that next line word is not a substring of word to split! (see 2122.tab)
            wordtosplit = tags[t][1]
            print("wordtosplit=",wordtosplit)
            if wordtosplit.find(next_lword)  == -1:
                return False
            else:
                print(next_lword, " is a substring of ", wordtosplit, "!!")
                return True
        else:
            return True
    else:
        return True

def equal(word1, word2):
    print("Check whether they're equal: ", word1, " ", word2)
    #ignore case
    word1 = string.lower(word1)
    word2 = string.lower(word2)

    # (-) is equal to -
    word1 = word1.replace('(-)','-')
    word2 = word2.replace('(-)','-')


        #Check whether they're equal:
        #Windmill_voorzitterGert_Harmsen   Windmill-voorzitter_Gert_Harmsen
    # don't differ by -
    # but both must contain either - or _
    if (word1.find('-') and word2.find('_')) or \
       (word2.find('-') and word1.find('_')):
        print("REPLACING in ", word1, " and ", word2)
        word1 = word1.replace('-','')
        word1 = word1.replace('_','')
        word2 = word2.replace('_','')
        word2 = word2.replace('-','')
    
    
    # words don't differ only by underscores
    w1 = word1.replace('_','')
    w2 = word2.replace('_','')
    print("w1,w2:",w1,w2)
    if w1 == w2:
         return True


    # literally different
    #check if they differ only by some ending
    print("check ending: word2=",word2, " word1=",word1)
    if isValidEnding(word2,word1) or isValidEnding(word1,word2):
        return True
        
    # same check but with _ removed
    if isValidEnding(w2,w1) or isValidEnding(w1,w2):
        return True
        
    return (word1 == word2)

def print_list(l):
    for i in l:
        print(i)


def isValidEnding(word,longword):
         if (longword.startswith(word)):
        rest = longword.replace(word,"",1)
        print("rest: ", rest, " len=", len(rest))
               if len(rest) <= 3 and not len(rest.split('_')) != 1 and isSuffix(rest):
            # only a few chars matter
            print("OK isValidEnding: word=",word,"longword=",longword)
             return True
    elif len(longword.split('_')) > 1:
        # check middle for endings
        print("check middle for endings")
        rest = word
        split = longword.split('_')
        print("rest=",rest, " split=", split, " word=",word)
        # adapted for WR-P-P-L-0000000003.p.46.s.2.tab, add find/else
        for part in split:
            if rest.find(part) != -1:
                rest = rest.replace(part,'XX')
            else:
                return False
        print(rest)
        tmp_listrest = rest.split('XX')
        # filter rest to get only nonempty ones
        listrest = [m for m in tmp_listrest if m != ""]
        print(listrest)
        if len(listrest) == 0:
            return False
        #if len(rest) == 0:
        #    return False
        else:
            returnValue = False
            for rest in listrest:
                if len(rest) <= 3 and not len(rest.split('_')) != 1 and isSuffix(rest):
                    returnValue = True
                else:
                    return False
            return returnValue


def differ(word1, word2):
    print("Check whether they differ: ", word1, " ", word2)
    #ignore case
    word1 = string.lower(word1)
    word2 = string.lower(word2)
    
    # words don't differ only by underscores
    w1 = word1.replace('_','')
    w2 = word2.replace('_','')
    print("w1,w2:",w1,w2)
    if w1 == w2:
         return False
    # literally different
    if word1 != word2:
        return True


def isSuffix(rest):
    print("Check suffix: ",rest)
    if rest == 'e':
        return True
    if rest == 'le':
        return True
    if rest == 'te':
        return True
    if rest == 'ste':
        return True
    if rest == 'en':
        return True
    if rest == 'pen':
        return True
    if rest == 's':
        return True
    if rest == 'd':
        return True
    if rest == 'je':
        return True
    if rest == 'ge': # prefix also ok
        return True
    else:
        print("Is not a suffix (rest): ",rest)
        return False

def write_output(outtabfn, tags, lines):
    outstream = open(outtabfn, 'w')

    print_list(tags)
    print("** printing:")
    print_list(lines)
    
    t = 0
    for fields in lines:
        if debug:
            print(fields)
        # skip empty lines
        if len(fields) > 2:
            fields = fields[:4]  + tags[t][2:4] + fields[4:]
            t += 1
            outstream.write(string.join(fields, '\t'))
    outstream.write("\n")

def write_output_single(outtabfn, lines):
    outstream = open(outtabfn, 'w')

    print("** printing:")
    print_list(lines)
    
    t = 0
    for fields in lines:
        if debug:
            print(fields)
        # skip empty lines
        if len(fields) > 2:
            t += 1
            outstream.write(string.join(fields, '\t'))
    outstream.write("\n")

            
def getLengthMWU(word,taglist,startindex):
    # underscore in first part
    print("getLengthMWU: word=",word," taglist=",taglist, " startindex=",startindex)
    part = taglist[startindex]
    newword = part[1]
    print("part=",part, " newword=",newword)
    #x = len(newword.split('_')) - 1
    #x = str.count(newword,'_')
    #j = len(word.split('_')) - x
    if word.startswith(newword):
        x = len(newword.split('_')) - 1
                #x = str.count(newword,'_')
        j = len(word.split('_')) - x
    else:
        j = len(word.split('_'))


    return j


def getEffectiveLengthMWU(word,taglist,startindex,j_max):
    newword = taglist[startindex][1]
    print("word =", word)
    print("newword=", newword)

    ## ignore DIM at end
    if word.endswith("_DIM"):
        j_max = j_max-1
    
    # try to build up word
    build = ""
    j=j_max
    for i in range(0,j_max):
        print(i)
        build += taglist[startindex+i][1]
        print("current build: ",build)
        if equal(build,word):
            j= i+1
            print("found effective max! build=",build," j=",j)
            break
    return j

def getSplitPositions(word):
    print("find split pos: ", word)
    positions = []
    start = 0
    max_j = len(word.split('_')) - 1
    for i in range(0,max_j):
        pos = str.find(word,'_',start)
        positions.append(pos)
        start = pos + 1
    return positions

def getEffectiveSplitPositions(word, positions, lines, startindex):
    newword = lines[startindex][1]
    print("word =", word)
    print("newword=", newword)

    ## if word contains _ remove those positions from list
    if str.count(newword,"_") > 0:
        try:
            for i in getSplitPositions(newword):
                positions.remove(i)
                print("removing: ",i)
                print("positions: ",positions)
        except ValueError:
            print("can't find split position - continue")
            return []

    j_max = len(positions)

    ## ignore DIM at end
    if word.endswith("_DIM"):
        j_max = j_max-1

    abs_max = len(word.split('_'))
    print("abs_max = ", abs_max)
    # try to build up word to find max len
    newpositions = []
    build = ""
    
    for i in range(0,j_max+1):
        print(i)
        if build == "":
            build += lines[startindex+i][1]
        else:
            build += "_"
            build += lines[startindex+i][1]
            
        print("current build: ",build)

        if (len(build.split('_'))+i) > abs_max:
            print("build bigger than absolute max (",abs_max,"!! stop")
            newpositions = positions[0:i]
            return newpositions

        if equal(build,word):
            #j= i+1
            #newpositions.append(len(build))
            newpositions = positions[0:i] # i inclusive
            #print "found effective max! build=",build," j=",j
            print("found effective max! build=",build," newpos=",newpositions)
            break


    return newpositions

def split_word(word,positions):
    l = []
    i = 0
    for j in positions:
        l.append(word[i:j])
        i = j+1
    l.append(word[i:])
    return l


# main stuff

usage = \
"""
        %prog [options] FILES


purpose:
        replaces original Alpino POS tags by Mbt POS tags

args:
        FILES           dependency trees in tabular format"""

parser = optparse.OptionParser(usage, version=__version__)

parser.add_option('-f', '--file',
                                        dest='file',
                                        action='store_true',
                                        default=False,
                                        help='write output to file, possibly overwriting original file')

parser.add_option('-s', '--split',
                                        dest='split',
                                        action='store_true',
                                        default=False,
                                        help='split further')


parser.add_option('-w', '--word-column',
                                        dest='wordcol',
                                        default=2,
                                        type='int',
                                        metavar='NUMBER',
                                        help='number of column containing words')

(options, args) = parser.parse_args()

if not args:
        sys.stderr.write('Error: incorrect number of arguments\n')

elif os.environ["ALPINO_HOME"] == "":
    sys.stderr.write('Error: no ALPINO_HOME set!\n')
else:
        for intabfn in args:
                if options.file:
                        outtabfn = os.path.basename(intabfn) + "2"
                        print('retagging %s to %s' % (intabfn, outtabfn), file=sys.stderr)    
                        retag(intabfn, outtabfn)
                else:
                        print('retagging', intabfn, file=sys.stderr)    
                        retag(intabfn)



