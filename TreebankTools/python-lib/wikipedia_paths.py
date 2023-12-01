#!/usr/bin/env python3

# wikipedia_paths.py -- filename manipulaties voor het wikipedia corpus

## Documentatie

# We willen een methode om corpora als het Wikipedia corpus op te
# splitsen in blokjes van voldoende grootte.
#
# Een logische opdeling van geschikte grootte zoals de opdeling per
# dag bij de nieuwsbladen is hier niet voorhanden.
#
# Wikipedia wordt opgedeeld in lemmata, en per lemma zijn er een
# aantal zinnen.
#
# Bestandsnamen zijn van de vorm <lemma>-<paragraafnummer>-<zinnummer>

# We creeren directories van de vorm (uitgaande van chunksize 250):
#
#   0000
#
# Zin 19, paragraaf 3, van lemma 23423 (wikipedia_path(23423,3,19)) komt
# dan te staan in:
#
#   0093/23423-3-19.xml
#
# Op basis van de filenaam kan dan eenvoudig de bijbehorende directory
# bepaald worden:
#
#   wikipedia/23423-3-19.xml
#
# is voldoende om het volledige pad te kunnen achterhalen.
#
# Door de directory-namen (filenames van de compacte corpora) uit
# te vullen kunnen we vanuit een shell de corpus files gemakkelijk
# in de juiste volgorde bijlangs.
#
# Voor de sortering binnenin de compacte corpora is uitvullen niet
# nodig.

### Code:

chunksize = 250                       # het aantal lemmata per directory

import re

# FIXME: de notie van file-id zonder .xml extensie maakt het geheel
#        rommelig
#        het lijkt logischer de .xml extensie hier mee te nemen.
#        bovendien zijn er veel teveel functies die bijna hetzelfde doen.

def wikipedia_directory(lemmanr):
    """retourneer een directory-naam op basis van LEMMANR"""
    global chunksize

    startblok = lemmanr // chunksize
    # eindblok  = startblok + 1

    return "%04d" % (startblok)


def wikipedia_file(lemmanr, paragraafnr, zinnr):
    """retourneer de filename van de zin met lemmanr, paragraafnr en zinnr"""
    return "%1d-%1d-%1d.xml" % (lemmanr, paragraafnr, zinnr)


def wikipedia_path(lemmanr, paranr, zinnr):
    """retourneer het pad naar de zin met lemmanr, paranr en zinnr"""
    return wikipedia_directory(lemmanr) + "/" + wikipedia_file(lemmanr, paranr, zinnr)


def fileid2path(fileid):
    """expandeer <id>-<paragraafnr>-<zinnr> tot het gehele pad"""
    (lemmanr, paranr, zinnr) = re.split(r'[-_]', fileid)
    return wikipedia_path(int(lemmanr), int(paranr), int(zinnr))


def getdirectory(basename):
    """retourneer de directory gebaseerd op de basename"""
    lemma = int(re.split(r'[-_]', basename)[0])
    return wikipedia_directory(lemma)


if __name__ == '__main__':

    usage = "Usage: wikipedia_path.py --test | fileid [fileids]\n"

    import sys

    if len(sys.argv) < 2:
        sys.stderr.write(usage)
        sys.exit(1)

    if sys.argv[1] == '--test':

        # Een aantal randgevallen testen:
        print(wikipedia_path(0           ,5, 1))
        print(wikipedia_path(chunksize-1 ,5, 1))
        print(wikipedia_path(chunksize   ,5, 1))
        print(wikipedia_path(chunksize+1 ,5, 1))

        # En een willekeurige andere file:
        print(wikipedia_path(23423, 3, 19))

    else:
        for i in sys.argv[1:]:
            print(fileid2path(i))
