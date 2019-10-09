#!/usr/bin/python
#
# Read 'sfr' input and produce a sequence of frames, one sentence per
# line.
#
#

import sys

if __name__ == '__main__':
    curId = ''
    frames = list()

    for line in sys.stdin:
        lineParts = line.split('|')

        if len(lineParts) != 7:
            continue # Ignore

        lineId = lineParts[2]
        if curId != lineId:
            if curId != '':
                if(frames):
                    print ' '.join(frames)
                frames = list()

            curId = lineId

        word = lineParts[0]
        his = lineParts[5]
        if "decap" in his:
            word=word.lower()

        frames.append(word)

    # Flush
    if(frames):
        print ' '.join(frames)
