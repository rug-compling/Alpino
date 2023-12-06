#!/usr/bin/env python3
#
# Read 'sfr' input and produce a sequence of frames, one sentence per
# line.
#
#

import sys

if __name__ == '__main__':
    curId = ''
    frames = []

    for line in sys.stdin:
        lineParts = line.split('|')

        if len(lineParts) < 8:
            continue # Ignore

        lineId = lineParts[2]
        if curId != lineId:
            if curId != '':
                if frames:
                    print(' '.join(frames))
                frames = []

            curId = lineId

        word = lineParts[0]
        his = lineParts[5]
        if "decap" in his:
            word=word.lower()

        frames.append(word)

    # Flush
    if frames:
        print(' '.join(frames))
