#!/usr/bin/env python3
#
# Read 'sfr' input and produce a sequence of frames, one sentence per
# line.
#
#

import io, sys

if __name__ == '__main__':
    curId = ''
    frames = []

    # sys.stdin.reconfigure(encoding='utf-8')
    # sys.stdout.reconfigure(encoding='utf-8')
    sys.stdin = io.TextIOWrapper(sys.stdin.detach(), encoding='utf-8', newline=None)
    sys.stdout = io.TextIOWrapper(sys.stdout.detach(), encoding='utf-8', newline=None, line_buffering=True)

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
