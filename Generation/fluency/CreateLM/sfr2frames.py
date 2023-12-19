#!/usr/bin/env python3
#
# Read 'sfr' input and produce a sequence of frames, one sentence per
# line.
#
#
# adapted by GvN: now undoes MWU, because that is how the model is applied
# in the generator
import sys

def is_mwu(tag):
    result=False
    final=False
    base=''
    parts = tag.split('-')
    if len(parts) == 2:
        prefix=parts[0]
        nos = prefix.split('/')
        if len(nos) == 2:
            result=True
            if nos[0] == nos[1]:
                final=True
                base=parts[1]
    return (result,final,base)

if __name__ == '__main__':
    curId = ''
    frames = []

    sys.stdin.reconfigure(encoding='utf-8')
    sys.stdout.reconfigure(encoding='utf-8')

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

        tag = lineParts[1]

        (result,final,base) = is_mwu(tag)

        if not result:
            frames.append(tag)
        if result and final:
            frames.append(base)

    # Flush
    if frames:
        print(' '.join(frames))
