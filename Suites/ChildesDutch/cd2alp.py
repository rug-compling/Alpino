#!/usr/bin/env python3.1

# Er zijn 115 regels in de data die beginnen met * en die niet in us-ascii zijn.
# De codering van deze regels is niet bekend, er worden verschillende coderingen
# door elkaar gebruikt. Daarom worden deze regels weggelaten.
#
# Uitvoer:
#   filename_regelnummer_naam_spreker|zin
# naam: lettercode, zie bronbestand voor echte naam
# spreker=C: target child
# spreker=A: andere spreker

import re

# aanpassing invoer voordat het naar de tokenizer gaat
# weghalen:
#    (=...)
#    [...]
#    <
#    >
#    (
#    )

RM = re.compile(r'\(=[^)]*\)|\[[^\]\[]*\]|[<>()]')
def cleanup(s):
    return RM.sub('', s)

import corpustools

import os, sys
from stat import S_ISDIR

os.chdir('/net/corpora/Childes_dutch')

dirlist = ['']
filelist = []

while dirlist:
    dirname = dirlist.pop()
    if dirname:
        dname = dirname
        prefix = dirname + '/'
    else:
        dname = '.'
        prefix = ''
    for filename in os.listdir(dname):
        fullname = prefix + filename
        if S_ISDIR(os.stat(fullname).st_mode):
            dirlist.append(fullname)
        elif fullname.endswith('.cha'):
            filelist.append(fullname)

filelist.sort()
for filename in filelist:
    blines = []
    with open(filename, 'rb') as fp:
        for bline in fp:
            bline = bline.rstrip()
            if not blines:
                blines.append([1, bline])
                continue
            if not bline or bline[:1] == b' ' or bline[:1] == b'\t':
                blines[-1][0] += 1
                blines[-1][1] += b' ' + bline.strip()
            else:
                blines.append([1, bline])

    idformat = filename[:-4].replace('/', '-') + '_{:05d}_{}_{}|{}\n'

    lineno = 1
    target = ''
    for count, bline in blines:
        reallineno = lineno
        lineno += count
        try:
            line = bline.decode('ascii')
        except:
            continue
        line = line.strip()

        if not line:
            continue

        if line.startswith('@Participants'):
            line = line.split(':', 1)[1]
            for item in line.split(','):
                a = item.split()
                if not a:
                    continue
                if a[-1].lower().endswith('child'):
                    target = a[0]
                    break

            # speciaal geval
            if not target:
                if line.find('MAT Matthijs Target_Child') >= 0:
                    target = 'MAT'

            if not target:
                sys.stderr.write(filename + ': No target child found:' + line + '\n')
                continue

        if not target and line[0] != '@':
            sys.stderr.write(filename + ': No target child found\n')
            target = 'XXXXXXXX'

        if line[0] == "*":
            a, b = line.split(':', 1)

            if a[1:] == target:
                s = 'C'
            else:
                s = 'A'

            b = cleanup(b)
            b = corpustools.tokenize(b)
            b = b.replace('\\', '\\\\').replace('[', '\\[').replace(']', '\\]').strip()
            if b:
                sys.stdout.write(idformat.format(reallineno, a[1:], s, b))
