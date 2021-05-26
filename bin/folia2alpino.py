#!/usr/bin/env python3

import io, re, sys
import xml.etree.ElementTree as ET

space = re.compile(r'[ \t\n\r\f]')
def escape(s):
    s = space.sub('_', s.strip())
    if not s:
        return '_'
    return s.replace('\\', '\\\\').replace('[', '\\[').replace(']', '\\]')

if sys.stdin.isatty() and len(sys.argv) < 2:
    sys.stderr.write("""
    Usage:

    find /net/corpora/Lassy/Sonar -name '*.folia.xml' -print | {} > results.txt

""".format(sys.argv[0]))
    sys.exit(1)

# make sure stdout is in utf-8
sys.stdout = io.TextIOWrapper(sys.stdout.detach(), encoding="utf-8")

def dofile(filename):
    filename = filename.strip()
    if not filename:
        return
    sys.stderr.write(filename + '\n')
    try:
        tree = ET.parse(filename)
        for sentence in tree.findall('.//{http://ilk.uvt.nl/folia}s'):
            sys.stdout.write(sentence.attrib['{http://www.w3.org/XML/1998/namespace}id'] + '|')
            sep = ''
            for word in sentence.findall('./{http://ilk.uvt.nl/folia}w'):
                text = word.findtext('./{http://ilk.uvt.nl/folia}t')
                pos = word.find('./{http://ilk.uvt.nl/folia}pos').attrib['class']
                lemma = word.find('./{http://ilk.uvt.nl/folia}lemma').attrib['class']
                sys.stdout.write('{}[ @folia {} {} {} ]'.format(sep, escape(lemma), escape(pos), escape(text)))
                sep = ' '
            sys.stdout.write('\n')
    except:
        sys.stderr.write(str(sys.exc_info()[1]) + '\n')

if len(sys.argv) > 1:
    for filename in sys.argv[1:]:
        dofile(filename)
else:
    for filename in sys.stdin:
        dofile(filename)
