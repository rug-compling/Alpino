#!/usr/bin/env python3

import sys

def main():
    id=""
    sentence=""
    for line in sys.stdin:
        fields = line.split()
        if not fields:
            print("{}|{}".format(id,sentence[:-1]))
        else:
            if fields[0] == '#':
                id=fields[1]
                sentence = ""
            else:
                sentence = sentence + fields[1] + " "

if __name__ == "__main__":
    main()
