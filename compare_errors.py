#!/usr/bin/python3

import sys

def usage(prog):
    print("Usage: {} logfile errorsfile".format(prog), file=sys.stderr)
    exit()


def main():
    if len(sys.argv) != 3:
        usage(sys.argv[0])
    scores = {}
    logf = sys.argv[1]
    errf = sys.argv[2]
    with open(logf) as file:
        for line in file:
            line = line.rstrip()
            [score,key] = line.split()
            scores[key] = score
    with open(errf) as file:
        for line in file:
            line = line.rstrip()
            words = line.split()
            key = words[0]
            if key in scores:
                if (scores[key] == "100.00"):
                    print("GONE {}".format(line))
                else:
                    print("{}".format(line))
                del scores[key]
            else:
                print("{}".format(line))
        for key in scores:
            if (scores[key] != "100.00"):
                print("{} NEW".format(key))

if __name__ == "__main__":
    main()
