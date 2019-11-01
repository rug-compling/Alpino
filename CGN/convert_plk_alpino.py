import sys
import re

def output_current(id,sent):
    if sent:
        print("{}|{}".format(id,sent))

def main():
    pattern = re.compile('<au id="(.*)" s=".*" tb=".*">')
    sent = ""
    id = 0
    
    for line in sys.stdin:
        match = pattern.match(line)
        if match:
            output_current(id,sent)
            sent = ""
            id = match.group(1)
        else:
            fields = line.split()
            word = fields[0]
            if sent:
                sent = sent + " " + word
            else:
                sent = word

    output_current(id,sent)


if __name__ == "__main__":
    main()
