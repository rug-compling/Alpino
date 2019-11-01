import sys
import re

def output_current(id,sent):
    if sent:
        print("{}|{}".format(id,sent))

def main(argv):
    mid=argv[1]
    pattern = re.compile('<au id="(.*)" s=".*" tb=".*">')
    sent = ""
    id = mid + "__" + "0"
    
    for line in sys.stdin:
        match = pattern.match(line)
        if match:
            output_current(id,sent)
            sent = ""
            id = mid + "__" + match.group(1)
        else:
            fields = line.split()
            word = fields[0]
            if sent:
                sent = sent + " " + word
            else:
                sent = word

    output_current(id,sent)


if __name__ == "__main__":
    main(sys.argv)
