import sys
import re

# copies two conllu files, but it removes sentid's if one of the files
# had a conversion error for that sentid

def add_keys(f):
    keys = set()
    reg_id = re.compile('# sent_id = (.*)$')
    reg_error = re.compile('# error')
    for line in f:
        line = line.rstrip()
        m = reg_id.match(line)
        if m:
            id = m.group(1)
            keys.add(id)
        m = reg_error.match(line)
        if m:
            keys.remove(id)
    return keys
    

def copy_conllu(inf,keys):
    reg_id = re.compile('# sent_id = (.*)$')
    reg_src = re.compile('# source = ')
    key_ok = True
    for line in inf:
        line = line.rstrip()
        m = reg_src.match(line)
        if m:
            print(line)           # always print source since it precedes sent_id
            continue
        m = reg_id.match(line)
        if m:
            id = m.group(1)
            if id in keys:
                key_ok = True
            else:
                print(line)
                print("# skipped")
                key_ok = False
        if key_ok:
            print(line)
            
            
def main():
    [_,f1,f2] = sys.argv
    with open(f1,'r') as in1:
        keys = add_keys(in1)
    with open(f2,'r') as in2:
        copy_conllu(in2,keys)
    

if __name__ == "__main__":
    main()

