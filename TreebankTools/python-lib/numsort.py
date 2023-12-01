# numsort.py
# sorting in numeric order
# for example:
#   ['aaa35', 'aaa6', 'aaa261']
# is sorted into:
#   ['aaa6', 'aaa35', 'aaa261']

def sorted_copy(alist):
    # inspired by Alex Martelli
    # http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/52234
    indices = list(map(_generate_index, alist))
    decorated = list(zip(indices, alist))
    decorated.sort()
    return [ item for index, item in decorated ]


def compare(str1, str2):
    a = _generate_index(str1)
    b = _generate_index(str2)
    return (a > b) - (a < b)


def _generate_index(s):
    """
    Splits a string into alpha and numeric elements, which
    is used as an index for sorting
    """
    #
    # the index is built progressively
    # using the _append function
    #
    index = []
    def _append(fragment):
        if fragment.isdigit():
            fragment = int(fragment)
        index.append(fragment)

    # initialize loop
    prev_isdigit = s[0].isdigit()
    current_fragment = ''
    # group a string into digit and non-digit parts
    for char in s:
        curr_isdigit = char.isdigit()
        if curr_isdigit == prev_isdigit:
            current_fragment += char
        else:
            _append(current_fragment)
            current_fragment = char
            prev_isdigit = curr_isdigit
    _append(current_fragment)
    return tuple(index)


def _test():
    initial_list = [ '35 Fifth Avenue', '5 Fifth Avenue', '261 Fifth Avenue' ]
    sorted_list = sorted_copy(initial_list)
    import pprint
    print("Before sorting...")
    pprint.pprint (initial_list)
    print("After sorting...")
    pprint.pprint (sorted_list)
    print("Normal python sorting produces...")
    initial_list.sort()
    pprint.pprint (initial_list)

if __name__ == '__main__':
    _test()
