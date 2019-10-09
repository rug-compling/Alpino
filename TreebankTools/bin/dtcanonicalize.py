#!/usr/bin/env python

# canonicalize.py -- maak een dependency tree canoniek
# GJK 2006-04-22

# Een dt canoniek maken:

# Eerst het bereik doorrekenen, dan sorteren, dan indicering opnieuw
# regelen: opnieuw nummeren en de eerste keer dat we de een index
# tegenkomen de gehele node plaatsen.  Iedere knoop krijgt een id
# attribuut dat bepaald wordt door de knopen in preorder te nummeren.
# De root node krijgt id=0.

# We moeten van alle knopen het bereik bepalen.  Daarvoor moeten we in
# principe de boom in postorder doorlopen, ware het niet dat we
# geindiceerde knopen tegen kunnen komen die zelf ook weer indices
# kunnen bevatten.

# We gebruiken een globale (hash-)tabel om geindiceerde knopen bij te
# houden.  Als key gebruiken we index zoals deze in het originele xml
# bestand is gebruikt.  Deze kan later hernummerd worden.

# - We beginnen met het uit de xml opbouwen van een boomstructuur in
#   python.
# - Iedere keer dat we een geindiceerde knoop te pakken hebben voegen
#   we (een pointer naar) het object toe aan de (globale)
#   indexed-nodes tabel.
# - We gaan het bereik van de geindiceerde knopen bepalen.  We moeten
#   daarbij zorgen dat we geen problemen krijgen met circulaire
#   afhankelijkheden.
#
#   We moeten daarvoor bijhouden:
#     - de knopen waaraan gewerkt wordt
#     - de knopen die al doorgerekend zijn
#
# Merk op dat een knoop best van een index voorzien kan zijn zonder
# dat er een kale index bijhoort.  In dit geval kan de index gewoon
# verwijderd worden.  Andersom, een kale index waarvan de verwijzing
# ontbreekt, betekent een corrupte boomstructuur.
# We geven echter in beide gevallen een fatal error.

# Sortering:
#
# eerst eindpositie, dan gedomineerde woordposities, dan alfabetisch
# op rel-waarde.

# Er kan door gebruik van co-indicering sprake zijn van knopen met
# hetzelfde bereik.  Bovenstaande sortering lijkt in principe
# voldoende.  Er wordt een foutmelding gegeven wanneer er alsnog een
# conflict optreedt.  Dan kan er gericht naar oplossingen gezocht
# worden: we moeten ervoor zorgen dat de sortering deterministisch
# blijft.

# De primaire sortering is op eindpositie.  Het volgende kan ermee te
# maken hebben dat bij sortering op eindpositie er minder bomen een
# "springerige verschijning" lijken te hebben:
#
# dtsearch -q '//node[@begin = preceding-sibling::node/@begin]'\
#         ~/compact-corpora/cdb  | wc -l
# ==> 3721
# dtsearch -q '//node[@end = preceding-sibling::node/@end]' \
#         ~/compact-corpora/cdb  | wc -l
# ==> 220


##


from xml.dom import minidom
from xml.parsers.expat import ExpatError
import sys, os
import string
import tempfile
import cgi

from optparse import OptionParser

class TreeNode:
    """Represents a single node
    - The 'real' attributes are stored in the attributes dictionary,
      and are available directly using node[attr_name] notation.
    - the class fields 'begin, end, etc.' are only used while
      sorting etc.
    """

    NORMAL    = 1
    INDEXED   = 2
    INDEX     = 3

    def __init__(self):
        self.children = []
        self.begin = None
        self.end = None
        self.index = None               # wordt gezet na het herordenen
        self.attributes = {}

    def add_child(self, child):
        self.children.append(child)

    def __getitem__(self, key):
        return self.attributes[key]

    def __setitem__(self, key, value):
        self.attributes[key] = value

    def __delitem__(self, key):
        del self.attributes[key]

    def has_key(self, key):
        return self.attributes.has_key(key)

    def keys(self):
        return self.attributes.keys()

    def has_children(self):
        return len(self.children) > 0

    def has_range(self):
        """returns True if the begin and end attributes are set"""
        return self.begin != None  # and self.end != None

    def gettype(self):
        if len(self.keys()) < 1:
            raise 'node without attributes!'

        type = self.NORMAL
        if self.has_key('index'):
            if self.has_key('cat') or self.has_key('pos'):
                type = self.INDEXED
            else:
                type = self.INDEX
        return type


    def debug_print_node(self, outfile=sys.stdout, indent=''):
        outfile.write(indent)
        outfile.write('node[ ')
        for key in self.attributes.keys():
            outfile.write('%s="%s" ' % (key, self.attributes[key]))
        outfile.write(']')
        if self.begin != None:
            outfile.write("begin=%d, end %d" % (self.begin, self.end))
        outfile.write('\n')
        for node in self.children:
            node.debug_print_node(outfile, indent + '  ')


    def write_xml(self, outfile=sys.stdout, indent='', version='1.5'):
        """Write the structure as XML, assume the encoder on outfile
        has been set"""
        outfile.write(indent)
        outfile.write('<node')
        attrs = self.attributes.keys()
        attrs.sort()
        for attr in attrs:
            outfile.write(' %s="' % attr)
            write_xml_data(outfile, self.attributes[attr])
            outfile.write('"')

        if self.has_children():
            outfile.write('>\n')
            for child in self.children:
                child.write_xml(outfile, indent + '  ')
            outfile.write(indent)
            outfile.write('</node>\n')
        else:
            outfile.write("/>\n")


class DTFile:
    """Represent an Alpino dependency structure file"""
    def __init__(self):
        self.rootnode = None
        self.sentence = ""
        self.sentid = ""
        self.metalist = []
        self.commentlist = []

    def set_sentence(self, s):
        self.sentence = s

    def set_sentid(self, s):
        self.sentid = s

    def add_meta(self, m):
        self.metalist.append(m)

    def add_comment(self,c):
        self.commentlist.append(c)

    def set_root_node(self, node):
        self.rootnode = node


    # quick hack: interface gelijk maken voor "container" operaties
    # in DTParser
    def add_child(self, node):
        if self.rootnode:
            raise "already got a root node!"
        self.rootnode = node

    # N.B. merk op dat wanneer we XML schrijven, wil er voor diff geen
    # verschil zijn tussen de output van Alpino, we dezelfde
    # xml-escape regels moeten hanteren
    def dump_content(self):
        self.rootnode.debug_print_node()
        print "sentence:", self.sentence
        for comment in self.commentlist:
            print "comment:", comment


    def write_xml(self, outfile, encoding='UTF-8', version='1.5'):
        """write the dependency structure to xml in encoding"""

        # FIXME?: moeten we ook wat doen met \u escapes?
        import codecs
        outfile = codecs.getwriter(encoding)(outfile)

        # de xml-header
        outfile.write('<?xml version="1.0" encoding="%s"?>\n'
                      % encoding)

        # de root node
        if version == "":
            outfile.write('<alpino_ds>\n')
        else:
            outfile.write('<alpino_ds version="%s">\n' % version)

        # metadata
        if len(self.metalist):
            outfile.write('  <metadata>\n')

            for meta in self.metalist:
                outfile.write('    <meta type="%s" name="%s" value="%s"/>\n' % (meta['type'], meta['name'], meta['value']))

            outfile.write('  </metadata>\n')

        # de nodes
        self.rootnode.write_xml(outfile, indent='  ')

        # de zin
        if self.sentid == "":
            outfile.write('  <sentence>')
        else:
            outfile.write('  <sentence sentid="%s">' % self.sentid)
        write_xml_data(outfile, self.sentence)
        outfile.write('</sentence>\n')

        # evt commentaar
        if len(self.commentlist):
            outfile.write('  <comments>\n')

            for comment in self.commentlist:
                outfile.write('    <comment>')
                write_xml_data(outfile,comment)
                outfile.write('</comment>\n')

            outfile.write('  </comments>\n')


        # en de sluittag niet vergeten...
        outfile.write('</alpino_ds>\n')


class DTParser:
    """Parse een Alpino xml file"""

    def __init__(self):

        self.version = ""
        # self.parsefile(xmlfile)
        pass

    def parsefile(self, xmlfile):
        xmldoc = minidom.parse(xmlfile)

        dtfile = DTFile()

        self.parse(xmldoc, dtfile)

        return dtfile, self.version

    # we gebruiken weer 'method dispatching' voor het parsen
    def parse(self, node, container):
        """A parsed XML document (from minidom.parse) is a tree of nodes
        of various types.  Each node is represented by an instance of the
        corresponding Python class (Element for a tag, Text for
        text data, Document for the top-level document).  The following
        statement constructs the name of a class method based on the type
        of node we're parsing ("parse_Element" for an Element node,
        "parse_Text" for a Text node, etc.) and then calls the method.
        """
        parseMethod = getattr(self, "parse_%s" % node.__class__.__name__)
        parseMethod(node, container)

    def parse_Document(self, node, container):
        """parse the document node

        The document node by itself isn't interesting (to us), but
        its only child, node.documentElement, is: it's the root node
        of the grammar.
        """
        self.parse(node.documentElement, container)
        try:
            self.version = node.firstChild.attributes['version'].value
        except:
            pass

    def parse_Text(self, node, container):
        """parse a text node"""
        pass

    def parse_Element(self, node, container):
        """parse an element

        An XML element corresponds to an actual tag in the source:
        Each element type is handled in its own method.  Like we did in
        parse(), we construct a method name based on the name of the
        element ("do_sentence" for a <sentence> tag, etc.) and
        call the method.
        """
        handlerMethod = getattr(self, "do_%s" % node.tagName)
        handlerMethod(node, container)

    def parse_Comment(self, node, container):
        """parse a comment

        The xml files can contain XML comments, but we ignore them
        """
        pass

    def do_alpino_ds(self, xmlnode, container):
        """Deze moet z'n kinderen activeren"""
        for elem in xmlnode.childNodes:
            if elem.nodeType == elem.TEXT_NODE:
                continue
            self.parse(elem, container)

    def do_node(self, xmlnode, container):
        treenode = TreeNode()

        # hier gaan de we de treenode vullen
        for attr in xmlnode.attributes.keys():
            treenode[attr] = xmlnode.attributes[attr].value

        container.add_child(treenode)

        for elem in xmlnode.childNodes:
            if elem.nodeType == elem.TEXT_NODE:
                continue
            self.parse(elem, treenode)

    def do_sentence(self, xmlnode, container):
        text = self.get_text(xmlnode)
        text.strip()
        container.set_sentence(text)
        try:
            sentid = xmlnode.attributes['sentid'].value
        except:
            pass
        else:
            container.set_sentid(cgi.escape(sentid, True))

    def do_metadata(self, xmlnode, container):
        for elem in xmlnode.childNodes:
            if elem.nodeType == elem.TEXT_NODE:
                continue
            self.parse(elem, container)

    def do_meta(self, xmlnode, container):
        a = {}
        for attr in xmlnode.attributes.keys():
            a[attr] = cgi.escape(xmlnode.attributes[attr].value, True)
        container.add_meta(a)

    def do_comments(self, xmlnode, container):
        for elem in xmlnode.childNodes:
            if elem.nodeType == elem.TEXT_NODE:
                continue
            self.parse(elem, container)

    def do_comment(self, xmlnode, container):
        text = self.get_text(xmlnode)
        text.strip()
        # moet deze selectie al op dit niveau?
        if text:
            container.add_comment(text)

    def get_text(self, xmlnode):
        """Get the text nodes from an xml element node"""
        the_text = ""
        for elem in xmlnode.childNodes:
            if elem.nodeType != elem.TEXT_NODE:
                raise 'unexpected element content!!!'
            the_text += elem.nodeValue
        return the_text


# FIXME: - re-init bij canonicalize()
#        - geen argumenten meer bij de constructor,
#        -
class Canonicalizer:

    class Error(Exception):
        '''Fatal error in Canonicalizer'''
        def __init__(self, file, msg):
            if file is sys.stdin:
                self.filename = 'standard input'
            else:
                self.filename = file
            self.msg = msg

        def __str__(self):
            '''Formatted error message'''
            return '%s: %s' % (self.filename, self.msg)

    def __init__(self, dtparser, filename):
        self.dtfile, self.version = dtparser.parsefile(filename)
        self.filename = filename

        # fixme? self.dtfile.rootnode gebruiken?
        self.rootnode = self.dtfile.rootnode

        self.indexed_nodes = {}
        self.index_nodes = {}

        self.next_index = 1

    def canonicalize(self):
        # eerst de geindiceerde knopen verzamelen

        self.fill_index_tables()
        self.check_indices()
        self.calculate_ranges()
        self.reorder_tree(self.rootnode)

    def fill_index_tables(self, node=None):
        """We verzamelen de knopen met een index.

        - ze zijn dan direct toegankelijk
        - we kunnen dan meteen index-gerelateerde sanity checks doen
        """

        if not node:
            node = self.rootnode

        if node.gettype() == node.INDEXED:
            index = node['index']
            if self.indexed_nodes.has_key(index):
                raise self.Error(self.filename, 'index "%s" is used more than once' % index.encode('utf-8'))

            node.original_index = index
            self.indexed_nodes[index] = node

        elif node.gettype() == node.INDEX:
            index = node['index']

            if not self.index_nodes.has_key(index):
                self.index_nodes[index] = [node]
            else:
                self.index_nodes[index].append(node)

        for child in node.children:
            self.fill_index_tables(child)


    def check_indices(self):

        # we hoeven niet te kijken of de indices wel allemaal numeriek
        # zijn, we hernummeren later toch

        # is er voor elke indexed node een index node?
        for index in self.indexed_nodes.keys():
            if not self.index_nodes.has_key(index):
                # # okee dan, we kunnen de index weghalen:
                #
                # # uit de knoop zelf...
                # # wow...  don't you just love all the syntactic sugar? :-)
                # del self.indexed_nodes[index]['index']
                #
                # # uit de indexed_nodes tabel...
                # del self.indexed_nodes[index]

                # de anotatoren verkiezen hier een fatal error
                raise self.Error(self.filename, 'no target node for index "%s"' % index.encode('utf-8'))

        # en is er voor iedere index node wel een indexed node?
        for index in self.index_nodes.keys():
            if not self.indexed_nodes.has_key(index):
                # een kale verwijzing
                raise self.Error(self.filename, 'no source node for index "%s"' % index.encode('utf-8'))


    def calculate_ranges(self):
        indices_in_progress = []
        self.calculate_range(self.rootnode, indices_in_progress)


    def calculate_range(self, node, indices_in_progress):
        if node.has_range():
            return (node.begin, node.end, node.dominated_words)

        if not node.has_children():

            if node.gettype() == node.INDEX:
                index = node['index']

                # circularity?
                if index in indices_in_progress:
                    # indices are strings here
                    raise self.Error(self.filename,
                                     "Circularity detected: found %s while processing indices %s"
                                      % (index, ' -> '.join(indices_in_progress)))

                # get begin, end, wordlist from the corresponding indexed node
                begin, end, wordlist = self.calculate_range(self.indexed_nodes[index],
                                                            indices_in_progress)
                # and set the attributes
                node['begin'] = str(begin)
                node['end']   = str(end)

            else:
                # at this point we know it's not an index node;
                # the node can be an indexed node, but that's ok

                # get the begin and end values
                begin, end = self.get_string_positions(node)
                wordlist = [begin]

            node.begin = begin
            node.end = end
            node.dominated_words = wordlist
            return begin, end, wordlist


        # OK, we've got children to work with!

        childranges = []

        # add the current index to the list of active indices
        if node.gettype() == node.INDEXED:
            indices_in_progress.append(node['index'])

        # process the children
        for child in node.children:
            childranges.append(self.calculate_range(child, indices_in_progress))

        # remove the current index from the list of active indices
        if node.gettype() == node.INDEXED:
            indices_in_progress.remove(node['index'])

        # determine the range of this node
        begin = min(map(lambda(x): x[0], childranges))
        end   = max(map(lambda(x): x[1], childranges))

        # build a list of all the words dominated by this node
        # (by number)
        wordlist = []
        for x in childranges:
            wordlist.extend(x[2])

        wordlist.sort()
        wordlist = remove_doubles(wordlist)

        node['begin'] = str(begin)
        node['end' ] = str(end)

        node.begin = begin
        node.end = end
        node.dominated_words = wordlist
        return begin, end, wordlist     # fixme? returning wordlist should be enough...


    def get_string_positions(self, node):
        """Zijn de stringposities wel numeriek?"""

        # make sure we have the necessary attributes
        self.check_leaf_node(node)

        try:
            begin = int(node['begin'])
        except ValueError:
            raise self.Error(self.filename, 'non-numeric begin position "%s"' % node['begin'].encode('utf-8'))

        try:
            end = int(node['end'])
        except ValueError:
            raise self.Error(self.filename, 'non-numeric end position "%s"' % node['end'].encode('utf-8'))

        return begin, end


    def check_leaf_node(self, node):
        """Check for missing attributes"""
        for attr in 'pos', 'begin', 'end', 'root', 'word':
            if not node.has_key(attr):
                raise self.Error(self.filename, "leaf node without @%s : %s!" % (attr, node.attributes))


    def reorder_tree(self, node, id=0):
        """Reorder the tree: resort the nodes, manage the indices"""

        # stel ik kom een geindiceerde knoop tegen:
        if node.index is None and node.gettype() == node.INDEXED:
            # het is een indexed node en we hebben de index nog niet
            # eerder gezien

            # markeer de node
            node.index = self.next_index

            # zet de nieuwe index maar bewaar de oude
            old_index = node['index']
            new_index = str(node.index)
            node['index'] = new_index

            # voorzie de bijbehorende index nodes van de nieuwe index
            for indexnode in self.index_nodes[old_index]:
                 # set a marker
                 indexnode.index = self.next_index

                 # update the real attribute
                 indexnode['index'] = new_index

            self.next_index += 1

        if node.index is None and node.gettype() == node.INDEX:

            # het is een index node en we hebben de index nog niet
            # eerder gezien

            old_index = node['index']
            indexed_node = self.indexed_nodes[old_index]

            # first set all the new index values
            new_index = str(self.next_index)

            indexed_node.index = self.next_index
            indexed_node['index'] = new_index

            for indexnode in self.index_nodes[old_index]:
                 # set a marker
                 indexnode.index = self.next_index
                 # update the real attribute
                 indexnode['index'] = new_index

            self.next_index += 1

            # we wisselen de attributen en de kinderen om,
            # alleen de rel blijft hetzelfde.
            # het id (als het er al is) wordt later weer overschreven
            # en kan gewoon gekopieerd worden

            this_rel = node['rel']
            other_rel = indexed_node['rel']

            this_attributes = node.attributes

            node.attributes = indexed_node.attributes
            node.children = indexed_node.children

            indexed_node.attributes = this_attributes
            indexed_node.children = []

            node['rel'] = this_rel
            indexed_node['rel'] = other_rel

            # NOTE: we could update the index_nodes/indexed_nodes
            # hashes, but we don't.  The keys used are still the old
            # index values, and some node types may have been swapped.
            # Do not try to access a node through any of these hashes
            # after the index value has been updated.
            # ==> don't use the hashes after this function has been used


        # add a new id
        node['id'] = str(id)
        id += 1

        # sort the children
        node.children.sort(self.node_cmp)

        for child in node.children:
            id = self.reorder_tree(child, id)
        # return the next id to use
        return id


    # moet deze hier gedefinieerd of onder TreeNode?
    def node_cmp(self, nodeA, nodeB):
        """- try sorting (comparing) on end position,
        - fallback on wordlist comparison
        - ??? then fallback on rel values???
        """
        sortval = cmp(nodeA.end, nodeB.end)
        if sortval == 0:
            sortval = cmp(nodeA.dominated_words, nodeB.dominated_words)
            if sortval == 0:
                sortval = cmp(nodeA['rel'], nodeB['rel'])
                if sortval == 0:
                    raise 'sorting conflict while sorting "%s"!' \
                          "\n%s \n%s\n" \
                          % (self.filename, nodeA.attributes, nodeB.attributes)
        return sortval

    def write_xml(self, outfile):
        self.dtfile.write_xml(outfile, version=self.version)


def remove_doubles(lst):
    """given a sorted list returns a new sorted list with duplicates removed"""

    if len(lst) == 1:
        return [lst[0]]

    newlist = [lst[0]]
    for i in range(1,len(lst)):
        if newlist[-1] != lst[i]:
            newlist.append(lst[i])
    return newlist


# we write the xml handling ourselves to have full control so we can
# ensure the output is identical to Alpino's output.
def write_xml_data(outfile, data):
    "Write data to outfile."
    data = data.replace("&", "&amp;").replace("<", "&lt;")
    data = data.replace("\"", "&quot;").replace(">", "&gt;")
    data = data.replace("'", "&apos;")  # eigenlijk overbodig
    outfile.write(data)


if __name__ == '__main__':
    progname = os.path.basename(sys.argv[0])

    # command line opties parsen
    parser = OptionParser("usage: %prog [options] <alpino xml-file(s)>", prog=progname)

    parser.add_option("--verbose", "-v", action="store_true", dest="verbose",
                      help="tell which file is being worked on")

    parser.add_option("--stdin", "-s", action="store_true", dest="stdin",
                      help="process a single file as a filter")

    (options, args) = parser.parse_args()

    if not args and not options.stdin:
        parser.print_help(sys.stderr)
        sys.exit(1)


    dtparser = DTParser()

    # de bestanden canoniek maken
    error_occurred = False
    if options.stdin:
        try:
            cn = Canonicalizer(dtparser, sys.stdin)
            cn.canonicalize()
            cn.write_xml(sys.stdout)
        except Canonicalizer.Error, err:
            print >> sys.stderr, '%s: Error: %s' % (progname, err)
            error_occurred = True
    else:
        for file in args:
            try:
                if options.verbose:
                    print >> sys.stderr, "making file", file, 'canonical'
                cn = Canonicalizer(dtparser, file)
                cn.canonicalize()

                # we moet een tempfile gebruiken zodat er nooit data verloren
                # gaat:

                #  - we maken in dezelfde directory een tempfile met mkstemp()
                (tempfd, tempname) = tempfile.mkstemp('',
                                                  os.path.basename(file) + ".",
                                                  os.path.dirname(file))

                #  - we schrijven de nieuwe file naar die tempfile
                tempfileobj = os.fdopen(tempfd, "w")
                cn.write_xml(tempfileobj)
                tempfileobj.close()

                # wat zijn de permissies van het originele bestand?
                perm = os.stat(file).st_mode

                #  - we verplaatsen de tempfile over het bronbestand
                #    (de rename() system call is een atomaire operatie)
                os.rename(tempname, file)

                # de tempfile had waarschijnlijk te strenge permissies
                os.chmod(file, perm)

            except Canonicalizer.Error, err:
                print >> sys.stderr, '%s: Error: %s' % (progname, err)
                error_occurred = True
            except ExpatError, err:
                print >> sys.stderr, '%s: while parsing %s: %s' % (progname, file, err)
                error_occurred = True

    sys.exit(error_occurred and 1 or 0)
