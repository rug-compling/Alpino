# actlib.py

# een module met functies voor "act" en "dtsearch"
# afgesplitst van "act"

# $Id: actlib.py,v 1.6 2006/10/16 15:24:38 geertk Exp $

import sys
import os
import re
from lxml import etree
from glob import glob

from compactcorpus import *
from indexedcorpus import CorpusReader
import numsort


#----------------------------------------------------------------------
# initialisatie

xml_re = re.compile('\.xml$')
filereader = CorpusReader()


# SIGPIPE afhandeling naar UNIX default: zo gaat '|head' ook goed
#
# By default negeert Python SIGPIPE.  Dat heeft tot gevolg dat we
# naar een pipe kunnen schrijven die al gesloten is, hetgeen ons
# de IO Error "Broken pipe" oplevert.
#
# Door de signal handler voor SIGPIPE naar de default te zetten
# zal het proces termineren bij een SIGPIPE. (zie signal(7)).
import signal
signal.signal(signal.SIGPIPE, signal.SIG_DFL)


#----------------------------------------------------------------------
# functions

def print_arg(arg):
    print arg

def extract_archives(args, targetdir, force=0):
    for arg in args:
        corpus_exists_or_die(arg)
        extract_archive(arg, targetdir, force)


def list_archives(archivelist):
    for archive in archivelist:
        if (os.path.isdir(archive)):
            xmlFiles = numsort.sorted_copy(glob(archive + '/*.xml'))
            for f in xmlFiles:
                print f
        else:
            corpus_exists_or_die(archive)
            list_archive(archive)


def corpus_exists_or_die(corpuspath):
    """Check if CORPUS_NOEXT.data.dz and CORPUS_NOEXT.index exist (FATAL)"""

    # exceptions gebruiken?
    # Als we de check weglaten krijgen we vanzelf exceptions :-|
    if not corpus_exists_p(corpuspath):
        msg('Error: corpus file missing for "%s".  Exiting...' % (corpuspath))
        sys.exit(1)


def create_archives(directories, targetdir, force=0, only_newer=0, remove=0):
    for directory in directories:
        if not os.path.isdir(directory):
            msg("Error: `%s' is not a directory.  Skipping..." % (directory))
            continue
        create_archive(directory, targetdir, force, only_newer, remove)


def update_archives(directories, targetdir, only_newer=0, remove=0):
    for directory in directories:
        if not os.path.isdir(directory):
            msg("Error: `%s' is not a directory.  Skipping..." % (directory))
            continue
        update_archive(directory, targetdir, only_newer, remove)


def recursive_create_archive(startdir, targetdir, update, only_newer=0, remove=0, force=0):
    """Recursively transform a directory tree with .xml files into a
    directory tree with compact corpora.

    TARGETDIR will be created if necessary.

    Symlinks will be skipped.

    When UPDATE is true, `update_archive' is used instead of 
    `create_archive' to create the archives.

    ONLY_NEWER is used to skip over directories that are (look) up to date

    When FORCE is set, force overwriting of archives when UPDATE is not set
    """
    
    # even een debug message
    if 0:
        msg("recursive_create_archive(%s, %s)" % (startdir, targetdir))

    # niet-directories overslaan
    if not os.path.isdir(startdir):
        msg("Error: `%s' is not a directory.  Skipping..." % (startdir))
        return

    # als startdir .xml files bevat maken we een compact corpus onder
    # targetdir
    if glob(os.path.join(startdir, "*.xml")):

        # Als targetdir nog niet bestaat, dan maken we die
        if not os.path.exists(targetdir):
            os.makedirs(targetdir)

        if update:
            update_archive(startdir, targetdir, only_newer, remove)
        else:
            create_archive(startdir, targetdir, force, only_newer, remove)

    # zo niet, dan herhalen we het kunstje voor alle subdirectories
    else:

        # symlinks skippen
        subdirs = []
        for item in os.listdir(startdir):
            fullpath = os.path.join(startdir, item)
            if os.path.isdir(fullpath):
                if os.path.islink(fullpath):
                    msg("WARNING: skipping symlink `%s'!" % (fullpath))
                else:
                    subdirs.append(item)
                        
        subdirs.sort()
        for dir in subdirs:
            # er zijn hier twee gevallen:
            #
            # 1. "dir" bevat .xml files
            #     --> "dir" is geen subdirectory onder "targetdir"
            # 2. "dir" bevat geen .xml files
            #    --> "dir" (mogelijk) wel een subdir onder "targetdir"

            ## FIXME: we doen dubbel werk; 2x glob()
            newstartdir = os.path.join(startdir, dir)
            if glob(os.path.join(newstartdir, "*.xml")):
                newtargetdir = targetdir
            else:
                newtargetdir = os.path.join(targetdir, dir)

            recursive_create_archive(newstartdir,
                                     newtargetdir,
                                     update,
                                     only_newer,
                                     remove,
                                     force)


def msg(str):
    sys.stderr.write(os.path.basename(sys.argv[0]) + ": " + str + "\n")


def is_xml_file(file):
    """File is een .xml file (op basis van extensie)"""
    global xml_re
    return xml_re.search(file)


def process_argument(arg, function, recursive=0, passData = True, **keywords):
    """Apply FUNCTION for every xml file in ARG.

    ARG can be one of the following:
    
    1. compact corpus
    2. directory
    3. single .xml file
    4. .xml file in compact corpus

    FUNCTION will be called with arguments DATA and FILENAME.

    When RECURSIVE is set this function works recursively.  See
    `process_directory' for details.
    """

    global filereader

    # bepaal wat voor soort argument het is

    # Directories hebben voorrang tov compacte corpora.
    # Wanneer de directory bestaat maar de file niet, wordt *niet*
    # alsnog gekeken of de file wel in het compacte corpus voorkomt.

    if is_xml_file(arg):
        if passData:
            # Get the corpus entry. If retrieval of the entry fails, we
            # will print the exception message to stderr. We do not want
            # to fail completely, since there may be other files to
            # process.
            try:
                data = filereader.data(arg)
            except RuntimeError, e:
                print >> sys.stderr, e.message
            else:
                if data:
                    apply(function,(data, arg), keywords)
        else:
            apply(function, (arg,), keywords)

    elif is_corpus_file(arg):
        process_compact_corpus(arg, function, passData, **keywords)

    elif os.path.isdir(arg):
        process_directory(arg, function, recursive, passData, **keywords)
    elif corpus_exists_p(arg):
        process_compact_corpus(arg, function, passData, **keywords)
    elif os.path.exists(arg):
        # we willen hier ook files die niet op .xml eindigen kunnen verwerken
        if passData:
            fp = open(arg)
            data = fp.read()
            apply(function,(data, arg), keywords)
            fp.close()
        else:
            apply(function, (arg,), keywords)
    else:
        msg('Error: "%s": no such file, directory, or compact corpus' % (arg))

def process_from_stdin(function, recursive = 0, passData = True, **keywords):
    """Apply FUNCTION to every argument read from stdin.

(compact corpus, directory, or path to a file)

FUNCTION will be called with arguments DATA and FILENAME.

Any extra keyword arguments will also be passed to FUNCTION.
""" 
    # Moet hier een check op tty?
    processed_files = 0
    for line in sys.stdin:
        processed_files = 1
        process_argument(line.rstrip(), function, recursive, passData, **keywords)
    if not processed_files:
        raise "no arguments found in stream!"
    

def process_arguments(args, function, use_stdin, recursive=0, passData = True, **keywords):
    """Wrapper function for `process_argument' and `process_from_stdin'."""
    if use_stdin:
        process_from_stdin(function, recursive, passData, **keywords)
    else:
        for arg in args:
            process_argument(arg, function, recursive, passData, **keywords)


def get_arglist_from_stdin():
    """Get the list of arguments from stdin"""
    l = []
    for line in sys.stdin:
        l.append(line.rstrip())
    if not l:
        msg("Error: no arguments found in stream!")
    return l


def process_directory(dir, function, recursive = 0, passData = True, **keywords):
    """Apply FUNCTION on the data of every .xml file in directory DIR.

    FUNCTION will be called with arguments DATA and FILENAME and any
    optional keyword arguments

    When RECURSIVE is *not* set, only the .xml files in DIR are
    processed.

    When RECURSIVE is true, this function will also look for compact
    corpora and will dive into subdirectories.

    When DIR contains .xml files, no further processing is done except
    for the .xml files.  (implied by the structure of compact corpora)
    """

    # xmlfiles
    xmlfiles = glob(os.path.join(dir, "*.xml"))
    xmlfiles = numsort.sorted_copy(xmlfiles)
    for file in xmlfiles:
        if passData:
            fp = open(file)
            data = fp.read()
            fp.close()
            apply(function, (data, file), keywords)
        else:
            apply(function, (file,) , keywords)

    # als we xmlfiles gevonden hebben hoeven niet verder te kijken
    if not recursive or xmlfiles:
        return

    # dictzip files
    dictzip_files = glob(os.path.join(dir, "*.data.dz"))
    dictzip_files.sort()

    for file in dictzip_files:
        # alleen die compacte corpora waar geen directory voor is
        dirname = get_corpus_noext(file)
        if not os.path.isdir(dirname):
            process_compact_corpus(file, function, passData, **keywords)
    
    # directories
    # FIXME?: hier ook symlinks overslaan??  Nee toch??
    subdirs = os.listdir(dir)
    subdirs = map(lambda x: os.path.join(dir, x), subdirs)
    subdirs = filter(os.path.isdir, subdirs)
    subdirs.sort()

    for subdir in subdirs:
        process_directory(subdir, function, recursive, passData, **keywords)
    

def xmlmatch_from_mem(xmldata, filename, query=None, stylesheet=None, params=None):
    """Laat QUERY los op XMLDATA zoals bij xmlmatch

    FILENAME is de filenaam die we voor de uitvoer gebruiken.

    Beschrijving keyword argumenten:

      QUERY       - een XPath (string-)expressie met de query
      STYLESHEET  - een geparsed stylesheet (of None)
      PARAMS      - een dictionary met parameters voor het stylesheet

    """

    try:
        doc = etree.fromstring(xmldata)
    except Exception, error:
        print >> sys.stderr, "Could not parse %s: %s" % (filename, error)
        return

    if query:
         xpathobj = doc.xpath(query)
    else:
         xpathobj = True

    if xpathobj:
        if stylesheet:

            # de filename parameter zetten
            params["filename"] = xslt_quote_string(filename)

            # het stylesheet loslaten op de file
            result = stylesheet(doc, **params)

            # uitvoer naar stdout
            result.write_output("-")
        else:
            print filename


def xslt_quote_string(str):
    """Quote STR als string parameter als bij libxslt/xsltproc

    Dit is nodig ivm. een beperking van libxslt.
    (.i.e. de interface om stringparameters zonder quoting door te geven is
    nog niet beschikbaar in libxslt)
    """

    if str.find('"') > -1:
        if str.find("'") > -1:
            raise "stringparam contains both quote and double quotes!: %.20s\n" \
                  % (str)
        str = "'" + str + "'"
    else:
        str = '"' + str + '"'
    return str


def print_xmldata(xmldata, filename):
    """Print XMLDATA naar stdout; bedoeld als argument voor `process_argument'"""
    sys.stdout.write(xmldata)


#----------------------------------------------------------------------
