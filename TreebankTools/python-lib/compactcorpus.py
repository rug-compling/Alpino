# Een module om compacte corpora mee te benaderen

# Een compact corpus bestaat uit een dictzip-file en een index file

import sys
import os
import re
import gzip
import numsort
from glob import glob
from indexedcorpus import IndexedCorpusReader, IndexedCorpusWriter
import tempfile

corpusfile_re = re.compile(r'(?:\.data\.dz|\.index)$')

def msg(str):
    sys.stderr.write("%s: %s\n" % (os.path.basename(sys.argv[0]), str + "\n"))


def extract(xmlData, filename, targetDir):
    outfile = os.path.join(targetDir, os.path.basename(filename))
    ofp = open(outfile,"w")
    ofp.write(xmlData)
    ofp.close

def endCurry(func, *curried):
    return lambda *args: func(*(args + curried))

def extract_archive(arg, targetdir, force=False):
    corpusName = get_corpus_name(arg)
    targetdir = os.path.join(targetdir, corpusName)

    if os.path.exists(targetdir) and not force:
        msg("Error: refusing to overwrite `%s'.  Exiting." % (targetdir))
        sys.exit(1)

    if not os.path.exists(targetdir):
        os.mkdir(targetdir)

    corpusName = get_corpus_name(arg)
    extractFun = endCurry(extract, targetdir)
    process_compact_corpus(arg, extractFun, True)

def printFileName(fileName):
    print fileName

def list_archive(archive):
    process_compact_corpus(archive, printFileName, False)

def get_corpus_noext(arg):
    """Get the basename ARG for ARG, ARG.data.dz and ARG.index.

    Retourneert het corpusbestand zonder extensie en evt. inclusief pad.
    """
    m = re.match(r"(.*?)(\.index|\.data\.dz)?$", arg)
    return m.group(1)


def get_corpus_filenames(arg):
    """Retourneer de corpusbestanden behorend bij ARG.

    ARG kan zijn:

        CORPUSNAAM
        CORPUSNAAM.index
        CORPUSNAAM.data.dz

    Retourneert (indexfile, datafile).
    """
    corpus_noext =  get_corpus_noext(arg)
    return (corpus_noext + ".index", corpus_noext + ".data.dz") 


def get_corpus_name(arg):
    """Retourneer de naam van het corpus behorend bij ARG.

    ARG kan zijn:

        CORPUSNAAM
        CORPUSNAAM.index
        CORPUSNAAM.data.dz

    Retourneert CORPUSNAAM
    """
    corpus_noext =  get_corpus_noext(arg)
    return (os.path.basename(corpus_noext))


def corpus_exists_p(corpuspath):
    """Check if CORPUS_NOEXT.data.dz and CORPUS_NOEXT.index exist"""
    (indexfile, datafile) = get_corpus_filenames(corpuspath)

    if not os.path.exists(indexfile):
        return(0)
    if not os.path.exists(datafile):
        return(0)
    return 1


def create_archive(directory, targetdir, force=0, only_newer=0, remove=0):
    """Maak een archief (compact corpus) van DIRECTORY.

    Alle .xml bestanden in DIRECTORY komen in het archief terecht.

    Creeert in TARGETDIR de files DIRECTORY.index.en DIRECTORY.data.dz.

    When FORCE is not set, existing compact corpora result in a fatal error.

    When ONLY_NEWER is set, only creates a new compact corpus when the
    source directory is newer. 

    When REMOVE is set, the source files in DIRECTORY will be deleted
    afterwards
    """
    startdir = os.getcwd()
    
    sys.stderr.write("processing `%s'\n" % (directory))

    filelist = glob(os.path.join(directory,'*.xml'))

    if not filelist:
        msg("Error: no .xml files found in directory `%s', skipping directory..." \
              % (directory))
        return

    # de laatste component van de directory wordt de naam van het corpus
    corpus_name= os.path.basename(os.path.normpath(directory))

    indexfile, datafile = get_corpus_filenames(os.path.join(targetdir, corpus_name))

    if os.path.exists(datafile):
        if not force:
            msg("FATAL ERROR: refusing to overwrite `%s', use --force to override" % (datafile))
            sys.exit(1)

        if only_newer:
            if os.path.getmtime(directory) > os.path.getmtime(datafile):
                msg("creating `%s'..." % (datafile))
            else:
                msg("corpus `%s' is up to date" % (datafile))
                return
        else:
            msg("compact corpus `%s' exists.  overwriting..." % (datafile))

    filelist = numsort.sorted_copy(filelist)

    basePath = os.path.join(targetdir, corpus_name)
    corpuswriter = IndexedCorpusWriter("%s.index" % basePath,
        "%s.data.dz" % basePath)

    offset = 0
    for file in filelist:
        indexName = os.path.basename(file)
        data = open(file).read()

        if len(data.strip()) == 0:
            print >> sys.stderr, "Skipping empty file: %s" % file
            continue

        corpuswriter.write(indexName, data)

    if remove:
        for file in filelist:
            os.remove(file)
        # try to remove the directory too
        try:
            os.rmdir(directory)
        except:
            pass


def update_archive(sourcedir, targetdir, only_newer=0, remove=0):
    """Update the compact corpus in TARGETDIR with the contents in
    SOURCEDIR.

    The .xml files in SOURCEDIR will be added to the compact corpus.

    The compact corpus files will be TARGETDIR/SOURCEDIR.{index,data.dz}

    When ONLY_NEWER is set, only creates a new compact corpus when the
    source directory is newer. 
    
    When REMOVE is set, the source files in SOURCEDIR will be deleted afterwards.

    A temporary backup will be made of the existing .index and .data.dz files.
    (.bak suffix added)
    """

    verbose = 0

    sourcefiles = glob(os.path.join(sourcedir, "*.xml"))
    if not sourcefiles:
        msg("No files found in %s, skipping directory..." % (sourcedir))
        return

    corpusname = os.path.basename(os.path.normpath(sourcedir))
    targetcorpuspath = os.path.join(targetdir, corpusname)

    # FIXME: should we check for left-over .bak files here?

    # if the compact corpus does not exist, just create a new one
    if not corpus_exists_p(targetcorpuspath):
        msg("Compact corpus %s does not exist, creating new corpus" %
                (targetcorpuspath))
        create_archive(sourcedir, targetdir, force=1, only_newer=only_newer, remove=remove)
        return

    # only_newer afhandelen
    # als de corpusfile nieuwer is zijn we klaar
    targetIndexFile, targetDataFile = get_corpus_filenames(targetcorpuspath)
    if only_newer:
        if os.path.getmtime(targetDataFile) > os.path.getmtime(sourcedir):
            msg("corpus `%s' is up to date" % (datafile))
            return

    # tell the user we're actually doing something...
    msg("Updating %s with %s..." % (targetcorpuspath, sourcedir))


    # we want to create a temporary compact corpus that we'll
    # rename later
    #
    # - the filenames should be on the same file system as the target
    #   compact corpus
    #
    # - there's no way to have mktemp make a  .index and a .data.dz 
    #   with the same basename
    #
    # - so we'll create a subdirectory within the target directory;
    #   this should be on the same filesystem


    # create a temporary directory
    tmpdir = tempfile.mkdtemp(dir=targetdir)

    # do a merge

    # create a temporary compact corpus
    tmpCorpusPath = os.path.join(tmpdir, corpusname)
    (tmpIndexFile, tmpDataFile) = get_corpus_filenames(tmpCorpusPath)
    writer = IndexedCorpusWriter(tmpIndexFile, tmpDataFile)

    # Get the filenames from the corpus we're supposed to be updating.
    (old_indexfile, old_datafile) = get_corpus_filenames(targetcorpuspath)
    oldcorpus = IndexedCorpusReader(old_indexfile, old_datafile)

    old_keys = oldcorpus.entries()
    sourcefiles = numsort.sorted_copy(sourcefiles)

    old_len = len(old_keys)
    update_len = len(sourcefiles)

    old_pos = 0
    update_pos = 0

    while (1):

        if update_pos >= update_len:
            # take the rest of the old corpus
            while old_pos < old_len:
                key = old_keys[old_pos]
                data = oldcorpus.data(key)
                writer.write(key, data)
                old_pos += 1

            # and we're done
            break
        
        if old_pos >= old_len:
            # take the rest of the new stuff
            while update_pos < update_len:
                name = os.path.basename(sourcefiles[update_pos])
                data = open(sourcefiles[update_pos]).read()
                writer.write(name, data)
                update_pos += 1

            # and we're done
            break

        update_key = os.path.basename(sourcefiles[update_pos])
        old_key = old_keys[old_pos]
        cmp_result = numsort.compare(update_key, old_key)

        if cmp_result == 0:
            # take the file from the update
            data = open(sourcefiles[update_pos]).read()
            writer.write(update_key, data)
            old_pos += 1
            update_pos += 1

        elif cmp_result < 0:
            # take the file from the update
            data = open(sourcefiles[update_pos]).read()
            writer.write(update_key, data)
            update_pos += 1

        else:
            # take the file from the old set
            data = oldcorpus.data(old_key)
            writer.write(old_key, data)
            old_pos += 1

    del writer

    # maak een backup van de bestaande corpus files
    for file in (targetIndexFile, targetDataFile):
        if verbose:
            msg('moving "%s" to "%s"' % (file, file + ".bak"))
        os.rename(file, file + ".bak")


    # verplaats de nieuwe corpus bestanden naar de juiste plek
    os.rename(tmpIndexFile, targetIndexFile)
    os.rename(tmpDataFile, targetDataFile)

    ## cleanup

    if verbose:
        msg("cleaning up...")

    # if we're here, the update was succesful; remove the .bak files

    # FIXME: there's still a slight chance things mess up on interrupt.

    os.remove(targetIndexFile + ".bak")
    os.remove(targetDataFile + ".bak")

    # rmdir should be enough now, tmpdir should not contain any files
    os.rmdir(tmpdir)

    if remove:
        for file in sourcefiles:
            os.remove(file)


def is_corpus_file(file):
    """File is een dictzip corpus file (op basis van extensie)"""
    global corpusfile_re
    return corpusfile_re.search(file)


def process_compact_corpus(corpus, function, passData, **keywords):
    """Apply FUNCTION for every piece of data in CORPUS.

    FUNCTION will be called with arguments DATA and FILENAME.
    Any optional keyword arguments will also be passed.

    We gaan er vanuit dat CORPUS altijd een compact corpus is.
    """
    (indexfile,datafile) = get_corpus_filenames(corpus)

    corpus_noext = get_corpus_noext(corpus)

    reader = IndexedCorpusReader(indexfile, datafile)

    for entry in reader.entries():
        if passData:
            xmldata = reader.data(entry)
 
        # zodat we "subdir/corpusnaam/filenaam.xml" als uitvoer krijgen
        filename = os.path.join(corpus_noext, entry)

        if passData:
            apply(function, (xmldata, filename), keywords)
        else:
            apply(function, (filename,), keywords)

if __name__ == '__main__':
    list_archive(sys.argv[1])
