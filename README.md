# Alpino
Alpino parser and related tools for Dutch

As per October 18, 2019, we use git at github.com for the distribution of Alpino. Note that old history is not included (for reasons of size). The old history is still available under subversion at the local server in Groningen.

Note: you need the lfs extension to git to be able to do a clone.
Normally, something like this should suffice:

    sudo apt-get install git-lfs
    git lfs install

## Installation

To compile Alpino from sources, you need SICStus Prolog 3.12.11 (it does not work with SICStus 4), or
SWI-Prolog 6.6.4 (it does not work with later SWI versions).

cp Makefile.defs.in to Makefile.defs and edit the file appropriately. After that,
ensure that the environment variable ALPINO_HOME points to the right directory, and run make

    cd Alpino
    cp Makefile.defs.in Makefile.defs
    vi Makefile.defs
    export ALPINO_HOME=`pwd`
    make
    make install

In order to build the Tokenization/partok tool, you need Go, and Peter Kleiweg's utilities:

    go get github.com/pebbe/util

