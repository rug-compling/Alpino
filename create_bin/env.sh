# tested with:
#   bash
#   dash
#   zsh

if [ "`basename -- $0`" = "env.sh" ]
then
    echo
    echo This script should be sourced, like this:
    echo
    echo . $0
    echo
    exit
fi

if [ "$ALPINO_HOME" = "" ]
then
    if [ "$BASH_VERSION" != "" ]
    then
        ALPINO_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"
    fi
fi

a=$ALPINO_HOME
if [ "$a" = "" ]
then
    echo
    echo Please set the environment variable ALPINO_HOME
    echo
elif [ ! -x "$a/bin/Alpino" ]
then
    echo
    echo '$ALPINO_HOME/bin/Alpino is not executable'
    echo 'Did you set ALPINO_HOME correctly?'
    echo
elif [ "$ALPINO_SET" = "" ]
then
    ALPINO_SET=1
    PATH="$a/bin:$a/Tokenization:$a/TreebankTools/bin:$PATH"
    LD_LIBRARY_PATH="$a/fadd:$a/unix:$a/create_bin/extralibs:$a/TreebankTools/IndexedCorpus${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}"
    if [ "$ZSH_VERSION" != "" ]
    then
        p="$PROMPT"
        prompt off
        PROMPT="[Alpino] $p"
        unset p
    else
        PS1="[Alpino] $PS1"
    fi
    SP_CSETLEN=212
    SP_CTYPE=utf8
    TCL_LIBRARY=$a/create_bin/extralibs/tcl8.5
    TK_LIBRARY=$a/create_bin/extralibs/tk8.5
    export TCL_LIBRARY
    export TK_LIBRARY
    export ALPINO_HOME
    export ALPINO_SET
    export LD_LIBRARY_PATH
    export SP_CSETLEN
    export SP_CTYPE
fi
unset a

if `which alto > /dev/null`
then
    export HAS_ALTO=1
else
    export HAS_ALTO=0
fi

## note why we always set TK_LIBRARY and TCL_LIBRARAY:
## there are different versions of tcl/tk 8.6, therefore we must ensure
## we use the same one as we used for compiling Alpino (if we use 8.6)
## therefore we cannot rely on an existing tcltk installation :-(
### example: our current one has (init.tcl):
### package require -exact Tcl 8.6.5
### whereas newer ones will have e.g.
### package require -exact Tcl 8.6.10
### and then it does no longer work
## perhaps this does not apply to 8.5

