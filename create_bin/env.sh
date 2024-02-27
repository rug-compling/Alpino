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
        a="$CDPATH"
        unset CDPATH
        ALPINO_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"
        if [ "$a" != "" ]
        then
            export CDPATH="$a"
        fi
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
    TCL_LIBRARY=$a/create_bin/extralibs/tcl8.6
    TK_LIBRARY=$a/create_bin/extralibs/tk8.6
    export TCL_LIBRARY
    export TK_LIBRARY
    export ALPINO_HOME
    export ALPINO_SET
    export LD_LIBRARY_PATH
    export SP_CSETLEN
    export SP_CTYPE
fi
unset a

