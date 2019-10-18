#!/bin/sh

export LANG=en_US.utf8
export LANGUAGE=en_US.utf8
export LC_ALL=en_US.utf8

SCRIPT=$(readlink -f "$0")
TOK=$(dirname "$SCRIPT")

tokenize="tok"
alp_escape="$TOK/alp_escape"
while getopts ":en" o; do
    case "${o}" in
	e)
	    alp_escape="cat"
	    ;;
	n)
	    no_breaks="-n"
	    tokenize="tok_no_breaks"
	    ;;
    esac
done

grep . |\
$TOK/recognize_enumerations |\
$TOK/entities |\
$TOK/$tokenize |\
$TOK/tokenize_more ${no_breaks} |\
${alp_escape}
