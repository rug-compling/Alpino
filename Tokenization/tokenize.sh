#!/bin/sh

export LANG=en_US.utf8
export LANGUAGE=en_US.utf8
export LC_ALL=en_US.utf8

tokenize="tok"
while getopts ":n" o; do
    case "${o}" in
	n)
	    no_breaks="-n"
	    tokenize="tok_no_breaks"
	    ;;
    esac
done

SCRIPT=$(readlink -f "$0")
TOK=$(dirname "$SCRIPT")

grep . |\
$TOK/recognize_enumerations |\
$TOK/entities |\
$TOK/$tokenize |\
$TOK/tokenize_more ${no_breaks}|\
$TOK/alp_escape



