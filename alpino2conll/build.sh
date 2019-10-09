#!/bin/sh

# Script to derive convert Alpino XML to conll format
# Based on Erwin Marsi's Conll 2006 conversion data
# But uses Alpino POS tags instead of MBT wotan tags

# Barbara, September 2009

if [ "$#" -eq 0 ]; then
  echo "Please specify a path with Alpino xml files!"
  exit
fi

if [ -z ${ALPINO_HOME}  ]; then
  echo "Please specify ALPINO_HOME!"
  exit
fi

echo "Using $ALPINO_HOME"

if [ -z ${ALPINO2CONLL_HOME}  ]; then
  echo "Please specify ALPINO2CONLL_HOME!"
  exit
fi


#dir=/storage/bplank/tools/erwins-software/data/dutch/alpino
#dir=/storage3/data/bplank/sw/conversion/alpino2conll

# path to scripts (without tools at end)
### SET THIS VARIABLE in your .bashrc!
#export ALPINO2CONLL_HOME=/home/p252438/sw/conversion/alpino2conll/

dir=${ALPINO2CONLL_HOME}

echo $ALPINO2CONLL_HOME

#treebankdir=$dir/treebank
#if [ "$1" != "" ]; then
#    treebankdir=$1
#    echo "Using path $treebankdir"
#fi

train_treebank=$1

output_file=output.conll
#stores output in utf-8 format (see conversion below)

#echo $train_treebanks
#echo $output_file

#cd $treebankdir

cd $train_treebank
#cd treebank
echo "current dir:"
pwd

#for tb in $train_treebanks 
#do
#  mkdir -p $tb
  #cd $tb

rm -f *.tab
rm -f *.tab2
rm -f retag.log
pwd


echo "### Converting $tb to tabular format ###"
echo $dir/tools/alpino2tab.py
$dir/tools/alpino2tab.py -c -f -p -r -t'\n' -w *.xml > alpino2tab.log 2>&1

  
echo "### Retagging $tb ###"
for f in `ls -v *.tab`; do
    echo $f
    $dir/tools/tag.py -w2 -f $f >> retag.log 

    #cleaning
#    rm ${f}2.parsed
    rm $f
#    rm sent
done

  
echo "### Add data to conll file ###"
rm -f $output_file
for f in `ls -v *.tab2`; do
    echo $f
#    iconv -f `file $f | cut -d' ' -f2 ` -t utf-8 $f > $f.tmp
#    cat $f.tmp >> $output_file.utf8
    cat $f >> $output_file
    #cat $f >> $output_file
    #rm $f.tmp
done
echo "## Done. Written to $1$output_file ###"
 
cd ..

#done 



