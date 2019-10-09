#!/bin/sh

# Script to convert Alpino XML to sents file
# where Multi word units are marked as [ @mwu ]

# Barbara, January 2010

if [ "$#" -eq 0 ]; then
  echo "Please specify a path!"
  exit
fi

utf8=0
#if set to 1 will encode in utf8, by default latin1
if [ "$3" ]; then
    utf8=1
fi

dir=/storage3/data/bplank/sw/conversion/alpino2conll

train_treebanks=$1

# give absolute path in $2
if [ "$2" == "" ]; then
    output_file=output.sents.mwu
else
    output_file=$2.sents.mwu
fi

rm -f $output_file
echo $output_file

#echo $train_treebanks
#echo $output_file

cd $dir
cd treebank
pwd

for tb in $train_treebanks 
do

  cd $tb
  rm -f *.tab
  rm -f *.tab2
  rm -f retag.log
  pwd

  echo "### Converting $tb to tabular format marking MWU in Alpino format ###"
  $dir/tools/alpino2tab.py -c -m -f -p -r -t'\n' -w *.xml > alpino2tab.log 2>&1

  echo "### Extracting sents.mwu  ###"
  for f in `ls -v *.tab`; do
      awk -v id=`echo $f | rev | cut --complement -c1-4 | rev` 'BEGIN{ printf "%s|", id }{if (NF>0) printf "%s ", $2; else print "" }' $f | sed 's/_/ /g' > $f.tmp
      if [ $utf8 -eq 1 ]; then
	  iconv -f `file $f.tmp | cut -d' ' -f2 ` -t utf-8 $f.tmp >> $output_file
      else 
	  iconv -f `file $f.tmp | cut -d' ' -f2 ` -t ISO-8859-1 $f.tmp >> $output_file
      fi
      rm $f.tmp
  done

  
#   echo "### Add data to conll file ###"
#   rm -f $output_file
#   for f in `ls -v *.tab2`; do
#       echo $f
#       iconv -f `file $f | cut -d' ' -f2 ` -t utf-8 $f > $f.tmp
#       cat $f.tmp >> $output_file
#       rm $f.tmp
#   done
   echo "## Done. Written to $output_file ###"
  
  cd ..
done 



