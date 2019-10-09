#!/bin/sh
#
# Usage: run_alpino.sh corpus part
#

if [ ! -z $1 ] ; then
	CORPUS=$1
else
	CORPUS=test
fi

if [ ! -z $2 ] ; then
	PART=$2
else
	PART=1
fi

SBOX=$(pwd)

# Sicstus/Alpino settings
PROLOGMAXSIZE=2048MB
USER_MAX=60000

# Alpino version
ALPINO_VERSION=15557
ALPINOGZ=Alpino-${ALPINO_VERSION}-i386-centos4.tar.gz

# LFNs
DIST_LFN=lfn:/grid/ncf/rugai/dekok/dist
CORPUS_LFN=lfn:/grid/ncf/rugai/dekok/corpus/${CORPUS}
INPUTS_LFN=${CORPUS_LFN}
TREEBANK_LFN=${CORPUS_LFN}/Treebank

# Retrieve and extract Alpino
echo "Retreiving Alpino"
lcg-cp --vo ncf ${DIST_LFN}/${ALPINOGZ} file://${SBOX}/Alpino.tar.gz
tar zxf Alpino.tar.gz

# Retrieve node/parts map
echo "Retrieving node map '${CORPUS}'..."
lcg-cp --vo ncf ${INPUTS_LFN}/nodemap.txt file://${SBOX}/nodemap.txt

sent_files=$(grep "^${PART} " < ${SBOX}/nodemap.txt | cut -d ' ' -f 2- )

for sent_file in ${sent_files} ; do
	sents=$(basename ${sent_file})
	name=$(basename ${sent_file} .sents.gz)
	dir=$(dirname ${sent_file})

	# Retrieve part 
	echo "Retrieving ${sent_file} from corpus '${CORPUS}'..."
	lcg-cp --vo ncf ${INPUTS_LFN}/${sent_file} file://${SBOX}/${sents}

	# Run
	mkdir ${SBOX}/${name}
	export ALPINO_HOME=${SBOX}/Alpino
	echo $ALPINO_HOME
	zcat ${SBOX}/${sents} | ${ALPINO_HOME}/bin/Alpino -notk -parse -flag treebank ${SBOX}/${name} end_hook=xml user_max=${USER_MAX}

	# Pack and store...
	( cd ${SBOX} ; ${ALPINO_HOME}/bin/miniact -c ${name} )
	echo "Storing: ${TREEBANK_LFN}/${dir}/${name}.data.dz on se.grid.rug.nl..."
	lcg-del -a --vo ncf ${TREEBANK_LFN}/${dir}/${name}.data.dz 
	lcg-cr --vo ncf -l ${TREEBANK_LFN}/${dir}/${name}.data.dz -d se.grid.rug.nl file://${SBOX}/${name}.data.dz
	lcg-del -a --vo ncf ${TREEBANK_LFN}/${dir}/${name}.index
	lcg-cr --vo ncf -l ${TREEBANK_LFN}/${dir}/${name}.index -d se.grid.rug.nl file://${SBOX}/${name}.index

	rm -rf ${SBOX}/${name}
	rm -f ${SBOX}/${name}.data.dz
	rm -f ${SBOX}/${name}.index
	rm -f ${SBOX}/${sents}
done


# Clean up...
rm -rf ${SBOX}/Alpino
rm -f ${SBOX}/Alpino.tar.gz
