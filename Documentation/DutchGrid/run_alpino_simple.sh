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
ALPINO_VERSION=15399
ALPINOGZ=Alpino-${ALPINO_VERSION}-i386-centos4.tar.gz

# LFNs
DIST_LFN=lfn:/grid/ncf/rugai/dekok/dist
CORPUS_LFN=lfn:/grid/ncf/rugai/dekok/corpus/${CORPUS}
INPUTS_LFN=${CORPUS_LFN}/inputs
TREEBANK_LFN=${CORPUS_LFN}/treebank

# Retrieve and extract Alpino
echo "Retreiving Alpino"
lcg-cp --vo ncf ${DIST_LFN}/${ALPINOGZ} file://${SBOX}/Alpino.tar.gz
tar zxf Alpino.tar.gz

# Retrieve corpus data
echo "Retrieving part ${PART} from corpus '${CORPUS}'..."
lcg-cp --vo ncf ${INPUTS_LFN}/${PART}.txt file://${SBOX}/input.txt

# Run
mkdir ${SBOX}/treebank
export ALPINO_HOME=${SBOX}/Alpino
echo $ALPINO_HOME
cat ${SBOX}/input.txt | ${ALPINO_HOME}/bin/Alpino -notk -parse -flag treebank ${SBOX}/treebank end_hook=xml user_max=${USER_MAX}

# Pack and store...
tar czf treebank.tar.gz treebank
echo "Storing: ${TREEBANK_LFN}/${PART}.tar.gz on se.grid.rug.nl..."
lcg-del -a --vo ncf ${TREEBANK_LFN}/${PART}.tar.gz 
lcg-cr --vo ncf -l ${TREEBANK_LFN}/${PART}.tar.gz -d se.grid.rug.nl file://${SBOX}/treebank.tar.gz


# Clean up...
rm -rf ${SBOX}/Alpino
rm -rf ${SBOX}/treebank
rm -f ${SBOX}/Alpino.tar.gz
rm -f ${SBOX}/input.txt
rm -f ${SBOX}/treebank.tar.gz
