#!/bin/bash

## needs 'gnused' from Nix on MacOSX

if [ $# -lt 1 ]; then
  echo "$0 <report-???.txt>"
  exit 1
fi

BASEDIR=`dirname $0`
REPFILE=$1
BLIDs=
NODES="c-a-1 c-a-2 c-a-3 c-b-1 c-b-2 c-c-1 c-c-2"

BLIDs=`awk '/transactions in fork:/{start=1;next} start; !NF && start{start=0}' $REPFILE  \
   | cut -d " " -f 2 | sort | uniq | sed -ne '2,$p' | paste -s -d " " -`


echo -n "        "
for BLID in ${BLIDs}; do
  echo -n "   ${BLID}  "
done
echo
for NODE in ${NODES}; do
  echo -n "$NODE"
  FN="${NODE}.log"
  ${BASEDIR}/xblocks.sh $FN > ${FN}.blocks
  for BLID in ${BLIDs}; do
    #CNT=`fgrep "previous block: ${BLID}" $FN | wc -l`
    CNT=`cat ${FN}.blocks | ${BASEDIR}/parse.hs ${BLID} | tail -1 | cut -d " " -f 1`
    echo -n  "          ${CNT}"
  done
  echo
done
