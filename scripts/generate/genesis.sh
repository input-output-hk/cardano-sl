#!/usr/bin/env bash

# This script generates genesis files.
# Launch it from the project root directory.
# It will create a directory with all needed data inside. It will not 
# copy generated genesis files into the project, this is to be done
# manually.

scriptsDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/..
echo $scriptsDir
outputDir="genesis-qanet-$(date +%F_%H-%M)"

if [[ -d "./$outputDir" ]]; then
  echo "./$outputDir exists already" >&2
  exit 1
fi

mkdir $outputDir

utxo_file=$scriptsDir/avvm-files/utxo-dump-last-new.json
blacklisted=$scriptsDir/avvm-files/full_blacklist.js

if [[ "$M" == "" ]]; then 
  M=5 # rich keys amount
fi
if [[ "$N" == "" ]]; then 
  N=12000 # poor keys amount
fi

F=100 # fake avvm keys

# print commit
git show HEAD --oneline | tee $outputDir/genesisCreation.log

keygenCmd="stack exec cardano-keygen -- generate-genesis --genesis-dir $outputDir -m $M -n $N --richmen-share 0.94 --testnet-stake 19072918462000000 --utxo-file $utxo_file --blacklisted $blacklisted --fake-avvm-entries $F"
echo "Running command: $keygenCmd"
$keygenCmd |& tee -a $outputDir/genesisCreation.log

echo "Cleaning up"
find $outputDir -name "*.lock" | xargs rm 

echo "Creating archive"
tar -czf "$outputDir.tgz" $outputDir
