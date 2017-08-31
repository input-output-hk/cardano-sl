#!/usr/bin/env bash

# This script generates genesis files.
# Launch it from the project root directory.
#
# It will create a directory with all needed data inside. 
#
# (!) Make sure you compile project with a appropriate mode 
# (see constants.yaml or scripts/build/cardano-sl.sh MODES).
# Script will also copy generated genesis files depending on the 
# --install-as-suffix flag. 

set -e

BUILD_MODE=stack
INSTALL_TOO=
FAKE_AVVM_ENTRIES=100
RICHMEN_SHARE=0.94
TESTNET_STAKE=19072918462000000
fail() { echo "ERROR: $*" >&2; exit 1;
}
while test $# -ge 1; do
case "$1" in
        --build-mode )
                case "$2" in 'nix' ) true;; 'stack' ) true;; * ) error "--build-mode should be either 'nix' or 'stack'";; esac;
                BUILD_MODE="$2"; shift;;
        --install-as-suffix )
                case "$2" in 'tn' ) true;; 'dn' ) true;; * ) error "--install-as-suffix should be either 'tn' or 'dn'";; esac;
                INSTALL_AS_SUFFIX="$2"; shift;;
        "--"* ) error "unknown option: $1";;
        * ) break;; esac; shift; done

scriptsDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/..
echo $scriptsDir
outputDir="genesis-$INSTALL_AS_SUFFIX-$(date +%F_%H-%M)"

if [[ -d "./$outputDir" ]]; then
  echo "./$outputDir exists already" >&2
  exit 1
fi

mkdir $outputDir

utxo_file=$scriptsDir/avvm-files/utxo-dump-last-new.json
blacklisted=$scriptsDir/avvm-files/full_blacklist.js

if [[ "$M" == "" ]]; then
    echo "You didn't set M (rich keys amount)";
    exit 1
fi
if [[ "$N" == "" ]]; then
    echo "You didn't set N (poor keys amount)";
    exit 1
fi

case "${BUILD_MODE}" in
nix )
        keygen="$(nix-build -A cardano-sl-tools --no-out-link $scriptsDir/../)/bin/cardano-keygen";;
stack ) keygen="stack exec cardano-keygen --";;
esac

# print commit
git show HEAD --oneline | tee $outputDir/genesisCreation.log

keygenCmd="${keygen} generate-genesis --genesis-dir $outputDir -m $M -n $N --richmen-share ${RICHMEN_SHARE} --testnet-stake ${TESTNET_STAKE} --utxo-file $utxo_file --blacklisted $blacklisted --fake-avvm-entries ${FAKE_AVVM_ENTRIES}"
echo "Running command: $keygenCmd"
$keygenCmd |& tee -a $outputDir/genesisCreation.log

echo "Cleaning up"
find $outputDir -name "*.lock" | xargs rm

echo "Creating archive"
tar -czf "$outputDir.tgz" $outputDir

if test -z "${INSTALL_AS_SUFFIX}"; then exit 0; fi
echo "Installing genesis with suffix '${INSTALL_AS_SUFFIX}'"

gencore="core/genesis-core-${INSTALL_AS_SUFFIX}.bin"
gentoss="godtossing/genesis-godtossing-${INSTALL_AS_SUFFIX}.bin"
geninfo="genesis-info/${INSTALL_AS_SUFFIX}.log"
cp "${outputDir}"/genesis-core.bin       "${gencore}"
cp "${outputDir}"/genesis-godtossing.bin "${gentoss}"
cp "${outputDir}"/genesisCreation.log    "${geninfo}"

echo "Genesis installed:"
ls -l "${gencore}"
ls -l "${gentoss}"
ls -l "${geninfo}"
