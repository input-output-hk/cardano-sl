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

buildMode=stack
installAsSuffix=dn
fakeAVVMEntries=100
richmenShare=0.94
testnetStake=19072918462000000
verboseBuild=
seedOpt=
parallelBuild=true
error() { echo "ERROR: $*" >&2; exit 1;
}
while test $# -ge 1; do
case "$1" in
        --build-mode )         case "$2" in 'nix' | 'stack' ) true;; * ) error "--build-mode should be either 'nix' or 'stack'";; esac;
                               buildMode="$2";       shift;;
        --install-as-suffix )  case "$2" in 'tn'  | 'dn'    ) true;; * ) error "--install-as-suffix should be either 'tn' or 'dn'";; esac;
                               installAsSuffix="$2"; shift;;
        --fake-avvm-entries )  fakeAVVMEntries="$2"; shift;;
        --richmen-share )      richmenShare="$2";    shift;;
        --testnet-stake )      testnetStake="$2";    shift;;
        --rich-keys | -M )     M="$2";               shift;;
        --poor-keys | -N )     N="$2";               shift;;
        --output-dir )         outputDir="$2";       shift;;
        --no-parallel-build )  parallelBuild="";     shift;;
        --verbose-build )      verboseBuild=yes;     shift;;
        --seed )               seedOpt="--seed $2";  shift;;
        "--"* ) error "unknown option: $1";;
        * ) break;; esac; shift; done

scriptsDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/..
echo $scriptsDir
outputDir="${outputDir:-genesis-$installAsSuffix-$(date +%F_%H-%M)}"

test ! -e "./$outputDir" || error "./$outputDir exists already" >&2
test -n "$M" || error "You didn't set/pass M (rich keys amount)";
test -n "$N" || error "You didn't set/pass N (poor keys amount)";

mkdir $outputDir

utxo_file=$scriptsDir/avvm-files/utxo-dump-last-new.json
blacklisted=$scriptsDir/avvm-files/full_blacklist.js

case "${buildMode}" in
nix )   keygen="$(nix-build -A cardano-sl-tools --no-out-link ${parallelBuild:+--cores 0 --max-jobs 4} ${verboseBuild:---no-build-output} $scriptsDir/../)/bin/cardano-keygen";;
stack ) keygen="stack exec cardano-keygen --";;
esac

# print commit
PAGER=cat git show HEAD --oneline --no-patch --text | tee $outputDir/genesisCreation.log

keygenCmd="${keygen} generate-genesis --genesis-dir $outputDir -m $M -n $N --richmen-share ${richmenShare} --testnet-balance ${testnetStake} --utxo-file $utxo_file --blacklisted $blacklisted --fake-avvm-entries ${fakeAVVMEntries} ${seedOpt}"
echo "Running command: $keygenCmd"
$keygenCmd |& tee -a $outputDir/genesisCreation.log

echo "Cleaning up"
find $outputDir -name "*.lock" | xargs rm

echo "Creating archive"
tar -czf "$outputDir.tgz" $outputDir

if test -z "${installAsSuffix}"; then exit 0; fi
echo "Installing genesis with suffix '${installAsSuffix}'"

gencore="core/genesis-core-${installAsSuffix}.bin"
gentoss="godtossing/genesis-godtossing-${installAsSuffix}.bin"
geninfo="genesis-info/${installAsSuffix}.log"
cp "${outputDir}"/genesis-core.bin       "${gencore}"
cp "${outputDir}"/genesis-godtossing.bin "${gentoss}"
cp "${outputDir}"/genesisCreation.log    "${geninfo}"

echo "Genesis installed:"
ls -l "${gencore}"
ls -l "${gentoss}"
ls -l "${geninfo}"
