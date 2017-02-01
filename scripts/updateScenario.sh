#!/bin/sh

set -e
csldir=$(pwd)

# Sanity checks
which stack > /dev/null
which tmux > /dev/null
which webfsd > /dev/null
which pkill > /dev/null
if [ ! ${csldir: -10} == "cardano-sl" ]; then echo "You should launch this script from git repo of cardano-sl" && exit; fi
if [ -z "$TMUX" ]; then echo "You should run this script inside tmux session" && exit; fi

# Checking cardano-updater repo is there, cloning if not
if [ ! -d ../cardano-updater ]; then
    echo "Didn't manage to locate cardano-update repo, cloning"
    cd ..
    git clone https://github.com/input-output-hk/cardano-updater.git
fi

# Building updater
cd ../cardano-updater
echo "Building cardano-updater"
stack build --fast
cd $csldir

updater=$(find ../cardano-updater/.stack-work/install/ -name "cardano-updater" -exec readlink -f {} \; | head -n 1)

serverPort=8100 # Port for webfsd

# Replacing server address in constants-dev
sedline="s/updateServers:.*/updateServers: \[ \"http:\/\/localhost:${serverPort}\/\" \]/"
sed -i.backup -E "$sedline" constants-dev.yaml

echo "Building cardano-sl"
stack build --fast

# Copying artefacts for v0.0.0
echo "Preparing binaries with 0.0.0"
csl_bin=$(find .stack-work/install/ -iname "bin")
rm -rf binaries_v000 && mkdir binaries_v000
cp -v $csl_bin/* binaries_v000/

# Updating version in csl sources to v0.1.0
sed -i.backup "s/BlockVersion 0 0 0/BlockVersion 0 1 0/" src/Pos/Constants.hs
echo "Building cardano-sl with version 0.1.0"
stack build --fast
rm -rf binaries_v010 && mkdir binaries_v010
cp -v $csl_bin/* binaries_v010/

# Restoring version and binaries
echo "Restoring binaries"
mv -v src/Pos/Constants.hs.backup src/Pos/Constants.hs
cp binaries_v010/* $csl_bin/

updatetar="binaries_000_010.tar"
rm -rf $updatetar
echo "Creating diff tar $updatetar (might take a while)"
stack exec cardano-genupdate -- binaries_v000 binaries_v010 $updatetar


echo "Launching 3 nodes in 5 secs"
sleep 5
./scripts/launch.sh 3
sleep 10
tmux select-window -t 0

echo "Launching wallet"
set +e

# Sometimes node hangs, so here's a timeout
walletcmd="propose-update 0 0.1.0 1 20 10000000 cardano-1 ${updatetar}"
pkill cardano-wallet
echo "$walletcmd"
walletoutput=$(./scripts/wallet.sh cmd --commands "$walletcmd" -p 0)
echo "$walletoutput"

proposalHash=$(echo "$walletoutput" | grep -i "Update proposal submitted" | cut -d' ' -f 5)
test -z "$proposalHash" && echo "Didn't manage to retrieve proposal hash, aborting"  && exit
echo "Proposal hash is $proposalHash"

updateVersion=$(echo "$walletoutput" | grep -i "Read file succesfuly, its hash:" | cut -d' ' -f 6)
test -z "$proposalHash" && echo "Didn't manage to retrieve update version, aborting" && exit
echo "Update version is $updateVersion"

# Voting for hash
pkill cardano-wallet
./scripts/wallet.sh cmd --commands "vote 1 y $proposalHash" -p 0 
pkill cardano-wallet
set -e

# Sharing in webfs
rm -rf webfsfolder && mkdir webfsfolder
cp -v $updatetar webfsfolder/$updateVersion
pkill webfsd
webfsd -p $serverPort -r webfsfolder
echo "Launched webfs server"

# Do your launcher stuff here

notify-send "updater scenario: ready"
