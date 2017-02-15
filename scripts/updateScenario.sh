#!/bin/sh

build=false
runNode=false
wallet_cli=""

echo "Parsing command line arguments..."

while [[ $# -gt 0 ]]
do
  key="$1"
  case $key in
    -n|--node)
      echo "--node flag is on"
      runNode=true
      ;;
    -b|--build)
      echo "--build flag is on"
      build=true
      ;;
    -w|--wallet)
      echo "--wallet is $2"
      wallet_cli="--wallet $2"
      shift
      ;;
    *)
      # unknown option
      ;;
  esac
  shift # past argument or value
done

set -e
csldir=$(pwd)
serverPort=8100 # Port for webfsd
updatetar="binaries_000_010.tar"


# Sanity checks
echo "Sanity checks..."

which stack > /dev/null
which tmux > /dev/null
which webfsd > /dev/null
which pkill > /dev/null
if [ ! ${csldir: -10} == "cardano-sl" ]; then echo "You should launch this script from git repo of cardano-sl" && exit; fi
if [ -z "$TMUX" ]; then echo "You should run this script inside tmux session" && exit; fi

# Checking cardano-updater repo is there, cloning if not
echo "Checking cardano-updater repo..."
cd ..
if [ ! -d cardano-updater ]; then
    echo "Didn't manage to locate cardano-update repo, cloning"
    git clone https://github.com/input-output-hk/cardano-updater.git
fi
cd cardano-updater
echo "Searching for cardano-updater install path..."
cardano_updater_local_bin=$(stack path --local-install-root)/bin
echo "Searching for cardano-updater binary..."
updater=$(find $cardano_updater_local_bin -name "cardano-updater" -exec readlink -f {} \; | head -n 1)
cd $csldir

echo "Launching 3 nodes in 5 secs"
pkill cardano-node || true
sleep 5
./scripts/launch.sh 3
sleep 20
tmux select-window -t 0

if $runNode; then
  # Launcher launching
  echo "Launching launcher"
  sleep 1
  rm -rf update-node-tmp.log
  stack exec cardano-launcher -- --node binaries_v000/cardano-node --node-log-config scripts/update-log-config.yaml -n "--update-server"  -n "http://localhost:$serverPort" -n "--update-latest-path" -n "updateDownloaded.tar" -n "--listen" -n "127.0.0.1:3004" -n "--peer" -n "127.0.0.1:3000/a_P8zb6fNP7I2H54FtGuhqxaMDAwMDAwMDAwMDAwMDA=" -n "--flat-distr" -n "(3,100000)" -n "--rebuild-db" -n "--wallet" -n "--web-port" -n 8090 --updater $updater -u "dir" -u "binaries_v000" --node-timeout 5 --report-server http://localhost:8555/ --update-archive updateDownloaded.tar $wallet_cli &
  echo "Luncher started"
fi

if $build; then
  # Building updater
  cd ../cardano-updater
  echo "Building cardano-updater"
  stack build --fast
  cd $csldir
  echo "Building cardano-sl"
  stack clean cardano-sl
  grep "BlockVersion 0 0 0" src/Pos/Constants.hs  # fails if not found
  stack build --fast 

  csl_bin=$(stack path --local-install-root)/bin
  originalMd5=$(md5sum $csl_bin/cardano-node)
  # Copying artefacts for v0.0.0
  echo "Preparing binaries with 0.0.0"
  rm -rf binaries_v000 && mkdir binaries_v000
  cp -v $csl_bin/* binaries_v000/
  beforeBumpMd5=$(md5sum binaries_v000/cardano-node)
  echo "$beforeBumpMd5"
  
  # Updating version in csl sources to v0.1.0
  sed -i.backup "s/BlockVersion 0 0 0/BlockVersion 0 1 0/" src/Pos/Constants.hs
  echo "Building cardano-sl with version 0.1.0"
  stack build --fast
  rm -rf binaries_v010 && mkdir binaries_v010
  cp -v $csl_bin/* binaries_v010/
  afterBumpMd5=$(md5sum binaries_v010/cardano-node)
  echo "$afterBumpMd5"
  if [ "$beforeBumpMd5" == "$afterBumpMd5" ]; then
      echo "md5 before bump '$beforeBumpMd5' matches '$afterBumpMd5' after bump but should not"
  fi
  
  # Restoring version and binaries
  echo "Restoring binaries"
  mv -v src/Pos/Constants.hs.backup src/Pos/Constants.hs
  cp binaries_v000/* $csl_bin/
  #stack build --fast
  afterRestoreMd5=$(md5sum $csl_bin/cardano-node)
  if [ "$originalMd5" != "$afterRestoreMd5" ]; then
      echo "md5 '$originalMd5' doesn't match '$afterRestoreMd5'"
  fi

  rm -rf $updatetar
  echo "Creating diff tar $updatetar (might take a while)"
  stack exec cardano-genupdate -- binaries_v000 binaries_v010 $updatetar
fi

echo "Launching wallet"
set +e

# Sometimes node hangs, so here's a timeout
walletcmd="propose-update 0 0.1.0 1 20 200000 cardano-1 ${updatetar}"
pkill cardano-wallet
sleep 1
echo "Running: '$walletcmd'"

walletOutputLog='walletOutput.log'
until $(timeout 90 ./scripts/wallet.sh cmd --commands "$walletcmd" -p 0 > $walletOutputLog)
do 
    echo "Wallet exited with non-zero code $?, retrying" 
    cat $walletOutputLog
    pkill cardano-wallet
    sleep 1
done
echo "Exit code of wallet: $?"

cat $walletOutputLog

proposalHash=$(cat $walletOutputLog | grep -i "Update proposal submitted" | cut -d' ' -f 5)
test -z "$proposalHash" && echo "Didn't manage to retrieve proposal hash, aborting"  && exit
echo "Proposal hash is '$proposalHash'"

updateVersion=$(cat $walletOutputLog | grep -i "Read file succesfuly, its hash:" | cut -d' ' -f 6)
test -z "$updateVersion" && echo "Didn't manage to retrieve update version, aborting" && exit
echo "Update version is '$updateVersion'"

rm -rfv $walletOutputLog

# Voting for hash
echo "Running wallet 2 in 10s"
pkill cardano-wallet
sleep 10
until $(timeout 60 ./scripts/wallet.sh cmd --commands "vote 1 y $proposalHash" -p 0 &> /dev/tty)
do 
  echo "Wallet 2 exited with non-zero code $?, retrying"
  pkill cardano-wallet
  sleep 1
done

pkill cardano-wallet
set -e

# Sharing in webfs
echo "Launching webfs server"
rm -rf webfsfolder && mkdir webfsfolder
cp -v $updatetar webfsfolder/$updateVersion
pkill webfsd || true
webfsd -p $serverPort -r webfsfolder 
echo "Launched webfs server"

notify-send "updater scenario: ready"
