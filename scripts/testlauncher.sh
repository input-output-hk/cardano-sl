#!/usr/bin/env bash

# get path to cardano-updater

if [ "$#" -ne 1 ]; then
    echo "Usage: testlauncher.sh <path to cardano-updater>"
    exit 1
fi

updpath=$1

# remove existing directories, etc. and create new ones

rm -rf /tmp/blah-v000 /tmp/blah-v001
rm -f /tmp/blah-update.tar

mkdir /tmp/blah-v000

# create a wallet and a node (v0.0)

echo "
import Control.Concurrent
import Data.Foldable
main = do
  putStrLn \"node v0.0\"
  for_ [1..] $ \i -> do
    threadDelay 5000000
    putStrLn (\"node is still alive, \" ++ show i)
" >> /tmp/blah-v000/node.hs

echo "
import Control.Concurrent
import System.Exit
main = do
  putStrLn \"wallet v0.0\"
  threadDelay 20000000
  putStrLn \"exiting wallet\"
  exitWith (ExitFailure 20)
" >> /tmp/blah-v000/wallet.hs

# create a wallet and a node (v0.1)

cp -r /tmp/blah-v000 /tmp/blah-v001

sed -i 's/v0.0/v0.1/g' /tmp/blah-v001/node.hs
sed -i 's/v0.0/v0.1/g' /tmp/blah-v001/wallet.hs

# compile everything

pushd /tmp/blah-v000
ghc node.hs
ghc wallet.hs
popd

pushd /tmp/blah-v001
ghc node.hs
ghc wallet.hs
popd

# create an update and schedule it to be available later

stack exec cardano-genupdate /tmp/blah-v000 /tmp/blah-v001 /tmp/blah-upd_.tar

(sleep 10 && mv /tmp/blah-upd_.tar /tmp/blah-upd.tar) &

# start the launcher

stack exec cardano-launcher -- \
  --node /tmp/blah-v000/node \
  --node-timeout 5 \
  --wallet /tmp/blah-v000/wallet \
  --updater "$updpath dir /tmp/blah-v000" \
  --update-archive /tmp/blah-upd.tar \
