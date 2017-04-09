#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/..
name=genesis-qanet-`date +%F`

if [[ -d "$DIR/$name" ]]; then
  echo "$name exists" >&2
  exit 1
fi

mkdir $DIR/$name
cd $DIR/$name

utxo_file=$DIR/util-scripts/avvm-files/utxo-dump-last-new.json
blacklist=$DIR/util-scripts/avvm-files/full_blacklist.js

M=5
N=12000

function abc {
  stack exec cardano-keygen -- --genesis-file genesis.bin -f "secrets/secret-{}.key" -m $M -n 12000 --richmen-share 0.94 --testnet-stake 19072918462000000 --utxo-file "$utxo_file" --randcerts --blacklisted "$blacklist" --fake-avvm-seed-pattern "avvm/fake-{}.seed" --fake-avvm-entries 100
  rm secrets/*.lock
  mkdir nodes
  i=1
  while [[ $i -le $M ]]; do
    mv -v secrets/secret-$i.key.primary nodes/key$i
    i=$((i+1))
  done
  cp -v redeemingHolderKey* nodes/key0
}

abc 2>&1 | tee genesis.info

cd ..

rm -f "$name.tgz"
tar -czf "$name.tgz" $name
