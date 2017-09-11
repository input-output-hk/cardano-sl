#!/bin/bash

set -e

genesis=$1

if [[ ! -d "$genesis" ]]; then
  echo "No genesis"
  exit 1
fi

genesis_=`cd $genesis && pwd`

postvend_app_dir=`pwd`/../postvend-app

{ for i in $genesis/keys-fakeavvm/*.seed; do cat $i; echo ''; done; } > $postvend_app_dir/seeds.txt

DIR=`pwd`

cd $postvend_app_dir

rm -Rf paper-certs-* redeem-certs-*

stack exec postvend-cli -- gen-test-certs --seeds-file seeds.txt

mkdir $genesis_/certs

mv paper-certs-mnem-* $genesis_/certs/paper-certs-mnem
mv paper-certs-* $genesis_/certs/paper-certs
mv redeem-certs-* $genesis_/certs/redeem-certs

cd $DIR

tar -czf `basename $genesis`-certs-secrets.tgz $genesis/certs $genesis/keys-testnet/poor
