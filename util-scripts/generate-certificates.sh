#!/bin/bash

set -e

genesis=$1

if [[ ! -d "$genesis" ]]; then
  echo "No genesis"
  exit 1
fi

genesis_=`cd $genesis && pwd`

{ for i in $genesis/avvm/*.seed; do cat $i; echo ''; done; } > ../postvend-app/seeds.txt

DIR=`pwd`

cd ../postvend-app

rm -Rf paper-certs-* redeem-certs-*

stack exec postvend-cli -- gen-test-certs --seeds-file seeds.txt

mkdir $genesis_/certs

mv paper-certs-mnem-* $genesis_/certs/paper-certs-mnem
mv paper-certs-* $genesis_/certs/paper-certs
mv redeem-certs-* $genesis_/certs/redeem-certs

cd $DIR

tar -czf `basename $genesis`-certs-secrets.tgz $genesis/certs $genesis/secrets
