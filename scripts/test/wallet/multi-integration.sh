#!/usr/bin/env bash

# This script can be run from ./cardano-sl in order to 
# run integration.sh for 100x times. If test integration 
# exited with failure, it will output its log into ./cardano-sl/integration-logs/run-<number>

rm -rf integration-logs
mkdir integration-logs

for i in {1..100}
do
   echo "Running test $i... "
   ./scripts/test/wallet/integration.sh $1 &> integration-logs/run-$i
done
