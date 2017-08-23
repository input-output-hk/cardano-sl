#!/usr/bin/env bash
set -e
set -o pipefail

# Cleans all data according to docs to prepare for wallets running:
#     https://cardanodocs.com/technical/wallets/

# TODO: support purescript API rebuilding?

echo "Cleaning Explorer db..."

rm -rf wallet-db/
rm -rf db-qanet/
rm -rf wdb-qanet/
rm -rf node-db/
rm -rf node-*.*key*
rm -rf kademlia-abc.dump
rm -rf *.log
rm *key*
rm kademlia.dump
