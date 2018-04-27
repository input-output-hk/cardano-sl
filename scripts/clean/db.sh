#!/usr/bin/env bash
set -e
set -o pipefail

# Cleans all data according to docs to prepare for wallets running:
#     https://cardanodocs.com/technical/wallets/

echo "Cleaning Cardano SL db..."

rm -rf run/*
rm -rf wallet-db/
rm -rf node-db/
rm -rf db-testnet-staging/
rm -rf wdb-testnet-staging/
rm -rf db-testnet-public/
rm -rf wdb-testnet-public/
rm -rf db-mainnet/
rm -rf wdb-mainnet/
rm -rf node-*.*key*
rm -rf kademlia-abc.dump
rm -rf kademlia.dump

