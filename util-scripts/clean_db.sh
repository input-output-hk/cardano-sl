# Cleans all data according to docs to prepare for wallets running:
#     https://cardanodocs.com/technical/wallets/

# TODO: support purescript API rebuilding?

rm -rf ./run/*
rm -rf wallet-db/
rm -rf db-abc/
rm -rf node-*.*key*
rm -rf kademlia-abc.dump
