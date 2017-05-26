#!/usr/bin/env bash

backend="src/Pos/Wallet/Web/"

# types were already modified, exclude `Api`
mv ${backend}Api.hs ${backend}..
./rename.sh ${backend}
mv ${backend}../Api.hs ${backend}

./rename.sh daedalus
