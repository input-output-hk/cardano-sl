#!/bin/sh

items="
https://github.com/serokell/universum.git
https://github.com/serokell/serokell-util.git
https://github.com/serokell/acid-state.git
https://github.com/serokell/log-warper.git
https://github.com/serokell/kademlia.git
https://github.com/serokell/rocksdb-haskell.git
https://github.com/serokell/time-warp-nt.git
https://github.com/thoughtpolice/hs-ed25519.git
https://github.com/serokell/network-transport.git
https://github.com/serokell/network-transport-tcp.git
https://github.com/input-output-hk/cardano-crypto.git
https://github.com/input-output-hk/cardano-report-server.git
https://github.com/input-output-hk/plutus-prototype.git
https://github.com/serokell/engine.io.git
https://github.com/input-output-hk/cardano-sl.git
"

for item in $items
do
  revision=$(git ls-remote $item | grep refs/heads/master | cut -f 1)
  echo $item $revision
done
