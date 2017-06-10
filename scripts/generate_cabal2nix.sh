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
https://github.com/input-output-hk/cardano-sl-explorer.git
https://github.com/input-output-hk/cardano-report-server.git
https://github.com/input-output-hk/plutus-prototype.git
https://github.com/serokell/engine.io.git
https://github.com/input-output-hk/cardano-sl.git
"

echo "#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i bash -p pkgs.cabal2nix pkgs.nix-prefetch-scripts
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/464c79ea9f929d1237dbc2df878eedad91767a72.tar.gz

set -xe
"

# echo https://github.com/input-output-hk/cardano-sl.git | sed -n 's#.*/\([^.]*\)\.git#\1#p'

for item in $items
do
  revision=$(git ls-remote $item | grep refs/heads/master | cut -f 1)
  project_name=$(echo $item | sed -n 's#.*/\([^.]*\)\.git#\1#p')
  echo "cabal2nix $item --revision $revision > $project_name.nix"
done