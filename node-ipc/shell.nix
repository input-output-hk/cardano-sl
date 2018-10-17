with import (import ../nix/fetchNixpkgs.nix (builtins.fromJSON (builtins.readFile ../nixpkgs-src.json))) {};
with import ../. { gitrev = "gitrev"; };
runCommand "dummy" { buildInputs = [ nodejs cardanoPackages.cardano-sl-wallet-new ]; } ''echo only for use with nix-shell''
