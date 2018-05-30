with import (import ../fetch-nixpkgs.nix) {};
with import ../. { gitrev = "gitrev"; };
runCommand "dummy" { buildInputs = [ nodejs cardano-sl-wallet-new ]; } ''echo only for use with nix-shell''
