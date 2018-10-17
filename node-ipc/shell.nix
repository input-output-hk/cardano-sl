with import (import ../nix/fetch-nixpkgs.nix) {};
with import ../. { gitrev = "gitrev"; };
runCommand "dummy" { buildInputs = [ nodejs cardanoPackages.cardano-sl-wallet-new ]; } ''echo only for use with nix-shell''
