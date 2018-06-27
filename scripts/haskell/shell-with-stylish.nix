let
 cardanoPkgs = import ../../. {};
 pkgs = import ((import ../../lib.nix).fetchNixPkgs) { };
in pkgs.runCommand "name" { buildInputs = [ cardanoPkgs.stylish-haskell ]; } ""
