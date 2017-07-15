let
  lib = import ./lib.nix;
  pkgs = import lib.fetchNixPkgs { config={}; };
  cardano = import ./. { inherit pkgs; };
in {
  inherit (cardano) cardano-sl cardano-sl-static;
}
