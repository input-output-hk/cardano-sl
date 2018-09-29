# a random commit from nixos-unstable
# the current nixpkgs-src.json lacks hnix but this one works
with import (builtins.fetchTarball https://github.com/nixos/nixpkgs/archive/083220867c7.tar.gz) {};

let
  hspkgs = haskellPackages.override {
    overrides = super: self: {
      cabal-merger = self.callCabal2nix "cabal-merger" ./. {};
    };
  };
in hspkgs.cabal-merger
