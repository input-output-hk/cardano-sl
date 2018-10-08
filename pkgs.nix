{ pkgs ? import <nixpkgs> {}
, localLib
}:
let
  # all packages from hackage as nix expressions
  hackage = import localLib.fetchHackage;
  # a different haskell infrastructure
  haskell = import localLib.fetchHaskell hackage;

  # the set of all stackage snapshots
  stackage = import localLib.fetchStackage { inherit pkgs hackage haskell; };

  # our packages
  stack-pkgs = import ./stack-pkgs.nix;

  # pick the repsective stackage version here
  # and augment them with out packages
  stackPackages = stackage.lts-11_13 {
    extraDeps = hsPkgs: (stack-pkgs.extraDeps hsPkgs
                      // stack-pkgs.packages  hsPkgs); };
in stackPackages
