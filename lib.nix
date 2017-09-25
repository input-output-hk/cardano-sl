let
  # Allow overriding pinned nixpkgs for debugging purposes via cardano_pkgs
  fetchNixPkgs = let try = builtins.tryEval <cardano_pkgs>;
    in if try.success
    then builtins.trace "using host <cardano_pkgs>" try.value
    else import ./fetch-nixpkgs.nix;

  pkgs = import fetchNixPkgs {};
  lib = pkgs.lib;
in lib // (rec {
  inherit fetchNixPkgs;
})
