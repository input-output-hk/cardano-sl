let
  localLib = import ../../../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
}:

let 
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: [ps.turtle ps.universum]);

in
  # This is an environment for running the frontend deps regeneration script.
  pkgs.stdenv.mkDerivation {
    name = "explorer-frontend-shell";
    buildInputs = with pkgs; [ nodePackages.bower2nix ghc coreutils ];
    shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
    src = null;
  }
