let
  hostPkgs = import <nixpkgs> {};
  lib = (import <nixpkgs> {}).lib;
in lib // (rec {
  # fetch nixpkgs and give the expected hash
  fetchNixpkgsWithNixpkgs = nixpkgs: nixpkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile ./nixpkgs-src.json));
  fetchNixPkgs = if builtins.getEnv "NIX_PATH_LOCKED" == "1"
    then builtins.trace "using host nixpkgs" <nixpkgs>
    else builtins.trace "fetching nixpkgs"   fetchNixpkgsWithNixpkgs hostPkgs;
})
