let
  lib = (import <nixpkgs> {}).lib;
in lib // (rec {
  # fetch nixpkgs and give the expected hash
  fetchNixPkgs = (import <nixpkgs> {}).fetchFromGitHub (builtins.fromJSON (builtins.readFile ./nixpkgs-src.json));
})
