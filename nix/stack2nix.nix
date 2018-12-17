{pkgs, fetchFromGitHub }:

let
  hostPkgs = import pkgs.path { config = {}; system = builtins.currentSystem; overlays = []; };
in import (hostPkgs.fetchFromGitHub {
  owner = "avieth";
  repo = "stack2nix";
  rev = "c51db2d31892f7c4e7ff6acebe4504f788c56dca";
  sha256 = "10jcj33sxpq18gxf3zcck5i09b2y4jm6qjggqdlwd9ss86wg3ksb";
}) { inherit pkgs; }
