{pkgs, fetchFromGitHub }:

<<<<<<< HEAD
import (fetchFromGitHub {
  owner = "input-output-hk";
=======
let
  hostPkgs = import pkgs.path { config = {}; system = builtins.currentSystem; overlays = []; };
in import (hostPkgs.fetchFromGitHub {
  owner = "avieth";
>>>>>>> release/2.0.0
  repo = "stack2nix";
  rev = "60c36985f07ab87ed01a8a68b6978aba58c8afbd";
  sha256 = "13swg8wxsvy91gkbqs0j661kk4gz2mhqbjghwhhsjqqpwxp2wlns";
}) { inherit pkgs; }
