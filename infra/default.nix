# Running `nix-shell` on this file puts you in an environment where all the
# dependencies needed to work on this package using `Cabal` are available.
#
# Running `nix-build` builds this package.
let
  drv = nixpkgs.cardano-sl-infra;
  nixpkgs = import ../. {};
in
  if nixpkgs.pkgs.lib.inNixShell
    then drv.env
    else drv
