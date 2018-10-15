# Running `nix-shell` on this file puts you in an environment where all the
# dependencies needed to work on this package using `Cabal` are available.
#
# Running `nix-build` builds this package.
let
  drv = (import ../. {}).cardano-sl;
in
  if ((import <nixpkgs> {}).stdenv.lib.inNixShell)
    then drv.env
    else drv
