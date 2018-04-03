# Running `nix-shell` on this file puts you in an environment where all the
# dependencies needed to work on `wallet-new` using `Cabal` are available.
#
# Running `nix-build` builds the `wallet-new` code.
let
  drv = (import ../. {}).cardano-sl-wallet-new;
in
  if ((import <nixpkgs> {}).stdenv.lib.inNixShell)
    then drv.env
    else drv
