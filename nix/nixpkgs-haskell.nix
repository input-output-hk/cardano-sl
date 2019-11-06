############################################################################
# Nixpkgs 19.03 with Haskell.nix overlay
#
# This works by importing the base Nixpkgs with extra config and
# overlays provided by Haskell.nix. It will contain patched GHCs
# suitable for cross-compiling to Windows, and the `haskell-nix`
# component builder.
#
# To update the Nixpkgs version, use:
#
#     nix-prefetch-git https://github.com/NixOS/nixpkgs-channels refs/heads/nixpkgs-19.03-darwin | tee nix/nixpkgs-src.json
#
# This will pick the latest revision on the 19.03 branch.
#
# To update the Haskell.nix version, use:
#
#     nix-prefetch-git https://github.com/input-output-hk/haskell.nix | tee nix/haskell-nix-src.json
#
############################################################################

# Arguments to pass when importing nixpkgs.
{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
}:

let
  haskell-nix-src = import ./dep.nix {
    name = "haskell.nix";
    specJSON = ./haskell-nix-src.json;
  };
  nixpkgs = import ./dep.nix {
    name = "nixpkgs";
    specJSON = ./nixpkgs-src.json;
  };

  haskellNixArgs = import haskell-nix-src;

  # Merge config and overlays provided by Haskell.nix into
  # our own nixpkgs args.
  args = haskellNixArgs // {
    inherit system crossSystem;
    config = (haskellNixArgs.config or {}) // config;
    overlays = (haskellNixArgs.overlays or []) ++ overlays;
  };

  overlays = [];
in
  import nixpkgs args
