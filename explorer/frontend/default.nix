if builtins.compareVersions "1.11.7" builtins.nixVersion == 1 then
  abort ''
    This project requires Nix >= 1.11.7, please upgrade:

       curl https://nixos.org/nix/install | sh
  ''
else

with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/cb90e6a0361554d01b7a576af6c6fae4c28d7513.tar.gz) {});

# https://github.com/paf31/purescript-derive-lenses/issues/12
# cabal2nix https://github.com/paf31/purescript-derive-lenses.git > purescript-derive-lenses.nix

let
  hspkgs = pkgs.haskell.packages.ghc802.override {
    overrides = self: super: {
      purescript-derive-lenses = hspkgs.callPackage ./purescript-derive-lenses.nix {};
    };
  };
in stdenv.mkDerivation {
  name = "explorer-bridge";

  buildInputs = with hspkgs; [ nodejs yarn nodePackages.bower purescript purescript-derive-lenses ];

  src = null;
}
