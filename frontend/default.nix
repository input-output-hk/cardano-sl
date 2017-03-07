with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/d4787680bcc9c5163eec15756e871044b2220b4e.tar.gz) {});

# cabal2nix https://github.com/paf31/purescript-derive-lenses.git > purescript-derive-lenses.nix

let
  hspkgs = pkgs.haskell.packages.ghc801.override {
    overrides = self: super: {
      purescript = super.purescript_0_10_5;
      purescript-derive-lenses = hspkgs.callPackage ./purescript-derive-lenses.nix {};
    };
  };
in stdenv.mkDerivation {
  name = "daedalus-bridge";

  buildInputs = with hspkgs; [ nodejs nodePackages.bower purescript purescript-derive-lenses ];

  src = null;
}
