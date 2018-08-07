let
  localLib = import ../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, nixpkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
}:
let
  inherit (nixpkgs) pkgs lib;
  inherit (pkgs) stdenv;
  localExplorer = self: super: {
       explorer = self.callPackage ./. {};
    };
  cardanoPkgs = (import ../. {}).extend(localExplorer);
  frontend = pkgs.callPackage ./frontend { cardano-sl-explorer = cardanoPkgs.explorer; };
in
  nixpkgs.mkShell {
    inputsFrom = [ pkgs.yarn frontend cardanoPkgs.explorer ];
    shellHook = "echo hook";
  }
