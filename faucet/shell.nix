let
  localLib = import ../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, nixpkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
}:
let
  inherit (nixpkgs) pkgs lib;
  localFaucet = self: super: {
       cardano-sl-faucet = self.callPackage ./. {};
    };
  cardanoPkgs = (import ../. {}).extend(localFaucet);
in
  if pkgs.lib.inNixShell
    then cardanoPkgs.cardano-sl-faucet.env
    else cardanoPkgs.cardano-sl-faucet
