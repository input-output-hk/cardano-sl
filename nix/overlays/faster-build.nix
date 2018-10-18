# Disabling optimization for cardano-sl packages will
# return a build ~20% faster (measured in DEVOPS-1032).

{ pkgs }:

with import ../../lib.nix;

self: super: {
  mkDerivation = args: super.mkDerivation (args // optionalAttrs (isCardanoSL args.pname) {
    configureFlags = (args.configureFlags or []) ++ [ "--ghc-options=-O0" ];
  });
}
