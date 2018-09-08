let
  localLib = import ../../../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { })
, buildTools ? with pkgs; [ git nix gnumake ]
}:

with pkgs.lib;
with pkgs;

let
  iohkPkgs = import ../../.. { inherit config system pkgs; };

  cache-s3 = pkgs.callPackage ./cache-s3.nix {};

  stackRebuild = runCommand "stack-rebuild" {} ''
    ${iohkPkgs.ghc.withPackages (ps: [ps.turtle ps.safe ps.transformers])}/bin/ghc -o $out ${./rebuild.hs}
  '';

in
  writeScript "stack-rebuild-wrapped" ''
    #!${stdenv.shell}
    export PATH=${lib.makeBinPath ([ cache-s3 stack gnused coreutils ] ++ buildTools)}
    exec ${stackRebuild} "$@"
  ''
