# This is the derivation used by "stack --nix".
# It provides the system dependencies required for a stack build.
{ system ? builtins.currentSystem
, config ? {}
, iohkPkgs ? import ./.. {inherit config system; }
, pkgs ? iohkPkgs.pkgs
}:
with pkgs;

haskell.lib.buildStackProject {
  inherit (haskell.packages.ghc822) ghc;
  name = "cardano-sl-stack-env";

  buildInputs = [
    zlib openssh autoreconfHook openssl
    gmp rocksdb git bsdiff ncurses lzma
    perl bash
  ] ++ (lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [ Cocoa CoreServices libcxx libiconv ]));

  phases = ["nobuildPhase"];
  nobuildPhase = "mkdir -p $out";
}
