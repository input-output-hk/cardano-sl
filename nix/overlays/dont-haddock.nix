{ pkgs }:

with pkgs.haskell.lib;

self: super: {
  # these are pkgs which don't have any haddock input files.
  # it's actually quite stupid that we fail on that; or even
  # try to build haddocks for packages in the first place without
  # explicitly requesting it.
  bytestring-builder = dontHaddock super.bytestring-builder;
  fail = dontHaddock super.fail;
  nats = dontHaddock super.nats;
}
