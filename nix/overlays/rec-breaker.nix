{ pkgs }:

with pkgs.haskell.lib;
# mark packages as dontCheck, that would otherwise lead to recursion.
# note: this would be much less of a problem, if we had components.
#
# This file makes me very sad.
#
self: super: {
  time = dontCheck super.time;
  containers = dontCheck super.containers;
  bytestring = dontCheck super.bytestring;
  binary = dontCheck super.binary;
  # hspec -> HUnit -> call-stack -> nanospec -> hspec
  #   '-------------------^
  call-stack = dontCheck super.call-stack;
  # text -> test-framework -> xml -> text
  test-framework = dontCheck super.test-framework;
  text = dontCheck super.text;

  # crytonite -> tasty -> clock
  tasty = dontCheck super.tasty;
  cryptonite = dontCheck super.cryptonite;
  clock = dontCheck super.clock;

  hspec = dontCheck super.hspec;
  hspec-core = dontCheck super.hspec-core;
  nanospec = dontCheck super.nanospec;

  # bytestring-handle missing
  tar = dontCheck super.tar;

  yaml = dontCheck super.yaml;
  statistics = dontCheck super.statistics;
  conduit = dontCheck super.conduit;
  mono-traversable = dontCheck super.mono-traversable;
  mwc-random = dontCheck super.mwc-random;

  servant-client = dontCheck super.servant-client;
  servant-server = dontCheck super.servant-server;
  system-filepath = dontCheck super.system-filepath;

  vector-builder = dontCheck super.vector-builder;

  http-streams = dontCheck super.http-streams;
}
