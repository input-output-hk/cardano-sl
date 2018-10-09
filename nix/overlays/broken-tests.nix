{ pkgs }:

with pkgs.haskell.lib;
# testsuites which are broken (apparently) in nix.

self: super: {
  filelock = dontCheck super.filelock;
  network-transport-tests = null; #dontCheck super.network-transport-tests;
  network-transport-inmemory = dontCheck super.network-transport-inmemory;
  DRBG = dontCheck super.DRBG;
  rocksdb-haskell-ng = dontCheck super.rocksdb-haskell-ng;
  xmlgen = dontCheck super.xmlgen;
  kademlia = dontCheck super.kademlia;
  math-functions = dontCheck super.math-functions;
  attoparsec = dontCheck super.attoparsec;
  vector-algorithms = dontCheck super.vector-algorithms;
  HTF = dontCheck super.HTF;
  # missing quickcheck-classes
  ip = dontCheck super.ip;

  # doctest
  network = dontCheck super.network;
  distributive = dontCheck super.distributive;
  comonad = dontCheck super.comonad;
  iproute = dontCheck super.iproute;
  systemd = dontCheck super.systemd;
  semigroupoids = dontCheck super.semigroupoids;
  unix-time = dontCheck super.unix-time;
  o-clock = dontCheck super.o-clock;
  lens = dontCheck super.lens;
  http-types = dontCheck super.http-types;
  wai-logger = dontCheck super.wai-logger;
  http-date = dontCheck super.http-date;
  dns = dontCheck super.dns;
  http-api-data = dontCheck super.http-api-data;
  lens-action = dontCheck super.lens-action;
  lens-aeson = dontCheck super.lens-aeson;
  http2 = dontCheck super.http2;
  servant = dontCheck super.servant;
  swagger2 = dontCheck super.swagger2;
  fmt = dontCheck super.fmt;
  universum = dontCheck super.universum;
  ed25519 = dontCheck super.ed25519;
  servant-swagger = dontCheck super.servant-swagger;
  aeson-diff = dontCheck super.aeson-diff;
  trifecta = dontCheck super.trifecta;
  bytes = dontCheck super.bytes;
  heaps = dontCheck super.heaps;
  intervals = dontCheck super.intervals;
  loc = dontCheck super.loc;

  # missing fgl-arbitrary
  graphviz = dontCheck super.graphviz;

  hspec-expectations-pretty-diff = dontCheck super.hspec-expectations-pretty-diff;
  linear = dontCheck super.linear;
  pipes-group = dontCheck super.pipes-group;

  wai-cors = dontCheck super.wai-cors;

  # https://hydra.iohk.io/build/272394/nixlog/1
  mockery = dontCheck super.mockery;

  # Use --quickcheck-replay=923601 to reproduce.
  diagrams-solve = dontCheck super.diagrams-solve;
}
