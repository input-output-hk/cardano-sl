{ mkDerivation, aeson, aeson-options, async, attoparsec, base
, binary, bytestring, cardano-sl-chain, cardano-sl-util, containers
, contravariant, ekg-core, formatting, hashable, hspec, hspec-core
, lens, mtl, mwc-random, network, network-transport
, network-transport-inmemory, network-transport-tcp, QuickCheck
, random, safe-exceptions, scientific, serokell-util, statistics
, stdenv, stm, text, these, time, time-units, universum
, unordered-containers, vector
}:
mkDerivation {
  pname = "cardano-sl-networking";
  version = "2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-options async attoparsec base binary bytestring
    cardano-sl-chain cardano-sl-util containers ekg-core formatting
    hashable lens mtl network network-transport
    network-transport-tcp random safe-exceptions scientific stm text
    these time time-units universum unordered-containers
  ];
  executableHaskellDepends = [
    async base binary bytestring cardano-sl-util containers
    contravariant network-transport network-transport-tcp random
  ];
  testHaskellDepends = [
    async base binary bytestring cardano-sl-util containers hspec
    hspec-core lens mtl network-transport network-transport-inmemory
    network-transport-tcp QuickCheck random serokell-util stm
    time-units
  ];
  benchmarkHaskellDepends = [
    async base mwc-random network-transport network-transport-tcp
    statistics stm time time-units vector
  ];
  license = stdenv.lib.licenses.mit;
}
