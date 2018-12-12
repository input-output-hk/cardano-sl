{ mkDerivation, aeson, async, base, base64-bytestring, bytestring
, cardano-report-server, cardano-sl-binary, cardano-sl-binary-test
, cardano-sl-chain, cardano-sl-chain-test, cardano-sl-core
, cardano-sl-core-test, cardano-sl-crypto, cardano-sl-crypto-test
, cardano-sl-db, cardano-sl-networking, cardano-sl-util
, cardano-sl-util-test, clock, conduit, containers, cpphs
, directory, dns, ekg-core, ekg-statsd, ekg-wai, ether, exceptions
, filepath, formatting, generic-arbitrary, hashable, hedgehog
, hspec, http-client, http-client-tls, iproute, kademlia, lens
, lzma-conduit, mtl, network-info, network-transport
, network-transport-tcp, optparse-applicative, parsec, QuickCheck
, safe-exceptions, serokell-util, stdenv, stm, tagged, tar, text
, time, time-units, universum, unix, unliftio, unordered-containers
, yaml
}:
mkDerivation {
  pname = "cardano-sl-infra";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base base64-bytestring bytestring cardano-report-server
    cardano-sl-binary cardano-sl-chain cardano-sl-core
    cardano-sl-crypto cardano-sl-db cardano-sl-networking
    cardano-sl-util clock conduit containers directory dns ekg-core
    ekg-statsd ekg-wai ether exceptions filepath formatting hashable
    http-client http-client-tls iproute kademlia lens lzma-conduit mtl
    network-info network-transport network-transport-tcp
    optparse-applicative parsec safe-exceptions serokell-util stm
    tagged tar text time time-units universum unix unliftio
    unordered-containers yaml
  ];
  libraryToolDepends = [ cpphs ];
  testHaskellDepends = [
    async base bytestring cardano-sl-binary-test cardano-sl-chain
    cardano-sl-chain-test cardano-sl-core cardano-sl-core-test
    cardano-sl-crypto cardano-sl-crypto-test cardano-sl-networking
    cardano-sl-util-test containers dns generic-arbitrary hedgehog
    hspec iproute kademlia QuickCheck universum
  ];
  description = "Cardano SL - infrastructural";
  license = stdenv.lib.licenses.mit;
}
