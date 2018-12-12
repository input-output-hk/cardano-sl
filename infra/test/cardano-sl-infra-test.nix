{ mkDerivation, async, base, bytestring, cardano-sl-binary-test
, cardano-sl-chain, cardano-sl-chain-test, cardano-sl-core
, cardano-sl-core-test, cardano-sl-crypto, cardano-sl-crypto-test
, cardano-sl-infra, cardano-sl-networking, cardano-sl-util-test
, containers, dns, generic-arbitrary, hedgehog, hspec, iproute
, kademlia, QuickCheck, stdenv, universum
}:
mkDerivation {
  pname = "cardano-sl-infra-test";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    async base bytestring cardano-sl-binary-test cardano-sl-chain
    cardano-sl-chain-test cardano-sl-core cardano-sl-core-test
    cardano-sl-crypto cardano-sl-crypto-test cardano-sl-infra
    cardano-sl-networking cardano-sl-util-test containers dns
    generic-arbitrary hedgehog hspec iproute kademlia QuickCheck
    universum
  ];
  description = "Cardano SL - generators for cardano-sl-infra";
  license = stdenv.lib.licenses.mit;
}
