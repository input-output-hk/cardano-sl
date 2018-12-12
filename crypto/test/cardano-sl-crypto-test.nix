{ mkDerivation, base, bytestring, cardano-crypto, cardano-sl-binary
, cardano-sl-binary-test, cardano-sl-crypto, cardano-sl-util
, cardano-sl-util-test, cryptonite, generic-arbitrary, hedgehog
, memory, QuickCheck, quickcheck-instances, stdenv, universum
}:
mkDerivation {
  pname = "cardano-sl-crypto-test";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cardano-crypto cardano-sl-binary
    cardano-sl-binary-test cardano-sl-crypto cardano-sl-util
    cardano-sl-util-test cryptonite generic-arbitrary hedgehog memory
    QuickCheck quickcheck-instances universum
  ];
  description = "Cardano SL - arbitrary instances for cardano-sl-crypto";
  license = stdenv.lib.licenses.mit;
}
