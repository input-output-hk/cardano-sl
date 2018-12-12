{ mkDerivation, base, bytestring, cardano-crypto, cardano-sl-binary
, cardano-sl-binary-test, cardano-sl-core, cardano-sl-crypto
, cardano-sl-crypto-test, cardano-sl-util, cardano-sl-util-test
, containers, cryptonite, generic-arbitrary, hedgehog, QuickCheck
, quickcheck-instances, random, serokell-util, stdenv, text
, time-units, universum, unordered-containers
}:
mkDerivation {
  pname = "cardano-sl-core-test";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cardano-crypto cardano-sl-binary
    cardano-sl-binary-test cardano-sl-core cardano-sl-crypto
    cardano-sl-crypto-test cardano-sl-util cardano-sl-util-test
    containers cryptonite generic-arbitrary hedgehog QuickCheck
    quickcheck-instances random serokell-util text time-units universum
    unordered-containers
  ];
  description = "Cardano SL - core functionality (tests)";
  license = stdenv.lib.licenses.mit;
}
