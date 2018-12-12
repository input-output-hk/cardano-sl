{ mkDerivation, base, cardano-sl-binary, cardano-sl-chain
, cardano-sl-chain-test, cardano-sl-core-test
, cardano-sl-crypto-test, cardano-sl-db, cardano-sl-util-test
, generic-arbitrary, QuickCheck, stdenv, universum
, unordered-containers
}:
mkDerivation {
  pname = "cardano-sl-db-test";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base cardano-sl-binary cardano-sl-chain cardano-sl-chain-test
    cardano-sl-core-test cardano-sl-crypto-test cardano-sl-db
    cardano-sl-util-test generic-arbitrary QuickCheck universum
    unordered-containers
  ];
  description = "Cardano SL - arbitrary instances for cardano-sl-db";
  license = stdenv.lib.licenses.mit;
}
