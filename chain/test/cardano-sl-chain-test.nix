{ mkDerivation, aeson, base, base16-bytestring, bytestring
, cardano-crypto, cardano-sl-binary, cardano-sl-binary-test
, cardano-sl-chain, cardano-sl-core, cardano-sl-core-test
, cardano-sl-crypto, cardano-sl-crypto-test, cardano-sl-util
, cardano-sl-util-test, containers, cryptonite, data-default
, formatting, generic-arbitrary, hedgehog, pvss, QuickCheck, random
, reflection, serokell-util, stdenv, time-units, universum
, unordered-containers, vector
}:
mkDerivation {
  pname = "cardano-sl-chain-test";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring cardano-crypto
    cardano-sl-binary cardano-sl-binary-test cardano-sl-chain
    cardano-sl-core cardano-sl-core-test cardano-sl-crypto
    cardano-sl-crypto-test cardano-sl-util cardano-sl-util-test
    containers cryptonite data-default formatting generic-arbitrary
    hedgehog pvss QuickCheck random reflection serokell-util time-units
    universum unordered-containers vector
  ];
  description = "Cardano SL - arbitrary instances for cardano-sl-chain";
  license = stdenv.lib.licenses.mit;
}
