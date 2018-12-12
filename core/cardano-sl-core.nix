{ mkDerivation, aeson, aeson-options, ansi-terminal, base
, base58-bytestring, bytestring, canonical-json, cardano-crypto
, cardano-report-server, cardano-sl-binary, cardano-sl-binary-test
, cardano-sl-crypto, cardano-sl-crypto-test, cardano-sl-util
, cardano-sl-util-test, cborg, cereal, containers, cpphs
, cryptonite, data-default, deepseq, deriving-compat, ekg-core
, ether, exceptions, formatting, generic-arbitrary, hashable
, hedgehog, hspec, http-api-data, lens, memory, mmorph
, monad-control, mtl, parsec, plutus-prototype, QuickCheck
, quickcheck-instances, random, reflection, resourcet
, safe-exceptions, safecopy, serokell-util, servant, stdenv, stm
, template-haskell, text, time, time-units, transformers
, transformers-base, transformers-lift, universum, unliftio
, unliftio-core, unordered-containers
}:
mkDerivation {
  pname = "cardano-sl-core";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-options ansi-terminal base base58-bytestring bytestring
    canonical-json cardano-report-server cardano-sl-binary
    cardano-sl-crypto cardano-sl-util cborg cereal containers
    cryptonite data-default deepseq deriving-compat ekg-core ether
    exceptions formatting hashable http-api-data lens memory mmorph
    monad-control mtl parsec plutus-prototype random reflection
    resourcet safe-exceptions safecopy serokell-util servant stm
    template-haskell text time time-units transformers
    transformers-base transformers-lift universum unliftio
    unliftio-core unordered-containers
  ];
  libraryToolDepends = [ cpphs ];
  testHaskellDepends = [
    base bytestring cardano-crypto cardano-sl-binary
    cardano-sl-binary-test cardano-sl-crypto cardano-sl-crypto-test
    cardano-sl-util cardano-sl-util-test containers cryptonite
    formatting generic-arbitrary hedgehog hspec QuickCheck
    quickcheck-instances random serokell-util text time-units universum
    unordered-containers
  ];
  testToolDepends = [ cpphs ];
  description = "Cardano SL - core";
  license = stdenv.lib.licenses.mit;
}
