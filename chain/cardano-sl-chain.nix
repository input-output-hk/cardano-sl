{ mkDerivation, aeson, aeson-options, array, base
, base16-bytestring, bytestring, Cabal, canonical-json
, cardano-crypto, cardano-sl-binary, cardano-sl-binary-test
, cardano-sl-core, cardano-sl-core-test, cardano-sl-crypto
, cardano-sl-crypto-test, cardano-sl-util, cardano-sl-util-test
, cborg, cereal, conduit, containers, cpphs, criterion, cryptonite
, data-default, deepseq, ekg-core, ether, exceptions, extra
, filepath, fmt, formatting, free, generic-arbitrary, hashable
, hedgehog, hspec, lens, lrucache, memory, mmorph, mono-traversable
, mtl, neat-interpolation, parsec, plutus-prototype, pvss
, QuickCheck, random, reflection, safe-exceptions, safecopy
, serokell-util, stdenv, template-haskell, text, time, time-units
, transformers, universum, unordered-containers, vector
}:
mkDerivation {
  pname = "cardano-sl-chain";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-options array base bytestring Cabal canonical-json
    cardano-sl-binary cardano-sl-core cardano-sl-crypto cardano-sl-util
    cborg cereal conduit containers cryptonite data-default deepseq
    ekg-core ether exceptions extra filepath fmt formatting free
    generic-arbitrary hashable lens lrucache memory mmorph
    mono-traversable mtl neat-interpolation parsec plutus-prototype
    QuickCheck reflection safe-exceptions safecopy serokell-util
    template-haskell text time time-units transformers universum
    unordered-containers
  ];
  libraryToolDepends = [ cpphs ];
  testHaskellDepends = [
    aeson base base16-bytestring bytestring cardano-crypto
    cardano-sl-binary cardano-sl-binary-test cardano-sl-core
    cardano-sl-core-test cardano-sl-crypto cardano-sl-crypto-test
    cardano-sl-util cardano-sl-util-test containers cryptonite
    data-default fmt formatting generic-arbitrary hedgehog hspec lens
    mtl pvss QuickCheck random serokell-util time-units universum
    unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    base bytestring cardano-sl-binary cardano-sl-core
    cardano-sl-core-test cardano-sl-crypto cardano-sl-crypto-test
    cardano-sl-util-test criterion data-default deepseq formatting
    generic-arbitrary QuickCheck random text universum
    unordered-containers vector
  ];
  description = "Cardano SL - transaction processing";
  license = stdenv.lib.licenses.mit;
}
