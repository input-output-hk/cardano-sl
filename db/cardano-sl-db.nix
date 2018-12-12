{ mkDerivation, aeson, base, binary, bytestring, cardano-crypto
, cardano-sl-binary, cardano-sl-chain, cardano-sl-chain-test
, cardano-sl-core, cardano-sl-core-test, cardano-sl-crypto
, cardano-sl-util, cardano-sl-util-test, concurrent-extra, conduit
, containers, cpphs, cryptonite, data-default, directory, ekg-core
, ether, exceptions, filepath, formatting, hedgehog, lens, lrucache
, memory, mmorph, mtl, resourcet, rocksdb-haskell-ng
, safe-exceptions, serokell-util, stdenv, stm, tagged, temporary
, text, time-units, transformers, universum, unliftio
, unordered-containers
}:
mkDerivation {
  pname = "cardano-sl-db";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary bytestring cardano-sl-binary cardano-sl-chain
    cardano-sl-core cardano-sl-crypto cardano-sl-util concurrent-extra
    conduit containers cryptonite data-default directory ekg-core ether
    exceptions filepath formatting lens lrucache memory mmorph mtl
    resourcet rocksdb-haskell-ng safe-exceptions serokell-util stm
    tagged text time-units transformers universum unliftio
    unordered-containers
  ];
  libraryToolDepends = [ cpphs ];
  testHaskellDepends = [
    base cardano-crypto cardano-sl-binary cardano-sl-chain
    cardano-sl-chain-test cardano-sl-core cardano-sl-core-test
    cardano-sl-crypto cardano-sl-util cardano-sl-util-test data-default
    filepath hedgehog lens mtl temporary universum unordered-containers
  ];
  description = "Cardano SL - basic DB interfaces";
  license = stdenv.lib.licenses.mit;
}
