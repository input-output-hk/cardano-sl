{ mkDerivation, aeson, base, bytestring, cardano-sl
, cardano-sl-binary, cardano-sl-binary-test, cardano-sl-block
, cardano-sl-block-test, cardano-sl-core, cardano-sl-core-test
, cardano-sl-crypto, cardano-sl-crypto-test, cardano-sl-db
, cardano-sl-delegation, cardano-sl-generator, cardano-sl-infra
, cardano-sl-lrc, cardano-sl-networking, cardano-sl-ssc
, cardano-sl-txp, cardano-sl-txp-test, cardano-sl-update
, cardano-sl-util, conduit, containers, cpphs, criterion
, cryptonite, data-default, engine-io, engine-io-wai, ether
, exceptions, formatting, free, generic-arbitrary, hspec
, http-types, lens, log-warper, memory, mmorph, mtl
, optparse-applicative, optparse-simple, purescript-bridge
, QuickCheck, resourcet, rocksdb-haskell-ng, safe-exceptions
, serokell-util, servant, servant-generic, servant-multipart
, servant-server, servant-swagger, socket-io, stdenv, stm, swagger2
, text, text-format, time, time-units, transformers, universum
, unliftio, unordered-containers, vector, wai, wai-cors, wai-extra
, warp, weigh
}:
mkDerivation {
  pname = "cardano-sl-explorer";
  version = "1.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring cardano-sl cardano-sl-binary cardano-sl-block
    cardano-sl-block-test cardano-sl-core cardano-sl-core-test
    cardano-sl-crypto cardano-sl-crypto-test cardano-sl-db
    cardano-sl-delegation cardano-sl-generator cardano-sl-infra
    cardano-sl-lrc cardano-sl-networking cardano-sl-ssc cardano-sl-txp
    cardano-sl-txp-test cardano-sl-update cardano-sl-util conduit
    containers data-default engine-io engine-io-wai ether exceptions
    formatting free generic-arbitrary http-types lens log-warper memory
    mmorph mtl QuickCheck resourcet rocksdb-haskell-ng safe-exceptions
    serokell-util servant servant-generic servant-server socket-io stm
    text text-format time time-units transformers universum unliftio
    unordered-containers vector wai wai-cors wai-extra warp
  ];
  libraryToolDepends = [ cpphs ];
  executableHaskellDepends = [
    aeson base bytestring cardano-sl cardano-sl-core cardano-sl-crypto
    cardano-sl-infra cardano-sl-networking cardano-sl-update
    cardano-sl-util lens log-warper optparse-applicative
    optparse-simple purescript-bridge servant-multipart servant-server
    servant-swagger swagger2 universum
  ];
  executableToolDepends = [ cpphs ];
  testHaskellDepends = [
    base bytestring cardano-sl cardano-sl-binary-test cardano-sl-block
    cardano-sl-block-test cardano-sl-core cardano-sl-crypto
    cardano-sl-txp cardano-sl-util containers cryptonite engine-io
    hspec lens log-warper QuickCheck universum warp
  ];
  testToolDepends = [ cpphs ];
  benchmarkHaskellDepends = [
    base cardano-sl cardano-sl-txp cardano-sl-txp-test criterion
    QuickCheck universum weigh
  ];
  benchmarkToolDepends = [ cpphs ];
  description = "Cardano explorer";
  license = stdenv.lib.licenses.mit;
}
