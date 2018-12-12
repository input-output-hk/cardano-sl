{ mkDerivation, aeson, aeson-options, ansi-terminal, ansi-wl-pprint
, async, base, bytestring, canonical-json, cardano-crypto
, cardano-sl-binary, cardano-sl-binary-test, cardano-sl-chain
, cardano-sl-chain-test, cardano-sl-core, cardano-sl-core-test
, cardano-sl-crypto, cardano-sl-crypto-test, cardano-sl-db
, cardano-sl-db-test, cardano-sl-infra, cardano-sl-infra-test
, cardano-sl-networking, cardano-sl-util, cardano-sl-util-test
, cborg, conduit, constraints, containers, contravariant, cpphs
, criterion, cryptonite, data-default, deepseq, directory, ekg
, ekg-core, ether, exceptions, extra, filelock, filepath
, formatting, generic-arbitrary, generics-sop, hedgehog, hspec
, http-api-data, http-client, http-client-tls, http-conduit
, http-types, lens, lifted-async, memory, monad-control, mtl
, neat-interpolation, network, network-transport
, network-transport-inmemory, optparse-applicative, parsec, pvss
, QuickCheck, quickcheck-instances, random, reflection
, safe-exceptions, serokell-util, servant, servant-client
, servant-client-core, servant-server, servant-swagger
, servant-swagger-ui, servant-swagger-ui-core
, servant-swagger-ui-redoc, stdenv, stm, streaming-commons
, swagger2, systemd, tagged, template-haskell, text, time
, time-units, tls, transformers, universum, unix, unliftio
, unordered-containers, wai, warp, warp-tls, x509, x509-store
, x509-validation, yaml
}:
mkDerivation {
  pname = "cardano-sl";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-options ansi-terminal ansi-wl-pprint async base
    bytestring canonical-json cardano-sl-binary cardano-sl-binary-test
    cardano-sl-chain cardano-sl-core cardano-sl-crypto
    cardano-sl-crypto-test cardano-sl-db cardano-sl-infra
    cardano-sl-networking cardano-sl-util cborg conduit constraints
    containers contravariant cryptonite data-default directory ekg
    ekg-core ether exceptions filelock filepath formatting generics-sop
    hspec http-api-data http-client http-client-tls http-conduit
    http-types lens lifted-async memory monad-control mtl
    neat-interpolation network network-transport optparse-applicative
    parsec pvss QuickCheck quickcheck-instances random reflection
    safe-exceptions serokell-util servant servant-client
    servant-client-core servant-server servant-swagger
    servant-swagger-ui servant-swagger-ui-core servant-swagger-ui-redoc
    stm streaming-commons swagger2 systemd tagged template-haskell text
    time time-units tls transformers universum unix unliftio
    unordered-containers wai warp warp-tls x509 x509-store
    x509-validation yaml
  ];
  libraryToolDepends = [ cpphs ];
  testHaskellDepends = [
    base bytestring cardano-crypto cardano-sl-binary
    cardano-sl-binary-test cardano-sl-chain cardano-sl-chain-test
    cardano-sl-core cardano-sl-core-test cardano-sl-crypto
    cardano-sl-crypto-test cardano-sl-db cardano-sl-db-test
    cardano-sl-infra cardano-sl-infra-test cardano-sl-networking
    cardano-sl-util cardano-sl-util-test conduit containers cryptonite
    data-default deepseq extra filelock formatting generic-arbitrary
    hedgehog hspec lens network-transport network-transport-inmemory
    pvss QuickCheck random reflection serokell-util tagged text time
    time-units universum unordered-containers
  ];
  testToolDepends = [ cpphs ];
  benchmarkHaskellDepends = [
    base bytestring cardano-sl-chain cardano-sl-chain-test
    cardano-sl-core cardano-sl-core-test cardano-sl-crypto
    cardano-sl-crypto-test cardano-sl-db cardano-sl-infra
    cardano-sl-networking cardano-sl-util cardano-sl-util-test conduit
    criterion deepseq formatting network-transport
    network-transport-inmemory optparse-applicative QuickCheck
    universum
  ];
  benchmarkToolDepends = [ cpphs ];
  description = "Cardano SL main implementation";
  license = stdenv.lib.licenses.mit;
}
