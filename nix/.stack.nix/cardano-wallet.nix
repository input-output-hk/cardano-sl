{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-wallet";
        version = "3.0.2";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "The Wallet Backend for a Cardano node.";
      description = "Please see README.md";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.acid-state)
          (hsPkgs.aeson)
          (hsPkgs.aeson-options)
          (hsPkgs.aeson-pretty)
          (hsPkgs.async)
          (hsPkgs.base58-bytestring)
          (hsPkgs.beam-core)
          (hsPkgs.beam-migrate)
          (hsPkgs.beam-sqlite)
          (hsPkgs.bifunctors)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-sl)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-client)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-core-test)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-mnemonic)
          (hsPkgs.cardano-sl-infra)
          (hsPkgs.cardano-sl-networking)
          (hsPkgs.cardano-sl-node)
          (hsPkgs.cardano-sl-node-ipc)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cardano-sl-utxo)
          (hsPkgs.cardano-sl-x509)
          (hsPkgs.cereal)
          (hsPkgs.clock)
          (hsPkgs.conduit)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.data-default-class)
          (hsPkgs.directory)
          (hsPkgs.exceptions)
          (hsPkgs.filepath)
          (hsPkgs.foldl)
          (hsPkgs.formatting)
          (hsPkgs.formatting)
          (hsPkgs.generics-sop)
          (hsPkgs.http-api-data)
          (hsPkgs.http-client)
          (hsPkgs.http-types)
          (hsPkgs.ixset-typed)
          (hsPkgs.lens)
          (hsPkgs.memory)
          (hsPkgs.mtl)
          (hsPkgs.mwc-random)
          (hsPkgs.neat-interpolation)
          (hsPkgs.optparse-applicative)
          (hsPkgs.QuickCheck)
          (hsPkgs.reflection)
          (hsPkgs.resourcet)
          (hsPkgs.retry)
          (hsPkgs.safe-exceptions)
          (hsPkgs.safecopy)
          (hsPkgs.serokell-util)
          (hsPkgs.servant)
          (hsPkgs.servant-client)
          (hsPkgs.servant-client-core)
          (hsPkgs.servant-server)
          (hsPkgs.servant-swagger)
          (hsPkgs.servant-swagger-ui)
          (hsPkgs.servant-swagger-ui-core)
          (hsPkgs.servant-swagger-ui-redoc)
          (hsPkgs.sqlite-simple)
          (hsPkgs.sqlite-simple-errors)
          (hsPkgs.stm)
          (hsPkgs.stm-chans)
          (hsPkgs.strict)
          (hsPkgs.strict-concurrency)
          (hsPkgs.swagger2)
          (hsPkgs.tar)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.time-units)
          (hsPkgs.tls)
          (hsPkgs.transformers)
          (hsPkgs.universum)
          (hsPkgs.unliftio-core)
          (hsPkgs.unordered-containers)
          (hsPkgs.uuid)
          (hsPkgs.vector)
          (hsPkgs.wai)
          (hsPkgs.wai-middleware-throttle)
          (hsPkgs.warp)
          (hsPkgs.x509)
          (hsPkgs.zlib)
        ];
      };
      exes = {
        "cardano-node" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-wallet)
            (hsPkgs.universum)
          ];
        };
        "cardano-generate-swagger-file" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-wallet)
            (hsPkgs.optparse-applicative)
            (hsPkgs.swagger2)
            (hsPkgs.universum)
          ];
        };
      };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.acid-state)
            (hsPkgs.aeson)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-chain-test)
            (hsPkgs.cardano-sl-client)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-core-test)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-db)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.cardano-sl-mnemonic)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-sl-util-test)
            (hsPkgs.cardano-sl-utxo)
            (hsPkgs.cardano-wallet)
            (hsPkgs.cereal)
            (hsPkgs.conduit)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.data-default)
            (hsPkgs.directory)
            (hsPkgs.formatting)
            (hsPkgs.hedgehog)
            (hsPkgs.hspec)
            (hsPkgs.hspec-core)
            (hsPkgs.insert-ordered-containers)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.normaldistribution)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.random)
            (hsPkgs.safe-exceptions)
            (hsPkgs.safecopy)
            (hsPkgs.serokell-util)
            (hsPkgs.servant)
            (hsPkgs.servant-server)
            (hsPkgs.servant-swagger)
            (hsPkgs.string-conv)
            (hsPkgs.swagger2)
            (hsPkgs.tabl)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.time-units)
            (hsPkgs.universum)
            (hsPkgs.vector)
          ];
        };
        "nightly" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.async)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-wallet)
            (hsPkgs.formatting)
            (hsPkgs.hspec)
            (hsPkgs.hspec-core)
            (hsPkgs.QuickCheck)
            (hsPkgs.safe-exceptions)
            (hsPkgs.serokell-util)
            (hsPkgs.text)
            (hsPkgs.universum)
          ];
        };
        "integration" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.QuickCheck)
            (hsPkgs.servant-client)
            (hsPkgs.aeson)
            (hsPkgs.aeson-qq)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-client)
            (hsPkgs.cardano-sl-cluster)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-mnemonic)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-wallet)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.filepath)
            (hsPkgs.generic-lens)
            (hsPkgs.hspec)
            (hsPkgs.hspec-core)
            (hsPkgs.hspec-expectations-lifted)
            (hsPkgs.http-api-data)
            (hsPkgs.http-client)
            (hsPkgs.http-types)
            (hsPkgs.memory)
            (hsPkgs.optparse-applicative)
            (hsPkgs.servant-client-core)
            (hsPkgs.template-haskell)
            (hsPkgs.text)
            (hsPkgs.universum)
          ];
        };
      };
    };
  } // rec {
    src = .././../wallet;
  }