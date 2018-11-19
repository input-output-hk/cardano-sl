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
        name = "cardano-sl-wallet-new";
        version = "2.0.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-sl/#readme";
      url = "";
      synopsis = "The Wallet Backend for a Cardano node.";
      description = "Please see README.md";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.binary)
          (hsPkgs.acid-state)
          (hsPkgs.aeson)
          (hsPkgs.aeson-options)
          (hsPkgs.aeson-pretty)
          (hsPkgs.async)
          (hsPkgs.base58-bytestring)
          (hsPkgs.basement)
          (hsPkgs.beam-core)
          (hsPkgs.beam-migrate)
          (hsPkgs.beam-sqlite)
          (hsPkgs.bifunctors)
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
          (hsPkgs.cardano-sl-infra)
          (hsPkgs.cardano-sl-mnemonic)
          (hsPkgs.cardano-sl-networking)
          (hsPkgs.cardano-sl-node-ipc)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cardano-sl-utxo)
          (hsPkgs.cardano-sl-x509)
          (hsPkgs.cereal)
          (hsPkgs.clock)
          (hsPkgs.conduit)
          (hsPkgs.connection)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.data-default-class)
          (hsPkgs.directory)
          (hsPkgs.exceptions)
          (hsPkgs.foldl)
          (hsPkgs.formatting)
          (hsPkgs.formatting)
          (hsPkgs.filepath)
          (hsPkgs.generics-sop)
          (hsPkgs.http-api-data)
          (hsPkgs.http-client)
          (hsPkgs.http-client-tls)
          (hsPkgs.http-types)
          (hsPkgs.ixset-typed)
          (hsPkgs.lens)
          (hsPkgs.memory)
          (hsPkgs.mtl)
          (hsPkgs.mwc-random)
          (hsPkgs.neat-interpolation)
          (hsPkgs.optparse-applicative)
          (hsPkgs.QuickCheck)
          (hsPkgs.QuickCheck)
          (hsPkgs.reflection)
          (hsPkgs.resourcet)
          (hsPkgs.retry)
          (hsPkgs.safecopy)
          (hsPkgs.safe-exceptions)
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
          (hsPkgs.vector)
          (hsPkgs.wai)
          (hsPkgs.wai-middleware-throttle)
          (hsPkgs.warp)
          (hsPkgs.x509)
          (hsPkgs.x509-store)
          (hsPkgs.zlib)
        ];
      };
      exes = {
        "cardano-node" = {
          depends  = [
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-sl-wallet-new)
            (hsPkgs.universum)
          ];
        };
        "cardano-generate-swagger-file" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.aeson)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-wallet-new)
            (hsPkgs.optparse-applicative)
            (hsPkgs.swagger2)
            (hsPkgs.universum)
          ];
        };
        "wal-integr-test" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.QuickCheck)
            (hsPkgs.aeson)
            (hsPkgs.aeson-diff)
            (hsPkgs.aeson-pretty)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-mnemonic)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-sl-wallet-new)
            (hsPkgs.containers)
            (hsPkgs.exceptions)
            (hsPkgs.formatting)
            (hsPkgs.hspec)
            (hsPkgs.http-client)
            (hsPkgs.http-types)
            (hsPkgs.lens)
            (hsPkgs.memory)
            (hsPkgs.mtl)
            (hsPkgs.optparse-applicative)
            (hsPkgs.pretty-show)
            (hsPkgs.serokell-util)
            (hsPkgs.servant)
            (hsPkgs.servant-client)
            (hsPkgs.servant-quickcheck)
            (hsPkgs.servant-server)
            (hsPkgs.text)
            (hsPkgs.formatting)
            (hsPkgs.universum)
            (hsPkgs.x509-store)
          ];
        };
      };
      tests = {
        "wallet-unit-tests" = {
          depends  = [
            (hsPkgs.acid-state)
            (hsPkgs.async)
            (hsPkgs.base)
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
            (hsPkgs.cardano-sl-wallet-new)
            (hsPkgs.cereal)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.data-default)
            (hsPkgs.formatting)
            (hsPkgs.hspec)
            (hsPkgs.hspec-core)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.safe-exceptions)
            (hsPkgs.safecopy)
            (hsPkgs.serokell-util)
            (hsPkgs.servant-server)
            (hsPkgs.tabl)
            (hsPkgs.text)
            (hsPkgs.time-units)
            (hsPkgs.formatting)
            (hsPkgs.universum)
            (hsPkgs.vector)
            (hsPkgs.bytestring)
            (hsPkgs.conduit)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.normaldistribution)
            (hsPkgs.optparse-applicative)
            (hsPkgs.random)
            (hsPkgs.time)
          ];
        };
        "wallet-new-specs" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-client)
            (hsPkgs.cardano-sl-client)
            (hsPkgs.cardano-sl-chain-test)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-core-test)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-sl-util-test)
            (hsPkgs.cardano-sl-wallet-new)
            (hsPkgs.cereal)
            (hsPkgs.data-default)
            (hsPkgs.directory)
            (hsPkgs.directory)
            (hsPkgs.formatting)
            (hsPkgs.hedgehog)
            (hsPkgs.hspec)
            (hsPkgs.insert-ordered-containers)
            (hsPkgs.lens)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.safecopy)
            (hsPkgs.safe-exceptions)
            (hsPkgs.servant)
            (hsPkgs.servant-server)
            (hsPkgs.servant-swagger)
            (hsPkgs.string-conv)
            (hsPkgs.swagger2)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.universum)
          ];
        };
      };
    };
  } // rec {
    src = .././../wallet;
  }
