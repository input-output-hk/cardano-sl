{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-wallet-new";
          version = "0.1.0.0";
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
        cardano-sl-wallet-new = {
          depends  = [
            hsPkgs.base
            hsPkgs.QuickCheck
            hsPkgs.aeson
            hsPkgs.aeson-pretty
            hsPkgs.bytestring
            hsPkgs.containers
            hsPkgs.data-default
            hsPkgs.exceptions
            hsPkgs.formatting
            hsPkgs.generics-sop
            hsPkgs.http-api-data
            hsPkgs.http-client
            hsPkgs.http-types
            hsPkgs.json-sop
            hsPkgs.ixset-typed
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.memory
            hsPkgs.mtl
            hsPkgs.safe-exceptions
            hsPkgs.servant
            hsPkgs.servant-client
            hsPkgs.servant-client-core
            hsPkgs.servant-quickcheck
            hsPkgs.servant-server
            hsPkgs.servant-swagger-ui
            hsPkgs.string-conv
            hsPkgs.swagger2
            hsPkgs.time
            hsPkgs.time-units
            hsPkgs.text
            hsPkgs.text-format
            hsPkgs.transformers
            hsPkgs.reflection
            hsPkgs.universum
            hsPkgs.unordered-containers
            hsPkgs.vector
            hsPkgs.serokell-util
            hsPkgs.wai
            hsPkgs.cardano-sl-block
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-db
            hsPkgs.cardano-sl-util
            hsPkgs.cardano-sl-wallet
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-txp
            hsPkgs.cardano-sl
            hsPkgs.cardano-sl-client
            hsPkgs.cardano-sl-infra
            hsPkgs.cardano-sl-ssc
            hsPkgs.cardano-sl-update
          ];
        };
        exes = {
          cardano-node = {
            depends  = [
              hsPkgs.base
              hsPkgs.QuickCheck
              hsPkgs.aeson
              hsPkgs.aeson-pretty
              hsPkgs.bytestring
              hsPkgs.conduit
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-update
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-wallet
              hsPkgs.cardano-sl-wallet-new
              hsPkgs.containers
              hsPkgs.exceptions
              hsPkgs.formatting
              hsPkgs.formatting
              hsPkgs.http-types
              hsPkgs.ixset-typed
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.mtl
              hsPkgs.neat-interpolation
              hsPkgs.optparse-applicative
              hsPkgs.safe-exceptions
              hsPkgs.servant
              hsPkgs.servant-server
              hsPkgs.servant-swagger
              hsPkgs.servant-swagger-ui
              hsPkgs.string-conv
              hsPkgs.swagger2
              hsPkgs.text
              hsPkgs.time-units
              hsPkgs.universum
              hsPkgs.wai
              hsPkgs.wai-cors
              hsPkgs.wai-extra
              hsPkgs.warp
            ];
          };
          cardano-integration-test = {
            depends  = [
              hsPkgs.base
              hsPkgs.QuickCheck
              hsPkgs.aeson
              hsPkgs.aeson-pretty
              hsPkgs.bytestring
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-update
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-wallet
              hsPkgs.cardano-sl-wallet-new
              hsPkgs.containers
              hsPkgs.data-default
              hsPkgs.exceptions
              hsPkgs.formatting
              hsPkgs.formatting
              hsPkgs.hspec
              hsPkgs.http-api-data
              hsPkgs.http-types
              hsPkgs.insert-ordered-containers
              hsPkgs.ixset-typed
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.mtl
              hsPkgs.neat-interpolation
              hsPkgs.optparse-applicative
              hsPkgs.servant
              hsPkgs.servant-server
              hsPkgs.servant-swagger
              hsPkgs.serokell-util
              hsPkgs.memory
              hsPkgs.stm
              hsPkgs.string-conv
              hsPkgs.swagger2
              hsPkgs.text
              hsPkgs.text-format
              hsPkgs.time-units
              hsPkgs.universum
              hsPkgs.unordered-containers
              hsPkgs.wai
              hsPkgs.wai-cors
              hsPkgs.wai-extra
              hsPkgs.warp
            ];
          };
        };
        tests = {
          wallet-unit-tests = {
            depends  = [
              hsPkgs.base
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-db
              hsPkgs.cardano-sl-delegation
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-update
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-wallet-new
              hsPkgs.constraints
              hsPkgs.containers
              hsPkgs.data-default
              hsPkgs.formatting
              hsPkgs.hspec
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.mmorph
              hsPkgs.mtl
              hsPkgs.QuickCheck
              hsPkgs.serokell-util
              hsPkgs.text
              hsPkgs.text-format
              hsPkgs.universum
              hsPkgs.unordered-containers
            ];
          };
          wallet-new-specs = {
            depends  = [
              hsPkgs.base
              hsPkgs.QuickCheck
              hsPkgs.aeson
              hsPkgs.aeson-pretty
              hsPkgs.bytestring
              hsPkgs.conduit
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-update
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-wallet
              hsPkgs.cardano-sl-wallet-new
              hsPkgs.containers
              hsPkgs.data-default
              hsPkgs.exceptions
              hsPkgs.formatting
              hsPkgs.hspec
              hsPkgs.http-client
              hsPkgs.http-types
              hsPkgs.ixset-typed
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.neat-interpolation
              hsPkgs.optparse-applicative
              hsPkgs.quickcheck-instances
              hsPkgs.safe-exceptions
              hsPkgs.serokell-util
              hsPkgs.servant
              hsPkgs.servant-quickcheck
              hsPkgs.servant-server
              hsPkgs.servant-swagger
              hsPkgs.string-conv
              hsPkgs.stm
              hsPkgs.swagger2
              hsPkgs.text
              hsPkgs.time
              hsPkgs.time-units
              hsPkgs.universum
            ];
          };
        };
        benchmarks = {
          cardano-sl-wallet-new-bench = {
            depends  = [
              hsPkgs.aeson
              hsPkgs.async
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-wallet
              hsPkgs.cardano-sl-wallet-new
              hsPkgs.cassava
              hsPkgs.connection
              hsPkgs.cryptonite
              hsPkgs.data-default
              hsPkgs.gauge
              hsPkgs.http-client
              hsPkgs.http-client-tls
              hsPkgs.optparse-applicative
              hsPkgs.random
              hsPkgs.QuickCheck
              hsPkgs.semigroups
              hsPkgs.servant
              hsPkgs.servant-client
              hsPkgs.servant-client-core
              hsPkgs.servant-generic
              hsPkgs.text
              hsPkgs.time
              hsPkgs.tls
              hsPkgs.universum
              hsPkgs.vector
              hsPkgs.yaml
            ];
          };
        };
      };
    } // rec {
      src = ../wallet-new;
    }