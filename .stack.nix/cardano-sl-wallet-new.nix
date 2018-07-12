{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-wallet-new";
          version = "1.3.0";
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
        "cardano-sl-wallet-new" = {
          depends  = [
            hsPkgs.base
            hsPkgs.QuickCheck
            hsPkgs.acid-state
            hsPkgs.aeson
            hsPkgs.aeson-options
            hsPkgs.aeson-pretty
            hsPkgs.async
            hsPkgs.beam-core
            hsPkgs.beam-migrate
            hsPkgs.beam-sqlite
            hsPkgs.bytestring
            hsPkgs.cardano-sl
            hsPkgs.cardano-sl-block
            hsPkgs.cardano-sl-client
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-core-test
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-db
            hsPkgs.cardano-sl-infra
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-node-ipc
            hsPkgs.cardano-sl-ssc
            hsPkgs.cardano-sl-txp
            hsPkgs.cardano-sl-update
            hsPkgs.cardano-sl-util
            hsPkgs.cardano-sl-wallet
            hsPkgs.conduit
            hsPkgs.connection
            hsPkgs.containers
            hsPkgs.cryptonite
            hsPkgs.data-default
            hsPkgs.data-default-class
            hsPkgs.directory
            hsPkgs.exceptions
            hsPkgs.formatting
            hsPkgs.generics-sop
            hsPkgs.http-api-data
            hsPkgs.http-client
            hsPkgs.http-client-tls
            hsPkgs.http-types
            hsPkgs.ixset-typed
            hsPkgs.json-sop
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.memory
            hsPkgs.mtl
            hsPkgs.neat-interpolation
            hsPkgs.network-transport
            hsPkgs.optparse-applicative
            hsPkgs.QuickCheck
            hsPkgs.reflection
            hsPkgs.safe-exceptions
            hsPkgs.safecopy
            hsPkgs.serokell-util
            hsPkgs.servant
            hsPkgs.servant-client
            hsPkgs.servant-client-core
            hsPkgs.servant-server
            hsPkgs.servant-swagger
            hsPkgs.servant-swagger-ui
            hsPkgs.servant-swagger-ui-core
            hsPkgs.servant-swagger-ui-redoc
            hsPkgs.sqlite-simple
            hsPkgs.sqlite-simple-errors
            hsPkgs.swagger2
            hsPkgs.text
            hsPkgs.formatting
            hsPkgs.time
            hsPkgs.time-units
            hsPkgs.tls
            hsPkgs.transformers
            hsPkgs.universum
            hsPkgs.unliftio
            hsPkgs.unliftio-core
            hsPkgs.unordered-containers
            hsPkgs.vector
            hsPkgs.wai
            hsPkgs.wai-cors
            hsPkgs.warp
            hsPkgs.x509
            hsPkgs.x509-store
          ];
        };
        exes = {
          "cardano-node" = {
            depends  = [
              hsPkgs.base
              hsPkgs.QuickCheck
              hsPkgs.acid-state
              hsPkgs.aeson
              hsPkgs.aeson-pretty
              hsPkgs.bytestring
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-wallet
              hsPkgs.cardano-sl-wallet-new
              hsPkgs.conduit
              hsPkgs.log-warper
              hsPkgs.stm
              hsPkgs.text
              hsPkgs.universum
            ];
          };
          "cardano-generate-swagger-file" = {
            depends  = [
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.aeson
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-wallet-new
              hsPkgs.optparse-applicative
              hsPkgs.swagger2
              hsPkgs.universum
            ];
          };
          "wal-integr-test" = {
            depends  = [
              hsPkgs.base
              hsPkgs.QuickCheck
              hsPkgs.aeson
              hsPkgs.aeson-diff
              hsPkgs.aeson-pretty
              hsPkgs.bytestring
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-wallet
              hsPkgs.cardano-sl-wallet-new
              hsPkgs.containers
              hsPkgs.exceptions
              hsPkgs.formatting
              hsPkgs.hspec
              hsPkgs.http-client
              hsPkgs.http-types
              hsPkgs.lens
              hsPkgs.mtl
              hsPkgs.optparse-applicative
              hsPkgs.pretty-show
              hsPkgs.servant
              hsPkgs.servant-client
              hsPkgs.servant-quickcheck
              hsPkgs.servant-server
              hsPkgs.text
              hsPkgs.formatting
              hsPkgs.universum
              hsPkgs.unordered-containers
              hsPkgs.wai
              hsPkgs.wai-cors
              hsPkgs.wai-extra
              hsPkgs.warp
              hsPkgs.x509
              hsPkgs.x509-store
            ];
          };
        };
        tests = {
          "wallet-unit-tests" = {
            depends  = [
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.acid-state
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-core-test
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-db
              hsPkgs.cardano-sl-delegation
              hsPkgs.cardano-sl-lrc
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-txp-test
              hsPkgs.cardano-sl-update
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-wallet-new
              hsPkgs.constraints
              hsPkgs.containers
              hsPkgs.cryptonite
              hsPkgs.data-default
              hsPkgs.formatting
              hsPkgs.hspec
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.mtl
              hsPkgs.QuickCheck
              hsPkgs.quickcheck-instances
              hsPkgs.safe-exceptions
              hsPkgs.serokell-util
              hsPkgs.tabl
              hsPkgs.text
              hsPkgs.formatting
              hsPkgs.universum
              hsPkgs.unordered-containers
              hsPkgs.vector
              hsPkgs.bytestring
              hsPkgs.conduit
              hsPkgs.directory
              hsPkgs.filepath
              hsPkgs.normaldistribution
              hsPkgs.optparse-applicative
              hsPkgs.random
              hsPkgs.time
            ];
          };
          "wallet-new-specs" = {
            depends  = [
              hsPkgs.base
              hsPkgs.acid-state
              hsPkgs.aeson
              hsPkgs.bytestring
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-util-test
              hsPkgs.cardano-sl-wallet
              hsPkgs.cardano-sl-wallet-new
              hsPkgs.data-default
              hsPkgs.directory
              hsPkgs.directory
              hsPkgs.formatting
              hsPkgs.hspec
              hsPkgs.lens
              hsPkgs.QuickCheck
              hsPkgs.quickcheck-instances
              hsPkgs.safe-exceptions
              hsPkgs.servant
              hsPkgs.servant-server
              hsPkgs.servant-swagger
              hsPkgs.servant-swagger-ui
              hsPkgs.string-conv
              hsPkgs.swagger2
              hsPkgs.text
              hsPkgs.time
              hsPkgs.universum
            ];
          };
        };
        benchmarks = {
          "cardano-sl-wallet-new-bench" = {
            depends  = [
              hsPkgs.aeson
              hsPkgs.aeson-options
              hsPkgs.async
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-wallet
              hsPkgs.cassava
              hsPkgs.connection
              hsPkgs.cryptonite
              hsPkgs.data-default
              hsPkgs.gauge
              hsPkgs.http-client
              hsPkgs.http-client-tls
              hsPkgs.optparse-applicative
              hsPkgs.random
              hsPkgs.servant
              hsPkgs.servant-client
              hsPkgs.servant-client-core
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