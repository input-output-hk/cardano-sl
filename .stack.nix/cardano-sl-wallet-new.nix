{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-wallet-new";
          version = "1.1.1";
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
            hsPkgs.acid-state
            hsPkgs.aeson
            hsPkgs.aeson-options
            hsPkgs.aeson-pretty
            hsPkgs.bytestring
            hsPkgs.cardano-sl
            hsPkgs.cardano-sl-block
            hsPkgs.cardano-sl-client
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-db
            hsPkgs.cardano-sl-infra
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-ssc
            hsPkgs.cardano-sl-txp
            hsPkgs.cardano-sl-update
            hsPkgs.cardano-sl-util
            hsPkgs.cardano-sl-wallet
            hsPkgs.conduit
            hsPkgs.containers
            hsPkgs.data-default
            hsPkgs.exceptions
            hsPkgs.formatting
            hsPkgs.generics-sop
            hsPkgs.http-api-data
            hsPkgs.http-client
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
            hsPkgs.safecopy
            hsPkgs.safe-exceptions
            hsPkgs.serokell-util
            hsPkgs.servant
            hsPkgs.servant-client
            hsPkgs.servant-client-core
            hsPkgs.servant-server
            hsPkgs.servant-swagger
            hsPkgs.servant-swagger-ui
            hsPkgs.string-conv
            hsPkgs.swagger2
            hsPkgs.text
            hsPkgs.time
            hsPkgs.time-units
            hsPkgs.transformers
            hsPkgs.universum
            hsPkgs.unliftio
            hsPkgs.unliftio-core
            hsPkgs.unordered-containers
            hsPkgs.vector
            hsPkgs.wai
            hsPkgs.wai-cors
            hsPkgs.warp
          ];
        };
        exes = {
          cardano-node = {
            depends  = [
              hsPkgs.base
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-wallet
              hsPkgs.cardano-sl-wallet-new
              hsPkgs.log-warper
              hsPkgs.stm
              hsPkgs.text
              hsPkgs.universum
            ];
          };
          cardano-integration-test = {
            depends  = [
              hsPkgs.base
              hsPkgs.aeson
              hsPkgs.aeson-diff
              hsPkgs.aeson-pretty
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
              hsPkgs.QuickCheck
              hsPkgs.servant
              hsPkgs.servant-client
              hsPkgs.servant-quickcheck
              hsPkgs.servant-server
              hsPkgs.text
              hsPkgs.universum
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
              hsPkgs.universum
              hsPkgs.unordered-containers
            ];
          };
          wallet-new-specs = {
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
              hsPkgs.string-conv
              hsPkgs.swagger2
              hsPkgs.text
              hsPkgs.time
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
              hsPkgs.semigroups
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