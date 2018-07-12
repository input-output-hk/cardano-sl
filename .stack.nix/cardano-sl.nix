{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl";
          version = "1.3.0";
        };
        license = "MIT";
        copyright = "2016 IOHK";
        maintainer = "Serokell <hi@serokell.io>";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL main implementation";
        description = "Please see README.md";
        buildType = "Simple";
      };
      components = {
        "cardano-sl" = {
          depends  = ([
            hsPkgs.base
            hsPkgs.QuickCheck
            hsPkgs.acid-state
            hsPkgs.acid-state-exts
            hsPkgs.async
            hsPkgs.aeson
            hsPkgs.aeson-options
            hsPkgs.async
            hsPkgs.ansi-terminal
            hsPkgs.ansi-wl-pprint
            hsPkgs.bytestring
            hsPkgs.canonical-json
            hsPkgs.cardano-crypto
            hsPkgs.cardano-sl-binary
            hsPkgs.cardano-sl-binary-test
            hsPkgs.cardano-sl-block
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-crypto-test
            hsPkgs.cardano-sl-db
            hsPkgs.cardano-sl-delegation
            hsPkgs.cardano-sl-infra
            hsPkgs.cardano-sl-lrc
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-ssc
            hsPkgs.cardano-sl-txp
            hsPkgs.cardano-sl-update
            hsPkgs.cardano-sl-util
            hsPkgs.cardano-sl-util-test
            hsPkgs.cereal
            hsPkgs.conduit
            hsPkgs.constraints
            hsPkgs.containers
            hsPkgs.contravariant
            hsPkgs.cryptonite
            hsPkgs.data-default
            hsPkgs.directory
            hsPkgs.ed25519
            hsPkgs.ekg-core
            hsPkgs.ether
            hsPkgs.exceptions
            hsPkgs.filelock
            hsPkgs.filepath
            hsPkgs.formatting
            hsPkgs.generic-arbitrary
            hsPkgs.hashable
            hsPkgs.hspec
            hsPkgs.http-client
            hsPkgs.http-client-tls
            hsPkgs.http-conduit
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.memory
            hsPkgs.mmorph
            hsPkgs.monad-control
            hsPkgs.mtl
            hsPkgs.neat-interpolation
            hsPkgs.network
            hsPkgs.network-transport
            hsPkgs.optparse-applicative
            hsPkgs.parsec
            hsPkgs.pipes
            hsPkgs.plutus-prototype
            hsPkgs.pvss
            hsPkgs.random
            hsPkgs.reflection
            hsPkgs.safe-exceptions
            hsPkgs.safecopy
            hsPkgs.serokell-util
            hsPkgs.servant
            hsPkgs.servant-client
            hsPkgs.servant-client-core
            hsPkgs.servant-server
            hsPkgs.servant-swagger
            hsPkgs.stm
            hsPkgs.streaming-commons
            hsPkgs.tagged
            hsPkgs.template-haskell
            hsPkgs.text
            hsPkgs.formatting
            hsPkgs.time
            hsPkgs.time-units
            hsPkgs.tls
            hsPkgs.transformers
            hsPkgs.universum
            hsPkgs.unliftio
            hsPkgs.unordered-containers
            hsPkgs.wai
            hsPkgs.warp
            hsPkgs.warp-tls
            hsPkgs.x509
            hsPkgs.x509-store
            hsPkgs.x509-validation
            hsPkgs.yaml
            hsPkgs.cpphs
            hsPkgs.cborg
          ] ++ pkgs.lib.optional (!system.isWindows) hsPkgs.unix) ++ pkgs.lib.optional (!system.isWindows && !system.isFreebsd) hsPkgs.systemd;
          build-tools = [
            hsPkgs.buildPackages.cpphs
          ];
        };
        tests = {
          "cardano-test" = {
            depends  = [
              hsPkgs.QuickCheck
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.cardano-crypto
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-binary
              hsPkgs.cardano-sl-binary-test
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-block-test
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-core-test
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-crypto-test
              hsPkgs.cardano-sl-db
              hsPkgs.cardano-sl-delegation
              hsPkgs.cardano-sl-delegation-test
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-infra-test
              hsPkgs.cardano-sl-lrc
              hsPkgs.cardano-sl-lrc-test
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-ssc-test
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-txp-test
              hsPkgs.cardano-sl-update
              hsPkgs.cardano-sl-update-test
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-util-test
              hsPkgs.cborg
              hsPkgs.containers
              hsPkgs.cryptonite
              hsPkgs.data-default
              hsPkgs.deepseq
              hsPkgs.extra
              hsPkgs.filelock
              hsPkgs.fmt
              hsPkgs.formatting
              hsPkgs.generic-arbitrary
              hsPkgs.hspec
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.mtl
              hsPkgs.network-transport
              hsPkgs.network-transport-inmemory
              hsPkgs.pipes
              hsPkgs.pvss
              hsPkgs.random
              hsPkgs.reflection
              hsPkgs.safecopy
              hsPkgs.serokell-util
              hsPkgs.stm
              hsPkgs.tagged
              hsPkgs.text
              hsPkgs.formatting
              hsPkgs.time
              hsPkgs.time-units
              hsPkgs.universum
              hsPkgs.unordered-containers
              hsPkgs.vector
            ];
            build-tools = [
              hsPkgs.buildPackages.cpphs
            ];
          };
        };
        benchmarks = {
          "cardano-bench-criterion" = {
            depends  = [
              hsPkgs.QuickCheck
              hsPkgs.aeson
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-block-test
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-core-test
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-crypto-test
              hsPkgs.cardano-sl-db
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-lrc
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-txp-test
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-util-test
              hsPkgs.criterion
              hsPkgs.deepseq
              hsPkgs.formatting
              hsPkgs.log-warper
              hsPkgs.network
              hsPkgs.network-transport
              hsPkgs.network-transport-tcp
              hsPkgs.optparse-applicative
              hsPkgs.pipes
              hsPkgs.stm
              hsPkgs.time-units
              hsPkgs.transformers
              hsPkgs.universum
              hsPkgs.vector
            ];
            build-tools = [
              hsPkgs.buildPackages.cpphs
            ];
          };
        };
      };
    } // rec { src = ../lib; }