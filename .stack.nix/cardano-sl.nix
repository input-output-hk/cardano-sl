{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl";
          version = "1.1.1";
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
        cardano-sl = {
          depends  = [
            hsPkgs.base
            hsPkgs.QuickCheck
            hsPkgs.acid-state
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
            hsPkgs.cereal
            hsPkgs.conduit
            hsPkgs.constraints
            hsPkgs.containers
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
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.mmorph
            hsPkgs.monad-control
            hsPkgs.mtl
            hsPkgs.neat-interpolation
            hsPkgs.optparse-applicative
            hsPkgs.parsec
            hsPkgs.plutus-prototype
            hsPkgs.pvss
            hsPkgs.random
            hsPkgs.resourcet
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
            hsPkgs.tagged
            hsPkgs.template-haskell
            hsPkgs.text
            hsPkgs.time
            hsPkgs.time-units
            hsPkgs.transformers
            hsPkgs.transformers-base
            hsPkgs.transformers-lift
            hsPkgs.universum
            hsPkgs.unliftio
            hsPkgs.unordered-containers
            hsPkgs.wai
            hsPkgs.warp
            hsPkgs.warp-tls
            hsPkgs.yaml
            hsPkgs.cpphs
            hsPkgs.cborg
          ] ++ pkgs.lib.optionals (!system.isWindows) [
            hsPkgs.unix
            hsPkgs.systemd
          ];
          build-tools = [ hsPkgs.cpphs ];
        };
        tests = {
          cardano-test = {
            depends  = [
              hsPkgs.MonadRandom
              hsPkgs.QuickCheck
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.canonical-json
              hsPkgs.cardano-crypto
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-binary
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
              hsPkgs.cborg
              hsPkgs.cereal
              hsPkgs.containers
              hsPkgs.cryptonite
              hsPkgs.data-default
              hsPkgs.extra
              hsPkgs.filelock
              hsPkgs.fmt
              hsPkgs.formatting
              hsPkgs.generic-arbitrary
              hsPkgs.half
              hsPkgs.hspec
              hsPkgs.lens
              hsPkgs.mtl
              hsPkgs.pvss
              hsPkgs.random
              hsPkgs.reflection
              hsPkgs.safecopy
              hsPkgs.serokell-util
              hsPkgs.tagged
              hsPkgs.text
              hsPkgs.time-units
              hsPkgs.universum
              hsPkgs.unordered-containers
              hsPkgs.vector
            ];
            build-tools = [ hsPkgs.cpphs ];
          };
        };
        benchmarks = {
          cardano-bench-criterion = {
            depends  = [
              hsPkgs.QuickCheck
              hsPkgs.aeson
              hsPkgs.base
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-util
              hsPkgs.containers
              hsPkgs.criterion
              hsPkgs.formatting
              hsPkgs.universum
              hsPkgs.vector
            ];
            build-tools = [ hsPkgs.cpphs ];
          };
        };
      };
    } // rec { src = ../lib; }