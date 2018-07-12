{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-auxx";
          version = "1.3.0";
        };
        license = "MIT";
        copyright = "2016 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - Auxx";
        description = "Cardano SL - Auxx";
        buildType = "Simple";
      };
      components = {
        "cardano-sl-auxx" = {
          depends  = [
            hsPkgs.QuickCheck
            hsPkgs.Earley
            hsPkgs.MonadRandom
            hsPkgs.ansi-wl-pprint
            hsPkgs.async
            hsPkgs.aeson
            hsPkgs.base
            hsPkgs.bytestring
            hsPkgs.canonical-json
            hsPkgs.cardano-sl
            hsPkgs.cardano-sl-block
            hsPkgs.cardano-sl-client
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-core-test
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-db
            hsPkgs.cardano-sl-generator
            hsPkgs.cardano-sl-infra
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-ssc
            hsPkgs.cardano-sl-txp
            hsPkgs.cardano-sl-update
            hsPkgs.cardano-sl-util
            hsPkgs.conduit
            hsPkgs.constraints
            hsPkgs.containers
            hsPkgs.data-default
            hsPkgs.formatting
            hsPkgs.generic-arbitrary
            hsPkgs.haskeline
            hsPkgs.lens
            hsPkgs.loc
            hsPkgs.log-warper
            hsPkgs.megaparsec
            hsPkgs.mtl
            hsPkgs.neat-interpolation
            hsPkgs.optparse-applicative
            hsPkgs.parser-combinators
            hsPkgs.quickcheck-instances
            hsPkgs.random
            hsPkgs.resourcet
            hsPkgs.safe-exceptions
            hsPkgs.scientific
            hsPkgs.serokell-util
            hsPkgs.split
            hsPkgs.stm
            hsPkgs.text
            hsPkgs.formatting
            hsPkgs.time-units
            hsPkgs.transformers
            hsPkgs.universum
            hsPkgs.unordered-containers
            hsPkgs.validation
          ] ++ pkgs.lib.optional (!system.isWindows) hsPkgs.unix;
          build-tools = [
            hsPkgs.buildPackages.cpphs
          ];
        };
        exes = {
          "cardano-auxx" = {
            depends  = [
              hsPkgs.base
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-auxx
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-update
              hsPkgs.log-warper
              hsPkgs.temporary
              hsPkgs.network-transport-tcp
              hsPkgs.safe-exceptions
              hsPkgs.universum
              hsPkgs.formatting
            ] ++ pkgs.lib.optional (!system.isWindows) hsPkgs.unix;
            build-tools = [
              hsPkgs.buildPackages.cpphs
            ];
          };
        };
        tests = {
          "cardano-auxx-test" = {
            depends  = [
              hsPkgs.QuickCheck
              hsPkgs.cardano-sl-auxx
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.hspec
              hsPkgs.universum
            ];
            build-tools = [
              hsPkgs.buildPackages.cpphs
            ];
          };
        };
      };
    } // rec { src = ../auxx; }