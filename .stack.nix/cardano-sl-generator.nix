{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-generator";
          version = "1.3.0";
        };
        license = "MIT";
        copyright = "2017 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - arbitrary data generation";
        description = "Cardano SL - arbitrary data generation";
        buildType = "Simple";
      };
      components = {
        "cardano-sl-generator" = {
          depends  = [
            hsPkgs.MonadRandom
            hsPkgs.QuickCheck
            hsPkgs.base
            hsPkgs.bytestring
            hsPkgs.cardano-sl
            hsPkgs.cardano-sl-block
            hsPkgs.cardano-sl-client
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-core-test
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-crypto-test
            hsPkgs.cardano-sl-db
            hsPkgs.cardano-sl-delegation
            hsPkgs.cardano-sl-infra
            hsPkgs.cardano-sl-lrc
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-ssc
            hsPkgs.cardano-sl-ssc-test
            hsPkgs.cardano-sl-txp
            hsPkgs.cardano-sl-update
            hsPkgs.cardano-sl-util
            hsPkgs.containers
            hsPkgs.cryptonite
            hsPkgs.data-default
            hsPkgs.ether
            hsPkgs.exceptions
            hsPkgs.formatting
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.monad-control
            hsPkgs.random
            hsPkgs.safe-exceptions
            hsPkgs.serokell-util
            hsPkgs.text
            hsPkgs.formatting
            hsPkgs.time-units
            hsPkgs.transformers-base
            hsPkgs.universum
            hsPkgs.unliftio
            hsPkgs.unordered-containers
            hsPkgs.vector
          ];
          build-tools = [
            hsPkgs.buildPackages.cpphs
          ];
        };
        tests = {
          "cardano-generator-test" = {
            depends  = [
              hsPkgs.MonadRandom
              hsPkgs.QuickCheck
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-binary
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-block-test
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-core-test
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-crypto-test
              hsPkgs.cardano-sl-db
              hsPkgs.cardano-sl-delegation
              hsPkgs.cardano-sl-delegation-test
              hsPkgs.cardano-sl-generator
              hsPkgs.cardano-sl-lrc
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-ssc-test
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-txp-test
              hsPkgs.cardano-sl-update
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-util-test
              hsPkgs.data-default
              hsPkgs.formatting
              hsPkgs.hspec
              hsPkgs.lens
              hsPkgs.safe-exceptions
              hsPkgs.serokell-util
              hsPkgs.universum
              hsPkgs.unordered-containers
            ];
            build-tools = [
              hsPkgs.buildPackages.cpphs
            ];
          };
        };
      };
    } // rec { src = ../generator; }