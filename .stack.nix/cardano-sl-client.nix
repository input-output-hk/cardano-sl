{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-client";
          version = "1.3.0";
        };
        license = "MIT";
        copyright = "2017 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL client modules";
        description = "Cardano SL client modules";
        buildType = "Simple";
      };
      components = {
        "cardano-sl-client" = {
          depends  = [
            hsPkgs.base
            hsPkgs.cardano-sl
            hsPkgs.cardano-sl-block
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-db
            hsPkgs.cardano-sl-infra
            hsPkgs.cardano-sl-lrc
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-txp
            hsPkgs.cardano-sl-update
            hsPkgs.cardano-sl-util
            hsPkgs.containers
            hsPkgs.data-default
            hsPkgs.formatting
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.mtl
            hsPkgs.safe-exceptions
            hsPkgs.serokell-util
            hsPkgs.stm
            hsPkgs.formatting
            hsPkgs.transformers
            hsPkgs.universum
            hsPkgs.unordered-containers
            hsPkgs.vector
            hsPkgs.QuickCheck
          ];
          build-tools = [
            hsPkgs.buildPackages.cpphs
          ];
        };
        tests = {
          "cardano-client-test" = {
            depends  = [
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-crypto-test
              hsPkgs.cardano-sl-db
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-update
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-util-test
              hsPkgs.containers
              hsPkgs.formatting
              hsPkgs.hspec
              hsPkgs.QuickCheck
              hsPkgs.universum
              hsPkgs.unordered-containers
            ];
            build-tools = [
              hsPkgs.buildPackages.cpphs
            ];
          };
        };
      };
    } // rec { src = ../client; }