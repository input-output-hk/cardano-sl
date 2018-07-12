{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-txp-test";
          version = "1.3.0";
        };
        license = "MIT";
        copyright = "2018 IOHK";
        maintainer = "rupert.horlick@iohk.io";
        author = "Rupert Horlick";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - arbitrary instances for cardano-sl-txp";
        description = "Cardano SL - arbitrary instances for cardano-sl-txp";
        buildType = "Simple";
      };
      components = {
        "cardano-sl-txp-test" = {
          depends  = [
            hsPkgs.QuickCheck
            hsPkgs.base
            hsPkgs.cardano-sl-binary
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-core-test
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-crypto-test
            hsPkgs.cardano-sl-infra
            hsPkgs.cardano-sl-txp
            hsPkgs.cardano-sl-util-test
            hsPkgs.data-default
            hsPkgs.generic-arbitrary
            hsPkgs.universum
            hsPkgs.vector
          ];
        };
      };
    } // rec { src = ../txp/test; }