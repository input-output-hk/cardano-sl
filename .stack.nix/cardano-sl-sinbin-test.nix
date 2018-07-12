{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-sinbin-test";
          version = "1.3.0";
        };
        license = "MIT";
        copyright = "2018 IOHK";
        maintainer = "operations@iohk.io";
        author = "IOHK Engineering Team";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - generators for cardano-sl-sinbin";
        description = "This package contains generators for the data types temporarily held in sinbin.";
        buildType = "Simple";
      };
      components = {
        "cardano-sl-sinbin-test" = {
          depends  = [
            hsPkgs.QuickCheck
            hsPkgs.base
            hsPkgs.cardano-sl-core-test
            hsPkgs.cardano-sl-sinbin
            hsPkgs.generic-arbitrary
            hsPkgs.universum
          ];
        };
      };
    } // rec {
      src = ../sinbin/test;
    }