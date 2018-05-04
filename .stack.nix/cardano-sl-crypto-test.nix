{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-crypto-test";
          version = "1.1.0";
        };
        license = "MIT";
        copyright = "2016 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - arbitrary instances for cardano-sl-crypto";
        description = "This package contains arbitrary instances for the cryptography primitives used in Cardano SL.";
        buildType = "Simple";
      };
      components = {
        cardano-sl-crypto-test = {
          depends  = [
            hsPkgs.QuickCheck
            hsPkgs.base
            hsPkgs.cardano-sl-binary
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-util
            hsPkgs.generic-arbitrary
            hsPkgs.memory
            hsPkgs.quickcheck-instances
            hsPkgs.universum
          ];
        };
      };
    } // rec {
      src = ../crypto/tests;
    }