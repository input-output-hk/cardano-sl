{ system
, compiler
, flags ? {}
, pkgs
, hsPkgs
, pkgconfPkgs }:
  let
    _flags = {} // flags;
  in {
    flags = _flags;
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-sl-delegation-test";
        version = "1.3.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "IOHK <support@iohk.io>";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - delegation (tests)";
      description = "Cardano SL - delegation (tests)";
      buildType = "Simple";
    };
    components = {
      "cardano-sl-delegation-test" = {
        depends  = [
          (hsPkgs.QuickCheck)
          (hsPkgs.base)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-core-test)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-crypto-test)
          (hsPkgs.cardano-sl-delegation)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
        ];
      };
    };
  } // rec {
    src = ../delegation/test;
  }