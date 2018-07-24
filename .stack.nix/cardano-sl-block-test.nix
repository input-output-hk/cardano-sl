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
        name = "cardano-sl-block-test";
        version = "1.3.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "IOHK <support@iohk.io>";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - block processing (tests)";
      description = "QuickCheck Arbitrary instances for Cardano SL block\nprocessing.";
      buildType = "Simple";
    };
    components = {
      "cardano-sl-block-test" = {
        depends  = [
          (hsPkgs.QuickCheck)
          (hsPkgs.base)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-block)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-core-test)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-crypto-test)
          (hsPkgs.cardano-sl-delegation-test)
          (hsPkgs.cardano-sl-ssc-test)
          (hsPkgs.cardano-sl-update-test)
          (hsPkgs.formatting)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.random)
          (hsPkgs.text)
          (hsPkgs.universum)
        ];
      };
    };
  } // rec {
    src = ../block/test;
  }