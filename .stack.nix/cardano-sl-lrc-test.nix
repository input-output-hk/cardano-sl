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
        name = "cardano-sl-lrc-test";
        version = "1.3.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "rupert.horlick@iohk.io";
      author = "Rupert Horlick";
      homepage = "";
      url = "";
      synopsis = "Testing modules for the Cardano SL lrc package";
      description = "Testing modules for the Cardano SL lrc package";
      buildType = "Simple";
    };
    components = {
      "cardano-sl-lrc-test" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-core-test)
          (hsPkgs.cardano-sl-lrc)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.QuickCheck)
          (hsPkgs.reflection)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
        ];
        build-tools = [
          (hsPkgs.buildPackages.cpphs)
        ];
      };
    };
  } // rec { src = ../lrc/test; }