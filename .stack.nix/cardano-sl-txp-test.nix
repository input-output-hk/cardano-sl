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
      "cardano-sl-txp-test" = {};
    };
  } // rec { src = ../txp/test; }