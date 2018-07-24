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
        name = "cardano-sl-wallet-test";
        version = "1.3.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "IOHK <support@iohk.io>";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - wallet (Arbitrary instances)";
      description = "QuickCheck Arbitrary instances for the Cardano SL wallet\nfunctionality.";
      buildType = "Simple";
    };
    components = {
      "cardano-sl-wallet-test" = {
        depends  = [
          (hsPkgs.QuickCheck)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl-core-test)
          (hsPkgs.cardano-sl-wallet)
          (hsPkgs.serokell-util)
          (hsPkgs.universum)
        ];
      };
    };
  } // rec {
    src = ../wallet/test;
  }