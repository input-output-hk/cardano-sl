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
        name = "cardano-sl-core-test";
        version = "1.3.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "IOHK <support@iohk.io>";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - core functionality (tests)";
      description = "QuickCheck Arbitrary instances for the Cardano SL core\nfunctionality.";
      buildType = "Simple";
    };
    components = {
      "cardano-sl-core-test" = {
        depends  = [
          (hsPkgs.QuickCheck)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-binary-test)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-crypto-test)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cardano-sl-util-test)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.hedgehog)
          (hsPkgs.pvss)
          (hsPkgs.quickcheck-instances)
          (hsPkgs.random)
          (hsPkgs.serokell-util)
          (hsPkgs.text)
          (hsPkgs.time-units)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
        ];
      };
    };
  } // rec {
    src = .././../core/test;
  }