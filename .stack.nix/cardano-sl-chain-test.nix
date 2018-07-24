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
        name = "cardano-sl-chain-test";
        version = "1.3.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "rupert.horlick@iohk.io";
      author = "Rupert Horlick";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - arbitrary instances for cardano-sl-chain";
      description = "Cardano SL - arbitrary instances for cardano-sl-chain";
      buildType = "Simple";
    };
    components = {
      "cardano-sl-chain-test" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-core-test)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-crypto-test)
          (hsPkgs.cardano-sl-util-test)
          (hsPkgs.formatting)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.hedgehog)
          (hsPkgs.QuickCheck)
          (hsPkgs.random)
          (hsPkgs.reflection)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
        ];
      };
    };
  } // rec {
    src = ../chain/test;
  }