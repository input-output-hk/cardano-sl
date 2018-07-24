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
        name = "cardano-sl-binary-test";
        version = "1.3.0";
      };
      license = "MIT";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - binary serializarion (tests)";
      description = "This package contains test helpers for cardano-sl-binary.";
      buildType = "Simple";
    };
    components = {
      "cardano-sl-binary-test" = {
        depends  = [
          (hsPkgs.QuickCheck)
          (hsPkgs.aeson)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-util-test)
          (hsPkgs.cborg)
          (hsPkgs.cereal)
          (hsPkgs.cryptonite)
          (hsPkgs.containers)
          (hsPkgs.directory)
          (hsPkgs.cryptonite)
          (hsPkgs.filepath)
          (hsPkgs.formatting)
          (hsPkgs.half)
          (hsPkgs.hedgehog)
          (hsPkgs.hspec)
          (hsPkgs.mtl)
          (hsPkgs.pretty-show)
          (hsPkgs.quickcheck-instances)
          (hsPkgs.safecopy)
          (hsPkgs.serokell-util)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.universum)
        ];
        build-tools = [
          (hsPkgs.buildPackages.cpphs)
        ];
      };
    };
  } // rec {
    src = ../binary/test;
  }