{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-sl-binary";
        version = "3.0.2";
      };
      license = "MIT";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - binary serialization";
      description = "This package defines a type class for binary serialization,\nhelpers and instances.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.aeson)
          (hsPkgs.base)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.canonical-json)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cborg)
          (hsPkgs.cereal)
          (hsPkgs.containers)
          (hsPkgs.digest)
          (hsPkgs.formatting)
          (hsPkgs.hashable)
          (hsPkgs.lens)
          (hsPkgs.recursion-schemes)
          (hsPkgs.safecopy)
          (hsPkgs.safe-exceptions)
          (hsPkgs.serokell-util)
          (hsPkgs.tagged)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.th-utilities)
          (hsPkgs.time-units)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
        ];
        build-tools = [
          (hsPkgs.buildPackages.cpphs)
        ];
      };
      tests = {
        "binary-test" = {
          depends = [
            (hsPkgs.QuickCheck)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-util-test)
            (hsPkgs.cborg)
            (hsPkgs.cereal)
            (hsPkgs.containers)
            (hsPkgs.formatting)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.half)
            (hsPkgs.hedgehog)
            (hsPkgs.hspec)
            (hsPkgs.mtl)
            (hsPkgs.pretty-show)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.safecopy)
            (hsPkgs.serokell-util)
            (hsPkgs.tagged)
            (hsPkgs.text)
            (hsPkgs.formatting)
            (hsPkgs.time-units)
            (hsPkgs.universum)
            (hsPkgs.unordered-containers)
          ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover)
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
    };
  } // rec {
    src = .././../binary;
  }