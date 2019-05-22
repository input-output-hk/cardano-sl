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
        name = "cardano-sl-crypto";
        version = "3.0.2";
      };
      license = "MIT";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - cryptography primitives";
      description = "This package contains cryptography primitives used in Cardano SL.";
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
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cborg)
          (hsPkgs.cereal)
          (hsPkgs.cryptonite)
          (hsPkgs.cryptonite-openssl)
          (hsPkgs.data-default)
          (hsPkgs.formatting)
          (hsPkgs.hashable)
          (hsPkgs.lens)
          (hsPkgs.memory)
          (hsPkgs.mtl)
          (hsPkgs.pvss)
          (hsPkgs.reflection)
          (hsPkgs.safecopy)
          (hsPkgs.safe-exceptions)
          (hsPkgs.scrypt)
          (hsPkgs.serokell-util)
          (hsPkgs.text)
          (hsPkgs.formatting)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
        ];
        build-tools = [
          (hsPkgs.buildPackages.cpphs)
        ];
      };
      tests = {
        "crypto-test" = {
          depends = [
            (hsPkgs.QuickCheck)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-binary-test)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-sl-util-test)
            (hsPkgs.cryptonite)
            (hsPkgs.formatting)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.hedgehog)
            (hsPkgs.hspec)
            (hsPkgs.memory)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.text)
            (hsPkgs.universum)
            (hsPkgs.unordered-containers)
          ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover)
          ];
        };
      };
    };
  } // rec {
    src = .././../crypto;
  }