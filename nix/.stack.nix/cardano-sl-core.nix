{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = { asserts = true; };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-sl-core";
        version = "3.0.2";
      };
      license = "MIT";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - core";
      description = "Cardano SL - core";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.aeson)
          (hsPkgs.aeson-options)
          (hsPkgs.ansi-terminal)
          (hsPkgs.base)
          (hsPkgs.base58-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.canonical-json)
          (hsPkgs.cardano-report-server)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cborg)
          (hsPkgs.cereal)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.deepseq)
          (hsPkgs.deriving-compat)
          (hsPkgs.ekg-core)
          (hsPkgs.ether)
          (hsPkgs.exceptions)
          (hsPkgs.formatting)
          (hsPkgs.hashable)
          (hsPkgs.http-api-data)
          (hsPkgs.lens)
          (hsPkgs.parsec)
          (hsPkgs.memory)
          (hsPkgs.mmorph)
          (hsPkgs.monad-control)
          (hsPkgs.mtl)
          (hsPkgs.random)
          (hsPkgs.reflection)
          (hsPkgs.resourcet)
          (hsPkgs.safecopy)
          (hsPkgs.safe-exceptions)
          (hsPkgs.safecopy)
          (hsPkgs.serokell-util)
          (hsPkgs.servant)
          (hsPkgs.stm)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.time-units)
          (hsPkgs.transformers)
          (hsPkgs.transformers-base)
          (hsPkgs.transformers-lift)
          (hsPkgs.universum)
          (hsPkgs.unliftio)
          (hsPkgs.unliftio-core)
          (hsPkgs.unordered-containers)
        ];
        build-tools = [
          (hsPkgs.buildPackages.cpphs)
        ];
      };
      tests = {
        "core-test" = {
          depends = [
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
            (hsPkgs.formatting)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.hedgehog)
            (hsPkgs.hspec)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.random)
            (hsPkgs.serokell-util)
            (hsPkgs.text)
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
  } // rec { src = .././../core; }