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
        name = "cardano-sl-generator";
        version = "3.0.2";
      };
      license = "MIT";
      copyright = "2017 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - arbitrary data generation";
      description = "Cardano SL - arbitrary data generation";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.MonadRandom)
          (hsPkgs.QuickCheck)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-chain-test)
          (hsPkgs.cardano-sl-client)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-core-test)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-infra)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.ether)
          (hsPkgs.exceptions)
          (hsPkgs.formatting)
          (hsPkgs.lens)
          (hsPkgs.monad-control)
          (hsPkgs.random)
          (hsPkgs.safe-exceptions)
          (hsPkgs.serokell-util)
          (hsPkgs.text)
          (hsPkgs.formatting)
          (hsPkgs.time-units)
          (hsPkgs.transformers-base)
          (hsPkgs.universum)
          (hsPkgs.unliftio)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
        ];
        build-tools = [
          (hsPkgs.buildPackages.cpphs)
        ];
      };
      exes = {
        "cardano-sl-verification-bench-exe" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-db)
            (hsPkgs.cardano-sl-generator)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.formatting)
            (hsPkgs.MonadRandom)
            (hsPkgs.optparse-applicative)
            (hsPkgs.random)
            (hsPkgs.text)
            (hsPkgs.time-units)
            (hsPkgs.universum)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
      tests = {
        "cardano-generator-test" = {
          depends = [
            (hsPkgs.MonadRandom)
            (hsPkgs.QuickCheck)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-chain-test)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-db)
            (hsPkgs.cardano-sl-generator)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-sl-util-test)
            (hsPkgs.containers)
            (hsPkgs.data-default)
            (hsPkgs.formatting)
            (hsPkgs.hspec)
            (hsPkgs.lens)
            (hsPkgs.safe-exceptions)
            (hsPkgs.serokell-util)
            (hsPkgs.universum)
            (hsPkgs.unordered-containers)
          ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover)
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
      benchmarks = {
        "cardano-sl-verification-bench" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-chain-test)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-db)
            (hsPkgs.cardano-sl-generator)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.criterion)
            (hsPkgs.MonadRandom)
            (hsPkgs.random)
            (hsPkgs.time-units)
            (hsPkgs.universum)
            (hsPkgs.serokell-util)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
    };
  } // rec {
    src = .././../generator;
  }