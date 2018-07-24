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
        name = "cardano-sl-generator";
        version = "1.3.0";
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
      "cardano-sl-generator" = {
        depends  = [
          (hsPkgs.MonadRandom)
          (hsPkgs.QuickCheck)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-client)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-core-test)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-crypto-test)
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
          (hsPkgs.log-warper)
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
          depends  = [
            (hsPkgs.async)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-db)
            (hsPkgs.cardano-sl-generator)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.cardano-sl-networking)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.containers)
            (hsPkgs.criterion)
            (hsPkgs.cryptonite)
            (hsPkgs.data-default)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.ether)
            (hsPkgs.formatting)
            (hsPkgs.lens)
            (hsPkgs.log-warper)
            (hsPkgs.lrucache)
            (hsPkgs.MonadRandom)
            (hsPkgs.mtl)
            (hsPkgs.optparse-applicative)
            (hsPkgs.QuickCheck)
            (hsPkgs.random)
            (hsPkgs.text)
            (hsPkgs.time-units)
            (hsPkgs.universum)
            (hsPkgs.unliftio-core)
            (hsPkgs.unordered-containers)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
      tests = {
        "cardano-generator-test" = {
          depends  = [
            (hsPkgs.MonadRandom)
            (hsPkgs.QuickCheck)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-chain-test)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-core-test)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-crypto-test)
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
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
      benchmarks = {
        "cardano-sl-verification-bench" = {
          depends  = [
            (hsPkgs.async)
            (hsPkgs.base)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-db)
            (hsPkgs.cardano-sl-generator)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.cardano-sl-networking)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.containers)
            (hsPkgs.criterion)
            (hsPkgs.cryptonite)
            (hsPkgs.data-default)
            (hsPkgs.ether)
            (hsPkgs.lens)
            (hsPkgs.log-warper)
            (hsPkgs.lrucache)
            (hsPkgs.MonadRandom)
            (hsPkgs.mtl)
            (hsPkgs.QuickCheck)
            (hsPkgs.random)
            (hsPkgs.time-units)
            (hsPkgs.universum)
            (hsPkgs.unliftio-core)
            (hsPkgs.unordered-containers)
            (hsPkgs.serokell-util)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
    };
  } // rec { src = ../generator; }