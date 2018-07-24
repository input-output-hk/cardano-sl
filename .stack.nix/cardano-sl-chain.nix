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
        name = "cardano-sl-chain";
        version = "1.3.0";
      };
      license = "MIT";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - transaction processing";
      description = "Cardano SL - transaction processing";
      buildType = "Simple";
    };
    components = {
      "cardano-sl-chain" = {
        depends  = [
          (hsPkgs.Cabal)
          (hsPkgs.aeson)
          (hsPkgs.aeson-options)
          (hsPkgs.array)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.conduit)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.deepseq)
          (hsPkgs.ekg-core)
          (hsPkgs.ether)
          (hsPkgs.exceptions)
          (hsPkgs.fmt)
          (hsPkgs.formatting)
          (hsPkgs.free)
          (hsPkgs.hashable)
          (hsPkgs.lens)
          (hsPkgs.log-warper)
          (hsPkgs.lrucache)
          (hsPkgs.memory)
          (hsPkgs.mtl)
          (hsPkgs.mmorph)
          (hsPkgs.mono-traversable)
          (hsPkgs.neat-interpolation)
          (hsPkgs.parsec)
          (hsPkgs.plutus-prototype)
          (hsPkgs.safe-exceptions)
          (hsPkgs.serokell-util)
          (hsPkgs.reflection)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.time-units)
          (hsPkgs.formatting)
          (hsPkgs.transformers)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
        ];
        build-tools = [
          (hsPkgs.buildPackages.cpphs)
        ];
      };
      tests = {
        "test" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-binary-test)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-core-test)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-crypto-test)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-sl-util-test)
            (hsPkgs.containers)
            (hsPkgs.fmt)
            (hsPkgs.formatting)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.hedgehog)
            (hsPkgs.hspec)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.QuickCheck)
            (hsPkgs.random)
            (hsPkgs.serokell-util)
            (hsPkgs.formatting)
            (hsPkgs.universum)
            (hsPkgs.unordered-containers)
            (hsPkgs.vector)
          ];
        };
      };
      benchmarks = {
        "block-bench" = {
          depends  = [
            (hsPkgs.QuickCheck)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.criterion)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-core-test)
            (hsPkgs.cardano-sl-crypto-test)
            (hsPkgs.cardano-sl-util-test)
            (hsPkgs.deepseq)
            (hsPkgs.formatting)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.random)
            (hsPkgs.text)
            (hsPkgs.universum)
            (hsPkgs.unordered-containers)
          ];
        };
      };
    };
  } // rec { src = ../chain; }