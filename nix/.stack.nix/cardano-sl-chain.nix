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
        name = "cardano-sl-chain";
        version = "3.0.2";
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
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.aeson-options)
          (hsPkgs.array)
          (hsPkgs.base16-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.Cabal)
          (hsPkgs.cardano-crypto)
          (hsPkgs.canonical-json)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cborg)
          (hsPkgs.cereal)
          (hsPkgs.conduit)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.deepseq)
          (hsPkgs.ekg-core)
          (hsPkgs.ether)
          (hsPkgs.exceptions)
          (hsPkgs.extra)
          (hsPkgs.filepath)
          (hsPkgs.fmt)
          (hsPkgs.formatting)
          (hsPkgs.formatting)
          (hsPkgs.free)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.hashable)
          (hsPkgs.lens)
          (hsPkgs.lrucache)
          (hsPkgs.memory)
          (hsPkgs.mmorph)
          (hsPkgs.mono-traversable)
          (hsPkgs.mtl)
          (hsPkgs.neat-interpolation)
          (hsPkgs.parsec)
          (hsPkgs.QuickCheck)
          (hsPkgs.reflection)
          (hsPkgs.safecopy)
          (hsPkgs.safe-exceptions)
          (hsPkgs.serokell-util)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.time-units)
          (hsPkgs.transformers)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
        ];
        build-tools = [
          (hsPkgs.buildPackages.cpphs)
        ];
      };
      tests = {
        "chain-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.base16-bytestring)
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
            (hsPkgs.cryptonite)
            (hsPkgs.data-default)
            (hsPkgs.fmt)
            (hsPkgs.formatting)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.hedgehog)
            (hsPkgs.hspec)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.pvss)
            (hsPkgs.QuickCheck)
            (hsPkgs.random)
            (hsPkgs.serokell-util)
            (hsPkgs.formatting)
            (hsPkgs.time-units)
            (hsPkgs.universum)
            (hsPkgs.unordered-containers)
            (hsPkgs.vector)
          ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover)
          ];
        };
      };
      benchmarks = {
        "block-bench" = {
          depends = [
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
            (hsPkgs.containers)
            (hsPkgs.data-default)
            (hsPkgs.deepseq)
            (hsPkgs.formatting)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.random)
            (hsPkgs.text)
            (hsPkgs.universum)
            (hsPkgs.unordered-containers)
            (hsPkgs.vector)
          ];
        };
      };
    };
  } // rec {
    src = .././../chain;
  }