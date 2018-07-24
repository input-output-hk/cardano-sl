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
        name = "cardano-sl-ssc";
        version = "1.3.0";
      };
      license = "MIT";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - shared seed computation";
      description = "Cardano SL - shared seed computation";
      buildType = "Simple";
    };
    components = {
      "cardano-sl-ssc" = {
        depends  = [
          (hsPkgs.aeson)
          (hsPkgs.aeson-options)
          (hsPkgs.array)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-lrc)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.ether)
          (hsPkgs.exceptions)
          (hsPkgs.formatting)
          (hsPkgs.lens)
          (hsPkgs.log-warper)
          (hsPkgs.memory)
          (hsPkgs.mmorph)
          (hsPkgs.mono-traversable)
          (hsPkgs.mtl)
          (hsPkgs.parsec)
          (hsPkgs.reflection)
          (hsPkgs.rocksdb-haskell-ng)
          (hsPkgs.serokell-util)
          (hsPkgs.stm)
          (hsPkgs.tagged)
          (hsPkgs.text)
          (hsPkgs.formatting)
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
        "test" = {
          depends  = [
            (hsPkgs.QuickCheck)
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-sl-binary-test)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-core-test)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-crypto-test)
            (hsPkgs.cardano-sl-ssc)
            (hsPkgs.cardano-sl-util-test)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.hedgehog)
            (hsPkgs.parsec)
            (hsPkgs.universum)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
    };
  } // rec { src = ../ssc; }