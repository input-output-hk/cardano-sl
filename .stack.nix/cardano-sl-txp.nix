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
        name = "cardano-sl-txp";
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
      "cardano-sl-txp" = {
        depends  = [
          (hsPkgs.aeson)
          (hsPkgs.aeson-options)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.conduit)
          (hsPkgs.containers)
          (hsPkgs.data-default)
          (hsPkgs.ekg-core)
          (hsPkgs.ether)
          (hsPkgs.exceptions)
          (hsPkgs.fmt)
          (hsPkgs.formatting)
          (hsPkgs.free)
          (hsPkgs.hashable)
          (hsPkgs.lens)
          (hsPkgs.log-warper)
          (hsPkgs.memory)
          (hsPkgs.mmorph)
          (hsPkgs.mtl)
          (hsPkgs.neat-interpolation)
          (hsPkgs.plutus-prototype)
          (hsPkgs.resourcet)
          (hsPkgs.rocksdb-haskell-ng)
          (hsPkgs.safe-exceptions)
          (hsPkgs.serokell-util)
          (hsPkgs.stm)
          (hsPkgs.reflection)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.formatting)
          (hsPkgs.transformers)
          (hsPkgs.universum)
          (hsPkgs.unliftio)
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
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-core-test)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-txp)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-sl-util-test)
            (hsPkgs.containers)
            (hsPkgs.fmt)
            (hsPkgs.hspec)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.QuickCheck)
            (hsPkgs.serokell-util)
            (hsPkgs.formatting)
            (hsPkgs.universum)
            (hsPkgs.unordered-containers)
            (hsPkgs.vector)
          ];
        };
      };
    };
  } // rec { src = ../txp; }