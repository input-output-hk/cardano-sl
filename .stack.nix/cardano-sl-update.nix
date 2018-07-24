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
        name = "cardano-sl-update";
        version = "1.3.0";
      };
      license = "MIT";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - update";
      description = "Cardano SL - update";
      buildType = "Simple";
    };
    components = {
      "cardano-sl-update" = {
        depends  = [
          (hsPkgs.Cabal)
          (hsPkgs.aeson)
          (hsPkgs.aeson-options)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-lrc)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.conduit)
          (hsPkgs.containers)
          (hsPkgs.data-default)
          (hsPkgs.deepseq)
          (hsPkgs.ether)
          (hsPkgs.exceptions)
          (hsPkgs.formatting)
          (hsPkgs.hashable)
          (hsPkgs.lens)
          (hsPkgs.log-warper)
          (hsPkgs.mtl)
          (hsPkgs.reflection)
          (hsPkgs.resourcet)
          (hsPkgs.rocksdb-haskell-ng)
          (hsPkgs.serokell-util)
          (hsPkgs.stm)
          (hsPkgs.tagged)
          (hsPkgs.time-units)
          (hsPkgs.transformers)
          (hsPkgs.universum)
          (hsPkgs.unliftio)
          (hsPkgs.unordered-containers)
        ];
        build-tools = [
          (hsPkgs.buildPackages.cpphs)
        ];
      };
    };
  } // rec { src = ../update; }