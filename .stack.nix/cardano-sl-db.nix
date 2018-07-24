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
        name = "cardano-sl-db";
        version = "1.3.0";
      };
      license = "MIT";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - basic DB interfaces";
      description = "Cardano SL - basic DB interfaces";
      buildType = "Simple";
    };
    components = {
      "cardano-sl-db" = {
        depends  = [
          (hsPkgs.aeson)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.concurrent-extra)
          (hsPkgs.conduit)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.directory)
          (hsPkgs.ekg-core)
          (hsPkgs.ether)
          (hsPkgs.filepath)
          (hsPkgs.formatting)
          (hsPkgs.lens)
          (hsPkgs.log-warper)
          (hsPkgs.lrucache)
          (hsPkgs.memory)
          (hsPkgs.mmorph)
          (hsPkgs.mtl)
          (hsPkgs.reflection)
          (hsPkgs.resourcet)
          (hsPkgs.rocksdb-haskell-ng)
          (hsPkgs.safe-exceptions)
          (hsPkgs.serokell-util)
          (hsPkgs.stm)
          (hsPkgs.tagged)
          (hsPkgs.text)
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
  } // rec { src = ../db; }