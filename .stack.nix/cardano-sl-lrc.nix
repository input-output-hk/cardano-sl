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
        name = "cardano-sl-lrc";
        version = "1.3.0";
      };
      license = "MIT";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - Leaders and Richmen computation";
      description = "Cardano SL - Leaders and Richmen computation";
      buildType = "Simple";
    };
    components = {
      "cardano-sl-lrc" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-txp)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.conduit)
          (hsPkgs.ether)
          (hsPkgs.formatting)
          (hsPkgs.lens)
          (hsPkgs.log-warper)
          (hsPkgs.rocksdb-haskell-ng)
          (hsPkgs.formatting)
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
            (hsPkgs.cardano-sl-lrc)
            (hsPkgs.cardano-sl-util-test)
            (hsPkgs.containers)
            (hsPkgs.hspec)
            (hsPkgs.QuickCheck)
            (hsPkgs.universum)
          ];
        };
      };
    };
  } // rec { src = ../lrc; }