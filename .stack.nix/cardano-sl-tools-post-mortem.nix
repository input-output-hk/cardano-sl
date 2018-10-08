{ system
, compiler
, flags ? {}
, pkgs
, hsPkgs
, pkgconfPkgs }:
  let
    _flags = {
      for-installer = false;
    } // flags;
  in {
    flags = _flags;
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-sl-tools-post-mortem";
        version = "1.3.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - post-mortem tool";
      description = "Cardano SL - post-mortem tool";
      buildType = "Simple";
    };
    components = {
      exes = {
        "cardano-post-mortem" = {
          depends  = pkgs.lib.optionals (!_flags.for-installer) [
            (hsPkgs.Chart)
            (hsPkgs.Chart-diagrams)
            (hsPkgs.MonadRandom)
            (hsPkgs.aeson)
            (hsPkgs.attoparsec)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.containers)
            (hsPkgs.cassava)
            (hsPkgs.directory)
            (hsPkgs.fgl)
            (hsPkgs.filepath)
            (hsPkgs.foldl)
            (hsPkgs.graphviz)
            (hsPkgs.optparse-applicative)
            (hsPkgs.pipes)
            (hsPkgs.pipes-bytestring)
            (hsPkgs.pipes-interleave)
            (hsPkgs.pipes-safe)
            (hsPkgs.process)
            (hsPkgs.random)
            (hsPkgs.text)
            (hsPkgs.time-units)
            (hsPkgs.universum)
          ];
        };
      };
    };
  } // rec {
    src = ../tools/post-mortem;
  }