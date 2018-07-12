{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-block-bench";
          version = "1.3.0";
        };
        license = "MIT";
        copyright = "2018 IOHK";
        maintainer = "IOHK <support@iohk.io>";
        author = "IOHK";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - block benchmark";
        description = "Cardano SL - block benchmark";
        buildType = "Simple";
      };
      components = {
        benchmarks = {
          "bench-block" = {
            depends  = [
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.criterion
              hsPkgs.cardano-sl-binary
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-block-test
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-core
              hsPkgs.formatting
              hsPkgs.universum
              hsPkgs.deepseq
            ];
          };
        };
      };
    } // rec {
      src = ../block/bench;
    }