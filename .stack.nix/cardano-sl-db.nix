{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
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
            hsPkgs.base
            hsPkgs.bytestring
            hsPkgs.cardano-sl-binary
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-util
            hsPkgs.concurrent-extra
            hsPkgs.conduit
            hsPkgs.containers
            hsPkgs.data-default
            hsPkgs.directory
            hsPkgs.ether
            hsPkgs.filepath
            hsPkgs.formatting
            hsPkgs.lens
            hsPkgs.memory
            hsPkgs.mtl
            hsPkgs.resourcet
            hsPkgs.rocksdb-haskell-ng
            hsPkgs.serokell-util
            hsPkgs.formatting
            hsPkgs.transformers
            hsPkgs.universum
          ];
          build-tools = [
            hsPkgs.buildPackages.cpphs
          ];
        };
      };
    } // rec { src = ../db; }