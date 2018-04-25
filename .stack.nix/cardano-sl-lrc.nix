{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-lrc";
          version = "1.1.0";
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
        cardano-sl-lrc = {
          depends  = [
            hsPkgs.QuickCheck
            hsPkgs.base
            hsPkgs.bytestring
            hsPkgs.cardano-sl-binary
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-db
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-util
            hsPkgs.conduit
            hsPkgs.ether
            hsPkgs.formatting
            hsPkgs.generic-arbitrary
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.reflection
            hsPkgs.rocksdb-haskell-ng
            hsPkgs.text-format
            hsPkgs.universum
            hsPkgs.unliftio
            hsPkgs.unordered-containers
          ];
          build-tools = [ hsPkgs.cpphs ];
        };
      };
    } // rec { src = ../lrc; }