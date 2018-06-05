{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-binary";
          version = "1.1.1";
        };
        license = "MIT";
        copyright = "2016 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - binary serialization";
        description = "This package defines a type class for binary serialization,\nhelpers and instances.";
        buildType = "Simple";
      };
      components = {
        cardano-sl-binary = {
          depends  = [
            hsPkgs.QuickCheck
            hsPkgs.base
            hsPkgs.autoexporter
            hsPkgs.binary
            hsPkgs.bytestring
            hsPkgs.cborg
            hsPkgs.containers
            hsPkgs.digest
            hsPkgs.formatting
            hsPkgs.half
            hsPkgs.hashable
            hsPkgs.lens
            hsPkgs.mtl
            hsPkgs.safe-exceptions
            hsPkgs.serokell-util
            hsPkgs.tagged
            hsPkgs.template-haskell
            hsPkgs.text
            hsPkgs.th-utilities
            hsPkgs.time-units
            hsPkgs.transformers
            hsPkgs.universum
            hsPkgs.unordered-containers
            hsPkgs.vector
          ];
          build-tools = [ hsPkgs.cpphs ];
        };
      };
    } // rec { src = ../binary; }