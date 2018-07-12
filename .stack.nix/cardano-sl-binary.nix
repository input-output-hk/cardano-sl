{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-binary";
          version = "1.3.0";
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
        "cardano-sl-binary" = {
          depends  = [
            hsPkgs.QuickCheck
            hsPkgs.base
            hsPkgs.binary
            hsPkgs.bytestring
            hsPkgs.cardano-sl-util
            hsPkgs.cborg
            hsPkgs.cereal
            hsPkgs.containers
            hsPkgs.digest
            hsPkgs.formatting
            hsPkgs.half
            hsPkgs.hashable
            hsPkgs.lens
            hsPkgs.mtl
            hsPkgs.safecopy
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
          build-tools = [
            hsPkgs.buildPackages.cpphs
          ];
        };
        tests = {
          "test" = {
            depends  = [
              hsPkgs.QuickCheck
              hsPkgs.aeson
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.canonical-json
              hsPkgs.cardano-sl-binary
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-util-test
              hsPkgs.cborg
              hsPkgs.cereal
              hsPkgs.containers
              hsPkgs.cryptonite
              hsPkgs.data-default
              hsPkgs.directory
              hsPkgs.extra
              hsPkgs.filepath
              hsPkgs.filelock
              hsPkgs.file-embed
              hsPkgs.fmt
              hsPkgs.formatting
              hsPkgs.generic-arbitrary
              hsPkgs.half
              hsPkgs.hedgehog
              hsPkgs.hspec
              hsPkgs.lens
              hsPkgs.mtl
              hsPkgs.pretty-show
              hsPkgs.pvss
              hsPkgs.quickcheck-instances
              hsPkgs.random
              hsPkgs.reflection
              hsPkgs.safecopy
              hsPkgs.serokell-util
              hsPkgs.tagged
              hsPkgs.template-haskell
              hsPkgs.text
              hsPkgs.formatting
              hsPkgs.time-units
              hsPkgs.universum
              hsPkgs.unordered-containers
              hsPkgs.vector
            ];
            build-tools = [
              hsPkgs.buildPackages.cpphs
            ];
          };
        };
      };
    } // rec { src = ../binary; }