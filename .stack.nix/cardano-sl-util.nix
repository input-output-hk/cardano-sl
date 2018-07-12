{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-util";
          version = "1.3.0";
        };
        license = "MIT";
        copyright = "2016 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - general utilities";
        description = "This package contains utility functions not specific\nto Cardano SL which extend 3rd party libraries or implement\nsomething from scratch.";
        buildType = "Simple";
      };
      components = {
        "cardano-sl-util" = {
          depends  = [
            hsPkgs.aeson
            hsPkgs.base
            hsPkgs.binary
            hsPkgs.cborg
            hsPkgs.cereal
            hsPkgs.containers
            hsPkgs.concurrent-extra
            hsPkgs.contravariant
            hsPkgs.cryptonite
            hsPkgs.data-default
            hsPkgs.deepseq
            hsPkgs.directory
            hsPkgs.ether
            hsPkgs.exceptions
            hsPkgs.file-embed
            hsPkgs.filepath
            hsPkgs.formatting
            hsPkgs.hashable
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.lrucache
            hsPkgs.megaparsec
            hsPkgs.mtl
            hsPkgs.optparse-applicative
            hsPkgs.parsec
            hsPkgs.process
            hsPkgs.reflection
            hsPkgs.resourcet
            hsPkgs.safe-exceptions
            hsPkgs.serokell-util
            hsPkgs.stm
            hsPkgs.tagged
            hsPkgs.template-haskell
            hsPkgs.text
            hsPkgs.formatting
            hsPkgs.th-lift-instances
            hsPkgs.time
            hsPkgs.time-units
            hsPkgs.transformers
            hsPkgs.transformers-base
            hsPkgs.transformers-lift
            hsPkgs.universum
            hsPkgs.unliftio-core
            hsPkgs.unordered-containers
          ];
          build-tools = [
            hsPkgs.buildPackages.cpphs
          ];
        };
        tests = {
          "test" = {
            depends  = [
              hsPkgs.base
              hsPkgs.async
              hsPkgs.cardano-sl-util
              hsPkgs.hspec
              hsPkgs.QuickCheck
              hsPkgs.quickcheck-instances
              hsPkgs.serokell-util
              hsPkgs.stm
              hsPkgs.text
              hsPkgs.time
              hsPkgs.time-units
              hsPkgs.universum
              hsPkgs.unordered-containers
            ];
            build-tools = [
              hsPkgs.buildPackages.cpphs
            ];
          };
        };
      };
    } // rec { src = ../util; }