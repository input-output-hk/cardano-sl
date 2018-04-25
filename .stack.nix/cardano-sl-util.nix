{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-util";
          version = "1.1.0";
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
        cardano-sl-util = {
          depends  = [
            hsPkgs.QuickCheck
            hsPkgs.aeson
            hsPkgs.autoexporter
            hsPkgs.base
            hsPkgs.bytestring
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
            hsPkgs.filepath
            hsPkgs.formatting
            hsPkgs.hashable
            hsPkgs.hspec
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.lrucache
            hsPkgs.megaparsec
            hsPkgs.mtl
            hsPkgs.optparse-applicative
            hsPkgs.parsec
            hsPkgs.process
            hsPkgs.quickcheck-instances
            hsPkgs.reflection
            hsPkgs.resourcet
            hsPkgs.safe-exceptions
            hsPkgs.serokell-util
            hsPkgs.stm
            hsPkgs.tagged
            hsPkgs.template-haskell
            hsPkgs.text
            hsPkgs.text-format
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
          build-tools = [ hsPkgs.cpphs ];
        };
      };
    } // rec { src = ../util; }