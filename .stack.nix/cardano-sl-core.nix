{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {
      asserts = true;
    } // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-core";
          version = "1.1.0";
        };
        license = "MIT";
        copyright = "2016 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - core";
        description = "Cardano SL - core";
        buildType = "Simple";
      };
      components = {
        cardano-sl-core = {
          depends  = [
            hsPkgs.aeson
            hsPkgs.ansi-terminal
            hsPkgs.autoexporter
            hsPkgs.base
            hsPkgs.base58-bytestring
            hsPkgs.binary
            hsPkgs.bytestring
            hsPkgs.Cabal
            hsPkgs.canonical-json
            hsPkgs.cardano-sl-binary
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-util
            hsPkgs.cborg
            hsPkgs.containers
            hsPkgs.cryptonite
            hsPkgs.data-default
            hsPkgs.deepseq
            hsPkgs.deriving-compat
            hsPkgs.exceptions
            hsPkgs.extra
            hsPkgs.filepath
            hsPkgs.fmt
            hsPkgs.formatting
            hsPkgs.generic-arbitrary
            hsPkgs.hashable
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.memory
            hsPkgs.mtl
            hsPkgs.plutus-prototype
            hsPkgs.quickcheck-instances
            hsPkgs.random
            hsPkgs.reflection
            hsPkgs.safe-exceptions
            hsPkgs.serokell-util
            hsPkgs.template-haskell
            hsPkgs.text
            hsPkgs.text-format
            hsPkgs.th-lift-instances
            hsPkgs.time
            hsPkgs.time-units
            hsPkgs.universum
            hsPkgs.unordered-containers
            hsPkgs.vector
            hsPkgs.QuickCheck
          ];
          build-tools = [ hsPkgs.cpphs ];
        };
      };
    } // rec { src = ../core; }