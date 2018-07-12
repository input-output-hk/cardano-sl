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
          version = "1.3.0";
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
        "cardano-sl-core" = {
          depends  = [
            hsPkgs.aeson
            hsPkgs.aeson-options
            hsPkgs.ansi-terminal
            hsPkgs.base
            hsPkgs.base58-bytestring
            hsPkgs.bifunctors
            hsPkgs.bytestring
            hsPkgs.Cabal
            hsPkgs.canonical-json
            hsPkgs.cardano-sl-binary
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-util
            hsPkgs.cborg
            hsPkgs.cereal
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
            hsPkgs.hashable
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.memory
            hsPkgs.mtl
            hsPkgs.plutus-prototype
            hsPkgs.random
            hsPkgs.reflection
            hsPkgs.safecopy
            hsPkgs.safe-exceptions
            hsPkgs.serokell-util
            hsPkgs.template-haskell
            hsPkgs.text
            hsPkgs.formatting
            hsPkgs.th-lift-instances
            hsPkgs.time
            hsPkgs.time-units
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
              hsPkgs.base
              hsPkgs.base16-bytestring
              hsPkgs.bytestring
              hsPkgs.cardano-crypto
              hsPkgs.cardano-sl-binary
              hsPkgs.cardano-sl-binary-test
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-crypto-test
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-util-test
              hsPkgs.containers
              hsPkgs.cryptonite
              hsPkgs.deepseq
              hsPkgs.formatting
              hsPkgs.generic-arbitrary
              hsPkgs.hedgehog
              hsPkgs.hspec
              hsPkgs.hedgehog
              hsPkgs.pvss
              hsPkgs.QuickCheck
              hsPkgs.quickcheck-instances
              hsPkgs.random
              hsPkgs.serokell-util
              hsPkgs.text
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
    } // rec { src = ../core; }