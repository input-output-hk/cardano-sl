{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-delegation";
          version = "1.1.1";
        };
        license = "MIT";
        copyright = "2017 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - delegation";
        description = "Cardano SL - delegation";
        buildType = "Simple";
      };
      components = {
        cardano-sl-delegation = {
          depends  = [
            hsPkgs.QuickCheck
            hsPkgs.aeson
            hsPkgs.aeson-options
            hsPkgs.base
            hsPkgs.cardano-sl-binary
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-db
            hsPkgs.cardano-sl-infra
            hsPkgs.cardano-sl-lrc
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-util
            hsPkgs.conduit
            hsPkgs.ether
            hsPkgs.formatting
            hsPkgs.generic-arbitrary
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.lrucache
            hsPkgs.mmorph
            hsPkgs.mtl
            hsPkgs.reflection
            hsPkgs.resourcet
            hsPkgs.safe-exceptions
            hsPkgs.rocksdb-haskell-ng
            hsPkgs.serokell-util
            hsPkgs.time
            hsPkgs.time-units
            hsPkgs.transformers
            hsPkgs.universum
            hsPkgs.unliftio
            hsPkgs.unordered-containers
          ];
          build-tools = [
            hsPkgs.buildPackages.cpphs
          ];
        };
      };
    } // rec {
      src = ../delegation;
    }