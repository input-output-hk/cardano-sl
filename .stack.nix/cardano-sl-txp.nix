{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-txp";
          version = "1.1.0";
        };
        license = "MIT";
        copyright = "2016 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - transaction processing";
        description = "Cardano SL - transaction processing";
        buildType = "Simple";
      };
      components = {
        cardano-sl-txp = {
          depends  = [
            hsPkgs.QuickCheck
            hsPkgs.aeson
            hsPkgs.base
            hsPkgs.bytestring
            hsPkgs.cardano-sl-binary
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-db
            hsPkgs.cardano-sl-infra
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-util
            hsPkgs.conduit
            hsPkgs.containers
            hsPkgs.data-default
            hsPkgs.ekg-core
            hsPkgs.ether
            hsPkgs.exceptions
            hsPkgs.fmt
            hsPkgs.formatting
            hsPkgs.free
            hsPkgs.generic-arbitrary
            hsPkgs.hashable
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.memory
            hsPkgs.mmorph
            hsPkgs.mtl
            hsPkgs.neat-interpolation
            hsPkgs.plutus-prototype
            hsPkgs.resourcet
            hsPkgs.rocksdb-haskell-ng
            hsPkgs.safe-exceptions
            hsPkgs.serokell-util
            hsPkgs.stm
            hsPkgs.reflection
            hsPkgs.tagged
            hsPkgs.template-haskell
            hsPkgs.text
            hsPkgs.text-format
            hsPkgs.transformers
            hsPkgs.universum
            hsPkgs.unliftio
            hsPkgs.unordered-containers
            hsPkgs.vector
          ];
          build-tools = [ hsPkgs.cpphs ];
        };
      };
    } // rec { src = ../txp; }