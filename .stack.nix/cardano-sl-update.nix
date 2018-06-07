{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-update";
          version = "1.1.1";
        };
        license = "MIT";
        copyright = "2016 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - update";
        description = "Cardano SL - update";
        buildType = "Simple";
      };
      components = {
        cardano-sl-update = {
          depends  = [
            hsPkgs.Cabal
            hsPkgs.QuickCheck
            hsPkgs.aeson
            hsPkgs.aeson-options
            hsPkgs.base
            hsPkgs.bytestring
            hsPkgs.cardano-sl-binary
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-crypto-test
            hsPkgs.cardano-sl-db
            hsPkgs.cardano-sl-infra
            hsPkgs.cardano-sl-lrc
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-util
            hsPkgs.conduit
            hsPkgs.containers
            hsPkgs.data-default
            hsPkgs.directory
            hsPkgs.ether
            hsPkgs.exceptions
            hsPkgs.formatting
            hsPkgs.generic-arbitrary
            hsPkgs.hashable
            hsPkgs.http-client
            hsPkgs.http-client-tls
            hsPkgs.http-conduit
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.memory
            hsPkgs.mtl
            hsPkgs.reflection
            hsPkgs.resourcet
            hsPkgs.rocksdb-haskell-ng
            hsPkgs.safe-exceptions
            hsPkgs.serokell-util
            hsPkgs.stm
            hsPkgs.tagged
            hsPkgs.template-haskell
            hsPkgs.text
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
    } // rec { src = ../update; }