{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-crypto";
          version = "1.1.0";
        };
        license = "MIT";
        copyright = "2016 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - cryptography primitives";
        description = "This package contains cryptography primitives used in Cardano SL.";
        buildType = "Simple";
      };
      components = {
        cardano-sl-crypto = {
          depends  = [
            hsPkgs.QuickCheck
            hsPkgs.aeson
            hsPkgs.autoexporter
            hsPkgs.base
            hsPkgs.binary
            hsPkgs.bytestring
            hsPkgs.cardano-crypto
            hsPkgs.cardano-sl-binary
            hsPkgs.cardano-sl-util
            hsPkgs.cborg
            hsPkgs.cryptonite
            hsPkgs.cryptonite-openssl
            hsPkgs.data-default
            hsPkgs.ed25519
            hsPkgs.formatting
            hsPkgs.generic-arbitrary
            hsPkgs.hashable
            hsPkgs.lens
            hsPkgs.memory
            hsPkgs.mtl
            hsPkgs.pvss
            hsPkgs.quickcheck-instances
            hsPkgs.reflection
            hsPkgs.safe-exceptions
            hsPkgs.scrypt
            hsPkgs.serokell-util
            hsPkgs.text
            hsPkgs.text-format
            hsPkgs.universum
            hsPkgs.unordered-containers
          ];
          build-tools = [ hsPkgs.cpphs ];
        };
      };
    } // rec { src = ../crypto; }