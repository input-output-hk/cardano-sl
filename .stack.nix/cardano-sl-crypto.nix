{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-crypto";
          version = "1.1.1";
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
            hsPkgs.aeson
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
            hsPkgs.hashable
            hsPkgs.lens
            hsPkgs.memory
            hsPkgs.mtl
            hsPkgs.pvss
            hsPkgs.reflection
            hsPkgs.safe-exceptions
            hsPkgs.scrypt
            hsPkgs.serokell-util
            hsPkgs.text
            hsPkgs.universum
            hsPkgs.unordered-containers
          ];
          build-tools = [ hsPkgs.cpphs ];
        };
        tests = {
          test = {
            depends  = [
              hsPkgs.MonadRandom
              hsPkgs.QuickCheck
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.canonical-json
              hsPkgs.cardano-sl-binary
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-util
              hsPkgs.cborg
              hsPkgs.cereal
              hsPkgs.containers
              hsPkgs.cryptonite
              hsPkgs.data-default
              hsPkgs.extra
              hsPkgs.filelock
              hsPkgs.fmt
              hsPkgs.formatting
              hsPkgs.generic-arbitrary
              hsPkgs.half
              hsPkgs.hspec
              hsPkgs.lens
              hsPkgs.memory
              hsPkgs.mtl
              hsPkgs.pvss
              hsPkgs.quickcheck-instances
              hsPkgs.random
              hsPkgs.reflection
              hsPkgs.safecopy
              hsPkgs.serokell-util
              hsPkgs.tagged
              hsPkgs.text
              hsPkgs.text-format
              hsPkgs.time-units
              hsPkgs.universum
              hsPkgs.unordered-containers
              hsPkgs.vector
            ];
            build-tools = [ hsPkgs.cpphs ];
          };
        };
      };
    } // rec { src = ../crypto; }