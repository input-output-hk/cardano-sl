{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {
      benchmarks = false;
    } // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.20";
        identifier = {
          name = "cardano-sl-networking";
          version = "1.3.0";
        };
        license = "MIT";
        copyright = "";
        maintainer = "";
        author = "";
        homepage = "";
        url = "";
        synopsis = "";
        description = "";
        buildType = "Simple";
      };
      components = {
        "cardano-sl-networking" = {
          depends  = [
            hsPkgs.aeson
            hsPkgs.async
            hsPkgs.attoparsec
            hsPkgs.base
            hsPkgs.cardano-sl-util
            hsPkgs.containers
            hsPkgs.cryptonite
            hsPkgs.binary
            hsPkgs.bytestring
            hsPkgs.exceptions
            hsPkgs.formatting
            hsPkgs.hashable
            hsPkgs.kademlia
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.mmorph
            hsPkgs.monad-control
            hsPkgs.mtl
            hsPkgs.network
            hsPkgs.network-transport
            hsPkgs.network-transport-tcp
            hsPkgs.mtl
            hsPkgs.random
            hsPkgs.resourcet
            hsPkgs.transformers-lift
            hsPkgs.universum
            hsPkgs.unliftio-core
            hsPkgs.safe-exceptions
            hsPkgs.serokell-util
            hsPkgs.stm
            hsPkgs.text
            hsPkgs.formatting
            hsPkgs.time
            hsPkgs.time-units
            hsPkgs.transformers
            hsPkgs.transformers-base
            hsPkgs.ekg-core
          ];
        };
        exes = {
          "discovery" = {
            depends  = [
              hsPkgs.base
              hsPkgs.binary
              hsPkgs.bytestring
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-util
              hsPkgs.containers
              hsPkgs.contravariant
              hsPkgs.network-transport
              hsPkgs.network-transport-tcp
              hsPkgs.random
            ];
          };
          "ping-pong" = {
            depends  = [
              hsPkgs.base
              hsPkgs.async
              hsPkgs.binary
              hsPkgs.bytestring
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-util
              hsPkgs.contravariant
              hsPkgs.network-transport
              hsPkgs.network-transport-tcp
              hsPkgs.random
            ];
          };
          "bench-sender" = {
            depends  = [
              hsPkgs.async
              hsPkgs.base
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-util
              hsPkgs.contravariant
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.MonadRandom
              hsPkgs.mtl
              hsPkgs.network-transport
              hsPkgs.network-transport-tcp
              hsPkgs.optparse-simple
              hsPkgs.random
              hsPkgs.safe-exceptions
              hsPkgs.serokell-util
              hsPkgs.time
              hsPkgs.time-units
            ];
          };
          "bench-receiver" = {
            depends  = [
              hsPkgs.base
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-util
              hsPkgs.contravariant
              hsPkgs.network-transport-tcp
              hsPkgs.optparse-simple
              hsPkgs.random
              hsPkgs.safe-exceptions
              hsPkgs.serokell-util
              hsPkgs.text
            ];
          };
          "bench-log-reader" = {
            depends  = [
              hsPkgs.base
              hsPkgs.attoparsec
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-util
              hsPkgs.conduit
              hsPkgs.conduit-extra
              hsPkgs.containers
              hsPkgs.formatting
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.mtl
              hsPkgs.optparse-simple
              hsPkgs.resourcet
              hsPkgs.safe-exceptions
              hsPkgs.text
              hsPkgs.formatting
              hsPkgs.unliftio-core
            ];
          };
        };
        tests = {
          "cardano-sl-networking-test" = {
            depends  = [
              hsPkgs.async
              hsPkgs.base
              hsPkgs.binary
              hsPkgs.bytestring
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-util
              hsPkgs.containers
              hsPkgs.hspec
              hsPkgs.hspec-core
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.mtl
              hsPkgs.network-transport
              hsPkgs.network-transport-tcp
              hsPkgs.network-transport-inmemory
              hsPkgs.QuickCheck
              hsPkgs.random
              hsPkgs.serokell-util
              hsPkgs.stm
              hsPkgs.time-units
            ];
          };
        };
        benchmarks = {
          "qdisc-simulation" = {
            depends  = [
              hsPkgs.base
              hsPkgs.async
              hsPkgs.network-transport-tcp
              hsPkgs.network-transport
              hsPkgs.time-units
              hsPkgs.stm
              hsPkgs.mwc-random
              hsPkgs.statistics
              hsPkgs.vector
              hsPkgs.time
            ];
          };
        };
      };
    } // rec {
      src = ../networking;
    }