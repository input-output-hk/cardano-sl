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
          version = "1.1.1";
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
        cardano-sl-networking = {
          depends  = [
            hsPkgs.aeson
            hsPkgs.async
            hsPkgs.attoparsec
            hsPkgs.base
            hsPkgs.cardano-sl-util
            hsPkgs.containers
            hsPkgs.contravariant
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
            hsPkgs.QuickCheck
            hsPkgs.random
            hsPkgs.resourcet
            hsPkgs.transformers-lift
            hsPkgs.universum
            hsPkgs.unliftio-core
            hsPkgs.safe-exceptions
            hsPkgs.serokell-util
            hsPkgs.stm
            hsPkgs.text
            hsPkgs.time
            hsPkgs.time-units
            hsPkgs.transformers
            hsPkgs.transformers-base
            hsPkgs.ekg-core
          ];
        };
        exes = {
          discovery = {
            depends  = [
              hsPkgs.base
              hsPkgs.binary
              hsPkgs.bytestring
              hsPkgs.cardano-sl-networking
              hsPkgs.containers
              hsPkgs.network-transport-tcp
              hsPkgs.random
              hsPkgs.safe-exceptions
              hsPkgs.time-units
            ];
          };
          ping-pong = {
            depends  = [
              hsPkgs.base
              hsPkgs.binary
              hsPkgs.bytestring
              hsPkgs.cardano-sl-networking
              hsPkgs.network-transport-tcp
              hsPkgs.random
              hsPkgs.safe-exceptions
              hsPkgs.time-units
            ];
          };
          bench-sender = {
            depends  = [
              hsPkgs.base
              hsPkgs.cardano-sl-networking
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.MonadRandom
              hsPkgs.mtl
              hsPkgs.network-transport-tcp
              hsPkgs.optparse-simple
              hsPkgs.random
              hsPkgs.safe-exceptions
              hsPkgs.serokell-util
              hsPkgs.time-units
            ];
          };
          bench-receiver = {
            depends  = [
              hsPkgs.base
              hsPkgs.cardano-sl-networking
              hsPkgs.log-warper
              hsPkgs.network-transport-tcp
              hsPkgs.optparse-simple
              hsPkgs.random
              hsPkgs.safe-exceptions
              hsPkgs.serokell-util
              hsPkgs.time-units
            ];
          };
          bench-log-reader = {
            depends  = [
              hsPkgs.base
              hsPkgs.attoparsec
              hsPkgs.cardano-sl-networking
              hsPkgs.conduit
              hsPkgs.conduit-extra
              hsPkgs.containers
              hsPkgs.exceptions
              hsPkgs.formatting
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.mtl
              hsPkgs.optparse-simple
              hsPkgs.resourcet
              hsPkgs.safe-exceptions
              hsPkgs.text
            ];
          };
        };
        tests = {
          cardano-sl-networking-test = {
            depends  = [
              hsPkgs.base
              hsPkgs.binary
              hsPkgs.bytestring
              hsPkgs.cardano-sl-networking
              hsPkgs.containers
              hsPkgs.hspec
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.mtl
              hsPkgs.network-transport
              hsPkgs.network-transport-tcp
              hsPkgs.network-transport-inmemory
              hsPkgs.QuickCheck
              hsPkgs.random
              hsPkgs.safe-exceptions
              hsPkgs.serokell-util
              hsPkgs.stm
              hsPkgs.time-units
            ];
          };
        };
        benchmarks = {
          qdisc-simulation = {
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