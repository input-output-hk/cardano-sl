{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-infra";
          version = "1.3.0";
        };
        license = "MIT";
        copyright = "2016 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - infrastructural";
        description = "Cardano SL - infrastructural";
        buildType = "Simple";
      };
      components = {
        "cardano-sl-infra" = {
          depends  = [
            hsPkgs.aeson
            hsPkgs.aeson-options
            hsPkgs.async
            hsPkgs.base
            hsPkgs.parsec
            hsPkgs.base64-bytestring
            hsPkgs.bytestring
            hsPkgs.cardano-sl-binary
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-db
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-sinbin
            hsPkgs.cardano-sl-util
            hsPkgs.cardano-report-server
            hsPkgs.conduit
            hsPkgs.containers
            hsPkgs.directory
            hsPkgs.dns
            hsPkgs.ekg-core
            hsPkgs.ekg-statsd
            hsPkgs.ekg-wai
            hsPkgs.ether
            hsPkgs.exceptions
            hsPkgs.filepath
            hsPkgs.formatting
            hsPkgs.lzma-conduit
            hsPkgs.hashable
            hsPkgs.http-client
            hsPkgs.http-client-tls
            hsPkgs.iproute
            hsPkgs.kademlia
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.mtl
            hsPkgs.network-info
            hsPkgs.network-transport
            hsPkgs.network-transport-tcp
            hsPkgs.optparse-applicative
            hsPkgs.reflection
            hsPkgs.safe-exceptions
            hsPkgs.serokell-util
            hsPkgs.stm
            hsPkgs.clock
            hsPkgs.tagged
            hsPkgs.template-haskell
            hsPkgs.tar
            hsPkgs.text
            hsPkgs.formatting
            hsPkgs.time
            hsPkgs.time-units
            hsPkgs.transformers
            hsPkgs.network-transport
            hsPkgs.universum
            hsPkgs.unordered-containers
            hsPkgs.yaml
          ] ++ pkgs.lib.optional (!system.isWindows) hsPkgs.unix;
          build-tools = [
            hsPkgs.buildPackages.cpphs
          ];
        };
        tests = {
          "test" = {
            depends  = [
              hsPkgs.async
              hsPkgs.base
              hsPkgs.cardano-sl-infra
              hsPkgs.containers
              hsPkgs.QuickCheck
              hsPkgs.hspec
            ];
          };
        };
      };
    } // rec { src = ../infra; }