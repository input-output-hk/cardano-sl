{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {
      for-installer = false;
      postmortem = false;
    } // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-tools";
          version = "1.3.0";
        };
        license = "MIT";
        copyright = "2016 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - Tools";
        description = "Cardano SL - Tools";
        buildType = "Simple";
      };
      components = {
        "cardano-sl-tools" = {
          depends  = [
            hsPkgs.aeson
            hsPkgs.base
            hsPkgs.directory
            hsPkgs.filepath
            hsPkgs.parsers
            hsPkgs.text
            hsPkgs.trifecta
            hsPkgs.universum
          ];
        };
        exes = {
          "dbgen" = {
            depends  = pkgs.lib.optionals (!_flags.for-installer) [
              hsPkgs.base
              hsPkgs.acid-state
              hsPkgs.acid-state-exts
              hsPkgs.aeson
              hsPkgs.ansi-terminal
              hsPkgs.bytestring
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-core-test
              hsPkgs.cardano-sl-db
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-txp-test
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-wallet
              hsPkgs.containers
              hsPkgs.cryptonite
              hsPkgs.data-default
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.mtl
              hsPkgs.network-transport-tcp
              hsPkgs.optparse-applicative
              hsPkgs.optparse-applicative
              hsPkgs.optparse-generic
              hsPkgs.optparse-generic
              hsPkgs.QuickCheck
              hsPkgs.serokell-util
              hsPkgs.stm
              hsPkgs.string-conv
              hsPkgs.text
              hsPkgs.time
              hsPkgs.time-units
              hsPkgs.universum
              hsPkgs.unordered-containers
            ];
            build-tools = [
              hsPkgs.buildPackages.cpphs
            ];
          };
          "cardano-genupdate" = {
            depends  = [
              hsPkgs.base
              hsPkgs.ansi-wl-pprint
              hsPkgs.bytestring
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-util
              hsPkgs.cryptonite
              hsPkgs.neat-interpolation
              hsPkgs.optparse-applicative
              hsPkgs.filepath
              hsPkgs.formatting
              hsPkgs.process
              hsPkgs.tar
              hsPkgs.text
              hsPkgs.universum
              hsPkgs.unix-compat
            ];
          };
          "cardano-keygen" = {
            depends  = pkgs.lib.optionals (!_flags.for-installer) [
              hsPkgs.base
              hsPkgs.base58-bytestring
              hsPkgs.bytestring
              hsPkgs.canonical-json
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cryptonite
              hsPkgs.directory
              hsPkgs.filepath
              hsPkgs.formatting
              hsPkgs.Glob
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.optparse-applicative
              hsPkgs.serokell-util
              hsPkgs.text
              hsPkgs.universum
            ];
          };
          "cardano-launcher" = {
            depends  = [
              hsPkgs.aeson
              hsPkgs.aeson-options
              hsPkgs.ansi-wl-pprint
              hsPkgs.async
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.cardano-report-server
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-db
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-tools
              hsPkgs.cardano-sl-update
              hsPkgs.cardano-sl-util
              hsPkgs.directory
              hsPkgs.filepath
              hsPkgs.formatting
              hsPkgs.lens
              hsPkgs.lifted-async
              hsPkgs.log-warper
              hsPkgs.neat-interpolation
              hsPkgs.optparse-applicative
              hsPkgs.process
              hsPkgs.safe-exceptions
              hsPkgs.serokell-util
              hsPkgs.silently
              hsPkgs.text
              hsPkgs.time-units
              hsPkgs.universum
              hsPkgs.unordered-containers
              hsPkgs.yaml
            ] ++ pkgs.lib.optional (!system.isWindows) hsPkgs.unix;
          };
          "cardano-addr-convert" = {
            depends  = [
              hsPkgs.base
              hsPkgs.ansi-wl-pprint
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-util
              hsPkgs.neat-interpolation
              hsPkgs.optparse-applicative
              hsPkgs.text
              hsPkgs.universum
            ];
          };
          "cardano-cli-docs" = {
            depends  = pkgs.lib.optionals (!_flags.for-installer) [
              hsPkgs.base
              hsPkgs.cardano-sl
              hsPkgs.directory
              hsPkgs.filepath
              hsPkgs.neat-interpolation
              hsPkgs.optparse-applicative
              hsPkgs.process
              hsPkgs.text
              hsPkgs.universum
            ];
            build-tools = [
              hsPkgs.buildPackages.cpphs
            ];
          };
          "cardano-post-mortem" = {
            depends  = pkgs.lib.optionals (_flags.postmortem && !_flags.for-installer) [
              hsPkgs.Chart
              hsPkgs.Chart-diagrams
              hsPkgs.MonadRandom
              hsPkgs.aeson
              hsPkgs.attoparsec
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-util
              hsPkgs.containers
              hsPkgs.cassava
              hsPkgs.directory
              hsPkgs.fgl
              hsPkgs.filepath
              hsPkgs.foldl
              hsPkgs.graphviz
              hsPkgs.optparse-applicative
              hsPkgs.pipes
              hsPkgs.pipes-bytestring
              hsPkgs.pipes-interleave
              hsPkgs.pipes-safe
              hsPkgs.process
              hsPkgs.random
              hsPkgs.text
              hsPkgs.time-units
              hsPkgs.universum
            ];
          };
          "cardano-blockchain-analyser" = {
            depends  = pkgs.lib.optionals (!_flags.for-installer) [
              hsPkgs.ansi-wl-pprint
              hsPkgs.base
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-binary
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-db
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-util
              hsPkgs.directory
              hsPkgs.formatting
              hsPkgs.lens
              hsPkgs.mtl
              hsPkgs.neat-interpolation
              hsPkgs.optparse-applicative
              hsPkgs.serokell-util
              hsPkgs.tabl
              hsPkgs.text
              hsPkgs.universum
            ];
            build-tools = [
              hsPkgs.buildPackages.cpphs
            ];
          };
          "cardano-x509-certificates" = {
            depends  = [
              hsPkgs.base
              hsPkgs.aeson
              hsPkgs.asn1-encoding
              hsPkgs.asn1-types
              hsPkgs.bytestring
              hsPkgs.base64-bytestring
              hsPkgs.cryptonite
              hsPkgs.filepath
              hsPkgs.hourglass
              hsPkgs.optparse-applicative
              hsPkgs.universum
              hsPkgs.unordered-containers
              hsPkgs.x509
              hsPkgs.x509-validation
              hsPkgs.x509-store
              hsPkgs.data-default-class
              hsPkgs.yaml
            ];
          };
          "genesis-hash" = {
            depends  = pkgs.lib.optionals (!_flags.for-installer) [
              hsPkgs.base
              hsPkgs.universum
              hsPkgs.bytestring
              hsPkgs.cryptonite
              hsPkgs.canonical-json
            ];
          };
        };
        tests = {
          "cardano-sl-tools-test" = {
            depends  = [
              hsPkgs.base
              hsPkgs.aeson
              hsPkgs.cardano-sl-tools
              hsPkgs.directory
              hsPkgs.hspec
              hsPkgs.temporary
            ];
            build-tools = [
              hsPkgs.buildPackages.cpphs
            ];
          };
        };
      };
    } // rec { src = ../tools; }