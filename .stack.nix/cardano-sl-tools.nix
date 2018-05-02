{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {
      for-installer = false;
    } // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-tools";
          version = "1.1.1";
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
        cardano-sl-tools = {
          depends  = [
            hsPkgs.aeson
            hsPkgs.base
            hsPkgs.bytestring
            hsPkgs.conduit
            hsPkgs.containers
            hsPkgs.directory
            hsPkgs.filepath
            hsPkgs.parsers
            hsPkgs.text
            hsPkgs.trifecta
            hsPkgs.universum
            hsPkgs.yaml
          ];
        };
        exes = {
          dbgen = {
            depends  = pkgs.lib.optionals (!_flags.for-installer) [
              hsPkgs.base
              hsPkgs.acid-state
              hsPkgs.aeson
              hsPkgs.ansi-terminal
              hsPkgs.unordered-containers
              hsPkgs.containers
              hsPkgs.cryptonite
              hsPkgs.serokell-util
              hsPkgs.stm
              hsPkgs.text
              hsPkgs.time
              hsPkgs.time-units
              hsPkgs.QuickCheck
              hsPkgs.bytestring
              hsPkgs.string-conv
              hsPkgs.mtl
              hsPkgs.lens
              hsPkgs.optparse-generic
              hsPkgs.optparse-applicative
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-db
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-wallet
              hsPkgs.optparse-generic
              hsPkgs.optparse-applicative
              hsPkgs.mtl
              hsPkgs.network-transport-tcp
              hsPkgs.log-warper
              hsPkgs.data-default
              hsPkgs.lens
              hsPkgs.universum
            ];
            build-tools = [ hsPkgs.cpphs ];
          };
          cardano-dht-keygen = {
            depends  = pkgs.lib.optionals (!_flags.for-installer) [
              hsPkgs.QuickCheck
              hsPkgs.aeson
              hsPkgs.array
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.data-default
              hsPkgs.filepath
              hsPkgs.formatting
              hsPkgs.kademlia
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.optparse-applicative
              hsPkgs.parsec
              hsPkgs.random
              hsPkgs.random-shuffle
              hsPkgs.serokell-util
              hsPkgs.stm
              hsPkgs.text
              hsPkgs.time
              hsPkgs.universum
              hsPkgs.unordered-containers
              hsPkgs.vector
            ];
            build-tools = [ hsPkgs.cpphs ];
          };
          cardano-genupdate = {
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
          cardano-keygen = {
            depends  = pkgs.lib.optionals (!_flags.for-installer) [
              hsPkgs.Glob
              hsPkgs.QuickCheck
              hsPkgs.aeson
              hsPkgs.ansi-wl-pprint
              hsPkgs.base
              hsPkgs.base58-bytestring
              hsPkgs.bytestring
              hsPkgs.canonical-json
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-txp
              hsPkgs.containers
              hsPkgs.cryptonite
              hsPkgs.data-default
              hsPkgs.directory
              hsPkgs.ed25519
              hsPkgs.filepath
              hsPkgs.formatting
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.neat-interpolation
              hsPkgs.optparse-applicative
              hsPkgs.parsec
              hsPkgs.random
              hsPkgs.safe-exceptions
              hsPkgs.serokell-util
              hsPkgs.silently
              hsPkgs.text
              hsPkgs.universum
              hsPkgs.unordered-containers
              hsPkgs.yaml
            ];
          };
          cardano-launcher = {
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
              hsPkgs.conduit
              hsPkgs.containers
              hsPkgs.directory
              hsPkgs.filepath
              hsPkgs.formatting
              hsPkgs.lens
              hsPkgs.lifted-async
              hsPkgs.log-warper
              hsPkgs.neat-interpolation
              hsPkgs.optparse-applicative
              hsPkgs.parsers
              hsPkgs.process
              hsPkgs.safe-exceptions
              hsPkgs.serokell-util
              hsPkgs.silently
              hsPkgs.text
              hsPkgs.time-units
              hsPkgs.trifecta
              hsPkgs.universum
              hsPkgs.unordered-containers
              hsPkgs.yaml
            ] ++ pkgs.lib.optional (!system.isWindows) hsPkgs.unix;
          };
          cardano-addr-convert = {
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
          cardano-cli-docs = {
            depends  = pkgs.lib.optionals (!_flags.for-installer) [
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.cardano-sl
              hsPkgs.directory
              hsPkgs.filepath
              hsPkgs.neat-interpolation
              hsPkgs.optparse-applicative
              hsPkgs.process
              hsPkgs.text
              hsPkgs.universum
            ];
            build-tools = [ hsPkgs.cpphs ];
          };
          cardano-post-mortem = {
            depends  = pkgs.lib.optionals (!_flags.for-installer) [
              hsPkgs.Chart
              hsPkgs.Chart-diagrams
              hsPkgs.MonadRandom
              hsPkgs.aeson
              hsPkgs.attoparsec
              hsPkgs.base
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-util
              hsPkgs.containers
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
          cardano-blockchain-analyser = {
            depends  = pkgs.lib.optionals (!_flags.for-installer) [
              hsPkgs.ansi-wl-pprint
              hsPkgs.base
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-binary
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-db
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-lrc
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-util
              hsPkgs.data-default
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
            build-tools = [ hsPkgs.cpphs ];
          };
        };
        tests = {
          cardano-sl-tools-test = {
            depends  = [
              hsPkgs.QuickCheck
              hsPkgs.aeson
              hsPkgs.base
              hsPkgs.cardano-sl-tools
              hsPkgs.directory
              hsPkgs.filepath
              hsPkgs.hspec
              hsPkgs.hspec-discover
              hsPkgs.parsers
              hsPkgs.temporary
              hsPkgs.text
              hsPkgs.trifecta
              hsPkgs.universum
              hsPkgs.yaml
            ];
            build-tools = [ hsPkgs.cpphs ];
          };
        };
      };
    } // rec { src = ../tools; }