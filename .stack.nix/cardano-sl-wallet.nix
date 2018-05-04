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
          name = "cardano-sl-wallet";
          version = "1.1.1";
        };
        license = "MIT";
        copyright = "2017 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - wallet";
        description = "Cardano SL - wallet";
        buildType = "Simple";
      };
      components = {
        cardano-sl-wallet = {
          depends  = [
            hsPkgs.QuickCheck
            hsPkgs.acid-state
            hsPkgs.aeson
            hsPkgs.async
            hsPkgs.base
            hsPkgs.base58-bytestring
            hsPkgs.bytestring
            hsPkgs.cardano-sl
            hsPkgs.cardano-sl-block
            hsPkgs.cardano-sl-client
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-delegation
            hsPkgs.cardano-sl-generator
            hsPkgs.cardano-sl-db
            hsPkgs.cardano-sl-infra
            hsPkgs.cardano-sl-ssc
            hsPkgs.cardano-sl-txp
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-update
            hsPkgs.cardano-sl-util
            hsPkgs.containers
            hsPkgs.cryptonite
            hsPkgs.data-default
            hsPkgs.directory
            hsPkgs.dlist
            hsPkgs.ether
            hsPkgs.exceptions
            hsPkgs.filepath
            hsPkgs.formatting
            hsPkgs.hashable
            hsPkgs.hspec
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.memory
            hsPkgs.monad-control
            hsPkgs.mtl
            hsPkgs.quickcheck-instances
            hsPkgs.random
            hsPkgs.reflection
            hsPkgs.safe-exceptions
            hsPkgs.safecopy
            hsPkgs.semver
            hsPkgs.serokell-util
            hsPkgs.servant
            hsPkgs.servant-generic
            hsPkgs.servant-multipart
            hsPkgs.servant-server
            hsPkgs.servant-swagger
            hsPkgs.servant-swagger-ui
            hsPkgs.stm
            hsPkgs.swagger2
            hsPkgs.text
            hsPkgs.text
            hsPkgs.time
            hsPkgs.time-units
            hsPkgs.transformers
            hsPkgs.universum
            hsPkgs.unliftio
            hsPkgs.unordered-containers
            hsPkgs.wai
            hsPkgs.wai-websockets
            hsPkgs.warp
            hsPkgs.websockets
          ] ++ pkgs.lib.optional (!system.isWindows) hsPkgs.unix;
          build-tools = [ hsPkgs.cpphs ];
        };
        tests = {
          cardano-wallet-test = {
            depends  = [
              hsPkgs.MonadRandom
              hsPkgs.QuickCheck
              hsPkgs.base
              hsPkgs.cardano-sl
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-client
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-db
              hsPkgs.cardano-sl-delegation
              hsPkgs.cardano-sl-generator
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-lrc
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-txp
              hsPkgs.cardano-sl-update
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-wallet
              hsPkgs.containers
              hsPkgs.data-default
              hsPkgs.ether
              hsPkgs.formatting
              hsPkgs.hspec
              hsPkgs.lens
              hsPkgs.log-warper
              hsPkgs.mtl
              hsPkgs.safecopy
              hsPkgs.safe-exceptions
              hsPkgs.serokell-util
              hsPkgs.servant-server
              hsPkgs.stm
              hsPkgs.universum
              hsPkgs.unordered-containers
            ];
            build-tools = [ hsPkgs.cpphs ];
          };
        };
      };
    } // rec { src = ../wallet; }