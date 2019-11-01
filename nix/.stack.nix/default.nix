{
  extras = hackage:
    {
      packages = {
        "fmt" = (((hackage.fmt)."0.6").revisions).default;
        "servant-multipart" = (((hackage.servant-multipart)."0.11.2").revisions).default;
        "normaldistribution" = (((hackage.normaldistribution)."1.1.0.3").revisions).default;
        "aeson-options" = (((hackage.aeson-options)."0.1.0").revisions).default;
        "servant-swagger-ui-redoc" = (((hackage.servant-swagger-ui-redoc)."0.3.0.1.21.2").revisions).default;
        "wai-middleware-throttle" = (((hackage.wai-middleware-throttle)."0.3.0.0").revisions).default;
        "clock" = (((hackage.clock)."0.8").revisions).default;
        "ether" = (((hackage.ether)."0.5.1.0").revisions).default;
        "servant-quickcheck" = (((hackage.servant-quickcheck)."0.0.7.2").revisions).default;
        "o-clock" = (((hackage.o-clock)."0.1.1").revisions).default;
        "beam-sqlite" = (((hackage.beam-sqlite)."0.3.2.3").revisions).default;
        "katip" = (((hackage.katip)."0.6.3.0").revisions).default;
        "Chart" = (((hackage.Chart)."1.9").revisions).default;
        "Chart-diagrams" = (((hackage.Chart-diagrams)."1.9").revisions).default;
        "SVGFonts" = (((hackage.SVGFonts)."1.6.0.3").revisions).default;
        "pvss" = (((hackage.pvss)."0.2.0").revisions).default;
        "systemd" = (((hackage.systemd)."1.1.2").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "pipes-interleave" = (((hackage.pipes-interleave)."1.1.3").revisions).default;
        "tabl" = (((hackage.tabl)."1.0.3").revisions).default;
        "loc" = (((hackage.loc)."0.1.3.3").revisions).default;
        "hspec" = (((hackage.hspec)."2.5.5").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "token-bucket" = (((hackage.token-bucket)."0.1.0.1").revisions).default;
        "lzma-clib" = (((hackage.lzma-clib)."5.2.2").revisions).default;
        "megaparsec" = (((hackage.megaparsec)."7.0.4").revisions).default;
        "neat-interpolation" = (((hackage.neat-interpolation)."0.3.2.4").revisions).default;
        "config-ini" = (((hackage.config-ini)."0.2.4.0").revisions).default;
        "stylish-haskell" = (((hackage.stylish-haskell)."0.9.2.2").revisions).default;
        "hedgehog" = (((hackage.hedgehog)."1.0").revisions).default;
        cardano-sl-util = ./cardano-sl-util.nix;
        cardano-sl-util-test = ./cardano-sl-util-test.nix;
        cardano-sl-networking = ./cardano-sl-networking.nix;
        cardano-sl-binary = ./cardano-sl-binary.nix;
        cardano-sl-binary-test = ./cardano-sl-binary-test.nix;
        cardano-sl-crypto = ./cardano-sl-crypto.nix;
        cardano-sl-crypto-test = ./cardano-sl-crypto-test.nix;
        cardano-sl-core = ./cardano-sl-core.nix;
        cardano-sl-core-test = ./cardano-sl-core-test.nix;
        cardano-sl-db = ./cardano-sl-db.nix;
        cardano-sl-db-test = ./cardano-sl-db-test.nix;
        cardano-sl-infra = ./cardano-sl-infra.nix;
        cardano-sl-infra-test = ./cardano-sl-infra-test.nix;
        cardano-sl-chain = ./cardano-sl-chain.nix;
        cardano-sl-chain-test = ./cardano-sl-chain-test.nix;
        cardano-sl = ./cardano-sl.nix;
        cardano-sl-generator = ./cardano-sl-generator.nix;
        cardano-sl-client = ./cardano-sl-client.nix;
        cardano-sl-auxx = ./cardano-sl-auxx.nix;
        cardano-sl-script-runner = ./cardano-sl-script-runner.nix;
        cardano-sl-explorer = ./cardano-sl-explorer.nix;
        cardano-sl-node = ./cardano-sl-node.nix;
        cardano-sl-tools = ./cardano-sl-tools.nix;
        cardano-sl-tools-post-mortem = ./cardano-sl-tools-post-mortem.nix;
        cardano-sl-utxo = ./cardano-sl-utxo.nix;
        cardano-wallet = ./cardano-wallet.nix;
        cardano-sl-node-ipc = ./cardano-sl-node-ipc.nix;
        cardano-sl-faucet = ./cardano-sl-faucet.nix;
        acid-state-exts = ./acid-state-exts.nix;
        cardano-sl-x509 = ./cardano-sl-x509.nix;
        cardano-sl-cluster = ./cardano-sl-cluster.nix;
        cardano-sl-mnemonic = ./cardano-sl-mnemonic.nix;
        yaml-validation = ./yaml-validation.nix;
        cardano-crypto = ./cardano-crypto.nix;
        ip = ./ip.nix;
        time-units = ./time-units.nix;
        kademlia = ./kademlia.nix;
        network-transport = ./network-transport.nix;
        network-transport-tcp = ./network-transport-tcp.nix;
        network-transport-inmemory = ./network-transport-inmemory.nix;
        acid-state = ./acid-state.nix;
        socket-io = ./socket-io.nix;
        engine-io = ./engine-io.nix;
        engine-io-wai = ./engine-io-wai.nix;
        canonical-json = ./canonical-json.nix;
        rocksdb-haskell-ng = ./rocksdb-haskell-ng.nix;
        log-warper = ./log-warper.nix;
        universum = ./universum.nix;
        serokell-util = ./serokell-util.nix;
        inspector = ./inspector.nix;
        };
      compiler.version = "8.4.4";
      compiler.nix-name = "ghc844";
      };
  resolver = "lts-12.26";
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "ether" = {
            flags = { "disable-tup-instances" = lib.mkOverride 900 true; };
            };
          };
        })
    {
      packages = {
        "cardano-sl-binary-test" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-auxx" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-db" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-tools" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-generator" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-crypto-test" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-mnemonic" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-core-test" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-node" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-infra" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-binary" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-tools-post-mortem" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-node-ipc" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-util-test" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-x509" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-util" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-client" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-chain-test" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-networking" = {
          package = {
            ghcOptions = "-Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-core" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-crypto" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-explorer" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-db-test" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-wallet" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-chain" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        "cardano-sl-cluster" = {
          package = {
            ghcOptions = "-Wall -Werror -Wcompat -fwarn-redundant-constraints";
            };
          };
        };
      }
    ];
  compiler = "ghc-8.4.4";
  }