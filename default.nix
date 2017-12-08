let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, gitrev ? localLib.commitIdFromGitRepo ./.git
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
# profiling slows down performance by 50% so we don't enable it by default
, enableProfiling ? false
, enableDebugging ? false
}:

with pkgs.lib;
with pkgs.haskell.lib;

let
  addGitRev = subject: subject.overrideAttrs (drv: { GITREV = gitrev; });
  addRealTimeTestLogs = drv: overrideCabal drv (attrs: {
    testTarget = "--log=test.log || (sleep 10 && kill $TAILPID && false)";
    preCheck = ''
      mkdir -p dist/test
      touch dist/test/test.log
      tail -F dist/test/test.log &
      export TAILPID=$!
    '';
    postCheck = ''
      sleep 10
      kill $TAILPID
    '';
  });
  cardanoPkgs = ((import ./pkgs { inherit pkgs; }).override {
    overrides = self: super: {
      cardano-sl-core = overrideCabal super.cardano-sl-core (drv: {
        configureFlags = (drv.configureFlags or []) ++ [
          "-f-asserts"
        ];
      });

      cardano-sl = overrideCabal super.cardano-sl (drv: {
        # production full nodes shouldn't use wallet as it means different constants
        configureFlags = (drv.configureFlags or []) ++ [
          "-f-asserts"
        ];
        # waiting on load-command size fix in dyld
        doCheck = ! pkgs.stdenv.isDarwin;
        passthru = {
          inherit enableProfiling;
        };
      });

      cardano-sl-client = addRealTimeTestLogs super.cardano-sl-client;
      cardano-sl-generator = addRealTimeTestLogs super.cardano-sl-generator;
      cardano-sl-auxx = addGitRev super.cardano-sl-auxx;
      cardano-sl-node = addGitRev super.cardano-sl-node;
      cardano-sl-wallet = addGitRev super.cardano-sl-wallet;
      cardano-sl-wallet-static = addGitRev (justStaticExecutables super.cardano-sl-wallet);
      cardano-sl-wallet-new = addGitRev (justStaticExecutables super.cardano-sl-wallet-new);
      cardano-sl-tools = addGitRev (justStaticExecutables (overrideCabal super.cardano-sl-tools (drv: {
        # waiting on load-command size fix in dyld
        doCheck = ! pkgs.stdenv.isDarwin;
      })));

      cardano-sl-static = justStaticExecutables self.cardano-sl;
      cardano-sl-explorer-static = addGitRev (justStaticExecutables self.cardano-sl-explorer);
      cardano-report-server-static = justStaticExecutables self.cardano-report-server;

      # Undo configuration-nix.nix change to hardcode security binary on darwin
      # This is needed for macOS binary not to fail during update system (using http-client-tls)
      # Instead, now the binary is just looked up in $PATH as it should be installed on any macOS
      x509-system = overrideDerivation super.x509-system (drv: {
        postPatch = ":";
      });

      # TODO: get rid of pthreads option once cryptonite 0.25 is released
      # DEVOPS-393: https://github.com/haskell-crypto/cryptonite/issues/193
      cryptonite = appendPatch (appendConfigureFlag super.cryptonite "--ghc-option=-optl-pthread") ./pkgs/cryptonite-segfault-blake.patch;

      # Due to https://github.com/input-output-hk/stack2nix/issues/56
      hfsevents = self.callPackage ./pkgs/hfsevents.nix { inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa CoreServices; };

      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = enableProfiling;
        enableExecutableProfiling = enableProfiling;
      } // optionalAttrs enableDebugging {
        # TODO: DEVOPS-355
        dontStrip = true;
        configureFlags = (args.configureFlags or []) ++ [ "--ghc-options=-g --disable-executable-stripping --disable-library-stripping" "--profiling-detail=toplevel-functions"];
      });
    };
  });
  rawDockerImage = let
    topologyFile = pkgs.writeText "topology.yaml" ''
      wallet:
        relays:
          [
            [
              { host: nodes.awstest.iohkdev.io }
            ]
          ]
        valency: 1
        fallbacks: 7
    '';
    SYSTEM_START_TIME = 1504820421;
    configFiles = pkgs.runCommand "cardano-config" {} ''
      mkdir -pv $out
      cd $out
      cp -vi ${cardanoPkgs.cardano-sl.src + "/configuration.yaml"} configuration.yaml
      cp -vi ${cardanoPkgs.cardano-sl.src + "/mainnet-genesis-dryrun-with-stakeholders.json"} mainnet-genesis-dryrun-with-stakeholders.json
      cp -vi ${cardanoPkgs.cardano-sl.src + "/mainnet-genesis.json"} mainnet-genesis.json
      cp -vi ${cardanoPkgs.cardano-sl.src + "/../scripts/log-templates/log-config-qa.yaml"} log-config-qa.yaml
      cp -vi ${topologyFile} topology.yaml
    '';
    startScript = pkgs.writeScriptBin "cardano-start" ''
      #!/bin/sh
      set -e
      set -x
      set -o pipefail
      if [ ! -d /wallet ]; then
        echo /wallet volume not mounted, you need to create one with `docket volume create` and pass the correct -v flag to `docker run`
        exit 1
      fi
      if [ ! -d /wallet/tls ]; then
        mkdir /wallet/tls/
        openssl req -x509 -newkey rsa:2048 -keyout /wallet/tls/server.key -out /wallet/tls/server.cert -days 3650 -nodes -subj "/CN=localhost"
      fi
      cardano-node \
        "--tlscert" /wallet/tls/server.cert \
        "--tlskey" /wallet/tls/server.key \
        "--tlsca" /wallet/tls/server.cert \
        "--no-ntp" \
        "--topology" "${configFiles}/topology.yaml" \
        "--log-config" "${configFiles}/log-config-qa.yaml" \
        "--logs-prefix" "/wallet/logs/" \
        "--db-path" "/wallet/db" \
        "--wallet-db-path" "/wallet/wdb" \
        "--system-start" ${toString SYSTEM_START_TIME} \
        "--keyfile" "/wallet/secret.key" \
        "--configuration-file" "${configFiles}/configuration.yaml" \
        "--configuration-key" "mainnet_dryrun_full" \
        "--wallet-address" "0.0.0.0:8090"
    '';
  in pkgs.dockerTools.buildImage {
    name = "cardano-container-staging-1.0";
    contents = [ cardanoPkgs.cardano-sl-wallet pkgs.iana-etc startScript pkgs.openssl ] ++ optional true (with pkgs; [ bashInteractive coreutils utillinux iproute iputils curl socat ]);
    config = {
      Cmd = [
        "cardano-start"
      ];
      ExposedPorts = {
        "3000/tcp" = {};
        "8090/tcp" = {};
      };
    };
  };
  dockerImage = pkgs.runCommand "cardano-container-hydra" {} ''
    mkdir -pv $out/nix-support/
    cat <<EOF > $out/nix-support/hydra-build-products
    file dockerimage ${rawDockerImage}
    EOF
  '';
  upstream = {
    stack2nix = import (pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "stack2nix";
      rev = "8d70f7e632fe7c665ce0a5432515b918e88fe76c";
      sha256 = "1ippcbki5hgsanh2xi6wzgwbpqcl6iq81wqvfz91j0rldyh7kbl8";
    }) { inherit pkgs; };
    inherit (pkgs) purescript;
    inherit rawDockerImage dockerImage;
  };
in cardanoPkgs // upstream
