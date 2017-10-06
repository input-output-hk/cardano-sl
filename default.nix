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
  cardanoPkgs = ((import ./pkgs { inherit pkgs; }).override {
    overrides = self: super: {
      cardano-sl = overrideCabal super.cardano-sl (drv: {
        # production full nodes shouldn't use wallet as it means different constants
        configureFlags = (drv.configureFlags or []) ++ [
          "-f-asserts"
          "-f-dev-mode"
        ];
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
        # waiting on load-command size fix in dyld
        doCheck = ! pkgs.stdenv.isDarwin;
        enableExecutableProfiling = enableProfiling;
        passthru = {
          inherit enableProfiling;
        };
      });
      cardano-sl-core = overrideCabal super.cardano-sl-core (drv: {
        configureFlags = (drv.configureFlags or []) ++ [
          "-f-asserts"
          "-f-dev-mode"
          "--ghc-options=-DGITREV=${gitrev}"
        ];
      });

      cardano-sl-wallet = justStaticExecutables super.cardano-sl-wallet;
      cardano-sl-tools = justStaticExecutables (overrideCabal super.cardano-sl-tools (drv: {
        # waiting on load-command size fix in dyld
        doCheck = ! pkgs.stdenv.isDarwin;
      }));

      cardano-sl-static = justStaticExecutables self.cardano-sl;
      cardano-sl-explorer-static = justStaticExecutables self.cardano-sl-explorer;
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

      # Darwin fixes upstreamed in nixpkgs commit 71bebd52547f4486816fd320bb3dc6314f139e67
      hinotify = if pkgs.stdenv.isDarwin then self.hfsevents else super.hinotify;
      hfsevents = self.callPackage ./pkgs/hfsevents.nix { inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa CoreServices; };
      fsnotify = if pkgs.stdenv.isDarwin
        then addBuildDepend (dontCheck super.fsnotify) pkgs.darwin.apple_sdk.frameworks.Cocoa
        else dontCheck super.fsnotify;

      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = enableProfiling;
      } // optionalAttrs enableDebugging {
        # TODO: DEVOPS-355
        dontStrip = true;
        configureFlags = (args.configureFlags or []) ++ [ "--ghc-options=-g --disable-executable-stripping --disable-library-stripping" ];
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
    CLUSTER = "mainnet-1.0";
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
  in pkgs.dockerTools.buildImage {
    name = "cardano-container-${CLUSTER}";
    contents = [ cardanoPkgs.cardano-sl-wallet pkgs.iana-etc ] ++ optional true (with pkgs; [ bashInteractive coreutils utillinux iproute iputils ]);
    config = {
      Cmd = [
        "cardano-node"
        "--tlscert" "${cardanoPkgs.cardano-sl.src + "/../scripts/tls-files/server.crt"}"
        "--tlskey" "${cardanoPkgs.cardano-sl.src + "/../scripts/tls-files/server.key"}"
        "--tlsca" "${cardanoPkgs.cardano-sl.src + "/../scripts/tls-files/ca.crt"}"
        "--no-ntp"
        "--topology" "${configFiles}/topology.yaml"
        "--log-config" "${configFiles}/log-config-qa.yaml"
        "--logs-prefix" "/wallet/logs/${CLUSTER}"
        "--db-path" "/wallet/db-${CLUSTER}"
        "--wallet-db-path" "/wallet/wdb-${CLUSTER}"
        "--system-start" (toString SYSTEM_START_TIME)
        "--keyfile" "/wallet/secret-${CLUSTER}.key"
        "--configuration-file" "${configFiles}/configuration.yaml"
        "--configuration-key" "mainnet_dryrun_full"
        "--wallet-address" "0.0.0.0:8090"
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
      rev = "be52e67113332280911bcc4924d42f90e21f1144";
      sha256 = "13n7gjyzll3prvdsb6kjyxk9g0by5bv0q34ld7a2nbvdcl1q67fb";
    }) { inherit pkgs; };
    inherit (pkgs) purescript;
    inherit rawDockerImage dockerImage;
  };
in cardanoPkgs // upstream
