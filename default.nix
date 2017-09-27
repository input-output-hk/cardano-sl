let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, dconfig ? "testnet_staging_full"
, gitrev ? "unknown"
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; }) }:

with pkgs.lib;
with pkgs.haskell.lib;

let
  addConfigureFlags = flags: drv: overrideCabal drv (drv: {
    configureFlags = flags;
  });
  cardanoPkgs = ((import ./pkgs { inherit pkgs; }).override {
    overrides = self: super: {
      cardano-sl = overrideCabal super.cardano-sl (drv: {
        # production full nodes shouldn't use wallet as it means different constants
        configureFlags = [
          "-f-asserts"
          "-f-dev-mode"
          "--ghc-option=-optl-lm"
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
      });
      cardano-sl-core = overrideCabal super.cardano-sl-core (drv: {
        configureFlags = [
          "-f-embed-config"
          "-f-asserts"
          "-f-dev-mode"
          "--ghc-options=-DCONFIG=${dconfig}"
          "--ghc-options=-DGITREV=${gitrev}"
        ];
      });
      cardano-sl-update = overrideCabal super.cardano-sl-update (drv: {
        patchPhase = ''
          export CSL_SYSTEM_TAG=${if pkgs.stdenv.isDarwin then "macos64" else "linux64"}
        '';
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

      # Gold linker fixes
      cryptonite = addConfigureFlags ["--ghc-option=-optl-pthread"] super.cryptonite;

      # Darwin fixes upstreamed in nixpkgs commit 71bebd52547f4486816fd320bb3dc6314f139e67
      hinotify = if pkgs.stdenv.isDarwin then self.hfsevents else super.hinotify;
      hfsevents = self.callPackage ./pkgs/hfsevents.nix { inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa CoreServices; };
      fsnotify = if pkgs.stdenv.isDarwin
        then addBuildDepend (dontCheck super.fsnotify) pkgs.darwin.apple_sdk.frameworks.Cocoa
        else dontCheck super.fsnotify;

      mkDerivation = args: super.mkDerivation (args // {
        #enableLibraryProfiling = true;
      });
    };
  });
  rawDockerImage = let
    DOMAIN = "aws.iohk.io";
    topologyFile = pkgs.writeText "topology.yaml" ''
      wallet:
        relays:
          [
            [
              { host: cardano-node-0.${DOMAIN}, port: 3000 },
              { host: cardano-node-1.${DOMAIN}, port: 3000 },
              { host: cardano-node-2.${DOMAIN}, port: 3000 },
              { host: cardano-node-3.${DOMAIN}, port: 3000 },
              { host: cardano-node-4.${DOMAIN}, port: 3000 },
              { host: cardano-node-5.${DOMAIN}, port: 3000 },
              { host: cardano-node-6.${DOMAIN}, port: 3000 }
            ]
          ]
        valency: 1
        fallbacks: 7
    '';
    CLUSTER = "testnet-0.6";
    SYSTEM_START_TIME = 1504820421;
    debugDeps = pkgs.buildEnv {
      name = "debug-deps";
      paths = with pkgs; [ bashInteractive coreutils utillinux iproute iputils ];
    };
    saneShell = pkgs.writeScript "sane-shell.sh" ''
      #! ${pkgs.stdenv.shell}
      export PATH=${debugDeps}/bin/
      bash
    '';
  in pkgs.dockerTools.buildImage {
    name = "cardano-container-${CLUSTER}";
    contents = [ cardanoPkgs.cardano-sl-static pkgs.iana-etc ];
    config = {
      Cmd2 = [ saneShell ];
      Cmd = [
        "${cardanoPkgs.cardano-sl-wallet}/bin/cardano-node"
        "--tlscert" "${cardanoPkgs.cardano-sl.src + "/../scripts/tls-files/server.crt"}"
        "--tlskey" "${cardanoPkgs.cardano-sl.src + "/../scripts/tls-files/server.key"}"
        "--tlsca" "${cardanoPkgs.cardano-sl.src + "/../scripts/tls-files/ca.crt"}"
        "--no-ntp"
        "--topology" "${topologyFile}"
        "--log-config" "${cardanoPkgs.cardano-sl.src + "/../scripts/log-templates/log-config-qa.yaml"}"
        "--logs-prefix" "logs/${CLUSTER}"
        "--db-path" "db-${CLUSTER}"
        "--wallet-db-path" "wdb-${CLUSTER}"
        "--system-start" (toString SYSTEM_START_TIME)
      ];
      ExposedPorts = {
        "3000/tcp" = {};
      };
    };
  };
  dockerImage = pkgs.runCommand "cardano-container-hydra" {} ''
    mkdir -pv $out/nix-support/
    cat <<EOF > $out/nix-support/hydra-build-products
    file dockerimage ${rawDockerImage}
    EOF
  '';
  other = {
    inherit rawDockerImage dockerImage;
    stack2nix = import (pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "stack2nix";
      rev = "be52e67113332280911bcc4924d42f90e21f1144";
      sha256 = "13n7gjyzll3prvdsb6kjyxk9g0by5bv0q34ld7a2nbvdcl1q67fb";
    }) { inherit pkgs; };
  };
in cardanoPkgs // other
