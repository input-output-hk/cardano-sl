{ localLib ? import ./../../../../lib.nix
, config ? {}
, numCoreNodes ? 4
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "./state-demo"
, system ? builtins.currentSystem
, pkgs ? import localLib.fetchNixPkgs { inherit system config; }
, gitrev ? "123456" # Dummy git revision to prevent mass rebuilds
, ghcRuntimeArgs ? "-N2 -qg -A1m -I0 -T"
, additionalNodeArgs ? ""
}:

with localLib;

let
  demo-cluster = iohkPkgs.demoCluster.override {
    inherit gitrev numCoreNodes stateDir;
    keepAlive = false;
  };
  executables =  {
    integration-test = "${iohkPkgs.cardano-sl-wallet-new}/bin/cardano-integration-test";
  };
  iohkPkgs = import ./../../../.. { inherit config system pkgs gitrev; };
in pkgs.writeScript "integration-tests" ''
  source ${demo-cluster}
  mkdir -p scripts
  cp -r ${stateDir}/tls-files scripts/tls-files
  ${executables.integration-test} --tls-ca-cert ${stateDir}/tls-files/ca.crt --tls-client-cert ${stateDir}/tls-files/client.crt --tls-key ${stateDir}/tls-files/client.key
  EXIT_STATUS=$?
  stop_cardano
''
