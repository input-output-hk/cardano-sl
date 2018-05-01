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
    assetLockAddresses = [ "DdzFFzCqrhswMWoTiWaqXUDZJuYUx63qB6Aq8rbVbhFbc8NWqhpZkC7Lhn5eVA7kWf4JwKvJ9PqQF78AewMCzDZLabkzm99rFzpNDKp5" ];
  };
  executables =  {
    integration-test = "${iohkPkgs.cardano-sl-wallet-new}/bin/cardano-integration-test";
  };
  iohkPkgs = import ./../../../.. { inherit config system pkgs gitrev; };
in pkgs.writeScript "integration-tests" ''
  source ${demo-cluster}
  mkdir -p scripts
  cp -r ${stateDir}/tls-files scripts/tls-files
  ${executables.integration-test}
  EXIT_STATUS=$?
  stop_cardano
''
