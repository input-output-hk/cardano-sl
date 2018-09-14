{ localLib ? import ./../../../../lib.nix
, config ? {}
, numCoreNodes ? 4
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "./state-demo"
, system ? builtins.currentSystem
, pkgs ? import localLib.fetchNixPkgs { inherit system config; }
, gitrev ? "123456" # Dummy git revision to prevent mass rebuilds
, ghcRuntimeArgs ? "-N2 -qg -A1m -I0 -T"
, additionalNodeArgs ? ""
, useStackBinaries ? false
}:

with localLib;

let
  stackExec = optionalString useStackBinaries "stack exec -- ";
  cardanoDeps = with iohkPkgs; [ cardano-sl-tools ];
  integrationTestDeps = with pkgs; [ gnugrep ];
  allDeps = integrationTestDeps ++ (optionals (!useStackBinaries ) cardanoDeps);
  demo-cluster = iohkPkgs.demoCluster.override {
    inherit gitrev numCoreNodes stateDir useStackBinaries;
    keepAlive = false;
    assetLockAddresses = [ "DdzFFzCqrhswMWoTiWaqXUDZJuYUx63qB6Aq8rbVbhFbc8NWqhpZkC7Lhn5eVA7kWf4JwKvJ9PqQF78AewMCzDZLabkzm99rFzpNDKp5" ];
  };
  executables =  {
    integration-test = "${iohkPkgs.cardano-sl-wallet-new}/bin/wal-integr-test";
  };
  iohkPkgs = import ./../../../.. { inherit config system pkgs gitrev; };
in pkgs.writeScript "integration-tests" ''
  #!${pkgs.stdenv.shell}
  export PATH=${pkgs.lib.makeBinPath allDeps}:$PATH
  set -e
  source ${demo-cluster}
  ${stackExec}wal-integr-test --tls-ca-cert ${stateDir}/tls/client/ca.crt --tls-client-cert ${stateDir}/tls/client/client.pem --tls-key ${stateDir}/tls/client/client.key "$@"
  EXIT_STATUS=$?
  # Verify we see "transaction list is empty after filtering out asset-locked source addresses" in at least 1 core node log file
  if [[ $EXIT_STATUS -eq 0 ]]
  then
    grep "transaction list is empty after filtering out asset-locked source addresses" state-demo/logs/core*.json
    EXIT_STATUS=$?
  fi
  stop_cardano
''
