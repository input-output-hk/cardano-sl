with import ../../../../lib.nix;

{ stdenv, writeScript, gnugrep

, cardano-sl-tools, cardano-wallet
, demoCluster

## options for tests
, numCoreNodes ? 4
, stateDir ? maybeEnv "CARDANO_STATE_DIR" "./state-demo"
, ghcRuntimeArgs ? "-N2 -qg -A1m -I0 -T"
, additionalNodeArgs ? ""
, useStackBinaries ? false
}:


let
  stackExec = optionalString useStackBinaries "stack exec -- ";
  cardanoDeps = [ cardano-sl-tools cardano-wallet ];
  integrationTestDeps = [ gnugrep ];
  allDeps = integrationTestDeps ++ (optionals (!useStackBinaries ) cardanoDeps);
  demo-cluster = demoCluster.override {
    inherit numCoreNodes stateDir;
    keepAlive = false;
    assetLockAddresses = [ "DdzFFzCqrhswMWoTiWaqXUDZJuYUx63qB6Aq8rbVbhFbc8NWqhpZkC7Lhn5eVA7kWf4JwKvJ9PqQF78AewMCzDZLabkzm99rFzpNDKp5" ];
  };
in writeScript "integration-tests" ''
  #!${stdenv.shell}
  export PATH=${stdenv.lib.makeBinPath allDeps}:$PATH
  set -e
  source ${demo-cluster}
  echo "Demo Cluster Started"
  set +e
  echo "Running Wallet Integration tests"
  ${stackExec}wal-integr-test --tls-ca-cert ${stateDir}/tls/wallet/ca.crt --tls-client-cert ${stateDir}/tls/wallet/client.pem --tls-key ${stateDir}/tls/wallet/client.key "$@" 2>&1 | tee state-demo/logs/test.output
  EXIT_STATUS=$PIPESTATUS
  echo "Wallet Integration tests completed"
  # Verify we see "transaction list is empty after filtering out asset-locked source addresses" in at least 1 core node log file
  if [[ $EXIT_STATUS -eq 0 ]]
  then
    echo "EXIT_STATUS is 0 => verify asset-locked source addresses"
    grep -r "transaction list is empty after filtering out asset-locked source addresses" ${stateDir}/logs
    EXIT_STATUS=$?
  fi
  stop_cardano
''
