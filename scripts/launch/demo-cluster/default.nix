with import ../../../lib.nix;

{ stdenv, runCommand, writeText, writeScript
, jq, coreutils, curl, gnused, openssl

, cardano-sl-cluster, cardano-sl

, useStackBinaries ? false

## lots of options!
, stateDir ? maybeEnv "CARDANO_STATE_DIR" "./state-demo"
, runWallet ? true
, runExplorer ? false
, numCoreNodes ? 4
, numRelayNodes ? 1
, numImportedWallets ? 11
, assetLockAddresses ? []
, ghcRuntimeArgs ? "-N2 -qg -A1m -I0 -T"
, additionalNodeArgs ? ""
, keepAlive ? true
, configurationKey ? "default"
, disableClientAuth ? false
}:

let
  stackExec = optionalString useStackBinaries "stack exec -- ";
  cardanoDeps = [ cardano-sl-cluster ];
  demoClusterDeps = [ jq coreutils curl gnused openssl ];
  allDeps =  demoClusterDeps ++ (optionals (!useStackBinaries ) cardanoDeps);

  ifWallet = optionalString (runWallet);
  numEdgeNodes = if runWallet then 1 else 0;
  ifKeepAlive = optionalString (keepAlive);
  topologyFile = import ./make-topology.nix { inherit (stdenv) lib; cores = numCoreNodes; relays = numRelayNodes; };
  assetLockFile = writeText "asset-lock-file" (intersperse "\n" assetLockAddresses);
  ifAssetLock = optionalString (assetLockAddresses != []);
  ifDisableClientAuth = optionalString disableClientAuth;
  configFiles = runCommand "cardano-config" {} ''
      mkdir -pv $out
      cd $out
      cp -vi ${cardano-sl.src + "/configuration.yaml"} configuration.yaml
      cp -vi ${cardano-sl.src + "/mainnet-genesis-dryrun-with-stakeholders.json"} mainnet-genesis-dryrun-with-stakeholders.json
      cp -vi ${cardano-sl.src + "/mainnet-genesis.json"} mainnet-genesis.json
    '';

  prepareGenesis = callPackage ../../prepare-genesis {
    inherit numCoreNodes stateDir;
    configurationKey = "testnet_full";
    configurationKeyLaunch = "testnet_launch";
  };

in writeScript "demo-cluster" ''
  #!${stdenv.shell} -e
  export PATH=${stdenv.lib.makeBinPath allDeps}:$PATH
  export DEMO_STATE_DIR=${stateDir}
  export DEMO_CONFIGURATION_FILE=${configFiles}/configuration.yaml
  ${ifAssetLock "export DEMO_ASSET_LOCK_FILE=${assetLockFile}"}
  ${ifDisableClientAuth "export DEMO_NO_CLIENT_AUTH=True"}
  # Set to 0 (passing) by default. Tests using this cluster can set this variable
  # to force the `stop_cardano` function to exit with a different code.
  EXIT_STATUS=0
  function stop_cardano {
    trap "" INT TERM
    echo "Received TERM!"
    echo "Stopping Cardano Demo Cluster"
    kill $pid
    echo "Stopped all Cardano processes, exiting with code $EXIT_STATUS!"
    exit $EXIT_STATUS
  }

  # Remove previous state
  rm -rf ${stateDir}
  mkdir -p ${stateDir}/logs

  trap "stop_cardano" INT TERM
  echo "Launching a demo cluster..."
  cardano-sl-cluster-demo --no-genesis-wallets --cores ${builtins.toString numCoreNodes} --relays ${builtins.toString numRelayNodes} --edges ${builtins.toString numEdgeNodes} &
  pid=$!

  ${ifWallet ''
    # Query node info until synced
    SYNCED=0
    while [[ $SYNCED != 100 ]]
    do
      PERC=$(curl --silent --cacert ${stateDir}/tls/wallet/ca.crt --cert ${stateDir}/tls/wallet/client.pem https://127.0.0.1:8090/api/v1/node-info | jq .data.syncProgress.quantity)
      if [[ $PERC == "100" ]]
      then
        echo Blockchain Synced: $PERC%
        SYNCED=100
      elif [[ $SYNCED -ge 20 ]]
      then
        echo Blockchain Syncing: $PERC%
        echo "Sync Failed, Exiting!"
        EXIT_STATUS=1
        stop_cardano
      else
        echo Blockchain Syncing: $PERC%
        SYNCED=$((SYNCED + 1))
        sleep 5
      fi
    done
    echo Blockchain Synced: $PERC%
    if [ ${builtins.toString numImportedWallets} -gt 0 ]
    then
      echo "Importing ${builtins.toString numImportedWallets} poor HD keys/wallet..."
      for i in {0..${builtins.toString numImportedWallets}}
      do
          echo "Importing $i.key ..."
          curl https://localhost:8090/api/internal/import-wallet \
          --cacert ${stateDir}/tls/wallet/ca.crt \
          --cert ${stateDir}/tls/wallet/client.pem \
          -X POST \
          -H 'cache-control: no-cache' \
          -H 'Content-Type: application/json; charset=utf-8' \
          -H 'Accept: application/json; charset=utf-8' \
          -d "{\"filePath\": \"${stateDir}/generated-keys/poor/$i.key\"}" | jq .
      done
    fi
  ''}
  ${ifKeepAlive ''
    echo "The demo cluster has started and will stop when you exit with Ctrl-C."
    echo "Log files are in ${stateDir}/logs."
    ${ifWallet ''
    echo "Use ${stateDir}/curl to make requests to the wallet."
    ''}
    sleep infinity
  ''}
''
