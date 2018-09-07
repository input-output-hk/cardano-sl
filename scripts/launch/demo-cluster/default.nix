{ localLib ? import ./../../../lib.nix
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "./state-demo"
, config ? {}
, runWallet ? true
, runExplorer ? false
, numCoreNodes ? 4
, numRelayNodes ? 1
, numImportedWallets ? 11
, assetLockAddresses ? []
, system ? builtins.currentSystem
, pkgs ? import localLib.fetchNixPkgs { inherit system config; }
, gitrev ? localLib.commitIdFromGitRepo ./../../../.git
, ghcRuntimeArgs ? "-N2 -qg -A1m -I0 -T"
, additionalNodeArgs ? ""
, keepAlive ? true
, launchGenesis ? false
, configurationKey ? "default"
, useStackBinaries ? false
, disableClientAuth ? false
}:

with localLib;

let
  stackExec = optionalString useStackBinaries "stack exec -- ";
  cardanoDeps = with iohkPkgs; [ cardano-sl-tools cardano-sl-wallet-new-static cardano-sl-node-static ];
  demoClusterDeps = with pkgs; [ jq coreutils curl gnused openssl ];
  allDeps =  demoClusterDeps ++ (optionals (!useStackBinaries ) cardanoDeps);
  walletConfig = {
    inherit stateDir disableClientAuth;
    topologyFile = walletTopologyFile;
    environment = "demo";
  };
  walletEnvironment = if launchGenesis then {
    environment = "override";
    relays = "127.0.0.1";
    confKey = "testnet_full";
    confFile = "${stateDir}/configuration.yaml";
  } else {
    environment = "demo";
  };
  demoWallet = pkgs.callPackage ./../connect-to-cluster ({ inherit gitrev useStackBinaries; debug = false; } // walletEnvironment // walletConfig);
  ifWallet = optionalString (runWallet);
  ifKeepAlive = optionalString (keepAlive);
  iohkPkgs = import ./../../.. { inherit config system pkgs gitrev; };
  src = ./../../..;
  topologyFile = import ./make-topology.nix { inherit (pkgs) lib; cores = numCoreNodes; relays = numRelayNodes; };
  walletTopologyFile = builtins.toFile "wallet-topology.yaml" (builtins.toJSON {
    wallet = {
      relays = [ [ { addr = "127.0.0.1"; port = 3101; } ] ];
      valency = 1;
      fallbacks = 1;
    };
  });
  assetLockFile = pkgs.writeText "asset-lock-file" (intersperse "\n" assetLockAddresses);
  ifAssetLock = optionalString (assetLockAddresses != []);
  configFiles = pkgs.runCommand "cardano-config" {} ''
      mkdir -pv $out
      cd $out
      cp -vi ${iohkPkgs.cardano-sl.src + "/configuration.yaml"} configuration.yaml
      cp -vi ${iohkPkgs.cardano-sl.src + "/mainnet-genesis-dryrun-with-stakeholders.json"} mainnet-genesis-dryrun-with-stakeholders.json
      cp -vi ${iohkPkgs.cardano-sl.src + "/mainnet-genesis.json"} mainnet-genesis.json
    '';
  prepareGenesis = import ../../prepare-genesis {
    inherit config system pkgs gitrev numCoreNodes;
    configurationKey = "testnet_full";
    configurationKeyLaunch = "testnet_launch";
  };

in pkgs.writeScript "demo-cluster" ''
  #!${pkgs.stdenv.shell}
  export PATH=${pkgs.lib.makeBinPath allDeps}:$PATH
  # Set to 0 (passing) by default. Tests using this cluster can set this variable
  # to force the `stop_cardano` function to exit with a different code.
  EXIT_STATUS=0
  function stop_cardano {
    trap "" INT TERM
    echo "Received TERM!"
    echo "Stopping Cardano core nodes"
    for pid in ''${core_pid[@]}
    do
      echo killing pid $pid
      kill $pid
    done
    for pid in ''${relay_pid[@]}
    do
      echo killing pid $pid
      kill $pid
    done
    ${ifWallet ''
      echo killing wallet pid $wallet_pid
    kill $wallet_pid
    ''}
    wait
    echo "Stopped all Cardano processes, exiting with code $EXIT_STATUS!"
    exit $EXIT_STATUS
  }
  system_start=$((`date +%s` + 15))
  echo "Using system start time "$system_start



  # Remove previous state
  rm -rf ${stateDir}
  mkdir -p ${stateDir}/logs

  ${if launchGenesis then ''
    echo "Creating genesis data and keys using external method..."
    config_files=${stateDir}
    ${prepareGenesis} $config_files
  '' else ''
    echo "Creating genesis keys..."
    config_files=${configFiles}
    ${stackExec}cardano-keygen --system-start 0 generate-keys-by-spec --genesis-out-dir ${stateDir}/genesis-keys --configuration-file $config_files/configuration.yaml --configuration-key ${configurationKey}
  ''}

  trap "stop_cardano" INT TERM
  echo "Launching a demo cluster..."
  for i in {1..${builtins.toString numCoreNodes}}
  do
    node_args="--db-path ${stateDir}/core-db$i --rebuild-db ${if launchGenesis then "--keyfile ${stateDir}/genesis-keys/generated-keys/rich/key$((i - 1)).sk" else "--genesis-secret $i"} --listen 127.0.0.1:$((3000 + i)) --json-log ${stateDir}/logs/core$i.json --logs-prefix ${stateDir}/logs --system-start $system_start --metrics +RTS -N2 -qg -A1m -I0 -T -RTS --node-id core$i --topology ${topologyFile} --configuration-file $config_files/configuration.yaml --configuration-key ${configurationKey} ${ifAssetLock "--asset-lock-file ${assetLockFile}"}"
    echo Launching core node $i: cardano-node-simple $node_args
    ${stackExec}cardano-node-simple $node_args &> ${stateDir}/logs/core$i.log &
    core_pid[$i]=$!

  done
  for i in {1..${builtins.toString numRelayNodes}}
  do
    node_args="--db-path ${stateDir}/relay-db$i --rebuild-db --listen 127.0.0.1:$((3100 + i)) --json-log ${stateDir}/logs/relay$i.json --logs-prefix ${stateDir}/logs --system-start $system_start --metrics +RTS -N2 -qg -A1m -I0 -T -RTS --node-id relay$i --topology ${topologyFile} --configuration-file $config_files/configuration.yaml --configuration-key ${configurationKey}"
    echo Launching relay node $i: cardano-node-simple $node_args
    ${stackExec}cardano-node-simple $node_args &> ${stateDir}/logs/relay$i.log &
    relay_pid[$i]=$!

  done
  ${ifWallet ''
    ${utf8LocaleSetting}
    echo Launching wallet node: ${demoWallet}
    ${demoWallet} --runtime-args "--system-start $system_start" &> ${stateDir}/logs/wallet.log &
    wallet_pid=$!

    # Query node info until synced
    SYNCED=0
    while [[ $SYNCED != 100 ]]
    do
      PERC=$(curl --silent --cacert ${stateDir}/tls/client/ca.crt --cert ${stateDir}/tls/client/client.pem https://${demoWallet.walletListen}/api/v1/node-info | jq .data.syncProgress.quantity)
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
          echo "Importing key$i.sk ..."
          curl https://${demoWallet.walletListen}/api/wallets/keys \
          --cacert ${stateDir}/tls/client/ca.crt \
          --cert ${stateDir}/tls/client/client.pem \
          -X POST \
          -H 'cache-control: no-cache' \
          -H 'content-type: application/json' \
          -d "\"${stateDir}/genesis-keys/generated-keys/poor/key$i.sk\"" | jq .
      done
    fi
  ''}
  ${ifKeepAlive ''
    sleep infinity
  ''}
''
