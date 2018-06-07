{ localLib ? import ./../../../lib.nix
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "./state-demo"
, config ? {}
, runWallet ? true
, runExplorer ? false
, numCoreNodes ? 4
, numRelayNodes ? 1
, system ? builtins.currentSystem
, pkgs ? import localLib.fetchNixPkgs { inherit system config; }
, gitrev ? localLib.commitIdFromGitRepo ./../../../.git
, ghcRuntimeArgs ? "-N2 -qg -A1m -I0 -T"
, additionalNodeArgs ? ""
, keepAlive ? true
}:

with localLib;

let
  executables =  {
    corenode = "${iohkPkgs.cardano-sl-node-static}/bin/cardano-node-simple";
    wallet = "${iohkPkgs.cardano-sl-wallet-new}/bin/cardano-node";
    integration-test = "${iohkPkgs.cardano-sl-wallet-new}/bin/cardano-integration-test";
    keygen = "${iohkPkgs.cardano-sl-tools}/bin/cardano-keygen";
    explorer = "${iohkPkgs.cardano-sl-explorer-static}/bin/cardano-explorer";
  };
  demoClusterDeps = with pkgs; (with iohkPkgs; [ jq coreutils pkgs.curl gnused openssl cardano-sl-tools cardano-sl-wallet-new cardano-sl-node-static ]);
  walletConfig = {
    inherit stateDir;
    topologyFile = walletTopologyFile;
  };
  demoWallet = pkgs.callPackage ./../connect-to-cluster ({ inherit gitrev; debug = false; environment = "demo"; } // walletConfig);
  ifWallet = localLib.optionalString (runWallet);
  ifKeepAlive = localLib.optionalString (keepAlive);
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
  configFiles = pkgs.runCommand "cardano-config" {} ''
    mkdir -pv $out
    cd $out
    cp -vi ${iohkPkgs.cardano-sl.src + "/configuration.yaml"} configuration.yaml
    cp -vi ${iohkPkgs.cardano-sl.src + "/mainnet-genesis-dryrun-with-stakeholders.json"} mainnet-genesis-dryrun-with-stakeholders.json
    cp -vi ${iohkPkgs.cardano-sl.src + "/mainnet-genesis.json"} mainnet-genesis.json
  '';
in pkgs.writeScript "demo-cluster" ''
  #!${pkgs.stdenv.shell}
  export PATH=${pkgs.lib.makeBinPath demoClusterDeps}
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
  mkdir -p ${stateDir}
  echo "Creating genesis keys..."
  cardano-keygen --system-start 0 generate-keys-by-spec --genesis-out-dir ${stateDir}/genesis-keys --configuration-file ${configFiles}/configuration.yaml

  trap "stop_cardano" INT TERM
  echo "Launching a demo cluster..."
  for i in {1..${builtins.toString numCoreNodes}}
  do
    node_args="--db-path ${stateDir}/core-db''${i} --rebuild-db --genesis-secret ''${i} --listen 127.0.0.1:300''${i} --json-log ${stateDir}/logs/node''${i}.json --logs-prefix ${stateDir}/logs --system-start $system_start --metrics +RTS -N2 -qg -A1m -I0 -T -RTS --node-id core''${i} --topology ${topologyFile} --configuration-file ${configFiles}/configuration.yaml"
    echo Launching core node $i with args: $node_args
    cardano-node-simple $node_args &> /dev/null &
    core_pid[$i]=$!

  done
  for i in {1..${builtins.toString numRelayNodes}}
  do
    node_args="--db-path ${stateDir}/relay-db''${i} --rebuild-db --listen 127.0.0.1:310''${i} --json-log ${stateDir}/logs/node''${i}.json --logs-prefix ${stateDir}/logs --system-start $system_start --metrics +RTS -N2 -qg -A1m -I0 -T -RTS --node-id relay''${i} --topology ${topologyFile} --configuration-file ${configFiles}/configuration.yaml"
    echo Launching relay node $i with args: $node_args
    cardano-node-simple $node_args &> /dev/null &
    relay_pid[$i]=$!

  done
  ${ifWallet ''
    export LC_ALL=C.UTF-8
    echo Launching wallet node: ${demoWallet}
    ${demoWallet} --runtime-args "--system-start $system_start" &> /dev/null &
    wallet_pid=$!
  ''}
  # Query node info until synced
  SYNCED=0
  while [[ $SYNCED != 100 ]]
  do
    PERC=$(curl --silent --cacert ${stateDir}/tls/client/ca.crt --cert ${stateDir}/tls/client/client.pem https://localhost:8090/api/v1/node-info | jq .data.syncProgress.quantity)
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
  # import keys
  echo "Importing poor HD keys/wallet..."

  for i in {0..11}
  do
      echo "Importing key$i.sk ..."
      curl https://localhost:8090/api/wallets/keys \
      --cacert ${stateDir}/tls/client/ca.crt \
      --cert ${stateDir}/tls/client/client.pem \
      -X POST \
      -H 'cache-control: no-cache' \
      -H 'content-type: application/json' \
      -d "\"${stateDir}/genesis-keys/generated-keys/poor/key$i.sk\"" | jq .
  done
  ${ifKeepAlive ''
    sleep infinity
  ''}
''
