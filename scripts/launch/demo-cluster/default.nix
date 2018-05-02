{ localLib ? import ./../../../lib.nix
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "./state-demo"
, config ? {}
, runWallet ? true
, runExplorer ? false
, numCoreNodes ? 4
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
  ifWallet = localLib.optionalString (runWallet);
  ifKeepAlive = localLib.optionalString (keepAlive);
  iohkPkgs = import ./../../.. { inherit config system pkgs gitrev; };
  src = ./../../..;
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
  source ${src + "/scripts/common-functions.sh"}
  LOG_TEMPLATE=${src + "/log-configs/template-demo.yaml"}
  function stop_cardano {
    trap "" INT TERM
    echo "Received TERM!"
    echo "Stopping Cardano core nodes"
    for pid in ''${core_pid[@]}
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

  echo "Generating Topology"
  gen_kademlia_topology ${builtins.toString (numCoreNodes + 1)} ${stateDir}

  trap "stop_cardano" INT TERM
  echo "Launching a demo cluster..."
  for i in {0..${builtins.toString (numCoreNodes - 1)}}
  do
    node_args="$(node_cmd $i "" "$system_start" "${stateDir}" "" "${stateDir}/logs" "${stateDir}") --configuration-file ${configFiles}/configuration.yaml"
    echo Launching core node $i with args: $node_args
    cardano-node-simple $node_args &> /dev/null &
    core_pid[$i]=$!

  done
  ${ifWallet ''
    export LC_ALL=en_GB.UTF-8
    if [ ! -d ${stateDir}/tls-files ]; then
      mkdir -p ${stateDir}/tls-files
      openssl req -x509 -newkey rsa:2048 -keyout ${stateDir}/tls-files/server.key -out ${stateDir}/tls-files/server.crt -days 30 -nodes -subj "/CN=localhost"
    fi
    echo Launching wallet node:
    i=${builtins.toString numCoreNodes}
    wallet_args=" --tlscert ${stateDir}/tls-files/server.crt --tlskey ${stateDir}/tls-files/server.key --tlsca ${stateDir}/tls-files/server.crt"
    # TODO: remove wallet-debug and use TLS when the tests support it
    wallet_args="$wallet_args --wallet-address 127.0.0.1:8090 --wallet-db-path ${stateDir}/wallet-db --wallet-debug"
    node_args="$(node_cmd $i "$wallet_args" "$system_start" "${stateDir}" "" "${stateDir}/logs" "${stateDir}") --configuration-file ${configFiles}/configuration.yaml"
    echo Running wallet with args: $node_args
    cardano-node $node_args &> /dev/null &
    wallet_pid=$!
  ''}
  # Query node info until synced
  SYNCED=0
  while [[ $SYNCED != 100 ]]
  do
    # TODO: switch to https when wallet-debug is removed
    PERC=$(curl --silent -k http://localhost:8090/api/v1/node-info | jq .data.syncProgress.quantity)
    if [[ $PERC == "100" ]]
    then
      echo Blockchain Synced: $PERC%
      SYNCED=100
    elif [[ $SYNCED -ge 10 ]]
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
      # TODO: switch to https when wallet-debug is removed
      curl -k -X POST http://localhost:8090/api/wallets/keys -H 'cache-control: no-cache' -H 'content-type: application/json' -d "\"${stateDir}/genesis-keys/generated-keys/poor/key$i.sk\"" | jq .
  done
  ${ifKeepAlive ''
    sleep infinity
  ''}
''
