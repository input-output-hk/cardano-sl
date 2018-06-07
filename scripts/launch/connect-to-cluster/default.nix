{ environment ? "mainnet"
, localLib ? import ./../../../lib.nix
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "state-${executable}-${environment}"
, config ? {}
, executable ? "wallet"
, topologyFile ? null
, system ? builtins.currentSystem
, pkgs ? import localLib.fetchNixPkgs { inherit system config; }
, gitrev ? localLib.commitIdFromGitRepo ./../../../.git
, walletListen ? "127.0.0.1:8090"
, walletDocListen ? "127.0.0.1:8091"
, ekgListen ? "127.0.0.1:8000"
, ghcRuntimeArgs ? "-N2 -qg -A1m -I0 -T"
, additionalNodeArgs ? ""
, confKey ? null
, relays ? null
, debug ? false
, extraParams ? ""
}:

with localLib;

# TODO: DEVOPS-159: relays DNS should be more predictable
# TODO: DEVOPS-499: developer clusters based on runtime JSON
# TODO: DEVOPS-462: exchanges should use a different topology

let
  ifDebug = localLib.optionalString (debug);
  environments = {
    mainnet = {
      relays = "relays.cardano-mainnet.iohk.io";
      confKey = "mainnet_full";
    };
    mainnet-staging = {
      relays = "relays.awstest.iohkdev.io";
      confKey = "mainnet_dryrun_full";
    };
    demo = {
      confKey = "dev";
      relays = "127.0.0.1";
    };
    override = {
      inherit relays confKey;
    };
  };
  executables =  {
    wallet = "${iohkPkgs.cardano-sl-wallet-new}/bin/cardano-node";
    explorer = "${iohkPkgs.cardano-sl-explorer-static}/bin/cardano-explorer";
  };
  ifWallet = localLib.optionalString (executable == "wallet");
  iohkPkgs = import ./../../../default.nix { inherit config system pkgs gitrev; };
  src = ./../../../.;
  topologyFileDefault = pkgs.writeText "topology-${environment}" ''
    wallet:
      relays: [[{ host: ${environments.${environment}.relays} }]]
      valency: 1
      fallbacks: 7
  '';
  configFiles = pkgs.runCommand "cardano-config" {} ''
    mkdir -pv $out
    cd $out
    cp -vi ${iohkPkgs.cardano-sl.src + "/configuration.yaml"} configuration.yaml
    cp -vi ${iohkPkgs.cardano-sl.src + "/mainnet-genesis-dryrun-with-stakeholders.json"} mainnet-genesis-dryrun-with-stakeholders.json
    cp -vi ${iohkPkgs.cardano-sl.src + "/mainnet-genesis.json"} mainnet-genesis.json
    cp -vi ${iohkPkgs.srcroot + "/log-configs/connect-to-cluster.yaml"} log-config-connect-to-cluster.yaml
    cp -vi ${if topologyFile != null then topologyFile else topologyFileDefault } topology.yaml
  '';
in pkgs.writeScript "${executable}-connect-to-${environment}" ''
  #!${pkgs.stdenv.shell} -e

  if [[ "$1" == "--delete-state" ]]; then
    echo "Deleting ${stateDir} ... "
    rm -Rf ${stateDir}
    shift
  fi
  if [[ "$1" == "--runtime-args" ]]; then
    RUNTIME_ARGS=$2
    shift 2
  else
    RUNTIME_ARGS=""
  fi

  echo "Keeping state in ${stateDir}"
  mkdir -p ${stateDir}/logs

  echo "Launching a node connected to '${environment}' ..."
  ${ifWallet ''
  export LC_ALL=en_GB.UTF-8
  export LANG=en_GB.UTF-8
  if [ ! -d ${stateDir}/tls ]; then
    mkdir -p ${stateDir}/tls/server && mkdir -p ${stateDir}/tls/client
    ${iohkPkgs.cardano-sl-tools}/bin/cardano-x509-certificates   \
      --server-out-dir ${stateDir}/tls/server                    \
      --clients-out-dir ${stateDir}/tls/client                   \
      --configuration-key ${environments.${environment}.confKey} \
      --configuration-file ${configFiles}/configuration.yaml
  fi
  ''}

  ${executables.${executable}}                                     \
    --configuration-file ${configFiles}/configuration.yaml         \
    --configuration-key ${environments.${environment}.confKey}     \
    ${ ifWallet "--tlscert ${stateDir}/tls/server/server.crt"}     \
    ${ ifWallet "--tlskey ${stateDir}/tls/server/server.key"}      \
    ${ ifWallet "--tlsca ${stateDir}/tls/server/ca.crt"}           \
    --log-config ${configFiles}/log-config-connect-to-cluster.yaml \
    --topology "${configFiles}/topology.yaml"                      \
    --logs-prefix "${stateDir}/logs"                               \
    --db-path "${stateDir}/db"   ${extraParams}                    \
    ${ ifWallet "--wallet-db-path '${stateDir}/wallet-db'"}        \
    ${ ifDebug "--wallet-debug"}                                   \
    --keyfile ${stateDir}/secret.key                               \
    ${ ifWallet "--wallet-address ${walletListen}" }               \
    ${ ifWallet "--wallet-doc-address ${walletDocListen}" }        \
    --ekg-server ${ekgListen} --metrics                            \
    +RTS ${ghcRuntimeArgs} -RTS                                    \
    ${additionalNodeArgs}                                          \
    $RUNTIME_ARGS
''
