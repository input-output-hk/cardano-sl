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
, ekgListen ? "127.0.0.1:8000"
}:

with localLib;

# TODO: DEVOPS-159: relays DNS should be more predictable
# TODO: DEVOPS-499: developer clusters based on runtime JSON
# TODO: DEVOPS-462: exchanges should use a different topology

let
  environments = {
    mainnet = {
      relays = "relays.cardano-mainnet.iohk.io";
      confKey = "mainnet_full";
    };
    mainnet-staging = {
      relays = "relays.awstest.iohkdev.io";
      confKey = "mainnet_dryrun_full";
    };
  };
  executables =  {
    wallet = "${iohkPkgs.cardano-sl-wallet}/bin/cardano-node";
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
    cp -vi ${iohkPkgs.cardano-sl.src + "/../scripts/log-templates/log-config-qa.yaml"} log-config-qa.yaml
    cp -vi ${if topologyFile != null then topologyFile else topologyFileDefault } topology.yaml
  '';
in pkgs.writeScript "${executable}-connect-to-${environment}" ''
  #!${pkgs.stdenv.shell}

  if [[ "$1" == "--delete-state" ]]; then
    echo "Deleting ${stateDir} ... "
    rm -Rf ${stateDir}
  fi

  echo "Keeping state in ${stateDir}"
  mkdir -p ${stateDir}/logs

  echo "Launching a node connected to '${environment}' ..."
  ${ifWallet ''
  if [ ! -d ${stateDir}/tls ]; then
    mkdir ${stateDir}/tls/
    ${pkgs.openssl}/bin/openssl req -x509 -newkey rsa:2048 -keyout ${stateDir}/tls/server.key -out ${stateDir}/tls/server.cert -days 3650 -nodes -subj "/CN=localhost"
  fi
  ''}


  ${executables.${executable}}                                     \
    --no-ntp                                                       \
    --configuration-file ${configFiles}/configuration.yaml         \
    --configuration-key ${environments.${environment}.confKey}     \
    ${ ifWallet "--web"}                                           \
    ${ ifWallet "--tlscert ${stateDir}/tls/server.cert"}           \
    ${ ifWallet "--tlskey ${stateDir}/tls/server.key"}             \
    ${ ifWallet "--tlsca ${stateDir}/tls/server.cert"}             \
    --log-config ${configFiles}/log-config-qa.yaml                 \
    --topology "${configFiles}/topology.yaml"                      \
    --logs-prefix "${stateDir}/logs"                               \
    --db-path "${stateDir}/db"                                     \
    ${ ifWallet "--wallet-db-path '${stateDir}/wallet-db'"}        \
    --keyfile ${stateDir}/secret.key                               \
    ${ ifWallet "--wallet-address ${walletListen}" }               \
    --ekg-server ${ekgListen} --metrics +RTS -T -RTS
''
