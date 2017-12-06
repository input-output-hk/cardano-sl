{ environment ? "mainnet"
, localLib ? import ./../../../lib.nix
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "state-${executable}-${environment}"
, config ? {}
, executable ? "wallet"
, system ? builtins.currentSystem
, pkgs ? import localLib.fetchNixPkgs { inherit system config; }
, rev ? ""
}:

# TODO: DEVOPS-462: docker to use this script
# TODO: DEVOPS-159: relays should be more predictable 
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
  iohkPkgs = import ./../../../default.nix { inherit config system pkgs; gitrev = rev; };
  src = ./../../../.;
  topologyFile = pkgs.writeText "topology-${environment}" ''
    wallet:
      relays: [[{ host: ${environments.${environment}.relays} }]]
      valency: 1
      fallbacks: 7
  '';
in pkgs.writeScript "${executable}-connect-to-${environment}" ''
  if [[ "$1" == "--delete-state" ]]; then
    echo "Deleting ${stateDir} ... "
    rm -Rf ${stateDir}               
  fi

  echo "Keeping state in ${stateDir}"
  mkdir -p ${stateDir}/logs

  echo "Launching a single node connected to '${environment}' ..."

  ${executables.${executable}}                                     \
    ${ ifWallet "--web"}                                           \
    --no-ntp                                                       \
    --configuration-file ${src}/node/configuration.yaml            \
    --configuration-key ${environments.${environment}.confKey}     \
    ${ ifWallet "--tlscert ${src}/scripts/tls-files/server.crt"}   \
    ${ ifWallet "--tlskey ${src}/scripts/tls-files/server.key"}    \
    ${ ifWallet "--tlsca ${src}/scripts/tls-files/ca.crt"}         \
    --log-config ${src}/scripts/log-templates/log-config-qa.yaml   \
    --topology "${topologyFile}"                                   \
    --logs-prefix "${stateDir}/logs"                               \
    --db-path "${stateDir}/db"                                     \
    ${ ifWallet "--wallet-db-path '${stateDir}/wallet-db'"}        \
    --keyfile ${stateDir}/secret.key                   
''
