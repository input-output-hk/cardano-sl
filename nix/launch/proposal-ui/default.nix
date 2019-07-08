with import ../../../lib.nix;

{ stdenv, writeText, writeScript, curl
, testcases
, cardanoConfig

## options!
, environment ? "mainnet"
, stateDir ? maybeEnv "CARDANO_STATE_DIR" "state-proposal-gui-${environment}"
, topologyFile ? null
, confFile ? null
, confKey ? null
, relays ? null
, extraParams ? ""
}:

# TODO: DEVOPS-159: relays DNS should be more predictable
# TODO: DEVOPS-499: developer clusters based on runtime JSON
# TODO: DEVOPS-462: exchanges should use a different topology

let
  env = if environment == "override"
    then { inherit relays confKey confFile; }
    else environments.${environment};
  topologyFileDefault = writeText "topology-${environment}" ''
    wallet:
      relays: [[{ host: ${env.relays} }]]
      valency: 1
      fallbacks: 7
  '';
  configurationArgs = concatStringsSep " " [
    "--configuration-file ${env.confFile or "${cardanoConfig}/lib/configuration.yaml"}"
    "--configuration-key ${env.confKey}"
  ];

in writeScript "proposal-ui-${environment}" ''
  #!${stdenv.shell}

  set -euo pipefail

  if [[ "''${1-}" == "--delete-state" ]]; then
    echo "Deleting ${stateDir} ... "
    rm -Rf ${stateDir}
    shift
  fi

  echo "Keeping state in ${stateDir}"
  mkdir -pv ${stateDir}
  pushd ${stateDir}

  echo "Launching a proposal gui connected to '${environment}' ..."
  SCRIPT=none exec ${testcases}/bin/testcases \
    ${configurationArgs}                                                               \
    --log-console-off                                                                  \
    --db-path db                                                                       \
    --keyfile secret.key                                                               \
    --log-config ${cardanoConfig}/log-configs/connect-to-cluster.yaml                  \
    --logs-prefix logs                                                                 \
    --topology "${if topologyFile != null then topologyFile else topologyFileDefault}" \
''
