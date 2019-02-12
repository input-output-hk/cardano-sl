with import ../../../lib.nix;

{ stdenv, writeText, writeScript, curl

, cardano-wallet-static, cardano-sl-explorer-static, cardano-sl-tools-static
, cardano-sl-config

## options!
, environment ? "mainnet"
, stateDir ? maybeEnv "CARDANO_STATE_DIR" "state-${executable}-${environment}"
, executable ? "wallet"
, topologyFile ? null
, walletListen ? "127.0.0.1:8090"
, walletDocListen ? "127.0.0.1:8091"
, ekgListen ? "127.0.0.1:8000"
, ghcRuntimeArgs ? "-N2 -qg -A1m -I0 -T"
, additionalNodeArgs ? ""
, confFile ? null
, confKey ? null
, relays ? null
, debug ? false
, disableClientAuth ? false
, useLegacyDataLayer ? false
, extraParams ? ""
, useStackBinaries ? false
, tlsConfig ? {}
}:

# TODO: DEVOPS-159: relays DNS should be more predictable
# TODO: DEVOPS-499: developer clusters based on runtime JSON
# TODO: DEVOPS-462: exchanges should use a different topology

let
  ifDebug = optionalString (debug);
  ifDisableClientAuth = optionalString (disableClientAuth);
  walletDataLayer = if useLegacyDataLayer then "--legacy-wallet" else "";
  env = if environment == "override"
    then { inherit relays confKey confFile; }
    else environments.${environment};
  executables =  {
    wallet = if useStackBinaries then "stack exec -- cardano-node" else "${cardano-wallet-static}/bin/cardano-node";
    explorer = if useStackBinaries then "stack exec -- cardano-explorer" else "${cardano-sl-explorer-static}/bin/cardano-explorer";
    x509gen = if useStackBinaries then "stack exec -- cardano-x509-certificates" else "${cardano-sl-tools-static}/bin/cardano-x509-certificates";
  };
  ifWallet = optionalString (executable == "wallet");

  topologyFileDefault = writeText "topology-${environment}" ''
    wallet:
      relays: [[{ host: ${env.relays} }]]
      valency: 1
      fallbacks: 7
  '';
  configurationArgs = concatStringsSep " " [
    "--configuration-file ${env.confFile or "${cardano-sl-config}/lib/configuration.yaml"}"
    "--configuration-key ${env.confKey}"
  ];

  curlScript = writeScript "curl-wallet-${environment}" ''
    #!${stdenv.shell}

    request_path=$1
    shift

    if [ -z "$request_path" ]; then
    >&2 cat <<EOF
    usage: ${stateDir}/curl REQUEST_PATH [ OPTIONS ]

    Wrapper script to make HTTP requests to the wallet.
    All the normal curl options apply.

    Examples:
      ${stateDir}/curl api/v1/node-info -i
      ${stateDir}/curl api/v1/wallets -d '{ ... }' | jq .

    EOF
    fi

    exec ${curl}/bin/curl --silent                       \
      --cacert ${stateDir}/tls/client/ca.crt             \
      --cert ${stateDir}/tls/client/client.pem           \
      -H 'cache-control: no-cache'                       \
      -H "Accept: application/json; charset=utf-8"       \
      -H "Content-Type: application/json; charset=utf-8" \
      "https://${walletListen}/$request_path" "$@"
  '';
  tlsConfigResultant = {
    organization     = "Input Output HK";

    caCommonName     = "Cardano SL Self-Signed Root CA";
    caEexpiryDays    = 3650;

    serverCommonName = "Cardano SL Server Node";
    serverExpiryDays = 365;
    serverAltDNS     = [
      "localhost"
      "localhost.localdomain"
      "127.0.0.1"
      "::1"
    ];
    serverAltDNSExtra = [];

    clientCommonName = "Daedalus Wallet";
    clientExpiryDays = 365;
  } // tlsConfig;
  tlsConfigFile = let cfg = tlsConfigResultant; in writeText "tls-config-${environment}.yaml" (''
    ${env.confKey}:
      tls:
        ca:
          organization: ${cfg.organization}
          commonName: ${cfg.caCommonName}
          expiryDays: ${toString cfg.caEexpiryDays}

        server:
          organization: ${cfg.organization}
          commonName: ${cfg.serverCommonName}
          expiryDays: ${toString cfg.serverExpiryDays}
          altDNS:
          '' +
          (let sep = "        - "; in sep + (concatStringsSep ("\n" + sep) (cfg.serverAltDNS ++ cfg.serverAltDNSExtra)) + "\n")
          + ''
    ####
        clients:
          - organization: ${cfg.organization}
            commonName: ${cfg.clientCommonName}
            expiryDays: ${toString cfg.clientExpiryDays}
  '');

in writeScript "${executable}-connect-to-${environment}" ''
  #!${stdenv.shell}

  set -euo pipefail

  if [[ "''${1-}" == "--delete-state" ]]; then
    echo "Deleting ${stateDir} ... "
    rm -Rf ${stateDir}
    shift
  fi
  if [[ "''${1-}" == "--runtime-args" ]]; then
    RUNTIME_ARGS="''${2-}"
    shift 2
  else
    RUNTIME_ARGS=""
  fi

  echo "Keeping state in ${stateDir}"
  mkdir -p ${stateDir}/logs

  echo "Launching a node connected to '${environment}' ..."
  ${ifWallet ''
  ${utf8LocaleSetting}
  if [ ! -d ${stateDir}/tls ]; then
    mkdir -p ${stateDir}/tls/server && mkdir -p ${stateDir}/tls/client
    ${executables.x509gen}                                       \
      --server-out-dir ${stateDir}/tls/server                    \
      --clients-out-dir ${stateDir}/tls/client                   \
      --configuration-file ${tlsConfigFile}                      \
      --configuration-key ${env.confKey}
  fi
  ln -sf ${curlScript} ${stateDir}/curl
  ''}

  exec ${executables.${executable}}                                                    \
    ${configurationArgs}                                                               \
    ${ ifWallet "--tlscert ${stateDir}/tls/server/server.crt"}                         \
    ${ ifWallet "--tlskey ${stateDir}/tls/server/server.key"}                          \
    ${ ifWallet "--tlsca ${stateDir}/tls/server/ca.crt"}                               \
    --log-config ${cardano-sl-config}/log-configs/connect-to-cluster.yaml              \
    --topology "${if topologyFile != null then topologyFile else topologyFileDefault}" \
    --logs-prefix "${stateDir}/logs"                                                   \
    --db-path "${stateDir}/db"   ${extraParams}                                        \
    ${ ifWallet "--wallet-db-path '${stateDir}/wallet-db' ${walletDataLayer}"}         \
    ${ ifDebug "--wallet-debug"}                                                       \
    ${ ifDisableClientAuth "--no-client-auth"}                                         \
    --keyfile ${stateDir}/secret.key                                                   \
    ${ ifWallet "--wallet-address ${walletListen}" }                                   \
    ${ ifWallet "--wallet-doc-address ${walletDocListen}" }                            \
    --ekg-server ${ekgListen} --metrics                                                \
    +RTS ${ghcRuntimeArgs} -RTS                                                        \
    ${additionalNodeArgs}                                                              \
    $RUNTIME_ARGS
'' // { inherit walletListen walletDocListen ekgListen; }
