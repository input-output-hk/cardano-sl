{ environment ? "mainnet"
, localLib ? import ./../../../lib.nix
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "state-${executable}-${environment}"
, config ? {}
, executable ? "wallet"
, topologyFile ? null
, system ? builtins.currentSystem
, pkgs ? import localLib.fetchNixPkgs { inherit system config; }
, gitrev ? localLib.commitIdFromGitRepo ./../../../.git
, walletListen ? "localhost:8090"
, walletDocListen ? "localhost:8091"
, walletExternalIPAddress ? builtins.elemAt (localLib.splitString ":" walletListen) 0
, ekgListen ? "localhost:8000"
, ghcRuntimeArgs ? "-N2 -qg -A1m -I0 -T"
, additionalNodeArgs ? ""
, confFile ? null
, confKey ? null
, relays ? null
, debug ? false
, disableClientAuth ? false
, extraParams ? ""
, tlsConfig ? null
}:

with localLib;

# TODO: DEVOPS-159: relays DNS should be more predictable
# TODO: DEVOPS-499: developer clusters based on runtime JSON
# TODO: DEVOPS-462: exchanges should use a different topology

let
  ifDebug = localLib.optionalString (debug);
  ifDisableClientAuth = localLib.optionalString (disableClientAuth);
  environments = {
    mainnet = {
      relays = "relays.cardano-mainnet.iohk.io";
      confKey = "mainnet_full";
    };
    mainnet-staging = {
      relays = "relays.awstest.iohkdev.io";
      confKey = "mainnet_dryrun_full";
    };
    testnet = {
      relays = "relays.cardano-testnet.iohkdev.io";
      confKey = "testnet_full";
    };
    demo = {
      confKey = "dev";
      relays = "127.0.0.1";
    };
    override = {
      inherit relays confKey confFile;
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
  configFiles = iohkPkgs.cardano-sl-config;
  tlsConfigResultant = {
    organization     = "Company Name";

    caCommonName     = "Company Name Self-Signed Root CA";
    caEexpiryDays    = 3650;

    serverCommonName = "Company Name Wallet Node";
    serverExpiryDays = 365;
    serverAltDNS     = [
      "localhost"
      "localhost.localdomain"
      "127.0.0.1"
      "::1"
    ];
    serverAltDNSExtra = [];

    clientCommonName = "Company Name Wallet Node Client";
    clientExpiryDays = 365;
  } // tlsConfig;
  tlsConfigFile = let cfg = tlsConfigResultant; in pkgs.writeText "tls-config-${environment}.yaml" (''
    ${environments.${environment}.confKey}:
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
  configurationArgs = pkgs.lib.concatStringsSep " " [
    "--configuration-file ${environments.${environment}.confFile or "${configFiles}/lib/configuration.yaml"}"
    "--configuration-key ${environments.${environment}.confKey}"
  ];
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
      --configuration-file ${tlsConfigFile}                      \
      --configuration-key ${environments.${environment}.confKey}
  fi
  ''}

  exec ${executables.${executable}}                                \
    ${configurationArgs}                                           \
    ${ ifWallet "--tlscert ${stateDir}/tls/server/server.crt"}     \
    ${ ifWallet "--tlskey ${stateDir}/tls/server/server.key"}      \
    ${ ifWallet "--tlsca ${stateDir}/tls/server/ca.crt"}           \
    --log-config ${configFiles}/log-configs/connect-to-cluster.yaml \
    --topology "${if topologyFile != null then topologyFile else topologyFileDefault}" \
    --logs-prefix "${stateDir}/logs"                               \
    --db-path "${stateDir}/db"   ${extraParams}                    \
    ${ ifWallet "--wallet-db-path '${stateDir}/wallet-db'"}        \
    ${ ifDebug "--wallet-debug"}                                   \
    ${ ifDisableClientAuth "--no-client-auth"}                     \
    --keyfile ${stateDir}/secret.key                               \
    ${ ifWallet "--wallet-address ${walletListen}" }               \
    ${ ifWallet "--wallet-doc-address ${walletDocListen}" }        \
    --ekg-server ${ekgListen} --metrics                            \
    +RTS ${ghcRuntimeArgs} -RTS                                    \
    ${additionalNodeArgs}                                          \
    $RUNTIME_ARGS
'' // { inherit walletListen walletDocListen ekgListen; }
