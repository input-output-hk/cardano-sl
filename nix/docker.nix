{ writeScriptBin, dockerTools

, glibcLocales, iana-etc, openssl, bashInteractive, coreutils, utillinux, iproute, iputils, curl, socat
, cardano-sl-node-static, cardano-wallet-static, cardano-sl-explorer-static
, cardano-sl-config, version

# Connect script function.
, connect
# Additional arguments to supply to connect script function.
, connectArgs ? {}

# Used to generate the docker image names
, repoName ? "cardano-sl"

# Configure start script to use this network.
, environment ? "mainnet"

# Specialize start script command to one of
# wallet (default if null), explorer, node.
, type ? null
}:

assert builtins.elem type [ null "wallet" "explorer" "node" ];

with import ../lib.nix;

let
  useConfigVolume = environment == "demo";
  connectToCluster = connect ({
    inherit environment;
    stateDir = "/wallet/${environment}";
    walletListen = "0.0.0.0:8090";
    walletDocListen = "0.0.0.0:8091";
    ekgListen = "0.0.0.0:8000";
  } // optionalAttrs useConfigVolume {
    topologyFile = "/config/topology.yaml";
  } // connectArgs);

  startScriptConnect = name: args: writeScriptBin "cardano-start-${name}" ''
    #!/bin/sh
    set -euo pipefail
    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    if [ ! -d /wallet ]; then
      echo '/wallet volume not mounted, you need to create one with `docker volume create` and pass the correct -v flag to `docker run`'
      exit 1
    fi

    ${optionalString useConfigVolume ''
    if [ ! -f /config/topology.yaml ]; then
      echo '/config/topology.yaml does not exist.'
      echo 'You need to bind-mount a config directory to'
      echo 'the /config volume (the -v flag to `docker run`)'
      exit 2
    fi
    ''}

    cd /wallet
    exec ${connectToCluster.override args}${optionalString useConfigVolume " --runtime-args \"$RUNTIME_ARGS\""}
  '';
  confKey = environments.${environment}.confKey;

  startScriptNode = writeScriptBin "cardano-start-node" ''
    #!/bin/sh
    set -euo pipefail

    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    if [ ! -d /state ]; then
      echo '/state volume not mounted, you need to create one with `docker volume create` and pass the correct -v flag to `docker run`'
    exit 1

    cd /state
    node_args="--db-path /state/db --rebuild-db --listen 0.0.0.0:3000 --json-log /state/logs/node.json --logs-prefix /state/logs --log-config /state/logs/log-config-node.yaml --metrics +RTS -N2 -qg -A1m -I0 -T -RTS --configuration-file ${cardano-sl-config}/lib/configuration.yaml --configuration-key ${confKey}"

    exec ${cardano-sl-node-static}/bin/cardano-node-simple $node_args "$@"
  '';

  # Layer of tools which aren't going to change much between versions.
  baseImage = dockerTools.buildImage {
    name = "${repoName}-env";
    contents = [ iana-etc openssl bashInteractive coreutils utillinux iproute iputils curl socat ];
  };

  # Adds cardano-sl packages to image: core node, wallet, explorer, config.
  cardanoSLImage = dockerTools.buildImage {
    name = repoName;
    tag = version;
    fromImage = baseImage;
    contents = [
      cardano-wallet-static
      cardano-sl-node-static
      cardano-sl-explorer-static
      cardano-sl-config
    ];
  };

  # Adds start scripts for the environment.
  environmentImage = dockerTools.buildImage {
    name = repoName;
    tag = "${version}-${environment}";
    fromImage = cardanoSLImage;
    contents = [
      (startScriptConnect "wallet" { executable = "wallet"; })
      (startScriptConnect "explorer" { executable = "explorer"; })
      startScriptNode
    ];
    config = mkConfig null;
  };

  mkConfig = type: {
    Cmd = [ "cardano-start-${if (type != null) then type else "wallet"}" ];
    ExposedPorts = {
      "3000/tcp" = {}; # protocol
      "8090/tcp" = {}; # wallet
      "8091/tcp" = {}; # wallet doc
      "8100/tcp" = {}; # explorer api
      "8000/tcp" = {}; # ekg
    };
  } // optionalAttrs useConfigVolume {
    Env = [ "RUNTIME_ARGS=" ];
  };

in
  # Final image, possibly specialized to a certain node type.
  dockerTools.buildImage {
    name = repoName;
    tag = "${version}-${environment}${if (type != null) then "-" + type else ""}";
    fromImage = environmentImage;
    config = mkConfig type;
  } // { inherit version; }
