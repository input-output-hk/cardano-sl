{ writeScriptBin, dockerTools

, glibcLocales, iana-etc, openssl, bashInteractive, coreutils, utillinux, iproute, iputils, curl, socat
, cardano-node-simple, cardanoConfig

, environment ? "mainnet"
, type ? null
, connect
, connectArgs ? {}
, version ? "unstable"
}:

# assertOneOf "type" type [ "wallet" "explorer" "node" ];

let
  localLib = import ../lib.nix;
  connectToCluster = connect ({
    inherit environment;
    stateDir = "/wallet/${environment}";
    walletListen = "0.0.0.0:8090";
    walletDocListen = "0.0.0.0:8091";
    ekgListen = "0.0.0.0:8000";
  } // connectArgs);

  startScriptConnect = name: args: writeScriptBin "cardano-start-${name}" ''
    #!/bin/sh
    set -euo pipefail
    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    if [ ! -d /wallet ]; then
      echo '/wallet volume not mounted, you need to create one with `docker volume create` and pass the correct -v flag to `docker run`'
    exit 1
    fi
    cd /wallet
    exec ${connectToCluster.override args}
  '';
  confKey = localLib.environments.${environment}.confKey;
  startScripts = [
    (startScriptConnect "wallet" {})
    (startScriptConnect "explorer" { executable = "explorer"; })
    (writeScriptBin "cardano-start-node" ''
      #!/bin/sh
      set -euo pipefail

      export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
      if [ ! -d /state ]; then
        echo '/state volume not mounted, you need to create one with `docker volume create` and pass the correct -v flag to `docker run`'
      exit 1

      cd /state
      node_args="--db-path /state/db --rebuild-db --listen 0.0.0.0:3000 --json-log /state/logs/node.json --logs-prefix /state/logs --log-config /state/logs/log-config-node.yaml --metrics +RTS -N2 -qg -A1m -I0 -T -RTS --configuration-file ${cardanoConfig}/lib/configuration.yaml --configuration-key ${confKey}"

      exec ${cardano-node-simple}/bin/cardano-node-simple $node_args "$@"
    '')
  ];

in dockerTools.buildImage {
  name = "cardano-sl";
  tag = "${version}${if (type != null) then "-" + type else ""}-${environment}";
  contents = [ iana-etc openssl bashInteractive coreutils utillinux iproute iputils curl socat ]
   ++ startScripts;
  config = {
    Cmd = [ "cardano-start-${if (type != null) then type else "wallet"}" ];
    ExposedPorts = {
      "3000/tcp" = {}; # protocol
      "8090/tcp" = {}; # wallet
      "8091/tcp" = {}; # wallet doc
      "8100/tcp" = {}; # explorer api
      "8000/tcp" = {}; # ekg
    };
  };
  created = "2019-07-11T00:00:00Z";
  extraCommands = ''
  '';
}
