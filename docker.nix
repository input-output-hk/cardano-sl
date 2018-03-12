{ environment ? "mainnet"
, connect
, gitrev
, pkgs
, connectArgs ? {}
}:

with pkgs.lib;

let
  connectToCluster = connect ({
    inherit gitrev environment;
    stateDir = "/wallet/${environment}";
    walletListen = "0.0.0.0:8090";
    ekgListen = "0.0.0.0:8000";
  } // connectArgs);
  startScript = pkgs.writeScriptBin "cardano-start" ''
    #!/bin/sh
    set -e
    set -o pipefail
    if [ ! -d /wallet ]; then
      echo '/wallet volume not mounted, you need to create one with `docker volume create` and pass the correct -v flag to `docker run`'
    exit 1
    fi
    cd /wallet
    exec ${connectToCluster}
  '';
in pkgs.dockerTools.buildImage {
  name = "cardano-container-${environment}";
  contents = with pkgs; [ iana-etc startScript openssl bashInteractive coreutils utillinux iproute iputils curl socat ];
  config = {
    Cmd = [ "cardano-start" ];
    ExposedPorts = {
      "3000/tcp" = {}; # protocol
      "8090/tcp" = {}; # wallet
      "8100/tcp" = {}; # explorer api
      "8000/tcp" = {}; # ekg
    };
  };
}
