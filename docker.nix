{ environment ? "mainnet", connect, gitrev, pkgs }:

with pkgs.lib;
let
  connectToCluster = connect {
    inherit gitrev environment;
    stateDir = "/wallet/${environment}";
    walletListen = "0.0.0.0:8090";
  };
  startScript = pkgs.writeScriptBin "cardano-start" ''
    #!/bin/sh
    set -e
    set -o pipefail
    if [ ! -d /wallet ]; then
      echo '/wallet volume not mounted, you need to create one with `docker volume create` and pass the correct -v flag to `docker run`'
    exit 1
    fi
    exec ${connectToCluster}
  '';
in pkgs.dockerTools.buildImage {
  name = "cardano-container-${environment}-1.0";
  contents = [ pkgs.iana-etc startScript pkgs.openssl ] ++ optional true (with pkgs; [ bashInteractive coreutils utillinux iproute iputils curl socat ]);
  config = {
    Cmd = [ "cardano-start" ];
    ExposedPorts = {
      "3000/tcp" = {};
      "8090/tcp" = {};
    };
  };
}
