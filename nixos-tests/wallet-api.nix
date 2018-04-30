let
  localLib = import ../lib.nix;
  system = builtins.currentSystem;
  pkgs = import (localLib.fetchNixPkgs) { inherit system config; };
  config = {};
  cardano_sl = pkgs.callPackage ../default.nix { gitrev = "abcdef"; allowCustomConfig = false; };
in
import (pkgs.path + "/nixos/tests/make-test.nix") ({ pkgs, ... }: {
  name = "cardano-node";

  nodes.server = { config, pkgs, ... }: {
    virtualisation = {
      qemu.options = [
        "-cpu Haswell"
        "-device virtio-rng-pci"
      ];
      memorySize = 2048;
    };
    security.rngd.enable = pkgs.lib.mkForce true;
    systemd.services.cardano_node_default = {
      description = "Cardano Node";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = cardano_sl.connectScripts.stagingWallet;
        Type = "notify";
        NotifyAccess = "all";
        TimeoutStartSec = 600;
      };
    };
    systemd.services.cardano_node_custom_port = {
      description = "Cardano Node";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = cardano_sl.connectScripts.stagingWallet.override ( { walletListen = "127.0.0.1:8091"; ekgListen = "127.0.0.1:8001"; stateDir = "cardano-state-staging-custom-port"; } );
        Type = "notify";
        NotifyAccess = "all";
        TimeoutStartSec = 600;
      };
    };
  };

  testScript = ''
    $server->waitForUnit("cardano_node_default");
    $server->waitForOpenPort(8090);
    $server->succeed("${pkgs.curl}/bin/curl -f -k https://127.0.0.1:8090/docs/v1/index/");
    $server->succeed("${pkgs.curl}/bin/curl -f -k https://127.0.0.1:8090/api/info");
    $server->succeed("${pkgs.curl}/bin/curl -f -k https://127.0.0.1:8090/api/v1/node-info");
    $server->waitForUnit("cardano_node_custom_port");
    $server->waitForOpenPort(8091);
    $server->succeed("${pkgs.curl}/bin/curl -f -k https://127.0.0.1:8091/api/info");
    $server->succeed("${pkgs.curl}/bin/curl -f -k https://127.0.0.1:8091/api/v1/node-info");
  '';
})
