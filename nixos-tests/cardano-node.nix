let
  localLib = import ../lib.nix;
  system = builtins.currentSystem;
  pkgs = import (localLib.fetchNixPkgs) { inherit system config; };
  config = {};
  cardano_sl = pkgs.callPackage ../default.nix { gitrev = "3344a1eb7"; };
  cardano_connect_scripts = cardano_sl.connectScripts;
  cardano_staging_wallet = cardano_connect_scripts.stagingWallet;
in
import <nixpkgs/nixos/tests/make-test.nix> ({ pkgs, ... }: {
  name = "cardano-node";

  nodes.server = { config, pkgs, ... }: {
    systemd.services.cardano_node = {
      description = "Cardano Node";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = cardano_staging_wallet;
      };
    };
  };

  testScript = ''
    $server->waitForOpenPort(8090);
    $server->waitForUnit("cardano_node");
    $server->succeed("${pkgs.curl}/bin/curl -f -k https://127.0.0.1:8090/docs/v1/index/");
    $server->succeed("${pkgs.curl}/bin/curl -f -k https://127.0.0.1:8090/api/info");
    $server->succeed("${pkgs.curl}/bin/curl -f -k https://127.0.0.1:8090/api/v1/node-info");
  '';
})
