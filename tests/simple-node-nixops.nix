import <nixpkgs/nixos/tests/make-test.nix> ({ pkgs, ... }:
let
in {
  name = "boot";
  nodes = {
    machine = { config, pkgs, ... }: {
      imports = [ (import (iohk-nixops + "/modules/cardano-node-config.nix") 0 "") <nixops/nix/options.nix> <nixops/nix/resource.nix> ];
      services.cardano-node = {
        autoStart = true;
        initialPeers = [];
      };
    };
  };
  testScript = ''
    startAll
    $machine->waitForUnit("cardano-node.service");
    # TODO, implement sd_notify?
    $machine->waitForOpenPort(3000);
  '';
})
