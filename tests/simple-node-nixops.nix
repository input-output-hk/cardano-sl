{ pkgs, ... }:
let
  iohk-nixops = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "iohk-nixops";
    rev = "1777ae1edcfd64696e72e35de9c4b3bc947ba43e";
    sha256 = "0xy6m1lgrwbn1vnnzgrn7fpccvfzf9l3fnrzc11bpvcag6rkyzbd";
  };
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
}
