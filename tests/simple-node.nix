import <nixpkgs/nixos/tests/make-test.nix> ({ pkgs, ... }:
let
  iohk-nixops = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "iohk-nixops";
    rev = "88c2b0b05c993287988255d0f32c6e13aad74f1c";
    sha256 = "03vyxxb608nds10c0vhjr1a42dqvsm8mip12dcfin0jgnwxl5ssc";
  };
  genesis = (import ../default.nix { inherit pkgs; }).make-genesis;
in {
  name = "simple-node";
  nodes = {
    machine = { config, pkgs, ... }: {
      imports = [ ../nixos/cardano-node.nix ];
      services.cardano-node = {
        enable = true;
        nodeIndex = 0;
        executable = "${(import ../. {}).testjob}/bin/cardano-node-simple";
        autoStart = true;
        #initialPeers = [];
        initialKademliaPeers = [];
        genesisN = 6;
        enableP2P = true;
        type = "core";
        nodeName = "node0";
        productionMode = true;
      };
      systemd.services.cardano-node.preStart = ''
        cp -vi ${genesis}/keys-testnet/rich/testnet1.key /var/lib/cardano-node/key0.sk
        ls -ltrh /var/lib/cardano-node/
      '';
    };
  };
  testScript = ''
    startAll
    $machine->waitForUnit("cardano-node.service");
    # TODO, implement sd_notify?
    $machine->waitForOpenPort(3000);
  '';
})
