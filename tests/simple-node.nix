import <nixpkgs/nixos/tests/make-test.nix> ({ pkgs, ... }:
let
  iohk-nixops = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "iohk-nixops";
    rev = "88c2b0b05c993287988255d0f32c6e13aad74f1c";
    sha256 = "03vyxxb608nds10c0vhjr1a42dqvsm8mip12dcfin0jgnwxl5ssc";
  };
  genesis = (import ../default.nix { inherit pkgs; }).make-genesis;
  mkMachine = index: { config, pkgs, ... }: {
    imports = [ ../nixos/cardano-node.nix ];
    services.cardano-node = {
      enable = true;
      nodeIndex = index;
      executable = "${(import ../. {}).testjob}/bin/cardano-node-simple";
      autoStart = true;
      #initialPeers = [];
      initialKademliaPeers = [];
      genesisN = 6;
      enableP2P = true;
      type = "core";
      nodeName = "node${toString index}";
      productionMode = true;
      systemStart = 1501546200; # 2017-08-01 00:10:00
    };
    virtualisation.qemu.options = [ "-rtc base='2017-08-01'" ];
    boot.kernelParams = [ "quiet" ];
    systemd.services.cardano-node.preStart = ''
      cp -v ${genesis}/keys-testnet/rich/testnet${toString index}.key /var/lib/cardano-node/key${toString index}.sk
      ls -ltrh /var/lib/cardano-node/
    '';
  };
in {
  name = "simple-node";
  nodes = {
    node1 = mkMachine 1;
    node2 = mkMachine 2;
    node3 = mkMachine 3;
    node4 = mkMachine 4;
    node5 = mkMachine 5;
  };
  testScript = ''
    startAll
    $node1->waitForUnit("cardano-node.service");
    # TODO, implement sd_notify?
    $node1->waitForOpenPort(3000);
  '';
})
