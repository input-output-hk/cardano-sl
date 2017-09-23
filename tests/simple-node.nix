{ pkgs, ... }:
let
  iohk-nixops = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "iohk-nixops";
    rev = "88c2b0b05c993287988255d0f32c6e13aad74f1c";
    sha256 = "03vyxxb608nds10c0vhjr1a42dqvsm8mip12dcfin0jgnwxl5ssc";
  };
  mkNode = index: peers: {
    type = "core";
    region = "";
    static-routes = peers; # list of lists of names
    host = "node${toString index}.cardano";
  };
  topology = {
    nodes = {
      node1 = mkNode 1 [ [ "node5" ] [ "node2" ] ];
      node2 = mkNode 2 [ [ "node1" ] [ "node3" ] ];
      node3 = mkNode 3 [ [ "node2" ] [ "node4" ] ];
      node4 = mkNode 4 [ [ "node3" ] [ "node5" ] ];
      node5 = mkNode 5 [ [ "node4" ] [ "node1" ] ];
    };
  };
  topologyFile = pkgs.writeText "topology.json" (builtins.toJSON topology);
  cardanoPkgs = import ../. { inherit pkgs; };
  genesis = cardanoPkgs.make-genesis;
  mkMachine = index: { config, pkgs, ... }: {
    imports = [ ../nixos/cardano-node.nix ];
    services.dnsmasq.enable = true;
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
      systemStart = 1501545900; # 2017-08-01 00:05:00
      topologyFile = "${topologyFile}";
      publicIP = "192.168.1.${toString index}";
      statsdServer = "127.0.0.1:8125";
      autoRestart = false;
    };
    networking.firewall.enable = false;
    networking.extraHosts = ''
      192.168.1.1 node1.cardano
      192.168.1.2 node2.cardano
      192.168.1.3 node3.cardano
      192.168.1.4 node4.cardano
      192.168.1.5 node5.cardano
    '';
    virtualisation.qemu.options = [
      "-cpu Haswell"
      "-rtc base='2017-08-01'"
      "-net dump,vlan=0,file=$out/capture-0-${toString index}.pcap"
      "-net dump,vlan=1,file=$out/capture-1-${toString index}.pcap"
    ];
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
    my @list = ($node1, $node2, $node3, $node4, $node5);
    my @names = ("node1", "node2", "node3", "node4", "node4");
    my $x;

    startAll;

    foreach $x (@list) {
      $x->waitForUnit("cardano-node.service");
      $x->waitForOpenPort(3000);
    }
    $node1->sleep(600);
    foreach $x (@list) {
      $x->execute("systemctl stop cardano-node --no-block");
    }
    sleep(5);
    foreach $x (@list) {
      $x->execute("journalctl -u cardano-node > /tmp/shared/`hostname`.log");
      $x->execute("cp /var/lib/cardano-node/jsonLog.json /tmp/shared/`hostname`.json");
    }
    system("ls -ltrh xchg-shared");
    system('mkdir $out/logs');
    system('mv -v xchg-shared/node* $out/logs');
    my $out = $ENV{'out'};
    my $fname = "$out/nix-support/hydra-build-products";
    open(my $fh, '>', $fname) or die "unable to open $fname";
    for $x (@names) {
      print $fh "file journal $out/logs/$x.log\n";
    }
    print $fh <<"EOF";
    file pcap $out/capture-1-1.pcap
    file pcap $out/capture-1-2.pcap
    file pcap $out/capture-1-3.pcap
    file pcap $out/capture-1-4.pcap
    file pcap $out/capture-1-5.pcap
    EOF
    system("${cardanoPkgs.cardano-sl-tools}/bin/cardano-post-mortem testPostProcess $out/logs/");
  '';
}
