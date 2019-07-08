{ runCommand, yamlValidation }:

runCommand "yamlValidation" {
  logConfigs = [
    ../../log-configs/daedalus.yaml
    #../../log-configs/greppable.yaml
    ../../log-configs/connect-to-cluster.yaml
    ../../log-configs/cluster.yaml
    ../../log-configs/template-demo.yaml
  ];
  topologyConfigs = [
    ../../docs/network/example-topologies/mainnet-staging.yaml
    ../../docs/network/example-topologies/behind-nat-no-dns.yaml
    ../../docs/network/example-topologies/behind-nat-with-dns.yaml
    ../../docs/network/example-topologies/p2p.yaml
    ../../docs/network/example-topologies/static-no-dns.yaml
    ../../docs/network/example-topologies/static-with-dns.yaml
    ../../docs/network/example-topologies/traditional.yaml
  ];
} ''
  ${yamlValidation}/bin/yamlValidation
''
