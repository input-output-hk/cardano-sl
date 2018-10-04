{ lib, cores, relays }:
with lib;
let
  coreNames = map (index: "core${toString index}") (range 1 cores);
  relayNames = map (index: "relay${toString index}") (range 1 relays);
  peers = node: remove node (coreNames ++ relayNames);
  mkNodeTopology = type: index: let
    offset = if type == "relay" then 100 else 0;
  in rec {
    name = "${type}${toString index}";
    value = {
      region = "undefined";
      type = type;
      addr = "127.0.0.1";
      static-routes = map (x: [ x ]) (peers name);
      port = 3000 + offset + index;
    };
  };
  topology = {
    nodes = builtins.listToAttrs ((map (mkNodeTopology "core") (range 1 cores)) ++ (map (mkNodeTopology "relay") (range 1 relays)));
  };
  topologyFile = builtins.toFile "topology.yaml" (builtins.toJSON topology);
in topologyFile
