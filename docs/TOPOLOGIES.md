# Topology configuration

The topology is a `.yaml` file specified using the `--topology` command line
flag. When no `.yaml` file is specified the node is considered to be a
behind-NAT edge node with a hardcoded set of default domain names to discover
relays (see "Behind-NAT Nodes", below).

## Statically configured nodes (core or relay)

Statically configured nodes are provided with a full list of all nodes and
their types. It looks something like this:

``` yaml
nodes:
  "node0":
    type: core
    region: eu-central-1
    static-routes: [["node1"], ["node2"]]
    host: node0.local
    # default port
  "node1":
    type: core
    region: eu-west-1
    static-routes: [["node0"], ["node2"]]
    addr: 12.34.56.78
    # default port
  "node2":
    type: relay
    region: eu-west-2
    static-routes: [["node0"], ["node1"]]
    # uses 'node2' as the hostname
    port: 3000
```

The idea is that all statically configured nodes in the cluster share this file,
and are additionally told which node in this list they are (`--node-id` command
line flag). The yaml file  contains the following information for each node:

* Its type (`core` or `relay`)
* Its region
* Its hostname (`host`, to be resolved through DNS) or its IP address (`addr`);
  if neither of these is used, the node's name will be used as its hostname.
* Its port number. This is optional; if not used, the default port number will
  be used (currently set at 3000, can be changed using `--default-port`).
* It's peers (explained in more detail below).

The interpretation of the list of lists in the `static-routes` is as follows:
when a message gets enqueued to a set of peers

``` yaml
[[a1,a2,a3],[b1,b2,b3],[c1,c2,c3],[d1,d2,d3]]
```

then the message will be sent to four nodes: one chosen from `[a1,a2,a3]`,
one chosen from `[b1,b2,b3]`, etc. (this choice is made based on heuristics that
approximate how busy the destination is). These static routing tables should
be carefully constructed to minimize network fragmentation.

Note that relay nodes register themselves with the Kademia network, but core
nodes do not (see below for Kademlia configuration).

## Behind-NAT edge nodes

Behind NAT nodes are provided with a list of lists of domain names, which
they use to discover relays. It looks something like

``` yaml
relays: [[{"host": "domain1"},{"host": "domain2", "port": 1234}]]
```

The interpretation of the list of lists here is as follows. If the list looks
like

``` yaml
[[a1,a2],[b1,b2],[c1,c2]]
```

then the node will query DNS to resolve domain names `a1` and `a2` by default.
This yields a set of IP addresses (possibly larger than 2, since a domain name
can result in multiple IP addresses), which the edge node will try in turn.
The alternative sets of domain names will be used only if one of the domain
names in the first list could not be resolved. The relay node will be contacted
on the port number specified in the yaml file, or on the default port otherwise
(`--default-port`).

In order to support behind NAT nodes that for whatever reason cannot do DNS
resolution, it is also possible to include relay nodes by IP address in this
list, using `addr` instead of `host`; the structure remains otherwise the same.

## Peer-to-peer nodes

Peer-to-peer nodes are very similar to behind-NAT nodes; they are also
considered edge nodes, but instead of using DNS to find relays, they use
Kademlia. Their configuration looks something like

``` yaml
p2p:
  variant: normal 
  valency: 3
  fallbacks: 1
```

The `valency` and `fallbacks` parameters specify what kind of routing we should
set up (in the static nodes example above, `valency = 4` and `fallbacks = 2`
(TODO: verify that that is true).

## Traditional mode

In traditional mode all nodes find each other using Kademlia, and moreover all
nodes are considered core nodes. The configuration is similar to that of P2P
nodes:

``` yaml
p2p:
  variant: traditional
  valency: 3
  fallbacks: 1
```

Behind-NAT is not (explicitly) supported in traditional mode (though
workarounds using SSH tunnelling or similar are of course possible).

## Light wallet

The light wallet uses a specialized topology, in which it is given a specific
set of nodes to connect to. This is hardcoded in the light wallet itself, there
is no yaml format for this mode. 

# Kademlia configuration

TODO
