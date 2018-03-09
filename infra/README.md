# cardano-sl-infra

A library implementing the network infrastructure for the Cardano SL network.

This library contains:

* The diffusion layer; the mechanism by which nodes on the Cardano network
  communicate with one another.
* An implementation of peer discovery using using Kademlia Distributed Hash Table
  using the [kademlia] library.
* Management of a list of peers on the Cardano network.
* Code for checking that the system clock on a node is not too far out of sync
  with others on the network using NTP (Network Time Protocol).
* Code for safely shutting down a node.
* Code for monitoring and collecting the internal state of a node, generating
  statistics and reporting this data.
* Code for dealing with slots within an epoch.

[kademlia]: https://hackage.haskell.org/package/kademlia
