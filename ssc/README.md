# cardano-sl-ssc

A library to do the Shared Seed Computation (SSC) required for selection of
slot leaders in the [Ouroborous] Proof-of-Stake protocol for Cardano SL.

Code in this library types and functions for:

* Generating random seeds
* Communicating SSC state (using SSC specific messaging protocols) to other nodes
  in the network.
* Storing and retrieving SSC state from the database.
* Interacting with the LRC (Leaders and Richment Computation)
* Ability to ignore network addresses and public key addresses which are thought
  to be attacking the Cardano network at the Proof-of-Stake protocol level.

[Ouroborous]: https://eprint.iacr.org/2016/889.pdf
