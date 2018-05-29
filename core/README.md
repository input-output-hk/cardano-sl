# cardano-sl-core

Core components of Cardano SL, including types and operations for the following
concepts:

* A `Coin` type which is used to denominate the currency.
* An `Address` type which is the source and destination for transactions.
* Blockchain related components like `Block`, `BlockHeader`, `HeaderHash` (the hash
  of a block header) etc.
* Handling of stakeholder and delegation (of stake) for Proof-of-Stake operations.
* Epochs, a finite period of time for which slot leaders are predetermined.
* Slots, a period of time during which at most one block is minted/mined.
* Shared Seed Computation (SSC) which is used in the process of electing a slot
  leaders where slot leaders get to mine/mint the block for the slot they lead.
* Software updates, including proposals for updating and voting on updates.
