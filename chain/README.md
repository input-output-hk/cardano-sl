# cardano-sl-txp

This library implements transaction processing for Cardano SL. It includes:

* Logic for verifying transactions, applying them the blockchain and rolling
  them back.
* Logic for local and global transactions. Global transactions are ones that
  have already been added to the blockchain while local ones have not yet been
  added.
* A database interface that stores UTXOs and stakes.
* A wrapper over [Plutus], the scripting language used in transactions.
* The mempool which holds the UTXOs (unspent transaction outputs) for a node.

[Plutus]: https://github.com/input-output-hk/plutus-prototype
