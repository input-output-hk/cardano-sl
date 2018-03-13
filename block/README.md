# cardano-sl-block

This package defines functions that operate on blocks for Cardano SL.

The block type is defined in the cardano-sl-core package, but this is the package
with functions for:

* Creating blocks.
* Validating blocks.
* Applying blocks to the blockchain and rolling them back.
* Calculating chain quality (number of main blocks divided by number of slots so
  far).
* Code to retrieve block headers and blocks from the Cardano network.
* Functions for iterating over a chain of blocks.
