# How is the coinbase transactions validated?

This document sets out to describe how the coinbase transaction of a block is
validated.

## What is the coinbase transaction?
Whenever a slot leader mints a new block for their slot within an epoch, that
slot leader gets to claim the block reward for that block. That block reward
is a transaction with zero inputs (check this) and one output (check this).
The coinbase transaction includes the block reward and the transaction fees
from all the transactions in the block.



## Unresolved questions:

Some of these questions arise from knowing how Bitcoin works and wanting to know
if and how Cardano differs.

* How much is the block reward? Is there a pre-determined schedule of change in
  the block reward over time?
* Does the coinbase transaction have zero inputs? Can it have more than one
  output?
* Is the minting fee that a transaction pays simply the differences between its
  inputs and the outputs?
* Is the coinbase transaction output amount simply the block reward plus the sum
  of the differences between the inputs and the outputs of all the transactions
  in the block?
