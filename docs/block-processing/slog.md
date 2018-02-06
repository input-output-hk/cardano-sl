Slog is a part of block processing that does things which don't fit in any other part (other parts being `Toil` in `txp`, `Poll` in `update`, `Toss` in `ssc`, and `lrc`).

## What it does

Slog is responsible for the following (see explanations below):

* Pure verification
* Ensuring that leaders from genesis blocks match leaders from LRC
* State maintenance:
    * Current tip
    * `lastBlkSlots`, a list of slots of the last `blkSecurityParam` blocks
    * Max seen difficulty
    * Forward links
    * `inMainChain` flags
* Calling callbacks (`BListener`)

Pure verification is *TODO DESCRIBE*

Genesis blocks do not contain any information that cannot be derived from elsewhere, but we still store them as physical entities for convenience. Here we want to ensure that the list of leaders that is stored within genesis blocks is the same as the one computed by LRC based on the previous epoch.

The list of slots of the last `blkSecurityParam` blocks (the last of which must be the tip) is an optimization for computing chain density.

We maintain maximum seen difficulty to ensure that we never rollback deeper than `blkSecurityParam` from it.

A forward link is a link from a block to its immediate child. They are currently only used in Explorer and Wallet.

`inMainChain` is a boolean flag that is set whenever the block is an ancestor of the current tip. It is needed for computing LCA of forks with the main chain.

Finally, we often want to store some extra data whenever we apply or rollback a block, which is what `BListener` is all about.

## How it works

Slog basically consists of 3 major functions: `slogVerifyBlocks`, `slogApplyBlocks`, and `slogRollbackBlocks`.

`slogVerifyBlocks` takes a list of blocks and either throws or returns a list of `SlogUndo`s (for each block, a `SlogUndo` is the `SlotId` of the block `blkSecurityParam` blocks before it). `slogApplyBlocks` and `slogRollbackBlocks` take a list of blunds and return a list of `SomeBatchOp`s, updating the in-memory state themselves.

### `slogVerifyBlocks`

Note: it only supports verification of blocks from the same epoch.

1. If the oldest block is a genesis block, verify that its leaders match the ones computed by LRC.
2. Call pure verification. If it fails, throw.
3. Compute `SlogUndo`s and return them.

### `slogApplyBlocks`

1. Put blunds in BlockDB (done first to preserve the invariant that the tip must exist in BlockDB).
2. Call `BListener`, get extra `SomeBatchOp`s from it.
3. Update `lastBlkSlots` in-memory.
4. Return `SomeBatchOp`s for:
    1. Updating tip
    2. Updating max seen difficulty
    3. `BListener`'s batch
    4. Updating `lastBlkSlots` in the DB
    5. Adding new forward links
    6. Setting `inMainChain` flags

### `slogRollbackBlocks`

1. Assert that we are not rolling back 0th genesis block.
2. Check that we are not rolling back more than `blkSecurityParam` blocks.
3. Call `BListener`, get extra `SomeBatchOp`s from it.
4. Return `SomeBatchOp`s for:
    1. Reverting tip
    2. `BListener`'s batch
    3. Reverting `lastBlkSlots`
    4. Removing forward links
    5. Removing `inMainChain` flags
