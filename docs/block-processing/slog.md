Slog is a part of block processing that does things which don't fit in any other
part (other parts being `Toil` in `txp`, `Poll` in `update`, `Toss` in `ssc`,
`Cede` in `delegation` and `lrc`).

## What it does

Slog is responsible for the following (see explanations below):

-   Pure verification
-   Ensuring that leaders from genesis blocks match leaders from LRC
-   State maintenance:
    -   Current tip
    -   `lastBlkSlots`, a list of slots of the last `blkSecurityParam` blocks
    -   `maxSeenDifficulty`
    -   Forward links
    -   `inMainChain` flags
-   Calling callbacks (`BListener`)

Pure verification is a set of checks in `Pos.Block.Pure` that operate on
arbitrary sequences of blocks and verify in a pure context whether these blocks
constitute a valid chain. For more information, see Haddock comments in
`Pos.Block.Pure`

Genesis blocks contain no information that cannot be derived from preceding main
blocks, but we still store them as physical entities for convenience. Here we
want to ensure that the list of leaders that is stored within genesis blocks is
the same as the one computed by LRC based on the previous epoch.

The list of slots of the last `blkSecurityParam` blocks (the last of which must
be the tip) is an optimization for computing chain density.

We maintain the highest difficulty ever seen in GState (`maxSeenDifficulty`) to
ensure that we never rollback deeper than `blkSecurityParam` from it.

A forward link is a link from a block to its immediate child from the main
chain. Of course, they are not stored in the blocks themselves, and instead are
only a feature of the DB.

`inMainChain` is a boolean flag that is set whenever the block is an ancestor of
the current tip. It is needed for quickly computing LCA of forks with the main
chain.

Finally, we often want to notify Wallet and Explorer whenever we apply or
rollback a block, so that they can store some extra data. This is precisely what
`BListener` does.

## How it works

Slog basically consists of 3 major functions: `slogVerifyBlocks`,
`slogApplyBlocks`, and `slogRollbackBlocks`.

`slogVerifyBlocks` takes a list of blocks and either throws or returns a list of
`SlogUndo`s (for each block, a `SlogUndo` is the `SlotId` of the block
`blkSecurityParam` blocks before it). `slogApplyBlocks` and `slogRollbackBlocks`
take a list of blunds and return a list of `SomeBatchOp`s, updating the
in-memory state themselves.

Please refer to Haddock comments in the code for details on the algorithms.
