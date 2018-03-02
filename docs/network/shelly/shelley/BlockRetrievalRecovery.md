# Block retrieval and recovery mode over diffusion layer

This document shows how the interface proposed [here](./Interface.md) can
be used to implement the current block retrieval and recovery mode system
without significant changes. We will eventually make significant changes, but
it would be nice to shim the diffusion layer interface in as a separate work
item before making other improvements.

Let's go in order of a block header's journey through the system. It starts
when it enters the block retrieval queue.

## `handleBlockHeaders`

Block headers enter the system here. It's a time-warp listener.
`handleBlockHeaders` calls `handleUnsolicitedHeaders` (which in
fact only works if there's precisely one header), classifying the
header (continues, alternative, useless, or invalid) according to current
state (our tip) and dropping it into the block retrieval queue if
it's not useless or invalid.

This is fine, all we're doing is changing the source of the block headers.
Instead of coming from an explicit time-warp conversation, they'll come
from the diffusion layer (which uses a time-warp conversation, but that's
abstracted). We can get rid of `handleUnsolicitedHeaders` because all
we need is `handleUnsolicitedHeader`, which can be called whenever the
diffusion layer presents a block header. `handleUnsolicitedHeader` requires
a `NodeId` of the provenance of the block, but that's only there so it
knows who to ask for the body, so it should be removed because it's the
diffusion layer's responsibility to know where to find the body.

Comment: Duncan

> And are we confident that the block cache layer can indeed do this. Will it need to track where it knows it can fetch new blocks from?

Comment: Alex

> Yes I imagine that's how it would work. Included in the cache would be a set of NodeIds for peers which have announced that header.

## `retrievalWorker`

The block retrieval queue (sourced by `handleBlockHeaders`) is cleared by
this worker. Either `handleContinues`, `handleAlternative`, or
`handleRecovery` will come next.

This is independent of the network layer.

### `handleContinues`

The block header is taken out of the block header queue by `retrievalWorkerImpl`
and it continues the current tip. In this case `handleContinues` is called with
the header. Its `NodeId` parameter can be removed.

Oddly enough, it is classified again (it was classified by
`handleUnsolicitedHeader` before being added to the queue). Perhaps this
is due to fear that the tip might have changed in the meantime, hence the
(stateful) `classifyNewHeader` function will give a different answer
TODO check that out, but it's an existing problem so we don't have to fix
it.

Now `getProcessBlocks` is called. See the section entitled `getProcessBlocks`.

That's all. The header has been processed once `getProcessBlocks` is done
and the retrieval worker restarts its loop.

### `handleAlternative`

The block header is taken out of the block header queue by `retrievalWorkerImpl`
and it does not continue the current tip. In this case `handleAlternative` is
called with the header. Just like for `handleContinues`, its `NodeId` parameter
can be removed. And just like for `handleContinues`, the header is classified
again. See TODO from the description of that item; we should look into this
but we don't have to fix it now.

`updateRecoveryHeader` is called, putting the header in some `TMVar` which
indicates the header that we should recover to (hopefully it's the tip
of the best chain). Call this the recovery header var.

This is independent of the network layer.

### `handleRecovery`

There's no block header in the retrieval queue but the recovery header var is
full (perhaps because `handleAlternative` called `updateRecoveryHeader`). Its
`NodeId` parameter can be removed.

`mkHeadersRequest` makes a datatype describing the request, but doesn't do the
actual request. It gets the checkpoints to use.

Next it enqueues a message (time-warp conversation) to request these headers,
and then call `getProcessBlocks` passing the oldest header and the hash of
the newest header, effectively instructing it to fetch the whole chain that the
server responded with. This network conversation can be dropped and replaced
by a synchronous diffusion layer call: get the headers with this tip and
these checkpoints.

See `getProcessBlocks` section for details on that.

## `retrieveBlocks`

This repeatedly `recvLimited`s a new block and constructs an
`OldestFirst NE Block` (`NE` meaning non-empty I assume), stopping the
`recvLimited` only when the header hash of the last received block matches the
expected end header hash (or if there's an error)

This can be moved down to the diffusion layer, as it's all about networking.
We'll clean it up a bit and think about perhaps streaming the results rather
than building a list of all the blocks.

## `getProcessBlocks`

The final piece of the retrieval and recovery mode puzzle.

The first two arguments of this function can be dropped (`EnqueueMsg m`
and `NodeId`) because they're needed only for network communication which
is now taken care of by the diffusion layer.

The body of `getProcessBlocks` will be largely the same, but with the
time-warp conversation stripped off. Instead of enqueueing this conversation,
it will ask the diffusion layer for the required blocks (computed using its
two header arguments). This is essentially a call to `retrieveBlocks`, but
maybe under a different name. The `send` call will of course be removed since
the diffusion layer will take care of that as necessary.

After the blocks are retrieved, all networking is finished for this round of
the retrieval worker.

## One improvement worth mentioning

RE `handleRecovery` and `getProcessBlocks`: the former requests the headers,
and the latter requests the blocks for those headers. It's probably better to
just request the blocks. Should we make that improvement now, or later? If
we defer it, we'll need to put a `getHeaders` into the synchronous diffusion
layer interface (only a similar one for `getBlocks` is there, and a block
contains its header).

Comment: Duncan

> If we can do it all at the same time without causing too much chaos, that'd be preferable.
