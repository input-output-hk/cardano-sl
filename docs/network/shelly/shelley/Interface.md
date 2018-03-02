# Diffusion / application interface

A good first step in Shelley networking implementation is to carve out the
interface between application and diffusion, so that we can do our work
independently (besides the architectural advantages).

In summary: all "conversation" and relaying is done by the diffusion layer
only. This way application latency does not inflate network latency. Input to
the application layer will come from the diffusion layer rather than from a
set of network listeners. Output to the network will go via the diffusion
layer, and may not offer any feedback on success or failure. 

A rough sketch of the interface:

```Haskell
-- Direction: application => diffusion

-- Synchronously ask the diffusion layer for the blocks from a tip to a set
-- of checkpoints. Inspired by the current block headers request that's made
-- during recovery mode.
getBlocks :: HeaderHash -> [HeaderHash] -> IO [Block]
-- Special case of getting one block. May be faster to implement not in terms
-- of getBlocks?
getBlock :: HeaderHash -> IO (Maybe Block)

-- Ask for the tip-of-chain for peers.
-- TBD whether this will be needed.
-- Diffusion layer decides when it returns (must have a timeout) and of course
-- which peers to ask.
--
-- A 'RawBlockHeader' is given, i.e. it's decoded from the wire but not
-- validated. Application layer can validate it as needed.
getTips :: IO [RawBlockHeader]

-- Offer a block for diffusion.
-- Is it OK to return right away? Or should the diffusion layer respond with
-- some indication of whether the block was actually sent to some threshold of
-- peers?
announceBlock :: Block -> IO ()

-- Similar for other data that can be broadcast
-- (transactions, ssc, update, etc.)
```

```Haskell
-- Direction: diffusion => application

-- Give various data to the application layer asynchronously (drop it in some
-- queue).
block :: Block -> IO ()
blockHeader :: BlockHeader -> IO ()
transaction :: Tx -> IO ()
...

-- Check whether a block header could possibly be for a legitimate block
-- (signed by slot leader).
isValidBlockHeader :: BlockHeader -> IO Bool

-- Quick sanity check for a transaction, to determine whether we should forward
-- it.
isValidTransaction :: Tx -> IO Bool

-- Application is in control of databases, but the diffusion layer may want to
-- use it, for instance if a peer asks for a block that's not in its cache.
getBlockFromDB :: HeaderHash -> IO Block

-- TBD whether we'll need this.
-- If the application layer ever needs to know the tips of peers, then we will
-- indeed need it.
getTipFromDB :: IO RawBlockHeader
```

## Queueing / buffering between these layers

It's expected that the two layers will operate at different rates / have
different throughput. The diffusion layer will probably be able to process more
blocks per second than the application can (we hope). It's important not to
impose too much synchronization between the two. So we'll want some kind of
queueing mechanism for passing data up from diffusion to application.
This way `block`, `blockHeader`, `transaction`, etc. can be non-blocking
always. We'll probably want to prioritize certain data (a block for the
current slot) and have it jump the queue.

For a treatment like this, we'd have to rearchitecht cardano-sl so that it
doesn't spawn arbitrarily-many threads (it clears the queue from the diffusion
layer at a rate proportional to actual productivity).

We may want queueing in the other direction as well, from application to
diffusion, since it's plausible that the diffusion layer could get backed up
too.

## [CSL-1817]

Must provide an interface satisfying the needs of [CSL-1817], so that the
application layer can make use of the (presently hypothetical) connectivity
status.
Ideally we can satisfy [CSL-1817] very soon, by deciding on an interface,
backing it with existing infrastructure, and improving it later on.

We probably need to know more information than just the known peers.
  - To whom we actually have a connection.
  - The quality of and activity on these connections.
    - Network-relevant (in terms of bytes) but maybe also application-relevant
      (in terms of blocks, transactions, etc.).
  - Possible to give bittorrent style metrics: connected to n of m peers,
    where the m comes from discovery (Kademlia).

What should the `diffusion => application` interface look like in order to
accomodate this? Give a `TVar` containing all the relevant info? Have a
side-channel for metadata like these?

## Implementation plan

We'll swap out all of the time-warp listeners for functions which take the
relevant input from the diffusion layer (block, transaction, etc.) and move
the listeners themselves down to the diffusion layer without the application
logic parts.

There will be a single thread in the application layer which clears the
input queue from the diffusion layer and forks a thread with a handler for
each.

```Haskell
data InputFromDiffusionLayer =
    BlockHeader HeaderHash
  | Transaction Tx
  | MPC RelevantMPCData
  | Update RelevantUpdateData
  | ...

type Application m = InputFromDiffusionLayer -> m ()

type InputQueue = TBQueue InputFromDiffusionLayer

withDiffusionLayerInput :: Application m -> InputQueue -> m x
withDiffusionLayerInput go q = do
  inp <- readTBQueue q
  _ <- async (go inp)
  withDiffusionLayerInput go q
```

### Inv/Req/Data

Much of the network interaction is handled by the Inv/Req/Data relay system.
Pulling this down into the diffusion layer should be straightforward. It will
use synchronous `diffusion => application` API functions to get the data in
response to requests, and the asynchronous API in response to data from peers
(the data will be passed on through some queue).

### Block, block headers, and recovery mode

Block and block header communication and relaying does not use the Inv/Req/Data
system.

#### Block header announcement and block serving

To announce a block header, simply pass it to the diffusion layer. The
implementation of serving blocks and block headers will be moved to the
diffusion layer, and supported by the synchronous API to get blocks and block
headers (from the database, which is owned by the application layer). In
future we'll put in a diffusion layer cache, but that's not part of this
initial work.

#### Block retrieval

Block retrieval has more moving parts than header announcement. There's a
retrieval queue which came to be because block headers are sent without
their bodies, and the recipient is not offered a chance to ask for the body
within the same conversation. So the headers are thrown into a queue and some
thread will eventually request the body (unless it considered useless by the
time it comes out of the queue).

We can leave this in place for the initial work, and discuss a better solution
later. The application layer handler for block headers will remain largely the
same, except that it will take input from the diffusion layer and drop it into
the application layer queue as it does now. The block retrieval worker will
pick it up and do the relevant logic, unless...

#### Recovery mode

Sometimes the block retrieval worker will trigger recovery mode, in case it
finds a header which doesn't immediately continue the tip. This recovery
process uses time-warp conversations for control flow: it will get the headers
and blocks needed to go from current tip to new tip, and then return to the
block retrieval queue clearing loop, possibly ending recovery mode.

We can keep this as is (equally bad, not worse) by using the synchronous
`getBlocks` function and letting the diffusion layer take care of finding them.

## Concrete details of implementation

Now for some actual Haskell text.

The form of the program will be something like this: set up the diffusion and
logic layers, then run them concurrently.
In this snippet, IO is used as a stand-in. I know we'll actually be using
something a little more sophisticated.

```Haskell
-- Logic layer interface.
-- Used by the diffusion layer.
data Logic = Logic
  { -- Signed by the slot leader?
  , isValidBlockHeader :: BlockHeader -> IO Bool
    -- Consistent with the UTXO?
    -- Is it OK to have two transactions in the mempool such that only one
    -- could be included in a block? e.g. each one spends all of the agent's
    -- cash.
  , isValidTransaction :: Tx -> IO Bool
    -- Do we need this?
  , getOurStakeholderId :: IO StakeholderId
  , getBlock :: HeaderHash -> IO (Either GetBlockError Block)
    -- There's no unexceptional reason why the tip can't be retrieved, so it's
    -- not Either GetTipError BlockHeader; failure will just be an IO
    -- exception (couldn't open DB or something horrible like that).
  , getTip :: IO BlockHeader
    -- Give a block header to the logic layer.
    -- This will just call handleUnsolicitedHeader
    -- The NodeId of the peer who gave it must be given, but that's only for
    -- the first iteration (recovery mode needs it, so that we can ask that
    -- same peer for the block body/bodies). In the future we'll let the
    -- diffusion layer figure out who to ask.
  , postBlockHeader :: BlockHeader -> NodeId -> IO ()
  , ...
  }

data LogicLayer = LogicLayer
  { -- The interface.
  , logic :: Logic
    -- an IO to make it go (launch all the "workers", among other things).
  , runLogicLayer :: IO ()
  }

-- Diffusion layer interface.
-- Used by the logic layer.
data Diffusion = Diffusion
  { -- Logic layer wants some blocks from a particular node.
    -- In the future, it won't give the NodeId, because the diffusion layer
    -- should abstract that. It's only here for the first iteration.
    -- NB: getBlockHeaders is rolled into this function, because as far as I
    -- can see there's no point at which we want to get headers but not the
    -- corresponding blocks.
    getBlocks :: NodeId -> Maybe HeaderHash -> [HeaderHash] -> IO (Either GetBlocksError [Block])
    -- This one may not be needed. It's supposed to get the tip of chain from
    -- an assortment of peers.
  , getTips :: IO [BlockHeader]
    -- Ask the diffusion layer to announce a block header, probably because the
    -- logic layer just minted it.
  , announceBlockHeader :: BlockHeader -> IO ()
    -- TODO exhaustive list of other things that can be announced...
    -- Perhaps a data type for this?
    -- Will replace I think 'invReqDataFlowDo'/'invReqDataFlowTK'.
    -- From lib/src/Pos/Communication/Methods.hs
    --  sendTx
    --  sendVote
    --  sendUpdateProposal
    -- From ssc/Pos/Ssc/Worker.hs
    --  checkNSendOurCert
    --  sendOurData
    -- From auxx/src/Command/Proc.hs
    --  two calls to 'dataFlow'
  , ...
  }

data DiffusionLayer = DiffusionLayer
  { diffusion :: Disfussion
  , runDiffusionLayer :: IO ()
  }

-- Cardano SL main for some choice of logic and diffusion.
-- All of the goodies are abstracted in Logic and Diffusion.
-- Both full node and light relay can be expressed by this.
--
-- It's in continuation style because the function passed must be capable of
-- bracketing, for resource acquisition and release. For example: the logic
-- layer will bracket databases and maybe some other things (see
-- allocateNodeResources, releaseNodeResources).
--
-- There could be and will be another layer of bracketing. It's assumed that
-- runLogicLayer and runDiffusionLayer take care of any necessary bracketing /
-- exception handling there. For example: a network-transport and a kademlia
-- instance will be bracketed in 'runDiffusionLayer', as these parts should
-- only come up once both of layers are ready (i.e. within the continuation we
-- pass to withLayers).
cslMain :: (forall x . ((LogicLayer, DiffusionLayer) -> IO x) -> IO x) -> IO ()
cslMain withLayers = withLayers $ \(logicLayer, diffusionLayer) ->
  -- TBD; concurrently, or race? Latter may be a better option, but if they're
  -- both expected to run indefinitely (forall x . IO x) then it doesn't matter,
  -- an exception in either will halt them both.
  void $ concurrently (runLogicLayer logicLayer) (runDiffusionLayer diffusionLayer)

-- Lazy in the diffusion layer: nothing is forced until the logic layer
-- is set in motion by 'runLogic'.
--
-- Uses the 'Diffusion' record to discharge the monad transformers found in
-- the current cardano-sl implementation.
--
-- It's in continuation style in order to facilitate bracketing and recursive
-- do. Check out 'withLayersFullNode' to see how it's used (where the recursive
-- do arises).
withLogicLayerFullNode :: ((Diffusion -> IO LogicLayer) -> IO x) -> IO x
withLogicLayerFullNode expectDiffusionLayer =
  bracket acquireNodeResources releaseNodeResources $ \nodeResources -> do
    ...
    expectDiffusionLayer $ \diffusionLayer -> do
      ...
      pure LogicLayer {..}

-- Lazy in the logic layer: nothing is forced until the diffusion layer
-- is set in motion by 'runDiffusion'.
--
-- Uses the 'Logic' record to implement certain features.
--
-- Again, in a slightly weird continuation style because of bracketing and
-- recursive do.
withDiffusionLayer :: ((Logic -> IO DiffusionLayer) -> IO x) -> IO x
withDiffusionLayer expectLogicLayer =
  bracket acquire release $ \resources -> do
    ...
    expectLogicLayer $ \logicLayer -> do
      ...
      pure DiffusionLayer {..}

-- A simulation diffusion layer which delivers data to the logic layer
-- according to some deterministic process. No bracketing necessary, so its
-- type is simpler (not continuation-based).
mkDiffusionLayerSimulation :: IO DiffusionLayer

-- The diffusion and logic layers are mutually dependent. They're acquired
-- using recursive do.
withLayersFullNode :: ((LogicLayer, DiffusionLayer) -> IO x) -> IO x
withLayersFullNode k =
  withLogicLayerFullNode $ \mkLogic ->
    withDiffusionLayer $ \mkDiffusion -> mdo
      logicLayer <- mkLogic (diffusion diffusionLayer)
      diffusionLayer <- mkDiffusion (logic logicLayer)
      k (logicLayer, diffusionLayer)

main :: IO ()
main = cslMain withLayersFullNode
```

Now the work factors into two parts:

  1. `withLogicLayerFullNode`
  2. `withDiffusionLayer`

And that's neat: core team can do the first one, and we can do the second.
For maximal parallelism, we can do it without deleting any of the existing
implementation, only copying the pieces that are needed.

In `Pos.Launcher.Resource`, for instance, the logic layer and diffusion layer
resources are mixed together. Instead of paring this file down, we'll copy out
the pieces relevant to each layer, to a file relevant to each layer.

### Use in derivative programs

auxx, tools, wallet, and explorer.

#### auxx

Looking at `Main.hs`, it seems no change is necessary. A logic/diffusion layer
split isn't needed here, so it runs neither of them. The complexity of the
diffusion layer would be overkill given that the auxx only.

NB: it uses the existing `Pos.Launcher` infrastructure and *does* spin up a
time-warp node. That's fine.

#### wallet

Similar story to auxx: it will run a time-warp node etc. via the `Pos.Launcher`
infrastructure. However, the wallet *should* use the logic/diffusion layer
split, as it does at the moment run all the full node logic.

#### explorer

Again, similar story.

### Relationship to monad transformer stacks

The diffusion and logic layers must not be required to work in the same
monad. There are capabilities exposed by the logic layer's monadic context
which must not be available in the diffusion layer, such as the database.

#### Case study: reading from database

The current database abstraction is `MonadRealDB`, which gives an accessor to
information necessary to use rocks databases (file path, various operations on
different logical databases).

We don't want to offer all of this information to the diffusion layer. We don't
want the diffusion layer to even know it's reading from a database, let alone
a rocks database at a particular file path. Whatever monad the diffusion layer
works within, it will not satisfy `MonadRealDB`.

But the monad of the logic layer will be `MonadRealDB`, because it is now and
we don't want to change that. So in the logic layer we'll still see for example

```Haskell
getBlock :: MonadRealDB m => HeaderHash -> m Block
```

and this will eventually be discharged by `openNodeDBs` and putting the
resulting value into some reader context. But the result of `openNodeDBs` will
also be used to come up with

```haskell
getBlock' :: HeaderHash -> IO Block
```

to be put into the `Logic` record and used by the diffusion layer.

This can expressed in the types given above. It will go something like this:

```Haskell
withLogicLayerFullNode :: ((Diffusion -> IO LogicLayer) -> IO x) -> IO x
withLogicLayerFullNode expectDiffusionLayer =
  bracket acquireNodeResources releaseNodeResources $ \nodeResources -> do
    let dbs = nrNodeDBs nodeResources
        getBlock :: HeaderHash -> IO Block
        getBlock = ... - use dbs to implement this.
        initModeContext = InitModeContext dbs ... -- existing code does this.
    ...
    expectDiffusionLayer $ \diffusionLayer -> do
      -- Existing code. Uses the DBs in initModeContext to discharge the
      -- particular concrete MonadRealDB stack.
      let runLogicLayer = runInitMode initModeContext ...
      ...
      -- getBlock and runLogicLayer are in scope
      pure LogicLayer {..}
```

### Delivering incoming data

For the first interation, delivering incoming data from diffusion to logic
will be simple and will not depart significantly from the status quo.

#### Block headers

When a new block header is received, the diffusion layer will drop it into
the block retrieval queue, i.e. call `handleUnsolicitedHeader`:

```Haskell
postBlockHeader :: BlockHeader -> IO ()
postBlockHeader header = handleUnsolicitedHeader header
```

The interface for `handleUnsolicitedHeader` will change slightly, as per
[these notes](./BlockRetrievalRecovery.md). Also note that `postBlockHeader`
will look slightly different because it will have to discharge the moand
transformer stack to get down to `IO` or `Production`.

#### Any other data

All of the other incoming data is processed by the inv/req/data framework.
The logic layer must inform the diffusion layer on how to deal with the
various types of messages: it will give the `InvReqDataParams` or
`DataParams` values which describe these thing, and include synchronous calls
into the logic layer to, for instance, process a new transaction or update
proposal (`handleData`). These calls also determine whether to relay the data.

The logic layer will *not* inform the diffusion layer on which listeners it
should bring up, so it's not appropriate to have the logic layer interface
include a set of `Relay` values from which the listeners can be automatically
derived. Instead, it will offer the relevant calls for all of the relay types:

  - ssc
  - tx
  - delegation
  - update

#### Future improvements

Queueing will be placed between diffusion and logic. All new data coming in
from the diffusion layer will pass through this queue, allowing us to
prioritize and drop.

We'll eventually move to something like this:

```Haskell
inputDispatcher :: DiffusionLayer -> IO ()
inputDispatcher diffusionLayer = do
  nextItem <- readChan (inputChannel diffusionLayer)
  case nextItem of
    BlockHeader header -> handleUnsolicitedHeader header
    Ssc ssc -> handleSsc ssc
    Tx tx -> handleTx tx
    Delegation del -> handleDelegation del
    Update upd -> handleUpdate upd
    -- Nothing for subscription, that's diffusion layer only.
  inputDispatcher diffusionLayer
```

This would be one of the "workers" of the logic layer, forked at
`runLogicLayer` time.

### Making a DiffusionLayer

Coming up with the diffusion layer interface before bringing up a transport
and a time-warp-nt node is possible thanks to the existing `OutboundQ`.

```Haskell
withDiffusionLayer :: ((Logic -> IO DiffusionLayer) -> IO x) -> IO x
withDiffusionLayer expectLogicLayer =
  bracket acquire release $ \resources -> do
    -- Create the outbound queue. Could also put it into 'resources', but it
    -- doesn't require any teardown so it's OK here.
    oqueue <- ...
    let getBlocks :: [HeaderHash] -> IO Either GetBlocksFailure [Block]
        getBlocks headers = enqueue oqueue (MsgGetBlocks headers) ...
        getBlockHeaders :: Maybe HeaderHash -> [HeaderHash] -> IO [HeaderHash]
        getBlockHeaders mTip checkpoints = enqueue oqueue
    expectLogicLayer $ \logicLayer -> do
      let runDiffusionLayer = node ... $ \theNode -> do
            ...
            withAsync (dequeueThread oqueue (sendMsg theNode)) $ \dqThread -> do
              ...
      pure DiffusionLayer {..}
```

*Aside*: we can't eliminate the `NodeId` from `handleUnsolicitedHeader` and
recovery mode in general just yet, because diffusion layer will still need
it in order to decide who to talk to. Eventually we'll have a more sophisticated
diffusion layer which will figure this out automatically, but for now we
should leave it.
