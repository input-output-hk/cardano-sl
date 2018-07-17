{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module ChainExperiment2 where

import Data.Word
import Data.List
import Data.Maybe
import Data.Graph
import Data.Hashable
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Map (Map)

import Control.Applicative
import Control.Concurrent.STM (STM, atomically, retry)

import Test.QuickCheck


--
-- Simple blockchain data type.
--
-- These are the abstract types, the specification.
--

type Chain = [Block]  -- most recent block at the front

data Block = Block {
       blockId      :: BlockId,  -- ^ hash of other fields
       prevBlockId  :: BlockId,  -- ^ 'blockId' of the previous block
       blockSlot    :: Slot,
       blockPayload :: Payload
     }
  deriving (Show, Eq)

type BlockId = Int
type Slot    = Word
type Payload = String


hashBlock :: Block -> BlockId
hashBlock Block{prevBlockId, blockSlot, blockPayload} =
    hash (prevBlockId, blockSlot, blockPayload)

--
-- What it means for a chain to be valid
--

validChain :: Chain -> Bool
validChain []     = True
validChain (b:bs) = validChainExtension b bs && validChain bs

validChainExtension :: Block -> Chain -> Bool
validChainExtension b _
  | blockId b /= hashBlock b = False

validChainExtension b []     = prevBlockId b == 0
validChainExtension b (b':_) = prevBlockId b == blockId b'
                            && blockSlot b > blockSlot b'

--
-- And useful later: chain fragments
-- 

-- | Like 'Chain but does not have to chain onto the genesis block. Its final
-- back pointer can be anything at all.
--
type ChainFragment = [Block]

validChainFragment :: ChainFragment -> Bool
validChainFragment []     = True
validChainFragment (b:bs) = validChainFragmentExtension b bs
                         && validChainFragment bs

validChainFragmentExtension :: Block -> Chain -> Bool
validChainFragmentExtension b _
  | blockId b /= hashBlock b = False

validChainFragmentExtension _ []     = True -- any prevBlockId is ok
validChainFragmentExtension b (b':_) = prevBlockId b == blockId b'
                                    && blockSlot b > blockSlot b'

--
-- Generating valid chains
--

mkBlock :: BlockId -> Slot -> Payload -> Block
mkBlock blockid' slot payload = block
  where
    block   = Block blockid blockid' slot payload
    blockid = hashBlock block

genBlock :: BlockId -> Slot -> Gen Block
genBlock blockid slot = do
    payload <- vectorOf 4 (choose ('A', 'Z'))
    return (mkBlock blockid slot payload)

genNBlocks :: Int -> BlockId -> Slot -> Gen [Block]
genNBlocks 1 blockid0 slot0 = (:[]) <$> genBlock blockid0 slot0
genNBlocks n blockid0 slot0 = do
    c@(b':_) <- genNBlocks (n-1) blockid0 slot0
    b        <- genBlock (blockId b') (blockSlot b' + 1)
    return (b:c)

genChain :: Int -> Gen Chain
genChain n = genNBlocks n 0 1

newtype TestChain = TestChain Chain
  deriving Show

instance Arbitrary TestChain where
  arbitrary = do
    Positive n <- arbitrary
    TestChain <$> genChain n

prop_TestChain :: TestChain -> Bool
prop_TestChain (TestChain chain) = validChain chain

--
-- The operation on the abstract type
--

data ChainUpdate = AddBlock   Block
                 | SwitchFork Int     -- rollback by n
                              [Block] -- add more blocks
  deriving Show

-- This is the key operation on chains in this model
applyChainUpdate :: ChainUpdate -> Chain -> Chain
applyChainUpdate (AddBlock     b)  c = b:c
applyChainUpdate (SwitchFork n bs) c = bs ++ drop n c

applyChainUpdates :: [ChainUpdate] -> Chain -> Chain
applyChainUpdates = flip (foldl (flip applyChainUpdate))

validChainUpdate :: ChainUpdate -> Chain -> Bool
validChainUpdate cu c = validChainUpdate' cu
                     && validChain (applyChainUpdate cu c)

validChainUpdate' :: ChainUpdate -> Bool
validChainUpdate' (AddBlock    _b)  = True
validChainUpdate' (SwitchFork n bs) = n >= 0 && n <= k && length bs == n + 1

k :: Int
k = 5 -- maximum fork length

chainHeadBlockId :: Chain -> BlockId
chainHeadBlockId []    = 0
chainHeadBlockId (b:_) = blockId b

chainHeadSlot :: Chain -> Slot
chainHeadSlot []    = 0
chainHeadSlot (b:_) = blockSlot b

--
-- Generating valid chain updates
--

genChainUpdate :: Chain -> Gen ChainUpdate
genChainUpdate chain = do
    let maxRollback = length (take k chain)
    n <- choose (-10, maxRollback)
    if n <= 0
      then AddBlock     <$> genBlock (chainHeadBlockId chain)
                                     (chainHeadSlot chain + 1)
      else SwitchFork n <$> let chain' = drop n chain in
                            genNBlocks (n+1) (chainHeadBlockId chain')
                                             (chainHeadSlot chain' + 1)

genChainUpdates :: Chain -> Int -> Gen [ChainUpdate]
genChainUpdates _     0 = return []
genChainUpdates chain n = do
    update  <- genChainUpdate chain
    let chain' = applyChainUpdate update chain
    updates <- genChainUpdates chain' (n-1)
    return (update : updates)

data TestChainAndUpdates = TestChainAndUpdates Chain [ChainUpdate]
  deriving Show

instance Arbitrary TestChainAndUpdates where
  arbitrary = do
    (Positive n, Positive m) <- arbitrary
    chain   <- genChain n
    updates <- genChainUpdates chain m
    return (TestChainAndUpdates chain updates)

prop_TestChainAndUpdates :: TestChainAndUpdates -> Bool
prop_TestChainAndUpdates (TestChainAndUpdates chain updates) =
    all validChain chains
 && all (uncurry validChainUpdate) (zip updates chains)
  where
    chains = scanl (flip applyChainUpdate) chain updates

--
-- Data types for a plausibly-realisic representation of a blockchain,
-- plus an associated set of readers/consumers of the chain.
--
-- These are the concrete types, the implementaiton.
--

-- | Represent a chain as two overlapping parts: an immutable chain and
-- a volatile chain fragment.
--
-- Readers represented as a relation between blocks, reader ids and
-- reader state.
--
data ChainState
   = ChainState {
       chainImmutable :: Immutable,
       chainVolatile  :: Volatile,
       chainReaders   :: ReaderRelation
     }
  deriving (Eq, Show)

data Immutable = Immutable Chain -- re-using the spec type for simplicity
  deriving (Eq, Show)

-- | Representation of a chain fragment as a graph with backwards and forward
-- pointers.
--
data Volatile = Volatile
                  (Map BlockId (Block, Maybe BlockId))
                  (Maybe BlockId) -- ^ current tip, or empty
  deriving (Eq, Show)

type ReaderRelation = [((Slot, BlockId), ReaderId, ReaderState)]
type ReaderId = Int

data ReaderState =
       -- | The reader is on the current producer's chain and can move forward
       -- (if it's not already at the chain head). In this case the
       -- (slot, blockid) from the relation tells us where the reader is now.
       ReaderOnChain

       -- | The reader is on an old fork, and has to rollback to get onto
       -- the chain. This records where the reader is now, and how many blocks
       -- they have to roll back. In this case the (slot, blockid) from the
       -- relation tells us where the reader will end up after rolling back.
     | ReaderMustRollback Int Slot BlockId
  deriving (Eq, Show)
{-
--
-- The data invariants
--

invChainState :: ChainState -> Bool
invImmutable  :: Immutable  -> Bool
invVolatile   :: Volatile   -> Bool

invChainState (ChainState i v)
  | invImmutable i
  , invVolatile  v
  , Just (i', overlap, v') <- absImmutable i `chainsOverlap` absVolatile v
  , validChain (i' ++ overlap ++ v' )
  = True

  | otherwise
  = False

invImmutable (Immutable c) = validChain c

invVolatile (Volatile blocks Nothing) =
    -- The whole thing can be empty, with no tip.
    Map.null blocks

invVolatile (Volatile blocks (Just tip)) =
    -- But if it's not empty, then:
    and [
        -- The tip is in the map, and is marked as such
        case Map.lookup tip blocks of
          Just (_, TipBlock) -> True; _ -> False

        -- There is only one tip
      , length [ () | (_, TipBlock) <- Map.elems blocks ] == 1

        -- all blocks have the right key
      , and [ b == b' | (b, (Block{blockId = b'}, _)) <- Map.toList blocks ]

        -- no cycles within back pointers
      , noCycles [ (b, blockId, [prevBlockId])
                 | (b@Block{blockId, prevBlockId}, _) <- Map.elems blocks ]

        -- no cycles within the forward pointers
      , noCycles [ (b, blockId, maybeToList (nextBlockId next))
                 | (b@Block{blockId}, next) <- Map.elems blocks ]

        -- normal forward pointers have to be consistent with the back pointer:
        -- following a normal forward pointer gets to a block that points back
      , and [ case Map.lookup bid' blocks of
                Just (b',_) | prevBlockId b' == blockId b -> True
                _                                         -> False
            | (b, NextBlock bid') <- Map.elems blocks ]

        -- the 'activechain' is the blocks reachable backwards from the tip
        -- the activechain must form a valid chain fragment
      , validChainFragment activechain

        -- the activechain blocks must have normal forward pointers
      , and [ case next of
                TipBlock       -> True
                NextBlock{}    -> True
                NextRollback{} -> False
            | (_b, next) <- activechain' ]

        -- all blocks not reachable backwards from the tip must have
        -- rollback-flavour forward pointers
      , and [ case next of
                NextRollback{} -> True
                _              -> False
            | let active = Set.fromList (map blockId activechain)
            , (b, next) <- Map.elems blocks
            , blockId b `Set.notMember` active ]
      ]

    -- rollback pointers must either point to a block in the vchain or
    --   to a block in the immutable chain
    -- rollback numbers must be consistent with the number of back pointers to
    -- chase to get back to the target block
    -- rollback pointers with a rollback number N must have M>N blocks in the
    -- chain forward (ie since forks switches are always to longer ones)

  where
    noCycles g = null [ () | CyclicSCC _nodes <- stronglyConnComp g ]

    nextBlockId TipBlock           = Nothing
    nextBlockId (NextBlock      b) = Just b
    nextBlockId (NextRollback _ b) = Just b

    activechain  = chainBackwardsFrom  blocks tip
    activechain' = chainBackwardsFrom' blocks tip

chainBackwardsFrom :: Map BlockId (Block, BlockForwardLink)
                   -> BlockId
                   -> [Block]
chainBackwardsFrom blocks bid =
    case Map.lookup bid blocks of
      Nothing    -> []
      Just (b,_) -> b : chainBackwardsFrom blocks (prevBlockId b)

chainBackwardsFrom' :: Map BlockId (Block, BlockForwardLink)
                    -> BlockId
                    -> [(Block, BlockForwardLink)]
chainBackwardsFrom' blocks bid =
    case Map.lookup bid blocks of
      Nothing      -> []
      Just e@(b,_) -> e : chainBackwardsFrom' blocks (prevBlockId b)


--
-- The abstraction function
--

absChainState :: ChainState -> Chain
absImmutable  :: Immutable  -> Chain
absVolatile   :: Volatile   -> ChainFragment

absImmutable (Immutable c) = c

absVolatile  (Volatile _      Nothing)    = []
absVolatile  (Volatile blocks (Just tip)) = chainBackwardsFrom blocks tip

absChainState (ChainState i v)
  | Just (i',  overlap, v') <- absImmutable i `chainsOverlap` absVolatile v
  = concat [ i', overlap, v' ]
  -- pattern match guaranteed by the invariant.


--
-- Important helper function: chain overlaps
--

-- | If the chain fragments connect, returns the overlapping part and the
-- remaining non-overlapping parts. 
--
-- >      Just (xs', overlap, ys')         = chainsOverlap xs ys
-- > <==> (xs' ++ overlap, overlap) ++ ys' = (xs, ys)
--
chainsOverlap :: ChainFragment -> ChainFragment
              -> Maybe (ChainFragment, ChainFragment, ChainFragment)
chainsOverlap [] ys = Just ([], [], ys)
chainsOverlap xs [] = Just (xs, [], [])
chainsOverlap xs ys =
    let lastx = last xs in
    case break (\b -> blockId b == prevBlockId lastx) ys of

      -- Annoying special case: the ys chain is exactly a suffix of xs. The
      -- normal approach of looking for the thing last x points back to doesn't
      -- work, for that to work the ys chain has to go one block further back.
      ((_:_), [])
        | blockId (last ys) == blockId lastx
        , ys `isSuffixOf` xs
       -> Just (take (length xs - length ys) xs, ys, [])

      (_,   []) -> Nothing

      (possibleOverlap, _)
        | possibleOverlap `isSuffixOf` xs
       -> let overlap    = possibleOverlap
              overlaplen = length overlap
              xs'len     = length xs - overlaplen
              xs'        = take xs'len xs
              ys'        = drop overlaplen ys
           in Just (xs', overlap, ys')
        | otherwise
       -> Nothing

prop_chainsOverlap :: TestChain -> Bool
prop_chainsOverlap (TestChain chain) =
    and [ validChainFragment xs
       && validChain            ys
       && case chainsOverlap xs ys of
            Nothing                  -> False
            Just (xs', overlap, ys') ->
                xs' ++ overlap == xs
             && overlap ++ ys' == ys
             && validChain (xs' ++ overlap ++ ys')

        | n <- [0 .. length chain]
        , m <- [0 .. n]
        , let xs = take n chain
              ys = drop m chain
        ]


--
-- Step 1: empty chains
--

emptyChainState :: ChainState
emptyChainState = ChainState emptyImmutable emptyVolatile

emptyImmutable :: Immutable
emptyImmutable = Immutable []

emptyVolatile :: Volatile
emptyVolatile  = Volatile Map.empty Nothing

-- the empty chain value should
-- 1. satisfy the invariant
-- 2. be equivalent to the empty chain abstract value [].
--
prop_emptyChainState :: Bool
prop_emptyChainState = invChainState emptyChainState
                    && absChainState emptyChainState == []


--
-- Step 2: adding single blocks
--

addBlockVolatile :: Block -> Volatile -> Volatile
addBlockVolatile b (Volatile _ Nothing) =
    Volatile (Map.singleton (blockId b) (b, TipBlock)) (Just (blockId b))

addBlockVolatile b' (Volatile blocks (Just tip))
  | prevBlockId b' == tip = Volatile blocks' (Just tip')
  | otherwise             = error "addBlockVolatile: wrong back pointer"
  where
    tip'    = blockId b'
    blocks' = Map.insert tip' (b', TipBlock)
            . Map.adjust (\(b, TipBlock) -> (b, NextBlock tip')) tip
            $ blocks

-- | For building a chain from empty using the 'addBlockVolatile', at each step
-- the invariant holds, and the concrete and abstract values are equivalent
--
prop_addBlockVolatile :: TestChain -> Bool
prop_addBlockVolatile (TestChain chain) =
    all invVolatile vsteps
 && and [ absVolatile v == chain'
        | (v, chain') <- zip (reverse vsteps) (tails chain) ]
  where
    vsteps = scanl (flip addBlockVolatile) emptyVolatile (reverse chain)


--
-- Step 3: switching forks
--

switchForkVolatile :: Int -> [Block] -> Volatile -> Volatile
switchForkVolatile _rollback _newblocks (Volatile _ Nothing) =
    error "switchForkVolatile: precondition violation"

switchForkVolatile rollback newblocks (Volatile blocks (Just tip)) =
    Volatile blocks' (Just tip')
  where
    tip'    = blockId (head newblocks)
    blocks' = Map.adjust (\(b,_) -> (b, NextBlock rollforwardFrom)) rollbackTo
            $ foldl' (\bs (b,fp) -> Map.insert (blockId b) (b,fp) bs)
                     blocks updates
    updates :: [(Block, BlockForwardLink)]
    updates = zip newblocks (TipBlock : map (NextBlock . blockId) newblocks)
           ++ [ (b, NextRollback n rollbackTo)
              | (b, n) <- zip undos [1..] ]
    undos  = take rollback (chainBackwardsFrom blocks tip)
    rollbackTo      = prevBlockId (last undos)
    rollforwardFrom = blockId (last newblocks)


-- | This is now the simulation property covering both the add block and
-- switch fork operations.
--
prop_switchForkVolatile :: TestChainAndUpdates -> Bool
prop_switchForkVolatile (TestChainAndUpdates chain updates) =
    all invVolatile vs
 && all (\(v, c) -> absVolatile v == c) (zip vs chains)
  where
    v0     = foldr addBlockVolatile emptyVolatile chain

    vs     = scanl (flip applyChainUpdateVolatile) v0 updates
    chains = scanl (flip applyChainUpdate)      chain updates

    applyChainUpdateVolatile (AddBlock     b)  = addBlockVolatile b
    applyChainUpdateVolatile (SwitchFork n bs) = switchForkVolatile n bs


-- We will actually want additional properties, beyond simulation,
-- but these are specific to the concrete representation and the
-- extra operations we will provide.
--
-- * switching back and forth on the same set of forks works ok (corresponding
--    to two long running competing forks)
-- * an immutability property that all blocks and back pointers are immutable
--   and stay in the map, and only the forward pointers change. If we do no
--   slot-expiry pruning then there should be a strict subset property, since
--   the map will only grow, and only the forward pointers change.

--
-- Additional operations on the concrete representation.
--
-- Both correspond to an identity operation on the abstract version.
--

-- | Flush blocks in the volatile chain that are now immutable into the
-- immutable part of the chain.
--
flushNewImmutableBlocks :: Int -> ChainState -> ChainState
flushNewImmutableBlocks upto ChainState {
                               chainImmutable = Immutable chainImm,
                               chainVolatile
                             } =
    -- This is not intended to be efficient. It could be made efficient
    -- by caching pointers to the overlap and flush positions.
    case chainsOverlap chainImm (absVolatile chainVolatile) of
      Just (_, _, chainVol) ->
        let available = drop k chainVol
            toflush   = reverse . take upto . reverse $ available
         in ChainState {
              chainImmutable = Immutable (toflush ++ chainImm),
              chainVolatile
            }
      _ -> error "flushNewImmutableBlocks: invariant violation"


-- | Provided we flush blocks before we get to 2k, we can drop older
-- blocks from the volatile segment.
--
pruneVolatileBlocks :: ChainState -> ChainState
pruneVolatileBlocks cs@ChainState {
                      chainVolatile = Volatile blocks (Just tip)
                    } =
    cs {
      chainVolatile = Volatile blocks' (Just tip)
    }
  where
    -- just drop all entries that are older than 2k slots from the tip
    blocks' = Map.filter (\(b, _) -> blockSlot b > tipSlot - 2 * fromIntegral k) blocks
    tipSlot = blockSlot (fst (blocks Map.! tip))

pruneVolatileBlocks cs = cs

--
-- Final simulation property
--

--TODO !
--
-- The general simulation propert for a suitablely constrained sequence of
-- the concrete operations.
--
-- The flush and prune operations allow quite a bit of flexibility about when
-- we do them, but there is a constraint that we flush before we prune so
-- that we do not break the chain overlap.
--
-- Could pick a specific flush policy but would like to check that an arbitrary
-- valid policy is still ok.
-}

--
-- STM based protocol
--

-- | An STM-based interface provided by a chain producer to chain consumers.
--
data ChainProducer = ChainProducer {
       establishChainConsumer :: [(Slot, BlockId)]
                              -> STM (ChainConsumer, [(Slot, BlockId)])
     }

data ChainConsumer = ChainConsumer {
       currentReadPoint :: STM (Slot, BlockId),
       improveReadPoint :: [(Slot, BlockId)] -> STM (),
       tryPeekChain     :: STM (Maybe (ConsumeChain Block)),
       tryReadChain     :: STM (Maybe (ConsumeChain Block))
     }

data ConsumeChain block = RollForward block
                        | RollBackTo  Slot BlockId

type MaxReadBlocks = Int

readRollForwardOnly :: ChainConsumer -> MaxReadBlocks -> STM [Block]
readRollForwardOnly ChainConsumer{tryPeekChain, tryReadChain} maxBlocks =
    go maxBlocks
  where 
    go 0 = return []
    go n = do
      res <- tryPeekChain
      case res of
        Just (RollForward b) -> do
          _ <- tryReadChain
          bs <- go (n-1)
          return (b:bs)
        _ -> return []

-- | Like 'tryReadChain' but reads multiple blocks in one go.
--
tryReadChainN :: ChainConsumer
              -> MaxReadBlocks -- ^ The maximum number of blocks to read
              -> STM (Maybe (ConsumeChain [Block]))
tryReadChainN cs@ChainConsumer{..} maxBlocks = do
    res <- tryReadChain
    case res of
      -- If we're at the chain head or it's a rollback we just return that.
      Nothing                 -> return Nothing
      Just (RollBackTo s bid) -> return (Just (RollBackTo s bid))
      -- If we get one block we peek at what's ahead and consume any
      -- more blocks, up to our limit.
      Just (RollForward b) -> do
        bs <- readRollForwardOnly cs (maxBlocks-1)
        return (Just (RollForward (b:bs)))

-- | Like 'tryReadChainN' but blocks at the chain head.
--
readChainN :: ChainConsumer
           -> MaxReadBlocks -- ^ The maximum number of blocks to read
           -> STM (ConsumeChain [Block])
readChainN cs@ChainConsumer{..} maxBlocks = do
    res <- tryReadChain
    case res of
      -- If it's the chain head we block by retrying.
      Nothing                 -> retry
      -- If it's a rollback we just return that.
      Just (RollBackTo s bid) -> return (RollBackTo s bid)
      -- If we get one block we peek at what's ahead and consume any
      -- more blocks, up to our limit.
      Just (RollForward b) -> do
        bs <- readRollForwardOnly cs (maxBlocks-1)
        return (RollForward (b:bs))

