{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module ChainExperiment2 where

import           Data.Hashable
import           Data.List (find, foldl', tails)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Word

import           Control.Applicative
import           Control.Exception (assert)

import           Test.QuickCheck


--
-- Simple blockchain data type.
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
                 | RollBack Point
  deriving Show

-- This is the key operation on chains in this model
applyChainUpdate :: ChainUpdate -> Chain -> Chain
applyChainUpdate (AddBlock     b)  c = b:c
applyChainUpdate (RollBack p)      c = go c
    where
    go [] = []
    go (b : bs) | blockPoint b == p = b : bs
                | otherwise         = go bs

applyChainUpdates :: [ChainUpdate] -> Chain -> Chain
applyChainUpdates = flip (foldl (flip applyChainUpdate))

validChainUpdate :: ChainUpdate -> Chain -> Bool
validChainUpdate cu c = validChain (applyChainUpdate cu c)

k :: Int
k = 5 -- maximum fork length in these tests

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
      then AddBlock <$> genBlock (chainHeadBlockId chain)
                                     (chainHeadSlot chain + 1)
      else return $ RollBack (blockPoint (head (drop (n - 1) chain)))

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
-- Data types for a plausibly-realisic representation of a blockchain.
--

-- | Represent a chain simply as a volatile chain fragment.
--
data ChainState = ChainState {
       chainVolatile :: Volatile
     }
  deriving (Eq, Show)

-- | Representation of a chain fragment as a graph with backwards and forward
-- pointers.
--
data Volatile = Volatile
                  (Map BlockId (Block, Maybe BlockId))
                  (Maybe BlockId) -- ^ current tip, or empty
  deriving (Eq, Show)


--
-- The data invariants
--

invChainState   :: ChainState -> Bool
invVolatile     :: Volatile   -> Bool

invChainState (ChainState v) =
    invVolatile  v

invVolatile (Volatile blocks Nothing) =
    -- The whole thing can be empty, with no tip.
    Map.null blocks

invVolatile (Volatile blocks (Just tip)) =
    -- But if it's not empty, then:
    and [
        -- The tip is in the map, and is marked as such
        case Map.lookup tip blocks of
          Just (_, Nothing) -> True; _ -> False

        -- There is only one tip
      , length [ () | (_, Nothing) <- Map.elems blocks ] == 1

        -- all blocks have the right key
      , and [ b == b' | (b, (Block{blockId = b'}, _)) <- Map.toList blocks ]

        -- There is only one dangling back pointer
      , length [ () | (b, _) <- Map.elems blocks
                    , prevBlockId b `Map.notMember` blocks ] == 1

        -- Back pointers have to be consistent with the forward pointer:
        -- following a back pointer gets a block that points forward to the same
      , and [ case Map.lookup (prevBlockId b) blocks of
                Nothing                               -> True
                Just (_, Just bid) | bid == blockId b -> True
                _                                     -> False
            | (b, _) <- Map.elems blocks ]

        -- Forward pointers have to be consistent with the back pointer:
        -- following a forward pointer gets a block that points back to the same
      , and [ case Map.lookup bid' blocks of
                Just (b',_) | prevBlockId b' == blockId b -> True
                _                                         -> False
            | (b, Just bid') <- Map.elems blocks ]

        -- The chain arising must form a valid chain fragment
      , validChainFragment (chainBackwardsFrom blocks tip)

      ]

chainBackwardsFrom :: Map BlockId (Block, Maybe BlockId)
                   -> BlockId
                   -> [Block]
chainBackwardsFrom blocks bid =
    case Map.lookup bid blocks of
      Nothing    -> []
      Just (b,_) -> b : chainBackwardsFrom blocks (prevBlockId b)

chainBackwardsFrom' :: Map BlockId (Block, Maybe BlockId)
                    -> BlockId
                    -> [(Block, Maybe BlockId)]
chainBackwardsFrom' blocks bid =
    case Map.lookup bid blocks of
      Nothing      -> []
      Just e@(b,_) -> e : chainBackwardsFrom' blocks (prevBlockId b)


--
-- The abstraction function
--

absChainState :: ChainState -> Chain
absVolatile   :: Volatile   -> ChainFragment

absVolatile  (Volatile _      Nothing)    = []
absVolatile  (Volatile blocks (Just tip)) = chainBackwardsFrom blocks tip

absChainState (ChainState v) = absVolatile v

--
-- Step 1: empty chains
--

emptyChainState :: ChainState
emptyChainState = ChainState emptyVolatile

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

addBlock :: Block -> ChainState -> ChainState
addBlock b (ChainState v) = ChainState (addBlockVolatile b v)

addBlockVolatile :: Block -> Volatile -> Volatile
addBlockVolatile b (Volatile _ Nothing) =
    Volatile (Map.singleton (blockId b) (b, Nothing)) (Just (blockId b))

addBlockVolatile b' (Volatile blocks (Just tip))
  | prevBlockId b' == tip = Volatile blocks' (Just tip')
  | otherwise             = error "addBlockVolatile: wrong back pointer"
  where
    tip'    = blockId b'
    blocks' = Map.insert tip' (b', Nothing)
            . Map.adjust (\(b, Nothing) -> (b, Just tip')) tip
            $ blocks

-- | For building a chain from empty using the 'addBlock', at each step
-- the invariant holds, and the concrete and abstract values are equivalent
--
prop_addBlock :: TestChain -> Bool
prop_addBlock (TestChain chain) =
    all invChainState steps
 && and [ absChainState c == c'
        | (c, c') <- zip (reverse steps) (tails chain) ]
  where
    steps = scanl (flip addBlock) emptyChainState (reverse chain)


--
-- Step 3: switching forks
--

switchFork :: Int -> [Block] -> ChainState -> ChainState
switchFork rollback newblocks (ChainState v) =
    ChainState (switchForkVolatile rollback newblocks v)

switchForkVolatile :: Int -> [Block] -> Volatile -> Volatile
switchForkVolatile _rollback _newblocks (Volatile _ Nothing) =
    error "switchForkVolatile: precondition violation"

switchForkVolatile rollback newblocks (Volatile blocks (Just tip)) =
    Volatile blocks' (Just tip')
  where
    tip'    = blockId (head newblocks)
    blocks' = fixLink . addBlocks forwards . delBlocks backwards $ blocks

    backwards :: [Block]
    backwards = take rollback (chainBackwardsFrom blocks tip)

    forwards :: [(Block, Maybe BlockId)]
    forwards  = zip newblocks (Nothing : map (Just . blockId) newblocks)

    addBlocks = flip (foldl' (\bs (b,fp) -> Map.insert (blockId b) (b,fp) bs))
    delBlocks = flip (foldl' (\bs  b     -> Map.delete (blockId b)        bs))
    fixLink   = Map.adjust (\(b,_) -> (b, Just rollforwardFrom)) rollbackTo
      where
        rollbackTo      = prevBlockId (last backwards)
        rollforwardFrom = blockId (last newblocks)

rollback :: Point -> ChainState -> ChainState
rollback p (ChainState v) = ChainState (rollbackVolatile p v)

rollbackVolatile :: Point -> Volatile -> Volatile
rollbackVolatile = undefined

applyChainStateUpdate :: ChainUpdate -> ChainState -> ChainState
applyChainStateUpdate (AddBlock     b) = addBlock b
applyChainStateUpdate (RollBack p)     = rollback p

-- | This is now the simulation property covering both the add block and
-- switch fork operations.
--
prop_switchFork :: TestChainAndUpdates -> Bool
prop_switchFork (TestChainAndUpdates chain updates) =
    all invChainState chains'
 && all (\(c, c') -> absChainState c' == c) (zip chains chains')
  where
    c0     = foldr addBlock emptyChainState chain

    chains' = scanl (flip applyChainStateUpdate) c0 updates
    chains  = scanl (flip applyChainUpdate)   chain updates

--
-- Step 4: switching forks again! Roll back and roll forward.
--

-- For the chain following protocol we will need to relax the constraint that
-- we always switch fork all in one go (and to a longer chain), and have to
-- break it up into a rollback followed by adding more blocks.
--
-- Furthermore, it turns out for the chain following protocol that it works
-- better to specify the rollback in terms of where to roll back to, rather
-- than on how many blocks to roll back.

--rollBackToVolatile :: Point -> Volatile -> Volatile
--rollBackToVolatile _ (Volatile _ Nothing) =
--    error "rollBackToVolatile: precondition violation"

--rollBackToVolatile (slot, bid) (Volatile blocks (Just tip)) =


--
-- Read pointer operations
--

-- A 'ChainState' plus an associated set of readers/consumers of the chain.

data ChainProducerState = ChainProducerState {
       chainState   :: ChainState,
       chainReaders :: ReaderStates
     }

-- | Readers are represented here as a relation.
--
type ReaderStates = [ReaderState]

-- | A point on the chain is identified by the 'Slot' number and its 'BlockId'.
-- The 'Slot' tells us where to look and the 'BlockId' either simply serves as
-- a check, or in some contexts it disambiguates blocks from different forks
-- that were in the same slot.
--
type Point        = (Slot, BlockId)

blockPoint :: Block -> Point
blockPoint b = (blockSlot b, blockId b)

type ReaderId     = Int
data ReaderState  = ReaderState {
       -- | Where the chain of the consumer and producer intersect. If the
       -- consumer is on the chain then this is the same as the 'readerHead',
       -- but if the consumer 'readerHead' is off the chain then this is the
       -- point the consumer will need to rollback to.
       readerIntersection :: Point,

       -- | Where the chain consumer was last reading from (typically the
       -- head of the consumer's chain). If this is on the producer chain
       -- then it is equal to the 'readerIntersection'.
       readerHead         :: Point,

       -- | A unique tag per reader, to distinguish different readers.
       readerId           :: ReaderId
     }
  deriving (Eq, Show)


invChainProducerState :: ChainProducerState -> Bool
invChainProducerState (ChainProducerState cs rs) =
    invChainState cs
 && invReaderStates cs rs

invReaderStates :: ChainState -> ReaderStates  -> Bool
invReaderStates cs rs =
    and [
        -- All the reader intersection points must be on the chain
        and [ pointOnChain cs readerIntersection
            | ReaderState{readerIntersection} <- rs ]

        -- All rollback pointer states start from blocks off the chain,
        -- and rollback must go backwards in slot number
      , and [ not (pointOnChain cs readerHead) &&
              fst readerIntersection < fst readerHead
            | ReaderState{readerIntersection, readerHead} <- rs
            , readerIntersection /= readerHead ]

      ]

pointOnChain :: ChainState -> Point -> Bool
pointOnChain (ChainState (Volatile blocks _)) (slot, bid) =
    case Map.lookup bid blocks of
      Just (block, _) -> blockSlot block == slot
      Nothing         -> False


{-
Hmm, perhaps this version does too much, lets simplify

initialiseReadPointer :: [Point]
                      -> ChainState
                      -> Maybe (ChainState, ReadPointer)
initialiseReadPointer checkpoints (ChainState v rs) = do
    (c, c') <- findIntersectionRange checkpoints
    let rs' = (c, readPtr, ) : rs
    return (ChainState v rs')
  where
    readPtr = freshReaderId rs

    findIntersectionRange cs =
      find (checkpointOnChain . fst)
           (zip cs (head cs ++ cs))

-}

-- Given a list of points, find the most recent pair such that older is on the
-- chain and the newer is not.
--
-- > [x, x'] `subseq` xs, not (onChain x), onChain x'
--


initialiseReader :: Point
                 -> Point
                 -> ChainProducerState
                 -> (ChainProducerState, ReaderId)
initialiseReader hpoint ipoint (ChainProducerState cs rs) =
    assert (pointOnChain cs ipoint) $
    (ChainProducerState cs (r:rs), readerId r)
  where
    r = ReaderState {
          readerIntersection = ipoint,
          readerHead         = hpoint,
          readerId           = freshReaderId rs
        }

freshReaderId :: ReaderStates -> ReaderId
freshReaderId rs = 1 + maximum [ readerId | ReaderState{readerId} <- rs ]

updateReader :: ReaderId
             -> Point
             -> Maybe Point
             -> ChainProducerState
             -> ChainProducerState
updateReader rid hpoint mipoint (ChainProducerState cs rs) =
    ChainProducerState cs [ if readerId r == rid then update r else r
                          | r <- rs ]
  where
    update r = case mipoint of
      Nothing     -> r { readerHead = hpoint }
      Just ipoint -> assert (pointOnChain cs ipoint) $
                     r {
                       readerHead         = hpoint,
                       readerIntersection = ipoint
                     }

lookupReader :: ChainProducerState -> ReaderId -> ReaderState
lookupReader (ChainProducerState _ rs) rid = r
  where
    Just r = find (\r -> readerId r == rid) rs

readerInstruction :: ChainProducerState
                  -> ReaderId
                  -> Maybe (ChainProducerState, ConsumeChain Block)
readerInstruction cps rid =
    Nothing
  where
    r = lookupReader cps rid

data ConsumeChain block = RollForward  block
                        | RollBackward Point

improveReaderState :: ChainProducerState
                   -> ReaderId
                   -> [Point]
                   -> ChainProducerState
improveReaderState cps rid = undefined

findIntersection :: ChainProducerState -> Point -> [Point] -> Maybe (Point, Point)
findIntersection (ChainProducerState cs rs) hpoint points =
    go hpoint (hpoint:points)
  where
    go _ []     = Nothing
    go p (p':ps)
      | pointOnChain cs p' = Just (p', p)
      | otherwise          = go p' ps

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
