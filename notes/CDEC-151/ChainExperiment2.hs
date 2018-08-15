{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module ChainExperiment2 where

import           Data.Hashable
import           Data.List (find, foldl', intersect, tails)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
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
                   -> [Block] -- ^ newest first
chainBackwardsFrom blocks bid =
    case Map.lookup bid blocks of
      Nothing    -> []
      Just (b,_) -> b : chainBackwardsFrom blocks (prevBlockId b)

-- |
-- @'chainBackwardsFrom'@ returns a list of blocks ordered from newest to
-- oldest.
invChainBackwardFrom
    :: Map BlockId (Block, Maybe BlockId)
    -> Bool
invChainBackwardFrom blocks =
    all (\bid -> go (chainBackwardsFrom blocks bid)) (Map.keys blocks)
    where
    go :: [Block] -> Bool
    go []  = True
    go [_] = True
    go (x : y : ys) = prevBlockId x == blockId y && go (y : ys)

chainBackwardsFromTo
    :: Map BlockId (Block, Maybe BlockId)
    -> BlockId
    -- ^ from
    -> BlockId
    -- ^ to
    -> Maybe [Block]
    -- ^ newest first, it is guaranteed that the list ends on the block after
    -- @toBid@ block
chainBackwardsFromTo blocks fromBid toBid =
    case Map.lookup fromBid blocks of
        Nothing -> Nothing
        Just (b, _)
            | blockId b == toBid
                -> Just []
            | otherwise
                -> fmap (b :) $ chainBackwardsFromTo blocks (prevBlockId b) toBid

chainBackwardsFrom' :: Map BlockId (Block, Maybe BlockId)
                    -> BlockId
                    -> [(Block, Maybe BlockId)] -- ^ newest first
chainBackwardsFrom' blocks bid =
    case Map.lookup bid blocks of
      Nothing      -> []
      Just e@(b,_) -> e : chainBackwardsFrom' blocks (prevBlockId b)


chainForwardFrom :: Map BlockId (Block, Maybe BlockId)
                 -> BlockId
                 -> [Block] -- ^ oldest first
chainForwardFrom blocks blockId = go blockId []
    where
    go :: BlockId -> [Block] -> [Block]
    go bid !acu = case Map.lookup bid blocks of
        Nothing                 -> []
        Just (block, Nothing)   -> [block]
        Just (block, Just bid') -> go bid' (block : acu)

-- |
-- @'chainForwardFrom'@ returns a list of blocks ordered from  oldest to newest.
invChainForwardForm
    :: Map BlockId (Block, Maybe BlockId)
    -> Bool
invChainForwardForm blocks = all (\blockId -> go $ chainForwardFrom blocks blockId) (Map.keys blocks)
    where
    go []  = True
    go [_] = True
    go (x : y : ys) = blockId x == prevBlockId y && go (y : ys)

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

-- TODO: perhaps we shouldn't remove blocks too eagerly or maybe we should have
-- a cache of blocks at some level so we don't need to redownload in case of two
-- competing tines.
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

readerInstructions
    :: ChainProducerState
    -> ReaderId
    -> [ConsumeChain Block]
readerInstructions cps@(ChainProducerState (ChainState (Volatile blocks _)) _) rid =
    let ReaderState{readerIntersection, readerHead} = lookupReader cps rid
        ccs = map RollForward $ chainForwardFrom blocks (snd readerHead)
    in if readerIntersection == readerHead
        then ccs
        else case chainBackwardsFromTo blocks (snd readerHead) (snd readerIntersection) of
            Nothing -> []
            Just bs -> (fmap (RollBackward . blockPoint) bs) ++ ccs

-- |
-- Rollback pointers if we are switching to another tine (which will remove
-- blocks from volatile fork).
-- This function must be run before `switchForkVolatile` otherwise we will loose
-- pointers.
--
-- Assumptions:
--  * rollback should be shallower than the length of the tine,
--    otherewise pointers might get out of range
normalizeChainProducerState
    :: ChainUpdate
    -> ChainProducerState
    -> ChainProducerState
normalizeChainProducerState (AddBlock _) cps = cps
normalizeChainProducerState _ (ChainProducerState {chainState=ChainState (Volatile _ Nothing)})
    = error "normalizeChainState: precondition validation"
normalizeChainProducerState
    (RollBack p)
    (cps@ChainProducerState
        { chainState   = cs@(ChainState (Volatile blocks (Just tip)))
        , chainReaders = rs
        })
    = case takeUntil p $ chainBackwardsFrom blocks tip of
        []     -> cps
        (b:bs) ->
            let newPoint = blockPoint b
            in ChainProducerState
                { chainState   = cs
                , chainReaders = foldl' (updateReaderStates newPoint) rs bs
                }
    where
    -- take blocks until the point is met (including)
    takeUntil :: Point -> [Block] -> [Block]
    takeUntil p = go []
        where
        go xs [] = xs
        go xs (b : bs) =
            if blockPoint b == p
                then b : xs
                else go (b : xs) bs

    updatePointer
        :: Block -- ^ block which will be rolled back
        -> Point -- ^ new point
        -> Point -- ^ current point
        -> Point -- ^ updated point
    updatePointer b newPoint oldPoint =
        if blockPoint b == oldPoint
            then newPoint
            else oldPoint

    updateReaderStates :: Point -> [ReaderState] -> Block -> [ReaderState]
    updateReaderStates newPoint rs b = map (updateReaderState newPoint b) rs

    updateReaderState :: Point -> Block -> ReaderState -> ReaderState
    updateReaderState newPoint b ReaderState {readerIntersection, readerHead, readerId}
        = ReaderState
            { readerIntersection = updatePointer b newPoint readerIntersection
            , readerHead         = updatePointer b newPoint readerHead
            , readerId
            }

applyChainProducerUpdate :: ChainUpdate -> ChainProducerState -> ChainProducerState
applyChainProducerUpdate cu (cps@ChainProducerState {chainState, chainReaders})
    = (normalizeChainProducerState cu cps) { chainState = applyChainStateUpdate cu chainState }

invApplyChainProducerUpdate :: ChainUpdate -> ChainProducerState ->  Bool
invApplyChainProducerUpdate cu cps = case applyChainProducerUpdate cu cps of
    ChainProducerState
        { chainState    = ChainState (Volatile blocks _)
        , chainReaders
        } -> and
            [
              -- all pointers should be still in volatile chain
              and [ Map.member intersectionBlockId blocks && Map.member readerBlockId blocks
                  | ReaderState
                      { readerIntersection = (_, intersectionBlockId)
                      , readerHead         = (_, readerBlockId)
                      } <- chainReaders
                  ]
            ]

data ConsumeChain block = RollForward  block
                        | RollBackward Point

-- It does not validate if the new reader state is an improvment over the old
-- one.  If a node (an attacker) will force us to use older intersection point,
-- he will get longer instruction sets, so it's in its interest to send honest
-- information.
--
-- Assumptions:
--  * each reader is placed only once in `chainReaders` list (we update only the
--    first one in the list)
improveReaderState :: ChainProducerState
                   -> ReaderId
                   -> [Point] -- newest first
                   -> ChainProducerState
improveReaderState cps _ []  = cps
improveReaderState
    (cps@ChainProducerState
        { chainState = ChainState (Volatile _ Nothing)
        })
    _ _ = cps
improveReaderState
    (cps@ChainProducerState
        { chainState   = ChainState (Volatile blocks (Just tip))
        , chainReaders = rs
        })
    rid ps = cps { chainReaders = updateReader rs }
    where
    updateReader :: [ReaderState] -> [ReaderState]
    updateReader [] = []
    updateReader (r : rs) =
        if readerId r == rid
            then go Nothing ps r : rs
            else r : updateReader rs

    go :: Maybe Point -> [Point] -> ReaderState -> ReaderState
    go _ []                  rs = rs
    go readerHead (point@(_, blockId) : ps') rs =
        let -- check if point is a new readerHead
            readerHead' = case readerHead of
                Just _  -> readerHead
                Nothing -> if Map.member blockId blocks
                            then Just point
                            else Nothing
        in case chainBackwardsFrom blocks blockId of
            []          -> go readerHead' ps' rs -- ^ blockId is not in volatile chain
            readersTine -> case chainBackwardsFrom blocks tip of
                []            -> go readerHead' ps' rs
                producersTine -> case readersTine `intersect_` producersTine of
                    Nothing                 -> go readerHead' ps' rs
                    Just readerIntersection -> ReaderState
                        { readerIntersection
                        , readerHead = fromMaybe point readerHead'
                        , readerId   = rid
                        }

    -- intersect two tines
    intersect_ :: [Block] -> [Block] -> Maybe Point
    intersect_ bs bs' =
        case map blockPoint bs `intersect` map blockPoint bs' of
            p : _ -> Just p
            []    -> Nothing

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
