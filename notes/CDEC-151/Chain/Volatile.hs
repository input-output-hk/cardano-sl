{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
module Chain.Volatile where

-- import Data.Word
import           Data.List (foldl', intersect, tails)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)

import           Block (Block (..), BlockId, Point, ReaderId, ReaderState (..), ReaderStates,
                        blockPoint)
import qualified Chain.Abstract as Chain.Abs
import           Chain.Update (ChainUpdate (..))
import           ChainExperiment2 (ChainProducerState (..), ConsumeChain (..), lookupReader)

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
-- The abstraction function
--

absChainState :: ChainState -> Chain.Abs.Chain
absVolatile   :: Volatile   -> Chain.Abs.ChainFragment

absVolatile  (Volatile _      Nothing)    = []
absVolatile  (Volatile blocks (Just tip)) = chainBackwardsFrom blocks tip

absChainState (ChainState v) = absVolatile v

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
      , Chain.Abs.validChainFragment (chainBackwardsFrom blocks tip)

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
    go []           = True
    go [_]          = True
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
    go []           = True
    go [_]          = True
    go (x : y : ys) = blockId x == prevBlockId y && go (y : ys)

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
                    && null (absChainState emptyChainState)

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
            . Map.adjust (\(b, _) -> (b, Just tip')) tip
            $ blocks

-- | For building a chain from empty using the 'addBlock', at each step
-- the invariant holds, and the concrete and abstract values are equivalent
prop_addBlock :: Chain.Abs.TestChain -> Bool
prop_addBlock (Chain.Abs.TestChain chain) =
    all invChainState steps
 && and [ absChainState c == c'
        | (c, c') <- zip (reverse steps) (tails chain) ]
  where
    steps  = scanl (flip addBlock) emptyChainState (reverse chain)

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

applyChainUpdate :: ChainUpdate -> ChainState -> ChainState
applyChainUpdate (AddBlock b) = addBlock b
applyChainUpdate (RollBack p) = rollback p

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

readerInstructions
    :: ChainProducerState ChainState
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
    -> ChainProducerState ChainState
    -> ChainProducerState ChainState
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

-- It does not validate if the new reader state is an improvment over the old
-- one.  If a node (an attacker) will force us to use older intersection point,
-- he will get longer instruction sets, so it's in its interest to send honest
-- information.
--
-- Assumptions:
--  * each reader is placed only once in `chainReaders` list (we update only the
--    first one in the list)
improveReaderState :: ChainProducerState ChainState
                   -> ReaderId
                   -> [Point] -- newest first
                   -> ChainProducerState ChainState
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

findIntersection
    :: ChainState
    -> Point
    -> [Point]
    -> Maybe (Point, Point)
findIntersection cs hpoint points =
    go hpoint (hpoint : points)
  where
    go _ []                = Nothing
    go p (p':ps)
      | pointOnChain cs p' = Just (p', p)
      | otherwise          = go p' ps
