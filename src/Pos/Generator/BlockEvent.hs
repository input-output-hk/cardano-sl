{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Generator.BlockEvent
       (
       -- * Block apply
         BlockApplyResult(..)
       , BlockEventApply'(..)
       , BlockEventApply
       , beaInput
       , beaOutValid
       -- * Block rollback
       , BlockRollbackResult(..)
       , BlockEventRollback'(..)
       , BlockEventRollback
       , berInput
       , berOutValid
       -- * Block event sum
       , BlockEvent'(..)
       , BlockEvent
       -- * Generation
       , BlockEventCount(..)
       , Chance(..)
       , BlockEventGenParams(..)
       , begpBlockCountMax
       , begpBlockEventCount
       , begpRollbackChance
       , begpFailureChance
       , begpSecrets
       , genBlockEvents
       ) where

import           Universum

import           Control.Lens                (folded, makeLenses)
import           Control.Monad.Random.Strict (MonadRandom (..), RandT, Random (..),
                                              RandomGen, runRand, uniform, weighted)
import           Control.Monad.State         (MonadState (..))
import           Data.Coerce                 (coerce)
import           Data.Default                (def)
import           Data.List                   ((!!))
import qualified Data.List.NonEmpty          as NE
import qualified Data.Semigroup              as Smg

import           Pos.Block.Types             (Blund)
import           Pos.Core                    (BlockCount (..))
import           Pos.Generator.Block         (AllSecrets, BlockGenParams (..),
                                              MonadBlockGen, genBlocks)
import           Pos.Ssc.GodTossing.Type     (SscGodTossing)
import           Pos.Util.Chrono             (NE, NewestFirst (..), OldestFirst (..),
                                              toNewestFirst, toOldestFirst, _NewestFirst,
                                              _OldestFirst)
import           Pos.Util.Util               (minMaxOf)

type BlundDefault = Blund SscGodTossing

----------------------------------------------------------------------------
-- Block event types
----------------------------------------------------------------------------

-- | Determine whether the result of a block event is an expected failure.
class IsBlockEventFailure a where
    isBlockEventFailure :: a -> Bool

data BlockApplyResult
    = BlockApplySuccess
    | BlockApplyFailure {- TODO: attach error info, such as:
                            * block is not a continuation of the chain
                            * block signature is invalid
                            * etc -}

instance IsBlockEventFailure BlockApplyResult where
    isBlockEventFailure = \case
        BlockApplyFailure -> True
        _ -> False

data BlockEventApply' blund = BlockEventApply
    { _beaInput    :: !(OldestFirst NE blund)
    , _beaOutValid :: !BlockApplyResult
    } deriving (Functor, Foldable)

makeLenses ''BlockEventApply'

instance IsBlockEventFailure (BlockEventApply' blund) where
    isBlockEventFailure = isBlockEventFailure . view beaOutValid

type BlockEventApply = BlockEventApply' BlundDefault

data BlockRollbackResult
    = BlockRollbackSuccess
    | BlockRollbackFailure {- TODO: attach error info, such as:
                                * not enough blocks to rollback
                                * rollback limit exceeded
                                * genesis block rollback
                                * etc -}

instance IsBlockEventFailure BlockRollbackResult where
    isBlockEventFailure = \case
        BlockRollbackFailure -> True
        _ -> False

data BlockEventRollback' blund = BlockEventRollback
    { _berInput    :: !(NewestFirst NE blund)
    , _berOutValid :: !BlockRollbackResult
    } deriving (Functor, Foldable)

makeLenses ''BlockEventRollback'

instance IsBlockEventFailure (BlockEventRollback' blund) where
    isBlockEventFailure = isBlockEventFailure . view berOutValid

type BlockEventRollback = BlockEventRollback' BlundDefault

data BlockEvent' blund
    = BlkEvApply (BlockEventApply' blund)
    | BlkEvRollback (BlockEventRollback' blund)
    deriving (Functor, Foldable)

instance IsBlockEventFailure (BlockEvent' blund) where
    isBlockEventFailure = \case
        BlkEvApply    a -> isBlockEventFailure a
        BlkEvRollback a -> isBlockEventFailure a

type BlockEvent = BlockEvent' BlundDefault

----------------------------------------------------------------------------
-- Block event generation
----------------------------------------------------------------------------

{- |
  Block indices. They're neither 0-based nor 1-based, they can start from any
  index (even negative). Therefore it's necessary to adjust them before using
  for lookup in a data structure.
-}
newtype BlockIndex = BlockIndex {getBlockIndex :: Int}
    deriving (Eq, Ord, Num, Real, Integral, Enum, Read, Show,
              Buildable, Generic, Typeable, NFData, Hashable, Random)

newtype BlockEventCount = BlockEventCount {getBlockEventCount :: Word64}
    deriving (Eq, Ord, Num, Real, Integral, Enum, Read, Show,
              Buildable, Generic, Typeable, NFData, Hashable, Random)

-- | A coefficient in the range [0,1]. Pass it to 'weighted' if you ever get
-- the chance.
newtype Chance = Chance {getChance :: Rational}

-- | Generate a boolean that may happen to be of true value.
byChance :: (Monad m, RandomGen g) => Chance -> RandT g m Bool
byChance (Chance c) = weighted [(False, 1 - c), (True, c)]

data BlockEventGenParams = BlockEventGenParams
    { _begpSecrets         :: !AllSecrets
    , _begpBlockCountMax   :: !BlockCount {- ^ the maximum possible amount of
    blocks in a BlockApply event. Must be 1 or more. Can be violated by 1 in
    some cases (see Note on reordering corner cases), so if you specify '52'
    here, some rare events may contain up to '53' blocks. -}
    , _begpBlockEventCount :: !BlockEventCount {- ^ the amount of events to
    generate excluding the last complete rollback event. There can be less
    events if we generate an expected failure. For example, if you specify '10'
    here, you'll either get 11 events (the last is a complete rollback) or
    some amount of events from 1 to 10 (the last is an expected failure).
    If you set the failure chance to '0', you'll always get the requested
    amount of events. -}
    , _begpRollbackChance  :: !Chance
    , _begpFailureChance   :: !Chance
    }

makeLenses ''BlockEventGenParams

{- |
  Return the range of block indices as a half-open interval (closed on the
  lower bound, open on the upper bound). For example, for block indices
  @[0, -2, 4, 7]@ the range is a tuple @(-2, 8)@. The reasons for the interval
  to be half-open:
    * it's possible to encode an empty range as @(k, k)@
    * to compute the amount of elements, one can use simple subtraction
-}

getBlockIndexRange ::
       [BlockEvent' BlockIndex]
    -> (BlockIndex, BlockIndex)
getBlockIndexRange =
    maybe (0, 0) (\(lower, upper) -> (lower, upper+1)) . minMaxOf (folded.folded)

-- | Generate a random sequence of block events. The final event is either an
-- expected failure or a rollback of the entire chain to the initial (empty)
-- state. There's no forking at the moment.
genBlockEvents ::
       (MonadBlockGen ctx m, RandomGen g)
    => BlockEventGenParams
    -> RandT g m [BlockEvent]
genBlockEvents begp = do
    preBlockEvents <- genBlockEvents' begp
    let
        (blockIndexStart, blockIndexEnd) =
            getBlockIndexRange preBlockEvents
        blockCount = BlockCount $
            fromIntegral (blockIndexEnd - blockIndexStart)
    blocks <- genBlocks $ BlockGenParams
        { _bgpSecrets     = begp ^. begpSecrets
        , _bgpBlockCount  = blockCount
        , _bgpTxGenParams = def -- should be better, right?
        , _bgpInplaceDB   = False
        }
    let
        toZeroBased :: BlockIndex -> Int
        toZeroBased i =
            getBlockIndex i - getBlockIndex blockIndexStart
        getBlock :: BlockIndex -> BlundDefault
        getBlock i =
            -- Indexing is safe here because by construction we have enough
            -- blocks for the lookup to succeed.
            getOldestFirst blocks !! toZeroBased i
    return $ fmap getBlock <$> preBlockEvents

data GenBlockEventState
    = GbesStartingState
    | GbesBlockchain (OldestFirst NE BlockIndex)
    | GbesExpectedFailure

-- | A version of 'genBlockEvents' that generates event with block indices
-- instead of actual blocks.
genBlockEvents' ::
       (Monad m, RandomGen g)
    => BlockEventGenParams
    -> RandT g m [BlockEvent' BlockIndex]
genBlockEvents' begp =
    flip evalStateT GbesStartingState $ do
        events <- replicateWhileM
            (fromIntegral $ begp ^. begpBlockEventCount)
            (genBlockEvent begp)
        finalEvent <- get <&> \case
            GbesStartingState -> []
            GbesExpectedFailure -> []
            GbesBlockchain blockchain ->
                -- Rollback the entire blockchain.
                [BlkEvRollback $ BlockEventRollback
                    { _berInput    = toNewestFirst blockchain
                    , _berOutValid = BlockRollbackSuccess
                    }]
        return $ events ++ finalEvent

replicateWhileM :: Monad m => Int -> m (Maybe a) -> m [a]
replicateWhileM n m = go n
  where
    go k | k < 1 = return []
         | otherwise =
             m >>= \case
                 Nothing -> return []
                 Just a  -> (a:) <$> go (k-1)

data RollbackFailureType
    = RftRollbackExcess
    | RftRollbackDrop
    deriving (Enum, Bounded)

instance Random RollbackFailureType where
    random = randomR (minBound, maxBound)
    randomR (lo,hi) = runRand $ uniform [lo..hi]

data ApplyFailureType
    = AftApplyBad
    | AftApplyNonCont
    deriving (Enum, Bounded)

instance Random ApplyFailureType where
    random = randomR (minBound, maxBound)
    randomR (lo,hi) = runRand $ uniform [lo..hi]

newtype IsFailure = IsFailure Bool
newtype IsRollback = IsRollback Bool

genBlockEvent ::
       (Monad m, RandomGen g)
    => BlockEventGenParams
    -> StateT GenBlockEventState (RandT g m) (Maybe (BlockEvent' BlockIndex))
genBlockEvent begp = do
    gbes <- get
    failure <- lift $ IsFailure <$> byChance (begp ^. begpFailureChance)
    rollback <- lift $ IsRollback <$> byChance (begp ^. begpRollbackChance)
    (mBlockEvent, gbes') <- case gbes of
        GbesExpectedFailure ->
            return (Nothing, gbes)
        GbesStartingState ->
            genBlockStartingState failure rollback
        GbesBlockchain blockchain ->
            genBlockInBlockchain blockchain failure rollback
    put gbes' $> mBlockEvent
  where
    genBlockIndices blockIndexStart = do
        len <- getRandomR (1, fromIntegral $ begp ^. begpBlockCountMax)
        -- 'NE.fromList' assumes that 'len >= 1', which holds because we require
        -- 'begpBlockCountMax >= 1'.
        return $ OldestFirst . NE.fromList . take len $ [blockIndexStart..]

    -- Fail with rollback. In the starting state it's easy,
    -- because any rollback will fail when we don't have any
    -- blocks yet.
    genBlockStartingState (IsFailure True) (IsRollback True) = do
        blockIndices <- genBlockIndices 0
        let
            ev = BlkEvRollback $ BlockEventRollback
                { _berInput    = toNewestFirst blockIndices
                , _berOutValid = BlockRollbackFailure
                }
            gbes = GbesExpectedFailure
        return (Just ev, gbes)
    -- Fail without rollback (with apply). In the starting state,
    -- the only way to do this is to generate a sequence of blocks
    -- invalid by itself.
    genBlockStartingState (IsFailure True) (IsRollback False) = do
        blockIndices <- genBlockIndices 1
        let
            blkZeroth = BlockIndex 0 :| []
            blockIndices' =
                -- Those block indices are broken by construction
                -- because we append the 0-th block to the end.
                -- Sometimes we can violate the maximum bound on
                -- block count, but this is a documented infelicity.
                -- See NOTE on reordering corner cases.
                over _OldestFirst (Smg.<> blkZeroth) blockIndices
            ev =
                BlkEvApply $ BlockEventApply
                { _beaInput    = blockIndices'
                , _beaOutValid = BlockApplyFailure
                }
            gbes = GbesExpectedFailure
        return (Just ev, gbes)
    -- Succeed with or without rollback. Unfortunately, it's
    -- impossible to successfully rollback any blocks when
    -- there are no blocks, so we will generate a block apply
    -- event in both cases.
    genBlockStartingState (IsFailure False) (IsRollback _) = do
        blockIndices <- genBlockIndices 0
        let
            ev = BlkEvApply $ BlockEventApply
                { _beaInput    = blockIndices
                , _beaOutValid = BlockApplySuccess
                }
            gbes = GbesBlockchain blockIndices
        return (Just ev, gbes)

    -- Fail with rollback.
    genBlockInBlockchain blockchain (IsFailure True) (IsRollback True) = do
        rft <- getRandom
        case rft of
            RftRollbackExcess -> do
                -- Attempt to rollback the entire blockchain and then
                -- some more. Example: suppose we have blockchain [0, 1, 2],
                -- we may generate rollback [2, 1, 0, -1], rollback of the
                -- -1-st block will be unsuccessful.
                let
                    blockIndices = blockchain & over _OldestFirst
                        (\(x :| xs) -> x-1 :| x : xs)
                    ev = BlkEvRollback $ BlockEventRollback
                        { _berInput    = toNewestFirst blockIndices
                        , _berOutValid = BlockRollbackFailure
                        }
                    gbes = GbesExpectedFailure
                return (Just ev, gbes)
            RftRollbackDrop -> do
                -- Attempt to rollback a part of the blockchain which
                -- is not its prefix (drop some blocks from the tip).
                -- The corner case here is that we may have a
                -- one-element blockchain: then we ensure failure by
                -- asking to rollback more blocks.
                case blockchain of
                    OldestFirst (x :| []) -> do -- oops, the corner case
                        let
                            gbes = GbesExpectedFailure
                            ev = BlkEvRollback $ BlockEventRollback
                                { _berInput    = NewestFirst $ x :| [x-1]
                                , _berOutValid = BlockRollbackFailure
                                }
                        return (Just ev, gbes)
                    _ -> do
                        -- Here we decide how many blocks to rollback.
                        -- This will yield a valid rollback, so we're
                        -- going to drop a single block from the tip.
                        -- Therefore, 'len' should be no less than 2,
                        -- otherwise we won't have a non-empty list
                        -- after the drop.
                        len <- getRandomR (2, length blockchain)
                        let
                            -- 'NE.fromList' is valid here because:
                            --    * the input has more than two elements
                            --    * 'len >= 2'
                            select = NE.fromList . drop 1 . NE.take len
                            blockIndices = toNewestFirst blockchain &
                                over _NewestFirst select
                            ev = BlkEvRollback $ BlockEventRollback
                                { _berInput    = blockIndices
                                , _berOutValid = BlockRollbackFailure
                                }
                            gbes = GbesExpectedFailure
                        return (Just ev, gbes)
    -- Fail without rollback (with apply).
    genBlockInBlockchain blockchain (IsFailure True) (IsRollback False) = do
        aft <- getRandom
        case aft of
            AftApplyBad -> do
                -- Attempt to apply an invalid sequence of blocks.
                let tip = NE.last (getOldestFirst blockchain)
                blockIndices <- genBlockIndices (tip + 1)
                let
                    blockIndices' =
                        -- Those block indices are broken by construction
                        -- because we append the current tip to the end.
                        -- Sometimes we can violate the maximum bound on
                        -- block count, but this is a documented infelicity.
                        -- See NOTE on reordering corner cases.
                        over _OldestFirst (Smg.<> pure tip) blockIndices
                    ev = BlkEvApply $ BlockEventApply
                        { _beaInput    = blockIndices'
                        , _beaOutValid = BlockApplyFailure
                        }
                    gbes = GbesExpectedFailure
                return (Just ev, gbes)
            AftApplyNonCont -> do
                -- Attempt to apply blocks which are not a valid
                -- contuniation of our chain.
                let
                    tip  = NE.last (getOldestFirst blockchain)
                    -- The amount of blocks to skip. One is enough.
                    skip = 1
                blockIndices <- genBlockIndices (tip + 1 + skip)
                let
                    ev = BlkEvApply $ BlockEventApply
                        { _beaInput    = blockIndices
                        , _beaOutValid = BlockApplyFailure
                        }
                    gbes = GbesExpectedFailure
                return (Just ev, gbes)
    -- Success with rollback.
    genBlockInBlockchain blockchain (IsFailure False) (IsRollback True) = do
        len <- getRandomR (1, length blockchain)
        let
            (blockIndices, remainingBlockchain) = splitAtNewestFirst len $
                toNewestFirst blockchain
            -- 'NE.fromList' is valid here because 'len >= 1'.
            blockIndices' = over _NewestFirst NE.fromList blockIndices
            remainingBlockchain' = nonEmptyOldestFirst $
                toOldestFirst remainingBlockchain
            ev = BlkEvRollback $ BlockEventRollback
                { _berInput    = blockIndices'
                , _berOutValid = BlockRollbackSuccess
                }
            gbes = maybe GbesStartingState GbesBlockchain remainingBlockchain'
        return (Just ev, gbes)
    -- Success without rollback (with apply).
    genBlockInBlockchain blockchain (IsFailure False) (IsRollback False) = do
        let tip = NE.last (getOldestFirst blockchain)
        blockIndices <- genBlockIndices (tip + 1)
        let
            ev = BlkEvApply $ BlockEventApply
                { _beaInput = blockIndices
                , _beaOutValid = BlockApplySuccess
                }
            gbes = GbesBlockchain (blockchain Smg.<> blockIndices)
        return (Just ev, gbes)

splitAtNewestFirst ::
    forall a.
       Int
    -> NewestFirst NE a
    -> (NewestFirst [] a, NewestFirst [] a)
splitAtNewestFirst = coerce (NE.splitAt @a)

nonEmptyOldestFirst ::
    forall a.
       OldestFirst [] a
    -> Maybe (OldestFirst NE a)
nonEmptyOldestFirst = coerce (nonEmpty @a)

{- NOTE: Reordering corner cases
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When reordering blocks to get an invalid sequence, there's a corner case:
if there's only one block, it's impossible to get an invalid ordering.
The reason one might have only one block depends on the means of generation,
but for 'genBlockIndices' there are two possible causes:

* we rolled 'blockIndexEnd' equal to 'blockIndexStart'
* 'blockIndexMax' is 1

To avoid dealing with this, we append one more block. The downside is that
appending this block may cause the sequence to be longer than 'blockIndexMax'.

-}
