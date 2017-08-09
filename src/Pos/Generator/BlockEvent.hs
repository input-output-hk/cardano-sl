{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Generator.BlockEvent
       (
       -- * Util
         IsBlockEventFailure(..)
       -- * Block apply
       , BlockApplyResult(..)
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
       -- * Snapshot management
       , SnapshotId(..)
       , SnapshotOperation(..)
       , enrichWithSnapshotChecking
       , CheckCount(..)
       -- * Block event sum
       , BlockEvent'(..)
       , BlockEvent
       -- * Block scenario
       , BlockScenario'(..)
       , BlockScenario
       , _BlockScenario
       -- * Path
       , PathSegment(..)
       , Path(..)
       , pathSequence
       -- * Chance
       , Chance(..)
       , byChance
       -- * Block description
       , BlockDesc(..)
       -- * Generation
       , MonadBlockGen
       , BlockEventCount(..)
       , genBlocksInStructure
       ) where

import           Universum

import           Control.Lens                (foldMapOf, folded, makeLenses, makePrisms)
import           Control.Monad.Random.Strict (RandT, Random (..), RandomGen, mapRandT,
                                              weighted)
import           Data.Default                (def)
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as Map
import qualified Data.Sequence               as Seq
import qualified Data.Text.Buildable
import           Formatting                  (bprint, build, sformat, shown, (%))
import qualified Prelude
import           Serokell.Util               (listJson)

import           Pos.Block.Types             (Blund)
import           Pos.Core                    (HeaderHash, headerHash, prevBlockL)
import           Pos.Crypto.Hashing          (hashHexF)
import           Pos.Generator.Block         (AllSecrets, BlockGenParams (..),
                                              MonadBlockGen, TxGenParams, genBlocks)
import           Pos.GState.Context          (withClonedGState)
import           Pos.Ssc.GodTossing.Type     (SscGodTossing)
import           Pos.Util.Chrono             (NE, NewestFirst (..), OldestFirst (..),
                                              toNewestFirst, toOldestFirst, _OldestFirst)

type BlundDefault = Blund SscGodTossing

----------------------------------------------------------------------------
-- Blockchain tree
----------------------------------------------------------------------------

-- NB. not a monoid, so the user can be sure that `(<>)` acts as expected
-- on string literals for paths (not path segments).
newtype PathSegment = PathSegment { pathSegmentText :: Text }
    deriving (Eq, Ord, IsString)

instance Show PathSegment where
    showsPrec p (PathSegment s) = Prelude.showsPrec p s

-- | Construct a path using string literals and monoidal/semigroupoidal
--   operations: @"first" <> "next"@, @stimes 15 "next"@.
--   An empty path does not point at any block, don't use it on its own! (but
--   it's a valid 'mempty')
newtype Path = Path (Seq PathSegment)
    deriving (Eq, Ord, Semigroup, Monoid)

instance IsString Path where
    fromString = Path . Seq.singleton . fromString

instance Buildable Path where
    build (Path segs) = bprint build
        (fold . intersperse "/" . toList $ pathSegmentText <$> segs)

-- | Convert a sequence of relative paths into a sequence of absolute paths:
--   @pathSequence "base" ["a", "b", "c"] = ["base" <> "a", "base" <> "a" <> "b", "base" <> "a" <> "b" <> "c"]
pathSequence ::
       Path -- ^ base path
    -> OldestFirst NE Path -- ^ relative paths
    -> OldestFirst NE Path -- ^ absolute paths
pathSequence basePath = over _OldestFirst $
    -- 'NE.fromList' here is safe because `NE.inits` applied to `NonEmpty` has
    -- at least two elements (first is [], second is a singleton list), so even
    -- after `NE.tail` we've still got a non-empty sequence.
    fmap (mappend basePath . mconcat) . NE.fromList . NE.tail . NE.inits

type BlockchainForest a = Map PathSegment (BlockchainTree a)

data BlockchainTree a = BlockchainTree a (BlockchainForest a)
  deriving (Show, Functor, Foldable)

data BlockDesc
    = BlockDescDefault -- a random valid block
    | BlockDescCustom TxGenParams -- a random valid block with custom gen params
    deriving (Show)

-- Precondition: input paths are non-empty
buildBlockchainForest :: a -> Map Path a -> BlockchainForest a
buildBlockchainForest defElem elements =
    fmap (buildBlockchainTree defElem) . Map.fromListWith Map.union $ do
        (Path path, a) <- Map.toList elements
        case Seq.viewl path of
            Seq.EmptyL -> error
                "buildBlockchainForest: precondition violated, empty path"
            pathSegment Seq.:< path' ->
                [(pathSegment, Map.singleton (Path path') a)]

buildBlockchainTree :: a -> Map Path a -> BlockchainTree a
buildBlockchainTree defElem elements =
    let
        topPath = Path Seq.empty
        topElement = Map.findWithDefault defElem topPath elements
        -- 'otherElements' has its empty path deleted (if there was one in the
        -- first place), so it satisfies the precondition of 'buildBlockchainForest'
        otherElements = Map.delete topPath elements
    in
        BlockchainTree topElement (buildBlockchainForest defElem otherElements)

-- Inverse to 'buildBlockchainForest'.
flattenBlockchainForest' :: BlockchainForest a -> Map Path a
flattenBlockchainForest' =
    Map.fromList . flattenBlockchainForest (Path Seq.empty)

flattenBlockchainForest :: Path -> BlockchainForest a -> [(Path, a)]
flattenBlockchainForest (Path prePath) forest = do
    (pathSegment, subtree) <- Map.toList forest
    flattenBlockchainTree (Path $ prePath Seq.|> pathSegment) subtree

flattenBlockchainTree :: Path -> BlockchainTree a -> [(Path, a)]
flattenBlockchainTree prePath tree = do
    let BlockchainTree a forest = tree
    (prePath, a) : flattenBlockchainForest prePath forest

genBlocksInForest ::
       (MonadBlockGen ctx m, RandomGen g)
    => AllSecrets
    -> BlockchainForest BlockDesc
    -> RandT g m (BlockchainForest BlundDefault)
genBlocksInForest secrets =
    traverse $ mapRandT withClonedGState . genBlocksInTree secrets

genBlocksInTree ::
       (MonadBlockGen ctx m, RandomGen g)
    => AllSecrets
    -> BlockchainTree BlockDesc
    -> RandT g m (BlockchainTree BlundDefault)
genBlocksInTree secrets blockchainTree = do
    let
        BlockchainTree blockDesc blockchainForest = blockchainTree
        txGenParams = case blockDesc of
            BlockDescDefault  -> def
            BlockDescCustom p -> p
        blockGenParams = BlockGenParams
            { _bgpSecrets     = secrets
            , _bgpBlockCount  = 1
            , _bgpTxGenParams = txGenParams
            , _bgpInplaceDB   = True
            }
    -- Partial pattern-matching is safe because we specify
    -- blockCount = 1 in the generation parameters.
    OldestFirst [block] <- genBlocks blockGenParams
    forestBlocks <- genBlocksInForest secrets blockchainForest
    return $ BlockchainTree block forestBlocks

-- Precondition: paths in the structure are non-empty.
genBlocksInStructure ::
       ( MonadBlockGen ctx m
       , Functor t, Foldable t
       , RandomGen g )
    => AllSecrets
    -> Map Path BlockDesc
    -> t Path
    -> RandT g m (t BlundDefault)
genBlocksInStructure secrets annotations s = do
    let
        getAnnotation :: Path -> BlockDesc
        getAnnotation path =
            Map.findWithDefault BlockDescDefault path annotations
        paths :: Map Path BlockDesc
        paths = foldMapOf folded
            (\path -> Map.singleton path (getAnnotation path))
            s
        descForest :: BlockchainForest BlockDesc
        descForest = buildBlockchainForest BlockDescDefault paths
    blockForest :: BlockchainForest BlundDefault <-
        genBlocksInForest secrets descForest
    let
        getBlock :: Path -> BlundDefault
        getBlock path = Map.findWithDefault
            (error "genBlocksInStructure: impossible happened")
            path
            (flattenBlockchainForest' blockForest)
    return $ fmap getBlock s

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
    deriving (Show)

instance IsBlockEventFailure BlockApplyResult where
    isBlockEventFailure = \case
        BlockApplyFailure -> True
        _ -> False

data BlockEventApply' blund = BlockEventApply
    { _beaInput    :: !(OldestFirst NE blund)
    , _beaOutValid :: !BlockApplyResult
    } deriving (Show, Functor, Foldable)

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
    deriving (Show)

instance IsBlockEventFailure BlockRollbackResult where
    isBlockEventFailure = \case
        BlockRollbackFailure -> True
        _ -> False

data BlockEventRollback' blund = BlockEventRollback
    { _berInput    :: !(NewestFirst NE blund)
    , _berOutValid :: !BlockRollbackResult
    } deriving (Show, Functor, Foldable)

makeLenses ''BlockEventRollback'

instance IsBlockEventFailure (BlockEventRollback' blund) where
    isBlockEventFailure = isBlockEventFailure . view berOutValid

type BlockEventRollback = BlockEventRollback' BlundDefault

newtype SnapshotId = SnapshotId Text
    deriving (Eq, Ord, IsString)

instance Show SnapshotId where
    showsPrec p (SnapshotId s) = Prelude.showsPrec p s

data SnapshotOperation
    = SnapshotSave SnapshotId {- Save the current db state into a snapshot
    under the specified identifier. Overwrites an existing snapshot with the
    same name or creates a new one. -}
    | SnapshotLoad SnapshotId {- Set the current db state to a state saved in
    a snapshot earlier. -}
    | SnapshotEq SnapshotId {- Compare the current db state to a state saved
    in a snapshot earlier, checking for equivalence. If logical discrepancies
    are found, throw an error. -}
    deriving (Show)

instance Buildable SnapshotOperation where
    build = bprint shown

data BlockEvent' blund
    = BlkEvApply (BlockEventApply' blund)
    | BlkEvRollback (BlockEventRollback' blund)
    | BlkEvSnap SnapshotOperation
    deriving (Show, Functor, Foldable)

instance Buildable blund => Buildable (BlockEvent' blund) where
    build = \case
        BlkEvApply ev -> bprint ("Apply blocks: "%listJson) (getOldestFirst $ ev ^. beaInput)
        BlkEvRollback ev -> bprint ("Rollback blocks: "%listJson) (getNewestFirst $ ev ^. berInput)
        BlkEvSnap s -> bprint build s

instance IsBlockEventFailure (BlockEvent' blund) where
    isBlockEventFailure = \case
        BlkEvApply    a -> isBlockEventFailure a
        BlkEvRollback a -> isBlockEventFailure a
        BlkEvSnap     _ -> False

type BlockEvent = BlockEvent' BlundDefault

newtype BlockScenario' blund = BlockScenario [BlockEvent' blund]
    deriving (Show, Functor, Foldable)

instance Buildable blund => Buildable (BlockScenario' blund) where
    build (BlockScenario xs) = bprint listJson xs

type BlockScenario = BlockScenario' BlundDefault

makePrisms ''BlockScenario'

----------------------------------------------------------------------------
-- Block event generation
----------------------------------------------------------------------------

newtype BlockEventCount = BlockEventCount {getBlockEventCount :: Word64}
    deriving (Eq, Ord, Num, Real, Integral, Enum, Read, Show,
              Buildable, Generic, Typeable, NFData, Hashable, Random)

-- | A coefficient in the range [0,1]. Pass it to 'weighted' if you ever get
-- the chance.
newtype Chance = Chance {getChance :: Rational}
    deriving (Buildable, Num, Fractional)

-- | Generate a boolean that may happen to be of true value.
byChance :: (Monad m, RandomGen g) => Chance -> RandT g m Bool
byChance (Chance c) = weighted [(False, 1 - c), (True, c)]

newtype CheckCount = CheckCount Word
    deriving (Eq, Ord, Show, Num)

-- The tip after the block event. 'Nothing' when the event doesn't affect the tip.
blkEvTip :: BlockEvent -> Maybe HeaderHash
blkEvTip = \case
    BlkEvApply bea -> Just $
        (headerHash . NE.head . getNewestFirst . toNewestFirst . view beaInput) bea
    BlkEvRollback ber -> Just $
        (headerHash . view prevBlockL . NE.head . getOldestFirst . toOldestFirst . view berInput) ber
    _ -> Nothing

-- | Empty: hash snapshot unavailable
--   Zero: hash snapshot available but unused
--   Other: hash snapshot was used N times
type HhStatusMap = Map HeaderHash CheckCount

hhSnapshotId :: HeaderHash -> SnapshotId
hhSnapshotId = SnapshotId . sformat hashHexF

-- | Whenever the resulting tips of apply/rollback operations coincide,
-- add a snapshot equivalence comparison.
enrichWithSnapshotChecking :: BlockScenario -> (BlockScenario, CheckCount)
enrichWithSnapshotChecking (BlockScenario bs) = (BlockScenario bs', checkCount)
  where
    checkCount = sum (hhStatusEnd :: HhStatusMap)
    (hhStatusEnd, revBs') = go Map.empty [] bs
    bs' = reverse revBs'
    -- NB. 'go' is tail-recursive.
    go hhStatusSoFar revNewBs = \case
        [] -> (hhStatusSoFar, revNewBs)
        (ev:evs) -> case blkEvTip ev of
            Nothing -> go hhStatusSoFar (ev:revNewBs) evs
            Just tipHh -> case Map.lookup tipHh hhStatusSoFar of
                Nothing ->
                    let
                        snapSave = BlkEvSnap (SnapshotSave (hhSnapshotId tipHh))
                        needSnapSave =
                            -- We tie the knot here to determine whether to actually emit
                            -- the command to save the snapshot.
                            Map.findWithDefault 0 tipHh hhStatusEnd > 0
                        appendSnapSave = if needSnapSave then (snapSave:) else identity
                    in
                        go (Map.insert tipHh 0 hhStatusSoFar) (appendSnapSave $ ev:revNewBs) evs
                Just k ->
                    let snapEq = BlkEvSnap (SnapshotEq (hhSnapshotId tipHh))
                    in go (Map.insert tipHh (1+k) hhStatusSoFar) (snapEq:ev:revNewBs) evs
