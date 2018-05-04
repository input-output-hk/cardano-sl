{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE RankNTypes     #-}

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
       , BlockRollbackFailure(..)
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

import           Control.Lens (folded, makeLenses, makePrisms, to, toListOf)
import           Control.Monad.Random.Strict (RandT, Random (..), RandomGen, mapRandT, weighted)
import qualified Data.ByteString.Short as SBS
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text.Buildable
import           Formatting (bprint, build, sformat, shown, (%))
import qualified Prelude
import           Serokell.Util (listJson)

import           Pos.AllSecrets (AllSecrets)
import           Pos.Block.Types (Blund)
import           Pos.Core (GenesisWStakeholders, HasConfiguration, HeaderHash, headerHash,
                           prevBlockL)
import           Pos.Crypto.Hashing (hashHexF)
import           Pos.Generator.Block (BlockGenParams (..), BlockTxpGenMode, MonadBlockGen,
                                      TxGenParams (..), genBlocks)
import           Pos.GState (withClonedGState)
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Pos.Txp (TxpGlobalSettings)
import           Pos.Txp.Configuration (HasTxpConfiguration)
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..), toNewestFirst,
                                  toOldestFirst, _OldestFirst)
import           Pos.Util.Util (lensOf')

----------------------------------------------------------------------------
-- Blockchain tree
----------------------------------------------------------------------------

-- NB. not a monoid, so the user can be sure that `(<>)` acts as expected
-- on string literals for paths (not path segments).
newtype PathSegment = PathSegment { pathSegmentByteString :: SBS.ShortByteString }
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
        (fold . intersperse ("/" :: Text) . toList $
            decodeUtf8 . SBS.fromShort . pathSegmentByteString <$> segs)

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

-- Empty input paths are ignored.
buildBlockchainForest :: a -> [(Path, a)] -> BlockchainForest a
buildBlockchainForest defElem elements =
    fmap (buildBlockchainTree defElem . getDList) . Map.fromListWith (<>) $ do
        (Path path, a) <- elements
        case Seq.viewl path of
            Seq.EmptyL -> []
            pathSegment Seq.:< path' ->
                [(pathSegment, dlistSingleton (Path path', a))]
  where
    dlistSingleton a = Endo (a:)
    getDList (Endo dl) = dl []

buildBlockchainTree :: a -> [(Path, a)] -> BlockchainTree a
buildBlockchainTree defElem elements =
    let topElement = fromMaybe defElem $ List.lookup (Path Seq.empty) elements
    in BlockchainTree topElement (buildBlockchainForest defElem elements)

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

genBlocksInForest
    :: (HasTxpConfiguration, BlockTxpGenMode g ctx m)
    => AllSecrets
    -> GenesisWStakeholders
    -> BlockchainForest BlockDesc
    -> RandT g m (BlockchainForest Blund)
genBlocksInForest secrets bootStakeholders =
    traverse $ mapRandT withClonedGState .
    genBlocksInTree secrets bootStakeholders

genBlocksInTree
    :: (HasTxpConfiguration, BlockTxpGenMode g ctx m)
    => AllSecrets
    -> GenesisWStakeholders
    -> BlockchainTree BlockDesc
    -> RandT g m (BlockchainTree Blund)
genBlocksInTree secrets bootStakeholders blockchainTree = do
    txpSettings <- view (lensOf' @TxpGlobalSettings)
    let BlockchainTree blockDesc blockchainForest = blockchainTree
        txGenParams = case blockDesc of
            BlockDescDefault  -> TxGenParams (0, 0) 0
            BlockDescCustom p -> p
        blockGenParams = BlockGenParams
            { _bgpSecrets         = secrets
            , _bgpGenStakeholders = bootStakeholders
            , _bgpBlockCount      = 1
            , _bgpTxGenParams     = txGenParams
            , _bgpInplaceDB       = True
            , _bgpSkipNoKey       = False
            , _bgpTxpGlobalSettings = txpSettings
            }
    blocks <- genBlocks blockGenParams maybeToList
    block <- case blocks of
        [block] -> return block
        _ ->
            -- We specify '_bgpBlockCount = 1' above, so the output must contain
            -- exactly one block.
            error "genBlocksInTree: impossible - 'genBlocks' generated unexpected amount of blocks"
    forestBlocks <- genBlocksInForest secrets bootStakeholders blockchainForest
    return $ BlockchainTree block forestBlocks

-- Precondition: paths in the structure are non-empty.
genBlocksInStructure ::
       ( HasTxpConfiguration
       , BlockTxpGenMode g ctx m
       , Functor t, Foldable t)
    => AllSecrets
    -> GenesisWStakeholders
    -> Map Path BlockDesc
    -> t Path
    -> RandT g m (t Blund)
genBlocksInStructure secrets bootStakeholders annotations s = do
    let
        getAnnotation :: Path -> BlockDesc
        getAnnotation path =
            Map.findWithDefault BlockDescDefault path annotations
        paths :: [(Path, BlockDesc)]
        paths = toListOf (folded . to (\path -> (path, getAnnotation path))) s
        descForest :: BlockchainForest BlockDesc
        descForest = buildBlockchainForest BlockDescDefault paths
    blockForest :: BlockchainForest Blund <-
        genBlocksInForest secrets bootStakeholders descForest
    let
        getBlock :: Path -> Blund
        getBlock path = Map.findWithDefault
            (error "genBlocksInStructure: impossible happened")
            path
            (flattenBlockchainForest' blockForest)
    return $ fmap getBlock s

----------------------------------------------------------------------------
-- Block event types
----------------------------------------------------------------------------

data BlockApplyResult
    = BlockApplySuccess
    | BlockApplyFailure {- TODO: attach error info, such as:
                            * block is not a continuation of the chain
                            * block signature is invalid
                            * etc -}
    deriving (Show)

data BlockEventApply' blund = BlockEventApply
    { _beaInput    :: !(OldestFirst NE blund)
    , _beaOutValid :: !BlockApplyResult
    } deriving (Show, Functor, Foldable)

makeLenses ''BlockEventApply'

type BlockEventApply = BlockEventApply' Blund

-- | The type of failure that we expect from a rollback.
-- Extend this data type as necessary if you need to check for
-- other types of failures.
data BlockRollbackFailure
    = BlkRbSecurityLimitExceeded
    deriving (Show)

data BlockRollbackResult
    = BlockRollbackSuccess
    | BlockRollbackFailure BlockRollbackFailure
    deriving (Show)

data BlockEventRollback' blund = BlockEventRollback
    { _berInput    :: !(NewestFirst NE blund)
    , _berOutValid :: !BlockRollbackResult
    } deriving (Show, Functor, Foldable)

makeLenses ''BlockEventRollback'

type BlockEventRollback = BlockEventRollback' Blund

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

type BlockEvent = BlockEvent' Blund

newtype BlockScenario' blund = BlockScenario [BlockEvent' blund]
    deriving (Show, Functor, Foldable)

instance Buildable blund => Buildable (BlockScenario' blund) where
    build (BlockScenario xs) = bprint listJson xs

type BlockScenario = BlockScenario' Blund

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
blkEvTip :: (HasConfiguration, HasSscConfiguration) => BlockEvent -> Maybe HeaderHash
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
enrichWithSnapshotChecking :: (HasConfiguration, HasSscConfiguration) => BlockScenario -> (BlockScenario, CheckCount)
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
