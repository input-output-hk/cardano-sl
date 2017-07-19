{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal block logic. Mostly needed for use in 'Pos.Lrc' -- using
-- lrc requires to apply and rollback blocks, but applying many blocks
-- requires triggering lrc recalculations.

module Pos.Block.Logic.Internal
       (
         -- * Constraints
         MonadBlockBase
       , MonadBlockVerify
       , MonadBlockApply

       , applyBlocksUnsafe
       , rollbackBlocksUnsafe

         -- * Garbage
       , toUpdateBlock
       , toTxpBlock
       ) where

import           Universum

import           Control.Lens                (each, _Wrapped)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Ether.Internal              (HasLens (..))
import           Formatting                  (sformat, (%))
import           Mockable                    (CurrentTime, Mockable)
import           Serokell.Util.Text          (listJson)

import           Pos.Block.BListener         (MonadBListener)
import           Pos.Block.Core              (Block, GenesisBlock, MainBlock, mbTxPayload,
                                              mbUpdatePayload)
import           Pos.Block.Slog              (MonadSlogApply, MonadSlogBase,
                                              slogApplyBlocks, slogRollbackBlocks)
import           Pos.Block.Types             (Blund, Undo (undoTx, undoUS))
import           Pos.Core                    (IsGenesisHeader, IsMainHeader, epochIndexL,
                                              gbBody, gbHeader, headerHash)
import           Pos.DB                      (MonadDB, MonadGState, SomeBatchOp (..))
import           Pos.DB.Block                (MonadBlockDB, MonadSscBlockDB)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Delegation.Logic        (dlgApplyBlocks, dlgNormalizeOnRollback,
                                              dlgRollbackBlocks)
import           Pos.Discovery.Class         (MonadDiscovery)
import           Pos.Exception               (assertionFailed)
import qualified Pos.GState                  as GS
import           Pos.Lrc.Context             (LrcContext)
import           Pos.Reporting               (HasReportingContext, reportingFatal)
import           Pos.Ssc.Class.Helpers       (SscHelpersClass)
import           Pos.Ssc.Class.LocalData     (SscLocalDataClass)
import           Pos.Ssc.Class.Storage       (SscGStateClass)
import           Pos.Ssc.Extra               (MonadSscMem, sscApplyBlocks, sscNormalize,
                                              sscRollbackBlocks)
import           Pos.Ssc.Util                (toSscBlock)
import           Pos.Txp.Core                (TxPayload)
import           Pos.Txp.MemState            (MonadTxpMem)
import           Pos.Txp.Settings            (TxpBlock, TxpBlund, TxpGlobalSettings (..))
import           Pos.Update.Context          (UpdateContext)
import           Pos.Update.Core             (UpdateBlock, UpdatePayload)
import           Pos.Update.Logic            (usApplyBlocks, usNormalize,
                                              usRollbackBlocks)
import           Pos.Update.Poll             (PollModifier)
import           Pos.Util                    (Some (..), spanSafe)
import           Pos.Util.Chrono             (NE, NewestFirst (..), OldestFirst (..))
import           Pos.WorkMode.Class          (TxpExtra_TMP)
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp            (eTxNormalize)
#else
import           Pos.Txp.Logic               (txNormalize)
#endif

-- | Set of basic constraints used by high-level block processing.
type MonadBlockBase ssc ctx m
     = ( MonadSlogBase ssc m
       -- Needed because SSC state is fully stored in memory.
       , MonadSscMem ssc ctx m
       -- Needed to load blocks (at least delegation does it).
       , MonadBlockDB ssc m
       , MonadSscBlockDB ssc m
       -- Needed by some components.
       , MonadGState m
       -- This constraints define block components' global logic.
       , HasLens TxpGlobalSettings ctx TxpGlobalSettings
       , HasLens LrcContext ctx LrcContext
       , SscGStateClass ssc
       , MonadReader ctx m
       )

-- | Set of constraints necessary for high-level block verification.
type MonadBlockVerify ssc ctx m = MonadBlockBase ssc ctx m

-- | Set of constraints necessary to apply or rollback blocks at high-level.
type MonadBlockApply ssc ctx m
     = ( MonadBlockBase ssc ctx m
       , MonadSlogApply ssc ctx m
       -- It's obviously needed to write something to DB, for instance.
       , MonadDB m
       -- Needed for iteration over DB.
       , MonadMask m
       -- Needed to embed custom logic.
       , MonadBListener m
       -- Needed for normalization.
       , MonadTxpMem TxpExtra_TMP ctx m
       , MonadDelegation ctx m
       , SscLocalDataClass ssc
       , HasLens UpdateContext ctx UpdateContext
       , Mockable CurrentTime m
       -- Needed for error reporting.
       , HasReportingContext ctx
       , MonadDiscovery m
       , MonadBaseControl IO m
       , MonadReader ctx m
       )

-- | Applies a definitely valid prefix of blocks. This function is unsafe,
-- use it only if you understand what you're doing. That means you can break
-- system guarantees.
--
-- Invariant: all blocks have the same epoch.
applyBlocksUnsafe
    :: forall ssc ctx m . MonadBlockApply ssc ctx m
    => OldestFirst NE (Blund ssc) -> Maybe PollModifier -> m ()
applyBlocksUnsafe blunds pModifier = reportingFatal $ do
    -- Check that all blunds have the same epoch.
    unless (null nextEpoch) $ assertionFailed $
        sformat ("applyBlocksUnsafe: tried to apply more than we should"%
                 "thisEpoch"%listJson%"\nnextEpoch:"%listJson)
                (map (headerHash . fst) thisEpoch)
                (map (headerHash . fst) nextEpoch)
    -- It's essential to apply genesis block separately, before
    -- applying other blocks.
    -- That's because applying genesis block may change protocol version
    -- which may potentially change protocol rules.
    -- We would like to avoid dependencies between components, so we have
    -- chosen this approach. Related issue is CSL-660.
    -- Also note that genesis block can be only in the head, because all
    -- blocks are from the same epoch.
    case blunds ^. _Wrapped of
        (b@(Left _,_):|[])     -> app' (b:|[])
        (b@(Left _,_):|(x:xs)) -> app' (b:|[]) >> app' (x:|xs)
        _                      -> app blunds
  where
    app x = applyBlocksUnsafeDo x pModifier
    app' = app . OldestFirst
    (thisEpoch, nextEpoch) =
        spanSafe ((==) `on` view (_1 . epochIndexL)) $ getOldestFirst blunds

applyBlocksUnsafeDo
    :: forall ssc ctx m . MonadBlockApply ssc ctx m
    => OldestFirst NE (Blund ssc) -> Maybe PollModifier -> m ()
applyBlocksUnsafeDo blunds pModifier = do
    -- Note: it's important to do 'slogApplyBlocks' first, because it
    -- puts blocks in DB.
    slogBatch <- slogApplyBlocks blunds
    TxpGlobalSettings {..} <- view (lensOf @TxpGlobalSettings)
    usBatch <- SomeBatchOp <$> usApplyBlocks (map toUpdateBlock blocks) pModifier
    delegateBatch <- SomeBatchOp <$> dlgApplyBlocks blocks
    txpBatch <- tgsApplyBlocks $ map toTxpBlund blunds
    sscBatch <- SomeBatchOp <$>
        -- TODO: pass not only 'Nothing'
        sscApplyBlocks (map toSscBlock blocks) Nothing
    GS.writeBatchGState
        [ delegateBatch
        , usBatch
        , txpBatch
        , sscBatch
        , slogBatch
        ]
    -- We normalize all mempools except the delegation one.
    -- That's because delegation mempool normalization is harder and is done
    -- within block application.
    sscNormalize
#ifdef WITH_EXPLORER
    eTxNormalize
#else
    txNormalize
#endif
    usNormalize
  where
    blocks = fmap fst blunds

-- | Rollback sequence of blocks, head-newest order expected with head being
-- current tip. It's also assumed that lock on block db is taken already.
rollbackBlocksUnsafe
    :: forall ssc ctx m. (MonadBlockApply ssc ctx m)
    => NewestFirst NE (Blund ssc)
    -> m ()
rollbackBlocksUnsafe toRollback = reportingFatal $ do
    slogRoll <- slogRollbackBlocks toRollback
    dlgRoll <- SomeBatchOp <$> dlgRollbackBlocks toRollback
    usRoll <- SomeBatchOp <$> usRollbackBlocks
                  (toRollback & each._2 %~ undoUS
                              & each._1 %~ toUpdateBlock)
    TxpGlobalSettings {..} <- view (lensOf @TxpGlobalSettings)
    txRoll <- tgsRollbackBlocks $ map toTxpBlund toRollback
    sscBatch <- SomeBatchOp <$> sscRollbackBlocks
        (map (toSscBlock . fst) toRollback)
    GS.writeBatchGState
        [ dlgRoll
        , usRoll
        , txRoll
        , sscBatch
        , slogRoll
        ]
    -- After blocks are rolled back it makes sense to recreate the
    -- delegation mempool.
    -- We don't normalize other mempools, because they are normalized
    -- in 'applyBlocksUnsafe' and we always ensure that some blocks
    -- are applied after rollback.
    dlgNormalizeOnRollback @ssc

----------------------------------------------------------------------------
-- Garbage
----------------------------------------------------------------------------

-- [CSL-1156] Need something more elegant.
toTxpBlock
    :: forall ssc.
       SscHelpersClass ssc
    => Block ssc -> TxpBlock
toTxpBlock = bimap convertGenesis convertMain
  where
    convertGenesis :: GenesisBlock ssc -> Some IsGenesisHeader
    convertGenesis = Some . view gbHeader
    convertMain :: MainBlock ssc -> (Some IsMainHeader, TxPayload)
    convertMain blk = (Some $ blk ^. gbHeader, blk ^. gbBody . mbTxPayload)

-- [CSL-1156] Yes, definitely need something more elegant.
toTxpBlund
    :: forall ssc.
       SscHelpersClass ssc
    => Blund ssc -> TxpBlund
toTxpBlund = bimap toTxpBlock undoTx

-- [CSL-1156] Sure, totally need something more elegant
toUpdateBlock
    :: forall ssc.
       SscHelpersClass ssc
    => Block ssc -> UpdateBlock
toUpdateBlock = bimap convertGenesis convertMain
  where
    convertGenesis :: GenesisBlock ssc -> Some IsGenesisHeader
    convertGenesis = Some . view gbHeader
    convertMain :: MainBlock ssc -> (Some IsMainHeader, UpdatePayload)
    convertMain blk = (Some $ blk ^. gbHeader, blk ^. gbBody . mbUpdatePayload)
