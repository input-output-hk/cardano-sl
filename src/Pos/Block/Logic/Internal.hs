{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal block logic. Mostly needed for use in 'Pos.Lrc' -- using
-- lrc requires to apply and rollback blocks, but applying many blocks
-- requires triggering lrc recalculations.

module Pos.Block.Logic.Internal
       (
         -- * Constraints
         BlockMode
       , BlockVerifyMode
       , BlockApplyMode

       , applyBlocksUnsafe
       , rollbackBlocksUnsafe

         -- * Garbage
       , toUpdateBlock
       , toTxpBlock
       ) where

import           Universum

import           Control.Lens            (each, _Wrapped)
import qualified Ether
import           Paths_cardano_sl        (version)

import           Pos.Block.Core          (Block, GenesisBlock, MainBlock, mbTxPayload,
                                          mbUpdatePayload)
import           Pos.Block.Logic.Slog    (SlogMode, slogApplyBlocks, slogRollbackBlocks)
import           Pos.Block.Types         (Blund, Undo (undoTx, undoUS))
import           Pos.Core                (IsGenesisHeader, IsMainHeader, epochIndexL,
                                          gbBody, gbHeader)
import           Pos.DB                  (MonadDB, SomeBatchOp (..))
import           Pos.DB.Block            (MonadBlockDB)
import qualified Pos.DB.GState           as GS
import           Pos.Delegation.Logic    (delegationApplyBlocks, delegationRollbackBlocks)
import           Pos.Lrc.Context         (LrcContext)
import           Pos.Txp.Core            (TxPayload)
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp        (eTxNormalize)
#else
import           Pos.Txp.Logic           (txNormalize)
#endif
import           Pos.Block.BListener     (MonadBListener)
import           Pos.Delegation.Class    (MonadDelegation)
import           Pos.Discovery.Class     (MonadDiscovery)
import           Pos.Reporting           (MonadReportingMem, reportingFatal)
import           Pos.Ssc.Class.Helpers   (SscHelpersClass)
import           Pos.Ssc.Class.LocalData (SscLocalDataClass)
import           Pos.Ssc.Class.Storage   (SscGStateClass)
import           Pos.Ssc.Extra           (MonadSscMem, sscApplyBlocks, sscNormalize,
                                          sscRollbackBlocks)
import           Pos.Ssc.Util            (toSscBlock)
import           Pos.Txp.MemState        (MonadTxpMem)
import           Pos.Txp.Settings        (TxpBlock, TxpBlund, TxpGlobalSettings (..))
import           Pos.Update.Context      (UpdateContext)
import           Pos.Update.Core         (UpdateBlock, UpdatePayload)
import           Pos.Update.Logic        (usApplyBlocks, usNormalize, usRollbackBlocks)
import           Pos.Update.Poll         (PollModifier)
import           Pos.Util                (Some (..), spanSafe)
import           Pos.Util.Chrono         (NE, NewestFirst (..), OldestFirst (..))
import           Pos.WorkMode.Class      (TxpExtra_TMP)

-- | Set of basic constraints used by high-level block processing.
type BlockMode ssc m
     = ( SlogMode ssc m
       -- Needed because SSC state is fully stored in memory.
       , MonadSscMem ssc m
       -- Needed to load blocks (at least delegation does it).
       , MonadBlockDB ssc m
       -- LRC is really needed.
       , Ether.MonadReader' LrcContext m
       -- Probably won't be needed after porting everything to smaller monads.
       , MonadDB m
       -- This constraints define block components' global logic.
       , Ether.MonadReader' TxpGlobalSettings m
       , SscGStateClass ssc
       )

-- | Set of constraints necessary for high-level block verification.
type BlockVerifyMode ssc m = BlockMode ssc m

-- | Set of constraints necessary to apply or rollback blocks at high-level.
type BlockApplyMode ssc m
     = ( BlockMode ssc m
       -- Needed for iteration over DB.
       , MonadMask m
       -- Needed to embed custom logic.
       , MonadBListener m
       -- Needed for normalization.
       , MonadTxpMem TxpExtra_TMP m
       , MonadDelegation m
       , SscLocalDataClass ssc
       , Ether.MonadReader' UpdateContext m
       -- Needed for error reporting.
       , MonadReportingMem m
       , MonadDiscovery m
       )

-- | Applies a definitely valid prefix of blocks. This function is unsafe,
-- use it only if you understand what you're doing. That means you can break
-- system guarantees.
--
-- Invariant: all blocks have the same epoch.
applyBlocksUnsafe
    :: forall ssc m . BlockApplyMode ssc m
    => OldestFirst NE (Blund ssc) -> Maybe PollModifier -> m ()
applyBlocksUnsafe blunds0 pModifier =
    reportingFatal version $
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
    -- [CSL-1167] Here we check that invariant holds, but we silently
    -- ignore some blocks if it doesn't.
    -- We should report a fatal error instead.
    (OldestFirst -> blunds, _) =
        spanSafe ((==) `on` view (_1 . epochIndexL)) $ getOldestFirst blunds0

applyBlocksUnsafeDo
    :: forall ssc m . BlockApplyMode ssc m
    => OldestFirst NE (Blund ssc) -> Maybe PollModifier -> m ()
applyBlocksUnsafeDo blunds pModifier = do
    -- Note: it's important to do 'slogApplyBlocks' first, because it
    -- puts blocks in DB.
    slogBatch <- slogApplyBlocks blunds
    TxpGlobalSettings {..} <- Ether.ask'
    usBatch <- SomeBatchOp <$> usApplyBlocks (map toUpdateBlock blocks) pModifier
    delegateBatch <- SomeBatchOp <$> delegationApplyBlocks blocks
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
    sscNormalize
#ifdef WITH_EXPLORER
    eTxNormalize
#else
    txNormalize
#endif
    usNormalize
  where
    blocks = fmap fst blunds

-- | Rollback sequence of blocks, head-newest order exepected with
-- head being current tip. It's also assumed that lock on block db is
-- taken.  application is taken already.
rollbackBlocksUnsafe
    :: forall ssc m. (BlockApplyMode ssc m)
    => NewestFirst NE (Blund ssc)
    -> m ()
rollbackBlocksUnsafe toRollback = reportingFatal version $ do
    slogRoll <- slogRollbackBlocks toRollback
    dlgRoll <- SomeBatchOp <$> delegationRollbackBlocks toRollback
    usRoll <- SomeBatchOp <$> usRollbackBlocks
                  (toRollback & each._2 %~ undoUS
                              & each._1 %~ toUpdateBlock)
    TxpGlobalSettings {..} <- Ether.ask'
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
