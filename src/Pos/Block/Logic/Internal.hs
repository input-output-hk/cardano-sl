{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal block logic. Mostly needed for use in 'Pos.Lrc' -- using
-- lrc requires to apply and rollback blocks, but applying many blocks
-- requires triggering lrc recalculations.

module Pos.Block.Logic.Internal
       ( applyBlocksUnsafe
       , rollbackBlocksUnsafe
       , withBlkSemaphore
       , withBlkSemaphore_
       , toUpdateBlock
       ) where

import           Control.Arrow                    ((&&&))
import           Control.Lens                     (each, _Wrapped)
import           Control.Monad.Catch              (bracketOnError)
import qualified Data.List.NonEmpty               as NE
import           Formatting                       (build, sformat, (%))
import           Paths_cardano_sl                 (version)
import           Serokell.Util                    (Color (Red), colorize)
import           System.Wlog                      (logWarning)
import           Universum

import           Pos.Block.BListener              (MonadBListener (..))
import           Pos.Block.Types                  (Blund, Undo (undoTx, undoUS))
import           Pos.Context                      (WithNodeContext, getNodeContext,
                                                   ncTxpGlobalSettings, putBlkSemaphore,
                                                   takeBlkSemaphore)
import           Pos.Core                         (IsGenesisHeader, IsMainHeader)
import           Pos.DB                           (SomeBatchOp (..))
import qualified Pos.DB.Block                     as DB
import qualified Pos.DB.DB                        as DB
import qualified Pos.DB.GState                    as GS
import           Pos.Delegation.Logic             (delegationApplyBlocks,
                                                   delegationRollbackBlocks)
import           Pos.Exception                    (assertionFailed)
import           Pos.Reporting                    (reportingFatal)
import           Pos.Slotting                     (putSlottingData)
import           Pos.Txp.Core                     (TxPayload)
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp                 (eTxNormalize)
#else
import           Pos.Txp.Logic                    (txNormalize)
#endif
import           Pos.Communication.Types.Protocol (NodeId)
import           Pos.Ssc.Class                    (Ssc)
import           Pos.Ssc.Extra                    (sscApplyBlocks, sscNormalize,
                                                   sscRollbackBlocks)
import           Pos.Txp.Settings                 (TxpBlund, TxpGlobalSettings (..))
import           Pos.Types                        (Block, GenesisBlock, HeaderHash,
                                                   MainBlock, epochIndexL, gbBody,
                                                   gbHeader, headerHash, headerHashG,
                                                   mbTxPayload, mbUpdatePayload,
                                                   prevBlockL)
import           Pos.Update.Core                  (UpdateBlock, UpdatePayload)
import qualified Pos.Update.DB                    as UDB
import           Pos.Update.Logic                 (usApplyBlocks, usNormalize,
                                                   usRollbackBlocks)
import           Pos.Update.Poll                  (PollModifier)
import           Pos.Util                         (Some (..), inAssertMode, spanSafe,
                                                   _neLast)
import           Pos.Util.Chrono                  (NE, NewestFirst (..), OldestFirst (..))
import           Pos.WorkMode                     (WorkMode)

-- [CSL-780] Totally need something more elegant
toUpdateBlock
    :: forall ssc.
       Ssc ssc
    => Block ssc -> UpdateBlock
toUpdateBlock = bimap convertGenesis convertMain
  where
    convertGenesis :: GenesisBlock ssc -> Some IsGenesisHeader
    convertGenesis = Some . view gbHeader
    convertMain :: MainBlock ssc -> (Some IsMainHeader, UpdatePayload)
    convertMain blk = (Some $ blk ^. gbHeader, blk ^. gbBody . mbUpdatePayload)

-- | Run action acquiring lock on block application. Argument of
-- action is an old tip, result is put as a new tip.
withBlkSemaphore
    :: Each [MonadIO, MonadMask, WithNodeContext ssc] '[m]
    => (HeaderHash -> m (a, HeaderHash)) -> m a
withBlkSemaphore action =
    bracketOnError takeBlkSemaphore putBlkSemaphore doAction
  where
    doAction tip = do
        (res, newTip) <- action tip
        res <$ putBlkSemaphore newTip

-- | Version of withBlkSemaphore which doesn't have any result.
withBlkSemaphore_
    :: Each [MonadIO, MonadMask, WithNodeContext ssc] '[m]
    => (HeaderHash -> m HeaderHash) -> m ()
withBlkSemaphore_ = withBlkSemaphore . (fmap pure .)

-- | Applies a definitely valid prefix of blocks. This function is unsafe,
-- use it only if you understand what you're doing. That means you can break
-- system guarantees.
--
-- Invariant: all blocks have the same epoch.
applyBlocksUnsafe
    :: forall ssc m . WorkMode ssc m
    => m (Set NodeId) -> OldestFirst NE (Blund ssc) -> Maybe PollModifier -> m ()
applyBlocksUnsafe getPeers blunds0 pModifier =
    reportingFatal getPeers version $
    case blunds ^. _Wrapped of
        (b@(Left _,_):|[])     -> app' (b:|[])
        (b@(Left _,_):|(x:xs)) -> app' (b:|[]) >> app' (x:|xs)
        _                      -> app blunds
  where
    app x = applyBlocksUnsafeDo x pModifier
    app' = app . OldestFirst
    (OldestFirst -> blunds, _) =
        spanSafe ((==) `on` view (_1 . epochIndexL)) $ getOldestFirst blunds0

applyBlocksUnsafeDo
    :: forall ssc m . WorkMode ssc m
    => OldestFirst NE (Blund ssc) -> Maybe PollModifier -> m ()
applyBlocksUnsafeDo blunds pModifier = do
    -- Note: it's important to put blocks first
    mapM_ putToDB blunds
    -- If the program is interrupted at this point (after putting on block),
    -- we will rollback all wallet sets at the next launch.
    onApplyBlocks blunds `catch` logWarn
    TxpGlobalSettings {..} <- ncTxpGlobalSettings <$> getNodeContext
    usBatch <- SomeBatchOp <$> usApplyBlocks (map toUpdateBlock blocks) pModifier
    delegateBatch <- SomeBatchOp <$> delegationApplyBlocks blocks
    txpBatch <- tgsApplyBlocks $ map toTxpBlund blunds
    sscApplyBlocks blocks Nothing -- TODO: pass not only 'Nothing'
    let putTip = SomeBatchOp $
                 GS.PutTip $
                 headerHash $
                 NE.last $
                 getOldestFirst blunds
    GS.writeBatchGState [putTip, delegateBatch, usBatch, txpBatch, forwardLinksBatch, inMainBatch]
    sscNormalize
#ifdef WITH_EXPLORER
    eTxNormalize
#else
    txNormalize
#endif
    usNormalize
    DB.sanityCheckDB
    putSlottingData =<< UDB.getSlottingData
  where
    -- hehe it's not unsafe yet TODO
    blocks = fmap fst blunds
    forwardLinks = map (view prevBlockL &&& view headerHashG) $ toList blocks
    forwardLinksBatch = SomeBatchOp $ map (uncurry GS.AddForwardLink) forwardLinks
    inMainBatch = SomeBatchOp . getOldestFirst $
        fmap (GS.SetInMainChain True . view headerHashG . fst) blunds
    putToDB (blk, undo) = DB.putBlock undo blk
    logWarn :: SomeException -> m ()
    logWarn = logWarning . sformat ("onApplyBlocks raised exception: "%build)

-- | Rollback sequence of blocks, head-newest order exepected with
-- head being current tip. It's also assumed that lock on block db is
-- taken.  application is taken already.
rollbackBlocksUnsafe
    :: forall ssc m . (WorkMode ssc m)
    => m (Set NodeId) -> NewestFirst NE (Blund ssc) -> m ()
rollbackBlocksUnsafe getPeers toRollback = reportingFatal getPeers version $ do
    -- If program is interrupted after call @onRollbackBlocks@,
    -- we will load all wallet set not rolled yet at the next launch.
    onRollbackBlocks toRollback `catch` logWarn
    delRoll <- SomeBatchOp <$> delegationRollbackBlocks toRollback
    usRoll <- SomeBatchOp <$> usRollbackBlocks
                  (toRollback & each._2 %~ undoUS
                              & each._1 %~ toUpdateBlock)
    TxpGlobalSettings {..} <- ncTxpGlobalSettings <$> getNodeContext
    txRoll <- tgsRollbackBlocks $ map toTxpBlund toRollback
    sscRollbackBlocks $ fmap fst toRollback
    let putTip = SomeBatchOp $
                 GS.PutTip $
                 headerHash $
                 (NE.last $ getNewestFirst toRollback) ^. prevBlockL
    GS.writeBatchGState [putTip, delRoll, usRoll, txRoll, forwardLinksBatch, inMainBatch]
    DB.sanityCheckDB
    inAssertMode $
        when (isGenesis0 (toRollback ^. _Wrapped . _neLast . _1)) $
        assertionFailed $
        colorize Red "FATAL: we are TRYING TO ROLLBACK 0-TH GENESIS block"
  where
    inMainBatch =
        SomeBatchOp . getNewestFirst $
        fmap (GS.SetInMainChain False . view headerHashG . fst) toRollback
    forwardLinksBatch =
        SomeBatchOp . getNewestFirst $
        fmap (GS.RemoveForwardLink . view prevBlockL . fst) toRollback
    isGenesis0 (Left genesisBlk) = genesisBlk ^. epochIndexL == 0
    isGenesis0 (Right _)         = False
    logWarn :: SomeException -> m ()
    logWarn = logWarning . sformat ("onRollbackBlocks raised exception: "%build)

-- [CSL-780] Need something more elegant, at least eliminate copy-paste.
-- Should be done soon™.
toTxpBlund
    :: forall ssc.
       Ssc ssc
    => Blund ssc -> TxpBlund
toTxpBlund = bimap (bimap convertGenesis convertMain) undoTx
  where
    convertGenesis :: GenesisBlock ssc -> Some IsGenesisHeader
    convertGenesis = Some . view gbHeader
    convertMain :: MainBlock ssc -> (Some IsMainHeader, TxPayload)
    convertMain blk = (Some $ blk ^. gbHeader, blk ^. gbBody . mbTxPayload)
