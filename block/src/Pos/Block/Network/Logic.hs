{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | Network-related logic that's mostly methods and dialogs between
-- nodes. Also see "Pos.Block.Network.Retrieval" for retrieval worker
-- loop logic.
module Pos.Block.Network.Logic
       (
         BlockNetLogicException (..)
       , triggerRecovery
       , handleBlocks

       , handleUnsolicitedHeader
       ) where

import           Universum

import           Control.Concurrent.STM (isFullTBQueue, readTVar, writeTBQueue,
                     writeTVar)
import           Control.Exception (IOException)
import           Control.Exception.Safe (Exception (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text.Buildable as B
import           Formatting (bprint, build, sformat, shown, stext, (%))
import           Mockable (forConcurrently)
import           Serokell.Util.Text (listJson)
import qualified System.Metrics.Gauge as Metrics
import           System.Wlog (logDebug, logInfo, logWarning)

import           Pos.Binary.Txp ()
import           Pos.Block.BlockWorkMode (BlockWorkMode)
import           Pos.Block.Error (ApplyBlocksException)
import           Pos.Block.Logic (ClassifyHeaderRes (..), classifyNewHeader,
                     lcaWithMainChain, verifyAndApplyBlocks)
import qualified Pos.Block.Logic as L
import           Pos.Block.RetrievalQueue (BlockRetrievalQueue,
                     BlockRetrievalQueueTag, BlockRetrievalTask (..))
import           Pos.Block.Types (Blund, LastKnownHeaderTag)
import           Pos.Core (HasHeaderHash (..), HeaderHash, ProtocolConstants,
                     SlotCount, gbHeader, headerHashG, isMoreDifficult,
                     pcEpochSlots, prevBlockL)
import           Pos.Core.Block (Block, BlockHeader, blockHeader)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..),
                     _NewestFirst, _OldestFirst)
import           Pos.Crypto (ProtocolMagic, shortHashF)
import qualified Pos.DB.Block.Load as DB
import           Pos.Exception (cardanoExceptionFromException,
                     cardanoExceptionToException)
import           Pos.Infra.Communication.Protocol (NodeId)
import           Pos.Infra.Diffusion.Types (Diffusion)
import qualified Pos.Infra.Diffusion.Types as Diffusion
                     (Diffusion (announceBlockHeader, requestTip))
import           Pos.Infra.Recovery.Info (recoveryInProgress)
import           Pos.Infra.Reporting.MemState (HasMisbehaviorMetrics (..),
                     MisbehaviorMetrics (..))
import           Pos.Infra.StateLock (Priority (..), modifyStateLock)
import           Pos.Infra.Util.JsonLog.Events (MemPoolModifyReason (..),
                     jlAdoptedBlock)
import           Pos.Infra.Util.TimeWarp (CanJsonLog (..))
import           Pos.Util (buildListBounds, multilineBounds, _neLast)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Util (lensOf)

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

-- FIXME this same thing is defined in full diffusion layer.
-- Must finish the proper factoring. There should be no 'Block.Network'
-- in cardano-sl-block; it should just use the Diffusion and Logic interfaces.
data BlockNetLogicException
    = DialogUnexpected Text
      -- ^ Node's response in any network/block related logic was
      -- unexpected.
    | BlockNetLogicInternal Text
      -- ^ We don't expect this to happen. Most probably it's internal
      -- logic error.
    deriving (Show)

instance B.Buildable BlockNetLogicException where
    build e = bprint ("BlockNetLogicException: "%shown) e

instance Exception BlockNetLogicException where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty

----------------------------------------------------------------------------
-- Recovery
----------------------------------------------------------------------------

-- | Start recovery based on established communication. “Starting recovery”
-- means simply sending all our neighbors a 'MsgGetHeaders' message (see
-- 'requestTip'), so sometimes 'triggerRecovery' is used simply to ask for
-- tips.
--
-- Note that when recovery is in progress (see 'recoveryInProgress'),
-- 'triggerRecovery' does nothing. It's okay because when recovery is in
-- progress and 'ncRecoveryHeader' is full, we'll be requesting blocks anyway
-- and until we're finished we shouldn't be asking for new blocks.
triggerRecovery
    :: ( BlockWorkMode ctx m
       )
    => ProtocolMagic -> SlotCount -> Diffusion m -> m ()
triggerRecovery pm epochSlots diffusion = unlessM (recoveryInProgress epochSlots) $ do
    logDebug "Recovery triggered, requesting tips from neighbors"
    -- The 'catch' here is for an exception when trying to enqueue the request.
    -- In 'requestTipsAndProcess', IO exceptions are caught, for each
    -- individual request per-peer. Those are not re-thrown.
    void requestTipsAndProcess `catch`
        \(e :: SomeException) -> do
           logDebug ("Error happened in triggerRecovery: " <> show e)
           throwM e
    logDebug "Finished requesting tips for recovery"
  where
    requestTipsAndProcess = do
        requestsMap <- Diffusion.requestTip diffusion
        forConcurrently (M.toList requestsMap) $ \it@(nodeId, _) -> waitAndProcessOne it `catch`
            -- Catch and squelch IOExceptions so that one failed request to one
            -- particlar peer does not stop the others.
            \(e :: IOException) ->
                logDebug $ sformat ("Error requesting tip from "%shown%": "%shown) nodeId e
    waitAndProcessOne (nodeId, mbh) = do
        -- 'mbh' is an 'm' term that returns when the header has been
        -- downloaded.
        bh <- mbh
        -- I know, it's not unsolicited. TODO rename.
        handleUnsolicitedHeader pm epochSlots bh nodeId

----------------------------------------------------------------------------
-- Headers processing
----------------------------------------------------------------------------

handleUnsolicitedHeader
    :: BlockWorkMode ctx m
    => ProtocolMagic
    -> SlotCount
    -> BlockHeader
    -> NodeId
    -> m ()
handleUnsolicitedHeader pm epochSlots header nodeId = do
    logDebug $ sformat
        ("handleUnsolicitedHeader: single header was propagated, processing:\n"
         %build) header
    classificationRes <- classifyNewHeader pm epochSlots header
    -- TODO: should we set 'To' hash to hash of header or leave it unlimited?
    case classificationRes of
        CHContinues -> do
            logDebug $ sformat continuesFormat hHash
            addHeaderToBlockRequestQueue nodeId header True
        CHAlternative -> do
            logDebug $ sformat alternativeFormat hHash
            addHeaderToBlockRequestQueue nodeId header False
        CHUseless reason -> logDebug $ sformat uselessFormat hHash reason
        CHInvalid _ -> do
            logWarning $ sformat ("handleUnsolicited: header "%shortHashF%
                                " is invalid") hHash
            pass -- TODO: ban node for sending invalid block.
  where
    hHash = headerHash header
    continuesFormat =
        "Header " %shortHashF %
        " is a good continuation of our chain, will process"
    alternativeFormat =
        "Header " %shortHashF %
        " potentially represents good alternative chain, will process"
    uselessFormat =
        "Header " %shortHashF%" is useless for the following reason: " %stext

----------------------------------------------------------------------------
-- Putting things into request queue
----------------------------------------------------------------------------

-- | Given a valid blockheader and nodeid, this function will put them into
-- download queue and they will be processed later.
addHeaderToBlockRequestQueue
    :: forall ctx m.
       (BlockWorkMode ctx m)
    => NodeId
    -> BlockHeader
    -> Bool -- ^ Was the block classified as chain continuation?
    -> m ()
addHeaderToBlockRequestQueue nodeId header continues = do
    let hHash = headerHash header
    logDebug $ sformat ("addToBlockRequestQueue, : "%shortHashF) hHash
    queue <- view (lensOf @BlockRetrievalQueueTag)
    lastKnownH <- view (lensOf @LastKnownHeaderTag)
    added <- atomically $ do
        updateLastKnownHeader lastKnownH header
        addTaskToBlockRequestQueue nodeId queue $
            BlockRetrievalTask { brtHeader = header, brtContinues = continues }
    if added
    then logDebug $ sformat ("Added headers to block request queue: nodeId="%build%
                             ", header="%build)
                            nodeId hHash
    else logWarning $ sformat ("Failed to add headers from "%build%
                               " to block retrieval queue: queue is full")
                              nodeId

addTaskToBlockRequestQueue
    :: NodeId
    -> BlockRetrievalQueue
    -> BlockRetrievalTask
    -> STM Bool
addTaskToBlockRequestQueue nodeId queue task = do
    ifM (isFullTBQueue queue)
        (pure False)
        (True <$ writeTBQueue queue (nodeId, task))

updateLastKnownHeader
    :: TVar (Maybe BlockHeader)
    -> BlockHeader
    -> STM ()
updateLastKnownHeader lastKnownH header = do
    oldV <- readTVar lastKnownH
    let needUpdate = maybe True (header `isMoreDifficult`) oldV
    when needUpdate $ writeTVar lastKnownH (Just header)

----------------------------------------------------------------------------
-- Handling blocks
----------------------------------------------------------------------------

-- | Carefully apply blocks that came from the network.
handleBlocks
    :: forall ctx m .
       ( BlockWorkMode ctx m
       , HasMisbehaviorMetrics ctx
       )
    => ProtocolMagic
    -> ProtocolConstants
    -> OldestFirst NE Block
    -> Diffusion m
    -> m ()
handleBlocks pm pc blocks diffusion = do
    logDebug "handleBlocks: processing"
    inAssertMode $ logInfo $
        sformat ("Processing sequence of blocks: " % buildListBounds % "...") $
            getOldestFirst $ map headerHash blocks
    maybe onNoLca handleBlocksWithLca =<<
        lcaWithMainChain (map (view blockHeader) blocks)
    inAssertMode $ logDebug $ "Finished processing sequence of blocks"
  where
    onNoLca = logWarning $
        "Sequence of blocks can't be processed, because there is no LCA. " <>
        "Probably rollback happened in parallel"

    handleBlocksWithLca :: HeaderHash -> m ()
    handleBlocksWithLca lcaHash = do
        logDebug $ sformat ("Handling block w/ LCA, which is "%shortHashF) lcaHash
        -- Head blund in result is the youngest one.
        toRollback <- DB.loadBlundsFromTipWhile $ \blk -> headerHash blk /= lcaHash
        maybe (applyWithoutRollback pm pc diffusion blocks)
              (applyWithRollback pm pc diffusion blocks lcaHash)
              (_NewestFirst nonEmpty toRollback)

applyWithoutRollback
    :: forall ctx m.
       ( BlockWorkMode ctx m
       , HasMisbehaviorMetrics ctx
       )
    => ProtocolMagic
    -> ProtocolConstants
    -> Diffusion m
    -> OldestFirst NE Block
    -> m ()
applyWithoutRollback pm pc diffusion blocks = do
    logInfo . sformat ("Trying to apply blocks w/o rollback. " % multilineBounds 6)
       . getOldestFirst . map (view blockHeader) $ blocks
    modifyStateLock HighPriority ApplyBlock applyWithoutRollbackDo >>= \case
        Left (pretty -> err) ->
            onFailedVerifyBlocks (getOldestFirst blocks) err
        Right newTip -> do
            when (newTip /= newestTip) $
                logWarning $ sformat
                    ("Only blocks up to "%shortHashF%" were applied, "%
                     "newer were considered invalid")
                    newTip
            let toRelay =
                    fromMaybe (error "Listeners#applyWithoutRollback is broken") $
                    find (\b -> headerHash b == newTip) blocks
                prefix = blocks
                    & _OldestFirst %~ NE.takeWhile ((/= newTip) . headerHash)
                    & map (view blockHeader)
                applied = NE.fromList $
                    getOldestFirst prefix <> one (toRelay ^. blockHeader)
            relayBlock (pcEpochSlots pc) diffusion toRelay
            logInfo $ blocksAppliedMsg applied
            for_ blocks $ jsonLog . jlAdoptedBlock
  where
    newestTip = blocks ^. _OldestFirst . _neLast . headerHashG
    applyWithoutRollbackDo
        :: HeaderHash -> m (HeaderHash, Either ApplyBlocksException HeaderHash)
    applyWithoutRollbackDo curTip = do
        logInfo "Verifying and applying blocks..."
        res <- verifyAndApplyBlocks pm pc False blocks
        logInfo "Verifying and applying blocks done"
        let newTip = either (const curTip) identity res
        pure (newTip, res)

applyWithRollback
    :: ( BlockWorkMode ctx m
       , HasMisbehaviorMetrics ctx
       )
    => ProtocolMagic
    -> ProtocolConstants
    -> Diffusion m
    -> OldestFirst NE Block
    -> HeaderHash
    -> NewestFirst NE Blund
    -> m ()
applyWithRollback pm pc diffusion toApply lca toRollback = do
    logInfo . sformat ("Trying to apply blocks w/o rollback. " % multilineBounds 6)
       . getOldestFirst . map (view blockHeader) $ toApply
    logInfo $ sformat ("Blocks to rollback "%listJson) toRollbackHashes
    res <- modifyStateLock HighPriority ApplyBlockWithRollback $ \curTip -> do
        res <- L.applyWithRollback pm pc toRollback toApplyAfterLca
        pure (either (const curTip) identity res, res)
    case res of
        Left (pretty -> err) ->
            logWarning $ "Couldn't apply blocks with rollback: " <> err
        Right newTip -> do
            logDebug $ sformat
                ("Finished applying blocks w/ rollback, relaying new tip: "%shortHashF)
                newTip
            reportRollback
            logInfo $ blocksRolledBackMsg (getNewestFirst toRollback)
            logInfo $ blocksAppliedMsg (getOldestFirst toApply)
            for_ (getOldestFirst toApply) $ jsonLog . jlAdoptedBlock
            relayBlock (pcEpochSlots pc) diffusion $ toApply ^. _OldestFirst . _neLast
  where
    toRollbackHashes = fmap headerHash toRollback
    reportRollback = do
        let rollbackDepth = length toRollback

        -- Commit rollback value to EKG
        whenJustM (view misbehaviorMetrics) $ liftIO .
            flip Metrics.set (fromIntegral rollbackDepth) . _mmRollbacks

    panicBrokenLca = error "applyWithRollback: nothing after LCA :<"
    toApplyAfterLca =
        OldestFirst $
        fromMaybe panicBrokenLca $ nonEmpty $
        NE.dropWhile ((lca /=) . (^. prevBlockL)) $
        getOldestFirst $ toApply

relayBlock
    :: forall ctx m.
       (BlockWorkMode ctx m)
    => SlotCount -> Diffusion m -> Block -> m ()
relayBlock _ _ (Left _) = logDebug "Not relaying Genesis block"
relayBlock epochSlots diffusion (Right mainBlk) = do
    recoveryInProgress epochSlots >>= \case
        True  -> logDebug "Not relaying block in recovery mode"
        False -> do
            logDebug $ sformat ("Calling announceBlock for "%shortHashF%".")
                       (mainBlk ^. gbHeader . headerHashG)
            void $ Diffusion.announceBlockHeader diffusion $ mainBlk ^. gbHeader

----------------------------------------------------------------------------
-- Common logging / logic sink points
----------------------------------------------------------------------------

-- TODO: ban node for it!
onFailedVerifyBlocks
    :: forall ctx m .
       BlockWorkMode ctx m
    => NonEmpty Block -> Text -> m ()
onFailedVerifyBlocks blocks err = do
    logWarning $ sformat ("Failed to verify blocks: "%stext%"\n  blocks = "%listJson)
        err (fmap headerHash blocks)
    throwM $ DialogUnexpected err

blocksAppliedMsg
    :: forall a.
       HasHeaderHash a
    => NonEmpty a -> Text
blocksAppliedMsg (block :| []) =
    sformat ("Block has been adopted "%shortHashF) (headerHash block)
blocksAppliedMsg blocks =
    sformat ("Blocks have been adopted: "%listJson) (fmap (headerHash @a) blocks)

blocksRolledBackMsg
    :: forall a.
       HasHeaderHash a
    => NonEmpty a -> Text
blocksRolledBackMsg =
    sformat ("Blocks have been rolled back: "%listJson) . fmap (headerHash @a)
