{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | Network-related logic that's mostly methods and dialogs between
-- nodes. Also see "Pos.Chain.Block.Network.Retrieval" for retrieval worker
-- loop logic.
module Pos.Network.Block.Logic
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
import           Formatting (bprint, build, sformat, shown, stext, (%))
import qualified Formatting.Buildable as B
import           Serokell.Util.Text (listJson)
import qualified System.Metrics.Gauge as Metrics

import           Pos.Chain.Block (ApplyBlocksException, Block, BlockHeader,
                     Blund, HasHeaderHash (..), HeaderHash, LastKnownHeaderTag,
                     blockHeader, gbHeader, headerHashG, prevBlockL)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Core (isMoreDifficult)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..),
                     _NewestFirst, _OldestFirst)
import           Pos.Core.Conc (forConcurrently)
import           Pos.Core.Exception (cardanoExceptionFromException,
                     cardanoExceptionToException)
import           Pos.Core.Reporting (HasMisbehaviorMetrics (..),
                     MisbehaviorMetrics (..))
import           Pos.Crypto (ProtocolMagic, shortHashF)
import           Pos.DB.Block (ClassifyHeaderRes (..), classifyNewHeader,
                     lcaWithMainChain, verifyAndApplyBlocks)
import qualified Pos.DB.Block as L
import qualified Pos.DB.Block as DB
import           Pos.DB.GState.Lock (Priority (..), modifyStateLock)
import           Pos.Infra.Communication.Protocol (NodeId)
import           Pos.Infra.Diffusion.Types (Diffusion)
import qualified Pos.Infra.Diffusion.Types as Diffusion
import           Pos.Infra.Recovery.Info (recoveryInProgress)
import           Pos.Infra.Util.JsonLog.Events (MemPoolModifyReason (..))
import           Pos.Network.Block.RetrievalQueue (BlockRetrievalQueue,
                     BlockRetrievalQueueTag, BlockRetrievalTask (..))
import           Pos.Network.Block.WorkMode (BlockWorkMode)
import           Pos.Util (buildListBounds, multilineBounds, _neLast)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Trace (noTrace)
import           Pos.Util.Trace.Named (TraceNamed, logDebug, logInfo,
                     logWarning, natTrace)
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
    => TraceNamed m
    -> ProtocolMagic -> Diffusion m -> m ()
triggerRecovery logTrace pm diffusion = unlessM recoveryInProgress $ do
    logDebug logTrace "Recovery triggered, requesting tips from neighbors"
    -- The 'catch' here is for an exception when trying to enqueue the request.
    -- In 'requestTipsAndProcess', IO exceptions are caught, for each
    -- individual request per-peer. Those are not re-thrown.
    void requestTipsAndProcess `catch`
        \(e :: SomeException) -> do
           logDebug logTrace ("Error happened in triggerRecovery: " <> show e)
           throwM e
    logDebug logTrace "Finished requesting tips for recovery"
  where
    requestTipsAndProcess = do
        requestsMap <- Diffusion.requestTip diffusion
        forConcurrently (M.toList requestsMap) $ \it@(nodeId, _) -> waitAndProcessOne it `catch`
            -- Catch and squelch IOExceptions so that one failed request to one
            -- particlar peer does not stop the others.
            \(e :: IOException) ->
                logDebug logTrace $ sformat ("Error requesting tip from "%shown%": "%shown) nodeId e
    waitAndProcessOne (nodeId, mbh) = do
        -- 'mbh' is an 'm' term that returns when the header has been
        -- downloaded.
        bh <- mbh
        -- I know, it's not unsolicited. TODO rename.
        handleUnsolicitedHeader logTrace pm bh nodeId

----------------------------------------------------------------------------
-- Headers processing
----------------------------------------------------------------------------

handleUnsolicitedHeader
    :: ( BlockWorkMode ctx m
       )
    => TraceNamed m
    -> ProtocolMagic
    -> BlockHeader
    -> NodeId
    -> m ()
handleUnsolicitedHeader logTrace pm header nodeId = do
    logDebug logTrace $ sformat
        ("handleUnsolicitedHeader: single header was propagated, processing:\n"
         %build) header
    classificationRes <- classifyNewHeader pm header
    -- TODO: should we set 'To' hash to hash of header or leave it unlimited?
    case classificationRes of
        CHContinues -> do
            logDebug logTrace $ sformat continuesFormat hHash
            addHeaderToBlockRequestQueue logTrace nodeId header True
        CHAlternative -> do
            logDebug logTrace $ sformat alternativeFormat hHash
            addHeaderToBlockRequestQueue logTrace nodeId header False
        CHUseless reason -> logDebug logTrace $ sformat uselessFormat hHash reason
        CHInvalid _ -> do
            logWarning logTrace $ sformat ("handleUnsolicited: header "%shortHashF%
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
        "Header " %shortHashF % " is useless for the following reason: " %stext

----------------------------------------------------------------------------
-- Putting things into request queue
----------------------------------------------------------------------------

-- | Given a valid blockheader and nodeid, this function will put them into
-- download queue and they will be processed later.
addHeaderToBlockRequestQueue
    :: forall ctx m.
       (BlockWorkMode ctx m)
    => TraceNamed m
    -> NodeId
    -> BlockHeader
    -> Bool -- ^ Was the block classified as chain continuation?
    -> m ()
addHeaderToBlockRequestQueue logTrace nodeId header continues = do
    let hHash = headerHash header
    logDebug logTrace $ sformat ("addToBlockRequestQueue, : "%shortHashF) hHash
    queue <- view (lensOf @BlockRetrievalQueueTag)
    lastKnownH <- view (lensOf @LastKnownHeaderTag)
    added <- atomically $ do
        updateLastKnownHeader lastKnownH header
        addTaskToBlockRequestQueue nodeId queue $
            BlockRetrievalTask { brtHeader = header, brtContinues = continues }
    if added
    then logDebug logTrace $ sformat ("Added headers to block request queue: nodeId="%build%
                             ", header="%build)
                            nodeId hHash
    else logWarning logTrace $ sformat ("Failed to add headers from "%build%
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
    => TraceNamed IO
    -> ProtocolMagic
    -> TxpConfiguration
    -> OldestFirst NE Block
    -> Diffusion m
    -> m ()
handleBlocks logTrace pm txpConfig blocks diffusion = do
    logDebug logTrace' "handleBlocks: processing"
    inAssertMode $ logInfo logTrace' $
        sformat ("Processing sequence of blocks: " % buildListBounds % "...") $
            getOldestFirst $ map headerHash blocks
    maybe onNoLca handleBlocksWithLca =<<
        lcaWithMainChain (map (view blockHeader) blocks)
    inAssertMode $ logDebug logTrace' $ "Finished processing sequence of blocks"
  where
    logTrace' = natTrace liftIO logTrace
    onNoLca = logWarning logTrace' $
        "Sequence of blocks can't be processed, because there is no LCA. " <>
        "Probably rollback happened in parallel"

    handleBlocksWithLca :: HeaderHash -> m ()
    handleBlocksWithLca lcaHash = do
        logDebug logTrace' $ sformat ("Handling block w/ LCA, which is "%shortHashF) lcaHash
        -- Head blund in result is the youngest one.
        toRollback <- DB.loadBlundsFromTipWhile $ \blk -> headerHash blk /= lcaHash
        maybe (applyWithoutRollback logTrace pm txpConfig diffusion blocks)
              (applyWithRollback logTrace pm txpConfig diffusion blocks lcaHash)
              (_NewestFirst nonEmpty toRollback)

applyWithoutRollback
    :: forall ctx m.
       ( BlockWorkMode ctx m
       , HasMisbehaviorMetrics ctx
       )
    => TraceNamed IO
    -> ProtocolMagic
    -> TxpConfiguration
    -> Diffusion m
    -> OldestFirst NE Block
    -> m ()
applyWithoutRollback logTrace pm txpConfig diffusion blocks = do
    logInfo logTrace' . sformat ("Trying to apply blocks w/o rollback. " % multilineBounds 6)
       . getOldestFirst . map (view blockHeader) $ blocks
    modifyStateLock noTrace HighPriority ApplyBlock applyWithoutRollbackDo >>= \case
        Left (pretty -> err) ->
            onFailedVerifyBlocks logTrace' (getOldestFirst blocks) err
        Right newTip -> do
            when (newTip /= newestTip) $
                logWarning logTrace' $ sformat
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
            relayBlock logTrace' diffusion toRelay
            logInfo logTrace' $ blocksAppliedMsg applied
            -- TODO for_ blocks $ jsonLog . jlAdoptedBlock
  where
    logTrace' = natTrace liftIO logTrace
    newestTip = blocks ^. _OldestFirst . _neLast . headerHashG
    applyWithoutRollbackDo
        :: HeaderHash -> m (HeaderHash, Either ApplyBlocksException HeaderHash)
    applyWithoutRollbackDo curTip = do
        logInfo logTrace' "Verifying and applying blocks..."
        ctx <- L.getVerifyBlocksContext
        res <- fmap fst <$> verifyAndApplyBlocks logTrace pm txpConfig ctx False blocks
        logInfo logTrace' "Verifying and applying blocks done"
        let newTip = either (const curTip) identity res
        pure (newTip, res)

applyWithRollback
    :: ( BlockWorkMode ctx m
       , HasMisbehaviorMetrics ctx
       )
    => TraceNamed IO
    -> ProtocolMagic
    -> TxpConfiguration
    -> Diffusion m
    -> OldestFirst NE Block
    -> HeaderHash
    -> NewestFirst NE Blund
    -> m ()
applyWithRollback logTrace pm txpConfig diffusion toApply lca toRollback = do
    logInfo logTrace' . sformat ("Trying to apply blocks w/o rollback. " % multilineBounds 6)
       . getOldestFirst . map (view blockHeader) $ toApply
    logInfo logTrace' $ sformat ("Blocks to rollback "%listJson) toRollbackHashes
    res <- modifyStateLock noTrace HighPriority ApplyBlockWithRollback $ \curTip -> do
        res <- L.applyWithRollback logTrace pm txpConfig toRollback toApplyAfterLca
        pure (either (const curTip) identity res, res)
    case res of
        Left (pretty -> err) ->
            logWarning logTrace' $ "Couldn't apply blocks with rollback: " <> err
        Right newTip -> do
            logDebug logTrace' $ sformat
                ("Finished applying blocks w/ rollback, relaying new tip: "%shortHashF)
                newTip
            reportRollback
            logInfo logTrace' $ blocksRolledBackMsg (getNewestFirst toRollback)
            logInfo logTrace' $ blocksAppliedMsg (getOldestFirst toApply)
            -- TODO for_ (getOldestFirst toApply) $ jsonLog . jlAdoptedBlock
            relayBlock logTrace' diffusion $ toApply ^. _OldestFirst . _neLast
  where
    logTrace' = natTrace liftIO logTrace
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
    => TraceNamed m
    -> Diffusion m -> Block -> m ()
relayBlock logTrace _ (Left _)                  = logDebug logTrace "Not relaying Genesis block"
relayBlock logTrace diffusion (Right mainBlk) = do
    recoveryInProgress >>= \case
        True -> logDebug logTrace "Not relaying block in recovery mode"
        False -> do
            logDebug logTrace $ sformat ("Calling announceBlock for "%shortHashF%".")
                       (mainBlk ^. gbHeader . headerHashG)
            void $ Diffusion.announceBlockHeader diffusion $ mainBlk ^. gbHeader

----------------------------------------------------------------------------
-- Common logging / logic sink points
----------------------------------------------------------------------------

-- TODO: ban node for it!
onFailedVerifyBlocks
    :: forall ctx m.
       (BlockWorkMode ctx m)
    => TraceNamed m
    -> NonEmpty Block -> Text -> m ()
onFailedVerifyBlocks logTrace blocks err = do
    logWarning logTrace $ sformat ("Failed to verify blocks: "%stext%"\n  blocks = "%listJson)
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
