{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Retrieval
       ( retrievalWorker
       ) where

import           Control.Concurrent.STM     (putTMVar, swapTMVar, tryReadTBQueue,
                                             tryReadTMVar, tryTakeTMVar)
import           Control.Lens               (_Wrapped)
import           Control.Monad.Except       (ExceptT, runExceptT, throwError)
import           Control.Monad.STM          (retry)
import           Data.List.NonEmpty         ((<|))
import qualified Ether
import           Formatting                 (build, builder, int, sformat, shown, stext,
                                             (%))
import           Mockable                   (delay, handleAll, throw)
import           Paths_cardano_sl           (version)
import           Serokell.Data.Memory.Units (unitBuilder)
import           Serokell.Util              (listJson, sec)
import           System.Wlog                (logDebug, logError, logInfo, logWarning)
import           Universum

import           Pos.Binary.Class           (biSize)
import           Pos.Binary.Communication   ()
import           Pos.Block.Core             (Block, BlockHeader, blockHeader)
import           Pos.Block.Logic            (ClassifyHeaderRes (..), classifyNewHeader)
import           Pos.Block.Network.Announce (announceBlockOuts)
import           Pos.Block.Network.Logic    (handleBlocks, mkBlocksRequest,
                                             mkHeadersRequest, requestHeaders,
                                             triggerRecovery)
import           Pos.Block.Network.Types    (MsgBlock (..), MsgGetBlocks (..),
                                             MsgHeaders (..))
import           Pos.Block.RetrievalQueue   (BlockRetrievalTask (..))
import           Pos.Communication.Limits   (LimitedLength, recvLimited, reifyMsgLimit)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             NodeId, OutSpecs, SendActions (..),
                                             WorkerSpec, convH, toOutSpecs, worker)
import           Pos.Context                (BlockRetrievalQueueTag, ProgressHeaderTag,
                                             RecoveryHeaderTag)
import           Pos.Core                   (HasHeaderHash (..), HeaderHash, difficultyL,
                                             isMoreDifficult, prevBlockL)
import           Pos.Crypto                 (shortHashF)
import           Pos.Reporting.Methods      (reportingFatal)
import           Pos.Shutdown               (runIfNotShutdown)
import           Pos.Ssc.Class              (SscWorkersClass)
import           Pos.Util                   (_neHead, _neLast)
import           Pos.Util.Chrono            (NE, NewestFirst (..), OldestFirst (..))
import           Pos.WorkMode.Class         (WorkMode)

retrievalWorker
    :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => (WorkerSpec m, OutSpecs)
retrievalWorker = worker outs retrievalWorkerImpl
  where
    outs = announceBlockOuts <>
           toOutSpecs [convH (Proxy :: Proxy MsgGetBlocks)
                             (Proxy :: Proxy (MsgBlock ssc))
                      ]

-- I really don't like join
{-# ANN retrievalWorkerImpl ("HLint: ignore Use join" :: Text) #-}

-- | Worker that queries blocks. It has two jobs:
--
-- * If there are headers in 'BlockRetrievalQueue', this worker retrieves
--   blocks according to that queue.
--
-- * If recovery is in progress, this worker keeps recovery going by asking
--   headers (and then switching to block retrieval on next loop iteration).
--
-- If both happen at the same time, 'BlockRetrievalQueue' takes precedence.
--
retrievalWorkerImpl
    :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => SendActions m -> m ()
retrievalWorkerImpl sendActions =
    handleAll mainLoopE $ do
        logDebug "Starting retrievalWorker loop"
        mainLoop
  where
    mainLoop = runIfNotShutdown $ reportingFatal version $ do
        queue        <- Ether.ask @BlockRetrievalQueueTag
        recHeaderVar <- Ether.ask @RecoveryHeaderTag
        logDebug "Waiting on the block queue or recovery header var"
        thingToDoNext <- atomically $ do
            mbQueuedHeadersChunk <- tryReadTBQueue queue
            mbRecHeader <- tryReadTMVar recHeaderVar
            case (mbQueuedHeadersChunk, mbRecHeader) of
                (Nothing, Nothing) -> retry
                (Just (nodeId, task), _) ->
                    pure (handleBlockRetrieval nodeId task)
                (_, Just (nodeId, rHeader))  ->
                    pure (handleHeadersRecovery nodeId rHeader)
        thingToDoNext
        mainLoop
    mainLoopE e = do
        logError $ sformat ("retrievalWorker: error caught "%shown) e
        throw e
    --
    handleBlockRetrieval nodeId BlockRetrievalTask{..} =
        handleAll (handleBlockRetrievalE nodeId brtHeader) $
        reportingFatal version $
        (if brtContinues then handleContinues else handleAlternative)
            nodeId
            brtHeader
    handleContinues nodeId header = do
        endedRecoveryVar <- newEmptyMVar
        processContHeader sendActions endedRecoveryVar nodeId header
    handleAlternative nodeId header = do
        mghM <- mkHeadersRequest (headerHash header)
        whenJust mghM $ \mgh -> do
            updateRecoveryHeader nodeId header
            endedRecoveryVar <- newEmptyMVar
            let cont (headers :: NewestFirst NE (BlockHeader ssc)) =
                    let firstHeader = headers ^. _Wrapped . _neLast
                    in handleCHsValid sendActions endedRecoveryVar nodeId
                                      firstHeader (headerHash header)
            reifyMsgLimit (Proxy @(MsgHeaders ssc)) $ \limPx ->
                withConnectionTo sendActions nodeId $ \_ -> pure $ Conversation $ \conv ->
                requestHeaders cont mgh nodeId limPx conv
                    `finally` void (tryPutMVar endedRecoveryVar False)
            -- Block until the conversation has ended.
            whenM (readMVar endedRecoveryVar) $
                logDebug "Recovery mode exited gracefully"
    handleBlockRetrievalE nodeId header e = do
        logWarning $ sformat
            ("Error handling nodeId="%build%", header="%build%": "%shown)
            nodeId (headerHash header) e
        dropUpdateHeader
        dropRecoveryHeaderAndRepeat sendActions nodeId
    --
    handleHeadersRecovery nodeId rHeader = do
        logDebug "Block retrieval queue is empty and we're in recovery mode,\
                 \ so we will request more headers and blocks"
        handleBlockRetrieval nodeId $
            BlockRetrievalTask { brtHeader = rHeader, brtContinues = False }

-- | Result of attempt to update recovery header.
data UpdateRecoveryResult ssc
    = RecoveryStarted NodeId (BlockHeader ssc)
      -- ^ Recovery header was absent, so we've set it.
    | RecoveryShifted NodeId (BlockHeader ssc) NodeId (BlockHeader ssc)
      -- ^ Header was present, but we've replaced it with another
      -- (more difficult) one.
    | RecoveryContinued NodeId (BlockHeader ssc)
      -- ^ Header is good, but is irrelevant, so recovery variable is
      -- unchanged.

-- | Be careful to run this in the same thread that ends recovery mode
-- (or synchronise those threads with an MVar), otherwise a race
-- condition can occur where we are caught in the recovery mode
-- indefinitely.
updateRecoveryHeader
    :: WorkMode ssc m
    => NodeId
    -> BlockHeader ssc
    -> m ()
updateRecoveryHeader nodeId hdr = do
    recHeaderVar <- Ether.ask @RecoveryHeaderTag
    logDebug "Updating recovery header..."
    updated <- atomically $ do
        mbRecHeader <- tryReadTMVar recHeaderVar
        case mbRecHeader of
            Nothing -> do
                putTMVar recHeaderVar (nodeId, hdr)
                return $ RecoveryStarted nodeId hdr
            Just (oldNodeId, oldHdr) -> do
                let needUpdate = hdr `isMoreDifficult` oldHdr
                if needUpdate
                    then swapTMVar recHeaderVar (nodeId, hdr) $>
                         RecoveryShifted oldNodeId oldHdr nodeId hdr
                    else return $ RecoveryContinued oldNodeId oldHdr
    logDebug $ case updated of
        RecoveryStarted rNodeId rHeader -> sformat
            ("Recovery started with nodeId="%build%" and tip="%build)
            rNodeId
            (headerHash rHeader)
        RecoveryShifted rNodeId' rHeader' rNodeId rHeader -> sformat
            ("Recovery shifted from nodeId="%build%" and tip="%build%
             " to nodeId="%build%" and tip="%build)
            rNodeId' (headerHash rHeader')
            rNodeId  (headerHash rHeader)
        RecoveryContinued rNodeId rHeader -> sformat
            ("Recovery continued with nodeId="%build%" and tip="%build)
            rNodeId
            (headerHash rHeader)

dropUpdateHeader :: WorkMode ssc m => m ()
dropUpdateHeader = do
    progressHeaderVar <- Ether.ask @ProgressHeaderTag
    void $ atomically $ tryTakeTMVar progressHeaderVar

-- | The returned 'Bool' signifies whether given peer was kicked and recovery
-- was stopped.
--
-- NB. The reason @nodeId@ is passed is that we want to avoid a race
-- condition. If you work with peer P and header H, after failure you want to
-- drop communication with P; however, if at the same time a new block
-- arrives and another thread replaces peer and header to (P2, H2), you want
-- to continue working with P2 and ignore the exception that happened with P.
-- So, @nodeId@ is used to check that the peer wasn't replaced mid-execution.
dropRecoveryHeader
    :: WorkMode ssc m
    => NodeId
    -> m Bool
dropRecoveryHeader nodeId = do
    recHeaderVar <- Ether.ask @RecoveryHeaderTag
    (kicked,realPeer) <- atomically $ do
        let processKick (peer,_) = do
                let p = peer == nodeId
                when p $ void $ tryTakeTMVar recHeaderVar
                pure (p, Just peer)
        maybe (pure (True,Nothing)) processKick =<< tryReadTMVar recHeaderVar
    when kicked $ logWarning $
        sformat ("Recovery mode communication dropped with peer "%build) nodeId
    unless kicked $
        logDebug $ "Recovery mode wasn't disabled: " <>
                   maybe "noth" show realPeer <> " vs " <> show nodeId
    pure kicked

dropRecoveryHeaderAndRepeat
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => SendActions m -> NodeId -> m ()
dropRecoveryHeaderAndRepeat sendActions nodeId = do
    kicked <- dropRecoveryHeader nodeId
    when kicked $ attemptRestartRecovery
  where
    attemptRestartRecovery = do
        logInfo "Attempting to restart recovery"
        delay $ sec 2
        handleAll handleRecoveryTriggerE $ triggerRecovery sendActions
        logDebug "Attempting to restart recovery over"
    handleRecoveryTriggerE e =
        logError $ "Exception happened while trying to trigger " <>
                   "recovery inside recoveryWorker: " <> show e

-- | Process header that was thought to be continuation. If it's not
-- now, it is discarded.
processContHeader
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => SendActions m
    -> MVar Bool
    -> NodeId
    -> BlockHeader ssc
    -> m ()
processContHeader sendActions endedRecoveryVar nodeId header = do
    classificationRes <- classifyNewHeader header
    case classificationRes of
        CHContinues ->
            handleCHsValid sendActions endedRecoveryVar nodeId
                           header (headerHash header)
        res -> logDebug $
            "processContHeader: expected header to " <>
             "be continuation, but it's " <> show res

handleCHsValid
    :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => SendActions m
    -> MVar Bool
    -> NodeId
    -> BlockHeader ssc
    -> HeaderHash
    -> m ()
handleCHsValid sendActions endedRecoveryVar nodeId lcaChild newestHash = do
    let lcaChildHash = headerHash lcaChild
    logDebug $ sformat validFormat lcaChildHash newestHash
    reifyMsgLimit (Proxy @(MsgBlock ssc)) $ \(_ :: Proxy s0) ->
      withConnectionTo sendActions nodeId $
      \_ -> pure $ Conversation $
          \(conv :: ConversationActions MsgGetBlocks
             (LimitedLength s0 (MsgBlock ssc)) m) -> do
        send conv $ mkBlocksRequest lcaChildHash newestHash
        chainE <- runExceptT (retrieveBlocks conv lcaChild newestHash)
        recHeaderVar <- Ether.ask @RecoveryHeaderTag
        case chainE of
            Left e -> do
                logWarning $ sformat
                    ("Error retrieving blocks from "%shortHashF%
                     " to "%shortHashF%" from peer "%build%": "%stext)
                    lcaChildHash newestHash nodeId e
                dropRecoveryHeaderAndRepeat sendActions nodeId
            Right blocks -> do
                logDebug $ sformat
                    ("retrievalWorker: retrieved blocks of size "%builder%": "%listJson)
                    (unitBuilder $ biSize blocks)
                    (map (headerHash . view blockHeader) blocks)
                handleBlocks nodeId blocks sendActions
                dropUpdateHeader
                -- If we've downloaded any block with bigger
                -- difficulty than ncrecoveryheader, we're
                -- gracefully exiting recovery mode.
                let isMoreDifficultThan b x = b ^. difficultyL >= x ^. difficultyL
                endedRecovery <- atomically $ do
                    mRecHeader <- tryReadTMVar recHeaderVar
                    case mRecHeader of
                        Nothing -> return False
                        Just (_, rHeader) ->
                            if any (`isMoreDifficultThan` rHeader) blocks
                                then isJust <$> tryTakeTMVar recHeaderVar
                                else return False
                void $ tryPutMVar endedRecoveryVar endedRecovery
  where
    validFormat =
        "Requesting blocks from " %shortHashF % " to " %shortHashF

retrieveBlocks
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => ConversationActions MsgGetBlocks (LimitedLength s (MsgBlock ssc)) m
    -> BlockHeader ssc
    -> HeaderHash
    -> ExceptT Text m (OldestFirst NE (Block ssc))
retrieveBlocks conv lcaChild endH = do
    blocks <- retrieveBlocks' 0 conv (lcaChild ^. prevBlockL) endH
    let b0 = blocks ^. _Wrapped . _neHead
    if headerHash b0 == headerHash lcaChild
       then pure blocks
       else throwError $ sformat
                ("First block of chain is "%build%
                 " instead of expected "%build)
                (b0 ^. blockHeader) lcaChild


retrieveBlocks'
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => Int        -- ^ Index of block we're requesting
    -> ConversationActions MsgGetBlocks (LimitedLength s (MsgBlock ssc)) m
    -> HeaderHash -- ^ We're expecting a child of this block
    -> HeaderHash -- ^ Block at which to stop
    -> ExceptT Text m (OldestFirst NE (Block ssc))
retrieveBlocks' i conv prevH endH = lift (recvLimited conv) >>= \case
    Nothing -> throwError $ sformat ("Failed to receive block #"%int) i
    Just (MsgBlock block) -> do
        let prevH' = block ^. prevBlockL
            curH = headerHash block
        when (prevH' /= prevH) $ do
            throwError $ sformat
                ("Received block #"%int%" with prev hash "%shortHashF%
                 " while "%shortHashF%" was expected: "%build)
                i prevH' prevH (block ^. blockHeader)
        progressHeaderVar <- Ether.ask @ProgressHeaderTag
        atomically $ do void $ tryTakeTMVar progressHeaderVar
                        putTMVar progressHeaderVar $ block ^. blockHeader
        if curH == endH
        then pure $ one block
        else over _Wrapped (block <|) <$> retrieveBlocks' (i+1) conv curH endH
