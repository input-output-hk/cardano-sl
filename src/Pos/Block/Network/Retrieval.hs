{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Retrieval
       ( retrievalWorker
       ) where

import           Control.Concurrent.STM     (putTMVar, tryReadTBQueue, tryReadTMVar,
                                             tryTakeTMVar)
import           Control.Lens               (_Wrapped)
import           Control.Monad.Except       (ExceptT, runExceptT, throwError)
import           Control.Monad.STM          (retry)
import           Data.List.NonEmpty         ((<|))
import qualified Ether
import           Formatting                 (build, int, sformat, shown, stext, (%))
import           Mockable                   (handleAll, throw)
import           Paths_cardano_sl           (version)
import           Serokell.Util.Text         (listJson)
import           System.Wlog                (logDebug, logError, logWarning)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Block.Core             (Block, BlockHeader, blockHeader)
import           Pos.Block.Logic            (ClassifyHeaderRes (..),
                                             ClassifyHeadersRes (..), classifyHeaders,
                                             classifyNewHeader, needRecovery)
import           Pos.Block.Network.Announce (announceBlockOuts)
import           Pos.Block.Network.Logic    (handleBlocks, mkBlocksRequest,
                                             mkHeadersRequest, requestHeaders,
                                             triggerRecovery)
import           Pos.Block.Network.Types    (MsgBlock (..), MsgGetBlocks (..),
                                             MsgHeaders (..))
import           Pos.Communication.Limits   (LimitedLength, recvLimited, reifyMsgLimit)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             NodeId, OutSpecs, SendActions (..),
                                             WorkerSpec, convH, toOutSpecs, worker)
import           Pos.Context                (BlockRetrievalQueueTag, ProgressHeaderTag,
                                             RecoveryHeaderTag)
import           Pos.Core                   (HasHeaderHash (..), HeaderHash, difficultyL,
                                             prevBlockL)
import           Pos.Crypto                 (shortHashF)
import           Pos.DB.Class               (MonadDBCore)
import           Pos.Reporting.Methods      (reportingFatal)
import           Pos.Shutdown               (runIfNotShutdown)
import           Pos.Ssc.Class              (SscWorkersClass)
import           Pos.Util                   (_neHead, _neLast)
import           Pos.Util.Chrono            (NE, NewestFirst (..), OldestFirst (..))
import           Pos.WorkMode.Class         (WorkMode)

retrievalWorker
    :: forall ssc m.
       (MonadDBCore m, SscWorkersClass ssc, WorkMode ssc m)
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
       (MonadDBCore m, SscWorkersClass ssc, WorkMode ssc m)
    => SendActions m -> m ()
retrievalWorkerImpl sendActions =
    handleAll mainLoopE $ do
        logDebug "Starting retrievalWorker loop"
        mainLoop
  where
    mainLoop = runIfNotShutdown $ reportingFatal version $ do
        queue        <- Ether.ask @BlockRetrievalQueueTag
        recHeaderVar <- Ether.ask @RecoveryHeaderTag
        -- It is not our job to *start* recovery; if we actually need
        -- recovery, the 'checkForReceivedBlocksWorker' worker in
        -- Pos.Security.Workers will trigger it. What we do here is simply an
        -- optimisation: if after doing recovery for some time we are now
        -- less than K blocks behind, we have two choices:
        --   a) ask for blocks up to 'ncRecoveryHeader', download them,
        --      realise that we're still behind (because while recovery was
        --      in progress new blocks have likely appeared), and then ask our
        --      neighbors for tips and download another, final chunk of blocks
        --   b) just clear 'ncRecoveryHeader', ask for tips and download all
        --      missing blocks in one go
        -- So, here we take the second approach because it lets us do just one
        -- block request instead of two.
        needRecovery_ <- needRecovery @ssc
        unless needRecovery_ $
            whenJustM (atomically $ tryTakeTMVar recHeaderVar) $ const $ do
                logDebug "Requesting tips from main loop with triggerRecovery"
                triggerRecovery sendActions
        -- Here we decide what we'll actually do next
        logDebug "Waiting on the block queue or recovery header var"
        thingToDoNext <- atomically $ do
            mbQueuedHeadersChunk <- tryReadTBQueue queue
            mbRecHeader <- if needRecovery_
                               then tryReadTMVar recHeaderVar
                               else pure Nothing
            case (mbQueuedHeadersChunk, mbRecHeader) of
                (Nothing, Nothing) -> retry
                (Just chunk, _)    -> pure (handleBlockRetrieval chunk)
                (_, Just rec')     -> pure (handleHeadersRecovery rec')
        thingToDoNext
        mainLoop
    mainLoopE e = do
        logError $ sformat ("retrievalWorker: error caught "%shown) e
        throw e
    --
    handleBlockRetrieval chunk =
        handleAll (handleBlockRetrievalE chunk) $
        reportingFatal version $
        workerHandle sendActions chunk
    handleBlockRetrievalE (nodeId, headers) e = do
        logWarning $ sformat
            ("Error handling nodeId="%build%", headers="%listJson%": "%shown)
            nodeId (fmap headerHash headers) e
        dropUpdateHeader
        dropRecoveryHeaderAndRepeat sendActions nodeId
    --
    handleHeadersRecovery (nodeId, rHeader) = do
        logDebug "Block retrieval queue is empty and we're in recovery mode,\
                 \ so we will request more headers"
        whenJustM (mkHeadersRequest (Just $ headerHash rHeader)) $ \mghNext ->
            handleAll (handleHeadersRecoveryE nodeId) $
            reportingFatal version $
            reifyMsgLimit (Proxy @(MsgHeaders ssc)) $ \limPx ->
            withConnectionTo sendActions nodeId $ \_ -> pure $ Conversation $
                requestHeaders mghNext nodeId (Just rHeader) limPx
    handleHeadersRecoveryE nodeId e = do
        logWarning $ sformat
            ("Failed while trying to get more headers "%
             "for recovery from nodeId="%build%", error: "%shown)
            nodeId e
        dropUpdateHeader
        dropRecoveryHeaderAndRepeat sendActions nodeId

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
        logDebug "Attempting to restart recovery"
        handleAll handleRecoveryTriggerE $ triggerRecovery sendActions
        logDebug "Attempting to restart recovery over"
    handleRecoveryTriggerE e =
        logError $ "Exception happened while trying to trigger " <>
                   "recovery inside recoveryWorker: " <> show e


-- | Request blocks corresponding to a chain of headers, if we need those
-- blocks
workerHandle
    :: (MonadDBCore m, SscWorkersClass ssc, WorkMode ssc m)
    => SendActions m
    -> (NodeId, NewestFirst NE (BlockHeader ssc))
    -> m ()
workerHandle sendActions (nodeId, headers) = do
    logDebug $ sformat
        ("retrievalWorker: handling nodeId="%build%", headers="%listJson)
        nodeId (fmap headerHash headers)
    classificationRes <- classifyHeaders' headers
    let newestHeader = headers ^. _Wrapped . _neHead
        newestHash = headerHash newestHeader
        oldestHash = headerHash $ headers ^. _Wrapped . _neLast
    case classificationRes of
        CHsValid lcaChild ->
            void $ handleCHsValid sendActions nodeId lcaChild newestHash
        CHsUseless reason ->
            logDebug $ sformat uselessFormat oldestHash newestHash reason
        CHsInvalid reason ->
            logWarning $ sformat invalidFormat oldestHash newestHash reason
  where
    classifyHeaders' (NewestFirst (header :| [])) = do
        classificationRes <- classifyNewHeader header
        -- TODO: should we set 'To' hash to hash of header or leave it unlimited?
        case classificationRes of
            CHContinues -> pure $ CHsValid header
            CHAlternative ->
                pure $ CHsInvalid "Expected header to be continuation, not alternative"
            CHUseless reason -> pure $ CHsUseless reason
            CHInvalid reason -> pure $ CHsInvalid reason
    classifyHeaders' h = classifyHeaders h
    invalidFormat =
        "Chain of headers from " %shortHashF % " to " %shortHashF %
        " is considered invalid: " %stext
    uselessFormat =
        "Chain of headers from " %shortHashF % " to " %shortHashF %
        " is useless for the following reason: " %stext

handleCHsValid
    :: forall ssc m.
       (MonadDBCore m, SscWorkersClass ssc, WorkMode ssc m)
    => SendActions m
    -> NodeId
    -> BlockHeader ssc
    -> HeaderHash
    -> m ()
handleCHsValid sendActions nodeId lcaChild newestHash = do
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
                    ("retrievalWorker: retrieved blocks "%listJson)
                    (map (headerHash . view blockHeader) blocks)
                handleBlocks nodeId blocks sendActions
                dropUpdateHeader
                -- If we've downloaded any block with bigger
                -- difficulty than ncrecoveryheader, we're
                -- gracefully exiting recovery mode.
                let isMoreDifficultThan b x = b ^. difficultyL >= x ^. difficultyL
                atomically $ whenJustM (tryReadTMVar recHeaderVar) $ \(_,rHeader) ->
                    when (any (`isMoreDifficultThan` rHeader) blocks)
                         (void $ tryTakeTMVar recHeaderVar)
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
