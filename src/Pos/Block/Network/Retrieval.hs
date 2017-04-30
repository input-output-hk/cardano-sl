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
import           Formatting                 (build, int, sformat, shown, stext, (%))
import           Mockable                   (handleAll, throw)
import           Paths_cardano_sl           (version)
import           Serokell.Util.Text         (listJson)
import           System.Wlog                (logDebug, logError, logWarning)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Block.Logic            (ClassifyHeaderRes (..),
                                             ClassifyHeadersRes (..), classifyHeaders,
                                             classifyNewHeader)
import           Pos.Block.Network.Announce (announceBlockOuts)
import           Pos.Block.Network.Logic    (handleBlocks, mkBlocksRequest,
                                             mkHeadersRequest, needRecovery,
                                             requestHeaders, triggerRecovery)
import           Pos.Block.Network.Types    (MsgBlock (..), MsgGetBlocks (..),
                                             MsgHeaders (..))
import           Pos.Communication.Limits   (LimitedLength, recvLimited, reifyMsgLimit)
import           Pos.Communication.Protocol (ConversationActions (..), NodeId, OutSpecs,
                                             SendActions (..), WorkerSpec, convH,
                                             toOutSpecs, worker)
import           Pos.Context                (NodeContext (..), getNodeContext)
import           Pos.Crypto                 (shortHashF)
import           Pos.DB.Class               (MonadDBCore)
import           Pos.Reporting.Methods      (reportingFatal)
import           Pos.Shutdown               (runIfNotShutdown)
import           Pos.Ssc.Class              (SscWorkersClass)
import           Pos.Types                  (Block, BlockHeader, HasHeaderHash (..),
                                             HeaderHash, blockHeader, difficultyL,
                                             prevBlockL)
import           Pos.Util                   (_neHead, _neLast)
import           Pos.Util.Chrono            (NE, NewestFirst (..), OldestFirst (..))
import           Pos.WorkMode               (WorkMode)

retrievalWorker
    :: forall ssc m.
       (MonadDBCore m, SscWorkersClass ssc, WorkMode ssc m)
    => m (Set NodeId) -> (WorkerSpec m, OutSpecs)
retrievalWorker getPeers = worker outs (retrievalWorkerImpl getPeers)
  where
    outs = announceBlockOuts <>
           toOutSpecs [convH (Proxy :: Proxy MsgGetBlocks)
                             (Proxy :: Proxy (MsgBlock ssc))
                      ]

-- | Worker that queries blocks. It has two jobs:
--
-- * If there are headers in 'ncBlockRetrievalQueue', this worker retrieves
--   blocks according to that queue.
--
-- * If recovery is in progress, this worker keeps recovery going.
--
-- If both happen at the same time, 'ncBlockRetrievalQueue' takes precedence.
--
retrievalWorkerImpl
    :: forall ssc m.
       (MonadDBCore m, SscWorkersClass ssc, WorkMode ssc m)
    => m (Set NodeId) -> SendActions m -> m ()
retrievalWorkerImpl getPeers sendActions =
    handleAll mainLoopE $ do
        logDebug "Starting retrievalWorker loop"
        mainLoop
  where
    mainLoop = runIfNotShutdown $ reportingFatal getPeers version $ do
        queue        <- ncBlockRetrievalQueue <$> getNodeContext
        recHeaderVar <- ncRecoveryHeader      <$> getNodeContext
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
                triggerRecovery getPeers sendActions
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
        reportingFatal getPeers version $
        workerHandle getPeers sendActions chunk
    handleBlockRetrievalE (peerId, headers) e = do
        logWarning $ sformat
            ("Error handling peerId="%build%", headers="%listJson%": "%shown)
            peerId (fmap headerHash headers) e
        dropUpdateHeader
        dropRecoveryHeaderAndRepeat getPeers sendActions peerId
    --
    handleHeadersRecovery (peerId, rHeader) = do
        logDebug "Block retrieval queue is empty and we're in recovery mode,\
                 \ so we will request more headers"
        whenJustM (mkHeadersRequest (Just $ headerHash rHeader)) $ \mghNext ->
            handleAll (handleHeadersRecoveryE peerId) $
            reportingFatal getPeers version $
            reifyMsgLimit (Proxy @(MsgHeaders ssc)) $ \limPx ->
            withConnectionTo sendActions peerId $ \_peerData ->
                requestHeaders mghNext peerId (Just rHeader) limPx
    handleHeadersRecoveryE peerId e = do
        logWarning $ sformat
            ("Failed while trying to get more headers "%
             "for recovery from peerId="%build%", error: "%shown)
            peerId e
        dropUpdateHeader
        dropRecoveryHeaderAndRepeat getPeers sendActions peerId

dropUpdateHeader :: WorkMode ssc m => m ()
dropUpdateHeader = do
    progressHeaderVar <- ncProgressHeader <$> getNodeContext
    void $ atomically $ tryTakeTMVar progressHeaderVar

dropRecoveryHeader
    :: WorkMode ssc m
    => NodeId
    -> m Bool           -- ^ Whether given peer was kicked and recovery was
                        --   stopped
dropRecoveryHeader peerId = do
    recHeaderVar <- ncRecoveryHeader <$> getNodeContext
    (kicked,realPeer) <- atomically $ do
        let processKick (peer,_) = do
                let p = peer == peerId
                when p $ void $ tryTakeTMVar recHeaderVar
                pure (p, Just peer)
        maybe (pure (True,Nothing)) processKick =<< tryReadTMVar recHeaderVar
    when kicked $ logWarning $
        sformat ("Recovery mode communication dropped with peer "%build) peerId
    unless kicked $
        logDebug $ "Recovery mode wasn't disabled: " <>
                   maybe "noth" show realPeer <> " vs " <> show peerId
    pure kicked

dropRecoveryHeaderAndRepeat
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => m (Set NodeId) -> SendActions m -> NodeId -> m ()
dropRecoveryHeaderAndRepeat getPeers sendActions peerId = do
    kicked <- dropRecoveryHeader peerId
    when kicked $ attemptRestartRecovery
  where
    attemptRestartRecovery = do
        logDebug "Attempting to restart recovery"
        handleAll handleRecoveryTriggerE $ triggerRecovery getPeers sendActions
        logDebug "Attempting to restart recovery over"
    handleRecoveryTriggerE e =
        logError $ "Exception happened while trying to trigger " <>
                   "recovery inside recoveryWorker: " <> show e


-- | Request blocks corresponding to a chain of headers, if we need those
-- blocks
workerHandle
    :: (MonadDBCore m, SscWorkersClass ssc, WorkMode ssc m)
    => m (Set NodeId)
    -> SendActions m
    -> (NodeId, NewestFirst NE (BlockHeader ssc))
    -> m ()
workerHandle getPeers sendActions (peerId, headers) = do
    logDebug $ sformat
        ("retrievalWorker: handling peerId="%build%", headers="%listJson)
        peerId (fmap headerHash headers)
    classificationRes <- classifyHeaders' headers
    let newestHeader = headers ^. _Wrapped . _neHead
        newestHash = headerHash newestHeader
        oldestHash = headerHash $ headers ^. _Wrapped . _neLast
    case classificationRes of
        CHsValid lcaChild ->
            void $ handleCHsValid getPeers sendActions peerId lcaChild newestHash
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
    => m (Set NodeId)
    -> SendActions m
    -> NodeId
    -> BlockHeader ssc
    -> HeaderHash
    -> m ()
handleCHsValid getPeers sendActions peerId lcaChild newestHash = do
    let lcaChildHash = headerHash lcaChild
    logDebug $ sformat validFormat lcaChildHash newestHash
    reifyMsgLimit (Proxy @(MsgBlock ssc)) $ \(_ :: Proxy s0) ->
      withConnectionTo sendActions peerId $
      \_peerData (conv :: ConversationActions MsgGetBlocks
            (LimitedLength s0 (MsgBlock ssc)) m) -> do
        send conv $ mkBlocksRequest lcaChildHash newestHash
        chainE <- runExceptT (retrieveBlocks conv lcaChild newestHash)
        recHeaderVar <- ncRecoveryHeader <$> getNodeContext
        case chainE of
            Left e -> do
                logWarning $ sformat
                    ("Error retrieving blocks from "%shortHashF%
                     " to "%shortHashF%" from peer "%build%": "%stext)
                    lcaChildHash newestHash peerId e
                dropRecoveryHeaderAndRepeat getPeers sendActions peerId
            Right blocks -> do
                logDebug $ sformat
                    ("retrievalWorker: retrieved blocks "%listJson)
                    (map (headerHash . view blockHeader) blocks)
                handleBlocks getPeers peerId blocks sendActions
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
        progressHeaderVar <- ncProgressHeader <$> getNodeContext
        atomically $ do void $ tryTakeTMVar progressHeaderVar
                        putTMVar progressHeaderVar $ block ^. blockHeader
        if curH == endH
        then pure $ one block
        else over _Wrapped (block <|) <$> retrieveBlocks' (i+1) conv curH endH
