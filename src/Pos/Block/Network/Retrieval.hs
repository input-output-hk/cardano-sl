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
       (SscWorkersClass ssc, WorkMode ssc m)
    => (WorkerSpec m, OutSpecs)
retrievalWorker = worker outs retrievalWorkerImpl
  where
    outs = announceBlockOuts <>
           toOutSpecs [convH (Proxy :: Proxy MsgGetBlocks)
                             (Proxy :: Proxy (MsgBlock ssc))
                      ]

retrievalWorkerImpl
    :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => SendActions m -> m ()
retrievalWorkerImpl sendActions = handleAll handleTop $ do
    logDebug "Starting retrievalWorker loop"
    mainLoop
  where
    mainLoop = runIfNotShutdown $ reportingFatal version $ do
        queue <- ncBlockRetrievalQueue <$> getNodeContext
        recHeaderVar <- ncRecoveryHeader <$> getNodeContext
        inRecovery <- needRecovery (Proxy @ssc)
        unless inRecovery $
            whenJustM (atomically $ tryTakeTMVar recHeaderVar) $
                const (triggerRecovery sendActions)
        logDebug "Waiting on the queue"
        loopCont <- atomically $ do
            qV <- tryReadTBQueue queue
            rV <- if inRecovery
                     then tryReadTMVar recHeaderVar
                     else pure Nothing
            case (qV, rV) of
                (Nothing, Nothing) -> retry
                (Just v, _)        -> pure $ Left v
                (Nothing, Just r)  -> pure $ Right r
        either onLeft onRight loopCont
        mainLoop
    onLeft ph =
        handleAll (handleBlockRetrievalE ph) $
        reportingFatal version $ workerHandle sendActions ph
    onRight (peerId, rHeader) = do
        logDebug "Queue is empty, we're in recovery mode -> querying more"
        whenJustM (mkHeadersRequest (Just $ headerHash rHeader)) $ \mghNext ->
            handleAll (handleHeadersRecoveryE peerId) $
            reportingFatal version $
            reifyMsgLimit (Proxy @(MsgHeaders ssc)) $ \limPx ->
            withConnectionTo sendActions peerId $ \_peerData ->
                requestHeaders mghNext peerId (Just rHeader) limPx
    handleBlockRetrievalE (peerId, headers) e = do
        logWarning $ sformat
            ("Error handling peerId="%build%", headers="%listJson%": "%shown)
            peerId (fmap headerHash headers) e
        dropUpdateHeader
        dropRecoveryHeaderAndRepeat sendActions peerId
    handleHeadersRecoveryE peerId e = do
        logWarning $ sformat
            ("Failed while trying to get more headers "%
             "for recovery from peerId="%build%", error: "%shown)
            peerId e
        dropUpdateHeader
        dropRecoveryHeaderAndRepeat sendActions peerId
    handleTop e = do
        logError $ sformat ("retrievalWorker: error caught "%shown) e
        throw e

dropUpdateHeader :: WorkMode ssc m => m ()
dropUpdateHeader = do
    progressHeaderVar <- ncProgressHeader <$> getNodeContext
    void $ atomically $ tryTakeTMVar progressHeaderVar

dropRecoveryHeader :: WorkMode ssc m => NodeId -> m Bool
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

{-# ANN dropRecoveryHeaderAndRepeat ("HLint: ignore Use whenM" :: Text) #-}
dropRecoveryHeaderAndRepeat
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => SendActions m -> NodeId -> m ()
dropRecoveryHeaderAndRepeat sendActions peerId = do
    kicked <- dropRecoveryHeader peerId
    when kicked $ attemptRestartRecovery
  where
    attemptRestartRecovery = do
        logDebug "Attempting to restart recovery"
        handleAll handleRecoveryTriggerE $ triggerRecovery sendActions
        logDebug "Attempting to restart recovery over"
    handleRecoveryTriggerE e =
        logError $ "Exception happened while trying to trigger " <>
                   "recovery inside recoveryWorker: " <> show e


workerHandle
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => SendActions m -> (NodeId, NewestFirst NE (BlockHeader ssc)) -> m ()
workerHandle sendActions (peerId, headers) = do
    logDebug $ sformat
        ("retrievalWorker: handling peerId="%build%", headers="%listJson)
        peerId (fmap headerHash headers)
    classificationRes <- classifyHeaders' headers
    let newestHeader = headers ^. _Wrapped . _neHead
        newestHash = headerHash newestHeader
        oldestHash = headerHash $ headers ^. _Wrapped . _neLast
    case classificationRes of
        CHsValid lcaChild ->
            void $ handleCHsValid sendActions peerId lcaChild newestHash
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
       (SscWorkersClass ssc, WorkMode ssc m)
    => SendActions m -> NodeId -> (BlockHeader ssc) -> HeaderHash -> m ()
handleCHsValid sendActions peerId lcaChild newestHash = do
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
                dropRecoveryHeaderAndRepeat sendActions peerId
            Right blocks -> do
                logDebug $ sformat
                    ("retrievalWorker: retrieved blocks "%listJson)
                    (map (headerHash . view blockHeader) blocks)
                handleBlocks peerId blocks sendActions
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
    => Int
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
                ("Received block #"%int%" with prev hash "%shortHashF%" while "%
                 shortHashF%" was expected: "%build)
                i prevH' prevH (block ^. blockHeader)
        progressHeaderVar <- ncProgressHeader <$> getNodeContext
        atomically $ do void $ tryTakeTMVar progressHeaderVar
                        putTMVar progressHeaderVar $ block ^. blockHeader
        if curH == endH
        then pure $ one block
        else over _Wrapped (block <|) <$> retrieveBlocks' (i+1) conv curH endH
