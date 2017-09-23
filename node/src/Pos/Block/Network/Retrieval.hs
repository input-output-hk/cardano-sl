{-# LANGUAGE RankNTypes #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Retrieval
       ( retrievalWorker
       ) where

import           Universum

import           Control.Concurrent.STM     (putTMVar, swapTMVar, tryReadTBQueue,
                                             tryReadTMVar, tryTakeTMVar)
import           Control.Lens               (to, _Wrapped)
import           Control.Monad.Except       (ExceptT, runExceptT, throwError)
import           Control.Monad.STM          (retry)
import           Data.List.NonEmpty         ((<|))
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as S
import           Ether.Internal             (HasLens (..))
import           Formatting                 (build, builder, int, sformat, stext, (%))
import           Mockable                   (delay, handleAll)
import           Serokell.Data.Memory.Units (unitBuilder)
import           Serokell.Util              (listJson, sec)
import           System.Wlog                (logDebug, logInfo, logWarning)

import           Pos.Binary.Class           (biSize)
import           Pos.Binary.Communication   ()
import           Pos.Block.Core             (Block, BlockHeader, blockHeader)
import           Pos.Block.Logic            (ClassifyHeaderRes (..), classifyNewHeader)
import           Pos.Block.Network.Announce (announceBlockOuts)
import           Pos.Block.Network.Logic    (BlockNetLogicException (DialogUnexpected),
                                             MkHeadersRequestResult (..), handleBlocks,
                                             mkBlocksRequest, mkHeadersRequest,
                                             requestHeaders, triggerRecovery)
import           Pos.Block.Network.Types    (MsgBlock (..), MsgGetBlocks (..))
import           Pos.Block.RetrievalQueue   (BlockRetrievalTask (..))
import           Pos.Communication.Limits   (recvLimited)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             EnqueueMsg, MsgType (..), NodeId, OutSpecs,
                                             SendActions (..), WorkerSpec, convH,
                                             toOutSpecs, waitForConversations, worker)
import           Pos.Context                (BlockRetrievalQueueTag, ProgressHeaderTag,
                                             RecoveryHeaderTag)
import           Pos.Core                   (HasHeaderHash (..), HeaderHash, difficultyL,
                                             isMoreDifficult, prevBlockL)
import           Pos.Crypto                 (shortHashF)
import           Pos.Reporting              (reportOrLogE, reportOrLogW)
import           Pos.Ssc.Class              (SscWorkersClass)
import           Pos.Util                   (_neHead, _neLast)
import           Pos.Util.Chrono            (NE, NewestFirst (..), OldestFirst (..),
                                             _NewestFirst, _OldestFirst)
import           Pos.WorkMode.Class         (WorkMode)

retrievalWorker
    :: forall ssc ctx m.
       (SscWorkersClass ssc, WorkMode ssc ctx m)
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
    :: forall ssc ctx m.
       (SscWorkersClass ssc, WorkMode ssc ctx m)
    => SendActions m -> m ()
retrievalWorkerImpl SendActions {..} =
    handleAll mainLoopE $ do
        logDebug "Starting retrievalWorker loop"
        mainLoop
  where
    mainLoop = do
        queue        <- view (lensOf @BlockRetrievalQueueTag)
        recHeaderVar <- view (lensOf @RecoveryHeaderTag)
        logDebug "Waiting on the block queue or recovery header var"
        thingToDoNext <- atomically $ do
            mbQueuedHeadersChunk <- tryReadTBQueue queue
            mbRecHeader <- tryReadTMVar recHeaderVar
            case (mbQueuedHeadersChunk, mbRecHeader) of
                (Nothing, Nothing) -> retry
                (Just (nodeId, task), _) ->
                    pure (handleBlockRetrievalFromQueue nodeId task)
                (_, Just (nodeId, rHeader))  ->
                    pure (handleHeadersRecovery nodeId rHeader)
        thingToDoNext
        mainLoop
    mainLoopE e = do
        -- REPORT:ERROR 'reportOrLogE' in block retrieval worker.
        reportOrLogE "retrievalWorker: error caught " e
        delay (30 & sec)
        mainLoop

    -- 2 types of task performed by recovery worker

    handleHeadersRecovery nodeId rHeader = do
        logDebug "Block retrieval queue is empty and we're in recovery mode,\
                 \ so we will request more headers and blocks"
        handleBlockRetrieval nodeId $
            BlockRetrievalTask { brtHeader = rHeader, brtContinues = False }
    handleBlockRetrievalFromQueue nodeId task = do
        logDebug $ sformat
            ("Block retrieval queue task received, nodeId="%build%
             ", header="%build%", continues="%build)
            nodeId
            (headerHash $ brtHeader task)
            (brtContinues task)
        handleBlockRetrieval nodeId task

    -- Tasks dispatcher (with handler)

    handleBlockRetrieval nodeId BlockRetrievalTask{..} =
        handleAll (handleBlockRetrievalE nodeId brtHeader) $
        (if brtContinues then handleContinues else handleAlternative)
            nodeId
            brtHeader
    handleBlockRetrievalE nodeId header e = do
        -- REPORT:ERROR 'reportOrLogW' in block retrieval worker.
        reportOrLogW (sformat
            ("Error handling nodeId="%build%", header="%build%": ")
            nodeId (headerHash header)) e
        dropUpdateHeader
        dropRecoveryHeaderAndRepeat enqueueMsg nodeId

    -- task dispatcher continuation

    handleContinues nodeId header = do
        classificationRes <- classifyNewHeader header
        case classificationRes of
            CHContinues ->
                void $ getProcessBlocks enqueueMsg nodeId header (headerHash header)
            res -> logDebug $
                "processContHeader: expected header to " <>
                 "be continuation, but it's " <> show res
    handleAlternative nodeId header = mkHeadersRequest (headerHash header) >>= \case
        MhrrBlockAdopted ->
            logDebug "Block already adopted, nothing to be done"
        MhrrWithCheckpoints mgh -> do
            logDebug "Checkpoints available, headers request assembled"

            updateRecoveryHeader nodeId header
            let cont (headers :: NewestFirst NE (BlockHeader ssc)) =
                    let oldestHeader = headers ^. _NewestFirst . _neLast
                        newestHeader = headers ^. _NewestFirst . _neHead
                    in getProcessBlocks enqueueMsg
                                        nodeId
                                        oldestHeader
                                        (headerHash newestHeader)
            -- If it returns w/o exception then we're:
            --  * Either still in recovery mode
            --  * Or just exited it and recoverVar was taken.
            enqueueMsgSingle enqueueMsg (MsgRequestBlockHeaders $ Just $ S.singleton nodeId) $
                Conversation $ requestHeaders cont mgh nodeId
            logInfo "Recovery mode exited gracefully"

----------------------------------------------------------------------------
-- Entering and exiting recovery mode
----------------------------------------------------------------------------

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
    :: WorkMode ssc ctx m
    => NodeId
    -> BlockHeader ssc
    -> m ()
updateRecoveryHeader nodeId hdr = do
    recHeaderVar <- view (lensOf @RecoveryHeaderTag)
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

dropUpdateHeader :: WorkMode ssc ctx m => m ()
dropUpdateHeader = do
    progressHeaderVar <- view (lensOf @ProgressHeaderTag)
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
    :: WorkMode ssc ctx m
    => NodeId
    -> m Bool
dropRecoveryHeader nodeId = do
    recHeaderVar <- view (lensOf @RecoveryHeaderTag)
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
    :: (SscWorkersClass ssc, WorkMode ssc ctx m)
    => EnqueueMsg m -> NodeId -> m ()
dropRecoveryHeaderAndRepeat enqueue nodeId = do
    kicked <- dropRecoveryHeader nodeId
    when kicked $ attemptRestartRecovery
  where
    attemptRestartRecovery = do
        logInfo "Attempting to restart recovery"
        delay $ sec 2
        handleAll handleRecoveryTriggerE $ triggerRecovery enqueue
        logDebug "Attempting to restart recovery over"
    handleRecoveryTriggerE =
        -- REPORT:ERROR 'reportOrLogE' somewhere in block retrieval.
        reportOrLogE $ "Exception happened while trying to trigger " <>
                       "recovery inside recoveryWorker: "

----------------------------------------------------------------------------
-- Block request/processing logic
----------------------------------------------------------------------------

-- Returns only if blocks were successfully downloaded and
-- processed. Throws exception if something goes wrong.
getProcessBlocks
    :: forall ssc ctx m.
       (SscWorkersClass ssc, WorkMode ssc ctx m)
    => EnqueueMsg m
    -> NodeId
    -> BlockHeader ssc
    -> HeaderHash
    -> m ()
getProcessBlocks enqueue nodeId lcaChild newestHash = do
    -- The conversation will attempt to retrieve the necessary blocks and apply
    -- them. Each one gives a 'Bool' where 'True' means that a recovery was
    -- completed (depends upon the state of the recovery-mode TMVar).
    enqueueMsgSingle enqueue (MsgRequestBlocks (S.singleton nodeId)) $ Conversation $
      \(conv :: ConversationActions MsgGetBlocks (MsgBlock ssc) m) -> do
        let lcaChildHash = headerHash lcaChild
        logDebug $ sformat ("Requesting blocks from "%shortHashF%" to "%shortHashF)
                           lcaChildHash
                           newestHash
        send conv $ mkBlocksRequest lcaChildHash newestHash
        chainE <- runExceptT (retrieveBlocks conv lcaChild newestHash)
        recHeaderVar <- view (lensOf @RecoveryHeaderTag)
        case chainE of
            Left e -> do
                let msg = sformat ("Error retrieving blocks from "%shortHashF%
                                   " to "%shortHashF%" from peer "%
                                   build%": "%stext)
                                  lcaChildHash newestHash nodeId e
                logWarning msg
                throwM $ DialogUnexpected msg
            Right blocks -> do
                logDebug $ sformat
                    ("Retrieved "%int%" blocks of total size "%builder%": "%listJson)
                    (blocks ^. _OldestFirst . to NE.length)
                    (unitBuilder $ biSize blocks)
                    (map (headerHash . view blockHeader) blocks)
                handleBlocks nodeId blocks enqueue
                dropUpdateHeader
                -- If we've downloaded any block with bigger
                -- difficulty than ncrecoveryheader, we're
                -- gracefully exiting recovery mode.
                let isMoreDifficultThan b x = b ^. difficultyL >= x ^. difficultyL
                atomically $ tryReadTMVar recHeaderVar >>= \case
                    -- We're not in recovery mode? That must be ok.
                    Nothing -> pass
                    -- If we're in recovery mode we should exit it if
                    -- any block is more difficult than one in
                    -- recHeader.
                    Just (_, rHeader) ->
                        when (any (`isMoreDifficultThan` rHeader) blocks) $
                            void $ tryTakeTMVar recHeaderVar


----------------------------------------------------------------------------
-- Block retrieving functions
----------------------------------------------------------------------------

retrieveBlocks
    :: (SscWorkersClass ssc, WorkMode ssc ctx m)
    => ConversationActions MsgGetBlocks (MsgBlock ssc) m
    -> BlockHeader ssc
    -> HeaderHash
    -> ExceptT Text m (OldestFirst NE (Block ssc))
retrieveBlocks conv lcaChild endH = do
    blocks <- retrieveBlocksDo 0 conv (lcaChild ^. prevBlockL) endH
    let b0 = blocks ^. _OldestFirst . _neHead
    if headerHash b0 == headerHash lcaChild
       then pure blocks
       else throwError $ sformat
                ("First block of chain is "%build%
                 " instead of expected "%build)
                (b0 ^. blockHeader) lcaChild

retrieveBlocksDo
    :: (SscWorkersClass ssc, WorkMode ssc ctx m)
    => Int        -- ^ Index of block we're requesting
    -> ConversationActions MsgGetBlocks (MsgBlock ssc) m
    -> HeaderHash -- ^ We're expecting a child of this block
    -> HeaderHash -- ^ Block at which to stop
    -> ExceptT Text m (OldestFirst NE (Block ssc))
retrieveBlocksDo i conv prevH endH = lift (recvLimited conv) >>= \case
    Nothing ->
        throwError $ sformat ("Failed to receive block #"%int) i
    Just (MsgNoBlock t) ->
        throwError $ sformat ("Server failed to return block #"%int%": "%stext) i t
    Just (MsgBlock block) -> do
        let prevH' = block ^. prevBlockL
            curH = headerHash block
        when (prevH' /= prevH) $ do
            throwError $ sformat
                ("Received block #"%int%" with prev hash "%shortHashF%
                 " while "%shortHashF%" was expected: "%build)
                i prevH' prevH (block ^. blockHeader)
        progressHeaderVar <- view (lensOf @ProgressHeaderTag)
        atomically $ do void $ tryTakeTMVar progressHeaderVar
                        putTMVar progressHeaderVar $ block ^. blockHeader
        if curH == endH
        then pure $ one block
        else over _Wrapped (block <|) <$> retrieveBlocksDo (i+1) conv curH endH

----------------------------------------------------------------------------
-- Networking
----------------------------------------------------------------------------

-- | Expects sending message to exactly one node. Receives result or
-- fails if no result was obtained (no nodes available, timeout, etc).
enqueueMsgSingle ::
       (MonadThrow m)
    => (t2 -> (t1 -> t -> NonEmpty x) -> m (Map NodeId (m b)))
    -> t2
    -> x
    -> m b
enqueueMsgSingle enqueue msg conv = do
    results <- enqueue msg (\_ _ -> one conv) >>=
               waitForConversations
    case toList results of
        [] ->      throwM $ DialogUnexpected $
            "enqueueMsgSingle: contacted no peers"
        (_:_:_) -> throwM $ DialogUnexpected $
            "enqueueMsgSingle: contacted more than one peers, probably internal error"
        [x] -> pure x
