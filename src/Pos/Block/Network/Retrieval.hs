{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Retrieval
       ( retrievalWorker
       , triggerRecovery
       , handleUnsolicitedHeaders
       , requestTip
       , addToBlockRequestQueue
       , mkHeadersRequest
       , requestTipOuts
       , needRecovery
       ) where

import           Control.Concurrent.STM     (isEmptyTBQueue, isFullTBQueue, putTMVar,
                                             readTVar, tryReadTBQueue, tryReadTMVar,
                                             tryTakeTMVar, writeTBQueue, writeTVar)
import           Control.Lens               (_Wrapped)
import           Control.Monad.Except       (ExceptT, runExceptT, throwError)
import           Data.List.NonEmpty         ((<|))
import qualified Data.List.NonEmpty         as NE
import           Data.Reflection            (reify)
import           Formatting                 (build, int, sformat, shown, stext, (%))
import           Mockable                   (fork, handleAll, throw)
import           Serokell.Util.Text         (listJson)
import           Serokell.Util.Verify       (isVerSuccess)
import           System.Wlog                (logDebug, logError, logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Block.Logic            (ClassifyHeaderRes (..),
                                             ClassifyHeadersRes (..), classifyHeaders,
                                             classifyNewHeader, getHeadersOlderExp,
                                             lcaWithMainChain, verifyAndApplyBlocks,
                                             withBlkSemaphore)
import qualified Pos.Block.Logic            as L
import           Pos.Block.Network.Announce (announceBlock, announceBlockOuts)
import           Pos.Block.Network.Types    (MsgBlock (..), MsgGetBlocks (..),
                                             MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Block.Types            (Blund)
import           Pos.Communication.Protocol (ConversationActions (..), NodeId, OutSpecs,
                                             SendActions (..), WorkerSpec, convH,
                                             toOutSpecs, worker)
import           Pos.Constants              (blkSecurityParam)
import           Pos.Context                (NodeContext (..), getNodeContext,
                                             isRecoveryMode)
import           Pos.Crypto                 (shortHashF)
import qualified Pos.DB                     as DB
import qualified Pos.DB.GState              as GState
import           Pos.DHT.Model              (converseToNeighbors)
import           Pos.Reporting.Methods      (reportMisbehaviourMasked, reportingFatal)
import           Pos.Slotting               (getCurrentSlot)
import           Pos.Ssc.Class              (Ssc, SscWorkersClass)
import           Pos.Types                  (Block, BlockHeader, HasHeaderHash (..),
                                             HeaderHash, blockHeader, difficultyL,
                                             gbHeader, getEpochOrSlot, prevBlockL,
                                             verifyHeaders)
import           Pos.Util                   (NE, NewestFirst (..), OldestFirst (..),
                                             inAssertMode, _neHead, _neLast)
import           Pos.Util.Shutdown          (ifNotShutdown)
import           Pos.WorkMode               (WorkMode)

data VerifyBlocksException = VerifyBlocksException Text deriving Show
instance Exception VerifyBlocksException

retrievalWorker
    :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => (WorkerSpec m, OutSpecs)
retrievalWorker = worker outs retrievalWorkerImpl
  where
    outs = announceBlockOuts <>
           toOutSpecs [convH (Proxy :: Proxy MsgGetBlocks)
                             (Proxy :: Proxy (MsgBlock s0 ssc))]

needRecovery :: WorkMode ssc m => m Bool
needRecovery = maybe (pure True) onSlotKnown =<< getCurrentSlot
  where
    onSlotKnown slot = do
        header <- DB.getTipBlockHeader
        return $ toEnum (fromEnum (getEpochOrSlot header) + blkSecurityParam) < getEpochOrSlot slot

retrievalWorkerImpl
    :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => SendActions m -> m ()
retrievalWorkerImpl sendActions = handleAll handleTop $ do
    logDebug "Starting retrievalWorker loop"
    NodeContext{..} <- getNodeContext
    mainLoop ncBlockRetrievalQueue ncRecoveryHeader
  where
    mainLoop queue recHeaderVar = ifNotShutdown $ reportingFatal $ do
        inRecovery <- needRecovery
        when (not inRecovery) $
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
        mainLoop queue recHeaderVar
      where
        onLeft ph =
            handleAll (handleBlockRetrievalE recHeaderVar ph) $
            reportingFatal $ handle ph
        onRight (peerId, rHeader) = do
            logDebug "Queue is empty, we're in recovery mode -> querying more"
            whenJustM (mkHeadersRequest (Just $ headerHash rHeader)) $ \mghNext ->
                handleAll (handleHeadersRecoveryE recHeaderVar peerId) $
                reportingFatal $
                withConnectionTo sendActions peerId $ \_peerData ->
                    requestHeaders mghNext peerId rHeader
            dropRecoveryHeaderAndRepeat recHeaderVar peerId
        handleBlockRetrievalE recHeaderVar (peerId, headers) e = do
            logWarning $ sformat
                ("Error handling peerId="%build%", headers="%listJson%": "%shown)
                peerId (fmap headerHash headers) e
            dropUpdateHeader
            dropRecoveryHeaderAndRepeat recHeaderVar peerId
        handleHeadersRecoveryE recHeaderVar peerId e = do
            logWarning $ sformat
                ("Failed while trying to get more headers "%
                 "for recovery from peerId="%build%", error: "%shown)
                peerId e
            dropUpdateHeader

    dropUpdateHeader = do
        progressHeaderVar <- ncProgressHeader <$> getNodeContext
        void $ atomically $ tryTakeTMVar progressHeaderVar
    dropRecoveryHeader recHeaderVar peerId = do
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
    dropRecoveryHeaderAndRepeat recHeaderVar peerId = do
        kicked <- dropRecoveryHeader recHeaderVar peerId
        when kicked $ attemptRestartRecovery

    attemptRestartRecovery = do
        logDebug "Attempting to restart recovery"
        handleAll handleRecoveryTrigger $ triggerRecovery sendActions
        logDebug "Attempting to restart recovery over"

    handleTop e = do
        logError $ sformat ("retrievalWorker: error caught "%shown) e
        throw e
    handleRecoveryTrigger e =
        logError $ "Exception happened while trying to trigger " <>
                   "recovery inside recoveryWorker: " <> show e

    handle (peerId, headers) = do
        logDebug $ sformat
            ("retrievalWorker: handling peerId="%build%", headers="%listJson)
            peerId (fmap headerHash headers)
        classificationRes <- classifyHeaders' headers
        let newestHeader = headers ^. _Wrapped . _neHead
            newestHash = headerHash newestHeader
            oldestHash = headerHash $ headers ^. _Wrapped . _neLast
        case classificationRes of
            CHsValid lcaChild ->
                void $ handleCHsValid peerId lcaChild newestHash
            CHsUseless reason ->
                logDebug $ sformat uselessFormat oldestHash newestHash reason
            CHsInvalid reason ->
                logWarning $ sformat invalidFormat oldestHash newestHash reason
    classifyHeaders' (NewestFirst (header :| [])) = do
        classificationRes <- classifyNewHeader header
        -- TODO: should we set 'To' hash to hash of header or leave it unlimited?
        case classificationRes of
            CHContinues -> pure $ CHsValid header
            CHAlternative ->
                pure $ CHsInvalid "Expected header to be continuation, not alternative"
            CHUseless reason -> pure $ CHsUseless reason
            CHInvalid reason -> pure $ CHsInvalid reason
    classifyHeaders' headers = classifyHeaders headers
    validFormat =
        "Requesting blocks from " %shortHashF % " to " %shortHashF
    invalidFormat =
        "Chain of headers from " %shortHashF % " to " %shortHashF %
        " is considered invalid: " %stext
    uselessFormat =
        "Chain of headers from " %shortHashF % " to " %shortHashF %
        " is useless for the following reason: " %stext
    handleCHsValid peerId lcaChild newestHash = do
        let lcaChildHash = headerHash lcaChild
        logDebug $ sformat validFormat lcaChildHash newestHash
        maxBlockSize <- GState.getMaxBlockSize
        -- Yay, reflection magic! Here we indirectly pass 'maxBlockSize' as
        -- a parameter to the 'Bi' instance of 'MsgBlock'.
        reify maxBlockSize $ \(_ :: Proxy s0) ->
          withConnectionTo sendActions peerId $
          \_peerData (conv :: ConversationActions MsgGetBlocks (MsgBlock s0 ssc) m) -> do
            send conv $ mkBlocksRequest lcaChildHash newestHash
            chainE <- runExceptT (retrieveBlocks conv lcaChild newestHash)
            recHeaderVar <- ncRecoveryHeader <$> getNodeContext
            case chainE of
                Left e -> do
                    logWarning $ sformat
                        ("Error retrieving blocks from "%shortHashF%
                         " to "%shortHashF%" from peer "%build%": "%stext)
                        lcaChildHash newestHash peerId e
                    dropRecoveryHeaderAndRepeat recHeaderVar peerId
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
    retrieveBlocks conv lcaChild endH = do
        blocks <- retrieveBlocks' 0 conv (lcaChild ^. prevBlockL) endH
        let b0 = blocks ^. _Wrapped . _neHead
        if headerHash b0 == headerHash lcaChild
           then return blocks
           else throwError $ sformat
                    ("First block of chain is "%build%
                     " instead of expected "%build)
                    (b0 ^. blockHeader) lcaChild
    retrieveBlocks'
        :: forall s.
           Int
        -> ConversationActions MsgGetBlocks (MsgBlock s ssc) m
        -> HeaderHash          -- ^ We're expecting a child of this block
        -> HeaderHash          -- ^ Block at which to stop
        -> ExceptT Text m (OldestFirst NE (Block ssc))
    retrieveBlocks' i conv prevH endH = lift (recv conv) >>= \case
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

-- | Triggers recovery based on established communication.
triggerRecovery :: WorkMode ssc m => SendActions m -> m ()
triggerRecovery sendActions = unlessM isRecoveryMode $ do
    logDebug "Recovery triggered, requesting tips"
    converseToNeighbors sendActions requestTip `catch`
        (\(e :: SomeException) ->
           logDebug ("Error happened in triggerRecovery" <> show e) >> throwM e)
    logDebug "Recovery triggered ended"

-- | Make 'GetHeaders' message using our main chain. This function
-- chooses appropriate 'from' hashes and puts them into 'GetHeaders'
-- message.
mkHeadersRequest
    :: WorkMode ssc m
    => Maybe HeaderHash -> m (Maybe MsgGetHeaders)
mkHeadersRequest upto = do
    mbHeaders <- nonEmpty . toList <$> getHeadersOlderExp Nothing
    pure $ (\h -> MsgGetHeaders (NE.toList h) upto) <$> mbHeaders

-- Second case of 'handleBlockheaders'
handleUnsolicitedHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => NonEmpty (BlockHeader ssc)
    -> NodeId
    -> ConversationActions MsgGetHeaders (MsgHeaders ssc) m
    -> m ()
handleUnsolicitedHeaders (header :| []) peerId conv =
    handleUnsolicitedHeader header peerId conv
-- TODO: ban node for sending more than one unsolicited header.
handleUnsolicitedHeaders (h:|hs) _ _ = do
    logWarning "Someone sent us nonzero amount of headers we didn't expect"
    logWarning $ sformat ("Here they are: "%listJson) (h:hs)

handleUnsolicitedHeader
    :: forall ssc m.
       (WorkMode ssc m)
    => BlockHeader ssc
    -> NodeId
    -> ConversationActions MsgGetHeaders (MsgHeaders ssc) m
    -> m ()
handleUnsolicitedHeader header peerId conv = do
    logDebug $ sformat
        ("handleUnsolicitedHeader: single header "%shortHashF%
         " was propagated, processing")
        hHash
    classificationRes <- classifyNewHeader header
    -- TODO: should we set 'To' hash to hash of header or leave it unlimited?
    case classificationRes of
        CHContinues -> do
            logDebug $ sformat continuesFormat hHash
            addToBlockRequestQueue (one header) peerId header
        CHAlternative -> do
            logInfo $ sformat alternativeFormat hHash
            mghM <- mkHeadersRequest (Just hHash)
            whenJust mghM $ \mgh ->
                requestHeaders mgh peerId header conv
        CHUseless reason -> logDebug $ sformat uselessFormat hHash reason
        CHInvalid _ -> do
            logDebug $ sformat ("handleUnsolicited: header "%shortHashF%
                                " is invalid") hHash
            pass -- TODO: ban node for sending invalid block.
  where
    hHash = headerHash header
    continuesFormat =
        "Header " %shortHashF %
        " is a good continuation of our chain, requesting it"
    alternativeFormat =
        "Header " %shortHashF %
        " potentially represents good alternative chain, requesting more headers"
    uselessFormat =
        "Header " %shortHashF % " is useless for the following reason: " %stext


-- | Result of 'matchHeadersRequest'
data MatchReqHeadersRes
    = MRGood
      -- ^ Headers were indeed requested and precisely match our
      -- request
    | MRUnexpected
      -- ^ Headers don't represent valid response to our request.
    deriving (Show)

matchRequestedHeaders
    :: (Ssc ssc)
    => NewestFirst NE (BlockHeader ssc) -> MsgGetHeaders -> Bool -> MatchReqHeadersRes
matchRequestedHeaders headers MsgGetHeaders{..} inRecovery =
    let newTip      = headers ^. _Wrapped . _neHead
        startHeader = headers ^. _Wrapped . _neLast
        startMatches =
            or [ (startHeader ^. headerHashG) `elem` mghFrom
               , (startHeader ^. prevBlockL) `elem` mghFrom]
        mghToMatches
            | inRecovery = True
            | isNothing mghTo = True
            | otherwise = Just (headerHash newTip) == mghTo
        boolMatch = and [startMatches, mghToMatches, formChain]
     in if boolMatch
           then MRGood
           else MRUnexpected
  where
    formChain = isVerSuccess $
        verifyHeaders True (headers & _Wrapped %~ toList)

requestTipOuts :: OutSpecs
requestTipOuts =
    toOutSpecs [ convH (Proxy :: Proxy MsgGetHeaders)
                       (Proxy :: Proxy (MsgHeaders ssc)) ]

-- Is used if we're recovering after offline and want to know what's
-- current blockchain state.
requestTip
    :: forall ssc m.
       (WorkMode ssc m)
    => NodeId -> ConversationActions MsgGetHeaders (MsgHeaders ssc) m -> m ()
requestTip peerId conv = do
    logDebug "Requesting tip..."
    send conv (MsgGetHeaders [] Nothing)
    whenJustM (recv conv) handleTip
  where
    handleTip (MsgHeaders (NewestFirst (tip:|[]))) = do
        logDebug $ sformat ("Got tip "%shortHashF%", processing") (headerHash tip)
        handleUnsolicitedHeader tip peerId conv
    handleTip _ = pass

-- Second argument is mghTo block header (not hash). Don't pass it
-- only if you don't know it.
requestHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => MsgGetHeaders
    -> NodeId
    -> BlockHeader ssc
    -> ConversationActions MsgGetHeaders (MsgHeaders ssc) m
    -> m ()
requestHeaders mgh peerId origTip conv = do
    logDebug $ sformat ("requestHeaders: withConnection: sending "%build) mgh
    send conv mgh
    mHeaders <- recv conv
    inRecovery <- needRecovery
    logDebug $ sformat ("requestHeaders: inRecovery = "%shown) inRecovery
    flip (maybe onNothing) mHeaders $ \(MsgHeaders headers) -> do
        logDebug $ sformat
            ("requestHeaders: withConnection: received "%listJson)
            (map headerHash headers)
        case matchRequestedHeaders headers mgh inRecovery of
            MRGood       -> do
                handleRequestedHeaders headers peerId origTip
            MRUnexpected -> handleUnexpected headers peerId
  where
    onNothing = logWarning "requestHeaders: received Nothing, waiting for MsgHeaders"
    handleUnexpected hs _ = do
        -- TODO: ban node for sending unsolicited header in conversation
        logWarning $ sformat
            ("requestHeaders: headers received were not requested, address: " % build)
            peerId
        logWarning $ sformat ("requestHeaders: unexpected headers: "%listJson) hs


-- First case of 'handleBlockheaders'
handleRequestedHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => NewestFirst NE (BlockHeader ssc)
    -> NodeId
    -> BlockHeader ssc
    -> m ()
handleRequestedHeaders headers peerId origTip = do
    logDebug "handleRequestedHeaders: headers were requested, will process"
    classificationRes <- classifyHeaders headers
    let newestHeader = headers ^. _Wrapped . _neHead
        newestHash = headerHash newestHeader
        oldestHash = headerHash $ headers ^. _Wrapped . _neLast
    case classificationRes of
        CHsValid lcaChild -> do
            let lcaHash = lcaChild ^. prevBlockL
            let headers' = NE.takeWhile ((/= lcaHash) . headerHash)
                                        (getNewestFirst headers)
            logDebug $ sformat validFormat (headerHash lcaChild)newestHash
            case NE.nonEmpty headers' of
                Nothing -> logWarning $
                    "handleRequestedHeaders: couldn't find LCA child " <>
                    "within headers returned, most probably classifyHeaders is broken"
                Just headersPostfix ->
                    addToBlockRequestQueue (NewestFirst headersPostfix) peerId origTip
        CHsUseless reason ->
            logDebug $ sformat uselessFormat oldestHash newestHash reason
        CHsInvalid reason ->
             -- TODO: ban node for sending invalid block.
            logDebug $ sformat invalidFormat oldestHash newestHash reason
  where
    validFormat =
        "Received valid headers, can request blocks from " %shortHashF % " to " %shortHashF
    genericFormat what =
        "Chain of headers from " %shortHashF % " to " %shortHashF %
        " is "%what%" for the following reason: " %stext
    uselessFormat = genericFormat "useless"
    invalidFormat = genericFormat "invalid"

-- | Given nonempty list of valid blockheaders and nodeid, this
-- function will put them into download queue and they will be
-- processed later. Second argument is optional recovery mode tip --
-- after pack of blocks is processed, next pack of headers will be
-- requested until this header hash is received.
addToBlockRequestQueue
    :: forall ssc m.
       (WorkMode ssc m)
    => NewestFirst NE (BlockHeader ssc)
    -> NodeId
    -> BlockHeader ssc
    -> m ()
addToBlockRequestQueue headers peerId origTip = do
    queue <- ncBlockRetrievalQueue <$> getNodeContext
    recHeaderVar <- ncRecoveryHeader <$> getNodeContext
    lastKnownH <- ncLastKnownHeader <$> getNodeContext
    atomically $ do
        oldV <- readTVar lastKnownH
        when (maybe True (origTip `isMoreDifficult`) oldV) $
            writeTVar lastKnownH (Just origTip)
    atomically $ do
        let replace = tryTakeTMVar recHeaderVar >>= \case
                Just (_, header')
                    | not (origTip `isMoreDifficult` header') -> pass
                _ -> putTMVar recHeaderVar (peerId, origTip)
        tryReadTMVar recHeaderVar >>= \case
            Nothing -> replace
            Just (_,curRecHeader) ->
                when (origTip `isMoreDifficult` curRecHeader) replace
    added <- atomically $ do
        ifM (isFullTBQueue queue)
            (pure False)
            (True <$ writeTBQueue queue (peerId, headers))
    if added
    then logDebug $ sformat ("Added to block request queue: peerId="%build%
                             ", headers="%listJson)
                            peerId (fmap headerHash headers)
    else logWarning $ sformat ("Failed to add headers from "%build%
                               " to block retrieval queue: queue is full")
                              peerId
  where
    a `isMoreDifficult` b = a ^. difficultyL > b ^. difficultyL

-- | Make message which requests chain of blocks which is based on our
-- tip. LcaChild is the first block after LCA we don't
-- know. WantedBlock is the newest one we want to get.
mkBlocksRequest :: HeaderHash -> HeaderHash -> MsgGetBlocks
mkBlocksRequest lcaChild wantedBlock =
    MsgGetBlocks
    { mgbFrom = lcaChild
    , mgbTo = wantedBlock
    }

handleBlocks
    :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => NodeId
    -> OldestFirst NE (Block ssc)
    -> SendActions m
    -> m ()
handleBlocks peerId blocks sendActions = do
    logDebug "handleBlocks: processing"
    inAssertMode $
        logInfo $
            sformat ("Processing sequence of blocks: " %listJson % "...") $
                    fmap headerHash blocks
    maybe onNoLca (handleBlocksWithLca peerId sendActions blocks) =<<
        lcaWithMainChain (map (view blockHeader) blocks)
    inAssertMode $ logDebug $ "Finished processing sequence of blocks"
  where
    onNoLca = logWarning $
        "Sequence of blocks can't be processed, because there is no LCA. " <>
        "Probably rollback happened in parallel"

handleBlocksWithLca
    :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => NodeId
    -> SendActions m
    -> OldestFirst NE (Block ssc)
    -> HeaderHash
    -> m ()
handleBlocksWithLca peerId sendActions blocks lcaHash = do
    logDebug $ sformat lcaFmt lcaHash
    -- Head blund in result is the youngest one.
    toRollback <- DB.loadBlundsFromTipWhile $ \blk -> headerHash blk /= lcaHash
    maybe (applyWithoutRollback sendActions blocks)
          (applyWithRollback peerId sendActions blocks lcaHash)
          (_Wrapped nonEmpty toRollback)
  where
    lcaFmt = "Handling block w/ LCA, which is "%shortHashF

applyWithoutRollback
    :: forall ssc m.
       (WorkMode ssc m, SscWorkersClass ssc)
    => SendActions m -> OldestFirst NE (Block ssc) -> m ()
applyWithoutRollback sendActions blocks = do
    logInfo $ sformat ("Trying to apply blocks w/o rollback: "%listJson) $
        fmap (view blockHeader) blocks
    withBlkSemaphore applyWithoutRollbackDo >>= \case
        Left err     ->
            onFailedVerifyBlocks (getOldestFirst blocks) err
        Right newTip -> do
            when (newTip /= newestTip) $
                logWarning $ sformat
                    ("Only blocks up to "%shortHashF%" were applied, "%
                     "newer were considered invalid")
                    newTip
            let toRelay =
                    fromMaybe (panic "Listeners#applyWithoutRollback is broken") $
                    find (\b -> headerHash b == newTip) blocks
                prefix = blocks
                    & _Wrapped %~ NE.takeWhile ((/= newTip) . headerHash)
                    & map (view blockHeader)
                applied = NE.fromList $
                    getOldestFirst prefix <> one (toRelay ^. blockHeader)
            relayBlock sendActions toRelay
            logInfo $ blocksAppliedMsg applied
  where
    newestTip = blocks ^. _Wrapped . _neLast . headerHashG
    applyWithoutRollbackDo
        :: HeaderHash -> m (Either Text HeaderHash, HeaderHash)
    applyWithoutRollbackDo curTip = do
        res <- verifyAndApplyBlocks False blocks
        let newTip = either (const curTip) identity res
        pure (res, newTip)

applyWithRollback
    :: forall ssc m.
       (WorkMode ssc m, SscWorkersClass ssc)
    => NodeId
    -> SendActions m
    -> OldestFirst NE (Block ssc)
    -> HeaderHash
    -> NewestFirst NE (Blund ssc)
    -> m ()
applyWithRollback peerId sendActions toApply lca toRollback = do
    logInfo $ sformat ("Trying to apply blocks w/ rollback: "%listJson)
        (map (view blockHeader) toApply)
    logInfo $ sformat ("Blocks to rollback "%listJson) toRollbackHashes
    res <- withBlkSemaphore $ \curTip -> do
        res <- L.applyWithRollback toRollback toApplyAfterLca
        pure (res, either (const curTip) identity res)
    case res of
        Left err -> logWarning $ "Couldn't apply blocks with rollback: " <> err
        Right newTip -> do
            logDebug $ sformat
                ("Finished applying blocks w/ rollback, relaying new tip: "%shortHashF)
                newTip
            reportRollback
            logInfo $ blocksRolledBackMsg (getNewestFirst toRollback)
            logInfo $ blocksAppliedMsg (getOldestFirst toApply)
            relayBlock sendActions $ toApply ^. _Wrapped . _neLast
  where
    toRollbackHashes = fmap headerHash toRollback
    toApplyHashes = fmap headerHash toApply
    reportF =
        "Fork happened, data received from "%build%
        ". Blocks rolled back: "%listJson%
        ", blocks applied: "%listJson
    reportRollback =
        unlessM isRecoveryMode $ do
            logDebug "Reporting rollback happened"
            reportMisbehaviourMasked $
                sformat reportF peerId toRollbackHashes toApplyHashes
    panicBrokenLca = panic "applyWithRollback: nothing after LCA :<"
    toApplyAfterLca =
        OldestFirst $
        fromMaybe panicBrokenLca $ nonEmpty $
        NE.dropWhile ((lca /=) . (^. prevBlockL)) $
        getOldestFirst $ toApply

relayBlock
    :: forall ssc m.
       (WorkMode ssc m)
    => SendActions m -> Block ssc -> m ()
relayBlock _ (Left _)                  = pass
relayBlock sendActions (Right mainBlk) = do
    -- Why 'ncPropagation' is not considered?
    unlessM isRecoveryMode $
        void $ fork $ announceBlock sendActions $ mainBlk ^. gbHeader

----------------------------------------------------------------------------
-- Logging formats
----------------------------------------------------------------------------

-- TODO: ban node for it!
onFailedVerifyBlocks
    :: forall ssc m.
       (WorkMode ssc m)
    => NonEmpty (Block ssc) -> Text -> m ()
onFailedVerifyBlocks blocks err = do
    logWarning $ sformat ("Failed to verify blocks: "%stext%"\n  blocks = "%listJson)
        err (fmap headerHash blocks)
    throwM $ VerifyBlocksException err

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
