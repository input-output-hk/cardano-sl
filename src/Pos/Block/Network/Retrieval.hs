{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Retrieval
       ( retrievalWorker
       , requestHeaders
       , addToBlockRequestQueue
       , mkHeadersRequest
       ) where

import           Control.Concurrent.STM     (isEmptyTBQueue, isFullTBQueue, putTMVar,
                                             readTBQueue, tryReadTMVar, tryTakeTMVar,
                                             writeTBQueue)
import           Control.Lens               (_Wrapped)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.List.NonEmpty         ((<|))
import qualified Data.List.NonEmpty         as NE
import           Formatting                 (build, int, sformat, shown, stext, (%))
import           Mockable                   (fork, handleAll, throw)
import           Node                       (ConversationActions (..), NodeId,
                                             SendActions (..))
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
import           Pos.Block.Network.Announce (announceBlock)
import           Pos.Block.Network.Types    (MsgBlock (..), MsgGetBlocks (..),
                                             MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Block.Types            (Blund)
import           Pos.Communication.BiP      (BiP (..))
import           Pos.Constants              (blkSecurityParam)
import           Pos.Context                (NodeContext (..), getNodeContext)
import           Pos.Crypto                 (hash, shortHashF)
import qualified Pos.DB                     as DB
import           Pos.DHT.Model              (nodeIdToAddress)
import           Pos.Ssc.Class              (Ssc, SscWorkersClass)
import           Pos.Types                  (Block, BlockHeader, HasHeaderHash (..),
                                             HeaderHash, blockHeader, difficultyL,
                                             gbHeader, prevBlockL, verifyHeaders)
import           Pos.Util                   (NE, NewestFirst (..), OldestFirst (..),
                                             inAssertMode, toNewestFirst, _neHead,
                                             _neLast)
import           Pos.WorkMode               (WorkMode)

retrievalWorker :: (SscWorkersClass ssc, WorkMode ssc m) => SendActions BiP m -> m ()
retrievalWorker sendActions = handleAll handleWE $ do
    NodeContext{..} <- getNodeContext
    loop ncBlockRetrievalQueue ncRecoveryHeader
  where
    handleWE e = do
        logError $ sformat ("retrievalWorker: error caught "%shown) e
        throw e
    loop queue recHeaderVar = forever $ do
       ph <- atomically $ over _2 NewestFirst <$> readTBQueue queue
       handleAll (handleLE ph) $ handle ph
       needQueryMore <- atomically $ do
           isEmpty <- isEmptyTBQueue queue
           recHeader <- tryReadTMVar recHeaderVar
           pure $ guard isEmpty *> recHeader
       whenJust needQueryMore $ \(peerId, rHeader) -> do
           logDebug "Queue is empty, we're in recovery mode -> querying more"
           whenJustM (mkHeadersRequest (Just $ headerHash rHeader)) $ \mghNext ->
               withConnectionTo sendActions peerId $
                   requestHeaders mghNext (Just rHeader) peerId
    handleLE (peerId, headers) e =
        logWarning $ sformat
            ("Error handling peerId="%shown%", headers="%listJson%": "%shown)
            peerId headers e
    handle (peerId, headers) = do
        logDebug $ sformat
            ("retrievalWorker: handling peerId="%shown%", headers="%listJson)
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
        withConnectionTo sendActions peerId $ \conv -> do
            send conv $ mkBlocksRequest lcaChildHash newestHash
            chainE <- runExceptT (retrieveBlocks conv lcaChild newestHash)
            case chainE of
                Left e ->
                    logWarning $ sformat
                        ("Error retrieving blocks from "%shortHashF%" to"%
                                      shortHashF%" from peer "%shown%": "%stext)
                        lcaChildHash newestHash peerId e
                Right blocks -> do
                    logDebug $ sformat ("retrievalWorker: retrieved blocks "%listJson)
                        $ map (headerHash . view blockHeader) $ toList blocks
                    handleBlocks blocks sendActions
                    -- If we've downloaded block that has header
                    -- ncRecoveryHeader, we're not in recovery mode
                    -- anymore.
                    recHeaderVar <- ncRecoveryHeader <$> getNodeContext
                    atomically $ whenJustM (tryReadTMVar recHeaderVar) $ \(_,rHeader) ->
                        when (headerHash rHeader `elem` map headerHash blocks) $
                            void $ tryTakeTMVar recHeaderVar
    retrieveBlocks conv lcaChild endH = do
        blocks <- retrieveBlocks' 0 conv (lcaChild ^. prevBlockL) endH
        let b0 = blocks ^. _Wrapped . _neHead
        if headerHash b0 == headerHash lcaChild
           then return blocks
           else throwE $ sformat
                    ("First block of chain "%build%
                     " instead of expected "%build)
                    (b0 ^. blockHeader) lcaChild
    retrieveBlocks'
        :: WorkMode ssc m
        => Int
        -> ConversationActions MsgGetBlocks (MsgBlock ssc) m
        -> HeaderHash          -- ^ We're expecting a child of this block
        -> HeaderHash          -- ^ Block at which to stop
        -> ExceptT Text m (OldestFirst NE (Block ssc))
    retrieveBlocks' i conv prevH endH = do
        mBlock <- lift $ recv conv
        case mBlock of
            Nothing -> throwE $ sformat ("Failed to receive block #"%int) i
            Just (MsgBlock block) -> do
                let prevH' = block ^. prevBlockL
                    curH = headerHash block
                when (prevH' /= prevH) $
                    throwE $ sformat
                        ("Received block #"%int%" with prev hash "%shortHashF
                              %" while "%shortHashF%" expected: "%build)
                        i prevH' prevH (block ^. blockHeader)
                if curH == endH
                  then return $ one block
                  else over _Wrapped (block <|) <$>
                       retrieveBlocks' (i+1) conv curH endH

-- | Make 'GetHeaders' message using our main chain. This function
-- chooses appropriate 'from' hashes and puts them into 'GetHeaders'
-- message.
mkHeadersRequest
    :: WorkMode ssc m
    => Maybe HeaderHash -> m (Maybe MsgGetHeaders)
mkHeadersRequest upto = do
    mbHeaders <- nonEmpty . toList <$> getHeadersOlderExp Nothing
    pure $ (\h -> MsgGetHeaders h upto) <$> mbHeaders

-- | Result of 'matchHeadersRequest'
data MatchReqHeadersRes
    = MRGood
      -- ^ Headers were indeed requested and precisely match our
      -- request
    | MRUnexpected
      -- ^ Headers don't represent valid response to our request.
    | MRRecovery
      -- ^ Headers are valid, but chain length is too big and last
      -- element doesn't match expected 'mghTo', so we're in "after
      -- offline ~ recovery" mode.
    deriving (Show)

matchRequestedHeaders
    :: (Ssc ssc)
    => NewestFirst NE (BlockHeader ssc) -> MsgGetHeaders -> MatchReqHeadersRes
matchRequestedHeaders headers MsgGetHeaders{..} =
    let newTip      = headers ^. _Wrapped . _neHead
        startHeader = headers ^. _Wrapped . _neLast
        startMatches =
            or [ (startHeader ^. headerHashG) `elem` mghFrom
               , (startHeader ^. prevBlockL) `elem` mghFrom]
        recoveryMode = length headers > blkSecurityParam
        mghToMatches
            | recoveryMode = True
            | isNothing mghTo = True
            | otherwise =  Just (headerHash newTip) == mghTo
        boolMatch = and [startMatches, mghToMatches, formChain]
     in case (boolMatch, recoveryMode) of
        (True, False) -> MRGood
        (True, True)  -> MRRecovery
        (False, _)    -> MRUnexpected
  where
    formChain = isVerSuccess $
        verifyHeaders True (headers & _Wrapped %~ toList)

-- Second argument is mghTo block header (not hash). Don't pass it
-- only if you don't know it.
requestHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => MsgGetHeaders
    -> Maybe (BlockHeader ssc)
    -> NodeId
    -> ConversationActions MsgGetHeaders (MsgHeaders ssc) m
    -> m ()
requestHeaders mgh recoveryHeader peerId conv = do
    logDebug $ sformat ("handleUnsolicitedHeader: withConnection: sending "%shown) mgh
    send conv mgh
    mHeaders <- recv conv
    whenJust mHeaders $ \(MsgHeaders headers) -> do
        logDebug $ sformat
            ("handleUnsolicitedHeader: withConnection: received "%listJson)
            (map headerHash headers)
        case matchRequestedHeaders headers mgh of
            MRGood       -> do
                handleRequestedHeaders headers Nothing peerId
            MRUnexpected -> handleUnexpected headers peerId
            MRRecovery   -> do
                logDebug "Handling header requests in recovery mode"
                handleRequestedHeaders headers recoveryHeader peerId
  where
    handleUnexpected hs _ = do
        -- TODO: ban node for sending unsolicited header in conversation
        logWarning $ sformat
            ("handleUnsolicitedHeader: headers received were not requested, address: " % shown)
            (nodeIdToAddress peerId)
        logWarning $ sformat ("handleUnsolicitedHeader: unexpected headers: "%listJson) hs

-- First case of 'handleBlockheaders'
handleRequestedHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => NewestFirst NE (BlockHeader ssc)
    -> Maybe (BlockHeader ssc)
    -> NodeId
    -> m ()
handleRequestedHeaders headers recoveryTip peerId = do
    logDebug "handleRequestedHeaders: headers were requested, will process"
    classificationRes <- classifyHeaders headers
    let newestHeader = headers ^. _Wrapped . _neHead
        newestHash = headerHash newestHeader
        oldestHash = headerHash $ headers ^. _Wrapped . _neLast
    case classificationRes of
        CHsValid lcaChild -> do
            let lcaChildHash = hash lcaChild
            logDebug $ sformat validFormat lcaChildHash newestHash
            addToBlockRequestQueue (getNewestFirst headers) recoveryTip peerId
        CHsUseless reason ->
            logDebug $ sformat uselessFormat oldestHash newestHash reason
        CHsInvalid _ -> pass -- TODO: ban node for sending invalid block.
  where
    validFormat =
        "Received valid headers, can request blocks from " %shortHashF % " to " %shortHashF
    uselessFormat =
        "Chain of headers from " %shortHashF % " to " %shortHashF %
        " is useless for the following reason: " %stext

-- | Given nonempty list of valid blockheaders and nodeid, this
-- function will put them into download queue and they will be
-- processed later. Second argument is optional recovery mode tip --
-- after pack of blocks is processed, next pack of headers will be
-- requested until this header hash is received.
addToBlockRequestQueue
    :: forall ssc m.
       (WorkMode ssc m)
    => NonEmpty (BlockHeader ssc)
    -> Maybe (BlockHeader ssc)
    -> NodeId
    -> m ()
addToBlockRequestQueue headers recoveryTip peerId = do
    queue <- ncBlockRetrievalQueue <$> getNodeContext
    recHeaderVar <- ncRecoveryHeader <$> getNodeContext
    let updateQueue = True <$ writeTBQueue queue (peerId, headers)
        updateRecoveryHeader (Just recTip) = do
            let replace = do void $ tryTakeTMVar recHeaderVar
                             putTMVar recHeaderVar (peerId, recTip)
            tryReadTMVar recHeaderVar >>= \case
                Nothing -> replace
                Just (_,curRecHeader) ->
                    when (((>) `on` view difficultyL) recTip curRecHeader) replace
        updateRecoveryHeader _ = pass
    added <- atomically $ do
        updateRecoveryHeader recoveryTip
        ifM (isFullTBQueue queue) (pure False) updateQueue
    if added
    then logDebug $ sformat ("Added to block request queue: peerId="%shown%
                             ", headers="%listJson)
                            peerId (fmap headerHash headers)
    else logWarning $ sformat ("Failed to add headers from "%shown%
                               " to block retrieval queue: queue is full")
                              peerId

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
    => OldestFirst NE (Block ssc)
    -> SendActions BiP m
    -> m ()
handleBlocks blocks sendActions = do
    logDebug "handleBlocks: processing"
    inAssertMode $
        logInfo $
            sformat ("Processing sequence of blocks: " %listJson % "…") $
                    fmap headerHash blocks
    maybe onNoLca (handleBlocksWithLca sendActions blocks) =<<
        lcaWithMainChain (map (view blockHeader) (toNewestFirst blocks))
    inAssertMode $ logDebug $ "Finished processing sequence of blocks"
  where
    onNoLca = logWarning $
        "Sequence of blocks can't be processed, because there is no LCA. " <>
        "Probably rollback happened in parallel"

handleBlocksWithLca :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => SendActions BiP m -> OldestFirst NE (Block ssc) -> HeaderHash -> m ()
handleBlocksWithLca sendActions blocks lcaHash = do
    logDebug $ sformat lcaFmt lcaHash
    -- Head blund in result is the youngest one.
    toRollback <- DB.loadBlundsFromTipWhile $ \blk -> headerHash blk /= lcaHash
    maybe (applyWithoutRollback sendActions blocks)
          (applyWithRollback sendActions blocks lcaHash)
          (_Wrapped nonEmpty toRollback)
  where
    lcaFmt = "Handling block w/ LCA, which is "%shortHashF

applyWithoutRollback
    :: forall ssc m.
       (WorkMode ssc m, SscWorkersClass ssc)
    => SendActions BiP m -> OldestFirst NE (Block ssc) -> m ()
applyWithoutRollback sendActions blocks = do
    logInfo $ sformat ("Trying to apply blocks w/o rollback: "%listJson) $
        fmap (view blockHeader) blocks
    withBlkSemaphore applyWithoutRollbackDo >>= \case
        Left err     -> onFailedVerifyBlocks (getOldestFirst blocks) err
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
    logDebug "Finished applying blocks w/o rollback"
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
    => SendActions BiP m
    -> OldestFirst NE (Block ssc)
    -> HeaderHash
    -> NewestFirst NE (Blund ssc)
    -> m ()
applyWithRollback sendActions toApply lca toRollback = do
    logInfo $ sformat ("Trying to apply blocks w/ rollback: "%listJson)
        (map (view blockHeader) toApply)
    logInfo $
        sformat ("Blocks to rollback "%listJson) (fmap headerHash toRollback)
    res <- withBlkSemaphore $ \curTip -> do
        res <- L.applyWithRollback toRollback toApplyAfterLca
        pure (res, either (const curTip) identity res)
    case res of
        Left err -> logWarning $ "Couldn't apply blocks with rollback: " <> err
        Right newTip -> do
            logDebug $ sformat
                ("Finished applying blocks w/ rollback, relaying new tip: "%shortHashF)
                newTip
            logInfo $ blocksRolledBackMsg (getNewestFirst toRollback)
            logInfo $ blocksAppliedMsg (getOldestFirst toApply)
            relayBlock sendActions $ toApply ^. _Wrapped . _neLast
  where
    panicBrokenLca = panic "applyWithRollback: nothing after LCA :/"
    toApplyAfterLca =
        OldestFirst $
        fromMaybe panicBrokenLca $ nonEmpty $
        NE.dropWhile ((lca /=) . (^. prevBlockL)) $
        getOldestFirst $ toApply

relayBlock
    :: forall ssc m.
       (WorkMode ssc m)
    => SendActions BiP m -> Block ssc -> m ()
relayBlock _ (Left _)                  = pass
relayBlock sendActions (Right mainBlk) = do
    recHeaderVar <- ncRecoveryHeader <$> getNodeContext
    isRecovery <- isJust <$> atomically (tryReadTMVar recHeaderVar)
    -- Why 'ncPropagation' is not considered?
    when isRecovery $
        void $ fork $ announceBlock sendActions $ mainBlk ^. gbHeader

----------------------------------------------------------------------------
-- Logging formats
----------------------------------------------------------------------------

-- TODO: ban node for it!
onFailedVerifyBlocks
    :: forall ssc m.
       (WorkMode ssc m)
    => NonEmpty (Block ssc) -> Text -> m ()
onFailedVerifyBlocks blocks err = logWarning $
    sformat ("Failed to verify blocks: "%stext%"\n  blocks = "%listJson)
            err (fmap headerHash blocks)

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
