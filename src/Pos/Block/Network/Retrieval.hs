{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Retrieval
       ( retrievalWorker
       , requestHeaders
       , addToBlockRequestQueue
       , mkHeadersRequest
       ) where

import           Control.Concurrent.STM.TBQueue (isFullTBQueue, readTBQueue, writeTBQueue)
import           Control.Lens                   (view, (^.))
import           Control.Monad.Trans.Except     (ExceptT, runExceptT, throwE)
import           Data.List.NonEmpty             (NonEmpty ((:|)), nonEmpty, (<|))
import qualified Data.List.NonEmpty             as NE
import           Formatting                     (build, int, sformat, shown, stext, (%))
import           Mockable                       (fork, handleAll, throw)
import           Node                           (ConversationActions (..), NodeId,
                                                 SendActions (..))
import           Serokell.Util.Text             (listJson)
import           Serokell.Util.Verify           (isVerSuccess)
import           System.Wlog                    (logDebug, logError, logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication       ()
import           Pos.Block.Logic                (ClassifyHeaderRes (..),
                                                 ClassifyHeadersRes (..), classifyHeaders,
                                                 classifyNewHeader, getHeadersOlderExp,
                                                 lcaWithMainChain, verifyAndApplyBlocks,
                                                 withBlkSemaphore)
import qualified Pos.Block.Logic                as L
import           Pos.Block.Network.Announce     (announceBlock)
import           Pos.Block.Network.Types        (MsgBlock (..), MsgGetBlocks (..),
                                                 MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.BiP          (BiP (..))
import           Pos.Constants                  (blkSecurityParam)
import           Pos.Context                    (getNodeContext, ncBlockRetrievalQueue)
import           Pos.Crypto                     (hash, shortHashF)
import qualified Pos.DB                         as DB
import           Pos.DHT.Model                  (nodeIdToAddress)
import           Pos.Ssc.Class                  (Ssc, SscWorkersClass)
import           Pos.Types                      (Block, BlockHeader, Blund,
                                                 HasHeaderHash (..), HeaderHash, NEBlocks,
                                                 blockHeader, gbHeader, prevBlockL,
                                                 verifyHeaders)
import           Pos.Util                       (inAssertMode, _neHead, _neLast)
import           Pos.WorkMode                   (WorkMode)

retrievalWorker :: (SscWorkersClass ssc, WorkMode ssc m) => SendActions BiP m -> m ()
retrievalWorker sendActions = handleAll handleWE $
    ncBlockRetrievalQueue <$> getNodeContext >>= loop
  where
    handleWE e = do
        logError $ sformat ("retrievalWorker: error caught "%shown) e
        throw e
    loop queue = do
       ph <- liftIO . atomically $ readTBQueue queue
       handleAll (handleLE ph) $ handle ph
       loop queue
    handleLE (peerId, headers) e =
        logWarning $ sformat
            ("Error handling peerId="%shown%", headers="%listJson%": "%shown)
            peerId headers e
    handle (peerId, headers) = do
        logDebug $ sformat
            ("retrievalWorker: handling peerId="%shown%", headers="%listJson)
            peerId headers
        classificationRes <- classifyHeaders' headers
        let newestHeader = headers ^. _neHead
            newestHash = headerHash newestHeader
            oldestHash = headerHash $ headers ^. _neLast
        case classificationRes of
            CHsValid lcaChild ->
                void $ handleCHsValid peerId lcaChild newestHash
            CHsUseless reason ->
                logDebug $ sformat uselessFormat oldestHash newestHash reason
            CHsInvalid reason ->
                logWarning $ sformat invalidFormat oldestHash newestHash reason
    classifyHeaders' (header :| []) = do
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
        let lcaChildHash = hash lcaChild
        logDebug $ sformat validFormat lcaChildHash newestHash
        void $ withConnectionTo sendActions peerId $ \conv -> do
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
    retrieveBlocks conv lcaChild endH =
        retrieveBlocks' 0 conv (lcaChild ^. prevBlockL) endH >>=
            \blocks@(b0 :| _) ->
                if headerHash b0 == headerHash lcaChild
                   then return blocks
                   else throwE $ sformat
                            ("First block of chain "%build
                                %" instead of expected "%build)
                            (b0 ^. blockHeader) lcaChild
    retrieveBlocks' :: WorkMode ssc m
                   => Int
                   -> ConversationActions (MsgGetBlocks ssc) (MsgBlock ssc) m
                   -> HeaderHash ssc
                   -> HeaderHash ssc
                   -> ExceptT Text m (NonEmpty (Block ssc))
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
                  then return $ block :| []
                  else (block <|) <$> retrieveBlocks' (i+1) conv curH endH

-- | Make 'GetHeaders' message using our main chain. This function
-- chooses appropriate 'from' hashes and puts them into 'GetHeaders'
-- message.
mkHeadersRequest
    :: WorkMode ssc m
    => Maybe (HeaderHash ssc) -> m (Maybe (MsgGetHeaders ssc))
mkHeadersRequest upto = do
    headers <- NE.nonEmpty <$> getHeadersOlderExp Nothing
    pure $ (\h -> MsgGetHeaders h upto) <$> headers

matchRequestedHeaders
    :: (Ssc ssc)
    => NonEmpty (BlockHeader ssc) -> MsgGetHeaders ssc -> Bool
matchRequestedHeaders headers@(newTip :| hs) MsgGetHeaders {..} =
    let startHeader = NE.last headers
        startMatches =
            or [ (startHeader ^. headerHashG) `elem` mghFrom
               , (startHeader ^. prevBlockL) `elem` mghFrom]
        mghToMatches
            | length headers > blkSecurityParam = True
            | isNothing mghTo = True
            | otherwise =  Just (hash newTip) == mghTo
     in and [ startMatches
            , mghToMatches
            , formChain
            ]
  where
    formChain = isVerSuccess (verifyHeaders True $ newTip:hs)

requestHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => MsgGetHeaders ssc
    -> NodeId
    -> ConversationActions (MsgGetHeaders ssc) (MsgHeaders ssc) m
    -> m ()
requestHeaders mgh peerId conv = do
    logDebug $ sformat ("handleUnsolicitedHeader: withConnection: sending "%shown) mgh
    send conv mgh
    mHeaders <- recv conv
    whenJust mHeaders $ \(MsgHeaders headers) -> do
        logDebug $ sformat
            ("handleUnsolicitedHeader: withConnection: received "%listJson)
            headers
        if matchRequestedHeaders headers mgh
           then handleRequestedHeaders headers peerId
           else handleUnexpected headers peerId
  where
    handleUnexpected (h:|hs) _ = do
        -- TODO: ban node for sending unsolicited header in conversation
        logWarning $ sformat
            ("handleUnsolicitedHeader: headers received were not requested, address: " % shown)
            (nodeIdToAddress peerId)
        logWarning $ sformat ("handleUnsolicitedHeader: unexpected headers: "%listJson) (h:hs)

-- First case of 'handleBlockheaders'
handleRequestedHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => NonEmpty (BlockHeader ssc)
    -> NodeId
    -> m ()
handleRequestedHeaders headers peerId = do
    logDebug "handleRequestedHeaders: headers were requested, will process"
    classificationRes <- classifyHeaders headers
    let newestHeader = headers ^. _neHead
        newestHash = headerHash newestHeader
        oldestHash = headerHash $ headers ^. _neLast
    case classificationRes of
        CHsValid lcaChild -> do
            let lcaChildHash = hash lcaChild
            logDebug $ sformat validFormat lcaChildHash newestHash
            addToBlockRequestQueue headers peerId
        CHsUseless reason ->
            logDebug $ sformat uselessFormat oldestHash newestHash reason
        CHsInvalid _ -> pass -- TODO: ban node for sending invalid block.
  where
    validFormat =
        "Received valid headers, can request blocks from " %shortHashF % " to " %shortHashF
    uselessFormat =
        "Chain of headers from " %shortHashF % " to " %shortHashF %
        " is useless for the following reason: " %stext

addToBlockRequestQueue
    :: forall ssc m.
       (WorkMode ssc m)
    => NonEmpty (BlockHeader ssc)
    -> NodeId
    -> m ()
addToBlockRequestQueue headers peerId = do
    queue <- ncBlockRetrievalQueue <$> getNodeContext
    added <- liftIO . atomically $
                ifM (isFullTBQueue queue)
                    (pure False)
                    (True <$ writeTBQueue queue (peerId, headers))
    if added
      then logDebug $ sformat
        ("Added to block request queue: peerId="%shown%", headers="%listJson) peerId headers
      else logWarning $ sformat
        ("Failed to add headers from "%shown%" to block retrieval queue: queue is full")
        peerId

-- | Make message which requests chain of blocks which is based on our
-- tip. LcaChild is the first block after LCA we don't
-- know. WantedBlock is the newest one we want to get.
mkBlocksRequest :: HeaderHash ssc -> HeaderHash ssc -> MsgGetBlocks ssc
mkBlocksRequest lcaChild wantedBlock =
    MsgGetBlocks
    { mgbFrom = lcaChild
    , mgbTo = wantedBlock
    }

handleBlocks
    :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => NonEmpty (Block ssc)
    -> SendActions BiP m
    -> m ()
-- Head block is the oldest one here.
handleBlocks blocks sendActions = do
    logDebug "handleBlocks: processing"
    inAssertMode $
        logInfo $
            sformat ("Processing sequence of blocks: " %listJson % "…") $
                    fmap headerHash blocks
    maybe onNoLca (handleBlocksWithLca sendActions blocks) =<<
        lcaWithMainChain (map (view blockHeader) $ NE.reverse blocks)
    inAssertMode $ logDebug $ "Finished processing sequence of blocks"
  where
    onNoLca = logWarning $
        "Sequence of blocks can't be processed, because there is no LCA. " <>
        "Probably rollback happened in parallel"

handleBlocksWithLca :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => SendActions BiP m -> NonEmpty (Block ssc) -> HeaderHash ssc -> m ()
handleBlocksWithLca sendActions blocks lcaHash = do
    logDebug $ sformat lcaFmt lcaHash
    -- Head blund in result is the youngest one.
    toRollback <- DB.loadBlundsFromTipWhile $ \blk -> headerHash blk /= lcaHash
    maybe (applyWithoutRollback sendActions blocks)
          (applyWithRollback sendActions blocks lcaHash)
          (nonEmpty toRollback)
  where
    lcaFmt = "Handling block w/ LCA, which is "%shortHashF

applyWithoutRollback
    :: forall ssc m.
       (WorkMode ssc m, SscWorkersClass ssc)
    => SendActions BiP m -> NonEmpty (Block ssc) -> m ()
applyWithoutRollback sendActions blocks = do
    logInfo $ sformat ("Trying to apply blocks w/o rollback: "%listJson)
        (map (view blockHeader) blocks)
    withBlkSemaphore applyWithoutRollbackDo >>= \case
        Left err  -> onFailedVerifyBlocks blocks err
        Right newTip -> do
            when (newTip /= newestTip) $
                logWarning $ sformat
                    ("Only blocks up to "%shortHashF%" were applied, "%
                     "newer were considered invalid")
                    newTip
            let toRelay =
                    fromMaybe (panic "Listeners#applyWithoutRollback is broken") $
                    find (\b -> b ^. headerHashG == newTip) blocks
                prefix = fmap (view blockHeader) $
                    NE.takeWhile ((/= newTip) . view headerHashG) blocks
                applied = NE.reverse (toRelay ^. blockHeader :| reverse prefix)
            relayBlock sendActions toRelay
            logInfo $ blocksAppliedMsg applied
    logDebug "Finished applying blocks w/o rollback"
  where
    newestTip = blocks ^. _neLast . headerHashG
    applyWithoutRollbackDo
        :: HeaderHash ssc -> m (Either Text (HeaderHash ssc), HeaderHash ssc)
    applyWithoutRollbackDo curTip = do
        res <- verifyAndApplyBlocks False blocks
        let newTip = either (const curTip) identity res
        pure (res, newTip)

-- | Head of @toRollback@ - the youngest block.
applyWithRollback
    :: forall ssc m.
       (WorkMode ssc m, SscWorkersClass ssc)
    => SendActions BiP m -> NonEmpty (Block ssc) -> HeaderHash ssc -> NonEmpty (Blund ssc) -> m ()
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
            logInfo $ blocksRolledBackMsg toRollback
            logInfo $ blocksAppliedMsg toApply
            relayBlock sendActions $ toApply ^. _neLast
  where
    panicBrokenLca = panic "applyWithRollback: nothing after LCA :/"
    toApplyAfterLca =
        fromMaybe panicBrokenLca $ NE.nonEmpty $
        NE.dropWhile ((lca /=) . (^. prevBlockL)) toApply

relayBlock
    :: forall ssc m.
       (WorkMode ssc m)
    => SendActions BiP m -> Block ssc -> m ()
relayBlock _ (Left _)                  = pass
relayBlock sendActions (Right mainBlk) =
    void $ fork $ announceBlock sendActions $ mainBlk ^. gbHeader

----------------------------------------------------------------------------
-- Logging formats
----------------------------------------------------------------------------

-- TODO: ban node for it!
onFailedVerifyBlocks
    :: forall ssc m.
       (WorkMode ssc m)
    => NEBlocks ssc -> Text -> m ()
onFailedVerifyBlocks blocks err = logWarning $
    sformat ("Failed to verify blocks: "%stext%"\n  blocks = "%listJson)
            err (fmap headerHash blocks)

blocksAppliedMsg
    :: forall ssc a.
       HasHeaderHash a ssc
    => NonEmpty a -> Text
blocksAppliedMsg (block :| []) =
    sformat ("Block has been adopted "%shortHashF) (headerHash block)
blocksAppliedMsg blocks =
    sformat ("Blocks have been adopted: "%listJson) (fmap (headerHash @a @ssc) blocks)

blocksRolledBackMsg
    :: forall ssc a.
       HasHeaderHash a ssc
    => NonEmpty a -> Text
blocksRolledBackMsg =
    sformat ("Blocks have been rolled back: "%listJson) . fmap (headerHash @a @ssc)
