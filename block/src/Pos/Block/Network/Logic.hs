{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | Network-related logic that's mostly methods and dialogs between
-- nodes. Also see "Pos.Block.Network.Retrieval" for retrieval worker
-- loop logic.
module Pos.Block.Network.Logic
       (
         BlockNetLogicException (..)
       , triggerRecovery
       , requestTipOuts
       , requestTip

       , handleUnsolicitedHeaders
       , mkHeadersRequest
       , MkHeadersRequestResult(..)
       , requestHeaders

       , mkBlocksRequest
       , handleBlocks
       ) where

import           Universum

import           Control.Concurrent.STM (isFullTBQueue, readTVar, writeTBQueue, writeTVar)
import           Control.Exception (Exception (..))
import           Control.Lens (to)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Buildable as B
import           Ether.Internal (lensOf)
import           Formatting (bprint, build, builder, int, sformat, shown, stext, (%))
import           Serokell.Data.Memory.Units (unitBuilder)
import           Serokell.Util.Text (listJson)
import           System.Wlog (logDebug, logInfo, logWarning)

import           Pos.Binary.Class (biSize)
import           Pos.Binary.Txp ()
import           Pos.Block.BlockWorkMode (BlockInstancesConstraint, BlockWorkMode)
import           Pos.Block.Error (ApplyBlocksException)
import           Pos.Block.Logic (ClassifyHeaderRes (..), ClassifyHeadersRes (..), classifyHeaders,
                                  classifyNewHeader, getHeadersOlderExp, lcaWithMainChain,
                                  verifyAndApplyBlocks)
import qualified Pos.Block.Logic as L
import           Pos.Block.Network.Announce (announceBlock)
import           Pos.Block.Network.Types (MsgGetBlocks (..), MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Block.RetrievalQueue (BlockRetrievalQueue, BlockRetrievalQueueTag,
                                           BlockRetrievalTask (..))
import           Pos.Block.Types (Blund, LastKnownHeaderTag)
import           Pos.Communication.Limits.Types (recvLimited)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             EnqueueMsg, MsgType (..), NodeId, OutSpecs, convH,
                                             toOutSpecs, waitForConversations)
import           Pos.Core (EpochOrSlot (..), HasConfiguration, HasHeaderHash (..), HeaderHash,
                           SlotId (..), criticalForkThreshold, crucialSlot, epochIndexL,
                           epochOrSlotG, gbHeader, headerHashG, isMoreDifficult, prevBlockL)
import           Pos.Core.Block (Block, BlockHeader, blockHeader)
import           Pos.Crypto (shortHashF)
import qualified Pos.DB.Block.Load as DB
import qualified Pos.DB.BlockIndex as DB
import           Pos.Exception (cardanoExceptionFromException, cardanoExceptionToException)
import           Pos.Lrc.Error (LrcError (UnknownBlocksForLrc))
import           Pos.Lrc.Worker (lrcSingleShot)
import           Pos.Recovery.Info (recoveryInProgress)
import           Pos.Reporting.Methods (reportMisbehaviour)
import           Pos.StateLock (Priority (..), modifyStateLock, withStateLockNoMetrics)
import           Pos.Util (_neHead, _neLast)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..), _NewestFirst,
                                  _OldestFirst)
import           Pos.Util.JsonLog (jlAdoptedBlock)
import           Pos.Util.TimeWarp (CanJsonLog (..))

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

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
    :: BlockWorkMode ctx m
    => EnqueueMsg m -> m ()
triggerRecovery enqueue = unlessM recoveryInProgress $ do
    logDebug "Recovery triggered, requesting tips from neighbors"
    void (enqueue (MsgRequestBlockHeaders Nothing) (\addr _ -> pure (Conversation (requestTip addr))) >>= waitForConversations) `catch`
        \(e :: SomeException) -> do
           logDebug ("Error happened in triggerRecovery: " <> show e)
           throwM e
    logDebug "Finished requesting tips for recovery"

requestTipOuts :: BlockInstancesConstraint m => Proxy m -> OutSpecs
requestTipOuts _ =
    toOutSpecs [ convH (Proxy :: Proxy MsgGetHeaders)
                       (Proxy :: Proxy MsgHeaders) ]

-- | Is used if we're recovering after offline and want to know what's
-- current blockchain state. Sends "what's your current tip" request
-- to everybody we know.
requestTip
    :: BlockWorkMode ctx m
    => NodeId
    -> ConversationActions MsgGetHeaders MsgHeaders m
    -> m ()
requestTip nodeId conv = do
    logDebug "Requesting tip..."
    send conv (MsgGetHeaders [] Nothing)
    whenJustM (recvLimited conv) handleTip
  where
    handleTip (MsgHeaders (NewestFirst (tip:|[]))) = do
        logDebug $ sformat ("Got tip "%shortHashF%", processing") (headerHash tip)
        handleUnsolicitedHeader tip nodeId
    handleTip t =
        logWarning $ sformat ("requestTip: got unexpected response: "%shown) t

----------------------------------------------------------------------------
-- Headers processing
----------------------------------------------------------------------------

-- | Result of creating a request message for more headers.
data MkHeadersRequestResult
    = MhrrBlockAdopted
      -- ^ The block pointed by the header is already adopted, no need to
      -- make the request.
    | MhrrWithCheckpoints MsgGetHeaders
      -- ^ A good request with checkpoints can be made.

-- | Make 'GetHeaders' message using our main chain. This function
-- chooses appropriate 'from' hashes and puts them into 'GetHeaders'
-- message.
mkHeadersRequest
    :: forall ctx m.
       BlockWorkMode ctx m
    => HeaderHash -> m MkHeadersRequestResult
mkHeadersRequest upto = do
    uHdr <- DB.getHeader upto
    if isJust uHdr then return MhrrBlockAdopted else do
        bHeaders <- toList <$> getHeadersOlderExp Nothing
        pure $ MhrrWithCheckpoints $ MsgGetHeaders (toList bHeaders) (Just upto)

-- Second case of 'handleBlockheaders'
handleUnsolicitedHeaders
    :: BlockWorkMode ctx m
    => NonEmpty BlockHeader
    -> NodeId
    -> m ()
handleUnsolicitedHeaders (header :| []) nodeId =
    handleUnsolicitedHeader header nodeId
-- TODO: ban node for sending more than one unsolicited header.
handleUnsolicitedHeaders (h:|hs) _ = do
    logWarning "Someone sent us nonzero amount of headers we didn't expect"
    logWarning $ sformat ("Here they are: "%listJson) (h:hs)

handleUnsolicitedHeader
    :: BlockWorkMode ctx m
    => BlockHeader
    -> NodeId
    -> m ()
handleUnsolicitedHeader header nodeId = do
    logDebug $ sformat
        ("handleUnsolicitedHeader: single header was propagated, processing:\n"
         %build) header
    classificationRes <- classifyNewHeader header
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
        "Header " %shortHashF % " is useless for the following reason: " %stext

-- | Result of 'matchHeadersRequest'
data MatchReqHeadersRes
    = MRGood
      -- ^ Headers were indeed requested and precisely match our
      -- request
    | MRUnexpected Text
      -- ^ Headers don't represent valid response to our
      -- request. Reason is attached.
    deriving (Show)

-- TODO This function is used ONLY in recovery mode, so passing the
-- flag is redundant, it's always True.
matchRequestedHeaders
    :: HasConfiguration
    => NewestFirst NE BlockHeader -> MsgGetHeaders -> Bool -> MatchReqHeadersRes
matchRequestedHeaders headers mgh@MsgGetHeaders {..} inRecovery =
    let newTip = headers ^. _NewestFirst . _neHead
        startHeader = headers ^. _NewestFirst . _neLast
        startMatches =
            or [ (startHeader ^. headerHashG) `elem` mghFrom
               , (startHeader ^. prevBlockL) `elem` mghFrom
               ]
        mghToMatches
            | inRecovery = True
            | isNothing mghTo = True
            | otherwise = Just (headerHash newTip) == mghTo
    in if | not startMatches ->
            MRUnexpected $ sformat ("start (from) header "%build%
                                    " doesn't match request "%build)
                                   startHeader mgh
          | not mghToMatches ->
            MRUnexpected $ sformat ("finish (to) header "%build%
                                    " doesn't match request "%build%
                                    ", recovery: "%shown%", newTip:"%build)
                                   mghTo mgh inRecovery newTip
          | otherwise -> MRGood

requestHeaders
    :: forall ctx m.
       BlockWorkMode ctx m
    => (NewestFirst NE BlockHeader -> m ())
    -> MsgGetHeaders
    -> NodeId
    -> ConversationActions MsgGetHeaders MsgHeaders m
    -> m ()
requestHeaders cont mgh nodeId conv = do
    logDebug $ sformat ("requestHeaders: sending "%build) mgh
    send conv mgh
    mHeaders <- recvLimited conv
    inRecovery <- recoveryInProgress
    -- TODO: it's very suspicious to see False here as requestHeaders
    -- is only called when we're in recovery mode.
    logDebug $ sformat ("requestHeaders: inRecovery = "%shown) inRecovery
    case mHeaders of
        Nothing -> do
            logWarning "requestHeaders: received Nothing as a response on MsgGetHeaders"
            throwM $ DialogUnexpected $
                sformat ("requestHeaders: received Nothing from "%build) nodeId
        Just (MsgNoHeaders t) -> do
            logWarning $ "requestHeaders: received MsgNoHeaders: " <> t
            throwM $ DialogUnexpected $
                sformat ("requestHeaders: received MsgNoHeaders from "%
                         build%", msg: "%stext)
                        nodeId
                        t
        Just (MsgHeaders headers) -> do
            logDebug $ sformat
                ("requestHeaders: received "%int%" headers of total size "%builder%
                 " from nodeId "%build%": "%listJson)
                (headers ^. _NewestFirst . to NE.length)
                (unitBuilder $ biSize headers)
                nodeId
                (map headerHash headers)
            case matchRequestedHeaders headers mgh inRecovery of
                MRGood           ->
                    handleRequestedHeaders cont inRecovery headers
                MRUnexpected msg ->
                    handleUnexpected headers msg
  where
    handleUnexpected hs msg = do
        -- TODO: ban node for sending unsolicited header in conversation
        logWarning $ sformat
            ("requestHeaders: headers received were not requested or are invalid"%
             ", peer id: "%build%", reason:"%stext)
            nodeId msg
        logWarning $ sformat
            ("requestHeaders: unexpected or invalid headers: "%listJson) hs
        throwM $ DialogUnexpected $
            sformat ("requestHeaders: received unexpected headers from "%build) nodeId

handleRequestedHeaders
    :: forall ctx m.
       BlockWorkMode ctx m
    => (NewestFirst NE BlockHeader -> m ())
    -> Bool -- recovery in progress?
    -> NewestFirst NE BlockHeader
    -> m ()
handleRequestedHeaders cont inRecovery headers = do
    -- Try to calculate LRC for the oldest header epoch. If we're in
    -- recovery and oldest header is from the next epoch, no lrc will
    -- be automatically calculated as all workers are locked in
    -- recovery mode. So we should try to do it manually.
    tryCalculateLrc

    classificationRes <- classifyHeaders inRecovery headers
    case classificationRes of
        CHsValid lcaChild -> do
            let lcaHash = lcaChild ^. prevBlockL
            let headers' = NE.takeWhile ((/= lcaHash) . headerHash)
                                        (getNewestFirst headers)
            logDebug $ sformat validFormat (headerHash lcaChild)newestHash
            case nonEmpty headers' of
                Nothing ->
                    throwM $ BlockNetLogicInternal $
                        "handleRequestedHeaders: couldn't find LCA child " <>
                        "within headers returned, most probably classifyHeaders is broken"
                Just headersPostfix ->
                    cont (NewestFirst headersPostfix)
        CHsUseless reason -> do
            let msg = sformat uselessFormat oldestHash newestHash reason
            logDebug msg
            throwM $ DialogUnexpected msg
        CHsInvalid reason -> do
             -- TODO: ban node for sending invalid block.
            let msg = sformat invalidFormat oldestHash newestHash reason
            logDebug msg
            throwM $ DialogUnexpected msg
  where
    newestHeader = headers ^. _NewestFirst . _neHead
    oldestHeader = headers ^. _NewestFirst . _neLast
    newestHash = headerHash newestHeader
    oldestHash = headerHash oldestHeader
    oldestEpoch = oldestHeader ^. epochIndexL

    tryCalculateLrc = do
        tip <- DB.getTipHeader
        let tipEpochOrSlot = tip ^. epochOrSlotG
        let tipEpoch = tip ^. epochIndexL
        let differentEpochs = oldestEpoch == tipEpoch + 1
        -- CSL-1660 We should also check that there's k blocks after
        -- crucial slot.
        let tipAfterCrucial = case unEpochOrSlot tipEpochOrSlot of
                -- we assume differentEpochs is true, so this is
                -- before zero's slot of epoch e, while new header has
                -- epoch e+1.
                Left _   -> False
                Right si -> siSlot si > siSlot (crucialSlot $ siEpoch si)
        let shouldExecute = differentEpochs && tipAfterCrucial

        if shouldExecute then do
            logDebug "handleRequesteHeaders: LRC started"
            let handler UnknownBlocksForLrc =
                    logWarning $
                    "handleRequestedHeaders: tried lrcSingleShot, " <>
                    "got UnknownBlocksForLrc"
                handler e                   = throwM e
            withStateLockNoMetrics HighPriority $ const $
                lrcSingleShot oldestEpoch `catch` handler
            logDebug "handleRequesteHeaders: LRC ended"
        else logDebug $ sformat ("handleRequestHeaders: not calculating LRC: "%
                                 "oldest header epoch "%build%", tip epoch "%build)
                                oldestEpoch tipEpoch

    validFormat =
        "Received valid headers, can request blocks from "%shortHashF%
        " to "%shortHashF
    genericFormat what =
        "handleRequestedHeaders: chain of headers from "%shortHashF%
        " to "%shortHashF%
        " is "%what%" for the following reason: "%stext
    uselessFormat = genericFormat "useless"
    invalidFormat = genericFormat "invalid"

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
    -> Bool -- ^ Was classified as chain continuation
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
    :: BlockWorkMode ctx m
    => NodeId
    -> OldestFirst NE Block
    -> EnqueueMsg m
    -> m ()
handleBlocks nodeId blocks enqueue = do
    logDebug "handleBlocks: processing"
    inAssertMode $
        logInfo $
            sformat ("Processing sequence of blocks: " %listJson % "...") $
                    fmap headerHash blocks
    maybe onNoLca (handleBlocksWithLca nodeId enqueue blocks) =<<
        lcaWithMainChain (map (view blockHeader) blocks)
    inAssertMode $ logDebug $ "Finished processing sequence of blocks"
  where
    onNoLca = logWarning $
        "Sequence of blocks can't be processed, because there is no LCA. " <>
        "Probably rollback happened in parallel"

handleBlocksWithLca
    :: BlockWorkMode ctx m
    => NodeId
    -> EnqueueMsg m
    -> OldestFirst NE Block
    -> HeaderHash
    -> m ()
handleBlocksWithLca nodeId enqueue blocks lcaHash = do
    logDebug $ sformat lcaFmt lcaHash
    -- Head blund in result is the youngest one.
    toRollback <- DB.loadBlundsFromTipWhile $ \blk -> headerHash blk /= lcaHash
    maybe (applyWithoutRollback enqueue blocks)
          (applyWithRollback nodeId enqueue blocks lcaHash)
          (_NewestFirst nonEmpty toRollback)
  where
    lcaFmt = "Handling block w/ LCA, which is "%shortHashF

applyWithoutRollback
    :: forall ctx m.
       BlockWorkMode ctx m
    => EnqueueMsg m
    -> OldestFirst NE Block
    -> m ()
applyWithoutRollback enqueue blocks = do
    logInfo $ sformat ("Trying to apply blocks w/o rollback: "%listJson) $
        fmap (view blockHeader) blocks
    modifyStateLock HighPriority "applyWithoutRollback" applyWithoutRollbackDo >>= \case
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
            relayBlock enqueue toRelay
            logInfo $ blocksAppliedMsg applied
            for_ blocks $ jsonLog . jlAdoptedBlock
  where
    newestTip = blocks ^. _OldestFirst . _neLast . headerHashG
    applyWithoutRollbackDo
        :: HeaderHash -> m (HeaderHash, Either ApplyBlocksException HeaderHash)
    applyWithoutRollbackDo curTip = do
        logInfo "Verifying and applying blocks..."
        res <- verifyAndApplyBlocks False blocks
        logInfo "Verifying and applying blocks done"
        let newTip = either (const curTip) identity res
        pure (newTip, res)

applyWithRollback
    :: BlockWorkMode ctx m
    => NodeId
    -> EnqueueMsg m
    -> OldestFirst NE Block
    -> HeaderHash
    -> NewestFirst NE Blund
    -> m ()
applyWithRollback nodeId enqueue toApply lca toRollback = do
    logInfo $ sformat ("Trying to apply blocks w/ rollback: "%listJson)
        (map (view blockHeader) toApply)
    logInfo $ sformat ("Blocks to rollback "%listJson) toRollbackHashes
    res <- modifyStateLock HighPriority "applyWithRollback" $ \curTip -> do
        res <- L.applyWithRollback toRollback toApplyAfterLca
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
            relayBlock enqueue $ toApply ^. _OldestFirst . _neLast
  where
    toRollbackHashes = fmap headerHash toRollback
    toApplyHashes = fmap headerHash toApply
    reportF =
        "Fork happened, data received from "%build%
        ". Blocks rolled back: "%listJson%
        ", blocks applied: "%listJson
    reportRollback = do
        let rollbackDepth = length toRollback
        let isCritical = rollbackDepth >= criticalForkThreshold
        -- REPORT:MISBEHAVIOUR(F/T) Blockchain fork occurred (depends on depth).
        reportMisbehaviour isCritical $
            sformat reportF nodeId toRollbackHashes toApplyHashes

    panicBrokenLca = error "applyWithRollback: nothing after LCA :<"
    toApplyAfterLca =
        OldestFirst $
        fromMaybe panicBrokenLca $ nonEmpty $
        NE.dropWhile ((lca /=) . (^. prevBlockL)) $
        getOldestFirst $ toApply

relayBlock
    :: forall ctx m.
       (BlockWorkMode ctx m)
    => EnqueueMsg m -> Block -> m ()
relayBlock _ (Left _)                  = logDebug "Not relaying Genesis block"
relayBlock enqueue (Right mainBlk) = do
    recoveryInProgress >>= \case
        True -> logDebug "Not relaying block in recovery mode"
        False -> do
            logDebug $ sformat ("Calling announceBlock for "%shortHashF%".")
                       (mainBlk ^. gbHeader . headerHashG)
            void $ announceBlock enqueue $ mainBlk ^. gbHeader

----------------------------------------------------------------------------
-- Common logging / logic sink points
----------------------------------------------------------------------------

-- TODO: ban node for it!
onFailedVerifyBlocks
    :: forall ctx m.
       (BlockWorkMode ctx m)
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
