{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Network-related logic that's mostly methods and dialogs between
-- nodes. Also see "Pos.Block.Network.Retrieval" for retrieval worker
-- loop logic.
module Pos.Block.Network.Logic
       (
         triggerRecovery
       , requestTipOuts
       , requestTip

       , handleUnsolicitedHeaders
       , mkHeadersRequest
       , requestHeaders

       , mkBlocksRequest
       , handleBlocks
       ) where

import           Universum

import           Control.Concurrent.STM     (isFullTBQueue, putTMVar, readTVar,
                                             tryReadTMVar, tryTakeTMVar, writeTBQueue,
                                             writeTVar)
import           Control.Exception          (Exception (..))
import           Control.Lens               (_Wrapped)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text.Buildable        as B
import qualified Ether
import           Formatting                 (bprint, build, sformat, shown, stext, (%))
import           Mockable                   (fork)
import           Paths_cardano_sl           (version)
import           Serokell.Util.Text         (listJson)
import           Serokell.Util.Verify       (VerificationRes (..), formatFirstError)
import           System.Wlog                (logDebug, logInfo, logWarning)

import           Pos.Binary.Communication   ()
import           Pos.Binary.Txp             ()
import           Pos.Block.Core             (Block, BlockHeader, blockHeader)
import           Pos.Block.Logic            (ClassifyHeaderRes (..),
                                             ClassifyHeadersRes (..), classifyHeaders,
                                             classifyNewHeader, getHeadersOlderExp,
                                             lcaWithMainChain, needRecovery,
                                             verifyAndApplyBlocks, withBlkSemaphore)
import qualified Pos.Block.Logic            as L
import           Pos.Block.Network.Announce (announceBlock)
import           Pos.Block.Network.Types    (MsgGetBlocks (..), MsgGetHeaders (..),
                                             MsgHeaders (..))
import           Pos.Block.Pure             (verifyHeaders)
import           Pos.Block.Types            (Blund)
import           Pos.Communication.Limits   (LimitedLength, recvLimited, reifyMsgLimit)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             NodeId, OutSpecs, SendActions (..), convH,
                                             toOutSpecs)
import           Pos.Context                (BlockRetrievalQueueTag, LastKnownHeaderTag,
                                             RecoveryHeaderTag, recoveryInProgress)
import           Pos.Core                   (HasHeaderHash (..), HeaderHash, difficultyL,
                                             gbHeader, headerHashG, prevBlockL)
import           Pos.Crypto                 (shortHashF)
import qualified Pos.DB.DB                  as DB
import           Pos.Discovery              (converseToNeighbors)
import           Pos.Exception              (cardanoExceptionFromException,
                                             cardanoExceptionToException)
import           Pos.Reporting.Methods      (reportMisbehaviourMasked)
import           Pos.Ssc.Class              (SscHelpersClass, SscWorkersClass)
import           Pos.Util                   (inAssertMode, _neHead, _neLast)
import           Pos.Util.Chrono            (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Util.JsonLog           (jlAdoptedBlock)
import           Pos.Util.TimeWarp          (CanJsonLog (..))
import           Pos.WorkMode.Class         (WorkMode)

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data BlockNetLogicException
    = VerifyBlocksException Text
      -- ^ Failed to verify blocks coming from node.
    | DialogUnexpected Text
      -- ^ Node's response in any network/block related logic was
      -- unexpected.
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
-- tips (e.g. in 'queryBlocksWorker').
--
-- Note that when recovery is in progress (see 'recoveryInProgress'),
-- 'triggerRecovery' does nothing. It's okay because when recovery is in
-- progress and 'ncRecoveryHeader' is full, we'll be requesting blocks anyway
-- and until we're finished we shouldn't be asking for new blocks.
triggerRecovery :: forall ssc m.
    (SscWorkersClass ssc, WorkMode ssc m)
    => SendActions m -> m ()
triggerRecovery sendActions = unlessM recoveryInProgress $ do
    logDebug "Recovery started, requesting tips from neighbors"
    reifyMsgLimit (Proxy @(MsgHeaders ssc)) $ \limitProxy -> do
        converseToNeighbors sendActions (pure . Conversation . requestTip limitProxy) `catch`
            \(e :: SomeException) -> do
               logDebug ("Error happened in triggerRecovery: " <> show e)
               throwM e
        logDebug "Finished requesting tips for recovery"

requestTipOuts :: OutSpecs
requestTipOuts =
    toOutSpecs [ convH (Proxy :: Proxy MsgGetHeaders)
                       (Proxy :: Proxy (MsgHeaders ssc)) ]

-- | Is used if we're recovering after offline and want to know what's
-- current blockchain state. Sends "what's your current tip" request
-- to everybody we know.
requestTip
    :: forall ssc s m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => Proxy s
    -> NodeId
    -> ConversationActions MsgGetHeaders (LimitedLength s (MsgHeaders ssc)) m
    -> m ()
requestTip _ nodeId conv = do
    logDebug "Requesting tip..."
    send conv (MsgGetHeaders [] Nothing)
    whenJustM (recvLimited conv) handleTip
  where
    handleTip (MsgHeaders (NewestFirst (tip:|[]))) = do
        logDebug $ sformat ("Got tip "%shortHashF%", processing") (headerHash tip)
        handleUnsolicitedHeader tip nodeId conv
    handleTip _ = pass

----------------------------------------------------------------------------
-- Headers processing
----------------------------------------------------------------------------

-- | Make 'GetHeaders' message using our main chain. This function
-- chooses appropriate 'from' hashes and puts them into 'GetHeaders'
-- message.
mkHeadersRequest
    :: forall ssc m.
       WorkMode ssc m
    => Maybe HeaderHash -> m (Maybe MsgGetHeaders)
mkHeadersRequest upto = do
    mbHeaders <- nonEmpty . toList <$> getHeadersOlderExp @ssc Nothing
    pure $ (\h -> MsgGetHeaders (toList h) upto) <$> mbHeaders

-- Second case of 'handleBlockheaders'
handleUnsolicitedHeaders
    :: forall ssc s m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => NonEmpty (BlockHeader ssc)
    -> NodeId
    -> ConversationActions MsgGetHeaders (LimitedLength s (MsgHeaders ssc)) m
    -> m ()
handleUnsolicitedHeaders (header :| []) nodeId conv =
    handleUnsolicitedHeader header nodeId conv
-- TODO: ban node for sending more than one unsolicited header.
handleUnsolicitedHeaders (h:|hs) _ _ = do
    logWarning "Someone sent us nonzero amount of headers we didn't expect"
    logWarning $ sformat ("Here they are: "%listJson) (h:hs)

handleUnsolicitedHeader
    :: forall ssc s m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => BlockHeader ssc
    -> NodeId
    -> ConversationActions MsgGetHeaders (LimitedLength s (MsgHeaders ssc)) m
    -> m ()
handleUnsolicitedHeader header nodeId conv = do
    logDebug $ sformat
        ("handleUnsolicitedHeader: single header "%shortHashF%
         " was propagated, processing")
        hHash
    classificationRes <- classifyNewHeader header
    -- TODO: should we set 'To' hash to hash of header or leave it unlimited?
    case classificationRes of
        CHContinues -> do
            logDebug $ sformat continuesFormat hHash
            addToBlockRequestQueue (one header) nodeId Nothing
        CHAlternative -> do
            logInfo $ sformat alternativeFormat hHash
            mghM <- mkHeadersRequest (Just hHash)
            whenJust mghM $ \mgh ->
                requestHeaders mgh nodeId (Just header) Proxy conv
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
    | MRUnexpected Text
      -- ^ Headers don't represent valid response to our
      -- request. Reason is attached.
    deriving (Show)

matchRequestedHeaders
    :: (SscHelpersClass ssc)
    => NewestFirst NE (BlockHeader ssc) -> MsgGetHeaders -> Bool -> MatchReqHeadersRes
matchRequestedHeaders headers MsgGetHeaders {..} inRecovery =
    let newTip = headers ^. _Wrapped . _neHead
        startHeader = headers ^. _Wrapped . _neLast
        startMatches =
            or [ (startHeader ^. headerHashG) `elem` mghFrom
               , (startHeader ^. prevBlockL) `elem` mghFrom
               ]
        mghToMatches
            | inRecovery = True
            | isNothing mghTo = True
            | otherwise = Just (headerHash newTip) == mghTo
        verRes = verifyHeaders (headers & _Wrapped %~ toList)
    in if | not startMatches -> MRUnexpected "start (from) doesn't match"
          | not mghToMatches -> MRUnexpected "finish (to) doesn't match"
          | VerFailure errs <- verRes ->
              MRUnexpected $ "headers are bad: " <> formatFirstError errs
          | otherwise -> MRGood

-- Second argument is mghTo block header (not hash). Don't pass it
-- only if you don't know it.
requestHeaders
    :: forall ssc s m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => MsgGetHeaders
    -> NodeId
    -> Maybe (BlockHeader ssc)
    -> Proxy s
    -> ConversationActions MsgGetHeaders (LimitedLength s (MsgHeaders ssc)) m
    -> m ()
requestHeaders mgh nodeId origTip _ conv = do
    logDebug $ sformat ("requestHeaders: withConnection: sending "%build) mgh
    send conv mgh
    mHeaders <- recvLimited conv
    inRecovery <- needRecovery @ssc
    logDebug $ sformat ("requestHeaders: inRecovery = "%shown) inRecovery
    flip (maybe onNothing) mHeaders $ \(MsgHeaders headers) -> do
        logDebug $ sformat
            ("requestHeaders: withConnection: received "%listJson)
            (map headerHash headers)
        case matchRequestedHeaders headers mgh inRecovery of
            MRGood           -> do
                handleRequestedHeaders headers nodeId origTip
            MRUnexpected msg -> handleUnexpected headers msg
  where
    onNothing = do
        logWarning "requestHeaders: received Nothing, waiting for MsgHeaders"
        throwM $ DialogUnexpected $
            sformat ("requestHeaders: received Nothing from "%build) nodeId
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

-- First case of 'handleBlockheaders'
handleRequestedHeaders
    :: forall ssc m.
       WorkMode ssc m
    => NewestFirst NE (BlockHeader ssc)
    -> NodeId
    -> Maybe (BlockHeader ssc)
    -> m ()
handleRequestedHeaders headers nodeId origTip = do
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
            case nonEmpty headers' of
                Nothing -> logWarning $
                    "handleRequestedHeaders: couldn't find LCA child " <>
                    "within headers returned, most probably classifyHeaders is broken"
                Just headersPostfix ->
                    addToBlockRequestQueue (NewestFirst headersPostfix) nodeId origTip
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
    -> Maybe (BlockHeader ssc)
    -> m ()
addToBlockRequestQueue headers nodeId mrecoveryTip = do
    queue <- Ether.ask @BlockRetrievalQueueTag
    recHeaderVar <- Ether.ask @RecoveryHeaderTag
    lastKnownH <- Ether.ask @LastKnownHeaderTag
    let updateRecoveryHeader (Just recoveryTip) = do
            oldV <- readTVar lastKnownH
            when (maybe True (recoveryTip `isMoreDifficult`) oldV) $
                writeTVar lastKnownH (Just recoveryTip)
            let replace = tryTakeTMVar recHeaderVar >>= \case
                    Just (_, header')
                        | not (recoveryTip `isMoreDifficult` header') -> pass
                    _ -> putTMVar recHeaderVar (nodeId, recoveryTip)
            tryReadTMVar recHeaderVar >>= \case
                Nothing -> replace
                Just (_,curRecHeader) ->
                    when (recoveryTip `isMoreDifficult` curRecHeader) replace
        updateRecoveryHeader Nothing = pass
    added <- atomically $ do
        updateRecoveryHeader mrecoveryTip
        ifM (isFullTBQueue queue)
            (pure False)
            (True <$ writeTBQueue queue (nodeId, headers))
    if added
    then logDebug $ sformat ("Added to block request queue: nodeId="%build%
                             ", headers="%listJson)
                            nodeId (fmap headerHash headers)
    else logWarning $ sformat ("Failed to add headers from "%build%
                               " to block retrieval queue: queue is full")
                              nodeId
  where
    a `isMoreDifficult` b = a ^. difficultyL > b ^. difficultyL


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
    :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => NodeId
    -> OldestFirst NE (Block ssc)
    -> SendActions m
    -> m ()
handleBlocks nodeId blocks sendActions = do
    logDebug "handleBlocks: processing"
    inAssertMode $
        logInfo $
            sformat ("Processing sequence of blocks: " %listJson % "...") $
                    fmap headerHash blocks
    maybe onNoLca (handleBlocksWithLca nodeId sendActions blocks) =<<
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
handleBlocksWithLca nodeId sendActions blocks lcaHash = do
    logDebug $ sformat lcaFmt lcaHash
    -- Head blund in result is the youngest one.
    toRollback <- DB.loadBlundsFromTipWhile $ \blk -> headerHash blk /= lcaHash
    maybe (applyWithoutRollback sendActions blocks)
          (applyWithRollback nodeId sendActions blocks lcaHash)
          (_Wrapped nonEmpty toRollback)
  where
    lcaFmt = "Handling block w/ LCA, which is "%shortHashF

applyWithoutRollback
    :: forall ssc m.
       (WorkMode ssc m, SscWorkersClass ssc)
    => SendActions m
    -> OldestFirst NE (Block ssc)
    -> m ()
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
                    fromMaybe (error "Listeners#applyWithoutRollback is broken") $
                    find (\b -> headerHash b == newTip) blocks
                prefix = blocks
                    & _Wrapped %~ NE.takeWhile ((/= newTip) . headerHash)
                    & map (view blockHeader)
                applied = NE.fromList $
                    getOldestFirst prefix <> one (toRelay ^. blockHeader)
            relayBlock sendActions toRelay
            logInfo $ blocksAppliedMsg applied
            forM_ blocks $ jsonLog . jlAdoptedBlock
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
applyWithRollback nodeId sendActions toApply lca toRollback = do
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
            forM_ (getOldestFirst toApply) $ jsonLog . jlAdoptedBlock
            relayBlock sendActions $ toApply ^. _Wrapped . _neLast
  where
    toRollbackHashes = fmap headerHash toRollback
    toApplyHashes = fmap headerHash toApply
    reportF =
        "Fork happened, data received from "%build%
        ". Blocks rolled back: "%listJson%
        ", blocks applied: "%listJson
    reportRollback =
        unlessM recoveryInProgress $ do
            logDebug "Reporting rollback happened"
            reportMisbehaviourMasked version $
                sformat reportF nodeId toRollbackHashes toApplyHashes
    panicBrokenLca = error "applyWithRollback: nothing after LCA :<"
    toApplyAfterLca =
        OldestFirst $
        fromMaybe panicBrokenLca $ nonEmpty $
        NE.dropWhile ((lca /=) . (^. prevBlockL)) $
        getOldestFirst $ toApply

relayBlock
    :: forall ssc m.
       (WorkMode ssc m)
    => SendActions m -> Block ssc -> m ()
relayBlock _ (Left _)                  = logDebug "Not relaying Genesis block"
relayBlock sendActions (Right mainBlk) = do
    recoveryInProgress >>= \case
        True -> logDebug "Not relaying block in recovery mode"
        False -> do
            logDebug $ sformat ("Calling announceBlock for "%build%".") (mainBlk ^. gbHeader)
            void $ fork $ announceBlock sendActions $ mainBlk ^. gbHeader

----------------------------------------------------------------------------
-- Common logging / logic sink points
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
