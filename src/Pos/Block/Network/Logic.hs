{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Control.Concurrent.STM     (isFullTBQueue, readTVar, writeTBQueue,
                                             writeTVar)
import           Control.Exception          (Exception (..))
import           Control.Lens               (_Wrapped)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text.Buildable        as B
import qualified Ether
import           Formatting                 (bprint, build, builder, sformat, shown,
                                             stext, (%))
import           Mockable                   (fork)
import           Paths_cardano_sl           (version)
import           Serokell.Data.Memory.Units (unitBuilder)
import           Serokell.Util.Text         (listJson)
import           Serokell.Util.Verify       (VerificationRes (..), formatFirstError)
import           System.Wlog                (logDebug, logInfo, logWarning)

import           Pos.Binary.Class           (biSize)
import           Pos.Binary.Communication   ()
import           Pos.Binary.Txp             ()
import           Pos.Block.Core             (Block, BlockHeader, blockHeader)
import           Pos.Block.Logic            (ClassifyHeaderRes (..),
                                             ClassifyHeadersRes (..), classifyHeaders,
                                             classifyNewHeader, getHeadersOlderExp,
                                             lcaWithMainChain, verifyAndApplyBlocks,
                                             withBlkSemaphore)
import qualified Pos.Block.Logic            as L
import           Pos.Block.Network.Announce (announceBlock)
import           Pos.Block.Network.Types    (MsgGetBlocks (..), MsgGetHeaders (..),
                                             MsgHeaders (..))
import           Pos.Block.Pure             (verifyHeaders)
import           Pos.Block.RetrievalQueue   (BlockRetrievalQueue, BlockRetrievalTask (..))
import           Pos.Block.Types            (Blund)
import           Pos.Communication.Limits   (LimitedLength, recvLimited, reifyMsgLimit)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             NodeId, OutSpecs, SendActions (..), convH,
                                             toOutSpecs)
import           Pos.Context                (BlockRetrievalQueueTag, LastKnownHeaderTag,
                                             recoveryCommGuard, recoveryInProgress)
import           Pos.Core                   (HasHeaderHash (..), HeaderHash, gbHeader,
                                             headerHashG, isMoreDifficult, prevBlockL)
import           Pos.Crypto                 (shortHashF)
import           Pos.DB.Block               (blkGetHeader)
import qualified Pos.DB.DB                  as DB
import           Pos.Discovery              (converseToNeighbors)
import           Pos.Exception              (cardanoExceptionFromException,
                                             cardanoExceptionToException)
import           Pos.Reporting.Methods      (reportMisbehaviourSilent)
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
    logDebug "Recovery triggered, requesting tips from neighbors"
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
        handleUnsolicitedHeader tip nodeId
    handleTip _ = pass

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
    :: forall ssc m.
       WorkMode ssc m
    => HeaderHash -> m MkHeadersRequestResult
mkHeadersRequest upto = do
    uHdr <- blkGetHeader @ssc upto
    if isJust uHdr then return MhrrBlockAdopted else do
        bHeaders <- toList <$> getHeadersOlderExp @ssc Nothing
        pure $ MhrrWithCheckpoints $ MsgGetHeaders (toList bHeaders) (Just upto)

-- Second case of 'handleBlockheaders'
handleUnsolicitedHeaders
    :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => NonEmpty (BlockHeader ssc)
    -> NodeId
    -> m ()
handleUnsolicitedHeaders (header :| []) nodeId =
    handleUnsolicitedHeader header nodeId
-- TODO: ban node for sending more than one unsolicited header.
handleUnsolicitedHeaders (h:|hs) _ = do
    logWarning "Someone sent us nonzero amount of headers we didn't expect"
    logWarning $ sformat ("Here they are: "%listJson) (h:hs)

handleUnsolicitedHeader
    :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => BlockHeader ssc
    -> NodeId
    -> m ()
handleUnsolicitedHeader header nodeId = do
    logDebug $ sformat
        ("handleUnsolicitedHeader: single header "%shortHashF%
         " was propagated, processing")
        hHash
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

matchRequestedHeaders
    :: (SscHelpersClass ssc)
    => NewestFirst NE (BlockHeader ssc) -> MsgGetHeaders -> Bool -> MatchReqHeadersRes
matchRequestedHeaders headers mgh@MsgGetHeaders {..} inRecovery =
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
    in if | not startMatches ->
            MRUnexpected $ sformat ("start (from) header "%build%
                                    " doesn't match request "%build)
                                   startHeader mgh
          | not mghToMatches ->
            MRUnexpected $ sformat ("finish (to) header "%build%
                                    " doesn't match request "%build%
                                    ", recovery: "%shown%", newTip:"%build)
                                   mghTo mgh inRecovery newTip
          | VerFailure errs <- verRes ->
              MRUnexpected $ "headers are bad: " <> formatFirstError errs
          | otherwise -> MRGood

requestHeaders
    :: forall ssc s m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => (NewestFirst NE (BlockHeader ssc) -> m ())
    -> MsgGetHeaders
    -> NodeId
    -> Proxy s
    -> ConversationActions MsgGetHeaders (LimitedLength s (MsgHeaders ssc)) m
    -> m ()
requestHeaders cont mgh nodeId _ conv = do
    logDebug $ sformat ("requestHeaders: withConnection: sending "%build) mgh
    send conv mgh
    mHeaders <- recvLimited conv
    inRecovery <- recoveryInProgress
    logDebug $ sformat ("requestHeaders: inRecovery = "%shown) inRecovery
    flip (maybe onNothing) mHeaders $ \(MsgHeaders headers) -> do
        logDebug $ sformat
            ("requestHeaders: withConnection: received "%listJson%
             " from nodeId "%build%" of total size "%builder)
            (map headerHash headers) nodeId (unitBuilder $ biSize headers)
        case matchRequestedHeaders headers mgh inRecovery of
            MRGood           -> do
                handleRequestedHeaders cont inRecovery headers
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
    => (NewestFirst NE (BlockHeader ssc) -> m ())
    -> Bool -- recovery in progress?
    -> NewestFirst NE (BlockHeader ssc)
    -> m ()
handleRequestedHeaders cont inRecovery headers = do
    classificationRes <- classifyHeaders inRecovery headers
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
                Nothing ->
                    throwM $ BlockNetLogicInternal $
                        "handleRequestedHeaders: couldn't find LCA child " <>
                        "within headers returned, most probably classifyHeaders is broken"
                Just headersPostfix ->
                    cont (NewestFirst headersPostfix)
        CHsUseless reason -> do
            let msg = sformat uselessFormat oldestHash newestHash reason
            logDebug msg
            -- It's weird to have useless headers in recovery mode.
            whenM recoveryInProgress $ throwM $ BlockNetLogicInternal msg
        CHsInvalid reason -> do
             -- TODO: ban node for sending invalid block.
            let msg = sformat invalidFormat oldestHash newestHash reason
            logDebug msg
            throwM $ DialogUnexpected msg
  where
    validFormat =
        "Received valid headers, can request blocks from " %shortHashF % " to " %shortHashF
    genericFormat what =
        "Chain of headers from " %shortHashF % " to " %shortHashF %
        " is "%what%" for the following reason: " %stext
    uselessFormat = genericFormat "useless"
    invalidFormat = genericFormat "invalid"

----------------------------------------------------------------------------
-- Putting things into request queue
----------------------------------------------------------------------------

-- | Given a valid blockheader and nodeid, this function will put them into
-- download queue and they will be processed later.
addHeaderToBlockRequestQueue
    :: forall ssc m.
       (WorkMode ssc m)
    => NodeId
    -> BlockHeader ssc
    -> Bool -- Continues?
    -> m ()
addHeaderToBlockRequestQueue nodeId header continues = do
    logDebug $ sformat ("addToBlockRequestQueue, : "%build) header
    queue <- Ether.ask @BlockRetrievalQueueTag
    lastKnownH <- Ether.ask @LastKnownHeaderTag
    added <- atomically $ do
        updateLastKnownHeader lastKnownH header
        addTaskToBlockRequestQueue nodeId queue $
            BlockRetrievalTask { brtHeader = header, brtContinues = continues }
    if added
    then logDebug $ sformat ("Added headers to block request queue: nodeId="%build%
                             ", header="%build)
                            nodeId (headerHash header)
    else logWarning $ sformat ("Failed to add headers from "%build%
                               " to block retrieval queue: queue is full")
                              nodeId

addTaskToBlockRequestQueue
    :: NodeId
    -> BlockRetrievalQueue ssc
    -> BlockRetrievalTask ssc
    -> STM Bool
addTaskToBlockRequestQueue nodeId queue task = do
    ifM (isFullTBQueue queue)
        (pure False)
        (True <$ writeTBQueue queue (nodeId, task))

updateLastKnownHeader
    :: TVar (Maybe (BlockHeader ssc))
    -> BlockHeader ssc
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
            for_ blocks $ jsonLog . jlAdoptedBlock
  where
    newestTip = blocks ^. _Wrapped . _neLast . headerHashG
    applyWithoutRollbackDo
        :: HeaderHash -> m (Either Text HeaderHash, HeaderHash)
    applyWithoutRollbackDo curTip = do
        logInfo "Verifying and applying blocks..."
        res <- verifyAndApplyBlocks False blocks
        logInfo "Verifying and applying blocks done"
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
            for_ (getOldestFirst toApply) $ jsonLog . jlAdoptedBlock
            relayBlock sendActions $ toApply ^. _Wrapped . _neLast
  where
    toRollbackHashes = fmap headerHash toRollback
    toApplyHashes = fmap headerHash toApply
    reportF =
        "Fork happened, data received from "%build%
        ". Blocks rolled back: "%listJson%
        ", blocks applied: "%listJson
    reportRollback =
        recoveryCommGuard $ do
            logDebug "Reporting rollback happened"
            reportMisbehaviourSilent version $
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
