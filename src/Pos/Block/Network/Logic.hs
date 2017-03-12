{-# LANGUAGE ScopedTypeVariables #-}

-- | Network-related logic that's mostly methods and dialogs between
-- nodes. Also see "Pos.Block.Network.Retrieval" for retrieval worker
-- loop logic.
module Pos.Block.Network.Logic
       (
         needRecovery
       , triggerRecovery
       , requestTipOuts
       , requestTip

       , handleUnsolicitedHeaders
       , mkHeadersRequest
       , requestHeaders

       , mkBlocksRequest
       , handleBlocks
       ) where

import           Control.Concurrent.STM     (isFullTBQueue, putTMVar, readTVar,
                                             tryReadTMVar, tryTakeTMVar, writeTBQueue,
                                             writeTVar)
import           Control.Lens               (_Wrapped)
import qualified Data.List.NonEmpty         as NE
import           Formatting                 (build, sformat, shown, stext, (%))
import           Mockable                   (fork)
import           Paths_cardano_sl           (version)
import           Serokell.Util.Text         (listJson)
import           Serokell.Util.Verify       (isVerSuccess)
import           System.Wlog                (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Block.Logic            (ClassifyHeaderRes (..),
                                             ClassifyHeadersRes (..), classifyHeaders,
                                             classifyNewHeader, getHeadersOlderExp,
                                             lcaWithMainChain, verifyAndApplyBlocks,
                                             withBlkSemaphore)
import qualified Pos.Block.Logic            as L
import           Pos.Block.Network.Announce (announceBlock)
import           Pos.Block.Network.Types    (MsgGetBlocks (..), MsgGetHeaders (..),
                                             MsgHeaders (..))
import           Pos.Block.Types            (Blund)
import           Pos.Communication.Limits   (LimitedLength, recvLimited, reifyMsgLimit)
import           Pos.Communication.Protocol (ConversationActions (..), NodeId, OutSpecs,
                                             SendActions (..), convH, toOutSpecs)
import           Pos.Constants              (blkSecurityParam)
import           Pos.Context                (NodeContext (..), getNodeContext,
                                             isRecoveryMode)
import           Pos.Crypto                 (shortHashF)
import qualified Pos.DB.DB                  as DB
import           Pos.DHT.Model              (converseToNeighbors)
import           Pos.Reporting.Methods      (reportMisbehaviourMasked)
import           Pos.Slotting               (getCurrentSlot)
import           Pos.Ssc.Class              (Ssc, SscWorkersClass)
import           Pos.Types                  (Block, BlockHeader, HasHeaderHash (..),
                                             HeaderHash, blockHeader, difficultyL,
                                             gbHeader, getEpochOrSlot, headerHashG,
                                             prevBlockL, verifyHeaders)
import           Pos.Util                   (NE, NewestFirst (..), OldestFirst (..),
                                             inAssertMode, _neHead, _neLast)
import           Pos.WorkMode               (WorkMode)



----------------------------------------------------------------------------
-- Recovery
----------------------------------------------------------------------------

needRecovery :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
       => Proxy ssc -> m Bool
needRecovery _ = getCurrentSlot >>= maybe (pure True) needRecoveryCheck
  where
    needRecoveryCheck slot = do
        header <- DB.getTipBlockHeader @ssc
        pure $
            toEnum (fromEnum (getEpochOrSlot header) + blkSecurityParam) <
            getEpochOrSlot slot

-- | Triggers recovery based on established communication.
triggerRecovery :: forall ssc m.
    (SscWorkersClass ssc, WorkMode ssc m)
    => SendActions m -> m ()
triggerRecovery sendActions = unlessM isRecoveryMode $ do
    logDebug "Recovery triggered, requesting tips"
    reifyMsgLimit (Proxy @(MsgHeaders ssc)) $ \limitProxy -> do
        converseToNeighbors sendActions (requestTip limitProxy) `catch`
            \(e :: SomeException) -> do
               logDebug ("Error happened in triggerRecovery" <> show e)
               throwM e
        logDebug "Recovery triggered ended"


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
requestTip _ peerId conv = do
    logDebug "Requesting tip..."
    send conv (MsgGetHeaders [] Nothing)
    whenJustM (recvLimited conv) handleTip
  where
    handleTip (MsgHeaders (NewestFirst (tip:|[]))) = do
        logDebug $ sformat ("Got tip "%shortHashF%", processing") (headerHash tip)
        handleUnsolicitedHeader tip peerId conv
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
handleUnsolicitedHeaders (header :| []) peerId conv =
    handleUnsolicitedHeader header peerId conv
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
                requestHeaders mgh peerId header Proxy conv
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

-- Second argument is mghTo block header (not hash). Don't pass it
-- only if you don't know it.
requestHeaders
    :: forall ssc s m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => MsgGetHeaders
    -> NodeId
    -> BlockHeader ssc
    -> Proxy s
    -> ConversationActions MsgGetHeaders (LimitedLength s (MsgHeaders ssc)) m
    -> m ()
requestHeaders mgh peerId origTip _ conv = do
    logDebug $ sformat ("requestHeaders: withConnection: sending "%build) mgh
    send conv mgh
    mHeaders <- recvLimited conv
    inRecovery <- needRecovery (Proxy @ssc)
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
            case nonEmpty headers' of
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
                    fromMaybe (error "Listeners#applyWithoutRollback is broken") $
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
            reportMisbehaviourMasked version $
                sformat reportF peerId toRollbackHashes toApplyHashes
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
relayBlock _ (Left _)                  = pass
relayBlock sendActions (Right mainBlk) = do
    -- Why 'ncPropagation' is not considered?
    unlessM isRecoveryMode $
        void $ fork $ announceBlock sendActions $ mainBlk ^. gbHeader

----------------------------------------------------------------------------
-- Common logging / logic sink points
----------------------------------------------------------------------------

data VerifyBlocksException = VerifyBlocksException Text deriving Show
instance Exception VerifyBlocksException

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
