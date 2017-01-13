{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Server.Listeners
       ( blockListeners
       , blockStubListeners
       ) where

import           Control.Lens                   (view, (^.))
import           Data.List.NonEmpty             (NonEmpty ((:|)), nonEmpty)
import qualified Data.List.NonEmpty             as NE
import           Data.Proxy                     (Proxy (..))
import           Formatting                     (build, sformat, shown, stext, (%))
import           Mockable                       (fork)
import           Node                           (ConversationActions (..),
                                                 ListenerAction (..), NodeId (..),
                                                 SendActions (..), sendTo)
import           Serokell.Util.Text             (listJson)
import           System.Wlog                    (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication       ()
import           Pos.Block.Logic                (ClassifyHeaderRes (..),
                                                 ClassifyHeadersRes (..))
import qualified Pos.Block.Logic                as L
import           Pos.Block.Network.Announce     (announceBlock)
import           Pos.Block.Network.Request      (mkHeadersRequest, replyWithBlocksRequest)
import           Pos.Block.Network.Server.State (ProcessBlockMsgRes (..),
                                                 matchRequestedHeaders, processBlockMsg)
import           Pos.Block.Network.Types        (InConv (..), MsgBlock (..),
                                                 MsgGetBlocks (..), MsgGetHeaders (..),
                                                 MsgHeaders (..))
import           Pos.Communication.BiP          (BiP (..))
import           Pos.Communication.PeerState    (WithPeerState (..))
import           Pos.Crypto                     (hash, shortHashF)
import qualified Pos.DB                         as DB
import           Pos.DB.Error                   (DBError (DBMalformed))
import           Pos.DHT.Model                  (nodeIdToAddress)
import           Pos.Ssc.Class                  (SscWorkersClass (..))
import           Pos.Types                      (Block, BlockHeader, Blund,
                                                 HasHeaderHash (..), HeaderHash, NEBlocks,
                                                 blockHeader, gbHeader, prevBlockL)
import           Pos.Util                       (inAssertMode, _neHead, _neLast)
import           Pos.Util                       (stubListenerConv, stubListenerOneMsg)
import           Pos.WorkMode                   (WorkMode)

blockListeners
    :: ( WorkMode ssc m, SscWorkersClass ssc )
    => [ListenerAction BiP m]
blockListeners =
    [ handleGetHeaders
    , handleGetBlocks
    , handleBlockHeaders
    , handleBlock
    ]

blockStubListeners
    :: ( Monad m )
    => Proxy ssc -> [ListenerAction BiP m]
blockStubListeners p =
    [ stubListenerConv $ (const Proxy :: Proxy ssc -> Proxy (MsgGetHeaders ssc)) p
    , stubListenerOneMsg $ (const Proxy :: Proxy ssc -> Proxy (MsgGetBlocks ssc)) p
    --, stubListenerOneMsg $ (const Proxy :: Proxy ssc -> Proxy (MsgHeaders ssc)) p
    --, stubListenerOneMsg $ (const Proxy :: Proxy ssc -> Proxy (MsgBlock ssc)) p
    ]

-- | Handles GetHeaders request which means client wants to get
-- headers from some checkpoints that are older than optional @to@
-- field.
handleGetHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => ListenerAction BiP m
handleGetHeaders = ListenerActionConversation $
    \_ _ conv -> do
        (msg :: Maybe (MsgGetHeaders ssc)) <- recv conv
        whenJust msg $ \(MsgGetHeaders {..}) -> do
            logDebug "Got request on handleGetHeaders"
            rhResult <- L.getHeadersFromManyTo mghFrom mghTo
            case nonEmpty rhResult of
                Nothing ->
                    logWarning $
                        "handleGetHeaders@retrieveHeadersFromTo returned empty " <>
                        "list, not responding to node"
                Just ne -> send conv $ MsgHeaders ne

handleGetBlocks
    :: forall ssc m.
       (WorkMode ssc m)
    => ListenerAction BiP m
handleGetBlocks = ListenerActionOneMsg $
    \peerId sendActions (MsgGetBlocks {..} :: MsgGetBlocks ssc) -> do
        logDebug "Got request on handleGetBlocks"
        hashes <- L.getHeadersFromToIncl mgbFrom mgbTo
        maybe warn (sendBlocks sendActions peerId) hashes
  where
    warn = logWarning $ "getBLocksByHeaders@retrieveHeaders returned Nothing"
    failMalformed =
        throwM $ DBMalformed $
        "hadleGetBlocks: getHeadersFromToIncl returned header that doesn't " <>
        "have corresponding block in storage."
    sendBlocks sendActions peerId hashes = do
        logDebug $ sformat
            ("handleGetBlocks: started sending blocks one-by-one: "%listJson) hashes
        forM_ hashes $ \hHash -> do
            block <- maybe failMalformed pure =<< DB.getBlock hHash
            sendTo sendActions peerId $ MsgBlock block
        logDebug "handleGetBlocks: blocks sending done"


-- First case of 'handleBlockheaders'
handleRequestedHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => NonEmpty (BlockHeader ssc)
    -> NodeId
    -> SendActions BiP m
    -> m ()
handleRequestedHeaders headers peerId sendActions = do
    logDebug "handleRequestedHeaders: headers were requested, will process"
    classificationRes <- L.classifyHeaders headers
    let newestHeader = headers ^. _neHead
        newestHash = headerHash newestHeader
        oldestHash = headerHash $ headers ^. _neLast
    case classificationRes of
        CHsValid lcaChild -> do
            let lcaChildHash = hash lcaChild
            logDebug $ sformat validFormat lcaChildHash newestHash
            replyWithBlocksRequest lcaChildHash newestHash peerId sendActions
        CHsUseless reason ->
            logDebug $ sformat uselessFormat oldestHash newestHash reason
        CHsInvalid _ -> pass -- TODO: ban node for sending invalid block.
  where
    validFormat =
        "Received valid headers, requesting blocks from " %shortHashF % " to " %shortHashF
    uselessFormat =
        "Chain of headers from " %shortHashF % " to " %shortHashF %
        " is useless for the following reason: " %stext

-- Second case of 'handleBlockheaders'
handleUnsolicitedHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => NonEmpty (BlockHeader ssc)
    -> NodeId
    -> SendActions BiP m
    -> m ()
handleUnsolicitedHeaders (header :| []) peerId sendActions =
    handleUnsolicitedHeader header peerId sendActions
-- TODO: ban node for sending more than one unsolicited header.
handleUnsolicitedHeaders (h:|hs) _ _ = do
    logWarning "Someone sent us nonzero amount of headers we didn't expect"
    logWarning $ sformat ("Here they are: "%listJson) (h:hs)

handleUnsolicitedHeader
    :: forall ssc m.
       (WorkMode ssc m)
    => BlockHeader ssc
    -> NodeId
    -> SendActions BiP m
    -> m ()
handleUnsolicitedHeader header peerId sendActions = do
    logDebug "handleUnsolicitedHeader: single header was propagated, processing"
    classificationRes <- L.classifyNewHeader header
    -- TODO: should we set 'To' hash to hash of header or leave it unlimited?
    case classificationRes of
        CHContinues -> do
            logDebug $ sformat continuesFormat hHash
            replyWithBlocksRequest hHash hHash peerId sendActions -- exactly one block in the range
        CHAlternative -> do
            logInfo $ sformat alternativeFormat hHash
            mgh <- mkHeadersRequest (Just hHash)
            withConnectionTo sendActions peerId $ \conv -> do
                logDebug "handleUnsolicitedHeader: withConnection: sending MsgGetHeaders"
                send conv mgh
                logDebug "handleUnsolicitedHeader: withConnection: receiving MsgHeaders"
                mHeaders <- fmap inConvMsg <$> recv conv
                logDebug "handleUnsolicitedHeader: withConnection: received MsgHeaders"
                whenJust mHeaders $ \headers -> do
                    logDebug "handleUnsolicitedHeader: got some block headers"
                    if matchRequestedHeaders headers mgh
                       then handleRequestedHeaders headers peerId sendActions
                       else handleUnexpected headers peerId
        CHUseless reason -> logDebug $ sformat uselessFormat hHash reason
        CHInvalid _ -> pass -- TODO: ban node for sending invalid block.
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
    handleUnexpected (h:|hs) _ = do
        -- TODO: ban node for sending unsolicited header in conversation
        logWarning $ sformat
            ("handleUnsolicitedHeader: headers received were not requested, address: " % shown)
            (nodeIdToAddress peerId)
        logWarning $ sformat ("handleUnsolicitedHeader: unexpected headers: "%listJson) (h:hs)

-- | Handles MsgHeaders request, unsolicited usecase
handleBlockHeaders
    :: forall ssc m.
        (WorkMode ssc m)
    => ListenerAction BiP m
handleBlockHeaders = ListenerActionOneMsg $
    \peerId sendActions (MsgHeaders headers :: MsgHeaders ssc) -> do
        logDebug "handleBlockHeaders: got some unsolicited block header(s)"
        handleUnsolicitedHeaders headers peerId sendActions

----------------------------------------------------------------------------
-- Handle Block
----------------------------------------------------------------------------

handleBlock
    :: forall ssc m.
       (WorkMode ssc m, SscWorkersClass ssc)
    => ListenerAction BiP m
handleBlock = ListenerActionOneMsg $
    \peerId sendActions ((msg@(MsgBlock blk)) :: MsgBlock ssc) -> do
        logDebug $ sformat ("handleBlock: got block "%build) (headerHash blk)
        pbmr <- processBlockMsg msg =<< getPeerState peerId
        case pbmr of
            -- [CSL-335] Process intermediate blocks ASAP.
            PBMintermediate -> do
                logDebug $ sformat intermediateFormat (headerHash blk)
            PBMfinal blocks -> do
                logDebug "handleBlock: it was final block, launching handleBlocks"
                handleBlocks blocks peerId sendActions
            PBMunsolicited ->
                -- TODO: ban node for sending unsolicited block.
                logDebug "handleBlock: got unsolicited"
  where
    intermediateFormat = "Received intermediate block " %shortHashF

handleBlocks
    :: forall ssc m.
       (WorkMode ssc m, SscWorkersClass ssc)
    => NonEmpty (Block ssc)
    -> NodeId
    -> SendActions BiP m
    -> m ()
-- Head block is the oldest one here.
handleBlocks blocks _ sendActions = do
    logDebug "handleBlocks: processing"
    inAssertMode $
        logDebug $
        sformat ("Processing sequence of blocks: " %listJson % "…") $
        fmap headerHash blocks
    maybe onNoLca (handleBlocksWithLca sendActions blocks) =<<
        L.lcaWithMainChain (map (view blockHeader) $ NE.reverse blocks)
    inAssertMode $ logDebug $ "Finished processing sequence of blocks"
  where
    onNoLca = logWarning $
        "Sequence of blocks can't be processed, because there is no LCA. " <>
        "Probably rollback happened in parallel"

handleBlocksWithLca :: forall ssc m.
       (WorkMode ssc m, SscWorkersClass ssc)
    => SendActions BiP m -> NonEmpty (Block ssc) -> HeaderHash ssc -> m ()
handleBlocksWithLca sendActions blocks lcaHash = do
    logDebug $ sformat lcaFmt lcaHash
    -- Head block in result is the newest one.
    toRollback <- DB.loadBlocksFromTipWhile $ \blk _ -> headerHash blk /= lcaHash
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
    logDebug $ sformat ("Trying to apply blocks w/o rollback: "%listJson)
        (map (view blockHeader) blocks)
    L.withBlkSemaphore applyWithoutRollbackDo >>= \case
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
            logDebug $ blocksAppliedMsg applied
    logDebug "Finished applying blocks w/o rollback"
  where
    newestTip = blocks ^. _neLast . headerHashG
    applyWithoutRollbackDo
        :: HeaderHash ssc -> m (Either Text (HeaderHash ssc), HeaderHash ssc)
    applyWithoutRollbackDo curTip = do
        res <- L.verifyAndApplyBlocks False blocks
        let newTip = either (const curTip) identity res
        pure (res, newTip)

-- | Head of @toRollback@ - the youngest block.
applyWithRollback
    :: forall ssc m.
       (WorkMode ssc m, SscWorkersClass ssc)
    => SendActions BiP m -> NonEmpty (Block ssc) -> HeaderHash ssc -> NonEmpty (Blund ssc) -> m ()
applyWithRollback sendActions toApply lca toRollback = do
    logDebug $ sformat ("Trying to apply blocks w/ rollback: "%listJson)
        (map (view blockHeader) toApply)
    logDebug $
        sformat ("Blocks to rollback "%listJson) (fmap headerHash toRollback)
    res <- L.withBlkSemaphore $ \curTip -> do
        res <- L.applyWithRollback toRollback toApplyAfterLca
        pure (res, either (const curTip) identity res)
    case res of
        Left err -> logWarning $ "Couldn't apply blocks with rollback: " <> err
        Right newTip -> do
            logDebug $ sformat
                ("Finished applying blocks w/ rollback, relaying new tip: "%shortHashF)
                newTip
            logDebug $ blocksRolledBackMsg toRollback
            logDebug $ blocksAppliedMsg toApply
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
relayBlock sendActions (Right mainBlk) = void $ fork $ announceBlock sendActions $ mainBlk ^. gbHeader

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
