{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Server.Listeners
       ( blockListeners
       ) where

import           Control.Lens                   (view, (^.), _1)
import           Data.List.NonEmpty             (NonEmpty ((:|)), nonEmpty)
import qualified Data.List.NonEmpty             as NE
import           Formatting                     (build, sformat, stext, (%))
import           Serokell.Util.Text             (listJson)
import           System.Wlog                    (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication       ()
import           Pos.Block.Logic                (ClassifyHeaderRes (..),
                                                 ClassifyHeadersRes (..), applyBlocks,
                                                 classifyHeaders, classifyNewHeader,
                                                 getHeadersFromManyTo,
                                                 getHeadersFromToIncl, lcaWithMainChain,
                                                 rollbackBlocks, verifyBlocks,
                                                 withBlkSemaphore_)
import           Pos.Block.Network.Announce     (announceBlock)
import           Pos.Block.Network.Request      (replyWithBlocksRequest,
                                                 replyWithHeadersRequest)
import           Pos.Block.Network.Server.State (ProcessBlockMsgRes (..),
                                                 matchRequestedHeaders, processBlockMsg)
import           Pos.Block.Network.Types        (MsgBlock (..), MsgGetBlocks (..),
                                                 MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Types        (MutSocketState, ResponseMode)
import           Pos.Crypto                     (hash, shortHashF)
import qualified Pos.DB                         as DB
import           Pos.DB.Error                   (DBError (DBMalformed))
import           Pos.DHT.Model                  (ListenerDHT (..), MonadDHTDialog,
                                                 getUserState, replyToNode)
import           Pos.Types                      (Block, BlockHeader, Blund,
                                                 HasHeaderHash (..), HeaderHash,
                                                 blockHeader, gbHeader, prevBlockL)
import           Pos.Util                       (inAssertMode, _neHead, _neLast)
import           Pos.WorkMode                   (WorkMode)

-- | Listeners for requests related to blocks processing.
blockListeners
    :: (MonadDHTDialog (MutSocketState ssc) m, WorkMode ssc m)
    => [ListenerDHT (MutSocketState ssc) m]
blockListeners =
    [ ListenerDHT handleGetHeaders
    , ListenerDHT handleGetBlocks
    , ListenerDHT handleBlockHeaders
    , ListenerDHT handleBlock
    ]

-- | Handles GetHeaders request which means client wants to get
-- headers from some checkpoints that are older than optional @to@
-- field.
handleGetHeaders
    :: forall ssc m.
       (ResponseMode ssc m)
    => MsgGetHeaders ssc -> m ()
handleGetHeaders MsgGetHeaders {..} = do
    logDebug "Got request on handleGetHeaders"
    rhResult <- getHeadersFromManyTo mghFrom mghTo
    case nonEmpty rhResult of
        Nothing ->
            logWarning $
            "handleGetHeaders@retrieveHeadersFromTo returned empty " <>
            "list, not responding to node"
        Just ne -> replyToNode $ MsgHeaders ne

handleGetBlocks
    :: forall ssc m.
       (ResponseMode ssc m)
    => MsgGetBlocks ssc -> m ()
handleGetBlocks MsgGetBlocks {..} = do
    logDebug "Got request on handleGetBlocks"
    hashes <- getHeadersFromToIncl mgbFrom mgbTo
    maybe warn sendBlocks hashes
  where
    warn = logWarning $ "getBLocksByHeaders@retrieveHeaders returned Nothing"
    failMalformed =
        throwM $ DBMalformed $
        "hadleGetBlocks: getHeadersFromToIncl returned header that doesn't " <>
        "have corresponding block in storage."
    sendBlocks hashes = do
        logDebug $ sformat
            ("handleGetBlocks: started sending blocks one-by-one: "%listJson) hashes
        forM_ hashes $ \hHash -> do
            block <- maybe failMalformed pure =<< DB.getBlock hHash
            replyToNode $ MsgBlock block
        logDebug "handleGetBlocks: blocks sending done"

-- | Handles MsgHeaders request. There are two usecases:
--
-- * If we've requested MsgGetHeaders, that's a response
-- * If we didn't, probably somebody wanted to share the block (e.g. new one)
handleBlockHeaders
    :: forall ssc m.
       (ResponseMode ssc m)
    => MsgHeaders ssc -> m ()
handleBlockHeaders (MsgHeaders headers) = do
    logDebug "handleBlockHeaders: got some block headers"
    ifM (matchRequestedHeaders headers =<< getUserState)
        (handleRequestedHeaders headers)
        (handleUnsolicitedHeaders headers)

-- First case of 'handleBlockheaders'
handleRequestedHeaders
    :: forall ssc m.
       (ResponseMode ssc m)
    => NonEmpty (BlockHeader ssc) -> m ()
handleRequestedHeaders headers = do
    logDebug "handleRequestedHeaders: headers were requested, will process"
    classificationRes <- classifyHeaders headers
    let newestHeader = headers ^. _neHead
        newestHash = headerHash newestHeader
        oldestHash = headerHash $ headers ^. _neLast
    case classificationRes of
        CHsValid lcaChild -> do
            let lcaChildHash = hash lcaChild
            logDebug $ sformat validFormat lcaChildHash newestHash
            replyWithBlocksRequest lcaChildHash newestHash
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
       (ResponseMode ssc m)
    => NonEmpty (BlockHeader ssc) -> m ()
handleUnsolicitedHeaders (header :| []) = handleUnsolicitedHeader header
-- TODO: ban node for sending more than one unsolicited header.
handleUnsolicitedHeaders (h:|hs)        = do
    logWarning "Someone sent us nonzero amount of headers we didn't expect"
    logWarning $ sformat ("Here they are: "%listJson) (h:hs)

handleUnsolicitedHeader
    :: forall ssc m.
       (ResponseMode ssc m)
    => BlockHeader ssc -> m ()
handleUnsolicitedHeader header = do
    logDebug "handleUnsolicitedHeader: single header was propagated, processing"
    classificationRes <- classifyNewHeader header
    -- TODO: should we set 'To' hash to hash of header or leave it unlimited?
    case classificationRes of
        CHContinues -> do
            logDebug $ sformat continuesFormat hHash
            replyWithBlocksRequest hHash hHash -- exactly one block in the range
        CHAlternative -> do
            logInfo $ sformat alternativeFormat hHash
            replyWithHeadersRequest (Just hHash)
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

----------------------------------------------------------------------------
-- Handle Block
----------------------------------------------------------------------------

-- | Handle MsgBlock request. That's a response for @mkBlocksRequest@.
handleBlock
    :: forall ssc m.
       (ResponseMode ssc m)
    => MsgBlock ssc -> m ()
handleBlock msg@(MsgBlock blk) = do
    logDebug $ sformat ("handleBlock: got block "%build) (headerHash blk)
    pbmr <- processBlockMsg msg =<< getUserState
    case pbmr of
        -- [CSL-335] Process intermediate blocks ASAP.
        PBMintermediate -> do
            logDebug $ sformat intermediateFormat (headerHash blk)
        PBMfinal blocks -> do
            logDebug "handleBlock: it was final block, launching handleBlocks"
            handleBlocks blocks
        PBMunsolicited ->
            -- TODO: ban node for sending unsolicited block.
            logDebug "handleBlock: got unsolicited"
  where
    intermediateFormat = "Received intermediate block " %shortHashF

handleBlocks
    :: forall ssc m.
       (ResponseMode ssc m)
    => NonEmpty (Block ssc) -> m ()
-- Head block is the oldest one here.
handleBlocks blocks = do
    logDebug "handleBlocks: processing"
    inAssertMode $
        logDebug $
        sformat ("Processing sequence of blocks: " %listJson % "…") $
        fmap headerHash blocks
    maybe onNoLca (handleBlocksWithLca blocks) =<<
        lcaWithMainChain (map (view blockHeader) $ NE.reverse blocks)
    inAssertMode $ logDebug $ "Finished processing sequence of blocks"
  where
    onNoLca = logWarning $
        "Sequence of blocks can't be processed, because there is no LCA. " <>
        "Probably rollback happened in parallel"

handleBlocksWithLca :: forall ssc m.
       (ResponseMode ssc m)
    => NonEmpty (Block ssc) -> HeaderHash ssc -> m ()
handleBlocksWithLca blocks lcaHash = do
    logDebug $ sformat lcaFmt lcaHash
    -- Head block in result is the newest one.
    toRollback <- DB.loadBlocksFromTipWhile $ \blk _ -> headerHash blk /= lcaHash
    maybe (applyWithoutRollback blocks)
          (applyWithRollback blocks lcaHash)
          (nonEmpty toRollback)
  where
    lcaFmt = "Handling block w/ LCA, which is "%shortHashF

applyWithoutRollback
    :: forall ssc m.
       (ResponseMode ssc m)
    => NonEmpty (Block ssc) -> m ()
applyWithoutRollback blocks = do
    logDebug $ sformat ("Trying to apply blocks w/o rollback: "%listJson)
        (map (view blockHeader) blocks)
    verRes <- verifyBlocks blocks
    either
        onFailedVerifyBlocks
        (withBlkSemaphore_ . applyWithoutRollbackDo . NE.zip blocks)
        verRes
    logDebug "Finished applying blocks w/o rollback"
  where
    oldestToApply = blocks ^. _neHead
    assumedTip = oldestToApply ^. prevBlockL
    newTip = blocks ^. _neLast . headerHashG
    applyWithoutRollbackDo :: NonEmpty (Blund ssc)
                           -> HeaderHash ssc
                           -> m (HeaderHash ssc)
    applyWithoutRollbackDo blunds tip
        | tip /= assumedTip =
            tip <$ logWarning (tipMismatchMsg "apply" tip assumedTip)
        | otherwise = newTip <$ do
            applyBlocks blunds
            logInfo $ blocksAppliedMsg @ssc blunds
            relayBlock $ fst $ NE.last blunds

applyWithRollback
    :: forall ssc m.
       (ResponseMode ssc m)
    => NonEmpty (Block ssc) -> HeaderHash ssc -> NonEmpty (Blund ssc) -> m ()
applyWithRollback toApply lca toRollback = do
    logDebug $ sformat ("Trying to apply blocks w/ rollback: "%listJson)
        (map (view blockHeader) toApply)
    logDebug $
        sformat ("Blocks to rollback "%listJson) (fmap headerHash toRollback)
    withBlkSemaphore_ applyWithRollbackDo
    logDebug "Finished applying blocks w/ rollback"
  where
    newestToRollback = toRollback ^. _neHead . _1 . prevBlockL
    newTip = toApply ^. _neLast . headerHashG
    panicBrokenLca = panic "applyWithRollback: nothing after LCA :/"
    toApplyAfterLca =
        fromMaybe panicBrokenLca $ NE.nonEmpty $
        NE.dropWhile ((lca /=) . (^. prevBlockL)) toApply
    applyWithRollbackDo :: HeaderHash ssc -> m (HeaderHash ssc)
    applyWithRollbackDo tip
        | tip /= newestToRollback =
            tip <$ logWarning (tipMismatchMsg "rollback" tip newestToRollback)
        | otherwise = do
            rollbackBlocks toRollback
            logInfo $ blocksRolledBackMsg toRollback
            verRes <- verifyBlocks toApplyAfterLca
            case verRes of
                Right undos -> newTip <$ do
                    applyBlocks (NE.zip toApplyAfterLca undos)
                    logInfo $ blocksAppliedMsg toApplyAfterLca
                    relayBlock $ NE.last toApplyAfterLca
                Left errors -> tip <$ do
                    onFailedVerifyBlocks errors
                    logDebug "Applying rollbacked blocks…"
                    applyBlocks toRollback
                    logDebug "Finished applying rollback blocks"

relayBlock
    :: forall ssc m.
       (WorkMode ssc m)
    => Block ssc -> m ()
relayBlock (Left _)        = pass
relayBlock (Right mainBlk) = announceBlock $ mainBlk ^. gbHeader

----------------------------------------------------------------------------
-- Logging formats
----------------------------------------------------------------------------

-- TODO: ban node for it!
onFailedVerifyBlocks
    :: forall ssc m.
       (ResponseMode ssc m)
    => Text -> m ()
onFailedVerifyBlocks = logWarning . sformat ("Failed to verify blocks: " %stext)

tipMismatchMsg :: Text -> HeaderHash ssc -> HeaderHash ssc -> Text
tipMismatchMsg action storedTip attemptedTip =
    sformat
        ("Can't "%stext%" block because of tip mismatch (stored is "
         %shortHashF%", attempted is "%shortHashF%")")
        action storedTip attemptedTip

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
