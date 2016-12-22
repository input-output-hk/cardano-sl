{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Server which deals with blocks processing.

module Pos.Block.Server.Listeners
       ( blockListeners
       ) where

import           Control.Lens             ((^.), _1)
import           Data.List.NonEmpty       (NonEmpty ((:|)), nonEmpty)
import qualified Data.List.NonEmpty       as NE
import           Formatting               (sformat, stext, (%))
import           Serokell.Util.Text       (listJson)
import           System.Wlog              (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Logic          (ClassifyHeaderRes (..),
                                           ClassifyHeadersRes (..), applyBlocks,
                                           classifyHeaders, classifyNewHeader,
                                           getBlocksByHeaders, lcaWithMainChain,
                                           retrieveHeadersFromTo, rollbackBlocks,
                                           verifyBlocks, withBlkSemaphore)
import           Pos.Block.Requests       (replyWithBlocksRequest,
                                           replyWithHeadersRequest)
import           Pos.Block.Server.State   (ProcessBlockMsgRes (..), matchRequestedHeaders,
                                           processBlockMsg)
import           Pos.Communication.Types  (MsgBlock (..), MsgGetBlocks (..),
                                           MsgGetHeaders (..), MsgHeaders (..),
                                           MutSocketState, ResponseMode)
import           Pos.Crypto               (hash, shortHashF)
import           Pos.DB                   (getTip, loadBlocksWithUndoWhile)
import           Pos.DHT.Model            (ListenerDHT (..), MonadDHTDialog, getUserState,
                                           replyToNode)
import           Pos.Types                (Block, BlockHeader, HeaderHash, Undo,
                                           blockHeader, headerHash, headerHashG,
                                           prevBlockL)
import           Pos.Util                 (inAssertMode, _neHead, _neLast)
import           Pos.WorkMode             (WorkMode)

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
    rhResult <- retrieveHeadersFromTo mghFrom mghTo
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
    blocks <- getBlocksByHeaders mgbFrom mgbTo
    maybe warn sendMsg blocks
  where
    warn = logWarning $ "getBLocksByHeaers@retrieveHeaders returned Nothing"
    sendMsg blocksToSend = do
        logDebug "handleGetBlocks: started sending blocks one-by-one"
        forM_ blocksToSend $ replyToNode . MsgBlock
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
    ifM (matchRequestedHeaders headers =<< getUserState)
        (handleRequestedHeaders headers)
        (handleUnsolicitedHeaders headers)

-- First case of 'handleBlockheaders'
handleRequestedHeaders
    :: forall ssc m.
       (ResponseMode ssc m)
    => NonEmpty (BlockHeader ssc) -> m ()
handleRequestedHeaders headers = do
    classificationRes <- classifyHeaders headers
    let newestHeader = headers ^. _neHead
        newestHash = headerHash newestHeader
        oldestHeader = headers ^. _neLast
        oldestHash = headerHash oldestHeader
    case classificationRes of
        CHsValid lcaChild -> do
            let lcaHash = hash lcaChild
            logDebug $ sformat validFormat lcaHash newestHash
            replyWithBlocksRequest lcaHash newestHash
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
handleUnsolicitedHeaders _              = pass

handleUnsolicitedHeader
    :: forall ssc m.
       (ResponseMode ssc m)
    => BlockHeader ssc -> m ()
handleUnsolicitedHeader header = do
    classificationRes <- classifyNewHeader header
    -- TODO: should we set 'To' hash to hash of header or leave it unlimited?
    case classificationRes of
        CHContinues -> do
            logDebug $ sformat continuesFormat hHash
            replyWithBlocksRequest (header ^. prevBlockL) hHash
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
        "potentially represents good alternative chain, requesting more headers"
    uselessFormat =
        "Header " %shortHashF % " is useless for the following reason: " %stext

-- | Handle MsgBlock request. That's a response for @mkBlocksRequest@.
handleBlock
    :: forall ssc m.
       (ResponseMode ssc m)
    => MsgBlock ssc -> m ()
handleBlock msg@(MsgBlock blk) = do
    pbmr <- processBlockMsg msg =<< getUserState
    case pbmr of
        -- [CSL-335] Process intermediate blocks ASAP.
        PBMintermediate ->
            logDebug $ sformat intermediateFormat (headerHash blk)
        PBMfinal blocks -> handleBlocks blocks
        PBMunsolicited -> pass -- TODO: ban node for sending unsolicited block.
  where
    intermediateFormat = "Received intermediate block " %shortHashF

handleBlocks
    :: forall ssc m.
       (ResponseMode ssc m)
    => NonEmpty (Block ssc) -> m ()
-- Head block is the oldest one here.
handleBlocks blocks = do
    inAssertMode $ logDebug $ sformat ("Processing sequence of blocks: "%listJson) $ fmap headerHash blocks
    lcaHashMb <- lcaWithMainChain $ map (^. blockHeader) $ NE.reverse blocks
    let lcaHash = fromMaybe (panic "Invalid blocks, LCA not found") lcaHashMb
    tip <- getTip
    -- Head block in result is the newest one.
    toRollback <- loadBlocksWithUndoWhile tip ((lcaHash /= ). headerHash)
    case nonEmpty toRollback of
        Nothing           -> whenNoRollback
        Just toRollbackNE -> withBlkSemaphore $ whenRollback toRollbackNE lcaHash
  where
    newTip = blocks ^. _neLast . headerHashG
    whenNoRollback :: m ()
    whenNoRollback = do
        verRes <- verifyBlocks blocks
        case verRes of
            Right undos -> withBlkSemaphore $ whenNoRollbackDo (NE.zip blocks undos)
            Left errors -> onFailedVerify errors
    whenNoRollbackDo :: NonEmpty (Block ssc, Undo) -> HeaderHash ssc -> m (HeaderHash ssc)
    whenNoRollbackDo blund tip
        | tip /= blund ^. _neHead . _1 . headerHashG = pure tip
        | otherwise = newTip <$ applyBlocks blund
    whenRollback :: NonEmpty (Block ssc, Undo)
                 -> HeaderHash ssc
                 -> HeaderHash ssc
                 -> m (HeaderHash ssc)
    whenRollback toRollback lca tip
        | tip /= toRollback ^. _neHead . _1 . headerHashG = pure tip
        | otherwise = do
            rollbackBlocks toRollback
            let newBlocksList = NE.dropWhile ((lca /=) . (^. prevBlockL)) blocks
            maybe (panic "No new blocks")
                (\newBlocks -> do
                  verRes <- verifyBlocks newBlocks
                  case verRes of
                      Right undos -> newTip <$ applyBlocks (NE.zip newBlocks undos)
                      Left errors ->
                          onFailedVerify errors >> applyBlocks toRollback $>
                          tip
                ) (nonEmpty newBlocksList)

-- TODO: ban node for it!
onFailedVerify
    :: forall ssc m.
       (ResponseMode ssc m)
    => Text -> m ()
onFailedVerify = logWarning . sformat ("Failed to verify blocks: " %stext)
