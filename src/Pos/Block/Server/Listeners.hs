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
import           System.Wlog              (logDebug, logWarning)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Logic          (ClassifyHeaderRes (..),
                                           ClassifyHeadersRes (..), applyBlocks,
                                           classifyHeaders, classifyNewHeader,
                                           lcaWithMainChain, retrieveHeadersFromTo,
                                           rollbackBlocks, verifyBlocks, withBlkSemaphore)
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
import           Pos.Util                 (_neHead, _neLast)
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

handleGetHeaders
    :: forall ssc m.
       (ResponseMode ssc m)
    => MsgGetHeaders ssc -> m ()
handleGetHeaders MsgGetHeaders {..} = do
    rhResult <- retrieveHeadersFromTo mghFrom mghTo
    case nonEmpty rhResult of
        Nothing ->
            logWarning "retrieveHeadersFromTo returned empty list, not responding to node"
        Just ne -> replyToNode $ MsgHeaders ne

handleGetBlocks
    :: forall ssc m.
       (ResponseMode ssc m)
    => MsgGetBlocks ssc -> m ()
handleGetBlocks MsgGetBlocks {..} = pass

handleBlockHeaders
    :: forall ssc m.
       (ResponseMode ssc m)
    => MsgHeaders ssc -> m ()
handleBlockHeaders (MsgHeaders headers) = do
    ifM (matchRequestedHeaders headers =<< getUserState)
        (handleRequestedHeaders headers)
        (handleUnsolicitedHeaders headers)

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
        CHsValid lca -> replyWithBlocksRequest (hash lca) newestHash
        CHsUseless reason ->
            logDebug $
            sformat
                ("Chain of headers from "%shortHashF%" to "%shortHashF%
                 " is useless for the following reason: "%stext)
                oldestHash newestHash reason
        CHsInvalid _ -> pass -- TODO: ban node for sending invalid block.

handleUnsolicitedHeaders
    :: forall ssc m.
       (ResponseMode ssc m)
    => NonEmpty (BlockHeader ssc) -> m ()
handleUnsolicitedHeaders (header :| []) = do
    classificationRes <- classifyNewHeader header
    -- TODO: should we set 'To' hash to hash of header or leave it unlimited?
    case classificationRes of
        CHContinues ->
            replyWithBlocksRequest (header ^. prevBlockL) (headerHash header)
        CHAlternative -> replyWithHeadersRequest (Just $ headerHash header)
        CHUseless reason ->
            logDebug $
            sformat
                ("Header " %shortHashF %
                 " is useless for the following reason: " %stext)
                (headerHash header)
                reason
        CHInvalid _ -> pass -- TODO: ban node for sending invalid block.
-- TODO: ban node for sending more than one unsolicited header.
handleUnsolicitedHeaders _ = pass

handleBlock
    :: forall ssc m.
       (ResponseMode ssc m)
    => MsgBlock ssc -> m ()
handleBlock msg = do
    pbmr <- processBlockMsg msg =<< getUserState
    case pbmr of
        -- [CSL-335] Process intermediate blocks ASAP.
        PBMintermediate -> pass
        PBMfinal blocks -> handleBlocks blocks
        PBMunsolicited  -> pass -- TODO: ban node for sending unsolicited block.

handleBlocks
    :: forall ssc m.
       (ResponseMode ssc m)
    => NonEmpty (Block ssc) -> m ()
-- Head block is the oldest one here.
handleBlocks blocks = do
    lcaHashMb <- lcaWithMainChain $ map (^. blockHeader) $ NE.reverse blocks
    maybe (panic "Invalid blocks, LCA not found")
        (\lcaHash -> do
            tip <- getTip
            -- Head block in result is the newest one.
            toRollback <- loadBlocksWithUndoWhile tip ((lcaHash /= ). headerHash)
            case nonEmpty toRollback of
                Nothing           -> whenNoRollback
                Just toRollbackNE -> withBlkSemaphore $ whenRollback toRollbackNE lcaHash
        ) lcaHashMb
  where
    newTip = blocks ^. _neLast . headerHashG
    whenNoRollback :: m ()
    whenNoRollback = do
        verRes <- verifyBlocks blocks
        case verRes of
            Right undos -> withBlkSemaphore $ whenNoRollbackDo (NE.zip blocks undos)
            Left errors -> reportError errors
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
                          reportError errors >> applyBlocks toRollback $>
                          tip
                ) (nonEmpty newBlocksList)
    -- TODO: ban node on error!
    reportError =
        logWarning . sformat ("Failed to verify blocks: " %stext)
