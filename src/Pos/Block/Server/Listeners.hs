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
import qualified Data.Text                as T
import           Formatting               (sformat, stext, (%))
import           Serokell.Util.Verify     (VerificationRes (..))
import           System.Wlog              (logDebug, logWarning)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Logic          (ClassifyHeaderRes (..), applyBlocks,
                                           classifyHeaders, classifyNewHeader,
                                           rollbackBlocks, verifyBlocks, withBlkSemaphore)
import           Pos.Block.Requests       (replyWithBlocksRequest,
                                           replyWithHeadersRequest)
import           Pos.Block.Server.State   (ProcessBlockMsgRes (..), matchRequestedHeaders,
                                           processBlockMsg)
import           Pos.Communication.Types  (MsgBlock (..), MsgGetBlocks (..),
                                           MsgGetHeaders (..), MsgHeaders (..),
                                           MutSocketState, ResponseMode)
import           Pos.Crypto               (shortHashF)
import           Pos.DHT.Model            (ListenerDHT (..), MonadDHTDialog, getUserState)
import           Pos.Types                (Block, BlockHeader, HeaderHash, Undo,
                                           headerHash, headerHashG, prevBlockL)
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
handleGetHeaders MsgGetHeaders {..} = pass

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
    classificationRes <- classifyHeaders $ toList headers
    let startHeader = headers ^. _neHead
        startHash = headerHash startHeader
        endHeader = headers ^. _neLast
        endHash = headerHash endHeader
    case classificationRes of
        CHRcontinues ->
            replyWithBlocksRequest startHash endHash
        CHRalternative -> do
            lcaChild <- undefined
            replyWithBlocksRequest lcaChild endHash
        CHRuseless reason ->
            logDebug $
            sformat
                ("Chain of headers from " %shortHashF % " to " %shortHashF %
                 " is useless for the following reason: " %stext)
                startHash endHash reason
        CHRinvalid _ -> pass -- TODO: ban node for sending invalid block.

handleUnsolicitedHeaders
    :: forall ssc m.
       (ResponseMode ssc m)
    => NonEmpty (BlockHeader ssc) -> m ()
handleUnsolicitedHeaders (header :| []) = do
    classificationRes <- classifyNewHeader header
    -- TODO: should we set 'To' hash to hash of header or leave it unlimited?
    case classificationRes of
        CHRcontinues ->
            replyWithBlocksRequest (header ^. prevBlockL) (headerHash header)
        CHRalternative -> replyWithHeadersRequest (Just $ headerHash header)
        CHRuseless reason ->
            logDebug $
            sformat
                ("Header " %shortHashF %
                 " is useless for the following reason: " %stext)
                (headerHash header)
                reason
        CHRinvalid _ -> pass -- TODO: ban node for sending invalid block.
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
    -- TODO: find LCA. Head block in result is the newest one.
    toRollback <- undefined blocks
    case nonEmpty toRollback of
        Nothing           -> whenNoRollback
        Just toRollbackNE -> withBlkSemaphore $ whenRollback toRollbackNE
  where
    newTip = blocks ^. _neLast . headerHashG
    whenNoRollback :: m ()
    whenNoRollback = do
        verRes <- verifyBlocks blocks
        case verRes of
            VerSuccess        -> withBlkSemaphore whenNoRollbackDo
            VerFailure errors -> reportErrors errors
    whenNoRollbackDo :: HeaderHash ssc -> m (HeaderHash ssc)
    whenNoRollbackDo tip
        | tip /= blocks ^. _neHead . headerHashG = pure tip
        | otherwise = newTip <$ applyBlocks blocks
    whenRollback :: NonEmpty (Block ssc, Undo)
                 -> HeaderHash ssc
                 -> m (HeaderHash ssc)
    whenRollback toRollback tip
        | tip /= toRollback ^. _neHead . _1 . headerHashG = pure tip
        | otherwise = do
            rollbackBlocks toRollback
            verRes <- verifyBlocks blocks
            case verRes of
                VerSuccess -> newTip <$ applyBlocks blocks
                VerFailure errors ->
                    reportErrors errors >> applyBlocks (fmap fst toRollback) $>
                    tip
    -- TODO: ban node on error!
    reportErrors =
        logWarning . sformat ("Failed to verify blocks: " %stext) .
        T.intercalate ";"
