{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Server which deals with blocks processing.

module Pos.Block.Server.Listeners
       ( blockListeners
       ) where

import           Control.Lens             ((^.))
import           Data.List.NonEmpty       (NonEmpty ((:|)))
import           Formatting               (sformat, stext, (%))
import           System.Wlog              (logDebug)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Logic          (ClassifyHeaderRes (..), classifyHeaders,
                                           classifyNewHeader)
import           Pos.Block.Requests       (replyWithBlocksRequest,
                                           replyWithHeadersRequest)
import           Pos.Block.Server.State   (matchRequestedHeaders)
import           Pos.Communication.Types  (MsgBlock (..), MsgGetBlocks (..),
                                           MsgGetHeaders (..), MsgHeaders (..),
                                           MutSocketState, ResponseMode)
import           Pos.Crypto               (shortHashF)
import           Pos.DHT.Model            (ListenerDHT (..), MonadDHTDialog, getUserState)
import           Pos.Types                (BlockHeader, headerHash, prevBlockL)
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
handleBlock (MsgBlock _) = pass
