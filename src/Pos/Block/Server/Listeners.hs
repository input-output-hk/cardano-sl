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
import           Pos.Block.Logic          (ClassifyHeaderRes (..), classifyNewHeader)
import           Pos.Block.Requests       (replyWithBlockRequest)
import           Pos.Communication.Types  (MsgBlock (..), MsgGetBlocks (..),
                                           MsgGetHeaders (..), MsgHeaders (..),
                                           MutSocketState, ResponseMode)
import           Pos.Crypto               (shortHashF)
import           Pos.DHT.Model            (ListenerDHT (..), MonadDHTDialog)
import           Pos.Types                (BlockHeader, headerHash, prevBlockL)
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
    -- TODO: decide what to do depending on socket state
    handleUnsolicitedHeaders headers

handleUnsolicitedHeaders
    :: forall ssc m.
       (ResponseMode ssc m)
    => NonEmpty (BlockHeader ssc) -> m ()
handleUnsolicitedHeaders (header :| []) = do
    classificationRes <- classifyNewHeader header
    case classificationRes of
        CHRcontinues ->
            replyWithBlockRequest (header ^. prevBlockL) (headerHash header)
        CHRalternative -> pass -- TODO: request multiple blocks or headers, dunno
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
