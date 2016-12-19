{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrappers for network requests.

module Pos.Block.Requests
       ( mkHeadersRequest
       , replyWithHeadersRequest
       , mkBlockRequest
       , replyWithBlockRequest
       ) where

import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Server.State   (recordBlocksRequest, recordHeadersRequest)
import           Pos.Communication.Types  (MsgGetBlocks (..), MsgGetHeaders (..),
                                           ResponseMode)
import           Pos.DHT.Model            (getUserState, replyToNode)
import           Pos.Types                (HeaderHash)
import           Pos.WorkMode             (WorkMode)

-- | Make 'GetHeaders' message using our main chain. This function
-- chooses appropriate 'from' hashes and puts them into 'GetHeaders'
-- message.
mkHeadersRequest
    :: WorkMode ssc m
    => Maybe (HeaderHash ssc) -> m (MsgGetHeaders ssc)
mkHeadersRequest _ = notImplemented

replyWithHeadersRequest
    :: forall ssc m . ResponseMode ssc m
    => Maybe (HeaderHash ssc) -> m ()
replyWithHeadersRequest upto = do
    msg <- mkHeadersRequest upto
    recordHeadersRequest msg =<< getUserState
    replyToNode msg

-- | Make message which requests a single block which is based on our tip.
mkBlockRequest :: HeaderHash ssc -> HeaderHash ssc -> MsgGetBlocks ssc
mkBlockRequest ourTip wantedBlock =
    MsgGetBlocks
    { mgbFrom = [ourTip]
    , mgbTo = Just wantedBlock
    }

-- | Reply with message which requests a single block which is based
-- on our tip. This request is recorded in SocketState.
replyWithBlockRequest
    :: forall ssc m . ResponseMode ssc m
    => HeaderHash ssc -> HeaderHash ssc -> m ()
replyWithBlockRequest ourTip wantedBlock = do
    recordBlocksRequest msg =<< getUserState
    replyToNode msg
  where
    msg = mkBlockRequest ourTip wantedBlock
