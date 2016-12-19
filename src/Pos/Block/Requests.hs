{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrappers for network requests.

module Pos.Block.Requests
       ( mkBlockRequest
       , replyWithBlockRequest
       ) where

import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Server.State   (recordBlocksRequest)
import           Pos.Communication.Types  (MsgGetBlocks (..), ResponseMode)
import           Pos.DHT.Model            (getUserState, replyToNode)
import           Pos.Types                (HeaderHash)

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
