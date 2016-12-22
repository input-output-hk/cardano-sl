{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrappers for network requests.

module Pos.Block.Requests
       ( mkHeadersRequest
       , replyWithHeadersRequest
       , mkBlocksRequest
       , replyWithBlocksRequest
       ) where

import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Logic          (getHeadersOlderExp)
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
mkHeadersRequest upto = do
    headers <- getHeadersOlderExp upto
    pure $ MsgGetHeaders headers upto

replyWithHeadersRequest
    :: forall ssc m . ResponseMode ssc m
    => Maybe (HeaderHash ssc) -> m ()
replyWithHeadersRequest upto = do
    msg <- mkHeadersRequest upto
    recordHeadersRequest msg =<< getUserState
    replyToNode msg

-- | Make message which requests chain of blocks which is based on our
-- tip. LcdChild is the first block after LCD we don't
-- know. WantedBlock is the newest one we want to get.
mkBlocksRequest :: HeaderHash ssc -> HeaderHash ssc -> MsgGetBlocks ssc
mkBlocksRequest lcdChild wantedBlock =
    MsgGetBlocks
    { mgbFrom = lcdChild
    , mgbTo = wantedBlock
    }

-- | Reply with message which requests chain of blocks which is based
-- on our tip. This request is recorded in SocketState.
replyWithBlocksRequest
    :: forall ssc m . ResponseMode ssc m
    => HeaderHash ssc -> HeaderHash ssc -> m ()
replyWithBlocksRequest ourTip wantedBlock = do
    recordBlocksRequest ourTip wantedBlock =<< getUserState
    replyToNode msg
  where
    msg = mkBlocksRequest ourTip wantedBlock
