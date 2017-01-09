{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrappers for network requests.

module Pos.Block.Network.Request
       ( mkHeadersRequest
       , replyWithHeadersRequest
       , mkBlocksRequest
       , replyWithBlocksRequest
       ) where

import           Formatting                     (build, sformat, (%))
import           System.Wlog                    (logDebug)
import           Universum

import           Message.Message                (BinaryP, messageName)
import           Mockable.Monad                 (MonadMockable (..))
import           Node                           (Listener (..), ListenerAction (..),
                                                 NodeId (..), SendActions (..), sendTo)
import           Pos.Binary.Communication       ()
import           Pos.Block.Logic                (getHeadersOlderExp)
import           Pos.Block.Network.Server.State (recordBlocksRequest,
                                                 recordHeadersRequest)
import           Pos.Block.Network.Types        (MsgGetBlocks (..), MsgGetHeaders (..))
import           Pos.Communication.BiP          (BiP (..))
import           Pos.Communication.PeerState    (getPeerState)
import           Pos.Ssc.Class.Types            (Ssc (..))
import           Pos.Types                      (HeaderHash)
import           Pos.WorkMode                   (NewWorkMode)



-- | Make 'GetHeaders' message using our main chain. This function
-- chooses appropriate 'from' hashes and puts them into 'GetHeaders'
-- message.
mkHeadersRequest
    :: NewWorkMode ssc m
    => Maybe (HeaderHash ssc) -> m (MsgGetHeaders ssc)
mkHeadersRequest upto = do
    headers <- getHeadersOlderExp Nothing
    pure $ MsgGetHeaders headers upto

replyWithHeadersRequest
    :: forall ssc m.
       (Ssc ssc, NewWorkMode ssc m)
    => Maybe (HeaderHash ssc)
    -> NodeId
    -> SendActions BiP m
    -> m ()
replyWithHeadersRequest upto peerId sendActions = do
    logDebug "replyWithHeadersRequest: preparing request to be sent"
    msg <- mkHeadersRequest upto
    -- TODO [CSL-447] Do smth with recorded result
    recorded <- recordHeadersRequest msg =<< getPeerState peerId
    sendTo sendActions peerId msg
    logDebug "replyWithHeadersRequest: data sent"

-- | Make message which requests chain of blocks which is based on our
-- tip. LcaChild is the first block after LCA we don't
-- know. WantedBlock is the newest one we want to get.
mkBlocksRequest :: HeaderHash ssc -> HeaderHash ssc -> MsgGetBlocks ssc
mkBlocksRequest lcaChild wantedBlock =
    MsgGetBlocks
    { mgbFrom = lcaChild
    , mgbTo = wantedBlock
    }

-- | Reply with message which requests chain of blocks which is based
-- on our tip. This request is recorded in PeerState.
replyWithBlocksRequest
    :: forall ssc m.
       (Ssc ssc, NewWorkMode ssc m)
    => HeaderHash ssc
    -> HeaderHash ssc
    -> NodeId
    -> SendActions BiP m
    -> m ()
replyWithBlocksRequest lcaChild wantedBlock peerId sendActions = do
    logDebug $
        sformat ("replyWithBlocksRequest: asking from (lca child) "%build%" to (new tip) "%build)
                lcaChild wantedBlock
    -- TODO [CSL-447] Do smth with recorded result
    recorded <- recordBlocksRequest lcaChild wantedBlock =<< getPeerState peerId
    logDebug "replyWithBlocksRequest: replying to node"
    sendTo sendActions peerId msg
    logDebug "replyWithBlocksRequest: replied"
  where
    msg = mkBlocksRequest lcaChild wantedBlock
