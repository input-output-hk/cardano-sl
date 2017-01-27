{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrappers on top of communication methods

module Pos.Communication.Methods
       ( sendTx
       , sendVote
       , sendUpdateProposal
       ) where

import           Formatting                       (build, sformat, shown, (%))
import           Mockable                         (handleAll)
import           Node                             (SendActions)
import           System.Wlog                      (logWarning)
import           Universum

import           Pos.Binary.Communication         ()
import           Pos.Binary.Relay                 ()
import           Pos.Binary.Types                 ()
import           Pos.Communication.BiP            (BiP)
import           Pos.Communication.Message        ()
import           Pos.Communication.Relay          (DataMsg (..))
import           Pos.Communication.Types.Protocol (PeerId)
import           Pos.Crypto                       (hash)
import           Pos.DHT.Model.Neighbors          (sendToNode)
import           Pos.Txp.Types.Communication      (TxMsgContents (..))
import           Pos.Types                        (TxAux)
import           Pos.Update                       (UpId, UpdateProposal, UpdateVote,
                                                   mkVoteId)
import           Pos.Util.TimeWarp                (NetworkAddress)
import           Pos.WorkMode                     (MinWorkMode)

-- | Send Tx to given address.
sendTx :: (MinWorkMode m) => SendActions BiP PeerId m -> NetworkAddress -> TxAux -> m ()
sendTx sendActions addr (tx,w,d) = handleAll handleE $ do
    sendToNode sendActions addr $ DataMsg (TxMsgContents tx w d) (hash tx)
  where
    handleE e =
      logWarning $ sformat ("Error sending tx "%build%" to "%shown%": "%shown) tx addr e

-- Send UpdateVote to given address.
sendVote :: (MinWorkMode m) => SendActions BiP PeerId m -> NetworkAddress -> UpdateVote -> m ()
sendVote sendActions addr vote = handleAll handleE $
    sendToNode sendActions addr $ DataMsg vote (mkVoteId vote)
  where
    handleE e = logWarning $ sformat ("Error sending UpdateVote "%build%" to "%shown%": "%shown) vote addr e

-- Send UpdateProposal to given address.
sendUpdateProposal
    :: (MinWorkMode m)
    => SendActions BiP PeerId m
    -> NetworkAddress
    -> UpId
    -> UpdateProposal
    -> [UpdateVote]
    -> m ()
sendUpdateProposal sendActions addr upid proposal votes =
    handleAll handleE $
        sendToNode sendActions addr $ DataMsg (proposal, votes) upid
  where
    handleE e = logWarning $ sformat ("Error sending UpdateProposal "%build%" to "%shown%": "%shown) (proposal, votes) addr e
