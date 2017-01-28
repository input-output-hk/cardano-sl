{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrappers on top of communication methods (for Pos.Util.Relay based methods).

module Pos.Communication.Methods
       ( sendTx
       , sendVote
       , sendUpdateProposal
       ) where

import           Formatting                  (build, sformat, shown, (%))
import           Mockable                    (handleAll)
import           Node                        (SendActions)
import           System.Wlog                 (logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Binary.Relay            ()
import           Pos.Binary.Types            ()
import           Pos.Communication.BiP       (BiP)
import           Pos.Crypto                  (encodeHash, hash)
import           Pos.DHT.Model.Neighbors     (sendToNode)
import           Pos.Txp.Types.Communication (TxMsgContents (..))
import           Pos.Types                   (TxAux)
import           Pos.Update                  (UpId, UpdateProposal, UpdateVote, mkVoteId)
import           Pos.Util.Relay              (DataMsg (..))
import           Pos.Util.TimeWarp           (NetworkAddress)
import           Pos.WorkMode                (MinWorkMode)

-- | Send Tx to given address.
sendTx :: (MinWorkMode m) => SendActions BiP m -> NetworkAddress -> TxAux -> m ()
sendTx sendActions addr (tx,w,d) = handleAll handleE $ do
    sendToNode sendActions addr $ DataMsg (TxMsgContents tx w d) (hash tx)
  where
    handleE e =
      logWarning $ sformat ("Error sending tx "%build%" to "%shown%": "%shown) tx addr e

-- Send UpdateVote to given address.
sendVote :: (MinWorkMode m) => SendActions BiP m -> NetworkAddress -> UpdateVote -> m ()
sendVote sendActions addr vote = handleAll handleE $
    sendToNode sendActions addr $ DataMsg vote (mkVoteId vote)
  where
    handleE e = logWarning $ sformat ("Error sending UpdateVote "%build%" to "%shown%": "%shown) vote addr e

-- Send UpdateProposal to given address.
sendUpdateProposal
    :: (MinWorkMode m)
    => SendActions BiP m
    -> NetworkAddress
    -> UpId
    -> UpdateProposal
    -> [UpdateVote]
    -> m ()
sendUpdateProposal sendActions addr upid proposal votes = do
    logInfo $ sformat ("Announcing proposal with id "%build%
                        " (base64 is "%build%")")
        upid (encodeHash upid)
    handleAll handleE $
        sendToNode sendActions addr $ DataMsg (proposal, votes) upid
  where
    handleE e = logWarning $ sformat ("Error sending UpdateProposal "%build%" to "%shown%": "%shown) (proposal, votes) addr e
