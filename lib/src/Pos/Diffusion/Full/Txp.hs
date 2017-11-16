{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Diffusion.Full.Txp
       ( sendTx
       ) where

import           Universum

import           Pos.Binary.Communication ()
import           Pos.Binary.Core ()
import           Pos.Binary.Relay ()
import           Pos.Binary.Txp ()
import           Pos.Communication.Message ()
import           Pos.Communication.Protocol (EnqueueMsg, MsgType (..), Origin (..))
import           Pos.Communication.Relay (invReqDataFlowTK, resOk)
import           Pos.Core.Txp (TxAux (..))
import           Pos.Crypto (hash)
import           Pos.Txp.Network.Types (TxMsgContents (..))
import           Pos.WorkMode.Class (WorkMode)


-- | Send Tx to given addresses.
-- Returns 'True' if any peer accepted and applied this transaction.
sendTx
    :: ( WorkMode ctx m )
    => EnqueueMsg m
    -> TxAux
    -> m Bool
sendTx enqueue txAux = do
    anySucceeded <$> invReqDataFlowTK
        "tx"
        enqueue
        (MsgTransaction OriginSender)
        (hash $ taTx txAux)
        (TxMsgContents txAux)
  where
    anySucceeded outcome =
        not $ null
        [ ()
        | Right (Just peerResponse) <- toList outcome
        , resOk peerResponse
        ]
