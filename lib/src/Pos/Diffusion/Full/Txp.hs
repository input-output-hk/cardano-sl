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
import           Pos.Communication.Limits (HasTxpLimits)
import           Pos.Communication.Protocol (EnqueueMsg, MsgType (..), Origin (..))
import           Pos.Communication.Relay (invReqDataFlowTK, resOk, MinRelayWorkMode)
import           Pos.Core.Txp (TxAux (..))
import           Pos.Crypto (hash)
import           Pos.Txp.Network.Types (TxMsgContents (..))

-- | Send Tx to given addresses.
-- Returns 'True' if any peer accepted and applied this transaction.
sendTx
    :: ( MinRelayWorkMode m
       , HasTxpLimits m
       )
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
