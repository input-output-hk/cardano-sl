{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrappers on top of communication methods (for Pos.Util.Relay based methods).

module Pos.Communication.Methods
       ( sendTx
       ) where

import           Node                        (SendActions)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Binary.Relay            ()
import           Pos.Binary.Types            ()
import           Pos.Communication.BiP       (BiP)
import           Pos.Crypto                  (hash)
import           Pos.DHT.Model.Neighbors     (sendToNode)
import           Pos.Txp.Types.Communication (TxMsgContents (..))
import           Pos.Types                   (TxAux)
import           Pos.Util.Relay              (DataMsg (..))
import           Pos.Util.TimeWarp           (NetworkAddress)
import           Pos.WorkMode                (MinWorkMode)

-- | Send Tx to given address.
sendTx :: (MinWorkMode m) => SendActions BiP m -> NetworkAddress -> TxAux -> m ()
sendTx sendActions addr (tx,w,d) = do
    --sendToNode addr VersionReq
    sendToNode sendActions addr $ DataMsg (TxMsgContents tx w d) (hash tx)
    return ()
