{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrappers on top of communication methods (for Pos.Util.Relay based methods).

module Pos.Communication.RelayMethods
       ( sendTx
       ) where

import           Control.TimeWarp.Rpc        (NetworkAddress)
import           Universum

import           Pos.Binary.Class            (Bi)
import           Pos.Binary.Communication    ()
import           Pos.Binary.Relay            ()
import           Pos.Binary.Types            ()
import           Pos.Crypto                  (hash)
import           Pos.DHT.Model               (MonadMessageDHT, sendToNode)
import           Pos.Txp.Types.Communication (TxMsgContents (..))
import           Pos.Types                   (TxAux)
import           Pos.Util.Relay              (DataMsg (..))

-- | Send Tx to given address.
sendTx :: (MonadMessageDHT s m) => NetworkAddress -> TxAux -> m ()
sendTx addr (tx,w,d) = do
    --sendToNode addr VersionReq
    sendToNode addr $ DataMsg (TxMsgContents tx w d) (hash tx)
