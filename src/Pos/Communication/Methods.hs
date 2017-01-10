{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrappers on top of communication methods (for Pos.Util.Relay based methods).

module Pos.Communication.Methods
       ( sendTx
       ) where

import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Binary.Relay            ()
import           Pos.Binary.Types            ()
import           Pos.Crypto                  (hash)
import           Pos.Txp.Types.Communication (TxMsgContents (..))
import           Pos.Types                   (TxAux)
import           Pos.Util.Relay              (DataMsg (..))
import           Pos.Util.TimeWarp           (NetworkAddress)

-- | Send Tx to given address.
sendTx :: (Monad m) => NetworkAddress -> TxAux -> m ()
sendTx addr (tx,w,d) = do
    --sendToNode addr VersionReq
    -- TODO: CSL-447 uncomment
    --sendToNode addr $ DataMsg (TxMsgContents tx w d) (hash tx)
    return ()
