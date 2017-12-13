-- | Abstraction of wallet's networking actions (like sending tx, etc)

module Pos.Wallet.Web.Networking
       ( MonadWalletSendActions (..)
       ) where

import           Universum

import           Pos.Core.Txp (TxAux)

-- TODO deprecate and replace.
-- In the mean-time, we can get an instance of MonadWalletSendActions for a
--  ReaderT r m  equipped with some terms derived from a  Diffusion m ,
-- namely  sendTx :: Diffusion m -> TxAux -> m Bool
-- I say deprecate and replace because it's not clear whether there's much use
-- having a class for this, versus using a  Diffusion m  record to pass its
--  sendTx  to the wallet web server handlers which actually need it.
class Monad m => MonadWalletSendActions m where
    sendTxToNetwork :: TxAux -> m Bool
