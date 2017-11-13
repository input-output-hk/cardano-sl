-- | Abstraction of wallet's networking actions (like sending tx, etc)

module Pos.Wallet.Web.Networking
       ( MonadWalletSendActions (..)
       ) where

import           Universum

import           Pos.Core.Txp (TxAux)

class Monad m => MonadWalletSendActions m where
    sendTxToNetwork :: TxAux -> m Bool
