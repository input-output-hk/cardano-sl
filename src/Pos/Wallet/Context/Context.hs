-- | Runtime context of wallet

module Pos.Wallet.Context.Context
       ( WalletContext (..)
       ) where

import           Pos.Types (Timestamp)

data WalletContext = WalletContext
    { wcSystemStart :: !Timestamp -- ^ Time when system started working
    }
