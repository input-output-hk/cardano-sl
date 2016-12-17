-- | Runtime context of wallet

module Pos.Wallet.Context.Context
       ( WalletContext (..)
       , ctxFromParams
       ) where

import           Pos.Types                 (Timestamp)
import           Universum

import           Pos.Wallet.Launcher.Param (WalletParams (..))

data WalletContext = WalletContext
    { wcSystemStart :: !Timestamp -- ^ Time when system started working
    }

ctxFromParams :: WalletParams -> WalletContext
ctxFromParams WalletParams {..} = WalletContext
    { wcSystemStart = wpSystemStart
    }
