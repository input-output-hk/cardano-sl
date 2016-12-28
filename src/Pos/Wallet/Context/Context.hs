-- | Runtime context of wallet

module Pos.Wallet.Context.Context
       ( WalletContext (..)
       , ctxFromParams
       , fromNodeCtx
       ) where

import           Pos.Context               (NodeContext (..))
import           Pos.Types                 (Timestamp)

import           Pos.Wallet.Launcher.Param (WalletParams (..))

data WalletContext = WalletContext
    { wcSystemStart :: !Timestamp -- ^ Time when system started working
    }

ctxFromParams :: WalletParams -> WalletContext
ctxFromParams WalletParams {..} = WalletContext
    { wcSystemStart = wpSystemStart
    }

fromNodeCtx :: NodeContext ssc -> WalletContext
fromNodeCtx NodeContext {..} = WalletContext
    { wcSystemStart = ncSystemStart
    }
