-- | Runtime context of wallet

module Pos.Wallet.Context.Context
       ( WalletContext (..)
       , fromNodeCtx
       ) where

import           Pos.Context (NodeContext)

data WalletContext = WalletContext
    { wcUnit   :: !() -- ^ There is exactly one value of this type.
    }

fromNodeCtx :: NodeContext ssc -> WalletContext
fromNodeCtx _ = WalletContext
    { wcUnit = ()
    }
