module Cardano.Wallet.WalletLayer.Types
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    ) where

import           Universum ()
import           Cardano.Wallet.API.V1.Types (WalletId)
import           Pos.Util.Chrono (NE, OldestFirst (..))
import           Pos.Block.Types(Blund)

-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    { pwlGetWalletIds       :: m [WalletId]
    , pwlApplyBlocks        :: OldestFirst NE Blund -> m ()
    }

-- An active wallet layer. See @ActiveWallet@.
data ActiveWalletLayer m = ActiveWalletLayer {
      -- | The underlying passive wallet layer
      walletPassiveLayer :: PassiveWalletLayer m
    }
