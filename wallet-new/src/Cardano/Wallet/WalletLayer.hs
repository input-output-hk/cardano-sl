module Cardano.Wallet.WalletLayer
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    ) where

import           Universum

import           Cardano.Wallet.API.V1.Types (WalletId)
import           Pos.Wallet.Web.ClientTypes (CWalletMeta)

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))


-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    { pwlGetWalletAddresses :: m [WalletId]
    , pwlGetWalletMeta      :: WalletId -> m (Maybe CWalletMeta)
    }

--makeLenses ''PassiveWalletLayer

-- | Active wallet layer
--
-- An active wallet layer. See @ActiveWallet@.
data ActiveWalletLayer m = ActiveWalletLayer {
      -- | The underlying passive wallet layer
      walletPassiveLayer :: PassiveWalletLayer m

      -- | The wallet diffusion layer
    , walletDiffusion    :: WalletDiffusion
    }

