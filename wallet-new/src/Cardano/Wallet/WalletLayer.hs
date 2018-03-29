module Cardano.Wallet.WalletLayer
    ( PassiveWalletLayer (..)
    ) where

import           Universum

import           Cardano.Wallet.API.V1.Types (WalletId)
import           Pos.Wallet.Web.ClientTypes (CWalletMeta)

-- | The wallet data layer.
data PassiveWalletLayer m = PassiveWalletLayer
    { pwlGetWalletAddresses :: m [WalletId]
    , pwlGetWalletMeta      :: WalletId -> m (Maybe CWalletMeta)
    }


--makeLenses ''PassiveWalletLayer
