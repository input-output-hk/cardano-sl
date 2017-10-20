module Cardano.Wallet.API.V1.Wallets where

import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Types

import           Servant

type API
    = "wallets"  :> Capture "walletId" WalletId
                 :> "accounts"
                 :> Summary "Creates a new Account for the given Wallet."
                 :> ReqBody '[JSON] Account :> Post '[JSON] Account
