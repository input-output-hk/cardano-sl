module Cardano.Wallet.API.V1.Handlers (handlers) where

import           Servant
import           Universum

import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.V1.Handlers.Accounts as Accounts
import qualified Cardano.Wallet.API.V1.Handlers.Addresses as Addresses
import qualified Cardano.Wallet.API.V1.Handlers.Info as Info
import qualified Cardano.Wallet.API.V1.Handlers.Settings as Settings
import qualified Cardano.Wallet.API.V1.Handlers.Transactions as Transactions
import qualified Cardano.Wallet.API.V1.Handlers.Wallets as Wallets

import           Cardano.Wallet.WalletLayer (ActiveWalletLayer,
                     walletPassiveLayer)

-- TODO: Move to proper place
--import qualified Cardano.Node.API as Node


handlers :: ActiveWalletLayer IO -> Server V1.API
handlers w =  Addresses.handlers    passiveWallet
         :<|> Wallets.handlers      passiveWallet
         :<|> Accounts.handlers     passiveWallet
         :<|> Transactions.handlers w
         :<|> Settings.handlers     passiveWallet
         :<|> Info.handlers         w
  where
    passiveWallet = walletPassiveLayer w
