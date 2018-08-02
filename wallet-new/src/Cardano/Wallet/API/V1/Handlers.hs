module Cardano.Wallet.API.V1.Handlers (handlers) where

import           Servant
import           Universum

import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.V1.Handlers.Accounts as Accounts
import qualified Cardano.Wallet.API.V1.Handlers.Addresses as Addresses
import qualified Cardano.Wallet.API.V1.Handlers.Transactions as Transactions
import qualified Cardano.Wallet.API.V1.Handlers.Wallets as Wallets

import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)
import           Cardano.Wallet.WalletLayer.Types (walletPassiveLayer)


handlers :: ActiveWalletLayer IO -> Server V1.API
handlers w =  Addresses.handlers passiveWallet
         :<|> Wallets.handlers   passiveWallet
         :<|> Accounts.handlers passiveWallet
         :<|> Transactions.handlers w
         :<|> settings
         :<|> info
  where
    passiveWallet = walletPassiveLayer w

    settings = todo
    info = todo

    todo = error "TODO"
