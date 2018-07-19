module Cardano.Wallet.API.V1.Handlers (handlers) where

import           Servant
import           Universum

import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.V1.Handlers.Addresses as Addresses
import qualified Cardano.Wallet.API.V1.Handlers.Transactions as Transactions
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)
import           Cardano.Wallet.WalletLayer.Types
import           Pos.Core.Mockable

handlers :: ActiveWalletLayer Production -> Server V1.API
handlers w =  Addresses.handlers w
         :<|> wallets
         :<|> accounts
         :<|> Transactions.handlers w
         :<|> settings
         :<|> info
  where
    _ = walletPassiveLayer w

    wallets = todo
    accounts = todo
    settings = todo
    info = todo

    todo = error "TODO"
