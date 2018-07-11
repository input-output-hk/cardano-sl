module Cardano.Wallet.API.V1.Handlers (handlers) where

import           Servant
import           Universum

import qualified Cardano.Wallet.API.V1 as V1
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)
import           Cardano.Wallet.WalletLayer.Types
import           Mockable

handlers :: ActiveWalletLayer Production -> Server V1.API
handlers w = addresses :<|> wallets :<|> accounts :<|> transactions :<|> settings :<|> info
  where
    _ = walletPassiveLayer w

    addresses = todo
    wallets = todo
    accounts = todo
    transactions = (todo :<|> getTransactionsHistory :<|> todo)
    settings = todo
    info = todo

    todo = error "TODO"

    getTransactionsHistory _ _ _ _ _ _ =
      liftIO $ runProduction ret
        where
          ret = error "TODO" -- CBR-239
