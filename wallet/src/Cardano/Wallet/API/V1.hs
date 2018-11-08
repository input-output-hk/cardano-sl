module Cardano.Wallet.API.V1 where


import           Servant ((:<|>))

import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import qualified Cardano.Wallet.API.V1.Addresses as Addresses
import qualified Cardano.Wallet.API.V1.Transactions as Transactions
import qualified Cardano.Wallet.API.V1.Wallets as Wallets

import qualified Pos.Node.API as Node

type API =  Addresses.API
       :<|> Wallets.API
       :<|> Accounts.API
       :<|> Transactions.API
       :<|> Node.SettingsAPI
       :<|> Node.InfoAPI
