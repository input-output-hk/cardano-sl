
module Cardano.Wallet.Server where

import Cardano.Wallet.API
import qualified Cardano.Wallet.API.V1.Handlers as V1
import qualified Cardano.Wallet.API.V0.Handlers as V0

import Servant

walletServer :: Server WalletAPI
walletServer =   V0.handlers
            :<|> V0.handlers
            :<|> V1.handlers
