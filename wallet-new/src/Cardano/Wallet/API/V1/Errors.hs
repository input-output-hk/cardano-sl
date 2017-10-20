
module Cardano.Wallet.API.V1.Errors where

import           Cardano.Wallet.API.V1.Types (WalletError (..))



walletNotFound :: WalletError
walletNotFound = WalletError {
      err_code = 600
    , err_message = "The requested Wallet cannot be found."
    }
