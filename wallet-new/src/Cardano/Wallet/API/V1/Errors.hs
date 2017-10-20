
module Cardano.Wallet.API.V1.Errors where

import           Cardano.Wallet.API.V1.Types (WalletError (..))



walletNotFound :: WalletError
walletNotFound = WalletError {
      errCode = 600
    , errMessage = "The requested Wallet cannot be found."
    }
