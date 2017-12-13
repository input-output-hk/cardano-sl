
module Cardano.Wallet.API.V1.Transactions where

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types

import           Servant

type API
    =    "transactions" :> Summary "Generates a new transaction from the source to one or multiple target addresses."
                        :> ReqBody '[ValidJSON] (New Payment)
                        :> Post '[ValidJSON] (WalletResponse Transaction)
    :<|> "transactions" :> Summary "Returns the transaction history, i.e the list of all the past transactions."
                        :> Capture "walletId" WalletId
                        :> WalletRequestParams
                        :> Get '[ValidJSON] (WalletResponse [Transaction])
    :<|> "transactions" :> "fees"
                        :> Summary "Estimate the fees which would originate from the payment."
                        :> ReqBody '[ValidJSON] Payment
                        :> Post '[ValidJSON] (WalletResponse EstimatedFees)
