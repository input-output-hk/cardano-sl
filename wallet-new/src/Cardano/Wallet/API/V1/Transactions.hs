
module Cardano.Wallet.API.V1.Transactions where

import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Types

import           Servant

type API
    =    "transactions" :> Summary "Generates a new transaction from the source to one or multiple target addresses."
                        :> ReqBody '[JSON] Payment
                        :> Post '[JSON] Transaction
    :<|> "transactions" :> "fees"
                        :> Summary "Estimate the fees which would originate from the payment."
                        :> ReqBody '[JSON] Payment
                        :> Post '[JSON] EstimatedFees
