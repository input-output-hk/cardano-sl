
module Cardano.Wallet.API.V1.Payments where

import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Types

import           Servant

type API
    =    "payments" :> Summary "Generates a new payment from the source Account to the target Address."
                    :> ReqBody '[JSON] Payment
                    :> Post '[JSON] Transaction
    :<|> "payments" :> "fees"
                    :> Summary "Estimate the fees which would originate from the payment."
                    :> ReqBody '[JSON] Payment
                    :> Post '[JSON] EstimatedFees
