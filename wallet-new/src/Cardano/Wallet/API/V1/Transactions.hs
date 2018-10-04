module Cardano.Wallet.API.V1.Transactions where

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types
import qualified Pos.Core as Core

import           Servant

type API = Tags '["Transactions"] :>
    (    "transactions" :> Summary "Generates a new transaction from the source to one or multiple target addresses."
                        :> ReqBody '[ValidJSON] Payment
                        :> Post '[ValidJSON] (WalletResponse Transaction)
    :<|> "transactions" :> Summary "Returns the transaction history, i.e the list of all the past transactions."
                        :> QueryParam "wallet_id" WalletId
                        :> QueryParam "account_index" AccountIndex
                        :> QueryParam "address" (V1 Core.Address)
                        :> WalletRequestParams
                        :> FilterBy '[ V1 Core.TxId
                                     , V1 Core.Timestamp
                                     ] Transaction
                        :> SortBy   '[ V1 Core.Timestamp
                                     ] Transaction
                        :> Get '[ValidJSON] (WalletResponse [Transaction])
    :<|> "transactions" :> "fees"
                        :> Summary "Estimate the fees which would originate from the payment."
                        :> ReqBody '[ValidJSON] Payment
                        :> Post '[ValidJSON] (WalletResponse EstimatedFees)
    )
