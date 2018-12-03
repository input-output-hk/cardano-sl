module Cardano.Wallet.API.V1.Transactions where

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types
import qualified Pos.Chain.Txp as Txp
import qualified Pos.Core as Core

import           Servant

type API = Tags '["Transactions"] :>
    (    "transactions" :> Summary "Generates a new transaction from the source to one or multiple target addresses."
                        :> ReqBody '[ValidJSON] Payment
                        :> Post '[ValidJSON] (APIResponse Transaction)
    :<|> "transactions" :> Summary "Returns the transaction history, i.e the list of all the past transactions."
                        :> QueryParam "wallet_id" WalletId
                        :> QueryParam "account_index" AccountIndex
                        :> QueryParam "address" (V1 Core.Address)
                        :> WalletRequestParams
                        :> FilterBy '[ V1 Txp.TxId
                                     , V1 Core.Timestamp
                                     ] Transaction
                        :> SortBy   '[ V1 Core.Timestamp
                                     ] Transaction
                        :> Get '[ValidJSON] (APIResponse [Transaction])
    :<|> "transactions" :> "fees"
                        :> Summary "Estimate the fees which would originate from the payment."
                        :> ReqBody '[ValidJSON] Payment
                        :> Post '[ValidJSON] (APIResponse EstimatedFees)
    :<|> "transactions" :> "certificates"
                        :> Summary "Redeem a certificate"
                        :> ReqBody '[ValidJSON] Redemption
                        :> Post '[ValidJSON] (APIResponse Transaction)
    )
