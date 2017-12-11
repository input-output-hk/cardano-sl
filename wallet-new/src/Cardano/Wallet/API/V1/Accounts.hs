module Cardano.Wallet.API.V1.Accounts where

import           Servant

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types


type API
    =    "accounts" :> Capture "accountId" AccountId
                    :> Summary "Deletes an Account."
                    :> DeleteNoContent '[JSON] NoContent
    :<|> "accounts" :> Capture "accountId" AccountId
                    :> Summary "Retrieves a specific Account, given its Id."
                    :> Get '[JSON] (WalletResponse Account)
    :<|> "accounts" :> WalletRequestParams
                    :> Summary "Retrieves the full list of Accounts."
                    :> Get '[JSON] (WalletResponse [Account])
    :<|> "accounts" :> Summary "Creates a new Account for the given Wallet."
                    :> ReqBody '[JSON] (New Account)
                    :> Post '[JSON] (WalletResponse Account)
    :<|> "accounts" :> Capture "accountId" AccountId
                    :> Summary "Update an Account for the given Wallet."
                    :> ReqBody '[JSON] (Update Account)
                    :> Put '[JSON] (WalletResponse Account)
