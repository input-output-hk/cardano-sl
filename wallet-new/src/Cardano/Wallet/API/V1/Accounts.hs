module Cardano.Wallet.API.V1.Accounts where

import           Servant

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types


type API
    = Tags '["Accounts"] :>
    (    "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId
          :> Summary "Deletes an Account."
          :> DeleteNoContent '[ValidJSON] NoContent
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId
          :> Summary "Retrieves a specific Account."
          :> Get '[ValidJSON] (WalletResponse Account)
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> WalletRequestParams
          :> Summary "Retrieves the full list of Accounts."
          :> Get '[ValidJSON] (WalletResponse [Account])
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> Summary "Creates a new Account for the given Wallet."
          :> ReqBody '[ValidJSON] (New Account)
          :> Post '[ValidJSON] (WalletResponse Account)
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId
          :> Summary "Update an Account for the given Wallet."
          :> ReqBody '[ValidJSON] (Update Account)
          :> Put '[ValidJSON] (WalletResponse Account)
    )
