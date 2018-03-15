module Cardano.Wallet.API.V1.Wallets where

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types

import           Servant

type API = Tags '["Wallets"] :>
    (    "wallets" :> Summary "Creates a new or restores an existing Wallet."
                   :> ReqBody '[ValidJSON] (New Wallet)
                   :> PostCreated '[ValidJSON] (WalletResponse Wallet)
    :<|> "wallets" :> Summary "Returns all the available wallets."
                   :> WalletRequestParams
                   :> FilterBy '["wallet_id", "balance"] Wallet
                   :> SortBy   '["balance"] Wallet
                   :> Get '[ValidJSON] (WalletResponse [Wallet])
    :<|> "wallets" :> CaptureWalletId
                   :> "password"
                   :> Summary "Updates the password for the given Wallet."
                   :> ReqBody '[ValidJSON] PasswordUpdate
                   :> Put '[ValidJSON] (WalletResponse Wallet)
    :<|> "wallets" :> CaptureWalletId
                   :> Summary "Deletes the given Wallet and all its accounts."
                   :> DeleteNoContent '[ValidJSON] NoContent
    :<|> "wallets" :> CaptureWalletId
                   :> Summary "Returns the Wallet identified by the given walletId."
                   :> Get '[ValidJSON] (WalletResponse Wallet)
    :<|> "wallets" :> CaptureWalletId
                   :> Summary "Update the Wallet identified by the given walletId."
                   :> ReqBody '[ValidJSON] (Update Wallet)
                   :> Put '[ValidJSON] (WalletResponse Wallet)
    )
    -- Nest the Accounts API, note that it is left out of the "Wallets" tag purposedly
    :<|> "wallets" :> Accounts.API
