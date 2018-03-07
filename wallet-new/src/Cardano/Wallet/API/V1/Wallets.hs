module Cardano.Wallet.API.V1.Wallets where

import           Cardano.Wallet.API.Request
import qualified Cardano.Wallet.API.Request.Parameters as P
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types

import           Servant

type API =
         "wallets" :> Summary "Creates a new or restores an existing Wallet."
                   :> ReqBody '[ValidJSON] (New Wallet)
                   :> PostCreated '[ValidJSON] (WalletResponse Wallet)
    :<|> "wallets" :> Summary "Returns all the available wallets."
                   :> WalletRequestParams
                   :> FilterBy '[P.WalletId, P.Balance] Wallet
                   :> SortBy   '[P.Balance] Wallet
                   :> Get '[ValidJSON] (WalletResponse [Wallet])
    :<|> "wallets" :> Capture "walletId" WalletId
                   :> ( "password" :> Summary "Updates the password for the given Wallet."
                                   :> ReqBody '[ValidJSON] PasswordUpdate
                                   :> Put '[ValidJSON] (WalletResponse Wallet)
                   :<|> Summary "Deletes the given Wallet and all its accounts."
                        :> DeleteNoContent '[ValidJSON] NoContent
                   :<|> Summary "Returns the Wallet identified by the given walletId."
                        :> Get '[ValidJSON] (WalletResponse Wallet)
                   :<|> Summary "Update the Wallet identified by the given walletId."
                        :> ReqBody '[ValidJSON] (Update Wallet)
                        :> Put '[ValidJSON] (WalletResponse Wallet)
                   -- Nest the Accounts API
                   :<|> Tags '["Accounts"] :> Accounts.API
                   )
