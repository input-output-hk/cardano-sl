module Cardano.Wallet.API.V1.Wallets where

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types

import           Servant

type API =
         "wallets" :> Summary "Creates a new Wallet."
                   :> ReqBody '[JSON] (New Wallet)
                   :> PostCreated '[JSON] (WalletResponse Wallet)
    :<|> "wallets" :> Summary "Returns all the available wallets."
                   :> WalletRequestParams
                   :> Get '[JSON] (WalletResponse [Wallet])
    :<|> "wallets" :> Capture "walletId" WalletId
                   :> ( "password" :> Summary "Updates the password for the given Wallet."
                                   :> ReqBody '[JSON] PasswordUpdate
                                   :> Put '[JSON] (WalletResponse Wallet)
                   :<|> Summary "Deletes the given Wallet and all its accounts."
                        :> DeleteNoContent '[JSON] NoContent
                   :<|> Summary "Returns the Wallet identified by the given walletId."
                        :> Get '[JSON] (WalletResponse Wallet)
                   :<|> Summary "Update the Wallet identified by the given walletId."
                        :> ReqBody '[JSON] (Update Wallet)
                        :> Put '[JSON] (WalletResponse Wallet)
                   -- Nest the Accounts API
                   :<|> Tags '["Accounts"] :> Accounts.API
                   )
